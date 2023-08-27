use std::io::Read;
use itertools::{Either, Itertools};
use crate::classfile::{ClassInfo, ConstantPool, ConstantPoolElement, DoubleInfo, FloatInfo, IntegerInfo, LongInfo, MethodHandleInfo, MethodTypeInfo, NameAndTypeInfo, parse_u1, parse_u1_as_usize, parse_u2, parse_u2_as_usize, parse_u4, parse_u4_as_usize, parse_vec, StringInfo, Utf8Info};
use crate::code::{Code, PcMap};
use crate::errors::{AttributeTagMismatchError, ClassFileParseError, ConstantPoolTagMismatchError};

fn check_attribute_length<R: Read>(reader: &mut R, length: u32) -> Result<(), ClassFileParseError> {
	let len = parse_u4(reader)?;
	if len == length {
		Ok(())
	} else {
		Err(ClassFileParseError::InvalidAttributeLength {
			expected: length,
			actual: len,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValueAttribute { // 4.7.2
	Long(LongInfo),
	Float(FloatInfo),
	Double(DoubleInfo),
	Integer(IntegerInfo),
	String(StringInfo),
}
impl ConstantValueAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<ConstantValueAttribute, ClassFileParseError> {
		check_attribute_length(reader, 2)?;

		match constant_pool.parse_index_get(reader)? {
			ConstantPoolElement::Long(long) => Ok(ConstantValueAttribute::Long(long.clone())),
			ConstantPoolElement::Float(float) => Ok(ConstantValueAttribute::Float(float.clone())),
			ConstantPoolElement::Double(double) => Ok(ConstantValueAttribute::Double(double.clone())),
			ConstantPoolElement::Integer(integer) => Ok(ConstantValueAttribute::Integer(integer.clone())),
			ConstantPoolElement::String(string) => Ok(ConstantValueAttribute::String(string.clone())),
			tag => Err(ConstantPoolTagMismatchError {
				expected: "Long/Float/Double/Integer".to_string(),
				actual: format!("{tag:?}"),
			})?,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeAttribute { // 4.7.3
	pub max_stack: u16,
	pub max_locals: u16,
	pub code: Code,
	pub exception_table: Vec<ExceptionTableEntry>,
	pub attributes: Vec<AttributeInfo>,

	pub line_number_table: Vec<LineNumberTableEntry>,
	pub stack_map_table: StackMapTableAttribute,
}

impl CodeAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<CodeAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;

		let max_stack = parse_u2(reader)?;
		let max_locals = parse_u2(reader)?;

		let code = parse_vec(reader,
			parse_u4_as_usize,
			parse_u1
		)?;
		let code = Code::parse(code, constant_pool)?;

		let exception_table = parse_vec(reader,
			parse_u2_as_usize,
			|r| ExceptionTableEntry::parse(r, constant_pool, &code.pc_map)
		)?;

		let attributes = parse_vec(reader,
		   parse_u2_as_usize,
		   |r| AttributeInfo::parse(r, constant_pool)
		)?;

		// LineNumberTableAttribute
		let (line_number_tables, attributes): (Vec<LineNumberTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::LineNumberTable(line_number_table) => Either::Left(line_number_table),
				other => Either::Right(other),
			});
		let line_number_table: Vec<LineNumberTableEntry> = line_number_tables.into_iter()
			.map(|table| table.line_number_table)
			.flatten()
			.map(|entry| entry.map_pc(&code.pc_map))
			.collect::<Result<_, _>>()?;

		// StackMapTableAttribute
		let (stack_map_tables, attributes): (Vec<StackMapTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::StackMapTable(stack_map_table) => Either::Left(stack_map_table),
				other => Either::Right(other),
			});

		let stack_map_table = match stack_map_tables.len() {
			0 => StackMapTableAttribute { entries: Vec::new() },
			1 => {
				stack_map_tables.into_iter().next().unwrap()
			}.map_pc(&code.pc_map)?,
			_ => todo!("there may at max be one stack map frame!"),
		};

		Ok(CodeAttribute {
			max_stack,
			max_locals,
			code,
			exception_table,
			attributes,
			line_number_table,
			stack_map_table,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExceptionTableEntry { // 4.7.3, exception_table
	start_pc: usize,
	end_pc: usize,
	handler_pc: usize,
	catch_type: Option<ClassInfo>,
}
impl ExceptionTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool, pc_map: &PcMap) -> Result<ExceptionTableEntry, ClassFileParseError> {
		Ok(ExceptionTableEntry {
			start_pc: pc_map.map(parse_u2_as_usize(reader)?)?,
			end_pc: pc_map.map(parse_u2_as_usize(reader)?)?,
			handler_pc: pc_map.map(parse_u2_as_usize(reader)?)?,
			catch_type: constant_pool.parse_index_optional(reader)?,
		})
	}
}

/// The [StackMapTableAttribute] attribute is a variable-length attribute in the attributes table of a [CodeAttribute] attribute. This attribute is used during
/// the process of verification by type checking (§4.10.1). A method's [CodeAttribute] attribute may have at most one [StackMapTableAttribute] attribute.
///
/// A [StackMapTableAttribute] attribute consists of zero or more stack map frames. Each stack map frame specifies (either explicitly or implicitly) a bytecode
/// offset, the verification types (§4.10.1.2) for the local variables, and the verification types for the operand stack.
///
/// The type checker deals with and manipulates the expected types of a method's local variables and operand stack. Throughout this section, a location refers
/// to either a single local variable or to a single operand stack entry.
///
/// We will use the terms stack map frame and type state interchangeably to describe a mapping from locations in the operand stack and local variables of a
/// method to verification types. We will usually use the term stack map frame when such a mapping is provided in the class file, and the term type state when
/// the mapping is used by the type checker.
///
/// In a class file whose version number is greater than or equal to 50.0, if a method's [CodeAttribute] attribute does not have a [StackMapTableAttribute]
/// attribute, it has an implicit stack map attribute. This implicit stack map attribute is equivalent to a [StackMapTableAttribute] attribute with
/// `number_of_entries` equal to zero.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackMapTableAttribute { // 4.7.4
	entries: Vec<StackMapFrame>,
}
impl StackMapTableAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<StackMapTableAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;

		let mut is_first_explicit_frame = true;
		let mut last_bytecode_position = 0;
		let count = parse_u2_as_usize(reader)?;
		let mut entries = Vec::with_capacity(count);
		for _ in 0..count {
			let (frame, new_bytecode_position) = StackMapFrame::parse(reader, constant_pool, last_bytecode_position, is_first_explicit_frame)?;
			entries.push(frame);
			
			is_first_explicit_frame = false;
			last_bytecode_position = new_bytecode_position;
		}

		Ok(StackMapTableAttribute {
			entries,
		})
	}

	fn map_pc(mut self, pc_map: &PcMap) -> Result<StackMapTableAttribute, ClassFileParseError> {
		Ok(StackMapTableAttribute {
			entries: self.entries.into_iter()
				.map(|e| e.map_pc(pc_map))
				.collect::<Result<Vec<_>, _>>()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationTypeInfo {
	/// The [VerificationTypeInfo::Top] type indicates that the local variable has the verification type `top`.
	Top,
	/// The [VerificationTypeInfo::Integer] type indicates that the location contains the verification type `int`.
	Integer,
	/// The [VerificationTypeInfo::Float] type indicates that the location contains the verification type `float`.
	Float,
	/// The [VerificationTypeInfo::Long] type indicates that the location contains the verification type `long`.
	/// This structure gives the contents of two locations in the operand stack or in the local variable array.
	///
	/// If the location is a local variable, then:
	/// - It must not be the local variable with the highest index.
	/// - The next higher numbered local variable contains the verification type `top`.
	///
	/// If the location is an operand stack entry, then:
	/// - The current location must not be the topmost location of the operand stack.
	/// - The next location closer to the top of the operand stack contains the verification type `top`.
	Long,
	/// The [VerificationTypeInfo::Double] type indicates that the location contains the verification type `double`.
	/// This structure gives the contents of two locations in the operand stack or in the local variable array.
	///
	/// If the location is a local variable, then:
	/// - It must not be the local variable with the highest index.
	/// - The next higher numbered local variable contains the verification type `top`.
	///
	/// If the location is an operand stack entry, then:
	/// - The current location must not be the topmost location of the operand stack.
	/// - The next location closer to the top of the operand stack contains the verification type `top`.
	Double,
	/// The [VerificationTypeInfo::Null] type indicates that location contains the verification type `null`.
	Null,
	/// The [VerificationTypeInfo::UninitializedThis] type indicates that the location contains the verification type `uninitializedThis`.
	UninitializedThis,
	/// The [VerificationTypeInfo::Object] type indicates that the location contains an instance of the class represented by the [ClassInfo] structure found in
	/// the [ConstantPool] table at the index given by `cpool_index`.
	Object(ClassInfo),
	/// The [VerificationTypeInfo::Uninitialized] type indicates that the location contains the verification type `uninitialized(offset)`.
	Uninitialized {
		///  The `bytecode_offset` item indicates the offset, in the code array of the [CodeAttribute] that contains this [StackMapTableAttribute], of the
		/// [Opcode::New] instruction that created the object being stored in the location.
		bytecode_offset: usize,
	},
}

impl VerificationTypeInfo {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<VerificationTypeInfo, ClassFileParseError> {
		match parse_u1(reader)? {
			0 => Ok(Self::Top),
			1 => Ok(Self::Integer),
			2 => Ok(Self::Float),
			3 => Ok(Self::Double),
			4 => Ok(Self::Long),
			5 => Ok(Self::Null),
			6 => Ok(Self::UninitializedThis),
			7 => Ok(Self::Object(constant_pool.parse_index(reader)?)),
			8 => Ok(Self::Uninitialized {
				bytecode_offset: parse_u2_as_usize(reader)?,
			}),
			tag => Err(ClassFileParseError::UnknownVerificationTypeInfoTag(tag)),
		}
	}

	fn map_pc(mut self, pc_map: &PcMap) -> Result<VerificationTypeInfo, ClassFileParseError> {
		Ok(match self {
			Self::Uninitialized { bytecode_offset } => Self::Uninitialized {
				bytecode_offset: pc_map.map(bytecode_offset)?
			},
			verification_type_info => verification_type_info,
		})
	}
}

/// Each [StackMapFrame] structure specifies the type state at a particular bytecode offset. Each frame type specifies (explicitly or implicitly) a value,
/// `offset_delta`, that is used to calculate the actual bytecode offset at which a frame applies. The bytecode offset at which a frame applies is calculated
/// by adding `offset_delta + 1` to the bytecode offset of the previous frame, unless the previous frame is the initial frame of the method, in which case the
/// bytecode offset is `offset_delta`.
///
/// By using an offset delta rather than the actual bytecode offset we ensure, by definition, that stack map frames are in the correctly sorted order.
/// Furthermore, by consistently using the formula `offset_delta + 1` for all explicit frames, we guarantee the absence of duplicates.
///
/// We say that an instruction in the bytecode has a corresponding stack map frame if the instruction starts at offset `i` in the code array of a
/// [CodeAttribute] attribute, and the [CodeAttribute] attribute has a [StackMapTableAttribute] attribute whose entries array has a [StackMapFrame] structure
/// that applies at bytecode offset `i`.
///
/// The [StackMapFrame] structure consists of a one-byte tag followed by zero or more bytes, giving more information, depending upon the tag.
///
/// All frame types, even [StackMapFrame::Full], rely on the previous frame for some of their semantics. This raises the question of what is the very first
/// frame? The initial frame is implicit, and computed from the method descriptor.
///
/// Tags in the range `128..=246` are reserved for future use.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackMapFrame {
	/// If the frame type is [StackMapFrame::Same], it means the frame has exactly the same locals as the previous stack map frame and that the number of
	/// stack items is zero.
	Same {
		bytecode_offset: usize,
	},
	/// If the `frame_type` is [StackMapFrame::SameLocals1StackItem], it means the frame has exactly the same locals as the previous stack map frame and that the
	/// number of stack items is 1. There is a [VerificationTypeInfo] following the `frame_type` for the one stack item.
	SameLocals1StackItem {
		bytecode_offset: usize,
		stack: VerificationTypeInfo,
	},
	/// If the `frame_type` is [StackMapFrame::Chop], it means that the operand stack is empty and the current locals are the same as the locals in the
	/// previous frame, except that the `k` last locals are absent. The value of `k` is given by the formula `251 - frame_type`.
	Chop {
		bytecode_offset: usize,
		k: u8,
	},
	/// If the `frame_type` is [StackMapFrame::Append], it means that the operand stack is empty and the current locals are the same as the locals in the
	/// previous frame, except that `k` additional locals are defined. The value of `k` is given by the formula `frame_type - 251`.
	///
	/// The 0th entry in locals represents the type of the first additional local variable. If `locals[M]` represents local variable `N`, then `locals[M+1]`
	/// represents local variable `N+1` if `locals[M]` is one of:
	/// - [VerificationTypeInfo::Top]
	/// - [VerificationTypeInfo::Integer]
	/// - [VerificationTypeInfo::Float]
	/// - [VerificationTypeInfo::Null]
	/// - [VerificationTypeInfo::UninitializedThis]
	/// - [VerificationTypeInfo::Object]
	/// - [VerificationTypeInfo::Uninitialized]
	/// Otherwise `locals[M+1]` represents local variable `N+2`.
	///
	/// It is an error if, for any index `i`, `locals[i]` represents a local variable whose index is greater than the maximum number of local variables for the
	/// method.
	Append {
		bytecode_offset: usize,
		locals: Vec<VerificationTypeInfo>,
	},
	/// The 0th entry in locals represents the type of local variable 0. `If locals[M]` represents local variable `N`, then `locals[M+1]` represents local
	/// variable `N+1` if `locals[M]` is one of:
	/// - [VerificationTypeInfo::Top]
	/// - [VerificationTypeInfo::Integer]
	/// - [VerificationTypeInfo::Float]
	/// - [VerificationTypeInfo::Null]
	/// - [VerificationTypeInfo::UninitializedThis]
	/// - [VerificationTypeInfo::Object]
	/// - [VerificationTypeInfo::Uninitialized]
	/// Otherwise `locals[M+1]` represents local variable `N+2`.
	///
	/// It is an error if, for any index `i`, `locals[i]` represents a local variable whose index is greater than the maximum number of local variables for the
	/// method.
	///
	/// The 0th entry in stack represents the type of the bottom of the stack, and subsequent entries represent types of stack elements closer to the top of
	/// the operand stack. We shall refer to the bottom element of the stack as stack element 0, and to subsequent elements as stack element 1, 2 etc.
	/// If `stack[M]` represents stack element `N`, then `stack[M+1]` represents stack element `N+1` if `stack[M]` is one of:
	/// - [VerificationTypeInfo::Top]
	/// - [VerificationTypeInfo::Integer]
	/// - [VerificationTypeInfo::Float]
	/// - [VerificationTypeInfo::Null]
	/// - [VerificationTypeInfo::UninitializedThis]
	/// - [VerificationTypeInfo::Object]
	/// - [VerificationTypeInfo::Uninitialized]
	/// Otherwise, `stack[M+1]` represents stack element `N+2`.
	///
	/// It is an error if, for any index `i`, `stack[i]` represents a stack entry whose index is greater than the maximum operand stack size for the method.
	Full {
		bytecode_offset: usize,
		locals: Vec<VerificationTypeInfo>,
		stack: Vec<VerificationTypeInfo>,
	}
}

impl StackMapFrame {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool, last_bytecode_position: usize, is_first_explicit_frame: bool) ->
			Result<(StackMapFrame, usize), ClassFileParseError> {
		let frame_type = parse_u1(reader)?;
		
		let delta_to_position = |offset_delta| if is_first_explicit_frame {
			last_bytecode_position + offset_delta
		} else {
			last_bytecode_position + offset_delta + 1
		};

		match frame_type {
			offset_delta @ 0..=63 => {
				let bytecode_offset = delta_to_position(offset_delta as usize);
				Ok((Self::Same { bytecode_offset }, bytecode_offset))
			},
			frame_type @ 64..=127 => {
				let bytecode_offset = delta_to_position(frame_type as usize - 64);
				Ok((
					Self::SameLocals1StackItem {
						bytecode_offset,
						stack: VerificationTypeInfo::parse(reader, constant_pool)?,
					},
					bytecode_offset
				))
			},
			128..=246 => Err(ClassFileParseError::UnknownStackMapFrameType(frame_type)),
			247 => {
				let bytecode_offset = delta_to_position(parse_u2_as_usize(reader)?);
				Ok((
					Self::SameLocals1StackItem {
						bytecode_offset,
						stack: VerificationTypeInfo::parse(reader, constant_pool)?,
					},
					bytecode_offset
				))
			},
			frame_type @ 248..=250 => {
				let bytecode_offset = delta_to_position(parse_u2_as_usize(reader)?);
				Ok((
					Self::Chop {
						bytecode_offset,
						k: 251 - frame_type,
					},
					bytecode_offset
				))
			},
			251 => {
				let bytecode_offset = delta_to_position(parse_u2_as_usize(reader)?);
				Ok((Self::Same { bytecode_offset }, bytecode_offset))
			},
			frame_type @ 252..=254 => {
				let bytecode_offset = delta_to_position(parse_u2_as_usize(reader)?);
				Ok((
					Self::Append {
						bytecode_offset,
						locals: parse_vec(reader,
							|_| Ok::<usize, ClassFileParseError>((frame_type - 251) as usize),
							|r| VerificationTypeInfo::parse(r, constant_pool)
						)?,
					},
					bytecode_offset
				))
			},
			255 => {
				let bytecode_offset = delta_to_position(parse_u2_as_usize(reader)?);
				Ok((
					Self::Full {
						bytecode_offset,
						locals: parse_vec(reader,
							parse_u2_as_usize,
								|r| VerificationTypeInfo::parse(r, constant_pool)
						)?,
						stack: parse_vec(reader,
							parse_u2_as_usize,
							|r| VerificationTypeInfo::parse(r, constant_pool)
						)?,
					},
					bytecode_offset
				))
			},
		}
	}

	fn map_pc(mut self, pc_map: &PcMap) -> Result<StackMapFrame, ClassFileParseError> {
		Ok(match self {
			Self::Same { bytecode_offset } => Self::Same {
				bytecode_offset: pc_map.map(bytecode_offset)?,
			},
			Self::SameLocals1StackItem { bytecode_offset, stack } => Self::SameLocals1StackItem {
				bytecode_offset: pc_map.map(bytecode_offset)?,
				stack: stack.map_pc(pc_map)?,
			},
			Self::Chop { bytecode_offset, k } => Self::Chop {
				bytecode_offset: pc_map.map(bytecode_offset)?,
				k,
			},
			Self::Append { bytecode_offset, locals } => Self::Append {
				bytecode_offset: pc_map.map(bytecode_offset)?,
				locals: locals.into_iter()
					.map(|verification_type_info| verification_type_info.map_pc(pc_map))
					.collect::<Result<Vec<_>, _>>()?,
			},
			Self::Full { bytecode_offset, locals, stack } => Self::Full {
				bytecode_offset: pc_map.map(bytecode_offset)?,
				locals: locals.into_iter()
					.map(|verification_type_info| verification_type_info.map_pc(pc_map))
					.collect::<Result<Vec<_>, _>>()?,
				stack: stack.into_iter()
					.map(|verification_type_info| verification_type_info.map_pc(pc_map))
					.collect::<Result<Vec<_>, _>>()?,
			}
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExceptionsAttribute { // 4.7.5
	exception_table: Vec<ClassInfo>,
}
impl ExceptionsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<ExceptionsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;

		Ok(ExceptionsAttribute {
			exception_table: parse_vec(reader,
				parse_u2_as_usize,
				|r| constant_pool.parse_index(r)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InnerClassesAttribute { // 4.7.6
	classes: Vec<InnerClassesAttributeClassesElement>,
}
impl InnerClassesAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<InnerClassesAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;

		Ok(InnerClassesAttribute {
			classes: parse_vec(reader,
				parse_u2_as_usize,
				|r| InnerClassesAttributeClassesElement::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct InnerClassesAttributeClassesElement { // 4.7.6, classes
	inner_class: ClassInfo,
	outer_class: Option<ClassInfo>,
	inner_name: Option<ClassInfo>,
	inner_class_access_flags: u16,
}
impl InnerClassesAttributeClassesElement {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<InnerClassesAttributeClassesElement, ClassFileParseError> {
		Ok(InnerClassesAttributeClassesElement {
			inner_class: constant_pool.parse_index(reader)?,
			outer_class: constant_pool.parse_index_optional(reader)?,
			inner_name: constant_pool.parse_index_optional(reader)?,
			inner_class_access_flags: parse_u2(reader)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnclosingMethodAttribute { // 4.7.7
	class: ClassInfo,
	method: NameAndTypeInfo,
}
impl EnclosingMethodAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<EnclosingMethodAttribute, ClassFileParseError> {
		check_attribute_length(reader, 4)?;
		Ok(EnclosingMethodAttribute {
			class: constant_pool.parse_index(reader)?,
			method: constant_pool.parse_index(reader)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntheticAttribute {} // 4.7.6
impl SyntheticAttribute {
	fn parse<R: Read>(reader: &mut R) -> Result<SyntheticAttribute, ClassFileParseError> {
		check_attribute_length(reader, 0)?;
		Ok(SyntheticAttribute {})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureAttribute { // 4.7.9
	signature: Utf8Info,
}
impl SignatureAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<SignatureAttribute, ClassFileParseError> {
		check_attribute_length(reader, 2)?;
		Ok(SignatureAttribute {
			signature: constant_pool.parse_index(reader)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFileAttribute { // 4.7.10
	sourcefile: Utf8Info,
}
impl SourceFileAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<SourceFileAttribute, ClassFileParseError> {
		check_attribute_length(reader, 2)?;
		Ok(SourceFileAttribute {
			sourcefile: constant_pool.parse_index(reader)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceDebugExtensionAttribute { // 4.7.11
	debug_extension: Vec<u8>,
}
impl SourceDebugExtensionAttribute {
	fn parse<R: Read>(reader: &mut R) -> Result<SourceDebugExtensionAttribute, ClassFileParseError> {
		Ok(SourceDebugExtensionAttribute {
			debug_extension: parse_vec(reader,
				parse_u4_as_usize,
				parse_u1
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineNumberTableAttribute { // 4.7.12
	line_number_table: Vec<LineNumberTableEntry>,
}
impl LineNumberTableAttribute {
	fn parse<R: Read>(reader: &mut R) -> Result<LineNumberTableAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(LineNumberTableAttribute {
			line_number_table: parse_vec(reader,
				parse_u2_as_usize,
				|r| LineNumberTableEntry::parse(r)
			)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LineNumberTableAttribute, ClassFileParseError> {
		Ok(LineNumberTableAttribute {
			line_number_table: self.line_number_table.into_iter()
				.map(|e| e.map_pc(pc_map))
				.collect::<Result<Vec<_>, _>>()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineNumberTableEntry { // 4.7.12, line_number_table
	start_pc: usize,
	line_number: u16,
}
impl LineNumberTableEntry {
	fn parse<R: Read>(reader: &mut R) -> Result<LineNumberTableEntry, ClassFileParseError> {
		Ok(LineNumberTableEntry {
			start_pc: parse_u2_as_usize(reader)?,
			line_number: parse_u2(reader)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LineNumberTableEntry, ClassFileParseError> {
		Ok(LineNumberTableEntry {
			start_pc: pc_map.map(self.start_pc)?,
			line_number: self.line_number,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTableAttribute { // 4.7.13
	local_variable_table: Vec<LocalVariableTableEntry>,
}
impl LocalVariableTableAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTableAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(LocalVariableTableAttribute {
			local_variable_table: parse_vec(reader,
				parse_u2_as_usize,
				|r| LocalVariableTableEntry::parse(r, constant_pool)
			)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LocalVariableTableAttribute, ClassFileParseError> {
		Ok(LocalVariableTableAttribute {
			local_variable_table: self.local_variable_table.into_iter()
				.map(|e| e.map_pc(pc_map))
				.collect::<Result<Vec<_>, _>>()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTableEntry { // 4.7.13, local_variable_table
	start_pc: usize,
	end_pc: usize,
	name: Utf8Info,
	descriptor: Utf8Info,
	lv_index: u16,
}
impl LocalVariableTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTableEntry, ClassFileParseError> {
		let start_pc = parse_u2_as_usize(reader)?;
		let end_pc = start_pc + parse_u2_as_usize(reader)?;
		Ok(LocalVariableTableEntry {
			start_pc, end_pc,
			name: constant_pool.parse_index(reader)?,
			descriptor: constant_pool.parse_index(reader)?,
			lv_index: parse_u2(reader)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LocalVariableTableEntry, ClassFileParseError> {
		Ok(LocalVariableTableEntry {
			start_pc: pc_map.map(self.start_pc)?,
			end_pc: pc_map.map(self.end_pc)?,
			..self
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTypeTableAttribute { // 4.7.14
	local_variable_type_table: Vec<LocalVariableTypeTableEntry>,
}
impl LocalVariableTypeTableAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTypeTableAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(LocalVariableTypeTableAttribute {
			local_variable_type_table: parse_vec(reader,
				parse_u2_as_usize,
				|r| LocalVariableTypeTableEntry::parse(r, constant_pool)
			)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LocalVariableTypeTableAttribute, ClassFileParseError> {
		Ok(LocalVariableTypeTableAttribute {
			local_variable_type_table: self.local_variable_type_table.into_iter()
				.map(|e| e.map_pc(pc_map))
				.collect::<Result<Vec<_>, _>>()?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTypeTableEntry { // 4.7.14, local_variable_type_table
	start_pc: usize,
	end_pc: usize,
	name: Utf8Info,
	signature: Utf8Info,
	lv_index: u16,
}
impl LocalVariableTypeTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTypeTableEntry, ClassFileParseError> {
		let start_pc = parse_u2_as_usize(reader)?;
		let end_pc = start_pc + parse_u2_as_usize(reader)?;
		Ok(LocalVariableTypeTableEntry {
			start_pc, end_pc,
			name: constant_pool.parse_index(reader)?,
			signature: constant_pool.parse_index(reader)?,
			lv_index: parse_u2(reader)?,
		})
	}

	fn map_pc(self, pc_map: &PcMap) -> Result<LocalVariableTypeTableEntry, ClassFileParseError> {
		Ok(LocalVariableTypeTableEntry {
			start_pc: pc_map.map(self.start_pc)?,
			end_pc: pc_map.map(self.end_pc)?,
			..self
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeprecatedAttribute {} // 4.7.15
impl DeprecatedAttribute {
	fn parse<R: Read>(reader: &mut R) -> Result<DeprecatedAttribute, ClassFileParseError> {
		check_attribute_length(reader, 0)?;
		Ok(DeprecatedAttribute {})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeVisibleAnnotationsAttribute { // 4.7.16
	annotations: Vec<Annotation>,
}
impl RuntimeVisibleAnnotationsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<RuntimeVisibleAnnotationsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(RuntimeVisibleAnnotationsAttribute {
			annotations: parse_vec(reader,
				parse_u2_as_usize,
				|r| Annotation::parse(r, constant_pool),
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation { // 4.7.16, annotations
	annotation_type: Utf8Info,
	element_value_pairs: Vec<AnnotationElementValuePair>,
}
impl Annotation {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Annotation, ClassFileParseError> {
		Ok(Annotation {
			annotation_type: constant_pool.parse_index(reader)?,
			element_value_pairs: parse_vec(reader,
				parse_u2_as_usize,
				|r| AnnotationElementValuePair::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct AnnotationElementValuePair { // 4.7.16, element_value_pairs
	element_name: Utf8Info,
	value: AnnotationElementValue,
}
impl AnnotationElementValuePair {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<AnnotationElementValuePair, ClassFileParseError> {
		Ok(AnnotationElementValuePair {
			element_name: constant_pool.parse_index(reader)?,
			value: AnnotationElementValue::parse(reader, constant_pool)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationElementValue { // 4.7.16.1, value
	// TODO: this should also be split up, and use the constant_pool::* structures instead of index
	ConstantValueIndex { // B, C, D, F, I, J, S, Z, s
		const_value_index: u16,
	},
	EnumConstValue { // e
		type_name_index: u16,
		const_name_index: u16,
	},
	ClassInfoIndex { // c
		class_info_index: u16,
	},
	AnnotationValue { // @
		annotation_value: Annotation,
	},
	ArrayValue { // [
		values: Vec<AnnotationElementValue>,
	}
}
impl AnnotationElementValue {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> where Self: Sized {
		let tag = parse_u1(reader)?;

		let tag = tag.try_into().unwrap(); // TODO: fix this, this should be handled gracefully!

		Ok(match tag {
			'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' => Self::ConstantValueIndex {
				const_value_index: parse_u2(reader)?,
			},
			'e' => Self::EnumConstValue {
				type_name_index: parse_u2(reader)?,
				const_name_index: parse_u2(reader)?,
			},
			'c' => Self::ClassInfoIndex {
				class_info_index: parse_u2(reader)?,
			},
			'@' => Self::AnnotationValue {
				annotation_value: Annotation::parse(reader, constant_pool)?,
			},
			'[' => {
				Self::ArrayValue {
					values: parse_vec(reader,
						parse_u2_as_usize,
						|r| AnnotationElementValue::parse(r, constant_pool)
					)?,
				}
			},
			_ => Err(ClassFileParseError::UnknownAnnotationElementValueTag(tag as u8))?
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeInvisibleAnnotationsAttribute { // 4.7.17
	annotations: Vec<Annotation>,
}
impl RuntimeInvisibleAnnotationsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<RuntimeInvisibleAnnotationsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(RuntimeInvisibleAnnotationsAttribute {
			annotations: parse_vec(reader,
				parse_u2_as_usize,
				|r| Annotation::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeVisibleParameterAnnotationsAttribute { // 4.7.18
	parameter_annotations: Vec<ParameterAnnotationPair>,
}
impl RuntimeVisibleParameterAnnotationsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<RuntimeVisibleParameterAnnotationsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(RuntimeVisibleParameterAnnotationsAttribute {
			parameter_annotations: parse_vec(reader,
				parse_u1_as_usize,
				|r| ParameterAnnotationPair::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterAnnotationPair { // 4.7.18, parameter_annotations
	annotations: Vec<Annotation>,
}
impl ParameterAnnotationPair {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<ParameterAnnotationPair, ClassFileParseError> {
		Ok(ParameterAnnotationPair {
			annotations: parse_vec(reader,
				parse_u2_as_usize,
				|r| Annotation::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeInvisibleParameterAnnotationsAttribute { // 4.7.19
	parameter_annotations: Vec<ParameterAnnotationPair>,
}
impl RuntimeInvisibleParameterAnnotationsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<RuntimeInvisibleParameterAnnotationsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(RuntimeInvisibleParameterAnnotationsAttribute {
			parameter_annotations: parse_vec(reader,
		        parse_u1_as_usize,
		        |r| ParameterAnnotationPair::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationDefaultAttribute { // 4.7.20
	default_value: AnnotationElementValue,
}
impl AnnotationDefaultAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<AnnotationDefaultAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(AnnotationDefaultAttribute {
			default_value: AnnotationElementValue::parse(reader, constant_pool)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BootstrapMethodsAttribute { // 4.7.21
	bootstrap_methods: Vec<BootstrapMethodsAttributeEntry>,
}
impl BootstrapMethodsAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<BootstrapMethodsAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(BootstrapMethodsAttribute {
			bootstrap_methods: parse_vec(reader,
				parse_u2_as_usize,
				|r| BootstrapMethodsAttributeEntry::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BootstrapMethodsAttributeEntry { // 4.7.21, bootstrap_methods
	boostrap_method: MethodHandleInfo,
	bootstrap_arguments: Vec<BootstrapMethodArgument>,
}
impl BootstrapMethodsAttributeEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<BootstrapMethodsAttributeEntry, ClassFileParseError> {
		Ok(BootstrapMethodsAttributeEntry {
			boostrap_method: constant_pool.parse_index(reader)?,
			bootstrap_arguments: parse_vec(reader,
				parse_u2_as_usize,
				|r| BootstrapMethodArgument::parse(r, constant_pool)
			)?
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BootstrapMethodArgument {
	String(StringInfo),
	Class(ClassInfo),
	Integer(IntegerInfo),
	Long(LongInfo),
	Float(FloatInfo),
	Double(DoubleInfo),
	MethodHandle(MethodHandleInfo),
	MethodType(MethodTypeInfo),
}
impl BootstrapMethodArgument {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<BootstrapMethodArgument, ClassFileParseError> {
		match constant_pool.parse_index_get(reader)? {
			ConstantPoolElement::String(string) => Ok(Self::String(string.clone())),
			ConstantPoolElement::Class(class) => Ok(Self::Class(class.clone())),
			ConstantPoolElement::Integer(integer) => Ok(Self::Integer(integer.clone())),
			ConstantPoolElement::Long(long) => Ok(Self::Long(long.clone())),
			ConstantPoolElement::Float(float) => Ok(Self::Float(float.clone())),
			ConstantPoolElement::Double(double) => Ok(Self::Double(double.clone())),
			ConstantPoolElement::MethodHandle(method_handle) => Ok(Self::MethodHandle(method_handle.clone())),
			ConstantPoolElement::MethodType(method_type) => Ok(Self::MethodType(method_type.clone())),
			tag => Err(ClassFileParseError::ConstantPoolTagMismatchError(ConstantPoolTagMismatchError {
				expected: "Long/Float/Double/Integer".to_string(),
				actual: format!("{tag:?}"),
			})),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodParametersAttribute {
	parameters: Vec<MethodParameterEntry>,
}

impl MethodParametersAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<MethodParametersAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(MethodParametersAttribute {
			parameters: parse_vec(reader,
				parse_u1_as_usize,
				|r| MethodParameterEntry::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodParameterEntry {
	name: Option<Utf8Info>,
	access_flags: MethodParameterAccessFlags,
}

impl MethodParameterEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<MethodParameterEntry, ClassFileParseError> {
		Ok(MethodParameterEntry {
			name: constant_pool.parse_index_optional(reader)?,
			access_flags: MethodParameterAccessFlags::parse(parse_u2(reader)?)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodParameterAccessFlags {
	pub is_final: bool,
	pub is_synthetic: bool,
	pub is_mandated: bool,
}

impl MethodParameterAccessFlags {
	fn parse(access_flags: u16) -> Result<MethodParameterAccessFlags, ClassFileParseError> {
		let is_final     = access_flags & 0x0010 != 0;
		let is_synthetic = access_flags & 0x1000 != 0;
		let is_mandated  = access_flags & 0x8000 != 0;
		// other bits are reserved for future use

		Ok(MethodParameterAccessFlags {
			is_final, is_synthetic, is_mandated,
		})
	}
}

macro_rules! try_from_enum_impl {
	($enum_type:ty, $pattern:path, $inner_type:ty) => {
		impl TryFrom<$enum_type> for $inner_type {
			type Error = AttributeTagMismatchError;
			fn try_from(value: $enum_type) -> Result<Self, Self::Error> {
				match value {
					$pattern(value) => Ok(value),
					v => Err(AttributeTagMismatchError {
						expected: stringify!($pattern).to_string(),
						actual: format!("{:?}", v),
					}),
				}
			}
		}
	}
}

try_from_enum_impl!(AttributeInfo, AttributeInfo::ConstantValue, ConstantValueAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::Code, CodeAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::StackMapTable, StackMapTableAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::Exceptions, ExceptionsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::InnerClasses, InnerClassesAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::EnclosingMethod, EnclosingMethodAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::Synthetic, SyntheticAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::Signature, SignatureAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::SourceFile, SourceFileAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::SourceDebugExtension, SourceDebugExtensionAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::LineNumberTable, LineNumberTableAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::LocalVariableTable, LocalVariableTableAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::LocalVariableTypeTable, LocalVariableTypeTableAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::Deprecated, DeprecatedAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::RuntimeVisibleAnnotations, RuntimeVisibleAnnotationsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::RuntimeInvisibleAnnotations, RuntimeInvisibleAnnotationsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::RuntimeVisibleParameterAnnotations, RuntimeVisibleParameterAnnotationsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::RuntimeInvisibleParameterAnnotations, RuntimeInvisibleParameterAnnotationsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::AnnotationDefault, AnnotationDefaultAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::BootstrapMethods, BootstrapMethodsAttribute);
try_from_enum_impl!(AttributeInfo, AttributeInfo::MethodParameters, MethodParametersAttribute);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeInfo { // 4.7
	ConstantValue(ConstantValueAttribute), // 1.0.2, 45.3
	Code(CodeAttribute), // 1.0.2, 45.3
	StackMapTable(StackMapTableAttribute), // 6, 50.0
	Exceptions(ExceptionsAttribute), // 1.0.2, 45.3
	InnerClasses(InnerClassesAttribute), // 1.1, 45.3
	EnclosingMethod(EnclosingMethodAttribute), // 5.0, 49.0
	Synthetic(SyntheticAttribute), // 1.1, 45.3
	Signature(SignatureAttribute), // 5.0, 49.0
	SourceFile(SourceFileAttribute), // 1.0.2, 45.3
	SourceDebugExtension(SourceDebugExtensionAttribute), // 5.0, 49.0
	LineNumberTable(LineNumberTableAttribute), // 1.0.2, 45.3
	LocalVariableTable(LocalVariableTableAttribute), // 1.0.2, 45.3
	LocalVariableTypeTable(LocalVariableTypeTableAttribute), // 5.0, 49.0
	Deprecated(DeprecatedAttribute), // 1.1, 45.3
	RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute), // 5.0, 49.0
	RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute), // 5.0, 49.0
	RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute), // 5.0, 49.0
	RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute), // 5.0, 49.0
	AnnotationDefault(AnnotationDefaultAttribute), // 5.0, 49.0
	BootstrapMethods(BootstrapMethodsAttribute), // 7, 51.0
    MethodParameters(MethodParametersAttribute), // 8, 52.0
	Unknown {
		name: Utf8Info,
		info: Vec<u8>,
	},
}

impl AttributeInfo {
	pub fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		let name: Utf8Info = constant_pool.parse_index(reader)?;
		Ok(match name.bytes.as_slice() {
			b"ConstantValue" => Self::ConstantValue(ConstantValueAttribute::parse(reader, constant_pool)?),
			b"Code" => Self::Code(CodeAttribute::parse(reader, constant_pool)?),
			b"StackMapTable" => Self::StackMapTable(StackMapTableAttribute::parse(reader, constant_pool)?),
			b"Exceptions" => Self::Exceptions(ExceptionsAttribute::parse(reader, constant_pool)?),
			b"InnerClasses" => Self::InnerClasses(InnerClassesAttribute::parse(reader, constant_pool)?),
			b"EnclosingMethod" => Self::EnclosingMethod(EnclosingMethodAttribute::parse(reader, constant_pool)?),
			b"Synthetic" => Self::Synthetic(SyntheticAttribute::parse(reader)?),
			b"Signature" => Self::Signature(SignatureAttribute::parse(reader, constant_pool)?),
			b"SourceFile" => Self::SourceFile(SourceFileAttribute::parse(reader, constant_pool)?),
			b"SourceDebugExtension" => Self::SourceDebugExtension(SourceDebugExtensionAttribute::parse(reader)?),
			b"LineNumberTable" => Self::LineNumberTable(LineNumberTableAttribute::parse(reader)?),
			b"LocalVariableTable" => Self::LocalVariableTable(LocalVariableTableAttribute::parse(reader, constant_pool)?),
			b"LocalVariableTypeTable" => Self::LocalVariableTypeTable(LocalVariableTypeTableAttribute::parse(reader, constant_pool)?),
			b"Deprecated" => Self::Deprecated(DeprecatedAttribute::parse(reader)?),
			b"RuntimeVisibleAnnotations" => Self::RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute::parse(reader, constant_pool)?),
			b"RuntimeInvisibleAnnotations" => Self::RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute::parse(reader, constant_pool)?),
			b"RuntimeVisibleParameterAnnotations" => Self::RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute::parse(reader, constant_pool)?),
			b"RuntimeInvisibleParameterAnnotations" => Self::RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute::parse(reader, constant_pool)?),
			b"AnnotationDefault" => Self::AnnotationDefault(AnnotationDefaultAttribute::parse(reader, constant_pool)?),
			b"BootstrapMethods" => Self::BootstrapMethods(BootstrapMethodsAttribute::parse(reader, constant_pool)?),
			b"MethodParameters" => Self::MethodParameters(MethodParametersAttribute::parse(reader, constant_pool)?),
			_ => {
				let info = parse_vec(reader,
					parse_u4_as_usize,
					parse_u1
				)?;
				eprintln!("WARN: unknown attr: {name}: {info:?}"); // TODO: print?
				Self::Unknown { name, info }
			},
		})
	}
}
