use std::io::Read;
use itertools::{Either, Itertools};
use crate::class_instance::ClassData;
use crate::classfile::{ClassInfo, ConstantPool, ConstantPoolElement, DoubleInfo, FloatInfo, IntegerInfo, LongInfo, MethodHandleInfo, MethodTypeInfo, NameAndTypeInfo, parse_u1, parse_u1_as_usize, parse_u2, parse_u2_as_usize, parse_u4, parse_u4_as_usize, parse_vec, StringInfo, Utf8Info};
use crate::code::Code;
use crate::errors::{ClassFileParseError, ConstantPoolTagMismatchError, OutOfBoundsError};

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
			tag => Err(ClassFileParseError::InvalidConstantPoolTag(ConstantPoolTagMismatchError {
				expected: "Long/Float/Double/Integer".to_string(),
				actual: format!("{tag:?}"),
				msg: "".to_string(),
			})),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeAttribute { // 4.7.3
	pub max_stack: u16,
	pub max_locals: u16,
	pub code_: Code,
	pub code: Vec<u8>,
	pub exception_table: Vec<ExceptionTableEntry>,
	pub attributes: Vec<AttributeInfo>,

	pub line_number_table: Option<LineNumberTableAttribute>,
	pub local_variable_table: Option<LocalVariableTableAttribute>,
	pub local_variable_type_table: Option<LocalVariableTypeTableAttribute>,
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

		let exception_table = parse_vec(reader,
			parse_u2_as_usize,
			|r| ExceptionTableEntry::parse(r, constant_pool)
		)?;

		let mut attributes = parse_vec(reader,
		   parse_u2_as_usize,
		   |r| AttributeInfo::parse(r, constant_pool)
		)?;

		// LineNumberTableAttribute
		let (line_number_tables, attributes): (Vec<LineNumberTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::LineNumberTable(line_number_table) => Either::Left(line_number_table),
				other => Either::Right(other),
			});
		let line_number_table = line_number_tables.into_iter().next();

		// LocalVariableTableAttribute
		let (local_variable_tables, attributes): (Vec<LocalVariableTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::LocalVariableTable(local_variable_table) => Either::Left(local_variable_table),
				other => Either::Right(other),
			});
		let local_variable_table = local_variable_tables.into_iter().next();

		// LocalVariableTypeTableAttribute
		let (local_variable_type_tables, attributes): (Vec<LocalVariableTypeTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::LocalVariableTypeTable(local_variable_type_table) => Either::Left(local_variable_type_table),
				other => Either::Right(other),
			});
		let local_variable_type_table = local_variable_type_tables.into_iter().next();

		// StackMapTableAttribute
		let (stack_map_tables, attributes): (Vec<StackMapTableAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::StackMapTable(stack_map_table) => Either::Left(stack_map_table),
				other => Either::Right(other),
			});

		// ignore any, but the first StackMapTableAttribute
		let stack_map_table = stack_map_tables.into_iter()
			.next()
			.unwrap_or_else(|| StackMapTableAttribute {
				entries: Vec::new(),
			});

		Ok(CodeAttribute {
			max_stack,
			max_locals,
			code_: match Code::parse(code.clone()) {
				Ok(e) => e,
				_ => panic!(),
			},
			code,
			exception_table,
			attributes,
			line_number_table,
			local_variable_table,
			local_variable_type_table,
			stack_map_table,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExceptionTableEntry { // 4.7.3, exception_table
	start_pc: u16,
	end_pc: u16,
	handler_pc: u16,
	catch_type: Option<ClassInfo>,
}
impl ExceptionTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<ExceptionTableEntry, ClassFileParseError> {
		Ok(ExceptionTableEntry {
			start_pc: parse_u2(reader)?,
			end_pc: parse_u2(reader)?,
			handler_pc: parse_u2(reader)?,
			catch_type: constant_pool.parse_index_optional(reader)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackMapTableAttribute { // 4.7.4
	entries: Vec<StackMapFrame>,
}
impl StackMapTableAttribute {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<StackMapTableAttribute, ClassFileParseError> {
		let _attribute_length = parse_u4(reader)?;
		Ok(StackMapTableAttribute {
			entries: parse_vec(reader,
				parse_u2_as_usize,
				|r| StackMapFrame::parse(r, constant_pool)
			)?,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationTypeInfo {
	Top, Integer, Float, Long, Double, Null, UninitializedThis,
	Object(ClassInfo),
	Uninitialized {
		offset: u16,
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
				offset: parse_u2(reader)?,
			}),
			tag => Err(ClassFileParseError::UnknownVerificationTypeInfoTag(tag)),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackMapFrame {
	Same {
		offset_delta: u16,
	},
	SameLocals1StackItem {
		offset_delta: u16,
		stack: VerificationTypeInfo,
	},
	Chop {
		offset_delta: u16,
		k: u8,
	},
	Append {
		offset_delta: u16,
		locals: Vec<VerificationTypeInfo>,
	},
	Full {
		offset_delta: u16,
		locals: Vec<VerificationTypeInfo>,
		stack: Vec<VerificationTypeInfo>,
	}
}

impl StackMapFrame {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<StackMapFrame, ClassFileParseError> {
		let frame_type = parse_u1(reader)?;

		match frame_type {
			offset_delta @ 0..=63 => Ok(Self::Same {
				offset_delta: offset_delta as u16,
			}),
			frame_type @ 64..=127 => Ok(Self::SameLocals1StackItem {
				offset_delta: frame_type as u16 - 64,
				stack: VerificationTypeInfo::parse(reader, constant_pool)?,
			}),
			128..=246 => Err(ClassFileParseError::UnknownStackMapFrameType(frame_type)),
			247 => Ok(Self::SameLocals1StackItem {
				offset_delta: parse_u2(reader)?,
				stack: VerificationTypeInfo::parse(reader, constant_pool)?,
			}),
			frame_type @ 248..=250 => Ok(Self::Chop {
				offset_delta: parse_u2(reader)?,
				k: 251 - frame_type,
			}),
			251 => Ok(Self::Same {
				offset_delta: parse_u2(reader)?,
			}),
			frame_type @ 252..=254 => Ok(Self::Append {
				offset_delta: parse_u2(reader)?,
				locals: parse_vec(reader,
					|_| Ok((frame_type - 251) as usize),
					|r| VerificationTypeInfo::parse(r, constant_pool)
				)?,
			}),
			255 => Ok(Self::Full {
				offset_delta: parse_u2(reader)?,
				locals: parse_vec(reader,
					parse_u2_as_usize,
					|r| VerificationTypeInfo::parse(r, constant_pool)
				)?,
				stack: parse_vec(reader,
					parse_u2_as_usize,
					|r| VerificationTypeInfo::parse(r, constant_pool)
				)?,
			}),
		}
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
pub struct SourceDebugExtensionsAttribute { // 4.7.11
	debug_extension: Vec<u8>,
}
impl SourceDebugExtensionsAttribute {
	fn parse<R: Read>(reader: &mut R) -> Result<SourceDebugExtensionsAttribute, ClassFileParseError> {
		Ok(SourceDebugExtensionsAttribute {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LineNumberTableEntry { // 4.7.12, line_number_table
	start_pc: u16,
	line_number: u16,
}
impl LineNumberTableEntry {
	fn parse<R: Read>(reader: &mut R) -> Result<LineNumberTableEntry, ClassFileParseError> {
		Ok(LineNumberTableEntry {
			start_pc: parse_u2(reader)?,
			line_number: parse_u2(reader)?,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTableEntry { // 4.7.13, local_variable_table
	start_pc: u16,
	length: u16,
	name: Utf8Info,
	descriptor: Utf8Info,
	lv_index: u16,
}
impl LocalVariableTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTableEntry, ClassFileParseError> {
		Ok(LocalVariableTableEntry {
			start_pc: parse_u2(reader)?,
			length: parse_u2(reader)?,
			name: constant_pool.parse_index(reader)?,
			descriptor: constant_pool.parse_index(reader)?,
			lv_index: parse_u2(reader)?,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalVariableTypeTableEntry { // 4.7.14, local_variable_type_table
	start_pc: u16,
	length: u16,
	name: Utf8Info,
	signature: Utf8Info,
	lv_index: u16,
}
impl LocalVariableTypeTableEntry {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<LocalVariableTypeTableEntry, ClassFileParseError> {
		Ok(LocalVariableTypeTableEntry {
			start_pc: parse_u2(reader)?,
			length: parse_u2(reader)?,
			name: constant_pool.parse_index(reader)?,
			signature: constant_pool.parse_index(reader)?,
			lv_index: parse_u2(reader)?,
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
			tag => Err(ClassFileParseError::InvalidConstantPoolTag(ConstantPoolTagMismatchError {
				expected: "Long/Float/Double/Integer".to_string(),
				actual: format!("{tag:?}"),
				msg: "".to_string(),
			})),
		}
	}
}

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
	SourceDebugExtension(SourceDebugExtensionsAttribute), // 5.0, 49.0
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
	Unknown {
		name: &'static str,
		info: Vec<u8>,
	},
}

impl AttributeInfo {
	pub fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		let s: Utf8Info = constant_pool.parse_index(reader)?;

		Ok(match s.to_string().as_str() {
			"ConstantValue" => Self::ConstantValue(ConstantValueAttribute::parse(reader, constant_pool)?),
			"Code" => Self::Code(CodeAttribute::parse(reader, constant_pool)?),
			"StackMapTable" => Self::StackMapTable(StackMapTableAttribute::parse(reader, constant_pool)?),
			"Exceptions" => Self::Exceptions(ExceptionsAttribute::parse(reader, constant_pool)?),
			"InnerClasses" => Self::InnerClasses(InnerClassesAttribute::parse(reader, constant_pool)?),
			"EnclosingMethod" => Self::EnclosingMethod(EnclosingMethodAttribute::parse(reader, constant_pool)?),
			"Synthetic" => Self::Synthetic(SyntheticAttribute::parse(reader)?),
			"Signature" => Self::Signature(SignatureAttribute::parse(reader, constant_pool)?),
			"SourceFile" => Self::SourceFile(SourceFileAttribute::parse(reader, constant_pool)?),
			"SourceDebugExtension" => Self::SourceDebugExtension(SourceDebugExtensionsAttribute::parse(reader)?),
			"LineNumberTable" => Self::LineNumberTable(LineNumberTableAttribute::parse(reader)?),
			"LocalVariableTable" => Self::LocalVariableTable(LocalVariableTableAttribute::parse(reader, constant_pool)?),
			"LocalVariableTypeTable" => Self::LocalVariableTypeTable(LocalVariableTypeTableAttribute::parse(reader, constant_pool)?),
			"Deprecated" => Self::Deprecated(DeprecatedAttribute::parse(reader)?),
			"RuntimeVisibleAnnotations" => Self::RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute::parse(reader, constant_pool)?),
			"RuntimeInvisibleAnnotations" => Self::RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute::parse(reader, constant_pool)?),
			"RuntimeVisibleParameterAnnotations" => Self::RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute::parse(reader, constant_pool)?),
			"RuntimeInvisibleParameterAnnotations" => Self::RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute::parse(reader, constant_pool)?),
			"AnnotationDefault" => Self::AnnotationDefault(AnnotationDefaultAttribute::parse(reader, constant_pool)?),
			"BootstrapMethods" => Self::BootstrapMethods(BootstrapMethodsAttribute::parse(reader, constant_pool)?),
			name => {
				let info = parse_vec(reader,
					parse_u4_as_usize,
					parse_u1
				)?;
				eprintln!("WARN: unknown attr: {name}: {info:?}");
				// TODO: let it actually store the name :iea:
				Self::Unknown { name: "name", info }
			},
		})
	}
}
