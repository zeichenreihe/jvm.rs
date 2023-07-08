use std::fmt::{Debug, Formatter};
use std::io::Read;

use gen::declare_jvm_struct;
use crate::errors::{ClassFileParseError, ConstantPoolTagMismatchError};

macro_rules! gen_parse_u_int {
	($name:tt, $n:literal, $t:ty) => {
		fn $name<R: Read>(reader: &mut R) -> Result<$t, std::io::Error> {
			let mut buf = [0u8; $n];
			let length = reader.read(&mut buf)?;
			if length == $n {
				Ok(<$t>::from_be_bytes(buf))
			} else {
				Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof))
			}
		}
	}
}
gen_parse_u_int!(parse_u1, 1, u8);
gen_parse_u_int!(parse_u2, 2, u16);
gen_parse_u_int!(parse_u4, 4, u32);

/// First calls the `size` parameter to get the length of the data, then calls `element` so often to read the data, returning the data then. The argument
/// `reader` is given to both closures.
fn parse_vec<T, R: Read, E, SIZE, ELEMENT>(reader: &mut R, size: SIZE, element: ELEMENT) -> Result<Vec<T>, E>
	where
		SIZE: FnOnce(&mut R) -> Result<usize, E>,
		ELEMENT: Fn(&mut R) -> Result<T, E>
{
	let count = size(reader)?;
	let mut vec = Vec::with_capacity(count);
	for _ in 0..count {
		vec.push(element(reader)?);
	}
	Ok(vec)
}

pub trait Parse<R: Read> {
	#[deprecated]
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self:Sized;
}

pub trait ParseMulti<R: Read, T: Parse<R>> {
	#[deprecated]
	fn parse_multi(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>, count: usize) -> Result<Vec<T>, ClassFileParseError>;
}

impl <R: Read, T: Parse<R>> ParseMulti<R, T> for Vec<T> {
	#[allow(deprecated)]
	fn parse_multi(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>, count: usize) -> Result<Vec<T>, ClassFileParseError> {
		let mut vec = Vec::with_capacity(count);
		for _ in 0..count { vec.push(T::parse(reader, constant_pool)?); }
		Ok(vec)
	}
}

macro_rules! impl_parse_for {
	($t:ty, $n:literal) => {
		impl<R: Read> Parse<R> for $t {
			fn parse(reader: &mut R, _: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> {
				let mut buf = [0u8; $n];
				let length = reader.read(&mut buf)?;
				if length == $n {
					Ok(<$t>::from_be_bytes(buf))
				} else {
					Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof))?
				}
			}
		}
	}
}
impl_parse_for!(u8, 1);
impl_parse_for!(u16, 2);
impl_parse_for!(u32, 4);

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct JUtf8(pub Vec<u8>);

impl Debug for JUtf8 {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "Utf8 {{ \"{}\" }}", String::from_utf8_lossy(&self.0))
	}
}

impl From<Vec<u8>> for JUtf8 {
	fn from(value: Vec<u8>) -> Self {
		Self(value)
	}
}

impl From<&str> for JUtf8 {
	fn from(value: &str) -> Self {
		Self ( value.as_bytes().into() )
	}
}

impl JUtf8 {
	pub fn to_vec(&self) -> Vec<u8> {
		self.0.clone()
	}
}

impl PartialEq<str> for JUtf8 {
	fn eq(&self, other: &str) -> bool {
		self.0.eq(other.as_bytes())
	}
}
impl PartialEq<&str> for JUtf8 {
	fn eq(&self, other: &&str) -> bool {
		self.0.eq(other.as_bytes())
	}
}

macro_rules! try_from_enum_impl {
	($enum_type:ty, $pattern:path, $inner_type:ty, $error_type:ty) => {
		impl TryFrom<$enum_type> for $inner_type {
			type Error = $error_type;
			fn try_from(value: $enum_type) -> Result<Self, Self::Error> {
				match value {
					$pattern(value) => Ok(value),
					_ => Err(Self::Error {})
				}
			}
		}
	}
}

declare_jvm_struct!(
	struct CpInfoClass {
		u2 name_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::Class, CpInfoClass, ConstantPoolTagMismatchError);
impl CpInfoClass {
	pub fn name<'a>(&'a self, class_file: &'a ClassFile) -> Result<JUtf8, ClassFileParseError> {
		if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.name_index as usize)? {
			Ok(JUtf8(utf8.bytes.clone()))
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)
		}
	}
}

declare_jvm_struct!(
	struct CpInfoFieldRef {
		u2 class_index;
		u2 name_and_type_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::FieldRef, CpInfoFieldRef, ConstantPoolTagMismatchError);
impl CpInfoFieldRef {
	pub fn class_name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8, JUtf8), ClassFileParseError> {
		let class = if let CpInfo::Class(class) = class_file.get_constant_pool(self.class_index as usize)? {
			class.name(class_file)?
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		let (name, desc) = if let CpInfo::NameAndType(name_and_type) = class_file.get_constant_pool(self.name_and_type_index as usize)? {
			name_and_type.name_descriptor(class_file)?
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		Ok((class.clone(), name.clone(), desc.clone()))
	}
}

declare_jvm_struct!(
	struct CpInfoMethodRef {
		u2 class_index;
		u2 name_and_type_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::MethodRef, CpInfoMethodRef, ConstantPoolTagMismatchError);
impl CpInfoMethodRef {
	pub fn class_name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8, JUtf8), ClassFileParseError> {
		let class = if let CpInfo::Class(class) = class_file.get_constant_pool(self.class_index as usize)? {
			class.name(class_file)?
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		let (name, descriptor) = if let CpInfo::NameAndType(name_and_type) = class_file.get_constant_pool(self.name_and_type_index as usize)? {
			name_and_type.name_descriptor(class_file)?
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		Ok((class.clone(), name, descriptor))
	}
}

declare_jvm_struct!(
	struct CpInfoInterfaceMethodRef {
		u2 class_index;
		u2 name_and_type_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::InterfaceMethodRef, CpInfoInterfaceMethodRef, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoString {
		u2 string_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::String, CpInfoString, ConstantPoolTagMismatchError);

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoInteger {
	bytes: u32,
}
impl CpInfoInteger {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoInteger, ClassFileParseError> {
		Ok(Self {
			bytes: parse_u4(reader)?,
		})
	}
}
try_from_enum_impl!(CpInfo, CpInfo::Integer, CpInfoInteger, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoFloat {
		u4 bytes;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::Float, CpInfoFloat, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoLong {
		u4 high_bytes;
		u4 low_bytes;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::Long, CpInfoLong, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoDouble {
		u4 high_bytes;
		u4 low_bytes;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::Double, CpInfoDouble, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoNameAndType {
		u2 name_index;
		u2 descriptor_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::NameAndType, CpInfoNameAndType, ConstantPoolTagMismatchError);
impl CpInfoNameAndType {
	pub fn name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8), ClassFileParseError> {
		let name: JUtf8 = if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.name_index as usize)? {
			JUtf8(utf8.bytes.clone())
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		let descriptor: JUtf8 = if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.descriptor_index as usize)? {
			JUtf8(utf8.bytes.clone())
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)?
		};

		Ok((name.clone(), descriptor.clone()))
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct CpInfoUtf8 {
	bytes: Vec<u8>,
}
impl CpInfoUtf8 {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		Ok(Self {
			bytes: parse_vec(reader,
				|r| Ok(parse_u2(r)? as usize),
				|r| parse_u1(r)
			)?,
		})
	}
}
impl Debug for CpInfoUtf8 {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "CpInfoUtf8 {{ bytes: \"{}\" }}", String::from_utf8_lossy(&self.bytes))
	}
}
try_from_enum_impl!(CpInfo, CpInfo::Utf8, CpInfoUtf8, ConstantPoolTagMismatchError);

declare_jvm_struct!(
	struct CpInfoMethodHandle {
		u1 reference_kind;
		u2 reference_index;
	}
);
try_from_enum_impl!(CpInfo, CpInfo::MethodHandle, CpInfoMethodHandle, ConstantPoolTagMismatchError);

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoInvokeDynamic {
	bootstrap_method_attr_index: u16,
	name_and_type_index: u16,
}
impl CpInfoInvokeDynamic {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoInvokeDynamic, ClassFileParseError> {
		Ok(CpInfoInvokeDynamic {
			bootstrap_method_attr_index: parse_u2(reader)?,
			name_and_type_index: parse_u2(reader)?,
		})
	}
}
try_from_enum_impl!(CpInfo, CpInfo::InvokeDynamic, CpInfoInvokeDynamic, ConstantPoolTagMismatchError);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CpInfo { // 4.4, Table 4.3
	Class(CpInfoClass), FieldRef(CpInfoFieldRef), MethodRef(CpInfoMethodRef), InterfaceMethodRef(CpInfoInterfaceMethodRef),
	String(CpInfoString), Integer(CpInfoInteger), Float(CpInfoFloat), Long(CpInfoLong), Double(CpInfoDouble),
	NameAndType(CpInfoNameAndType), Utf8(CpInfoUtf8), MethodHandle(CpInfoMethodHandle), InvokeDynamic(CpInfoInvokeDynamic),
}
impl CpInfo {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		match parse_u1(reader)? {
			1  => Ok(Self::Utf8(CpInfoUtf8::parse(reader)?)),
			3  => Ok(Self::Integer(CpInfoInteger::parse(reader)?)),
			4  => Ok(Self::Float(CpInfoFloat::parse(reader, None)?)),
			5  => Ok(Self::Long(CpInfoLong::parse(reader, None)?)),
			6  => Ok(Self::Double(CpInfoDouble::parse(reader, None)?)),
			7  => Ok(Self::Class(CpInfoClass::parse(reader, None)?)),
			8  => Ok(Self::String(CpInfoString::parse(reader, None)?)),
			9  => Ok(Self::FieldRef(CpInfoFieldRef::parse(reader, None)?)),
			10 => Ok(Self::MethodRef(CpInfoMethodRef::parse(reader, None)?)),
			11 => Ok(Self::InterfaceMethodRef(CpInfoInterfaceMethodRef::parse(reader, None)?)),
			12 => Ok(Self::NameAndType(CpInfoNameAndType::parse(reader, None)?)),
			15 => Ok(Self::MethodHandle(CpInfoMethodHandle::parse(reader, None)?)),
			18 => Ok(Self::InvokeDynamic(CpInfoInvokeDynamic::parse(reader)?)),
			tag => Err(ClassFileParseError::UnknownConstantPoolTag(tag)),
		}
	}
}
impl CpInfo {
	#[deprecated]
	pub fn as_jutf8(&self) -> Result<JUtf8, ClassFileParseError> {
		if let Self::Utf8(utf8) = self {
			Ok(JUtf8(utf8.bytes.clone()))
		} else {
			Err(ClassFileParseError::WrongConstantPoolTag)
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
struct FieldInfo { // 4.5
	access_flags: u16,
	name: CpInfoUtf8,
	descriptor: CpInfoUtf8,
	attributes: Vec<AttributeInfo>,
}

impl FieldInfo {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		Ok(FieldInfo {
			access_flags: parse_u2(reader)?,
			name: constant_pool.parse_index(reader)?,
			descriptor: constant_pool.parse_index(reader)?,
			attributes: parse_vec(reader,
			    |r| Ok(parse_u2(r)? as usize),
			    |r| AttributeInfo::parse(r, constant_pool),
			)?,
		})
	}
}

declare_jvm_struct!( // 4.7.2
	struct ConstantValueAttribute {
		u4 attribute_length = 2;
		u2 constantvalue_index;
	}
);

declare_jvm_struct!( // 4.7.3
	struct CodeAttribute {
		u4 attribute_length;
		u2 max_stack;
		u2 max_locals;
		u4 code_length;
		u1 code[code_length];
		u2 exception_table_length;
		ExceptionTableEntry exception_table[exception_table_length];
		u2 attributes_count;
		AttributeInfo attributes[attributes_count];
	}
);

declare_jvm_struct!( // 4.7.3, exception_table
	struct ExceptionTableEntry {
		u2 start_pc;
		u2 end_pc;
		u2 handler_pc;
		u2 catch_type;
	}
);

declare_jvm_struct!( // 4.7.4
	struct StackMapTableAttribute {
		u4 attribute_length;
		u2 number_of_entries;
		StackMapFrame entries[number_of_entries];
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationTypeInfo {
	Top, Integer, Float, Long, Double, Null, UninitializedThis,
	Object {
		constant_pool_index: u16,
	},
	Uninitialized {
		offset: u16,
	},
}

impl<R: Read> Parse<R> for VerificationTypeInfo {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let tag = u8::parse(reader, constant_pool)?;

		match tag {
			0 => Ok(Self::Top),
			1 => Ok(Self::Integer),
			2 => Ok(Self::Float),
			3 => Ok(Self::Double),
			4 => Ok(Self::Long),
			5 => Ok(Self::Null),
			6 => Ok(Self::UninitializedThis),
			7 => Ok(Self::Object {
				constant_pool_index: u16::parse(reader, constant_pool)?,
			}),
			8 => Ok(Self::Uninitialized {
				offset: u16::parse(reader, constant_pool)?,
			}),
			_ => Err(ClassFileParseError::UnknownVerificationTypeInfoTag(tag)),
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
		number_of_locals: u16,
		locals: Vec<VerificationTypeInfo>, // [number_of_locals]
		number_of_stack_items: u16,
		stack: Vec<VerificationTypeInfo>, // [number_of_stack_items]
	}
}

impl<R: Read> Parse<R> for StackMapFrame {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let frame_type = u8::parse(reader, constant_pool)?;

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
				offset_delta: u16::parse(reader, constant_pool)?,
				stack: VerificationTypeInfo::parse(reader, constant_pool)?,
			}),
			frame_type @ 248..=250 => Ok(Self::Chop {
				offset_delta: u16::parse(reader, constant_pool)?,
				k: 251 - frame_type,
			}),
			251 => Ok(Self::Same {
				offset_delta: u16::parse(reader, constant_pool)?,
			}),
			frame_type @ 252..=254 => Ok(Self::Append {
				offset_delta: u16::parse(reader, constant_pool)?,
				locals: Vec::parse_multi(reader, constant_pool, (frame_type - 251) as usize)?,
			}),
			255 => {
				let offset_delta = u16::parse(reader, constant_pool)?;
				let number_of_locals = u16::parse(reader, constant_pool)?;
				let locals = Vec::parse_multi(reader, constant_pool, number_of_locals as usize)?;
				let number_of_stack_items = u16::parse(reader, constant_pool)?;
				let stack = Vec::parse_multi(reader, constant_pool, number_of_stack_items as usize)?;

				Ok(Self::Full { offset_delta, number_of_locals, locals, number_of_stack_items, stack })
			},
		}
	}
}

declare_jvm_struct!( // 4.7.5
	struct ExceptionsAttribute {
		u4 attribute_length;
		u2 number_of_exceptions;
		u2 exception_index_table[number_of_exceptions];
	}
);

declare_jvm_struct!( // 4.7.6
	struct InnerClassesAttribute {
		u4 attribute_length;
		u2 number_of_classes;
		InnerClassesAttributeClassesElement classes[number_of_classes];
	}
);

declare_jvm_struct!( // 4.7.6, classes
	struct InnerClassesAttributeClassesElement {
		u2 inner_class_info_index;
		u2 outer_class_info_index;
		u2 inner_name_index;
		u2 inner_class_access_flags;
	}
);

declare_jvm_struct!( // 4.7.7
	struct EnclosingMethodAttribute {
		u4 attribute_length = 4;
		u2 class_index;
		u2 method_index;
	}
);

declare_jvm_struct!( // 4.7.6
	struct SyntheticAttribute {
		u4 attribute_length = 0;
	}
);

declare_jvm_struct!( // 4.7.9
	struct SignatureAttribute {
		u4 attribute_length = 2;
		u2 signature_index;
	}
);

declare_jvm_struct!( // 4.7.10
	struct SourceFileAttribute {
		u4 attribute_length = 2;
		u2 sourcefile_index;
	}
);

declare_jvm_struct!( // 4.7.11
	struct SourceDebugExtensionsAttribute {
		u4 attribute_length;
		u1 debug_extension[attribute_length];
	}
);

declare_jvm_struct!( // 4.7.12
	struct LineNumberTableAttribute {
		u4 attribute_length;
		u2 line_number_table_length;
		LineNumberTableEntry line_number_table[line_number_table_length];
	}
);

declare_jvm_struct!( // 4.7.12, line_number_table
	struct LineNumberTableEntry {
		u2 start_pc;
		u2 line_number;
	}
);

declare_jvm_struct!( // 4.7.13
	struct LocalVariableTableAttribute {
		u4 attribute_length;
		u2 local_variable_table_length;
		LocalVariableTableEntry local_variable_table[local_variable_table_length];
	}
);

declare_jvm_struct!( // 4.7.13, local_variable_table
	struct LocalVariableTableEntry {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 descriptor_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.14
	struct LocalVariableTypeTableAttribute {
		u4 attribute_length;
		u2 local_variable_type_table_length;
		LocalVariableTypeTableEntry local_variable_type_table[local_variable_type_table_length];
	}
);

declare_jvm_struct!( // 4.7.14, local_variable_type_table
	struct LocalVariableTypeTableEntry {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 signature_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.15
	struct DeprecatedAttribute {
		u4 attribute_length = 0;
	}
);

declare_jvm_struct!( // 4.7.16
	struct RuntimeVisibleAnnotationsAttribute {
		u4         attribute_length;
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.16, annotations
	struct Annotation {
		u2 type_index;
		u2 num_element_value_pairs;
		AnnotationElementValuePair element_value_pairs[num_element_value_pairs];
	}
);

declare_jvm_struct!( // 4.7.16, element_value_pairs
	struct AnnotationElementValuePair {
		u2            element_name_index;
		AnnotationElementValue value;
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationElementValue { // 4.7.16.1, value
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
		num_values: u16,
		values: Vec<AnnotationElementValue>, // [num_values]
	}
}

impl<R: Read> Parse<R> for AnnotationElementValue {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let tag = u8::parse(reader, constant_pool)?;

		let tag = tag.try_into().unwrap(); // TODO: fix this, this should be handled gracefully!

		Ok(match tag {
			'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' => Self::ConstantValueIndex {
				const_value_index: u16::parse(reader, constant_pool)?,
			},
			'e' => Self::EnumConstValue {
				type_name_index: u16::parse(reader, constant_pool)?,
				const_name_index: u16::parse(reader, constant_pool)?,
			},
			'c' => Self::ClassInfoIndex {
				class_info_index: u16::parse(reader, constant_pool)?,
			},
			'@' => Self::AnnotationValue {
				annotation_value: Annotation::parse(reader, constant_pool)?,
			},
			'[' => {
				let num_values = u16::parse(reader, constant_pool)?;
				let values = Vec::parse_multi(reader, constant_pool, num_values as usize)?;
				Self::ArrayValue { num_values, values }
			},
			_ => Err(ClassFileParseError::UnknownAnnotationElementValueTag(tag as u8))?
		})
	}
}

declare_jvm_struct!( // 4.7.17
	struct RuntimeInvisibleAnnotationsAttribute {
		u4         attribute_length;
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.18
	struct RuntimeVisibleParameterAnnotationsAttribute {
		u4 attribute_length;
		u1 num_parameters;
		ParameterAnnotationPair parameter_annotations[num_parameters];
	}
);

declare_jvm_struct!( // 4.7.18, parameter_annotations
	struct ParameterAnnotationPair {
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.19
	struct RuntimeInvisibleParameterAnnotationsAttribute {
		u4 attribute_length;
		u1 num_parameters;
		ParameterAnnotationPair parameter_annotations[num_parameters];
	}
);

declare_jvm_struct!( // 4.7.20
	struct AnnotationDefaultAttribute {
		u4            attribute_length;
    	AnnotationElementValue default_value;
	}
);

declare_jvm_struct!( // 4.7.21
	struct BootstrapMethodsAttribute {
		u4 attribute_length;
		u2 num_bootstrap_methods;
		BootstrapMethodsAttributeEntry bootstrap_methods[num_bootstrap_methods];
	}
);

declare_jvm_struct!( // 4.7.21, bootstrap_methods
	struct BootstrapMethodsAttributeEntry {
		u2 bootstrap_method_ref;
		u2 num_bootstrap_arguments;
		u2 bootstrap_arguments[num_bootstrap_arguments];
	}
);

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
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		let s: CpInfoUtf8 = constant_pool.parse_index(reader)?;

		let pool = Some(&constant_pool.0);
		Ok(match std::str::from_utf8(&s.bytes)? {
			"ConstantValue" => Self::ConstantValue(ConstantValueAttribute::parse(reader, pool)?),
			"Code" => Self::Code(CodeAttribute::parse(reader, pool)?),
			"StackMapTable" => Self::StackMapTable(StackMapTableAttribute::parse(reader, pool)?),
			"Exceptions" => Self::Exceptions(ExceptionsAttribute::parse(reader, pool)?),
			"InnerClasses" => Self::InnerClasses(InnerClassesAttribute::parse(reader, pool)?),
			"EnclosingMethod" => Self::EnclosingMethod(EnclosingMethodAttribute::parse(reader, pool)?),
			"Synthetic" => Self::Synthetic(SyntheticAttribute::parse(reader, pool)?),
			"Signature" => Self::Signature(SignatureAttribute::parse(reader, pool)?),
			"SourceFile" => Self::SourceFile(SourceFileAttribute::parse(reader, pool)?),
			"SourceDebugExtension" => Self::SourceDebugExtension(SourceDebugExtensionsAttribute::parse(reader, pool)?),
			"LineNumberTable" => Self::LineNumberTable(LineNumberTableAttribute::parse(reader, pool)?),
			"LocalVariableTable" => Self::LocalVariableTable(LocalVariableTableAttribute::parse(reader, pool)?),
			"LocalVariableTypeTable" => Self::LocalVariableTypeTable(LocalVariableTypeTableAttribute::parse(reader, pool)?),
			"Deprecated" => Self::Deprecated(DeprecatedAttribute::parse(reader, pool)?),
			"RuntimeVisibleAnnotations" => Self::RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeInvisibleAnnotations" => Self::RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeVisibleParameterAnnotations" => Self::RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeInvisibleParameterAnnotations" => Self::RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute::parse(reader, pool)?),
			"AnnotationDefault" => Self::AnnotationDefault(AnnotationDefaultAttribute::parse(reader, pool)?),
			"BootstrapMethods" => Self::BootstrapMethods(BootstrapMethodsAttribute::parse(reader, pool)?),
			name => {
				let info = parse_vec(reader,
				   |r| Ok(parse_u4(r)? as usize),
				   |r| parse_u1(r)
				)?;
				eprintln!("WARN: unknown attr: {name}: {info:?}");
				Self::Unknown { name, info }
			},
		})
	}
}


#[derive(Debug, Clone, PartialEq)]
struct MethodInfo { // 4.6
	access_flags: u16,
	name: CpInfoUtf8,
	descriptor: CpInfoUtf8,
	attributes: Vec<AttributeInfo>,
}

impl MethodInfo {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		Ok(MethodInfo {
			access_flags: parse_u2(reader)?,
			name: constant_pool.parse_index(reader)?,
			descriptor: constant_pool.parse_index(reader)?,
			attributes: parse_vec(reader,
			  |r| Ok(parse_u2(r)? as usize),
			  |r| AttributeInfo::parse(r, constant_pool)
			)?
		})
	}

	pub fn get_code(&self) -> Result<&AttributeInfo, ClassFileParseError> {
		for attribute in &self.attributes {
			if matches!(attribute, AttributeInfo::Code(_)) {
				return Ok(attribute);
			}
		}
		Err(ClassFileParseError::NoSuchAttribute("`Code` attribute not found."))
	}

	pub fn name(&self) -> Result<JUtf8, ClassFileParseError> {
		Ok(JUtf8(self.name.bytes.clone()))
	}
}

#[derive(Clone, Debug, PartialEq)]
struct ConstantPool(Vec<CpInfo>);

impl ConstantPool {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let vec: Vec<CpInfo> = parse_vec(reader,
			|r| Ok(parse_u2(r)? as usize - 1),
			|r| CpInfo::parse(r)
		)?;
		Ok(ConstantPool(vec))
	}

	/// Reads an [u16] and interprets it as an index into the constant pool, and tries to convert the tag there into the correct type.
	fn parse_index<R: Read, T: TryFrom<CpInfo>>(&self, reader: &mut R) -> Result<T, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<CpInfo>>::Error>
	{
		let index = parse_u2(reader)? as usize - 1; // TODO: can this panic?
		match self.0.get(index) {
			Some(item) => Ok(T::try_from(item.clone())?),
			None => Err(ClassFileParseError::NoSuchConstantPoolEntry(index))
		}
	}

	/// Reads an [u16] and interprets it as an index into the constant pool, allowing `0` for [Option::None]. If not zero, reads the constant pool at the given
	/// index and converts the element there into the correct type.
	fn parse_index_optional<R: Read, T: TryFrom<CpInfo>>(&self, reader: &mut R) -> Result<Option<T>, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<CpInfo>>::Error>
	{
		let index = parse_u2(reader)? as usize;
		if index == 0 {
			return Ok(None);
		}
		let index = index - 1;
		match self.0.get(index) {
			Some(item) => Ok(Some(T::try_from(item.clone())?)),
			None => Err(ClassFileParseError::NoSuchConstantPoolEntry(index))
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFile { // 4.1
	minor_version: u16,
	major_version: u16,
	constant_pool: ConstantPool,
	access_flags: u16,
	this_class: CpInfoClass,
	super_class: Option<CpInfoClass>,
	interfaces: Vec<CpInfoClass>,
	fields: Vec<FieldInfo>,
	methods: Vec<MethodInfo>,
	attributes: Vec<AttributeInfo>,
}

impl ClassFile {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let magic = parse_u4(reader)?;
		if magic != 0xCAFE_BABE {
			return Err(ClassFileParseError::InvalidMagic(magic));
		}

		let minor_version = parse_u2(reader)?;
		let major_version = parse_u2(reader)?;

		let constant_pool = ConstantPool::parse(reader)?;

		let access_flags = parse_u2(reader)?;

		let this_class: CpInfoClass = constant_pool.parse_index(reader)?;
		let super_class: Option<CpInfoClass> = constant_pool.parse_index_optional(reader)?;

		let interfaces: Vec<CpInfoClass> = {
			let count = parse_u2(reader)? as usize;
			let mut vec = Vec::with_capacity(count);
			for _ in 0..count {
				vec.push(constant_pool.parse_index(reader)?);
			}
			vec
		};

		let fields = parse_vec(reader,
			|r| Ok(parse_u2(r)? as usize),
			|r| FieldInfo::parse(r, &constant_pool)
		)?;
		let methods = parse_vec(reader,
		   |r| Ok(parse_u2(r)? as usize),
		   |r| MethodInfo::parse(r, &constant_pool)
		)?;
		let attributes = parse_vec(reader,
		   |r| Ok(parse_u2(r)? as usize),
		   |r| AttributeInfo::parse(r, &constant_pool)
		)?;

		Ok(ClassFile { minor_version, major_version, constant_pool, access_flags, this_class, super_class, interfaces, fields, methods, attributes })
	}

	#[deprecated]
	pub fn get_constant_pool(&self, constant_pool_address: usize) -> Result<&CpInfo, ClassFileParseError> {
		let address = constant_pool_address - 1;
		self.constant_pool.0
			.get(address)
			.ok_or(ClassFileParseError::NoSuchConstantPoolEntry(address))
	}

	pub fn verify(&self) -> Result<(), ClassFileParseError> {
		Ok(())
	}

	pub fn name(&self) -> Result<JUtf8, ClassFileParseError> {
		self.this_class.name(self)
	}
}

#[cfg(test)]
mod testing {
	use std::fs::File;
	use std::io::BufReader;
	use zip::ZipArchive;
	use super::ClassFile;
	#[test]
	fn try_parse_classfile() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let classfile = ClassFile::parse(&mut &bytes[..]).unwrap();
		classfile.verify().unwrap();

		println!("{:#?}", classfile);

		for method in classfile.methods {
			println!("method: {:#?}", method.get_code());
		}
	}

	#[test]
	#[cfg(target_os = "linux")]
	fn try_parse_classfile_from_zip() {
		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut class_file = rt.by_name("java/lang/Object.class").unwrap();
		let classfile = ClassFile::parse(&mut class_file).unwrap();

		println!("{classfile:#?}");
	}
}