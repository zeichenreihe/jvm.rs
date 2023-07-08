use std::fmt::{Debug, Formatter};
use std::io::Read;
use gen::declare_jvm_struct;
use crate::classfile::{ClassFile, JUtf8, parse_u1, parse_u2, parse_u4, parse_vec, Parse, ParseMulti};
use crate::errors::{ClassFileParseError, ConstantPoolTagMismatchError};


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
	pub bytes: Vec<u8>,
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
	pub fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
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