use std::fmt::{Debug, Display, Formatter};
use std::io::Read;
use crate::classfile::{parse_u1, parse_u2, parse_u4, parse_vec};
use crate::errors::{ClassFileParseError, ConstantPoolTagMismatchError};

// note that the types starting with CpInfo are parsed from the reader in the first run
// then in the second run, they are translated to the *Info types

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpIndex(u16);

macro_rules! try_from_enum_impl {
	($enum_type:ty, $pattern:path, $inner_type:ty, $error_type:ty) => {
		impl TryFrom<$enum_type> for $inner_type {
			type Error = $error_type;
			fn try_from(value: $enum_type) -> Result<Self, Self::Error> {
				match value {
					$pattern(value) => Ok(value),
					v => Err(Self::Error {
						expected: stringify!($pattern).to_string(),
						actual: format!("{:?}", v),
						msg: "".to_string(),
					}),
				}
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoClass {
	name_index: CpIndex, // Utf8
}
impl CpInfoClass {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoClass, ClassFileParseError> {
		Ok(CpInfoClass{
			name_index: CpIndex(parse_u2(reader)?),
		})
	}

	fn pool(&self, pool: &CpInfoVec) -> Result<ClassInfo, ClassFileParseError> {
		Ok(ClassInfo {
			name: pool.get::<CpInfoUtf8>(&self.name_index)?.into(),
		})
	}
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ClassInfo {
	pub name: Utf8Info,
}

impl From<&str> for ClassInfo {
	fn from(value: &str) -> Self {
		ClassInfo { name: Utf8Info::from(value) }
	}
}

impl Debug for ClassInfo {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "class: \"{}\"", self.name)
	}
}

impl Display for ClassInfo {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.name)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoFieldRef {
	class_index: CpIndex, // Class
	name_and_type_index: CpIndex, // NameAndType
}
impl CpInfoFieldRef {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoFieldRef, ClassFileParseError> {
		Ok(CpInfoFieldRef {
			class_index: CpIndex(parse_u2(reader)?),
			name_and_type_index: CpIndex(parse_u2(reader)?),
		})
	}

	fn pool(&self, pool: &CpInfoVec) -> Result<FieldRefInfo, ClassFileParseError> {
		Ok(FieldRefInfo {
			class: pool.get::<CpInfoClass>(&self.class_index)?.pool(pool)?,
			name_and_type: pool.get::<CpInfoNameAndType>(&self.name_and_type_index)?.pool(pool)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldRefInfo {
	pub class: ClassInfo,
	pub name_and_type: NameAndTypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoMethodRef {
	class_index: CpIndex, // Class
	name_and_type_index: CpIndex, // NameAndType
}
impl CpInfoMethodRef {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoMethodRef, ClassFileParseError> {
		Ok(CpInfoMethodRef {
			class_index: CpIndex(parse_u2(reader)?),
			name_and_type_index: CpIndex(parse_u2(reader)?),
		})
	}

	fn pool(&self, pool: &CpInfoVec) -> Result<MethodRefInfo, ClassFileParseError> {
		Ok(MethodRefInfo {
			class: pool.get::<CpInfoClass>(&self.class_index)?.pool(pool)?,
			name_and_type: pool.get::<CpInfoNameAndType>(&self.name_and_type_index)?.pool(pool)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodRefInfo {
	pub class: ClassInfo,
	pub name_and_type: NameAndTypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoInterfaceMethodRef {
	class_index: CpIndex, // Class
	name_and_type_index: CpIndex, // NameAndType
}
impl CpInfoInterfaceMethodRef {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoInterfaceMethodRef, ClassFileParseError> {
		Ok(CpInfoInterfaceMethodRef {
			class_index: CpIndex(parse_u2(reader)?),
			name_and_type_index: CpIndex(parse_u2(reader)?),
		})
	}

	fn pool(&self, pool: &CpInfoVec) -> Result<InterfaceMethodRefInfo, ClassFileParseError> {
		Ok(InterfaceMethodRefInfo {
			class: pool.get::<CpInfoClass>(&self.class_index)?.pool(pool)?,
			name_and_type: pool.get::<CpInfoNameAndType>(&self.name_and_type_index)?.pool(pool)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InterfaceMethodRefInfo {
	class: ClassInfo,
	name_and_type: NameAndTypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoString {
	string_index: CpIndex, // Utf8
}
impl CpInfoString {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoString, ClassFileParseError> {
		Ok(CpInfoString {
			string_index: CpIndex(parse_u2(reader)?),
		})
	}
}

#[derive(Clone, PartialEq, Eq)]
pub struct StringInfo {
	utf8: Utf8Info,
}

impl Debug for StringInfo {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "string: \"{}\"", self.utf8)
	}
}

impl Display for StringInfo {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.utf8)
	}
}

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerInfo {
	bytes: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoFloat {
	bytes: u32,
}
impl CpInfoFloat {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoFloat, ClassFileParseError> {
		Ok(CpInfoFloat {
			bytes: parse_u4(reader)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FloatInfo {
	bytes: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoLong {
	high_bytes: u32,
	low_bytes: u32,
}
impl CpInfoLong {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoLong, ClassFileParseError> {
		Ok(CpInfoLong {
			high_bytes: parse_u4(reader)?,
			low_bytes: parse_u4(reader)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LongInfo {
	high_bytes: u32,
	low_bytes: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoDouble {
	high_bytes: u32,
	low_bytes: u32,
}
impl CpInfoDouble {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoDouble, ClassFileParseError> {
		Ok(CpInfoDouble {
			high_bytes: parse_u4(reader)?,
			low_bytes: parse_u4(reader)?,
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DoubleInfo {
	high_bytes: u32,
	low_bytes: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoNameAndType {
	name_index: CpIndex, // Utf8
	descriptor_index: CpIndex, // Utf8
}
impl CpInfoNameAndType {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoNameAndType, ClassFileParseError> {
		Ok(CpInfoNameAndType {
			name_index: CpIndex(parse_u2(reader)?),
			descriptor_index: CpIndex(parse_u2(reader)?),
		})
	}
	fn pool(&self, pool: &CpInfoVec) -> Result<NameAndTypeInfo, ClassFileParseError> {
		Ok(NameAndTypeInfo {
			name: pool.get::<CpInfoUtf8>(&self.name_index)?.into(),
			descriptor: pool.get::<CpInfoUtf8>(&self.descriptor_index)?.into(),
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameAndTypeInfo {
	pub name: Utf8Info,
	pub descriptor: Utf8Info,
}

#[derive(Clone, PartialEq, Eq)]
struct CpInfoUtf8 {
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Utf8Info {
	pub bytes: Vec<u8>,
}
impl From<CpInfoUtf8> for Utf8Info {
	fn from(value: CpInfoUtf8) -> Self {
		Self { bytes: value.bytes }
	}
}
impl From<&str> for Utf8Info {
	fn from(value: &str) -> Self {
		Utf8Info { bytes: value.bytes().collect() }
	}
}
impl Display for Utf8Info {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		// todo!("that's hard, because of strange utf8 impl!")
		write!(f, "{}", String::from_utf8(self.bytes.clone()).unwrap())
	}
}
impl Debug for Utf8Info {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		//write!(f, "Utf8Info {{ string: \"{}\", raw: {:?} }}", self.to_string(), self.bytes)
		write!(f, "Utf8Info {{ string: \"{}\" }}", self.to_string())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoMethodHandle {
	reference_kind: u8,
	reference_index: CpIndex,
}
impl CpInfoMethodHandle {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoMethodHandle, ClassFileParseError> {
		Ok(CpInfoMethodHandle {
			reference_kind: parse_u1(reader)?,
			reference_index: CpIndex(parse_u2(reader)?),
		})
	}

	fn pool(&self, pool: &CpInfoVec) -> Result<MethodHandleInfo, ClassFileParseError> {
		let kind = self.reference_kind;
		match kind {
			kind @ (1 | 2 | 3 | 4) => {
				let field_ref = pool.get::<CpInfoFieldRef>(&self.reference_index)?.pool(pool)?;
				Ok(match kind {
					1 => MethodHandleInfo::GetField(field_ref),
					2 => MethodHandleInfo::GetStatic(field_ref),
					3 => MethodHandleInfo::PutField(field_ref),
					4 => MethodHandleInfo::PutStatic(field_ref),
					_ => unreachable!()
				})
			},
			kind @ (5 | 6 | 7 | 8) => {
				let method_ref = pool.get::<CpInfoMethodRef>(&self.reference_index)?.pool(pool)?;
				// TODO: if kind == 8: must be <init>, else: must not be <init>/<clinit>
				Ok(match kind {
					5 => MethodHandleInfo::InvokeVirtual(method_ref),
					6 => MethodHandleInfo::InvokeStatic(method_ref),
					7 => MethodHandleInfo::InvokeSpecial(method_ref),
					8 => MethodHandleInfo::NewInvokeSpecial(method_ref),
					_ => unreachable!()
				})
			},
			9 => {
				let interface_method_ref = pool.get::<CpInfoInterfaceMethodRef>(&self.reference_index)?.pool(pool)?;
				// TODO: must not be <init>/<clinit>
				Ok(MethodHandleInfo::InvokeInterface(interface_method_ref))
			},
			kind => Err(ClassFileParseError::UnknownMethodHandleInfoKind(kind)),
		}
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MethodHandleInfo {
	GetField(FieldRefInfo),
	GetStatic(FieldRefInfo),
	PutField(FieldRefInfo),
	PutStatic(FieldRefInfo),
	InvokeVirtual(MethodRefInfo),
	InvokeStatic(MethodRefInfo),
	InvokeSpecial(MethodRefInfo),
	NewInvokeSpecial(MethodRefInfo),
	InvokeInterface(InterfaceMethodRefInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoMethodType {
	descriptor_index: CpIndex, // Utf8
}
impl CpInfoMethodType {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoMethodType, ClassFileParseError> {
		Ok(CpInfoMethodType {
			descriptor_index: CpIndex(parse_u2(reader)?),
		})
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MethodTypeInfo {
	descriptor: Utf8Info,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CpInfoInvokeDynamic {
	bootstrap_method_attr_index: u16,
	name_and_type_index: CpIndex, // NameAndType
}
impl CpInfoInvokeDynamic {
	fn parse<R: Read>(reader: &mut R) -> Result<CpInfoInvokeDynamic, ClassFileParseError> {
		Ok(CpInfoInvokeDynamic {
			bootstrap_method_attr_index: parse_u2(reader)?,
			name_and_type_index: CpIndex(parse_u2(reader)?),
		})
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct InvokeDynamicInfo {
	bootstrap_method_attr_index: u16,
	name_and_type: NameAndTypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CpInfo { // 4.4, Table 4.3
	Class(CpInfoClass), FieldRef(CpInfoFieldRef), MethodRef(CpInfoMethodRef), InterfaceMethodRef(CpInfoInterfaceMethodRef),
	String(CpInfoString), Integer(CpInfoInteger), Float(CpInfoFloat), Long(CpInfoLong), Double(CpInfoDouble),
	NameAndType(CpInfoNameAndType), Utf8(CpInfoUtf8), MethodHandle(CpInfoMethodHandle), InvokeDynamic(CpInfoInvokeDynamic),
	MethodType(CpInfoMethodType),
}
impl CpInfo {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		match parse_u1(reader)? {
			1  => Ok(Self::Utf8(CpInfoUtf8::parse(reader)?)),
			3  => Ok(Self::Integer(CpInfoInteger::parse(reader)?)),
			4  => Ok(Self::Float(CpInfoFloat::parse(reader)?)),
			5  => Ok(Self::Long(CpInfoLong::parse(reader)?)),
			6  => Ok(Self::Double(CpInfoDouble::parse(reader)?)),
			7  => Ok(Self::Class(CpInfoClass::parse(reader)?)),
			8  => Ok(Self::String(CpInfoString::parse(reader)?)),
			9  => Ok(Self::FieldRef(CpInfoFieldRef::parse(reader)?)),
			10 => Ok(Self::MethodRef(CpInfoMethodRef::parse(reader)?)),
			11 => Ok(Self::InterfaceMethodRef(CpInfoInterfaceMethodRef::parse(reader)?)),
			12 => Ok(Self::NameAndType(CpInfoNameAndType::parse(reader)?)),
			15 => Ok(Self::MethodHandle(CpInfoMethodHandle::parse(reader)?)),
			16 => Ok(Self::MethodType(CpInfoMethodType::parse(reader)?)),
			18 => Ok(Self::InvokeDynamic(CpInfoInvokeDynamic::parse(reader)?)),
			tag => Err(ClassFileParseError::UnknownConstantPoolTag(tag)),
		}
	}
}
impl CpInfo {
	fn unwrap(&self, pool: &CpInfoVec) -> Result<ConstantPoolElement, ClassFileParseError> {
		let v = match self {
			CpInfo::Class(class) => ConstantPoolElement::Class(ClassInfo {
				name: pool.get::<CpInfoUtf8>(&class.name_index)?.into(),
			}),
			CpInfo::FieldRef(field) => ConstantPoolElement::FieldRef(
				field.pool(pool)?
			),
			CpInfo::MethodRef(method) => ConstantPoolElement::MethodRef(
				method.pool(pool)?
			),
			CpInfo::InterfaceMethodRef(interface_method) => ConstantPoolElement::InterfaceMethodRef(
				interface_method.pool(pool)?
			),
			CpInfo::String(string) => ConstantPoolElement::String(StringInfo {
				utf8: pool.get::<CpInfoUtf8>(&string.string_index)?.into(),
			}),
			CpInfo::Integer(integer) => ConstantPoolElement::Integer(IntegerInfo {
				bytes: integer.bytes,
			}),
			CpInfo::Float(float) => ConstantPoolElement::Float(FloatInfo {
				bytes: float.bytes,
			}),
			CpInfo::Long(long) => ConstantPoolElement::Long(LongInfo {
				high_bytes: long.high_bytes,
				low_bytes: long.low_bytes,
			}),
			CpInfo::Double(double) => ConstantPoolElement::Double(DoubleInfo {
				high_bytes: double.high_bytes,
				low_bytes: double.low_bytes,
			}),
			CpInfo::NameAndType(name_and_type) => ConstantPoolElement::NameAndType(NameAndTypeInfo {
				name: pool.get::<CpInfoUtf8>(&name_and_type.name_index)?.into(),
				descriptor: pool.get::<CpInfoUtf8>(&name_and_type.descriptor_index)?.into(),
			}),
			CpInfo::Utf8(utf8) => ConstantPoolElement::Utf8(Utf8Info {
				bytes: utf8.bytes.clone(),
			}),
			CpInfo::MethodHandle(method_handle) => ConstantPoolElement::MethodHandle(method_handle.pool(pool)?),
			CpInfo::MethodType(method_type) => ConstantPoolElement::MethodType(MethodTypeInfo {
				descriptor: pool.get::<CpInfoUtf8>(&method_type.descriptor_index)?.into(),
			}),
			CpInfo::InvokeDynamic(invoke_dynamic) => ConstantPoolElement::InvokeDynamic(InvokeDynamicInfo {
				bootstrap_method_attr_index: invoke_dynamic.bootstrap_method_attr_index,
				name_and_type: pool.get::<CpInfoNameAndType>(&invoke_dynamic.name_and_type_index)?.pool(pool)?,
			})
		};
		Ok(v)
	}
}

#[derive(Debug)]
struct CpInfoVec(Vec<CpInfo>);
impl CpInfoVec {
	fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let vec: Vec<CpInfo> = parse_vec(reader,
			 |r| Ok(parse_u2(r)? as usize - 1),
			 |r| CpInfo::parse(r)
		)?;
		Ok(Self(vec))
	}

	fn get<T: TryFrom<CpInfo>>(&self, index: &CpIndex) -> Result<T, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<CpInfo>>::Error>
	{
		let index = index.0 as usize;
		match self.0.get(index - 1) { // TODO: this can panic!
			Some(item) => Ok(T::try_from(item.clone())?),
			None => Err(ClassFileParseError::NoSuchConstantPoolEntry(index)),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConstantPoolElement {
	Class(ClassInfo), FieldRef(FieldRefInfo), MethodRef(MethodRefInfo), InterfaceMethodRef(InterfaceMethodRefInfo), String(StringInfo), Integer(IntegerInfo),
	Float(FloatInfo), Long(LongInfo), Double(DoubleInfo), NameAndType(NameAndTypeInfo), Utf8(Utf8Info), MethodHandle(MethodHandleInfo),
	MethodType(MethodTypeInfo), InvokeDynamic(InvokeDynamicInfo),
}

try_from_enum_impl!(CpInfo, CpInfo::Class, CpInfoClass, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::FieldRef, CpInfoFieldRef, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::MethodRef, CpInfoMethodRef, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::InterfaceMethodRef, CpInfoInterfaceMethodRef, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::String, CpInfoString, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::Integer, CpInfoInteger, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::Float, CpInfoFloat, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::Long, CpInfoLong, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::Double, CpInfoDouble, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::NameAndType, CpInfoNameAndType, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::Utf8, CpInfoUtf8, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::MethodHandle, CpInfoMethodHandle, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::MethodType, CpInfoMethodType, ConstantPoolTagMismatchError);
try_from_enum_impl!(CpInfo, CpInfo::InvokeDynamic, CpInfoInvokeDynamic, ConstantPoolTagMismatchError);

try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Class, ClassInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::FieldRef, FieldRefInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::MethodRef, MethodRefInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::InterfaceMethodRef, InterfaceMethodRefInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::String, StringInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Integer, IntegerInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Float, FloatInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Long, LongInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Double, DoubleInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::NameAndType, NameAndTypeInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::Utf8, Utf8Info, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::MethodHandle, MethodHandleInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::MethodType, MethodTypeInfo, ConstantPoolTagMismatchError);
try_from_enum_impl!(ConstantPoolElement, ConstantPoolElement::InvokeDynamic, InvokeDynamicInfo, ConstantPoolTagMismatchError);

#[derive(Clone, Debug, PartialEq)]
pub struct ConstantPool(Vec<ConstantPoolElement>);

impl ConstantPool {
	pub fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let cp_info_vec = CpInfoVec::parse(reader)?;

		let mut vec = Vec::with_capacity(cp_info_vec.0.len() + 1);
		vec.push(ConstantPoolElement::String(StringInfo {
			utf8: Utf8Info::from("Nope! This is the 0-index constant pool entry, you shouldn't be able to get this!")
		}));
		for i in &cp_info_vec.0 {
			vec.push(i.unwrap(&cp_info_vec)?);
		}
		Ok(ConstantPool(vec))
	}

	/// Reads an [u16] and interprets it as an index into the constant pool, and tries to convert the tag there into the correct type.
	pub fn parse_index<R: Read, T: TryFrom<ConstantPoolElement>>(&self, reader: &mut R) -> Result<T, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<ConstantPoolElement>>::Error>
	{
		self.get(parse_u2(reader)? as usize)
	}

	/// Reads an [u16] and interprets it as an index into the constant pool, allowing `0` for [None]. If not zero, reads the constant pool at the given
	/// index and converts the element there into the correct type.
	pub fn parse_index_optional<R: Read, T: TryFrom<ConstantPoolElement>>(&self, reader: &mut R) -> Result<Option<T>, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<ConstantPoolElement>>::Error>
	{
		let index = parse_u2(reader)? as usize;
		if index == 0 {
			return Ok(None);
		}
		match self.get(index) {
			Ok(item) => Ok(Some(item)),
			Err(e) => Err(e),
		}
	}

	/// Reads an [u16] and interprets it as an index into the constant pool. Doesn't try to convert the tag there.
	pub fn parse_index_get<R: Read>(&self, reader: &mut R) -> Result<&ConstantPoolElement, ClassFileParseError> {
		let index = parse_u2(reader)? as usize;
		match self.0.get(index) {
			Some(item) => Ok(item),
			None => Err(ClassFileParseError::NoSuchConstantPoolEntry(index)),
		}
	}

	/// Takes in an [usize] and gives back the corresponding constant pool entry. Tries to convert into it.
	pub fn get<T: TryFrom<ConstantPoolElement>>(&self, index: usize) -> Result<T, ClassFileParseError>
		where ClassFileParseError: From<<T as TryFrom<ConstantPoolElement>>::Error>
	{
		match self.0.get(index) {
			Some(item) => Ok(T::try_from(item.clone())?),
			None => Err(ClassFileParseError::NoSuchConstantPoolEntry(index)),
		}
	}
}