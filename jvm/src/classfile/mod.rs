use std::fmt::{Debug, Formatter};
use std::io::Read;

use gen::declare_jvm_struct;
use crate::errors::{ClassFileParseError, ConstantPoolTagMismatchError};

mod attribute;
pub use attribute::*;

mod constant_pool;
pub use constant_pool::*;

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
		let bytes = include_bytes!("../../../java_example_classfiles/Test.class");
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