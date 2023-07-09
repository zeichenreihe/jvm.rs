use std::fmt::{Debug, Formatter};
use std::io::Read;

use crate::errors::ClassFileParseError;

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

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct JUtf8(pub Vec<u8>);

impl Debug for JUtf8 {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "Utf8 {{ \"{}\" }}", String::from_utf8_lossy(&self.0))
	}
}

impl From<Utf8Info> for JUtf8 {
	fn from(value: Utf8Info) -> Self {
		Self(value.bytes)
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
pub struct FieldInfo { // 4.5
	pub access_flags: u16,
	pub name: Utf8Info,
	pub descriptor: Utf8Info,
	pub attributes: Vec<AttributeInfo>,
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
pub struct MethodInfo { // 4.6
	pub access_flags: u16,
	pub name: Utf8Info,
	pub descriptor: Utf8Info,
	pub attributes: Vec<AttributeInfo>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFile { // 4.1
	pub minor_version: u16,
	pub major_version: u16,
	pub constant_pool: ConstantPool,
	pub access_flags: u16,
	pub this_class: ClassInfo,
	pub super_class: Option<ClassInfo>,
	pub interfaces: Vec<ClassInfo>,
	pub fields: Vec<FieldInfo>,
	pub methods: Vec<MethodInfo>,
	pub attributes: Vec<AttributeInfo>,
}

impl ClassFile {
	pub(crate) fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let magic = parse_u4(reader)?;
		if magic != 0xCAFE_BABE {
			return Err(ClassFileParseError::InvalidMagic(magic));
		}

		let minor_version = parse_u2(reader)?;
		let major_version = parse_u2(reader)?;

		let constant_pool = ConstantPool::parse(reader)?;

		let access_flags = parse_u2(reader)?;

		let this_class: ClassInfo = constant_pool.parse_index(reader)?;
		let super_class: Option<ClassInfo> = constant_pool.parse_index_optional(reader)?;

		let interfaces: Vec<ClassInfo> = {
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

	pub fn verify(&self) -> Result<(), ClassFileParseError> {
		Ok(())
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