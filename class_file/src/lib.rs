use std::fmt::Debug;
use std::io::Read;
use itertools::{Either, Itertools};

use crate::errors::ClassFileParseError;

mod verifier;

pub mod instruction;

pub mod name;
pub mod descriptor;
pub mod access;

pub mod cp;

use crate::access::{ClassInfoAccess, FieldInfoAccess, MethodInfoAccess};
use crate::cp::attribute::{AttributeInfo, CodeAttribute, ConstantValueAttribute};
use crate::cp::Pool;
use crate::descriptor::{FieldDescriptor, MethodDescriptor};
use crate::name::{ClassName, FieldName, MethodName};


macro_rules! gen_parse_u_int {
	($name:tt, $usize_parse_name:tt, $n:literal, $t:ty) => {
		#[inline]
		fn $name(&mut self) -> Result<$t, std::io::Error> {
			let mut buf = [0u8; $n];
			let length = self.read(&mut buf)?;
			if length == $n {
				Ok(<$t>::from_be_bytes(buf))
			} else {
				Err(std::io::Error::from(std::io::ErrorKind::UnexpectedEof))
			}
		}
		#[inline]
		fn $usize_parse_name(&mut self) -> Result<usize, std::io::Error> {
			Ok(self.$name()? as usize)
		}
	}
}

trait MyRead: Read {
	fn read_u1(&mut self) -> Result<u8, std::io::Error>;
	fn read_u2(&mut self) -> Result<u16, std::io::Error>;
	fn read_u4(&mut self) -> Result<u32, std::io::Error>;
	fn read_u1_as_usize(&mut self) -> Result<usize, std::io::Error>;
	fn read_u2_as_usize(&mut self) -> Result<usize, std::io::Error>;
	fn read_u4_as_usize(&mut self) -> Result<usize, std::io::Error>;
	/// First calls the `size` parameter to get the length of the data, then calls `element` so often to read the data, returning the data then. The
	/// argument `self` is given to both closures.
	#[inline]
	fn read_vec<T, E: From<E1>, SIZE, ELEMENT, E1>(&mut self, size: SIZE, element: ELEMENT) -> Result<Vec<T>, E>
	where
		SIZE: FnOnce(&mut Self) -> Result<usize, E1>,
		ELEMENT: Fn(&mut Self) -> Result<T, E>
	{
		let count = size(self)?;
		let mut vec = Vec::with_capacity(count);
		for _ in 0..count {
			vec.push(element(self)?);
		}
		Ok(vec)
	}
}
impl<T: Read> MyRead for T {
	gen_parse_u_int!(read_u1, read_u1_as_usize, 1, u8);
	gen_parse_u_int!(read_u2, read_u2_as_usize, 2, u16);
	gen_parse_u_int!(read_u4, read_u4_as_usize, 4, u32);
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo { // 4.5
	pub access_flags: FieldInfoAccess,
	pub name: FieldName,
	pub descriptor: FieldDescriptor,
	pub attributes: Vec<AttributeInfo>,
	pub constant_value: Option<ConstantValueAttribute>,
}

impl FieldInfo {
	fn parse<R: Read>(reader: &mut R, pool: &Pool) -> Result<Self, ClassFileParseError> {
		let access_flags = FieldInfoAccess::parse(reader.read_u2()?)?;
		let name = pool.get(reader.read_u2_as_usize()?)?;
		let descriptor = pool.get(reader.read_u2_as_usize()?)?;
		let attributes = reader.read_vec(
			|r| r.read_u2_as_usize(),
			|r| AttributeInfo::parse(r, pool)
		)?;

		let (constant_values, attributes): (Vec<ConstantValueAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::ConstantValue(constant_value) => Either::Left(constant_value),
				other => Either::Right(other),
			});
		let constant_value = constant_values.iter().next().cloned();

		Ok(FieldInfo {
			access_flags, name, descriptor, attributes, constant_value,
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodInfo { // 4.6
	pub access_flags: MethodInfoAccess,
	pub name: MethodName,
	pub descriptor: MethodDescriptor,
	pub attributes: Vec<AttributeInfo>,
	pub code: Option<CodeAttribute>,
}

impl MethodInfo {
	fn parse<R: Read>(reader: &mut R, pool: &Pool) -> Result<Self, ClassFileParseError> {
		let access_flags = MethodInfoAccess::parse(reader.read_u2()?)?;
		let name = pool.get(reader.read_u2_as_usize()?)?;
		let descriptor = pool.get(reader.read_u2_as_usize()?)?;
		let attributes = reader.read_vec(
		   |r| r.read_u2_as_usize(),
		   |r| AttributeInfo::parse(r, pool)
		)?;

		let (code, attributes): (Vec<CodeAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::Code(code) => Either::Left(code),
				other => Either::Right(other),
			});

		let code = if access_flags.is_native | access_flags.is_abstract {
			if code.len() != 0 {
				// ERR: found a code attribute when none was expected as of spec
				todo!("Found code, when code wasn't expected {code:?}");
			}
			None
		} else {
			if code.len() > 1 {
				todo!("found multiple code attributes!");
			}
			if code.len() == 0 {
				todo!("found no code attribute");
			}
			Some(code.into_iter().next().unwrap())
		};

		Ok(MethodInfo {
			access_flags,
			name,
			descriptor,
			attributes,
			code,
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassFile { // 4.1
	pub minor_version: u16,
	pub major_version: u16,
	pub access_flags: ClassInfoAccess,
	pub this_class: ClassName,
	pub super_class: Option<ClassName>,
	pub interfaces: Vec<ClassName>,
	pub fields: Vec<FieldInfo>,
	pub methods: Vec<MethodInfo>,
	pub attributes: Vec<AttributeInfo>,
}

impl ClassFile {
	pub fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
		let magic = reader.read_u4()?;
		if magic != 0xCAFE_BABE {
			return Err(ClassFileParseError::InvalidMagic(magic));
		}

		let minor_version = reader.read_u2()?;
		let major_version = reader.read_u2()?;

		if major_version <= 51 {
			return Err(ClassFileParseError::WrongVersion {
				major: major_version, minor: minor_version,
			});
		}

		let pool = Pool::parse(reader)?;

		let access_flags = ClassInfoAccess::parse(reader.read_u2()?)?;

		let this_class: ClassName = pool.get(reader.read_u2_as_usize()?)?;
		let super_class: Option<ClassName> = pool.get(reader.read_u2_as_usize()?)?;

		let interfaces: Vec<ClassName> = reader.read_vec(
			|r| r.read_u2_as_usize(),
			|r| pool.get(reader.read_u2_as_usize()?)
		)?;
		let fields = reader.read_vec(
			|r| r.read_u2_as_usize(),
			|r| FieldInfo::parse(r, &pool)
		)?;
		let methods = reader.read_vec(
		    |r| r.read_u2_as_usize(),
		    |r| MethodInfo::parse(r, &pool)
		)?;
		let attributes = reader.read_vec(
			|r| r.read_u2_as_usize(),
		   |r| AttributeInfo::parse(r, &pool)
		)?;

		let mut end = [0u8];
		if reader.read(&mut end)? != 0 {
			return Err(ClassFileParseError::IllegalInstruction("Contains bytes after class file."));
		}

		Ok(ClassFile { minor_version, major_version, access_flags, this_class, super_class, interfaces, fields, methods, attributes })
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
		let bytes = include_bytes!("../../../java_example_classfiles/Test3.class");
		let class_file = ClassFile::parse(&mut &bytes[..]).unwrap();
		class_file.verify().unwrap();

		//println!("{:#?}", class_file);

		//for method in class_file.methods {
		//	println!("method: {:#?}", method.code);
		//}
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