use anyhow::{bail, Result};
use std::fmt::Debug;
use std::io::Read;
use itertools::{Either, Itertools};

pub mod verifier;
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

pub trait MyRead: Read {
	fn read_n<const N: usize>(&mut self) -> Result<[u8; N]> {
		let mut buf = [0u8; N];
		let length = self.read(&mut buf)?;
		if length == N {
			Ok(buf)
		} else {
			bail!("unexpected data end")
		}
	}
	// TODO: rename to u8, u16, u32
	fn read_u1(&mut self) -> Result<u8> {
		self.read_n().map(|x| u8::from_be_bytes(x))
	}
	fn read_u2(&mut self) -> Result<u16> {
		Ok(u16::from_be_bytes(self.read_n()?))
	}
	fn read_u4(&mut self) -> Result<u32> {
		Ok(u32::from_be_bytes(self.read_n()?))
	}
	fn read_u1_as_usize(&mut self) -> Result<usize> {
		Ok(self.read_u1()? as usize)
	}
	fn read_u2_as_usize(&mut self) -> Result<usize> {
		Ok(self.read_u2()? as usize)
	}
	fn read_u4_as_usize(&mut self) -> Result<usize> {
		Ok(self.read_u4()? as usize)
	}
	fn read_i8(&mut self) -> Result<i8> {
		todo!()
	}
	fn read_i16(&mut self) -> Result<i16> {
		todo!()
	}
	fn read_i32(&mut self) -> Result<i32> {
		todo!()
	}
	/// First calls the `size` parameter to get the length of the data, then calls `element` so often to read the data, returning the data then. The
	/// argument `self` is given to both closures.
	#[inline]
	fn read_vec<T, S, E>(&mut self, size: S, element: E) -> Result<Vec<T>>
	where
		S: FnOnce(&mut Self) -> Result<usize>,
		E: Fn(&mut Self) -> Result<T>
	{
		let count = size(self)?;
		let mut vec = Vec::with_capacity(count);
		for _ in 0..count {
			vec.push(element(self)?);
		}
		Ok(vec)
	}
}
impl<T: Read> MyRead for T {}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo { // 4.5
	pub access_flags: FieldInfoAccess,
	pub name: FieldName,
	pub descriptor: FieldDescriptor,
	pub attributes: Vec<AttributeInfo>,
	pub constant_value: Option<ConstantValueAttribute>,
}

impl FieldInfo {
	fn parse<R: Read>(reader: &mut R, pool: &Pool) -> Result<Self> {
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
	fn parse<R: Read>(reader: &mut R, pool: &Pool) -> Result<Self> {
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
pub struct ClassFile {
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
	pub fn parse<R: Read>(reader: &mut R) -> Result<Self> {
		let magic = reader.read_u4()?;
		if magic != 0xCAFE_BABE {
			bail!("magic didn't match up: {magic:x}")
		}

		let minor_version = reader.read_u2()?;
		let major_version = reader.read_u2()?;

		if major_version <= 51 {
			bail!("we only accept class files with version >= 52.0, this one has: {major_version}.{minor_version}")
		}

		let pool = Pool::parse(reader)?;

		let access_flags = ClassInfoAccess::parse(reader.read_u2()?)?;

		let this_class: ClassName = pool.get(reader.read_u2_as_usize()?)?;
		let super_class: Option<ClassName> = pool.get(reader.read_u2_as_usize()?)?;

		let interfaces: Vec<ClassName> = reader.read_vec(
			|r| r.read_u2_as_usize(),
			|r| pool.get(r.read_u2_as_usize()?)
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
			bail!("expected end of class file")
		}

		Ok(ClassFile { minor_version, major_version, access_flags, this_class, super_class, interfaces, fields, methods, attributes })
	}

	pub fn verify(&self) -> Result<()> {
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