use std::fmt::{Debug, Formatter, Write};
use std::io::Read;
use itertools::{Either, Itertools};

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

#[derive(Clone, PartialEq)]
pub struct FieldInfoAccess {
	pub is_public: bool,
	pub is_private: bool,
	pub is_protected: bool,
	pub is_static: bool,
	pub is_final: bool,
	pub is_volatile: bool,
	pub is_transient: bool,
	pub is_synthetic: bool,
	pub is_enum: bool,
}

impl FieldInfoAccess {
	fn parse(access_flags: u16) -> Result<Self, ClassFileParseError> {
		let is_public       = access_flags & 0x0001 != 0;
		let is_private      = access_flags & 0x0002 != 0;
		let is_protected    = access_flags & 0x0004 != 0;
		let is_static       = access_flags & 0x0008 != 0;
		let is_final        = access_flags & 0x0010 != 0;
		let is_volatile     = access_flags & 0x0040 != 0;
		let is_transient    = access_flags & 0x0080 != 0;
		let is_synthetic    = access_flags & 0x1000 != 0;
		let is_enum         = access_flags & 0x4000 != 0;
		// other bits: reserved for future use

		// at most one of: is_public, is_private, is_protected
		// at most one of: is_final, is_volatile

		let is_interface_field = false;
		if is_interface_field {
			// must have: is_public, is_static, is_final
			// must not have: is_private, is_protected, is_volatile, is_transient, is_synthetic, is_enum
		}

		Ok(FieldInfoAccess {
			is_public, is_private, is_protected, is_static, is_final, is_volatile, is_transient, is_synthetic, is_enum,
		})
	}
}

impl Debug for FieldInfoAccess {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("FieldInfoAccess { ")?;
		if self.is_public    { f.write_str("public ")?; }
		if self.is_private   { f.write_str("private ")?; }
		if self.is_protected { f.write_str("protected ")?; }
		if self.is_static    { f.write_str("static ")?; }
		if self.is_final     { f.write_str("final ")?; }
		if self.is_volatile  { f.write_str("volatile ")?; }
		if self.is_transient { f.write_str("transient ")?; }
		if self.is_synthetic { f.write_str("synthetic ")?; }
		if self.is_enum      { f.write_str("enum ")?; }
		f.write_str("}")
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo { // 4.5
	pub access_flags: FieldInfoAccess,
	pub name: Utf8Info,
	pub descriptor: Utf8Info,
	pub attributes: Vec<AttributeInfo>,
}

impl FieldInfo {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		Ok(FieldInfo {
			access_flags: FieldInfoAccess::parse(parse_u2(reader)?)?,
			name: constant_pool.parse_index(reader)?,
			descriptor: constant_pool.parse_index(reader)?,
			attributes: parse_vec(reader,
			    |r| Ok(parse_u2(r)? as usize),
			    |r| AttributeInfo::parse(r, constant_pool),
			)?,
		})
	}
}

#[derive(Clone, PartialEq)]
pub struct MethodInfoAccess {
	pub is_public: bool,
	pub is_private: bool,
	pub is_protected: bool,
	pub is_static: bool,
	pub is_final: bool,
	pub is_synchronised: bool,
	pub is_bridge: bool,
	pub is_varargs: bool,
	pub is_native: bool,
	pub is_abstract: bool,
	pub is_strict: bool,
	pub is_synthetic: bool,
}

impl MethodInfoAccess {
	fn parse(access_flags: u16) -> Result<Self, ClassFileParseError> {
		let is_public       = access_flags & 0x0001 != 0;
		let is_private      = access_flags & 0x0002 != 0;
		let is_protected    = access_flags & 0x0004 != 0;
		let is_static       = access_flags & 0x0008 != 0;
		let is_final        = access_flags & 0x0010 != 0;
		let is_synchronised = access_flags & 0x0020 != 0;
		let is_bridge       = access_flags & 0x0040 != 0;
		let is_varargs      = access_flags & 0x0080 != 0;
		let is_native       = access_flags & 0x0100 != 0;
		let is_abstract     = access_flags & 0x0400 != 0;
		let is_strict       = access_flags & 0x0800 != 0;
		let is_synthetic    = access_flags & 0x1000 != 0;
		// other bits: reserved for future use

		// at most one of: is_public, is_private, is_protected!

		if is_abstract {
			// must not have: is_final, is_native, is_private, is_static, is_strict, is_synchronised
		}

		let is_interface_method = false;
		if is_interface_method {
			// must have: is_abstract, is_public
			// may have: is_bridge, is_varargs, is_synthetic
			// -> must not have: (is_private, is_protected), is_static, is_final, is_synchronised, is_native, is_strict
		}

		let is_specific_instance_initialisation_method = false;
		if is_specific_instance_initialisation_method {
			// at most one of: public, private, protected
			// may have: is_varargs, is_strict, is_synthetic
			// -> must not have: is_static, is_final, is_synchronised, is_bridge, is_native, is_abstract
		}

		// class and interface initialisation methods: don't check any of this

		Ok(MethodInfoAccess {
			is_public, is_private, is_protected, is_static, is_final, is_synchronised, is_bridge, is_varargs, is_native, is_abstract, is_strict, is_synthetic
		})
	}
}

impl Debug for MethodInfoAccess {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("MethodInfoAccess { ")?;
		if self.is_public       { f.write_str("public ")?; }
		if self.is_private      { f.write_str("private ")?; }
		if self.is_protected    { f.write_str("protected ")?; }
		if self.is_static       { f.write_str("static ")?; }
		if self.is_final        { f.write_str("final ")?; }
		if self.is_synchronised { f.write_str("synchronised ")?; }
		if self.is_bridge       { f.write_str("bridge ")?; }
		if self.is_varargs      { f.write_str("varargs ")?; }
		if self.is_native       { f.write_str("native ")?; }
		if self.is_abstract     { f.write_str("abstract ")?; }
		if self.is_strict       { f.write_str("strict ")?; }
		if self.is_synthetic    { f.write_str("synthetic ")?; }
		f.write_str("}")
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodInfo { // 4.6
	pub access_flags: MethodInfoAccess,
	pub name: Utf8Info,
	pub descriptor: Utf8Info,
	pub attributes: Vec<AttributeInfo>,
	pub code: Option<CodeAttribute>,
}

impl MethodInfo {
	fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		let access_flags = MethodInfoAccess::parse(parse_u2(reader)?)?;
		let name = constant_pool.parse_index(reader)?;
		let descriptor = constant_pool.parse_index(reader)?;
		let attributes = parse_vec(reader,
		   |r| Ok(parse_u2(r)? as usize),
		   |r| AttributeInfo::parse(r, constant_pool)
		)?;

		let (code, attributes): (Vec<CodeAttribute>, Vec<AttributeInfo>) = attributes.into_iter()
			.partition_map(|attribute| match attribute {
				AttributeInfo::Code(code) => Either::Left(code),
				other => Either::Right(other),
			});

		let code = if access_flags.is_native | access_flags.is_abstract {
			if code.len() != 0 {
				// ERR: found a code attribute when none was expected as of spec
				panic!("Found code, when code wasn't expected {code:?}");
			}
			None
		} else {
			if code.len() > 1 {
				panic!("found multiple code attributes!");
			}
			if code.len() == 0 {
				panic!("found no code attribute");
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
	pub fn parse<R: Read>(reader: &mut R) -> Result<Self, ClassFileParseError> {
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
		let class_file = ClassFile::parse(&mut &bytes[..]).unwrap();
		class_file.verify().unwrap();

		//println!("{:#?}", class_file);

		for method in class_file.methods {
		//	println!("method: {:#?}", method.code);
		}
	}

	#[test]
	#[cfg(target_os = "linux")]
	fn try_parse_classfile_from_zip() {
		let rt = File::open("/usr/lib/jvm/java-8-openjdk/jre/lib/rt.jar").unwrap();
		let mut rt = ZipArchive::new(BufReader::new(rt)).unwrap();

		let mut class_file = rt.by_name("java/lang/Object.class").unwrap();
		let classfile = ClassFile::parse(&mut class_file).unwrap();

		//println!("{classfile:#?}");
	}
}