use std::io::Read;
use std::str::Utf8Error;
use std::string::FromUtf8Error;

use gen::declare_jvm_struct;

#[derive(Debug)]
#[allow(unused)]
pub enum Error {
	ReadWrongAmountOfDataStr { actual: usize, expected: usize, string: &'static str },
	ReadWrongAmountOfData { actual: usize, expected: usize },
	ReadWrongTag { actual: u32, string: &'static str },
	IoError(std::io::Error),
	FromUtf8Error(FromUtf8Error),
	Utf8Error(Utf8Error),
	InvalidMagic { actual: u32, expected: u32 },
	InvalidConstantPoolAddress { address: u16 },
	InvalidConstantPoolTag ( u8 ),
	NoUtf8AtAddress ( u16 ),
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::IoError(value)
	}
}

impl From<FromUtf8Error> for Error {
	fn from(value: FromUtf8Error) -> Self {
		Self::FromUtf8Error(value)
	}
}

impl From<Utf8Error> for Error {
	fn from(value: Utf8Error) -> Self {
		Self::Utf8Error(value)
	}
}

pub trait Parse<R: Read> {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, Error> where Self:Sized;
}

pub trait ParseMulti<R: Read, T: Parse<R>> {
	fn parse_multi(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>, count: usize) -> Result<Vec<T>, Error>;
}

impl <R: Read, T: Parse<R>> ParseMulti<R, T> for Vec<T> {
	fn parse_multi(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>, count: usize) -> Result<Vec<T>, Error> {
		let mut vec = Vec::with_capacity(count);
		for _ in 0..count { vec.push(T::parse(reader, constant_pool)?); }
		Ok(vec)
	}
}

macro_rules! impl_parse_for {
	($t:ty, $n:literal) => {
		impl<R: Read> Parse<R> for $t {
			fn parse(reader: &mut R, _: Option<&Vec<CpInfo>>) -> Result<Self, Error> {
				let mut buf = [0u8; $n];
				let length = reader.read(&mut buf)?;
				if length == $n {
					Ok(<$t>::from_be_bytes(buf))
				} else {
					Err(Error::ReadWrongAmountOfDataStr { actual: length, expected: $n, string: stringify!($t) })
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

declare_jvm_struct!(
	struct CpInfoClass {
		u2 name_index;
	}
);
impl CpInfoClass {
	pub fn name<'a>(&'a self, class_file: &'a ClassFile) -> Result<JUtf8, Error> {
		if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.name_index as usize)? {
			Ok(JUtf8(utf8.bytes.clone()))
		} else {
			Err(Error::WrongConstantPoolTag)
		}
	}
}
declare_jvm_struct!(
	struct CpInfoFieldRef {
		u2 class_index;
		u2 name_and_type_index;
	}
);
impl CpInfoFieldRef {
	pub fn class_name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8, JUtf8), Error> {
		let class = if let CpInfo::Class(class) = class_file.get_constant_pool(self.class_index as usize)? {
			class.name(class_file)?
		} else {
			Err(Error::WrongConstantPoolTag)?
		};

		let (name, desc) = if let CpInfo::NameAndType(name_and_type) = class_file.get_constant_pool(self.name_and_type_index as usize)? {
			name_and_type.name_descriptor(class_file)?
		} else {
			Err(Error::WrongConstantPoolTag)?
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
impl CpInfoMethodRef {
	pub fn class_name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8, JUtf8), Error> {
		let class = if let CpInfo::Class(class) = class_file.get_constant_pool(self.class_index as usize)? {
			class.name(class_file)?
		} else {
			Err(Error::WrongConstantPoolTag)?
		};

		let (name, descriptor) = if let CpInfo::NameAndType(name_and_type) = class_file.get_constant_pool(self.name_and_type_index as usize)? {
			name_and_type.name_descriptor(class_file)?
		} else {
			Err(Error::WrongConstantPoolTag)?
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
declare_jvm_struct!(
	struct CpInfoString {
		u2 string_index;
	}
);
declare_jvm_struct!(
	struct CpInfoInteger {
		u4 bytes;
	}
);
declare_jvm_struct!(
	struct CpInfoFloat {
		u4 bytes;
	}
);
declare_jvm_struct!(
	struct CpInfoLong {
		u4 high_bytes;
		u4 low_bytes;
	}
);
declare_jvm_struct!(
	struct CpInfoDouble {
		u4 high_bytes;
		u4 low_bytes;
	}
);
declare_jvm_struct!(
	struct CpInfoNameAndType {
		u2 name_index;
		u2 descriptor_index;
	}
);
impl CpInfoNameAndType {
	pub fn name_descriptor<'a>(&'a self, class_file: &'a ClassFile) -> Result<(JUtf8, JUtf8), Error> {
		let name: JUtf8 = if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.name_index as usize)? {
			JUtf8(utf8.bytes.clone())
		} else {
			Err(Error::WrongConstantPoolTag)?
		};

		let descriptor: JUtf8 = if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.descriptor_index as usize)? {
			JUtf8(utf8.bytes.clone())
		} else {
			Err(Error::WrongConstantPoolTag)?
		};

		Ok((name.clone(), descriptor.clone()))
	}
}
declare_jvm_struct!(
	struct CpInfoUtf8 {
		u2 length;
		u1 bytes[length];
	}
);
declare_jvm_struct!(
	struct CpInfoMethodHandle {
		u1 reference_kind;
		u2 reference_index;
	}
);
declare_jvm_struct!(
	struct CpInfoInvokeDynamic {
		u2 bootstrap_method_attr_index;
		u2 name_and_type_index;
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CpInfo { // 4.4, Table 4.3
	Class(CpInfoClass), FieldRef(CpInfoFieldRef), MethodRef(CpInfoMethodRef), InterfaceMethodRef(CpInfoInterfaceMethodRef),
	String(CpInfoString), Integer(CpInfoInteger), Float(CpInfoFloat), Long(CpInfoLong), Double(CpInfoDouble),
	NameAndType(CpInfoNameAndType), Utf8(CpInfoUtf8), MethodHandle(CpInfoMethodHandle), InvokeDynamic(CpInfoInvokeDynamic),
}
impl<R: Read> Parse<R> for CpInfo {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, Error> {
		let tag: u8 = u8::parse(reader, constant_pool)?;
		match tag {
			7  => Ok(Self::Class(CpInfoClass::parse(reader, constant_pool)?)),
			9  => Ok(Self::FieldRef(CpInfoFieldRef::parse(reader, constant_pool)?)),
			10 => Ok(Self::MethodRef(CpInfoMethodRef::parse(reader, constant_pool)?)),
			11 => Ok(Self::InterfaceMethodRef(CpInfoInterfaceMethodRef::parse(reader, constant_pool)?)),
			8  => Ok(Self::String(CpInfoString::parse(reader, constant_pool)?)),
			3  => Ok(Self::Integer(CpInfoInteger::parse(reader, constant_pool)?)),
			4  => Ok(Self::Float(CpInfoFloat::parse(reader, constant_pool)?)),
			5  => Ok(Self::Long(CpInfoLong::parse(reader, constant_pool)?)),
			6  => Ok(Self::Double(CpInfoDouble::parse(reader, constant_pool)?)),
			12 => Ok(Self::NameAndType(CpInfoNameAndType::parse(reader, constant_pool)?)),
			1  => Ok(Self::Utf8(CpInfoUtf8::parse(reader, constant_pool)?)),
			15 => Ok(Self::MethodHandle(CpInfoMethodHandle::parse(reader, constant_pool)?)),
			18 => Ok(Self::InvokeDynamic(CpInfoInvokeDynamic::parse(reader, constant_pool)?)),
			_ => Err(Error::InvalidConstantPoolTag(tag)),
		}
	}
}
impl CpInfo {
	pub fn as_jutf8(&self) -> Result<JUtf8, Error> {
		if let Self::Utf8(utf8) = self {
			Ok(JUtf8(utf8.bytes.clone()))
		} else {
			Err(Error::InvalidConstantPoolTag(255))
		}
	}
}

declare_jvm_struct!( // 4.5
	struct FieldInfo {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    AttributeInfo  attributes[attributes_count];
	}
);

declare_jvm_struct!( // 4.7.3
	struct ExceptionTableElement {
		u2 start_pc;
		u2 end_pc;
		u2 handler_pc;
		u2 catch_type;
	}
);

declare_jvm_struct!( // 4.7.6
	struct AttributeInnerClassesInnerStruct {
		u2 inner_class_info_index;
		u2 outer_class_info_index;
		u2 inner_name_index;
		u2 inner_class_access_flags;
	}
);

declare_jvm_struct!( // 4.7.12
	struct AttributeLineNumberTableInnerStruct {
		u2 start_pc;
		u2 line_number;
	}
);

declare_jvm_struct!( // 4.7.13
	struct AttributeLocalVariableTableInnerStruct {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 descriptor_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.14
	struct AttributeLocalVariableTypeTable {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 signature_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.21
	struct AttributeBootstrapMethodsInnerStruct {
		u2 bootstrap_method_ref;
		u2 num_bootstrap_arguments;
		u2 bootstrap_arguments[num_bootstrap_arguments];
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackMapFrame(); // TODO: fill

//#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeInfo { // 4.7
	ConstantValue { // 1.0.2, 45.3
		constantvalue_index: u16,
	},
	Code { // 1.0.2, 45.3
		max_stack: u16,
		max_locals: u16,
		code_length: u32,
		code: Vec<u8>, //[code_length];
		exception_table_length: u16,
		exception_table: Vec<ExceptionTableElement>, // [exception_table_length]
		attributes_count: u16,
		attributes: Vec<AttributeInfo>, // [attributes_count]
	},
	StackMapTable { // 6, 50.0
		number_of_entries: u16,
		entries: Vec<StackMapFrame>, // [number_of_entries]
	},
	Exceptions { // 1.0.2, 45.3
		number_of_exceptions: u16,
		exception_index_table: Vec<u16>, // [number_of_exceptions]
	},
	InnerClasses { // 1.1, 45.3
		number_of_classes: u16,
		classes: Vec<AttributeInnerClassesInnerStruct>, // [number_of_classes]
	},
	EnclosingMethod { // 5.0, 49.0
		class_index: u16,
		method_index: u16,
	},
	Synthetic { }, // 1.1, 45.3
	Signature { // 5.0, 49.0
		signature_index: u16,
	},
	SourceFile { // 1.0.2, 45.3
		sourcefile_index: u16,
	},
	SourceDebugExtension { // 5.0, 49.0
		debug_extension: Vec<u8>, // [attribute_length]
	},
	LineNumberTable { // 1.0.2, 45.3
		line_number_table_length: u16,
		line_number_table: Vec<AttributeLineNumberTableInnerStruct>, // [line_number_table_length]
	},
	LocalVariableTable { // 1.0.2, 45.3
		local_variable_table_length: u16,
		local_variable_table: Vec<AttributeLocalVariableTableInnerStruct>, // [local_variable_table_length]
	},
	LocalVariableTypeTable { // 5.0, 49.0
		local_variable_type_table_length: u16,
		local_variable_type_table: Vec<AttributeLocalVariableTypeTable>, // [local_variable_type_table_length]
	},
	Deprecated { }, // 1.1, 45.3
	RuntimeVisibleAnnotations { // 5.0, 49.0
		// TODO
	},
	RuntimeInvisibleAnnotations { // 5.0, 49.0
		// TODO
	},
	RuntimeVisibleParameterAnnotations { // 5.0, 49.0
		// TODO
	},
	RuntimeInvisibleParameterAnnotations { // 5.0, 49.0
		// TODO
	},
	AnnotationDefault { // 5.0, 49.0
		// TODO
	},
	BootstrapMethods { // 7, 51.0
		num_bootstrap_methods: u16,
		bootstrap_methods: Vec<AttributeBootstrapMethodsInnerStruct>, // [num_bootstrap_methods]
	},
	Unknown {
		attribute_name_index: u16,
		attribute_length: u32,
		info: Vec<u8>, // [attribute_length];
	},
}

impl<R: Read> Parse<R> for AttributeInfo {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, Error> where Self: Sized {
		let pool = constant_pool.expect("Cannot parse AttributeInfo without a constant pool, this should not be reachable!");

		let attribute_name_index = u16::parse(reader, constant_pool)?;
		let attribute_length = u32::parse(reader, constant_pool)?;
		let attribute_name = pool.get(attribute_name_index as usize - 1)
			.ok_or_else(|| Error::InvalidConstantPoolAddress {
				address: attribute_name_index,
			})?;

		let s = if let CpInfo::Utf8(CpInfoUtf8 { bytes, ..}) = attribute_name {
			std::str::from_utf8(bytes)?
		} else {
			Err(Error::NoUtf8AtAddress(attribute_name_index))?
		};

		Ok(match (attribute_length, s) {
			(2, "ConstantValue") => Self::ConstantValue {
				constantvalue_index: u16::parse(reader, constant_pool)?,
			},
			(_, "Code") => {
				let max_stack = u16::parse(reader, constant_pool)?;
				let max_locals = u16::parse(reader, constant_pool)?;
				let code_length = u32::parse(reader, constant_pool)?;
				let code = Vec::parse_multi(reader, constant_pool, code_length as usize)?;
				let exception_table_length = u16::parse(reader, constant_pool)?;
				let exception_table = Vec::parse_multi(reader, constant_pool, exception_table_length as usize)?;
				let attributes_count = u16::parse(reader, constant_pool)?;
				let attributes = Vec::parse_multi(reader, constant_pool, attributes_count as usize)?;
				Self::Code {
					max_stack, max_locals, code_length, code, exception_table_length, exception_table, attributes_count, attributes
				}
			},
			(2, "SourceFile") => Self::SourceFile { sourcefile_index: u16::parse(reader, constant_pool)? },
			(_, "LineNumberTable") => {
				let line_number_table_length = u16::parse(reader, constant_pool)?;
				let line_number_table = Vec::parse_multi(reader, constant_pool, line_number_table_length as usize)?;
				Self::LineNumberTable {
					line_number_table_length, line_number_table
				}
			},
			(0, "Deprecated") => Self::Deprecated {},
			_ => Self::Unknown {
				attribute_name_index, attribute_length,
				info: {
					println!("unknown attr: {s}");
					let mut vec = Vec::with_capacity(attribute_length as usize);
					for _ in 0..(attribute_length as usize) {
						vec.push(u8::parse(reader, constant_pool)?);
					}
					vec
				}
			},
		})
	}
}

declare_jvm_struct!( // 4.6
	struct MethodInfo {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    AttributeInfo  attributes[attributes_count];
	}
);

impl MethodInfo {
	pub fn get_code(&self) -> Result<&AttributeInfo, Error> {
		for attribute in &self.attributes {
			match attribute {
				AttributeInfo::Code {..} => return Ok(attribute),
				_ => (),
			}
		}
		Err(Error::NoCodeAttribute)
	}

	pub fn name<'a>(&'a self, class_file: &'a ClassFile) -> Result<JUtf8, Error> {
		if let CpInfo::Utf8(utf8) = class_file.get_constant_pool(self.name_index as usize)? {
			Ok(JUtf8(utf8.bytes.clone()))
		} else {
			Err(Error::WrongConstantPoolTag)
		}
	}
}

declare_jvm_struct!( // 4.1
	struct ClassFile {
	    u2             minor_version;
	    u2             major_version;
	    u2             constant_pool_count;
	    CpInfo         constant_pool[constant_pool_count-1];
	    u2             access_flags;
	    u2             this_class;
	    u2             super_class;
	    u2             interfaces_count;
	    u2             interfaces[interfaces_count];
	    u2             fields_count;
	    FieldInfo      fields[fields_count];
	    u2             methods_count;
	    MethodInfo     methods[methods_count];
	    u2             attributes_count;
	    AttributeInfo  attributes[attributes_count];
	} {
		let constant_pool = None;
		// verify the "magic"
		let magic = u32::parse(reader, None)?;
		if magic != 0xCAFE_BABE {
			Err(Error::InvalidMagic { expected: 0xCAFE_BABE, actual: magic})?
		}
	}
);

impl ClassFile {
	pub fn get_constant_pool(&self, constant_pool_address: usize) -> Result<&CpInfo, Error> {
		self.constant_pool.get(constant_pool_address - 1)
			.ok_or(Error::InvalidConstantPoolAddress { address: constant_pool_address as u16 - 1 })
	}

	pub fn verify(&self) -> Result<(), Error> {
		Ok(())
	}

	pub fn name(&self) -> Result<JUtf8, Error> {
		if let CpInfo::Class(class) = self.get_constant_pool(self.this_class as usize)? {
			class.name(self)
		} else {
			Err(Error::WrongConstantPoolTag)
		}
	}
}

#[cfg(test)]
mod testing {
	use std::fs::File;
	use std::io::BufReader;
	use zip::ZipArchive;
	use super::{ClassFile, Parse};
	#[test]
	fn try_parse_classfile() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let classfile = ClassFile::parse(&mut &bytes[..], None).unwrap();
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
		let classfile = ClassFile::parse(&mut class_file, None).unwrap();

		println!("{classfile:#?}");

	}
}