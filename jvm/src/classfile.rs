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

declare_jvm_struct!( // 4.4, Table 4.3
	enum CpInfo {
		Class {
    		u2 name_index;
		},
		FieldRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		MethodRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		InterfaceMethodRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		String {
			u2 string_index;
		},
		Integer {
			u4 bytes;
		},
		Float {
			u4 bytes;
		},
		Long {
			u4 high_bytes;
			u4 low_bytes;
		},
		Double {
			u4 high_bytes;
			u4 low_bytes;
		},
		NameAndType {
			u2 name_index;
			u2 descriptor_index;
		},
		Utf8 {
			u2 length;
			u1 bytes[length];
		},
		MethodHandle {
			u1 reference_kind;
			u2 reference_index;
		},
		InvokeDynamic {
			u2 bootstrap_method_attr_index;
			u2 name_and_type_index;
		},
	}

	impl<R: Read> Parse<R> for CpInfo {
		fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, Error> {
			let tag: u8 = u8::parse(reader, constant_pool)?;
			match tag {
				7  => Ok(Self::Class),
				9  => Ok(Self::FieldRef),
				10 => Ok(Self::MethodRef),
				11 => Ok(Self::InterfaceMethodRef),
				8  => Ok(Self::String),
				3  => Ok(Self::Integer),
				4  => Ok(Self::Float),
				5  => Ok(Self::Long),
				6  => Ok(Self::Double),
				12 => Ok(Self::NameAndType),
				1  => Ok(Self::Utf8),
				15 => Ok(Self::MethodHandle),
				18 => Ok(Self::InvokeDynamic),
				_ => Err(Error::InvalidConstantPoolTag(tag)),
			}
		}
	}
);

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

		let s = if let CpInfo::Utf8 { length: _, bytes } = attribute_name {
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
			}
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

		println!("{:?}", classfile);
	}
}