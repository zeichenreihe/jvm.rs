use std::io::Read;

use gen::declare_jvm_struct;

#[derive(Debug)]
enum Error {
	ReadWrongAmountOfDataStr { actual: usize, expected: usize, string: &'static str },
	ReadWrongAmountOfData { actual: usize, expected: usize },
	ReadWrongTag { actual: u32, string: &'static str },
	IoError(std::io::Error),
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::IoError(value)
	}
}

trait Load {
	fn load<R: Read>(reader: &mut R) -> Result<Self, Error> where Self: Sized;
}

macro_rules! impl_load_for {
	($t:ty, $n:literal) => {
		impl Load for $t {
			fn load<R: Read>(reader: &mut R) -> Result<Self, Error> where Self: Sized {
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

impl_load_for!(u8, 1);
impl_load_for!(u16, 2);
impl_load_for!(u32, 4);

declare_jvm_struct!( // 4.4, Table 4.3
	enum CpInfo (u1) {
		7 = Class {
    		u2 name_index;
		},
		9 = FieldRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		10 = MethodRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		11 = InterfaceMethodRef {
			u2 class_index;
			u2 name_and_type_index;
		},
		8 = String {
			u2 string_index;
		},
		3 = Integer {
			u4 bytes;
		},
		4 = Float {
			u4 bytes;
		},
		5 = Long {
			u4 high_bytes;
			u4 low_bytes;
		},
		6 = Double {
			u4 high_bytes;
			u4 low_bytes;
		},
		12 = NameAndType {
			u2 name_index;
			u2 descriptor_index;
		},
		1 = Utf8 {
			u2 length;
			u1 bytes[length];
		},
		15 = MethodHandle {
			u1 reference_kind;
			u2 reference_index;
		},
		18 = InvokeDynamic {
			u2 bootstrap_method_attr_index;
			u2 name_and_type_index;
		},
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

declare_jvm_struct!( // 4.7
	struct AttributeInfo {
	    u2 attribute_name_index;
	    u4 attribute_length;
	    u1 info[attribute_length];
	}
);

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
	    u4             magic;
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
	}
);

#[cfg(test)]
mod testing {
	use super::{ClassFile, Load};
	#[test]
	fn try_parse_classfile() {
		let bytes = include_bytes!("../../java_example_classfiles/Test.class");
		let classfile = ClassFile::load(&mut &bytes[..]).unwrap();
		dbg!(classfile);
	}
}