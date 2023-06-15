use std::io::Read;

use gen::declare_jvm_struct;

#[derive(Debug)]
enum Error {
	ReadWrongAmountOfData{ actual: usize, expected: usize, string: &'static str},
	IoError(std::io::Error)
}

impl From<std::io::Error> for Error {
	fn from(value: std::io::Error) -> Self {
		Self::IoError(value)
	}
}

trait Load {
	fn load<R: Read>(reader: &mut R) -> Result<Self, Error> where Self: Sized;
}

declare_jvm_struct!(
	CpInfo {
	    u1 tag;
	    u1 info[];
	}
);

declare_jvm_struct!(
	FieldInfo {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    AttributeInfo  attributes[attributes_count];
	}
);

declare_jvm_struct!(
	AttributeInfo {
	    u2 attribute_name_index;
	    u4 attribute_length;
	    u1 info[attribute_length];
	}
);

declare_jvm_struct!(
	MethodInfo {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    AttributeInfo  attributes[attributes_count];
	}
);

declare_jvm_struct!(
	ClassFile {
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
		let bytes = include_bytes!("../../java_example_classfiles/Test.java");
		let classfile = ClassFile::load(&mut &bytes[..]).unwrap();
		dbg!(classfile);
	}
}