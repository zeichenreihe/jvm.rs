use zerocopy::{AsBytes, FromBytes};

use gen::declare_jvm_struct;

declare_jvm_struct!(
	cp_info {
	    u1 tag;
	    u1 info[];
	}
);

declare_jvm_struct!(
	field_info {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    attribute_info attributes[attributes_count];
	}
);

declare_jvm_struct!(
	attribute_info {
	    u2 attribute_name_index;
	    u4 attribute_length;
	    u1 info[attribute_length];
	}
);

declare_jvm_struct!(
	method_info {
	    u2             access_flags;
	    u2             name_index;
	    u2             descriptor_index;
	    u2             attributes_count;
	    attribute_info attributes[attributes_count];
	}
);

declare_jvm_struct!(
	ClassFile {
	    u4             magic;
	    u2             minor_version;
	    u2             major_version;
	    u2             constant_pool_count;
	    cp_info        constant_pool[constant_pool_count-1];
	    u2             access_flags;
	    u2             this_class;
	    u2             super_class;
	    u2             interfaces_count;
	    u2             interfaces[interfaces_count];
	    u2             fields_count;
	    field_info     fields[fields_count];
	    u2             methods_count;
	    method_info    methods[methods_count];
	    u2             attributes_count;
	    attribute_info attributes[attributes_count];
	}
);

impl ClassFile {

}

#[cfg(test)]
mod testing {
	#[test]
	fn try_parse_classfile() {

	}
}