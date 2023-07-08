use std::io::Read;
use gen::declare_jvm_struct;
use crate::classfile::{ConstantPool, CpInfo, CpInfoUtf8, Parse, parse_u1, parse_u4, parse_vec, ParseMulti};
use crate::errors::ClassFileParseError;


declare_jvm_struct!( // 4.7.2
	struct ConstantValueAttribute {
		u4 attribute_length = 2;
		u2 constantvalue_index;
	}
);

declare_jvm_struct!( // 4.7.3
	struct CodeAttribute {
		u4 attribute_length;
		u2 max_stack;
		u2 max_locals;
		u4 code_length;
		u1 code[code_length];
		u2 exception_table_length;
		ExceptionTableEntry exception_table[exception_table_length];
		u2 attributes_count;
		AttributeInfo attributes[attributes_count];
	}
);

declare_jvm_struct!( // 4.7.3, exception_table
	struct ExceptionTableEntry {
		u2 start_pc;
		u2 end_pc;
		u2 handler_pc;
		u2 catch_type;
	}
);

declare_jvm_struct!( // 4.7.4
	struct StackMapTableAttribute {
		u4 attribute_length;
		u2 number_of_entries;
		StackMapFrame entries[number_of_entries];
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationTypeInfo {
	Top, Integer, Float, Long, Double, Null, UninitializedThis,
	Object {
		constant_pool_index: u16,
	},
	Uninitialized {
		offset: u16,
	},
}

impl<R: Read> Parse<R> for VerificationTypeInfo {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let tag = u8::parse(reader, constant_pool)?;

		match tag {
			0 => Ok(Self::Top),
			1 => Ok(Self::Integer),
			2 => Ok(Self::Float),
			3 => Ok(Self::Double),
			4 => Ok(Self::Long),
			5 => Ok(Self::Null),
			6 => Ok(Self::UninitializedThis),
			7 => Ok(Self::Object {
				constant_pool_index: u16::parse(reader, constant_pool)?,
			}),
			8 => Ok(Self::Uninitialized {
				offset: u16::parse(reader, constant_pool)?,
			}),
			_ => Err(ClassFileParseError::UnknownVerificationTypeInfoTag(tag)),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackMapFrame {
	Same {
		offset_delta: u16,
	},
	SameLocals1StackItem {
		offset_delta: u16,
		stack: VerificationTypeInfo,
	},
	Chop {
		offset_delta: u16,
		k: u8,
	},
	Append {
		offset_delta: u16,
		locals: Vec<VerificationTypeInfo>,
	},
	Full {
		offset_delta: u16,
		number_of_locals: u16,
		locals: Vec<VerificationTypeInfo>, // [number_of_locals]
		number_of_stack_items: u16,
		stack: Vec<VerificationTypeInfo>, // [number_of_stack_items]
	}
}

impl<R: Read> Parse<R> for StackMapFrame {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let frame_type = u8::parse(reader, constant_pool)?;

		match frame_type {
			offset_delta @ 0..=63 => Ok(Self::Same {
				offset_delta: offset_delta as u16,
			}),
			frame_type @ 64..=127 => Ok(Self::SameLocals1StackItem {
				offset_delta: frame_type as u16 - 64,
				stack: VerificationTypeInfo::parse(reader, constant_pool)?,
			}),
			128..=246 => Err(ClassFileParseError::UnknownStackMapFrameType(frame_type)),
			247 => Ok(Self::SameLocals1StackItem {
				offset_delta: u16::parse(reader, constant_pool)?,
				stack: VerificationTypeInfo::parse(reader, constant_pool)?,
			}),
			frame_type @ 248..=250 => Ok(Self::Chop {
				offset_delta: u16::parse(reader, constant_pool)?,
				k: 251 - frame_type,
			}),
			251 => Ok(Self::Same {
				offset_delta: u16::parse(reader, constant_pool)?,
			}),
			frame_type @ 252..=254 => Ok(Self::Append {
				offset_delta: u16::parse(reader, constant_pool)?,
				locals: Vec::parse_multi(reader, constant_pool, (frame_type - 251) as usize)?,
			}),
			255 => {
				let offset_delta = u16::parse(reader, constant_pool)?;
				let number_of_locals = u16::parse(reader, constant_pool)?;
				let locals = Vec::parse_multi(reader, constant_pool, number_of_locals as usize)?;
				let number_of_stack_items = u16::parse(reader, constant_pool)?;
				let stack = Vec::parse_multi(reader, constant_pool, number_of_stack_items as usize)?;

				Ok(Self::Full { offset_delta, number_of_locals, locals, number_of_stack_items, stack })
			},
		}
	}
}

declare_jvm_struct!( // 4.7.5
	struct ExceptionsAttribute {
		u4 attribute_length;
		u2 number_of_exceptions;
		u2 exception_index_table[number_of_exceptions];
	}
);

declare_jvm_struct!( // 4.7.6
	struct InnerClassesAttribute {
		u4 attribute_length;
		u2 number_of_classes;
		InnerClassesAttributeClassesElement classes[number_of_classes];
	}
);

declare_jvm_struct!( // 4.7.6, classes
	struct InnerClassesAttributeClassesElement {
		u2 inner_class_info_index;
		u2 outer_class_info_index;
		u2 inner_name_index;
		u2 inner_class_access_flags;
	}
);

declare_jvm_struct!( // 4.7.7
	struct EnclosingMethodAttribute {
		u4 attribute_length = 4;
		u2 class_index;
		u2 method_index;
	}
);

declare_jvm_struct!( // 4.7.6
	struct SyntheticAttribute {
		u4 attribute_length = 0;
	}
);

declare_jvm_struct!( // 4.7.9
	struct SignatureAttribute {
		u4 attribute_length = 2;
		u2 signature_index;
	}
);

declare_jvm_struct!( // 4.7.10
	struct SourceFileAttribute {
		u4 attribute_length = 2;
		u2 sourcefile_index;
	}
);

declare_jvm_struct!( // 4.7.11
	struct SourceDebugExtensionsAttribute {
		u4 attribute_length;
		u1 debug_extension[attribute_length];
	}
);

declare_jvm_struct!( // 4.7.12
	struct LineNumberTableAttribute {
		u4 attribute_length;
		u2 line_number_table_length;
		LineNumberTableEntry line_number_table[line_number_table_length];
	}
);

declare_jvm_struct!( // 4.7.12, line_number_table
	struct LineNumberTableEntry {
		u2 start_pc;
		u2 line_number;
	}
);

declare_jvm_struct!( // 4.7.13
	struct LocalVariableTableAttribute {
		u4 attribute_length;
		u2 local_variable_table_length;
		LocalVariableTableEntry local_variable_table[local_variable_table_length];
	}
);

declare_jvm_struct!( // 4.7.13, local_variable_table
	struct LocalVariableTableEntry {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 descriptor_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.14
	struct LocalVariableTypeTableAttribute {
		u4 attribute_length;
		u2 local_variable_type_table_length;
		LocalVariableTypeTableEntry local_variable_type_table[local_variable_type_table_length];
	}
);

declare_jvm_struct!( // 4.7.14, local_variable_type_table
	struct LocalVariableTypeTableEntry {
		u2 start_pc;
		u2 length;
		u2 name_index;
		u2 signature_index;
		u2 index;
	}
);

declare_jvm_struct!( // 4.7.15
	struct DeprecatedAttribute {
		u4 attribute_length = 0;
	}
);

declare_jvm_struct!( // 4.7.16
	struct RuntimeVisibleAnnotationsAttribute {
		u4         attribute_length;
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.16, annotations
	struct Annotation {
		u2 type_index;
		u2 num_element_value_pairs;
		AnnotationElementValuePair element_value_pairs[num_element_value_pairs];
	}
);

declare_jvm_struct!( // 4.7.16, element_value_pairs
	struct AnnotationElementValuePair {
		u2            element_name_index;
		AnnotationElementValue value;
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnnotationElementValue { // 4.7.16.1, value
ConstantValueIndex { // B, C, D, F, I, J, S, Z, s
const_value_index: u16,
},
	EnumConstValue { // e
	type_name_index: u16,
		const_name_index: u16,
	},
	ClassInfoIndex { // c
	class_info_index: u16,
	},
	AnnotationValue { // @
	annotation_value: Annotation,
	},
	ArrayValue { // [
	num_values: u16,
		values: Vec<AnnotationElementValue>, // [num_values]
	}
}

impl<R: Read> Parse<R> for AnnotationElementValue {
	fn parse(reader: &mut R, constant_pool: Option<&Vec<CpInfo>>) -> Result<Self, ClassFileParseError> where Self: Sized {
		let tag = u8::parse(reader, constant_pool)?;

		let tag = tag.try_into().unwrap(); // TODO: fix this, this should be handled gracefully!

		Ok(match tag {
			'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' | 's' => Self::ConstantValueIndex {
				const_value_index: u16::parse(reader, constant_pool)?,
			},
			'e' => Self::EnumConstValue {
				type_name_index: u16::parse(reader, constant_pool)?,
				const_name_index: u16::parse(reader, constant_pool)?,
			},
			'c' => Self::ClassInfoIndex {
				class_info_index: u16::parse(reader, constant_pool)?,
			},
			'@' => Self::AnnotationValue {
				annotation_value: Annotation::parse(reader, constant_pool)?,
			},
			'[' => {
				let num_values = u16::parse(reader, constant_pool)?;
				let values = Vec::parse_multi(reader, constant_pool, num_values as usize)?;
				Self::ArrayValue { num_values, values }
			},
			_ => Err(ClassFileParseError::UnknownAnnotationElementValueTag(tag as u8))?
		})
	}
}

declare_jvm_struct!( // 4.7.17
	struct RuntimeInvisibleAnnotationsAttribute {
		u4         attribute_length;
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.18
	struct RuntimeVisibleParameterAnnotationsAttribute {
		u4 attribute_length;
		u1 num_parameters;
		ParameterAnnotationPair parameter_annotations[num_parameters];
	}
);

declare_jvm_struct!( // 4.7.18, parameter_annotations
	struct ParameterAnnotationPair {
		u2         num_annotations;
		Annotation annotations[num_annotations];
	}
);

declare_jvm_struct!( // 4.7.19
	struct RuntimeInvisibleParameterAnnotationsAttribute {
		u4 attribute_length;
		u1 num_parameters;
		ParameterAnnotationPair parameter_annotations[num_parameters];
	}
);

declare_jvm_struct!( // 4.7.20
	struct AnnotationDefaultAttribute {
		u4            attribute_length;
    	AnnotationElementValue default_value;
	}
);

declare_jvm_struct!( // 4.7.21
	struct BootstrapMethodsAttribute {
		u4 attribute_length;
		u2 num_bootstrap_methods;
		BootstrapMethodsAttributeEntry bootstrap_methods[num_bootstrap_methods];
	}
);

declare_jvm_struct!( // 4.7.21, bootstrap_methods
	struct BootstrapMethodsAttributeEntry {
		u2 bootstrap_method_ref;
		u2 num_bootstrap_arguments;
		u2 bootstrap_arguments[num_bootstrap_arguments];
	}
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeInfo { // 4.7
ConstantValue(ConstantValueAttribute), // 1.0.2, 45.3
Code(CodeAttribute), // 1.0.2, 45.3
StackMapTable(StackMapTableAttribute), // 6, 50.0
Exceptions(ExceptionsAttribute), // 1.0.2, 45.3
InnerClasses(InnerClassesAttribute), // 1.1, 45.3
EnclosingMethod(EnclosingMethodAttribute), // 5.0, 49.0
Synthetic(SyntheticAttribute), // 1.1, 45.3
Signature(SignatureAttribute), // 5.0, 49.0
SourceFile(SourceFileAttribute), // 1.0.2, 45.3
SourceDebugExtension(SourceDebugExtensionsAttribute), // 5.0, 49.0
LineNumberTable(LineNumberTableAttribute), // 1.0.2, 45.3
LocalVariableTable(LocalVariableTableAttribute), // 1.0.2, 45.3
LocalVariableTypeTable(LocalVariableTypeTableAttribute), // 5.0, 49.0
Deprecated(DeprecatedAttribute), // 1.1, 45.3
RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute), // 5.0, 49.0
RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute), // 5.0, 49.0
RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute), // 5.0, 49.0
RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute), // 5.0, 49.0
AnnotationDefault(AnnotationDefaultAttribute), // 5.0, 49.0
BootstrapMethods(BootstrapMethodsAttribute), // 7, 51.0
Unknown {
	name: &'static str,
	info: Vec<u8>,
},
}

impl AttributeInfo {
	pub fn parse<R: Read>(reader: &mut R, constant_pool: &ConstantPool) -> Result<Self, ClassFileParseError> {
		let s: CpInfoUtf8 = constant_pool.parse_index(reader)?;

		let pool = Some(&constant_pool.0);
		Ok(match std::str::from_utf8(&s.bytes)? {
			"ConstantValue" => Self::ConstantValue(ConstantValueAttribute::parse(reader, pool)?),
			"Code" => Self::Code(CodeAttribute::parse(reader, pool)?),
			"StackMapTable" => Self::StackMapTable(StackMapTableAttribute::parse(reader, pool)?),
			"Exceptions" => Self::Exceptions(ExceptionsAttribute::parse(reader, pool)?),
			"InnerClasses" => Self::InnerClasses(InnerClassesAttribute::parse(reader, pool)?),
			"EnclosingMethod" => Self::EnclosingMethod(EnclosingMethodAttribute::parse(reader, pool)?),
			"Synthetic" => Self::Synthetic(SyntheticAttribute::parse(reader, pool)?),
			"Signature" => Self::Signature(SignatureAttribute::parse(reader, pool)?),
			"SourceFile" => Self::SourceFile(SourceFileAttribute::parse(reader, pool)?),
			"SourceDebugExtension" => Self::SourceDebugExtension(SourceDebugExtensionsAttribute::parse(reader, pool)?),
			"LineNumberTable" => Self::LineNumberTable(LineNumberTableAttribute::parse(reader, pool)?),
			"LocalVariableTable" => Self::LocalVariableTable(LocalVariableTableAttribute::parse(reader, pool)?),
			"LocalVariableTypeTable" => Self::LocalVariableTypeTable(LocalVariableTypeTableAttribute::parse(reader, pool)?),
			"Deprecated" => Self::Deprecated(DeprecatedAttribute::parse(reader, pool)?),
			"RuntimeVisibleAnnotations" => Self::RuntimeVisibleAnnotations(RuntimeVisibleAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeInvisibleAnnotations" => Self::RuntimeInvisibleAnnotations(RuntimeInvisibleAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeVisibleParameterAnnotations" => Self::RuntimeVisibleParameterAnnotations(RuntimeVisibleParameterAnnotationsAttribute::parse(reader, pool)?),
			"RuntimeInvisibleParameterAnnotations" => Self::RuntimeInvisibleParameterAnnotations(RuntimeInvisibleParameterAnnotationsAttribute::parse(reader, pool)?),
			"AnnotationDefault" => Self::AnnotationDefault(AnnotationDefaultAttribute::parse(reader, pool)?),
			"BootstrapMethods" => Self::BootstrapMethods(BootstrapMethodsAttribute::parse(reader, pool)?),
			name => {
				let info = parse_vec(reader,
				 |r| Ok(parse_u4(r)? as usize),
				 |r| parse_u1(r)
				)?;
				eprintln!("WARN: unknown attr: {name}: {info:?}");
				Self::Unknown { name, info }
			},
		})
	}
}
