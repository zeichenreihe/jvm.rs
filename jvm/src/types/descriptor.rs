

/*

FieldDescriptor:
    FieldType

FieldType:
    BaseType:
		B
		C
		D
		F
		I
		J
		S
		Z
    ObjectType:
        L ClassName ;
	ArrayType:
		[ ComponentType:
		    FieldType
A field descriptor representing an array type is valid only if it represents a type with 255 or fewer dimensions.

MethodDescriptor:
    ( ParameterDescriptor* ) ReturnDescriptor

ParameterDescriptor:
    FieldType

ReturnDescriptor:
    FieldType
    VoidDescriptor:
        V

 */

use itertools::{Itertools, PeekingNext};
use crate::classfile::ConstantPoolElement::FieldRef;
use crate::classfile::Utf8Info;

#[derive(Debug, Clone, PartialEq)]
pub enum BaseOrObjectType {
	B, C, D, F, I, J, S, Z,
	Object(Utf8Info),
}
impl BaseOrObjectType {
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(mut iter: T) -> Result<BaseOrObjectType, ()> {
		Ok(match iter.next() {
			Some(b'L') => {
				let bytes: Vec<u8> = iter
					.peeking_take_while(|&ch| ch != b';')
					.collect();
				if let Some(b';') = iter.next() {} else {
					panic!("no semicolon at the end!");
				}
				BaseOrObjectType::Object(Utf8Info {
					bytes,
				})
			},
			Some(b'B') => BaseOrObjectType::B,
			Some(b'C') => BaseOrObjectType::C,
			Some(b'D') => BaseOrObjectType::D,
			Some(b'F') => BaseOrObjectType::F,
			Some(b'I') => BaseOrObjectType::I,
			Some(b'J') => BaseOrObjectType::J,
			Some(b'S') => BaseOrObjectType::S,
			Some(b'Z') => BaseOrObjectType::Z,
			_ => panic!("not a valid desc"),
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDescriptor {
	pub array_dimension: u8,
	pub base_or_object_type: BaseOrObjectType,
}

impl FieldDescriptor {
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(iter: &mut T) -> Result<FieldDescriptor, ()> {
		let array_index = iter.peeking_take_while(|&ch| ch == b'[').count();
		if array_index > 255 {
			panic!("array index may not be >255");
		}

		Ok(FieldDescriptor {
			array_dimension: array_index as u8,
			base_or_object_type: BaseOrObjectType::parse_iter(iter)?,
		})
	}
	pub fn parse(descriptor: &Utf8Info) -> Result<FieldDescriptor, ()> {
		let mut iter = descriptor.bytes.iter().copied().peekable();

		let descriptor = FieldDescriptor::parse_iter(&mut iter)?;

		if let Some(_) = iter.next() {
			panic!("field descriptor must end");
		}

		Ok(descriptor)
	}
}


#[derive(Debug, Clone, PartialEq)]
pub struct MethodDescriptor {
	pub parameters: Vec<FieldDescriptor>,
	pub return_array_dimension: u8,
	pub return_descriptor: ReturnDescriptor,
}

impl MethodDescriptor {
	pub fn parse(descriptor: &Utf8Info) -> Result<MethodDescriptor, ()> {
		let mut iter = descriptor.bytes.iter().copied().peekable();

		let mut parameters = Vec::new();

		if let Some(b'(') = iter.next() {} else {
			panic!("method desc doesn't start with '('.");
		}

		loop {
			match iter.peek() {
				Some(b')') | None => break,
				_ => {},
			}

			parameters.push(FieldDescriptor::parse_iter(&mut iter)?);
		}

		if let Some(b')') = iter.next() {} else {
			panic!("method desc doesn't contain ')'.");
		}

		// TODO: if it's V, you can't have an array of it...
		let return_array_index = iter.peeking_take_while(|&ch| ch == b'[').count();
		if return_array_index > 255 {
			panic!("array index may not be >255");
		}

		let return_descriptor = ReturnDescriptor::parse_iter(&mut iter)?;

		if let Some(_) = iter.next() {
			panic!("method descriptor must end");
		}

		Ok(MethodDescriptor {
			parameters,
			return_array_dimension: return_array_index as u8,
			return_descriptor,
		})
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnDescriptor {
	B, C, D, F, I, J, S, V, Z,
	Object(Utf8Info),
}
impl ReturnDescriptor {
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(iter: &mut T) -> Result<ReturnDescriptor, ()> {
		Ok(match iter.next() {
			Some(b'L') => {
				let bytes: Vec<u8> = iter
					.peeking_take_while(|&ch| ch != b';')
					.collect();
				if let Some(b';') = iter.next() {} else {
					panic!("no semicolon at the end!");
				}
				ReturnDescriptor::Object(Utf8Info {
					bytes,
				})
			},
			Some(b'B') => ReturnDescriptor::B,
			Some(b'C') => ReturnDescriptor::C,
			Some(b'D') => ReturnDescriptor::D,
			Some(b'F') => ReturnDescriptor::F,
			Some(b'I') => ReturnDescriptor::I,
			Some(b'J') => ReturnDescriptor::J,
			Some(b'S') => ReturnDescriptor::S,
			Some(b'V') => ReturnDescriptor::V, // void
			Some(b'Z') => ReturnDescriptor::Z,
			_ => panic!("not a valid desc"),
		})
	}
}

#[cfg(test)]
mod testing {
	use crate::classfile::Utf8Info;
	use crate::types::descriptor::{BaseOrObjectType, FieldDescriptor, MethodDescriptor, ReturnDescriptor};

	#[test]
	fn parse_field_descriptor() {
		let desc = Utf8Info::from("[[L[[net//<init>::/this_is_a_\"test$$023$()/Class;");
		assert_eq!(FieldDescriptor::parse(&desc).unwrap(), FieldDescriptor {
			array_dimension: 2,
			base_or_object_type: BaseOrObjectType::Object(
				Utf8Info::from("[[net//<init>::/this_is_a_\"test$$023$()/Class")
			),
		});
	}

	#[test]
	fn parse_method_descriptor() {
		let desc = Utf8Info::from("(I[[[Z[L[[net//<init>::/this_is_a_\"test$$023$()/Class;IB)V");
		assert_eq!(MethodDescriptor::parse(&desc).unwrap(), MethodDescriptor {
			parameters: vec![
				FieldDescriptor {
					array_dimension: 0,
					base_or_object_type: BaseOrObjectType::I,
				},
				FieldDescriptor {
					array_dimension: 3,
					base_or_object_type: BaseOrObjectType::Z,
				},
				FieldDescriptor {
					array_dimension: 1,
					base_or_object_type: BaseOrObjectType::Object(
						Utf8Info::from("[[net//<init>::/this_is_a_\"test$$023$()/Class")
					)
				},
				FieldDescriptor {
					array_dimension: 0,
					base_or_object_type: BaseOrObjectType::I,
				},
				FieldDescriptor {
					array_dimension: 0,
					base_or_object_type: BaseOrObjectType::B,
				}
			],
			return_array_dimension: 0,
			return_descriptor: ReturnDescriptor::V,
		});
	}
}