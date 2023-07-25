

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
use crate::classfile::{ClassInfo, Utf8Info};
use crate::errors::DescriptorParseError;

#[derive(Debug, Clone, PartialEq)]
pub enum BaseOrObjectType {
	B, C, D, F, I, J, S, Z,
	Object(ClassInfo),
}
impl BaseOrObjectType {
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(mut iter: T) -> Result<BaseOrObjectType, DescriptorParseError> {
		Ok(match iter.next() {
			Some(b'L') => {
				let bytes: Vec<u8> = iter
					.peeking_take_while(|&ch| ch != b';')
					.collect();
				if let Some(b';') = iter.next() {} else {
					return Err(DescriptorParseError::NoSemicolonFound(iter.collect()));
				}
				BaseOrObjectType::Object(ClassInfo {
					name: Utf8Info {
						bytes
					},
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
			Some(x) => return Err(DescriptorParseError::InvalidDescriptor(x, iter.collect())),
			None => return Err(DescriptorParseError::UnexpectedEnd(iter.collect())),
		})
	}

	/// gets the size in how many lv slots are needed
	pub fn get_size(&self) -> usize {
		match self {
			BaseOrObjectType::B => 1,
			BaseOrObjectType::C => 1,
			BaseOrObjectType::D => 2,
			BaseOrObjectType::F => 1,
			BaseOrObjectType::I => 1,
			BaseOrObjectType::J => 2,
			BaseOrObjectType::S => 1,
			BaseOrObjectType::Z => 1,
			BaseOrObjectType::Object(_) => 1,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDescriptor {
	pub array_dimension: u8,
	pub base_or_object_type: BaseOrObjectType,
}

impl FieldDescriptor {
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(iter: &mut T) -> Result<FieldDescriptor, DescriptorParseError> {
		let array_index = iter.peeking_take_while(|&ch| ch == b'[').count();
		if array_index > 255 {
			return Err(DescriptorParseError::ArrayDimensionTooLarge(array_index, iter.collect()));
		}

		Ok(FieldDescriptor {
			array_dimension: array_index as u8,
			base_or_object_type: BaseOrObjectType::parse_iter(iter)?,
		})
	}
	pub fn parse(descriptor: &Utf8Info) -> Result<FieldDescriptor, DescriptorParseError> {
		let mut iter = descriptor.bytes.iter().copied().peekable();

		let descriptor = FieldDescriptor::parse_iter(&mut iter)?;

		if let Some(_) = iter.peek() {
			return Err(DescriptorParseError::UnexpectedEnd(iter.collect()));
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
	pub fn parse(descriptor: &Utf8Info) -> Result<MethodDescriptor, DescriptorParseError> {
		let mut iter = descriptor.bytes.iter().copied().peekable();

		let mut parameters = Vec::new();

		match iter.peek() {
			Some(b'(') => { let _parenthesis = iter.next(); },
			_ => return Err(DescriptorParseError::NoOpeningParenthesisFound(descriptor.bytes.clone()))
		}

		loop {
			match iter.peek() {
				Some(b')') => break,
				None => return Err(DescriptorParseError::UnexpectedEnd(descriptor.bytes.clone())),
				_ => {},
			}

			parameters.push(FieldDescriptor::parse_iter(&mut iter)?);
		}

		if let Some(b')') = iter.next() {} else {
			return Err(DescriptorParseError::NoClosingParenthesisFound(descriptor.bytes.clone()));
		}

		let return_array_index = iter.peeking_take_while(|&ch| ch == b'[').count();
		if return_array_index > 255 {
			return Err(DescriptorParseError::ArrayDimensionTooLarge(return_array_index, descriptor.bytes.clone()));
		}

		let return_descriptor = ReturnDescriptor::parse_iter(&mut iter)?;

		if matches!(return_descriptor, ReturnDescriptor::V) {
			if return_array_index > 0 {
				todo!("if it's V, you can't have an array of it...") // make the design better, types!
			}
		}

		if let Some(_) = iter.next() {
			return Err(DescriptorParseError::UnexpectedEnd(iter.collect()));
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
	fn parse_iter<T: Iterator<Item = u8> + PeekingNext>(iter: &mut T) -> Result<ReturnDescriptor, DescriptorParseError> {
		Ok(match iter.next() {
			Some(b'L') => {
				let bytes: Vec<u8> = iter
					.peeking_take_while(|&ch| ch != b';')
					.collect();
				if let Some(b';') = iter.next() {} else {
					return Err(DescriptorParseError::NoSemicolonFound(iter.collect()));
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
			Some(x) => return Err(DescriptorParseError::InvalidDescriptor(x, iter.collect())),
			None => return Err(DescriptorParseError::UnexpectedEnd(iter.collect())),
		})
	}
}

#[cfg(test)]
mod testing {
	use crate::classfile::{ClassInfo, Utf8Info};
	use crate::types::descriptor::{BaseOrObjectType, FieldDescriptor, MethodDescriptor, ReturnDescriptor};

	#[test]
	fn parse_field_descriptor() {
		let desc = Utf8Info::from("[[L[[net//<init>::/this_is_a_\"test$$023$()/Class;");
		assert_eq!(FieldDescriptor::parse(&desc).unwrap(), FieldDescriptor {
			array_dimension: 2,
			base_or_object_type: BaseOrObjectType::Object(
				ClassInfo::from("[[net//<init>::/this_is_a_\"test$$023$()/Class")
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
						ClassInfo::from("[[net//<init>::/this_is_a_\"test$$023$()/Class")
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