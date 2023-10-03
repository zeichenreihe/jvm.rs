use anyhow::{bail, Result};
use std::iter::Peekable;
use crate::name::ClassName;

// TODO: this is not good...
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BaseOrObjectType {
	B, C, D, F, I, J, S, Z,
	Object(ClassName),
}

/// ```
/// FieldDescriptor:
///   FieldType
///
/// FieldType:
///   [ FieldType
///   L ClassName ;
///   B
///   C
///   D
///   F
///   I
///   J
///   S
///   Z
/// ```
/// A field descriptor representing an array type is valid only if it represents a type with 255 or fewer dimensions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDescriptor {
	pub array_dimension: usize,
	pub base_type: BaseOrObjectType, // TODO: this needs change!
}

impl FieldDescriptor {
	fn parse_iter<'a>(iter: &mut Peekable<impl Iterator<Item=&'a u8>>, value: &[u8]) -> Result<FieldDescriptor> {
		let mut array_dimension = 0;
		while let Some(b'[') = iter.peek().cloned().cloned() {
			iter.next();
			array_dimension += 1;
			if array_dimension >= 256 {
				bail!("array dimension not <= 255: '{}'", String::from_utf8_lossy(value));
			}
		}

		let base_type = match iter.next() {
			Some(b'L') => {
				let mut vec: Vec<u8> = Vec::new();

				loop {
					match iter.next() {
						Some(b';') => break, // class name ended
						Some(x) => vec.push(*x),
						None => bail!("unexpected end: '{}'", String::from_utf8_lossy(value)),
					}
				}

				let class_name = ClassName::from(&vec[..]);

				BaseOrObjectType::Object(class_name)
			},
			Some(b'B') => BaseOrObjectType::B,
			Some(b'C') => BaseOrObjectType::C,
			Some(b'D') => BaseOrObjectType::D,
			Some(b'F') => BaseOrObjectType::F,
			Some(b'I') => BaseOrObjectType::I,
			Some(b'J') => BaseOrObjectType::J,
			Some(b'S') => BaseOrObjectType::S,
			Some(b'Z') => BaseOrObjectType::Z,
			Some(_) => bail!("invalid field type: '{}'", String::from_utf8_lossy(value)),
			None => bail!("unexpected end: '{}'", String::from_utf8_lossy(value)),
		};

		Ok(FieldDescriptor {
			array_dimension,
			base_type,
		})
	}
}

impl TryFrom<&[u8]> for FieldDescriptor {
	type Error = anyhow::Error;

	fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
		let mut iter = value.iter().peekable();

		let descriptor = FieldDescriptor::parse_iter(&mut iter, value)?;

		if !matches!(iter.next(), None) {
			bail!("field descriptor doesn't end: '{}'", String::from_utf8_lossy(value));
		}

		Ok(descriptor)
	}
}

/// ```
/// MethodDescriptor:
///   ( FieldType* ) ReturnDescriptor
/// ReturnDescriptor:
///   FieldType
///   VoidDescriptor:
///     V
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodDescriptor {
	pub parameters: Vec<FieldDescriptor>,
	/// A value of `None` indicates the type `void`.
	pub return_type: Option<FieldDescriptor>,
}

impl TryFrom<&[u8]> for MethodDescriptor {
	type Error = anyhow::Error;

	fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
		let mut iter = value.into_iter().peekable();

		let mut parameters = Vec::new();

		if !matches!(iter.next(), Some(b'(')) {
			bail!("no opening parenthesis in method descriptor: '{}'", String::from_utf8_lossy(value));
		}

		loop {
			match iter.peek() {
				Some(b')') => break,
				_ => parameters.push(FieldDescriptor::parse_iter(&mut iter, value)?),
			}
		}

		if !matches!(iter.next(), Some(b')')) {
			bail!("no closing parenthesis in method descriptor: '{}'", String::from_utf8_lossy(value));
		}

		let return_type = if matches!(iter.peek(), Some(b'V')) {
			iter.next(); // consume 'V'
			None
		} else {
			Some(FieldDescriptor::parse_iter(&mut iter, value)?)
		};

		if !matches!(iter.next(), None) {
			bail!("method descriptor doesn't end: '{}'", String::from_utf8_lossy(value));
		}

		Ok(MethodDescriptor {
			parameters,
			return_type
		})
	}
}

#[cfg(test)]
mod test {
	use crate::descriptor::{BaseOrObjectType, FieldDescriptor, MethodDescriptor};
	use crate::name::ClassName;

	#[test]
	fn parse_field_descriptor() {
		let desc = b"[[L[[net//<init>::/this_is_a_\"test$$023$()/Class;";
		assert_eq!(FieldDescriptor::parse(desc), Ok(FieldDescriptor {
			array_dimension: 2,
			base_type: BaseOrObjectType::Object(
				ClassName::from(b"[[net//<init>::/this_is_a_\"test$$023$()/Class")
			),
		}));
	}

	#[test]
	fn parse_method_descriptor() {
		let desc = b"(I[[[Z[L[[net//<init>::/this_is_a_\"test$$023$()/Class;IB)V";
		assert_eq!(MethodDescriptor::parse(&desc), Ok(MethodDescriptor {
			parameters: vec![
				FieldDescriptor {
					array_dimension: 0,
					base_type: BaseOrObjectType::I,
				},
				FieldDescriptor {
					array_dimension: 3,
					base_type: BaseOrObjectType::Z,
				},
				FieldDescriptor {
					array_dimension: 1,
					base_type: BaseOrObjectType::Object(
						ClassInfo::from(b"[[net//<init>::/this_is_a_\"test$$023$()/Class")
					)
				},
				FieldDescriptor {
					array_dimension: 0,
					base_type: BaseOrObjectType::I,
				},
				FieldDescriptor {
					array_dimension: 0,
					base_type: BaseOrObjectType::B,
				}
			],
			return_type: None,
		}));
	}
}