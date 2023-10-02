use std::borrow::Borrow;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use crate::classfile::name::ClassName;

#[derive(Debug)]
pub struct DescriptorParseError(&'static str, Vec<u8>);
impl Error for DescriptorParseError {}
impl Display for DescriptorParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str(self.0)?;
		f.write_str(": '")?;
		f.write_str(&*String::from_utf8_lossy(&*self.1))?;
		f.write_str("'")
	}
}


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
	fn parse_iter<I>(iter: &mut Peekable<I>, value: &[u8]) -> Result<FieldDescriptor, DescriptorParseError>
	where
		I: Iterator,
		I::Item: Borrow<u8>,
	{
		let mut array_dimension = 0;
		while let Some(b'[') = iter.peek() {
			iter.next();
			array_dimension += 1;
			if array_dimension >= 256 {
				return Err(DescriptorParseError("array dimension not <= 255", value.to_vec()));
			}
		}

		let base_type = match iter.next() {
			Some(b'L') => {
				let mut vec: Vec<u8> = Vec::new();

				loop {
					match iter.next() {
						Some(b';') => break, // class name ended
						Some(x) => vec.push(x),
						None => Err(DescriptorParseError("unexpected end", value.to_vec()))?,
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
			Some(_) => Err(DescriptorParseError("invalid field type", value.to_vec()))?,
			None => Err(DescriptorParseError("unexpected end", value.to_vec()))?,
		};

		Ok(FieldDescriptor {
			array_dimension,
			base_type,
		})
	}
}

impl TryFrom<&[u8]> for FieldDescriptor {
	type Error = DescriptorParseError;

	fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
		let mut iter = value.iter();

		let descriptor = FieldDescriptor::parse_iter(&mut iter, value)?;

		if !matches!(iter.next(), None) {
			return Err(DescriptorParseError("field descriptor doesn't end", value.to_vec()));
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
	type Error = DescriptorParseError;

	fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
		let mut iter = value.into_iter().peekable();

		let mut parameters = Vec::new();

		if !matches!(iter.next(), Some(b'(')) {
			return Err(DescriptorParseError("no opening parenthesis in method descriptor", value.to_vec()));
		}

		loop {
			match iter.peek() {
				Some(b')') => break,
				_ => parameters.push(FieldDescriptor::parse_iter(&mut iter, value)?),
			}
		}

		if !matches!(iter.next(), Some(b')')) {
			return Err(DescriptorParseError("no closing parenthesis in method descriptor", value.to_vec()));
		}

		let return_type = if matches!(iter.peek(), Some(b'V')) {
			None
		} else {
			Some(FieldDescriptor::parse_iter(&mut iter, value)?)
		};

		if !matches!(iter.next(), None) {
			return Err(DescriptorParseError("method descriptor doesn't end", value.to_vec()));
		}

		Ok(MethodDescriptor {
			parameters,
			return_type
		})
	}
}

#[cfg(test)]
mod test {
	use crate::classfile::descriptor::{BaseOrObjectType, FieldDescriptor, MethodDescriptor};
	use crate::classfile::name::ClassName;

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