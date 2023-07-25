use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::classfile::ClassInfo;


#[derive(Clone, Debug, PartialEq)]
pub enum DescriptorParseError {
	ArrayDimensionTooLarge(usize, Vec<u8>),
	UnexpectedEnd(Vec<u8>),
	NoOpeningParenthesisFound(Vec<u8>), // starts with that even...
	NoClosingParenthesisFound(Vec<u8>),
	NoSemicolonFound(Vec<u8>),
	InvalidDescriptor(u8, Vec<u8>),
}
impl Error for DescriptorParseError {}
impl Display for DescriptorParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		std::fmt::Debug::fmt(self, f)
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct OutOfBoundsError;

impl Display for OutOfBoundsError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("out of bounds.")
	}
}

impl Error for OutOfBoundsError {}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantPoolTagMismatchError {
	pub expected: String,
	pub actual: String,
}

impl Display for ConstantPoolTagMismatchError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ConstantPoolTagMismatchError")
			.field("expected", &self.expected)
			.field("actual", &self.actual)
			.finish()
	}
}

impl Error for ConstantPoolTagMismatchError {}

#[derive(Debug, Clone, PartialEq)]
pub struct AttributeTagMismatchError {
	pub expected: String,
	pub actual: String,
}

impl Display for AttributeTagMismatchError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("AttributeTagMismatchError")
			.field("expected", &self.expected)
			.field("actual", &self.actual)
			.finish()
	}
}

impl Error for AttributeTagMismatchError {}

#[derive(Debug)]
pub enum ClassFileParseError {
	UnknownConstantPoolTag(u8),
	UnknownVerificationTypeInfoTag(u8),
	UnknownStackMapFrameType(u8),
	UnknownAnnotationElementValueTag(u8),
	UnknownMethodHandleInfoKind(u8),
	UnknownOpcode(u8),
	UnknownArrayType(u8),

	WrongConstantPoolTag,
	InvalidAttributeLength { expected: u32, actual: u32 },

	ConstantPoolTagMismatchError(ConstantPoolTagMismatchError),
	AttributeTagMismatchError(AttributeTagMismatchError),

	NoSuchConstantPoolEntry(usize),

	InvalidMagic(u32),

	IoError(std::io::Error),
	OutOfBoundsError(OutOfBoundsError),
	DescriptorParseError(DescriptorParseError),
}

impl Display for ClassFileParseError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "{self:?}")
	}
}

impl Error for ClassFileParseError {}

impl From<std::io::Error> for ClassFileParseError {
	fn from(value: std::io::Error) -> Self {
		Self::IoError(value)
	}
}
impl From<DescriptorParseError> for ClassFileParseError {
	fn from(value: DescriptorParseError) -> Self {
		Self::DescriptorParseError(value)
	}
}

impl From<std::str::Utf8Error> for ClassFileParseError {
	fn from(_value: std::str::Utf8Error) -> Self {
		todo!()
	}
}

impl From<std::string::FromUtf8Error> for ClassFileParseError {
	fn from(_value: std::string::FromUtf8Error) -> Self {
		todo!()
	}
}

impl From<ConstantPoolTagMismatchError> for ClassFileParseError {
	fn from(value: ConstantPoolTagMismatchError) -> Self {
		ClassFileParseError::ConstantPoolTagMismatchError(value)
	}
}
impl From<AttributeTagMismatchError> for ClassFileParseError {
	fn from(value: AttributeTagMismatchError) -> Self {
		ClassFileParseError::AttributeTagMismatchError(value)
	}
}

impl From<Infallible> for ClassFileParseError {
	fn from(_: Infallible) -> Self {
		unreachable!()
	}
}

impl From<OutOfBoundsError> for ClassFileParseError {
	fn from(value: OutOfBoundsError) -> Self {
		Self::OutOfBoundsError(value)
	}
}

#[derive(Debug)]
pub enum ClassLoadError {
	ParseError(ClassFileParseError),
	DescriptorParseError(DescriptorParseError),

	NoClassDefFoundError(ClassInfo),
	LinkageError(),
	ClassFormatError(),
	/* subclass */ UnsupportedClassVersionError(),
	IncompatibleClassChangeError(),
	ClassCircularityError(),
	VerifyError(),
}
/*
impl From<ClassFileParseError> for ClassLoadError {
	fn from(value: ClassFileParseError) -> Self {
		Self::ParseError(value)
	}
}*/
impl From<DescriptorParseError> for ClassLoadError {
	fn from(value: DescriptorParseError) -> Self {
		Self::DescriptorParseError(value)
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	ClassLoad(ClassLoadError),
	OutOfBounds(OutOfBoundsError),
	TypeMismatch,
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			//Self::NoClassDefFound(jutf8) => write!(f, "no class found: {jutf8:?}"),
			Self::ClassLoad(e) => write!(f, "while loading a class: {e:?}"),
			//Self::ClassAlreadyLoaded(jutf8) => write!(f, "class already loaded: {jutf8:?}"),
			Self::OutOfBounds(e) => write!(f, "out of bounds: {e:?}"),
			x => write!(f, "{x:?}"),
		}
	}
}

impl Error for RuntimeError {}

impl From<ClassLoadError> for RuntimeError {
	fn from(value: ClassLoadError) -> Self {
		Self::ClassLoad(value)
	}
}

impl From<ClassFileParseError> for RuntimeError {
	fn from(value: ClassFileParseError) -> Self {
		Self::ClassLoad(ClassLoadError::ParseError(value))
	}
}

impl From<OutOfBoundsError> for RuntimeError {
	fn from(value: OutOfBoundsError) -> Self {
		Self::OutOfBounds(value)
	}
}