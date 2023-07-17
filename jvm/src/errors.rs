use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::classfile::ClassInfo;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct OutOfBoundsError;

impl Display for OutOfBoundsError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("out of bounds.")
	}
}

impl Error for OutOfBoundsError {}

#[derive(Clone, PartialEq)]
pub struct ConstantPoolTagMismatchError {
	pub expected: String,
	pub actual: String,
	pub msg: String,
}

impl Display for ConstantPoolTagMismatchError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "ConstantPoolTagMismatchError {{ expected: {}, actual: {} }}", self.expected, self.actual)
	}
}
impl Debug for ConstantPoolTagMismatchError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "ConstantPoolTagMismatchError {{ expected: {}, actual: {} }}", self.expected, self.actual)
	}
}

impl Error for ConstantPoolTagMismatchError {}

#[derive(Debug)]
pub enum ClassFileParseError {
	UnknownConstantPoolTag(u8),
	UnknownVerificationTypeInfoTag(u8),
	UnknownStackMapFrameType(u8),
	UnknownAnnotationElementValueTag(u8),
	UnknownMethodHandleInfoKind(u8),

	WrongConstantPoolTag,
	InvalidAttributeLength { expected: u32, actual: u32 },
	InvalidConstantPoolTag(ConstantPoolTagMismatchError),

	NoSuchAttribute(&'static str),
	NoSuchConstantPoolEntry(usize),

	InvalidMagic(u32),
	InvalidValue(u32),

	IoError(std::io::Error),
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
		ClassFileParseError::InvalidConstantPoolTag(value)
	}
}

impl From<Infallible> for ClassFileParseError {
	fn from(_: Infallible) -> Self {
		unreachable!()
	}
}


#[derive(Debug)]
pub enum ClassLoadError {
	ParseError(ClassFileParseError),

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


#[derive(Debug)]
pub enum RuntimeError {
	ClassLoad(ClassLoadError),
	OutOfBounds(OutOfBoundsError),
}

impl Display for RuntimeError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		match self {
			//Self::NoClassDefFound(jutf8) => write!(f, "no class found: {jutf8:?}"),
			Self::ClassLoad(e) => write!(f, "while loading a class: {e:?}"),
			//Self::ClassAlreadyLoaded(jutf8) => write!(f, "class already loaded: {jutf8:?}"),
			Self::OutOfBounds(e) => write!(f, "out of bounds: {e:?}"),
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
	fn from(_: OutOfBoundsError) -> Self {
		Self::OutOfBounds(OutOfBoundsError)
	}
}