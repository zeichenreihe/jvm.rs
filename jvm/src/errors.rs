use crate::classfile::JUtf8;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct OutOfBoundsError;

#[derive(Debug)]
pub enum ClassFileParseError {
	UnknownConstantPoolTag(u8),
	UnknownVerificationTypeInfoTag(u8),
	UnknownStackMapFrameType(u8),
	UnknownAnnotationElementValueTag(u8),

	WrongConstantPoolTag,

	NoSuchAttribute(&'static str),
	NoSuchConstantPoolEntry(usize),

	InvalidMagic(u32),
	InvalidValue(u32),

	IoError(std::io::Error),
}

impl From<std::io::Error> for ClassFileParseError {
	fn from(value: std::io::Error) -> Self {
		Self::IoError(value)
	}
}

impl From<std::str::Utf8Error> for ClassFileParseError {
	fn from(value: std::str::Utf8Error) -> Self {
		todo!()
	}
}

impl From<std::string::FromUtf8Error> for ClassFileParseError {
	fn from(value: std::string::FromUtf8Error) -> Self {
		todo!()
	}
}

#[derive(Debug)]
pub enum RuntimeError {
	NoClassDefFound(JUtf8),
	ClassLoad(ClassFileParseError),
	ClassAlreadyLoaded(JUtf8),
	OutOfBounds(OutOfBoundsError),
}

impl From<ClassFileParseError> for RuntimeError {
	fn from(value: ClassFileParseError) -> Self {
		Self::ClassLoad(value)
	}
}

impl From<OutOfBoundsError> for RuntimeError {
	fn from(value: OutOfBoundsError) -> Self {
		Self::OutOfBounds(OutOfBoundsError)
	}
}