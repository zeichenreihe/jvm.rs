use std::convert::Infallible;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::classfile::access::AccessFlagError;
use crate::classfile::cp::attribute::AttributeTagMismatchError;
use crate::classfile::cp::ConstantPoolTagMismatchError;
use crate::classfile::descriptor::DescriptorParseError;
use crate::classfile::name::ClassName;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct OutOfBoundsError;

impl Display for OutOfBoundsError {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("out of bounds.")
	}
}

impl Error for OutOfBoundsError {}

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
	AccessFlagError(AccessFlagError),

	NoSuchConstantPoolEntry(usize),

	InvalidMagic(u32),
	WrongVersion { major: u16, minor: u16 },
	IllegalInstruction(&'static str),

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
impl From<AccessFlagError> for ClassFileParseError {
	fn from(value: AccessFlagError) -> Self {
		ClassFileParseError::AccessFlagError(value)
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

	// java classes:
	NoClassDefFoundError(ClassName),
	LinkageError(),
	ClassFormatError(),
	/* subclass */ UnsupportedClassVersionError(),
	IncompatibleClassChangeError(),
	ClassCircularityError(),
	VerifyError(),
}

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