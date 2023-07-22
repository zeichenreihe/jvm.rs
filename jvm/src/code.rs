use crate::errors::ClassFileParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Code {

}

impl Code {
	pub fn parse(code: Vec<u8>) -> Result<Code, ClassFileParseError> {
		todo!()
	}
}