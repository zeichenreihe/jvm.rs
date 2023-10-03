



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassName {
	inner: Vec<u8>,
}

impl ClassName {
	pub fn new(_: &str) -> ClassName {
		todo!()
	}
}

impl PartialEq<[u8]> for ClassName {
	fn eq(&self, other: &[u8]) -> bool {
		self.inner == other
	}
}
impl PartialEq<&[u8]> for ClassName {
	fn eq(&self, other: &&[u8]) -> bool {
		self.inner == *other
	}
}

impl From<&[u8]> for ClassName {
	fn from(value: &[u8]) -> Self {
		Self { inner: value.to_vec() }
	}
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldName {
	inner: Vec<u8>,
}

impl From<&[u8]> for FieldName {
	fn from(value: &[u8]) -> Self {
		Self { inner: value.to_vec() }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodName {
	inner: Vec<u8>,
}

impl From<&[u8]> for MethodName {
	fn from(value: &[u8]) -> Self {
		Self { inner: value.to_vec() }
	}
}

impl<const N: usize> From<&[u8; N]> for MethodName {
	fn from(value: &[u8; N]) -> Self {
		Self { inner: value.to_vec() }
	}
}
