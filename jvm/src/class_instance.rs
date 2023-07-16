use std::mem::size_of;
use std::sync::Arc;
use crate::classfile::ClassFile;
use crate::errors::OutOfBoundsError;
use crate::executor::{JInt, JReference};


#[derive(PartialEq)]
pub struct Field {

}

impl Field {
	fn get_size(&self) -> usize {
		todo!()
	}
}

pub struct Class {
	pub super_class_size: usize,
	pub class: ClassFile,
	pub fields: Vec<Field>,
	// method table
}

impl Class {
	fn instance_size(&self) -> usize {
		self.fields.iter()
			.map(|f| f.get_size())
			.sum::<usize>() + self.super_class_size
	}

	fn field_offset(&self, field: &Field) -> Result<usize, ()> {
		Ok(self.fields.iter()
			.take_while(|&f| f != field)
			.map(|f| f.get_size())
			.sum::<usize>() + self.super_class_size)
	}
}



/// Represents the memory that contains the fields of a class
pub struct ClassInstance<const SIZE: usize> {
	class: ClassFile,
	// in memory:
	// <parent class instance><first field><second field><...><last field>
	data: Box<[u8; SIZE]>
}

impl<const SIZE: usize> ClassInstance<SIZE> {
	/// Returns the whole size of the class instance.
	pub fn get_size(&self) -> usize {
		SIZE
	}

	/// Returns a [JInt] from the class instance.
	pub fn get_int(&self, offset: usize) -> Result<JInt, OutOfBoundsError> {
		let slice = self.data
			.get(offset..).ok_or(OutOfBoundsError)?
			.get(..size_of::<JInt>()).ok_or(OutOfBoundsError)?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		Ok(JInt::from_ne_bytes(slice))
	}
	/// Stores a [JInt] into the class instance.
	pub fn put_int(&mut self, offset: usize, int: JInt) -> Result<(), OutOfBoundsError> {
		let slice = self.data
			.get_mut(offset..).ok_or(OutOfBoundsError)?
			.get_mut(..size_of::<JInt>()).ok_or(OutOfBoundsError)?;
		slice.copy_from_slice(&int.to_ne_bytes());
		Ok(())
	}

	pub fn get_reference(&self, offset: usize) -> Result<JReference, ()> {
		todo!()
	}
	pub fn put_reference(&self, offset: usize, reference: JReference) -> Result<(), ()> {
		todo!()
	}

	// TODO: more!
}

/// Stores a [ClassInstance] and a count of references to it
pub struct ClassInstanceRefCounted<const SIZE: usize> {
	class_instance: Arc<ClassInstance<SIZE>>,
}