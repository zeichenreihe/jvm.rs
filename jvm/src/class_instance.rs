use anyhow::{anyhow, Result};
use std::collections::HashMap;
use std::mem::size_of;
use std::rc::Rc;
use class_file::{ClassFile, FieldInfo};
use class_file::cp::attribute::ConstantValueAttribute;
use class_file::descriptor::{BaseOrObjectType, FieldDescriptor};
use class_file::name::FieldName;
use crate::executor::{J_NULL, JInt, JReference, StackFrameLvType};


#[derive(Debug, PartialEq, Clone)]
pub struct Field {
	pub size: usize,
	pub field_offset: usize,
	pub field: FieldInfo,
}

impl Field {
	pub fn load<C: ClassData>(&self, class_data: &Rc<C>) -> Result<StackFrameLvType> {
		match self.field.descriptor.base_type {
			BaseOrObjectType::B => todo!(),
			BaseOrObjectType::C => todo!(),
			BaseOrObjectType::D => todo!(),
			BaseOrObjectType::F => todo!(),
			BaseOrObjectType::I => Ok(StackFrameLvType::Int(class_data.get_int(self.field_offset)?)),
			BaseOrObjectType::J => todo!(),
			BaseOrObjectType::S => todo!(),
			BaseOrObjectType::Z => todo!(),
			BaseOrObjectType::Object(_) => Ok(StackFrameLvType::Reference(class_data.get_reference(self.field_offset)?)),
		}
	}
	pub fn store_initial_value<C: ClassData>(&self, class_data: &mut C) -> Result<()> {
		if let Some(constant_value) = &self.field.constant_value {
			match constant_value {
				ConstantValueAttribute::Long(_) => todo!(),
				ConstantValueAttribute::Float(_) => todo!(),
				ConstantValueAttribute::Double(_) => todo!(),
				ConstantValueAttribute::Integer(integer) => {
					class_data.put_int(self.field_offset, 0) // TODO: implement
				}
				ConstantValueAttribute::String(_) => todo!(),
			}
		} else {
			match self.field.descriptor.base_type {
				BaseOrObjectType::I => class_data.put_int(self.field_offset, 0),
				BaseOrObjectType::Object(_) => Ok(class_data.put_reference(self.field_offset, J_NULL).unwrap()), // TODO: unwrap
				_ => todo!(),
			}
		}
	}
}

#[derive(Debug, Clone)]
pub struct Class {
	pub super_class_size: usize,
	pub class_size: usize,
	pub class: ClassFile,

	pub non_static_fields: HashMap<(FieldName, FieldDescriptor), Field>,
	pub static_fields: HashMap<(FieldName, FieldDescriptor), Field>,

	pub static_data: Vec<u8>,
	// method table
}

impl ClassData for Class {
	#[inline]
	fn get_data(&self) -> &Vec<u8> {
		&self.static_data
	}

	#[inline]
	fn get_data_mut(&mut self) -> &mut Vec<u8> {
		&mut self.static_data
	}
}



/// Represents the memory that contains the fields of a class
pub struct ClassInstance {
	// TODO: make private after testing again
	pub class: Rc<Class>,
	// in memory:
	// <parent class instance><first field><second field><...><last field>
	pub data: Vec<u8>
}

impl ClassData for ClassInstance {
	#[inline]
	fn get_data(&self) -> &Vec<u8> {
		&self.data
	}

	#[inline]
	fn get_data_mut(&mut self) -> &mut Vec<u8> {
		&mut self.data
	}
}

/// A trait to abstract away any concrete implementation of data holding of a class. This implements the loading and storing of complex types in the data,
/// checking bounds.
pub trait ClassData {
	fn get_data(&self) -> &Vec<u8>;
	fn get_data_mut(&mut self) -> &mut Vec<u8>;

	/// Returns a [JInt] from the class instance.
	#[inline]
	fn get_int(&self, offset: usize) -> Result<JInt> {
		let slice = self.get_data()
			.get(offset..).ok_or_else(|| anyhow!("out of bounds"))? // TODO: improve error messages
			.get(..size_of::<JInt>()).ok_or_else(|| anyhow!("out of bounds"))?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		Ok(JInt::from_ne_bytes(slice))
	}
	/// Stores a [JInt] into the class instance.
	#[inline]
	fn put_int(&mut self, offset: usize, int: JInt) -> Result<()> {
		let slice = self.get_data_mut()
			.get_mut(offset..).ok_or_else(|| anyhow!("out of bounds"))?
			.get_mut(..size_of::<JInt>()).ok_or_else(|| anyhow!("out of bounds"))?;
		slice.copy_from_slice(&int.to_ne_bytes());
		Ok(())
	}

	#[inline]
	fn get_reference(&self, offset: usize) -> Result<JReference> {
		let slice = self.get_data()
			.get(offset..).ok_or_else(|| anyhow!("out of bounds"))?
			.get(..size_of::<JReference>()).ok_or_else(|| anyhow!("out of bounds"))?
			.try_into().expect("unreachable: the slice is guaranteed to be 4 in length");
		Ok(JReference::from_ne_bytes(slice))
	}
	#[inline]
	fn put_reference(&mut self, offset: usize, reference: JReference) -> Result<()> {
		let slice = self.get_data_mut()
			.get_mut(offset..).ok_or_else(|| anyhow!("out of bounds"))?
			.get_mut(..size_of::<JReference>()).ok_or_else(|| anyhow!("out of bounds"))?;
		slice.copy_from_slice(&reference.to_ne_bytes());
		Ok(())
	}

	// TODO: more!
}

/// Stores a [ClassInstance] and a count of references to it
pub struct ClassInstanceRefCounted {
	class_instance: Rc<ClassInstance>,
}