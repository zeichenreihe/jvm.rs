use std::collections::HashMap;
use std::fs::File;
use std::path::Path;
use crate::class_instance::{Class, Field};
use crate::classfile::{ClassFile, ClassInfo};
use crate::errors::{ClassFileParseError, ClassLoadError};
use crate::types::descriptor::FieldDescriptor;

// class loading action list:
// - loading: load ClassFile from disk/...
// - linking: convert from ClassFile to Class
// - initialisation: call <clinit>

#[derive(Debug)]
/// Represents a searchable thing to check when loading a class.
pub enum ClassesSource {
	/// A `.zip` or `.jar` file containing `.class` files, given by a name like `/foo/bar/Baz.class` for the class `foo/bar/Baz`. Will be searched for a class
	/// matching in file name, then the name in the class file is checked.
	Jar(Box<Path>),
	/// A directory containing (possibly multiple) subdirectories containing `.class` files, given by a name like `foo/bar/Baz.class` for the class
	/// `foo/bar/Baz`. Will be searched for a class matching the file name and package, then the name in the class file is checked.
	Directory(Box<Path>),
	/// A single class file, referenced as a file. The file name isn't checked for anything, the class file referenced by `path` is checked to have the
	/// same name.
	Class {
		name: String, // TODO: ensure that the name here matches the file name (not super important) and the name in the file
		path: Box<Path>,
	},
	/// A class file that's embedded in the application that's running. The name is checked with the name in the bytes.
	Bytes {
		name: String, // TODO: ensure that the name here matches the name in the bytes
		bytes: Vec<u8>,
	},
}

impl ClassesSource {
	/// Attempts to locate and load a class. Returns `Ok(None)` if no class with the name can be found.
	fn load(&self, name: &ClassInfo) -> Result<Option<ClassFile>, ClassFileParseError> {
		if &name.name.to_string() == match self {
			ClassesSource::Class { name, .. } => name,
			ClassesSource::Bytes { name, .. } => name,
			_ => todo!(),
		} {
			let class_file = match self {
				ClassesSource::Class { path, .. } => {
					let mut file = File::open(path)?;
					ClassFile::parse(&mut file)?
				}
				ClassesSource::Bytes { bytes, .. } => {
					ClassFile::parse(&mut &bytes[..])?
				}
				_ => todo!(),
			};

			Ok(Some(class_file))
		} else {
			Ok(None)
		}
	}
}

#[derive(Debug)]
pub struct ClassLoader {
	pub sources: Vec<ClassesSource>,
	classes: HashMap<ClassInfo, Class>,
}

impl ClassLoader {
	pub fn new() -> ClassLoader {
		ClassLoader {
			sources: Vec::new(),
			classes: HashMap::new(),
		}
	}

	// call only if you tried getting and didn't find any
	fn load(&mut self, class_name: &ClassInfo, currently_loading: &mut Vec<ClassInfo>) -> Result<Class, ClassLoadError> {
		let class_file = self.sources.iter()
			.find_map(|source| {
				match source.load(class_name) {
					Ok(Some(class_file)) => Some(class_file),
					Ok(None) => None,
					Err(e) => {
						eprintln!("Error while trying to find classes: {e}");
						None
					},
				}
			})
			.ok_or_else(|| ClassLoadError::NoClassDefFoundError(class_name.clone()))?;

		let super_class_size = if let Some(super_class) = &class_file.super_class {
			let super_class = self.get_checked(super_class, currently_loading)?;
			super_class.super_class_size + super_class.class_size
		} else {
			0
		};

		let mut field_offset = 0;
		let fields = class_file.fields.iter()
			.map(|field| {
				let descriptor = FieldDescriptor::parse(&field.descriptor).unwrap(); // TODO: panic, fix this
				let size = descriptor.base_or_object_type.get_size() * 4;
				let f = Field {
					descriptor,
					size,
					field_offset,
				};
				field_offset += size;
				f
			})
			.collect();

		let class = Class {
			super_class_size,
			class_size: field_offset,
			class: class_file.clone(),
			fields,
		};

		Ok(class)
	}

	pub fn get(&mut self, class_name: &ClassInfo) -> Result<&Class, ClassLoadError> {
		self.get_checked(class_name, &mut Vec::new())
	}

	fn get_checked(&mut self, class_name: &ClassInfo, currently_loading: &mut Vec<ClassInfo>) -> Result<&Class, ClassLoadError> {
		if !self.classes.contains_key(class_name) {

			// Let C be a class that we try to load. This loading can load other classes, say C_1, C_2, C_3 and so on. To ensure that such a class isn't the
			// class C itself, we add any class we try to load to this Vec<String>, and if we try to load a class we're already loading, we detect the circular
			// class loading. The operations .push() and .pop() can be used as these will always be executed in pairs.

			if currently_loading.contains(class_name) {
				return Err(ClassLoadError::ClassCircularityError());
			}

			currently_loading.push(class_name.clone());

			let class = self.load(&class_name, currently_loading)?;
			self.classes.insert(class_name.clone(), class);

			// TODO: don't let this panic, throw some useful exception!
			assert_eq!(&currently_loading.pop().unwrap(), class_name);
		}

		Ok(self.classes.get(class_name).unwrap())
	}
}

mod testing {

}