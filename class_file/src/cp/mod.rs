use anyhow::{anyhow, bail, Result};
use crate::descriptor::{FieldDescriptor, MethodDescriptor};
use crate::MyRead;
use crate::name::{ClassName, FieldName, MethodName};

pub mod attribute;


#[derive(Debug)]
pub struct Pool(Vec<PoolEntry>);
impl Pool {
	pub fn parse<R: MyRead>(reader: &mut R) -> Result<Pool> {
		let count = reader.read_u2_as_usize()?;
		let mut vec = Vec::with_capacity(count);
		vec.push(PoolEntry::None); // constant pool indices are based on 0
		for _ in 0..count {
			vec.push(PoolEntry::parse(reader)?);
		}
		Ok(Pool(vec))
	}
	pub fn get<T: FromPoolEntry>(&self, index: usize) -> Result<T> {
		let entry = self.0.get(index)
			.ok_or_else(|| anyhow!("index out of bounds: {index} for {}", self.0.len()))?;
		T::from_pool_entry(self, entry)
	}
}

macro_rules! create_err {
	($entry:ident, $expected:ty) => {
		bail!("expected tag of {}, but got {:?}", stringify!($expected), $entry)
	}
}
trait FromPoolEntry {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self>
	where
		Self: Sized;
}

impl FromPoolEntry for &PoolEntry {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		Ok(entry)
	}
}

impl<T: FromPoolEntry> FromPoolEntry for Option<T> {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		match entry {
			// this works since the index `0` refers to the very first element, and that is PoolEntry::None...
			PoolEntry::None => Ok(None),
			other => Ok(Some(T::from_pool_entry(pool, other)?))
		}
	}
}

impl FromPoolEntry for &Vec<u8> {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		match entry {
			PoolEntry::Utf8(vec) => Ok(vec),
			_ => create_err!(entry, PoolEntry::Utf8),
		}
	}
}

impl FromPoolEntry for Utf8Info {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		match entry {
			PoolEntry::Utf8(vec) => Ok(Utf8Info {
				inner: vec.clone(),
			}),
			_ => create_err!(entry, PoolEntry::Utf8),
		}
	}
}

impl FromPoolEntry for ClassName {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::ClassName(index) = entry {
			if let PoolEntry::Utf8(vec) = pool.get::<&_>(*index)? {
				Ok(ClassName::from(&vec[..]))
			} else {
				create_err!(entry, PoolEntry::Utf8)
			}
		} else {
			create_err!(entry, PoolEntry::ClassName)
		}
	}
}

impl FromPoolEntry for FieldName {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::Utf8(vec) = entry {
			Ok(FieldName::from(&vec[..]))
		} else {
			create_err!(entry, PoolEntry::Utf8)
		}
	}
}

impl FromPoolEntry for FieldDescriptor {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::Utf8(vec) = entry {
			Ok(FieldDescriptor::try_from(&vec[..])?)
		} else {
			create_err!(entry, PoolEntry::Utf8)
		}
	}
}

impl FromPoolEntry for MethodName {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::Utf8(vec) = entry {
			Ok(MethodName::from(&vec[..]))
		} else {
			create_err!(entry, PoolEntry::Utf8)
		}
	}
}

impl FromPoolEntry for MethodDescriptor {
	fn from_pool_entry(_: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::Utf8(vec) = entry {
			Ok(MethodDescriptor::try_from(&vec[..])?)
		} else {
			create_err!(entry, PoolEntry::Utf8)
		}
	}
}

struct NameAndType<N, D> {
	name: N,
	descriptor: D,
}
impl<'a, 'b, N, D> FromPoolEntry for NameAndType<N, D>
where
	N: FromPoolEntry,
	D: FromPoolEntry,
{
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::NameAndType{ name_index, descriptor_index } = entry {
			let name       = N::from_pool_entry(pool, pool.get(*name_index      )?)?;
			let descriptor = D::from_pool_entry(pool, pool.get(*descriptor_index)?)?;
			Ok(NameAndType { name, descriptor })
		} else {
			create_err!(entry, PoolEntry::NameAndType)
		}
	}
}

impl FromPoolEntry for FieldRefInfo {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::FieldRef { class_index, name_and_type_index } = entry {
			let class: ClassName = pool.get(*class_index)?;
			let name_and_type: NameAndType<_, _> = pool.get(*name_and_type_index)?;
			Ok(Self { class, name: name_and_type.name, descriptor: name_and_type.descriptor })
		} else {
			create_err!(entry, PoolEntry::FieldRef)
		}
	}
}

impl FromPoolEntry for MethodRefInfo {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::MethodRef { class_index, name_and_type_index } = entry {
			let class: ClassName = pool.get(*class_index)?;
			let name_and_type: NameAndType<_, _> = pool.get(*name_and_type_index)?;
			Ok(Self { class, name: name_and_type.name, descriptor: name_and_type.descriptor })
		} else {
			create_err!(entry, PoolEntry::MethodRef)
		}
	}
}

impl FromPoolEntry for InterfaceMethodRefInfo {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::InterfaceMethodRef { class_index, name_and_type_index } = entry {
			let class: ClassName = pool.get(*class_index)?;
			let name_and_type: NameAndType<_, _> = pool.get(*name_and_type_index)?;
			Ok(Self { class, name: name_and_type.name, descriptor: name_and_type.descriptor })
		} else {
			create_err!(entry, PoolEntry::InterfaceMethodRef)
		}
	}
}

impl FromPoolEntry for MethodHandleInfo {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::MethodHandle(kind, index) = entry {
			match *kind {
				1 => Ok(MethodHandleInfo::GetField        (pool.get(*index)?)),
				2 => Ok(MethodHandleInfo::GetStatic       (pool.get(*index)?)),
				3 => Ok(MethodHandleInfo::PutField        (pool.get(*index)?)),
				4 => Ok(MethodHandleInfo::PutStatic       (pool.get(*index)?)),
				// TODO: 5-8: if kind == 8: must be <init>, else: must not be <init>/<clinit>
				5 => Ok(MethodHandleInfo::InvokeVirtual   (pool.get(*index)?)),
				6 => Ok(MethodHandleInfo::InvokeStatic    (pool.get(*index)?)),
				7 => Ok(MethodHandleInfo::InvokeSpecial   (pool.get(*index)?)),
				8 => Ok(MethodHandleInfo::NewInvokeSpecial(pool.get(*index)?)),
				// TODO: 9: must not be <init>/<clinit>
				9 => Ok(MethodHandleInfo::InvokeInterface (pool.get(*index)?)),
				kind => bail!("unknown method handle info kind"), // TODO: log value?
			}
		} else {
			create_err!(entry, PoolEntry::MethodHandle)
		}
	}
}

impl FromPoolEntry for InvokeDynamicInfo {
	fn from_pool_entry(pool: &Pool, entry: &PoolEntry) -> Result<Self> {
		if let PoolEntry::InvokeDynamic { bootstrap_method_attribute_index, name_and_type_index } = entry {
			let name_and_type: NameAndType<_, _> = pool.get(*name_and_type_index)?;
			Ok(InvokeDynamicInfo {
				bootstrap_method_attribute_index: *bootstrap_method_attribute_index,
				name: name_and_type.name, descriptor: name_and_type.descriptor,
			})
		} else {
			create_err!(entry, PoolEntry::InvokeDynamic)
		}
	}
}


/// This graph shows what depends (has an index to of a type) on what:
/// ```txt
/// Long  Double  Utf8  Integer  Float
///      __________/\_______________
///     /      /     \    \         \
/// String  Class  NameAndType  MethodType
///           |      |      \
///           FieldRef    InvokeDynamic
///           MethodRef
///       InterfaceMethodRef
///              |
///         MethodHandle
/// ```
#[derive(Debug)]
pub enum PoolEntry { // TODO: should also not be public
	None, // used for index = 0
	Utf8(Vec<u8>),
	Integer(u32), // TODO: figure out what to do with the simple data types!
	Float(u32),
	Long { high: u32, low: u32 },
	Double { high: u32, low: u32 },
	ClassName(usize),
	String(usize), // Utf8
	FieldRef {
		class_index: usize,
		name_and_type_index: usize,
	},
	MethodRef {
		class_index: usize,
		name_and_type_index: usize,
	},
	InterfaceMethodRef {
		class_index: usize,
		name_and_type_index: usize,
	},
	NameAndType {
		name_index: usize,
		descriptor_index: usize,
	},
	MethodHandle(u8, usize),
	MethodType(usize),
	InvokeDynamic {
		bootstrap_method_attribute_index: u16,
		name_and_type_index: usize,
	},
}
impl PoolEntry {
	fn parse<R: MyRead>(reader: &mut R) -> Result<PoolEntry> {
		match reader.read_u1()? {
			1 => Ok(Self::Utf8(reader.read_vec(
				|r| r.read_u2_as_usize(),
				|r| r.read_u1()
			)?)),
			3 => Ok(Self::Integer(reader.read_u4()?)),
			4 => Ok(Self::Float(reader.read_u4()?)),
			5 => Ok(Self::Long {
				high: reader.read_u4()?,
				low: reader.read_u4()?,
			}),
			6 => Ok(Self::Double {
				high: reader.read_u4()?,
				low: reader.read_u4()?,
			}),
			7 => Ok(Self::ClassName(reader.read_u2_as_usize()?)),
			8 => Ok(Self::String(reader.read_u2_as_usize()?)),
			9 => Ok(Self::FieldRef {
				class_index: reader.read_u2_as_usize()?,
				name_and_type_index: reader.read_u2_as_usize()?,
			}),
			10 => Ok(Self::MethodRef {
				class_index: reader.read_u2_as_usize()?,
				name_and_type_index: reader.read_u2_as_usize()?,
			}),
			11 => Ok(Self::InterfaceMethodRef {
				class_index: reader.read_u2_as_usize()?,
				name_and_type_index: reader.read_u2_as_usize()?,
			}),
			12 => Ok(Self::NameAndType {
				name_index: reader.read_u2_as_usize()?,
				descriptor_index: reader.read_u2_as_usize()?,
			}),
			15 => Ok(Self::MethodHandle(reader.read_u1()?, reader.read_u2_as_usize()?)),
			16 => Ok(Self::MethodType(reader.read_u2_as_usize()?)),
			18 => Ok(Self::InvokeDynamic {
				bootstrap_method_attribute_index: reader.read_u2()?,
				name_and_type_index: reader.read_u2_as_usize()?,
			}),
			tag => bail!("unknown constant pool tag"), // TODO: log value
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Utf8Info {
	inner: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldRefInfo {
	pub class: ClassName,
	pub name: FieldName,
	pub descriptor: FieldDescriptor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodRefInfo {
	pub class: ClassName,
	pub name: MethodName,
	pub descriptor: MethodDescriptor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceMethodRefInfo {
	pub class: ClassName,
	pub name: MethodName,
	pub descriptor: MethodDescriptor,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodHandleInfo {
	GetField(FieldRefInfo),
	GetStatic(FieldRefInfo),
	PutField(FieldRefInfo),
	PutStatic(FieldRefInfo),
	InvokeVirtual(MethodRefInfo),
	InvokeStatic(MethodRefInfo),
	InvokeSpecial(MethodRefInfo),
	NewInvokeSpecial(MethodRefInfo),
	InvokeInterface(InterfaceMethodRefInfo),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InvokeDynamicInfo {
	bootstrap_method_attribute_index: u16,
	name: MethodName,
	descriptor: MethodDescriptor,
}
