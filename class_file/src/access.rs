use anyhow::{bail, Result};
use std::fmt::{Debug, Formatter};

#[derive(Clone, PartialEq)]
pub struct ClassInfoAccess {
	pub is_public: bool,
	pub is_final: bool,
	pub is_super: bool, // consider this true for every class file...
	pub is_interface: bool,
	pub is_abstract: bool,
	pub is_synthetic: bool,
	pub is_annotation: bool,
	pub is_enum: bool,
}

impl ClassInfoAccess {
	pub fn parse(access_flags: u16) -> Result<Self> {
		let is_public     = access_flags & 0x0001 != 0;
		let is_final      = access_flags & 0x0010 != 0;
		let is_super      = access_flags & 0x0020 != 0;
		let is_interface  = access_flags & 0x0200 != 0;
		let is_abstract   = access_flags & 0x0400 != 0;
		let is_synthetic  = access_flags & 0x1000 != 0;
		let is_annotation = access_flags & 0x2000 != 0;
		let is_enum       = access_flags & 0x4000 != 0;
		// other bits: reserved for future use

		if is_interface && !is_abstract {
			bail!("ACC_INTERFACE must be ACC_ABSTRACT")
		} else if is_interface && is_final {
			bail!("ACC_INTERFACE must not be ACC_FINAL")
		} else if is_interface && is_super {
			bail!("ACC_INTERFACE must not be ACC_SUPER")
		} else if is_interface && is_enum {
			bail!("ACC_INTERFACE must not be ACC_ENUM")
		} else if !is_interface && is_annotation {
			bail!("ACC_ANNOTATION must not be set for non ACC_INTERFACE")
		} else if !is_interface && is_final && is_abstract {
			bail!("ACC_FINAL and ACC_ABSTRACT must not be set together")
		} else if is_annotation && !is_interface {
			bail!("ACC_ANNOTATION must be ACC_INTERFACE")
		} else {
			Ok(ClassInfoAccess { is_public, is_final, is_super, is_interface, is_abstract, is_synthetic, is_annotation, is_enum })
		}
	}
}

impl Debug for ClassInfoAccess {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("ClassInfoAccess { ")?;
		if self.is_public     { f.write_str("public ")?; }
		if self.is_final      { f.write_str("final ")?; }
		if self.is_super      { f.write_str("super ")?; }
		if self.is_interface  { f.write_str("interface ")?; }
		if self.is_abstract   { f.write_str("abstract ")?; }
		if self.is_synthetic  { f.write_str("synthetic ")?; }
		if self.is_annotation { f.write_str("annotation ")?; }
		if self.is_enum       { f.write_str("enum ")?; }
		f.write_str("}")
	}
}


#[derive(Clone, PartialEq)]
pub struct FieldInfoAccess {
	pub is_public: bool,
	pub is_private: bool,
	pub is_protected: bool,
	pub is_static: bool,
	pub is_final: bool,
	pub is_volatile: bool,
	pub is_transient: bool,
	pub is_synthetic: bool,
	pub is_enum: bool,
}

impl FieldInfoAccess {
	pub fn parse(access_flags: u16) -> Result<Self> {
		let is_public    = access_flags & 0x0001 != 0;
		let is_private   = access_flags & 0x0002 != 0;
		let is_protected = access_flags & 0x0004 != 0;
		let is_static    = access_flags & 0x0008 != 0;
		let is_final     = access_flags & 0x0010 != 0;
		let is_volatile  = access_flags & 0x0040 != 0;
		let is_transient = access_flags & 0x0080 != 0;
		let is_synthetic = access_flags & 0x1000 != 0;
		let is_enum      = access_flags & 0x4000 != 0;
		// other bits: reserved for future use

		if (is_public && is_private) || (is_private && is_protected) || (is_public && is_protected) {
			bail!("at most one of ACC_PUBLIC, ACC_PRIVATE and ACC_PROTECTED may be set")
		}
		if is_final && is_volatile {
			bail!("at most one of ACC_FINAL and ACC_VOLATILE may be set")
		}

		let is_interface_field = false;
		if is_interface_field {
			// must have: is_public, is_static, is_final
			// must not have: is_private, is_protected, is_volatile, is_transient, is_synthetic, is_enum
		}

		Ok(FieldInfoAccess { is_public, is_private, is_protected, is_static, is_final, is_volatile, is_transient, is_synthetic, is_enum })
	}
}

impl Debug for FieldInfoAccess {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("FieldInfoAccess { ")?;
		if self.is_public    { f.write_str("public ")?; }
		if self.is_private   { f.write_str("private ")?; }
		if self.is_protected { f.write_str("protected ")?; }
		if self.is_static    { f.write_str("static ")?; }
		if self.is_final     { f.write_str("final ")?; }
		if self.is_volatile  { f.write_str("volatile ")?; }
		if self.is_transient { f.write_str("transient ")?; }
		if self.is_synthetic { f.write_str("synthetic ")?; }
		if self.is_enum      { f.write_str("enum ")?; }
		f.write_str("}")
	}
}



#[derive(Clone, PartialEq)]
pub struct MethodInfoAccess {
	pub is_public: bool,
	pub is_private: bool,
	pub is_protected: bool,
	pub is_static: bool,
	pub is_final: bool,
	pub is_synchronised: bool,
	pub is_bridge: bool,
	pub is_varargs: bool,
	pub is_native: bool,
	pub is_abstract: bool,
	pub is_strict: bool,
	pub is_synthetic: bool,
}

impl MethodInfoAccess {
	pub fn parse(access_flags: u16) -> Result<Self> {
		let is_public       = access_flags & 0x0001 != 0;
		let is_private      = access_flags & 0x0002 != 0;
		let is_protected    = access_flags & 0x0004 != 0;
		let is_static       = access_flags & 0x0008 != 0;
		let is_final        = access_flags & 0x0010 != 0;
		let is_synchronised = access_flags & 0x0020 != 0;
		let is_bridge       = access_flags & 0x0040 != 0;
		let is_varargs      = access_flags & 0x0080 != 0;
		let is_native       = access_flags & 0x0100 != 0;
		let is_abstract     = access_flags & 0x0400 != 0;
		let is_strict       = access_flags & 0x0800 != 0;
		let is_synthetic    = access_flags & 0x1000 != 0;
		// other bits: reserved for future use

		if (is_public && is_private) || (is_private && is_protected) || (is_public && is_protected) {
			bail!("at most one of ACC_PUBLIC, ACC_PRIVATE and ACC_PROTECTED may be set")
		}

		/// Methods of interfaces may have any of the flags in Table 4.6-A set except ACC_PROTECTED, ACC_FINAL, ACC_SYNCHRONIZED,
		/// and ACC_NATIVE (JLS §9.4).
		///
		/// < 52.0   its ACC_PUBLIC and ACC_ABSTRACT flags set;
		/// >= 52.0  exactly one of its ACC_PUBLIC and ACC_PRIVATE flags set.
		let is_interface_method = false; // "methods of interfaces"
		if is_interface_method {
			if is_protected || is_final || is_synchronised || is_native {
				bail!("methods of interfaces may not have ACC_PROTECTED, ACC_FINAL, ACC_SYNCHRONIZED and ACC_NATIVE")
			}
			// exactly one of is_public, is_private
			// TODO: impl
		}

		if is_abstract {
			if is_private || is_static || is_final || is_synchronised || is_native || is_strict {
				// must not have: is_private, is_static, is_final, is_synchronised, is_native, is_strict
				bail!("ACC_ABSTRACT may not have ACC_PRIVATE, ACC_STATIC, ACC_FINAL, ACC_SYNCHRONIZED, ACC_NATIVE or ACC_STRICT")
			}
		}

		/// Each instance initialization method (§2.9) may have at most one of its ACC_PUBLIC, ACC_PRIVATE, and ACC_PROTECTED flags set,
		/// and may also have its ACC_VARARGS, ACC_STRICT, and ACC_SYNTHETIC flags set, but must not have any of the other flags in Table 4.6-A set.
		let is_specific_instance_initialisation_method = false;
		if is_specific_instance_initialisation_method {
			// at most one of: public, private, protected
			// may have: is_varargs, is_strict, is_synthetic
			// -> must not have: is_static, is_final, is_synchronised, is_bridge, is_native, is_abstract
		}

		/// Class and interface initialization methods are called implicitly by the Java Virtual Machine. The value of their access_flags
		/// item is ignored except for the setting of the ACC_STRICT flag.
		// class and interface initialisation methods: don't check any of this

		Ok(MethodInfoAccess {
			is_public, is_private, is_protected, is_static, is_final, is_synchronised, is_bridge, is_varargs, is_native, is_abstract, is_strict, is_synthetic
		})
	}
}

impl Debug for MethodInfoAccess {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		f.write_str("MethodInfoAccess { ")?;
		if self.is_public       { f.write_str("public ")?; }
		if self.is_private      { f.write_str("private ")?; }
		if self.is_protected    { f.write_str("protected ")?; }
		if self.is_static       { f.write_str("static ")?; }
		if self.is_final        { f.write_str("final ")?; }
		if self.is_synchronised { f.write_str("synchronised ")?; }
		if self.is_bridge       { f.write_str("bridge ")?; }
		if self.is_varargs      { f.write_str("varargs ")?; }
		if self.is_native       { f.write_str("native ")?; }
		if self.is_abstract     { f.write_str("abstract ")?; }
		if self.is_strict       { f.write_str("strict ")?; }
		if self.is_synthetic    { f.write_str("synthetic ")?; }
		f.write_str("}")
	}
}

