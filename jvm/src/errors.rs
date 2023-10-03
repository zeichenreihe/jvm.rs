use class_file::name::ClassName;


#[derive(Debug)]
pub enum ClassLoadError {
	// java classes:
	NoClassDefFoundError(ClassName),
	LinkageError(),
	ClassFormatError(),
	/* subclass */ UnsupportedClassVersionError(),
	IncompatibleClassChangeError(),
	ClassCircularityError(),
	VerifyError(),
}
