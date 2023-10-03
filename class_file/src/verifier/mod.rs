use itertools::Itertools;
use crate::descriptor::MethodDescriptor;
use crate::instruction::LvIndex;
use crate::instruction::opcode::Opcode;
use crate::MethodInfoAccess as MethodAccessFlags;
use crate::name::{ClassName, MethodName};

trait FailAsBool {
	fn fail(&self, message: &str) -> Bool;
}

impl FailAsBool for bool {
	fn fail(&self, message: &str) -> Bool {
		if *self {
			Ok(())
		} else {
			fail(message)
		}
	}
}

type Bool = std::result::Result<(), ()>;

type Result<T> = std::result::Result<T, ()>;

fn fail<T>(_message: & str) -> Result<T> {
	Err(())
}


#[derive(Debug, Clone, PartialEq, Eq)]
struct Class;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loader;
#[derive(Debug, Clone, PartialEq, Eq)]
struct Method;

const JAVA_LANG_OBJECT                : &[u8] = b"java/lang/Object";
const JAVA_LANG_THROWABLE             : &[u8] = b"java/lang/Throwable";
const JAVA_LANG_CLONEABLE             : &[u8] = b"java/lang/Cloneable";
const JAVA_LANG_SERIALIZABLE          : &[u8] = b"java/lang/Serializable";
const JAVA_LANG_STRING                : &[u8] = b"java/lang/String";
const JAVA_LANG_CLASS                 : &[u8] = b"java/lang/Class";
const JAVA_LANG_INVOKE_METHOD_TYPE    : &[u8] = b"java/lang/invoke/MethodType";
const JAVA_LANG_INVOKE_METHOD_HANDLE  : &[u8] = b"java/lang/invoke/MethodHandle";

fn get_java_lang_object_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_OBJECT), bl))
}
fn get_java_lang_throwable_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_THROWABLE), bl))
}
fn get_java_lang_string_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_STRING), bl))
}
fn get_java_lang_class_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_CLASS), bl))
}
fn get_java_lang_invoke_method_type_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_INVOKE_METHOD_TYPE), bl))
}
fn get_java_lang_invoke_method_handle_class() -> Result<VerificationType> {
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	Ok(VerificationType::Class(ClassName::from(JAVA_LANG_INVOKE_METHOD_HANDLE), bl))
}

fn get_bootstrap_loader() -> Result<Loader> {
	todo!()
}

// 4.10.1

// Iff the predicate classIsTypeSafe is not true, the type checker must throw the exception VerifyError to indicate that the class file is malformed.
// Otherwise, the class file has type checked successfully and bytecode verification has completed successfully.

// classIsTypeSafe(Class) :-
//     classClassName(Class, Name),
//     classDefiningLoader(Class, L),
//     superclassChain(Name, L, Chain),
//     Chain \= [],
//     classSuperClassName(Class, SuperclassName),
//     loadedClass(SuperclassName, L, Superclass),
//     classIsNotFinal(Superclass),
//     classMethods(Class, Methods),
//     checklist(methodIsTypeSafe(Class), Methods).
// classIsTypeSafe(Class) :-
//     classClassName(Class, 'java/lang/Object'),
//     classDefiningLoader(Class, L),
//     isBootstrapLoader(L),
//     classMethods(Class, Methods),
//     checklist(methodIsTypeSafe(Class), Methods).
#[allow(unused)]
fn class_is_type_safe(class: &Class) -> Bool {
	let name = class.class_name()?;
	let l = class.defining_loader()?;

	let chain = superclass_chain(&name, l)?;

	if chain.is_empty() { // is empty for java/lang/Object
		(name == JAVA_LANG_OBJECT).fail("")?;

		class.defining_loader()?
			.is_bootstrap_loader()?;
	} else {
		let superclass_name = class.super_class_name()?;
		let superclass = loaded_class(&superclass_name, l)?;

		superclass.is_not_final()?;
	}

	for method in class.methods()? {
		method_is_type_safe(class, &method)?;
	}

	Ok(())
}

// We stipulate the existence of 28 Prolog predicates ("accessors") that have certain expected behavior but whose formal
// definitions are not given in this specification.
impl Class {
	// classClassName(Class, ClassName)
	//     Extracts the name, ClassName, of the class Class.
	fn class_name(&self) -> Result<&ClassName> {
		todo!()
	}

	// classIsInterface(Class)
	//     True iff the class, Class, is an interface.
	fn is_interface(&self) -> Bool {
		todo!()
	}

	// classIsNotFinal(Class)
	//     True iff the class, Class, is not a final class.
	fn is_not_final(&self) -> Bool {
		todo!()
	}

	// classSuperClassName(Class, SuperClassName)
	//     Extracts the name, SuperClassName, of the superclass of class Class.
	fn super_class_name(&self) -> Result<ClassName> {
		// only fails on java/lang/Object
		todo!()
	}

	// classInterfaces(Class, Interfaces)
	//     Extracts a list, Interfaces, of the direct superinterfaces of the class Class.
	fn interfaces<T>(&self) -> Result<Vec<T>> {
		unreachable!("Never called from anywhere")
	}

	// classMethods(Class, Methods)
	//     Extracts a list, Methods, of the methods declared in the class Class.
	fn methods(&self) -> Result<Vec<Method>> {
		todo!()
	}

	// classAttributes(Class, Attributes)
	//     Extracts a list, Attributes, of the attributes of the class Class.
	//
	//     Each attribute is represented as a functor application of the form attribute(AttributeName, AttributeContents), where AttributeName is the name of the
	//     attribute. The format of the attribute's contents is unspecified.
	#[deprecated]
	fn attributes<T>(&self) -> Result<Vec<T>> {
		unreachable!("Never called from anywhere")
	}

	// classDefiningLoader(Class, Loader)
	//     Extracts the defining class loader, Loader, of the class Class.
	fn defining_loader(&self) -> Result<&Loader> {
		todo!()
	}
}

impl Loader {
	// isBootstrapLoader(Loader)
	//     True iff the class loader Loader is the bootstrap class loader.
	fn is_bootstrap_loader(&self) -> Bool {
		todo!()
	}
}

// loadedClass(Name, InitiatingLoader, ClassDefinition)
//     True iff there exists a class named Name whose representation (in accordance with this specification) when loaded by the class loader InitiatingLoader
//     is ClassDefinition.
fn loaded_class(_name: &ClassName, _initiating_loader: &Loader) -> Result<Class> {
	todo!()
}

impl Method {
	// methodName(Method, Name)
	//     Extracts the name, Name, of the method Method.
	fn name(&self) -> Result<MethodName> {
		todo!()
	}

	// methodAccessFlags(Method, AccessFlags)
	//     Extracts the access flags, AccessFlags, of the method Method.
	fn access_flags(&self) -> Result<MethodAccessFlags> {
		todo!()
	}

	// methodDescriptor(Method, Descriptor)
	//     Extracts the descriptor, Descriptor, of the method Method.
	fn descriptor(&self) -> Result<MethodDescriptor> {
		todo!()
	}

	// methodAttributes(Method, Attributes)
	//     Extracts a list, Attributes, of the attributes of the method Method.
	fn attributes_has_code(&self) -> Bool {
		todo!()
	}

	// isInit(Method)
	//     True iff Method (regardless of class) is <init>.
	fn is_init(&self) -> bool {
		todo!()
	}

	// isNotInit(Method)
	//     True iff Method (regardless of class) is not <init>.
	fn is_not_init(&self) -> bool {
		!self.is_init()
	}

	// isNotFinal(Method, Class)
	//     True iff Method in class Class is not final.
	#[deprecated]
	fn is_not_final(&self, _class: &Class) -> Bool {
		(!self.access_flags()?.is_final).fail("")
	}

	// defined in order to make it compile
	#[deprecated]
	fn is_final(&self, _class: &Class) -> Bool {
		self.access_flags()?.is_final.fail("")
	}

	// isStatic(Method, Class)
	//     True iff Method in class Class is static.
	#[deprecated]
	fn is_static(&self, _class: &Class) -> Bool {
		self.access_flags()?.is_static.fail("")
	}

	// isNotStatic(Method, Class)
	//     True iff Method in class Class is not static.
	#[deprecated]
	fn is_not_static(&self, _class: &Class) -> Bool {
		(!self.access_flags()?.is_static).fail("")
	}

	// isPrivate(Method, Class)
	//     True iff Method in class Class is private.
	#[deprecated]
	fn is_private(&self, _class: &Class) -> Bool {
		self.access_flags()?.is_private.fail("")
	}

	// isNotPrivate(Method, Class)
	//     True iff Method in class Class is not private.
	#[deprecated]
	fn is_not_private(&self, _class: &Class) -> Bool {
		(!self.access_flags()?.is_private).fail("")
	}
}

// isProtected(MemberClass, MemberName, MemberDescriptor)
//     True iff there is a member named MemberName with descriptor MemberDescriptor in the class MemberClass and it is protected.
fn is_protected<A, B, C>(a: A, b: B, c: C) -> Bool {
	todo!()
}

// isNotProtected(MemberClass, MemberName, MemberDescriptor)
//     True iff there is a member named MemberName with descriptor MemberDescriptor in the class MemberClass and it is not protected.
fn is_not_protected<A, B, C>(a: A, b: B, c: C) -> Bool {
	todo!()
}

// parseFieldDescriptor(Descriptor, Type)
//     Converts a field descriptor, Descriptor, into the corresponding verification type Type (§4.10.1.2).
fn parse_field_descriptor<Descriptor>(_descriptor: &Descriptor) -> Result<VerificationType> {
	todo!()
}

// parseMethodDescriptor(Descriptor, ArgTypeList, ReturnType)
//     Converts a method descriptor, Descriptor, into a list of verification types, ArgTypeList, corresponding to the method argument types, and a verification
//     type, ReturnType, corresponding to the return type.
fn parse_method_descriptor(_descriptor: MethodDescriptor) -> Result<(Vec<VerificationType>, Option<VerificationType>)> {
	todo!()
}

// parseCodeAttribute(Class, Method, FrameSize, MaxStack, ParsedCode, Handlers, StackMap)
//     Extracts the instruction stream, ParsedCode, of the method Method in Class, as well as the maximum operand stack size, MaxStack, the maximal number of
//     local variables, FrameSize, the exception handlers, Handlers, and the stack map StackMap.
//
//     The representation of the instruction stream and stack map attribute must be as specified in §4.10.1.3 and §4.10.1.4.
fn parse_code_attribute(_class: &Class, _method: &Method) -> Result<(usize, usize, Vec<Instruction>, Vec<Handler>, Vec<Instruction>)> {

	let frame_size = 0;
	let max_stack = 0;
	let parsed_code = Vec::new();
	let handlers = Vec::new();
	let stack_map = Vec::new();

	todo!();

	Ok((frame_size, max_stack, parsed_code, handlers, stack_map))
}

// samePackageName(Class1, Class2)
//     True iff the package names of Class1 and Class2 are the same.
fn same_package_name(_class1: &Class, _class2: &Class) -> Bool {
	todo!()
}

// differentPackageName(Class1, Class2)
//     True iff the package names of Class1 and Class2 are different.
fn different_package_name(_class1: &Class, _class2: &Class) -> Bool {
	todo!()
}

// When type checking a method's body, it is convenient to access information about the method. For this purpose, we define an environment, a six-tuple
// consisting of:
//    - a class
//    - a method
//    - the declared return type of the method
//    - the instructions in a method
//    - the maximal size of the operand stack
//    - a list of exception handlers
#[derive(Debug, PartialEq, Eq)]
struct Environment {
	class: Class,
	method: Method,
	/// `None` means `void`.
	return_type: Option<VerificationType>,
	instructions: Vec<Instruction>,
	max_stack: usize,
	handlers: Vec<Handler>,
}

// We specify accessors to extract information from the environment.
impl Environment {
	// allInstructions(Environment, Instructions) :-
	//     Environment = environment(_Class, _Method, _ReturnType,
	//                               Instructions, _, _).
	fn all_instructions(&self) -> Result<&Vec<Instruction>> {
		Ok(&self.instructions)
	}

	// exceptionHandlers(Environment, Handlers) :-
	//     Environment = environment(_Class, _Method, _ReturnType,
	//                               _Instructions, _, Handlers).
	#[deprecated]
	fn exception_handlers(&self) -> Result<&Vec<Handler>> {
		Ok(&self.handlers)
	}

	// maxOperandStackLength(Environment, MaxStack) :-
	//     Environment = environment(_Class, _Method, _ReturnType,
	//                               _Instructions, MaxStack, _Handlers).
	#[deprecated]
	fn max_operand_stack_length(&self) -> Result<usize> {
		Ok(self.max_stack)
	}

	// thisClass(Environment, class(ClassName, L)) :-
	//     Environment = environment(Class, _Method, _ReturnType,
	//                               _Instructions, _, _),
	//     classDefiningLoader(Class, L),
	//     classClassName(Class, ClassName).
	#[deprecated]
	fn this_class(&self) -> Result<(ClassName, &Loader)> {
		let l = self.class.defining_loader()?;
		let class_name = self.class.class_name()?;
		Ok((class_name.clone(), l))
	}

	// thisMethodReturnType(Environment, ReturnType) :-
	//     Environment = environment(_Class, _Method, ReturnType,
	//                               _Instructions, _, _).
	#[deprecated]
	fn this_method_return_type(&self) -> Result<&Option<VerificationType>> {
		Ok(&self.return_type)
	}

	// We specify additional predicates to extract higher-level information from the environment.

	// offsetStackFrame(Environment, Offset, StackFrame) :-
	//     allInstructions(Environment, Instructions),
	//     member(stackMap(Offset, StackFrame), Instructions).
	fn offset_stack_frame(&self, offset: usize) -> Result<&Frame> {
		self.all_instructions()?.iter()
			.find_map(|instruction| {
				match instruction {
					Instruction::StackMap(stack_map_offset, frame) if offset == *stack_map_offset => Some(frame),
					_ => None,
				}
			})
			.ok_or(fail::<()>("").unwrap_err())
	}

	// currentClassLoader(Environment, Loader) :-
	//     thisClass(Environment, class(_, Loader)).
	fn current_class_loader(&self) -> Result<&Loader> {
		self.class.defining_loader()
	}
}

// 4.10.1.2

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayType { // TODO: should not be public
	Byte, Boolean, Char, Short,
	Other(Box<VerificationType>),
}

impl ArrayType {
	fn as_verification_type(&self) -> VerificationType {
		match self {
			ArrayType::Byte | ArrayType::Boolean | ArrayType::Char | ArrayType::Short => VerificationType::Int,
			Self::Other(x) => *x.clone(),
		}
	}
}

// Verification type hierarchy:
//
//                              top
//                  ____________/\____________
//                 /                          \
//                /                            \
//             oneWord                       twoWord
//            /   |   \                     /       \
//           /    |    \                   /         \
//         int  float  reference        long        double
//                      /     \
//                     /       \_____________
//                    /                      \
//                   /                        \
//            uninitialized                    +------------------+
//             /         \                     |  Java reference  |
//            /           \                    |  type hierarchy  |
// uninitializedThis  uninitialized(Offset)    +------------------+
//                                                      |
//                                                      |
//                                                     null
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerificationType { // TODO: should not be public
	Top,
	OneWord,
	Int,
	Float,
	Reference,
	Uninitialized,
	UninitializedThis,
	UninitializedOffset(usize),
	Class(ClassName, Loader),
	ArrayOf(ArrayType),
	Null,
	TwoWord,
	Long,
	Double,
}

impl VerificationType {
	fn is_atom(&self) -> bool {
		use VerificationType::*;
		match self {
			Top | OneWord | Int | Float | Reference | Uninitialized | UninitializedThis | Null | TwoWord | Long | Double
				=> true,
			UninitializedOffset(..) | Class(..) | ArrayOf(..)
				=> false,
		}
	}

	fn is_assignable(from: &Self, to: &Self) -> Bool {
		Self::is_assignable_bool(from, to).fail("Type cannot be assigned!")
	}

	fn is_assignable_bool(from: &Self, to: &Self) -> bool {
		use VerificationType::*;

		// isAssignable(X, X).
		from == to || match (from, to) {
			// isAssignable(oneWord, top).
			(OneWord, Top) => true,

			// isAssignable(twoWord, top).
			(TwoWord, Top) => true,

			// isAssignable(int, X)    :- isAssignable(oneWord, X).
			(Int, to) => Self::is_assignable_bool(&OneWord, to),

			// isAssignable(float, X)  :- isAssignable(oneWord, X).
			(Float, to) => Self::is_assignable_bool(&OneWord, to),

			// isAssignable(long, X)   :- isAssignable(twoWord, X).
			(Long, to) => Self::is_assignable_bool(&TwoWord, to),

			// isAssignable(double, X) :- isAssignable(twoWord, X).
			(Double, to) => Self::is_assignable_bool(&TwoWord, to),

			// isAssignable(reference, X)   :- isAssignable(oneWord, X).
			(Reference, to) => Self::is_assignable_bool(&OneWord, to),

			// isAssignable(class(_, _), X) :- isAssignable(reference, X).
			(Class(_, _), to) => Self::is_assignable_bool(&Reference, to),

			// isAssignable(arrayOf(_), X)  :- isAssignable(reference, X).
			(ArrayOf(_), to) => Self::is_assignable_bool(&Reference, to),

			// isAssignable(uninitialized, X)     :- isAssignable(reference, X).
			(Uninitialized, to) => Self::is_assignable_bool(&Reference, to),

			// isAssignable(uninitializedThis, X) :- isAssignable(uninitialized, X).
			(UninitializedThis, to) => Self::is_assignable_bool(&Uninitialized, to),

			// isAssignable(uninitialized(_), X)  :- isAssignable(uninitialized, X).
			(UninitializedOffset(_), to) => Self::is_assignable_bool(&Uninitialized, to),

			// isAssignable(null, class(_, _)).
			(Null, Class(_, _)) => true,

			// isAssignable(null, arrayOf(_)).
			(Null, ArrayOf(_)) => true,

			// isAssignable(null, X) :- isAssignable(class('java/lang/Object', BL), X),
			//                          isBootstrapLoader(BL).
			(Null, to) => {
				(|| {
					Self::is_assignable_bool(&get_java_lang_object_class()?, to)
						.fail("")
				})().is_ok()
			},

			// isAssignable(class(X, Lx), class(Y, Ly)) :-
			//     isJavaAssignable(class(X, Lx), class(Y, Ly)).
			(Class(_, _), Class(_, _)) => Self::is_java_assignable(from, to),

			// isAssignable(arrayOf(X), class(Y, L)) :-
			//     isJavaAssignable(arrayOf(X), class(Y, L)).
			(ArrayOf(_), Class(_, _)) =>  Self::is_java_assignable(from, to),

			// isAssignable(arrayOf(X), arrayOf(Y)) :-
			//     isJavaAssignable(arrayOf(X), arrayOf(Y)).
			(ArrayOf(_), ArrayOf(_)) => Self::is_java_assignable(from, to),

			_ => false,
		}
	}

	fn is_array_interface(verification_type: &VerificationType) -> bool {
		use VerificationType::*;

		match verification_type {
			// isArrayInterface(class('java/lang/Cloneable', BL)) :-
			//     isBootstrapLoader(BL).
			Class(name, bl) if name == JAVA_LANG_CLONEABLE => bl.is_bootstrap_loader().is_ok(),

			// isArrayInterface(class('java/io/Serializable', BL)) :-
			//     isBootstrapLoader(BL).
			Class(name, bl) if name == JAVA_LANG_SERIALIZABLE => bl.is_bootstrap_loader().is_ok(),

			_ => false,
		}
	}

	fn is_java_assignable(from: &VerificationType, to: &VerificationType) -> bool {
		use VerificationType::*;

		match (from, to) {
			// isJavaAssignable(class(_, _), class(To, L)) :-
			//     loadedClass(To, L, ToClass),
			//     classIsInterface(ToClass).
			(Class(_, _), Class(to, l)) => {
				(|| {
					let to_class = loaded_class(to, l)?;
					to_class.is_interface()
				})().is_ok()
			},

			// isJavaAssignable(From, To) :-
			//     isJavaSubclassOf(From, To).
			(from, to) if Self::is_java_subclass_of(from, to) => true,

			// isJavaAssignable(arrayOf(_), class('java/lang/Object', BL)) :-
			//     isBootstrapLoader(BL).
			(ArrayOf(_), Class(name, bl)) if name == JAVA_LANG_OBJECT && bl.is_bootstrap_loader().is_ok() => true,

			// isJavaAssignable(arrayOf(_), X) :-
			//     isArrayInterface(X).
			(ArrayOf(_), x) if Self::is_array_interface(x) => true,

			// isJavaAssignable(arrayOf(X), arrayOf(Y)) :-
			//     atom(X),
			//     atom(Y),
			//     X = Y.
			// isJavaAssignable(arrayOf(X), arrayOf(Y)) :-
			//     compound(X), compound(Y), isJavaAssignable(X, Y).
			(ArrayOf(ArrayType::Other(x)), ArrayOf(ArrayType::Other(y))) if
				x.is_atom() && y.is_atom()
				=> x == y,
			(ArrayOf(ArrayType::Other(x)), ArrayOf(ArrayType::Other(y))) if
				!x.is_atom() && !y.is_atom()
				=> Self::is_java_assignable(x, y),

			// x != Other, y != Other
			// atom(x) and atom(y) not needed, only atoms exist
			(ArrayOf(x), ArrayOf(y)) => x == y,

			_ => false,
		}
	}

	fn is_java_subclass_of(from: &VerificationType, to: &VerificationType) -> bool {
		use VerificationType::*;

		match (from, to) {
			// isJavaSubclassOf(class(SubclassName, L), class(SubclassName, L)).
			(Class(x, xl), Class(y, yl)) if x == y && xl == yl => true,

			// isJavaSubclassOf(class(SubclassName, LSub), class(SuperclassName, LSuper)) :-
			//     superclassChain(SubclassName, LSub, Chain),
			//     member(class(SuperclassName, L), Chain),
			//     loadedClass(SuperclassName, L, Sup),
			//     loadedClass(SuperclassName, LSuper, Sup).
			(Class(subclass_name, l_sub), Class(superclass_name, l_super)) => {
				(|| {
					let chain = superclass_chain(subclass_name, l_sub)?;

					let l = chain.into_iter()
						.find_map(|(name, loader)| if &name == superclass_name {
							Some(loader)
						} else {
							None
						});

					if let Some(l) = l {
							let sup_from_l_sub = loaded_class(superclass_name, &l)?;
							let sup_from_l_super = loaded_class(superclass_name, l_super)?;

							(sup_from_l_sub == sup_from_l_super).fail("")
					} else {
						fail("")
					}
				})().is_ok()
			},

			_ => false,
		}
	}

	// isSmallArray(arrayOf(byte)).
	// isSmallArray(arrayOf(boolean)).
	// isSmallArray(null).
	fn is_small_array(&self) -> Bool {
		match self {
			Self::ArrayOf(ArrayType::Byte) => Ok(()),
			Self::ArrayOf(ArrayType::Boolean) => Ok(()),
			Self::Null => Ok(()),
			_ => fail("")
		}
	}

	// arrayComponentType(arrayOf(X), X).
	// arrayComponentType(null, null).
	fn array_component_type(&self) -> Result<ArrayType> {
		match self {
			Self::ArrayOf(x) => Ok(x.clone()),
			Self::Null => Ok(ArrayType::Other(Box::new(Self::Null))),
			_ => fail(""),
		}
	}
}

fn superclass_chain(class_name: &ClassName, l: &Loader) -> Result<Vec<(ClassName, Loader)>> {
	let mut class_name = class_name.clone();
	let mut l = l.clone();

	let mut chain = Vec::new();

	loop {
		let class = loaded_class(&class_name, &l)?;

		if class_name == JAVA_LANG_OBJECT {
			// superclassChain('java/lang/Object', L, []) :-
			//     loadedClass('java/lang/Object', L, Class),
			//     classDefiningLoader(Class, BL),
			//     isBootstrapLoader(BL).
			// reordering allowed since super_class_name fails on java/lang/Object
			class.defining_loader()?
				.is_bootstrap_loader()?;
			break;
		} else {
			// superclassChain(ClassName, L, [class(SuperclassName, Ls) | Rest]) :-
			//     loadedClass(ClassName, L, Class),
			//     classSuperClassName(Class, SuperclassName),
			//     classDefiningLoader(Class, Ls),
			//     superclassChain(SuperclassName, Ls, Rest).
			let super_class_name = class.super_class_name()?;
			let ls = class.defining_loader()?;

			chain.push((super_class_name.clone(), ls.clone()));

			class_name = super_class_name;
			l = ls.clone();
		}
	}

	Ok(chain)
}

// 4.10.1.3

// 4.10.1.4

#[derive(Debug, Clone, PartialEq, Eq)]
/// Note: things are stored in reverse, use `.pop()` to access the top element, `.push()` to add something.
struct OperandStack {
	inner: Vec<VerificationType>,
}

impl OperandStack {
	fn new(value: VerificationType) -> OperandStack {
		OperandStack {
			inner: vec![value],
		}
	}
	fn empty() -> OperandStack {
		OperandStack {
			inner: Vec::new(),
		}
	}
	fn is_assignable(from: &OperandStack, to: &OperandStack) -> Bool {
		(from.inner.len() == to.inner.len()).fail("")?;
		for i in 0..from.inner.len() { // range checked above
			VerificationType::is_assignable(&from.inner[i], &to.inner[i])?;
		}
		Ok(())
	}
	fn push(&mut self, value: VerificationType) {
		self.inner.push(value)
	}
	fn pop(&mut self) -> Result<VerificationType> {
		self.inner.pop().ok_or(fail::<()>("").unwrap_err())
	}
	fn head(&self) -> Result<&VerificationType> {
		self.inner.last()
			.ok_or(fail::<()>("").unwrap_err())
	}
	fn nth1(&self, index: usize) -> Result<&VerificationType> {
		self.inner.get(self.inner.len() - (index - 1))
			.ok_or(fail::<()>("").unwrap_err())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LocalVariables{
	inner: Vec<VerificationType>,
}

impl LocalVariables {
	fn new(inner: Vec<VerificationType>) -> LocalVariables {
		LocalVariables {
			inner
		}
	}
	fn is_assignable(from: &LocalVariables, to: &LocalVariables) -> Bool {
		(from.inner.len() == to.inner.len()).fail("")?;
		for i in 0..from.inner.len() { // range checked above
			VerificationType::is_assignable(&from.inner[i], &to.inner[i])?;
		}
		Ok(())
	}
	fn nth0(&self, index: usize) -> Result<VerificationType> {
		match self.inner.get(index) {
			Some(x) => Ok(x.clone()),
			None => fail(""),
		}
	}
	fn get_mut(&mut self, index: usize) -> Option<&mut VerificationType> {
		self.inner.get_mut(index)
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Frame {
	locals: LocalVariables,
	operand_stack: OperandStack,
	/// `flags: Vec<Flag>,`
	/// is a list which may either be empty or have the single element `flagThisUninit`. Represented here via one boolean field.
	/// `true` iff `flagThisUninit` would be there.
	flag_this_uninit: bool,
}

impl Frame {
	fn is_assignable(from: &Frame, to: &Frame) -> Bool {
		// frameIsAssignable(frame(Locals1, StackMap1, Flags1),
		//                   frame(Locals2, StackMap2, Flags2)) :-
		//     length(StackMap1, StackMapLength),
		//     length(StackMap2, StackMapLength),
		//     maplist(isAssignable, Locals1, Locals2),
		//     maplist(isAssignable, StackMap1, StackMap2),
		//     subset(Flags1, Flags2).

		LocalVariables::is_assignable(&from.locals, &to.locals)?;
		OperandStack::is_assignable(&from.operand_stack, &to.operand_stack)?;

		// subset, but using our changed definition of flags
		match (from.flag_this_uninit, to.flag_this_uninit) {
			// True if all elements of SubSet belong to Set as well.
			(false, false) => true,
			(false, true) => true,
			(true, false) => false,
			(true, true) => true,
		}.fail("")
	}
}

impl OperandStack {
	// operandStackHasLegalLength(Environment, OperandStack) :-
	//     length(OperandStack, Length),
	//     maxOperandStackLength(Environment, MaxStack),
	//     Length =< MaxStack.
	fn has_legal_length(&self, environment: &Environment) -> Bool {
		(self.inner.len() <= environment.max_stack).fail("")
	}
}

impl Frame {
	// nth1OperandStackIs(i, frame(_Locals, OperandStack, _Flags), Element) :-
	//     nth1(i, OperandStack, Element).
	#[deprecated]
	fn nth1_operand_stack_is(&self, i: usize) -> Result<&VerificationType> {
		self.operand_stack.nth1(i)
	}

	// canPop(frame(Locals, OperandStack, Flags), Types,
	//        frame(Locals, PoppedOperandStack, Flags)) :-
	//     popMatchingList(OperandStack, Types, PoppedOperandStack).
	fn can_pop(self, types: impl IntoIterator<Item=VerificationType>) -> Result<Frame> {
		Ok(Self {
			operand_stack: pop_matching_list(self.operand_stack, types)?,
			..self
		})
	}
}

// popMatchingList(OperandStack, [], OperandStack).
// popMatchingList(OperandStack, [P | Rest], NewOperandStack) :-
//     popMatchingType(OperandStack, P, TempOperandStack, _ActualType),
//     popMatchingList(TempOperandStack, Rest, NewOperandStack).
fn pop_matching_list(operand_stack: OperandStack, types: impl IntoIterator<Item=VerificationType>) -> Result<OperandStack> {
	let mut new_operand_stack = operand_stack.clone();

	for p in types {
		let (temp_operand_stack, _actual_type) = pop_matching_type(new_operand_stack, p)?;
		new_operand_stack = temp_operand_stack;
	}

	Ok(new_operand_stack)
}

// popMatchingType([ActualType | OperandStack],
//                 Type, OperandStack, ActualType) :-
//     sizeOf(Type, 1),
//     isAssignable(ActualType, Type).
//
// popMatchingType([top, ActualType | OperandStack],
//                 Type, OperandStack, ActualType) :-
//     sizeOf(Type, 2),
//     isAssignable(ActualType, Type).
fn pop_matching_type(mut operand_stack: OperandStack, type_: VerificationType) -> Result<(OperandStack, VerificationType)> {
	match operand_stack.pop()? {
		VerificationType::Top => {
			match operand_stack.pop()? {
				actual_type if actual_type.size() == TypeSize::OneWord => {
					VerificationType::is_assignable(&actual_type, &type_)?;

					Ok((operand_stack, actual_type))
				},
				_ => fail(""),
			}
		},
		actual_type if actual_type.size() == TypeSize::OneWord => {
			VerificationType::is_assignable(&actual_type, &type_)?;

			Ok((operand_stack, actual_type))
		},
		_ => fail(""),
	}
}

// sizeOf(X, 2) :- isAssignable(X, twoWord).
// sizeOf(X, 1) :- isAssignable(X, oneWord).
// sizeOf(top, 1).
/// Use `.size() == TypeSize::OneWord` if possible!
fn size_of(x: &VerificationType, size: usize) -> Bool {
	match size {
		2 => VerificationType::is_assignable(x, &VerificationType::TwoWord),
		1 if x == &VerificationType::Top => Ok(()),
		1 => VerificationType::is_assignable(x, &VerificationType::OneWord),
		_ => fail(""),
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeSize {
	OneWord, TwoWord,
}

impl VerificationType {
	fn size(&self) -> TypeSize {
		if self == &VerificationType::Top || VerificationType::is_assignable_bool(self, &VerificationType::OneWord) {
			TypeSize::OneWord
		} else if VerificationType::is_assignable_bool(self, &VerificationType::TwoWord) {
			TypeSize::TwoWord
		} else {
			unreachable!("this should never be reachable!")
		}
	}
}

// pushOperandStack(OperandStack, 'void', OperandStack).
// pushOperandStack(OperandStack, Type, [Type | OperandStack]) :-
//     sizeOf(Type, 1).
// pushOperandStack(OperandStack, Type, [top, Type | OperandStack]) :-
//     sizeOf(Type, 2).
fn push_operand_stack(mut operand_stack: OperandStack, type_: Option<VerificationType>) -> Result<OperandStack> {
	if let Some(type_) = type_ {
		match type_.size() {
			TypeSize::OneWord => {
				operand_stack.push(type_);
			},
			TypeSize::TwoWord => {
				operand_stack.push(type_);
				operand_stack.push(VerificationType::Top);
			},
		}
	}
	Ok(operand_stack)
}

// canSafelyPush(Environment, InputOperandStack, Type, OutputOperandStack) :-
//     pushOperandStack(InputOperandStack, Type, OutputOperandStack),
//     operandStackHasLegalLength(Environment, OutputOperandStack).
fn can_safely_push(environment: &Environment, input_operand_stack: OperandStack, type_: VerificationType) -> Result<OperandStack> {
	let output_operand_stack = push_operand_stack(input_operand_stack, Some(type_))?;
	output_operand_stack.has_legal_length(environment)?;
	Ok(output_operand_stack)
}

// canSafelyPushList(Environment, InputOperandStack, Types,
//                   OutputOperandStack) :-
//     canPushList(InputOperandStack, Types, OutputOperandStack),
//     operandStackHasLegalLength(Environment, OutputOperandStack).
fn can_safely_push_list(environment: &Environment, input_operand_stack: OperandStack, types: impl IntoIterator<Item=VerificationType>) -> Result<OperandStack> {
	let output_operand_stack = can_push_list(input_operand_stack, types)?;
	output_operand_stack.has_legal_length(environment)?;
	Ok(output_operand_stack)
}

// canPushList(InputOperandStack, [], InputOperandStack).
// canPushList(InputOperandStack, [Type | Rest], OutputOperandStack) :-
//     pushOperandStack(InputOperandStack, Type, InterimOperandStack),
//     canPushList(InterimOperandStack, Rest, OutputOperandStack).
fn can_push_list(mut operand_stack: OperandStack, types: impl IntoIterator<Item=VerificationType>) -> Result<OperandStack> {
	for type_ in types {
		operand_stack = push_operand_stack(operand_stack, Some(type_))?;
	}
	Ok(operand_stack)
}

// popCategory1([Type | Rest], Type, Rest) :-
//     Type \= top,
//     sizeOf(Type, 1).
fn pop_category_1(mut stack: OperandStack) -> Result<(VerificationType, OperandStack)> {
	let type_ = stack.pop()?;
	if type_ != VerificationType::Top && type_.size() == TypeSize::OneWord {
		Ok((type_, stack))
	} else {
		fail("")
	}
}

// popCategory2([top, Type | Rest], Type, Rest) :-
//     sizeOf(Type, 2).
fn pop_category_2(mut stack: OperandStack) -> Result<(VerificationType, OperandStack)> {
	if stack.pop()? == VerificationType::Top {
		let type_ = stack.pop()?;
		if type_.size() == TypeSize::TwoWord {
			Ok((type_, stack))
		} else {
			fail("")
		}
	} else {
		fail("")
	}
}

impl Frame {
	// validTypeTransition(Environment, ExpectedTypesOnStack, ResultType,
	//                     frame(Locals, InputOperandStack, Flags),
	//                     frame(Locals, NextOperandStack, Flags)) :-
	//     popMatchingList(InputOperandStack, ExpectedTypesOnStack,
	//                     InterimOperandStack),
	//     pushOperandStack(InterimOperandStack, ResultType, NextOperandStack),
	//     operandStackHasLegalLength(Environment, NextOperandStack).
	fn valid_type_transition(self, environment: &Environment, expected_types_on_stack: impl IntoIterator<Item=VerificationType>,
							 result_type: Option<VerificationType>) -> Result<Frame> {
		let interim_operand_stack = pop_matching_list(self.operand_stack, expected_types_on_stack)?;
		let next_operand_stack = push_operand_stack(interim_operand_stack, result_type)?;
		next_operand_stack.has_legal_length(environment)?;
		Ok(Frame {
			operand_stack: next_operand_stack,
			..self
		})
	}
}

// 4.10.1.5

fn method_is_type_safe(class: &Class, method: &Method) -> Bool {
	does_not_override_final_method(class, method)?;
	let access_flags = method.access_flags()?;

	if access_flags.is_abstract || access_flags.is_native {
		// methodIsTypeSafe(Class, Method) :-
		//     doesNotOverrideFinalMethod(Class, Method),
		//     methodAccessFlags(Method, AccessFlags),
		//     member(abstract, AccessFlags).
		// methodIsTypeSafe(Class, Method) :-
		//     doesNotOverrideFinalMethod(Class, Method),
		//     methodAccessFlags(Method, AccessFlags),
		//     member(native, AccessFlags).
		Ok(())
	} else {
		// methodIsTypeSafe(Class, Method) :-
		//     doesNotOverrideFinalMethod(Class, Method),
		//     methodAccessFlags(Method, AccessFlags),
		//     methodAttributes(Method, Attributes),
		//     notMember(native, AccessFlags),
		//     notMember(abstract, AccessFlags),
		//     member(attribute('Code', _), Attributes),
		//     methodWithCodeIsTypeSafe(Class, Method).
		method.attributes_has_code()?;
		method_with_code_is_type_safe(class, method)
	}
}

fn does_not_override_final_method(class: &Class, method: &Method) -> Bool {
	// doesNotOverrideFinalMethod(class('java/lang/Object', L), Method) :-
	//     isBootstrapLoader(L).
	let does_not_override_final_method_0 = || -> Bool {
		todo!()
	};

	let does_not_override_final_method_1 = |_| {
		let access_flags = method.access_flags()?;

		if access_flags.is_private || access_flags.is_static {
			// doesNotOverrideFinalMethod(Class, Method) :-
			//     isPrivate(Method, Class).
			// doesNotOverrideFinalMethod(Class, Method) :-
			//     isStatic(Method, Class).
			Ok(())
		} else {
			// doesNotOverrideFinalMethod(Class, Method) :-
			//     isNotPrivate(Method, Class),
			//     isNotStatic(Method, Class),
			//     doesNotOverrideFinalMethodOfSuperclass(Class, Method).
			does_not_override_final_method_of_superclass(class, method)
		}
	};

	does_not_override_final_method_0()
		.or_else(does_not_override_final_method_1)
}

// doesNotOverrideFinalMethodOfSuperclass(Class, Method) :-
//     classSuperClassName(Class, SuperclassName),
//     classDefiningLoader(Class, L),
//     loadedClass(SuperclassName, L, Superclass),
//     classMethods(Superclass, SuperMethodList),
//     finalMethodNotOverridden(Method, Superclass, SuperMethodList).
fn does_not_override_final_method_of_superclass(class: &Class, method: &Method) -> Bool {
	let superclass_name = class.super_class_name()?;
	let l = class.defining_loader()?;
	let superclass = loaded_class(&superclass_name, &l)?;
	let super_methods_list = superclass.methods()?;
	final_method_not_overridden(method, &superclass, &super_methods_list)
}

fn final_method_not_overridden(method: &Method, superclass: &Class, super_method_list: &Vec<Method>) -> Bool {
	fn member(name: &MethodName, descriptor: &MethodDescriptor, vec: &Vec<Method>) -> Result<bool> {
		for method in vec {
			if &method.name()? == name && &method.descriptor()? == descriptor {
				return Ok(true);
			}
		}
		Ok(false)
	}

	let name = method.name()?;
	let descriptor = method.descriptor()?;

	if member(&name, &descriptor, super_method_list)? {
		let access_flags = method.access_flags()?;

		if access_flags.is_final {
			// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
			//     methodName(Method, Name),
			//     methodDescriptor(Method, Descriptor),
			//     member(method(_, Name, Descriptor), SuperMethodList),
			//     isFinal(Method, Superclass),
			//     isPrivate(Method, Superclass).
			// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
			//     methodName(Method, Name),
			//     methodDescriptor(Method, Descriptor),
			//     member(method(_, Name, Descriptor), SuperMethodList),
			//     isFinal(Method, Superclass),
			//     isStatic(Method, Superclass).
			if access_flags.is_private || access_flags.is_static {
				Ok(())
			} else {
				fail("")
			}
		} else if access_flags.is_private || access_flags.is_static {
			// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
			//     methodName(Method, Name),
			//     methodDescriptor(Method, Descriptor),
			//     member(method(_, Name, Descriptor), SuperMethodList),
			//     isNotFinal(Method, Superclass),
			//     isPrivate(Method, Superclass),
			//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
			// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
			//     methodName(Method, Name),
			//     methodDescriptor(Method, Descriptor),
			//     member(method(_, Name, Descriptor), SuperMethodList),
			//     isNotFinal(Method, Superclass),
			//     isStatic(Method, Superclass),
			//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
			does_not_override_final_method_of_superclass(superclass, method)
		} else {
			// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
			//     methodName(Method, Name),
			//     methodDescriptor(Method, Descriptor),
			//     member(method(_, Name, Descriptor), SuperMethodList),
			//     isNotFinal(Method, Superclass),
			//     isNotStatic(Method, Superclass),
			//     isNotPrivate(Method, Superclass).
			Ok(())
		}
	} else {
		// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
		//     methodName(Method, Name),
		//     methodDescriptor(Method, Descriptor),
		//     notMember(method(_, Name, Descriptor), SuperMethodList),
		//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
		does_not_override_final_method_of_superclass(superclass, method)
	}
}

// 4.10.1.6

// methodWithCodeIsTypeSafe(Class, Method) :-
//     parseCodeAttribute(Class, Method, FrameSize, MaxStack,
//                        ParsedCode, Handlers, StackMap),
//     mergeStackMapAndCode(StackMap, ParsedCode, MergedCode),
//     methodInitialStackFrame(Class, Method, FrameSize, StackFrame, ReturnType),
//     Environment = environment(Class, Method, ReturnType, MergedCode,
//                               MaxStack, Handlers),
//     handlersAreLegal(Environment),
//     mergedCodeIsTypeSafe(Environment, MergedCode, StackFrame).
fn method_with_code_is_type_safe(class: &Class, method: &Method) -> Bool {
	let (frame_size, max_stack, parsed_code, handlers, stack_map) =
		parse_code_attribute(class, method)?;
	let merged_code = merge_stack_map_and_code(stack_map, parsed_code)?;
	let (stack_frame, return_type) = method_initial_stack_frame(class, method, frame_size)?;
	let environment = Environment {
		class: class.clone(),
		method: method.clone(),
		return_type,
		instructions: merged_code.clone(),
		max_stack,
		handlers,
	};
	handlers_are_legal(&environment)?;
	merged_code_is_type_safe(&environment, merged_code, stack_frame)
}

// handlersAreLegal(Environment) :-
//     exceptionHandlers(Environment, Handlers),
//     checklist(handlerIsLegal(Environment), Handlers).
fn handlers_are_legal(environment: &Environment) -> Bool {
	for handler in &environment.handlers {
		handler_is_legal(environment, handler)?;
	}

	Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Handler {
	start: usize,
	end: usize,
	target: usize,
	class_name: Option<ClassName>,
}

// handlerIsLegal(Environment, Handler) :-
//     Handler = handler(Start, End, Target, _),
//     Start < End,
//     allInstructions(Environment, Instructions),
//     member(instruction(Start, _), Instructions),
//     offsetStackFrame(Environment, Target, _),
//     instructionsIncludeEnd(Instructions, End),
//     currentClassLoader(Environment, CurrentLoader),
//     handlerExceptionClass(Handler, ExceptionClass, CurrentLoader),
//     isBootstrapLoader(BL),
//     isAssignable(ExceptionClass, class('java/lang/Throwable', BL)),
//     initHandlerIsLegal(Environment, Handler).
fn handler_is_legal(environment: &Environment, handler: &Handler) -> Bool {
	(handler.start < handler.end).fail("")?;
	let instructions = environment.all_instructions()?;

	instructions.iter()
		.filter_map(|x| match x {
			Instruction::Instruction(offset, _) => Some(*offset),
			_ => None,
		})
		.contains(&handler.start)
		.fail("")?;

	let _ = environment.offset_stack_frame(handler.target)?;
	instructions_include_end(instructions, handler.end)?;
	let exception_class = handler_exception_class(handler, environment.current_class_loader()?)?;
	VerificationType::is_assignable(&exception_class, &get_java_lang_throwable_class()?)?;
	init_handler_is_legal(environment, handler)
}


// instructionsIncludeEnd(Instructions, End) :-
//     member(instruction(End, _), Instructions).
// instructionsIncludeEnd(Instructions, End) :-
//     member(endOfCode(End), Instructions).
fn instructions_include_end(instructions: &[Instruction], end: usize) -> Bool {
	instructions.iter()
		.filter_map(|x| match x {
			Instruction::Instruction(offset, _) => Some(*offset),
			Instruction::EndOfCode(offset) => Some(*offset),
			_ => None,
		})
		.contains(&end)
		.fail("")
}

fn handler_exception_class(handler: &Handler, loader: &Loader) -> Result<VerificationType> {
	if let Some(name) = &handler.class_name {
		// handlerExceptionClass(handler(_, _, _, Name),
		//                       class(Name, L), L) :-
		//     Name \= 0.
		Ok(VerificationType::Class(name.clone(), loader.clone()))
	} else {
		// handlerExceptionClass(handler(_, _, _, 0),
		//                       class('java/lang/Throwable', BL), _) :-
		//     isBootstrapLoader(BL).
		Ok(get_java_lang_throwable_class()?)
	}
}

fn not_init_handler(environment: &Environment, _handler: &Handler) -> Bool {
	if environment.method.is_not_init() {
		// notInitHandler(Environment, Handler) :-
		//     Environment = environment(_Class, Method, _, Instructions, _, _),
		//     isNotInit(Method).
		Ok(())
	} else {
		// notInitHandler(Environment, Handler) :-
		//     Environment = environment(_Class, Method, _, Instructions, _, _),
		//     isInit(Method),
		//     member(instruction(_, invokespecial(CP)), Instructions),
		//     CP = method(MethodClassName, MethodName, Descriptor),
		//     MethodName \= '<init>'.
		let instructions = &environment.instructions;
		// this is searching the first invokespcial and then doing things with it
		//member(instruction(_, invokespecial(CP)), instructions)?;
		//CP = method(method_class_name, method_name, descriptor);
		//(method_name != "<init>").fail("")
		todo!()
	}
}

fn init_handler_is_legal(environment: &Environment, handler: &Handler) -> Bool {
	// initHandlerIsLegal(Environment, Handler) :-
	//     notInitHandler(Environment, Handler).
	let init_handler_is_legal_0 = || not_init_handler(environment, handler);

	// initHandlerIsLegal(Environment, Handler) :-
	//     isInitHandler(Environment, Handler),
	//     sublist(isApplicableInstruction(Target), Instructions,
	//             HandlerInstructions),
	//     noAttemptToReturnNormally(HandlerInstructions).
	let init_handler_is_legal_1 = |_| {
		is_init_handler(environment, handler)?;

		let handler_instructions = environment
			.all_instructions()?
			.iter()
			.filter(|instruction| is_applicable_instruction(handler.target, instruction));
		no_attempt_to_return_normally(handler_instructions)
	};

	init_handler_is_legal_0().or_else(init_handler_is_legal_1)
}

// isInitHandler(Environment, Handler) :-
//     Environment = environment(_Class, Method, _, Instructions, _, _),
//     isInit(Method).
//     member(instruction(_, invokespecial(CP)), Instructions),
//     CP = method(MethodClassName, '<init>', Descriptor).
fn is_init_handler(environment: &Environment, handler: &Handler) -> Bool {
	let method = &environment.method;
	let instructions = &environment.method;
	method.is_init().fail("")?;
	//member(instruction(_, invoekspecial(CP)), instructions)?;
	//CP = method(method_class_name, "<init>", descriptor);
	todo!()
}

// isApplicableInstruction(HandlerStart, instruction(Offset, _)) :-
//     Offset >= HandlerStart.
fn is_applicable_instruction(handler_start: usize, instruction: &Instruction) -> bool {
	match instruction {
		Instruction::Instruction(offset, _) => *offset >= handler_start,
		_ => false,
	}
}

// noAttemptToReturnNormally(Instructions) :-
//     notMember(instruction(_, return), Instructions).
// noAttemptToReturnNormally(Instructions) :-
//     member(instruction(_, athrow), Instructions).
fn no_attempt_to_return_normally<'a>(instructions: impl Iterator<Item=&'a Instruction>) -> Bool {
	let mut found_return = false;
	let mut found_athrow = false;

	for instruction in instructions {
		match instruction {
			Instruction::Instruction(_, Opcode::Return) => found_return = true,
			Instruction::Instruction(_, Opcode::AThrow) => found_athrow = true,
			_ => {},
		}
	}

	if found_return {
		if found_athrow {
			Ok(())
		} else {
			fail("")
		}
	} else {
		// no return -> is valid
		Ok(())
	}
}

// mergeStackMapAndCode([], CodeList, CodeList).
// mergeStackMapAndCode([stackMap(Offset, Map) | RestMap],
//                      [instruction(Offset, Parse) | RestCode],
//                      [stackMap(Offset, Map),
//                        instruction(Offset, Parse) | RestMerge]) :-
//     mergeStackMapAndCode(RestMap, RestCode, RestMerge).
// mergeStackMapAndCode([stackMap(OffsetM, Map) | RestMap],
//                      [instruction(OffsetP, Parse) | RestCode],
//                      [instruction(OffsetP, Parse) | RestMerge]) :-
//     OffsetP < OffsetM,
//     mergeStackMapAndCode([stackMap(OffsetM, Map) | RestMap],
//                          RestCode, RestMerge).
fn merge_stack_map_and_code(stack_map: Vec<Instruction>, code: Vec<Instruction>) -> Result<Vec<Instruction>> {
	let mut merged = Vec::with_capacity(stack_map.len() + code.len());

	let mut stack_map = stack_map.into_iter();
	let mut code = code.into_iter();

	loop {
		match (stack_map.next(), code.next()) {
			(None, Some(next_code)) => {
				merged.push(next_code);
			},
			(Some(frame), Some(opcode)) => {
				if frame.offset() == opcode.offset() {
					merged.push(frame);
					merged.push(opcode);
				} else if opcode.offset() < frame.offset() {
					merged.push(opcode);
				}
			},
			(None, None) => break,
			_ => fail("")?,
		}
	}

	Ok(merged)
}

// methodInitialStackFrame(Class, Method, FrameSize, frame(Locals, [], Flags),
//                         ReturnType):-
//     methodDescriptor(Method, Descriptor),
//     parseMethodDescriptor(Descriptor, RawArgs, ReturnType),
//     expandTypeList(RawArgs, Args),
//     methodInitialThisType(Class, Method, ThisList),
//     flags(ThisList, Flags),
//     append(ThisList, Args, ThisArgs),
//     expandToLength(ThisArgs, FrameSize, top, Locals).
fn method_initial_stack_frame(class: &Class, method: &Method, frame_size: usize) -> Result<(Frame, Option<VerificationType>)> {
	let (raw_args, return_type) = parse_method_descriptor(method.descriptor()?)?;
	let args = expand_type_list(raw_args)?;
	let this_list = method_initial_this_type(class, method)?;
	let flag_this_uninit = flags(&this_list)?;
	let this_args = {
		let mut vec = Vec::new();
		if let Some(i) = this_list {
			vec.push(i);
		}
		for i in args {
			vec.push(i);
		}
		vec
	};
	let locals = expand_to_length(this_args, frame_size, VerificationType::Top)?;

	let stack_frame = Frame {
		locals,
		operand_stack: OperandStack::empty(),
		flag_this_uninit,
	};

	Ok((stack_frame, return_type))
}

// expandTypeList([], []).
// expandTypeList([Item | List], [Item | Result]) :-
//     sizeOf(Item, 1),
//     expandTypeList(List, Result).
// expandTypeList([Item | List], [Item, top | Result]) :-
//     sizeOf(Item, 2),
//     expandTypeList(List, Result).
fn expand_type_list(list: Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	let mut out = Vec::new();
	for i in list {
		match i.size() {
			TypeSize::OneWord => {
				out.push(i);
			},
			TypeSize::TwoWord => {
				out.push(i);
				out.push(VerificationType::Top);
			},
		}
	}
	Ok(out)
}

// flags([uninitializedThis], [flagThisUninit]).
// flags(X, []) :- X \= [uninitializedThis].
fn flags(flags: &Option<VerificationType>) -> Result<bool> {
	match flags {
		Some(VerificationType::UninitializedThis) => Ok(true),
		_ => Ok(false),
	}
}

// expandToLength(List, Size, _Filler, List) :-
//     length(List, Size).
// expandToLength(List, Size, Filler, Result) :-
//     length(List, ListLength),
//     ListLength < Size,
//     Delta is Size - ListLength,
//     length(Extra, Delta),
//     checklist(=(Filler), Extra),
//     append(List, Extra, Result).
fn expand_to_length(mut list: Vec<VerificationType>, size: usize, filler: VerificationType) -> Result<LocalVariables> {
	if list.len() == size {
		Ok(LocalVariables::new(list))
	} else {
		let list_length = list.len();
		(list_length < size).fail("")?;
		let delta = size - list_length; // cannot panic, see check above
		for _ in 0..delta {
			list.push(filler.clone())
		}
		Ok(LocalVariables::new(list))
	}
}

fn method_initial_this_type(class: &Class, method: &Method) -> Result<Option<VerificationType>> {
	if method.access_flags()?.is_static {
		// methodInitialThisType(_Class, Method, []) :-
		//     methodAccessFlags(Method, AccessFlags),
		//     member(static, AccessFlags),
		//     methodName(Method, MethodName),
		//     MethodName \= '<init>'.
		let method_name = method.name()?;
		(method_name != MethodName::from(b"<init>")).fail("")?;
		Ok(None)
	} else {
		// methodInitialThisType(Class, Method, [This]) :-
		//     methodAccessFlags(Method, AccessFlags),
		//     notMember(static, AccessFlags),
		//     instanceMethodInitialThisType(Class, Method, This).
		Ok(Some(instance_method_initial_this_type(class, method)?))
	}
}

fn instance_method_initial_this_type(class: &Class, method: &Method) -> Result<VerificationType> {
	let class_name = class.class_name()?;
	let l = class.defining_loader()?;
	if method.name()? == MethodName::from(b"<init>") {
		let chain = superclass_chain(class_name, l)?;

		if chain.is_empty() {
			// instanceMethodInitialThisType(Class, Method, class('java/lang/Object', L)) :-
			//     methodName(Method, '<init>'),
			//     classDefiningLoader(Class, L),
			//     isBootstrapLoader(L),
			//     classClassName(Class, 'java/lang/Object').
			l.is_bootstrap_loader()?;
			(class_name == JAVA_LANG_OBJECT).fail("")?;
			Ok(get_java_lang_object_class()?)
		} else {
			// instanceMethodInitialThisType(Class, Method, uninitializedThis) :-
			//     methodName(Method, '<init>'),
			//     classClassName(Class, ClassName),
			//     classDefiningLoader(Class, CurrentLoader),
			//     superclassChain(ClassName, CurrentLoader, Chain),
			//     Chain \= [].
			Ok(VerificationType::UninitializedThis)
		}
	} else {
		// instanceMethodInitialThisType(Class, Method, class(ClassName, L)) :-
		//     methodName(Method, MethodName),
		//     MethodName \= '<init>',
		//     classDefiningLoader(Class, L),
		//     classClassName(Class, ClassName).
		Ok(VerificationType::Class(class_name.clone(), l.clone()))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
	Instruction(usize, Opcode),
	StackMap(usize, Frame),
	EndOfCode(usize),
}
impl Instruction {
	fn offset(&self) -> usize {
		*match self {
			Instruction::Instruction(offset, _) => offset,
			Instruction::StackMap(offset, _) => offset,
			Instruction::EndOfCode(offset) => offset,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FrameT {
	Frame(Frame),
	AfterGoto,
}

fn merged_code_is_type_safe(environment: &Environment, code: Vec<Instruction>, frame: Frame) -> Bool {
	let mut frame_t = FrameT::Frame(frame);

	for instruction in code {
		frame_t = match (instruction, frame_t) {
			// mergedCodeIsTypeSafe(Environment, [stackMap(Offset, MapFrame) | MoreCode],
			//                      frame(Locals, OperandStack, Flags)) :-
			//     frameIsAssignable(frame(Locals, OperandStack, Flags), MapFrame),
			//     mergedCodeIsTypeSafe(Environment, MoreCode, MapFrame).
			(Instruction::StackMap(_, map_frame), FrameT::Frame(frame)) => {
				Frame::is_assignable(&frame, &map_frame)?;
				FrameT::Frame(map_frame)
			},

			// mergedCodeIsTypeSafe(Environment, [instruction(Offset, Parse) | MoreCode],
			//                      frame(Locals, OperandStack, Flags)) :-
			//     instructionIsTypeSafe(Parse, Environment, Offset,
			//                           frame(Locals, OperandStack, Flags),
			//                           NextStackFrame, ExceptionStackFrame),
			//     instructionSatisfiesHandlers(Environment, Offset, ExceptionStackFrame),
			//     mergedCodeIsTypeSafe(Environment, MoreCode, NextStackFrame).
			(Instruction::Instruction(offset, parse), FrameT::Frame(frame)) => {
				let (next_stack_frame, exception_stack_frame) = instruction_is_type_safe(
					parse,
					environment,
					offset,
					frame
				)?;
				instruction_satisfies_handlers(environment, offset, exception_stack_frame)?;
				next_stack_frame
			},

			// mergedCodeIsTypeSafe(Environment, [stackMap(Offset, MapFrame) | MoreCode],
			//                      afterGoto) :-
			//     mergedCodeIsTypeSafe(Environment, MoreCode, MapFrame).
			(Instruction::StackMap(_, map_frame), FrameT::AfterGoto) => {
				FrameT::Frame(map_frame)
			},

			// mergedCodeIsTypeSafe(_Environment, [instruction(_, _) | _MoreCode],
			//                      afterGoto) :-
			//     write_ln('No stack frame after unconditional branch'),
			//     fail.
			(Instruction::Instruction(_, _), FrameT::AfterGoto) => {
				fail("No stack frame after unconditional branch")?
			},

			// mergedCodeIsTypeSafe(_Environment, [endOfCode(Offset)],
			//                      afterGoto).
			// no need to check for empty, since EndOfCode is only at the very end of the instruction list
			(Instruction::EndOfCode(_), FrameT::AfterGoto) => {
				return Ok(());
			},

			_ => fail("")?,
		}
	}

	// this should never get to run
	fail("")
}

// targetIsTypeSafe(Environment, StackFrame, Target) :-
//     offsetStackFrame(Environment, Target, Frame),
//     frameIsAssignable(StackFrame, Frame).
fn target_is_type_safe(environment: &Environment, stack_frame: &Frame, target: usize) -> Bool {
	let frame = environment.offset_stack_frame(target)?;
	Frame::is_assignable(stack_frame, frame)
}

// instructionSatisfiesHandlers(Environment, Offset, ExceptionStackFrame) :-
//     exceptionHandlers(Environment, Handlers),
//     sublist(isApplicableHandler(Offset), Handlers, ApplicableHandlers),
//     checklist(instructionSatisfiesHandler(Environment, ExceptionStackFrame),
//               ApplicableHandlers).
fn instruction_satisfies_handlers(environment: &Environment, offset: usize, exception_stack_frame: Frame) -> Bool {
	let applicable_handlers = environment.handlers.iter()
		.filter(|handler| is_applicable_handler(offset, handler));

	for handler in applicable_handlers {
		instruction_satisfies_handler(environment, &exception_stack_frame, handler)?;
	}

	Ok(())
}

// isApplicableHandler(Offset, handler(Start, End, _Target, _ClassName)) :-
//     Offset >= Start,
//     Offset < End.
fn is_applicable_handler(offset: usize, handler: &Handler) -> bool {
	offset >= handler.start && offset < handler.end
}

// instructionSatisfiesHandler(Environment, ExcStackFrame, Handler) :-
//     Handler = handler(_, _, Target, _),
//     currentClassLoader(Environment, CurrentLoader),
//     handlerExceptionClass(Handler, ExceptionClass, CurrentLoader),
//     /* The stack consists of just the exception. */
//     ExcStackFrame = frame(Locals, _, Flags),
//     TrueExcStackFrame = frame(Locals, [ ExceptionClass ], Flags),
//     operandStackHasLegalLength(Environment, TrueExcStackFrame),
//     targetIsTypeSafe(Environment, TrueExcStackFrame, Target).
fn instruction_satisfies_handler(environment: &Environment, exc_stack_frame: &Frame, handler: &Handler) -> Bool {
	let target = handler.target;
	let exception_class = handler_exception_class(handler, environment.current_class_loader()?)?;

	let true_exc_stack_frame = Frame {
		operand_stack: OperandStack::new(exception_class),
		..exc_stack_frame.clone()
	};

	true_exc_stack_frame.operand_stack.has_legal_length(environment)?;
	target_is_type_safe(environment, &true_exc_stack_frame, target)
}

// 4.10.1.7

impl Frame {
	// loadIsTypeSafe(Environment, Index, Type, StackFrame, NextStackFrame) :-
	//     StackFrame = frame(Locals, _OperandStack, _Flags),
	//     nth0(Index, Locals, ActualType),
	//     isAssignable(ActualType, Type),
	//     validTypeTransition(Environment, [], ActualType, StackFrame,
	//                         NextStackFrame).
	fn load_is_type_safe(self, environment: &Environment, index: &LvIndex, type_: VerificationType) -> Result<Frame> {
		let actual_type = self.locals.nth0(index.0)?;
		VerificationType::is_assignable(&actual_type, &type_)?;
		self.valid_type_transition(environment, [], Some(actual_type))
	}

	// storeIsTypeSafe(_Environment, Index, Type,
	//                 frame(Locals, OperandStack, Flags),
	//                 frame(NextLocals, NextOperandStack, Flags)) :-
	//     popMatchingType(OperandStack, Type, NextOperandStack, ActualType),
	//     modifyLocalVariable(Index, ActualType, Locals, NextLocals).
	fn store_is_type_safe(self, index: &LvIndex, type_: VerificationType) -> Result<Frame> {
		let (next_operand_stack, actual_type) = pop_matching_type(self.operand_stack, type_)?;
		Ok(Frame {
			locals: self.locals.modify_local_variable(index.0, actual_type)?,
			operand_stack: next_operand_stack,
			..self
		})
	}
}

impl LocalVariables {
	/// Given local variables `Locals`, modifying `Index` to have type `Type` results in the local variable list `NewLocals`.
	/// The modifications are somewhat involved, because some values (and their corresponding types) occupy two local variables.
	/// Hence, modifying `L[N]` may require modifying `L[N+1]` (because the type will occupy both the `N` and `N+1` slots) or
	/// `L[N-1]` (because local `N` used to be the upper half of the two word value/type starting at local `N-1`, and so local
	/// `N-1` must be invalidated), or both. This is described further below. We start at `L[0]` and count up.
	/// ```prolog
	/// modifyLocalVariable(Index, Type, Locals, NewLocals) :-
	///     modifyLocalVariable(0, Index, Type, Locals, NewLocals).
	/// ```
	/// Given `LocalsRest`, the suffix of the local variable list starting at index `I`, modifying local variable `Index` to have type
	/// `Type` results in the local variable list suffix `NextLocalsRest`.
	///
	/// If `I < Index-1`, just copy the input to the output and recurse forward. If `I = Index-1`, the type of local `I` may change.
	/// This can occur if `L[I]` has a type of size `2`. Once we set `L[I+1]` to the new type (and the corresponding value), the type/value
	/// of `L[I]` will be invalidated, as its upper half will be trashed. Then we recurse forward.
	/// ```prolog
	/// modifyLocalVariable(I, Index, Type,
	///                     [Locals1 | LocalsRest],
	///                     [Locals1 | NextLocalsRest] ) :-
	///     I < Index - 1,
	///     I1 is I + 1,
	///     modifyLocalVariable(I1, Index, Type, LocalsRest, NextLocalsRest).
	/// modifyLocalVariable(I, Index, Type,
	///                     [Locals1 | LocalsRest],
	///                     [NextLocals1 | NextLocalsRest] ) :-
	///     I =:= Index - 1,
	///     modifyPreIndexVariable(Locals1, NextLocals1),
	///     modifyLocalVariable(Index, Index, Type, LocalsRest, NextLocalsRest).
	/// ```
	/// When we find the variable, and it only occupies one word, we change it to `Type` and we're done. When we find the variable, and it
	/// occupies two words, we change its type to `Type` and the next word to `top`.
	/// ```prolog
	/// modifyLocalVariable(Index, Index, Type,
	///                     [_ | LocalsRest], [Type | LocalsRest]) :-
	///     sizeOf(Type, 1).
	/// modifyLocalVariable(Index, Index, Type,
	///                     [_, _ | LocalsRest], [Type, top | LocalsRest]) :-
	///     sizeOf(Type, 2).
	/// ```
	/// We refer to a local whose index immediately precedes a local whose type will be modified as a pre-index variable. The future type of
	/// a pre-index variable of type `InputType` is `Result`. If the type, `Type`, of the pre-index local is of size `1`, it doesn't change.
	/// If the type of the pre-index local, `Type`, is `2`, we need to mark the lower half of its two word value as unusable, by setting its
	/// type to `top`.
	fn modify_local_variable(mut self, index: usize, type_: VerificationType) -> Result<LocalVariables> {
		if let Some(mut t) = self.get_mut(index - 1) {
			*t = modify_pre_index_variable(t)?;
		}

		if type_.size() == TypeSize::TwoWord {
			if let Some(mut t) = self.get_mut(index + 1) {
				*t = VerificationType::Top;
			} else {
				fail("")?;
			}
		}

		if let Some(mut t) = self.get_mut(index) {
			*t = type_;
		}

		Ok(self)
	}
}

// modifyPreIndexVariable(Type, Type) :- sizeOf(Type, 1).
// modifyPreIndexVariable(Type, top) :- sizeOf(Type, 2).
fn modify_pre_index_variable(type_: &VerificationType) -> Result<VerificationType> {
	match type_.size() {
		TypeSize::OneWord => Ok(type_.clone()),
		TypeSize::TwoWord => Ok(VerificationType::Top),
	}
}

// 4.10.1.8

fn passes_protected_check<Name, Descriptor>(environment: &Environment, member_class_name: &ClassName, member_name: &Name, member_descriptor: &Descriptor, stack_frame: &Frame) -> Bool {
	let current_class_name = environment.class.class_name()?;
	let current_loader = environment.class.defining_loader()?;

	let chain = superclass_chain(&current_class_name, current_loader)?;

	let is_member = chain.iter()
		.map(|(name, _)| name)
		.contains(member_class_name);

	if !is_member {
		// passesProtectedCheck(Environment, MemberClassName, MemberName,
		//                      MemberDescriptor, StackFrame) :-
		//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
		//     superclassChain(CurrentClassName, CurrentLoader, Chain),
		//     notMember(class(MemberClassName, _), Chain).
		Ok(())
	} else {
		let list = classes_in_other_pkg_with_protected_member(
			(&current_class_name, &current_loader),
			member_name, member_descriptor, member_class_name, chain
		)?;

		if list.is_empty() {
			// passesProtectedCheck(Environment, MemberClassName, MemberName,
			//                      MemberDescriptor, StackFrame) :-
			//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
			//     superclassChain(CurrentClassName, CurrentLoader, Chain),
			//     member(class(MemberClassName, _), Chain),
			//     classesInOtherPkgWithProtectedMember(
			//       class(CurrentClassName, CurrentLoader),
			//       MemberName, MemberDescriptor, MemberClassName, Chain, []).
			Ok(())
		} else {
			let referenced_class = loaded_class(member_class_name, &current_loader)?;

			// passesProtectedCheck(Environment, MemberClassName, MemberName,
			//                      MemberDescriptor,
			//                      frame(_Locals, [Target | Rest], _Flags)) :-
			//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
			//     superclassChain(CurrentClassName, CurrentLoader, Chain),
			//     member(class(MemberClassName, _), Chain),
			//     classesInOtherPkgWithProtectedMember(
			//       class(CurrentClassName, CurrentLoader),
			//       MemberName, MemberDescriptor, MemberClassName, Chain, List),
			//     List /= [],
			//     loadedClass(MemberClassName, CurrentLoader, ReferencedClass),
			//     isNotProtected(ReferencedClass, MemberName, MemberDescriptor).
			let c = || -> Bool {
				is_not_protected(&referenced_class, member_name, member_descriptor)
			};

			// passesProtectedCheck(Environment, MemberClassName, MemberName,
			//                      MemberDescriptor,
			//                      frame(_Locals, [Target | Rest], _Flags)) :-
			//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
			//     superclassChain(CurrentClassName, CurrentLoader, Chain),
			//     member(class(MemberClassName, _), Chain),
			//     classesInOtherPkgWithProtectedMember(
			//       class(CurrentClassName, CurrentLoader),
			//       MemberName, MemberDescriptor, MemberClassName, Chain, List),
			//     List /= [],
			//     loadedClass(MemberClassName, CurrentLoader, ReferencedClass),
			//     isProtected(ReferencedClass, MemberName, MemberDescriptor),
			//     isAssignable(Target, class(CurrentClassName, CurrentLoader)).
			let d = |_| {
				let target = stack_frame.operand_stack.head()?;
				is_protected(&referenced_class, member_name, member_descriptor)?;
				VerificationType::is_assignable(target, &VerificationType::Class(current_class_name.clone(), current_loader.clone()))
			};

			c().or_else(d)
		}
	}
}

/// The predicate `classesInOtherPkgWithProtectedMember(Class, MemberName, MemberDescriptor, MemberClassName, Chain, List)`
/// is true if `List` is the set of classes in `Chain` with name `MemberClassName` that are in a different run-time package
/// than `Class` which have a protected member named `MemberName` with descriptor `MemberDescriptor`.
// classesInOtherPkgWithProtectedMember(_, _, _, _, [], []).
//
// classesInOtherPkgWithProtectedMember(Class, MemberName,
//                                      MemberDescriptor, MemberClassName,
//                                      [class(MemberClassName, L) | Tail],
//                                      [class(MemberClassName, L) | T]) :-
//     differentRuntimePackage(Class, class(MemberClassName, L)),
//     loadedClass(MemberClassName, L, Super),
//     isProtected(Super, MemberName, MemberDescriptor),
//     classesInOtherPkgWithProtectedMember(
//       Class, MemberName, MemberDescriptor, MemberClassName, Tail, T).
//
// classesInOtherPkgWithProtectedMember(Class, MemberName,
//                                      MemberDescriptor, MemberClassName,
//                                      [class(MemberClassName, L) | Tail],
//                                      T) :-
//     differentRuntimePackage(Class, class(MemberClassName, L)),
//     loadedClass(MemberClassName, L, Super),
//     isNotProtected(Super, MemberName, MemberDescriptor),
//     classesInOtherPkgWithProtectedMember(
//       Class, MemberName, MemberDescriptor, MemberClassName, Tail, T).
//
// classesInOtherPkgWithProtectedMember(Class, MemberName,
//                                      MemberDescriptor, MemberClassName,
//                                      [class(MemberClassName, L) | Tail],
//                                      T] :-
//     sameRuntimePackage(Class, class(MemberClassName, L)),
//     classesInOtherPkgWithProtectedMember(
//       Class, MemberName, MemberDescriptor, MemberClassName, Tail, T).
fn classes_in_other_pkg_with_protected_member<Name, Descriptor>(
	class: (&ClassName, &Loader),
	member_name: &Name,
	member_descriptor: &Descriptor,
	member_class_name: &ClassName,
	chain: Vec<(ClassName, Loader)>
) -> Result<Vec<(ClassName, Loader)>> {
	let mut vec = Vec::new();

	for (class_name, l) in chain {
		if &class_name == member_class_name {
			fn class_(_: &ClassName, _: &Loader) -> Class {
				todo!()
			}
			if different_runtime_package(&class_(class.0, class.1), &class_(&class_name, &l)).is_ok() {
				let super_ = loaded_class(member_class_name, &l)?;
				if is_protected(super_, member_name, member_descriptor).is_ok() {
					vec.push((class_name, l))
				} else {
					// is_not_protected
					// -> ignore
				}
			} else {
				// same_runtime_package
				// -> ignore
			}
		}
	}

	Ok(vec)
}

// sameRuntimePackage(Class1, Class2) :-
//     classDefiningLoader(Class1, L),
//     classDefiningLoader(Class2, L),
//     samePackageName(Class1, Class2).
fn same_runtime_package(class1: &Class, class2: &Class) -> Bool {
	let l1 = class1.defining_loader()?;
	let l2 = class2.defining_loader()?;
	(l1 == l2).fail("")?;
	same_package_name(class1, class2)
}

fn different_runtime_package(class1: &Class, class2: &Class) -> Bool {
	// differentRuntimePackage(Class1, Class2) :-
	//     classDefiningLoader(Class1, L1),
	//     classDefiningLoader(Class2, L2),
	//     L1 \= L2.
	let a = || {
		let l1 = class1.defining_loader()?;
		let l2 = class2.defining_loader()?;
		(l1 != l2).fail("")
	};

	// differentRuntimePackage(Class1, Class2) :-
	//     differentPackageName(Class1, Class2).
	let b = |_| {
		different_package_name(class1, class2)
	};

	a().or_else(b)
}

// 4.10.1.9

impl Frame {
	// exceptionStackFrame(StackFrame, ExceptionStackFrame) :-
	//     StackFrame = frame(Locals, _OperandStack, Flags),
	//     ExceptionStackFrame = frame(Locals, [], Flags).
	fn exception_stack_frame(&self) -> Result<Frame> {
		Ok(Frame {
			operand_stack: OperandStack::empty(),
			..self.clone()
		})
	}
}

impl VerificationType {
	// classDimension(arrayOf(X), Dimension) :-
	//     classDimension(X, Dimension1),
	//     Dimension is Dimension1 + 1.
	//
	// classDimension(_, Dimension) :-
	//     Dimension = 0.
	fn class_dimension(&self) -> usize {
		match self {
			Self::ArrayOf(x) => x.as_verification_type().class_dimension() + 1,
			_ => 0,
		}
	}
}

impl LocalVariables {
	// substitute(_Old, _New, [], []).
	// substitute(Old, New, [Old | FromRest], [New | ToRest]) :-
	//     substitute(Old, New, FromRest, ToRest).
	// substitute(Old, New, [From1 | FromRest], [From1 | ToRest]) :-
	//     From1 \= Old,
	//     substitute(Old, New, FromRest, ToRest).
	fn substitute(mut self, old: &VerificationType, new: &VerificationType) -> Self {
		for i in self.inner.iter_mut() {
			if i == old {
				*i = new.clone();
			}
		}
		self
	}
}
impl OperandStack {
	fn substitute(mut self, old: &VerificationType, new: &VerificationType) -> Self {
		for i in self.inner.iter_mut() {
			if i == old {
				*i = new.clone();
			}
		}
		self
	}

	fn not_member(&self, check: &VerificationType) -> Bool {
		for i in &self.inner {
			if i == check {
				return fail("")
			}
		}
		Ok(())
	}
}

fn instruction_is_type_safe(instruction: Opcode, environment: &Environment, offset: usize, stack_frame: Frame) -> Result<(FrameT, Frame)> {
	use VerificationType::*;
	use Opcode::*;

	let exception_stack_frame = stack_frame.exception_stack_frame()?;

	// instructionIsTypeSafe(Instruction, Environment, Offset, StackFrame,
	//                       NextStackFrame, ExceptionStackFrame) :-
	//     instructionHasEquivalentTypeRule(Instruction, IsomorphicInstruction),
	//     instructionIsTypeSafe(IsomorphicInstruction, Environment, Offset,
	//                           StackFrame, NextStackFrame,
	//                           ExceptionStackFrame).
	// note: this is implemented by using `A | B` match arms

	match instruction {
		// instructionIsTypeSafe(aaload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     nth1OperandStackIs(2, StackFrame, ArrayType),
		//     arrayComponentType(ArrayType, ComponentType),
		//     isBootstrapLoader(BL),
		//     validTypeTransition(Environment,
		//                         [int, arrayOf(class('java/lang/Object', BL))],
		//                         ComponentType, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AALoad => {
			let array_type = stack_frame.operand_stack.nth1(2)?;
			let component_type = array_type.array_component_type()?;
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[
					Int,
					ArrayOf(ArrayType::Other(Box::new(get_java_lang_object_class()?)))
				],
				Some(component_type.as_verification_type())
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(aastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     isBootstrapLoader(BL),
		//     canPop(StackFrame,
		//            [class('java/lang/Object', BL),
		//             int,
		//             arrayOf(class('java/lang/Object', BL))],
		//            NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[
					get_java_lang_object_class()?,
					Int,
					ArrayOf(ArrayType::Other(Box::new(get_java_lang_object_class()?)))
				]
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(aconst_null, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], null, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AConstNull => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Null)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(aload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, reference, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ALoad(index) => {
			let next_stack_frame = stack_frame.load_is_type_safe(environment, &index, Reference)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(anewarray(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     validTypeTransition(Environment, [int], arrayOf(CP),
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ANewArray(cp) => {
			match cp {
				Class(_, _) | ArrayOf(_) => Ok(()),
				_ => fail("")
			}?;

			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int],
				Some(ArrayOf(ArrayType::Other(Box::new(cp))))
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(areturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, ReturnType),
		//     isAssignable(ReturnType, reference),
		//     canPop(StackFrame, [ReturnType], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AReturn => {
			if let Some(return_type) = &environment.return_type {
				VerificationType::is_assignable(return_type, &Reference)?;
				let _popped_stack_frame = stack_frame.can_pop([return_type.clone()])?;
				Ok((FrameT::AfterGoto, exception_stack_frame))
			} else {
				fail("")
			}
		},

		// instructionIsTypeSafe(arraylength, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     nth1OperandStackIs(1, StackFrame, ArrayType),
		//     arrayComponentType(ArrayType, _),
		//     validTypeTransition(Environment, [top], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ArrayLength => {
			let array_type = stack_frame.operand_stack.nth1(1)?;
			let _ = array_type.array_component_type()?;
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Top],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(astore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, reference, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AStore(index) => {
			let next_stack_frame = stack_frame.store_is_type_safe(&index, Reference)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(athrow, _Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     isBootstrapLoader(BL),
		//     canPop(StackFrame, [class('java/lang/Throwable', BL)], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AThrow => {
			let _popped_stack_frame = stack_frame.can_pop(
				[get_java_lang_throwable_class()?]
			)?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(baload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :
		//     nth1OperandStackIs(2, StackFrame, ArrayType),
		//     isSmallArray(ArrayType),
		//     validTypeTransition(Environment, [int, top], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		BALoad => {
			let array_type = stack_frame.operand_stack.nth1(2)?;
			array_type.is_small_array()?;
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, Top],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(bastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     nth1OperandStackIs(3, StackFrame, ArrayType),
		//     isSmallArray(ArrayType),
		//     canPop(StackFrame, [int, int, top], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		BAStore => {
			let array_type = stack_frame.operand_stack.nth1(3)?;
			array_type.is_small_array()?;
			let next_stack_frame = stack_frame.can_pop([Int, Int, Top])?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// TODO: decide what to do with this
		Breakpoint => {
			// not safe
			fail("")
		}

		// instructionIsTypeSafe(caload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(char)], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		CALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Char)],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(castore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(char)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		CAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Int, Int, ArrayOf(ArrayType::Char)]
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(checkcast(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     isBootstrapLoader(BL),
		//     validTypeTransition(Environment, [class('java/lang/Object', BL)], CP,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		CheckCast(cp) => {
			match cp {
				Class(_, _) | ArrayOf(_) => Ok(()),
				_ => fail(""),
			}?;

			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[get_java_lang_object_class()?],
				Some(cp)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2f => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2i => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2l => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dadd, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double, double], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(ddiv, dadd).
		// instructionHasEquivalentTypeRule(dmul, dadd).
		// instructionHasEquivalentTypeRule(drem, dadd).
		// instructionHasEquivalentTypeRule(dsub, dadd).
		DAdd | DDiv | DMul | DRem | DSub => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double, Double],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(daload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(double)], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Other(Box::new(Double)))],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [double, int, arrayOf(double)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Double, Int, ArrayOf(ArrayType::Other(Box::new(Double)))]
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dcmpg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double, double], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(dcmpl, dcmpg).
		DCmpG | DCmpL => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double, Double],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(dconst_1, dconst_0).
		DConst0 | DConst1 => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DLoad(index) => {
			let next_stack_frame = stack_frame.load_is_type_safe(environment, &index, Double)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DNeg => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Double],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dreturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, double),
		//     canPop(StackFrame, [double], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DReturn => {
			(environment.return_type == Some(Double)).fail("")?;
			let _popped_stack_frame = stack_frame.can_pop([Double])?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(dstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DStore(index) => {
			let next_stack_frame = stack_frame.store_is_type_safe(&index, Double)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     popCategory1(InputOperandStack, Type, _),
		//     canSafelyPush(Environment, InputOperandStack, Type, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup => {
			let (type_, _) = pop_category_1(stack_frame.operand_stack.clone())?;
			let output_operand_stack = can_safely_push(environment, stack_frame.operand_stack, type_)?;
			let next_stack_frame = Frame {
				operand_stack: output_operand_stack,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup_x1, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     popCategory1(InputOperandStack, Type1, Stack1),
		//     popCategory1(Stack1, Type2, Rest),
		//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
		//                       OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DupX1 => {
			let (type_1, stack_1) = pop_category_1(stack_frame.operand_stack)?;
			let (type_2, rest) = pop_category_1(stack_1)?;
			let output_operand_stack = can_safely_push_list(
				environment,
				rest,
				[type_1.clone(), type_2, type_1]
			)?;
			let next_stack_frame = Frame {
				operand_stack: output_operand_stack,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup_x2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DupX2 => {
			fn dup_x2_form_is_type_safe(environment: &Environment, input_operand_stack: &OperandStack) -> Result<OperandStack> {
				// dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
				//
				// dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).

				let (type_1, stack_1) = pop_category_1(input_operand_stack.clone())?;

				// dup_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory1(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Stack2),
				//     popCategory1(Stack2, Type3, Rest),
				//     canSafelyPushList(Environment, Rest, [Type1, Type3, Type2, Type1],
				//                       OutputOperandStack).
				let form_1 = || -> Result<OperandStack> {
					let (type_2, stack_2) = pop_category_1(stack_1.clone())?;
					let (type_3, rest) = pop_category_1(stack_2)?;
					can_safely_push_list(
						environment,
						rest,
						[type_1.clone(), type_3, type_2, type_1.clone()]
					)
				};

				// dup_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory1(InputOperandStack, Type1, Stack1),
				//     popCategory2(Stack1, Type2, Rest),
				//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
				//                       OutputOperandStack).
				let form_2 = || {
					let (type_2, rest) = pop_category_2(stack_1.clone())?;
					can_safely_push_list(
						environment,
						rest,
						[type_1.clone(), type_2, type_1.clone()]
					)
				};

				form_1().or_else(|_| form_2())
			}

			let output_operand_stack = dup_x2_form_is_type_safe(environment, &stack_frame.operand_stack)?;
			let next_stack_frame = Frame {
				operand_stack: output_operand_stack,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2FormIsTypeSafe(Environment,InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2 => {
			fn dup_2_form_is_type_safe(environment: &Environment, input_operand_stack: OperandStack) -> Result<OperandStack> {
				// dup2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2Form1IsTypeSafe(Environment,InputOperandStack, OutputOperandStack).
				//
				// dup2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2Form2IsTypeSafe(Environment,InputOperandStack, OutputOperandStack).

				// dup2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack):-
				//     popCategory1(InputOperandStack, Type1, TempStack),
				//     popCategory1(TempStack, Type2, _),
				//     canSafelyPushList(Environment, InputOperandStack, [Type1, Type2],
				//                       OutputOperandStack).
				let form_1 = || {
					let (type_1, temp_stack) = pop_category_1(input_operand_stack.clone())?;
					let (type_2, _) = pop_category_1(temp_stack)?;
					can_safely_push_list(
						environment,
						input_operand_stack.clone(),
						[type_1, type_2]
					)
				};

				// dup2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack):-
				//     popCategory2(InputOperandStack, Type, _),
				//     canSafelyPush(Environment, InputOperandStack, Type, OutputOperandStack).
				let form_2 = || {
					let (type_, _) = pop_category_2(input_operand_stack.clone())?;
					can_safely_push(
						environment,
						input_operand_stack.clone(),
						type_
					)
				};

				form_1().or_else(|_| form_2())
			}

			let output_operand_stack = dup_2_form_is_type_safe(environment, stack_frame.operand_stack)?;
			let next_stack_frame = Frame {
				operand_stack: output_operand_stack,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup2_x1, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2_x1FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2X1 => {
			fn dup_2x1_form_is_type_safe(environment: &Environment, input_operand_stack: &OperandStack) -> Result<OperandStack> {
				// dup2_x1FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x1Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
				//
				// dup2_x1FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x1Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).

				// dup2_x1Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory1(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Stack2),
				//     popCategory1(Stack2, Type3, Rest),
				//     canSafelyPushList(Environment, Rest, [Type2, Type1, Type3, Type2, Type1],
				//                       OutputOperandStack).
				let form_1 = || {
					let (type_1, stack_1) = pop_category_1(input_operand_stack.clone())?;
					let (type_2, stack_2) = pop_category_1(stack_1)?;
					let (type_3, rest) = pop_category_1(stack_2)?;
					can_safely_push_list(
						environment,
						rest,
						[type_2.clone(), type_1.clone(), type_3, type_2, type_1]
					)
				};

				// dup2_x1Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory2(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Rest),
				//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
				//                       OutputOperandStack).
				let form_2 = || {
					let (type_1, stack_1) = pop_category_2(input_operand_stack.clone())?;
					let (type_2, rest) = pop_category_1(stack_1)?;
					can_safely_push_list(
						environment,
						rest,
						[type_1.clone(), type_2, type_1]
					)
				};

				form_1().or_else(|_| form_2())
			}

			let output_operand_stack = dup_2x1_form_is_type_safe(environment, &stack_frame.operand_stack)?;
			let next_stack_frame = Frame {
				locals: stack_frame.locals.clone(),
				operand_stack: output_operand_stack,
				flag_this_uninit: stack_frame.flag_this_uninit
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup2_x2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2X2 => {
			fn dup_2x2_form_is_type_safe(environment: &Environment, input_operand_stack: OperandStack) -> Result<OperandStack> {
				// dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
				//
				// dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
				//
				// dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x2Form3IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
				//
				// dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     dup2_x2Form4IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).

				// dup2_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory1(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Stack2),
				//     popCategory1(Stack2, Type3, Stack3),
				//     popCategory1(Stack3, Type4, Rest),
				//     canSafelyPushList(Environment, Rest,
				//                       [Type2, Type1, Type4, Type3, Type2, Type1],
				//                       OutputOperandStack).
				let form_1 = || {
					let (type_1, stack_1) = pop_category_1(input_operand_stack.clone())?;
					let (type_2, stack_2) = pop_category_1(stack_1)?;
					let (type_3, stack_3) = pop_category_1(stack_2)?;
					let (type_4, rest) = pop_category_1(stack_3)?;
					can_safely_push_list(
						environment,
						rest,
						[type_2.clone(), type_1.clone(), type_4, type_3, type_2, type_1]
					)
				};

				// dup2_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory2(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Stack2),
				//     popCategory1(Stack2, Type3, Rest),
				//     canSafelyPushList(Environment, Rest,
				//                       [Type1, Type3, Type2, Type1],
				//                       OutputOperandStack).
				let form_2 = || {
					let (type_1, stack_1) = pop_category_2(input_operand_stack.clone())?;
					let (type_2, stack_2) = pop_category_1(stack_1)?;
					let (type_3, rest) = pop_category_1(stack_2)?;
					can_safely_push_list(
						environment,
						rest,
						[type_1.clone(), type_3, type_2, type_1]
					)
				};

				// dup2_x2Form3IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory1(InputOperandStack, Type1, Stack1),
				//     popCategory1(Stack1, Type2, Stack2),
				//     popCategory2(Stack2, Type3, Rest),
				//     canSafelyPushList(Environment, Rest,
				//                       [Type2, Type1, Type3, Type2, Type1],
				//                       OutputOperandStack).
				let form_3 = || {
					let (type_1, stack_1) = pop_category_1(input_operand_stack.clone())?;
					let (type_2, stack_2) = pop_category_1(stack_1)?;
					let (type_3, rest) = pop_category_2(stack_2)?;
					can_safely_push_list(
						environment,
						rest,
						[type_2.clone(), type_1.clone(), type_3, type_2, type_1]
					)
				};

				// dup2_x2Form4IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
				//     popCategory2(InputOperandStack, Type1, Stack1),
				//     popCategory2(Stack1, Type2, Rest),
				//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
				//                       OutputOperandStack).
				let form_4 = || {
					let (type_1, stack_1) = pop_category_2(input_operand_stack.clone())?;
					let (type_2, rest) = pop_category_2(stack_1)?;
					can_safely_push_list(
						environment,
						rest,
						[type_1.clone(), type_2, type_1]
					)
				};

				form_1().or_else(|_| form_2()).or_else(|_| form_3()).or_else(|_| form_4())
			}

			let output_operand_stack = dup_2x2_form_is_type_safe(environment, stack_frame.operand_stack)?;
			let next_stack_frame = Frame {
				locals: stack_frame.locals.clone(),
				operand_stack: output_operand_stack,
				flag_this_uninit: stack_frame.flag_this_uninit
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(f2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2d => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(f2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2i => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(f2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2l => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fadd, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float, float], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(fdiv, fadd).
		// instructionHasEquivalentTypeRule(fmul, fadd).
		// instructionHasEquivalentTypeRule(frem, fadd).
		// instructionHasEquivalentTypeRule(fsub, fadd).
		FAdd | FDiv | FMul | FRem | FSub => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float, Float],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(faload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(float)], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Other(Box::new(Float)))],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [float, int, arrayOf(float)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Float, Int, ArrayOf(ArrayType::Other(Box::new(Float)))],
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fcmpg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float, float], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(fcmpl, fcmpg).
		FCmpG | FCmpL => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float, Float],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(fconst_1, fconst_0).
		// instructionHasEquivalentTypeRule(fconst_2, fconst_0).
		FConst0 | FConst1 | FConst2 => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FLoad(index) => {
			let next_stack_frame = stack_frame.load_is_type_safe(environment, &index, Float)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FNeg => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Float],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(freturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, float),
		//     canPop(StackFrame, [float], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FReturn => {
			(environment.return_type == Some(Float)).fail("")?;
			let _popped_stack_frame = stack_frame.can_pop([Float])?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(fstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FStore(index) => {
			let next_stack_frame = stack_frame.store_is_type_safe(&index, Float)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(getfield(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(FieldClass, FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     passesProtectedCheck(Environment, FieldClass, FieldName,
		//                          FieldDescriptor, StackFrame),
		//     validTypeTransition(Environment, [class(FieldClass)], FieldType,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		GetField(field) => {
			let field_type = parse_field_descriptor(&field.descriptor)?;
			passes_protected_check(environment, &field.class, &field.name, &field.descriptor, &stack_frame)?;

			let loader = get_bootstrap_loader()?; // TODO: figure this out
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Class(field.class, loader)],
				Some(field_type)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(getstatic(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(_FieldClass, _FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     validTypeTransition(Environment, [], FieldType,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		GetStatic(field) => {
			let field_type = parse_field_descriptor(&field.descriptor)?;
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(field_type)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(goto(Target), Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     targetIsTypeSafe(Environment, StackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(goto_w(Target), goto(Target)).
		Goto(target) => {
			target_is_type_safe(environment, &stack_frame, target.0)?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(i2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2d => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(i2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2f => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(i2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2l => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iadd, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, int], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(iand, iadd).
		// instructionHasEquivalentTypeRule(idiv, iadd).
		// instructionHasEquivalentTypeRule(imul, iadd).
		// instructionHasEquivalentTypeRule(ior, iadd).
		// instructionHasEquivalentTypeRule(irem, iadd).
		// instructionHasEquivalentTypeRule(ishl, iadd).
		// instructionHasEquivalentTypeRule(ishr, iadd).
		// instructionHasEquivalentTypeRule(iushr, iadd).
		// instructionHasEquivalentTypeRule(isub, iadd).
		// instructionHasEquivalentTypeRule(ixor, iadd).
		IAdd | IAnd | IDiv | IMul | IOr | IRem | IShl | IShr | IUShr | ISub | IXor => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, Int],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iaload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(int)], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Other(Box::new(Int)))],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(int)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Int, Int, ArrayOf(ArrayType::Other(Box::new(Int)))]
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iconst_m1, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(iconst_0, iconst_m1).
		// instructionHasEquivalentTypeRule(iconst_1, iconst_m1).
		// instructionHasEquivalentTypeRule(iconst_2, iconst_m1).
		// instructionHasEquivalentTypeRule(iconst_3, iconst_m1).
		// instructionHasEquivalentTypeRule(iconst_4, iconst_m1).
		// instructionHasEquivalentTypeRule(iconst_5, iconst_m1).
		IConstM1 | IConst0 | IConst1 | IConst2 | IConst3 | IConst4 | IConst5 => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		}

		// instructionIsTypeSafe(if_acmpeq(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference, reference], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(if_acmpne(Target), if_acmpeq(Target)).
		IfACmpEq(target) | IfACmpNe(target) => {
			let next_stack_frame = stack_frame.can_pop([Reference, Reference])?;
			target_is_type_safe(environment, &next_stack_frame, target.0)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(if_icmpeq(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(if_icmpge(Target), if_icmpeq(Target)).
		// instructionHasEquivalentTypeRule(if_icmpgt(Target), if_icmpeq(Target)).
		// instructionHasEquivalentTypeRule(if_icmple(Target), if_icmpeq(Target)).
		// instructionHasEquivalentTypeRule(if_icmplt(Target), if_icmpeq(Target)).
		// instructionHasEquivalentTypeRule(if_icmpne(Target), if_icmpeq(Target)).
		IfICmpEq(target) | IfICmpGe(target) | IfICmpGt(target) |
		IfICmpLe(target) | IfICmpLt(target) | IfICmpNe(target) => {
			let next_stack_frame = stack_frame.can_pop([Int, Int])?;
			target_is_type_safe(environment, &next_stack_frame, target.0)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ifeq(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(ifge(Target), ifeq(Target)).
		// instructionHasEquivalentTypeRule(ifgt(Target), ifeq(Target)).
		// instructionHasEquivalentTypeRule(ifle(Target), ifeq(Target)).
		// instructionHasEquivalentTypeRule(iflt(Target), ifeq(Target)).
		// instructionHasEquivalentTypeRule(ifne(Target), ifeq(Target)).
		IfEq(target) | IfGe(target) | IfGt(target) |
		IfLe(target) | IfLt(target) | IfNe(target) => {
			let next_stack_frame = stack_frame.can_pop([Int])?;
			target_is_type_safe(environment, &next_stack_frame, target.0)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ifnonnull(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(ifnull(Target), ifnonnull(Target)).
		IfNonNull(target) | IfNull(target) => {
			let next_stack_frame = stack_frame.can_pop([Reference])?;
			target_is_type_safe(environment, &next_stack_frame, target.0)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iinc(Index, _Value), _Environment, _Offset,
		//                       StackFrame, StackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, _OperandStack, _Flags),
		//     nth0(Index, Locals, int),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IInc{ lv_index, .. } => {
			let index = lv_index.0;
			(stack_frame.locals.nth0(index)? == Int).fail("")?;
			let next_stack_frame = stack_frame.clone();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ILoad(index) => {
			let next_stack_frame = stack_frame.load_is_type_safe(environment, &index, Int)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// TODO: decide what to do with these
		ImpDep1 | ImpDep2 => {
			fail("not safe")
		}

		// instructionIsTypeSafe(ineg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(i2b, ineg).
		// instructionHasEquivalentTypeRule(i2c, ineg).
		// instructionHasEquivalentTypeRule(i2s, ineg).
		INeg | I2b | I2c | I2s => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(instanceof(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     isBootstrapLoader(BL),
		//     validTypeTransition(Environment, [class('java/lang/Object', BL)], int,
		//                         StackFrame,NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InstanceOf(cp) => {
			match cp {
				Class(_, _) | ArrayOf(_) => Ok(()),
				_ => fail("")
			}?;

			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[get_java_lang_object_class()?],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(invokedynamic(CP,0,0), Environment, _Offset,
		//                       StackFrame, NextStackFrame, ExceptionStackFrame) :-
		//     CP = dmethod(CallSiteName, Descriptor),
		//     CallSiteName \= '<init>',
		//     CallSiteName \= '<clinit>',
		//     parseMethodDescriptor(Descriptor, OperandArgList, ReturnType),
		//     reverse(OperandArgList, StackArgList),
		//     validTypeTransition(Environment, StackArgList, ReturnType,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InvokeDynamic{..} => {
			let next_stack_frame = todo!();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(invokeinterface(CP, Count, 0), Environment, _Offset,
		//                       StackFrame, NextStackFrame, ExceptionStackFrame) :-
		//     CP = imethod(MethodIntfName, MethodName, Descriptor),
		//     MethodName \= '<init>',
		//     MethodName \= '<clinit>',
		//     parseMethodDescriptor(Descriptor, OperandArgList, ReturnType),
		//     currentClassLoader(Environment, CurrentLoader),
		//     reverse([class(MethodIntfName, CurrentLoader) | OperandArgList],
		//             StackArgList),
		//     canPop(StackFrame, StackArgList, TempFrame),
		//     validTypeTransition(Environment, [], ReturnType, TempFrame, NextStackFrame),
		//     countIsValid(Count, StackFrame, TempFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InvokeInterface{..} => {
			fn count_is_valid(count: &usize, input_frame: &Frame, output_frame: &Frame) -> Bool {
				// countIsValid(Count, InputFrame, OutputFrame) :-
				//     InputFrame = frame(_Locals1, OperandStack1, _Flags1),
				//     OutputFrame = frame(_Locals2, OperandStack2, _Flags2),
				//     length(OperandStack1, Length1),
				//     length(OperandStack2, Length2),
				//     Count =:= Length1 - Length2.
				let length1 = input_frame.operand_stack.inner.len();
				let length2 = output_frame.operand_stack.inner.len();
				(count + length2 == length1).fail("")
			}
			let next_stack_frame = todo!();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},


		// instructionIsTypeSafe(invokespecial(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = method(MethodClassName, MethodName, Descriptor),
		//     MethodName \= '<init>',
		//     MethodName \= '<clinit>',
		//     parseMethodDescriptor(Descriptor, OperandArgList, ReturnType),
		//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
		//     reverse([class(CurrentClassName, CurrentLoader) | OperandArgList],
		//             StackArgList),
		//     validTypeTransition(Environment, StackArgList, ReturnType,
		//                         StackFrame, NextStackFrame),
		//     reverse([class(MethodClassName, CurrentLoader) | OperandArgList],
		//             StackArgList2),
		//     validTypeTransition(Environment, StackArgList2, ReturnType,
		//                         StackFrame, _ResultStackFrame),
		//     isAssignable(class(CurrentClassName, CurrentLoader),
		//                  class(MethodClassName, CurrentLoader)).
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InvokeSpecial{..} => {
			let next_stack_frame = todo!();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(invokespecial(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = method(MethodClassName, '<init>', Descriptor),
		//     parseMethodDescriptor(Descriptor, OperandArgList, void),
		//     reverse(OperandArgList, StackArgList),
		//     canPop(StackFrame, StackArgList, TempFrame),
		//     TempFrame = frame(Locals, FullOperandStack, Flags),
		//     FullOperandStack = [UninitializedArg | OperandStack],
		//     currentClassLoader(Environment, CurrentLoader),
		//     rewrittenUninitializedType(UninitializedArg, Environment,
		//                                class(MethodClassName, CurrentLoader), This),
		//     rewrittenInitializationFlags(UninitializedArg, Flags, NextFlags),
		//     substitute(UninitializedArg, This, OperandStack, NextOperandStack),
		//     substitute(UninitializedArg, This, Locals, NextLocals),
		//     NextStackFrame = frame(NextLocals, NextOperandStack, NextFlags),
		//     ExceptionStackFrame = frame(Locals, [], Flags),
		//     passesProtectedCheck(Environment, MethodClassName, '<init>',
		//                          Descriptor, NextStackFrame).
		InvokeSpecial{..} => {
			let next_stack_frame = todo!();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// rewrittenUninitializedType(uninitializedThis, Environment,
		//                            MethodClass, MethodClass) :-
		//     MethodClass = class(MethodClassName, CurrentLoader),
		//     thisClass(Environment, MethodClass).
		//
		// rewrittenUninitializedType(uninitializedThis, Environment,
		//                            MethodClass, MethodClass) :-
		//     MethodClass = class(MethodClassName, CurrentLoader),
		//     thisClass(Environment, class(thisClassName, thisLoader)),
		//     superclassChain(thisClassName, thisLoader, [MethodClass | Rest]).
		//
		// rewrittenUninitializedType(uninitialized(Address), Environment,
		//                            MethodClass, MethodClass) :-
		//     allInstructions(Environment, Instructions),
		//     member(instruction(Address, new(MethodClass)), Instructions).
		//
		// rewrittenInitializationFlags(uninitializedThis, _Flags, []).
		// rewrittenInitializationFlags(uninitialized(_), Flags, Flags).

		// instructionIsTypeSafe(invokestatic(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = method(_MethodClassName, MethodName, Descriptor),
		//     MethodName \= '<init>',
		//     MethodName \= '<clinit>',
		//     parseMethodDescriptor(Descriptor, OperandArgList, ReturnType),
		//     reverse(OperandArgList, StackArgList),
		//     validTypeTransition(Environment, StackArgList, ReturnType,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InvokeStatic(method) => {
			(method.name != MethodName::from(b"<init>") && method.name != MethodName::from(b"<clinit>")).fail("")?;
			let (operand_arg_list, return_type) = parse_method_descriptor(method.descriptor)?;
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				operand_arg_list.into_iter().rev(),
				return_type
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(invokevirtual(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = method(MethodClassName, MethodName, Descriptor),
		//     MethodName \= '<init>',
		//     MethodName \= '<clinit>',
		//     parseMethodDescriptor(Descriptor, OperandArgList, ReturnType),
		//     reverse(OperandArgList, ArgList),
		//     currentClassLoader(Environment, CurrentLoader),
		//     reverse([class(MethodClassName, CurrentLoader) | OperandArgList],
		//             StackArgList),
		//     validTypeTransition(Environment, StackArgList, ReturnType,
		//                         StackFrame, NextStackFrame),
		//     canPop(StackFrame, ArgList, PoppedFrame),
		//     passesProtectedCheck(Environment, MethodClassName, MethodName,
		//                          Descriptor, PoppedFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InvokeVirtual(method) => {
			(method.name != MethodName::from(b"<init>") && method.name != MethodName::from(b"<clinit>")).fail("")?;
			let (operand_arg_list, return_type) = parse_method_descriptor(method.descriptor.clone())?;
			let arg_list: Vec<_> = operand_arg_list.iter().rev().cloned().collect();
			let current_loader = environment.current_class_loader()?.clone();
			fn reverse<T>(vec: Vec<T>) -> Vec<T> {
				vec.into_iter().rev().collect()
			}
			fn as_end<T>(last: T, mut vec: Vec<T>) -> Vec<T> { // TODO: make this better!
				vec.push(last);
				vec
			}
			let stack_arg_list = as_end(Class(method.class.clone(), current_loader), reverse(operand_arg_list));
			let next_stack_frame = stack_frame.clone().valid_type_transition(
				environment,
				stack_arg_list,
				return_type
			)?;
			let popped_frame = stack_frame.can_pop(arg_list)?;
			passes_protected_check(environment, &method.class, &method.name, &method.descriptor, &popped_frame)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ireturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, int),
		//     canPop(StackFrame, [int], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IReturn => {
			(environment.return_type == Some(Int)).fail("")?;
			let _popped_stack_frame = stack_frame.can_pop([Int])?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(istore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IStore(index) => {
			let next_stack_frame = stack_frame.store_is_type_safe(&index, Int)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2d => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2f => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2i => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ladd, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long, long], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(land, ladd).
		// instructionHasEquivalentTypeRule(ldiv, ladd).
		// instructionHasEquivalentTypeRule(lmul, ladd).
		// instructionHasEquivalentTypeRule(lor, ladd).
		// instructionHasEquivalentTypeRule(lrem, ladd).
		// instructionHasEquivalentTypeRule(lsub, ladd).
		// instructionHasEquivalentTypeRule(lxor, ladd).
		LAdd | LAnd | LDiv | LMul | LOr | LRem | LSub | LXor => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long, Long],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(laload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(long)], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Other(Box::new(Long)))],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [long, int, arrayOf(long)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Long, Int, ArrayOf(ArrayType::Other(Box::new(Long)))]
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lcmp, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long, long], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LCmp => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long, Long],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(lconst_1, lconst_0).
		LConst0 | LConst1 => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ldc(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     functor(CP, Tag, _),
		//     isBootstrapLoader(BL),
		//     member([Tag, Type], [
		//         [int, int],
		//         [float, float],
		//         [string, class('java/lang/String', BL)],
		//         [classConst, class('java/lang/Class', BL)],
		//         [methodTypeConst, class('java/lang/invoke/MethodType', BL)],
		//         [methodHandleConst, class('java/lang/invoke/MethodHandle', BL)],
		//     ]),
		//     validTypeTransition(Environment, [], Type, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(ldc_w(CP), ldc(CP))
		LdcInt(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		LdcFloat(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Float)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		LdcReferenceString(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(get_java_lang_string_class()?)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		LdcReferenceClass(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(get_java_lang_class_class()?)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		LdcReferenceMethodType(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(get_java_lang_invoke_method_type_class()?)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		LdcReferenceMethodHandle(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(get_java_lang_invoke_method_handle_class()?)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ldc2_w(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     functor(CP, Tag, _),
		//     member(Tag, [long, double]),
		//     validTypeTransition(Environment, [], Tag, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Ldc2WLong(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
		Ldc2WDouble(_) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Double)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LLoad(index) => {
			let next_stack_frame = stack_frame.load_is_type_safe(environment, &index, Long)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LNeg => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Long],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lookupswitch(Targets, Keys), Environment, _, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     sort(Keys, Keys),
		//     canPop(StackFrame, [int], BranchStackFrame),
		//     checklist(targetIsTypeSafe(Environment, BranchStackFrame), Targets),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LookupSwitch{..} => {
			// TODO: sort(keys, keys)
			let targets: Vec<_> = todo!();
			// TODO: what is the default case?
			let branch_stack_frame = stack_frame.can_pop([Int])?;
			for target in targets {
				target_is_type_safe(environment, &branch_stack_frame, target)?;
			}
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(lreturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, long),
		//     canPop(StackFrame, [long], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LReturn => {
			(environment.return_type == Some(Long)).fail("")?;
			let _popped_stack_frame = stack_frame.can_pop([Long])?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(lshl, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, long], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(lshr, lshl).
		// instructionHasEquivalentTypeRule(lushr, lshl).
		LShl | LShr | LUShr => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, Long],
				Some(Long)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LStore(index) => {
			let next_stack_frame = stack_frame.store_is_type_safe(&index, Long)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(monitorenter, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(monitorexit, monitorenter).
		MonitorEnter | MonitorExit => {
			let next_stack_frame = stack_frame.can_pop([Reference])?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(multianewarray(CP, Dim), Environment, _Offset,
		//                       StackFrame, NextStackFrame, ExceptionStackFrame) :-
		//     CP = arrayOf(_),
		//     classDimension(CP, Dimension),
		//     Dimension >= Dim,
		//     Dim > 0,
		//     /* Make a list of Dim ints */
		//     findall(int, between(1, Dim, _), IntList),
		//     validTypeTransition(Environment, IntList, CP,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		MultiANewArray(cp, dim) => {
			(matches!(cp, ArrayOf(_))).fail("")?;
			let dimension = cp.class_dimension();
			(dimension >= dim && dim > 0).fail("")?;
			let int_list = {
				// generate exactly dim items:
				let mut vec = Vec::with_capacity(dim);
				for _ in 0..dim {
					vec.push(Int);
				}
				vec
			};
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				int_list,
				Some(cp)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(new(CP), Environment, Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, OperandStack, Flags),
		//     CP = class(_, _),
		//     NewItem = uninitialized(Offset),
		//     notMember(NewItem, OperandStack),
		//     substitute(NewItem, top, Locals, NewLocals),
		//     validTypeTransition(Environment, [], NewItem,
		//                         frame(NewLocals, OperandStack, Flags),
		//                         NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		New(cp) => {
			(matches!(cp, Class(_, _))).fail("")?;
			let new_item = UninitializedOffset(offset);
			stack_frame.operand_stack.not_member(&new_item)?;
			let new_frame = Frame {
				locals: stack_frame.locals.substitute(&new_item, &Top),
				..stack_frame
			};
			let next_stack_frame = new_frame.valid_type_transition(
				environment,
				[],
				Some(new_item)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(newarray(TypeCode), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     primitiveArrayInfo(TypeCode, _TypeChar, ElementType, _VerifierType),
		//     validTypeTransition(Environment, [int], arrayOf(ElementType),
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		NewArray{..} => {
			let next_stack_frame = todo!();
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// primitiveArrayInfo(4,  0'Z, boolean, int).
		// primitiveArrayInfo(5,  0'C, char,    int).
		// primitiveArrayInfo(6,  0'F, float,   float).
		// primitiveArrayInfo(7,  0'D, double,  double).
		// primitiveArrayInfo(8,  0'B, byte,    int).
		// primitiveArrayInfo(9,  0'S, short,   int).
		// primitiveArrayInfo(10, 0'I, int,     int).
		// primitiveArrayInfo(11, 0'J, long,    long).

		// instructionIsTypeSafe(nop, _Environment, _Offset, StackFrame,
		//                       StackFrame, ExceptionStackFrame) :-
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Nop => {
			Ok((FrameT::Frame(stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(pop, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, [Type | Rest], Flags),
		//     Type \= top,
		//     sizeOf(Type, 1),
		//     NextStackFrame = frame(Locals, Rest, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Pop => {
			let mut operand_stack = stack_frame.operand_stack;
			match operand_stack.pop()? {
				Top => fail(""),
				type_ if type_.size() == TypeSize::OneWord => {
					let next_stack_frame = Frame {
						operand_stack,
						..stack_frame
					};
					Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
				},
				_ => fail(""),
			}
		},

		// instructionIsTypeSafe(pop2, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Pop2 => {
			fn pop2_some_form_is_type_safe(mut stack: OperandStack) -> Result<OperandStack> {
				let type_1 = stack.pop()?;
				let type_2 = stack.pop()?;
				// pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack) :-
				//     pop2Form1IsTypeSafe(InputOperandStack, OutputOperandStack).
				//
				// pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack) :-
				//     pop2Form2IsTypeSafe(InputOperandStack, OutputOperandStack).

				// pop2Form1IsTypeSafe([Type1, Type2 | Rest], Rest) :-
				//     sizeOf(Type1, 1),
				//     sizeOf(Type2, 1).
				let pop2_form1_is_type_safe = type_1.size() == TypeSize::OneWord && type_2.size() == TypeSize::OneWord;

				// pop2Form2IsTypeSafe([top, Type | Rest], Rest) :- sizeOf(Type, 2).
				let pop2_form2_is_type_safe = type_1 == Top && type_2.size() == TypeSize::TwoWord;

				if pop2_form1_is_type_safe || pop2_form2_is_type_safe {
					Ok(stack)
				} else {
					fail("")
				}
			}

			let next_stack_frame = Frame {
				operand_stack: pop2_some_form_is_type_safe(stack_frame.operand_stack)?,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(putfield(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(FieldClass, FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     canPop(StackFrame, [FieldType], PoppedFrame),
		//     passesProtectedCheck(Environment, FieldClass, FieldName,
		//                          FieldDescriptor, PoppedFrame),
		//     currentClassLoader(Environment, CurrentLoader),
		//     canPop(StackFrame, [FieldType, class(FieldClass, CurrentLoader)],
		//            NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		PutField(field) => {
			let field_type = parse_field_descriptor(&field.descriptor)?;
			let popped_frame = stack_frame.clone().can_pop([field_type.clone()])?;
			passes_protected_check(environment, &field.class, &field.name, &field.descriptor, &popped_frame)?;
			let current_loader = environment.current_class_loader()?.clone();
			let next_stack_frame = stack_frame.can_pop([field_type, Class(field.class, current_loader)])?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(putstatic(CP), _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(_FieldClass, _FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     canPop(StackFrame, [FieldType], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		PutStatic(field) => {
			let field_type = parse_field_descriptor(&field.descriptor)?;
			let next_stack_frame = stack_frame.can_pop([field_type])?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(return, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, void),
		//     StackFrame = frame(_Locals, _OperandStack, Flags),
		//     notMember(flagThisUninit, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Return => {
			environment.return_type.is_none().fail("")?;
			if stack_frame.flag_this_uninit {
				fail("")
			} else {
				Ok((FrameT::AfterGoto, exception_stack_frame))
			}
		},

		// instructionIsTypeSafe(saload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(short)], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		SALoad => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[Int, ArrayOf(ArrayType::Short)],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(sastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(short)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		SAStore => {
			let next_stack_frame = stack_frame.can_pop(
				[Int, Int, ArrayOf(ArrayType::Short)],
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(sipush(_Value), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(bipush(Value), sipush(Value)).
		SIPush(..) | BIPush(..) => {
			let next_stack_frame = stack_frame.valid_type_transition(
				environment,
				[],
				Some(Int)
			)?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(swap, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(_Locals, [Type1, Type2 | Rest], _Flags),
		//     sizeOf(Type1, 1),
		//     sizeOf(Type2, 1),
		//     NextStackFrame = frame(_Locals, [Type2, Type1 | Rest], _Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Swap => {
			let mut operand_stack = stack_frame.operand_stack;

			let type_1 = operand_stack.pop()?;
			let type_2 = operand_stack.pop()?;

			size_of(&type_1, 1)?;
			size_of(&type_2, 1)?;

			operand_stack.push(type_1);
			operand_stack.push(type_2);

			let next_stack_frame = Frame {
				operand_stack,
				..stack_frame
			};
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(tableswitch(Targets, Keys), Environment, _Offset,
		//                       StackFrame, afterGoto, ExceptionStackFrame) :-
		//     sort(Keys, Keys),
		//     canPop(StackFrame, [int], BranchStackFrame),
		//     checklist(targetIsTypeSafe(Environment, BranchStackFrame), Targets),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		TableSwitch {..} => {
			// TODO: sort(keys, keys)
			let targets: Vec<_> = todo!();
			// TODO: default case?
			let branch_stack_frame = stack_frame.can_pop([Int])?;
			for target in targets {
				target_is_type_safe(environment, &branch_stack_frame, target)?;
			}
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},
	}
}