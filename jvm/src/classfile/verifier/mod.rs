// this handles chapter 4.10 of the JVMS 8

use std::io::BufRead;
use crate::classfile::instruction::{LvIndex, Opcode};
use crate::classfile::MethodInfoAccess as MethodAccessFlags;

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
struct ClassName;

impl From<&str> for ClassName {
	fn from(value: &str) -> Self {
		todo!()
	}
}

impl PartialEq<str> for ClassName {
	fn eq(&self, other: &str) -> bool {
		todo!()
	}
}

impl PartialEq<&str> for ClassName {
	fn eq(&self, other: &&str) -> bool {
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Loader;
#[derive(Debug, Clone, PartialEq, Eq)]
struct Method;
#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodName;


impl PartialEq<&str> for MethodName {
	fn eq(&self, other: &&str) -> bool {
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MethodDescriptor;


const JAVA_LANG_OBJECT: &str = "java/lang/Object";
const JAVA_LANG_THROWABLE: &str = "java/lang/Throwable";

fn get_bootstrap_loader() -> Result<Loader> {
	todo!()
}

// 4.10.1

// Iff the predicate classIsTypeSafe is not true, the type checker must throw the exception VerifyError to indicate that the class file is malformed.
// Otherwise, the class file has type checked successfully and bytecode verification has completed successfully.

fn class_is_type_safe(class: &Class) -> Bool {
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
	let class_is_type_safe_0 = || -> Bool {
		let name = class.class_name()?;
		let l = class.defining_loader()?;

		let chain = superclass_chain(&name, &l)?;

		if chain.is_empty() {
			fail("")?;
		}

		let superclass_name = class.super_class_name()?;
		let superclass = loaded_class(&superclass_name, &l)?;

		superclass.is_not_final()?;

		let methods = class.methods()?;

		for method in methods {
			method_is_type_safe(class, &method)?;
		}

		Ok(())
	};

	// classIsTypeSafe(Class) :-
	//     classClassName(Class, 'java/lang/Object'),
	//     classDefiningLoader(Class, L),
	//     isBootstrapLoader(L),
	//     classMethods(Class, Methods),
	//     checklist(methodIsTypeSafe(Class), Methods).
	let class_is_type_safe_1 = |_| -> Bool {
		(class.class_name()? == "java/lang/Object").fail("")?;

		let l = class.defining_loader()?;

		l.is_bootstrap_loader()?;

		let methods = class.methods()?;

		for method in methods {
			method_is_type_safe(class, &method)?;
		}

		Ok(())
	};

	class_is_type_safe_0()
		.or_else(class_is_type_safe_1)
}

// We stipulate the existence of 28 Prolog predicates ("accessors") that have certain expected behavior but whose formal definitions are not given in this
// specification.
impl Class {
	// classClassName(Class, ClassName)
	//     Extracts the name, ClassName, of the class Class.
	fn class_name(&self) -> Result<ClassName> {
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
	fn attributes<T>(&self) -> Result<Vec<T>> {
		unreachable!("Never called from anywhere")
	}

	// classDefiningLoader(Class, Loader)
	//     Extracts the defining class loader, Loader, of the class Class.
	fn defining_loader(&self) -> Result<Loader> {
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
fn loaded_class(name: &ClassName, initiating_loader: &Loader) -> Result<Class> {
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
	fn is_init(&self) -> Bool {
		todo!()
	}

	// isNotInit(Method)
	//     True iff Method (regardless of class) is not <init>.
	fn is_not_init(&self) -> Bool {
		todo!()
	}

	// isNotFinal(Method, Class)
	//     True iff Method in class Class is not final.
	fn is_not_final(&self, class: &Class) -> Bool {
		todo!()
	}

	// defined in order to make it compile
	fn is_final(&self, class: &Class) -> Bool {
		todo!()
	}

	// isStatic(Method, Class)
	//     True iff Method in class Class is static.
	fn is_static(&self, class: &Class) -> Bool {
		todo!()
	}

	// isNotStatic(Method, Class)
	//     True iff Method in class Class is not static.
	fn is_not_static(&self, class: &Class) -> Bool {
		todo!()
	}

	// isPrivate(Method, Class)
	//     True iff Method in class Class is private.
	fn is_private(&self, class: &Class) -> Bool {
		todo!()
	}

	// isNotPrivate(Method, Class)
	//     True iff Method in class Class is not private.
	fn is_not_private(&self, class: &Class) -> Bool {
		todo!()
	}
}

// isProtected(MemberClass, MemberName, MemberDescriptor)
//     True iff there is a member named MemberName with descriptor MemberDescriptor in the class MemberClass and it is protected.

// isNotProtected(MemberClass, MemberName, MemberDescriptor)
//     True iff there is a member named MemberName with descriptor MemberDescriptor in the class MemberClass and it is not protected.

// parseFieldDescriptor(Descriptor, Type)
//     Converts a field descriptor, Descriptor, into the corresponding verification type Type (§4.10.1.2).
fn parse_field_descriptor<Descriptor>(descriptor: &Descriptor) -> Result<VerificationType> {
	todo!()
}

// parseMethodDescriptor(Descriptor, ArgTypeList, ReturnType)
//     Converts a method descriptor, Descriptor, into a list of verification types, ArgTypeList, corresponding to the method argument types, and a verification
//     type, ReturnType, corresponding to the return type.
fn parse_method_descriptor(descriptor: &MethodDescriptor) -> Result<(Vec<VerificationType>, VerificationType)> {
	todo!()
}

// parseCodeAttribute(Class, Method, FrameSize, MaxStack, ParsedCode, Handlers, StackMap)
//     Extracts the instruction stream, ParsedCode, of the method Method in Class, as well as the maximum operand stack size, MaxStack, the maximal number of
//     local variables, FrameSize, the exception handlers, Handlers, and the stack map StackMap.
//
//     The representation of the instruction stream and stack map attribute must be as specified in §4.10.1.3 and §4.10.1.4.
fn parse_code_attribute(class: &Class, method: &Method) -> Result<(usize, usize, Vec<Instruction>, Vec<Handler>, Vec<Instruction>)> {

	let frame_size = todo!();
	let max_stack = todo!();
	let parsed_code = todo!();
	let handlers = todo!();
	let stack_map = todo!();

	Ok((frame_size, max_stack, parsed_code, handlers, stack_map))
}

// samePackageName(Class1, Class2)
//     True iff the package names of Class1 and Class2 are the same.
fn same_package_name(class1: &Class, class2: &Class) -> Bool {
	todo!()
}

// differentPackageName(Class1, Class2)
//     True iff the package names of Class1 and Class2 are different.
fn different_package_name(class1: &Class, class2: &Class) -> Bool {
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
#[derive(Debug, Clone, PartialEq, Eq)]
struct Environment {
	class: Class,
	method: Method,
	return_type: VerificationType,
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
	fn exception_handlers(&self) -> Result<&Vec<Handler>> {
		Ok(&self.handlers)
	}

	// maxOperandStackLength(Environment, MaxStack) :-
	//     Environment = environment(_Class, _Method, _ReturnType,
	//                               _Instructions, MaxStack, _Handlers).
	fn max_operand_stack_length(&self) -> Result<usize> {
		Ok(self.max_stack)
	}

	// thisClass(Environment, class(ClassName, L)) :-
	//     Environment = environment(Class, _Method, _ReturnType,
	//                               _Instructions, _, _),
	//     classDefiningLoader(Class, L),
	//     classClassName(Class, ClassName).
	fn this_class(&self) -> Result<(ClassName, Loader)> {
		let class = &self.class;
		let l = class.defining_loader()?;
		let class_name = class.class_name()?;
		Ok((class_name, l))
	}

	// thisMethodReturnType(Environment, ReturnType) :-
	//     Environment = environment(_Class, _Method, ReturnType,
	//                               _Instructions, _, _).
	fn this_method_return_type(&self) -> Result<&VerificationType> {
		Ok(&self.return_type)
	}

	// We specify additional predicates to extract higher-level information from the environment.

	// offsetStackFrame(Environment, Offset, StackFrame) :-
	//     allInstructions(Environment, Instructions),
	//     member(stackMap(Offset, StackFrame), Instructions).
	fn offset_stack_frame(&self, offset: usize) -> Result<&Frame> {
		let instructions = self.all_instructions()?;

		let stack_map = instructions.iter()
			.find_map(|instruction| match instruction {
				Instruction::StackMap(StackMap { type_state, .. }) => Some(type_state),
				_ => None,
			});

		match stack_map {
			Some(stack_map) => Ok(stack_map),
			None => fail(""),
		}
	}

	// currentClassLoader(Environment, Loader) :-
	//     thisClass(Environment, class(_, Loader)).
	fn current_class_loader(&self) -> Result<Loader> {
		Ok(self.this_class()?.1)
	}
}

// Finally, we specify a general predicate used throughout the type rules:

// notMember(_, []).
// notMember(X, [A | More]) :- X \= A, notMember(X, More).
fn not_member<T: PartialEq>(x: &T, vec: &Vec<T>) -> Bool {
	if vec.contains(x) {
		Err(())
	} else {
		Ok(())
	}
}

fn member<T: PartialEq>(x: &T, vec: &Vec<T>) -> Bool {
	if vec.contains(x) {
		Ok(())
	} else {
		Err(())
	}
}

// 4.10.1.2
// class(_, _)
#[derive(Debug, Clone, PartialEq, Eq)]
struct ClassNameAndLoader {
	name: ClassName,
	loader: Loader,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ArrayType {
	Byte,
	Boolean,
	Char,
	Short,
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
enum VerificationType {
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
	fn is_atom(&self) -> Bool {
		use VerificationType::*;
		match self {
			Top | OneWord | Int | Float | Reference | Uninitialized | UninitializedThis | Null | TwoWord | Long | Double
				=> true,
			UninitializedOffset(..) | Class(..) | ArrayOf(..)
				=> false,
		}.fail("Verification type is not an atom")
	}
	fn is_compound(&self) -> Bool {
		use VerificationType::*;
		match self {
			Top | OneWord | Int | Float | Reference | Uninitialized | UninitializedThis | Null | TwoWord | Long | Double
			=> false,
			UninitializedOffset(..) | Class(..) | ArrayOf(..)
			=> true,
		}.fail("Verification type is not an compound")
	}

	fn is_assignable(from: &Self, to: &Self) -> Bool {
		use VerificationType::*;

		// isAssignable(X, X).
		let is_assignable_0 = || (from == to).fail("");

		// isAssignable(oneWord, top).
		let is_assignable_1 = |_| (matches!(from, OneWord) && matches!(to, Self::Top)).fail("");

		// isAssignable(twoWord, top).
		let is_assignable_2 = |_| (matches!(from, TwoWord) && matches!(to, Self::Top)).fail("");

		// isAssignable(int, X)    :- isAssignable(oneWord, X).
		let is_assignable_3 = |_| {
			matches!(from, Int).fail("")?;
			Self::is_assignable(&OneWord, to)
		};

		// isAssignable(float, X)  :- isAssignable(oneWord, X).
		let is_assignable_4 = |_| {
			matches!(from, Float).fail("")?;
			Self::is_assignable(&OneWord, to)
		};

		// isAssignable(long, X)   :- isAssignable(twoWord, X).
		let is_assignable_5 = |_| {
			matches!(from, Long).fail("")?;
			Self::is_assignable(&TwoWord, to)
		};

		// isAssignable(double, X) :- isAssignable(twoWord, X).
		let is_assignable_6 = |_| {
			matches!(from, Double).fail("")?;
			Self::is_assignable(&TwoWord, to)
		};

		// isAssignable(reference, X)   :- isAssignable(oneWord, X).
		let is_assignable_7 = |_| {
			matches!(from, Reference).fail("")?;
			Self::is_assignable(&OneWord, to)
		};

		// isAssignable(class(_, _), X) :- isAssignable(reference, X).
		let is_assignable_8 = |_| {
			matches!(from, Class(_, _)).fail("")?;
			Self::is_assignable(&Reference, to)
		};

		// isAssignable(arrayOf(_), X)  :- isAssignable(reference, X).
		let is_assignable_9 = |_| {
			matches!(from, ArrayOf(_)).fail("")?;
			Self::is_assignable(&Reference, to)
		};

		// isAssignable(uninitialized, X)     :- isAssignable(reference, X).
		let is_assignable_10 = |_| {
			matches!(from, Uninitialized).fail("")?;
			Self::is_assignable(&Reference, to)
		};

		// isAssignable(uninitializedThis, X) :- isAssignable(uninitialized, X).
		let is_assignable_11 = |_| {
			matches!(from, UninitializedThis).fail("")?;
			Self::is_assignable(&Uninitialized, to)
		};

		// isAssignable(uninitialized(_), X)  :- isAssignable(uninitialized, X).
		let is_assignable_12 = |_| {
			matches!(from, UninitializedOffset(_)).fail("")?;
			Self::is_assignable(&Uninitialized, to)
		};

		// isAssignable(null, class(_, _)).
		let is_assignable_13 = |_| (matches!(from, Null) && matches!(to, Class(_, _))).fail("");

		// isAssignable(null, arrayOf(_)).
		let is_assignable_14 = |_| (matches!(from, Null) && matches!(to, ArrayOf(_))).fail("");

		// isAssignable(null, X) :- isAssignable(class('java/lang/Object', BL), X),
		//                          isBootstrapLoader(BL).
		let is_assignable_15 = |_| {
			matches!(from, Null).fail("")?;
			let bl = get_bootstrap_loader()?;
			bl.is_bootstrap_loader()?;
			Self::is_assignable(&Class(JAVA_LANG_OBJECT.into(), bl), to)
		};

		// isAssignable(class(X, Lx), class(Y, Ly)) :-
		//     isJavaAssignable(class(X, Lx), class(Y, Ly)).
		let is_assignable_16 = |_| {
			(matches!(from, Class(_, _)) && matches!(to, Class(_, _))).fail("")?;
			Self::is_java_assignable(from, to)
		};

		// isAssignable(arrayOf(X), class(Y, L)) :-
		//     isJavaAssignable(arrayOf(X), class(Y, L)).
		let is_assignable_17 = |_| {
			(matches!(from, ArrayOf(_)) && matches!(to, Class(_, _))).fail("")?;
			Self::is_java_assignable(from, to)
		};

		// isAssignable(arrayOf(X), arrayOf(Y)) :-
		//     isJavaAssignable(arrayOf(X), arrayOf(Y)).
		let is_assignable_18 = |_| {
			(matches!(from, ArrayOf(_)) && matches!(to, ArrayOf(_))).fail("")?;
			Self::is_java_assignable(from, to)
		};

		is_assignable_0()
			.or_else(is_assignable_1)
			.or_else(is_assignable_2)
			.or_else(is_assignable_3)
			.or_else(is_assignable_4)
			.or_else(is_assignable_5)
			.or_else(is_assignable_6)
			.or_else(is_assignable_7)
			.or_else(is_assignable_8)
			.or_else(is_assignable_9)
			.or_else(is_assignable_10)
			.or_else(is_assignable_11)
			.or_else(is_assignable_12)
			.or_else(is_assignable_13)
			.or_else(is_assignable_14)
			.or_else(is_assignable_15)
			.or_else(is_assignable_16)
			.or_else(is_assignable_17)
			.or_else(is_assignable_18)
	}

	fn is_array_interface(verification_type: &VerificationType) -> Bool {
		use VerificationType::*;

		// isArrayInterface(class('java/lang/Cloneable', BL)) :-
		//     isBootstrapLoader(BL).
		let is_array_interface_0 = || {
			if let Class(name, bl) = verification_type {
				(name == "java/lang/Cloneable").fail("")?;
				bl.is_bootstrap_loader()
			} else {
				fail("")
			}
		};

		// isArrayInterface(class('java/io/Serializable', BL)) :-
		//     isBootstrapLoader(BL).
		let is_array_interface_1 = |_| {
			if let Class(name, bl) = verification_type {
				(name == "java/lang/Serializable").fail("")?;
				bl.is_bootstrap_loader()
			} else {
				fail("")
			}
		};

		is_array_interface_0()
			.or_else(is_array_interface_1)
	}

	fn is_java_assignable(from: &VerificationType, to: &VerificationType) -> Bool {
		use VerificationType::*;

		// isJavaAssignable(class(_, _), class(To, L)) :-
		//     loadedClass(To, L, ToClass),
		//     classIsInterface(ToClass).
		let is_java_assignable_0 = || {
			matches!(from, Class(_, _)).fail("")?;
			if let Class(to, l) = to {
				let to_class = loaded_class(to, l)?;
				to_class.is_interface()
			} else {
				fail("")
			}
		};

		// isJavaAssignable(From, To) :-
		//     isJavaSubclassOf(From, To).
		let is_java_assignable_1 = |_| Self::is_java_subclass_of(from, to);

		// isJavaAssignable(arrayOf(_), class('java/lang/Object', BL)) :-
		//     isBootstrapLoader(BL).
		let is_java_assignable_2 = |_| {
			matches!(from, ArrayOf(_)).fail("")?;
			if let Class(name, bl) = to {
				(name == JAVA_LANG_OBJECT).fail("")?;
				bl.is_bootstrap_loader()
			} else {
				fail("")
			}
		};

		// isJavaAssignable(arrayOf(_), X) :-
		//     isArrayInterface(X).
		let is_java_assignable_3 = |_| {
			matches!(from, ArrayOf(_)).fail("")?;
			Self::is_array_interface(to)
		};

		// isJavaAssignable(arrayOf(X), arrayOf(Y)) :-
		//     atom(X),
		//     atom(Y),
		//     X = Y.
		let is_java_assignable_4 = |_| {
			match (from, to) {
				(ArrayOf(ArrayType::Other(x)), ArrayOf(ArrayType::Other(y))) => {
					x.is_atom()?;
					y.is_atom()?;
					(x == y).fail("")
				},
				(ArrayOf(x), ArrayOf(y)) => {
					// x != Other, y != Other
					// atom(x) and atom(y) not needed, only atoms exist
					(x == y).fail("")
				},
				_ => fail(""),
			}
		};

		// isJavaAssignable(arrayOf(X), arrayOf(Y)) :-
		//     compound(X), compound(Y), isJavaAssignable(X, Y).
		let is_java_assignable_5 = |_| {
			match (from, to) {
				(ArrayOf(ArrayType::Other(x)), ArrayOf(ArrayType::Other(y))) => {
					x.is_compound()?;
					y.is_compound()?;
					Self::is_java_assignable(x, y)
				},
				_ => fail(""),
			}
		};

		is_java_assignable_0()
			.or_else(is_java_assignable_1)
			.or_else(is_java_assignable_2)
			.or_else(is_java_assignable_3)
			.or_else(is_java_assignable_4)
			.or_else(is_java_assignable_5)
	}

	fn is_java_subclass_of(from: &VerificationType, to: &VerificationType) -> Bool {
		use VerificationType::*;

		// isJavaSubclassOf(class(SubclassName, L), class(SubclassName, L)).
		let is_java_subclass_of_0 = || {
			match (from, to) {
				(Class(x, xl), Class(y, yl)) => {
					(x == y).fail("")?;
					(xl == yl).fail("")
				}
				_ => fail(""),
			}
		};

		// isJavaSubclassOf(class(SubclassName, LSub), class(SuperclassName, LSuper)) :-
		//     superclassChain(SubclassName, LSub, Chain),
		//     member(class(SuperclassName, L), Chain),
		//     loadedClass(SuperclassName, L, Sup),
		//     loadedClass(SuperclassName, LSuper, Sup).
		let is_java_subclass_of_1 = |_| {
			match (from, to) {
				(Class(subclass_name, l_sub), Class(superclass_name, l_super)) => {
					let chain = superclass_chain(subclass_name, l_sub)?;

					let l = 'a: {
						for i in chain {
							if &i.name == superclass_name {
								break 'a i.loader;
							}
						}
						return fail("");
					};

					let sup_from_l_sub = loaded_class(superclass_name, &l)?;
					let sup_from_l_super = loaded_class(superclass_name, l_super)?;

					(sup_from_l_sub == sup_from_l_super).fail("")
				},
				_ => fail(""),
			}
		};

		is_java_subclass_of_0()
			.or_else(is_java_subclass_of_1)
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

fn superclass_chain(class_name: &ClassName, l: &Loader) -> Result<Vec<ClassNameAndLoader>> {
	// superclassChain(ClassName, L, [class(SuperclassName, Ls) | Rest]) :-
	//     loadedClass(ClassName, L, Class),
	//     classSuperClassName(Class, SuperclassName),
	//     classDefiningLoader(Class, Ls),
	//     superclassChain(SuperclassName, Ls, Rest).
	let superclass_chain_0 = || -> Result<Vec<ClassNameAndLoader>> {
		let class = loaded_class(class_name, l)?;
		let superclass_name = class.super_class_name()?;
		let ls = class.defining_loader()?;

		let mut rest = superclass_chain(&superclass_name, &ls)?;

		rest.insert(0, ClassNameAndLoader {
			name: superclass_name,
			loader: ls,
		});

		Ok(rest)
	};

	// superclassChain('java/lang/Object', L, []) :-
	//     loadedClass('java/lang/Object', L, Class),
	//     classDefiningLoader(Class, BL),
	//     isBootstrapLoader(BL).
	let superclass_chain_1 = |_| {
		(class_name == JAVA_LANG_OBJECT).fail("")?;
		let class = loaded_class(&JAVA_LANG_OBJECT.into(), l)?;
		let bl = class.defining_loader()?;
		bl.is_bootstrap_loader()?;
		Ok(Vec::new())
	};

	superclass_chain_0()
		.or_else(superclass_chain_1)
}

// 4.10.1.3

// 4.10.1.4

#[derive(Debug, Clone, PartialEq, Eq)]
struct StackMap {
	offset: usize,
	type_state: Frame,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Frame {
	locals: Vec<VerificationType>,
	operand_stack: Vec<VerificationType>,
	/// `flags: Vec<Flag>,`
	/// is a list which may either be empty or have the single element `flagThisUninit`. Represented here via one boolean field.
	/// `true` iff `flagThisUninit` would be there.
	flag_this_uninit: bool,
}

impl Frame {
	fn is_assignable(from: &Frame, to: &Frame) -> Bool {
		// subset, but using our changed definition of flags
		fn subset(sub_set: bool, set: bool) -> Bool {
			// True if all elements of SubSet belong to Set as well.
			match (sub_set, set) {
				(false, false) => true,
				(false, true) => true,
				(true, false) => false,
				(true, true) => true,
			}.fail("")
		}

		// frameIsAssignable(frame(Locals1, StackMap1, Flags1),
		//                   frame(Locals2, StackMap2, Flags2)) :-
		//     length(StackMap1, StackMapLength),
		//     length(StackMap2, StackMapLength),
		//     maplist(isAssignable, Locals1, Locals2),
		//     maplist(isAssignable, StackMap1, StackMap2),
		//     subset(Flags1, Flags2).
		(from.operand_stack.len() == to.operand_stack.len()).fail("")?;

		// maplist requires same length
		(from.locals.len() == to.locals.len()).fail("")?;

		for i in 0..from.locals.len() { // range checked above
			VerificationType::is_assignable(&from.locals[i], &to.locals[i])?;
		}

		for i in 0..from.operand_stack.len() { // range checked above
			VerificationType::is_assignable(&from.operand_stack[i], &to.operand_stack[i])?;
		}

		subset(from.flag_this_uninit, to.flag_this_uninit)
	}
}

// operandStackHasLegalLength(Environment, OperandStack) :-
//     length(OperandStack, Length),
//     maxOperandStackLength(Environment, MaxStack),
//     Length =< MaxStack.
fn operand_stack_has_legal_length(environment: &Environment, operand_stack: &Vec<VerificationType>) -> Bool {
	let length = operand_stack.len();
	let max_stack = environment.max_operand_stack_length()?;
	(length <= max_stack).fail("")
}

// nth1OperandStackIs(i, frame(_Locals, OperandStack, _Flags), Element) :-
//     nth1(i, OperandStack, Element).
fn nth1_operand_stack_is(_i: usize, _frame: &Frame) -> Result<VerificationType> {
	todo!()
}

// canPop(frame(Locals, OperandStack, Flags), Types,
//        frame(Locals, PoppedOperandStack, Flags)) :-
//     popMatchingList(OperandStack, Types, PoppedOperandStack).
fn can_pop(frame: &Frame, types: &Vec<VerificationType>) -> Result<Frame> {
	let operand_stack = &frame.operand_stack;

	let popped_operand_stack = pop_matching_list(operand_stack, types)?;

	Ok(Frame {
		locals: frame.locals.clone(),
		operand_stack: popped_operand_stack,
		flag_this_uninit: frame.flag_this_uninit,
	})
}

fn head_rest<T: Clone>(vec: &Vec<T>) -> (Option<&T>, Vec<T>) {
	if let Some((a, b)) = vec.split_first() {
		(Some(a), b.to_vec())
	} else {
		(None, Vec::new())
	}
}

// popMatchingList(OperandStack, [], OperandStack).
// popMatchingList(OperandStack, [P | Rest], NewOperandStack) :-
//     popMatchingType(OperandStack, P, TempOperandStack, _ActualType),
//     popMatchingList(TempOperandStack, Rest, NewOperandStack).
fn pop_matching_list(operand_stack: &Vec<VerificationType>, types: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	if types.is_empty() {
		return Ok(operand_stack.clone());
	}

	let (p, rest) = head_rest(types);
	let p = p.unwrap(); // cannot panic see check above

	let (temp_operand_stack, _actual_type) = pop_matching_type(&operand_stack, p)?;
	let new_operand_stack = pop_matching_list(&temp_operand_stack, &rest)?;

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
fn pop_matching_type(operand_stack: &Vec<VerificationType>, type_: &VerificationType) -> Result<(Vec<VerificationType>, VerificationType)> {
	todo!()
}

// sizeOf(X, 2) :- isAssignable(X, twoWord).
// sizeOf(X, 1) :- isAssignable(X, oneWord).
// sizeOf(top, 1).
fn size_of(x: &VerificationType, size: usize) -> Bool {
	match size {
		2 => VerificationType::is_assignable(x, &VerificationType::TwoWord),
		1 if x == &VerificationType::Top => Ok(()),
		1 => VerificationType::is_assignable(x, &VerificationType::OneWord),
		_ => fail(""),
	}
}

// pushOperandStack(OperandStack, 'void', OperandStack).
// pushOperandStack(OperandStack, Type, [Type | OperandStack]) :-
//     sizeOf(Type, 1).
// pushOperandStack(OperandStack, Type, [top, Type | OperandStack]) :-
//     sizeOf(Type, 2).
fn push_operand_stack(_operand_stack: &Vec<VerificationType>, _type_: &VerificationType) -> Result<Vec<VerificationType>> {
	todo!()
}

// canSafelyPush(Environment, InputOperandStack, Type, OutputOperandStack) :-
//     pushOperandStack(InputOperandStack, Type, OutputOperandStack),
//     operandStackHasLegalLength(Environment, OutputOperandStack).
fn can_safely_push(environment: &Environment, input_operand_stack: &Vec<VerificationType>, type_: &VerificationType) -> Result<Vec<VerificationType>> {
	let output_operand_stack = push_operand_stack(input_operand_stack, type_)?;
	operand_stack_has_legal_length(environment, &output_operand_stack)?;
	Ok(output_operand_stack)
}

// canSafelyPushList(Environment, InputOperandStack, Types,
//                   OutputOperandStack) :-
//     canPushList(InputOperandStack, Types, OutputOperandStack),
//     operandStackHasLegalLength(Environment, OutputOperandStack).
fn can_safely_push_list(environment: &Environment, input_operand_stack: &Vec<VerificationType>, types: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	let output_operand_stack = can_push_list(input_operand_stack, types)?;
	operand_stack_has_legal_length(environment, &output_operand_stack)?;
	Ok(output_operand_stack)
}

// canPushList(InputOperandStack, [], InputOperandStack).
// canPushList(InputOperandStack, [Type | Rest], OutputOperandStack) :-
//     pushOperandStack(InputOperandStack, Type, InterimOperandStack),
//     canPushList(InterimOperandStack, Rest, OutputOperandStack).
fn can_push_list(input_operand_stack: &Vec<VerificationType>, types: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	if types.is_empty() {
		return Ok(input_operand_stack.clone())
	}

	let (first, rest) = head_rest(types);
	let first = first.unwrap(); // cannot panic see check above

	let interim_operand_stack = push_operand_stack(input_operand_stack, first)?;
	can_push_list(&interim_operand_stack, &rest)
}

// popCategory1([Type | Rest], Type, Rest) :-
//     Type \= top,
//     sizeOf(Type, 1).
fn pop_category_1() {
	todo!()
}

// popCategory2([top, Type | Rest], Type, Rest) :-
//     sizeOf(Type, 2).
fn pop_category_2() {
	todo!()
}

// validTypeTransition(Environment, ExpectedTypesOnStack, ResultType,
//                     frame(Locals, InputOperandStack, Flags),
//                     frame(Locals, NextOperandStack, Flags)) :-
//     popMatchingList(InputOperandStack, ExpectedTypesOnStack,
//                     InterimOperandStack),
//     pushOperandStack(InterimOperandStack, ResultType, NextOperandStack),
//     operandStackHasLegalLength(Environment, NextOperandStack).
fn valid_type_transition(environment: &Environment, expected_types_on_stack: &Vec<VerificationType>, result_type: &VerificationType, frame: &Frame) -> Result<Frame> {
	let interim_operand_stack = pop_matching_list(&frame.operand_stack, expected_types_on_stack)?;
	let next_operand_stack = push_operand_stack(&interim_operand_stack, result_type)?;
	operand_stack_has_legal_length(environment, &next_operand_stack)?;
	Ok(Frame {
		locals: frame.locals.clone(),
		operand_stack: next_operand_stack,
		flag_this_uninit: frame.flag_this_uninit,
	})
}

// 4.10.1.5

fn method_is_type_safe(class: &Class, method: &Method) -> Bool {
	// methodIsTypeSafe(Class, Method) :-
	//     doesNotOverrideFinalMethod(Class, Method),
	//     methodAccessFlags(Method, AccessFlags),
	//     member(abstract, AccessFlags).
	let method_is_type_safe_0 = || {
		does_not_override_final_method(class, method)?;
		let access_flags = method.access_flags()?;
		access_flags.is_abstract.fail("")
	};

	// methodIsTypeSafe(Class, Method) :-
	//     doesNotOverrideFinalMethod(Class, Method),
	//     methodAccessFlags(Method, AccessFlags),
	//     member(native, AccessFlags).
	let method_is_type_safe_1 = |_| {
		does_not_override_final_method(class, method)?;
		let access_flags = method.access_flags()?;
		access_flags.is_native.fail("")
	};

	// methodIsTypeSafe(Class, Method) :-
	//     doesNotOverrideFinalMethod(Class, Method),
	//     methodAccessFlags(Method, AccessFlags),
	//     methodAttributes(Method, Attributes),
	//     notMember(native, AccessFlags),
	//     notMember(abstract, AccessFlags),
	//     member(attribute('Code', _), Attributes),
	//     methodWithCodeIsTypeSafe(Class, Method).
	let method_is_type_safe_2 = |_| {
		does_not_override_final_method(class, method)?;
		let access_flags = method.access_flags()?;
		(!access_flags.is_native).fail("")?;
		(!access_flags.is_abstract).fail("")?;
		method.attributes_has_code()?;
		method_with_code_is_type_safe(class, method)
	};

	method_is_type_safe_0()
		.or_else(method_is_type_safe_1)
		.or_else(method_is_type_safe_2)
}

fn does_not_override_final_method(class: &Class, method: &Method) -> Bool {
	// doesNotOverrideFinalMethod(class('java/lang/Object', L), Method) :-
	//     isBootstrapLoader(L).
	let does_not_override_final_method_0 = || -> Bool {
		todo!()
	};

	// doesNotOverrideFinalMethod(Class, Method) :-
	//     isPrivate(Method, Class).
	let does_not_override_final_method_1 = |_| {
		method.is_private(class)
	};

	// doesNotOverrideFinalMethod(Class, Method) :-
	//     isStatic(Method, Class).
	let does_not_override_final_method_2 = |_| {
		method.is_static(class)
	};

	// doesNotOverrideFinalMethod(Class, Method) :-
	//     isNotPrivate(Method, Class),
	//     isNotStatic(Method, Class),
	//     doesNotOverrideFinalMethodOfSuperclass(Class, Method).
	let does_not_override_final_method_3 = |_| {
		method.is_not_private(class)?;
		method.is_not_static(class)?;
		does_not_override_final_method_of_superclass(class, method)
	};

	does_not_override_final_method_0()
		.or_else(does_not_override_final_method_1)
		.or_else(does_not_override_final_method_2)
		.or_else(does_not_override_final_method_3)
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
	fn member(name: &MethodName, descriptor: &MethodDescriptor, vec: &Vec<Method>) -> Bool {
		for method in vec {
			if &method.name()? == name && &method.descriptor()? == descriptor {
				return Ok(());
			}
		}
		fail("")
	}
	fn not_member(name: &MethodName, descriptor: &MethodDescriptor, vec: &Vec<Method>) -> Bool {
		for method in vec {
			if &method.name()? == name && &method.descriptor()? == descriptor {
				return fail("");
			}
		}
		Ok(())
	}

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     member(method(_, Name, Descriptor), SuperMethodList),
	//     isFinal(Method, Superclass),
	//     isPrivate(Method, Superclass).
	let final_method_not_overridden_0 = || {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		member(&name, &descriptor, super_method_list)?;
		method.is_final(superclass)?;
		method.is_private(superclass)
	};

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     member(method(_, Name, Descriptor), SuperMethodList),
	//     isFinal(Method, Superclass),
	//     isStatic(Method, Superclass).
	let final_method_not_overridden_1 = |_| {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		member(&name, &descriptor, super_method_list)?;
		method.is_final(superclass)?;
		method.is_static(superclass)
	};

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     member(method(_, Name, Descriptor), SuperMethodList),
	//     isNotFinal(Method, Superclass),
	//     isPrivate(Method, Superclass),
	//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
	let final_method_not_overridden_2 = |_| {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		member(&name, &descriptor, super_method_list)?;
		method.is_not_final(superclass)?;
		method.is_private(superclass)?;
		does_not_override_final_method_of_superclass(superclass, method)
	};

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     member(method(_, Name, Descriptor), SuperMethodList),
	//     isNotFinal(Method, Superclass),
	//     isStatic(Method, Superclass),
	//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
	let final_method_not_overridden_3 = |_| {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		member(&name, &descriptor, super_method_list)?;
		method.is_not_final(superclass)?;
		method.is_static(superclass)?;
		does_not_override_final_method_of_superclass(superclass, method)
	};

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     member(method(_, Name, Descriptor), SuperMethodList),
	//     isNotFinal(Method, Superclass),
	//     isNotStatic(Method, Superclass),
	//     isNotPrivate(Method, Superclass).
	let final_method_not_overridden_4 = |_| {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		member(&name, &descriptor, super_method_list)?;
		method.is_not_final(superclass)?;
		method.is_not_static(superclass)?;
		method.is_not_private(superclass)
	};

	// finalMethodNotOverridden(Method, Superclass, SuperMethodList) :-
	//     methodName(Method, Name),
	//     methodDescriptor(Method, Descriptor),
	//     notMember(method(_, Name, Descriptor), SuperMethodList),
	//     doesNotOverrideFinalMethodOfSuperclass(Superclass, Method).
	let final_method_not_overridden_5 = |_| {
		let name = method.name()?;
		let descriptor = method.descriptor()?;
		not_member(&name, &descriptor, super_method_list)?;
		does_not_override_final_method_of_superclass(superclass, method)
	};

	final_method_not_overridden_0()
		.or_else(final_method_not_overridden_1)
		.or_else(final_method_not_overridden_2)
		.or_else(final_method_not_overridden_3)
		.or_else(final_method_not_overridden_4)
		.or_else(final_method_not_overridden_5)
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
	let (frame_size, max_stack, parsed_code, handlers, stack_map) = parse_code_attribute(class, method)?;
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
	merged_code_is_type_safe(&environment, &merged_code, &FrameT::Frame(stack_frame))
}

// handlersAreLegal(Environment) :-
//     exceptionHandlers(Environment, Handlers),
//     checklist(handlerIsLegal(Environment), Handlers).
fn handlers_are_legal(environment: &Environment) -> Bool {
	let handlers = environment.exception_handlers()?;

	for handler in handlers {
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
	let start = handler.start;
	let end = handler.end;
	let target = handler.target;
	(start < end).fail("")?;
	let instructions = environment.all_instructions()?;
	{
		fn member(start: usize, instructions: &Vec<Instruction>) -> Bool {
			for instruction in instructions {
				if let Instruction::Instruction(offset, _) = instruction {
					if *offset == start {
						return Ok(())
					}
				}
			}
			fail("")
		}

		//member(instruction(start, _), instructions)
		member(start, instructions)?;
	}
	let _ = environment.offset_stack_frame(target)?;
	instructions_include_end(instructions, end)?;
	let current_loader = environment.current_class_loader()?;
	let exception_class = handler_exception_class(handler, &current_loader)?;
	let bl = get_bootstrap_loader()?;
	bl.is_bootstrap_loader()?;
	VerificationType::is_assignable(&exception_class, &VerificationType::Class(JAVA_LANG_THROWABLE.into(), bl))?;
	init_handler_is_legal(environment, handler)
}

fn instructions_include_end(instructions: &Vec<Instruction>, end: usize) -> Bool {
	// instructionsIncludeEnd(Instructions, End) :-
	//     member(instruction(End, _), Instructions).
	let instructions_include_end_0 = || {
		fn member(start: usize, instructions: &Vec<Instruction>) -> Bool {
			for instruction in instructions {
				if let Instruction::Instruction(offset, _) = instruction {
					if *offset == start {
						return Ok(())
					}
				}
			}
			fail("")
		}
		member(end, instructions)
	};

	// instructionsIncludeEnd(Instructions, End) :-
	//     member(endOfCode(End), Instructions).
	let instructions_include_end_1 = |_| {
		fn member(start: usize, instructions: &Vec<Instruction>) -> Bool {
			for instruction in instructions {
				if let Instruction::EndOfCode(offset) = instruction {
					if *offset == start {
						return Ok(())
					}
				}
			}
			fail("")
		}
		member(end, instructions)
	};

	instructions_include_end_0().or_else(instructions_include_end_1)
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
		let bl = get_bootstrap_loader()?;
		bl.is_bootstrap_loader()?;
		Ok(VerificationType::Class("java/lang/Throwable".into(), bl))
	}
}

fn not_init_handler(environment: &Environment, handler: &Handler) -> Bool {
	// notInitHandler(Environment, Handler) :-
	//     Environment = environment(_Class, Method, _, Instructions, _, _),
	//     isNotInit(Method).
	let not_init_handler_0 = || {
		let method = &environment.method;
		method.is_not_init()
	};

	// notInitHandler(Environment, Handler) :-
	//     Environment = environment(_Class, Method, _, Instructions, _, _),
	//     isInit(Method),
	//     member(instruction(_, invokespecial(CP)), Instructions),
	//     CP = method(MethodClassName, MethodName, Descriptor),
	//     MethodName \= '<init>'.
	let not_init_handler_1 = |_| {
		let method = &environment.method;
		let instructions = &environment.method;
		method.is_init()?;
		//member(instruction(_, invokespecial(CP)), instructions)?;
		//CP = method(method_class_name, method_name, descriptor);
		//(method_name != "<init>").fail("")
		todo!()
	};

	not_init_handler_0().or_else(not_init_handler_1)
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

		let instructions = environment.all_instructions()?;
		let target = handler.target;

		let handler_instructions: Vec<Instruction> = instructions.iter()
			.filter(|instruction| is_applicable_instruction(target, instruction).is_ok())
			.cloned()
			.collect();
		no_attempt_to_return_normally(&handler_instructions)
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
	method.is_init()?;
	//member(instruction(_, invoekspecial(CP)), instructions)?;
	//CP = method(method_class_name, "<init>", descriptor);
	todo!()
}

// isApplicableInstruction(HandlerStart, instruction(Offset, _)) :-
//     Offset >= HandlerStart.
fn is_applicable_instruction(handler_start: usize, instruction: &Instruction) -> Bool {
	(instruction.offset() >= handler_start).fail("")
}

fn no_attempt_to_return_normally(instructions: &Vec<Instruction>) -> Bool {
	// noAttemptToReturnNormally(Instructions) :-
	//     notMember(instruction(_, return), Instructions).
	let a = || {
		for instruction in instructions {
			if let Instruction::Instruction(_, Opcode::Return) = instruction {
				return false;
			}
		}
		true
	};

	// noAttemptToReturnNormally(Instructions) :-
	//     member(instruction(_, athrow), Instructions).
	let b = || {
		for instruction in instructions {
			if let Instruction::Instruction(_, Opcode::AThrow) = instruction {
				return true;
			}
		}
		false
	};

	a().fail("").or_else(|_| b().fail(""))
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
	todo!()
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
fn method_initial_stack_frame(class: &Class, method: &Method, frame_size: usize) -> Result<(Frame, VerificationType)> {
	let descriptor = method.descriptor()?;
	let (raw_args, return_type) = parse_method_descriptor(&descriptor)?;
	let args = expand_type_list(&raw_args)?;
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
		operand_stack: Vec::new(),
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
fn expand_type_list(list: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	let mut out = Vec::new();
	for i in list {
		if size_of(i, 1).is_ok() {
			out.push(i.clone());
		} else if size_of(i, 2).is_ok() {
			out.push(i.clone());
			out.push(VerificationType::Top);
		} else {
			fail("")?;
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
fn expand_to_length<T: Clone>(mut list: Vec<T>, size: usize, filler: T) -> Result<Vec<T>> {
	if list.len() == size {
		Ok(list)
	} else {
		let list_length = list.len();
		(list_length < size).fail("")?;
		let delta = size - list_length; // cannot panic, see check above
		for _ in 0..delta {
			list.push(filler.clone())
		}
		Ok(list)
	}
}

fn method_initial_this_type(class: &Class, method: &Method) -> Result<Option<VerificationType>> {
	// methodInitialThisType(_Class, Method, []) :-
	//     methodAccessFlags(Method, AccessFlags),
	//     member(static, AccessFlags),
	//     methodName(Method, MethodName),
	//     MethodName \= '<init>'.

	// methodInitialThisType(Class, Method, [This]) :-
	//     methodAccessFlags(Method, AccessFlags),
	//     notMember(static, AccessFlags),
	//     instanceMethodInitialThisType(Class, Method, This).

	if method.access_flags()?.is_static {
		let method_name = method.name()?;
		(method_name != "<init>").fail("")?;
		Ok(None)
	} else {
		// not_member(static, access_flags)
		Ok(Some(instance_method_initial_this_type(class, method)?))
	}
}

fn instance_method_initial_this_type(class: &Class, method: &Method) -> Result<VerificationType> {
	// instanceMethodInitialThisType(Class, Method, class('java/lang/Object', L)) :-
	//     methodName(Method, '<init>'),
	//     classDefiningLoader(Class, L),
	//     isBootstrapLoader(L),
	//     classClassName(Class, 'java/lang/Object').
	let a = || -> Result<VerificationType> {
		(method.name()? == "<init>").fail("")?;
		let l = class.defining_loader()?;
		l.is_bootstrap_loader()?;
		(class.class_name()? == JAVA_LANG_OBJECT).fail("")?;
		Ok(VerificationType::Class(JAVA_LANG_OBJECT.into(), l))
	};

	// instanceMethodInitialThisType(Class, Method, uninitializedThis) :-
	//     methodName(Method, '<init>'),
	//     classClassName(Class, ClassName),
	//     classDefiningLoader(Class, CurrentLoader),
	//     superclassChain(ClassName, CurrentLoader, Chain),
	//     Chain \= [].
	let b = |_| -> Result<VerificationType> {
		(method.name()? == "<init>").fail("")?;
		let class_name = class.class_name()?;
		let current_loader = class.defining_loader()?;
		let chain = superclass_chain(&class_name, &current_loader)?;
		(!chain.is_empty()).fail("")?;
		Ok(VerificationType::UninitializedThis)
	};

	// instanceMethodInitialThisType(Class, Method, class(ClassName, L)) :-
	//     methodName(Method, MethodName),
	//     MethodName \= '<init>',
	//     classDefiningLoader(Class, L),
	//     classClassName(Class, ClassName).
	let c = |_| {
		let method_name = method.name()?;
		(method_name != "<init>").fail("")?;
		let l = class.defining_loader()?;
		let class_name = class.class_name()?;
		Ok(VerificationType::Class(class_name, l))
	};

	a().or_else(b).or_else(c)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instruction {
	Instruction(usize, Opcode),
	StackMap(StackMap),
	EndOfCode(usize),
}
impl Instruction {
	fn offset(&self) -> usize {
		*match self {
			Instruction::Instruction(offset, _) => offset,
			Instruction::StackMap(StackMap { offset, .. }) => offset,
			Instruction::EndOfCode(offset) => offset,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FrameT {
	Frame(Frame),
	AfterGoto,
}

fn merged_code_is_type_safe(environment: &Environment, code: &Vec<Instruction>, frame_t: &FrameT) -> Bool {
	// mergedCodeIsTypeSafe(Environment, [stackMap(Offset, MapFrame) | MoreCode],
	//                      frame(Locals, OperandStack, Flags)) :-
	//     frameIsAssignable(frame(Locals, OperandStack, Flags), MapFrame),
	//     mergedCodeIsTypeSafe(Environment, MoreCode, MapFrame).
	let a = || {
		if let (Some(Instruction::StackMap(StackMap {
			offset: _,
			type_state: map_frame,
		})), more_code) = head_rest(code) {
			if let FrameT::Frame(frame) = frame_t {
				Frame::is_assignable(frame, map_frame)?;
				merged_code_is_type_safe(environment, &more_code, &FrameT::Frame(map_frame.clone()))
			} else {
				fail("")
			}
		} else {
			fail("")
		}
	};

	// mergedCodeIsTypeSafe(Environment, [instruction(Offset, Parse) | MoreCode],
	//                      frame(Locals, OperandStack, Flags)) :-
	//     instructionIsTypeSafe(Parse, Environment, Offset,
	//                           frame(Locals, OperandStack, Flags),
	//                           NextStackFrame, ExceptionStackFrame),
	//     instructionSatisfiesHandlers(Environment, Offset, ExceptionStackFrame),
	//     mergedCodeIsTypeSafe(Environment, MoreCode, NextStackFrame).
	let b = |_| {
		if let (Some(Instruction::Instruction(offset, parse)), more_code) = head_rest(code) {
			if let FrameT::Frame(frame) = frame_t {
				let (next_stack_frame, exception_stack_frame) = instruction_is_type_safe(
					parse,
					environment,
					*offset,
					frame
				)?;
				instruction_satisfies_handlers(environment, *offset, &exception_stack_frame)?;
				merged_code_is_type_safe(environment, &more_code, &next_stack_frame)
			} else {
				fail("")
			}
		} else {
			fail("")
		}
	};

	// mergedCodeIsTypeSafe(Environment, [stackMap(Offset, MapFrame) | MoreCode],
	//                      afterGoto) :-
	//     mergedCodeIsTypeSafe(Environment, MoreCode, MapFrame).
	let c = |_| {
		if let (Some(Instruction::StackMap(StackMap {
			offset: _,
			type_state: map_frame,
		})), more_code) = head_rest(code) {
			if let FrameT::AfterGoto = frame_t {
				merged_code_is_type_safe(environment, &more_code, &FrameT::Frame(map_frame.clone()))
			} else {
				fail("")
			}
		} else {
			fail("")
		}
	};

	// mergedCodeIsTypeSafe(_Environment, [instruction(_, _) | _MoreCode],
	//                      afterGoto) :-
	//     write_ln('No stack frame after unconditional branch'),
	//     fail.
	let d = |_| {
		if let (Some(Instruction::Instruction(_, _)), _) = head_rest(code) {
			if let FrameT::AfterGoto = frame_t {
				fail("No stack frame after unconditional branch")
			} else {
				fail("")
			}
		} else {
			fail("")
		}
	};

	// mergedCodeIsTypeSafe(_Environment, [endOfCode(Offset)],
	//                      afterGoto).
	let e = |_| {
		if let (Some(Instruction::EndOfCode(_)), vec) = head_rest(code) {
			if let FrameT::AfterGoto = frame_t {
				if vec.is_empty() {
					Ok(())
				} else {
					fail("")
				}
			} else {
				fail("")
			}
		} else {
			fail("")
		}
	};

	a().or_else(b).or_else(c).or_else(d).or_else(e)
}

// targetIsTypeSafe(Environment, StackFrame, Target) :-
//     offsetStackFrame(Environment, Target, Frame),
//     frameIsAssignable(StackFrame, Frame).
fn target_is_type_safe(environment: &Environment, stack_frame: &Frame, target: usize) -> Bool {
	let frame = environment.offset_stack_frame(target)?;
	Frame::is_assignable(stack_frame, &frame)
}

// instructionSatisfiesHandlers(Environment, Offset, ExceptionStackFrame) :-
//     exceptionHandlers(Environment, Handlers),
//     sublist(isApplicableHandler(Offset), Handlers, ApplicableHandlers),
//     checklist(instructionSatisfiesHandler(Environment, ExceptionStackFrame),
//               ApplicableHandlers).
fn instruction_satisfies_handlers(environment: &Environment, offset: usize, exception_stack_frame: &Frame) -> Bool {
	let handlers = environment.exception_handlers()?;
	let applicable_handlers = handlers.iter()
		.filter(|handler| is_applicable_handler(offset, handler))
		.collect::<Vec<_>>();

	for handler in applicable_handlers {
		instruction_satisfies_handler(environment, exception_stack_frame, handler)?;
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
	let current_loader = environment.current_class_loader()?;
	let exception_class = handler_exception_class(handler, &current_loader)?;

	let locals = &exc_stack_frame.locals;

	let true_exc_stack_frame = Frame {
		locals: locals.clone(),
		operand_stack: vec![exception_class],
		flag_this_uninit: exc_stack_frame.flag_this_uninit,
	};

	operand_stack_has_legal_length(environment, &true_exc_stack_frame.operand_stack)?;
	target_is_type_safe(environment, &true_exc_stack_frame, target)
}

// 4.10.1.7

fn nth0<T: Clone>(index: usize, vec: &Vec<T>) -> Result<T> {
	match vec.get(index) {
		Some(x) => Ok(x.clone()),
		None => fail(""),
	}
}

// loadIsTypeSafe(Environment, Index, Type, StackFrame, NextStackFrame) :-
//     StackFrame = frame(Locals, _OperandStack, _Flags),
//     nth0(Index, Locals, ActualType),
//     isAssignable(ActualType, Type),
//     validTypeTransition(Environment, [], ActualType, StackFrame,
//                         NextStackFrame).
fn load_is_type_safe(environment: &Environment, index: &LvIndex, type_: &VerificationType, stack_frame: &Frame) -> Result<Frame> {
	let locals = &stack_frame.locals;
	let actual_type = nth0(index.0, locals)?;
	VerificationType::is_assignable(&actual_type, type_)?;
	valid_type_transition(environment, &Vec::new(), &actual_type, stack_frame)
}

// storeIsTypeSafe(_Environment, Index, Type,
//                 frame(Locals, OperandStack, Flags),
//                 frame(NextLocals, NextOperandStack, Flags)) :-
//     popMatchingType(OperandStack, Type, NextOperandStack, ActualType),
//     modifyLocalVariable(Index, ActualType, Locals, NextLocals).
fn store_is_type_safe(_environment: &Environment, index: &LvIndex, type_: &VerificationType, frame: &Frame) -> Result<Frame> {
	let (next_operand_stack, actual_type) = pop_matching_type(&frame.operand_stack, type_)?;
	let next_locals = modify_local_variable(index.0, &actual_type, &frame.locals)?;
	Ok(Frame {
		locals: next_locals,
		operand_stack: next_operand_stack,
		flag_this_uninit: frame.flag_this_uninit,
	})
}

// modifyLocalVariable(Index, Type, Locals, NewLocals) :-
//     modifyLocalVariable(0, Index, Type, Locals, NewLocals).
fn modify_local_variable(index: usize, type_: &VerificationType, locals: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	modify_local_variable_5(0, index, type_, locals)
}

fn modify_local_variable_5(i: usize, index: usize, type_: &VerificationType, locals: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
	// modifyLocalVariable(I, Index, Type,
	//                     [Locals1 | LocalsRest],
	//                     [Locals1 | NextLocalsRest] ) :-
	//     I < Index - 1,
	//     I1 is I + 1,
	//     modifyLocalVariable(I1, Index, Type, LocalsRest, NextLocalsRest).
	let a = || -> Result<Vec<VerificationType>> {
		(i < index - 1).fail("")?; // TODO: could panic?
		let i1 = i + 1;

		todo!()
	};

	// modifyLocalVariable(I, Index, Type,
	//                     [Locals1 | LocalsRest],
	//                     [NextLocals1 | NextLocalsRest] ) :-
	//     I =:= Index - 1,
	//     modifyPreIndexVariable(Locals1, NextLocals1),
	//     modifyLocalVariable(Index, Index, Type, LocalsRest, NextLocalsRest).
	let b = |_| -> Result<Vec<VerificationType>> {
		(i == index - 1).fail("")?; // TODO: could panic?

		todo!()
	};

	// modifyLocalVariable(Index, Index, Type,
	//                     [_ | LocalsRest], [Type | LocalsRest]) :-
	//     sizeOf(Type, 1).
	let c = |_| -> Result<Vec<VerificationType>> {
		(i == index).fail("")?;
		size_of(type_, 1)?;
		todo!()
	};

	// modifyLocalVariable(Index, Index, Type,
	//                     [_, _ | LocalsRest], [Type, top | LocalsRest]) :-
	//     sizeOf(Type, 2).
	let d = |_| {
		(i == index).fail("")?;
		size_of(type_, 2)?;
		todo!()
	};

	a().or_else(b).or_else(c).or_else(d)
}

// modifyPreIndexVariable(Type, Type) :- sizeOf(Type, 1).
// modifyPreIndexVariable(Type, top) :- sizeOf(Type, 2).
fn modify_pre_index_variable(type_: &VerificationType) -> Result<VerificationType> {
	let a = || -> Result<VerificationType> {
		size_of(type_, 1)?;
		Ok(type_.clone())
	};
	let b = |_| {
		size_of(type_, 2)?;
		Ok(VerificationType::Top)
	};
	a().or_else(b)
}

// 4.10.1.8

fn passes_protected_check<Name, Descriptor>(environment: &Environment, member_class_name: &ClassName, member_name: &Name, member_descriptor: &Descriptor, stack_frame: &Frame) -> Bool {
	// passesProtectedCheck(Environment, MemberClassName, MemberName,
	//                      MemberDescriptor, StackFrame) :-
	//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
	//     superclassChain(CurrentClassName, CurrentLoader, Chain),
	//     notMember(class(MemberClassName, _), Chain).
	let a = || -> Bool {
		let (current_class_name, current_loader) = environment.this_class()?;
		let chain = superclass_chain(&current_class_name, &current_loader)?;
		//not_member(class(member_class_name, _), chain)
		todo!()
	};

	// passesProtectedCheck(Environment, MemberClassName, MemberName,
	//                      MemberDescriptor, StackFrame) :-
	//     thisClass(Environment, class(CurrentClassName, CurrentLoader)),
	//     superclassChain(CurrentClassName, CurrentLoader, Chain),
	//     member(class(MemberClassName, _), Chain),
	//     classesInOtherPkgWithProtectedMember(
	//       class(CurrentClassName, CurrentLoader),
	//       MemberName, MemberDescriptor, MemberClassName, Chain, []).
	let b = |_| -> Bool {
		let (current_class_name, current_loader) = environment.this_class()?;
		let chain = superclass_chain(&current_class_name, &current_loader)?;
		//member(class(member_class_name, _), chain)?;
		//classes_in_other_pkg_with_protected_member(
		//	(current_class_name, current_loader),
		//	member_name, member_descriptor, member_class_name, chain, Vec::new()
		//)
		todo!()
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
	//     isNotProtected(ReferencedClass, MemberName, MemberDescriptor).
	let c = |_| -> Bool {
		todo!()
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
		todo!()
	};

	a().or_else(b).or_else(c).or_else(d)
}

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
fn classes_in_other_pkg_with_protected_member<Name>(class: &Class, member_name: &Name, ) -> Bool {
	todo!()
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
			locals: self.locals.clone(),
			operand_stack: Vec::new(),
			flag_this_uninit: self.flag_this_uninit,
		})
	}
}

fn instruction_is_type_safe(instruction: &Opcode, environment: &Environment, offset: usize, stack_frame: &Frame) -> Result<(FrameT, Frame)> {
	use VerificationType::*;
	use Opcode::*;

	#[deprecated]
	let next_stack_frame = stack_frame.clone();

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
			let array_type = nth1_operand_stack_is(2, stack_frame)?;
			let component_type = array_type.array_component_type()?;
			let bl = get_bootstrap_loader()?;
			bl.is_bootstrap_loader()?;
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![
					Int,
					ArrayOf(ArrayType::Other(Box::new(Class(JAVA_LANG_OBJECT.into(), bl))))
				],
				&component_type.as_verification_type(),
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let bl = get_bootstrap_loader()?;
			bl.is_bootstrap_loader()?;
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![
					Class(JAVA_LANG_OBJECT.into(), bl.clone()),
					Int,
					ArrayOf(ArrayType::Other(Box::new(Class(JAVA_LANG_OBJECT.into(), bl))))
				]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(aconst_null, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], null, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AConstNull => {
			let next_stack_frame = valid_type_transition(
				environment,
				&Vec::new(),
				&Null,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(aload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, reference, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ALoad(index) => {
			let next_stack_frame = load_is_type_safe(environment, index, &Reference, stack_frame)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(anewarray(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     validTypeTransition(Environment, [int], arrayOf(CP),
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ANewArray{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(areturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, ReturnType),
		//     isAssignable(ReturnType, reference),
		//     canPop(StackFrame, [ReturnType], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AReturn => {
			let return_type = environment.this_method_return_type()?;
			VerificationType::is_assignable(return_type, &Reference)?;
			let _popped_stack_frame = can_pop(stack_frame, &vec![return_type.clone()])?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(arraylength, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     nth1OperandStackIs(1, StackFrame, ArrayType),
		//     arrayComponentType(ArrayType, _),
		//     validTypeTransition(Environment, [top], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ArrayLength => {
			let array_type = nth1_operand_stack_is(1, stack_frame)?;
			let _ = array_type.array_component_type()?;
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Top],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(astore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, reference, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AStore(index) => {
			let next_stack_frame = store_is_type_safe(
				environment,
				index,
				&Reference,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(athrow, _Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     isBootstrapLoader(BL),
		//     canPop(StackFrame, [class('java/lang/Throwable', BL)], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		AThrow => {
			let bl = get_bootstrap_loader()?;
			bl.is_bootstrap_loader()?;
			let _popped_stack_frame = can_pop(
				&stack_frame,
				&vec![Class(JAVA_LANG_THROWABLE.into(), bl)]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let array_type = nth1_operand_stack_is(2, &stack_frame)?;
			array_type.is_small_array()?;
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, Top],
				&Int,
				&stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(bastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     nth1OperandStackIs(3, StackFrame, ArrayType),
		//     isSmallArray(ArrayType),
		//     canPop(StackFrame, [int, int, top], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		BAStore => {
			let array_type = nth1_operand_stack_is(3, &stack_frame)?;
			array_type.is_small_array()?;
			let next_stack_frame = can_pop(
				&stack_frame,
				&vec![Int, Int, Top],
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Char)],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(castore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(char)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		CAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Int, Int, ArrayOf(ArrayType::Char)]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(checkcast(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     isBootstrapLoader(BL),
		//     validTypeTransition(Environment, [class('java/lang/Object', BL)], CP,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		CheckCast{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2f => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2i => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(d2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		D2l => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double, Double],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(daload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(double)], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DALoad => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Other(Box::new(Double)))],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [double, int, arrayOf(double)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Double, Int, ArrayOf(ArrayType::Other(Box::new(Double)))]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dcmpg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double, double], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(dcmpl, dcmpg).
		DCmpG | DCmpL => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double, Double],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(dconst_1, dconst_0).
		DConst0 | DConst1 => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DLoad(index) => {
			let next_stack_frame = load_is_type_safe(
				environment,
				index,
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [double], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DNeg => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Double],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dreturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, double),
		//     canPop(StackFrame, [double], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DReturn => {
			(environment.this_method_return_type()? == &Double).fail("")?;
			let _popped_stack_frame = can_pop(
				stack_frame,
				&vec![Double]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(dstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, double, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DStore(index) => {
			let next_stack_frame = store_is_type_safe(
				environment,
				index,
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(dup_x2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		DupX2 => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     dup_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).
		//
		// dup_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     dup_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack).

		// dup_x2Form1IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory1(InputOperandStack, Type1, Stack1),
		//     popCategory1(Stack1, Type2, Stack2),
		//     popCategory1(Stack2, Type3, Rest),
		//     canSafelyPushList(Environment, Rest, [Type1, Type3, Type2, Type1],
		//                       OutputOperandStack).

		// dup_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory1(InputOperandStack, Type1, Stack1),
		//     popCategory2(Stack1, Type2, Rest),
		//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
		//                       OutputOperandStack).

		// instructionIsTypeSafe(dup2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2FormIsTypeSafe(Environment,InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2 => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

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

		// dup2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack):-
		//     popCategory2(InputOperandStack, Type, _),
		//     canSafelyPush(Environment, InputOperandStack, Type, OutputOperandStack).

		// instructionIsTypeSafe(dup2_x1, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2_x1FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2X1 => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

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

		// dup2_x1Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory2(InputOperandStack, Type1, Stack1),
		//     popCategory1(Stack1, Type2, Rest),
		//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
		//                       OutputOperandStack).

		// instructionIsTypeSafe(dup2_x2, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     dup2_x2FormIsTypeSafe(Environment, InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Dup2X2 => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

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

		// dup2_x2Form2IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory2(InputOperandStack, Type1, Stack1),
		//     popCategory1(Stack1, Type2, Stack2),
		//     popCategory1(Stack2, Type3, Rest),
		//     canSafelyPushList(Environment, Rest,
		//                       [Type1, Type3, Type2, Type1],
		//                       OutputOperandStack).

		// dup2_x2Form3IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory1(InputOperandStack, Type1, Stack1),
		//     popCategory1(Stack1, Type2, Stack2),
		//     popCategory2(Stack2, Type3, Rest),
		//     canSafelyPushList(Environment, Rest,
		//                       [Type2, Type1, Type3, Type2, Type1],
		//                       OutputOperandStack).

		// dup2_x2Form4IsTypeSafe(Environment, InputOperandStack, OutputOperandStack) :-
		//     popCategory2(InputOperandStack, Type1, Stack1),
		//     popCategory2(Stack1, Type2, Rest),
		//     canSafelyPushList(Environment, Rest, [Type1, Type2, Type1],
		//                       OutputOperandStack).

		// instructionIsTypeSafe(f2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2d => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(f2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2i => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(f2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		F2l => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float, Float],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(faload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(float)], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FALoad => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Other(Box::new(Float)))],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [float, int, arrayOf(float)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Float, Int, ArrayOf(ArrayType::Other(Box::new(Float)))],
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fcmpg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float, float], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(fcmpl, fcmpg).
		FCmpG | FCmpL => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float, Float],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(fconst_1, fconst_0).
		// instructionHasEquivalentTypeRule(fconst_2, fconst_0).
		FConst0 | FConst1 | FConst2 => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FLoad(index) => {
			let next_stack_frame = load_is_type_safe(
				environment,
				index,
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(fneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [float], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FNeg => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Float],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(freturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, float),
		//     canPop(StackFrame, [float], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FReturn => {
			(environment.this_method_return_type()? == &Float).fail("")?;
			let _popped_stack_frame = can_pop(
				stack_frame,
				&vec![Float]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(fstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, float, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		FStore(index) => {
			let next_stack_frame = store_is_type_safe(
				environment,
				index,
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		GetField{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(getstatic(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(_FieldClass, _FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     validTypeTransition(Environment, [], FieldType,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		GetStatic{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(goto(Target), Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     targetIsTypeSafe(Environment, StackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(goto_w(Target), goto(Target)).
		Goto(target) => {
			target_is_type_safe(
				environment,
				stack_frame,
				target.0.0
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(i2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2d => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(i2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2f => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(i2l, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		I2l => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, Int],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iaload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(int)], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IALoad => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Other(Box::new(Int)))],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(int)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Int, Int, ArrayOf(ArrayType::Other(Box::new(Int)))]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		}

		// instructionIsTypeSafe(if_acmpeq(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference, reference], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(if_acmpne(Target), if_acmpeq(Target)).
		IfACmpEq(target) | IfACmpNe(target) => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Reference, Reference]
			)?;
			target_is_type_safe(environment, &next_stack_frame, target.0.0)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		IfICmpEq(target) | IfICmpGe(target) | IfICmpGt(target) | IfICmpLe(target) | IfICmpLt(target) | IfICmpNe(target) => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Int, Int]
			)?;
			target_is_type_safe(environment, &next_stack_frame, target.0.0)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		IfEq(target) | IfGe(target) | IfGt(target) | IfLe(target) | IfLt(target) | IfNe(target) => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Int]
			)?;
			target_is_type_safe(environment, &next_stack_frame, target.0.0)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ifnonnull(Target), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference], NextStackFrame),
		//     targetIsTypeSafe(Environment, NextStackFrame, Target),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(ifnull(Target), ifnonnull(Target)).
		IfNonNull(_) | IfNull(_) => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iinc(Index, _Value), _Environment, _Offset,
		//                       StackFrame, StackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, _OperandStack, _Flags),
		//     nth0(Index, Locals, int),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IInc{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(iload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		ILoad(index) => {
			let next_stack_frame = load_is_type_safe(environment, index, &Int, stack_frame)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(instanceof(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     (CP = class(_, _) ; CP = arrayOf(_)),
		//     isBootstrapLoader(BL),
		//     validTypeTransition(Environment, [class('java/lang/Object', BL)], int,
		//                         StackFrame,NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		InstanceOf { class } => {
			//todo!(); // class = class(_, _) || class = arrayOf(_)

			let bl = get_bootstrap_loader()?;
			bl.is_bootstrap_loader()?;
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Class(JAVA_LANG_OBJECT.into(), bl)],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// countIsValid(Count, InputFrame, OutputFrame) :-
		//     InputFrame = frame(_Locals1, OperandStack1, _Flags1),
		//     OutputFrame = frame(_Locals2, OperandStack2, _Flags2),
		//     length(OperandStack1, Length1),
		//     length(OperandStack2, Length2),
		//     Count =:= Length1 - Length2.

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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		//
		// substitute(_Old, _New, [], []).
		// substitute(Old, New, [Old | FromRest], [New | ToRest]) :-
		//     substitute(Old, New, FromRest, ToRest).
		// substitute(Old, New, [From1 | FromRest], [From1 | ToRest]) :-
		//     From1 \= Old,
		//     substitute(Old, New, FromRest, ToRest).

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
		InvokeStatic{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		InvokeVirtual{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ireturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, int),
		//     canPop(StackFrame, [int], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IReturn => {
			(environment.this_method_return_type()? == &Int).fail("")?;
			let _popped_stack_frame = can_pop(
				stack_frame,
				&vec![Int]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::AfterGoto, exception_stack_frame))
		},

		// instructionIsTypeSafe(istore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		IStore(index) => {
			let next_stack_frame = store_is_type_safe(environment, index, &Int, stack_frame)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2d, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], double,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2d => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long],
				&Double,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2f, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], float,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2f => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long],
				&Float,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(l2i, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		L2i => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long, Long],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(laload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(long)], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LALoad => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Other(Box::new(Long)))],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [long, int, arrayOf(long)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Long, Int, ArrayOf(ArrayType::Other(Box::new(Long)))]
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lcmp, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long, long], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LCmp => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long, Long],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lconst_0, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(lconst_1, lconst_0).
		LConst0 | LConst1 => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		LdcInt{..} | LdcFloat{..} | LdcReferenceString{..} | LdcReferenceClass{..} | LdcReferenceMethodType{..} | LdcReferenceMethodHandle {..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(ldc2_w(CP), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     functor(CP, Tag, _),
		//     member(Tag, [long, double]),
		//     validTypeTransition(Environment, [], Tag, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Ldc2WDouble{..} | Ldc2WLong{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lload(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     loadIsTypeSafe(Environment, Index, long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LLoad(index) => {
			let next_stack_frame = load_is_type_safe(environment, index, &Long, stack_frame)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lneg, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [long], long,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LNeg => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Long],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lookupswitch(Targets, Keys), Environment, _, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     sort(Keys, Keys),
		//     canPop(StackFrame, [int], BranchStackFrame),
		//     checklist(targetIsTypeSafe(Environment, BranchStackFrame), Targets),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LookupSwitch{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lreturn, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, long),
		//     canPop(StackFrame, [long], _PoppedStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LReturn => {
			(environment.this_method_return_type()? == &Long).fail("")?;
			let _popped_stack_frame = can_pop(stack_frame, &vec![Long])?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, Long],
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(lstore(Index), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     storeIsTypeSafe(Environment, Index, long, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		LStore(index) => {
			let next_stack_frame = store_is_type_safe(
				environment,
				index,
				&Long,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(monitorenter, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [reference], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(monitorexit, monitorenter).
		MonitorEnter | MonitorExit => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		MultiANewArray{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// classDimension(arrayOf(X), Dimension) :-
		//     classDimension(X, Dimension1),
		//     Dimension is Dimension1 + 1.
		//
		// classDimension(_, Dimension) :-
		//     Dimension = 0.

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
		New{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(newarray(TypeCode), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     primitiveArrayInfo(TypeCode, _TypeChar, ElementType, _VerifierType),
		//     validTypeTransition(Environment, [int], arrayOf(ElementType),
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		NewArray{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(stack_frame.clone()), exception_stack_frame))
		},

		// instructionIsTypeSafe(pop, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, [Type | Rest], Flags),
		//     Type \= top,
		//     sizeOf(Type, 1),
		//     NextStackFrame = frame(Locals, Rest, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Pop => {
			if let (Some(type_), rest) = head_rest(&stack_frame.operand_stack) {
				(type_ != &Top).fail("")?;
				size_of(type_, 1)?;
				let next_stack_frame = Frame {
					locals: stack_frame.locals.clone(),
					operand_stack: rest,
					flag_this_uninit: stack_frame.flag_this_uninit
				};
				let exception_stack_frame = stack_frame.exception_stack_frame()?;
				Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
			} else {
				fail("")
			}
		},

		// instructionIsTypeSafe(pop2, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     StackFrame = frame(Locals, InputOperandStack, Flags),
		//     pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack),
		//     NextStackFrame = frame(Locals, OutputOperandStack, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Pop2 => {
			fn pop2_some_form_is_type_safe(input_operand_stack: &Vec<VerificationType>) -> Result<Vec<VerificationType>> {
				let mut iter = input_operand_stack.iter();

				let type_1 = iter.next();
				let type_2 = iter.next();
				if let (Some(type_1), Some(type_2)) = (type_1, type_2) {
					// pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack) :-
					//     pop2Form1IsTypeSafe(InputOperandStack, OutputOperandStack).
					//
					// pop2SomeFormIsTypeSafe(InputOperandStack, OutputOperandStack) :-
					//     pop2Form2IsTypeSafe(InputOperandStack, OutputOperandStack).

					// pop2Form1IsTypeSafe([Type1, Type2 | Rest], Rest) :-
					//     sizeOf(Type1, 1),
					//     sizeOf(Type2, 1).
					let pop2_form1_is_type_safe = {
						size_of(type_1, 1).is_ok() &&
						size_of(type_2, 1).is_ok()
					};

					// pop2Form2IsTypeSafe([top, Type | Rest], Rest) :- sizeOf(Type, 2).
					let pop2_form2_is_type_safe = {
						type_1 == &Top &&
						size_of(type_2, 2).is_ok()
					};

					if pop2_form1_is_type_safe || pop2_form2_is_type_safe {
						Ok(iter.cloned().collect())
					} else {
						fail("")
					}
				} else {
					fail("")
				}
			}

			let next_stack_frame = Frame {
				locals: stack_frame.locals.clone(),
				operand_stack: pop2_some_form_is_type_safe(&stack_frame.operand_stack)?,
				flag_this_uninit: stack_frame.flag_this_uninit,
			};
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
		PutField{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(putstatic(CP), _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     CP = field(_FieldClass, _FieldName, FieldDescriptor),
		//     parseFieldDescriptor(FieldDescriptor, FieldType),
		//     canPop(StackFrame, [FieldType], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		PutStatic{..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(return, Environment, _Offset, StackFrame,
		//                       afterGoto, ExceptionStackFrame) :-
		//     thisMethodReturnType(Environment, void),
		//     StackFrame = frame(_Locals, _OperandStack, Flags),
		//     notMember(flagThisUninit, Flags),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		Return => {
			//(environment.this_method_return_type()? == &Void).fail("")?; // TODO: decide how to do this
			if stack_frame.flag_this_uninit {
				fail("")
			} else {
				let exception_stack_frame = stack_frame.exception_stack_frame()?;
				Ok((FrameT::AfterGoto, exception_stack_frame))
			}
		},

		// instructionIsTypeSafe(saload, Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [int, arrayOf(short)], int,
		//                         StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		SALoad => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![Int, ArrayOf(ArrayType::Short)],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(sastore, _Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     canPop(StackFrame, [int, int, arrayOf(short)], NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		SAStore => {
			let next_stack_frame = can_pop(
				stack_frame,
				&vec![Int, Int, ArrayOf(ArrayType::Short)],
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(sipush(_Value), Environment, _Offset, StackFrame,
		//                       NextStackFrame, ExceptionStackFrame) :-
		//     validTypeTransition(Environment, [], int, StackFrame, NextStackFrame),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		// instructionHasEquivalentTypeRule(bipush(Value), sipush(Value)).
		SIPush(..) | BIPush(..) => {
			let next_stack_frame = valid_type_transition(
				environment,
				&vec![],
				&Int,
				stack_frame
			)?;
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
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
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},

		// instructionIsTypeSafe(tableswitch(Targets, Keys), Environment, _Offset,
		//                       StackFrame, afterGoto, ExceptionStackFrame) :-
		//     sort(Keys, Keys),
		//     canPop(StackFrame, [int], BranchStackFrame),
		//     checklist(targetIsTypeSafe(Environment, BranchStackFrame), Targets),
		//     exceptionStackFrame(StackFrame, ExceptionStackFrame).
		TableSwitch {..} => {
			todo!();
			let exception_stack_frame = stack_frame.exception_stack_frame()?;
			Ok((FrameT::Frame(next_stack_frame), exception_stack_frame))
		},
	}
}