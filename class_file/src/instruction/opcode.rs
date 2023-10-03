use anyhow::{bail, Result};
use crate::cp::{FieldRefInfo, InterfaceMethodRefInfo, InvokeDynamicInfo, MethodHandleInfo, MethodRefInfo, Pool, PoolEntry};
use crate::descriptor::MethodDescriptor;
use crate::instruction::{BranchTarget, CodeReader, LvIndex};
use crate::name::ClassName;
use crate::verifier::VerificationType;

// TODO: remove
type LongInfo = i32;
type FloatInfo = i32;
type DoubleInfo = i32;
type IntegerInfo = i32;
type StringInfo = i32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayType {}

impl ArrayType {
	fn parse(_: u8) -> Result<ArrayType> {
		todo!()
	}
}

//#[warn(missing_docs)]

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
	/// Load `reference` from array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int ->
	/// ..., value: reference
	/// ```
	///
	/// # Description
	/// The `value` in the component of the array at `index` is retrieved and pushed onto the operand stack.
	///
	/// # Run-time Exceptions:
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	AALoad,
	/// Store into `reference` array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int, value: reference ->
	/// ...
	/// ```
	///
	/// # Description
	/// The `value` is stored as the component of the array at `index`.
	/// ... something about types, TODO: fill this in
	///
	/// # Run-time Exceptions
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	/// - If the actual type of `value` is not assignment compatible with the actual type of the components of the array, throw an
	///   `java.lang.ArrayStoreException`.
	AAStore,
	/// Push `null`.
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., null: reference
	/// ```
	///
	/// # Description
	/// Push the `null` object `reference` onto the operand stack.
	AConstNull,
	/// Load `reference` from local variable.
	///
	/// # Format
	/// ```txt
	/// ALoad
	/// index
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., objectref: reference
	/// ```
	///
	/// # Description
	/// The `index` is an unsigned byte that must be an index into the local variable array of the current frame. The local variable at `index` must contain a
	/// `reference`. The `objectref` in the local variable at `index` is pushed onto the operand stack.
	///
	/// # Notes
	/// The [Opcode::ALoad] instruction cannot be used to load a value of type `returnAddress` from a local variable onto the operand stack. This asymmetry
	/// with the [Opcode::AStore] instruction is intentional.
	ALoad(LvIndex),
	/// Create new array of `reference`.
	///
	/// # Format
	/// ```txt
	/// ANewArray
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ..., count: int ->
	/// ..., arrayref: reference
	/// ```
	///
	/// # Description
	/// The `count` represents the number of components of the array to be created.
	/// The unsigned `indexbyte1` and `indexbyte2` are used to construct an index into the run-time constant pool of the current class, where the value of the
	/// index is `(indexbyte1 << 8) | indexbyte2`.
	/// The run-time constant pool item at that index must be a symbolic reference to a class, array, or interface type.
	/// The named class, array, or interface type is resolved.
	/// A new array with components of that type, of length `count`, is allocated from the garbage-collected heap,
	/// and a reference arrayref to this new array object is pushed onto the operand stack. All components of the new array are initialized to `null`,
	/// the default value for reference types.
	///
	/// # Linking Exceptions
	/// Resolving the class, array or interface type can ofc throw.
	///
	/// # Run-time Exceptions
	/// - If `count` is less than zero, throw a `java.lang.NegativeArraySizeException`.
	ANewArray(VerificationType),
	/// Return `reference` from method.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., objectref: reference ->
	/// [empty]
	/// ```
	///
	/// # Description
	/// The `objectref` must refer to an object of a type that is assignment compatible with the type represented by the return descriptor of the current method.
	/// If the current method is a synchronized method, the monitor entered or reentered on invocation of the method is updated and possibly exited as if
	/// by execution of a [Self::MonitorExit] instruction in the current thread.
	/// If no exception is thrown, `objectref` is popped from the operand stack of the current frame and pushed onto the operand stack of the frame of the
	/// invoker. Any other values on the operand stack of the current method are discarded.
	///
	/// The interpreter then reinstates the frame of the invoker and returns control to the invoker.
	///
	/// # Run-time Exceptions
	/// ... not done yet, // TODO: fill in?
	AReturn,
	/// Get length of array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference ->
	/// ..., length: int
	/// ```
	///
	/// # Description
	/// The `length` of the array referenced by the `arrayref` is determined and pushed onto the stack.
	///
	/// # Run-time Exception
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	ArrayLength,
	/// Store `reference` into local variable.
	///
	/// # Format
	/// ```txt
	/// AStore
	/// index
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ..., objectref ->
	/// ...
	/// ```
	///
	/// # Description
	/// The `index` is an unsigned byte that must be an index into the local variable array of the current frame. The `objectref` on the top of the operand
	/// stack must be of type `returnAddress` or of type `reference`. It is popped from the operand stack, and the value of the local variable at `index` is set
	/// to `objectref`.
	///
	/// # Notes
	/// The [Opcode::AStore] instruction is used with an `objectref` of type `returnAddress` when implementing the finally clause of the Java programming
	/// language (§3.13).
	///
	/// The [Opcode::ALoad] instruction cannot be used to load a value of type `returnAddress` from a local variable onto the operand stack. This asymmetry with
	/// the [Opcode::AStore] instruction is intentional.
	AStore(LvIndex),
	/// Throw exception or error.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., objectref: reference ->
	/// objectref: reference
	/// ```
	///
	/// # Description
	/// The `objectref` must be of type reference and must refer to an object that is an instance of class `java/lang/Throwable` or of a subclass of
	/// `java/lang/Throwable`. It is popped from the operand stack. The `objectref` is then thrown by searching the current method for the first exception
	/// handler that matches the class of `objectref`, as given by the algorithm in §2.10.
	///
	/// If an exception handler that matches `objectref` is found, it contains the location of the code intended to handle this exception. The `pc` register is
	/// reset to that location, the operand stack of the current frame is cleared, `objectref` is pushed back onto the operand stack, and execution continues.
	///
	/// If no matching exception handler is found in the current frame, that frame is popped. If the current frame represents an invocation of a synchronized
	/// method, the monitor entered or reentered on invocation of the method is exited as if by execution of a [Opcode::MonitorExit] instruction. Finally, the
	/// frame of its invoker is reinstated, if such a frame exists, and the `objectref` is rethrown. If no such frame exists, the current thread exits.
	///
	/// # Run-time Exception
	/// - If `objectref` is `null`, throw a `java.lang.NullPointerException` instead of `objectref.
	///
	/// Otherwise, if the Java Virtual Machine implementation does not enforce the rules on structured locking described in §2.11.10, then if the method of the
	/// current frame is a synchronized method and the current thread is not the owner of the monitor entered or reentered on invocation of the method,
	/// [Opcode::AThrow] throws an `java.lang.IllegalMonitorStateException` instead of the object previously being thrown. This can happen, for example, if an
	/// abruptly completing synchronized method contains a [Opcode::MonitorExit] instruction, but no [Opcode::MonitorEnter] instruction, on the object on which
	/// the method is synchronized.
	///
	/// Otherwise, if the Java Virtual Machine implementation enforces the rules on structured locking described in §2.11.10 and if the first of those rules is
	/// violated during invocation of the current method, then [Opcode::AThrow] throws an `java.lang.IllegalMonitorStateException` instead of the object
	/// previously being thrown.
	///
	/// # Notes
	/// The operand stack diagram for the [Opcode::AThrow] instruction may be misleading: If a handler for this exception is matched in the current method, the
	/// [Opcode::AThrow] instruction discards all the values on the operand stack, then pushes the thrown object onto the operand stack. However, if no handler
	/// is matched in the current method and the exception is thrown farther up the method invocation chain, then the operand stack of the method (if any) that
	/// handles the exception is cleared and `objectref` is pushed onto that empty operand stack. All intervening frames from the method that threw the
	/// exception up to, but not including, the method that handles the exception are discarded.
	AThrow,
	/// Load `byte` or `boolean` from array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The `arrayref` must be of type `reference` and must refer to an array whose components are of type `byte` or of type `boolean`. The `index` must be of
	/// type `int`. Both `arrayref` and `index` are popped from the operand stack. The `byte` value in the component of the array at `index` is retrieved,
	/// sign-extended to an `int` value, and pushed onto the top of the operand stack.
	///
	/// # Run-time Exceptions
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	///
	/// # Notes
	/// The [Opcode::BALoad] instruction is used to load values from both `byte` and `boolean` arrays. In Oracle's Java Virtual Machine implementation,
	/// `boolean` arrays - that is, arrays of type `T_BOOLEAN` (§2.2, [Opcode::NewArray], [ArrayType::Boolean]) - are implemented as arrays of 8-bit values.
	/// Other implementations may implement packed boolean arrays; the [Opcode::BALoad] instruction of such implementations must be used to access those arrays.
	BALoad,
	/// Store into `byte` or `boolean` array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int, value: int ->
	/// ...
	/// ```
	///
	/// # Description
	/// The `arrayref` must be of type `reference` and must refer to an array whose components are of type `byte` or of type `boolean`. The `index` and the
	/// `value` must both be of type `int`. The `arrayref`, `index`, and `value` are popped from the operand stack. The `int` value is truncated to a `byte`
	/// and stored as the component of the array indexed by `index`.
	///
	/// # Run-time Exceptions
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	///
	/// # Notes
	/// The [Opcode::BAStore] instruction is used to store values into both `byte` and `boolean` arrays. In Oracle's Java Virtual Machine implementation,
	/// `boolean` arrays - that is, arrays of type `T_BOOLEAN` (§2.2, [Opcode::NewArray], [ArrayType::Boolean]) - are implemented as arrays of 8-bit values.
	/// Other implementations may implement packed boolean arrays; in such implementations the [Opcode::BAStore] instruction must be able to store `boolean`
	/// values into packed boolean arrays as well as `byte` values into `byte` arrays.
	BAStore,
	/// Push `byte`
	///
	/// # Format
	/// ```txt
	/// BIPush
	/// byte
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The immediate `byte` is sign-extended to an `int` value. That value is pushed onto the operand stack.
	BIPush(u8),
	/// Implementation instruction.
	/// This opcode is intended to be used by debuggers to implement breakpoints.
	Breakpoint,
	/// Load `char` from array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The `arrayref` must be of type `reference` and must refer to an array whose components are of type `char`. The `index` must be of type `int`. Both
	/// `arrayref` and `index` are popped from the operand stack. The component of the array at `index` is retrieved and zero-extended to an `int` value. That
	/// value is pushed onto the operand stack.
	///
	/// # Run-time Exceptions
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	CALoad,
	/// Store into `char` array.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., arrayref: reference, index: int, value: int ->
	/// ...
	/// ```
	///
	/// # Description
	/// The `arrayref` must be of type `reference` and must refer to an array whose components are of type `char`. The `index` and the `value` must both be of
	/// type `int`. The `arrayref`, `index`, and `value` are popped from the operand stack. The `int` value is truncated to a `char` and stored as the component
	/// of the array indexed by `index`.
	///
	/// # Run-time Exceptions
	/// - If `arrayref` is `null`, throw a `java.lang.NullPointerException`.
	/// - If `index` is not within the bounds of the array referenced by `arrayref`, throw an `java.lang.ArrayIndexOutOfBoundsException`.
	CAStore,
	/// Check whether object is of given type
	/// # Operand Stack
	/// ```txt
	/// ..., objectref: reference ->
	/// ..., objectref
	/// ```
	/// # Description
	/// The `objectref` must be of type `reference`. The unsigned `indexbyte1` and `indexbyte2` are used to construct an index into the run-time constant pool
	/// of the current class (§2.6), where the value of the index is `(indexbyte1 << 8) | indexbyte2`. The run-time constant pool item at the index must be a
	/// symbolic reference to a class, array, or interface type.
	///
	/// If `objectref` is `null`, then the operand stack is unchanged.
	///
	/// Otherwise, the named class, array, or interface type is resolved (§5.4.3.1). If `objectref` can be cast to the resolved class, array, or interface type,
	/// the operand stack is unchanged; otherwise, the `checkcast` instruction throws a `java/lang/ClassCastException`.
	///
	/// The following rules are used to determine whether an `objectref` that is not `null` can be cast to the resolved type: if `S` is the class of the object
	/// referred to by `objectref` and `T` is the resolved class, array, or interface type, `checkcast` determines whether `objectref` can be cast to type `T`
	/// as follows:
	/// - If `S` is an ordinary (nonarray) class, then:
	///   - If `T` is a class type, then `S` must be the same class as `T`, or `S` must be a subclass of `T`;
	///   - If `T` is an interface type, then `S` must implement interface `T`.
	/// - If `S` is an interface type, then:
	///   - If `T` is a class type, then `T` must be `java/lang/Object`.
	///   - If `T` is an interface type, then `T` must be the same interface as `S` or a superinterface of `S`.
	/// - If `S` is a class representing the array type `SC[]`, that is, an array of components of type `SC`, then:
	///   - If `T` is a class type, then `T` must be `java/lang/Object`.
	///   - If `T` is an interface type, then `T` must be one of the interfaces implemented by arrays (JLS §4.10.3).
	///   - If `T` is an array type `TC[]`, that is, an array of components of type `TC`, then one of the following must be true:
	///     - `TC` and `SC` are the same primitive type.
	///     - `TC` and `SC` are reference types, and type `SC` can be cast to `TC` by recursive application of these rules.
	CheckCast(VerificationType),
	D2f,
	D2i,
	D2l,
	DAdd,
	DALoad,
	DAStore,
	DCmpG,
	DCmpL,
	DConst0,
	DConst1,
	DDiv,
	DLoad(LvIndex),
	DMul,
	DNeg,
	DRem,
	DReturn,
	DStore(LvIndex),
	DSub,
	Dup,
	DupX1,
	DupX2,
	Dup2,
	Dup2X1,
	Dup2X2,
	F2d,
	F2i,
	F2l,
	FAdd,
	FALoad,
	FAStore,
	FCmpG,
	FCmpL,
	FConst0,
	FConst1,
	FConst2,
	FDiv,
	FLoad(LvIndex),
	FMul,
	FNeg,
	FRem,
	FReturn,
	FStore(LvIndex),
	FSub,
	GetField(FieldRefInfo),
	GetStatic(FieldRefInfo),
	/// Branch always.
	///
	/// # Format
	/// ```txt
	/// Goto
	/// branchbyte1
	/// branchbyte2
	/// ```
	///
	/// # Operand Stack
	/// No change.
	///
	/// # Description
	/// The unsigned bytes `branchbyte1` and `branchbyte2` are used to construct a signed 16-bit `branchoffset`:
	/// ```
	/// branchoffset = (branchbyte1 << 8) | branchbyte2
	/// ```
	/// Executions proceeds at that offset from the address of the opcode of this [Opcode::Goto] instruction.
	///
	/// # Or // TODO: improve doc
	/// Branch always, wide index.
	///
	/// # Format
	/// ```txt
	/// GotoW
	/// branchbyte1
	/// branchbyte2
	/// branchbyte3
	/// branchbyte4
	/// ```
	///
	/// # Operand Stack
	/// No change.
	///
	/// # Description
	/// The unsigned bytes `branchbyte1`, `branchbyte2`, `branchbyte3` and `branchbyte4` are used to construct a signed 32-bit `branchoffset`:
	/// ```
	/// branchoffset = (branchbyte1 << 24) | (branchbyte2 << 16) | (branchbyte3 << 8) | branchbyte4
	/// ```
	/// Executions proceeds at that offset from the address of the opcode of this [Opcode::GotoW] instruction.
	Goto(BranchTarget),
	I2b,
	I2c,
	I2d,
	I2f,
	I2l,
	I2s,
	IAdd,
	IALoad,
	IAnd,
	IAStore,
	IConstM1, IConst0, IConst1, IConst2, IConst3, IConst4, IConst5,
	IDiv,
	IfACmpEq(BranchTarget),
	IfACmpNe(BranchTarget),
	IfICmpEq(BranchTarget),
	IfICmpGe(BranchTarget),
	IfICmpGt(BranchTarget),
	IfICmpLe(BranchTarget),
	IfICmpLt(BranchTarget),
	IfICmpNe(BranchTarget),
	IfEq(BranchTarget),
	IfGe(BranchTarget),
	IfGt(BranchTarget),
	IfLe(BranchTarget),
	IfLt(BranchTarget),
	IfNe(BranchTarget),
	IfNonNull(BranchTarget),
	IfNull(BranchTarget),
	IInc { lv_index: LvIndex, const_: i32 },
	ILoad(LvIndex),
	ImpDep1, ImpDep2,
	IMul,
	INeg,
	InstanceOf(VerificationType),
	InvokeDynamic { call_site: InvokeDynamicInfo, zero1: u8, zero2: u8 },
	InvokeInterface { method_ref: InterfaceMethodRefInfo, count: u8, zero: u8 },
	InvokeSpecial(MethodRefInfo),
	InvokeStatic(MethodRefInfo),
	InvokeVirtual(MethodRefInfo),
	IOr,
	IRem,
	IReturn,
	IShl,
	IShr,
	IStore(LvIndex),
	ISub,
	IUShr,
	IXor,
	L2d,
	L2f,
	L2i,
	LAdd,
	LALoad,
	LAnd,
	LAStore,
	LCmp,
	LConst0, LConst1,
	/// Push item from run-time constant pool.
	///
	/// # Format
	/// ```txt
	/// ldc
	/// index
	/// ```
	/// or
	/// ```txt
	/// ldc_w
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., value
	/// ```
	///
	/// # Description
	/// - The `index` is an unsigned byte that must be a valid index into the run-time constant pool of the current class (§2.6). The run-time constant pool
	/// entry at `index` either must be a run-time constant of type `int` or `float`, or a `reference` to a string literal, or a symbolic reference to a class,
	/// method type, or method handle (§5.1).
	/// - The unsigned `indexbyte1` and `indexbyte2` are assembled into an unsigned 16-bit `index` into the run-time constant pool of the current class (§2.6),
	/// where the value of the `index` is calculated as `(indexbyte1 << 8) | indexbyte2`. The `index` must be a valid index into the run-time constant pool of
	/// the current class. The run-time constant pool entry at the index either must be a run-time constant of type `int` or `float`, or a `reference` to a
	/// string literal, or a symbolic reference to a class, method type, or method handle (§5.1).
	///
	/// If the run-time constant pool entry is a run-time constant of type `int` or `float`, the numeric value of that run-time constant is pushed onto the
	/// operand stack as an `int` or `float`, respectively.
	///
	/// Otherwise, if the run-time constant pool entry is a reference to an instance of class `String` representing a string literal (§5.1), then a reference
	/// to that instance, `value`, is pushed onto the operand stack.
	///
	/// Otherwise, if the run-time constant pool entry is a symbolic reference to a class (§5.1), then the named class is resolved (§5.4.3.1) and a reference
	/// to the `Class` object representing that class, `value`, is pushed onto the operand stack.
	///
	/// Otherwise, the run-time constant pool entry must be a symbolic reference to a method type or a method handle (§5.1). The method type or method handle
	/// is resolved (§5.4.3.5) and a reference to the resulting instance of `java.lang.invoke.MethodType` or `java.lang.invoke.MethodHandle`, `value`, is
	/// pushed onto the operand stack.
	///
	/// # Linking Exceptions
	/// During resolution of a symbolic reference to a class, any of the exceptions pertaining to class resolution (§5.4.3.1) can be thrown.
	///
	/// During resolution of a symbolic reference to a method type or method handle, any of the exception pertaining to method type or method handle
	/// resolution (§5.4.3.5) can be thrown.
	///
	/// # Notes
	/// The `ldc` instruction can only be used to push a value of type `float` taken from the float value set (§2.3.2) because a constant of type `float` in
	/// the constant pool (§4.4.4) must be taken from the float value set.
	///
	/// The `ldc_w` instruction is identical to the `ldc` instruction except for its wider run-time constant pool index.
	LdcInt(IntegerInfo),
	/// See [Opcode::LdcInt].
	LdcFloat(FloatInfo),
	/// See [Opcode::LdcInt].
	LdcReferenceString(StringInfo),
	/// See [Opcode::LdcInt].
	LdcReferenceClass(ClassName),
	/// See [Opcode::LdcInt].
	LdcReferenceMethodType(MethodDescriptor),
	/// See [Opcode::LdcInt].
	LdcReferenceMethodHandle(MethodHandleInfo),
	/// Push long or double from run-time constant pool (wide index).
	///
	/// # Format
	/// ```txt
	/// ldc2_w
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., value
	/// ```
	///
	/// # Description
	/// The unsigned `indexbyte1` and `indexbyte2` are assembled into an unsigned 16-bit `index` into the run-time constant pool of the current class (§2.6),
	/// where the value of the `index` is calculated as `(indexbyte1 << 8) | indexbyte2`. The `index` must be a valid index into the run-time constant pool of
	/// the current class. The run-time constant pool entry at the `index` must be a run-time constant of type `long` or `double` (§5.1). The numeric value of
	/// that run-time constant is pushed onto the operand stack as a `long` or `double`, respectively.
	///
	/// # Notes
	/// Only a wide-index version of the `ldc2_w` instruction exists; there is no `ldc2` instruction that pushes a long or double with a single-byte `index`.
	///
	/// The `ldc2_w` instruction can only be used to push a value of type `double` taken from the double value set (§2.3.2) because a constant of type `double`
	/// in the constant pool (§4.4.5) must be taken from the double value set.
	Ldc2WDouble(DoubleInfo),
	/// See [Opcode::Ldc2WDouble].
	Ldc2WLong(LongInfo),
	LDiv,
	LLoad(LvIndex),
	LMul,
	LNeg,
	/// Access jump table by key match and jump.
	///
	/// # Format
	/// ```txt
	/// LookupSwitch
	/// <0-3 byte pad>
	/// defaultbyte1
	/// defaultbyte2
	/// defaultbyte3
	/// defaultbyte4
	/// npairs1
	/// npairs2
	/// npairs3
	/// npairs4
	/// (
	///   match1
	///   match2
	///   match3
	///   match4
	///   offset1
	///   offset2
	///   offset3
	///   offset4
	/// ) [npairs]
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ..., key: int ->
	/// ...
	/// ```
	///
	/// # Description
	/// A [Opcode::LookupSwitch] is a variable-length instruction. Immediately after the [Opcode::LookupSwitch] opcode, between zero and three bytes must act
	/// as padding, such that `defaultbyte1` begins at an address that is a multiple of four bytes from the start of the current method (the opcode of its first
	/// instruction). Immediately after the padding follow a series of signed 32-bit values: `default`, `npairs`, and then `npairs` pairs of signed 32-bit
	/// values. The `npairs` must be greater than or equal to `0`. Each of the `npairs` pairs consists of an `int` match and a signed 32-bit offset. Each of
	/// these signed 32-bit values is constructed from four unsigned bytes as `(byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4`.
	///
	/// The table match-offset pairs of the [Opcode::LookupSwitch] instruction must be sorted in increasing numerical order by match.
	///
	/// The `key` must be of type `int` and is popped from the operand stack. The `key` is compared against the match values. If it is equal to one of them,
	/// then a target address is calculated by adding the corresponding offset to the address of the opcode of this [Opcode::LookupSwitch] instruction. If the
	/// `key` does not match any of the match values, the target address is calculated by adding `default` to the address of the opcode of this
	/// [Opcode::LookupSwitch] instruction. Execution then continues at the target address.
	///
	/// The target address that can be calculated from the offset of each match-offset pair, as well as the one calculated from default, must be the address of
	/// an opcode of an instruction within the method that contains this [Opcode::LookupSwitch] instruction.
	///
	/// # Notes
	/// The alignment required of the 4-byte operands of the [Opcode::LookupSwitch] instruction guarantees 4-byte alignment of those operands if and only if
	/// the method that contains the [Opcode::LookupSwitch] is positioned on a 4-byte boundary.
	///
	/// The match-offset pairs are sorted to support lookup routines that are quicker than linear search.
	LookupSwitch {
		default_target: BranchTarget,
		npairs: usize,
		targets: Vec<(i32, BranchTarget)>, // [npairs]
	},
	LOr,
	LRem,
	LReturn,
	LShl,
	LShr,
	LStore(LvIndex),
	LSub,
	LUShr,
	LXor,
	MonitorEnter,
	MonitorExit,
	/// Create new multidimensional array.
	///
	/// # Format
	/// ```txt
	/// MultiANewArray
	/// indexbyte1
	/// indexbyte2
	/// dimensions
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ..., count1: int, [count2: int, ...] ->
	/// ..., arrayref: reference
	/// ```
	///
	/// # Description
	/// The `dimensions` operand is an unsigned byte that must be greater than or equal to 1. Ite represents the number of dimensions of the array to be
	/// created. The operand stack must contain `dimension` values. Each such value represents the number of components in a dimension of the array to be
	/// created and must be non-negative. The `count1` is the desired length in the first dimension, `count2` in the second, etc.
	///
	/// ...; todo: fill in
	MultiANewArray(VerificationType, usize),
	New(VerificationType),
	NewArray { a_type: ArrayType },
	/// Do nothing.
	///
	/// # Operand Stack
	/// No change.
	///
	/// # Description
	/// Do nothing.
	Nop,
	Pop,
	Pop2,
	PutField(FieldRefInfo),
	PutStatic(FieldRefInfo),
	Return,
	SALoad,
	SAStore,
	/// Push `short`
	///
	/// # Format
	/// ```txt
	/// SIPush
	/// byte1
	/// byte2
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ... ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The immediate unsigned `byte1` and `byte2` values are assembled into an intermediate `short` where the value of the `short` is `(byte1 << 8) | byte2`.
	/// The intermediate value is then sign-extended to an `int` value. That value is pushed onto the operand stack.
	SIPush(i16),
	/// Swap the top two operand stack values.
	///
	/// # Operand Stack
	/// ```txt
	/// ..., value2, value1 ->
	/// ..., value1, value2
	/// ```
	///
	/// # Description
	/// This instruction must not be used unless `value1` and `value2` are both values of category 1 computational types.
	///
	/// # Notes
	/// The JVM does not prove an instruction implementing swap for category 2 computational types.
	Swap,
	/// Access jump table by index and jump.
	///
	/// # Format
	/// ```txt
	/// TableSwitch
	/// <0-3 byte pad>
	/// defaultbyte1
	/// defaultbyte2
	/// defaultbyte3
	/// defaultbyte4
	/// lowbyte1
	/// lowbyte2
	/// lowbyte3
	/// lowbyte4
	/// highbyte1
	/// highbyte2
	/// highbyte3
	/// highbyte4
	/// (
	///   byte1
	///   byte2
	///   byte3
	///   byte4
	/// ) [high - low + 1]
	/// ```
	///
	/// # Operand Stack
	/// ```txt
	/// ..., index: int ->
	/// ...
	/// ```
	///
	/// # Description
	/// A [Opcode::TableSwitch] is a variable-length instruction. Immediately after the opcode, between zero and three bytes must act as padding, such that
	/// `defaultbyte1` begins at an address that is a multiple of four bytes from the start of the current method (the opcode of its first instruction).
	/// Immediately after the padding are bytes constituting three signed 32-bit values: `default`, `low`, and `high`. Immediately following are bytes
	/// constituting a series of `high - low + 1` signed 32-bit offsets. The value `low` must be less than or equal to `high`. The `high - low + 1` signed
	/// 32-bit offsets are treated as a 0-based jump table. Each of these signed 32-bit values is constructed as `(byte1 << 24) | (byte2 << 16) | (byte3 << 8)
	/// | byte4`.
	///
	/// The index must be of type int and is popped from the operand stack. If `index` is less than `low` or `index` is greater than `high`, then a target
	/// address is calculated by adding `default` to the address of the opcode of this [Opcode::TableSwitch] instruction. Otherwise, the offset at position
	/// `index - low` of the jump table is extracted. The target address is calculated by adding that offset to the address of the opcode of this
	/// [Opcode::TableSwitch] instruction. Execution then continues at the target address.
	///
	/// The target address that can be calculated from each jump table offset, as well as the one that can be calculated from `default`, must be the address
	/// of an opcode of an instruction within the method that contains this [Opcode::TableSwitch] instruction.
	TableSwitch {
		default_target: BranchTarget,
		low: i32,
		high: i32,
		targets: Vec<BranchTarget>, // [high - low + 1]
	}
}

impl Opcode {
	pub(crate) fn parse(reader: &mut impl CodeReader, pool: &Pool) -> Result<Opcode> {
		match reader.read_u8()? {
			0x32 => Ok(Opcode::AALoad),
			0x53 => Ok(Opcode::AAStore),
			0x01 => Ok(Opcode::AConstNull),
			0x19 => Ok(Opcode::ALoad(LvIndex(reader.read_u8_as_usize()?))),
			0x2a => Ok(Opcode::ALoad(LvIndex(0))),
			0x2b => Ok(Opcode::ALoad(LvIndex(1))),
			0x2c => Ok(Opcode::ALoad(LvIndex(2))),
			0x2d => Ok(Opcode::ALoad(LvIndex(3))),
			0xbd => Ok(Opcode::ANewArray(pool.get(reader.read_u16_as_usize()?)?)),
			0xb0 => Ok(Opcode::AReturn),
			0xbe => Ok(Opcode::ArrayLength),
			0x3a => Ok(Opcode::AStore(LvIndex(reader.read_u8_as_usize()?))),
			0x4b => Ok(Opcode::AStore(LvIndex(0))),
			0x4c => Ok(Opcode::AStore(LvIndex(1))),
			0x4d => Ok(Opcode::AStore(LvIndex(2))),
			0x4e => Ok(Opcode::AStore(LvIndex(3))),
			0xbf => Ok(Opcode::AThrow),
			0x33 => Ok(Opcode::BALoad),
			0x54 => Ok(Opcode::BAStore),
			0x10 => Ok(Opcode::BIPush(reader.read_u8()?)),
			0xca => Ok(Opcode::Breakpoint),
			0x34 => Ok(Opcode::CALoad),
			0x55 => Ok(Opcode::CAStore),
			0xc0 => Ok(Opcode::CheckCast(pool.get(reader.read_u16_as_usize()?)?)),
			0x90 => Ok(Opcode::D2f),
			0x8e => Ok(Opcode::D2i),
			0x8f => Ok(Opcode::D2l),
			0x63 => Ok(Opcode::DAdd),
			0x31 => Ok(Opcode::DALoad),
			0x52 => Ok(Opcode::DAStore),
			0x98 => Ok(Opcode::DCmpG),
			0x97 => Ok(Opcode::DCmpL),
			0x0e => Ok(Opcode::DConst0),
			0x0f => Ok(Opcode::DConst1),
			0x6f => Ok(Opcode::DDiv),
			0x18 => Ok(Opcode::DLoad(LvIndex(reader.read_u8_as_usize()?))),
			0x26 => Ok(Opcode::DLoad(LvIndex(0))),
			0x27 => Ok(Opcode::DLoad(LvIndex(1))),
			0x28 => Ok(Opcode::DLoad(LvIndex(2))),
			0x29 => Ok(Opcode::DLoad(LvIndex(3))),
			0x6b => Ok(Opcode::DMul),
			0x77 => Ok(Opcode::DNeg),
			0x73 => Ok(Opcode::DRem),
			0xaf => Ok(Opcode::DReturn),
			0x39 => Ok(Opcode::DStore(LvIndex(reader.read_u8_as_usize()?))),
			0x47 => Ok(Opcode::DStore(LvIndex(0))),
			0x48 => Ok(Opcode::DStore(LvIndex(1))),
			0x49 => Ok(Opcode::DStore(LvIndex(2))),
			0x4a => Ok(Opcode::DStore(LvIndex(3))),
			0x67 => Ok(Opcode::DSub),
			0x59 => Ok(Opcode::Dup),
			0x5a => Ok(Opcode::DupX1),
			0x5b => Ok(Opcode::DupX2),
			0x5c => Ok(Opcode::Dup2),
			0x5d => Ok(Opcode::Dup2X1),
			0x5e => Ok(Opcode::Dup2X2),
			0x8d => Ok(Opcode::F2d),
			0x8b => Ok(Opcode::F2i),
			0x8c => Ok(Opcode::F2l),
			0x62 => Ok(Opcode::FAdd),
			0x30 => Ok(Opcode::FALoad),
			0x51 => Ok(Opcode::FAStore),
			0x96 => Ok(Opcode::FCmpG),
			0x95 => Ok(Opcode::FCmpL),
			0x0b => Ok(Opcode::FConst0),
			0x0c => Ok(Opcode::FConst1),
			0x0d => Ok(Opcode::FConst2),
			0x6e => Ok(Opcode::FDiv),
			0x17 => Ok(Opcode::FLoad(LvIndex(reader.read_u8_as_usize()?))),
			0x22 => Ok(Opcode::FLoad(LvIndex(0))),
			0x23 => Ok(Opcode::FLoad(LvIndex(1))),
			0x24 => Ok(Opcode::FLoad(LvIndex(2))),
			0x25 => Ok(Opcode::FLoad(LvIndex(3))),
			0x6a => Ok(Opcode::FMul),
			0x76 => Ok(Opcode::FNeg),
			0x72 => Ok(Opcode::FRem),
			0xae => Ok(Opcode::FReturn),
			0x38 => Ok(Opcode::FStore(LvIndex(reader.read_u8_as_usize()?))),
			0x43 => Ok(Opcode::FStore(LvIndex(0))),
			0x44 => Ok(Opcode::FStore(LvIndex(1))),
			0x45 => Ok(Opcode::FStore(LvIndex(2))),
			0x46 => Ok(Opcode::FStore(LvIndex(3))),
			0x66 => Ok(Opcode::FSub),
			0xb4 => Ok(Opcode::GetField(pool.get(reader.read_u16_as_usize()?)?)),
			0xb2 => Ok(Opcode::GetStatic(pool.get(reader.read_u16_as_usize()?)?)),
			0xa7 => Ok(Opcode::Goto(reader.read_i16_branchoffset()?)),
			0xc8 => Ok(Opcode::Goto(reader.read_i32_branchoffset()?)),
			0x91 => Ok(Opcode::I2b),
			0x92 => Ok(Opcode::I2c),
			0x87 => Ok(Opcode::I2d),
			0x86 => Ok(Opcode::I2f),
			0x85 => Ok(Opcode::I2l),
			0x93 => Ok(Opcode::I2s),
			0x60 => Ok(Opcode::IAdd),
			0x2e => Ok(Opcode::IALoad),
			0x7e => Ok(Opcode::IAnd),
			0x4f => Ok(Opcode::IAStore),
			0x02 => Ok(Opcode::IConstM1),
			0x03 => Ok(Opcode::IConst0),
			0x04 => Ok(Opcode::IConst1),
			0x05 => Ok(Opcode::IConst2),
			0x06 => Ok(Opcode::IConst3),
			0x07 => Ok(Opcode::IConst4),
			0x08 => Ok(Opcode::IConst5),
			0x6c => Ok(Opcode::IDiv),
			0xa5 => Ok(Opcode::IfACmpEq(reader.read_i16_branchoffset()?)),
			0xa6 => Ok(Opcode::IfACmpNe(reader.read_i16_branchoffset()?)),
			0x9f => Ok(Opcode::IfICmpEq(reader.read_i16_branchoffset()?)),
			0xa2 => Ok(Opcode::IfICmpGe(reader.read_i16_branchoffset()?)),
			0xa3 => Ok(Opcode::IfICmpGt(reader.read_i16_branchoffset()?)),
			0xa4 => Ok(Opcode::IfICmpLe(reader.read_i16_branchoffset()?)),
			0xa1 => Ok(Opcode::IfICmpLt(reader.read_i16_branchoffset()?)),
			0xa0 => Ok(Opcode::IfICmpNe(reader.read_i16_branchoffset()?)),
			0x99 => Ok(Opcode::IfEq(reader.read_i16_branchoffset()?)),
			0x9c => Ok(Opcode::IfGe(reader.read_i16_branchoffset()?)),
			0x9d => Ok(Opcode::IfGt(reader.read_i16_branchoffset()?)),
			0x9e => Ok(Opcode::IfLe(reader.read_i16_branchoffset()?)),
			0x9b => Ok(Opcode::IfLt(reader.read_i16_branchoffset()?)),
			0x9a => Ok(Opcode::IfNe(reader.read_i16_branchoffset()?)),
			0xc7 => Ok(Opcode::IfNonNull(reader.read_i16_branchoffset()?)),
			0xc6 => Ok(Opcode::IfNull(reader.read_i16_branchoffset()?)),
			0x84 => Ok(Opcode::IInc {
				lv_index: LvIndex(reader.read_u8_as_usize()?),
				const_: reader.read_u8()? as i32,
			}),
			0x15 => Ok(Opcode::ILoad(LvIndex(reader.read_u8_as_usize()?))),
			0x1a => Ok(Opcode::ILoad(LvIndex(0))),
			0x1b => Ok(Opcode::ILoad(LvIndex(1))),
			0x1c => Ok(Opcode::ILoad(LvIndex(2))),
			0x1d => Ok(Opcode::ILoad(LvIndex(3))),
			0xfe => Ok(Opcode::ImpDep1),
			0xff => Ok(Opcode::ImpDep2),
			0x68 => Ok(Opcode::IMul),
			0x74 => Ok(Opcode::INeg),
			0xc1 => Ok(Opcode::InstanceOf(pool.get(reader.read_u16_as_usize()?)?)),
			0xba => Ok(Opcode::InvokeDynamic {
				call_site: pool.get(reader.read_u16_as_usize()?)?,
				zero1: reader.read_u8()?, // == 0
				zero2: reader.read_u8()?, // == 0
			}),
			0xb9 => Ok(Opcode::InvokeInterface {
				method_ref: pool.get(reader.read_u16_as_usize()?)?,
				count: reader.read_u8()?,
				zero: reader.read_u8()?, // == 0
			}),
			0xb7 => Ok(Opcode::InvokeSpecial(pool.get(reader.read_u16_as_usize()?)?)),
			0xb8 => Ok(Opcode::InvokeStatic(pool.get(reader.read_u16_as_usize()?)?)),
			0xb6 => Ok(Opcode::InvokeVirtual(pool.get(reader.read_u16_as_usize()?)?)),
			0x80 => Ok(Opcode::IOr),
			0x70 => Ok(Opcode::IRem),
			0xac => Ok(Opcode::IReturn),
			0x78 => Ok(Opcode::IShl),
			0x7a => Ok(Opcode::IShr),
			0x36 => Ok(Opcode::IStore(LvIndex(reader.read_u8_as_usize()?))),
			0x3b => Ok(Opcode::IStore(LvIndex(0))),
			0x3c => Ok(Opcode::IStore(LvIndex(1))),
			0x3d => Ok(Opcode::IStore(LvIndex(2))),
			0x3e => Ok(Opcode::IStore(LvIndex(3))),
			0x64 => Ok(Opcode::ISub),
			0x7c => Ok(Opcode::IUShr),
			0x82 => Ok(Opcode::IXor),
			0xa8 => bail!("jsr instruction is not legal in class files of version 52.0 or greater"),
			0xc9 => bail!("jsr_w instruction is not legal in class files of version 52.0 or greater"),
			0x8a => Ok(Opcode::L2d),
			0x89 => Ok(Opcode::L2f),
			0x88 => Ok(Opcode::L2i),
			0x61 => Ok(Opcode::LAdd),
			0x2f => Ok(Opcode::LALoad),
			0x7f => Ok(Opcode::LAnd),
			0x50 => Ok(Opcode::LAStore),
			0x94 => Ok(Opcode::LCmp),
			0x09 => Ok(Opcode::LConst0),
			0x0a => Ok(Opcode::LConst1),
			opcode @ (0x12 | 0x13) => {
				let cp_index = match opcode {
					0x12 => reader.read_u8_as_usize()?, // ldc
					0x13 => reader.read_u16_as_usize()?, // ldc_w
					_ => unreachable!(),
				};

				match pool.get::<&PoolEntry>(cp_index)? {
					// TODO: when we have proper int,float,long,double types, we can update this (see also ldc2_w)
					PoolEntry::Integer(_)         => Ok(Opcode::LdcInt                  (pool.get(cp_index)?)),
					PoolEntry::Float(_)           => Ok(Opcode::LdcFloat                (pool.get(cp_index)?)),
					PoolEntry::String(_)          => Ok(Opcode::LdcReferenceString      (pool.get(cp_index)?)),
					PoolEntry::ClassName(_)       => Ok(Opcode::LdcReferenceClass       (pool.get(cp_index)?)),
					PoolEntry::MethodType(_)      => Ok(Opcode::LdcReferenceMethodType  (pool.get(cp_index)?)),
					PoolEntry::MethodHandle(_, _) => Ok(Opcode::LdcReferenceMethodHandle(pool.get(cp_index)?)),
					entry => bail!("ldc/ldc_w can only be used for int/float/String/Class/method type/method handle, got: {entry:?}"),
				}
			},
			0x14 => { // ldc2_w
				let cp_index = reader.read_u16_as_usize()?;
				match pool.get::<&PoolEntry>(cp_index)? {
					PoolEntry::Long {..}   => Ok(Opcode::Ldc2WLong  (pool.get(cp_index)?)),
					PoolEntry::Double {..} => Ok(Opcode::Ldc2WDouble(pool.get(cp_index)?)),
					entry => bail!("ldc2_w can only be used for long/double, got: {entry:?}"),
				}
			},
			0x6d => Ok(Opcode::LDiv),
			0x16 => Ok(Opcode::LLoad(LvIndex(reader.read_u8_as_usize()?))),
			0x1e => Ok(Opcode::LLoad(LvIndex(0))),
			0x1f => Ok(Opcode::LLoad(LvIndex(1))),
			0x20 => Ok(Opcode::LLoad(LvIndex(2))),
			0x21 => Ok(Opcode::LLoad(LvIndex(3))),
			0x69 => Ok(Opcode::LMul),
			0x75 => Ok(Opcode::LNeg),
			0xab => { // LookupSwitch
				reader.move_to_next_4_byte_boundary()?;

				let default_target = reader.read_i32_branchoffset()?;
				let npairs = reader.read_u32_as_usize()?;

				let mut targets = Vec::with_capacity(npairs);
				for _ in 0..npairs {
					let match_ = reader.read_i32()?;
					let branch_target = reader.read_i32_branchoffset()?;

					targets.push((match_, branch_target));
				}

				Ok(Opcode::LookupSwitch { default_target, npairs, targets })
			},
			0x81 => Ok(Opcode::LOr),
			0x71 => Ok(Opcode::LRem),
			0xad => Ok(Opcode::LReturn),
			0x79 => Ok(Opcode::LShl),
			0x7b => Ok(Opcode::LShr),
			0x37 => Ok(Opcode::LStore(LvIndex(reader.read_u8_as_usize()?))),
			0x3f => Ok(Opcode::LStore(LvIndex(0))),
			0x40 => Ok(Opcode::LStore(LvIndex(1))),
			0x41 => Ok(Opcode::LStore(LvIndex(2))),
			0x42 => Ok(Opcode::LStore(LvIndex(3))),
			0x65 => Ok(Opcode::LSub),
			0x7d => Ok(Opcode::LUShr),
			0x83 => Ok(Opcode::LXor),
			0xc2 => Ok(Opcode::MonitorEnter),
			0xc3 => Ok(Opcode::MonitorExit),
			0xc5 => Ok(Opcode::MultiANewArray(
				pool.get(reader.read_u16_as_usize()?)?,
				reader.read_u8_as_usize()? // >= 1
			)),
			0xbb => Ok(Opcode::New(pool.get(reader.read_u16_as_usize()?)?)),
			0xbc => Ok(Opcode::NewArray {
				a_type: ArrayType::parse(reader.read_u8()?)?,
			}),
			0x00 => Ok(Opcode::Nop),
			0x57 => Ok(Opcode::Pop),
			0x58 => Ok(Opcode::Pop2),
			0xb5 => Ok(Opcode::PutField(pool.get(reader.read_u16_as_usize()?)?)),
			0xb3 => Ok(Opcode::PutStatic(pool.get(reader.read_u16_as_usize()?)?)),
			0xa9 => bail!("ret instruction is not legal in class files of version 52.0 or greater"),
			0xb1 => Ok(Opcode::Return),
			0x35 => Ok(Opcode::SALoad),
			0x56 => Ok(Opcode::SAStore),
			0x11 => Ok(Opcode::SIPush(reader.read_i16()?)),
			0x5f => Ok(Opcode::Swap),
			0xaa => { // TableSwitch
				reader.move_to_next_4_byte_boundary()?;

				let default_target = reader.read_i32_branchoffset()?;
				let low = reader.read_i32()?;
				let high = reader.read_i32()?;

				let n = (high - low + 1) as usize; // TODO: could panic?

				let mut targets = Vec::with_capacity(n);
				for _ in 0..n {
					let branch_target = reader.read_i32_branchoffset()?;
					targets.push(branch_target);
				}

				Ok(Opcode::TableSwitch {
					default_target,
					low,
					high,
					targets,
				})
			},
			0xc4 => { // Wide
				match reader.read_u8()? {
					0x19 => Ok(Opcode::ALoad(LvIndex(reader.read_u16_as_usize()?))),
					0x3a => Ok(Opcode::AStore(LvIndex(reader.read_u16_as_usize()?))),
					0x18 => Ok(Opcode::DLoad(LvIndex(reader.read_u16_as_usize()?))),
					0x39 => Ok(Opcode::DStore(LvIndex(reader.read_u16_as_usize()?))),
					0x17 => Ok(Opcode::FLoad(LvIndex(reader.read_u16_as_usize()?))),
					0x38 => Ok(Opcode::FStore(LvIndex(reader.read_u16_as_usize()?))),
					0x15 => Ok(Opcode::ILoad(LvIndex(reader.read_u16_as_usize()?))),
					0x36 => Ok(Opcode::IStore(LvIndex(reader.read_u16_as_usize()?))),
					0x16 => Ok(Opcode::LLoad(LvIndex(reader.read_u16_as_usize()?))),
					0x37 => Ok(Opcode::LStore(LvIndex(reader.read_u16_as_usize()?))),
					0xa0 => bail!("wide ret instruction is not legal in class files of version 52.0 or greater"),
					0x84 => Ok(Opcode::IInc {
						lv_index: LvIndex(reader.read_u16_as_usize()?),
						const_: reader.read_i16()? as i32,
					}),
					opcode => bail!("illegal wide opcode: {opcode:x}"),
				}
			},
			opcode => bail!("illegal opcode {opcode:x}"),
		}
	}
}
