use crate::classfile::{ClassInfo, DoubleInfo, FieldRefInfo, FloatInfo, IntegerInfo, InterfaceMethodRefInfo, InvokeDynamicInfo, LongInfo, MethodHandleInfo, MethodRefInfo, MethodTypeInfo, StringInfo};
use crate::classfile::instruction::{BranchTarget, ConditionalBranch, LvIndex, UnconditionalBranch};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrayType {}

#[warn(missing_docs)]
/// An opcode of the JVM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
	/// Load `reference` from array.
	///
	/// # Operand Stack
	/// ```
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
	/// ```
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
	/// ```
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
	/// ```
	/// ALoad
	/// index
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	/// ```
	/// ANewArray
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	ANewArray { class: ClassInfo },
	/// Return `reference` from method.
	///
	/// # Operand Stack
	/// ```
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
	/// ```
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
	/// ```
	/// AStore
	/// index
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	/// ```
	/// ..., objectref: reference ->
	/// objectref: reference
	/// ```
	///
	/// # Description
	/// The `objectref` must be of type reference and must refer to an object that is an instance of class `java.lang.Throwable` or of a subclass of
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
	/// ```
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
	/// ```
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
	/// ```
	/// BIPush
	/// byte
	/// ```
	///
	/// # Operand Stack
	/// ```
	/// ... ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The immediate `byte` is sign-extended to an `int` value. That value is pushed onto the operand stack.
	BIPush(), // {
		//byte: u8,
	//},
	/// Implementation instruction.
	/// This opcode is intended to be used by debuggers to implement breakpoints.
	Breakpoint,
	/// Load `char` from array.
	///
	/// # Operand Stack
	/// ```
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
	/// ```
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
	CheckCast { class: ClassInfo },
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
	GetField { field_ref: FieldRefInfo },
	GetStatic { field_ref: FieldRefInfo },
	/// Branch always.
	///
	/// # Format
	/// ```
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
	/// ```
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
	Goto(UnconditionalBranch),
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
	IfACmpEq(ConditionalBranch),
	IfACmpNe(ConditionalBranch),
	IfICmpEq(ConditionalBranch),
	IfICmpGe(ConditionalBranch),
	IfICmpGt(ConditionalBranch),
	IfICmpLe(ConditionalBranch),
	IfICmpLt(ConditionalBranch),
	IfICmpNe(ConditionalBranch),
	IfEq(ConditionalBranch),
	IfGe(ConditionalBranch),
	IfGt(ConditionalBranch),
	IfLe(ConditionalBranch),
	IfLt(ConditionalBranch),
	IfNe(ConditionalBranch),
	IfNonNull(ConditionalBranch),
	IfNull(ConditionalBranch),
	IInc { lv_index: LvIndex, const_: i32 },
	ILoad(LvIndex),
	ImpDep1, ImpDep2,
	IMul,
	INeg,
	InstanceOf { class: ClassInfo },
	InvokeDynamic { call_site: InvokeDynamicInfo, zero1: u8, zero2: u8 },
	InvokeInterface { method_ref: InterfaceMethodRefInfo, count: u8, zero: u8 },
	InvokeSpecial { method_ref: MethodRefInfo },
	InvokeStatic { method_ref: MethodRefInfo },
	InvokeVirtual { method_ref: MethodRefInfo },
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
	/// ```
	/// ldc
	/// index
	/// ```
	/// or
	/// ```
	/// ldc_w
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	LdcInt { int: IntegerInfo },
	/// See [Opcode::LdcInt].
	LdcFloat { float: FloatInfo },
	/// See [Opcode::LdcInt].
	LdcReferenceString { string: StringInfo },
	/// See [Opcode::LdcInt].
	LdcReferenceClass { class: ClassInfo },
	/// See [Opcode::LdcInt].
	LdcReferenceMethodType { method_type: MethodTypeInfo },
	/// See [Opcode::LdcInt].
	LdcReferenceMethodHandle { method_handle: MethodHandleInfo },
	/// Push long or double from run-time constant pool (wide index).
	///
	/// # Format
	/// ```
	/// ldc2_w
	/// indexbyte1
	/// indexbyte2
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	Ldc2WDouble { double: DoubleInfo },
	/// See [Opcode::Ldc2WDouble].
	Ldc2WLong { long: LongInfo },
	LDiv,
	LLoad(LvIndex),
	LMul,
	LNeg,
	/// Access jump table by key match and jump.
	///
	/// # Format
	/// ```
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
	/// ```
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
		npairs: i32,
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
	/// ```
	/// MultiANewArray
	/// indexbyte1
	/// indexbyte2
	/// dimensions
	/// ```
	///
	/// # Operand Stack
	/// ```
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
	MultiANewArray { class: ClassInfo, dimensions: u8 },
	New { class: ClassInfo },
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
	PutField { field_ref: FieldRefInfo },
	PutStatic { field_ref: FieldRefInfo },
	Return,
	SALoad,
	SAStore,
	/// Push `short`
	///
	/// # Format
	/// ```
	/// SIPush
	/// byte1
	/// byte2
	/// ```
	///
	/// # Operand Stack
	/// ```
	/// ... ->
	/// ..., value: int
	/// ```
	///
	/// # Description
	/// The immediate unsigned `byte1` and `byte2` values are assembled into an intermediate `short` where the value of the `short` is `(byte1 << 8) | byte2`.
	/// The intermediate value is then sign-extended to an `int` value. That value is pushed onto the operand stack.
	SIPush(), // { byte1: u8, byte2: u8 },
	/// Swap the top two operand stack values.
	///
	/// # Operand Stack
	/// ```
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
	/// ```
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
	/// ```
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
