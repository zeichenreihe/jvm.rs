
#[derive(Debug)]
pub enum Error {
	UnknownOpcode ( u8 ),
}

#[warn(missing_docs)]
/// An opcode of the JVM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode { // 6.5
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
	ALoad { index: u8 },
	/// Load `reference` from local variable \<n\>.
	///
	/// # Operand Stack
	/// ```
	/// ... ->
	/// ..., objectref: reference
	/// ```
	///
	/// # Description
	/// The \<n\> must be an index into the local variable array of the current frame. The local variable at \<n\> must contain a `reference`. The `objectref`
	/// in the local variable at \<n\> is pushed onto the operand stack.
	///
	/// # Notes
	/// This instruction cannot be used to load a value of type `returnAddress` from a local variable onto the operand stack. This asymmetry with the
	/// corresponding `AStore` instruction is intentional.
	ALoad0,
	/// See [Opcode::ALoad0].
	ALoad1,
	/// See [Opcode::ALoad0].
	ALoad2,
	/// See [Opcode::ALoad0].
	ALoad3,
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
	ANewArray { indexbyte1: u8, indexbyte2: u8 },
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
	AStore { index: u8 },
	AStore0,
	/// See [Opcode::AStore0].
	AStore1,
	/// See [Opcode::AStore0].
	AStore2,
	/// See [Opcode::AStore0].
	AStore3,
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
	/// `boolean` arrays - that is, arrays of type `T_BOOLEAN` (§2.2, [Opcode::NewArray]) - are implemented as arrays of 8-bit values. Other implementations
	/// may implement packed boolean arrays; the [Opcode::BALoad] instruction of such implementations must be used to access those arrays.
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
	/// `boolean` arrays - that is, arrays of type `T_BOOLEAN` (§2.2, [Opcode::NewArray]) - are implemented as arrays of 8-bit values. Other implementations
	/// may implement packed boolean arrays; in such implementations the [Opcode::BAStore] instruction must be able to store `boolean` values into packed
	/// boolean arrays as well as `byte` values into `byte` arrays.
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
	BIPush {
		byte: u8,
	},
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
	CheckCast,
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
	DLoad { index: u8 },
	DLoad0,
	/// See [Opcode::DLoad0].
	DLoad1,
	/// See [Opcode::DLoad0].
	DLoad2,
	/// See [Opcode::DLoad0].
	DLoad3,
	DMul,
	DNeg,
	DRem,
	DReturn,
	DStore { index: u8 },
	DStore0,
	/// See [Opcode::DStore0].
	DStore1,
	/// See [Opcode::DStore0].
	DStore2,
	/// See [Opcode::DStore0].
	DStore3,
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
	FLoad { index: u8 },
	FLoad0,
	/// See [Opcode::FLoad0].
	FLoad1,
	/// See [Opcode::FLoad0].
	FLoad2,
	/// See [Opcode::FLoad0].
	FLoad3,
	FMul,
	FNeg,
	FRem,
	FReturn,
	FStore { index: u8 },
	FStore0,
	/// See [Opcode::FStore0].
	FStore1,
	/// See [Opcode::FStore0].
	FStore2,
	/// See [Opcode::FStore0].
	FStore3,
	FSub,
	GetField,
	GetStatic,
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
	Goto { branch_target: usize },
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
	GotoW { branch_target: usize },
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
	IfACmpEq, IfACmpNe,
	IfICmpEq, IfICmpGe, IfICmpGt, IfICmpLe, IfICmpLt, IfICmpNe,
	IfEq, IfGe, IfGt, IfLe, IfLt, IfNe,
	IfNonNull, IfNull,
	IInc,
	ILoad { index: u8 },
	ILoad0,
	/// See [Opcode::ILoad0].
	ILoad1,
	/// See [Opcode::ILoad0].
	ILoad2,
	/// See [Opcode::ILoad0].
	ILoad3,
	ImpDep1, ImpDep2,
	IMul,
	INeg,
	InstanceOf,
	InvokeDynamic,
	InvokeInterface,
	InvokeSpecial,
	InvokeStatic,
	InvokeVirtual,
	IOr,
	IRem,
	IReturn,
	IShl,
	IShr,
	IStore { index: u8 },
	IStore0,
	/// See [Opcode::IStore0].
	IStore1,
	/// See [Opcode::IStore0].
	IStore2,
	/// See [Opcode::IStore0].
	IStore3,
	ISub,
	IUShr,
	IXor,
	Jsr,
	JsrW,
	L2d,
	L2f,
	L2i,
	LAdd,
	LALoad,
	LAnd,
	LAStore,
	LCmp,
	LConst0, LConst1,
	Ldc,
	LdcW,
	Ldc2W,
	LDiv,
	LLoad { index: u8 },
	LLoad0,
	/// See [Opcode::LLoad1].
	LLoad1,
	/// See [Opcode::LLoad1].
	LLoad2,
	/// See [Opcode::LLoad1].
	LLoad3,
	LMul,
	LNeg,
	LookupSwitch,
	LOr,
	LRem,
	LReturn,
	LShl,
	LShr,
	LStore { index: u8 },
	LStore0,
	/// See [Opcode::LStore0].
	LStore1,
	/// See [Opcode::LStore0].
	LStore2,
	/// See [Opcode::LStore0].
	LStore3,
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
	MultiANewArray,
	New,
	NewArray,
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
	PutField,
	PutStatic,
	Ret,
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
	SIPush,
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
	/// )*
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
		default_target: usize,
		low: i32,
		high: i32,
		targets: Vec<usize>, // [high - low + 1]
	},
	WideALoad  { indexbyte1: u8, indexbyte2: u8 },
	WideAStore { indexbyte1: u8, indexbyte2: u8 },
	WideDLoad  { indexbyte1: u8, indexbyte2: u8 },
	WideDStore { indexbyte1: u8, indexbyte2: u8 },
	WideFLoad  { indexbyte1: u8, indexbyte2: u8 },
	WideFStore { indexbyte1: u8, indexbyte2: u8 },
	WideILoad  { indexbyte1: u8, indexbyte2: u8 },
	WideIStore { indexbyte1: u8, indexbyte2: u8 },
	WideLLoad  { indexbyte1: u8, indexbyte2: u8 },
	WideLStore { indexbyte1: u8, indexbyte2: u8 },
	WideRet    { indexbyte1: u8, indexbyte2: u8 },
	WideIInc   { indexbyte1: u8, indexbyte2: u8, constbyte1: u8, constbyte2: u8 },
}