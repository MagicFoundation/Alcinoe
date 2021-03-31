/// SpiderMonkey 45/52 *.h header port to Delphi
// if defined SM52 condition then SpiderMonkey 52 is used
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SpiderMonkey;
{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SyNode for mORMot Copyright (C) 2021 Pavel Mashlyakovsky & Vadim Orel
      pavel.mash at gmail.com

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is
  Pavel Mashlyakovsky & Vadim Orel.
  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel
  - Pavel Mashlyakovsky
  - win2014

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - initial release. Use SpiderMonkey 45
}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER
{$I SyNode.inc}   //define WITHASSERT

interface
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SynCommons,
  SynTable,
  SynLog,
  SysUtils;

type
  JSUnknown = Pointer; //Use this type for developping. In real case comment it and check than you use only known types
{$ifndef UNICODE}
  /// 8 bit signed integer type for C APIs
  int8 = ShortInt;
  /// 8 bit unsigned integer type for C APIs
  uint8 = Byte;

  /// 16 bit signed integer type for C APIs
  int16 = Smallint;
  /// 16 bit unsigned integer type for C APIs
  uint16 = Word;

  /// 32 bit signed integer type for C APIs
  int32 = Integer;
  /// 32 bit unsigned integer type for C APIs
  uint32 = Cardinal;
{$endif}
{$ifndef ISDELPHIXE2}
  uintptr = PtrUInt;
{$endif}
  uintN = PtrUInt;

{.$ifndef FPC}
  /// variable type used to store a buffer size (in bytes) for SMAPI
  size_t = PtrUInt;
{.$endif}
  psize_t = ^size_t;
  CChar = AnsiChar;
  PCChar = PAnsiChar;
  CChar16 = WideChar;
  PCChar16 = PWideChar;
  PPCChar16 = ^PCChar16;

{$Z4}
  JSType = (
    JSTYPE_VOID       = 0, // undefined
    JSTYPE_OBJECT     = 1, // object
    JSTYPE_FUNCTION   = 2, // function
    JSTYPE_STRING     = 3, // string
    JSTYPE_NUMBER     = 4, // number
    JSTYPE_BOOLEAN    = 5, // boolean
    JSTYPE_NULL       = 6, // null
    JSTYPE_SYMBOL     = 7, //symbol
    JSTYPE_LIMIT      = 8
  );

  JSGCParamKey = (
	// Maximum nominal heap before last ditch GC.
	JSGC_MAX_BYTES = 0,
	// Number of JS_malloc bytes before last ditch GC.
	JSGC_MAX_MALLOC_BYTES = 1,
	// Amount of bytes allocated by the GC.
	JSGC_BYTES = 3,
	// Number of times GC has been invoked. Includes both major and minor GC.
	JSGC_NUMBER = 4,
	// Max size of the code cache in bytes.
	JSGC_MAX_CODE_CACHE_BYTES = 5,
	// Select GC mode.
	JSGC_MODE = 6,
	// Number of cached empty GC chunks.
	JSGC_UNUSED_CHUNKS = 7,
	// Total number of allocated GC chunks.
	JSGC_TOTAL_CHUNKS = 8,
	// Max milliseconds to spend in an incremental GC slice.
	JSGC_SLICE_TIME_BUDGET = 9,
	// Maximum size the GC mark stack can grow to.
	JSGC_MARK_STACK_LIMIT = 10,
	// GCs less than this far apart in time will be considered 'high-frequency GCs'.
	// See setGCLastBytes in jsgc.cpp.
	JSGC_HIGH_FREQUENCY_TIME_LIMIT = 11,
	// Start of dynamic heap growth.
	JSGC_HIGH_FREQUENCY_LOW_LIMIT = 12,
	// End of dynamic heap growth.
	JSGC_HIGH_FREQUENCY_HIGH_LIMIT = 13,
	// Upper bound of heap growth.
	JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MAX = 14,
	// Lower bound of heap growth.
	JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MIN = 15,
	// Heap growth for low frequency GCs.
	JSGC_LOW_FREQUENCY_HEAP_GROWTH = 16,
	// If false, the heap growth factor is fixed at 3. If true, it is determined
	// based on whether GCs are high- or low- frequency.
	JSGC_DYNAMIC_HEAP_GROWTH = 17,
	// If true, high-frequency GCs will use a longer mark slice.
	JSGC_DYNAMIC_MARK_SLICE = 18,
	// Lower limit after which we limit the heap growth.
	JSGC_ALLOCATION_THRESHOLD = 19,
{$IFNDEF SM52}
	// We decommit memory lazily. If more than this number of megabytes is
	// available to be decommitted, then JS_MaybeGC will trigger a shrinking GC
	// to decommit it.
	JSGC_DECOMMIT_THRESHOLD = 20,
{$ENDIF}
	// We try to keep at least this many unused chunks in the free chunk pool at
	// all times, even after a shrinking GC.
	JSGC_MIN_EMPTY_CHUNK_COUNT = 21,
	// We never keep more than this many unused chunks in the free chunk pool.
	JSGC_MAX_EMPTY_CHUNK_COUNT = 22,
	// Whether compacting GC is enabled.
	JSGC_COMPACTING_ENABLED = 23
  {$IFDEF SM52}
  // If true, painting can trigger IGC slices.
  ,JSGC_REFRESH_FRAME_SLICES_ENABLED = 24
  {$ENDIF}
  );

  JSGCMode = (
    // Perform only global GCs.
    JSGC_MODE_GLOBAL = 0,
    // Perform per-compartment GCs until too much garbage has accumulated.
    JSGC_MODE_COMPARTMENT = 1,
    // Collect in short time slices rather than all at once. Implies
    // JSGC_MODE_COMPARTMENT.
    JSGC_MODE_INCREMENTAL = 2
  );

  JSVersion = (
  JSVERSION_ECMA_3  = 148,
  /// Run-time version enumeration corresponding to 1.6
  JSVERSION_1_6     = 160,
  /// Run-time version enumeration corresponding to 1.7
  JSVERSION_1_7     = 170,
  /// Run-time version enumeration corresponding to 1.8
  JSVERSION_1_8     = 180,
  /// Run-time version enumeration corresponding to ECMA standard 5, i.e. 1.8.5
  JSVERSION_ECMA_5  = 185,
  /// Run-time version enumeration corresponding to default version
  JSVERSION_DEFAULT = 0,
  /// Run-time version enumeration corresponding to an identified version
  JSVERSION_UNKNOWN = -1
  );

///  This enum is used to select if properties with JSPROP_DEFINE_LATE flag
//  should be defined on the object.
//  Normal JSAPI consumers probably always want DefineAllProperties here.
  JSPropertyDefinitionBehavior = (
  DefineAllProperties,
  OnlyDefineLateProperties,
  DontDefineLateProperties
  );
/// During global creation, we fire notifications to callbacks registered
// via the Debugger API. These callbacks are arbitrary script, and can touch
// the global in arbitrary ways. When that happens, the global should not be
// in a half-baked state. But this creates a problem for consumers that need
// to set slots on the global to put it in a consistent state.
// - This API provides a way for consumers to set slots atomically (immediately
// after the global is created), before any debugger hooks are fired. It's
// unfortunately on the clunky side, but that's the way the cookie crumbles.
// - If callers have no additional state on the global to set up, they may pass
// |FireOnNewGlobalHook| to JS_NewGlobalObject, which causes that function to
// fire the hook as its final act before returning. Otherwise, callers should
// pass |DontFireOnNewGlobalHook|, which means that they are responsible for
// invoking JS_FireOnNewGlobalObject upon successfully creating the global. If
// an error occurs and the operation aborts, callers should skip firing the
// hook. But otherwise, callers must take care to fire the hook exactly once
// before compiling any script in the global's scope (we have assertions in
// place to enforce this). This lets us be sure that debugger clients never miss
// breakpoints.
  OnNewGlobalHookOption = (
    FireOnNewGlobalHook,
    DontFireOnNewGlobalHook
  );
/// Dense index into cached prototypes and class atoms for standard objects.
{$IFDEF SM52}
  JSProtoKey = (
  JSProto_Null = 0,
  JSProto_Object,
  JSProto_Function,
  JSProto_Array,
  JSProto_Boolean,
  JSProto_JSON,
  JSProto_Date,
  JSProto_Math,
  JSProto_Number,
  JSProto_String,
  JSProto_RegExp,
  JSProto_Error,
  JSProto_InternalError,
  JSProto_EvalError,
  JSProto_RangeError,
  JSProto_ReferenceError,
  JSProto_SyntaxError,
  JSProto_TypeError,
  JSProto_URIError,
  JSProto_DebuggeeWouldRun,
  JSProto_CompileError,
  JSProto_RuntimeError,
  JSProto_Iterator,
  JSProto_StopIteration,
  JSProto_ArrayBuffer,
  JSProto_Int8Array,
  JSProto_Uint8Array,
  JSProto_Int16Array,
  JSProto_Uint16Array,
  JSProto_Int32Array,
  JSProto_Uint32Array,
  JSProto_Float32Array,
  JSProto_Float64Array,
  JSProto_Uint8ClampedArray,
  JSProto_Proxy,
  JSProto_WeakMap,
  JSProto_Map,
  JSProto_Set,
  JSProto_DataView,
  JSProto_Symbol,
  JSProto_SharedArrayBuffer,
  JSProto_Intl,
  JSProto_TypedObject,
  JSProto_Reflect,
  JSProto_SIMD,
  JSProto_WeakSet,
  JSProto_TypedArray,
  JSProto_Atomics,
  JSProto_SavedFrame,
  JSProto_WebAssembly,
  JSProto_WasmModule,
  JSProto_WasmInstance,
  JSProto_WasmMemory,
  JSProto_WasmTable,
  JSProto_Promise,
  JSProto_LIMIT
  );
{$ELSE}
  JSProtoKey = (
  JSProto_Null = 0,
  JSProto_Object,
  JSProto_Function,
  JSProto_Array,
  JSProto_Boolean,
  JSProto_JSON,
  JSProto_Date,
  JSProto_Math,
  JSProto_Number,
  JSProto_String,
  JSProto_RegExp,
  JSProto_Error,
  JSProto_InternalError,
  JSProto_EvalError,
  JSProto_RangeError,
  JSProto_ReferenceError,
  JSProto_SyntaxError,
  JSProto_TypeError,
  JSProto_URIError,
  JSProto_Iterator,
  JSProto_StopIteration,
  JSProto_ArrayBuffer,
  JSProto_Int8Array,
  JSProto_Uint8Array,
  JSProto_Int16Array,
  JSProto_Uint16Array,
  JSProto_Int32Array,
  JSProto_Uint32Array,
  JSProto_Float32Array,
  JSProto_Float64Array,
  JSProto_Uint8ClampedArray,
  JSProto_Proxy,
  JSProto_WeakMap,
  JSProto_Map,
  JSProto_Set,
  JSProto_DataView,
  JSProto_Symbol,
  JSProto_SharedArrayBuffer,
  JSProto_Intl,
  JSProto_TypedObject,
  JSProto_Reflect,
  JSProto_SIMD,
  JSProto_WeakSet,
  JSProto_TypedArray,
  JSProto_Atomics,
  JSProto_SavedFrame,
  JSProto_LIMIT
  );
{$ENDIF}
{$Z1}
/// Type of JSValue
  JSValueType = (
    JSVAL_TYPE_DOUBLE   = $00,
    JSVAL_TYPE_INT32    = $01,
    JSVAL_TYPE_UNDEFINED= $02,
    JSVAL_TYPE_BOOLEAN  = $03,
    JSVAL_TYPE_MAGIC    = $04,
    JSVAL_TYPE_STRING   = $05,
    JSVAL_TYPE_SYMBOL   = $06,
    {$IFDEF SM52}
    JSVAL_TYPE_PRIVATE_GCTHING = $07,
    JSVAL_TYPE_NULL     = $08,
    JSVAL_TYPE_OBJECT   = $0C,
    {$ELSE}
    JSVAL_TYPE_NULL     = $07,
    JSVAL_TYPE_OBJECT   = $08,
    {$ENDIF}
    // These never appear in a jsval; they are only provided as an out-of-band value.
    JSVAL_TYPE_UNKNOWN  = $20,
    JSVAL_TYPE_MISSING  = $21
  );

{$ifndef CPU64}
/// first 4 bytes for JSValue
{$MINENUMSIZE 4}
{$WARN COMBINING_SIGNED_UNSIGNED OFF}
{$WARN BOUNDS_ERROR OFF}
  JSValueTag = (
  JSVAL_TAG_CLEAR      = Cardinal($FFFFFF80),
  JSVAL_TAG_INT32      = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_INT32)),
  JSVAL_TAG_UNDEFINED  = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_UNDEFINED)),
  JSVAL_TAG_STRING     = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_STRING)),
  JSVAL_TAG_SYMBOL     = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_SYMBOL)),
  JSVAL_TAG_BOOLEAN    = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_BOOLEAN)),
  JSVAL_TAG_MAGIC      = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_MAGIC)),
  JSVAL_TAG_NULL       = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_NULL)),
  JSVAL_TAG_OBJECT     = Cardinal(JSVAL_TAG_CLEAR or UInt8(JSVAL_TYPE_OBJECT))
  );
{$WARN BOUNDS_ERROR ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$MINENUMSIZE 1}
{$endif}


{$Z2}
  /// Possible exception types
  // -These types are part of a JSErrorFormatString structure
  // - They define which error to throw in case of a runtime error
  // - JSEXN_NONE marks an unthrowable error
  JSExnType = (
    JSEXN_NONE = -1,
    JSEXN_ERR,
    JSEXN_INTERNALERR,
    JSEXN_EVALERR,
    JSEXN_RANGEERR,
    JSEXN_REFERENCEERR,
    JSEXN_SYNTAXERR,
    JSEXN_TYPEERR,
    JSEXN_URIERR,
    {$IFDEF SM52}
    JSEXN_DEBUGGEEWOULDRUN,
    JSEXN_WASMCOMPILEERROR,
    JSEXN_WASMRUNTIMEERROR,
    JSEXN_WARN,
    {$ENDIF}
    JSEXN_LIMIT
  );
{$Z1}


  Tint8Vector = array[0..(MaxInt div sizeof(int8))-1] of int8;
  Pint8Vector = ^Tint8Vector;
  Tuint8Vector = array[0..(MaxInt div sizeof(uint8))-1] of uint8;
  Puint8Vector = ^Tuint8Vector;
  Tint16Vector = array[0..(MaxInt div sizeof(int16))-1] of int16;
  Pint16Vector = ^Tint16Vector;
  Tuint16Vector = array[0..(MaxInt div sizeof(uint16))-1] of uint16;
  Puint16Vector = ^Tuint16Vector;
  Tint32Vector = array[0..(MaxInt div sizeof(int32))-1] of int32;
  Pint32Vector = ^Tint32Vector;
  Tuint32Vector = array[0..(MaxInt div sizeof(uint32))-1] of uint32;
  Puint32Vector = ^Tuint32Vector;
  Tfloat32Vector = array[0..(MaxInt div sizeof(single))-1] of single;
  Pfloat32Vector = ^Tfloat32Vector;
  Tfloat64Vector = array[0..(MaxInt div sizeof(double))-1] of double;
  Pfloat64Vector = ^Tfloat64Vector;

  /// the available types of elements in a typed array or data view
  // - obj must have passed a JS_IsArrayBufferView/JS_Is*Array test, or somehow
  // be known that it would pass such a test: it is an ArrayBufferView or a
  // wrapper of an ArrayBufferView, and the unwrapping will succeed.
  /// - jsabTYPE_UINT8_CLAMPED is a special type that is a uint8_t, but assignments
  // are clamped to [0,255]: treat the raw data type as a uint8_t.
  // - jsabTYPE_DATAVIEW is the type returned for a DataView. Note that
  // there is no single element type in this case
  JSArrayBufferViewType = (
    jsabTYPE_INT8 = 0,
    jsabTYPE_UINT8,
    jsabTYPE_INT16,
    jsabTYPE_UINT16,
    jsabTYPE_INT32,
    jsabTYPE_UINT32,
    jsabTYPE_FLOAT32,
    jsabTYPE_FLOAT64,
    jsabTYPE_UINT8_CLAMPED,
    jsabTYPE_MAXTYPEDARRAYVIEWTYPE,
    jsabTYPE_FLOAT32x4,
    jsabTYPE_INT32x4

);
const
  nullPtr: pointer = nil;
  /// Run-time version enumeration corresponding to the latest available
  // - that is, ECMA standard 5, i.e. 1.8.5
  JSVERSION_LATEST  = JSVERSION_ECMA_5;
type
// pointers
{$IFDEF SM52}
  JSFreeOp = pointer;
  PJSContextOptions = ^JSContextOptions;
{$ELSE}
  PJSRuntime = ^JSRuntime;
  PJSRuntimeOptions = ^JSRuntimeOptions;
{$ENDIF}
  PJSContext = ^JSContext;
  PJSCompartment = ^JSCompartment;
  PJS_CompartmentOptions = ^JS_CompartmentOptions;
  PJSObject = ^JSObject;
  PJSFunction = PJSObject;
  PJSString = ^JSString;
  PJSClass = ^JSClass;
  PJSCompileOptions = ^JSCompileOptions;
  PJSScript = JSUnknown;
  PJSRootedValue = ^JSRootedValue;
  PJSRootedObject = ^JSRootedObject;
  PJSRootedString = ^JSRootedString;
  PJSPropertySpec = ^JSPropertySpec;
  PJSFunctionSpec = ^JSFunctionSpec;
  PJSErrorReport = ^JSErrorReport;
  PJSErrorFormatString = ^JSErrorFormatString;
  PJSAtomState = JSUnknown;
  PJSPrincipals = JSUnknown;
  PJSAutoCheckCannotGC = JSUnknown;
  PJSStringFinalizer = ^JSStringFinalizer;

// jsid
  JSIdType = (
    JSID_TYPE_STRING = $0,
    JSID_TYPE_INT    = $1,
    JSID_TYPE_VOID   = $2,
    JSID_TYPE_SYMBOL = $4
  );
  jsid = record
    asBits: size_t;
    function isString: Boolean;
    function asJSString: PJSString;
  end;

  TjsidVector = array[0..(MaxInt div sizeof(jsid))-2] of jsid;
  PjsidVector = ^TjsidVector;

  _JSIdArray = record
    cx: PJSContext;
    mBegin: PjsidVector;    //* actually, length jsid words */
    mLength: size_t;
    mCapacity: size_t;
    mStorage: Int64;
  end;
/// internal Spidermonkey structure for storrage jsid
  {$ifdef USERECORDWITHMETHODS}JSIdArray  = record
    {$else}JSIdArray  = object{$endif}
  private
    _internal: _JSIdArray;
    procedure init(cx: PJSContext); {$ifdef HASINLINE}inline;{$endif}
  public
    property Length: size_t read _internal.mLength;
    property vector: PjsidVector read _internal.mBegin;
  end;

// jsvalue
  JSWhyMagic = Cardinal;
  jsval_payload = record
    case Byte of
      0: (i32: int32);
      1: (u32: uint32);
{$IFNDEF CPU64}
      2: (boo: uint32);   // Don't use |bool| -- it must be four bytes.
      3: (str: PJSString);
      4: (obj: PJSObject);
      5: (ptr: pointer);
{$ENDIF}
      6: (why: JSWhyMagic);
{$IFNDEF CPU64}
      7: (word: size_t);
      8: (uintptr: PtrUInt)
{$ENDIF}
  end;
{$ifdef IS_LITTLE_ENDIAN}
  jsval_val_layout = packed record
    payload: jsval_payload;
{$IFNDEF CPU64}
    tag: JSValueTag;
{$ENDIF}
  end;
{$else} //BIG_ENDIAN
  jsval_val_layout = record
    tag: JSValueTag;
    payload: jsval_payload;
  end;
{$endif}

  /// low-level definition of the jsval internals
  // - do not use directly
  jsval_layout = record
    case Byte of
      0: (asBits: QWord);
{$IFNDEF CPU64}
      1: (s: jsval_val_layout);
{$ENDIF}
      2: (asDouble: double);
      3: (asPtr: Pointer);
  end;

  /// used by JS_Stringify() method to incremently write the JSON content
  JSONWriteCallback = function(const buf: PCChar16; len: uint32; data: pointer): Boolean; cdecl;

  /// high-level definition of the JSValue
  {$ifdef USERECORDWITHMETHODS}jsval = record
    {$else}jsval = object{$endif}
  private
    _l: jsval_layout;

    function getIsVoid: Boolean;
    function getIsNull: Boolean;

    function getIsInteger: Boolean;
    function getAsInteger: Integer;
    procedure setAsInteger(const Value: Integer);

    function getAsInt64: Int64;
    procedure setAsInt64(const Value: Int64);

    function getIsDouble: Boolean;
    function getAsDouble: Double;
    procedure setAsDouble(const Value: Double);

    function getIsNumber: Boolean;

    function getIsBoolean: Boolean;
    function getAsBoolean: Boolean;
    procedure setAsBoolean(const Value: Boolean);

    function getIsObject: Boolean;
    function getAsObject: PJSObject;
    procedure setAsObject(const Value: PJSObject);

    function getIsString: Boolean;
    function getJSString: PJSString;
    procedure setJSString(const Value: PJSString);

    function getIsSimpleVariant(cx: PJSContext): Boolean;
    function getSimpleVariant(cx: PJSContext): Variant;
    procedure setSimpleVariant(cx: PJSContext; const Value: Variant);

    function getPrivate: Pointer;
    procedure setPrivate(const Value: Pointer);

    function getAsDate(cx: PJSContext): TDateTime;
    procedure setAsDate(cx: PJSContext; const Value: TDateTime);

    function getAsJson(cx: PJSContext): RawUTF8;
    procedure setAsJson(cx: PJSContext; const Value: RawUTF8);

    function IsPrimitive: Boolean;
    function IsMagic: Boolean;

  public
    /// Is vaule void
    property isVoid: Boolean read getIsVoid;
    /// Set vaule void
    procedure setVoid;

    /// Is vaule null
    property isNull: Boolean read getIsNull;
    /// Set vaule null
    procedure setNull;

    /// Is vaule 32bit integer
    property isInteger: Boolean read getIsInteger;
    /// Get/set vaule as 32bit integer
    property asInteger: Integer read getAsInteger write setAsInteger;

    /// Get/set vaule as 64bit integer (if more than 32 bits than as double)
    property asInt64: Int64 read getAsInt64 write setAsInt64;

    /// Is vaule double
    property isDouble: Boolean read getIsDouble;
    /// Get/set vaule as double
    property asDouble: Double read getAsDouble write setAsDouble;

    /// Is vaule Number (32bit integer or double)
    property isNumber: Boolean read getIsNumber;

    /// Is vaule boolean
    property isBoolean: Boolean read getIsBoolean;
    /// Get/set vaule as boolean
    property asBoolean: Boolean read getAsBoolean write setAsBoolean;

    /// Is vaule object(null is object too)
    property isObject: Boolean read getIsObject;
    /// Get/set vaule as object
    property asObject: PJSObject read getAsObject write setAsObject;

    /// Is vaule string
    property isString: Boolean read getIsString;
    /// Get/set vaule as JSString
    property asJSString: PJSString read getJSString write setJSString;

    /// Is vaule simple(void, null, boolean, number or string)
    property isSimpleVariant[cx: PJSContext]: Boolean read getIsSimpleVariant;
    /// Get/set vaule as simple
    property asSimpleVariant[cx: PJSContext]: Variant read getSimpleVariant write setSimpleVariant;

    /// Get/set vaule as Custom pointer
    property asPrivate: Pointer read getPrivate write setPrivate;

    /// Get/set vaule as DateTime(object Date used)
    property asDate[cx: PJSContext]: TDateTime read getAsDate write setAsDate;

    /// Add JSON representation of value to Writer
    procedure AddJSON(cx: PJSContext; W: TTextWriter);
    /// Get/set vaule as JSON representation
    property asJson[cx: PJSContext]: RawUTF8 read getAsJson write setAsJson;

    /// Get JSON representation of value and launch callback
    function Stringify(cx: PJSContext; var replacer: PJSObject; space: jsval;
      callback: JSONWriteCallback; data: pointer): Boolean;

    /// Get type of value
    function ValType(cx: PJSContext): JSType;

    /// Get source of value
    function toSource(cx: PJSContext): PJSString;

  end;

  /// an abstract array of jsval JavaScript values
  TjsvalVector = array[0..(MaxInt div sizeof(jsval))-3] of jsval;
  /// map an array of jsval JavaScript values
  PjsvalVector = ^TjsvalVector;
  TjsvalDynArray = array of jsval;

  /// low-level definition of arguments of function
  // - do not use it dirrectly
  _JSArgRec = record
    case boolean of
      true: (
        calle: jsval;
        this: jsval;
        argv: jsval;
      );
      false: (rval: jsval);
  end;

  /// hight-level definition of arguments of function
  {$ifdef USERECORDWITHMETHODS}JSArgRec = record
    {$else}JSArgRec = object{$endif}
  private
    rec: _JSArgRec;
    function GetIsConstructing: Boolean; {$ifdef HASINLINE}inline;{$endif}
    function getThis(cx: PJSContext): jsval; {$ifdef HASINLINE}inline;{$endif}
    function getThisObject(cx: PJSContext): PJSObject;
    function getArgv: PjsvalVector; {$ifdef HASINLINE}inline;{$endif}
    function getCalleObject: PJSObject; {$ifdef HASINLINE}inline;{$endif}
  public
  /// is function called as constructor
    property IsConstructing: Boolean read GetIsConstructing;
  /// function, that is called now
    property calle: jsval read rec.calle;
    property calleObject: PJSObject read getCalleObject;
  /// this value of function
    property this[cx: PJSContext]: jsval read getThis;
    property thisObject[cx: PJSContext]: PJSObject read getThisObject;
  /// arguments of function
    property argv: PjsvalVector read getArgv;
  /// return value of function
    property rval: jsval write rec.rval;
  end;

//Todo - set correct codes for some sitautions
  JS_ObjectOpResult = object
    code: UIntPtr;
  end;

// callback
  JSInterruptCallback = function(cx: PJSContext): Boolean; cdecl;
  /// callback prototype for returning an execution error
  JSErrorCallback = function(userRef: Pointer; const errorNumber: uintN): PJSErrorFormatString; cdecl;
  /// callback prototype for reporting error for a given runtime context
{$IFDEF SM52}
  JSWarningReporter = procedure(cx: PJSContext; report: PJSErrorReport); cdecl;
{$ELSE}
  JSErrorReporter = procedure(cx: PJSContext; _message: PCChar; report: PJSErrorReport); cdecl;
{$ENDIF}
  JSStringFinalizerOp = procedure(fin: PJSStringFinalizer; chars: PCChar16);  cdecl;

// Add or get a property named by id in obj.  Note the jsid id type -- id may
// be a string (Unicode property identifier) or an int (element index).  The
// *vp out parameter, on success, is the new property value after the action.
  JSAddPropertyOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
    out vp: jsval): Boolean; cdecl;
// Delete a property named by id in obj.
// - If an error occurred, return false as per normal JSAPI error practice.
// - If no error occurred, but the deletion attempt wasn't allowed (perhaps
// because the property was non-configurable), set *succeeded to false and
// return true.  This will cause |delete obj[id]| to evaluate to false in
// non-strict mode code, and to throw a TypeError in strict mode code.
// - If no error occurred and the deletion wasn't disallowed (this is *not* the
// same as saying that a deletion actually occurred -- deleting a non-existent
// property, or an inherited property, is allowed -- it's just pointless),
// set *succeeded to true and return true.
  JSDeletePropertyOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
    out res: JS_ObjectOpResult):Boolean; cdecl;
// Get a property named by id in obj.  Note the jsid id type -- id may
// be a string (Unicode property identifier) or an int (element index).  The
// *vp out parameter, on success, is the new property value after the action.
  JSGetterOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid; out vp: jsval):Boolean; cdecl;
// Set a property named by id in obj, treating the assignment as strict
// mode code if strict is true. Note the jsid id type -- id may be a string
// (Unicode property identifier) or an int (element index). The *vp out
// parameter, on success, is the new property value after the
// set.
  JSSetterOp  = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
    var vp: jsval; out res: JS_ObjectOpResult):Boolean; cdecl;
// The old-style JSClass.enumerate op should define all lazy properties not
// yet reflected in obj.
  JSEnumerateOp = function(cx: PJSContext; var obj: PJSObject): Boolean; cdecl;
// Resolve a lazy property named by id in obj by defining it directly in obj.
// Lazy properties are those reflected from some peer native property space
// (e.g., the DOM attributes for a given node reflected as obj) on demand.
// - JS looks for a property in an object, and if not found, tries to resolve
// the given id. *resolvedp should be set to true iff the property was
// was defined on |obj|.
  JSResolveOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid; out resolved: Boolean): Boolean; cdecl;
// A class with a resolve hook can optionally have a mayResolve hook. This hook
// must have no side effects and must return true for a given id if the resolve
// hook may resolve this id. This is useful when we're doing a "pure" lookup: if
// mayResolve returns false, we know we don't have to call the effectful resolve
// hook.
//
// maybeObj, if non-null, is the object on which we're doing the lookup. This
// can be nullptr: during JIT compilation we sometimes know the Class but not
// the object.
  JSMayResolveOp = function(names: PJSAtomState; id: jsid; maybeObj: PJSObject): Boolean; cdecl;
{$IFDEF SM52}
  JSFinalizeOp = procedure(var fop: JSFreeOp; obj: PJSObject); cdecl;
{$ELSE}
  JSFinalizeOp = procedure(var rt: PJSRuntime; obj: PJSObject); cdecl;
{$ENDIF}
// Check whether v is an instance of obj.  Return false on error or exception,
// true on success with true in *bp if v is an instance of obj, false in
// *bp otherwise.
  JSHasInstanceOp = function(cx: PJSContext; var obj: PJSObject; var vp: jsval; out b: Boolean): Boolean; cdecl;
// Typedef for native functions called by the JS VM.
  JSNative = function(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
// Function type for trace operation of the class called to enumerate all
// traceable things reachable from obj's private data structure. For each such
// thing, a trace implementation must call one of the JS_Call*Tracer variants
// on the thing.
//
// JSTraceOp implementation can assume that no other threads mutates object
// state. It must not change state of the object or corresponding native
// structures. The only exception for this rule is the case when the embedding
// needs a tight integration with GC. In that case the embedding can check if
// the traversal is a part of the marking phase through calling
// JS_IsGCMarkingTracer and apply a special code like emptying caches or
// marking its native structures.
  JSTraceOp = JSUnknown;

{$IFNDEF SM52}
  /// JavaScript execution runtime
  // - this object does not store anything, but just provide some helper methods
  JSRuntime = object
  private
    function GetPrivate: Pointer;
    procedure SetPrivate(const Value: Pointer);
    function GetOptions: PJSRuntimeOptions;
    function GetGCParameter(key: JSGCParamKey): uint32;
    procedure SetGCParameter(key: JSGCParamKey; const Value: uint32);
    function GetErrorReporter: JSErrorReporter;
    procedure SetErrorReporter(er: JSErrorReporter);
    function GetInterruptCallback: JSInterruptCallback;
    procedure SetInterruptCallback(callback: JSInterruptCallback);
    function GetEmptyString: PJSString; {$ifdef HASINLINE}inline;{$endif}
    function GetNowMs: int64; {$ifdef HASINLINE}inline;{$endif}

  public
    /// Initializes the JavaScript runtime.
    class function New(maxbytes: uint32; maxNurseryBytes: uint32; parentRuntime: PJSRuntime): PJSRuntime;
    /// Frees a JavaScript runtime.
    procedure Destroy;
    /// Wrote access a JSRuntime field for application-specific data.
    // Memory management for this private data is the application's responsibility.
    // The JavaScript engine itself never uses it.
    procedure SetNativeStackQuota(systemCodeStackSize: size_t);
    /// Performs garbage collection in the JS memory pool.
    procedure GC;
    /// Request a callback set using JS_SetInterruptCallback
    procedure RequestInterruptCallback;
    /// Create a new JSContext
    function NewContext(stackChunkSize: size_t): PJSContext;
    /// Read/write access a JSRuntime field for application-specific data.
    // Memory management for this private data is the application's responsibility.
    // The JavaScript engine itself never uses it.
    property PrivateData: Pointer read GetPrivate write SetPrivate;
    /// Get options of runtime
    property Options: PJSRuntimeOptions read GetOptions;
    /// Get/Set performance parameters related to garbage collection.
    property GCParameter[key: JSGCParamKey]: uint32 read GetGCParameter write SetGCParameter;
    /// Adjust performance parameters related to garbage collection based on available memory(in megabytes).
    procedure SetGCParametersBasedOnAvailableMemory(availMem: uint32);
    /// Get/Set the error reporting mechanism for an application.
    property ErrorReporter: JSErrorReporter read GetErrorReporter write SetErrorReporter;
    /// Get/Set a callback function that is automatically called periodically while JavaScript code runs.
    property InterruptCallback:JSInterruptCallback read GetInterruptCallback write SetInterruptCallback;

    /// Microseconds since the epoch, midnight, January 1, 1970 UTC.
    property NowMs: int64 read GetNowMs;
    /// Returns the empty string as a JSString object.
    property EmptyString: PJSString read GetEmptyString;
  end;

/// Options of runtime
  JSRuntimeOptions = object
  private
    function getOptions(const Index: Integer): Boolean;
    procedure setOptions(const Index: Integer; const Value: Boolean);
  public
    property Baseline: Boolean index 0 read getOptions write setOptions;
    property Ion: Boolean index 1 read getOptions write setOptions;
    property AsmJS: Boolean index 2 read getOptions write setOptions;
    property ThrowOnAsmJSValidationFailure: Boolean index 3 read getOptions write setOptions;
    property NativeRegExp: Boolean index 4 read getOptions write setOptions;
    property UnboxedArrays: Boolean index 5 read getOptions write setOptions;
    property AsyncStack: Boolean index 6 read getOptions write setOptions;
    property Werror: Boolean index 7 read getOptions write setOptions;
    property StrictMode: Boolean index 8 read getOptions write setOptions;
    property ExtraWarnings: Boolean index 9 read getOptions write setOptions;
  end;
{$ENDIF}
{$IFDEF SM52}
/// Options of context
  JSContextOptions = object
  private
    function getOptions(const Index: Integer): Boolean;
    procedure setOptions(const Index: Integer; const Value: Boolean);
  public
    property Baseline: Boolean index 0 read getOptions write setOptions;
    property Ion: Boolean index 1 read getOptions write setOptions;
    property AsmJS: Boolean index 2 read getOptions write setOptions;
    property Wasm: Boolean index 3 read getOptions write setOptions;
    property WasmAlwaysBaseline: Boolean index 4 read getOptions write setOptions;
    property ThrowOnAsmJSValidationFailure: Boolean index 5 read getOptions write setOptions;
    property NativeRegExp: Boolean index 6 read getOptions write setOptions;
    property UnboxedArrays: Boolean index 7 read getOptions write setOptions;
    property AsyncStack: Boolean index 8 read getOptions write setOptions;
    property ThrowOnDebuggeeWouldRun: Boolean index 9 read getOptions write setOptions;
    property Werror: Boolean index 10 read getOptions write setOptions;
    property StrictMode: Boolean index 11 read getOptions write setOptions;
    property ExtraWarnings: Boolean index 12 read getOptions write setOptions;
  end;
{$ENDIF}
  /// JavaScript execution context
  // - this object does not store anything, but just provide some helper methods
  JSContext = object
  private
    function GetPrivate: Pointer; {$ifdef HASINLINE}inline;{$endif}
    procedure SetPrivate(const Value: Pointer);
{$IFDEF SM52}
    function GetEmptyString: PJSString; {$ifdef HASINLINE}inline;{$endif}
    function GetGCParameter(key: JSGCParamKey): uint32; {$ifdef HASINLINE}inline;{$endif}
    procedure SetGCParameter(key: JSGCParamKey; const Value: uint32); {$ifdef HASINLINE}inline;{$endif}
    function GetNowMs: int64; {$ifdef HASINLINE}inline;{$endif}
    function GetWarningReporter: JSWarningReporter; {$ifdef HASINLINE}inline;{$endif}
    procedure SetWarningReporter(reporter: JSWarningReporter); {$ifdef HASINLINE}inline;{$endif}
    function GetOptions: PJSContextOptions; {$ifdef HASINLINE}inline;{$endif}
{$ELSE}
    function GetRuntime: PJSRuntime; {$ifdef HASINLINE}inline;{$endif}
{$ENDIF}
    function GetIsRunning: boolean; {$ifdef HASINLINE}inline;{$endif}
  protected
    // Return the ArrayBuffer underlying an ArrayBufferView
    // - If the buffer has been neutered, this will still return the neutered buffer.
    // - obj must be an object that would return true for JS_IsArrayBufferViewObject()
    function GetArrayBufferViewBuffer(var obj: PJSObject; out isSharedMemory: Boolean): PJSObject; overload;{$ifdef HASINLINE}inline;{$endif}
    function GetArrayBufferViewBuffer(var obj: PJSObject): PJSObject; overload;{$ifdef HASINLINE}inline;{$endif}
  public
{$IFDEF SM52}
    /// Initializes the JavaScript context.
    class function CreateNew(maxbytes: uint32; maxNurseryBytes: uint32 = 16 * (1 SHL 20); parentContext: PJSContext = nil): PJSContext;
    /// Performs garbage collection in the JS memory pool.
    procedure GC; {$ifdef HASINLINE}inline;{$endif}
    /// Returns the empty string as a JSString object.
    property EmptyString: PJSString read GetEmptyString;
    /// Get/Set performance parameters related to garbage collection.
    property GCParameter[key: JSGCParamKey]: uint32 read GetGCParameter write SetGCParameter;
    /// Adjust performance parameters related to garbage collection based on available memory(in megabytes).
    procedure SetGCParametersBasedOnAvailableMemory(availMem: uint32);
    /// Microseconds since the epoch, midnight, January 1, 1970 UTC.
    property NowMs: int64 read GetNowMs;
    /// Request a callback set using JS_SetInterruptCallback
    procedure RequestInterruptCallback; {$ifdef HASINLINE}inline;{$endif}
    /// Set the size of the native stack that should not be exceed. To disable
    // stack size checking pass 0.
    // - SpiderMonkey allows for a distinction between system code (such as GCs, which
    // may incidentally be triggered by script but are not strictly performed on
    // behalf of such script), trusted script (as determined by JS_SetTrustedPrincipals),
    // and untrusted script. Each kind of code may have a different stack quota,
    // allowing embedders to keep higher-priority machinery running in the face of
    // scripted stack exhaustion by something else.
    // - The stack quotas for each kind of code should be monotonically descending,
    // and may be specified with this function. If 0 is passed for a given kind
    // of code, it defaults to the value of the next-highest-priority kind.
    // - This function may only be called immediately after the runtime is initialized
    // and before any code is executed and/or interrupts requested.
    procedure SetNativeStackQuota(systemCodeStackSize: size_t); {$ifdef HASINLINE}inline;{$endif}
    /// Get options of context
    property Options: PJSContextOptions read GetOptions;
    /// Get/Set the warning reporting mechanism for an application.
    property WarningReporter: JSWarningReporter read GetWarningReporter write SetWarningReporter;
    /// Add callback for interrupt
    procedure AddInterruptCallback(callback: JSInterruptCallback); {$ifdef HASINLINE}inline;{$endif}
    /// Disable interrupt callbacks call
    procedure DisableInterruptCallback; {$ifdef HASINLINE}inline;{$endif}
    /// Disable/enable interrupt callbacks call
    procedure ResetInterruptCallback(disable: boolean); {$ifdef HASINLINE}inline;{$endif}
    /// Call interrupt callback if it is requested
    function CheckForInterrupt: Boolean; {$ifdef HASINLINE}inline;{$endif}
{$ELSE}
    /// Runtime of this context
    property rt: PJSRuntime read GetRuntime;
{$ENDIF}
    /// Read/Write access a JSContext field for application-specific data.
    // Memory management for this private data is the application's responsibility.
    // The JavaScript engine itself never uses it.
    property PrivateData: Pointer read GetPrivate write SetPrivate;
    /// Enter a different compartment on the given context, so that objects in that
    // compartment can be accessed.
    // - NB: This API is infallible; a NULL return value does not indicate error
    function EnterCompartment(target: PJSObject): PJSCompartment;
    /// Leave a the compartment, returning to the compartment active before the
    // corresponding JS_EnterCompartment.
    procedure LeaveCompartment(oldCompartment: PJSCompartment);
    /// indicates to the JS engine that the calling thread is entering a region
    // of code that may call into the JSAPI but does not block
    procedure BeginRequest; {$ifdef HASINLINE}inline;{$endif}
    /// indicates to the JS engine that the calling thread is leaving a region
    // of code that may call into the JSAPI but does not block
    procedure EndRequest; {$ifdef HASINLINE}inline;{$endif}
    /// Create Compile Options
    function NewCompileOptions: PJSCompileOptions;
    /// Free Compile Options
    procedure FreeCompileOptions(opt: PJSCompileOptions);

    /// Create a new JavaScript object for use as a global object.
    function NewGlobalObject(clasp: PJSClass): PJSObject;
    /// Initialize standard JS class constructors, prototypes, and any top-level
    // functions and constants associated with the standard classes (e.g. isNaN
    // for Number).
    // - NB: This sets cx's global object to obj if it was null.
    function InitStandardClasses(var obj: PJSObject): boolean;
    /// Add 'Reflect.parse', a SpiderMonkey extension, to the Reflect object on the
    // given global.
    function InitReflectParse(var obj: PJSObject): boolean;
    /// Initialize the 'ctypes' object on a global variable 'obj'. The 'ctypes'
    // object will be sealed.
    function InitCTypesClass(var obj: PJSObject): boolean;
    /// Initialize the 'Debugger' object on a global variable 'obj'. The 'ctypes'
    // object will be sealed.
    function DefineDebuggerObject(var obj: PJSObject): boolean;
    /// This function makes a cross-compartment wrapper for the given JS object.
    // Details see here http://stackoverflow.com/questions/18730477/what-does-js-wrapobject-do
    function WrapObject(var obj: PJSObject): boolean;

    ///// modules support

    /// Initialize modeles classes next 2 functions cannot work without calling this function
    function InitModuleClasses(var obj: PJSObject): boolean;
    /// Compile script as module
    function CompileModule(var obj: PJSObject; opts: PJSCompileOptions;
       chars: PCChar16; length: size_t): PJSObject;
    /// Set handler for module resolving
    procedure SetModuleResolveHook(var hook: PJSFunction);


    /// Invoke a constructor, like the JS expression `new ctor(...args)`. Returns
    // the new object, or null on error.
    function New(var ctor: PJSObject; argc: uintN; argv: PjsvalVector): PJSObject;
    /// Create a new object based on a specified class and root it
    function NewObject(clasp: PJSClass): PJSObject;
    /// Create a new object based on a specified class
    // - Unlike JS_NewObject, JS_NewObjectWithGivenProto does not compute a default
    // proto. If proto is nullptr, the JS object will have `null` as [[Prototype]].
    function NewObjectWithGivenProto(clasp: PJSClass; var proto: PJSObject): PJSObject;

    /// Create/Free rooted value(put value in rooting stack)
    // Garbage collection not performs to rooted values
    // - Warning!!! You must free rooted values in revers order for creating
    // that's why it's a bad idea store rooting values in any place except
    // local variables. Exception is root some values just after creating context
    // and unroot them (in reverse order) just before destroy context.
    // For other cases use reserved slots
    function NewRootedValue(val: jsval): PJSRootedValue; {$ifdef HASINLINE}inline;{$endif}
    procedure FreeRootedValue(str: PJSRootedValue);

    /// Create/Free rooted object(put object in rooting stack)
    // Garbage collection not performs to rooted objects
    // - Warning!!! You must free rooted objects in reverse order for creating
    // that's why it's a bad idea store rooting objects in any place except
    // local variables. Exception is root some objects just after creating context
    // and unroot them (in reverse order) just before destroy context.
    // For other cases use reserved slots
    function NewRootedObject(obj: PJSObject): PJSRootedObject; {$ifdef HASINLINE}inline;{$endif}
    procedure FreeRootedObject(obj: PJSRootedObject);

    /// Create/Free rooted string(put string in rooting stack)
    // Garbage collection not performs to rooted strings
    // - Warning!!! You must free rooted strings in reverse order for creating
    // that's why it's a bad idea store rooting strings in any place except
    // local variables. Exception is root some strings just after creating context
    // and unroot them (in reverse order) just before destroy context.
    // For other cases use reserved slots
    function NewRootedString(obj: PJSString): PJSRootedString; {$ifdef HASINLINE}inline;{$endif}
    procedure FreeRootedString(str: PJSRootedString);

    /// create a new JavaScript string instance
    function NewJSString(const Value: SynUnicode): PJSString; overload; {$ifdef HASINLINE}inline;{$endif}
    function NewJSString(const Value: RawUTF8): PJSString; overload; {$ifdef HASINLINE}inline;{$endif}
    function NewJSString(TextWide: PWideChar; TextLen: integer): PJSString; overload; {$ifdef HASINLINE}inline;{$endif}
    function NewJSString(TextAnsi: PAnsiChar; TextLen, CodePage: integer): PJSString; overload;
    function NewExternalString(const Value: SynUnicode): PJSString; {$ifdef HASINLINE}inline;{$endif}

    /// create a new JavaScript Date object instance
    function NewDateObject(year, mon, mday, hour, min, sec: int32): PJSObject; {$ifdef HASINLINE}inline;{$endif}
    function NewDateObjectMsec(msec: double): PJSObject; {$ifdef HASINLINE}inline;{$endif}

    /// create a new JavaScript Array object instance
    function NewArrayObject(length: size_t): PJSObject; overload; {$ifdef HASINLINE}inline;{$endif}
    function NewArrayObject(length: size_t; vector: PjsvalVector): PJSObject; overload; {$ifdef HASINLINE}inline;{$endif}

    /// create a new JavaScript Function object instance
    function NewFunction(call: JSNative; nargs: uintN; flags: uintN; name: PCChar): PJSObject; {$ifdef HASINLINE}inline;{$endif}

    /// Reports a memory allocation error
    // - Call JS_ReportOutOfMemory to report that an operation failed because the
    // system is out of memory
    // - When the JavaScript engine tries to allocate memory and allocation fails,
    // it reports an error as though by calling this function
    procedure ReportOutOfMemory;
    /// Create a new JavaScript Error object and set it to be the pending exception on cx.
    // The callback must then return JS_FALSE to cause the exception to be propagated
    // to the calling script.
    procedure ReportError(format: PCChar);
    /// Report an error with an application-defined error code.
    // - varargs is Additional arguments for the error message.
    //- These arguments must be of type jschar*
    // - The number of additional arguments required depends on the error
    // message, which is determined by the errorCallback
    procedure ReportErrorNumberUC(errorCallback: JSErrorCallback; userRef: pointer; const erroNubmer: uintN);
    /// Offer the JavaScript engine an opportunity to perform garbage collection if needed.
    procedure MaybeGC;

    /// Get the current pending exception for a given JSContext.
    function GetPendingException(out rv: jsval): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// Clear the currently pending exception in a context.
    procedure ClearPendingException; {$ifdef HASINLINE}inline;{$endif}

    /// Convert a jsid to type JS::Value.
    function IdToValue(id: jsid; out v: jsval): Boolean; //~~~ write delphi realization
    /// Convert a JS::Value to type jsid.
    function ValueToId(var v: jsval; out id: jsid): Boolean; //~~~ write delphi realization

    /// Determines the JS data type of a JS value.
    function TypeOfValue(v: jsval): JSType; //~~~ write delphi realization

    /// Compile and execute a script in the scope of the current global of cx.
    function EvaluateScript(opts: PJSCompileOptions;
       bytes: PCChar; length: size_t;
       out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function EvaluateUCScript(opts: PJSCompileOptions;
       chars: PCChar16; length: size_t;
       out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Compile a script, source, for execution.
    function CompileScript(bytes: PCChar; length: size_t; opts: PJSCompileOptions;
      out script: PJSScript): boolean; {$ifdef HASINLINE}inline;{$endif}
    function CompileUCScript(chars: PCChar16; length: size_t; opts: PJSCompileOptions;
      out script: PJSScript): boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Evaluate a script in the scope of the current global of cx.
    function ExecuteScript(var script: PJSScript; out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Return the global object for the active function on the context.
    // The global object is specific for a compartment
    function CurrentGlobalOrNull: PJSObject; {$ifdef HASINLINE}inline;{$endif}

    //ArrayBuffer support

    /// Create a new signed 8 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewInt8Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new unsigned 8 bit integer (byte) typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewUint8Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewUint8ClampedArray(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new signed 16 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewInt16Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new unsigned 16 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewUint16Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new signed 32 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewInt32Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new unsigned 32 bit integer typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewUint32Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new signed 32 bit float (single) typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewFloat32Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new signed 64 bit float (double) typed array with nelements elements
    // - will fill the newly created array with zeros
    function NewFloat64Array(nelements: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit signed integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewInt8ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit unsigned integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewUint8ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit unsigned integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewUint8ClampedArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 16 bit signed integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewInt16ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 16 bit unsigned integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewUint16ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit signed integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewInt32ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit unsigned integer typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewUint32ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit float (single) typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewFloat32ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 64 bit float (double) typed array and copy in values
    // from a given object
    // - The object is used as if it was an array; that is, the new array (if
    // successfully created) will have length given by array.length, and its
    // elements will be those specified by array[0], array[1], and so on, after
    // conversion to the typed array element type.
    function NewFloat64ArrayFromArray(var arr: PJSObject): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit signed integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewInt8ArrayWithBuffer(var arrayBuffer: PJSObject;
      byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit unsigned integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewUint8ArrayWithBuffer(var arrayBuffer: PJSObject;
      byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 8 bit unsigned integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewUint8ClampedArrayWithBuffer(var arrayBuffer: PJSObject;
      byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 16 bit signed integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewInt16ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 16 bit unsigned integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewUint16ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit signed integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewInt32ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit unsigned integer typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewUint32ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 32 bit float (single) typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewFloat32ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new 64 bit float (double) typed array using the given
    // ArrayBuffer for storage
    // - The length value is optional; if -1 is passed, enough elements to use up the
    // remainder of the byte array is used as the default value
    function NewFloat64ArrayWithBuffer(var arrayBuffer: PJSObject;
     byteOffset: uint32; length: int32): PJSObject; {$ifdef HASINLINE}inline;{$endif}

    /// Create a new SharedArrayBuffer with the given byte length.
    function NewSharedArrayBuffer(nbytes: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Create a new ArrayBuffer with the given byte length.
    function NewArrayBuffer(nbytes: uint32): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Indicates whether or not a script or function is currently executing in a given context.
    property IsRunning: boolean read GetIsRunning;
    /// Destroy a JSContext.
    procedure Destroy;
  end;

  /// JavaScript execution compartment
  // - this object does not store anything, but just provide some helper methods
  JSCompartment = object
  end;
  /// options of compartment
  JS_CompartmentOptions = record
    version: JSVersion;
    invisibleToDebugger: Boolean;
    mergeable: Boolean;
    discardSource: Boolean;
    disableLazyParsing_: Boolean;
    cloneSingletons: Boolean;
    extraWarningsOverride: JSUnknown;
    zone_: JSUnknown;
    traceGlobal: JSTraceOp;
    singletonsAsTemplates: Boolean;
    addonId: JSUnknown;
    preserveJitCode: Boolean;
  end;

  /// JSObject is the type of JavaScript objects in the JSAPI
  // - this object does not store anything, but just provide some helper methods
  // to access a PJSObject value via low-level API functions
  {$ifdef USERECORDWITHMETHODS}JSObject = record
    {$else}JSObject = object{$endif}
  private
    function GetPrivate: Pointer; {$ifdef HASINLINE}inline;{$endif}
    procedure SetPrivate(data: Pointer); cdecl; {$ifdef HASINLINE}inline;{$endif}
    function GetReservedSlot(index: uint32): jsval; {$ifdef HASINLINE}inline;{$endif}
    procedure SetReservedSlot(index: uint32; v: jsval); {$ifdef HASINLINE}inline;{$endif}
    function GetClass: PJSClass; {$ifdef HASINLINE}inline;{$endif}
    function GetConstructor(cx: PJSContext): PJSObject; {$ifdef HASINLINE}inline;{$endif}
    // Return the available byte length of an array buffer
    // - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
    // that it would pass such a test: it is an ArrayBuffer or a wrapper of an
    // ArrayBuffer, and the unwrapping will succeed
    function GetArrayBufferByteLength: uint32;{$ifdef HASINLINE}inline;{$endif}
    function GetSharedArrayBufferByteLength: uint32;{$ifdef HASINLINE}inline;{$endif}
    // Return a pointer to the start of the data referenced by any typed array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    // - Prefer the type-specific versions when possible
    function GetArrayBufferViewData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pointer;{$ifdef HASINLINE}inline;{$endif}
  public
    /// get a jsval corresponding to this object
    function ToJSValue: jsval; {$ifdef HASINLINE}inline;{$endif}

    /// Access the private data field of an object.
    property PrivateData: Pointer read GetPrivate write SetPrivate;

    property Ctor[cx: PJSContext]: PJSObject read GetConstructor;
    /// Read access an object's reserved slots.
    property ReservedSlot[index: uint32]: jsval read GetReservedSlot write SetReservedSlot;
    /// Retrieves the class associated with an object.
    property Class_: PJSClass read GetClass;

    /// JSAPI method equivalent to the instanceof operator in JavaScript.
    function HasInstance(cx: PJSContext; var val: jsval): Boolean;

    /// is object is Date object
    function isDate(cx: PJSContext): Boolean; {$ifdef HASINLINE}inline;{$endif}
    /// is object is Function object
    function isFunction(cx: PJSContext): Boolean; {$ifdef HASINLINE}inline;{$endif}
    /// is object is Array object
    function isArray(cx: PJSContext): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Retrieve the private data associated with an object, if that object is an
    // instance of a specified class.
    function GetInstancePrivate(cx: PJSContext; clasp: PJSClass): Pointer; {$ifdef HASINLINE}inline;{$endif}

    /// Create a new property on an object.
    function DefineProperty(cx: PJSContext; const name: PCChar;
        const value: jsval; attrs: uint32 = 0;
        getter: JSNative = nil; setter: JSNative = nil): boolean; {$ifdef HASINLINE}inline;{$endif}
    function DefineUCProperty(cx: PJSContext; const name: SynUnicode;
        const value: jsval; attrs: uint32 = 0;
        getter: JSNative = nil; setter: JSNative = nil): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    function DefineUCProperty(cx: PJSContext; const name: PCChar16; namelen: size_t;
        const value: jsval; attrs: uint32 = 0;
        getter: JSNative = nil; setter: JSNative = nil): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    function DefinePropertyById(cx: PJSContext; var id: jsid;
        const value: jsval; attrs: uint32 = 0;
        getter: JSNative = nil; setter: JSNative = nil): boolean; {$ifdef HASINLINE}inline;{$endif}
    /// Define multiple properties for a single object.
    // PJSPropertySpec must be null-terminated
    function DefineProperties(cx: PJSContext; ps: PJSPropertySpec): boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Find a specified property and retrieve its value.
    function GetPropValue(cx: PJSContext; const name: SynUnicode): jsval;
    function GetProperty(cx: PJSContext; const name: PCChar; out vp: jsval): boolean; {$ifdef HASINLINE}inline;{$endif}
    function GetUCProperty(cx: PJSContext; const name: PCChar16; namelen: size_t; out vp: jsval): boolean; {$ifdef HASINLINE}inline;{$endif}
    function GetPropertyById(cx: PJSContext; const id: jsid; out vp: jsval): boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Assign a value to a property of an object.
    function SetProperty(cx: PJSContext; const name: PCChar; const vp: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function SetUCProperty(cx: PJSContext; const name: PCChar16; namelen: size_t; const vp: jsval): boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Removes a specified property from an object.
    function DeletePropertyById(cx: PJSContext; const id: jsid;
      out res: JS_ObjectOpResult): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Determine whether a JavaScript object has a specified property.
    function HasProperty(cx: PJSContext; const name: PCChar): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function HasUCProperty(cx: PJSContext; const name: PCChar16; namelen: size_t; out found: Boolean): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Determine whether a property is already physically present on a JSObject.
    function AlreadyHasOwnUCProperty(cx: PJSContext; const name: PCChar16; namelen: size_t): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Create a native function and assign it as a property to a specified JS object
    function DefineFunction(cx: PJSContext; name: PCChar;
        call: JSNative; nargs: uintN; attrs: uintN = 0): PJSFunction; {$ifdef HASINLINE}inline;{$endif}
    function DefineUCFunction(cx: PJSContext; name: PCChar16;
        namelen: size_t; call: JSNative; nargs: uintN; attrs: uintN = 0): PJSFunction; {$ifdef HASINLINE}inline;{$endif}

    /// Create zero or more functions and makes them properties (methods)
    // of a specified object, obj, as if by calling JS_DefineFunction repeatedly
    function DefineFunctions(cx: PJSContext; fs: PJSFunctionSpec;
        behavior: JSPropertyDefinitionBehavior = DefineAllProperties): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Calls a specified JS function.
    // - Perform the method call `rval = obj[name](args)`.
    function RunMethod(cx: PJSContext; const name: PCChar;
      args: TjsvalDynArray; out rval: jsval): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    function RunMethod(cx: PJSContext; const name: PCChar;
      arg: jsval; out rval: jsval): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    function RunMethod(cx: PJSContext; const name: PCChar;
      out rval: jsval): Boolean; overload; {$ifdef HASINLINE}inline;{$endif}
    function CallFunction(cx: PJSContext; var fun: PJSFunction;
      argc: size_t; argv: PjsvalVector; out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function CallFunctionValue(cx: PJSContext; val: jsval;
      argc: size_t; argv: PjsvalVector; out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function CallFunctionName(cx: PJSContext; const name: PCChar;
      argc: size_t; argv: PjsvalVector; out rval: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Make a JSClass accessible to JavaScript code by creating its prototype,
    // constructor, properties, and functions.
    function InitClass(cx: PJSContext; var parent_proto: PJSObject;
      clasp: PJSClass; _constructor: JSNative; nargs: Cardinal;
      ps: PJSPropertySpec; fs: PJSFunctionSpec;
      static_ps: PJSPropertySpec; static_fs: PJSFunctionSpec): PJSObject; {$ifdef HASINLINE}inline;{$endif}

    /// Get the prototype of obj, storing it in result.
    // - Implements: ES6 [[GetPrototypeOf]] internal method.
    function GetPrototype(cx: PJSContext; out protop: PJSObject): Boolean; {$ifdef HASINLINE}inline;{$endif}
    /// Change the prototype of obj.
    // - Implements: ES6 [[SetPrototypeOf]] internal method.
    // - In cases where ES6 [[SetPrototypeOf]] returns false without an exception,
    // JS_SetPrototype throws a TypeError and returns false.
    // - Performance warning: JS_SetPrototype is very bad for performance. It may
    // cause compiled jit-code to be invalidated. It also causes not only obj but
    // all other objects in the same "group" as obj to be permanently deoptimized.
    // It's better to create the object with the right prototype from the start.
    function SetPrototype(cx: PJSContext; var proto: PJSObject): Boolean; {$ifdef HASINLINE}inline;{$endif}

    /// Get an array of the non-symbol enumerable properties of obj.
    // This function is roughly equivalent to:
    //
    //     var result = [];
    //     for (key in obj)
    //         result.push(key);
    //     return result;
    //
    // This is the closest thing we currently have to the ES6 [[Enumerate]]
    // internal method.
    function Enumerate(cx: PJSContext): JSIdArray;

    //array methods
    function GetArrayLength(cx: PJSContext; var length: uint32): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function GetElement(cx: PJSContext; index: uint32; out vp: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function SetElement(cx: PJSContext; index: uint32; const vp: jsval): Boolean; {$ifdef HASINLINE}inline;{$endif}
    function DeleteElement(cx: PJSContext; index: uint32; out res: JS_ObjectOpResult): Boolean; {$ifdef HASINLINE}inline;{$endif}

    //function methods
    function GetFunctionId: PJSString; {$ifdef HASINLINE}inline;{$endif}
    property FunctionId: PJSString read GetFunctionId;
    function DecompileFunction(cx: PJSContext; PrettyPrint: Boolean = true): PJSString;

    //arrayBuffer methods

    /// Check whether obj supports JS_GetTypedArray* APIs
    // - Note that this may return false if a security wrapper is encountered that
    // denies the unwrapping.
    // - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
    // the dedicated accessor JSAPI calls
    function IsTypedArrayObject: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Check whether obj supports JS_GetArrayBufferView* APIs
    // - Note that this may return false if a security wrapper is encountered that
    // denies the unwrapping.
    // - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
    // the dedicated ArrayBufferView accessor JSAPI calls
    function IsArrayBufferViewObject: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 8 bit signed integer typed array types (ArrayBufferView subtypes)
    function IsInt8Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
    function IsUint8Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
    function IsUint8ClampedArray: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 16 bit signed integer typed array types (ArrayBufferView subtypes)
    function IsInt16Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 16 bit unsigned integer typed array types (ArrayBufferView subtypes)
    function IsUint16Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 32 bit signed integer typed array types (ArrayBufferView subtypes)
    function IsInt32Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 32 bit unsigned integer typed array types (ArrayBufferView subtypes)
    function IsUint32Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 32 bit float (single) typed array types (ArrayBufferView subtypes)
    function IsFloat32Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Test for specific 64 bit float (double) typed array types (ArrayBufferView subtypes)
    function IsFloat64Array: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Return the isShared flag of a typed array, which denotes whether
    // the underlying buffer is a SharedArrayBuffer.
    //
    // |obj| must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
    // be known that it would pass such a test: it is a typed array or a wrapper of
    // a typed array, and the unwrapping will succeed.
    function GetTypedArraySharedness: Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 8 bit signed integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsInt8Array(out length: uint32; out isSharedMemory:Boolean; out Data: Pint8Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 8 bit unsigned integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsUint8Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 8 bit unsigned integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsUint8ClampedArray(out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 16 bit signed integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsInt16Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Pint16Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 16 bit unsigned integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsUint16Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Puint16Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 32 bit signed integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsInt32Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Pint32Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 32 bit unsigned integer typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsUint32Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Puint32Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 32 bit float (single) typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsFloat32Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Pfloat32Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap 64 bit float (double) typed array into direct memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsFloat64Array(out length: uint32; out isSharedMemory:Boolean; out  Data: Pfloat64Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap an object as its raw binary memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsArrayBufferView(out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Unwrap an object as its raw binary memory buffer
    // - Return nil without throwing any exception if the object cannot be viewed as the
    // correct typed array, or the typed array object on success, filling both out parameters
    function GetObjectAsArrayBuffer(out length: uint32; out Data: Puint8Vector): PJSObject;{$ifdef HASINLINE}inline;{$endif}

    /// Get the type of elements in a typed array, or jsabTYPE_DATAVIEW if a DataView
    function GetArrayBufferViewType: JSArrayBufferViewType;{$ifdef HASINLINE}inline;{$endif}

//    function GetSharedArrayBufferViewType: JSArrayBufferViewType;{$ifdef HASINLINE}inline;{$endif}

    /// Check whether obj supports the JS_GetArrayBuffer* APIs
    // - Note that this may return false if a security wrapper is encountered that denies the
    // unwrapping
    // - If this test succeeds, then it is safe to call the various accessor JSAPI calls
    function IsArrayBufferObject: Boolean;{$ifdef HASINLINE}inline;{$endif}

    function IsSharedArrayBufferObject: Boolean;

    /// Return true if the arrayBuffer contains any data. This will return false for
    // ArrayBuffer.prototype and neutered ArrayBuffers.
    //
    // |obj| must have passed a JS_IsArrayBufferObject test, or somehow be known
    // that it would pass such a test: it is an ArrayBuffer or a wrapper of an
    // ArrayBuffer, and the unwrapping will succeed.
    function ArrayBufferHasData: Boolean;

    /// Return a pointer to an array buffer's data
    // - The buffer is still owned by the array buffer object, and should not
    // be modified on another thread. The returned pointer is stable across GCs
    // - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
    // that it would pass such a test: it is an ArrayBuffer or a wrapper of an
    // ArrayBuffer, and the unwrapping will succeed.
    function GetArrayBufferData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector; overload;{$ifdef HASINLINE}inline;{$endif}
    function GetArrayBufferData: Puint8Vector; overload;{$ifdef HASINLINE}inline;{$endif}

    /// Check whether the obj is ArrayBufferObject and memory mapped. Note that this
    // may return false if a security wrapper is encountered that denies the
    // unwrapping.
    function IsMappedArrayBufferObject(obj: PJSObject): Boolean;{$ifdef HASINLINE}inline;{$endif}

    /// Return the number of elements in a typed array
    // - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
    // be known that it would pass such a test: it is a typed array or a wrapper of
    // a typed array, and the unwrapping will succeed.
    function GetTypedArrayLength: uint32;{$ifdef HASINLINE}inline;{$endif}

    /// Return the byte offset from the start of an array buffer to the start of a
    // typed array view
    // - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
    // be known that it would pass such a test: it is a typed array or a wrapper of
    // a typed array, and the unwrapping will succeed.
    function GetTypedArrayByteOffset: uint32;

    /// Return the byte length of a typed array
    // - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
    // be known that it would pass such a test: it is a typed array or a wrapper of
    // a typed array, and the unwrapping will succeed
    function GetTypedArrayByteLength: uint32;{$ifdef HASINLINE}inline;{$endif}

    /// More generic name for JS_GetTypedArrayByteLength to cover DataViews as well
    function GetArrayBufferViewByteLength: uint32;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 8 bit signed integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetInt8ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint8Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetUint8ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector; overload;{$ifdef HASINLINE}inline;{$endif}
    function GetUInt8ArrayData: Puint8Vector; overload;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetUint8ClampedArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 16 bit signed integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetInt16ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint16Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 16 bit unsigned integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetUint16ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint16Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 32 bit signed integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetInt32ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint32Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 32 bit unsigned integer array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetUint32ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint32Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 32 bit float (single) array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetFloat32ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pfloat32Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by a typed 64 bit float (double) array
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - obj must have passed a JS_Is*Array test, or somehow be known that it would
    // pass such a test: it is a typed array or a wrapper of a typed array, and the
    // unwrapping will succeed
    function GetFloat64ArrayData(out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pfloat64Vector;{$ifdef HASINLINE}inline;{$endif}

    /// Return a pointer to the start of the data referenced by any typed array
    // and it's length. For ArrayBufferView return a pointer and length of slice.
    // - The data is still owned by the typed array, and should not be modified on
    // another thread
    // - If JSObject is not a typed array or arrayBufferView return false
    function GetBufferDataAndLength(out data: Puint8Vector; out len: uint32): boolean;{$ifdef HASINLINE}inline;{$endif}
  end;


  JSString = object
    /// get the UTF-8 text corresponding to this string, for a given
    // runtime execution context
    function ToUTF8(cx: PJSContext): RawUTF8; overload;
    /// get the UTF-8 text corresponding to this string, for a given
    // runtime execution context
    // - slightly faster overloaded method (avoid string assignment)
    procedure ToUTF8(cx: PJSContext; var result: RawUTF8); overload;
    /// Add UTF-8 text corresponding to this string to writer,
    // without escaping
    procedure ToUTF8(cx: PJSContext; W: TTextWriter); overload;

    /// get the Ansi text corresponding to this string
    // if source string containt non-ascii chars = return ''
    function ToAnsi(cx: PJSContext): AnsiString;

    /// get the UTF-16 text corresponding to this string, for a given
    // runtime execution context
    function ToSynUnicode(cx: PJSContext): SynUnicode;
    function ToString(cx: PJSContext): string;
    procedure ToVariant(cx: PJSContext; var Value: Variant);
    procedure ToJSONString(cx: PJSContext; W: TTextWriter);
    /// get a jsval corresponding to this string
    function ToJSVal: jsval;

    function HasLatin1Chars: Boolean;
    function Length: size_t;

    function GetLatin1StringCharsAndLength(cx: PJSContext; out len: size_t):PCChar;
    function GetTwoByteStringCharsAndLength(cx: PJSContext; out len: size_t):PCChar16;
  end;
{$IFDEF SM52}
  JSClassOps = record
    addProperty:        JSAddPropertyOp;
    delProperty:        JSDeletePropertyOp;
    getProperty:        JSGetterOp;
    setProperty:        JSSetterOp;
    enumerate:          JSEnumerateOp;
    resolve:            JSResolveOp;
    mayResolve:         JSMayResolveOp;
    finalize:           JSFinalizeOp;
    call:               JSNative;
    hasInstance:        JSHasInstanceOp;
    construct:          JSNative;
    trace:              JSTraceOp;
  end;
  PJSClassOps = ^JSClassOps;
{$ENDIF}
  JSClass =  record
    name:               PCChar;
    flags:              uint32;
{$IFDEF SM52}
    cOps:               PJSClassOps;
    reserved:           array [0..2] of pointer;
{$ELSE}
    addProperty:        JSAddPropertyOp;
    delProperty:        JSDeletePropertyOp;
    getProperty:        JSGetterOp;
    setProperty:        JSSetterOp;
    enumerate:          JSEnumerateOp;
    resolve:            JSResolveOp;
    mayResolve:         JSMayResolveOp;
    finalize:           JSFinalizeOp;
    call:               JSNative;
    hasInstance:        JSHasInstanceOp;
    construct:          JSNative;
    trace:              JSTraceOp;
    reserved:           array [0..22] of pointer;
{$ENDIF}
  end;

  JSCompileOptions = record
    reserved: array[0..1] of Pointer;
    filename: PCChar;
    reserved1: array[3..24] of Pointer;
  end;

  JSRootedValue = record
    Stack: JSUnknown;
    prev: JSUnknown;
    ptr: jsval;
  end;
  JSRootedObjectStack = record
    Last: PJSRootedObject;
  end;
  PJSRootedObjectStack = ^JSRootedObjectStack;
  JSRootedObject = record
    Stack: PJSRootedObjectStack;
    prev: PJSRootedObject;
    ptr: PJSObject;
  end;

  JSRootedString = record
    Stack: JSUnknown;
    prev: JSUnknown;
    ptr: PJSString;
  end;

  PJSJitInfo = JSUnknown;
  JSNativeWrapper = record
    op: JSNative;
    info: PJSJitInfo
  end;
  SelfHostedWrapper = record
    unused: Pointer;
    funname: PCChar;
  end;
  JSPropertySpecGetSetRec = record
    case Boolean of
    true: (native: JSNativeWrapper);
    false: (selfHosted: SelfHostedWrapper);
  end;
  JSPropertySpec = record
    name: PCChar;
    flags: uint8;
    getter: JSPropertySpecGetSetRec;
    setter: JSPropertySpecGetSetRec;
  end;
  TJSPropertySpecDynArray = array of JSPropertySpec;

  JSFunctionSpec = record
    name: PCChar;
    call: JSNativeWrapper;
    nargs: uint16;
    flags: uint16;
    selfHostedName: PCChar;
  end;
  TJSFunctionSpecArray = array of JSFunctionSpec;

  /// internal structure used to report JavaScript errors
  JSErrorReport = record
{$IFDEF SM52}
    /// The (default) error message.
    // If ownsMessage is true, the it is freed in destructor.
    message_: PUTF8Char;
    /// offending source line without final #13
    // If ownsLinebuf is true, the buffer is freed in destructor.
    linebuf: PCChar16;
    /// number of chars in linebuf_. Does not include trailing '\0'
    linebufLength: size_t;
    /// The 0-based offset of error token in linebuf_.
    tokenOffset: size_t;
    /// source file name, URL, etc., or null
    filename: PCChar;
    /// source line number
    lineno: uint32;
    /// zero-based column index in line
    column: uint32;
    /// error/warning, etc.
    flags: uint32;
    /// the error number, e.g. see js.msg
    errorNumber: uint32;
    /// One of the JSExnType constants
    exnType: JSExnType;
    /// See the comment in ReadOnlyCompileOptions.
    isMuted: Boolean;
    ownsLinebuf: Boolean;
    ownsMessage: Boolean;
{$ELSE}
    /// offending source line without final #13
    linebuf: PCChar16;
    /// number of chars in linebuf_. Does not include trailing '\0'
    linebufLength: size_t;
    /// the 0-based offset of error token in linebuf_
    tokenOffset:size_t;
    /// source file name, URL, etc., or null
    filename: PCChar;
    /// source line number
    lineno: uint32;
    /// zero-based column index in line
    column: uint32;
    /// See the comment in ReadOnlyCompileOptions.
    isMuted: Boolean;
    /// error/warning, etc.
    flags: uint32;
    /// the error number, e.g. see js.msg
    errorNumber: uint32;
    /// the (default) error message
    ucmessage: PCChar16;
    /// arguments for the error message
    messageArgs: PPCChar16;
    /// One of the JSExnType constants
    exnType: JSExnType;
{$ENDIF}
  end;

  /// used by JSErrorCallback() callback
  JSErrorFormatString = record
{$IFDEF SM52}
    /// The error message name in ASCII.
    name: PCChar;
{$ENDIF}
    /// The error format string (UTF-8 if js_CStringsAreUTF8)
    format: PCChar;
    /// The number of arguments to expand in the formatted error message
    argCount: uint16;
    /// One of the JSExnType constants above
    exnType: JSExnType;
  end;

/// * Finalizes external strings created by JS_NewExternalString.
  JSStringFinalizer = record
    finalize: JSStringFinalizerOp;
  end;

const
  /// JSClass instance objects have private slot
  JSCLASS_HAS_PRIVATE             =   1 shl 0;
  /// JSClass instance class's initialization code will call
  // SetNewObjectMetadata itself
  JSCLASS_DELAY_METADATA_CALLBACK =   1 shl 1;
  /// JSClass instance private is (nsISupports*)
  JSCLASS_PRIVATE_IS_NSISUPPORTS =   1 shl 3;
  /// JSClass instance objects are DOM
  JSCLASS_IS_DOMJSCLASS          =   1 shl 4;
  /// JSClass instance objects of this class act like the value undefined,
  // in some contexts
  JSCLASS_EMULATES_UNDEFINED     =   1 shl 6;
  /// Reserved for embeddings.
  JSCLASS_USERBIT1               =   1 shl 7;
  /// JSClass instance room for 8 flags below
  JSCLASS_RESERVED_SLOTS_SHIFT   = 8;
  /// JSClass instance and 16 above this field
  JSCLASS_RESERVED_SLOTS_WIDTH   = 8;

  JSCLASS_RESERVED_SLOTS_MASK    = (uint32(1) shl JSCLASS_RESERVED_SLOTS_WIDTH)-1;
  JSCLASS_HIGH_FLAGS_SHIFT       = (JSCLASS_RESERVED_SLOTS_SHIFT + JSCLASS_RESERVED_SLOTS_WIDTH);

  JSCLASS_IS_ANONYMOUS           = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+0));
  JSCLASS_IS_GLOBAL              = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+1));
  JSCLASS_INTERNAL_FLAG2         = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+2));
  JSCLASS_INTERNAL_FLAG3         = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+3));

  JSCLASS_IS_PROXY               = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+4));

  JSCLASS_SKIP_NURSERY_FINALIZE  = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+5));
  // Reserved for embeddings.
  JSCLASS_USERBIT2               = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+6));
  JSCLASS_USERBIT3               = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+7));

  JSCLASS_BACKGROUND_FINALIZE    = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+8));
// Bits 26 through 31 are reserved for the CACHED_PROTO_KEY mechanism, see
// below.

  JSCLASS_GLOBAL_APPLICATION_SLOTS = 5;

//  JSProto_LIMIT = 46;
{$IFDEF SM52}
  JSCLASS_GLOBAL_SLOT_COUNT         = (JSCLASS_GLOBAL_APPLICATION_SLOTS + ord(JSProto_LIMIT) * 2 + 39);
{$ELSE}
  JSCLASS_GLOBAL_SLOT_COUNT         = (JSCLASS_GLOBAL_APPLICATION_SLOTS + ord(JSProto_LIMIT) * 3 + 36);
{$ENDIF}

  JSCLASS_GLOBAL_FLAGS              = JSCLASS_IS_GLOBAL or
                                      (((JSCLASS_GLOBAL_SLOT_COUNT) and JSCLASS_RESERVED_SLOTS_MASK) shl JSCLASS_RESERVED_SLOTS_SHIFT);


/// Property attributes, set in JSPropertySpec and passed to API functions.
// NB: The data structure in which some of these values are stored only uses
//     a uint8_t to store the relevant information. Proceed with caution if
//     trying to reorder or change the the first byte worth of flags.

  JSPROP_ENUMERATE        = $01; // property is visible to for/in loop
  JSPROP_READONLY         = $02; // not settable: assignment is no-op.
                                  //  This flag is only valid when neither
                                  //  JSPROP_GETTER nor JSPROP_SETTER is
                                  //  set.
  JSPROP_PERMANENT        = $04; // property cannot be deleted
  JSPROP_PROPOP_ACCESSORS = $08; // Passed to JS_Define(UC)Property* and
                                  //  JS_DefineElement if getters/setters
                                  //  are JSPropertyOp/JSStrictPropertyOp
  JSPROP_GETTER           = $10; // property holds getter function
  JSPROP_SETTER           = $20; // property holds setter function
  JSPROP_SHARED           = $40; // don't allocate a value slot for this
                                  // property; don't copy the property on
                                  // set of the same-named property in an
                                  // object that delegates to a prototype
                                  // containing this property
  JSPROP_INTERNAL_USE_BIT= $80; // name is actually (int) index
  JSPROP_DEFINE_LATE     = $100; // Don't define property when initially creating
                                  // the constructor. Some objects like Function/Object
                                  // have self-hosted functions that can only be defined
                                  // after the initialization is already finished.
  JSFUN_STUB_GSOPS       = $200; // use JS_PropertyStub getter/setter
                                  // instead of defaulting to class gsops
                                  // for property holding function

  JSFUN_CONSTRUCTOR      = $400; // native that can be called as a ctor

{$IFDEF SM52}
// unused
{$ELSE}
// Specify a generic native prototype methods, i.e., methods of a class
// prototype that are exposed as static methods taking an extra leading
// argument: the generic |this| parameter.
// If you set this flag in a JSFunctionSpec struct's flags initializer, then
// that struct must live at least as long as the native static method object
// created due to this flag by JS_DefineFunctions or JS_InitClass.  Typically
// JSFunctionSpec structs are allocated in static arrays.
//  JSFUN_GENERIC_NATIVE   = $800;

  JSFUN_GENERIC_NATIVE = $800;
{$ENDIF}
{$IFDEF SM52}
  JSFUN_HAS_REST = $1000; // function has ...rest parameter.
{$ENDIF}

  JSPROP_REDEFINE_NONCONFIGURABLE = $1000; // If set, will allow redefining a
                                           // non-configurable property, but
                                           // only on a non-DOM global.  This
                                           // is a temporary hack that will
                                           // need to go away in bug
                                           // 1105518

// Resolve hooks and enumerate hooks must pass this flag when calling
// JS_Define* APIs to reify lazily-defined properties.
// JSPROP_RESOLVING is used only with property-defining APIs. It tells the
// engine to skip the resolve hook when performing the lookup at the beginning
// of property definition. This keeps the resolve hook from accidentally
// triggering itself: unchecked recursion.
// For enumerate hooks, triggering the resolve hook would be merely silly, not
// fatal, except in some cases involving non-configurable properties.
  JSPROP_RESOLVING        = $2000;

  JSPROP_IGNORE_ENUMERATE = $4000;  // ignore the value in JSPROP_ENUMERATE.
                                    // This flag only valid when defining over
                                    // an existing property.
  JSPROP_IGNORE_READONLY  = $8000;  // ignore the value in JSPROP_READONLY.
                                    // This flag only valid when defining over
                                    // an existing property.
  JSPROP_IGNORE_PERMANENT = $10000; // ignore the value in JSPROP_PERMANENT.
                                    // This flag only valid when defining over
                                    // an existing property.
  JSPROP_IGNORE_VALUE     = $20000; // ignore the Value in the descriptor. Nothing was
                                    // specified when passed to Object.defineProperty
                                    // from script.

type
  /// available options for JS Objects Properties
  TJSPropertyAttr = (
    jspEnumerate, jspReadOnly, jspPermanent, jspPropAccessors, jspGetter,
    jspSetter, jspShared, jspInternal, jspDefineLate,
    jspFunStubGSOps, jspFunConstructor, jspFunGenericNative,
    jspRedefineNonConfigurable, jspResolving,
    jspIgnoreEnumerate, jspIgnoreReadOnly, jspIgnorePermanent, jspIgnoreValue);
  /// set of available options for JS Objects Properties
  TJSPropertyAttrs = set of TJSPropertyAttr;

  ESMException = class(ESynException)
  private
    FJSErrorNum: integer;
    FFileName: RawUTF8;
    FLineNum: integer;
    FJSStackTrace: SynUnicode;
  public
    /// constructor which will create JavaScript exception with JS stack trace
    constructor CreateWithTrace(const AFileName: RawUTF8; AJSErrorNum, ALineNum: integer;
       AMessage: string; const AStackTrace: SynUnicode);
    /// Format a JS exception as text
    // If SM_DEBUG is defined will write full JS stack, including SyNode core_modules calls
    //  if not - core_modules is cutched from stack trace for simplicity
    procedure WriteFormatted(WR: TTextWriter);

    {$ifndef NOEXCEPTIONINTERCEPT}
    /// Custmize SM exception log output
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    {$endif}
  published
    property ErrorNum: integer read FJSErrorNum;
    property Stack: SynUnicode read FJSStackTrace;
    property FileName: RawUTF8 read FFileName;
    property Line: integer read FLineNum;
  end;

/// pass exception of this type to JSError for raising JS RangeError exception
  ESMRangeException = class(ESMException);
/// pass exception of this type to JSError for raising JS TypeError exception
  ESMTypeException = class(ESMException);

/// to be used to catch Delphi exceptions inside JSNative function implementation
// - usage example:
// ! try
// !   doSomething()
// !   Result := True;
// ! except
// ! on E: Exception do begin
// !   vp.rval := JSVAL_VOID;
// !   JSError(cx, E);
// !   Result := False;
// ! end;

procedure JSError(cx: PJSContext; aException: Exception);
procedure JSErrorUC(cx: PJSContext; aMessage: WideString);
procedure JSRangeErrorUC(cx: PJSContext; aMessage: WideString);
procedure JSTypeErrorUC(cx: PJSContext; aMessage: WideString);

// must be called ONCE per process before any interaction with JavaScript
function InitJS: Boolean;
// must be called ONCE per process after all engine managers are destroyed
procedure ShutDownJS;

var
    nullObj: PJSObject = nil;
const
{$ifdef CPU64}
  JSVAL_TAG_SHIFT              = 47;
  JSVAL_PAYLOAD_MASK           = $00007FFFFFFFFFFF;
  JSVAL_TAG_MASK               = $FFFF800000000000;

  JSVAL_TAG_MAX_DOUBLE                = UInt32($1FFF0);
  JSVAL_TAG_INT32                     = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_INT32));
  JSVAL_TAG_UNDEFINED                 = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_UNDEFINED));
  JSVAL_TAG_STRING                    = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_STRING));
  JSVAL_TAG_SYMBOL                    = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_SYMBOL));
  JSVAL_TAG_BOOLEAN                   = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_BOOLEAN));
  JSVAL_TAG_MAGIC                     = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_MAGIC));
  JSVAL_TAG_NULL                      = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_NULL));
  JSVAL_TAG_OBJECT                    = UInt32(JSVAL_TAG_MAX_DOUBLE or UInt8(JSVAL_TYPE_OBJECT));

  JSVAL_SHIFTED_TAG_MAX_DOUBLE        = (uint64(JSVAL_TAG_MAX_DOUBLE) shl JSVAL_TAG_SHIFT) or $FFFFFFFF;
  JSVAL_SHIFTED_TAG_INT32             = uint64(JSVAL_TAG_INT32) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_UNDEFINED         = uint64(JSVAL_TAG_UNDEFINED) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_STRING            = uint64(JSVAL_TAG_STRING) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_SYMBOL            = uint64(JSVAL_TAG_SYMBOL) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_BOOLEAN           = uint64(JSVAL_TAG_BOOLEAN) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_MAGIC             = uint64(JSVAL_TAG_MAGIC) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_NULL              = uint64(JSVAL_TAG_NULL) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_OBJECT            = uint64(JSVAL_TAG_OBJECT) shl JSVAL_TAG_SHIFT;
{$endif}

{$ifdef CPU64}
  JSVAL_NULL_impl =  QWord(JSVAL_SHIFTED_TAG_NULL);
  JSVAL_VOID_impl =  QWord(JSVAL_SHIFTED_TAG_UNDEFINED);
{$ELSE}
  JSVAL_NULL_impl =  QWord(QWord(JSVAL_TAG_NULL     ) shl 32) or 0;
  JSVAL_VOID_impl =  QWord(QWord(JSVAL_TAG_UNDEFINED) shl 32) or 0;
{$endif}

  JSVAL_NAN_impl  =  $7FF8000000000000;

const
  JSVAL_NULL: jsval = (_l:(asBits: JSVAL_NULL_impl));
  JSVAL_VOID: jsval = (_l:(asBits: JSVAL_VOID_impl));
  JSVAL_NAN:  jsval = (_l:(asBits: JSVAL_NAN_impl));

const
  JSREPORT_ERROR = 0;
  JSREPORT_WARNING = 1;
  JSREPORT_EXCEPTION = 2;
  JSREPORT_STRICT = 4;
  JSREPORT_STRICT_MODE_ERROR = 8;

function SimpleVariantToJSval(cx: PJSContext; val: Variant): jsval;

const
{$IFDEF SM52}
  SpiderMonkeyLib = 'synmozjs52'{$IFDEF MSWINDOWS} + '.dll'{$ENDIF};
{$ELSE}
  SpiderMonkeyLib = 'mozjs-45'{$IFDEF MSWINDOWS} + '.dll'{$ENDIF};
{$ENDIF}

 /// Initialize SpiderMonkey, returning true only if initialization succeeded.
 // Once this method has succeeded, it is safe to call JS_NewRuntime and other
 // JSAPI methods.
 // - This method must be called before any other JSAPI method is used on any
 // thread.  Once it has been used, it is safe to call any JSAPI method, and it
 // remains safe to do so until JS_ShutDown is correctly called.
 // - It is currently not possible to initialize SpiderMonkey multiple times (that
 // is, calling JS_Init/JSAPI methods/JS_ShutDown in that order, then doing so
 // again).  This restriction may eventually be lifted.
function JS_Init: Boolean; cdecl; external SpiderMonkeyLib {$IFDEF SM52}name 'JS_Initialize'{$ENDIF};
 /// Destroy free-standing resources allocated by SpiderMonkey, not associated
 // with any runtime, context, or other structure.
 // - This method should be called after all other JSAPI data has been properly
 // cleaned up: every new runtime must have been destroyed, every new context
 // must have been destroyed, and so on.  Calling this method before all other
 // resources have been destroyed has undefined behavior.
 // - Failure to call this method, at present, has no adverse effects other than
 // leaking memory.  This may not always be the case; it's recommended that all
 // embedders call this method when all other JSAPI operations have completed.
 // - It is currently not possible to initialize SpiderMonkey multiple times (that
 // is, calling JS_Init/JSAPI methods/JS_ShutDown in that order, then doing so
 // again).  This restriction may eventually be lifted.
procedure JS_ShutDown; cdecl; external SpiderMonkeyLib;

/// Microseconds since the epoch, midnight, January 1, 1970 UTC.
function JS_Now: int64; cdecl; external SpiderMonkeyLib;

/// Returns the empty string as a JSString object.
{$IFDEF SM52}
function JS_GetEmptyString(cx: PJSContext): PJSString; cdecl; external SpiderMonkeyLib;
{$ELSE}
function JS_GetEmptyString(rt: PJSRuntime): PJSString; cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Determines the JS data type of a JS value.
function JS_TypeOfValue(cx: PJSContext; var v: jsval): JSType; cdecl; external SpiderMonkeyLib;

{$IFNDEF SM52}
/// Initializes the JavaScript runtime.
function JS_NewRuntime(maxbytes: uint32; maxNurseryBytes: uint32; parentRuntime: PJSRuntime): PJSRuntime;
  cdecl; external SpiderMonkeyLib;

/// Frees a JavaScript runtime.
procedure JS_DestroyRuntime(runtime: PJSRuntime); cdecl; external SpiderMonkeyLib;

/// Read access a JSRuntime field for application-specific data.
// Memory management for this private data is the application's responsibility.
// The JavaScript engine itself never uses it.
function JS_GetRuntimePrivate(rt: PJSRuntime): pointer; cdecl; external SpiderMonkeyLib;

/// Wrote access a JSRuntime field for application-specific data.
// Memory management for this private data is the application's responsibility.
// The JavaScript engine itself never uses it.
procedure JS_SetRuntimePrivate(rt: PJSRuntime; data: pointer); cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// indicates to the JS engine that the calling thread is entering a region
// of code that may call into the JSAPI but does not block
procedure JS_BeginRequest(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// indicates to the JS engine that the calling thread is leaving a region
// of code that may call into the JSAPI but does not block
procedure JS_EndRequest(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Create a new JSContext
{$IFDEF SM52}
function JS_NewContext(maxbytes: uint32; maxNurseryBytes: uint32 = 16 * (1 SHL 20); parentContext: PJSContext = nil): PJSContext;
  cdecl; external SpiderMonkeyLib;
function InitSelfHostedCode(cx: PJSContext): boolean; cdecl; external SpiderMonkeyLib;
{$ELSE}
function JS_NewContext(rt: PJSRuntime; stackChunkSize: size_t): PJSContext;
  cdecl; external SpiderMonkeyLib;
{$ENDIF}
/// Destroy a JSContext.
procedure JS_DestroyContext(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Read access a JSContext field for application-specific data.
// Memory management for this private data is the application's responsibility.
// The JavaScript engine itself never uses it.
function JS_GetContextPrivate(cx: PJSContext): Pointer; cdecl; external SpiderMonkeyLib;
/// Write access a JSContext field for application-specific data.
// Memory management for this private data is the application's responsibility.
// The JavaScript engine itself never uses it.
procedure JS_SetContextPrivate(cx: PJSContext; data: Pointer); cdecl; external SpiderMonkeyLib;

{$IFNDEF SM52}
/// Retrieves a pointer to the JSRuntime with which a specified JSContext, cx, is associated
function JS_GetRuntime(cx: PJSContext): PJSRuntime; cdecl; external SpiderMonkeyLib;
{$ENDIF}
/// This function makes a cross-compartment wrapper for the given JS object.
// Details see here http://stackoverflow.com/questions/18730477/what-does-js-wrapobject-do
function JS_WrapObject(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

/// Enter a different compartment on the given context, so that objects in that
// compartment can be accessed.
// - NB: This API is infallible; a NULL return value does not indicate error
function JS_EnterCompartment(cx: PJSContext; target: PJSObject): PJSCompartment;
  cdecl; external SpiderMonkeyLib;

/// Leave a the compartment, returning to the compartment active before the
// corresponding JS_EnterCompartment.
procedure JS_LeaveCompartment(cx: PJSContext; oldCompartment: PJSCompartment);
  cdecl; external SpiderMonkeyLib;

/// Initialize standard JS class constructors, prototypes, and any top-level
// functions and constants associated with the standard classes (e.g. isNaN
// for Number).
// - NB: This sets cx's global object to obj if it was null.
function JS_InitStandardClasses(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

///Return the global object for the active function on the context.
function JS_CurrentGlobalOrNull(cx: PJSContext):PJSObject; cdecl; external SpiderMonkeyLib name 'CurrentGlobalOrNull';

/// Add 'Reflect.parse', a SpiderMonkey extension, to the Reflect object on the
// given global.
function JS_InitReflectParse(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

/// Initialize the 'ctypes' object on a global variable 'obj'. The 'ctypes'
// object will be sealed.
function JS_InitCTypesClass(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

/// Initialize the 'Debugger' object on a global variable 'obj'. The 'ctypes'
// object will be sealed.
function JS_DefineDebuggerObject(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

/// Performs garbage collection in the JS memory pool.
{$IFDEF SM52}
procedure JS_GC(cx: PJSContext); cdecl; external SpiderMonkeyLib;
{$ELSE}
procedure JS_GC(rt: PJSRuntime); cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Offer the JavaScript engine an opportunity to perform garbage collection if needed.
procedure JS_MaybeGC(cx: PJSContext); cdecl; external SpiderMonkeyLib;

///Set performance parameters related to garbage collection.
{$IFDEF SM52}
procedure JS_SetGCParameter(cx: PJSContext; key: JSGCParamKey; value: uint32);
  cdecl; external SpiderMonkeyLib;
{$ELSE}
procedure JS_SetGCParameter(rt: PJSRuntime; key: JSGCParamKey; value: uint32);
  cdecl; external SpiderMonkeyLib;
{$ENDIF}

///Get performance parameters related to garbage collection.
{$IFDEF SM52}
function JS_GetGCParameter(cx: PJSContext; key: JSGCParamKey): uint32;
  cdecl; external SpiderMonkeyLib;
{$ELSE}
function JS_GetGCParameter(rt: PJSRuntime; key: JSGCParamKey): uint32;
  cdecl; external SpiderMonkeyLib;
{$ENDIF}

///Adjust performance parameters related to garbage collection based on available memory(in megabytes).
{$IFDEF SM52}
procedure JS_SetGCParametersBasedOnAvailableMemory(cx: PJSContext; availMem: uint32);
  cdecl; external SpiderMonkeyLib;
{$ELSE}
procedure JS_SetGCParametersBasedOnAvailableMemory(rt: PJSRuntime; availMem: uint32);
  cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Creates a new JSString whose characters are stored in external memory, i.e.,
//  memory allocated by the application, not the JavaScript engine
// - Since the program allocated the memory, it will need to free it;
//  this happens in an external string finalizer indicated by the type parameter.
// - chars is Pointer to the first element of an array of jschars.
//  This array is used as the character buffer of the JSString to be created.
//  The array must be populated with the desired character data before JS_NewExternalString
//  is called, and the array must remain in memory, with its contents unchanged,
//  for as long as the JavaScript engine needs to hold on to it.
// (Ultimately, the string will be garbage collected, and the JavaScript engine will
// call the string finalizer callback, allowing the application to free the array)
//  - The text buffer array does not need to be zero-terminated.
function JS_NewExternalString(cx: PJSContext; chars: PCChar16; length: size_t;
  fin: PJSStringFinalizer): PJSString; cdecl; external SpiderMonkeyLib;

/// Set the size of the native stack that should not be exceed. To disable
// stack size checking pass 0.
// - SpiderMonkey allows for a distinction between system code (such as GCs, which
// may incidentally be triggered by script but are not strictly performed on
// behalf of such script), trusted script (as determined by JS_SetTrustedPrincipals),
// and untrusted script. Each kind of code may have a different stack quota,
// allowing embedders to keep higher-priority machinery running in the face of
// scripted stack exhaustion by something else.
// - The stack quotas for each kind of code should be monotonically descending,
// and may be specified with this function. If 0 is passed for a given kind
// of code, it defaults to the value of the next-highest-priority kind.
// - This function may only be called immediately after the runtime is initialized
// and before any code is executed and/or interrupts requested.
{$IFDEF SM52}
procedure JS_SetNativeStackQuota(cx: PJSContext; systemCodeStackSize: size_t;
  trustedScriptStackSize: size_t = 0; untrustedScriptStackSize: size_t = 0); cdecl; external SpiderMonkeyLib;
{$ELSE}
procedure JS_SetNativeStackQuota(runtime: PJSRuntime; systemCodeStackSize: size_t;
  trustedScriptStackSize: size_t = 0; untrustedScriptStackSize: size_t = 0); cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Convert a JS::Value to type jsid.
function JS_ValueToId(cx: PJSContext; var v: jsval; out id: jsid): Boolean; cdecl; external SpiderMonkeyLib;
/// Convert a jsid to type JS::Value.
function JS_IdToValue(cx: PJSContext; id: jsid; out v: jsval): Boolean; cdecl; external SpiderMonkeyLib;

function JS_ValueToSource(cx: PJSContext; var v: jsval): PJSString; cdecl; external SpiderMonkeyLib;


/// Make a JSClass accessible to JavaScript code by creating its prototype,
// constructor, properties, and functions.
function JS_InitClass(cx: PJSContext; var obj: PJSObject; var parent_proto: PJSObject;
    clasp: PJSClass; _constructor: JSNative; nargs: uintN;
    ps: PJSPropertySpec; fs: PJSFunctionSpec;
    static_ps: PJSPropertySpec; static_fs: PJSFunctionSpec): PJSObject;
  cdecl; external SpiderMonkeyLib ;

/// Retrieves the class associated with an object.
function JS_GetClass(obj: PJSObject): PJSClass; cdecl; external SpiderMonkeyLib;

/// JSAPI method equivalent to the instanceof operator in JavaScript.
function JS_HasInstance(cx: PJSContext; var obj: PJSObject; var val: jsval; out res: Boolean): Boolean; cdecl; external SpiderMonkeyLib;

/// Access the private data field of an object.
function JS_GetPrivate(obj: PJSObject): Pointer; cdecl; external SpiderMonkeyLib;

/// Sets the private data field of an object.
procedure JS_SetPrivate(obj: PJSObject; data: Pointer); cdecl; external SpiderMonkeyLib;

/// Retrieves the constructor for an object.
function JS_GetConstructor(cx: PJSContext; var proto: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;

/// Retrieve the private data associated with an object, if that object is an
// instance of a specified class.
function JS_GetInstancePrivate(cx: PJSContext; var obj: PJSObject; clasp: PJSClass; args: JSUnknown): Pointer;
  cdecl; external SpiderMonkeyLib;

/// Create a new JavaScript object for use as a global object.
function JS_NewGlobalObject(cx: PJSContext; clasp: PJSClass; principals: PJSPrincipals;
  hookOption: OnNewGlobalHookOption; options: PJS_CompartmentOptions): PJSObject; cdecl; external SpiderMonkeyLib;

/// Spidermonkey does not have a good way of keeping track of what compartments should be marked on
/// their own. We can mark the roots unconditionally, but marking GC things only relevant in live
/// compartments is hard. To mitigate this, we create a static trace hook, installed on each global
/// object, from which we can be sure the compartment is relevant, and mark it.
///
/// It is still possible to specify custom trace hooks for global object classes. They can be
/// provided via the CompartmentOptions passed to JS_NewGlobalObject.
procedure JS_GlobalObjectTraceHook(trc: Pointer{ JSTracer }; global: PJSObject); cdecl; external SpiderMonkeyLib;

/// Create a new object based on a specified class
function JS_NewObject(cx: PJSContext; clasp: PJSClass): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new object based on a specified class
// - Unlike JS_NewObject, JS_NewObjectWithGivenProto does not compute a default
// proto. If proto is nullptr, the JS object will have `null` as [[Prototype]].
function JS_NewObjectWithGivenProto(cx: PJSContext; clasp: PJSClass; var proto: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;

/// Get the prototype of obj, storing it in result.
// - Implements: ES6 [[GetPrototypeOf]] internal method.
function JS_GetPrototype(cx: PJSContext; var obj: PJSObject; out result: PJSObject):Boolean; cdecl; external SpiderMonkeyLib;

/// Change the prototype of obj.
// - Implements: ES6 [[SetPrototypeOf]] internal method.
// - In cases where ES6 [[SetPrototypeOf]] returns false without an exception,
// JS_SetPrototype throws a TypeError and returns false.
// - Performance warning: JS_SetPrototype is very bad for performance. It may
// cause compiled jit-code to be invalidated. It also causes not only obj but
// all other objects in the same "group" as obj to be permanently deoptimized.
// It's better to create the object with the right prototype from the start.
function JS_SetPrototype(cx: PJSContext; var obj: PJSObject; var proto: PJSObject):Boolean; cdecl; external SpiderMonkeyLib;

/// Create a new property on an object.
// Name indentifies by ID
function JS_DefinePropertyById(cx: PJSContext; var obj: PJSObject; var id: jsid;
  var value: jsval; attrs: uint32; getter: JSNative; setter: JSNative): boolean;
    cdecl; external SpiderMonkeyLib;
/// Create a new property on an object.
// Name indentifies by ansi string
function JS_DefineProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar;
    var value: jsval; attrs: uint32; getter: JSNative; setter: JSNative): boolean;
    cdecl; external SpiderMonkeyLib;
/// Create a new property on an object.
// Name indentifies by unicode string
function JS_DefineUCProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar16;
    namelen: size_t; var value: jsval; attrs: uint32; getter: JSNative; setter: JSNative): Boolean;
    cdecl; external SpiderMonkeyLib;

/// Determine whether a JavaScript object has a specified property.
// Name indentifies by ansi string
function JS_HasProperty(cx: PJSContext; var obj: PJSObject;
    const name: PCChar; var found: Boolean): Boolean;
  cdecl; external SpiderMonkeyLib;
/// Determine whether a JavaScript object has a specified property.
// Name indentifies by unicode string
function JS_HasUCProperty(cx: PJSContext; var obj: PJSObject;
    const name: PCChar16; namelen: size_t; var found: Boolean): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Find a specified property and retrieve its value.
// Name indentifies by ID
function JS_GetPropertyById(cx: PJSContext; var obj: PJSObject; var id: jsid;
    out vp: jsval): boolean; cdecl; external SpiderMonkeyLib;
/// Find a specified property and retrieve its value.
// Name indentifies by ansi string
function JS_GetProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar;
    out vp: jsval): boolean; cdecl; external SpiderMonkeyLib;
/// Find a specified property and retrieve its value.
// Name indentifies by unicode string
function JS_GetUCProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar16; namelen: size_t;
    out vp: jsval): boolean; cdecl; external SpiderMonkeyLib;
/// Find a specified numeric property of an object and return its current value.
function JS_GetElement(cx: PJSContext; var obj: PJSObject; index: uint32;
  out vp: jsval): Boolean; cdecl; external SpiderMonkeyLib;

/// Assign a value to a property of an object.
// Name indentifies by ansi string
function JS_SetProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar;
    var vp: jsval): Boolean; cdecl; external SpiderMonkeyLib;
/// Assign a value to a property of an object.
// Name indentifies by unicode string
function JS_SetUCProperty(cx: PJSContext; var obj: PJSObject; const name: PCChar16; namelen: size_t;
    var vp: jsval): boolean; cdecl; external SpiderMonkeyLib;
/// Assign a value to a numeric property of an object.
function JS_SetElement(cx: PJSContext; var obj: PJSObject; index: uint32;
    var vp: jsval): Boolean; cdecl; external SpiderMonkeyLib;

/// Removes a specified property from an object.
// Name indentifies by ID
function JS_DeletePropertyById(cx: PJSContext; var obj: PJSObject; var id: jsid; out res: JS_ObjectOpResult): Boolean;
  cdecl; external SpiderMonkeyLib;
/// Removes a specified element or numeric property from an object.
function JS_DeleteElement(cx: PJSContext; var obj: PJSObject; index: uint32; out res: JS_ObjectOpResult): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Get an array of the non-symbol enumerable properties of obj.
// This function is roughly equivalent to:
//
//     var result = [];
//     for (key in obj)
//         result.push(key);
//     return result;
//
// This is the closest thing we currently have to the ES6 [[Enumerate]]
// internal method.
//
// The JSIdArray returned by JS_Enumerate must be rooted to protect its
// contents from garbage collection. Use JS::AutoIdArray.
function JS_Enumerate(cx: PJSContext; var obj: PJSObject; out props: JSIdArray): Boolean;
  cdecl; external SpiderMonkeyLib;

type
  JSHandleValueArray = record
    length: size_t;
    elements_: PjsvalVector;
  end;
/// Calls a specified JS function.
// Function identifies by jsvalue
// - equivalent of `rval = Reflect.apply(fun, obj, args)`.
function JS_CallFunctionValue(cx: PJSContext; var obj: PJSObject; var val: jsval;
  var args: JSHandleValueArray; out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;
/// Calls a specified JS function.
// Function identifies by PJSFunction
function JS_CallFunction(cx: PJSContext; var obj: PJSObject; var fun: PJSFunction;
  var args: JSHandleValueArray; out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;
/// Calls a specified JS function.
// Function identifies by ansi string
// - Perform the method call `rval = obj[name](args)`.
function JS_CallFunctionName(cx: PJSContext; var obj: PJSObject; const name: PCChar;
  var args: JSHandleValueArray; out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;

/// Invoke a constructor, like the JS expression `new ctor(...args)`. Returns
// the new object, or null on error.
function JS_New(cx: PJSContext; var ctor: PJSObject; var args: JSHandleValueArray): PJSObject; cdecl; external SpiderMonkeyLib;

/// Define multiple properties for a single object.
function JS_DefineProperties(cx: PJSContext; var obj: PJSObject; ps: PJSPropertySpec): boolean; cdecl; external SpiderMonkeyLib ;

/// Determine whether a property is already physically present on a JSObject.
// Name indentifies by unicode string
function JS_AlreadyHasOwnUCProperty(cx: PJSContext; var obj: PJSObject;
    const name: PCChar16; namelen: size_t; var foundp: Boolean): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Create a new Array object.
// Only length passed
function JS_NewArrayObject(cx: PJSContext; length: size_t): PJSObject; cdecl; external SpiderMonkeyLib;
/// Create a new Array object.
// Content passed
function JS_NewArrayObject2(cx: PJSContext; const contents: JSHandleValueArray): PJSObject; cdecl; external SpiderMonkeyLib;

/// Returns true and sets |*isArray| indicating whether |obj| is an Array object
// or a wrapper around one, otherwise returns false on failure.
// - This method returns true with |*isArray == false| when passed a proxy whose
// target is an Array, or when passed a revoked proxy.
function JS_IsArrayObject(cx: PJSContext; var obj: PJSObject; out isArray: Boolean): boolean; cdecl; external SpiderMonkeyLib;
/// JS_GetArrayLength gets the .length property of obj as though by calling JS_GetProperty
// and converts it to a 32-bit unsigned integer. If obj is an array (see JS_IsArrayObject),
// this is guaranteed to succeed, because the .length property of an array is always a number
// and can't be deleted or redefined.
// - On success, JS_GetArrayLength stores the length in *lengthp and returns true.
// On failure, it reports an error and returns false, and the value left in *lengthp
// is undefined.
function JS_GetArrayLength(cx: PJSContext; var obj: PJSObject;
  out length: uint32): Boolean; cdecl; external SpiderMonkeyLib;

/// Read access an object's reserved slots.
{$IFDEF SM52}
function JS_GetReservedSlot(obj: PJSObject; index: uint32): Int64; cdecl; external SpiderMonkeyLib name 'JS_GetReservedSlot1';
{$ELSE}
function JS_GetReservedSlot(obj: PJSObject; index: uint32): Int64; cdecl; external SpiderMonkeyLib;
{$ENDIF}
/// Write access an object's reserved slots
{$IFDEF SM52}
procedure JS_SetReservedSlot(obj: PJSObject; index: uint32; var v: jsval); cdecl; external SpiderMonkeyLib;
{$ELSE}
procedure JS_SetReservedSlot(obj: PJSObject; index: uint32; v: Int64); cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Create a new JavaScript function that is implemented as a JSNative.
function JS_NewFunction(cx: PJSContext; call: JSNative; nargs: uintN; flags: uintN; name: PCChar): PJSObject; cdecl; external SpiderMonkeyLib;

/// Return the function's identifier as a JSString, or null if fun is unnamed.
// The returned string lives as long as fun, so you don't need to root a saved
// reference to it if fun is well-connected or rooted, and provided you bound
// the use of the saved reference by fun's lifetime.
function JS_GetFunctionId(fun: PJSFunction): PJSString; cdecl; external SpiderMonkeyLib;

/// Infallible predicate to test whether obj is a function object (faster than
// comparing obj's class name to "Function", but equivalent unless someone has
// overwritten the "Function" identifier with a different constructor and then
// created instances using that constructor that might be passed in as obj).
function JS_ObjectIsFunction(cx: PJSContext; obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;

/// Create zero or more functions and makes them properties (methods)
// of a specified object, obj, as if by calling JS_DefineFunction repeatedly
function JS_DefineFunctions(cx: PJSContext; var obj: PJSObject; fs: PJSFunctionSpec;
    behavior: JSPropertyDefinitionBehavior): Boolean;
  cdecl; external SpiderMonkeyLib;
/// Create a native function and assign it as a property to a specified JS object
function JS_DefineFunction(cx: PJSContext; var obj: PJSObject; name: PCChar;
    call: JSNative; nargs: uintN; attrs: uintN): PJSFunction;
  cdecl; external SpiderMonkeyLib;
/// Unicode version to create a native function
function JS_DefineUCFunction(cx: PJSContext; var obj: PJSObject; name: PCChar16;
    namelen: size_t; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction;
  cdecl; external SpiderMonkeyLib;

/// Compile a script, source, for execution.
// Ansi version
function JS_CompileScript(cx: PJSContext; bytes: PCChar;
    length: size_t; options: PJSCompileOptions; out script: PJSScript): boolean;
  cdecl; external SpiderMonkeyLib;
/// Compile a script, source, for execution.
// Unicode version
function JS_CompileUCScript(cx: PJSContext;
    chars: PCChar16; length: size_t; options: PJSCompileOptions; out script: PJSScript): boolean;
  cdecl; external SpiderMonkeyLib;

/// Generate the complete source code of a function declaration from a compiled function
function JS_DecompileFunction(cx: PJSContext; var fun: PJSFunction; indent: uintN): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Evaluate a script in the scope of the current global of cx.
function JS_ExecuteScript(cx: PJSContext; var script: PJSScript;
  out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;

{$IFDEF SM52}
/// These functions allow setting an interrupt callback that will be called
// from the JS thread some time after any thread triggered the callback using
// JS_RequestInterruptCallback(cx).
// - To schedule the GC and for other activities the engine internally triggers
// interrupt callbacks. The embedding should thus not rely on callbacks being
//triggered through the external API only.
// - Important note: Additional callbacks can occur inside the callback handler
// if it re-enters the JS engine. The embedding must ensure that the callback
// is disconnected before attempting such re-entry.
function JS_CheckForInterrupt(cx: PJSContext): Boolean; cdecl; external SpiderMonkeyLib;
function JS_AddInterruptCallback(cx: PJSContext; callback: JSInterruptCallback):
  Boolean; cdecl; external SpiderMonkeyLib;
function JS_DisableInterruptCallback(cx: PJSContext):Boolean; cdecl; external SpiderMonkeyLib;
procedure JS_ResetInterruptCallback(cx: PJSContext; enable: Boolean); cdecl; external SpiderMonkeyLib;
/// Request a callback set using JS_SetInterruptCallback
procedure JS_RequestInterruptCallback(cx: PJSContext); cdecl; external SpiderMonkeyLib;
{$ELSE}
/// Set a callback function that is automatically called periodically while JavaScript code runs.
function JS_SetInterruptCallback(rt: PJSRuntime; callback: JSInterruptCallback):
  JSInterruptCallback; cdecl; external SpiderMonkeyLib;
/// Return the currently installed interrupt callback, or NULL if none is currently installed
function JS_GetInterruptCallback(rt: PJSRuntime): JSInterruptCallback;
  cdecl; external SpiderMonkeyLib;
/// Request a callback set using JS_SetInterruptCallback
procedure JS_RequestInterruptCallback(rt: PJSRuntime); cdecl; external SpiderMonkeyLib;
{$ENDIF}
/// Indicates whether or not a script or function is currently executing in a given context.
function JS_IsRunning(cx: PJSContext): Boolean; cdecl; external SpiderMonkeyLib;

/// Allocate space for a JavaScript string and its underlying storage,
// and copy n characters from a character array, s, into the new JSString
// Ansi version
function JS_NewStringCopyN(cx: PJSContext; s: PCChar; n: size_t): PJSString; cdecl; external SpiderMonkeyLib;
/// Allocate space for a JavaScript string and its underlying storage,
// and copy n characters from a character array, s, into the new JSString
// Unicode version
function JS_NewUCStringCopyN(cx: PJSContext; s: PCChar16; n: size_t): PJSString; cdecl; external SpiderMonkeyLib;

/// Return the length of a JavaScript string.
function JS_GetStringLength(str: PJSString): size_t; cdecl; external SpiderMonkeyLib;

/// Return true if the string's characters are stored as Latin1.
function JS_StringHasLatin1Chars(str: PJSString): boolean; cdecl; external SpiderMonkeyLib;
/// Return a pointer to the string, and store the length to *length
// Use it when characters are stored as Latin1.
function JS_GetLatin1StringCharsAndLength(cx: PJSContext; nogc: PJSAutoCheckCannotGC; str: PJSString; plength: psize_t):PCChar; cdecl; external SpiderMonkeyLib;
/// Return a pointer to the string, and store the length to *length
// Use it when characters are stored as Unicode
function JS_GetTwoByteStringCharsAndLength(cx: PJSContext; nogc: PJSAutoCheckCannotGC; str: PJSString; plength: psize_t):PCChar16; cdecl; external SpiderMonkeyLib;

/// converts a value to JSON, optionally replacing values if a replacer
// function is specified, or optionally including only the specified properties
// if a replacer array is specified
function JS_Stringify(cx: PJSContext; var vp: jsval; var replacer: PJSObject;
    var space: jsval; callback: JSONWriteCallback; data: pointer): Boolean;
  cdecl; external SpiderMonkeyLib;

/// parse a string using the JSON syntax described in ECMAScript 5 and
// return the corresponding value into vp
function JS_ParseJSON(cx: PJSContext; const chars: PCChar16;
    len: uint32; out vp: jsval): Boolean;  cdecl; external SpiderMonkeyLib;

/// Create a new JavaScript Error object and set it to be the pending exception on cx.
// The callback must then return JS_FALSE to cause the exception to be propagated
// to the calling script.
procedure JS_ReportError(cx: PJSContext; const format: PCChar);
  cdecl; varargs; external SpiderMonkeyLib{$IFDEF SM52} name 'JS_ReportErrorASCII'{$ENDIF};
/// Report an error with an application-defined error code.
// - varargs is Additional arguments for the error message.
//- These arguments must be of type jschar*
// - The number of additional arguments required depends on the error
// message, which is determined by the errorCallback
procedure JS_ReportErrorNumberUC(cx: PJSContext; errorCallback: JSErrorCallback;
  userRef: pointer; const erroNubmer: uintN); cdecl; varargs; external SpiderMonkeyLib;
/// Reports a memory allocation error
// - Call JS_ReportOutOfMemory to report that an operation failed because the
// system is out of memory
// - When the JavaScript engine tries to allocate memory and allocation fails,
// it reports an error as though by calling this function
procedure JS_ReportOutOfMemory(cx: PJSContext); cdecl; external SpiderMonkeyLib;

{$IFDEF SM52}
/// Get the warning reporting mechanism for an application. It is not working for errors.
function JS_GetWarningReporter(cx: PJSContext): JSWarningReporter;
  cdecl; external SpiderMonkeyLib name 'GetWarningReporter';
/// Specify the warning reporting mechanism for an application.  It is not working for errors.
function JS_SetWarningReporter(cx: PJSContext; reporter: JSWarningReporter): JSWarningReporter;
  cdecl; external SpiderMonkeyLib name 'SetWarningReporter';
{$ELSE}
/// Get the error reporting mechanism for an application.
function JS_GetErrorReporter(rt: PJSRuntime): JSErrorReporter; cdecl; external SpiderMonkeyLib;
/// Specify the error reporting mechanism for an application.
function JS_SetErrorReporter(rt: PJSRuntime; er: JSErrorReporter): JSErrorReporter;
  cdecl; external SpiderMonkeyLib;
{$ENDIF}

/// Create a new JavaScript date object
function JS_NewDateObject(cx: PJSContext; year, mon, mday, hour, min, sec: int32): PJSObject;
  cdecl; external SpiderMonkeyLib;
/// Create a new JavaScript date object from the Unix millisecond elapsed since EPOC
function JS_NewDateObjectMsec(cx: PJSContext; msec: double): PJSObject;
  cdecl; external SpiderMonkeyLib;
// Returns true and sets |*isDate| indicating whether |obj| is a Date object or
// a wrapper around one, otherwise returns false on failure.
// - This method returns true with |*isDate == false| when passed a proxy whose
// target is a Date, or when passed a revoked proxy.
function JS_ObjectIsDate(cx: PJSContext; var obj: PJSObject; out isDate: boolean): boolean; cdecl; external SpiderMonkeyLib;

/// Determine whether an exception is pending in the JS engine.
function JS_IsExceptionPending(cx: PJSContext): Boolean; cdecl; external SpiderMonkeyLib;
/// Get the current pending exception for a given JSContext.
function JS_GetPendingException(cx: PJSContext; out vp: jsval): Boolean; cdecl; external SpiderMonkeyLib;
/// Sets the current exception being thrown within a context.
procedure JS_SetPendingException(cx: PJSContext; var vp: jsval); cdecl; external SpiderMonkeyLib;
/// Clear the currently pending exception in a context.
procedure JS_ClearPendingException(cx: PJSContext); cdecl; external SpiderMonkeyLib;
{$IFDEF SM52}
/// If the given object is an exception object, the exception will have (or be
// able to lazily create) an error report struct, and this function will return
// the address of that struct.  Otherwise, it returns nullptr. The lifetime
// of the error report struct that might be returned is the same as the
// lifetime of the exception object.
function JS_ErrorFromException(cx: PJSContext; var obj: PJSObject): PJSErrorReport;
  cdecl; external SpiderMonkeyLib;
{$ELSE}
/// Forward the current pending exception in a given JSContext
// to the current JSErrorReporter callback.
function JS_ReportPendingException(cx: PJSContext): Boolean; cdecl; external SpiderMonkeyLib;
{$ENDIF}

{$IFDEF SM52}
/// Get options of context
function JS_GetContextOptions(cx: PJSContext): PJSContextOptions; cdecl; external SpiderMonkeyLib;
{$ELSE}
/// Get options of runtime
function JS_GetRuntimeOptions(runtime: PJSRuntime): PJSRuntimeOptions; cdecl; external SpiderMonkeyLib;
{$ENDIF}

//function JS_NewRootedValue(cx: PJSContext; val: jsval): PJSRootedValue; cdecl; external SpiderMonkeyLib;
function JS_NewRootedValue(cx: PJSContext; val: Int64): PJSRootedValue; cdecl; external SpiderMonkeyLib;
procedure JS_FreeRootedValue(val: PJSRootedValue); cdecl; external SpiderMonkeyLib name 'JS_FreeRooteValue';

function JS_NewRootedObject(cx: PJSContext; obj: PJSObject): PJSRootedObject; cdecl; external SpiderMonkeyLib;
procedure JS_FreeRootedObject(obj: PJSRootedObject); cdecl; external SpiderMonkeyLib;

function JS_NewRootedString(cx: PJSContext; obj: PJSString): PJSRootedString; cdecl; external SpiderMonkeyLib;
procedure JS_FreeRootedString(str: PJSRootedString); cdecl; external SpiderMonkeyLib;

/// Create Compile Options
function JS_NewCompileOptions(cx: PJSContext): PJSCompileOptions; cdecl; external SpiderMonkeyLib;
/// Free Compile Options
procedure JS_FreeCompileOptions(opt: PJSCompileOptions); cdecl; external SpiderMonkeyLib;
///////////////////

function JS_EvaluateScript(cx: PJSContext;
   options: PJSCompileOptions;
   bytes: PCChar; length: size_t;
   out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;

function JS_EvaluateUCScript(cx: PJSContext;
   options: PJSCompileOptions;
   chars: PCChar16; length: size_t;
   out rval: jsval): Boolean; cdecl; external SpiderMonkeyLib;

/// Compute |this| for the |vp| inside a JSNative, either boxing primitives or
// replacing with the global object as necessary.
// - This method will go away at some point: instead use |args.thisv()|.  If the
// value is an object, no further work is required.  If that value is |null| or
// |undefined|, use |JS_GetGlobalForObject| to compute the global object.  If
// the value is some other primitive, use |JS_ValueToObject| to box it.
// - low-level API used by JS_THIS() macro.
//function JS_ComputeThis(cx: PJSContext; var vp: jsval): jsval; cdecl; external SpiderMonkeyLib;
function JS_ComputeThis(cx: PJSContext; var vp: jsval): Int64; cdecl; external SpiderMonkeyLib;

procedure strFinalizeOp(fin: PJSStringFinalizer; chars: PCChar16);  cdecl;

const
  strFinalizer: JSStringFinalizer = (
    finalize: strFinalizeOp;
  );

{ ArrayBuffer support from jsfriendapi.h}

/// Create a new signed 8 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewInt8Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new unsigned 8 bit integer (byte) typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewUint8Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewUint8ClampedArray(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new signed 16 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewInt16Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new unsigned 16 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewUint16Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new signed 32 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewInt32Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new unsigned 32 bit integer typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewUint32Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new signed 32 bit float (single) typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewFloat32Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new signed 64 bit float (double) typed array with nelements elements
// - will fill the newly created array with zeros
function JS_NewFloat64Array(cx: PJSContext; nelements: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit signed integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewInt8ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint8ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint8ClampedArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit signed integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewInt16ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint16ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit signed integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewInt32ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint32ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit float (single) typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewFloat32ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 64 bit float (double) typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewFloat64ArrayFromArray(cx: PJSContext; var arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt8ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint8ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint8ClampedArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt16ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint16ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt32ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint32ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit float (single) typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewFloat32ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 64 bit float (double) typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewFloat64ArrayWithBuffer(cx: PJSContext; var arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new SharedArrayBuffer with the given byte length.
function JS_NewSharedArrayBuffer(cx: PJSContext; nbytes: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new ArrayBuffer with the given byte length.
function JS_NewArrayBuffer(cx: PJSContext; nbytes: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Check whether obj supports JS_GetTypedArray* APIs
// - Note that this may return false if a security wrapper is encountered that
// denies the unwrapping.
// - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
// the dedicated accessor JSAPI calls
function JS_IsTypedArrayObject(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Check whether obj supports JS_GetArrayBufferView* APIs
// - Note that this may return false if a security wrapper is encountered that
// denies the unwrapping.
// - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
// the dedicated ArrayBufferView accessor JSAPI calls
function JS_IsArrayBufferViewObject(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt8Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint8Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint8ClampedArray(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 16 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt16Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 16 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint16Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt32Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint32Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit float (single) typed array types (ArrayBufferView subtypes)
function JS_IsFloat32Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Test for specific 64 bit float (double) typed array types (ArrayBufferView subtypes)
function JS_IsFloat64Array(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Return the isShared flag of a typed array, which denotes whether
// the underlying buffer is a SharedArrayBuffer.
//
// |obj| must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
// be known that it would pass such a test: it is a typed array or a wrapper of
// a typed array, and the unwrapping will succeed.
function JS_GetTypedArraySharedness(obj: PJSObject): Boolean; cdecl; external SpiderMonkeyLib;

/// Unwrap 8 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt8Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out Data: Pint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 8 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint8Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 8 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint8ClampedArray(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 16 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt16Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Pint16Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 16 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint16Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Puint16Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt32Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Pint32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint32Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Puint32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit float (single) typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsFloat32Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Pfloat32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 64 bit float (double) typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsFloat64Array(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Pfloat64Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap an object as its raw binary memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsArrayBufferView(obj: PJSObject; out length: uint32; out isSharedMemory:Boolean; out  Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap an object as its raw binary memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsArrayBuffer(obj: PJSObject; out length: uint32; out Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;


  /// Get the type of elements in a typed array, or jsabTYPE_DATAVIEW if a DataView
function JS_GetArrayBufferViewType(obj: PJSObject): JSArrayBufferViewType;
  cdecl; external SpiderMonkeyLib;

//function JS_GetSharedArrayBufferViewType(obj: PJSObject): JSArrayBufferViewType;
//  cdecl; external SpiderMonkeyLib;

/// Check whether obj supports the JS_GetArrayBuffer* APIs
// - Note that this may return false if a security wrapper is encountered that denies the
// unwrapping
// - If this test succeeds, then it is safe to call the various accessor JSAPI calls
function JS_IsArrayBufferObject(obj: PJSObject): Boolean;
  cdecl; external SpiderMonkeyLib;

function JS_IsSharedArrayBufferObject(obj: PJSObject): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Return the available byte length of an array buffer
// - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
// that it would pass such a test: it is an ArrayBuffer or a wrapper of an
// ArrayBuffer, and the unwrapping will succeed
function JS_GetArrayBufferByteLength(obj: PJSObject): uint32;
  cdecl; external SpiderMonkeyLib;

function JS_GetSharedArrayBufferByteLength(obj: PJSObject): uint32;
  cdecl; external SpiderMonkeyLib;

/// Return true if the arrayBuffer contains any data. This will return false for
// ArrayBuffer.prototype and neutered ArrayBuffers.
//
// |obj| must have passed a JS_IsArrayBufferObject test, or somehow be known
// that it would pass such a test: it is an ArrayBuffer or a wrapper of an
// ArrayBuffer, and the unwrapping will succeed.
function JS_ArrayBufferHasData(obj: PJSObject): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Return a pointer to an array buffer's data
// - The buffer is still owned by the array buffer object, and should not
// be modified on another thread. The returned pointer is stable across GCs
// - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
// that it would pass such a test: it is an ArrayBuffer or a wrapper of an
// ArrayBuffer, and the unwrapping will succeed.
function JS_GetArrayBufferData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector; cdecl; external SpiderMonkeyLib;

/// Check whether the obj is ArrayBufferObject and memory mapped. Note that this
// may return false if a security wrapper is encountered that denies the
// unwrapping.
function JS_IsMappedArrayBufferObject(obj: PJSObject): Boolean;
  cdecl; external SpiderMonkeyLib;

/// Return the number of elements in a typed array
// - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
// be known that it would pass such a test: it is a typed array or a wrapper of
// a typed array, and the unwrapping will succeed.
function JS_GetTypedArrayLength(obj: PJSObject): uint32; cdecl; external SpiderMonkeyLib;

/// Return the byte offset from the start of an array buffer to the start of a
// typed array view
// - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
// be known that it would pass such a test: it is a typed array or a wrapper of
// a typed array, and the unwrapping will succeed.
function JS_GetTypedArrayByteOffset(obj: PJSObject): uint32; cdecl; external SpiderMonkeyLib;

/// Return the byte length of a typed array
// - obj must have passed a JS_IsTypedArrayObject/JS_Is*Array test, or somehow
// be known that it would pass such a test: it is a typed array or a wrapper of
// a typed array, and the unwrapping will succeed
function JS_GetTypedArrayByteLength(obj: PJSObject): uint32; cdecl; external SpiderMonkeyLib;

/// More generic name for JS_GetTypedArrayByteLength to cover DataViews as well
function JS_GetArrayBufferViewByteLength(obj: PJSObject): uint32; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 8 bit signed integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetInt8ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint8ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint8ClampedArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 16 bit signed integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetInt16ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint16Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 16 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint16ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint16Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit signed integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetInt32ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pint32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint32ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Puint32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit float (single) array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetFloat32ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pfloat32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 64 bit float (double) array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetFloat64ArrayData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pfloat64Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by any typed array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
// - Prefer the type-specific versions when possible
function JS_GetArrayBufferViewData(obj: PJSObject; out isSharedMemory: Boolean; nogc: PJSAutoCheckCannotGC): Pointer; cdecl; external SpiderMonkeyLib;

/// Return the ArrayBuffer underlying an ArrayBufferView
// - If the buffer has been neutered, this will still return the neutered buffer.
// - obj must be an object that would return true for JS_IsArrayBufferViewObject()
function JS_GetArrayBufferViewBuffer(cx: PJSContext; var obj: PJSObject; out isSharedMemory: Boolean): PJSObject; cdecl; external SpiderMonkeyLib;


////modules

/// Initialize modeles classes next 2 functions cannot work without calling this function
function JS_InitModuleClasses(cx: PJSContext; var obj: PJSObject): boolean; cdecl; external SpiderMonkeyLib;
/// Compile script as module
function JS_CompileModule(cx: PJSContext;
   var obj: PJSObject;
   options: PJSCompileOptions;
   chars: PCChar16; length: size_t): PJSObject; cdecl; external SpiderMonkeyLib;
/// Set handler for module resolving
procedure JS_SetModuleResolveHook(cx: PJSContext; var hook: PJSFunction); cdecl; external SpiderMonkeyLib;

type
  pjsval = ^jsval;

var
  /// global TSynAnsiConvert instance to handle LATIN1(ISO/IEC 8859-1) encoding
  // - this instance is global and instantied during the whole program life time
  // - Spidermonkey internal encoding is LATIN1 or UTF-16
  Latin1AnsiConvert: TSynAnsiConvert;

implementation

uses
  Variants;

const
  JSVAL_INT_MAX = int32($7fffffff);

procedure JSError(cx: PJSContext; aException: Exception);
var
  ws: WideString;
begin
  if not JS_IsExceptionPending(cx) then
    // raise only if this is the first exception in chain
    if aException is EOutOfMemory then
      JS_ReportOutOfMemory(cx)
    else if aException is ESMRangeException then begin
      ws := StringToSynUnicode(aException.Message);
      JSRangeErrorUC(cx, ws);
    end else if aException is ESMTypeException then begin
      ws := StringToSynUnicode(aException.Message);
      JSTypeErrorUC(cx, ws);
    end else begin
      ws := StringToSynUnicode(aException.Message);
      JSErrorUC(cx, ws);
    end;
end;

const
 ErrorUCFormatString: JSErrorFormatString =
  (
    format: '{0}';
    argCount: 1;
    exnType: JSEXN_ERR;
  );
 RangeErrorUCFormatString: JSErrorFormatString =
  (
    format: '{0}';
    argCount: 1;
    exnType: JSEXN_RANGEERR;
  );
 TypeErrorUCFormatString: JSErrorFormatString =
  (
    format: '{0}';
    argCount: 1;
    exnType: JSEXN_TYPEERR;
  );
  SMExceptionNumber = 500;//from 0 to JSErr_Limit(421 for SM 45 ) Error numbers are reserved

function ReportErrorUC(userRef: Pointer; const errorNumber: uintN): PJSErrorFormatString; cdecl;
begin
  result := @ErrorUCFormatString;
end;

function ReportRangeErrorUC(userRef: Pointer; const errorNumber: uintN): PJSErrorFormatString; cdecl;
begin
  result := @RangeErrorUCFormatString;
end;

function TypeRangeErrorUC(userRef: Pointer; const errorNumber: uintN): PJSErrorFormatString; cdecl;
begin
  result := @TypeErrorUCFormatString;
end;

procedure JSErrorUC(cx: PJSContext; aMessage: WideString);
begin
  if not JS_IsExceptionPending(cx) then
    JS_ReportErrorNumberUC(cx, ReportErrorUC, nil, SMExceptionNumber ,Pointer(aMessage));
end;

procedure JSRangeErrorUC(cx: PJSContext; aMessage: WideString);
begin
  if not JS_IsExceptionPending(cx) then
    JS_ReportErrorNumberUC(cx, ReportRangeErrorUC, nil, SMExceptionNumber ,Pointer(aMessage));
end;

procedure JSTypeErrorUC(cx: PJSContext; aMessage: WideString);
begin
  if not JS_IsExceptionPending(cx) then
    JS_ReportErrorNumberUC(cx, TypeRangeErrorUC, nil, SMExceptionNumber ,Pointer(aMessage));
end;

function InitJS: Boolean;
begin
  Result := JS_Init;
end;

procedure ShutDownJS;
begin
  JS_ShutDown;
end;


{ JSString }

procedure JSString.ToJSONString(cx: PJSContext; W: TTextWriter);
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
begin
  W.Add('"');
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
    W.AddJSONEscape(pointer(str8),strL);
  end else begin
    str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
    W.AddJSONEscapeW(pointer(str16),strL);
  end;
  W.Add('"');
end;

function JSString.ToJSVal: jsval;
begin
  Result.asJSString := @self
end;

function JSString.ToString(cx: PJSContext): string;
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
begin
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
    {$ifdef UNICODE}
      SetString(Result, str8, strL);
    {$else}
      Result := Latin1AnsiConvert.AnsiToUnicodeString(str8, strL);
    {$endif}
  end else begin
    str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
    RawUnicodeToString(PWideChar(str16),strL,result)
  end;
end;

function JSString.ToSynUnicode(cx: PJSContext): SynUnicode;
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
begin
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
    Result := Latin1AnsiConvert.AnsiToUnicodeString(str8, strL);
  end else begin
    str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
    SetString(Result, str16, strL);
  end;
end;

function JSString.ToUTF8(cx: PJSContext): RawUTF8;
begin
  ToUTF8(cx,result);
end;

procedure JSString.ToUTF8(cx: PJSContext; var result: RawUTF8);
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
begin
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
    result := Latin1AnsiConvert.AnsiBufferToRawUTF8(str8, strL);
  end else begin
    str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
    RawUnicodeToUTF8(str16,strL,result, [ccfNoTrailingZero, ccfReplacementCharacterForUnmatchedSurrogate]);
  end;
end;

function JSString.ToAnsi(cx: PJSContext): AnsiString;
var
  str8: PCChar;
  strL: size_t;
begin
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
    SetLength(Result, strL);
    MoveFast(Pointer(str8)^,Result[1],strL);
  end else
    result := '';
end;

procedure JSString.ToVariant(cx: PJSContext; var Value: Variant);
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
begin
  VarClear(Value);
  with TVarData(Value) do begin
    VType := varSynUnicode;
    VAny := nil; // avoid GPF below
    if JS_StringHasLatin1Chars(@self) then begin
      str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);
      SynUnicode(VAny) := Latin1AnsiConvert.AnsiToUnicodeString(str8, strL);
    end else begin
      str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
      SetString(SynUnicode(VAny), str16, strL);
    end;
  end;
end;

procedure JSString.ToUTF8(cx: PJSContext; W: TTextWriter);
var
  str8: PCChar;
  str16: PCChar16;
  strL: size_t;
  tmpU8: array[0..256*3] of AnsiChar;
  U8: PUTF8Char;
begin
  if JS_StringHasLatin1Chars(@self) then begin
    str8 := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @self, @strL);

    if strL>=SizeOf(tmpU8)div 3 then
      Getmem(U8,strL*3+1) else
      U8 := @tmpU8;
    strL := Latin1AnsiConvert.AnsiBufferToUTF8(U8,pointer(str8),strL)-U8;
    W.AddNoJSONEscape(pointer(U8), strL);
    if U8<>@tmpU8 then
      FreeMem(U8);
  end else begin
    str16 := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @self, @strL);
    W.AddNoJSONEscapeW(pointer(str16),strL);
  end;
end;

function JSString.HasLatin1Chars: Boolean;
begin
  result := JS_StringHasLatin1Chars(@self);
end;

function JSString.GetLatin1StringCharsAndLength(cx: PJSContext;
  out len: size_t): PCChar;
begin
  Result := JS_GetLatin1StringCharsAndLength(cx, @nullPtr, @Self, @len);
end;

function JSString.Length: size_t;
begin
  Result := JS_GetStringLength(@self);
end;

function JSString.GetTwoByteStringCharsAndLength(cx: PJSContext;
  out len: size_t): PCChar16;
begin
  Result := JS_GetTwoByteStringCharsAndLength(cx, @nullPtr, @Self, @len);
end;

{ JSContext }
{$IFDEF SM52}
function JSContext.CheckForInterrupt: Boolean;
begin
  result := JS_CheckForInterrupt(@Self);
end;

procedure JSContext.DisableInterruptCallback;
begin
  JS_DisableInterruptCallback(@self);
end;

procedure JSContext.AddInterruptCallback(callback: JSInterruptCallback);
begin
  JS_AddInterruptCallback(@self, callback);
end;

procedure JSContext.ResetInterruptCallback(disable: boolean);
begin
  JS_ResetInterruptCallback(@self, disable);
end;

{$ENDIF}

procedure JSContext.ClearPendingException;
begin
  JS_ClearPendingException(@self);
end;

function JSContext.CurrentGlobalOrNull: PJSObject;
begin
  result := JS_CurrentGlobalOrNull(@self);
end;

procedure JSContext.Destroy;
begin
  JS_DestroyContext(@self);
end;

function JSContext.EnterCompartment(target: PJSObject): PJSCompartment;
begin
  Result := JS_EnterCompartment(@self, target);
end;

function JSContext.GetPendingException(out rv: jsval): boolean;
begin
  Result := JS_GetPendingException(@self, rv);
end;

function JSContext.GetPrivate: Pointer;
begin
  result := JS_GetContextPrivate(@self);
end;

function JSContext.IdToValue(id: jsid; out v: jsval): Boolean;
begin
  Result := JS_IdToValue(@Self, id, v);
end;

function JSContext.InitStandardClasses(var obj: PJSObject): boolean;
begin
  Result := JS_InitStandardClasses(@Self, obj);
end;

procedure JSContext.SetModuleResolveHook(var hook: PJSFunction);
begin
  JS_SetModuleResolveHook(@Self, hook);
end;

function JSContext.NewDateObjectMsec(msec: double): PJSObject;
begin
  Result := JS_NewDateObjectMsec(@Self, msec);
end;

procedure JSContext.LeaveCompartment(oldCompartment: PJSCompartment);
begin
  JS_LeaveCompartment(@Self, oldCompartment);
end;

procedure JSContext.MaybeGC;
begin
  JS_MaybeGC(@self);
end;

function JSContext.NewDateObject(year, mon, mday, hour, min, sec: int32): PJSObject;
begin
  Result := JS_NewDateObject(@Self, year, mon, mday, hour, min, sec);
end;

function JS_NewCompartmentOptions(): PJS_CompartmentOptions; cdecl; external SpiderMonkeyLib;
procedure JS_FreeCompartmentOptions(opt: PJS_CompartmentOptions); cdecl; external SpiderMonkeyLib;

function JSContext.NewGlobalObject(clasp: PJSClass): PJSObject;
var
  Opt: PJS_CompartmentOptions;
begin
  Opt := JS_NewCompartmentOptions;
  {$IFNDEF SM52}
  Opt.version := JSVERSION_LATEST;
  {$ENDIF}
  Result := JS_NewGlobalObject(@Self, clasp, nil, DontFireOnNewGlobalHook, Opt);
  JS_FreeCompartmentOptions(Opt);
end;

function JSContext.NewInt16Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewInt16Array(@Self, nelements);
end;

function JSContext.NewInt16ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewInt16ArrayFromArray(@Self, arr);
end;

function JSContext.NewInt16ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewInt16ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewInt32Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewInt32Array(@Self, nelements);
end;

function JSContext.NewInt32ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewInt32ArrayFromArray(@Self, arr);
end;

function JSContext.NewInt32ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewInt32ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewInt8Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewInt8Array(@Self, nelements);
end;

function JSContext.NewInt8ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewInt8ArrayFromArray(@Self, arr);
end;

function JSContext.NewInt8ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewInt8ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewJSString(TextWide: PWideChar;
  TextLen: integer): PJSString;
begin
  result := JS_NewUCStringCopyN(@Self, pointer(TextWide), TextLen);
end;

function JSContext.NewJSString(TextAnsi: PAnsiChar; TextLen,
  CodePage: integer): PJSString;
var short: array[byte] of CChar16; // to avoid temp allocation on heap
    buf: PCChar16;
begin
  if TextLen<(sizeof(short) div 3) then
    buf := @short else
    GetMem(buf,TextLen*3+2);
  result := JS_NewUCStringCopyN(@Self, buf,
    TSynAnsiConvert.Engine(CodePage).AnsiBufferToUnicode(PWideChar(buf),TextAnsi,TextLen)-buf);
  if buf<>@short then
    FreeMem(buf);
end;

function JSContext.NewJSString(const Value: SynUnicode): PJSString;
begin
  result := JS_NewUCStringCopyN(@Self, pointer(Value), Length(Value));
end;

function JSContext.NewObject(clasp: PJSClass): PJSObject;
begin
  Result := JS_NewObject(@Self, clasp);
end;

function JSContext.NewObjectWithGivenProto(clasp: PJSClass; var proto: PJSObject): PJSObject;
begin
  Result := JS_NewObjectWithGivenProto(@Self, clasp, proto);
end;

procedure JSContext.ReportError(format: PCChar);
begin
  JS_ReportError(@Self, format);
end;

procedure JSContext.ReportErrorNumberUC(errorCallback: JSErrorCallback; userRef: pointer; const erroNubmer: uintN);
begin
  JS_ReportErrorNumberUC(@Self, errorCallback, userRef, erroNubmer);
end;

procedure JSContext.ReportOutOfMemory;
begin
  JS_ReportOutOfMemory(@self);
end;

procedure JSContext.SetPrivate(const Value: Pointer);
begin
  JS_SetContextPrivate(@self,Value);
end;

function JSContext.TypeOfValue(v: jsval): JSType;
begin
  result := JS_TypeOfValue(@Self, v);
end;

function JSContext.ValueToId(var v: jsval; out id: jsid): Boolean;
begin
  Result := JS_ValueToId(@Self, v, id);
end;

function JSContext.WrapObject(var obj: PJSObject): boolean;
begin
  Result := JS_WrapObject(@Self, obj);
end;

procedure JSContext.BeginRequest;
begin
  JS_BeginRequest(@self);
end;

procedure JSContext.EndRequest;
begin
  JS_EndRequest(@self);
end;

function JSContext.GetArrayBufferViewBuffer(var obj: PJSObject;
  out isSharedMemory: Boolean): PJSObject;
begin
  Result := JS_GetArrayBufferViewBuffer(@self, obj, isSharedMemory);
end;

function JSContext.GetArrayBufferViewBuffer(var obj: PJSObject): PJSObject;
var
  isSharedMemory: Boolean;
begin
  Result := JS_GetArrayBufferViewBuffer(@self, obj, isSharedMemory);
end;

function JSContext.GetIsRunning: boolean;
begin
  result := JS_IsRunning(@self);
end;

procedure JSContext.FreeCompileOptions(opt: PJSCompileOptions);
begin
  JS_FreeCompileOptions(opt);
end;

procedure JSContext.FreeRootedObject(obj: PJSRootedObject);
var
  curr: PJSRootedObject;
begin
  curr := obj.stack.Last;
  while curr.ptr = nil do curr := curr.prev;
  if curr <> obj then
    raise ESMException.Create('FreeRootedObject Stack error');
  JS_FreeRootedObject(obj);
end;

procedure JSContext.FreeRootedString(str: PJSRootedString);
begin
  if ppointer(str.stack)^ <> str then
    raise ESMException.Create('FreeRootedString Stack error');
  JS_FreeRootedString(str);
end;

procedure JSContext.FreeRootedValue(str: PJSRootedValue);
begin
  if ppointer(str.stack)^ <> str then
    raise ESMException.Create('FreeRootedValue Stack error');
  JS_FreeRootedValue(str);
end;

function JSContext.NewRootedObject(obj: PJSObject): PJSRootedObject;
begin
  Result := JS_NewRootedObject(@Self, obj);
end;

function JSContext.NewRootedString(obj: PJSString): PJSRootedString;
begin
  Result := JS_NewRootedString(@Self, obj)
end;

function JSContext.NewRootedValue(val: jsval): PJSRootedValue;
begin
  Result := JS_NewRootedValue(@Self, val._l.asBits)
end;

function JSContext.NewSharedArrayBuffer(nbytes: uint32): PJSObject;
begin
  Result := JS_NewSharedArrayBuffer(@Self, nbytes);
end;

function JSContext.NewUint16Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewUint16Array(@Self, nelements);
end;

function JSContext.NewUint16ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewUInt16ArrayFromArray(@Self, arr);
end;

function JSContext.NewUint16ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewUInt16ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewUint32Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewUInt32Array(@Self, nelements);
end;

function JSContext.NewUint32ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewUInt32ArrayFromArray(@Self, arr);
end;

function JSContext.NewUint32ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewUInt32ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewUint8Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewuInt8Array(@Self, nelements);
end;

function JSContext.NewUint8ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewUInt8ArrayFromArray(@Self, arr);
end;

function JSContext.NewUint8ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewUInt8ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewUint8ClampedArray(nelements: uint32): PJSObject;
begin
  Result := JS_NewUint8ClampedArray(@Self, nelements);
end;

function JSContext.NewUint8ClampedArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewUint8ClampedArrayFromArray(@Self, arr);
end;

function JSContext.NewUint8ClampedArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewUint8ClampedArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

procedure strFinalizeOp(fin: PJSStringFinalizer; chars: PCChar16);  cdecl;
begin

end;

function JSContext.NewExternalString(const Value: SynUnicode): PJSString;
begin
  Result := JS_NewExternalString(@Self,  pointer(Value), length(Value), @strFinalizer);
end;

function JSContext.NewFloat32Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewFloat32Array(@Self, nelements);
end;

function JSContext.NewFloat32ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewFloat32ArrayFromArray(@Self, arr);
end;

function JSContext.NewFloat32ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewFloat32ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewFloat64Array(nelements: uint32): PJSObject;
begin
  Result := JS_NewFloat64Array(@Self, nelements);
end;

function JSContext.NewFloat64ArrayFromArray(var arr: PJSObject): PJSObject;
begin
  Result := JS_NewFloat64ArrayFromArray(@Self, arr);
end;

function JSContext.NewFloat64ArrayWithBuffer(var arrayBuffer: PJSObject;
  byteOffset: uint32; length: int32): PJSObject;
begin
  Result := JS_NewFloat64ArrayWithBuffer(@Self, arrayBuffer, byteOffset, length);
end;

function JSContext.NewFunction(call: JSNative; nargs, flags: uintN;
  name: PCChar): PJSObject;
begin
  Result := JS_NewFunction(@Self, call, nargs, flags, name);
end;

function JSContext.DefineDebuggerObject(var obj: PJSObject): boolean;
begin
  Result := JS_DefineDebuggerObject(@Self, obj);
end;

function JSContext.NewCompileOptions: PJSCompileOptions;
begin
  result := JS_NewCompileOptions(@self);
end;

function JSContext.CompileModule(var obj: PJSObject; opts: PJSCompileOptions;
  chars: PCChar16; length: size_t): PJSObject;
begin
  Result := JS_CompileModule(@Self, obj, opts, chars, length);
end;

function JSContext.CompileScript(bytes: PCChar;
  length: size_t; opts: PJSCompileOptions; out script: PJSScript): boolean;
begin
  Result := JS_CompileScript(@Self, bytes, length, opts, script);
end;

function JSContext.CompileUCScript(chars: PCChar16; length: size_t;
  opts: PJSCompileOptions; out script: PJSScript): boolean;
begin
  Result := JS_CompileUCScript(@Self, chars, length, opts, script);
end;

function JSContext.EvaluateScript(opts: PJSCompileOptions; bytes: PCChar; length: size_t;
  out rval: jsval): Boolean;
begin
  Result := JS_EvaluateScript(@Self, opts, bytes, length, rval);
end;

function JSContext.EvaluateUCScript(opts: PJSCompileOptions; chars: PCChar16; length: size_t;
  out rval: jsval): Boolean;
begin
  Result := JS_EvaluateUCScript(@Self, opts, chars, length, rval);
end;

function JSContext.ExecuteScript(var script: PJSScript; out rval: jsval): Boolean;
begin
  Result := JS_ExecuteScript(@Self, script, rval);
end;

function JSContext.New(var ctor: PJSObject; argc: uintN; argv: PjsvalVector): PJSObject;
var
    args: JSHandleValueArray;
begin
  args.length := argc;
  args.elements_ := argv;
  Result := JS_New(@self, ctor, args);
end;

function JSContext.NewArrayBuffer(nbytes: uint32): PJSObject;
begin
  Result := JS_NewArrayBuffer(@Self, nbytes);
end;

function JSContext.NewArrayObject(length: size_t;
  vector: PjsvalVector): PJSObject;
var
    contents: JSHandleValueArray;
begin
  contents.length := length;
  contents.elements_ := vector;
  Result := JS_NewArrayObject2(@Self, contents);
end;

function JSContext.NewArrayObject(length: size_t): PJSObject;
begin
  Result := JS_NewArrayObject(@Self, length);
end;

{$IFNDEF SM52}
function JSContext.GetRuntime: PJSRuntime;
begin
  result := JS_GetRuntime(@self);
end;
{$ENDIF}

function JSContext.InitCTypesClass(var obj: PJSObject): boolean;
begin
  Result := JS_InitCTypesClass(@Self, obj);
end;

function JSContext.InitReflectParse(var obj: PJSObject): boolean;
begin
  Result := JS_InitReflectParse(@Self, obj);
end;

function JSContext.InitModuleClasses(var obj: PJSObject): boolean;
begin
  Result := JS_InitModuleClasses(@Self, obj);
end;

function JSContext.NewJSString(const Value: RawUTF8): PJSString;
begin
  result := NewJSString(pointer(Value),length(Value),CP_UTF8);
end;

{$IFDEF SM52}
function JSContextOptions.getOptions(const Index: Integer): Boolean;
{$ELSE}
function JSRuntimeOptions.getOptions(const Index: Integer): Boolean;
{$ENDIF}
{ JSRuntimeOptions }
begin
   Result := (pword(@self)^ and (1 shl Index)) <> 0;
end;

{$IFDEF SM52}
procedure JSContextOptions.setOptions(const Index: Integer;
{$ELSE}
procedure JSRuntimeOptions.setOptions(const Index: Integer;
{$ENDIF}
  const Value: Boolean);
var
  val: uint16;
begin
  val := 1 shl Index;
  if Value then
    pword(@self)^ := pword(@self)^ or val
  else
    pword(@self)^ := pword(@self)^ and (not val);
end;

{$IFNDEF SM52}
{ JSRuntime }
procedure JSRuntime.Destroy;
begin
  JS_DestroyRuntime(@self);
end;
{$ENDIF}

{$IFDEF SM52}
procedure JSContext.GC;
{$ELSE}
procedure JSRuntime.GC;
{$ENDIF}
begin
  JS_GC(@self);
end;

{$IFDEF SM52}
function JSContext.GetEmptyString: PJSString;
{$ELSE}
function JSRuntime.GetEmptyString: PJSString;
{$ENDIF}
begin
  Result := JS_GetEmptyString(@self);
end;

{$IFDEF SM52}
function JSContext.GetWarningReporter: JSWarningReporter;
begin
  Result := JS_GetWarningReporter(@self);
end;
{$ELSE}
function JSRuntime.GetErrorReporter: JSErrorReporter;
begin
  Result := JS_GetErrorReporter(@self);
end;
{$ENDIF}

{$IFDEF SM52}
function JSContext.GetGCParameter(key: JSGCParamKey): uint32;
{$ELSE}
function JSRuntime.GetGCParameter(key: JSGCParamKey): uint32;
{$ENDIF}
begin
  Result := JS_GetGCParameter(@Self, key);
end;

{$IFDEF SM52}
{$ELSE}
function JSRuntime.GetInterruptCallback: JSInterruptCallback;
begin
  Result := JS_GetInterruptCallback(@self);
end;
{$ENDIF}

{$IFDEF SM52}
function JSContext.GetNowMs: int64;
{$ELSE}
function JSRuntime.GetNowMs: int64;
{$ENDIF}
begin
  Result := JS_Now;
end;

{$IFDEF SM52}
class function JSContext.CreateNew(maxbytes: uint32; maxNurseryBytes: uint32; parentContext: PJSContext): PJSContext;
begin
  with TSynFPUException.ForLibraryCode do begin
    Result := JS_NewContext(maxbytes, maxNurseryBytes, parentContext);
    InitSelfHostedCode(Result);
  end;
end;
{$ELSE}

function JSRuntime.GetPrivate: Pointer;
begin
  Result := JS_GetRuntimePrivate(@self);
end;

class function JSRuntime.new(maxbytes: uint32; maxNurseryBytes: uint32; parentRuntime: PJSRuntime): PJSRuntime;
begin
  Result := JS_NewRuntime(maxbytes, maxNurseryBytes, parentRuntime);
end;

function JSRuntime.NewContext(stackChunkSize: size_t): PJSContext;
begin
  Result := JS_NewContext(@Self, stackChunkSize)
end;
{$ENDIF}

{$IFDEF SM52}
function JSContext.GetOptions: PJSContextOptions;
begin
  Result := JS_GetContextOptions(@self);
end;
{$ELSE}
function JSRuntime.GetOptions: PJSRuntimeOptions;
begin
  Result := JS_GetRuntimeOptions(@self);
end;
{$ENDIF}


{$IFDEF SM52}
procedure JSContext.RequestInterruptCallback;
{$ELSE}
procedure JSRuntime.RequestInterruptCallback;
{$ENDIF}
begin
  JS_RequestInterruptCallback(@self);
end;

{$IFDEF SM52}
procedure JSContext.SetWarningReporter(reporter: JSWarningReporter);
begin
  JS_SetWarningReporter(@self, reporter);
end;
{$ELSE}
procedure JSRuntime.SetErrorReporter(er: JSErrorReporter);
begin
  JS_SetErrorReporter(@self, er);
end;
{$ENDIF}

{$IFDEF SM52}
procedure JSContext.SetGCParameter(key: JSGCParamKey; const Value: uint32);
{$ELSE}
procedure JSRuntime.SetGCParameter(key: JSGCParamKey; const Value: uint32);
{$ENDIF}
begin
  JS_SetGCParameter(@Self, key, Value);
end;

{$IFDEF SM52}
procedure JSContext.SetGCParametersBasedOnAvailableMemory(availMem: uint32);
{$ELSE}
procedure JSRuntime.SetGCParametersBasedOnAvailableMemory(availMem: uint32);
{$ENDIF}
begin
  JS_SetGCParametersBasedOnAvailableMemory(@Self, availMem);
end;

{$IFDEF SM52}
{$ELSE}
procedure JSRuntime.SetInterruptCallback(callback: JSInterruptCallback);
begin
  JS_SetInterruptCallback(@Self, callback);
end;
{$ENDIF}

{$IFDEF SM52}
procedure JSContext.SetNativeStackQuota(systemCodeStackSize: size_t);
{$ELSE}
procedure JSRuntime.SetNativeStackQuota(systemCodeStackSize: size_t);
{$ENDIF}
begin
  JS_SetNativeStackQuota(@Self, systemCodeStackSize);
end;

{$IFDEF SM52}

{$ELSE}
procedure JSRuntime.SetPrivate(const Value: Pointer);
begin
  JS_SetRuntimePrivate(@Self, Value);
end;
{$ENDIF}

{ JSObject }

function JSObject.isArray(cx: PJSContext): Boolean;
var
  _isArray: Boolean;
  obj: PJSObject;
begin
  obj := @Self;
  Result := JS_IsArrayObject(cx, obj, _isArray) and _isArray;
end;

function JSObject.IsArrayBufferObject: Boolean;
begin
  result := JS_IsArrayBufferObject(@self)
end;

function JSObject.IsArrayBufferViewObject: Boolean;
begin
  result := JS_IsArrayBufferViewObject(@self)
end;

function JSObject.isDate(cx: PJSContext): Boolean;
var
  _isDate: Boolean;
  obj: PJSObject;
begin
  obj := @Self;
  Result := JS_ObjectIsDate(cx, obj, _isDate) and _isDate;
end;

function JSObject.IsFloat32Array: Boolean;
begin
  result := JS_IsFloat32Array(@self);
end;

function JSObject.IsFloat64Array: Boolean;
begin
  result := JS_IsFloat64Array(@self);
end;

function JSObject.isFunction(cx: PJSContext): Boolean;
begin
  Result := JS_ObjectIsFunction(cx, @Self);
end;

function JSObject.IsInt16Array: Boolean;
begin
  result := JS_IsInt16Array(@self)
end;

function JSObject.IsInt32Array: Boolean;
begin
  result := JS_IsInt32Array(@self)
end;

function JSObject.IsInt8Array: Boolean;
begin
  result := JS_IsInt8Array(@self)
end;

function JSObject.IsMappedArrayBufferObject(obj: PJSObject): Boolean;
begin
  result := JS_IsMappedArrayBufferObject(@self);
end;

function JSObject.IsSharedArrayBufferObject: Boolean;
begin
  result := JS_IsSharedArrayBufferObject(@self)
end;

function JSObject.IsTypedArrayObject: Boolean;
begin
  result := JS_IsTypedArrayObject(@self)
end;

function JSObject.IsUint16Array: Boolean;
begin
  result := JS_IsUInt16Array(@self)
end;

function JSObject.IsUint32Array: Boolean;
begin
  result := JS_IsUInt32Array(@self);
end;

function JSObject.IsUint8Array: Boolean;
begin
  result := JS_IsUint8Array(@self)
end;

function JSObject.IsUint8ClampedArray: Boolean;
begin
  result := JS_IsUint8ClampedArray(@self)
end;

function JSObject.GetArrayBufferViewData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pointer;
begin
  result := JS_GetArrayBufferViewData(@self, isSharedMemory, nogc);
end;

function JSObject.RunMethod(cx: PJSContext; const name: PCChar;
  out rval: jsval): Boolean;
begin
  Result := CallFunctionName(cx, name, 0, nil, rval);
end;

function JSObject.RunMethod(cx: PJSContext; const name: PCChar; arg: jsval;
  out rval: jsval): Boolean;
begin
  Result := CallFunctionName(cx, name, 1, @arg, rval);
end;

function JSObject.RunMethod(cx: PJSContext; const name: PCChar;
  args: TjsvalDynArray; out rval: jsval): Boolean;
begin
  Result := CallFunctionName(cx, name, Length(args), @args[0], rval);
end;

function JSObject.GetInstancePrivate(cx: PJSContext; clasp: PJSClass): Pointer;
var
  obj: PJSObject;
begin
  obj := @Self;
  result := JS_GetInstancePrivate(cx, obj, clasp, nil);
end;

function JSObject.GetInt16ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pint16Vector;
begin
  result := JS_GetInt16ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetInt32ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pint32Vector;
begin
  result := JS_GetInt32ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetInt8ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pint8Vector;
begin
  result := JS_GetInt8ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetObjectAsArrayBuffer(out length: uint32;
  out Data: Puint8Vector): PJSObject;
begin
  result := JS_GetObjectAsArrayBuffer(@self, length, data);
end;

function JSObject.GetObjectAsArrayBufferView(out length: uint32;
  out isSharedMemory: Boolean; out Data: Puint8Vector): PJSObject;
begin
  result := JS_GetObjectAsArrayBufferView(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsFloat32Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Pfloat32Vector): PJSObject;
begin
  result := JS_GetObjectAsFloat32Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsFloat64Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Pfloat64Vector): PJSObject;
begin
  result := JS_GetObjectAsFloat64Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsInt16Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Pint16Vector): PJSObject;
begin
  result := JS_GetObjectAsInt16Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsInt32Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Pint32Vector): PJSObject;
begin
  result := JS_GetObjectAsInt32Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsInt8Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Pint8Vector): PJSObject;
begin
  result := JS_GetObjectAsInt8Array(@self, length, isSharedMemory, data)
end;

function JSObject.GetObjectAsUint16Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Puint16Vector): PJSObject;
begin
  result := JS_GetObjectAsUInt16Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsUint32Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Puint32Vector): PJSObject;
begin
  result := JS_GetObjectAsUint32Array(@self, length, isSharedMemory, data);
end;

function JSObject.GetObjectAsUint8Array(out length: uint32;
  out isSharedMemory: Boolean; out Data: Puint8Vector): PJSObject;
begin
  result := JS_GetObjectAsUInt8Array(@self, length, isSharedMemory, data)
end;

function JSObject.GetObjectAsUint8ClampedArray(out length: uint32;
  out isSharedMemory: Boolean; out Data: Puint8Vector): PJSObject;
begin
  result := JS_GetObjectAsUint8ClampedArray(@self, length, isSharedMemory, data)
end;

function JSObject.GetPrivate: Pointer;
begin
  Result := JS_GetPrivate(@self);
end;

function JSObject.GetProperty(cx: PJSContext; const name: PCChar;
  out vp: jsval): boolean;
var
  obj: PJSObject;
begin
  obj := @Self;
  Result := JS_GetProperty(cx, obj, name, vp);
end;

function JSObject.GetPropertyById(cx: PJSContext; const id: jsid;
  out vp: jsval): boolean;
var
  obj: PJSObject;
  _id: jsid;
begin
  obj := @Self;
  _id := id;
  Result := JS_GetPropertyById(cx, obj, _id, vp);
end;

function JSObject.GetPropValue(cx: PJSContext; const name: SynUnicode): jsval;
begin
  {$ifdef WITHASSERT}
  Assert(
  {$ENDIF}
  GetUCProperty(cx, Pointer(name), Length(name), Result)
  {$ifdef WITHASSERT}
  );
  {$ENDIF}
end;

function JSObject.GetPrototype(cx: PJSContext; out protop: PJSObject): Boolean;
var
  obj: PJSObject;
begin
  obj := @Self;
  Result := JS_GetPrototype(cx, obj, protop);
end;

function JSObject.GetReservedSlot(index: uint32): jsval;
begin
{$IFDEF SM52}
  result._l.asBits := JS_GetReservedSlot(@Self, index)
{$ELSE}
  result._l.asBits := JS_GetReservedSlot(@Self, index)
{$ENDIF}
end;

function JSObject.GetSharedArrayBufferByteLength: uint32;
begin
  result := JS_GetSharedArrayBufferByteLength(@self);
end;

//function JSObject.GetSharedArrayBufferViewType: JSArrayBufferViewType;
//begin
//  result := JS_GetSharedArrayBufferViewType(@self);
//end;

function JSObject.GetTypedArrayByteLength: uint32;
begin
  result := JS_GetTypedArrayLength(@self);
end;

function JSObject.GetTypedArrayByteOffset: uint32;
begin
  result := JS_GetTypedArrayByteOffset(@self);
end;

function JSObject.GetTypedArrayLength: uint32;
begin
  result := JS_GetTypedArrayLength(@self);
end;

function JSObject.GetTypedArraySharedness: Boolean;
begin
  result := JS_GetTypedArraySharedness(@self)
end;

function JSObject.AlreadyHasOwnUCProperty(cx: PJSContext; const name: PCChar16;
  namelen: size_t): Boolean;
var
  obj: PJSObject;
  foundp: Boolean;
begin
  obj := @Self;
  Result := JS_AlreadyHasOwnUCProperty(cx, obj, name, namelen, foundp) and foundp;
end;

function JSObject.ArrayBufferHasData: Boolean;
begin
  result := JS_ArrayBufferHasData(@self);
end;

function JSObject.CallFunction(cx: PJSContext; var fun: PJSFunction;
  argc: uintN; argv: PjsvalVector; out rval: jsval): Boolean;
var obj: PJSObject;
    args: JSHandleValueArray;
begin
  obj := @Self;
  args.length := argc;
  args.elements_ := argv;
  Result := JS_CallFunction(cx, obj, fun, args, rval);
end;

function JSObject.CallFunctionName(cx: PJSContext; const name: PCChar;
  argc: uintN; argv: PjsvalVector; out rval: jsval): Boolean;
var obj: PJSObject;
    args: JSHandleValueArray;
begin
  obj := @Self;
  args.length := argc;
  args.elements_ := argv;
  Result := JS_CallFunctionName(cx, obj, name, args, rval);
end;

function JSObject.CallFunctionValue(cx: PJSContext; val: jsval; argc: uintN;
  argv: PjsvalVector; out rval: jsval): Boolean;
var obj: PJSObject;
    args: JSHandleValueArray;
begin
  obj := @Self;
  args.length := argc;
  args.elements_ := argv;
  Result := JS_CallFunctionValue(cx, obj, val, args, rval);
end;

const
  /// API extension: OR this into indent to avoid pretty-printing the decompiled
  // source resulting from JS_DecompileFunction{,Body}.
  JS_DONT_PRETTY_PRINT = $8000;
  prettyPrintAr: array[boolean] of uintN = (JS_DONT_PRETTY_PRINT, 0);
function JSObject.DecompileFunction(cx: PJSContext;
  PrettyPrint: Boolean): PJSString;
var
  fun: PJSFunction;
begin
  fun := @self;
  Result := JS_DecompileFunction(cx, fun, prettyPrintAr[PrettyPrint]);
end;

function JSObject.DefineFunction(cx: PJSContext; name: PCChar; call: JSNative;
  nargs, attrs: uintN): PJSFunction;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_DefineFunction(cx, obj, name, call, nargs, attrs);
end;

function JSObject.DefineFunctions(cx: PJSContext; fs: PJSFunctionSpec;
  behavior: JSPropertyDefinitionBehavior): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_DefineFunctions(cx, obj, fs, behavior);
end;

function JSObject.DefineProperties(cx: PJSContext;
  ps: PJSPropertySpec): boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DefineProperties(cx, obj, ps)
end;

function JSObject.DefineProperty(cx: PJSContext; const name: PCChar;
  const value: jsval; attrs: uint32; getter, setter: JSNative): boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DefineProperty(cx, obj, name, pjsval(@value)^, attrs, getter, setter)
end;

function JSObject.DefinePropertyById(cx: PJSContext; var id: jsid;
  const value: jsval; attrs: uint32; getter, setter: JSNative): boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DefinePropertyById(cx, obj, id, pjsval(@value)^, attrs, getter, setter)
end;

function JSObject.DefineUCFunction(cx: PJSContext; name: PCChar16;
  namelen: size_t; call: JSNative; nargs, attrs: uintN): PJSFunction;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_DefineUCFunction(cx, obj, name, namelen, call, nargs, attrs);
end;

function JSObject.DefineUCProperty(cx: PJSContext; const name: SynUnicode;
  const value: jsval; attrs: uint32; getter, setter: JSNative): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DefineUCProperty(cx, obj, Pointer(name), Length(name), pjsval(@value)^, attrs, getter, setter)
end;

function JSObject.DefineUCProperty(cx: PJSContext; const name: PCChar16;
  namelen: size_t; const value: jsval; attrs: uint32; getter,
  setter: JSNative): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DefineUCProperty(cx, obj, name, namelen, pjsval(@value)^, attrs, getter, setter)
end;

function JSObject.DeleteElement(cx: PJSContext; index: uint32;
  out res: JS_ObjectOpResult): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_DeleteElement(cx, obj, index, res);
end;

function JSObject.DeletePropertyById(cx: PJSContext; const id: jsid;
  out res: JS_ObjectOpResult): Boolean;
var obj: PJSObject;
    _id: jsid;
begin
  obj := @Self;
  _id := id;
  Result := JS_DeletePropertyById(cx, obj, _id, res);
end;

function JSObject.Enumerate(cx: PJSContext): JSIdArray;
var obj: PJSObject;
begin
  obj := @Self;
  Result.init(cx);
  {$ifdef WITHASSERT}
  Assert(
  {$ENDIF}
  JS_Enumerate(cx, obj, Result)
  {$ifdef WITHASSERT}
  );
  {$ENDIF}
end;

function JSObject.GetUCProperty(cx: PJSContext; const name: PCChar16;
  namelen: size_t; out vp: jsval): boolean;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_GetUCProperty(cx, obj, name, namelen, vp);
end;

function JSObject.GetUint16ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Puint16Vector;
begin
  result := JS_GetUInt16ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetUint32ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Puint32Vector;
begin
  result := JS_GetUint32ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetUint8ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Puint8Vector;
begin
  result := JS_GetUInt8ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetUint8ArrayData: Puint8Vector;
var isShared: Boolean;
begin
  result := JS_GetUInt8ArrayData(@self, isShared, nil);
end;

function JSObject.GetUint8ClampedArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Puint8Vector;
begin
  result := JS_GetUint8ClampedArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.HasInstance(cx: PJSContext; var val: jsval): Boolean;
var obj: PJSObject;
    res: Boolean;
begin
  obj := @Self;
  Result := JS_HasInstance(cx, obj, val, res) and res;
end;

function JSObject.HasProperty(cx: PJSContext; const name: PCChar): Boolean;
var obj: PJSObject;
    found: Boolean;
begin
  obj := @Self;
  Result := JS_HasProperty(cx, obj, name, found) and found;
end;

function JSObject.HasUCProperty(cx: PJSContext; const name: PCChar16;
  namelen: size_t; out found: Boolean): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_HasUCProperty(cx, obj, name, namelen, found);
end;

function JSObject.InitClass(cx: PJSContext; var parent_proto: PJSObject;
  clasp: PJSClass; _constructor: JSNative; nargs: Cardinal; ps: PJSPropertySpec;
  fs: PJSFunctionSpec; static_ps: PJSPropertySpec;
  static_fs: PJSFunctionSpec): PJSObject;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_InitClass(cx, obj, parent_proto, clasp, _constructor, nargs, ps, fs, static_ps, static_fs);
end;

function JSObject.GetArrayBufferByteLength: uint32;
begin
  result := JS_GetArrayBufferByteLength(@self);
end;

function JSObject.GetArrayBufferData: Puint8Vector;
var isShared: Boolean;
begin
  result := JS_GetArrayBufferData(@self, isShared, nil);
end;

function JSObject.GetArrayBufferData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Puint8Vector;
begin
  result := JS_GetArrayBufferData(@self, isSharedMemory, nogc);
end;

function JSObject.GetArrayBufferViewByteLength: uint32;
begin
  result := JS_GetArrayBufferViewByteLength(@self);
end;

function JSObject.GetArrayBufferViewType: JSArrayBufferViewType;
begin
  result := JS_GetArrayBufferViewType(@self);
end;

function JSObject.GetArrayLength(cx: PJSContext; var length: uint32): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_GetArrayLength(cx, obj, length);
end;

function JSObject.GetClass: PJSClass;
begin
  result := JS_GetClass(@self);
end;

function JSObject.GetConstructor(cx: PJSContext): PJSObject;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_GetConstructor(cx, obj);
end;

function JSObject.GetElement(cx: PJSContext; index: uint32;
  out vp: jsval): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_GetElement(cx, obj, index, vp);
end;

function JSObject.GetFloat32ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pfloat32Vector;
begin
  result := JS_GetFloat32ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetFloat64ArrayData(out isSharedMemory: Boolean;
  nogc: PJSAutoCheckCannotGC): Pfloat64Vector;
begin
  result := JS_GetFloat64ArrayData(@self, isSharedMemory, nogc);
end;

function JSObject.GetFunctionId: PJSString;
begin
  Result := JS_GetFunctionId(@self);
end;

function JSObject.SetElement(cx: PJSContext; index: uint32;
  const vp: jsval): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  result := JS_SetElement(cx, obj, index, pjsval(@vp)^);
end;

procedure JSObject.SetPrivate(data: Pointer);
begin
  JS_SetPrivate(@self, data);
end;

function JSObject.SetProperty(cx: PJSContext; const name: PCChar;
  const vp: jsval): Boolean;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_SetProperty(cx, obj, name, pjsval(@vp)^);
end;

function JSObject.SetPrototype(cx: PJSContext; var proto: PJSObject): Boolean;
var
  obj: PJSObject;
begin
  obj := @Self;
  Result := JS_SetPrototype(cx, obj, proto);
end;

procedure JSObject.SetReservedSlot(index: uint32; v: jsval);
begin
{$IFDEF SM52}
  JS_SetReservedSlot(@Self, index, v);
{$ELSE}
  JS_SetReservedSlot(@Self, index, v._l.asBits);
{$ENDIF}
end;

function JSObject.SetUCProperty(cx: PJSContext; const name: PCChar16;
  namelen: size_t; const vp: jsval): boolean;
var obj: PJSObject;
begin
  obj := @Self;
  Result := JS_SetUCProperty(cx, obj, name, namelen, pjsval(@vp)^);
end;

function JSObject.ToJSValue: jsval;
begin
  Result.asObject := @self;
end;

function JSObject.GetBufferDataAndLength(out data: Puint8Vector; out len: uint32): boolean;
var
  isShared: boolean;
begin
  if Self.IsArrayBufferViewObject then begin
    Result := Self.GetObjectAsArrayBufferView(len, isShared, data) <> nil;
  end else if Self.IsArrayBufferObject then begin
    data := Self.GetArrayBufferData;
    len := Self.GetArrayBufferByteLength;
    Result := True
  end else
    Result := False;
end;

{ ESMException }

constructor ESMException.CreateWithTrace(const AFileName: RawUTF8; AJSErrorNum, ALineNum: integer; AMessage: string; const AStackTrace: SynUnicode);
begin
  Create(AMessage);
  FJSErrorNum := AJSErrorNum;
  if AFileName='' then
    FFileName := '<>'
  else
    FFileName := AFileName;
  FLineNum := ALineNum;
  FJSStackTrace := AStackTrace;
end;

procedure ESMException.WriteFormatted(WR: TTextWriter);
{$IFNDEF SM_DEBUG}
var
  P, Pb: PWord;
{$ENDIF}
begin
  WR.AddJSONEscape(pointer(FileName), Length(fileName));
    WR.Add(':'); WR.Add(Line);
  WR.AddShort('\n\nError: ');
    WR.AddJSONEscapeString(Message); WR.AddShort('\n');
  {$IFDEF SM_DEBUG}
    WR.AddJSONEscapeString(Stack);
  {$ELSE}
    // any stack line what don't start with `@` is internal (core_modules) calls
    // remove it to simplify domain logic debugging
    if length(Stack) > 0 then begin
      P := PWord(pointer(Stack));
      while P^ <> 0 do begin
        if (P^ = Ord('@')) then begin
          Pb := P;
          while (P^ <> 10) and (P^ <> 0) do Inc(P);
          if (P^ = 10) then Inc(P);
          WR.AddJSONEscapeW(Pb, (PtrUInt(P)-PtrUInt(Pb)) div 2);
        end else
          while (P^ <> 10) and (P^ <> 0) do
            Inc(P);
          if (P^ = 10) then Inc(P);
      end;
    end;
  {$ENDIF}
end;

{$ifndef NOEXCEPTIONINTERCEPT}
function ESMException.CustomLog(
  WR: TTextWriter; const Context: TSynLogExceptionContext): boolean;
begin
  (Context.EInstance as ESMException).WriteFormatted(WR);
  result := true; // do not append a address
end;
{$endif}

{ JSArgRec }

function JSArgRec.getArgv: PjsvalVector;
begin
  Result := @rec.argv;
end;

function JSArgRec.getCalleObject: PJSObject;
begin
  Result := rec.calle.asObject;
end;

function JSArgRec.GetIsConstructing: Boolean;
begin
  Result := rec.this.IsMagic;
end;

function JSArgRec.getThis(cx: PJSContext): jsval;
begin
  if rec.this.IsPrimitive then
    result._l.asBits := JS_ComputeThis(cx, rec.calle) else
    result := rec.this;
end;

function JSArgRec.getThisObject(cx: PJSContext): PJSObject;
begin
  Result := this[cx].asObject;
end;

{ jsid }

const
  JSID_TYPE_MASK = $7;

function jsid.isString: Boolean;
begin
  Result := JSIdType(asBits and JSID_TYPE_MASK) = JSID_TYPE_STRING;
end;

function jsid.asJSString: PJSString;
begin
{$ifdef WITHASSERT}
  Assert(isString);
{$endif}
  Result := PJSString(asBits);
end;

{ JSIdArray }

procedure JSIdArray.init(cx: PJSContext);
begin
  _internal.cx := cx;
  _internal.mBegin := @_internal.mStorage;
  _internal.mLength := 0;
  _internal.mCapacity := 1;
end;


{ jsval }

const
{$ifdef CPU64}
  JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET         = JSVAL_TAG_NULL;
  JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET           = JSVAL_TAG_OBJECT;

  JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_NUMBER_SET      = JSVAL_SHIFTED_TAG_UNDEFINED;
  JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET   = JSVAL_SHIFTED_TAG_OBJECT;
{$else}
  JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET         = JSVAL_TAG_NULL;
  JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET           = JSVAL_TAG_OBJECT;
  JSVAL_UPPER_INCL_TAG_OF_NUMBER_SET              = JSVAL_TAG_INT32;
  JSVAL_LOWER_INCL_TAG_OF_GCTHING_SET             = JSVAL_TAG_STRING;
{$endif}

function jsval.getAsBoolean: Boolean;
begin
  {$ifdef WITHASSERT}
  assert(isBoolean);
  {$endif}
  {$ifdef  CPU64}
  Result := Boolean(_l.asBits and JSVAL_PAYLOAD_MASK)
  {$else}
  Result := Boolean(_l.s.payload.boo);
  {$endif}
end;

function jsval.getAsDate(cx: PJSContext): TDateTime;
var oDate: PJSObject;
{$ifdef CONSIDER_TIME_IN_Z} // as defined in SyNode.inc
    ms: double;
    ms64: Int64;
    fval: jsval;
{$else}
    d, m, Y, h, mn, s, ml: Integer;
    v, fval: jsval;
  function GetIntFuncPropVal(funcName: PWideChar): Integer;
  begin
    Result := 0;
    if oDate.GetUCProperty(cx, pointer(funcName), Length(funcName), fval) then
      if oDate.CallFunctionValue(cx, fval, 0, nil, v)   then
        Result := v.asInteger;
  end;
{$endif}
begin
  oDate := getAsObject;
  if not oDate.isDate(cx) then
    raise ESMException.create('not a DateTime object');
{$ifdef CONSIDER_TIME_IN_Z}
  ms := 0;
  if oDate.CallFunctionName(cx, PCChar('getTime'), 0, nil, fval) then
    ms := fval.asDouble;
  if ms = 0 then
    raise ESMException.Create('JSDateToDateTime: no getTime() in Date object');
  ms64 := Trunc(ms);
  // W/O millisec: Result := IncMilliSecond(UnixDateDelta, ms64);
  Result := UnixMSTimeToDateTime(ms64);
{$else}
  d := GetIntFuncPropVal('getDate');
  m := GetIntFuncPropVal('getMonth') + 1; //WTF months start from 0
  Y := GetIntFuncPropVal('getFullYear');
  h := GetIntFuncPropVal('getHours');
  mn := GetIntFuncPropVal('getMinutes');
  s := GetIntFuncPropVal('getSeconds');
  ml := GetIntFuncPropVal('getMilliseconds');
  Result := EncodeDateTime(Y, m, d, h, mn, s, ml);
{$endif}
end;

function jsval.getAsDouble: Double;
begin
  {$ifdef WITHASSERT}
  assert(isDouble);
  {$endif}
  Result := _l.asDouble;
end;

function jsval.getAsInteger: Integer;
begin
  {$ifdef WITHASSERT}
  assert(isInteger);
  {$endif}
  {$ifdef CPU64}
  Result := int32(_l.asBits);
  {$else}
  Result := _l.s.payload.i32;
  {$endif}
end;

function writeCallback(const buf: PCChar16; len: uint32; data: pointer): Boolean; cdecl;
begin
  TTextWriter(data).AddNoJSONEscapeW(pointer(buf),len);
  result := true;
end;

procedure jsval.AddJSON(cx: PJSContext; W: TTextWriter);
var
//  voidVal: jsval;
  T: JSType;
begin
  if @self=nil then
    W.AddShort('null')
  else begin
    T := cx.TypeOfValue(self);
    case T of
      JSTYPE_VOID,
      JSTYPE_NULL:
        W.AddShort('null');
      JSTYPE_STRING:
        asJSString.ToJSONString(cx,W);
      JSTYPE_NUMBER:
        if isInteger then
          W.Add(asInteger) else
          W.AddDouble(asDouble);
      JSTYPE_BOOLEAN:
        W.Add(asBoolean);
      JSTYPE_OBJECT,
      JSTYPE_FUNCTION: begin
        if not Stringify(cx, nullObj, JSVAL_VOID, writeCallback, pointer(W)) then
          ;
      end
      else
        raise ESMException.CreateFmt('Unhandled AddJSON(%d)',[ord(T)]);
    end;
  end;
end;

function jsval.getAsJson(cx: PJSContext): RawUTF8;
var W: TJSONWriter;
begin
  W := TJSONWriter.CreateOwnedStream;
  try
    AddJSON(cx,W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function jsval.getAsInt64: Int64;
begin
  if isInteger then
    result := asInteger else
  {$ifdef WITHASSERT}
  if not isDouble then
    raise ESMException.Create('jsval.getAsInt64!') else
  {$endif}
    result := trunc(asDouble);
end;

function jsval.getAsObject: PJSObject;
{$ifdef  CPU64}
var  ptrBits: UInt64 absolute Result;
{$endif}
begin
  {$ifdef WITHASSERT}
  assert(isObject);
  {$endif}
  {$ifdef CPU64}
  ptrBits := _l.asBits and JSVAL_PAYLOAD_MASK;
  {$ifdef WITHASSERT}
  assert(ptrBits and 7 = 0);
  {$endif}
  {$else}
  Result := _l.s.payload.obj;
  {$endif}
end;

function jsval.getIsBoolean: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits shr JSVAL_TAG_SHIFT = JSVAL_TAG_BOOLEAN;
  {$else}
  Result := _l.s.tag = JSVAL_TAG_BOOLEAN;
  {$endif}
end;

function jsval.getIsDouble: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits <= JSVAL_SHIFTED_TAG_MAX_DOUBLE
  {$else}
  Result := UInt32(_l.s.tag) <= UInt32(JSVAL_TAG_CLEAR)
  {$endif}
end;

function jsval.getIsInteger: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits shr JSVAL_TAG_SHIFT = JSVAL_TAG_INT32;
  {$else}
  Result := _l.s.tag = JSVAL_TAG_INT32;
  {$endif}
end;

function jsval.getIsNull: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits  = JSVAL_SHIFTED_TAG_NULL;
  {$else}
  Result := _l.s.tag = JSVAL_TAG_NULL;
  {$endif}
end;

function jsval.getIsNumber: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits  < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_NUMBER_SET;
  {$else}
  {$ifdef WITHASSERT}
  assert(_l.s.tag <> JSVAL_TAG_CLEAR);
  {$endif}
  Result := (UInt32(_l.s.tag) <= UInt32(JSVAL_UPPER_INCL_TAG_OF_NUMBER_SET));
  {$endif}
end;

function jsval.getIsObject: Boolean;
begin
  {$ifdef CPU64}
  {$ifdef WITHASSERT}
  Assert(_l.asBits shr JSVAL_TAG_SHIFT <= JSVAL_TAG_OBJECT);
  {$endif}
  Result := _l.asBits >= JSVAL_SHIFTED_TAG_OBJECT;
  {$else}
  {$ifdef WITHASSERT}
  Assert(_l.s.tag <= JSVAL_TAG_OBJECT);
  {$endif}
  Result := (UInt32(_l.s.tag) >= UInt32(JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET));
  {$endif}
end;

function jsval.getIsSimpleVariant(cx: PJSContext): Boolean;
var
  t: JSType;
begin
  t := ValType(cx);
  Result := (t = JSTYPE_VOID) or (t = JSTYPE_NULL) or (t = JSTYPE_STRING)
      or (t = JSTYPE_NUMBER) or (t = JSTYPE_BOOLEAN);
end;

function jsval.getIsString: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits shr JSVAL_TAG_SHIFT = JSVAL_TAG_STRING;
  {$else}
  Result := _l.s.tag = JSVAL_TAG_STRING;
  {$endif}
end;

function jsval.getIsVoid: Boolean;
begin
  {$ifdef CPU64}
  Result := _l.asBits = JSVAL_SHIFTED_TAG_UNDEFINED;
  {$else}
  Result := _l.s.tag = JSVAL_TAG_UNDEFINED;
  {$endif}
end;

function jsval.getJSString: PJSString;
begin
  {$ifdef CPU64}
  Result := PJSString(_l.asBits and JSVAL_PAYLOAD_MASK);
  {$else}
  {$ifdef WITHASSERT}
  Assert(isString);
  {$endif}
  Result := _l.s.payload.str;
  {$endif}
end;

function jsval.getPrivate: Pointer;
begin
  {$ifdef CPU64}
  {$ifdef WITHASSERT}
  Assert(_l.asBits and $8000000000000000 = 0);
  {$endif}
  Result := Pointer(_l.asBits shl 1);
  {$else}
  {$ifdef WITHASSERT}
  Assert(isDouble);
  {$endif}
  Result := _l.s.payload.ptr;
  {$endif}
end;

function jsval.getSimpleVariant(cx: PJSContext): Variant;
var
  t: JSType;
begin
  t := ValType(cx);
  case t of
    JSTYPE_VOID:
      VarClear(result);
    JSTYPE_NULL:
      SetVariantNull(result);
//    JSTYPE_OBJECT:
// todo
    JSTYPE_STRING:
       getJSString.ToVariant(cx,result);
    JSTYPE_NUMBER:
      if getIsInteger then
        result := getAsInteger else
        result := getAsDouble;
    JSTYPE_BOOLEAN:
      result := getAsBoolean;
//    JSTYPE_FUNCTION:
// todo
    else
      raise ESMException.CreateFmt('Unhandled ToVariant(%d)',[ord(t)]);
  end;

end;

function jsval.IsMagic: Boolean;
begin
  {$IFDEF CPU64}
  Result := _l.asBits shr JSVAL_TAG_SHIFT = JSVAL_TAG_MAGIC;
  {$ELSE}
  Result := _l.s.tag = JSVAL_TAG_MAGIC;
  {$ENDIF}
end;

function jsval.IsPrimitive: Boolean;
begin
  {$IFDEF CPU64}
  Result := _l.asBits < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET;
  {$ELSE}
  Result := (UInt32(_l.s.tag) < UInt32(JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET));
  {$ENDIF}
end;

function doubleIsInt(Value: Double):boolean; {$ifdef HASINLINE}inline;{$endif}
var Value_int64:Int64 absolute Value;
    len: Int16;
begin
  len := (Value_int64 shr 52) and $7FF - $3FF;
  result := ((len >= 0) and (len < 31) and ((Value_int64 and (QWORD(1) shl (52 - len)-1)) = 0))
  or (Value_int64 = 0) or (Value_int64 = $C1E0000000000000);
end;

function doubleToInt(Value: Double):Integer; {$ifdef HASINLINE}inline;{$endif}
var Value_int64:Int64 absolute Value;
    len: Smallint;
begin
  if Value_int64 = 0 then
    result := 0
  else begin
    len := (Value_int64 shr 52) and $7FF - $3FF;
    Result := (Value_int64 and $000FFFFFFFFFFFFF or $0010000000000000) shr (52-len);
    if (Value_int64 and $8000000000000000)<>0 then
      Result := -Result;
  end
end;

procedure jsval.setAsBoolean(const Value: Boolean);
begin
  {$ifdef CPU64}
  _l.asBits := uint64(ord(Value)) or JSVAL_SHIFTED_TAG_BOOLEAN;
  {$else}
  _l.s.tag := JSVAL_TAG_BOOLEAN;
  _l.s.payload.boo := ord(Value);
  {$endif}
end;

procedure jsval.setAsDate(cx: PJSContext; const Value: TDateTime);
var dmsec: double;
    unixTime: Int64;
  {$ifdef CONSIDER_TIME_IN_Z} // as defined in SyNode.inc
    oDate: PJSObject;
{$else}
    // this realisation is buggy - it ignores timezone rules change history
    // for server-side realisation the best solution is to use GMT time here
    ms: Word;
    STLocal, STUtc: TSystemTime;
    TZ: TTimeZoneInformation;
    AUTCDateTime: TDateTime;
{$endif}
begin
{$ifdef CONSIDER_TIME_IN_Z}
  unixTime := DateTimeToUnixMSTime(Value);
  dmsec := unixTime-(unixTime mod 1000);
  oDate := cx.NewDateObjectMsec(dmsec);
  if not oDate.IsDate(cx) then
    raise ESMException.CreateFmt('SetDateTime(%g): not a valid date',[Value]);
  setAsObject(oDate);
{$else}
  DateTimeToSystemTime(Value, STLocal);
  GetTimeZoneInformation(TZ);
  // use TzSpecificLocalTimeToSystemTime?
  TZ.Bias := -TZ.Bias;
  TZ.StandardBias := -TZ.StandardBias;
  TZ.DaylightBias := -TZ.DaylightBias;
  SystemTimeToTzSpecificLocalTime(@TZ, STLocal, STUtc);
  ms := STUtc.wMilliseconds;
  AUTCDateTime := SystemTimeToDateTime(STUtc);
  dmSec := DateTimeToUnixMSTime(AUTCDateTime) + ms;
  setAsObject(cx.NewDateObjectMsec(dmsec));
{$endif}
end;

procedure jsval.setAsDouble(const Value: Double);
begin
  if doubleIsInt(Value) then
    asInteger := doubleToInt(Value)
  else begin
    _l.asDouble := Value;
    if ((_l.asBits and $7FF0000000000000) = $7FF0000000000000) and
       ((_l.asBits and $000FFFFFFFFFFFFF) <> $0000000000000000) then
      _l.asBits := JSVAL_NAN_impl; // canonize NaN
  end;
end;

procedure jsval.setAsInteger(const Value: Integer);

begin
  {$ifdef CPU64}
  _l.asBits := Value;
  _l.asBits := uint32(Value) or JSVAL_SHIFTED_TAG_INT32;
  {$else}
  _l.s.tag := JSVAL_TAG_INT32;
  _l.s.payload.i32 := Value;
  {$endif}
end;

procedure jsval.setAsJson(cx: PJSContext; const Value: RawUTF8);
var tmp: RawUnicode;
    len: integer;
begin
  if Value='' then begin
    SetVoid;
  end else begin
    len := Utf8DecodeToRawUnicodeUI(Value,tmp);
    if not JS_ParseJSON(cx, pointer(tmp), len shr 1, self) then
    {$ifdef WITHASSERT}
      raise ESMException.Create('jsval.setAsJson!') else
    {$ELSE}
      SetVoid;
    {$endif}
  end;
end;

procedure jsval.setAsInt64(const Value: Int64);
begin
  if (Value>=Low(integer)) and (Value<=High(integer)) then
    setAsInteger(Value) else
    setAsDouble(Value);
end;

procedure jsval.setAsObject(const Value: PJSObject);
{$ifdef CPU64}
var objBits: UInt64 absolute Value;
{$endif}
begin
  {$ifdef CPU64}
  {$ifdef WITHASSERT}
  Assert(objBits shr JSVAL_TAG_SHIFT = 0);
  {$endif}
  _l.asBits := QWord(objBits or JSVAL_SHIFTED_TAG_OBJECT);
  {$else}
  if Value<>nil then begin
    _l.s.tag := JSVAL_TAG_OBJECT;
    _l.s.payload.obj := Value;
  end else
    _l.asBits := JSVAL_NULL_impl;
  {$endif}
end;

procedure jsval.setJSString(const Value: PJSString);
{$ifdef CPU64}
var strBits: UInt64 absolute Value;
{$endif}
begin
  {$ifdef CPU64}
  {$ifdef WITHASSERT}
  Assert(strBits shr JSVAL_TAG_SHIFT = 0);
  {$endif}
  _l.asBits := strBits or JSVAL_SHIFTED_TAG_STRING;
  {$else}
  {$ifdef WITHASSERT}
  assert(str<>nil) ;
  {$endif}
  _l.s.tag := JSVAL_TAG_STRING;
  _l.s.payload.str := Value;
  {$endif}
end;

procedure jsval.setNull;
begin
  _l.asBits := JSVAL_NULL_impl;
end;

procedure jsval.setPrivate(const Value: Pointer);
{$ifdef CPU64}
var ptrBits: UInt64 absolute Value;
{$endif}
begin
  {$ifdef CPU64}
  {$ifdef WITHASSERT}
  Assert(ptrBits and 1 = 0);
  {$endif}
  _l.asBits := ptrBits shr 1;
  {$ifdef WITHASSERT}
  assert(isDouble);
  {$endif}
  {$else}
  {$ifdef WITHASSERT}
  assert((uint32(ptr) and 1) = 0);
  {$endif}
  _l.s.tag := JSValueTag(0);
  _l.s.payload.ptr := Value;
  {$ifdef WITHASSERT}
  assert(isDouble);
  {$endif}
  {$endif}
end;

procedure jsval.setSimpleVariant(cx: PJSContext; const Value: Variant);
begin
  with TVarData(Value) do
  case VType of
    varNull: setNull;
    varEmpty: setVoid;
    varBoolean: setAsBoolean(VBoolean);
    varSmallint: setAsInteger(VSmallInt);
    {$ifndef DELPHI5OROLDER}
    varShortInt: setAsInteger(VShortInt);
    varWord: setAsInteger(VWord);
    varLongWord: setAsInt64(VLongWord);
    {$endif}
    varByte: setAsInteger(VByte);
    varInteger: setAsInteger(VInteger);
    {$ifdef FPC}varQword,{$endif}
    varInt64: setAsInt64(VInt64);

    varSingle: setAsDouble(VSingle);
    varDouble: setAsDouble(VDouble);
    varCurrency: setAsDouble(VCurrency);
    varDate: setAsDate(cx, VDate);
    varOleStr: setJSString(cx.NewJSString(WideString(VAny)));
    varString: setJSString(cx.NewJSString(VAny,length(RawByteString(VAny)),
    {$ifndef UNICODE}   CP_UTF8));
    {$else}             StringCodePage(RawByteString(VAny))));
    varUString: setJSString(cx.NewJSString(UnicodeString(VAny)));
    {$endif}
  else
    if VType=varByRef or varVariant then
      setSimpleVariant(cx,PVariant(VPointer)^) else
    if VType=varByRef or varOleStr then
      setJSString(cx.NewJSString(PWideString(VAny)^)) else
    {$ifdef UNICODE}
    if VType=varByRef or varUString then
      setJSString(cx.NewJSString(PUnicodeString(VAny)^)) else
    {$endif}
      raise ESMException.CreateFmt('Unhandled variant type %d',[VType]);
  end;
end;

procedure jsval.setVoid;
begin
  _l.asBits := JSVAL_VOID_impl;
end;

function jsval.Stringify(cx: PJSContext; var replacer: PJSObject;
  space: jsval; callback: JSONWriteCallback; data: pointer): Boolean;
begin
  with TSynFPUException.ForLibraryCode do
    Result := JS_Stringify(cx, Self, replacer, space, callback, data);
end;

function jsval.toSource(cx: PJSContext): PJSString;
begin
  Result := JS_ValueToSource(cx, self);
end;

function jsval.ValType(cx: PJSContext): JSType;
begin
  Result := cx.TypeOfValue(self);
end;

function SimpleVariantToJSval(cx: PJSContext; val: Variant): jsval;
begin
  Result.asSimpleVariant[cx] := val;
end;

initialization
  Latin1AnsiConvert := TSynAnsiConvert.Engine(CODEPAGE_LATIN1);
end.

