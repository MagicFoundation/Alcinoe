/// SpiderMonkey *.h header port to Delphi
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSMAPI;
{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

    Scripting support for mORMot Copyright (C) 2021 Pavel Mashlyakovsky
      pavel.mash at gmail.com

    Some ideas taken from
       http://code.google.com/p/delphi-javascript
       http://delphi.mozdev.org/javascript_bridge/

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
  Pavel Mashlyakovsky.
  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel

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

  ---------------------------------------------------------------------------
   Download the SpiderMonkey library at https://synopse.info/files/synsm.7z !
  ---------------------------------------------------------------------------

  Version 1.18
  - initial release. Use SpiderMonkey 24
    https://developer.mozilla.org/en-US/docs/SpiderMonkey/24
    and Modify jsapi.h for possibility import from Delphi
    and jsapi.cpp for possibility use JS_NumberValue
  - add JSString.ToJSVal method
  - change JS_ARGV macro result type to PjsvalVector
  - add debugger support functions

  road map:
    write Tjsval - object (record) wraper for jsval with methods  Is* To* As*
    migrate to IonMonkey
    compile mozjs without nspr
    enable ctypes? for byte array work #ifdef JS_HAS_CTYPES  JS_InitCTypesClass

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER
{$I SynSM.inc}   //define JS_THREADSAFE WITHASSERT

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  Variants,
  SynCommons;

const
{$ifdef MSWINDOWS}
  {$ifdef CPU64}
      SpiderMonkeyLib =  'mozjs64.dll';
  {$else}
      SpiderMonkeyLib =  'mozjs-24.dll';
      NSPRLib = 'libnspr4.dll';
  {$endif}
{$endif}

{$ifdef LINUX} // for Kylix/FPC (not tested yet)
  SpiderMonkeyLib = 'mozjs.so';
{$endif}

type
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

  /// define SpiderMonkey dedicated text buffer type
  // - in SM 1.8.5 exist mode JS_CSringAreUTF8, so it is possibe to use our
  // RawUTF8 strings (in all cases internaly in SM it will be converted to
  // Unicode - so need to test if SynCommons conversion faster or not -
  // - but in SM 1.8.8 JS_CSringAreUTF8 removed, and all API may have to
  // switch to jschar ( = Word/WideChar ) ?
  PCChar = PAnsiChar;


  /// 8 bit signed integer type for SMAPI
  JSInt8 = ShortInt;
  /// 16 bit signed integer type for SMAPI
  JSInt16 = SmallInt;
  /// 32 bit signed integer type for SMAPI
  JSInt32 = Integer;
  /// 8 bit unsigned integer type for SMAPI
  JSUint8 = Byte;
  /// 16 bit unsigned integer type for SMAPI
  JSUint16 = Word;
  /// 32 bit unsigned integer type for SMAPI
  JSUInt32 = Cardinal;
  /// 64 bit signed integer type for SMAPI
  JSInt64 = Int64;
  /// 64 bit unsigned integer type for SMAPI
  JSUInt64 = QWord;
{$ifndef FPC}
  /// variable type used to store a buffer size (in bytes) for SMAPI
  size_t = PtrUInt;
{$endif}

  /// jschar is the type of JavaScript "characters", the 16 bits elements
  // that make up JavaScript strings (maybe not truly valid UTF-16)
  // - It is a 16-bit unsigned integer type
  // - As required by the ECMAScript standard, ECMA 262-3 §4.3.16, JavaScript
  // strings are arbitrary sequences of 16-bit values
  // - A string may contain unmatched surrogates, which are not valid UTF-16
  // - It may also contain zeroes (0)
  // - so we did not define it as WideChar, but as abstract Word element
  jschar = Word;
  /// pointer to a sequence of JavaScript "characters", i.e. some 16 bits
  // elements that make up JavaScript strings (maybe not truly valid UTF-16)
  // - As required by the ECMAScript standard, ECMA 262-3 §4.3.16, JavaScript
  // strings are arbitrary sequences of 16-bit values
  // - A string may contain unmatched surrogates, which are not valid UTF-16
  // - It may also contain zeroes (0)
  Pjschar = ^jschar;
  /// pointer to a pointer of JavaScript "characters"
  // - is mostly used for arrays of JavaScript strings
  PPjschar = ^Pjschar;

  /// jsdouble is the internal type of numbers, i.e. floating-point values
  // - jsdouble is obsolete since JavaScript 1.8.7+ - instead we must use C double
  // so let's do it for future now
  // - in all cases see JSFloat64 - NSPR's floating point type is always 64 bits.
  jsdouble = Double;

  /// Pointer to JSScript structure defined if jsscript.h
  // - we do not directly use of this structure, so we define just a pointer
  PJSScript = type Pointer;


  /// type appropriate for most signed integer variables for SMAPI
  // - They are guaranteed to be at least 16 bits, though various architectures
  // may define them to be wider (e.g., 32 or even 64 bits). These types are
  // never valid for fields of a structure.
  JSIntn = PtrInt;
  /// type appropriate for most unsigned integer variables for SMAPI
  // - They are guaranteed to be at least 16 bits, though various architectures
  // may define them to be wider (e.g., 32 or even 64 bits). These types are
  // never valid for fields of a structure.
  JSUintn = PtrUInt;

  /// type appropriate for most flag set variables
  // - They are guaranteed to be at least 16 bits, though various architectures
  // may define them to be wider (e.g., 32 or even 64 bits). These types are
  // never valid for fields of a structure.
  uintn = PtrUInt;
  /// pointer to a flag set variable
  puintN = ^uintn;

  /// internal type of numbers, i.e. floating-point values for SMAPI
  // - NSPR's floating point type is always 64 bits.
  JSFloat64 = Double;

  /// a type for representing the size of objects for SMAPI
  JSSize = size_t;

  /// type for pointer arithmetic difference
  // - Variables of this type are suitable for storing a pointer or pointer sutraction
  ptrdiff_t = PtrInt;
  /// type for pointer arithmetic difference for SMAPI
  // - Variables of this type are suitable for storing a pointer or pointer sutraction
  JSPtrdiff = ptrdiff_t;

  ///  ordinal type used for pointer arithmetic
  // - Variables of this type are suitable for storing a pointer or pointer sutraction
  JSUIntPtr = PtrUInt;
  ///  pointer on an ordinal type used for pointer arithmetic
  PJSuintptr = ^JSuintptr;

  /// ordinal type used for pointer arithmetic
  // - Variables of this type are suitable for storing a pointer or pointer sutraction
  JSUptrdiff = JSUintPtr ;

  /// boolean type for variables and parameter types for SMAPI
  // - Use JS_FALSE and JS_TRUE constants for clarity of target type in assignments
  JSBool = JSIntn;
  /// pointer to boolean type for variables and parameter types for SMAPI
  PJSBool = ^JSBool;

const
  /// boolean TRUE value for variables and parameter types for SMAPI
  JS_TRUE = JSBool(1);
  /// boolean FALSE value for variables and parameter types for SMAPI
  JS_FALSE = JSBool(0);

  /// packed boolean type for variables and parameter types for SMAPI
  //  - use JSPackedBool within structs where bitfields are not desireable
  // but minimum and consistent overhead matters.
type
  JSPackedBool = JSUint8;

  /// a JSWord is a signed integer that is the same size as a pointer
  JSWord = PtrInt;
  /// a JSWord is an unsigned integer that is the same size as a pointer
  JSUword  = PtrUInt;



{ jsbuiltins.h conversion - "object" types }

  /// available options for JS Objects
  TJSOption = (
    jsoExtraWarning, jsoWError, jsoVarObjFix, jsoPrivateIsNSISupports, jsoCompileNGo,
    jsoUnused5, jsoUnused6, jsoUnused7, jsoDontReportUncaught, jsoUnused9,
    jsoUnused10, jsoUnused11, jsoNoScriptRVal, jsoUnrootedGlobal,
    jsoBaseLine, jsoPcCount, jsoTypeInference, jsoStrictMode, jsoIon, jsoAsmJS);
  /// set of available options for JS Objects
  TJSOptions = set of TJSOption;

  /// available options for JS Objects Properties
  TJSPropertyAttr = (
    jspEnumerate, jspReadOnly, jspPermanent, jspUnused, jspGetter,
    jspSetter, jspShared, jspIndex, sjpShortID);
  /// set of available options for JS Objects Properties
  TJSPropertyAttrs = set of TJSPropertyAttr;


  /// pointer to a JS Runtime instance
  PJSRuntime = ^JSRuntime;
  /// pointer to a JS String instance
  PJSString = ^JSString;
  /// pointer to a JS Object instance
  PJSObject = ^JSObject;

  /// map a generic JavaScript value internal representation
  // - a variable of type jsval can hold exactly the same values as a JavaScript
  // var or property: string, number, object, boolean, null, or undefined
  // (including Arrays, functions, and Errors are all objects)
  // - jsval is a variant type whose exact representation varies by architecture.
  // - you should never use this value internals, but pjsval and its TSMValue
  // wrapper, as defined in SynSM.pas unit, for a given TSMEngine execution
  // context - see
  // https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JSval
  jsval = type QWord;
  /// pointer to a jsval JavaScript value
  pjsval = ^jsval;
  /// an abstract array of jsval JavaScript values
  TjsvalVector = array[0..(MaxInt div sizeof(jsval))-1] of jsval;
  /// map an array of jsval JavaScript values
  PjsvalVector = ^TjsvalVector;

  /// used to store a JavaScript version
  JSVersion = Integer;

  /// JavaScript execution context
  // - this object does not store anything, but just provide some helper methods
  // to access a PPJSContext value via JS_*Context*(..) API functions
  JSContext = object
  private
    function GetVersion: JSVersion;
//  JS_SetVersion is not supported now Use CompartmentOptions in JS_NewGlobalObject
    function GetPrivate: Pointer;
      {$ifdef HASINLINE}inline;{$endif}
    procedure SetPrivate(const Value: Pointer);
  {$ifdef FIXBUGXE3}
  public
  {$endif}
    function GetOptions: TJSOptions;
    procedure SetOptions(const Value: TJSOptions);
  public
    /// wrapper to JS_DestroyContext(@self)
    procedure Destroy;
    /// wrapper to JS_GetRuntime(@self)
    function Runtime: PJSRuntime;
    /// wrapper to JS_InitStandardClasses(@self,global)
    function InitStandardClasses(global: PJSObject): boolean;
    {$ifndef FIXBUGXE3}
    /// wrapper to JS_GetOptions()/JS_SetOptions()
    // - due to a XE3 bug, you should use the GetOptions/SetOptions methods
    // instead of this property, under this compiler revision
    property Options: TJSOptions read GetOptions write SetOptions;
    {$endif}
    /// wrapper to JS_GetVersion()/JS_SetVersion()
    property Version: JSVersion read GetVersion;
    /// wrapper to JS_GetVersion() and string conversion
    function VersionToString: RawUTF8;
    /// wrapper to JS_GetContextPrivate()/JS_SetContextPrivate()
    property PrivateData: Pointer read GetPrivate write SetPrivate;
  public
    /// create a new JavaScript string instance from a given UTF-8 text
    function NewJSString(const Value: RawUTF8): PJSString; overload;
    /// create a new JavaScript string instance from a given UTF-16 text
    function NewJSString(const Value: SynUnicode): PJSString; overload;
    /// create a new JavaScript string instance from a given UTF-16 text buffer
    function NewJSString(TextWide: PWideChar; TextLen: integer): PJSString; overload;
    /// create a new JavaScript string instance from a given Ansi text buffer
    // - will use the specified Ansi code page for the conversion
    // - if CodePage is 0, will use the CurrentAnsiCodePage value
    function NewJSString(TextAnsi: PAnsiChar; TextLen, CodePage: integer): PJSString; overload;
  end;
  /// pointer to JavaScript execution context
  // - allows convenient access of JSContext wrapper methods
  PJSContext = ^JSContext;
  /// pointer to a pointer of JavaScript execution context
  PPJSContext = ^PJSContext;

  /// JavaScript execution runtime
  // - this object does not store anything, but just provide some helper methods
  // to access a PJSRuntime value via JS_*Runtime*(..) API functions
  JSRuntime = object
  private
    function GetPrivate: Pointer;
    procedure SetPrivate(const Value: Pointer);
  public
    /// wrapper to JS_LockRuntime(@self)
    procedure Lock;
    /// wrapper to JS_UnLockRuntime(@self)
    procedure Unlock;
    /// wrapper to JS_DestroyRuntime(@self)
    procedure Destroy;
    /// wrapper to JS_GetRuntimePrivate()/JS_SetRuntimePrivate()
    property PrivateData: Pointer read GetPrivate write SetPrivate;
  end;

  /// points to a JavaScript execution compartment
  // - allows convenient access of JSCompartment wrapper methods
  PJSCompartment = ^JSCompartment;

  /// JavaScript execution compartment
  // - this object does not store anything, but just provide some helper methods
  // to access a PJSRuntime value via JS_*Runtime*(..) API functions
  JSCompartment = object
  private
    fcx: PJSContext;
  public
    /// initialize and enter a JavaScript execution compartment
    function EnterCompartment(cx: PJSContext; target: PJSObject): PJSCompartment;
    /// leave a JavaScript execution compartment
    procedure Destroy;
  end;


  /// JavaScript string instance
  // - this object does not store anything, but just provide some helper methods
  // to access a PPJSString value via JS_*String*(..) API functions
  // - use function JSContext.NewJSString() to create a new instance for a given
  // execution context
  // - to understand string in SpiderMonkey good point is comments in vm\String.h
  // in short this is C structure:
  // $struct Data
  // $   {
  // $       size_t                     lengthAndFlags;      /* JSString */
  // $       union {
  // $           const jschar           *chars;              /* JSLinearString */
  // $           JSString               *left;               /* JSRope */
  // $       } u1;
  // $       union {
  // $           jschar    inlineStorage[NUM_INLINE_CHARS]; /* JS(Inline|Short)String */
  // $           struct {
  // $               union {
  // $                   JSLinearString *base;               /* JSDependentString */
  // $                   JSString       *right;              /* JSRope */
  // $                   size_t         capacity;            /* JSFlatString (extensible) */
  // $                   size_t         externalType;        /* JSExternalString */
  // $               } u2;
  // $               union {
  // $                   JSString       *parent;             /* JSRope (temporary) */
  // $                   void           *externalClosure;    /* JSExternalString */
  // $                   size_t         reserved;            /* may use for bug 615290 */
  // $               } u3;
  // $           } s;
  // $       };
  // $   } d;
  // but in API there is no need to use this structure, only pointer to it, and
  // high-level access to the SpiderMonkey API via this JSString wrapper
  JSString = object
  public
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
    /// get the UTF-16 text corresponding to this string, for a given
    // runtime execution context
    function ToSynUnicode(cx: PJSContext): SynUnicode;
    /// get the UTF-16 text corresponding to this string, for a given
    // runtime execution context
    function ToWideString(cx: PJSContext): WideString;
    /// get the UTF-16 text corresponding to this string as a variant,
    // for a given runtime execution context
    // - will store a SynUnicode value into the variant instance
    procedure ToVariant(cx: PJSContext; var Value: Variant);
    /// get the Delphi string text corresponding to this string, for a given
    // runtime execution context
    function ToString(cx: PJSContext): string;
    /// get the text encoded as a UTF-8 JSON string
    procedure ToJSONString(cx: PJSContext; W: TTextWriter);
    /// get a jsval corresponding to this string
    function ToJSVal: jsval;
  end;
  /// pointer to a pointer of JavaScript string
  PPJSString = ^PJSString;

  /// JSObject is the type of JavaScript objects in the JSAPI
  // - this object does not store anything, but just provide some helper methods
  // to access a PJSObject value via low-level API functions
  JSObject = object
  public
    /// get a jsval corresponding to this object
    function ToJSValue: jsval; {$ifdef HASINLINE}inline;{$endif}
  end;
  /// pointer to a pointer of JavaScript object
  PPJSObject = ^PJSObject;

  /// abstract pointer to a JavaScript function
  PJSFunction = type Pointer;

  /// jsid is a generic identifier for any JavaScript object property of method
  // - a jsid is an identifier for a property or method of an object which is
  // either a 31-bit signed integer, internal string or object. If XML is
  // enabled, there is an additional singleton jsid value; see
  // JS_DEFAULT_XML_NAMESPACE_ID below. Finally, there is an additional jsid
  // value, JSID_VOID, which does not occur in JS scripts but may be used to
  // indicate the absence of a valid jsid.
  jsid = size_t;
  /// pointer to a JavaScript object property of method identifier
  pjsid = ^jsid;
  /// abstract array to JavaScript object property of method identifiers
  // - set to -2 instead of -1 to allow JSIdArray record compilation
  TjsidVector = array[0..(MaxInt div sizeof(jsid))-2] of jsid;
  /// map an array to JavaScript object property of method identifiers
  PjsidVector = ^TjsidVector;



{ jspubtd.h conversion - public types }

const
  /// Run-time version enumeration corresponding to 1.0
  JSVERSION_1_0     = 100;
  /// Run-time version enumeration corresponding to 1.1
  JSVERSION_1_1     = 110;
  /// Run-time version enumeration corresponding to 1.2
  JSVERSION_1_2     = 120;
  /// Run-time version enumeration corresponding to 1.3
  JSVERSION_1_3     = 130;
  /// Run-time version enumeration corresponding to 1.4
  JSVERSION_1_4     = 140;
  /// Run-time version enumeration corresponding to ECMA standard 3, i.e. 1.4.8
  JSVERSION_ECMA_3  = 148;
  /// Run-time version enumeration corresponding to 1.5
  JSVERSION_1_5     = 150;
  /// Run-time version enumeration corresponding to 1.6
  JSVERSION_1_6     = 160;
  /// Run-time version enumeration corresponding to 1.7
  JSVERSION_1_7     = 170;
  /// Run-time version enumeration corresponding to 1.8
  JSVERSION_1_8     = 180;
  /// Run-time version enumeration corresponding to ECMA standard 5, i.e. 1.8.5
  JSVERSION_ECMA_5  = 185;
  /// Run-time version enumeration corresponding to default version
  JSVERSION_DEFAULT = 0;
  /// Run-time version enumeration corresponding to an identified version
  JSVERSION_UNKNOWN = -1;
  /// Run-time version enumeration corresponding to the latest available
  // - that is, ECMA standard 5, i.e. 1.8.5
  JSVERSION_LATEST  = JSVERSION_ECMA_5;

type
  /// Result of typeof operator enumeration
  JSType = (
    JSTYPE_VOID,                { undefined }
    JSTYPE_OBJECT,              { object }
    JSTYPE_FUNCTION,            { function }
    JSTYPE_STRING,              { string }
    JSTYPE_NUMBER,              { number }
    JSTYPE_BOOLEAN,             { boolean }
    JSTYPE_NULL,                { null }
//    JSTYPE_XML is not supported now
    JSTYPE_LIMIT
  );

  /// js_CheckAccess mode enumeration
  JSAccessMode = (
    JSACC_PROTO  = 0,           { XXXbe redundant w.r.t. id }
                                {
                                 * enum value #1 formerly called JSACC_PARENT,
                                 * gap preserved for ABI compatibility.
                                }
                                {
                                 * enum value #2 formerly called JSACC_IMPORT,
                                 * gap preserved for ABI compatibility.
                                 }
    JSACC_WATCH  = 3,           { a watchpoint on object foo for id 'bar' }
    JSACC_READ   = 4,           { a "get" of foo.bar }
    JSACC_WRITE  = 8,           { a "set" of foo.bar = baz }
    JSACC_LIMIT  = 9
  );

  /// This enum type is used to control the behavior of a JSObject property
  // iterator function that has type JSNewEnumerate.
  // - JSENUMERATE_INIT
  // A new, opaque iterator state should be allocated and stored in *statep.
  // (You can use PRIVATE_TO_JSVAL() to tag the pointer to be stored).
  // The number of properties that will be enumerated should be returned as
  // an integer jsval in *idp, if idp is non-null, and provided the number of
  // enumerable properties is known.  If idp is non-null and the number of
  // enumerable properties can't be computed in advance, *idp should be set
  // to JSVAL_ZERO.
  // - JSENUMERATE_INIT_ALL
  // Used identically to JSENUMERATE_INIT, but exposes all properties of the
  // object regardless of enumerability.
  // - JSENUMERATE_NEXT
  // A previously allocated opaque iterator state is passed in via statep.
  // Return the next jsid in the iteration using *idp.  The opaque iterator
  // state pointed at by statep is destroyed and *statep is set to JSVAL_NULL
  // if there are no properties left to enumerate.
  // - JSENUMERATE_DESTROY
  // Destroy the opaque iterator state previously allocated in *statep by a
  // call to this function when enum_op was JSENUMERATE_INIT or
  // JSENUMERATE_INIT_ALL.
  JSIterateOp = (
    //* Create new iterator state over enumerable properties. */
    JSENUMERATE_INIT,
    //* Create new iterator state over all properties. */
    JSENUMERATE_INIT_ALL,
    //* Iterate once. */
    JSENUMERATE_NEXT,
    //* Destroy iterator state. */
    JSENUMERATE_DESTROY
  );

 // JSClass (and js::ObjectOps where appropriate) function pointer typedefs

  /// JSClass method prototype to add, or get a property named by id in obj
  // - note the jsid id type -- id may be a string (Unicode property identifier)
  // or an int (element index)
  // - the vp out parameter, on success, is the new property value after
  // an add or get.  After a successful delete, *vp is JSVAL_FALSE iff
  // obj[id] can't be deleted (because it's permanent)
  JSPropertyOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
    vp: pjsval): JSBool; cdecl;

  /// JSClass method prototype to delete a property named by id in obj
  // - note the jsid id type -- id may be a string (Unicode property identifier)
  // or an int (element index)
  // - the *succeeded out parameter, on success, is the JSVAL_TRUE. *succeeded is
  // JSVAL_FALSE if obj[id] can't be deleted (because it's permanent)
  JSDeletePropertyOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
    succeeded: PJSBool):JSBool; cdecl;

  /// JSClass method prototype to set a property named by id in obj, treating
  // the assignment as strict mode code if strict is true
  // - note the jsid id type -- id may be a string (Unicode property identifier)
  // or an int (element index)
  // - the vp out parameter, on success, is the new property value after the set
  JSStrictPropertyOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
                                strict: JSBool; vp: pjsval): JSBool; cdecl;

  /// function prototype used for callbacks that enumerate the properties of
  // a JSObject
  // - The behavior depends on the value of enum_op
  // -The return value is used to indicate success, with a value of JS_FALSE
  // indicating failure.
  JSNewEnumerateOp = function(cx: PJSContext; var obj: PJSObject; enum_op: JSIterateOp;
                              statep: pjsval; idp: pjsid): JSBool; cdecl;

  /// The old-style JSClass.enumerate op should define all lazy properties not
  // yet reflected in obj.
  JSEnumerateOp = function(cx: PJSContext; var obj: PJSObject): JSBool; cdecl;

  /// function prototype used to resolve a lazy property named by id in obj
  // by defining it directly in obj
  // - Lazy properties are those reflected from some peer native property space
  // (e.g., the DOM attributes for a given node reflected as obj) on demand.
  // - JS looks for a property in an object, and if not found, tries to resolve
  // the given id.  If resolve succeeds, the engine looks again in case resolve
  // defined obj[id].  If no such property exists directly in obj, the process
  // is repeated with obj's prototype, etc.
  // - NB: JSNewResolveOp provides a cheaper way to resolve lazy properties.
  JSResolveOp = function(cx: PJSContext; var obj: PJSObject;
    var id: jsid): JSBool; cdecl;

  /// function prototype used to resolve a lazy property named by id in obj
  // by defining it directly in obj
  // - Like JSResolveOp, but flags provide contextual information as follows:
  // ! JSRESOLVE_QUALIFIED   a qualified property id: obj.id or obj[id], not id
  // ! JSRESOLVE_ASSIGNING   obj[id] is on the left-hand side of an assignment
  // ! JSRESOLVE_DETECTING   'if (o.p)...' or similar detection opcode sequence
  // ! JSRESOLVE_DECLARING   var, const, or function prolog declaration opcode
  // ! JSRESOLVE_CLASSNAME   class name used when constructing
  // - The *objp out parameter, on success, should be null to indicate that id
  // was not resolved; and non-null, referring to obj or one of its prototypes,
  // if id was resolved.
  // - This hook instead of JSResolveOp is called via the JSClass.resolve member
  // if JSCLASS_NEW_RESOLVE is set in JSClass.flags.
  // - Setting JSCLASS_NEW_RESOLVE and JSCLASS_NEW_RESOLVE_GETS_START further
  // extends this hook by passing in the starting object on the prototype chain
  // via *objp.  Thus a resolve hook implementation may define the property id
  // being resolved in the object in which the id was first sought, rather than
  // in a prototype object whose class led to the resolve hook being called.
  // - When using JSCLASS_NEW_RESOLVE_GETS_START, the resolve hook must therefore
  // null *objp to signify "not resolved".  With only JSCLASS_NEW_RESOLVE and no
  // JSCLASS_NEW_RESOLVE_GETS_START, the hook can assume *objp is null on entry.
  // This is not good practice, but enough existing hook implementations count
  // on it that we can't break compatibility by passing the starting object in
  // *objp without a new JSClass flag.
  JSNewResolveOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid; flags: uintN;
                            var objp: PPJSObject): JSBool; cdecl;
  // Convert obj to the given type, returning true with the resulting value in
  // vp on success, and returning false on error or exception.
  JSConvertOp = function(cx: PJSContext; var obj: PJSObject; typ: JSType;
    vp: pjsval): JSBool; cdecl;

  /// callback used to delegate typeof to an object so it can cloak a
  // primitive or another object
  JSTypeOfOp = function(cx: PJSContext; var obj: PJSObject): JSType; cdecl;

  /// Finalize obj, which the garbage collector has determined to be unreachable
  // from other live objects or from GC roots.  Obviously, finalizers must never
  // store a reference to obj.
  JSFinalizeOp = procedure(cx: PJSContext; obj: PJSObject); cdecl;

  /// callback used by JS_AddExternalStringFinalizer and JS_RemoveExternalStringFinalizer
  // to extend and reduce the set of string types finalized by the GC.
  JSStringFinalizeOp = procedure(cx: PJSContext; var obj: PJSString); cdecl;

  /// JSClass.checkAccess type: check whether obj[id] may be accessed per mode,
  // returning false on error/exception, true on success with obj[id]'s last-got
  // value in *vp, and its attributes in *attrsp.  As for JSPropertyOp above, id
  // is either a string or an int jsval.
  JSCheckAccessOp = function(cx: PJSContext; var obj: PJSObject; var id: jsid;
                              mode: JSAccessMode; vp: Pjsval): JSBool; cdecl;

  /// state value as expected by JSXDRObjectOp() prototype
  PJSXDRState = type Pointer;

  /// Encode or decode an object, given an XDR state record representing external data
  JSXDRObjectOp = function(xdr: PJSXDRState; var objp: PJSObject): JSBool; cdecl;

  /// callback used to check whether v is an instance of obj or not
  // - Return false on error or exception, true on success with JS_TRUE in bp
  // if v is an instance of obj, JS_FALSE in bp otherwise.
  JSHasInstanceOp = function(cx: PJSContext; obj: PJSObject; const v: Pjsval;
                              var bp: JSBool): JSBool; cdecl;

// here we miss trace and debug-only function defenition

  /// callback typedef for native functions called by the JS VM
  // - cx is the execution context
  // - argc is the number of supplied arguments
  // - vp[0] is the callee - see JS_CALLEE()
  // - vp[1] is the object instance - see JS_THIS()
  // - vp[2]..vp[argc+1] are the supplied arguments - see JS_ARGV_PTR()
  JSNative = function(cx: PJSContext; argc: uintN; vp: Pjsval): JSBool; cdecl;

  /// callback used for trace operation of a given class
  // - enumerate all  traceable things reachable from obj's private data structure.
  // - For each such thing, a trace implementation must call one of the
  // JS_Call*Tracer variants on the thing.
  // - JSTraceOp implementation can assume that no other threads mutates object
  // state. It must not change state of the object or corresponding native
  // structures. The only exception for this rule is the case when the embedding
  // needs a tight integration with GC. In that case the embedding can check if
  // the traversal is a part of the marking phase through calling
  // JS_IsGCMarkingTracer and apply a special code like emptying caches or
  // marking its native structures.
  JSTraceOp = procedure(cx: PJSContext; argc: uintN; vp: Pjsval); cdecl;

  /// a JavaScript tracer instance
  PJSTracer = Pointer;

  /// flag used for JSContextCallback() argument
  // - JSCONTEXT_NEW: JS_NewContext successfully created a new JSContext
  // instance. The callback can initialize the instance as
  // required. If the callback returns false, the instance
  // will be destroyed and JS_NewContext returns null. In
  // this case the callback is not called again.
  // - JSCONTEXT_DESTROY:  One of JS_DestroyContext* methods is called. The
  // callback may perform its own cleanup and must always
  // return true.
  // - Any other value: For future compatibility the callback must do nothing
  // and return true in this case.
  JSContextOp = (
    JSCONTEXT_NEW,
    JSCONTEXT_DESTROY
  );

  /// callback prototype for a given runtime context
  // - the possible values for contextOp when the runtime calls the callback
  // are defined by JSContextOp
  JSContextCallback = function(cx: PJSContext; contextOp: uintN): JSBool; cdecl;

  /// flag used for callback for a given runtime context garbage collection
  JSGCStatus = (
    JSGC_BEGIN,
    JSGC_END
  );

  /// callback prototype for a given runtime context garbage collection
  JSGCCallback = function(cx: PJSContext; status: JSGCStatus): JSBool; cdecl;

  /// generic operaiton callback prototype for a given runtime context
  JSOperationCallback = function(cx: PJSContext): JSBool; cdecl;

  /// Security protocol
  PJSPrincipals = Pointer;

  /// internal structure used to report JavaScript errors
  JSErrorReport = record
    /// source file name, URL, etc., or null
    filename: PCChar;
    /// see 'originPrincipals' comment above
    originPrincipals: PJSPrincipals;
    /// source line number
    lineno: uintN;
    /// offending source line without final #13
    linebuf: PCChar;
    /// pointer to error token in linebuf
    tokenptr: PCChar;
    /// unicode (original) line buffer
    uclinebuf: Pjschar;
    /// unicode (original) token pointer
    uctokenptr: Pjschar;
    /// error/warning, etc.
    flags: uintN;
    /// the error number, e.g. see js.msg
    errorNumber: uintN;
    /// the (default) error message
    ucmessage: Pjschar;
    /// arguments for the error message
    messageArgs: PPjschar;
    /// One of the JSExnType constants
    exnType: int16;
    /// zero-based column index in line
    column: uintN;
  end;
  /// map an internal structure used to report JavaScript errors
  PJSErrorReport = ^JSErrorReport;
  /// callback prototype for reporting error for a given runtime context
  JSErrorReporter = procedure(cx: PJSContext; _message: PCChar; report: PJSErrorReport); cdecl;

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
    JSEXN_LIMIT
  );
  /// used by JSErrorCallback() callback
  JSErrorFormatString = record
    /// The error format string (UTF-8 if js_CStringsAreUTF8)
    format: PCChar;
    /// The number of arguments to expand in the formatted error message
    argCount: uint16;
    /// One of the JSExnType constants above
    exnType: int16;
  end;
  /// pointer used by JSErrorCallback() callback
  PJSErrorFormatString = ^JSErrorFormatString;

  /// callback prototype for returning an execution error
  JSErrorCallback = function(userRef: Pointer; const locale: PCChar;
    const errorNumber: uintN): PJSErrorFormatString; cdecl;

// here we miss *Principals* functions - we do not use it for now


{ jsval.h conversion - extended types }

const
{$ifdef CPU64}
  JSVAL_TAG_SHIFT = 47;
  {$define JS_BITS_PER_WORD_IS64}
{$else}
  {$define JS_BITS_PER_WORD_IS32}
{$endif}

{$ifdef CPU64}
  JS_BYTES_PER_DOUBLE   = 8;
  JS_BYTES_PER_WORD     = 8;
  JS_BITS_PER_WORD_LOG2 = 6;
  JS_ALIGN_OF_POINTER   = 8;
{$else}
  JS_BYTES_PER_DOUBLE   = 8;
  JS_BYTES_PER_WORD     = 4;
  JS_BITS_PER_WORD_LOG2 = 5;
  JS_ALIGN_OF_POINTER   = 4;
{$endif}

// JSValueType
type
  JSValueType = Byte;

const
  JSVAL_TYPE_DOUBLE   = Byte($00);
  JSVAL_TYPE_INT32    = Byte($01);
  JSVAL_TYPE_UNDEFINED= Byte($02);
  JSVAL_TYPE_BOOLEAN  = Byte($03);
  JSVAL_TYPE_MAGIC    = Byte($04);
  JSVAL_TYPE_STRING   = Byte($05);
  JSVAL_TYPE_NULL     = Byte($06);
  JSVAL_TYPE_OBJECT   = Byte($07);

//JSValueTag
{$IFDEF CPU32}
type
  JSValueTag = Cardinal;
const
  JSVAL_TAG_CLEAR      = Cardinal($FFFFFF80);
  JSVAL_TAG_INT32      = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_INT32);
  JSVAL_TAG_UNDEFINED  = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_UNDEFINED);
  JSVAL_TAG_STRING     = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_STRING);
  JSVAL_TAG_BOOLEAN    = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_BOOLEAN);
  JSVAL_TAG_MAGIC      = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_MAGIC);
  JSVAL_TAG_NULL       = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_NULL);
  JSVAL_TAG_OBJECT     = Cardinal(JSVAL_TAG_CLEAR or JSVAL_TYPE_OBJECT);

{$ELSE}  //CPU64
type
  JSValueTag = Cardinal;
const
  JSVAL_TAG_MAX_DOUBLE= Cardinal($0001FFF0);
  JSVAL_TAG_INT32     = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_INT32);
  JSVAL_TAG_UNDEFINED = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_UNDEFINED);
  JSVAL_TAG_STRING    = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_STRING);
  JSVAL_TAG_BOOLEAN   = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_BOOLEAN);
  JSVAL_TAG_MAGIC     = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_MAGIC);
  JSVAL_TAG_NULL      = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_NULL);
  JSVAL_TAG_OBJECT    = Cardinal(JSVAL_TAG_MAX_DOUBLE or JSVAL_TYPE_OBJECT);

type
  JSValueShiftedTag = QWord;
const
  JSVAL_SHIFTED_TAG_MAX_DOUBLE = (QWord(JSVAL_TAG_MAX_DOUBLE) shl JSVAL_TAG_SHIFT) or $FFFFFFFF;
  JSVAL_SHIFTED_TAG_INT32      = QWord(JSVAL_TAG_INT32) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_UNDEFINED  = QWord(JSVAL_TAG_UNDEFINED) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_STRING     = QWord(JSVAL_TAG_STRING) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_BOOLEAN    = QWord(JSVAL_TAG_BOOLEAN) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_MAGIC      = QWord(JSVAL_TAG_MAGIC) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_NULL       = QWord(JSVAL_TAG_NULL) shl JSVAL_TAG_SHIFT;
  JSVAL_SHIFTED_TAG_OBJECT     = QWord(JSVAL_TAG_OBJECT) shl JSVAL_TAG_SHIFT;
{$ENDIF}

  JSVAL_LOWER_INCL_TYPE_OF_OBJ_OR_NULL_SET        = JSVAL_TYPE_NULL;
  JSVAL_UPPER_EXCL_TYPE_OF_PRIMITIVE_SET          = JSVAL_TYPE_OBJECT;
  JSVAL_UPPER_INCL_TYPE_OF_NUMBER_SET             = JSVAL_TYPE_INT32;
  JSVAL_LOWER_INCL_TYPE_OF_PTR_PAYLOAD_SET        = JSVAL_TYPE_MAGIC;

{$ifdef CPU32}
//#define JSVAL_TYPE_TO_TAG(type)      ((JSValueTag)(JSVAL_TAG_CLEAR | (type)))
  JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET         = JSVAL_TAG_NULL;
  JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET           = JSVAL_TAG_OBJECT;
  JSVAL_UPPER_INCL_TAG_OF_NUMBER_SET              = JSVAL_TAG_INT32;
  JSVAL_LOWER_INCL_TAG_OF_GCTHING_SET             = JSVAL_TAG_STRING;

{$else} // CPU64
  JSVAL_PAYLOAD_MASK : QWord = $00007FFFFFFFFFFF;
  JSVAL_TAG_MASK     : QWord = $FFFF800000000000;
  JSVAL_LOWER_INCL_SHIFTED_TAG_OF_OBJ_OR_NULL_SET  = JSVAL_SHIFTED_TAG_NULL;
  JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET    = JSVAL_SHIFTED_TAG_OBJECT;
  JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_NUMBER_SET       = JSVAL_SHIFTED_TAG_UNDEFINED;
  JSVAL_LOWER_INCL_SHIFTED_TAG_OF_GCTHING_SET      = JSVAL_SHIFTED_TAG_STRING;
{$endif}

type
  JSWhyMagic = (
    JS_ELEMENTS_HOLE,            // a hole in a native object's elements
    JS_NATIVE_ENUMERATE,         // indicates that a custom enumerate hook forwarded
                                 // to JS_EnumerateState, which really means the object can be
                                 // enumerated like a native object.
    JS_NO_ITER_VALUE,            // there is not a pending iterator value
    JS_GENERATOR_CLOSING,        // exception value thrown when closing a generator
    JS_NO_CONSTANT,              // compiler sentinel value
    JS_THIS_POISON,              // used in debug builds to catch tracing errors
    JS_ARG_POISON,               // used in debug builds to catch tracing errors
    JS_SERIALIZE_NO_NODE,        // an empty subnode in the AST serializer
    JS_LAZY_ARGUMENTS,           // lazy arguments value on the stack
    JS_OPTIMIZED_ARGUMENTS,      // optimized-away 'arguments' value
    JS_IS_CONSTRUCTING,          // magic value passed to natives to indicate construction
    JS_OVERWRITTEN_CALLEE,       // arguments.callee has been overwritten
    JS_FORWARD_TO_CALL_OBJECT,   // args object element stored in call object
    JS_BLOCK_NEEDS_CLONE,        // value of static block object slot
    JS_HASH_KEY_EMPTY,           // see class js::HashableValue
    JS_ION_ERROR,                // error while running Ion code
    JS_ION_BAILOUT,              // status code to signal EnterIon will OSR into Interpret
    JS_GENERIC_MAGIC             // for local use
  );


// here must be jsval_layout struct defenition and *_IMPL functions
// but it moved to implementation part of unit, because used only internaly


//#if JS_BITS_PER_WORD == 32
//#define BUILD_JSVAL(tag, payload) ((((uint64)(uint32)(tag)) << 32) | (uint32)(payload))
//#elif JS_BITS_PER_WORD == 64
//#define BUILD_JSVAL(tag, payload) ((((uint64)(uint32)(tag)) << JSVAL_TAG_SHIFT) | (payload))


{ jsapi.h conversion - main SpiderMonkey unit }

const
  CPU_SHIFT = {$ifdef CPU32}32{$else}JSVAL_TAG_SHIFT{$endif};

  JSVAL_NULL  : QWord = QWord((QWord(JSVAL_TAG_NULL) shl CPU_SHIFT) or 0);
  JSVAL_ZERO  : QWord = QWord((QWord(JSVAL_TAG_INT32) shl CPU_SHIFT) or 0);
  JSVAL_ONE   : QWord = QWord((QWord(JSVAL_TAG_INT32) shl CPU_SHIFT) or 1);
  JSVAL_FALSE : QWord = QWord((QWord(JSVAL_TAG_BOOLEAN) shl CPU_SHIFT) or JS_FALSE);
  JSVAL_TRUE  : QWord = QWord((QWord(JSVAL_TAG_BOOLEAN) shl CPU_SHIFT) or JS_TRUE);
  JSVAL_VOID  : QWord = QWord((QWord(JSVAL_TAG_UNDEFINED) shl CPU_SHIFT) or 0);

type
  jsint = JSInt32;
  jsuint = JSUInt32;

//#define JSVAL_BITS(v)    ((v).asBits)
//#define JSVAL_FROM_LAYOUT(l) (l)
//#define IMPL_TO_JSVAL(v) (v)
//#define JSID_BITS(id)    ((id).asBits)

/// check if a jsval is NULL
function JSVAL_IS_NULL(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if jsval is VOID
function JSVAL_IS_VOID(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// check if jsval is a 32 bit integer
function JSVAL_IS_INT(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a jsval into a 32 bit integer
// - there is no check that jsval is really a 32 bit integer: caller shall
// ensure this is the case (by using
function JSVAL_TO_INT(const v: jsval): jsint;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}

const
  JSVAL_INT_BITS = 32;
  JSVAL_INT_MIN = jsint($80000000);
  JSVAL_INT_MAX = jsint($7fffffff);

function INT_TO_JSVAL(i: int32): jsval;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_IS_DOUBLE(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_TO_DOUBLE(const v: jsval): Double;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function DOUBLE_TO_JSVAL(d: double): jsval;
  {$ifdef HASINLINE}inline;{$endif}
function UINT_TO_JSVAL(i: uint32): jsval;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_IS_NUMBER(const v: jsval): boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function JSVAL_IS_STRING(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_TO_STRING(const v: jsval): PJSString;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function STRING_TO_JSVAL(str: PJSString): jsval;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}

function JSVAL_IS_OBJECT(const v: jsval): Boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function JSVAL_TO_OBJECT(const v: jsval): PJSObject;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function OBJECT_TO_JSVAL(obj: PJSObject): jsval;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function JSVAL_IS_BOOLEAN(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_TO_BOOLEAN(const v: jsval): JSBool;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function BOOLEAN_TO_JSVAL(b: JSBool): jsval;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_IS_PRIMITIVE(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_IS_GCTHING(const v: jsval): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
function JSVAL_TO_GCTHING(const v: jsval): Pointer;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function PRIVATE_TO_JSVAL(ptr: Pointer): jsval;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
function JSVAL_TO_PRIVATE(const v: jsval): Pointer;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}

// A jsid is an identifier for a property or method of an object which is
// either a 31-bit signed integer, interned string or object. If XML is
// enabled, there is an additional singleton jsid value; see
// JS_DEFAULT_XML_NAMESPACE_ID below. Finally, there is an additional jsid
// value, JSID_VOID, which does not occur in JS scripts but may be used to
// indicate the absence of a valid jsid.
//
// A jsid is not implicitly convertible to or from a jsval; JS_ValueToId or
// JS_IdToValue must be used instead.
const
  JSID_TYPE_STRING                 = $0;
  JSID_TYPE_INT                    = $1;
  JSID_TYPE_VOID                   = $2;
  JSID_TYPE_OBJECT                 = $4;
  JSID_TYPE_MASK                   = $7;

//TODO make jsid macro conversion  jsapi.h line 308-462
//#define JSVAL_LOCK(cx,v)      (JSVAL_IS_GCTHING(v)                          \
//                                ? JS_LockGCThing(cx, JSVAL_TO_GCTHING(v))   \
//                                : JS_TRUE)
//#define JSVAL_UNLOCK(cx,v)    (JSVAL_IS_GCTHING(v)                          \
//                                ? JS_UnlockGCThing(cx, JSVAL_TO_GCTHING(v)) \
//                                : JS_TRUE)

// Property attributes, set in JSPropertySpec and passed to API functions
  JSPROP_ENUMERATE        = $01;    { property is visible to for/in loop }
  JSPROP_READONLY         = $02;    { not settable: assignment is no-op }
  JSPROP_PERMANENT        = $04;    { property cannot be deleted }
  JSPROP_GETTER           = $10;    { property holds getter function }
  JSPROP_SETTER           = $20;    { property holds setter function }
  JSPROP_SHARED           = $40;    { don't allocate a value slot for this
                                           property; don't copy the property on
                                           set of the same-named property in an
                                           object that delegates to a prototype
                                           containing this property }
  JSPROP_INDEX            = $80;    { name is actually (jsint) index }
  JSPROP_SHORTID          = $100;   { set in JSPropertyDescriptor.attrs
                                           if getters/setters use a shortid }
  JSFUN_STUB_GSOPS        = $200;   { use JS_PropertyStub getter/setter
                                           instead of defaulting to class gsops
                                           for property holding function }
  JSFUN_CONSTRUCTOR       = $400;   { native that can be called as a ctor }
{*
 * Specify a generic native prototype methods, i.e., methods of a class
 * prototype that are exposed as static methods taking an extra leading
 * argument: the generic |this| parameter.
 *
 * If you set this flag in a JSFunctionSpec struct's flags initializer, then
 * that struct must live at least as long as the native static method object
 * created due to this flag by JS_DefineFunctions or JS_InitClass.  Typically
 * JSFunctionSpec structs are allocated in static arrays.
 }
  JSFUN_GENERIC_NATIVE    =$800;

  JSFUN_FLAGS_MASK        =$e00;    { | of all the JSFUN_* flags }

function JS_Now: int64; cdecl; external SpiderMonkeyLib;
function JS_GetNaNValue(cx: PJSContext): jsval; cdecl; external SpiderMonkeyLib;
function JS_GetNegativeInfinityValue(cx: PJSContext): jsval; cdecl; external SpiderMonkeyLib;
function JS_GetPositiveInfinityValue(cx: PJSContext): jsval; cdecl; external SpiderMonkeyLib;
function JS_GetEmptyStringValue(cx: PJSContext): jsval; cdecl; external SpiderMonkeyLib;
function JS_GetEmptyString(cx: PJSRuntime): PJSString; cdecl; external SpiderMonkeyLib;

/// Converts a series of JS values, passed in an argument array,
// to their corresponding JS types
// - Format is a string of the following characters (spaces are insignificant),
// specifying the tabulated type conversions:
// $  b      JSBool          Boolean
// $  c      uint16/jschar   ECMA uint16, Unicode char
// $  i      int32           ECMA int32
// $  u      uint32          ECMA uint32
// $  j      int32           Rounded int32 (coordinate)
// $  d      jsdouble        IEEE double
// $  I      jsdouble        Integral IEEE double
// $  S      JSString *      Unicode string, accessed by a JSString pointer
// $  W      jschar *        Unicode character vector, 0-terminated (W for wide)
// $  o      JSObject *      Object reference
// $  f      JSFunction *    Function private
// $  v      jsval           Argument value (no conversion)
// $  *      N/A             Skip this argument (no vararg)
// $  /      N/A             End of required arguments
// The variable argument list after format must consist of &b, &c, &s, e.g.,
// where those variables have the types given above.  For the pointer types
// char *, JSString *, and JSObject *, the pointed-at memory returned belongs
// to the JS runtime, not to the calling native code.  The runtime promises
// to keep this memory valid so long as argv refers to allocated stack space
// (so long as the native function is active).
// - Fewer arguments than format specifies may be passed only if there is a /
// in format after the last required argument specifier and argc is at least
// the number of required arguments.  More arguments than format specifies
// may be passed without error; it is up to the caller to deal with trailing
// unconverted arguments.
// NOTE: we did not include this function yet, since it uses C varargs
// function JS_ConvertArguments(cx: PJSContext; argc: uintN; argv: pjsval;
//  const format: PCChar): JSBool; varargs; cdecl; external SpiderMonkeyLib;

//!!!!!!!!!!!!!!!!!!TODO!!!!!!!!!!!!!!!!
// wery important to provide similiar but for Delphi!!
//TODO JS_Convert(Push)Arguments  - provide conversation to(from) array of TVarRec?

/// JS_ConvertValue converts a JavaScript value, v, to the specified type
// - On success, the converted value is stored in *vp. Typically users of this
// function set vp to point to v, so that if conversion is successful, v now
// contains the converted value.
// - JS_ConvertValue calls other, type-specific conversion routines based
// on the type argument
// - Converting any value to JSTYPE_VOID always succeeds. The result is JSVAL_VOID.
// - Converting to JSTYPE_OBJECT works exactly like JS_ValueToObject.
// - Converting to JSTYPE_FUNCTION works like JS_ValueToFunction, but better:
// the result is a function object that has not been stripped of its lexical scope.
// It is safe to call the result (e.g. using JS_CallFunctionValue).
// - Converting to JSTYPE_STRING works exactly like JS_ValueToString.
// - Converting to JSTYPE_NUMBER works exactly like JS_ValueToNumber.
// - Converting to JSTYPE_BOOLEAN works exactly like JS_ValueToBoolean.
// - On success, JS_ConvertValue stores the converted value in *vp and returns JS_TRUE.
// - On error or exception, it returns JS_FALSE, and the value left in *vp is undefined
function JS_ConvertValue(cx: PJSContext; v: jsval; _type: JSType;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_ValueToObject converts a specified JavaScript value, v, to an object
// - On success, this function stores either NULL or a pointer to the resulting
// object in objp and returns JS_TRUE. Otherwise it returns JS_FALSE and
// the value left in objp is unspecified.
// - If v is JSVAL_NULL or JSVAL_VOID, the result is NULL.
// - If v is a boolean value, a number, or a string, the result is a new wrapper
// object of type Boolean, Number, or String.
// - Otherwise v is an object, and the result depends on the object. If v is a
// native JavaScript Object, this calls the object's valueOf method, if any. In
// any case, the result is not guaranteed to be the same object as v.
// (Implementation note: the object's JSObjectOps.defaultValue method is called
// with hint=JSTYPE_OBJECT.)
// - The resulting object is subject to garbage collection unless the variable
// objp is protected by a local root scope, an object property, or the
// JS_AddRoot function. Note that a local root scope is not sufficient to
// protect the resulting object in some cases involving the valueOf method!
function JS_ValueToObject(cx: PJSContext; v: jsval;
  var objp: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// convert a jsval to a JSFunction
// - If v is a Function object, this returns the associated JSFunction.
// - If v is null, undefined, a boolean, a number, or a string, a TypeError
// is reported and JS_ValueToFunction returns nil
// - Otherwise, v is an object. The JavaScript engine attempts to convert it
// to a function, which can be pretty unsafe! You have been warned!
function JS_ValueToFunction(cx: PJSContext; v: jsval): PJSFunction;
  cdecl; external SpiderMonkeyLib;

/// JS_ValueToString converts a specified JavaScript value, v, to a string.
// - It implements the ToString operator specified in ECMA 262-3 §9.8
// - The result is like the JavaScript expression ""+v.
// - If v is already a string, conversion succeeds.
// - If v is true, false, null, or undefined, conversion succeeds, and the
// result is the string "true", "false", "null", or "undefined", accordingly.
// - If v is a number, conversion succeeds, and the result is a
// string representation of that number as specified in ECMA 262-3 §9.8.1.
// This might be "NaN", "Infinity", or "-Infinity". Otherwise the result is a
// decimal representation of the number, possibly using exponential notation.
// - Otherwise v is an object. JS_ValueToString uses the steps below to convert
// it to a string. If at any point an error or exception occurs, or conversion
// succeeds, the rest of the steps are skipped. (This behavior is implemented by
// v's JSObjectOps.defaultValue method, so host objects can override it all.)
// If v.toString() is a function, it is called. If that method returns a
// primitive value, the value is converted to a string as described above and
// conversion succeeds. Otherwise, the resulting object's JSClass.convert
// callback is called. For standard classes, this is  JS_ConvertStub, which
// simply calls v.valueOf() if present. If the convert callback produces a
// primitive value, the value is converted to a string as described above and
// conversion succeeds. Otherwise conversion fails with a TypeError.
// - On success, JS_ValueToString returns a pointer to a string. On error or
// exception, it returns NULL. This happens, for example, if v is an object
// and v.toString() throws an exception.
// - The resulting JSString is subject to garbage collection. Protect it using
// a local root, an object property, or the JS_AddRoot function.
function JS_ValueToString(cx: PJSContext; v: jsval): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Convert any JavaScript value to its source representation
function JS_ValueToSource(cx: PJSContext; v: jsval): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Convert any JavaScript value to a floating-point number of type jsdouble.
function JS_ValueToNumber(cx: PJSContext; v: jsval;
  var dp: jsdouble): JSBool; cdecl; external SpiderMonkeyLib;

/// Check if the given double is in fact a 31 bit signed integer
function JS_DoubleIsInt32(d: jsdouble;
  var i: jsint): JSBool; cdecl; external SpiderMonkeyLib;

/// Convert a JavaScript value to an integer type as specified by the ECMAScript standard
function JS_ValueToECMAInt32(cx: PJSContext; v: jsval;
  var i: int32): JSBool; cdecl; external SpiderMonkeyLib;

/// Convert a JavaScript value to an integer type as specified
// by the ECMAScript standard
function JS_ValueToECMAUint32(cx: PJSContext; v: jsval;
  var ui: uint32): JSBool; cdecl; external SpiderMonkeyLib;

// JS_ValueToInt32 is obsolete
function JS_ValueToInt32(cx: PJSContext; v: jsval;
  var i: int32): JSBool; cdecl; external SpiderMonkeyLib;

/// Convert a JavaScript value to a 16 bit integer
// - ECMA ToUint16, e.g. for mapping a jsval to a Unicode point.
function JS_ValueToUint16(cx: PJSContext; v: jsval;
  var ui16: uint16): JSBool; cdecl; external SpiderMonkeyLib;

/// Convert a JavaScript value to a boolean
function JS_ValueToBoolean(cx: PJSContext; v: jsval;
  var b: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Determines the JS data type of a JS value
function JS_TypeOfValue(cx: PJSContext;
  v: jsval): JSType; cdecl; external SpiderMonkeyLib;

// Determines the JS data type name of a JS value
function JS_GetTypeName(cx: PJSContext;
  _type: JSType): PCChar; cdecl; external SpiderMonkeyLib;

/// Determine whether two JavaScript values are equal in the sense of the === operator
function JS_StrictlyEqual(cx: PJSContext;
  v1, v2: jsval; equal: PJSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Determines if two jsvals are the same, as determined by the SameValue
// algorithm in ECMAScript 262, 5th edition
// - SameValue slightly differs from strict equality (===) in that +0 and -0 are
// not the same and in that NaN is the same as NaN
function JS_SameValue(cx: PJSContext; v1, v2: jsval;
  equal: PJSBool): JSBool; cdecl; external SpiderMonkeyLib;


{ Initialization, locking, contexts, and memory allocation }

// It is important that the first runtime and first context be created in a
// single-threaded fashion, otherwise the behavior of the library is undefined
// See http://developer.mozilla.org/en/docs/Category:JSAPI_Reference

// some aliases defined in SM:
//  #define JS_NewRuntime       JS_Init
//  #define JS_DestroyRuntime   JS_Finish
//  #define JS_LockRuntime      JS_Lock
//  #define JS_UnlockRuntime    JS_Unlock

type
  /// defines how function JS_NewRuntime() instantiate its threading model
  JSUseHelperThreads =
    (JS_NO_HELPER_THREADS, JS_USE_HELPER_THREADS);

/// Initialize the JavaScript runtime
// - JS_NewRuntime initializes the JavaScript runtime environment.
// Call JS_NewRuntime before making any other API calls.
// - JS_NewRuntime allocates memory for the JSRuntime and initializes certain
// internal runtime structures. maxbytes specifies the number of allocated
// bytes after which garbage collection is run.
function JS_NewRuntime(maxbytes: uint32; useHelperThreads: JSUseHelperThreads): PJSRuntime;
  cdecl; external SpiderMonkeyLib;

/// Release the JavaScript runtime
procedure JS_DestroyRuntime(rt: PJSRuntime); cdecl; external SpiderMonkeyLib;

/// Free all resources used by the JS engine, not associated with specific runtimes
procedure JS_ShutDown;

function JS_GetRuntimePrivate(rt: PJSRuntime): pointer; cdecl; external SpiderMonkeyLib;

procedure JS_SetRuntimePrivate(rt: PJSRuntime; data: pointer); cdecl; external SpiderMonkeyLib;

/// indicates to the JS engine that the calling thread is entering a region
// of code that may call into the JSAPI but does not block
procedure JS_BeginRequest(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// indicates to the JS engine that the calling thread is leaving a region
// of code that may call into the JSAPI but does not block
procedure JS_EndRequest(cx: PJSContext); cdecl; external SpiderMonkeyLib;

// OBSOLETE procedure JS_YieldRequest(cx: PJSContext); cdecl; external SpiderMonkeyLib;

//OBSOLETE type
//OBSOLETE   jsrefcount = JSInt32;

//OBSOLETE function JS_SuspendRequest(cx: PJSContext): jsrefcount; cdecl; external SpiderMonkeyLib;
//OBSOLETE procedure JS_ResumeRequest(cx: PJSContext; saveDepth: jsrefcount); cdecl; external SpiderMonkeyLib;

/// checked is is within a code region protected by JS_BeginRequest()
function JS_IsInRequest(cx: PJSContext): JSBool; cdecl; external SpiderMonkeyLib;

procedure JS_LockRuntime(rt: PJSRuntime); cdecl; external SpiderMonkeyLib name 'JS_Lock';
procedure JS_UnlockRuntime(rt: PJSRuntime); cdecl; external SpiderMonkeyLib name 'JS_Unlock';

/// Specifies a callback function that is automatically called whenever
// a JSContext is created or destroyed
function JS_SetContextCallback(rt: PJSRuntime; cxCallback: JSContextCallback): JSContextCallback;
  cdecl; external SpiderMonkeyLib;

/// Create a new JSContext
function JS_NewContext(rt: PJSRuntime; stackChunkSize: size_t): PJSContext;
  cdecl; external SpiderMonkeyLib;

/// Release a new JSContext
procedure JS_DestroyContext(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Release a new JSContext without performing garbage collection
procedure JS_DestroyContextNoGC(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Release a new JSContext with potential garbage collection
// - this function may or may not perform garbage collection: the engine makes
// an educated guess as to whether enough memory would be reclaimed to justify the work
procedure JS_DestroyContextMaybeGC(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// read access to a JSContext field for application-specific data
function JS_GetContextPrivate(cx: PJSContext): Pointer;
  cdecl; external SpiderMonkeyLib;

/// write access to a JSContext field for application-specific data
procedure JS_SetContextPrivate(cx: PJSContext; data: Pointer);
  cdecl; external SpiderMonkeyLib;

/// retrieves a pointer to the JSRuntime with which a specified JSContext,
// is associated
function JS_GetRuntime(cx: PJSContext): PJSRuntime; cdecl; external SpiderMonkeyLib;

/// cycles through the JS contexts associated with a particular JSRuntime
function JS_ContextIterator(rt: PJSRuntime; iterp: PPJSContext): PJSContext;
  cdecl; external SpiderMonkeyLib;

/// retrieve the JavaScript version number used within a specified executable script context
function JS_GetVersion(cx: PJSContext): JSVersion;
  cdecl; external SpiderMonkeyLib;

//  JS_SetVersion is not supported now Use CompartmentOptions in JS_NewGlobalObject

/// retrieve the JavaScript version text used within a specified executable script context
function JS_VersionToString(version: JSVersion): PCChar;
  cdecl; external SpiderMonkeyLib;

/// configure a JSContext to use a specific version of the JavaScript language
function JS_StringToVersion(_string: PCChar): JSVersion; cdecl; external SpiderMonkeyLib;

const
  JSOPTION_EXTRA_WARNINGS = 1 shl 0;       { warn on dubious practice }
  JSOPTION_WERROR         = 1 shl 1;       { convert warning to error }
  JSOPTION_VAROBJFIX      = 1 shl 2;       { make JS_EvaluateScript use
                                                     the last object on its 'obj'
                                                     params scope chain as the
                                                     ECMA 'variables object' }
  JSOPTION_PRIVATE_IS_NSISUPPORTS = 1 shl 3;       { context private data points
                                                     to an nsISupports subclass }
  JSOPTION_COMPILE_N_GO   = 1 shl 4;       { caller of JS_Compile*Script
                                                     promises to execute compiled
                                                     script once only; enables
                                                     compile-time scope chain
                                                     resolution of consts. }
{ = 1 shl 5 is currently unused. }
{ = 1 shl 6 is currently unused. }
{ = 1 shl 7 is currently unused. }
  JSOPTION_DONT_REPORT_UNCAUGHT  = 1 shl 8;     { When returning from the
                                                   outermost API call, prevent
                                                   uncaught exceptions from
                                                   being converted to error
                                                   reports }
{ = 1 shl 9 is currently unused. }
{ = 1 shl 10 is currently unused. }
{ = 1 shl 11 is currently unused. }

  JSOPTION_NO_SCRIPT_RVAL = 1 shl 12;   { A promise to the compiler
                                                   that a null rval out-param
                                                   will be passed to each call
                                                   to JS_ExecuteScript. }
  JSOPTION_UNROOTED_GLOBAL = 1 shl 13;  { The GC will not root the
                                                     contexts' global objects
                                                     (see JS_GetGlobalObject),
                                                     leaving that up to the
                                                     embedding. }

  JSOPTION_BASELINE       = 1 shl 14;   { Baseline compiler. }
  JSOPTION_PCCOUNT        = 1 shl 15;   { Collect per-op execution counts }

  JSOPTION_TYPE_INFERENCE = 1 shl 16;   { Perform type inference. }
  JSOPTION_STRICT_MODE    = 1 shl 17;   { Provides a way to force
                                                   strict mode for all code
                                                   without requiring
                                                   "use strict" annotations. }

  JSOPTION_ION            = 1 shl 18;   { IonMonkey }

  JSOPTION_ASMJS          = 1 shl 19;   { optimizingasm.js compiler }

  JSOPTION_MASK           = 1 shl 20 - 1;

function JS_GetOptions(cx: PJSContext): uint32; cdecl; external SpiderMonkeyLib;
function JS_SetOptions(cx: PJSContext; options: uint32): uint32; cdecl; external SpiderMonkeyLib;
function JS_ToggleOptions(cx: PJSContext; options: uint32): uint32; cdecl; external SpiderMonkeyLib;
function JS_GetImplementationVersion: PCChar; cdecl; external SpiderMonkeyLib ;

// here is some *Compartment* functions - Compartment is obsolete, so we do not import them

// osolete
//  extern JS_PUBLIC_API(JSObject *)
//  JS_GetGlobalObject(JSContext *cx);
//
//  extern JS_PUBLIC_API(void)
//  JS_SetGlobalObject(JSContext *cx, JSObject *obj);

/// initializes the built-in JavaScript global properties.
// - These include all the standard ECMAScript global properties defined in
// ECMA 262-3 §15: Array, Boolean, Date, decodeURI, decodeURIComponent,
// encodeURI, encodeURIComponent, Error, eval, EvalError, Function, Infinity,
// isNaN, isFinite, Math, NaN, Number, Object, parseInt, parseFloat, RangeError,
// ReferenceError, RegExp, String, SyntaxError, TypeError, undefined, and URIError
// - and set obj as a global object for cx!
function JS_InitStandardClasses(cx: PJSContext; obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Resolve id, which must contain either a string or an int, to a standard
// class name in obj if possible, defining the class's constructor and/or
// prototype and storing true in resolved
// - If id does not name a standard
// class or a top-level property induced by initializing a standard class,
// store false in *resolved and just return true.  Return false on error,
// as usual for JSBool result-typed API entry points.
// - This API can be called directly from a global object class's resolve op,
// to define standard classes lazily.  The class's enumerate op should call
// JS_EnumerateStandardClasses(cx, obj), to define eagerly during for..in
// loops any classes not yet resolved lazily.
function JS_ResolveStandardClass(cx: PJSContext; obj: PJSObject; id: jsid;
  var resolved: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

function JS_EnumerateStandardClasses(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  JSIdArray = record
    length: jsint;
    vector: TjsidVector;    //* actually, length jsid words */
  end;
  PJSIdArray = ^JSIdArray;

/// Enumerate any already-resolved standard class ids into ida, or into a new
// JSIdArray if ida is null
// - Return the augmented array on success, null on failure with ida
// (if it was non-null on entry) destroyed
function JS_EnumerateResolvedStandardClasses(cx: PJSContext; obj: PJSObject;
   ida: PJSIdArray): PJSIdArray; cdecl; external SpiderMonkeyLib;

//extern JS_PUBLIC_API(JSBool)
//JS_GetClassObject(JSContext *cx, JSObject *obj, JSProtoKey key,
//                  JSObject **objp);
//
//extern JS_PUBLIC_API(JSObject *)
//JS_GetScopeChain(JSContext *cx);
//
//extern JS_PUBLIC_API(JSObject *)
//JS_GetGlobalForObject(JSContext *cx, JSObject *obj);
//
//extern JS_PUBLIC_API(JSObject *)
//JS_GetGlobalForScopeChain(JSContext *cx);

// here is #ifdef JS_HAS_CTYPES - currently not shure we can use it.

/// return the callee function of a JSNative callback
function JS_CALLEE(cx: PJSContext; vp: Pjsval): jsval;
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// return the this object of a JSNative callback
function JS_THIS(cx: PJSContext; vp: Pjsval): jsval;
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// return the this object of a JSNative callback
function JS_THIS_OBJECT(cx: PJSContext; vp: Pjsval): PJSObject;
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// points to the first argument of a JSNative callback
function JS_ARGV(cx: PJSContext; vp: Pjsval): PjsvalVector;
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// get the return value of a JSNative callback
function JS_RVAL(cx: PJSContext; vp: Pjsval): jsval;
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// set the return value of a JSNative callback
procedure JS_SET_RVAL(cx: PJSContext; vp: Pjsval; v: jsval);
  {$IFDEF HASINLINE}inline;{$ENDIF}

/// low-level API used by JS_THIS() macro
function JS_ComputeThis(cx: PJSContext; vp: Pjsval): jsval; cdecl; external SpiderMonkeyLib;

function JS_malloc(cx: PJSContext; nbytes: size_t): Pointer;
  cdecl; external SpiderMonkeyLib;
function JS_realloc(cx: PJSContext; p: Pointer; nbytes: size_t): Pointer;
  cdecl; external SpiderMonkeyLib;
procedure JS_free(cx: PJSContext; p: Pointer);
  cdecl; external SpiderMonkeyLib;
procedure JS_updateMallocCounter(cx: PJSContext; nbytes: size_t);
  cdecl; external SpiderMonkeyLib;
function JS_strdup(cx: PJSContext; s: PCChar): PCChar;
  cdecl; external SpiderMonkeyLib;
//JS_NewNumberValue Obsolete since JavaScript mozjs17 use JS_NumberValue instead
//https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JS_NewNumberValue
function JS_NumberValue(d: double): jsval;
  cdecl; external SpiderMonkeyLib name 'JS_NumberValue_';

/// The JS_Add*Root functions add a C/C++ variable to the garbage collector's root set,
// the set of variables used as starting points each time the collector checks
// to see what memory is reachable
// - The garbage collector aggressively collects
// and recycles memory that it deems unreachable, so roots are often necessary
// to protect data from being prematurely collected.
// - vp/spp/opp/rp is the address of a C/C++ variable (or field, or array element)
// of type JSString *, JSObject *, or jsval. This variable must already be initialized.
// (For example, it must not be an uninitialized local variable. That could cause
// sporadic crashes during garbage collection, which can be hard to debug.)
// The variable must remain in memory until the balancing call to JS_RemoveRoot.
// Note that this means that if the root is meant to live past the end of a function,
// the address of a local (stack-based) variable may not be used for rp.
// If JS_Add*Root succeeds, then as long as this variable points to a JavaScript
// value or pointer to GC-thing, that value/GC-thing is protected from garbage collection.
// If the variable points to an object, then any memory reachable from its properties
// is automatically protected from garbage collection, too.
// - JS_AddGCThingRoot allows the caller to have a single root that may hold
// either strings or objects.  A jsval is not a GC-thing (it has tag bits and
// may be a different size altogether) and thus the address of a jsval
// variable must not be passed to JS_AddGCThingRoot.
// - Do not pass a pointer to a JS string or object to any of these functions—rp
// must point to a variable, the location of the pointer itself, and not an object or string.
function JS_AddValueRoot(cx: PJSContext; vp: Pjsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// add a JSString variable to the garbage collector's root set
// - similar to JS_AddValueRoot(), but with a JSString value
function JS_AddStringRoot(cx: PJSContext; rp: PPJSString): JSBool;
  cdecl; external SpiderMonkeyLib;

/// add a JSObject variable to the garbage collector's root set
// - similar to JS_AddValueRoot(), but with a JSObject value
function JS_AddObjectRoot(cx: PJSContext; rp: PPJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// remove a jsval variable from the garbage collector's root set
function JS_RemoveValueRoot(cx: PJSContext; vp: Pjsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// remove a JSString variable from the garbage collector's root set
function JS_RemoveStringRoot(cx: PJSContext; rp: PPJSString): JSBool;
  cdecl; external SpiderMonkeyLib;

/// remove a JSObject variable from the garbage collector's root set
function JS_RemoveObjectRoot(cx: PJSContext; rp: PPJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

function JS_AddGCThingRoot(cx: PJSContext; rp: PPointer): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_RemoveGCThingRoot(cx: PJSContext; rp: PPointer): JSBool;
  cdecl; external SpiderMonkeyLib;


/// launch the GarbageCollection process of the given execution RunTime
procedure JS_GC(rt: PJSRuntime); cdecl; external SpiderMonkeyLib;

/// check if it would be worth it to launch the GarbageCollection process
// of the given execution RunTime, in the given context
// - this function is a no-op if there is nothing interesting to garbage
procedure JS_MaybeGC(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// defines a Garbage Collection call-back function
function JS_SetGCCallback(rt: PJSRuntime; cb: JSGCCallback): JSGCCallback;
  cdecl; external SpiderMonkeyLib;

/// JS_IsAboutToBeFinalized() checks if the given object is going to be finalized
// at the end of the current GC
// - When called outside of the context of a GC, this function will return false
// - Typically this function is used on weak references, where the reference
// should be nulled out or destroyed if the given object is about to be finalized
// - The argument to JS_IsAboutToBeFinalized is an in-out param: when the
// function returns false, the object being referenced is still alive, but the
// garbage collector might have moved it. In this case, the reference passed
// to JS_IsAboutToBeFinalized will be updated to the object's new location
// - Callers of this method are responsible for updating any state that is
// dependent on the object's address. For example, if the object's address is
// used as a key in a hashtable, then the object must be removed and
// re-inserted with the correct hash
function JS_IsAboutToBeFinalized(obj: PPJSObject): JSBool; cdecl; external SpiderMonkeyLib;

type
  JSGCParamKey = (
   // Maximum nominal heap before last ditch GC.
    JSGC_MAX_BYTES          = 0,
    // Number of JS_malloc bytes before last ditch GC.
    JSGC_MAX_MALLOC_BYTES   = 1,
    // Obsolete since JSAPI 12 JSGC_STACKPOOL_LIFESPAN = 2,
    // Obsolete since JSAPI 6 JSGC_TRIGGER_FACTOR = 3,
    //* Amount of bytes allocated by the GC. */
    JSGC_BYTES = 3,
    //* Number of times when GC was invoked. */
    JSGC_NUMBER = 4,
    //* Max size of the code cache in bytes. */
    JSGC_MAX_CODE_CACHE_BYTES = 5,
    //* Select GC mode. */
    JSGC_MODE = 6,
    //* Number of GC chunks waiting to expire. */
    JSGC_UNUSED_CHUNKS = 7,
    //* Total number of allocated GC chunks. */
    JSGC_TOTAL_CHUNKS = 8,

    //* Max milliseconds to spend in an incremental GC slice. */
    JSGC_SLICE_TIME_BUDGET = 9,

    //* Maximum size the GC mark stack can grow to. */
    JSGC_MARK_STACK_LIMIT = 10,

    (*
     * GCs less than this far apart in time will be considered 'high-frequency GCs'.
     * See setGCLastBytes in jsgc.cpp.
     *)
    JSGC_HIGH_FREQUENCY_TIME_LIMIT = 11,

    (* Start of dynamic heap growth. *)
    JSGC_HIGH_FREQUENCY_LOW_LIMIT = 12,

    (* End of dynamic heap growth. *)
    JSGC_HIGH_FREQUENCY_HIGH_LIMIT = 13,

    (* Upper bound of heap growth. *)
    JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MAX = 14,

    (* Lower bound of heap growth. *)
    JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MIN = 15,

    (* Heap growth for low frequency GCs. *)
    JSGC_LOW_FREQUENCY_HEAP_GROWTH = 16,

    (*
     * If false, the heap growth factor is fixed at 3. If true, it is determined
     * based on whether GCs are high- or low- frequency.
     *)
    JSGC_DYNAMIC_HEAP_GROWTH = 17,

    (* If true, high-frequency GCs will use a longer mark slice. *)
    JSGC_DYNAMIC_MARK_SLICE = 18,

    (* Number of megabytes of analysis data to allocate before purging. *)
    JSGC_ANALYSIS_PURGE_TRIGGER = 19,

    (* Lower limit after which we limit the heap growth. *)
    JSGC_ALLOCATION_THRESHOLD = 20,

    (*
     * We decommit memory lazily. If more than this number of megabytes is
     * available to be decommitted, then JS_MaybeGC will trigger a shrinking GC
     * to decommit it.
     *)
    JSGC_DECOMMIT_THRESHOLD = 21


  );

  JSGCMode = (
    //* Perform only global GCs. */
    JSGC_MODE_GLOBAL = 0,
    //* Perform per-compartment GCs until too much garbage has accumulated. */
    JSGC_MODE_COMPARTMENT = 1,
    //* Collect in short time slices rather than all at once. Implies
    // * JSGC_MODE_COMPARTMENT.
    JSGC_MODE_INCREMENTAL = 2
  );

procedure JS_SetGCParameter(rt: PJSRuntime; key: JSGCParamKey; value: uint32);
  cdecl; external SpiderMonkeyLib;
function JS_GetGCParameter(rt: PJSRuntime; key: JSGCParamKey): uint32;
  cdecl; external SpiderMonkeyLib;

procedure JS_SetGCParameterForThread(cx: PJSContext; key: JSGCParamKey; value: uint32);
  cdecl; external SpiderMonkeyLib;
function  JS_GetGCParameterForThread(cx: PJSContext; key: JSGCParamKey): uint32;
  cdecl; external SpiderMonkeyLib;

/// Set the size of the native stack that should not be exceeded
// - to disable stack size checking, just pass 0 as value
procedure JS_SetNativeStackQuota(tr: PJSRuntime; stackSize: size_t);
  cdecl; external SpiderMonkeyLib;

/// Flush the code cache for the current thread
// - The operation might be delayed if the cache cannot be flushed currently
// because native code is currently executing.
procedure JS_FlushCaches(cx: PJSContext); cdecl; external SpiderMonkeyLib;

type
/// * Finalizes external strings created by JS_NewExternalString.
  PJSStringFinalizer = ^JSStringFinalizer;

  JSStringFinalizerOp = procedure(fin: PJSStringFinalizer; chars: Pjschar);  cdecl;
  JSStringFinalizer = record
    finalize: JSStringFinalizerOp;
  end;

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
function JS_NewExternalString(cx: PJSContext; chars: pjschar; length: size_t;
  const fin: PJSStringFinalizer): PJSString; cdecl; external SpiderMonkeyLib;

/// Returns the external-string finalizer index for this string, or -1 if it is
// an "internal" (native to JS engine) string.
//TODO - in current compiled version there is no this functions. seems like a BUG!!!!!!!!
//JS_GetExternalStringGCType
//function JS_IsExternalString(rt: PJSRuntime; str: PJSString): intN; cdecl; external SpiderMonkeyLib;


// For detailed comments on the function pointer types, see jspubtd.h
type
  JSClassInternal = procedure; cdecl;
  PJSClass = ^JSClass;
  JSClass =  record
    name:               PCChar;
    flags:              uint32;
    //* Mandatory non-null function pointer members. */
    addProperty:        JSPropertyOp;
    delProperty:        JSDeletePropertyOp;
    getProperty:        JSPropertyOp;
    setProperty:        JSStrictPropertyOp;
    enumerate:          pointer; //JSEnumerateOp; or JSNewEnumerateOp
    resolve:            JSResolveOp;
    convert:            JSConvertOp;
    //* Optionally non-null members start here. */
    finalize:           JSFinalizeOp; {
                                      in jsapi.h file comments finalize is Mandatory
                                      but in real it is Optionally
                                      in examles finalize is not assigned
                                      }
    checkAccess:        JSCheckAccessOp;
    call:               JSNative;
    hasInstance:        JSHasInstanceOp;
    construct:          JSNative;
    trace:              JSTraceOp;
    reserved:           array [0..39] of pointer;
  end;

const
  /// JSClass instance objects have private slot
  JSCLASS_HAS_PRIVATE            =   1 shl 0;
  /// JSClass instance has JSNewEnumerateOp hook
  JSCLASS_NEW_ENUMERATE          =   1 shl 1;
  /// JSClass instance has JSNewResolveOp hook
  JSCLASS_NEW_RESOLVE            =   1 shl 2;
  /// JSClass instance private is (nsISupports
  JSCLASS_PRIVATE_IS_NSISUPPORTS =   1 shl 3;
  /// JSClass instance objects are DOM
  JSCLASS_IS_DOMJSCLASS          =   1 shl 4;
  /// Correctly implements GC read and write barriers
  JSCLASS_IMPLEMENTS_BARRIERS    =   1 shl 5;
  /// JSClass instance objects of this class act like the value undefined,
  // in some contexts
  JSCLASS_EMULATES_UNDEFINED     =   1 shl 6;
  /// Reserved for embeddings.
  JSCLASS_USERBIT1               =   1 shl 7;
  /// JSClass instance room for 8 flags below
  JSCLASS_RESERVED_SLOTS_SHIFT   = 8;
  /// JSClass instance and 16 above this field
  JSCLASS_RESERVED_SLOTS_WIDTH   = 8;

  JSCLASS_RESERVED_SLOTS_MASK    = (JSUint32(1) shl JSCLASS_RESERVED_SLOTS_WIDTH)-1;
  JSCLASS_HIGH_FLAGS_SHIFT       = (JSCLASS_RESERVED_SLOTS_SHIFT + JSCLASS_RESERVED_SLOTS_WIDTH);
  JSCLASS_IS_ANONYMOUS           = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+0));
  JSCLASS_IS_GLOBAL              = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+1));
  JSCLASS_INTERNAL_FLAG2         = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+2));
  JSCLASS_INTERNAL_FLAG3         = (1 shl (JSCLASS_HIGH_FLAGS_SHIFT+3));

  // JSProto_LIMIT is length of #include "jsproto.tbl"
  JSProto_LIMIT = 38;
  JSCLASS_GLOBAL_SLOT_COUNT         = (JSProto_LIMIT * 3 + 25);

  JSCLASS_GLOBAL_FLAGS              = JSCLASS_IS_GLOBAL or
                                      (((JSCLASS_GLOBAL_SLOT_COUNT) and JSCLASS_RESERVED_SLOTS_MASK) shl JSCLASS_RESERVED_SLOTS_SHIFT);

procedure JS_DestroyIdArray(cx: PJSContext; ida: PJSIdArray);
  cdecl; external SpiderMonkeyLib;
function JS_ValueToId(cx: PJSContext; v: jsval; var id: jsid): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_IdToValue(cx: PJSContext; id: jsid; var v: jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

const
  /// used by JS_ResolveStub() callback: resolve a qualified property id
  JSRESOLVE_QUALIFIED     = $01;
  /// used by JS_ResolveStub() callback: resolve on the left of assignment
  JSRESOLVE_ASSIGNING     = $02;
  /// used by JS_ResolveStub() callback: 'if (o.p)...' or '(o.p) ?...:...'
  JSRESOLVE_DETECTING     = $04;
  /// used by JS_ResolveStub() callback: var, const, or function prolog op
  JSRESOLVE_DECLARING     = $08;
  /// used by JS_ResolveStub() callback: class name used when constructing
  JSRESOLVE_CLASSNAME     = $10;
  /// used by JS_ResolveStub() callback: resolve inside a with statement
  JSRESOLVE_WITH          = $20;

/// default callback matching JSPropertyOp prototype of JSClass
function JS_PropertyStub(cx: PJSContext; var obj: PJSObject; var id: jsid; vp: pjsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// default callback matching JSStrictPropertyOp prototype of JSClass
function JS_StrictPropertyStub(cx: PJSContext; var obj: PJSObject; var id: jsid;
  _strict: JSBool; vp: pjsval): JSBool; cdecl; external SpiderMonkeyLib;

/// default callback matching JSEnumerateOp prototype of JSClass
function JS_EnumerateStub(cx: PJSContext; var obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// default callback matching JSResolveOp prototype of JSClass
function JS_ResolveStub(cx: PJSContext; var obj: PJSObject; var id: jsid): JSBool;
  cdecl; external SpiderMonkeyLib;

/// default callback matching JSConvertOp prototype of JSClass
function JS_ConvertStub(cx: PJSContext; var obj: PJSObject; _type: JSType; vp: pjsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// default callback matching JSDeletePropertyOp prototype of JSClass
function JS_DeletePropertyStub(cx: PJSContext; var obj: PJSObject; var id: jsid;
  succeeded: PJSBool): JSBool; cdecl; external SpiderMonkeyLib;

type
  /// JS object constant definition
  JSConstDoubleSpec = record
    dval: jsdouble;
    name: PCChar;
    flags: uint8;
    spare: array[0..2] of uint8;
  end;
  PJSConstDoubleSpec = ^JSConstDoubleSpec;
  TJSConstDoubleSpecArray = array of JSConstDoubleSpec;

  /// JS object property defenition
  PJSJitInfo = pointer;

  /// Wrappers to replace StrictPropertyOp for JSPropertySpecs
  // - This will allow us to pass one JSJitInfo per function with the property
  // spec, without additional field overhead.
  JSStrictPropertyOpWrapper = record
    op: JSStrictPropertyOp;
    info: PJSJitInfo;
  end;

  /// Wrappers to replace PropertyOp for JSPropertySpecs
  // - This will allow us to pass one JSJitInfo per function with the property
  // spec, without additional field overhead.
  JSPropertyOpWrapper = record
    op: JSPropertyOp;
    info: PJSJitInfo;
  end;

  /// defines custom JSProperty
  // - This will allow us to pass one JSJitInfo per function with the property
  // spec, without additional field overhead.
  JSPropertySpec = record
    name: PCChar;
    tinyid: int8;
    flags: uint8;
    getter: JSPropertyOpWrapper;
    setter: JSStrictPropertyOpWrapper;
  end;
  PJSPropertySpec = ^JSPropertySpec;
  TJSPropertySpecDynArray = array of JSPropertySpec;

  /// Wrappers to replace JSNatives for JSFunctionSpecs
  // - This will allow us to pass one JSJitInfo per function with the property
  // spec, without additional field overhead.
  JSNativeWrapper = record
    op: JSNative;
    info: PJSJitInfo;
  end;

  /// Defines a single function for an object
  JSFunctionSpec = record
    name: PCChar;
    call: JSNativeWrapper;
    nargs: uint16;
    flags: uint16;
    selfHostedName: PCChar;
  end;
  PJSFunctionSpec = ^JSFunctionSpec;
  TJSFunctionSpecArray = array of JSFunctionSpec;

/// Make a JSClass accessible to JavaScript code by creating its prototype,
// constructor, properties, and functions.
// - see https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JS_InitClass
function JS_InitClass(cx: PJSContext; obj: PJSObject; parent_proto: PJSObject;
    clasp: PJSClass; _constructor: JSNative; nargs: uintN;
    ps: PJSPropertySpec; fs: PJSFunctionSpec;
    static_ps: PJSPropertySpec; static_fs: PJSFunctionSpec): PJSObject;
  cdecl; external SpiderMonkeyLib ;

/// retrieve the JSClass of a given object
// - JS_GetClass() expects only one parameter in every case
function JS_GetClass(obj: PJSObject): PJSClass;
  cdecl; external SpiderMonkeyLib name 'JS_GetClass';
function JS_InstanceOf(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_HasInstance(cx: PJSContext; obj: PJSObject; v: jsval; bp: PJSBool): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_GetPrivate(obj: PJSObject): Pointer;
  cdecl; external SpiderMonkeyLib;
procedure JS_SetPrivate(obj: PJSObject; data: Pointer);
  cdecl; external SpiderMonkeyLib;
function JS_GetInstancePrivate(cx: PJSContext; obj: PJSObject; clasp: PJSClass; argv: pjsval): Pointer;
  cdecl; external SpiderMonkeyLib;
function JS_GetPrototype(cx: PJSContext; obj: PJSObject; var protop: PJSObject):JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_SetPrototype(cx: PJSContext; obj: PJSObject; proto: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_GetParent(obj: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;
function JS_SetParent(cx: PJSContext; obj: PJSObject; parent: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_GetConstructor(cx: PJSContext; proto: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Get a unique identifier for obj, good for the lifetime of obj (even if it
// is moved by a copying GC)
// - Return false on failure (likely out of memory), and true with idp
// containing the unique id on success
function JS_GetObjectId(cx: PJSContext; obj: PJSObject; idp: Pjsid): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  ZoneSpecifier = JSuintptr;

const
  zsFreshZone = JSuintptr(0);
  zsSystemZone = JSuintptr(1);
  zsSpecificZones = JSuintptr(2);

type
  CompartmentOptions = record
    zoneSpec: ZoneSpecifier;
    hasVersion: boolean;
    version: JSVersion;
  end;
  PCompartmentOptions = ^CompartmentOptions;

/// JS_NewGlobalObject creates a new global object based on the specified class
// - The new object has no parent. It initially has no prototype either,
// since it is typically the first object created;
// call JS_InitStandardClasses to create all the standard objects,
// including Object.prototype, and set the global object's prototype
function JS_NewGlobalObject(cx: PJSContext; clasp: PJSClass; principals: PJSPrincipals;
  options: PCompartmentOptions): PJSObject; cdecl; external SpiderMonkeyLib;

/// declare entering a safe compartment of the specified object
// - NB: This API is infallible; a NULL return value does not indicate error
function JS_EnterCompartment(cx: PJSContext; target: PJSObject): PJSCompartment;
  cdecl; external SpiderMonkeyLib;

/// declare leaving the safe compartment of the specified object
procedure JS_LeaveCompartment(cx: PJSContext; oldCompartment: PJSCompartment);
  cdecl; external SpiderMonkeyLib;

/// JS_NewObject creates a new object based on a specified class, prototype,
// and parent object
// - cx is a pointer to a context associated with the runtime
// in which to establish the new object.
// - clasp is a pointer to an existing class to use for internal methods, such as finalize
// - proto is an optional pointer to the prototype object with which to associate the new object
// see https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JS_NewObject
function JS_NewObject(cx: PJSContext; clasp: PJSClass; proto: PJSObject; parent: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Queries the [[Extensible]] property of the object.
function JS_IsExtensible(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Unlike JS_NewObject, JS_NewObjectWithGivenProto does not compute a default
// proto if proto's actual parameter value is null
function JS_NewObjectWithGivenProto(cx: PJSContext; clasp: PJSClass;
  proto: PJSObject; parent: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;

/// Freeze obj, and all objects it refers to, recursively
// - This will not recurse through non-extensible objects, on the assumption
// that those are already deep-frozen.
function JS_DeepFreezeObject(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Freezes an object; see ES5's Object.freeze(obj) method
function JS_FreezeObject(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// As of SpiderMonkey 1.8.8, JS_ConstructObject and JS_ConstructObjectWithArguments
// have been removed from the JSAPI.
// The preferred alternative is to save a copy of the constructor function for the class,
// then to call it using JS_New.
//  function JS_ConstructObject(cx: PJSContext; clasp: PJSClass; proto:
//    PJSObject; parent: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;
//  function JS_ConstructObjectWithArguments(cx: PJSContext; clasp: PJSClass; proto: PJSObject;
//    parent: PJSObject; argc: uintN; argv: pjsval): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create an object as though by using the new keyword and a JavaScript function
// - for instance:
// ! JS_New(cx, ctor, argc, argv)
// is equivalent to the JavaScript expression
// ! new ctor(argv[0], argv[1], ... argv[argc-1]).
// If ctor is not an object that can be used as a constructor, a TypeError is raised.
function JS_New(cx: PJSContext; ctor: PJSObject; argc: uintN; argv: Pjsval): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create an object that is a property of another object
function JS_DefineObject(cx: PJSContext; obj: PJSObject; name: PCChar; clasp: PJSClass;
  proto: PJSObject; attrs: uintN): PJSObject; cdecl; external SpiderMonkeyLib;

/// JS_DefineConstDoubles creates one or more properties for a specified object, obj,
// where each property consists of a double value.
//- Each property is automatically assigned attributes as specified in the flags
// field of the JSConstDoubleSpec structure pointed to by cds.
//- If flags is set to 0, the attributes for the property are automatically set
// to JSPROP_PERMANENT | JSPROP_READONLY
function JS_DefineConstDoubles(cx: PJSContext; obj: PJSObject; cds: PJSConstDoubleSpec): JSBool;
  cdecl; external SpiderMonkeyLib ;

/// JS_DefineProperties creates properties on a specified object, obj.
// Each property is defined as though by calling JS_DefinePropertyWithTinyId
function JS_DefineProperties(cx: PJSContext; obj: PJSObject; ps: PJSPropertySpec): JSBool; cdecl; external SpiderMonkeyLib ;

/// JS_DefineProperty is the fundamental operation on which several more convenient,
//  higher-level functions are based, including JS_DefineFunction, JS_DefineFunctions,
//  JS_DefineProperties, JS_DefineConstDoubles, JS_DefineObject, and JS_InitClass.
// - It differs from JS_SetProperty in that:
//  it does not behave like ordinary property assignment in the JavaScript language;
//  it allows the application to specify additional details (getter, setter, and attrs)
//  governing the new property's behavior;
//  it never calls a setter;
//  it can call the JSClass.addProperty callback when JS_SetProperty would not,
//  because it can replace an existing property.
// - The parameters specify the new property's name, initial stored value, getter,
// setter, and property attributes (attrs).
function JS_DefineProperty(cx: PJSContext; obj: PJSObject; const name: PCChar;
  value: jsval; getter: JSPropertyOp; setter: JSStrictPropertyOp; attrs: uintN): JSBool;
    cdecl; external SpiderMonkeyLib;

/// JS_DefinePropertyById is the same as JS_DefineProperty but takes a jsid for the property name
function JS_DefinePropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  value: jsval; getter: JSPropertyOp; setter: JSStrictPropertyOp; attrs: uintN): JSBool;
    cdecl; external SpiderMonkeyLib;

/// JS_DefineOwnProperty implements the ECMAScript defined function Object.defineProperty
// -  So the same restrictions apply as for that function
// (e.g. it is not possible to change a non-configurable property).
// - descriptor is supposed to be a property descriptor, this means you need to
// create an object with properties such as value, writable, get or set.
// - See Object.defineProperty for a list of possible fields.
// - A getter or setter defined with this functions will be observable from JS code.
function JS_DefineOwnProperty(cx: PJSContext; obj: PJSObject; id: jsid;
  descriptor: jsval; bp: PJSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Determine the attributes (JSPROP_* flags) of a property on a given object
//-  If the object does not have a property by that name, *foundp will be
// JS_FALSE and the value of *attrsp is undefined.
function JS_GetPropertyAttributes(cx: PJSContext; obj: PJSObject;
    const name: PCChar; var attrsp: uintN; var foundp: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Set the attributes of a property on a given object
// - If the object does not have a property by that name, *foundp will be
// JS_FALSE and nothing will be altered.
function JS_SetPropertyAttributes(cx: PJSContext; obj: PJSObject; const name: PCChar;
    attrs: uintN; var foundp: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

function JS_DefinePropertyWithTinyId(cx: PJSContext; obj: PJSObject; const name: PCChar;
 tinyid: int8; value: jsval; getter: JSPropertyOp; setter: JSStrictPropertyOp;
 attrs: uintN): JSBool; cdecl; external SpiderMonkeyLib;

// JS_AliasProperty is Obsolete since JSAPI 8
//function JS_AliasProperty(cx: PJSContext; obj: PJSObject; name: PAnsiChar; alias: PAnsiChar): JSBool;
// cdecl; external SpiderMonkeyLib ;

/// Determine whether a property is already physically present on a JSObject
// - For native objects—objects whose properties are stored in the default data
// structure provided by SpiderMonkey, these functions simply check that data
// structure to see if the specified field is present.
// They do not search anywhere else for the property. This means that:
// - The prototype chain of obj is not searched.
// - The object's JSClass.resolve hook is not called, so lazily defined
// properties are not found. (This is the only API that can directly detect
// that a lazily resolved property has not yet been resolved.)
// - Shared, permanent, delegated properties are not found. (Such properties
// are an implementation detail of SpiderMonkey. They are meant to be a
// transparent optimization; this is the only API that breaks the abstraction)
function JS_AlreadyHasOwnProperty(cx: PJSContext; obj: PJSObject; const name: PCChar;
  var found: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Determine whether a property is already physically present on a JSObject
// - this function will locate the property not by name, but by its jsid
function JS_AlreadyHasOwnPropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  var found: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_HasProperty searches an object, obj, and its prototype chain, for
// a property with the specified name
// - It behaves like the JavaScript expression name in obj
function JS_HasProperty(cx: PJSContext; obj: PJSObject; const name: PCChar;
  var found: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_HasProperty searches an object, obj, and its prototype chain, for
// a property with the specified jsid
function JS_HasPropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  var found: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Determine if a specified property exists, according to its name
// - On success, *vp receives the stored value of the property, if any.
function JS_LookupProperty(cx: PJSContext; obj: PJSObject; name: PCChar;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Determine if a specified property exists, according to its jsid
function JS_LookupPropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

function JS_LookupPropertyWithFlags(cx: PJSContext; obj: PJSObject;
    const name: PCChar; flags: uintN; var vp: jsval): JSBool;
  cdecl; external SpiderMonkeyLib;
function JS_LookupPropertyWithFlagsById(cx: PJSContext; obj: PJSObject;
    id: jsid; flags: uintN; var objp: PJSObject; var vp: jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  JSPropertyDescriptor = record
    obj: PJSObject;
    attrs: uintN;
    shortid: uintN;
    getter: JSPropertyOp;
    setter: JSStrictPropertyOp;
    value: jsval;
  end;
  PJSPropertyDescriptor = ^JSPropertyDescriptor;

/// Finds a specified property of an object and gets a detailed description of that property
function JS_GetPropertyDescriptorById(cx: PJSContext; obj: PJSObject; id: jsid; flags: uintN;
  objp: PPJSObject; desc: PJSPropertyDescriptor): JSBool; cdecl; external SpiderMonkeyLib;

/// Finds a specified property by name and retrieves its value
// - JS_GetProperty examines a specified JS object obj and its prototype chain
// for a property with the specified name
// - It behaves like the JavaScript expression obj[name].
function JS_GetProperty(cx: PJSContext; obj: PJSObject; const name: PCChar;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Finds a specified property by jsid and retrieves its value
// - JS_GetProperty examines a specified JS object obj and its prototype chain
// for a property with the specified jsid
function JS_GetPropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Finds a specified property by jsid and retrieves its value or a default value
function JS_GetPropertyByIdDefault(cx: PJSContext; obj: PJSObject; id: jsid;
  def: jsval; var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

// The following functions behave like JS_GetProperty and
// JS_GetPropertyById except when operating on E4X XML objects
//  extern JS_PUBLIC_API(JSBool) JS_GetMethodById(JSContext *cx,
//    JSObject *obj, jsid id, JSObject **objp, jsval *vp);
//  extern JS_PUBLIC_API(JSBool) JS_GetMethod(JSContext *cx,
//    JSObject *obj, const char *name, JSObject **objp, jsval *vp);

/// JS_SetProperty assigns the value vp to the property name of the object obj
// - it behaves like the JavaScript expression obj[name] = v
// - it will create the property if it does not exist, but for details see
//  https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JS_SetProperty
// - remark: in mozila description "sealed" object they talking about is OBSOLETE!
function JS_SetProperty(cx: PJSContext; obj: PJSObject; const name: PCChar;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_SetProperty assigns the value vp to the property jsid of the object obj
function JS_SetPropertyById(cx: PJSContext; obj: PJSObject; id: jsid;
  vp: Pjsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Removes a specified property from an object
function JS_DeleteProperty(cx: PJSContext; obj: PJSObject;
  const name: PCChar): JSBool; cdecl; external SpiderMonkeyLib;

/// Removes a specified property from an object and return the property value
function JS_DeleteProperty2(cx: PJSContext; obj: PJSObject; const name: PCChar;
  var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Removes a specified property from an object
function JS_DeletePropertyById(cx: PJSContext; obj: PJSObject; id: jsid): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Removes a specified property from an object and return the property value
function JS_DeletePropertyById2(cx: PJSContext; obj: PJSObject; id: jsid; var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// define unicode property
// - see JS_DefineProperty for details
function JS_DefineUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; const value: jsval;
    getter: JSPropertyOp; setter: JSStrictPropertyOp; attrs: uintN): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Determine the attributes (JSPROP_* flags) of a property on a given object
// - If the object does not have a property by that name, *foundp will be
// - JS_FALSE and the value of *attrsp is undefined.
function JS_GetUCPropertyAttributes(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t;
    var attrsp: uintN; var foundp: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// Set the attributes of a property on a given object.
// - If the object does not have a property by that name, *foundp will be
// - JS_FALSE and nothing will be altered.
function JS_SetUCPropertyAttributes(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t;
    attrs: uintN; var foundp: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

function JS_DefineUCPropertyWithTinyId(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; tinyid: int8; value: jsval;
    getter: JSPropertyOp; setter: JSStrictPropertyOp; attrs: uintN): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Determine whether a property is already physically present on a JSObject
// - For native objects—objects whose properties are stored in the default
// data structure provided by SpiderMonkey, these functions simply check that
// data structure to see if the specified field is present. They do not search
// anywhere else for the property. This means that:
//  The prototype chain of obj is not searched.
//  The object's JSClass.resolve hook is not called, so lazily defined
// properties are not found.
//  (This is the only API that can directly detect that a lazily resolved
// property has not yet been resolved.)
//  Shared, permanent, delegated properties are not found.
// - (Such properties are an implementation detail of SpiderMonkey.
// They are meant to be a transparent optimization; this is the only API
// that breaks the abstraction)
function JS_AlreadyHasOwnUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; var foundp: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

/// same as JS_HasProperty but unicode
function JS_HasUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; var found: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Determine if a specified property exists
// - On success, *vp receives the stored value of the property, if any.
function JS_LookupUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t;
    var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

function JS_GetUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; var vp : jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

function JS_SetUCProperty(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; var vp : jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

function JS_DeleteUCProperty2(cx: PJSContext; obj: PJSObject;
    const name: Pjschar; namelen: size_t; var rval: jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// JS_NewArrayObject creates a new array object with the specified length.
// - If vector is non-null, then for each index i from 0 to length - 1,
// - JS_NewArrayObject defines an enumerable array element with the value
// vector[i] on the new array (This means that if length is nonzero and vector
// is null, the result is like the JavaScript expression new Array(length): i.e.
// the new array has the specified length, but it doesn't have any elements)
// - On success, JS_NewArrayObject returns the new array object.
// - Otherwise it reports an error as though by calling JS_ReportOutOfMemory
// and returns NULL.
// - IMPORTANT! It is often better to call JS_NewArrayObject(cx, 0, NULL),
// store the returned object in a GC root, and then populate its elements
// with JS_SetElement or JS_DefineElement: this avoids unrooted jsvals in
// vector from being subject to garbage collection until the new object
// has been populated
function JS_NewArrayObject(cx: PJSContext; length: jsint;
  vector: PjsvalVector): PJSObject; cdecl; external SpiderMonkeyLib;

/// check if the supplied object is an array
function JS_IsArrayObject(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// retrieve the length of an object arrray
function JS_GetArrayLength(cx: PJSContext; obj: PJSObject;
  var length: jsuint): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_SetArrayLength sets the length property of an object obj
// - length indicates the number of elements
// - JS_SetArrayLength(cx, obj, n) is exactly the same as setting the length
// property of obj to n using JS_SetProperty
// - This is true even if obj is not an Array object
// - You can call JS_SetArrayLength either to set the number of elements for an
// array object you created without specifying an initial number of elements,
// or to change the number of elements allocated for an array
// - If you set a shorter array length on an existing array, the elements
// that no longer fit in the array are destroyed
// - Setting the number of array elements does not initialize those elements
// - To initialize an element call JS_DefineElement
// - If you call JS_SetArrayLength on an existing array, and length
// is less than the highest index number for previously defined elements, all
// elements greater than or equal to length are automatically deleted
// - On success, JS_SetArrayLength returns JS_TRUE. Otherwise it returns JS_FALSE.
function JS_SetArrayLength(cx: PJSContext; obj: PJSObject; length: jsuint): JSBool;
  cdecl; external SpiderMonkeyLib ;

/// JS_DefineElement defines a numeric property for a specified object, obj
// - Starting in SpiderMonkey 1.8.5, jsval can store a full 32-bit integer,
// so index is any 32-bit integer
function JS_DefineElement(cx: PJSContext; obj: PJSObject; index: jsint; value: jsval;
    getter: JSPropertyOp; setter: JSPropertyOp; attrs: uintN): JSBool;
  cdecl; external SpiderMonkeyLib;

// JS_AliasElement OBSOLETE
function JS_AlreadyHasOwnElement(cx: PJSContext; obj: PJSObject; index: jsint;
  var foundp: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// check if an object array has an element at the supplied index
function JS_HasElement(cx: PJSContext; obj: PJSObject; index: jsint;
  var foundp: JSBool): JSBool; cdecl; external SpiderMonkeyLib;

/// determine if a specified numeric property exists
// - examines a specified JavaScript object, obj, for a numeric property numbered index
// - On success, *vp receives the stored value of the property, if any.
function JS_LookupElement(cx: PJSContext; obj: PJSObject; index: jsint;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// find a specified numeric property of an object and return its current value
function JS_GetElement(cx: PJSContext; obj: PJSObject; index: jsint;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// assign a value to a numeric property of an object
function JS_SetElement(cx: PJSContext; obj: PJSObject; index: jsint;
  var vp: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// removes a specified element or numeric property from an object
function JS_DeleteElement(cx: PJSContext; obj: PJSObject; index: jsint): JSBool;
  cdecl; external SpiderMonkeyLib;

/// removes a specified element or numeric property from an object
// - this function also returns the deleted element
function JS_DeleteElement2(cx: PJSContext; obj: PJSObject; index: jsint;
  var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

//JS_ClearScope - OBSOLETE

/// JS_Enumerate gets the ids of all own properties of the specified object, obj,
// - that have the JSPROP_ENUMERATE attribute. This calls obj's JSClass.enumerate hook.
// - On success, JS_Enumerate returns a pointer to the first element of an
// array of property IDs.
// - The application must free this array using JS_DestroyIdArray. On error
// or exception, JS_Enumerate returns NULL.
// - Warning: The property ids in the returned JSIdArray are subject to
// garbage collection.
// - Therefore a program that loops over the property ids must either root them
// all, ensure that the properties are not deleted (in a multithreaded program
// this requires even greater care), or ensure that garbage collection does not occur
function JS_Enumerate(cx: PJSContext; obj: PJSObject): PJSIdArray;
  cdecl; external SpiderMonkeyLib;

/// Create an object to iterate over enumerable properties of obj, in arbitrary
// - property definition order.  NB: This differs from longstanding for..in loop
// - order, which uses order of property definition in obj.
function JS_NewPropertyIterator(cx: PJSContext; obj: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Return true on success with *idp containing the id of the next enumerable
// - property to visit using iterobj, or JSID_IS_VOID if there is no such property
// - left to visit.  Return false on error.
function JS_NextProperty(cx: PJSContext; iterobj: PJSObject; var idp: jsid): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Check whether a running script may access a given object property.
// - The access check proceeds as follows.
// - If obj has custom JSObjectOps, the access check is delegated to the
// JSObjectOps.checkAccess callback.
// - Otherwise, if obj's class has a non-null JSClass.checkAccess callback,
// then it is called to perform the check.
// - Otherwise, if a runtime-wide check-object-access callback has been
// installed by calling JS_SetCheckObjectAccessCallback, then that is called
// to perform the check.
// - Otherwise, access is granted.
// - On success, JS_CheckAccess returns JS_TRUE, *vp is set to the current
// value stored value, of the specified property,  and *attrsp is set to the
// property's attributes.
// - On error or exception, including if access is denied, JS_CheckAccess
// returns JS_FALSE, and the values left in *vp and *attrsp are undefined
function JS_CheckAccess(cx: PJSContext; obj: PJSObject; id: jsid; mode: JSAccessMode;
    var vp: jsval; var attrsp: uintN): JSBool; cdecl; external SpiderMonkeyLib;

/// read access an object's reserved slots
// - If a JSClass has JSCLASS_HAS_RESERVED_SLOTS(n) in its flags, with n > 0,
// or has a non-null JSClass.reserveSlots callback, then objects of that class
// have n reserved slots in which the application may store data.
// These fields are not directly exposed to scripts.
// - Reserved slots may contain any jsvalue, and the garbage collector will
// hold the value alive as long as the object itself is alive
// - Reserved slots may also contain private values to store pointer values
// (whose lowest bit is 0) or uint32_t, when non-JavaScript values must be stored;
// the garbage collector ignores such values when it sees them.
// - Note that private values must not be exposed directly to JavaScript.
// It's only legal to store and retrieve data from private values.  They cannot
// be returned from functions, set as properties on objects, embedded in
// JavaScript arrays, and so on
// - New objects' reserved slots are initialized to undefined.
// - TODO - collision in MSDN no parameter vp: pjsval but in jsapi.h is!
function JS_GetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32;
  vp: pjsval): JSBool; cdecl; external SpiderMonkeyLib;

/// write access an object's reserved slots
function JS_SetReservedSlot(cx: PJSContext; obj: PJSObject; index: uint32;
  v: jsval): JSBool; cdecl; external SpiderMonkeyLib;


// here must be Security protocol (*Principals*) but we do not use it.
// Mozilla recomend to not use it
// see https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_User_Guide#The_best_security_is_no_security_(really)


{ Functions and scripts }

/// JS_NewFunction creates a new JavaScript function implemented in Delphi
// - To create a new function implemented in JavaScript, use JS_CompileFunction
// - call is a C/C++ function pointer that the new function wraps
// - nargs is the number of arguments the function expects
// - flags must be 0. parent may be used to specify the new function's parent;
// NULL is usually the right thing here.
// - name is the name to assign to the function. If name is NULL, the new function
// has no name. (JS_GetFunctionId, passed the new function, will return NULL)
// - On success, JS_NewFunction returns a pointer to the newly created function
// - Otherwise it reports an out-of-memory error and returns NULL
function JS_NewFunction(cx: PJSContext; call: JSNative; nargs: uintN; flags: uintN;
  parent: PJSObject; const name: PCChar): PJSFunction; cdecl; external SpiderMonkeyLib;

/// Create the function with the name given by the id
// - JSID_IS_STRING(id) must be true.
function JS_NewFunctionById(cx: PJSContext; call: JSNative; nargs: uintN; flags: uintN;
  parent: PJSObject; id: jsid): PJSFunction; cdecl; external SpiderMonkeyLib;

/// Retrieves the object for a specified function.
// - fun should be a native function or JSAPI-compiled function!
// (result of a call to JS_CompileFunction, JS_CompileUCFunction)
function JS_GetFunctionObject(fun: PJSFunction): PJSObject; cdecl; external SpiderMonkeyLib;

/// Return the function's identifier as a JSString, or null if fun is unnamed.
// - The returned string lives as long as fun, so you don't need to root a saved
// - reference to it if fun is well-connected or rooted, and provided you bound
// - the use of the saved reference by fun's lifetime.
function JS_GetFunctionId(fun: PJSFunction): PJSString; cdecl; external SpiderMonkeyLib;

/// Return JSFUN_* flags for a specified function
function JS_GetFunctionFlags(fun: PJSFunction): uintN; cdecl; external SpiderMonkeyLib;

/// Return the arity (params count) for a specified function
function JS_GetFunctionArity(fun: PJSFunction): uint16; cdecl; external SpiderMonkeyLib;

/// Test whether a given object is a Function
function JS_ObjectIsFunction(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// check is the supplied object is callable
function JS_ObjectIsCallable(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// JS_DefineFunctions creates zero or more functions and makes them properties
// (methods) of a specified object, obj, as if by calling JS_DefineFunction repeatedly
// - fs is a pointer to the first element of an array of JSFunctionSpec records
// - This array is usually defined as a static global, with each record
// initialized using JS_FS or JS_FN.
// - Each array element defines a single function: its name, the native Delphi
// implementation, the number of JavaScript arguments the function expects,
// and any function flags and property attributes.
// - The last element of the array must contain 0 values
function JS_DefineFunctions(cx: PJSContext; obj: PJSObject;
  fs: PJSFunctionSpec): JSBool; cdecl; external SpiderMonkeyLib;

/// Create a native function and assign it as a property to a specified JS object
function JS_DefineFunction(cx: PJSContext; obj: PJSObject; name: PCChar;
    call: JSNative; nargs: uintN; attrs: uintN): PJSFunction;
  cdecl; external SpiderMonkeyLib;

/// Unicode version to create a native function
function JS_DefineUCFunction(cx: PJSContext; obj: PJSObject; name: Pjschar;
    namelen: size_t; call: JSNative; nargs: uintN; attrs: uintN): PJSFunction;
  cdecl; external SpiderMonkeyLib;

/// Create a native function and assign it as a property to a specified JS object
function JS_DefineFunctionById(cx: PJSContext; obj: PJSObject; id: jsid;
    call: JSNative; nargs: uintN; attrs: uintN): PJSFunction;
  cdecl; external SpiderMonkeyLib;

/// JS_CloneFunctionObject creates a new function object from funobj
// - The new object has the same code and argument list as funobj,
// but uses parent as its enclosing scope.
// - This can be helpful if funobj is an extant function that you wish to use as
// if it were enclosed by a newly-created global object
function JS_CloneFunctionObject(cx: PJSContext; funobj: PJSObject;
  parent: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;

/// validate a JavaScript statement
// - Given a buffer, return JS_FALSE if the buffer might become a valid
// javascript statement with the addition of more lines
// - Otherwise return JS_TRUE.  The intent is to support interactive compilation - accumulate
// - lines in a buffer until JS_BufferIsCompilableUnit is true, then pass it to
// the compiler.
function JS_BufferIsCompilableUnit(cx: PJSContext; obj: PJSObject;
  bytes: PCChar; length: size_t): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_CompileScript compiles a script source, for execution
// - The script is associated with a JS object.
// - source is the string containing the text of the script.
// - length indicates the size of the text version of the script in characters.
// - filename is the name of the file (or URL) containing the script.
// - This information is included in error messages if an error occurs during
// compilation.
// - Similarly, lineno is used to report the line number of the script or file
// where an error occurred during compilation.
// - If the script is not part of a larger document, lineno should be 1 (as the
// first line of a file is universally considered to be line 1, not line 0).
// - On success, JS_CompileScript and JS_CompileUCScript return an object
// representing the newly compiled script.
// - Otherwise, they report an error and return NULL.
// - To compile a script from an external file source rather than passing the
// actual script as an argument, use JS_CompileFile instead of JS_CompileScript
function JS_CompileScript(cx: PJSContext; obj: PJSObject; bytes: PCChar;
    length: size_t; filename: PCChar; lineno: uintN): PJSScript;
  cdecl; external SpiderMonkeyLib ;

/// Unicode version to compiles a script
function JS_CompileUCScript(cx: PJSContext; obj: PJSObject;
    chars: Pjschar; length: size_t; filename: PCChar; lineno: uintN): PJSScript;
  cdecl; external SpiderMonkeyLib ;


type
  TCStringVector = array[0..0] of PCChar;
  PCStringVector = ^TCStringVector;

/// JS_CompileFunction compiles a function from a text string, bytes, and
// optionally associates it with a JS object, obj
// - name is the name to assign to the newly created function.
// - nargs is the number of arguments the function takes, and argnames
// is a pointer to the first element of an array of names to assign each argument.
// - The number of argument names should match the number of arguments specified
// in nargs.
// - body is a string containing the source code of the function.
// - length is the length of the source code in characters.
// - filename is the name of the file (or URL) containing the function. This
// information is used in error messages if an error occurs during compilation.
// - Similarly, lineno is used to report the line number where an error occurred
// during compilation.
// - If both obj and name are non-null, the new function becomes a method of obj
// (a new property is defined on obj with the given name and the new Function
// object as its value).
function JS_CompileFunction(cx: PJSContext; obj: PJSObject; name: PCChar;
    nargs: uintN; argnames: PCStringVector; bytes: PCChar; length: size_t;
    filename: PCChar; lineno: uintN): PJSFunction; cdecl; external SpiderMonkeyLib;

/// JS_CompileUCFunction() is the Unicode version of JS_CompileFunction() function
function JS_CompileUCFunction(cx: PJSContext; obj: PJSObject; name: PCChar;
    nargs: uintN; argnames: PCStringVector;
    chars: Pjschar; length: size_t;
    filename: PCChar; lineno: uintN): PJSFunction; cdecl; external SpiderMonkeyLib;

/// decompiles a Script Object back into its JavaScript representation
function JS_DecompileScriptObject(cx: PJSContext; scriptObj: PJSObject;
  name: PCChar; indent: uintN): PJSString; cdecl; external SpiderMonkeyLib;

const
  /// API extension: OR this into indent to avoid pretty-printing the decompiled
  // source resulting from JS_DecompileFunction{,Body}.
  JS_DONT_PRETTY_PRINT: uintN = $8000;

/// generates the complete source code of a function declaration from a compiled function
function JS_DecompileFunction(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString;
  cdecl; external SpiderMonkeyLib;

/// generate the source code representing the body of a function, minus the
// function keyword, name, parameters, and braces
function JS_DecompileFunctionBody(cx: PJSContext; fun: PJSFunction; indent: uintN): PJSString;
  cdecl; external SpiderMonkeyLib;


/// Execute a compiled script.
// - On success, *rval receives the value from the last executed expression
// statement processed in the script
// - NB: JS_ExecuteScript and the JS_Evaluate*Script* quadruplets use the obj
// parameter as the initial scope chain header, the 'this' keyword value, and
// the variables object (ECMA parlance for where 'var' and 'function' bind
// names) of the execution context for script.
// - Using obj as the variables object is problematic if obj's parent (which is
// the scope chain link; see JS_SetParent and JS_NewObject) is not null: in
// this case, variables created by 'var x = 0', e.g., go in obj, but variables
// created by assignment to an unbound id, 'x = 0', go in the last object on
// the scope chain linked by parent.
// - ECMA calls that last scoping object the "global object", but note that many
// embeddings have several such objects.  ECMA requires that "global code" be
// executed with the variables object equal to this global object.  But these
// JS API entry points provide freedom to execute code against a "sub-global",
// i.e., a parented or scoped object, in which case the variables object will
// differ from the last object on the scope chain, resulting in confusing and
// non-ECMA explicit vs. implicit variable creation.
// - Caveat embedders: unless you already depend on this buggy variables object
// binding behavior, you should call JS_SetOptions(cx, JSOPTION_VAROBJFIX) or
// JS_SetOptions(cx, JS_GetOptions(cx) | JSOPTION_VAROBJFIX) -- the latter if
// someone may have set other options on cx already -- for each context in the
// application, if you pass parented objects as the obj parameter, or may ever
// pass such objects in the future.
// - Why a runtime option?  The alternative is to add six or so new API entry
// points with signatures matching the following six, and that doesn't seem
// worth the code bloat cost.  Such new entry points would probably have less
// obvious names, too, so would not tend to be used.  The JS_SetOption call,
// OTOH, can be more easily hacked into existing code that does not depend on
// the bug; such code can continue to use the familiar JS_EvaluateScript,
// etc., entry points.
function JS_ExecuteScript(cx: PJSContext; obj: PJSObject; scriptObj: PJSObject;
  var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

///  JS_EvaluateScript compiles and executes a script in the specified scope (obj)
// - Warning: this function perform conversion from Ansi to unicode (internaly
// SM is unicode), so for best performance convert your scripts from RawUTF8/Ansi
// to unicode using SynCommons  Utf8DecodeToRawUnicode /
// WinAnsiConvert.AnsiToRawUnicode and use JS_EvaluateUCScript()
// - For details about scope see JS_ExecuteScript
// - We recomend to use JSOPTION_VAROBJFIX and some of quality tools like
// jslint/jshint - jshint is prefered, because it suppotr ECMA5 (let strict
// and so on), jslint - only ECMA3
// - to avoid declare variables in global use var x = 10 instead of x = 10
function JS_EvaluateScript(cx: PJSContext; obj: PJSObject;
   bytes: PCChar; length: uintN; filename: PCChar; lineno: uintN;
   var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Unicode version of JS_EvaluateScript
// - For details about scope see JS_ExecuteScript
// - remember length is "the length of src, in characters" not in bytes
// for pjschar = PRawUnicode use length = (length(RawUnicode) shr 1)
function JS_EvaluateUCScript(cx: PJSContext; obj: PJSObject;
    chars: pjschar; length: uintN; filename: PCChar;
    lineno: uintN; var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// JS_CallFunction calls a specified function, fun, on an object, obj
// - In terms of function execution, the object is treated as this
// - Warning: Calling JS_CallFunction is safe only if the fun argument could
// be passed to JS_GetFunctionObject safely: that is, it is a function
// implemented by a JSNative or JSFastNative or the result of a call
// to JS_CompileFunction, JS_CompileUCFunction, JS_CompileFunctionForPrincipals,
// or JS_CompileUCFunctionForPrincipals.
// - Passing any other JSFunction pointer can lead to a crash or worse
function JS_CallFunction(cx: PJSContext; this: PJSObject; fun: PJSFunction;
    argc: uintN; argv: pjsval; var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Call a method of an object by name.
// - JS_CallFunctionName executes a function-valued property, name, belonging
// to a specified JS object, obj
function JS_CallFunctionName(cx: PJSContext; this: PJSObject; const name: PCChar;
  argc: uintN; argv: Pjsval; var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// Calls a specified JS function
// - fval is function value
function JS_CallFunctionValue(cx: PJSContext; this: PJSObject; fval: jsval;
  argc: uintn; argv: Pjsval; var rval: jsval): JSBool; cdecl; external SpiderMonkeyLib;

// These functions allow setting an operation callback that will be called
// from the thread the context is associated with some time after any thread
// triggered the callback using JS_TriggerOperationCallback(cx).
//
// In a threadsafe build the engine internally triggers operation callbacks
// under certain circumstances (i.e. GC and title transfer) to force the
// context to yield its current request, which the engine always
// automatically does immediately prior to calling the callback function.
// The embedding should thus not rely on callbacks being triggered through
// the external API only.
//
// Important note: Additional callbacks can occur inside the callback handler
// if it re-enters the JS engine. The embedding must ensure that the callback
// is disconnected before attempting such re-entry.

/// Set a callback function that is automatically called periodically while
// JavaScript code runs
// - cx is a Pointer to a JSContext in which this callback was installed.
// - The callback may use this context to call JSAPI functions, but it
// should first use JS_SetOperationCallback
// - to set the context's operation callback to NULL. Otherwise the
// engine may call the operation callback again, reentering it.
// - Provides request. In JS_THREADSAFE builds, the JavaScript engine calls
// this callback only from within an active request on cx.
// - The callback does not need to call JS_BeginRequest()
// - Some common uses for an operation callback are:
// To run garbage collection periodically, by calling JS_MaybeGC;
// To periodically take a break from script execution to update the UI (though
// note that Mozilla does not do this, by design);
// To enforce application limits on the amount of time a script may run.
// (In this case, the callback may terminate the script by returning JS_FALSE.)
function JS_SetOperationCallback(cx: PJSContext;
  callback: JSOperationCallback): JSOperationCallback; cdecl; external SpiderMonkeyLib;

/// retrieve the callback function that is automatically called periodically while
// JavaScript code runs in the given execution context
function JS_GetOperationCallback(cx: PJSContext): JSOperationCallback;
  cdecl; external SpiderMonkeyLib;

/// triggers a callback set using JS_SetOperationCallback
procedure JS_TriggerOperationCallback(rt: PJSRuntime); cdecl; external SpiderMonkeyLib;

/// determines if a script or function is currently executing in a specified
// JSContext, cx
// - If a script is executing, JS_IsRunning returns JS_TRUE
// - Otherwise it returns JS_FALSE.
function JS_IsRunning(cx: PJSContext): JSBool; cdecl; external SpiderMonkeyLib;


{ Strings process }

// NB: JS_NewUCString takes ownership of bytes on success, avoiding a copy;
// but on error (signified by null return), it leaves chars owned by the
// caller. So the caller must free bytes in the error case, if it has no use
// for them. In contrast, all the JS_New//StringCopy// functions do not take
// ownership of the character memory passed to them -- they copy it.

/// JS_NewStringCopyN allocates space for a JavaScript string and its underlying storage,
// and copies n characters from a C character array, s, into the new JSString.
// - JS_NewUCStringCopyN is the Unicode version of the function.
// The two functions differ only in the type of the character array s;
// both functions create ordinary JavaScript strings, and all
// JavaScript strings are made up of 16-bit characters.
//  - If the array s contains more than n characters, the new string contains
// a truncated version of the original string. The string may contain null
// characters (#0). They are copied into the new string like any other character.
//  - You can use JS_NewStringCopyN to copy binary data or to copy only
// a certain portion of a C string into a JavaScript string.
// - On success, JS_NewStringCopyN and JS_NewUCStringCopyN return a pointer
// to the new JS string.
// - Otherwise they return NULL.
function JS_NewStringCopyN(cx: PJSContext; s: PCChar; n: size_t): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Create a new JavaScript string based on a null-terminated C string
function JS_NewStringCopyZ(cx: PJSContext; s: PCChar): PJSString;
  cdecl; external SpiderMonkeyLib;

/// get an interned string from a given text buffer
// — that is, a JSString that is protected from GC and automatically shared
// with other code that needs a JSString with the same value
// - JS_InternUCString and JS_InternUCStringN are the Unicode versions of the function.
// - Each JSRuntime keeps a table of all existing interned strings.
// If an interned string already exists with the desired value, these functions
// return the existing string. Otherwise a new string is created and added to the table.
// - Strings created with these functions are protected from garbage collection
// for the lifetime of the JSRuntime
// - On success, these functions return a pointer to the interned string.
// - Otherwise they report an error and return NULL.
function JS_InternString(cx: PJSContext; s: PCChar): PJSString;
  cdecl; external SpiderMonkeyLib;

/// get an interned string from a given null-terminated C string
function JS_InternJSString(cx: PJSContext; str: PJSString): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Creates and returns a new string, using the memory starting at buf and
// ending at buf + length as the character storage.
// - The character array, buf, MUST be allocated on the heap using JS_malloc.
// - On success, the JavaScript engine adopts responsibility for memory
// management of this region.
// - The application must not read, write, or free the buffer.
// - This allows the JavaScript engine to avoid needless data copying.
// - On success, JS_NewUCString return a pointer to the new string.
// -On error or exception, they return NUL
// - !!!!! DO NOT USE THIS FUNCTION WITH DELPHI strings!!!!!!!!!
function JS_NewUCString(cx: PJSContext; s: pjschar; len: size_t): PJSString;
  cdecl; external SpiderMonkeyLib;

/// unicode version of JS_NewStringCopyN
// - faster then JS_NewStringCopyN because no need to transform into unicode
function JS_NewUCStringCopyN(cx: PJSContext; s: pjschar; n: size_t): PJSString;
  cdecl; external SpiderMonkeyLib;

/// unicode version of JS_NewStringCopyZ
function JS_NewUCStringCopyZ(cx: PJSContext; const s: pjschar): PJSString;
  cdecl; external SpiderMonkeyLib;

/// unicode version of JS_InternString (faster)
function JS_InternUCStringN(cx: PJSContext; const s: pjschar; len: size_t): PJSString;
  cdecl; external SpiderMonkeyLib;

function JS_InternUCString(cx: PJSContext; const s: pjschar): PJSString;
  cdecl; external SpiderMonkeyLib;

/// Compares two JS strings, str1 and str2
// - If the strings are identical in content and length, JS_CompareStrings
// stores 0 in *result.
// - If str1 is less than str2, *result is less than 0.
// ( If str1 is greater than str2, *result is greater than 0.
// - On success the function returns JS_TRUE.
// - On error, it returns JS_FALSE and the value in result is unchanged.
// - This function imposes a total order on all JavaScript strings, the same
// order imposed by the JavaScript string comparison operators (<, <=, >, >=),
// as described in ECMA 262-3 § 11.8.5.
function JS_CompareStrings(cx: PJSContext; str1, str2: PJSString;
  var res: int32): JSBool; cdecl; external SpiderMonkeyLib;

// TODO - no description provided in MSDN
// extern JS_PUBLIC_API(JSBool)
//JS_StringEqualsAscii(JSContext *cx, JSString *str, const char *asciiBytes, JSBool *match);
//
//extern JS_PUBLIC_API(size_t)
//JS_PutEscapedString(JSContext *cx, char *buffer, size_t size, JSString *str, char quote);
//
//extern JS_PUBLIC_API(JSBool)
//JS_FileEscapedString(FILE *fp, JSString *str, char quote);

/// Extracting string characters and length.
//
// - While getting the length of a string is infallible, getting the chars can
//  fail. As indicated by the lack of a JSContext parameter, there are two
//  special cases where getting the chars is infallible:
// - The first case is interned strings, i.e., strings from JS_InternString or
//  JSID_TO_STRING(id), using JS_GetInternedStringChars//.
// - The second case is "flat" strings that have been explicitly prepared in a
//  fallible context by JS_FlattenString. To catch errors, a separate opaque
//  JSFlatString type is returned by JS_FlattenString and expected by
//  JS_GetFlatStringChars. Note, though, that this is purely a syntactic
//  distinction: the input and output of JS_FlattenString are the same actual
//  GC-thing so only one needs to be rooted. If a JSString is known to be flat,
//  JS_ASSERT_STRING_IS_FLAT can be used to make a debug-checked cast. Example:
//   // in a fallible context
//   JSFlatString *fstr = JS_FlattenString(cx, str);
//   if (!fstr)
//     return JS_FALSE;
//   JS_ASSERT(fstr == JS_ASSERT_STRING_IS_FLAT(str));
//
//   // in an infallible context, for the same 'str'
//   const jschar *chars = JS_GetFlatStringChars(fstr)
//   JS_ASSERT(chars);
//
// The CharsZ APIs guarantee that the returned array has a null character at
// chars[length]. This can require additional copying so clients should prefer
// APIs without CharsZ if possible. The infallible functions also return
// null-terminated arrays. (There is no additional cost or non-Z alternative
// for the infallible functions, so 'Z' is left out of the identifier.)

/// Reports the length, in 16-bit code units, of the string str.
// - This is the same as the length property of the string.
// - This is the same as the length of the array returned by JS_GetStringChars,
// in jschars (not bytes).
// - Because some Unicode characters are represented using two 16-bit code units,
// the result is not necessarily the same as the number of Unicode characters
// in the string.
function JS_GetStringLength(str: PJSString): size_t; cdecl; external SpiderMonkeyLib;

/// Retrieve a pointer to the 16-bit values that make up a given string.
// - The array is not necessarily null-terminated. To get the length
// of the string, use JS_GetStringLength.
// - The program must not modify the array. If it does, the behavior is undefined.
// - The content of a JS string is not guaranteed to be valid UTF-16. It may
// contain surrogate code units that aren't properly paired. It may also contain zeroes.
// - The array returned by this function remains valid as long as str is valid.
// (Eventually, str becomes unreachable, the garbage collector collects it,
// and the array is freed by the system.)
function JS_GetStringCharsAndLength(cx: PJSContext; str: PJSString;
  var len: size_t): pjschar; cdecl; external SpiderMonkeyLib;

/// see JS_InternedString for details about what InternedString is.
function JS_GetInternedStringChars(str: PJSString): Pjschar;
  cdecl; external SpiderMonkeyLib;

function JS_GetInternedStringCharsAndLength(str: PJSString; var len: size_t): Pjschar;
  cdecl; external SpiderMonkeyLib;

// is the same as JS_GetStringChars except that it always returns either a
// null-terminated string or NULL, indicating out-of-memory
// - help provided at
// https://developer.mozilla.org/en-US/docs/SpiderMonkey/JSAPI_Reference/JS_GetStringChars
function JS_GetStringCharsZ(cx: PJSContext; str: PJSString): pjschar;
  cdecl; external SpiderMonkeyLib;

function JS_GetStringCharsZAndLength(cx: PJSContext; str: PJSString; var len: size_t): pjschar;
  cdecl; external SpiderMonkeyLib;

  //TODO add JS_FlattenString related function (do not use now)
  //TODO JS_Encode / DEcode functions. We do not need it - instead use Syn*


{ JSON functions }

type
  /// used by JS_Stringify() method to incremently write the JSON content
  JSONWriteCallback = function(const buf: Pjschar; len: uint32; data: pointer): JSBool; cdecl;

/// converts a value to JSON, optionally replacing values if a replacer
// function is specified, or optionally including only the specified properties
// if a replacer array is specified
function JS_Stringify(cx: PJSContext; vp: Pjsval; replacer: PJSObject;
    space: jsval; callback: JSONWriteCallback; data: pointer): JSBool;
  cdecl; external SpiderMonkeyLib;

/// parse a string using the JSON syntax described in ECMAScript 5 and
// return the corresponding value into vp
function JS_ParseJSON(cx: PJSContext; const chars: Pjschar;
    len: uint32; vp: Pjsval): JSBool;  cdecl; external SpiderMonkeyLib;

  //TODO StructuredClone functions


{ Error reporting }

 // Report an exception represented by the sprintf-like conversion of format
 // and its arguments.  This exception message string is passed to a pre-set
 // JSErrorReporter function (set by JS_SetErrorReporter; see jspubtd.h for
 // the JSErrorReporter typedef).

 /// JSErrorReport flag values.  These may be freely composed.
const
  // JSErrorReport pseudo-flag for default case
  JSREPORT_ERROR      = 0;
  // JSErrorReport reported via JS_ReportWarning
  JSREPORT_WARNING    = 1;
  // JSErrorReport exception was thrown
  JSREPORT_EXCEPTION  = 2;
  // JSErrorReport error or warning due to strict option
  JSREPORT_STRICT     = 4;

  // JSErrorReport error or warning depending on strict mode
  // - This condition is an error in strict mode code, a warning if
  // JS_HAS_STRICT_OPTION(cx), and otherwise should not be reported at
  // all.  We check the strictness of the context's top frame's script;
  // where that isn't appropriate, the caller should do the right checks
  // itself instead of using this flag.
  JSREPORT_STRICT_MODE_ERROR = 8;

/// JS_ReportError is the simplest JSAPI function for reporting errors
// - First it builds an error message from the given sprintf-style format string
// and any additional arguments passed after it.  The resulting error message
// is passed to the context's JSErrorReporter callback, if any
// - If the caller is in a JSAPI callback, JS_ReportError also creates a new
// JavaScript Error object and sets it to be the pending exception on cx
// - The callback must then return JS_FALSE to cause the exception to be
// propagated to the calling script
// - An example is shown in the JSAPI Phrasebook
// - For internationalization, use JS_ReportErrorNumber instead
// - To report an out-of-memory error, use JS_ReportOutOfMemory
procedure JS_ReportError(cx: PJSContext; const format: PCChar);
  cdecl; varargs; external SpiderMonkeyLib;

/// Report an error with an application-defined error code.
// - varargs is Additional arguments for the error message.
//- These arguments must be of type char*
// - The number of additional arguments required depends on the error
// message, which is determined by the errorCallback
procedure JS_ReportErrorNumber(cx: PJSContext; errorCallback: JSErrorCallback;
  userRef: pointer; const erroNubmer: uintN); cdecl; varargs; external SpiderMonkeyLib;

/// Report an error with an application-defined error code.
// - varargs is Additional arguments for the error message.
//- These arguments must be of type jschar*
// - The number of additional arguments required depends on the error
// message, which is determined by the errorCallback
procedure JS_ReportErrorNumberUC(cx: PJSContext; errorCallback: JSErrorCallback;
  userRef: pointer; const erroNubmer: uintN); cdecl; varargs; external SpiderMonkeyLib;

/// similar to JS_ReportError(), but report a warning instead of an error
// (JSREPORT_IS_WARNING(report.flags))
//  - Return true if there was no error trying to issue the warning, and if the
// warning was not converted into an error due to the JSOPTION_WERROR option
// being set, false otherwise
procedure JS_ReportWarning(cx: PJSContext; const format: PCChar);
  cdecl; varargs; external SpiderMonkeyLib;

function JS_ReportErrorFlagsAndNumber(cx: PJSContext; flags: uintN;
    errorCallback: JSErrorCallback; userRef: Pointer; const errorNumber: uintN): JSBool;
  varargs; cdecl; external SpiderMonkeyLib;
function JS_ReportErrorFlagsAndNumberUC(cx: PJSContext; flags: uintN;
    errorCallback: JSErrorCallback; userRef: Pointer; const errorNumber: uintN): JSBool;
  varargs; cdecl; external SpiderMonkeyLib;

/// Reports a memory allocation error
// - Call JS_ReportOutOfMemory to report that an operation failed because the
// system is out of memory
// - When the JavaScript engine tries to allocate memory and allocation fails,
// it reports an error as though by calling this function
procedure JS_ReportOutOfMemory(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Call JS_ReportAllocationOverflow if an operation fails because it tries to
// use more memory (or more of some other resource) than the application is
// designed to handle
// - When a script tries to grow an array beyond 230-1 elements, for example,
// or concatenate strings such that the result is more than 229-1 characters long,
// the JavaScript engine reports an error as though by calling this function.
// - The main difference between these two functions is that JS_ReportOutOfMemory
// does not cause a JavaScript exception to be thrown.
// The error therefore cannot be caught by try…catch statements in scripts.
// JS_ReportAllocationOverflow throws an InternalError which scripts can catch.
procedure JS_ReportAllocationOverflow(cx: PJSContext); cdecl; external SpiderMonkeyLib;

/// Specify the error reporting mechanism for an application.
// - JS_SetErrorReporter enables you to define and use your own error
// reporting mechanism in your applications.
// - The reporter you define is automatically passed a JSErrorReport structure
// when an error occurs and has been parsed by JS_ReportError.
//- JS_SetErrorReporter returns the previous error reporting function of the
// context, or NULL if no such function had been set.
// - Typically, the error reporting mechanism you define should log the error
// where appropriate (such as to a log file), and display an error to the user
// of your application.
// - The error you log and display can make use of the information passed about
// the error condition in the JSErrorReport structure.
// - The error reporter callback must not reenter the JSAPI.
// - Like all other SpiderMonkey callbacks, the error reporter callback must not
// throw any Delphi exception.
function JS_SetErrorReporter(cx: PJSContext; er: JSErrorReporter): JSErrorReporter;
  cdecl; external SpiderMonkeyLib;


{ Dates }

/// create a new JavaScript date object
function JS_NewDateObject(cx: PJSContext; year, mon, mday, hour, min, sec: int32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// create a new JavaScript date object from the Unix millisecond elapsed since EPOC
function JS_NewDateObjectMsec(cx: PJSContext; msec: jsdouble): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Infallible predicate to test whether obj is a JavaScript date object
function JS_ObjectIsDate(cx: PJSContext; obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;


{ Error hadling }

/// determine whether an exception is pending in the JS engine.
// - JS_IsExceptionPending returns JS_TRUE if an exception has been thrown
// in the context cx and the exception has not yet been caught or cleared.
// Otherwise, it returns JS_FALSE
// - This can be used from JSNative functions which call JS code to determine
// if the called JS code threw an exception or not.
function JS_IsExceptionPending(cx: PJSContext): JSBool;
  cdecl; external SpiderMonkeyLib;

/// get the current exception being thrown within a context
function JS_GetPendingException(cx: PJSContext; var vp: jsval): JSBool;
  cdecl; external SpiderMonkeyLib;

/// set the current exception being thrown within a context
// - JS_SetPendingException sets the current exception being thrown within a
// context. If an exception is already being thrown, it is replaced with the
// new one given.
// - v is the new value to throw as an exception.
// - A native function or hook using this to throw an exception must also
// return JS_FALSE to ensure the exception is thrown.
// - Each JSContext's pending-exception field is a GC root. That is,
// garbage collection never collects a pending exception
procedure JS_SetPendingException(cx: PJSContext; v: jsval);
  cdecl; external SpiderMonkeyLib;

procedure JS_ClearPendingException(cx: PJSContext);
  cdecl; external SpiderMonkeyLib;
function JS_ReportPendingException(cx: PJSContext): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  PJSExceptionState = type Pointer;

/// Save the current exception state.  This takes a snapshot of cx's current
// exception state without making any change to that state.
// - The returned state pointer MUST be passed later to JS_RestoreExceptionState
// (to restore that saved state, overriding any more recent state) or else to
// JS_DropExceptionState (to free the state struct in case it is not correct
// or desirable to restore it).  Both Restore and Drop free the state struct,
// so callers must stop using the pointer returned from Save after calling the
// Release or Drop API.
function JS_SaveExceptionState(cx: PJSContext): PJSExceptionState;
  cdecl; external SpiderMonkeyLib ;

/// Restore a specified exception state
procedure JS_RestoreExceptionState(cx: PJSContext; state: PJSExceptionState);
  cdecl; external SpiderMonkeyLib ;

/// Drop a specified exception state
procedure JS_DropExceptionState(cx: PJSContext; state: PJSExceptionState);
  cdecl; external SpiderMonkeyLib ;

/// retrieve an error report from an exception object
// - If the given value is an exception object that originated from an error,
// the exception will contain an error report struct, and this API will return
// the address of that struct.  Otherwise, it returns NULL.
// - The lifetime of the error report struct that might be returned is the same
// as the lifetime of the exception object
function JS_ErrorFromException(cx: PJSContext; v: jsval): PJSErrorReport;
  cdecl; external SpiderMonkeyLib;

/// Given a reported error's message and JSErrorReport struct pointer, throw
// the corresponding exception on cx
function JS_ThrowReportedError(cx: PJSContext; const msg: PCChar;
  reportp: PJSErrorReport): JSBool; cdecl; external SpiderMonkeyLib;

/// Throws a StopIteration exception on cx
function JS_ThrowStopIteration(cx: PJSContext): JSBool;
  cdecl; external SpiderMonkeyLib;

/// JS_IsConstructing must be called from within a native given the
// native's original cx and vp arguments
// - If JS_IsConstructing is true, JS_THIS must not be used;
// the constructor should construct and return a new object
// - Otherwise, the native is called as an ordinary function and
// JS_THIS may be used
function JS_IsConstructing(cx: PJSContext; const vp: PjsvalVector): boolean;
  {$ifdef HASINLINE}inline;{$endif}

type
  /// low-level definition of the jsval internals
  // - do not use directly
  jsval_payload = record
    case Byte of
      0: (i32: int32);
      1: (u32: uint32);
      2: (boo: JSBool);
      3: (str: PJSString);
      4: (obj: PJSObject);
      5: (ptr: pointer);
      6: (why: JSWhyMagic);
      7: (word: jsuword);
      8: (uintptr: JSuintptr)
  end;
{$ifdef IS_LITTLE_ENDIAN}
  jsval_val_layout = packed record
    payload: jsval_payload;
    tag: JSValueTag;
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
      1: (s: jsval_val_layout);
      2: (asDouble: double);
      3: (asPtr: Pointer);
  end;

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
function JS_NewInt8ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint8ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint8ClampedArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit signed integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewInt16ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint16ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit signed integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewInt32ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit unsigned integer typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewUint32ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit float (single) typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewFloat32ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 64 bit float (double) typed array and copy in values
// from a given object
// - The object is used as if it was an array; that is, the new array (if
// successfully created) will have length given by array.length, and its
// elements will be those specified by array[0], array[1], and so on, after
// conversion to the typed array element type.
function JS_NewFloat64ArrayFromArray(cx: PJSContext; arr: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt8ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint8ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 8 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint8ClampedArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt16ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 16 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint16ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit signed integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewInt32ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit unsigned integer typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewUint32ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 32 bit float (single) typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewFloat32ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new 64 bit float (double) typed array using the given
// ArrayBuffer for storage
// - The length value is optional; if -1 is passed, enough elements to use up the
// remainder of the byte array is used as the default value
function JS_NewFloat64ArrayWithBuffer(cx: PJSContext; arrayBuffer: PJSObject;
 byteOffset: uint32; length: int32): PJSObject; cdecl; external SpiderMonkeyLib;

/// Create a new ArrayBuffer with the given byte length.
function JS_NewArrayBuffer(cx: PJSContext; nbytes: uint32): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Check whether obj supports JS_GetTypedArray* APIs
// - Note that this may return false if a security wrapper is encountered that
// denies the unwrapping.
// - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
// the dedicated accessor JSAPI calls
function JS_IsTypedArrayObject(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Check whether obj supports JS_GetArrayBufferView* APIs
// - Note that this may return false if a security wrapper is encountered that
// denies the unwrapping.
// - if this test or one of the JS_Is*Array tests succeeds, then it is safe to call
// the dedicated ArrayBufferView accessor JSAPI calls
function JS_IsArrayBufferViewObject(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt8Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint8Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 8 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint8ClampedArray(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 16 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt16Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 16 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint16Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit signed integer typed array types (ArrayBufferView subtypes)
function JS_IsInt32Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit unsigned integer typed array types (ArrayBufferView subtypes)
function JS_IsUint32Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 32 bit float (single) typed array types (ArrayBufferView subtypes)
function JS_IsFloat32Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;

/// Test for specific 64 bit float (double) typed array types (ArrayBufferView subtypes)
function JS_IsFloat64Array(obj: PJSObject): JSBool; cdecl; external SpiderMonkeyLib;


type
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

/// Unwrap 8 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt8Array(obj: PJSObject; var length: uint32; var Data: Pint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 8 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint8Array(obj: PJSObject; var length: uint32; var Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 8 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint8ClampedArray(obj: PJSObject; var length: uint32; var Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 16 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt16Array(obj: PJSObject; var length: uint32; var Data: Pint16Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 16 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint16Array(obj: PJSObject; var length: uint32; var Data: Puint16Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit signed integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsInt32Array(obj: PJSObject; var length: uint32; var Data: Pint32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit unsigned integer typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsUint32Array(obj: PJSObject; var length: uint32; var Data: Puint32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 32 bit float (single) typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsFloat32Array(obj: PJSObject; var length: uint32; var Data: Pfloat32Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap 64 bit float (double) typed array into direct memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsFloat64Array(obj: PJSObject; var length: uint32; var Data: Pfloat64Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap an object as its raw binary memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsArrayBufferView(obj: PJSObject; var length: uint32; var Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

/// Unwrap an object as its raw binary memory buffer
// - Return nil without throwing any exception if the object cannot be viewed as the
// correct typed array, or the typed array object on success, filling both out parameters
function JS_GetObjectAsArrayBuffer(obj: PJSObject; var length: uint32; var Data: Puint8Vector): PJSObject;
  cdecl; external SpiderMonkeyLib;

type
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
    jsabTYPE_DATAVIEW,
    jsabTYPE_MAX
);

  /// Get the type of elements in a typed array, or jsabTYPE_DATAVIEW if a DataView
function JS_GetArrayBufferViewType(obj: PJSObject): JSArrayBufferViewType;
  cdecl; external SpiderMonkeyLib;

/// Check whether obj supports the JS_GetArrayBuffer* APIs
// - Note that this may return false if a security wrapper is encountered that denies the
// unwrapping
// - If this test succeeds, then it is safe to call the various accessor JSAPI calls
function JS_IsArrayBufferObject(obj: PJSObject): JSBool;
  cdecl; external SpiderMonkeyLib;

/// Return the available byte length of an array buffer
// - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
// that it would pass such a test: it is an ArrayBuffer or a wrapper of an
// ArrayBuffer, and the unwrapping will succeed
function JS_GetArrayBufferByteLength(obj: PJSObject): uint32;
  cdecl; external SpiderMonkeyLib;

/// Return a pointer to an array buffer's data
// - The buffer is still owned by the array buffer object, and should not
// be modified on another thread. The returned pointer is stable across GCs
// - obj must have passed a JS_IsArrayBufferObject test, or somehow be known
// that it would pass such a test: it is an ArrayBuffer or a wrapper of an
// ArrayBuffer, and the unwrapping will succeed.
function JS_GetArrayBufferData(obj: PJSObject): Puint8Vector; cdecl; external SpiderMonkeyLib;

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
function JS_GetInt8ArrayData(obj: PJSObject): Pint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint8ArrayData(obj: PJSObject): Puint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 8 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint8ClampedArrayData(obj: PJSObject): Puint8Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 16 bit signed integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetInt16ArrayData(obj: PJSObject): Pint16Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 16 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint16ArrayData(obj: PJSObject): Puint16Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit signed integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetInt32ArrayData(obj: PJSObject): Pint32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit unsigned integer array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetUint32ArrayData(obj: PJSObject): Puint32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 32 bit float (single) array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetFloat32ArrayData(obj: PJSObject): Pfloat32Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by a typed 64 bit float (double) array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
function JS_GetFloat64ArrayData(obj: PJSObject): Pfloat64Vector; cdecl; external SpiderMonkeyLib;

/// Return a pointer to the start of the data referenced by any typed array
// - The data is still owned by the typed array, and should not be modified on
// another thread
// - obj must have passed a JS_Is*Array test, or somehow be known that it would
// pass such a test: it is a typed array or a wrapper of a typed array, and the
// unwrapping will succeed
// - Prefer the type-specific versions when possible
function JS_GetArrayBufferViewData(obj: PJSObject): Pointer; cdecl; external SpiderMonkeyLib;

/// Return the ArrayBuffer underlying an ArrayBufferView
// - If the buffer has been neutered, this will still return the neutered buffer.
// - obj must be an object that would return true for JS_IsArrayBufferViewObject()
function JS_GetArrayBufferViewBuffer(obj: PJSObject): PJSObject; cdecl; external SpiderMonkeyLib;


{ NSPR library APIs }

const
  /// numbers of micro secs per second
  PRMJ_USEC_PER_SEC = 1000000;

  /// stipulate that the process should wait no time as defined by NSPR
  // - i.e. will return immediately
  // - defined in the PRIntervalTime namespace
  PR_INTERVAL_NO_WAIT    =$0;
  /// stipulate that the process should wait forever as defined by NSPR
  // - i.e. will never time out
  // - defined in the PRIntervalTime namespace
  PR_INTERVAL_NO_TIMEOUT =$ffffffff;

type
  /// unsigned 32 bit integer type as defined by NSPR
  PRUint32 = uint32;
  /// interval time type as defined by NSPR
  PRIntervalTime = PRUint32;

  /// a mutex/lock resource as defined by NSPR
  PRLock = Pointer;
  /// a event resource as defined by NSPR
  PRCondVar = Pointer;
  /// a thread resource as defined by NSPR
  PRThread = Pointer;

  /// status codes as defined by NSPR
  PRStatus = (PR_FAILURE = -1, PR_SUCCESS = 0);

  /// thread type as defined by NSPR
  PRThreadType = (PR_USER_THREAD, PR_SYSTEM_THREAD);

  /// thread priority as defined by NSPR
  // - PR_PRIORITY_LOW is the lowest possible priority
  // - PR_PRIORITY_NORMAL is the most common expected priority
  // - PR_PRIORITY_HIGH is the slightly more aggressive scheduling
  // - PR_PRIORITY_URGENT is there because it does little good to have one
  // more priority value
    PRThreadPriority = (
    PR_PRIORITY_FIRST = 0,
    PR_PRIORITY_LOW = 0,
    PR_PRIORITY_NORMAL = 1,
    PR_PRIORITY_HIGH = 2,
    PR_PRIORITY_URGENT = 3,
    PR_PRIORITY_LAST = 3);

  /// thread scope as defined by NSPR
  PRThreadScope = (PR_LOCAL_THREAD, PR_GLOBAL_THREAD, PR_GLOBAL_BOUND_THREAD);

  /// thread state as defined by NSPR
  PRThreadState = (PR_JOINABLE_THREAD, PR_UNJOINABLE_THREAD);

/// allocates a new NSPR mutex/lock
function PR_NewLock: PRLock; cdecl; external NSPRLib;

/// allocates a new NSPR event
function PR_NewCondVar(lock: PRLock): PRCondVar; cdecl; external NSPRLib;

/// free a previously allocated NSPR event
procedure PR_DestroyCondVar(cvar: PRCondVar); cdecl; external NSPRLib;

/// notify a previously allocated NSPR event
function PR_NotifyCondVar(cvar: PRCondVar): PRStatus; cdecl; external NSPRLib;

/// notify all previously allocated NSPR event
function PR_NotifyAllCondVar(cvar: PRCondVar): PRStatus; cdecl; external NSPRLib;

/// wait until a previously allocated NSPR event is notified
function PR_WaitCondVar(cvar: PRCondVar; timeout: PRIntervalTime): PRStatus;
  cdecl; external NSPRLib;

/// enter a previously allocated NSPR mutex/lock
procedure PR_Lock(lock: PRLock); cdecl; external NSPRLib;

/// leave a previously allocated NSPR mutex/lock
function PR_Unlock(lock: PRLock): PRStatus; cdecl; external NSPRLib;

/// free a previously allocated NSPR lock
procedure PR_DestroyLock(lock: PRLock); cdecl; external NSPRLib;

/// join a NSPR thread
function PR_JoinThread(thred: PRThread): PRStatus; cdecl; external NSPRLib;

/// initializes a NSPR thread
function PR_CreateThread(
  type_: PRThreadType; start: pointer; arg: pointer;
  priority: PRThreadPriority; scope: PRThreadScope;
  state: PRThreadState; stackSize: PRUint32): PRThread; cdecl; external NSPRLib;

/// change the current NSPR thread name
function PR_SetCurrentThreadName(name: PAnsiChar): PRStatus;
  cdecl; external NSPRLib;

/// returns the number of ticks per seconds as expected by NSPR
function PR_TicksPerSecond(): PRUint32; cdecl; external NSPRLib;


{ Debugger }

/// enables the JavaScript debugging mode for a given runtime
procedure JS_SetRuntimeDebugMode(rt: PJSRuntime; debug: JSBool);
  cdecl; external SpiderMonkeyLib;

/// enables the JavaScript debugging mode for a given context
function JS_SetDebugMode(cx: PJSContext; debug: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

/// check if the JavaScript debugging mode is set for a given context
function JS_GetDebugMode(cx: PJSContext): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  /// callback type to be called when debugging a JavaScript context
  JSNewScriptHook = procedure(cx: PJSContext; filename: PCChar; lineno: uintn;
    script:PJSScript; fun: PJSFunction; callerdata: pointer); cdecl;

/// set a JavaScript debugging hook callback
procedure JS_SetNewScriptHookProc(rt: PJSRuntime; hook: JSNewScriptHook;
  callerdata: Pointer); cdecl; external SpiderMonkeyLib;

type
  /// calback type to be called when an operation is freed
  PJSFreeOp = Pointer;

  /// calback type to be called when a JavaScript debugging hook is released
  JSDestroyScriptHook = procedure(fop: PJSFreeOp; script: PJSScript;
    callerdata: Pointer); cdecl;

/// set the callback to be called when a JavaScript debugging hook is released
procedure JS_SetDestroyScriptHookProc(rt: PJSRuntime; hook: JSDestroyScriptHook;
  callerdata: Pointer); cdecl; external SpiderMonkeyLib;

type
  /// used to store a JavaScript bytecode item
  jsbytecode = uint8;
  /// used to store a pointer to JavaScript bytecode items
  pjsbytecode = ^jsbytecode;
  /// JavaScript debugging trap status
  JSTrapStatus = (
    JSTRAP_ERROR,
    JSTRAP_CONTINUE,
    JSTRAP_RETURN,
    JSTRAP_THROW,
    JSTRAP_LIMIT);

  /// calback type to be called when a script is debugging
  JSDebuggerHandler = function(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
    rval: pjsval; closure: pointer): JSTrapStatus; cdecl;

/// set a debugging handler callback for a given hook and runtime
function JS_SetDebuggerHandler(rt: PJSRuntime; hook: JSDebuggerHandler;
  closure: pointer): JSBool; cdecl; external SpiderMonkeyLib;

type
  /// calback type to be called when a script is debugging and trapped
  JSTrapHandler = function(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
    rval: pjsval; closure: jsval): JSTrapStatus; cdecl;

/// set a trap debugging handler callback for a given execution context
function JS_SetTrap(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
  handler: JSTrapHandler; closure: jsval): JSBool; cdecl; external SpiderMonkeyLib;

/// clear a trap debugging handler callback for a given execution context
procedure JS_ClearTrap(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
  handler: JSTrapHandler; closure: jsval); cdecl; external SpiderMonkeyLib;

type
  /// calback type to be called when an error is triggerred during script debugging
  JSDebugErrorHook = function(cx: PJSContext; message: PCChar;
    report: PJSErrorReport; closure: pointer): JSBool; cdecl;

/// set a calback to be called when an error is triggerred during script debugging
function JS_SetDebugErrorHook(rt: PJSRuntime; hook: JSDebugErrorHook;
  closure: pointer): JSBool; cdecl; external SpiderMonkeyLib;

type
  /// calback type to be called when script debugging is interrupted
  JSInterruptHook = function(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
    rval: pjsval; closure: pointer): JSTrapStatus; cdecl;
  PJSInterruptHook = ^JSInterruptHook;

/// set a calback to be called when script debugging is interrupted
function JS_SetInterrupt(rt: PJSRuntime; handler: JSInterruptHook;
  closure: pointer): JSBool; cdecl; external SpiderMonkeyLib;

/// clear a calback to be called when script debugging is interrupted
function JS_ClearInterrupt(rt: PJSRuntime; hook: PJSInterruptHook;
  closurep: PPointer): JSBool; cdecl; external SpiderMonkeyLib;

/// set single step mode. In this mode script interrupts on each line
function JS_SetSingleStepMode(cx: PJSContext; script: PJSScript; singleStep: JSBool): JSBool;
  cdecl; external SpiderMonkeyLib;

type
  /// calback type to be called when script throws an exception
  JSThrowHook = function(cx: PJSContext; script: PJSScript; pc: pjsbytecode;
    rval: pjsval; closure: pointer): JSTrapStatus; cdecl;

/// set a calback to be called when script throws an exception
function JS_SetThrowHook(rt: PJSRuntime; hook: JSThrowHook;
  closure: pointer): JSBool; cdecl; external SpiderMonkeyLib;

/// decompile a JavaScript script
function JS_DecompileScript(cx: PJSContext; script: PJSScript;
  name: PCChar; indent: uintn ): PJSString; cdecl; external SpiderMonkeyLib;

type
  /// points to a frame description of a stack trace
  PFrameDescription = ^FrameDescription;
  /// defines a frame of a stack trace
  FrameDescription = record
   script: PJSScript;
   lineno: uintn;
   fun: PJSFunction;
  end;

  /// points to a stack trace
  PStackDescription = ^StackDescription;
  /// defines a stack trace
  StackDescription = record
   nframes: uintn;
   frames: PFrameDescription;
  end;

/// retrieve the stack trace of a given execution context
function DescribeStack(cx: PJSContext; maxFrames: uintn): PStackDescription;
  cdecl; external SpiderMonkeyLib;

/// release the stack trace description of a given execution context
procedure FreeStackDescription(cx: PJSContext; desc: PStackDescription);
  cdecl; external SpiderMonkeyLib;

type
  /// points to an extended frame description of a stack trace
  PFrameDescriptionEx = ^FrameDescriptionEx;
  /// defines an extended frame of a stack trace
  FrameDescriptionEx = record
   script: PJSScript;
   lineno: uintn;
   fun: PJSFunction;
   callObject: PJSObject;
   thisVal: jsval;
   raw: JSuintptr;
  end;

  /// points to an extended stack trace
  PStackDescriptionEx = ^StackDescriptionEx;
  /// defines to an extended stack trace
  StackDescriptionEx = record
   nframes: uintn;
   frames: PFrameDescriptionEX;
  end;

/// retrieve the extended stack trace of a given execution context
function DescribeStackEx(cx: PJSContext; maxFrames: uintn): PStackDescriptionEx;
  cdecl; external SpiderMonkeyLib;

/// release the extended stack trace description of a given execution context
procedure FreeStackDescriptionEx(cx: PJSContext; desc: PStackDescriptionEx);
  cdecl; external SpiderMonkeyLib;

/// dump stack trace info with the specified format
function FormatStackDump(cx: PJSContext; buf: PCChar;
  showArgs, showLocals, showThisProps: JSBool): PCChar;
   cdecl; external SpiderMonkeyLib;

/// retrieve the script line number from a byte code item position
function JS_PCToLineNumber(cx: PJSContext; script: PJSScript;
  pc: pjsbytecode): uintn; cdecl; external SpiderMonkeyLib;

/// retrieve the byte code item position corresponding to a script line number
function JS_LineNumberToPC(cx: PJSContext; script: PJSScript;
  lineno: uintn): pjsbytecode; cdecl; external SpiderMonkeyLib;

type
  /// define object property behavior
  // - JSPD_ENUMERATE means that the property is visible to for/in loop
  // - JSPD_READONLY means that the property assignment will trigger an error
  // - JSPD_PERMANENT means that the property cannot be deleted
  // - JSPD_ALIAS means that the property has an alias id
  // - JSPD_EXCEPTION means that an exception occurred fetching the property:
  // in this case, value is an exception
  // - JSPD_ERROR means that the native getter returned JS_FALSE without
  // throwing an exception
  JSPropertyDescFlag = (
    dfEnumerate, dfReadOnly, dfPermanent, dfAlias, dfException, dfError);
  /// define object property behaviors
  JSPropertyDescFlags = set of JSPropertyDescFlag;

  /// points to a JavaScript object property description
//  PJSPropertyDesc = ^JSPropertyDesc;
  /// defines a JavaScript object property description
  JSPropertyDesc = record
    // the ID of this property
    id: jsval;
    /// the JavaScript value of this property
    value: jsval;
    /// the property behavior
    flags: JSPropertyDescFlags;
    /// this item is never used
    spare_notused: uint8;
    /// contains the alias ID if dfAlias is included in description flags
    alias: jsval;
  end;
  TJSPropertyDescVector = array[0..(MaxInt div sizeof(JSPropertyDesc))-1] of JSPropertyDesc;
  PJSPropertyDescVector = ^TJSPropertyDescVector;

  /// stores JavaScript object properties description
  JSPropertyDescArray = record
   len: uint32;
   arr: PJSPropertyDescVector;
  end;

/// retrieve the description of a given JavaScript object property
function JS_GetPropertyDescArray(cx: PJSContext; obj: PJSObject;
  var pda: JSPropertyDescArray): JSBool; cdecl; external SpiderMonkeyLib;

/// define the description of a given JavaScript object property
procedure JS_PutPropertyDescArray(cx: PJSContext; var pda: JSPropertyDescArray);
  cdecl; external SpiderMonkeyLib;


/// cast a JavaScript function into a script instance
function JS_GetFunctionScript(cx: PJSContext; fun: PJSFunction): PJSScript;
  cdecl; external SpiderMonkeyLib;

function JS_GetScriptFunction(cx: PJSContext; script: PJSScript): PJSFunction;
  cdecl; external SpiderMonkeyLib;

function JS_GetParentOrScopeChain(cx: PJSContext; obj: PJSObject): PJSObject;
  cdecl; external SpiderMonkeyLib;

type
  PJSScriptSource = Pointer;

function JS_GetScriptSource(cx: PJSContext; script: PJSScript): PJSScriptSource;
  cdecl; external SpiderMonkeyLib;

/// retrieve the local name array information of a given function
function JS_GetFunctionLocalNameArray(cx: PJSContext; fun: PJSFunction;
  var markp: pointer): PJSuintptr; cdecl; external SpiderMonkeyLib;
/// release the local name array information of a given function
procedure JS_ReleaseFunctionLocalNameArray(cx: PJSContext; markp: pointer); cdecl;
  external SpiderMonkeyLib;

/// retrieves how many arguments expect a function
function JS_GetFunctionArgumentCount(cx: PJSContext; fun: PJSFunction): uint;
  cdecl; external SpiderMonkeyLib;

type
  /// stores a JavaScript Atom
  PJSAtom = Pointer;

/// convert a local name into a JavaScript atom
function JS_LocalNameToAtom(w: JSuintptr): PJSAtom; cdecl; external SpiderMonkeyLib;

/// retrieve the local name into a JavaScript atom
function JS_AtomKey(atom: PJSAtom): PJSString; cdecl; external SpiderMonkeyLib;

/// retrieve the base line number of a given script
function JS_GetScriptBaseLineNumber(cx: PJSContext; script: PJSScript): uint;
  cdecl; external SpiderMonkeyLib;

/// retrieve the extent of a given script
function JS_GetScriptLineExtent(cx: PJSContext; script: PJSScript): uint;
  cdecl; external SpiderMonkeyLib;

/// compile and execute a script in stack frame with raw identifier (obj)
function JS_evaluateUCInStackFrame(cx: PJSContext; raw: JSuintptr;
 chars: Pjschar; length: size_t; filename: PCChar; lineno: uintN; var rval: jsval): boolean;
  cdecl; external SpiderMonkeyLib;


implementation

// jsval.h *_IMPL functions

{$ifdef CPU32}

function JSVAL_IS_DOUBLE_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag <= JSVAL_TAG_CLEAR);
end;

function DOUBLE_TO_JSVAL_IMPL(const d: double): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  Result.asDouble := d;
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_DOUBLE_IMPL(Result));
  {$endif}
end;

function JSVAL_IS_INT32_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_INT32);
end;

function JSVAL_TO_INT32_IMPL(const l: jsval_layout):int32;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.payload.i32);
end;

function INT32_TO_JSVAL_IMPL(i32: int32):jsval_layout;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result.asBits := 0;
  Result.s.tag := JSVAL_TAG_INT32;
  Result.s.payload.i32 := i32;
end;

function JSVAL_IS_NUMBER_IMPL(const l: jsval_layout): Boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  assert(l.s.tag <> JSVAL_TAG_CLEAR);
  {$endif}
  Result := (l.s.tag <= JSVAL_UPPER_INCL_TAG_OF_NUMBER_SET);
end;

function JSVAL_IS_UNDEFINED_IMPL(const l: jsval_layout):Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_UNDEFINED);
end;

function JSVAL_IS_STRING_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_STRING);
end;

function STRING_TO_JSVAL_IMPL(str: PJSString): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  assert(str<>nil) ;
  {$endif}
  Result.asBits := 0;
  Result.s.tag := JSVAL_TAG_STRING;
  Result.s.payload.str := str;
end;

function JSVAL_TO_STRING_IMPL(const l: jsval_layout): PJSString;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := l.s.payload.str;
end;

function JSVAL_IS_BOOLEAN_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_BOOLEAN);
end;

function JSVAL_TO_BOOLEAN_IMPL(const l: jsval_layout): JSBool;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := l.s.payload.boo;
end;

function BOOLEAN_TO_JSVAL_IMPL(b: JSBool): jsval_layout;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result.asBits := 0;
  Result.s.tag := JSVAL_TAG_BOOLEAN;
  if b = JS_FALSE then
    Result.s.payload.boo := JS_FALSE else
    Result.s.payload.boo := JS_TRUE;
end;

function JSVAL_IS_MAGIC_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_MAGIC);
end;

function MAGIC_JSVAL_TO_OBJECT_OR_NULL_IMPL(const l: jsval_layout): PJSObject;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  Assert(JSVAL_IS_MAGIC_IMPL(l));
  {$endif}
  Result := l.s.payload.obj;
end;

function JSVAL_IS_OBJECT_IMPL(const l: jsval_layout):Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_OBJECT);
end;

function JSVAL_IS_PRIMITIVE_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag < JSVAL_UPPER_EXCL_TAG_OF_PRIMITIVE_SET);
end;

function JSVAL_IS_OBJECT_OR_NULL_IMPL(const l: jsval_layout): Boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  Assert(l.s.tag <= JSVAL_TAG_OBJECT);
  {$endif}
  Result := (l.s.tag >= JSVAL_LOWER_INCL_TAG_OF_OBJ_OR_NULL_SET);
end;

function JSVAL_TO_OBJECT_IMPL(const l: jsval_layout):PJSObject;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := l.s.payload.obj;
end;

function OBJECT_TO_JSVAL_IMPL(obj: PJSObject): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  Assert(obj <> nil);
  {$endif}
  Result.asBits := 0;
  Result.s.tag := JSVAL_TAG_OBJECT;
  Result.s.payload.obj := obj;
end;

function JSVAL_IS_NULL_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag = JSVAL_TAG_NULL);
end;

function PRIVATE_PTR_TO_JSVAL_IMPL(ptr: pointer): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  assert((uint32(ptr) and 1) = 0);
  {$endif}
  Result.s.tag := 0;
  Result.s.payload.ptr := ptr;
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_DOUBLE_IMPL(Result));
  {$endif}
end;

function JSVAL_TO_PRIVATE_PTR_IMPL(const l: jsval_layout): pointer;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := l.s.payload.ptr;
end;

function JSVAL_IS_GCTHING_IMPL(const l: jsval_layout): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.s.tag >= JSVAL_LOWER_INCL_TAG_OF_GCTHING_SET);
end;

function JSVAL_TO_GCTHING_IMPL(const l: jsval_layout): pointer;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := l.s.payload.ptr;
end;

function JSVAL_IS_TRACEABLE_IMPL(const l: jsval_layout): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ( ( l.s.tag = JSVAL_TAG_STRING ) or (l.s.tag = JSVAL_TAG_OBJECT) );
end;

function JSVAL_TRACE_KIND_IMPL(const l: jsval_layout): uint32;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result :=  JSBool(JSVAL_IS_STRING_IMPL(l));
end;
{$endif} //CPU32

{$ifdef CPU64}

function JSVAL_IS_DOUBLE_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.asBits <= JSVAL_SHIFTED_TAG_MAX_DOUBLE);
end;

function DOUBLE_TO_JSVAL_IMPL(d: double): jsval_layout;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result.asDouble := d;
  {$ifdef WITHASSERT}
  assert( JSVAL_IS_DOUBLE_IMPL(Result) );
  {$endif}
end;

function JSVAL_IS_INT32_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ( uint32(l.asBits shr JSVAL_TAG_SHIFT) = JSVAL_TAG_INT32 );
end;

function JSVAL_TO_INT32_IMPL(const l: jsval_layout): int32;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := int32(l.asBits);
end;

function INT32_TO_JSVAL_IMPL(i32: int32):jsval_layout;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result.asBits := QWord(QWord(uint32(i32)) OR JSVAL_SHIFTED_TAG_INT32);
end;

function JSVAL_IS_NUMBER_IMPL(const l: jsval_layout):Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.asBits < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_NUMBER_SET);
end;

function JSVAL_IS_UNDEFINED_IMPL(const l: jsval_layout):Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.asBits = JSVAL_SHIFTED_TAG_UNDEFINED);
end;

function JSVAL_IS_STRING_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ( uint32(l.asBits shr JSVAL_TAG_SHIFT) = JSVAL_TAG_STRING );
end;

function STRING_TO_JSVAL_IMPL(str: PJSString): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var strBits: QWord absolute str;
begin
  {$ifdef WITHASSERT}
  assert(str <> nil);
  assert( (strBits shr JSVAL_TAG_SHIFT) = 0 );
  {$endif}
  Result.asBits := (strBits or JSVAL_SHIFTED_TAG_STRING);
end;

function JSVAL_TO_STRING_IMPL(const l: jsval_layout): PJSString;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := PJSString(l.asBits and JSVAL_PAYLOAD_MASK);
end;

function JSVAL_IS_BOOLEAN_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ( uint32(l.asBits shr JSVAL_TAG_SHIFT) = JSVAL_TAG_BOOLEAN );
end;

function JSVAL_TO_BOOLEAN_IMPL(const l: jsval_layout): JSBool;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := JSBool(l.asBits);
end;

function BOOLEAN_TO_JSVAL_IMPL(b: JSBool): jsval_layout;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result.asBits := QWord(QWord(uint32(b)) or JSVAL_SHIFTED_TAG_BOOLEAN);
end;

function JSVAL_IS_MAGIC_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ((l.asBits shr JSVAL_TAG_SHIFT) = JSVAL_TAG_MAGIC);
end;

function MAGIC_JSVAL_TO_OBJECT_OR_NULL_IMPL(const l: jsval_layout): PJSObject;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var ptrBits: QWord;
begin
  ptrBits := QWord(l.asBits and JSVAL_PAYLOAD_MASK);
  {$ifdef WITHASSERT}
  Assert(JSVAL_IS_MAGIC_IMPL(l));
  Assert((ptrBits shr JSVAL_TAG_SHIFT) = 0);
  {$endif}
  Result := PJSObject(ptrBits);
end;

function JSVAL_IS_OBJECT_IMPL(const l: jsval_layout): Boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  Assert((l.asBits shr JSVAL_TAG_SHIFT) <= JSVAL_SHIFTED_TAG_OBJECT);
  {$endif}
  Result := ( l.asBits >= JSVAL_SHIFTED_TAG_OBJECT );
end;

function JSVAL_IS_PRIMITIVE_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := ( l.asBits < JSVAL_UPPER_EXCL_SHIFTED_TAG_OF_PRIMITIVE_SET );
end;

function JSVAL_IS_OBJECT_OR_NULL_IMPL(const l: jsval_layout): Boolean;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  Assert((l.asBits shr JSVAL_TAG_SHIFT) <= JSVAL_TAG_OBJECT);
  {$endif}
  Result := (l.asBits >= JSVAL_LOWER_INCL_SHIFTED_TAG_OF_OBJ_OR_NULL_SET);
end;

function JSVAL_TO_OBJECT_IMPL(const l: jsval_layout):PJSObject;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var ptrBits: QWord;
begin
  ptrBits := QWord(l.asBits and JSVAL_PAYLOAD_MASK);
  {$ifdef WITHASSERT}
  assert((ptrBits and $7) = 0);
  {$endif}
  Result := PJSObject(ptrBits);
end;

function OBJECT_TO_JSVAL_IMPL(obj: PJSObject): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var objBits: QWord;
begin
  objBits := QWord(obj);
  {$ifdef WITHASSERT}
  assert(objBits <> 0);
  assert( (objBits shr JSVAL_TAG_SHIFT) = 0);
  {$endif}
  Result.asBits := (objBits or JSVAL_SHIFTED_TAG_OBJECT);
end;

function JSVAL_IS_NULL_IMPL(const l: jsval_layout): Boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (l.asBits = JSVAL_SHIFTED_TAG_NULL);
end;

function PRIVATE_PTR_TO_JSVAL_IMPL(ptr: pointer): jsval_layout;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var ptrBits: QWord absolute ptr;
begin
  {$ifdef WITHASSERT}
  assert((ptrBits and 1) = 0);
  {$endif}
  Result.asBits := QWord(ptrBits shr 1);
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_DOUBLE_IMPL(Result));
  {$endif}
end;

function JSVAL_TO_PRIVATE_PTR_IMPL(const l: jsval_layout): pointer;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
begin
  {$ifdef WITHASSERT}
  assert((l.asBits and QWord($8000000000000000)) = 0);
  {$endif}
  Result := pointer(l.asBits shl 1);
end;

function JSVAL_IS_GCTHING_IMPL(const l: jsval_layout): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
 Result := (l.asBits >= JSVAL_LOWER_INCL_SHIFTED_TAG_OF_GCTHING_SET);
end;

function JSVAL_TO_GCTHING_IMPL(const l: jsval_layout): pointer;
  {$ifndef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
var ptrBits: QWord;
begin
  ptrBits := (l.asBits and JSVAL_PAYLOAD_MASK);
  {$ifdef WITHASSERT}
  assert( (ptrBits and $7) = 0);
  {$endif}
  Result := Pointer(ptrBits);
end;

function JSVAL_IS_TRACEABLE_IMPL(const l: jsval_layout): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  Result := (JSVAL_IS_GCTHING_IMPL(l) and not JSVAL_IS_NULL_IMPL(l));
end;

function JSVAL_TRACE_KIND_IMPL(const l: jsval_layout): uint32;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if JSVAL_IS_OBJECT_IMPL(l) then
    Result := uint32(JS_FALSE) else
    Result := uint32(JS_TRUE);
end;

{$endif} //CPU64



// jsapi.h functions

function JSVAL_IS_NULL(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_NULL_IMPL(jsval_layout(v));
end;

function JSVAL_IS_VOID(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_UNDEFINED_IMPL(jsval_layout(v));
end;

function JSVAL_IS_INT(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_INT32_IMPL(jsval_layout(v));
end;

function JSVAL_TO_INT(const v: jsval): jsint;
begin
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_INT(v));
  {$endif}
  Result := JSVAL_TO_INT32_IMPL(jsval_layout(v));
end;

function INT_TO_JSVAL(i: int32): jsval;
begin
  Result := INT32_TO_JSVAL_IMPL(i).asBits;
end;

function JSVAL_IS_DOUBLE(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_DOUBLE_IMPL(jsval_layout(v));
end;

function JSVAL_TO_DOUBLE(const v: jsval): Double;
begin
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_DOUBLE(v));
  {$endif}
  Result := jsval_layout(v).asDouble;
end;

function DOUBLE_TO_JSVAL(d: double): jsval;
begin
  with jsval_layout(result) do begin
    asDouble := d;
    if ((asBits and $7FF0000000000000) = $7FF0000000000000) and
       ((asBits and $000FFFFFFFFFFFFF) <> $0000000000000000) then
      asBits := $7FF8000000000000; // canonize NaN
  end;
end;

function UINT_TO_JSVAL(i: uint32): jsval;
begin
 if (i <= uint32(JSVAL_INT_MAX)) then
   result := INT_TO_JSVAL(int32(i)) else
   result := DOUBLE_TO_JSVAL(i);
end;

function JSVAL_IS_NUMBER(const v: jsval): boolean;
begin
  Result := JSVAL_IS_NUMBER_IMPL(jsval_layout(v));
end;

function JSVAL_IS_STRING(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_STRING_IMPL(jsval_layout(v));
end;

function JSVAL_TO_STRING(const v: jsval): PJSString;
begin
  {$ifdef WITHASSERT}
  Assert(JSVAL_IS_STRING(v));
  {$endif}
  Result := JSVAL_TO_STRING_IMPL(jsval_layout(v));
end;

function STRING_TO_JSVAL(str: PJSString): jsval;
begin
  Result := STRING_TO_JSVAL_IMPL(str).asBits;
end;

function JSVAL_IS_OBJECT(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_OBJECT_OR_NULL_IMPL(jsval_layout(v));
end;

function JSVAL_TO_OBJECT(const v: jsval): PJSObject;
begin
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_OBJECT(v));
  {$endif}
  Result := JSVAL_TO_OBJECT_IMPL(jsval_layout(v));
end;

function OBJECT_TO_JSVAL(obj: PJSObject): jsval;
begin
  if obj<>nil then
    Result := OBJECT_TO_JSVAL_IMPL(obj).asBits else
    Result := JSVAL_NULL;
end;

function JSVAL_IS_BOOLEAN(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_BOOLEAN_IMPL(jsval_layout(v));
end;

function JSVAL_TO_BOOLEAN(const v: jsval): JSBool;
begin
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_BOOLEAN(v));
  {$endif}
  Result := JSVAL_TO_BOOLEAN_IMPL(jsval_layout(v));
end;

function BOOLEAN_TO_JSVAL(b: JSBool): jsval;
begin
  Result := BOOLEAN_TO_JSVAL_IMPL(b).asBits;
end;

function JSVAL_IS_PRIMITIVE(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_PRIMITIVE_IMPL(jsval_layout(v));
end;

function JSVAL_IS_GCTHING(const v: jsval): Boolean;
begin
  Result := JSVAL_IS_GCTHING_IMPL(jsval_layout(v));
end;

function JSVAL_TO_GCTHING(const v: jsval): Pointer;
begin
  {$ifdef WITHASSERT}
  assert(JSVAL_IS_GCTHING(v));
  {$endif}
  Result := JSVAL_TO_GCTHING_IMPL(jsval_layout(v));
end;

function PRIVATE_TO_JSVAL(ptr: Pointer): jsval;
begin
  Result := PRIVATE_PTR_TO_JSVAL_IMPL(ptr).asBits;
end;

function JSVAL_TO_PRIVATE(const v: jsval): Pointer;
begin
  {$ifdef WITHASSERT}
  Assert(JSVAL_IS_DOUBLE(v));
  {$endif}
  Result := JSVAL_TO_PRIVATE_PTR_IMPL(jsval_layout(v));
end;


function JS_CALLEE(cx: PJSContext; vp: Pjsval): jsval;
begin
  Result := vp^;
end;

function JS_THIS(cx: PJSContext; vp: Pjsval): jsval;
begin
  if JSVAL_IS_PRIMITIVE_IMPL(jsval_layout(PjsvalVector(vp)[1])) then
    result := JS_ComputeThis(cx, vp) else
    result := PjsvalVector(vp)[1];
end;

function JS_THIS_OBJECT(cx: PJSContext; vp: Pjsval): PJSObject;
begin
  Result := JSVAL_TO_OBJECT(JS_THIS(cx,vp));
end;

function JS_ARGV(cx: PJSContext; vp: Pjsval): PjsvalVector;
begin
  inc(vp,2); //#define JS_ARGV(cx,vp) ((vp) + 2)
  result := pointer(vp);
end;

function JS_RVAL(cx: PJSContext; vp: Pjsval): jsval;
begin
  Result := vp^;
end;

procedure JS_SET_RVAL(cx: PJSContext; vp: Pjsval; v: jsval);
begin
  vp^ := v;
end;

{$ifndef JS_THREADSAFE}
function JS_GetClass(cx: PJSContext; obj: PJSObject): PJSClass;
begin
  Result := _JS_GetClass(cx, obj);
end;
{$endif}


function JS_IsConstructing(cx: PJSContext; const vp: PjsvalVector): boolean;
begin
//  l.asBits = JSVAL_BITS(vp[1]);
//  return JSVAL_IS_MAGIC_IMPL(l);
  Result := JSVAL_IS_MAGIC_IMPL(jsval_layout(vp[1])); //TODO check!
end;


{ JSContext }

procedure JSContext.Destroy;
begin
  if @self<>Nil then
    JS_DestroyContext(@self);
end;

function JSContext.GetOptions: TJSOptions;
begin
  result := TJSOptions(JS_GetOptions(@self));
end;

function JSContext.GetPrivate: Pointer;
begin
  result := JS_GetContextPrivate(@self);
end;

function JSContext.GetVersion: JSVersion;
begin
  result := JS_GetVersion(@self);
end;

function JSContext.VersionToString: RawUTF8;
var v: JSVersion;
begin
  v := JS_Getversion(@self);
  result := JS_VersionToString(v);
  if result[1]>'9' then
    result := FormatUTF8('% (%.%)',[result,v div  100,v mod 100])
end;

function JSContext.InitStandardClasses(global: PJSObject): boolean;
begin
  result := boolean(JS_InitStandardClasses(@self, global));
end;

function JSContext.Runtime: PJSRuntime;
begin
  result := JS_GetRuntime(@self);
end;

procedure JSContext.SetOptions(const Value: TJSOptions);
begin
  JS_SetOptions(@self,uint32(Value));
end;

procedure JSContext.SetPrivate(const Value: Pointer);
begin
  JS_SetContextPrivate(@self,Value);
end;

function JSContext.NewJSString(const Value: RawUTF8): PJSString;
begin
  result := NewJSString(pointer(Value),length(Value),CP_UTF8);
end;

function JSContext.NewJSString(const Value: SynUnicode): PJSString;
begin
  result := JS_NewUCStringCopyN(@Self, pointer(Value), Length(Value));
end;

function JSContext.NewJSString(TextWide: PWideChar; TextLen: integer): PJSString;
begin
  result := JS_NewUCStringCopyN(@Self, pointer(TextWide), TextLen);
end;

function JSContext.NewJSString(TextAnsi: PAnsiChar; TextLen, CodePage: integer): PJSString;
var short: array[byte] of WideChar; // to avoid temp allocation on heap
    buf: PWideChar;
begin
  if TextLen<(sizeof(short) div 3) then
    buf := @short else
    GetMem(buf,TextLen*3+2);
  result := JS_NewUCStringCopyN(@Self, PJschar(buf),
    TSynAnsiConvert.Engine(CodePage).AnsiBufferToUnicode(buf,TextAnsi,TextLen)-buf);
  if buf<>@short then
    FreeMem(buf);
end;


{ JSRuntime }

procedure JSRuntime.Destroy;
begin
  if @self<>nil then
    JS_DestroyRuntime(@self);
end;

function JSRuntime.GetPrivate: Pointer;
begin
  result := JS_GetRuntimePrivate(@self);
end;

procedure JSRuntime.Lock;
begin
  JS_LockRuntime(@self);
end;

procedure JSRuntime.SetPrivate(const Value: Pointer);
begin
  JS_SetRuntimePrivate(@self,Value);
end;

procedure JSRuntime.Unlock;
begin
  JS_UnlockRuntime(@self);
end;


{ JSString }

function JSString.ToJSVal: jsval;
begin
  Result := STRING_TO_JSVAL(@self)
end;

function JSString.ToString(cx: PJSContext): string;
var buf: pjschar;
    len: size_t;
begin
  if @self<>nil then begin
    buf := JS_GetStringCharsAndLength(cx, @self, len);
    if len>0 then
      RawUnicodeToString(PWideChar(buf),len,result) else
      result := '';
  end else
      result := '';
end;

procedure JSString.ToJSONString(cx: PJSContext; W: TTextWriter);
var buf: pjschar;
    len: size_t;
begin
  buf := JS_GetStringCharsAndLength(cx, @self, len);
  W.Add('"');
  W.AddJSONEscapeW(pointer(buf),len);
  W.Add('"');
end;

function JSString.ToWideString(cx: PJSContext): WideString;
var buf: pjschar;
    len: size_t;
begin
  buf := JS_GetStringCharsAndLength(cx, @self, len);
  if len>0 then
    SetString(result,PWideChar(buf),len) else
    result := '';
end;

procedure JSString.ToVariant(cx: PJSContext; var Value: Variant);
var len: size_t;
begin
  with TVarData(Value) do begin
    VarClear(Value);
    VType := varSynUnicode;
    VAny := nil; // avoid GPF below
    SetString(SynUnicode(VAny),
      PWideChar(JS_GetStringCharsAndLength(cx,@self,len)),len);
  end;
end;

function JSString.ToSynUnicode(cx: PJSContext): SynUnicode;
var buf: pjschar;
    len: size_t;
begin
  buf := JS_GetStringCharsAndLength(cx, @self, len);
  if len>0 then
    SetString(result,PWideChar(buf),len) else
    result := '';
end;

function JSString.ToUTF8(cx: PJSContext): RawUTF8;
begin
  ToUTF8(cx,result);
end;

procedure JSString.ToUTF8(cx: PJSContext; var result: RawUTF8);
var buf: pjschar;
    len: size_t;
begin
  buf := JS_GetStringCharsAndLength(cx,@self,len);
  if len>0 then
    RawUnicodeToUTF8(PWideChar(buf),len,result) else
    result := '';
end;

procedure JSString.ToUTF8(cx: PJSContext; W: TTextWriter);
var buf: pjschar;
    len: size_t;
begin
  buf := JS_GetStringCharsAndLength(cx, @self, len);
  W.AddNoJSONEscapeW(pointer(buf),len);
end;


{ JSObject }

function JSObject.ToJSValue: jsval;
begin
  if @self=nil then
    result := JSVAL_NULL else
    result := OBJECT_TO_JSVAL(@self);
end;


{ JSCompartment }

procedure JSCompartment.Destroy;
begin
  if @self<>nil then
    JS_LeaveCompartment(fcx,@self);
end;

function JSCompartment.EnterCompartment(cx: PJSContext;
  target: PJSObject): PJSCompartment;
begin
  Result := JS_EnterCompartment(cx,target);
  if Assigned(Result) then
    Result.fcx := cx;
end;

// procedure JS_ShutDown; cdecl; external SpiderMonkeyLib;

procedure JS_ShutDown; // avoid need of mozjs-24.dll even if not used
var Shutdown: procedure; cdecl;
begin
 Shutdown := GetProcAddress(GetModuleHandle(SpiderMonkeyLib),'JS_ShutDown');
 if Assigned(Shutdown) then
   Shutdown;
end;


initialization
  assert(1 shl ord(jsoTypeInference)=JSOPTION_TYPE_INFERENCE);
  assert(1 shl ord(sjpShortID)=JSPROP_SHORTID);

finalization
  JS_ShutDown;

end.

