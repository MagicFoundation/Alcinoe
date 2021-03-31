/// features JavaScript execution using the SpiderMonkey library
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSM;
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


  ---------------------------------------------------------------------------
   Download the SpiderMonkey library at https://synopse.info/files/synsm.7z !
  ---------------------------------------------------------------------------


  Version 1.18
  - initial release. Use SpiderMonkey 24
  - add TSMObject.defineNativeMethod
  - add JSError procedure for Exception handling inside of JSNative function
  - enhanced multi thread process
  - add TSMEngine.MaybeGarbageCollect method
  - add timeout Framework

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER
{$I SynSM.inc}   // define SM_DEBUG JS_THREADSAFE CONSIDER_TIME_IN_Z

interface

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  Variants,
  SynCommons,
  SynLog,
  SynTests,
  SynTable,
  SynSMAPI;

const
  /// default stack growing size, in bytes
  STACK_CHUNK_SIZE: cardinal = 8192;

type
  /// generic parent class of all SpiderMonkey-related Exception types
  ESMException = class(ESynException);

  {$M+}
  TSMEngineManager = class;
  {$M-}

  TSMEngine = class;


  /// just a wrapper around jsval API type, to be used with our object wrappers
  // - SpiderMonkey jsval type can be directly casted to this type via TSMValue(jsval)
  // - note that some methods expect an execution context to be supplied as
  // parameter, as soon as it contains a non primitive type (double/integer)
  TSMValue = object
  protected
    FValue: jsval;
  public
    /// type of the value
    // - you should better use this before calling other To*() methods
    function ValType(cx: PJSContext): JSType;
      {$ifdef HASINLINE}inline;{$endif}

    /// direct access to the internal jsval instance
    property AsJSVal: jsval read FValue write FValue;

    /// set the value as one 32 bit integer
    procedure SetInteger(const Value: integer);
      {$ifdef HASINLINE}inline;{$endif}
    /// read the value as one 32 bit integer
    function ToInteger: integer;
      {$ifdef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// access to the value as integer
    property AsInteger: integer read ToInteger write SetInteger;

    /// set the value as floating point
    procedure SetDouble(const Value: double);
      {$ifdef HASINLINE}inline;{$endif}
    /// read the value as floating point
    function ToDouble: double;
      {$ifdef WITHASSERT}{$ifdef HASINLINE}inline;{$endif}{$endif}
    /// access to the value as floating point
    property AsDouble: double read ToDouble write SetDouble;

    /// set the value as boolean
    procedure SetBoolean(const Value: boolean);
      {$ifdef HASINLINE}inline;{$endif}
    /// read the value as boolean
    function ToBoolean: boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// access to the value as boolean
    property AsBoolean: boolean read ToBoolean write SetBoolean;

    /// set the value as one 64 bit integer
    // - this is a somewhat dirty hack, since SpiderMonkey don't support int64:
    // but it is possible to transform int64 to double for ant value < (1 shl 51)
    // - sometimes we need int64 to be passed do SpiderMonkey (e.g. for an ID)
    procedure SetInt64(const Value: int64);
    /// read the value as one 64 bit integer
    // - note that SpiderMonkey is not able to store all Int64 values directly
    function ToInt64: int64;
    /// access to the value as one 64 bit integer
    property AsInt64: int64 read ToInt64 write SetInt64;

    /// set the value as VOID
    procedure SetVoid; {$ifdef HASINLINE}inline;{$endif}
    /// set the value as NULL
    procedure SetNull; {$ifdef HASINLINE}inline;{$endif}

    /// set the value as variant (not implemented yet)
    // - will set any custom variant type (e.g. TDocVariant) as a JavaScript
    // object value computed from the JSON serialization of the variant
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    procedure SetVariant(cx: PJSContext; const Value: Variant);
    /// return the value as variant (not implemented yet)
    // - will return any JavaScript string value directly as a RawUTF8
    // - will return any JavaScript object value as a TDocVariant document
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    function ToVariant(cx: PJSContext): Variant; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the value as variant (not implemented yet)
    // - will return any JavaScript string value directly as a RawUTF8
    // - will return any JavaScript object value as a TDocVariant document
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    procedure ToVariant(cx: PJSContext; var result: Variant); overload;

    /// set the value as TVarRec (i.e. an "array of const" open parameter)
    // - here any AnsiString parameter is expected to be a RawUTF8 before Delphi
    // 2009, or its correct code page will be retrieved since Delphi 2009
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    procedure SetTVarRec(cx: PJSContext; const V: TVarRec);
    /// set the value as an UTF-16 encoded buffer
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    // - warning - JSString string is a subject for GC so you must root it or set
    // as property of some object or use SetNativeString() method to pass the
    // value by reference
    procedure SetWideChar(cx: PJSContext; Text: PWideChar; TextLen: integer);
    /// set the value as an Ansi encoded buffer (may be UTF-8 or any code page)
    // - if CodePage is 0, will use the CurrentAnsiCodePage value
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    // - warning - JSString string is a subject for GC so you must root it or set
    // as property of some object or use SetNativeString() method to pass the
    // value by reference
    procedure SetAnsiChar(cx: PJSContext; Text: PAnsiChar; TextLen, CodePage: integer);

    /// set the value as an Unicode String
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    // - warning - JSString string is a subject for GC so you must root it or set
    // as property of some object or use SetNativeString() method to pass the
    // value by reference
    procedure SetSynUnicode(cx: PJSContext; const aStr: SynUnicode);
      {$ifdef HASINLINE}inline;{$endif}
    /// return the value as an Unicode String
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    function ToSynUnicode(cx: PJSContext): SynUnicode; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// return the value as an Unicode String
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    procedure ToSynUnicode(cx: PJSContext; var result: SynUnicode); overload;
      {$ifdef HASINLINE}inline;{$endif}

    /// set the value as an Unicode WideString
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    // - warning - JSString string is a subject for GC so you must root it or set
    // as property of some object or use SetNativeString() method to pass the
    // value by reference
    procedure SetWideString(cx: PJSContext; const aStr: WideString);
      {$ifdef HASINLINE}inline;{$endif}
    /// return the value as an Unicode WideString
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    function ToWideString(cx: PJSContext): WideString;
      {$ifdef HASINLINE}inline;{$endif}

    /// set the value as an UTF-8 String
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    // - warning - JSString string is a subject for GC so you must root it or set
    // as property of some object or use SetNativeString() method to pass the
    // value by reference
    procedure SetUTF8(cx: PJSContext; const aStr: RawUTF8);
      {$ifdef HASINLINE}inline;{$endif}
    /// return the value as an UTF-8 String
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    function ToUTF8(cx: PJSContext): RawUTF8;
      {$ifdef HASINLINE}inline;{$endif}

    /// set the value from UTF-8 encoded JSON
    // - returns TRUE if aJSON was valid, FALSE in case of an error
    function SetJSON(cx: PJSContext; const aJSON: RawUTF8): boolean;
    /// return the value as UTF-8 encoded JSON
    function ToJSON(cx: PJSContext): RawUTF8;
    /// add the value as UTF-8 encoded JSON
    procedure AddJSON(cx: PJSContext; W: TTextWriter);

    /// set the value as Unicode String by reference
    // - this is the fastest way to add a string to SpiderMonley: String is in
    // fact not copied to the SpiderMonkey engine, just passed by reference
    // - Only SynUnicode string support by now (SpiderMonkey is internally UTF-16 based)
    // - WARNING - as a consequence, aStr must be UNCHANGED until SpiderMonkey engine
    // points to it (SpiderMonkey will also consider its strings as immutable, so will
    // never change its content during execution) - for instance, never pass a
    // function result as aStr, nor use a local SynUnicode variable unless you
    // trigger the Garbage Collection before the end of the local method
    procedure SetNativeString(cx: PJSContext; const aStr: SynUnicode);

    /// set the value as a date/time
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    procedure SetDateTime(cx: PJSContext; const Value: TDateTime);
    /// return the value as a date/time
    // - in SpiderMonkey non-simple type instances do exist in a given JSContext,
    // so we need to know the execution context (using a property is not an option)
    function ToDateTime(cx: PJSContext): TDateTime;

    /// transform a JSValue to its UTF-16 string representation
    // - JavaScript equivalent is
    // ! variable.toString()
    function TransformToSynUnicode(cx: PJSContext): SynUnicode;
    /// transform a JSValue to its UTF-8 string representation
    // - JavaScript equivalent is
    // ! variable.toString()
    function TransformToUTF8(cx: PJSContext): RawUTF8;

    /// attemps to convert the value into a native function pointer
    function ToNativeFunction(cx: PJSContext): PJSFunction;
    /// attemps to convert the value into a native function name
    function ToNativeFunctionName(cx: PJSContext): RawUTF8;
  end;

  /// a pointer to a jsval wrapper
  PSMValue = ^TSMValue;

  /// a jsval wrappers array
  TSMValues = array[0..(MaxInt div sizeof(TSMValue))-1] of TSMValue;

  /// a pointer to a jsval wrappers array
  PSMValues = ^TSMValues;

  /// a dynamic array of jsval wrappers
  SMValArray = array of TSMValue;

  /// just a wrapper around JavaScript Object API type, to be used with other
  // values wrappers
  // - SpiderMonkey object type can NOT be directly casted to this type via
  // TSMObject(jsobject) - use JSObject wrapper instead - since we expects
  // an execution context to be specified
  // - to create instance of this structure, use TSMEngine.NewObject() or
  // MakeObject() overloaded methods
  TSMObject = object
  private
    FDefaultPropertyAttrs: TJSPropertyAttrs;
    function GetPrivate: pointer;
    procedure SetPrivate(const Value: pointer);
    function GetItem(aIndex: integer): variant;
    procedure SetItem(aIndex: integer; const Value: variant);
    procedure SetDefaultPropertyAttrs(const Value: TJSPropertyAttrs);
  protected
    fCx: PJSContext;
    fObj: PJSObject;
    procedure SetPropVariant(const propName: SynUnicode; const Value: variant);
  public
    /// get the parent object of a given object
    function Parent: TSMObject;
      {$ifdef HASINLINE}inline;{$endif}
    /// get the prototype of a given object
    function Prototype: TSMObject;
      {$ifdef HASINLINE}inline;{$endif}
    /// access the private data field of an object
    // - wrapper to JS_GetPrivate()/JS_SetPrivate()
    // - only works if the object's JSClass has the JSCLASS_HAS_PRIVATE flag:
    // it is safer to use GetPrivateData() method and providing the JSClass
    property PrivateData: pointer read GetPrivate write SetPrivate;
    /// retrieve the private data associated with an object, if that object
    // is an instance of a specified class
    // - wrapper to JS_GetInstancePrivate()
    function GetPrivateData(expectedClass: PJSClass): pointer;

    /// return TRUE if the object is an array
    function IsArray: boolean;
    /// return the number of elements in this array
    function ItemsCount: cardinal;
    /// delete an item of this object as array
    procedure DeleteItem(aIndex: integer);
    /// access to an item of this object as array
    property Items[aIndex: integer]: variant read GetItem write SetItem;

    /// define an object property with a value, specified as jsvalue
    // - this is not a direct JavaScript equivalent of
    // ! obj[name] = val
    // since any setter will be called
    // - to set a property in a global object, call either
    // ! SMEngine.Global.property := ...          // via late-binding
    // ! SMEngine.GlobalObject.DefineProperty()   // direct via TSMObject
    // equivalent in JavaScript to:
    // ! var name = value
    // outside a JavaScript function context (i.e. in global scope)
    // - if property already exists, it will just replace its value with the
    // supplied value
    // - this method will use the default properties attributes of this engine
    procedure DefineProperty(const name: SynUnicode; const value: TSMValue); overload;
    /// define an object property with a value, specified as jsvalue
    // - this is not a direct JavaScript equivalent of
    // ! obj[name] = val
    // since any setter will be called
    // - to set a property in a global object, call either
    // ! SMEngine.Global.property := ...          // via late-binding
    // ! SMEngine.GlobalObject.DefineProperty()   // direct via TSMObject
    // equivalent in JavaScript to:
    // ! var name = value
    // outside a JavaScript function context (i.e. in global scope)
    // - if property already exists, it will just replace its value with the
    // supplied value
    // - this method will allow to set custom properties attributes of this engine
    procedure DefineProperty(const name: SynUnicode; const value: TSMValue;
      attrs: TJSPropertyAttrs); overload;
    /// define an object property with a value, specified as variant
    // - you can also use the property Properties[]
    // - this is not a direct JavaScript equivalent of
    // ! obj[name] = val
    // since any setter will be called
    // - to set a property in a global object, call either
    // ! SMEngine.Global.property := ...          // via late-binding
    // ! SMEngine.GlobalObject.DefineProperty()   // direct via TSMObject
    // equivalent in JavaScript to:
    // ! var name = value
    // outside a JavaScript function context (i.e. in global scope)
    // - if property already exists, it will just replace its value with the
    // supplied value
    // - this method will use the default properties attributes of this engine
    procedure DefineProperty(const name: SynUnicode; const value: variant); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// define an object property with a value, specified as variant
    // - you can also use the property Properties[]
    // - this is not a direct JavaScript equivalent of
    // ! obj[name] = val
    // since any setter will be called
    // - to set a property in a global object, call either
    // ! SMEngine.Global.property := ...          // via late-binding
    // ! SMEngine.GlobalObject.DefineProperty()   // direct via TSMObject
    // equivalent in JavaScript to:
    // ! var name = value
    // outside a JavaScript function context (i.e. in global scope)
    // - if property already exists, it will just replace its value with the
    // supplied value
    // - this method will allow to set custom properties attributes of this engine
    procedure DefineProperty(const name: SynUnicode; const value: variant;
      attrs: TJSPropertyAttrs); overload;
      {$ifdef HASINLINE}inline;{$endif}

    /// add JSNative compatible function into JS object
    // - here the method name is specified as SynUnicode
    // - func  if reference to function with JSNative signature
    // - nargs is function argument count
    // - actually this method creates a JSFunction and assing its value to
    // ! obj[methodName]
    // - to add a global function, define it into the "global" object - i.e. call
    // ! TSMEngine.GlobalObject.DefineNativeMethod()
    // - this method will use the default properties attributes of this engine
    function DefineNativeMethod(const methodName: SynUnicode;
      func: JSNative; nargs: uintN): PJSFunction; overload;
    /// add JSNative compatible function into JS object
    // - here the method name is specified as SynUnicode
    // - func  if reference to function with JSNative signature
    // - nargs is function argument count
    // - actually this method creates a JSFunction and assing its value to
    // ! obj[methodName]
    // - to add a global function, define it into the "global" object - i.e. call
    // ! TSMEngine.GlobalObject.DefineNativeMethod()
    // - this method will allow to set custom properties attributes of this engine
    function DefineNativeMethod(const methodName: SynUnicode;
      func: JSNative; nargs: uintN; attrs: TJSPropertyAttrs): PJSFunction; overload;
    /// add JSNative compatible function into JS object
    // - here the method name is specified as AnsiString
    // - func  if reference to function with JSNative signature
    // - nargs is function argument count
    // - this method will use the default properties attributes of this engine
    function DefineNativeMethod(const methodName: AnsiString;
      func: JSNative; nargs: uintN): PJSFunction; overload;
    /// add JSNative compatible function into JS object
    // - here the method name is specified as AnsiString
    // - func  if reference to function with JSNative signature
    // - nargs is function argument count
    // - this method will allow to set custom properties attributes of this engine
    function DefineNativeMethod(const methodName: AnsiString;
      func: JSNative; nargs: uintN; attrs: TJSPropertyAttrs): PJSFunction; overload;

    /// check object property does exist (including prototype chain lookup)
    function HasProperty(const propName: SynUnicode): Boolean;
    /// Determine whether a property is physically present on a object
    // - JavaScript equivalent of
    // ! Object.hasOwnProperty(propName)
    function HasOwnProperty(const propName: SynUnicode): Boolean;
    /// get object property value (call geter for native)
    // - JavaScript equivalent of
    // ! obj[name]
    // - returns JSVAL_VOID if object does not have such property
    function GetPropValue(const propName: SynUnicode): TSMValue;
    /// get object property value (call geter for native)
    // - you can also use the property Properties[]
    // - JavaScript equivalent of
    // ! obj[name]
    // - returns null if object does not have such property
    function GetPropVariant(const propName: SynUnicode): variant;

    /// read/write access to the object properties as variant
    property Properties[const propName: SynUnicode]: variant
      read GetPropVariant write SetPropVariant; default;

    /// evaluate JavaScript script in the current object scope
    // - if exception raised in script - raise Delphi ESMException
    // - on success, returns the last executed expression statement processed
    // in the script in low-level result output variable
    // - JavaScript Equivalent of
    // ! with(obj) eval(script)
    // - be careful about execution scope - see JS_ExecuteScript() description
    // - usualy you need to evaluate script only in global object scope, so you
    // should better always call TSMEngine.Evaluate()
    procedure Evaluate(const script: SynUnicode; const scriptName: RawUTF8;
      lineNo: Cardinal; out result: TSMValue);

    /// executes a JavaScript object method using low-level SMVal arguments
    // - returns the function result as a TSMValue
    // - JavaScript equivalent of
    // ! rval := obj.methodName(argv[0], ....);
    procedure RunMethod(const methodName: AnsiString; const argv: SMValArray;
      out rval: TSMValue); overload;
    /// executes a JavaScript object method using a Delphi array of const
    // - returns the function result as a TSMValue
    // - JavaScript equivalent of
    // ! rval := obj.methodName(argv[0], ....);
    // - here any AnsiString parameter is expected to be a RawUTF8 before Delphi
    // 2009, or its correct code page will be retrieved since Delphi 2009
    procedure RunMethod(const methodName: AnsiString; const argv: array of const;
      out rval: TSMValue); overload;
    /// executes a JavaScript object method using a Delphi array of variants
    // - returns the function result as a variant
    // - JavaScript equivalent of
    // ! rval := obj.methodName(argv[0], ....);
    function Run(const methodName: AnsiString; const argv: array of variant): variant;

    /// returns the associated execution context
    property cx: PJSContext read fCx;
    /// returns the associated jsobject instance
    property obj: PJSObject read fObj;
    /// returns the associated jsobject instance as a jsvalue
    function AsSMValue: TSMValue;

    /// protect the object from Garbage Collection
    // - if this object is not set as property value of any other object
    // or passed as parameter to function, you must protect it
    procedure Root;
    /// unprotect a previously "rooted" object
    // - WARNING!! Object MUST be protected by a previous Root method call,
    // otherwise you get an access violation
    procedure UnRoot;
    /// set properties obj and cx to nil
    procedure Clear;

    /// returns the associated script engine instance
    function Engine: TSMEngine;
      {$ifdef HASINLINE}inline;{$endif}

    /// access to the default attributes when accessing any properties
    property DefaultPropertyAttrs: TJSPropertyAttrs read FDefaultPropertyAttrs write SetDefaultPropertyAttrs;
  end;


  //// variant-based callback signature used for TSMEngine.RegisterMethod()
  // - any Delphi exception raised during this execution will be converted into
  // a JavaScript exception by TSMEngine
  // - "this" JavaScript calling object is transmitted as a TSMVariant custom
  // variant: you can use late-binding over it to access its methods
  // or properties, or transtype it using TSMVariantData(Instance)
  // and access its low-level API content
  // - input arguments (and function result) are simple variant values, or
  // TDocVariant custom variant instance for any object as complex document
  // - corresponds to meVariant kind of callback method
  TSMEngineMethodEventVariant = function(const This: variant;
    const Args: array of variant): variant of object;

  //// JSON-based callback signature used for TSMEngine.RegisterMethod()
  // - any Delphi exception raised during this execution will be converted into
  // a JavaScript exception by TSMEngine
  // - similar to TServiceMethod.InternalExecute() as defined in mORMot.pas
  // (for instance, this callback will be used to execute native Delphi
  // interface-based methods from JavaScript code in mORMotSM.pas unit)
  // - "this" JavaScript calling object is transmitted as low-level TSMObject
  // - will expect as input a JSON array of parameters from Args, e.g.
  // ! '[1,2,3]'
  // - if the method only expect one result, shall return one JSON value, e.g.
  // ! '6'
  // - if the method expect more than one result (i.e. several var/out parameters
  // in addition to the main function result), it shall return a JSON object,
  // with parameter names for all var/out/result values, e.g.
  // ! '{"first":1,"second":2,"result":3}'
  // - this allows the function result to be consumed by the JavaScript as
  // a regular JS value or object
  // - corresponds to meJSON kind of callback method
  TSMEngineMethodEventJSON = function(const This: TSMObject;
    const Args: RawUTF8): RawUTF8 of object;

  /// pointer to our wrapper around JavaScript Object
  PSMObject = ^TSMObject;

  /// kinds of callback methods available for TSMEngine.RegisterMethod()
  TSMEngineMethodEventKind = (meVariant, meJSON);

  /// used to store one registered method event
  TSMEngineMethodEvent = record
    Func: PJSFunction;
    case EventKind: TSMEngineMethodEventKind of
      meVariant: (CallbackVariant: TSMEngineMethodEventVariant);
      meJSON:    (CallbackJSON:    TSMEngineMethodEventJSON);
  end;
  /// used to store the registered method events
  TSMEngineMethodEventDynArray = array of TSMEngineMethodEvent;

  /// implements a ThreadSafe JavaScript engine
  // - use TSMEngineManager.ThreadSafeEngine to retrieve the Engine instance
  // corresponding to the current thread, in multithread application
  // - contains JSRuntime + JSContext (to be ready for new SpiderMonkey version where
  // context and runtime is the same)
  // - contains also one "global" JavaScript object. From script it is
  // accessible via "global." (in browser, this is the "window." object)
  // - set SpiderMonkey error reporter and store last SpiderMonkey error in
  // LastError property
  TSMEngine = class
  protected
    fRt: PJSRuntime;
    fCx: PJSContext;
    fcomp: PJSCompartment;
    fNativeMethod: TSMEngineMethodEventDynArray;
    fNativeMethods: TDynArrayHashed;
    fNativeMethodCount: integer;
    FManager: TSMEngineManager;
    FGlobal: variant;
    FGlobalObject: TSMObject;
    FEngineContentVersion: Cardinal;
    FStringFinalizer: JSStringFinalizer;
    FThreadID: TThreadID;
    FLastErrorMsg: RawUTF8;
    FLastErrorFileName: RawUTF8;
    FLastErrorLine: integer;
    FLastErrorStackTrace: RawUTF8;
    FErrorExist: boolean;
    function InternalRegisterMethod(obj: PJSObject; const MethodName: SynUnicode;
      const Event: TMethod; Kind: TSMEngineMethodEventKind; ArgumentsCount: integer): PJSFunction;
    /// called from SpiderMonkey callback. Do not raise exception here
    // instead use CheckJSError metod after JSAPI compile/evaluate call
    procedure DoProcessJSError(errMsg: PCChar; report: PJSErrorReport); virtual;
    /// called from SpiderMonkey callback. It used for interrupt execution of script
    //  when it executes too long
    function DoProcessOperationCallback: JSBool; virtual;
    procedure CancelExecution;
  private
    FDefaultPropertyAttrs: TJSPropertyAttrs;
    procedure SetDefaultPropertyAttrs(const Value: TJSPropertyAttrs);
  protected
    // used by Watchdog thread state. See js.cpp
    fTimeOutAborted: Boolean;
    fTimedOut: Boolean;
    fWatchdogLock: PRLock;
    fWatchdogWakeup: PRCondVar;
    fWatchdogThread: PRThread;
    fWatchdogHasTimeout: Boolean;
    fWatchdogTimeout: Int64;
    fSleepWakeup: PRCondVar;
    fTimeoutInterval: double;
    function ScheduleWatchdog(t: Double): Boolean;
    procedure KillWatchdog;
    function InitWatchdog: boolean;
    procedure SetTimeoutValue(const Value: Double);
  public
    /// create one threadsafe JavaScript Engine instance
    // - initialize internal JSRuntime, JSContext, and global objects and
    // standard JavaScript classes
    // - do not create Engine directly via this constructor, but instead call
    // TSMEngineManager.ThreadSafeEngine
    constructor Create(aManager: TSMEngineManager); virtual;
    /// finalize the JavaScript engine instance
    destructor Destroy; override;

    /// check if last call to JSAPI compile/eval fucntion was successful
    // - raise ESMException if any error occurred
    // - put error description to SynSMLog
    procedure CheckJSError(res: JSBool); virtual;
    /// clear last JavaScript error
    // - called before every evaluate() function call
    procedure ClearLastError;

    /// trigger Garbage Collection
    // - all unrooted things (JSString, JSObject, VSVal) will be released
    procedure GarbageCollect;
    /// Offer the JavaScript engine an opportunity to perform garbage collection if needed
    // - Tries to determine whether garbage collection in would free up enough
    // memory to be worth the amount of time it would take. If so, it performs
    // some garbage collection
    // - Frequent calls are safe and will not cause the application to spend a
    // lot of time doing redundant garbage collection work
    procedure MaybeGarbageCollect;

    /// create new ordinary JavaScript object
    // - JavaScript equivalent of
    // ! {}
    // - new object is subject to Garbage Collection, so must be rooted or
    // assigned as value for a property to create new object type property,
    // as in JavaScript:
    // ! var obj = {}
    procedure NewObject(out newobj: TSMObject); overload;
    /// create new ordinary JavaScript object, stored as TSMVariant custom type
    // - JavaScript equivalent of
    // ! {}
    // - new object is subject to Garbage Collection, so should be
    // assigned as value for a property to create new object type property,
    // as in JavaScript:
    // ! var obj = {}
    function NewSMVariant: variant;
      {$ifdef HASINLINE}inline;{$endif}
    /// create new ordinary JavaScript object, stored as TSMVariant custom type,
    // and rooted to avoid garbage collection
    // - JavaScript equivalent of
    // ! {}
    // - new object is subject to Garbage Collection, so is rooted and should
    // be explicitly unrooted, e.g. via:
    // ! obj: variant;
    // ! ...
    // ! FManager.ThreadSafeEngine.NewSMVariantRooted(obj);
    // ! try
    // !   ... work with obj
    // ! finally
    // !   obj._UnRoot; // pseudo-method
    // ! end;
    procedure NewSMVariantRooted(out newobj: variant);
    /// create new JavaScript object with prototype
    // - JavaScript equivalent of
    // !  {}.__proto__ := prototype;
    procedure NewObject(const prototype: TSMObject; out newobj: TSMObject); overload;
    /// create new JavaScript object from its class
    procedure NewObjectWithClass(clasp: PJSClass; var newobj: TSMObject); overload;
    /// create new JavaScript object from its prototype
    procedure NewObjectWithClass(clasp: PJSClass; const prototype: TSMObject;  const parent: TSMObject; var newobj: TSMObject); overload;
    /// create new JavaScript object from its class and property specifications
    procedure InitClass(clasp: PJSClass; ps: PJSPropertySpec; var newobj: TSMObject);
    /// converts a JavaScript value into a JavaScript object
    procedure MakeObject(const value: TSMValue; out obj: TSMObject); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// converts a JavaScript low-level value into a JavaScript object
    procedure MakeObject(const value: jsval; out obj: TSMObject); overload;
    /// converts a JavaScript low-level object into a JavaScript object
    procedure MakeObject(jsobj: PJSObject; out obj: TSMObject); overload;

    /// register a native Delphi variant-based method for a given object
    // - the supplied function name is case-sensitive
    // - the supplied callback will be executed directly by the JavaScript
    // engine, supplying all parameters as variant (including TDocVariant for
    // any complex object), and returning the function result as variant
    // - raise an ESMException if the function could not be registered
    function RegisterMethod(obj: PJSObject; const MethodName: SynUnicode;
      const Event: TSMEngineMethodEventVariant; ArgumentsCount: integer): PJSFunction; overload;
    /// register a native Delphi JSON-based method for a given object
    // - the supplied function name is case-sensitive
    // - the supplied callback will be executed directly by the JavaScript
    // engine, supplying all parameters as JSON array, and returning the
    // function result either as a JSON value or a JSON object
    // - raise an ESMException if the function could not be registered
    function RegisterMethod(obj: PJSObject; const MethodName: SynUnicode;
      const Event: TSMEngineMethodEventJSON; ArgumentsCount: integer): PJSFunction; overload;
    /// unregister a native Delphi method for a given object
    // - raise an ESMException if the function was not previously registered
    // - you should not call it usually, but it is available in case
    procedure UnRegisterMethod(JSFunction: PJSFunction);

    /// evaluate a JavaScript script in the global scope
    // - a wrapper to GlobalObject.Evaluate(...)
    // - if exception raised in script - raise Delphi ESMException
    // - on success returns last executed expression statement processed
    // in the script as a variant
    // - JavaScript equivalent to
    // ! eval(script)
    function Evaluate(const script: SynUnicode; const scriptName: RawUTF8='script';
      lineNo: Cardinal=1): variant;

    /// access to the associated global object as a TSMVariant custom variant
    // - allows direct property and method executions in Delphi code, via
    // late-binding, for instance:
    // ! engine.Global.MyVariable := 1.0594631;
    // ! engine.Global.MyFunction(1,'text');
    property Global: variant read FGlobal;
    /// access to the associated global object as a TSMObject wrapper
    // - you can use it to register a method
    property GlobalObject: TSMObject read FGlobalObject;
    /// access to the associated global object as low-level PJSObject
    property GlobalObj: PJSObject read FGlobalObject.fobj;
    /// access to the associated execution context
    property cx: PJSContext read fCx;
    /// access to the associated execution runtime
    property rt: PJSRuntime read frt;
    /// access to the associated execution compartment
    property comp: PJSCompartment read fcomp;

    /// internal version number of engine scripts
    // - used in TSMEngine.ThreadSafeEngine to determine if context is up to
    // date, in order to trigger on-the-fly reload of scripts without the need
    // if restarting the application
    // - caller must change this parameter value e.g. in case of changes in
    // the scripts folder in an HTTP server
    property EngineContentVersion: Cardinal read FEngineContentVersion;

    /// last error message triggered during JavaScript execution
    property LastErrorMsg: RawUTF8 read FLastErrorMsg;
    /// last error source code line number triggered during JavaScript execution
    property LastErrorLine: integer read FLastErrorLine;
    /// last error file name triggered during JavaScript execution
    property LastErrorFileName: RawUTF8 read FLastErrorFileName;
    /// TRUE if an error was triggered during JavaScript execution
    property ErrorExist: boolean read FErrorExist;
    /// notifies a WatchDog timeout
    property TimeOutAborted: boolean read FTimeOutAborted;
    /// define a WatchDog timeout interval
    // - is set to -1 by default, i.e. meaning no execution timeout
    property TimeOutValue: Double read fTimeoutInterval write SetTimeoutValue;
    /// access to the default attributes when accessing any properties
    property DefaultPropertyAttrs: TJSPropertyAttrs read FDefaultPropertyAttrs write SetDefaultPropertyAttrs;
  end;


  /// prototype of SpideMonkey notification callback method
  TEngineEvent = procedure(const Engine: TSMEngine) of object;

  /// main access point to the SpiderMonkey per-thread scripting engines
  // - allow thread-safe access to an internal per-thread TSMEngine instance list
  // - contains runtime-level properties shared between thread-safe engines
  // - you can create several TSMEngineManager instances, if you need several
  // separate scripting instances
  // - set OnNewEngine callback to initialize each TSMEngine, when a new thread
  // is accessed, and tune per-engine memory allocation via MaxPerEngineMemory
  // and MaxRecursionDepth
  // - get the current per-thread TSMEngine instance via ThreadSafeEngine method
  TSMEngineManager = class
  protected
    FMaxPerEngineMemory: Cardinal;
    FMaxRecursionDepth: Cardinal;
    FEnginePool: TObjectList;
    FEngineCS: TRTLCriticalSection;
    FContentVersion: Cardinal;
    FOnNewEngine: TEngineEvent;
    procedure SetMaxPerEngineMemory(AMaxMem: Cardinal);
    /// returns -1 if none was defined yet
    // - this method is not protected via the global FEngineCS mutex/lock
    function ThreadEngineIndex(ThreadID: TThreadID): Integer;
    /// returns nil if none was defined yet
    function CurrentThreadEngine: TSMEngine;
    /// create a new SpiderMonkey Engine
    // - used by ThreadSafeEngine method to instantiate a new per-thread Engine
    function CreateNewEngine: TSMEngine; virtual;
    /// called when a new Engine is created
    // - this default implementation will run the OnNewEngine callback (if any)
    procedure DoOnNewEngine(const Engine: TSMEngine); virtual;
  public
    /// initialize the SpiderMonkey scripting engine
    constructor Create; virtual;
    /// finalize the SpiderMonkey scripting engine
    destructor Destroy; override;

    /// get or create one Engine associated with current running thread
    // - in single thread application will return the MainEngine
    function ThreadSafeEngine: TSMEngine;
    /// method to be called when a thread is about to be finished
    // - you can call this method just before a thread is finished to ensure
    // that the associated scripting Engine will be released
    // - could be used e.g. in a try...finally block inside a TThread.Execute
    // overriden method
    procedure ReleaseCurrentThreadEngine;

    /// internal version of the script files
    // - used in TSMEngine.ThreadSafeEngine to determine if context is up to
    // date, in order to trigger on-the-fly reload of scripts without the need
    // if restarting the application
    property ContentVersion: Cardinal read FContentVersion write FContentVersion;
    /// lock/mutex used for thread-safe access to the TSMEngine list
    property Lock: TRTLCriticalSection read FEngineCS;
  published
    /// max amount of memory (in bytes) for a single SpiderMonkey instance
    // - this parameter will be set only at Engine start, i.e. it  must be set
    // BEFORE any call to ThreadSafeEngine
    // - default is 8 MB
    property MaxPerEngineMemory: Cardinal read FMaxPerEngineMemory write SetMaxPerEngineMemory
      default 8*1024*1024;
    /// maximum expected recursion depth for JavaScript functions
    // - to avoid out of memory situation in functions like
    // ! function f(){ f() };
    // - default is 32, but you can specify some higher value
    property MaxRecursionDepth: Cardinal read FMaxRecursionDepth write FMaxRecursionDepth
      default 32;

    /// event triggered every time a new Engine is created
    // - here your code can change the initial state of the Engine
    property OnNewEngine: TEngineEvent read FOnNewEngine write FOnNewEngine;
  end;
  {$M-}


var
  /// the internal custom variant type used to register TSMVariant
  SMVariantType: TSynInvokeableVariantType = nil;

type
  /// pointer to a TSMVariant storage
  PSMVariantData = ^TSMVariantData;

  /// a custom variant type used to store a SpiderMonkey object in Delphi code
  // - via the magic of late binding, it will allow access of any JavaScript
  // object property, or execute any of its methods
  // - primitive types (i.e. null, string, or numbers) will be stored as
  // simple variant instances, but JavaScript objects (i.e. objects, prototypes
  // or functions) can be stored as an instance of this TSMVariant custom type
  // - you can use the _Root and _UnRoot pseudo-methods, which will protect
  // the object instance to avoid unexpected Garbage Collection
  TSMVariant = class(TSynInvokeableVariantType)
  protected
    /// fast getter/setter implementation of object properties
    function IntGet(var Dest: TVarData; const Instance: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
    function IntSet(const Instance, Value: TVarData;
      Name: PAnsiChar; NameLen: PtrInt): boolean; override;
  public
    /// initialize a variant instance to store a JavaScript object
    class procedure New(const aObject: TSMObject; out aValue: variant); overload;
    /// initialize a variant instance to store a JavaScript object
    class procedure New(cx: PJSContext; obj: PJSObject; out aValue: variant); overload;
    /// initialize a variant instance to store a new JavaScript object
    class procedure New(engine: TSMEngine; out aValue: variant); overload;
    // this implementation will let SpiderMonkey write directly the JSON content
    procedure ToJSON(W: TTextWriter; const Value: variant; Escape: TTextWriterKind); override;
    /// handle type conversion
    // - any TSMVariant will be converted to '<<JavaScript TSMVariant>>' text
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - any TSMVariant will be converted to '<<JavaScript TSMVariant>>' text
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// low-level callback to execute any JavaScript object method
    // - add the _(Index: integer): variant method to retrieve an item
    // if the object is an array
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
  end;

  {$A-} { packet object not allowed since Delphi 2009 :( }
  /// memory structure used for TSMVariant storage of any JavaScript object
  // as Delphi variant
  // - primitive types (i.e. null, string, or numbers) will be stored as
  // simple variant instances, but JavaScript objects (i.e. objects, prototypes
  // or functions) can be stored as an instance of this TSMVariant custom type
  // - this variant stores its execution context, so is pretty convenient to
  // work with in plain Delphi code, also thanks to late-binding feature
  {$ifdef UNICODE}
  TSMVariantData = record
  private
  {$else}
  TSMVariantData = object
  protected
  {$endif}
    VType: TVarType;
    {$IFDEF FPC} {$PUSH} {$ENDIF} {$HINTS OFF}
    // does not complain if Filler is declared but never used
    Filler: array[1..SizeOf(TVarData)-SizeOf(TVarType)-SizeOf(TSMObject)] of byte;
    {$IFDEF FPC} {$POP} {$ELSE} {$HINTS ON} {$ENDIF}
    VObject: TSMObject;
  public
    /// initialize a TSMVariant structure to store a specified JavaScript object
    procedure Init(const aObject: TSMObject); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// initialize a TSMVariant structure to store a specified JavaScript object
    procedure Init(aCx: PJSContext; aObj: PJSObject); overload;
    /// initialize a TSMVariant structure to store a new JavaScript object
    procedure InitNew(engine: TSMEngine);
    /// retrieve the global object of this execution context
    // - you can use this from a native function, e.g.:
    //!function TMyClass.MyFunction(const This: variant; const Args: array of variant): variant;
    //!var global: variant;
    //!begin
    //!  TSMVariantData(This).GetGlobal(global);
    //!  global.anotherFunction(Args[0],Args[1],'test');
    //!  // same as:
    //!  global := TSMVariantData(This).SMObject.Engine.Global;
    //!  global.anotherFunction(Args[0],Args[1],'test');
    //!  // but you may also write directly:
    //!  with TSMVariantData(This).SMObject.Engine do
    //!    Global.anotherFunction(Args[0],Args[1],'test');
    //!  result := AnyTextFileToSynUnicode(Args[0]);
    //!end;
    procedure GetGlobal(out global: variant);

    /// return the custom variant type identifier, i.e. SMVariantType.VarType
    property VarType: word read VType;
    /// returns the associated TSMObject instance
    property SMObject: TSMObject read VObject;
    /// returns the associated execution context
    property cx: PJSContext read VObject.fcx;
    /// returns the associated jsobject instance
    property obj: PJSObject read VObject.fobj;
  end;
  {$A+}


/// to be used to catch Delphi exceptions inside JSNative function implementation
// - usage example:
// ! try
// !   doSomething()
// !   Result := JS_TRUE;
// ! except
// ! on E: Exception do begin
// !   JS_SET_RVAL(cx, vp, JSVAL_VOID);
// !   JSError(cx, E);
// !   Result := JS_FALSE;
// ! end;
procedure JSError(cx: PJSContext; aException: Exception; const aContext: RawByteString='');

/// convert a variant to a Java Script value
function VariantToJSVal(cx: PJSContext; const Value: Variant): jsval;

var
  /// define the TSynLog class used for logging for all our SynSM related units
  // - you may override it with TSQLLog, if available from mORMot.pas
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynSMLog: TSynLogClass=TSynLog;

implementation

uses
  Math;

const
  jsglobal_class: JSClass = (name: 'global';
    flags: JSCLASS_HAS_PRIVATE or JSCLASS_GLOBAL_FLAGS { or JSCLASS_NEW_RESOLVE };
    addProperty: JS_PropertyStub;
    delProperty: JS_DeletePropertyStub;
    getProperty: JS_PropertyStub;
    setProperty: JS_StrictPropertyStub;
    enumerate: @JS_EnumerateStub;
    resolve: JS_ResolveStub;
    convert: JS_ConvertStub    //finalize is Optionally non-null member
    //in source it marked as Mandatory, but it doesn't
    //use in tests and there is no exported function JS_FinalizeStub
    );

/// handle errors from JavaScript. Just call DoProcessJSError of corresponding TSMEngine
// to set TSMEngine error properties
procedure ErrorReporter(cx: PJSContext; pErrMsg: PCChar; report: PJSErrorReport); cdecl;
begin
  TSMEngine(cx.PrivateData).DoProcessJSError(pErrMsg, report)
end;

procedure JSError(cx: PJSContext; aException: Exception;
  const aContext: RawByteString);
begin
  if JS_IsExceptionPending(cx)=JS_FALSE then
    // raise only if this is the first exception in chain
    if aException is EOutOfMemory then
      JS_ReportOutOfMemory(cx) else
      JS_ReportError(cx, PCchar(AnsiString(aException.Message)+AnsiString(aContext)));
end;

function OperationCallback(cx: PJSContext): JSBool; cdecl;
begin
  Result := TSMEngine(cx.PrivateData).DoProcessOperationCallback;
end;


{ TSMEngine }

// do nothing here
procedure ExternalStringFinalizer(fin: PJSStringFinalizer; chars: Pjschar); cdecl;
begin
  {}
end;

constructor TSMEngine.Create(aManager: TSMEngineManager);
const
  Opt: CompartmentOptions =
    (
      zoneSpec: zsFreshZone;
      hasVersion: True;
      version: JSVERSION_LATEST;
    );
  gMaxStackSize = 128 * sizeof(size_t) * 1024;
begin
  if aManager = nil then
    raise ESMException.CreateUTF8('%.Create(nil): No manager provided',[self]);
  FDefaultPropertyAttrs := [jspEnumerate];
  fNativeMethods.Init(TypeInfo(TSMEngineMethodEventDynArray),
    fNativeMethod,HashPointer,SortDynArrayPointer,nil,@fNativeMethodCount);

  {$ifdef RESETFPUEXCEPTION}
  TSynFPUException.ForLibraryCode;
  {$endif}
  FManager := aManager;
  FEngineContentVersion := FManager.ContentVersion;
  frt := JS_NewRuntime(FManager.MaxPerEngineMemory, JS_USE_HELPER_THREADS);
  if frt = nil then
    raise ESMException.CreateUTF8('%.Create runtime: out of memory',[self]);
  JS_SetNativeStackQuota(rt, gMaxStackSize);

  JS_SetGCParameter(frt, JSGC_MAX_BYTES, FManager.MaxPerEngineMemory);
  JS_SetGCParameter(frt, JSGC_MAX_MALLOC_BYTES, FManager.MaxPerEngineMemory div 2);
  JS_SetGCParameter(frt, JSGC_MODE, uint32(JSGC_MODE_INCREMENTAL) );

  fCx := JS_NewContext(rt, STACK_CHUNK_SIZE);
  if fCx = nil then
    raise ESMException.CreateUTF8('%.Create: JS_NewContext failure',[self]);

  // You must set jsoBaseLine,jsoTypeInference,jsoIon for the enabling ION
  // ION is disabled without these options
  {$ifdef FIXBUGXE3}
  fCx.SetOptions([jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs]);
  {$else}
  fCx.Options := [jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs];
  {$endif}

  fStringFinalizer.finalize := ExternalStringFinalizer;
  JS_SetContextPrivate(cx, self);
  JS_SetErrorReporter(cx, ErrorReporter);

  FGlobalObject.fCx := cx;
  FGlobalObject.fObj := JS_NewGlobalObject(cx, @jsglobal_class, nil, @Opt);
  if GlobalObj = nil then
    raise ESMException.CreateUTF8('%.Create: JS_NewGlobalObject failure',[self]);
  fcomp := fcomp.EnterCompartment(cx,GlobalObj);
  if JS_InitStandardClasses(cx, GlobalObj)<>JS_TRUE then
    raise ESMException.CreateUTF8('%.Create: JS_InitStandardClasses failure',[self]);
  FGlobalObject.DefineProperty('global', GlobalObject.AsSMValue,
     [jspEnumerate,jspPermanent,jspReadOnly]);
  TSMVariantData(FGlobal).Init(FGlobalObject);
  fTimeoutInterval := -1;
  if not InitWatchdog then
    raise ESMException.CreateUTF8('%.Create: InitWatchDog failure',[self]);
  JS_SetOperationCallback(cx, OperationCallback);
end;

destructor TSMEngine.Destroy;
begin
  inherited Destroy;
  VarClear(FGlobal);
  {$ifdef RESETFPUEXCEPTION}
  TSynFPUException.ForLibraryCode;
  {$endif}
  //JS_RemoveExternalStringFinalizer(ExternalStringFinalizer);
//  comp^.Destroy;
  JS_LeaveCompartment(cx, comp);
  if FThreadID=GetCurrentThreadId then
    cx^.Destroy; // SM 24 expects the context to be released in the same thread
  KillWatchdog;
  rt^.Destroy;
end;

procedure TSMEngine.DoProcessJSError(errMsg: PCChar; report: PJSErrorReport);
const PATTERN: PUTF8Char = '[JSError %] % (%): %';
var exc: jsval;
  pExObj: PJSObject;
  msg: SynUnicode;
begin
  FErrorExist := True;
  if report^.filename = nil then
    FLastErrorFileName := '(w/o name)' else
    FLastErrorFileName := CurrentAnsiConvert.AnsiBufferToRawUTF8(
      report^.filename,StrLen(pointer(report^.filename)));
  FLastErrorLine := report^.lineno;
  if report^.ucmessage=nil then
    FLastErrorMsg := FormatUTF8(PATTERN,[report^.errorNumber,FLastErrorFileName,
      FLastErrorLine,errMsg]) else
    FLastErrorMsg := FormatUTF8(PATTERN,[report^.errorNumber,FLastErrorFileName,
      FLastErrorLine,PWideChar(report^.ucmessage)]);
  FLastErrorStackTrace := '';
  if ( JS_GetPendingException(cx, exc) = JS_TRUE ) then begin
    if JSVAL_IS_OBJECT(exc) then begin
      pExObj := JSVAL_TO_OBJECT(exc);
      JS_GetProperty(cx, pExObj, 'stack', exc);
      if (exc <> JSVAL_VOID) and JSVAL_IS_STRING(exc) then begin
        msg := JSVAL_TO_STRING(exc).ToSynUnicode(cx);
        if msg <> '' then // this can happend in case syntax error while parse script
          FLastErrorStackTrace := SynUnicodeToUtf8(msg);
      end;
    end;
  end;
  (*
  // This situation is possible when application are run from the IDE
  // and stop on the breakpoint.
  // When we evaluate some js script with errors(like call JS_Stringify
  // for global object) this function will be called.
  // If breakpoint is set between ClearLastError and CheckJSError we get
  // FErrorExist value is equivalent true, but script have no error
  if DebugHook=0 then try
    CheckJSError(JS_FALSE);
  finally
    FErrorExist := false;
  end;
  *)
end;

procedure TSMEngine.CheckJSError(res: JSBool);
begin
  if FTimeOutAborted then
    raise ESMException.CreateUTF8('%: script runs for too long, abort',[self]);
  if FErrorExist then begin
    SynSMLog.Add.Log(sllError, FLastErrorMsg);
    raise ESMException.CreateUTF8('% error: %',[self,FLastErrorMsg]);
  end;
  if res=JS_FALSE then begin
    SynSMLog.Add.Log(sllError, 'Error compiling script %', FLastErrorFileName);
    raise ESMException.CreateUTF8('%: Error compiling script [%]. Line %',
      [self,FLastErrorFileName,FLastErrorLine]);
  end;
end;

procedure TSMEngine.ClearLastError;
begin
  JS_ClearPendingException(cx);
  FErrorExist := False;
  FTimeOutAborted := False;
end;

procedure TSMEngine.GarbageCollect;
begin
  JS_GC(rt);
end;

procedure TSMEngine.MaybeGarbageCollect;
begin
  JS_MaybeGC(cx);
end;

procedure TSMEngine.NewObject(out newobj: TSMObject);
begin
  newobj.fCx := cx;
  newobj.fObj := JS_NewObject(cx, nil{class}, nil{proto}, globalObj{parent});
  newobj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  if newobj.fObj=nil then
    raise ESMException.CreateUTF8('%.NewObject',[self]);
end;

procedure TSMEngine.NewObject(const prototype: TSMObject; out newobj: TSMObject);
begin
  newobj.fCx := cx;
  newobj.fObj := JS_NewObject(cx, nil{class}, prototype.obj, nil{parent});
  newobj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  if newobj.fObj=nil then
    raise ESMException.CreateUTF8('%.NewObject(prototype)',[self]);
end;

procedure TSMEngine.NewObjectWithClass(clasp: PJSClass; var newobj: TSMObject);
begin
  newobj.fCx := cx;
  newobj.fObj := JS_NewObject(cx, clasp, nil, nil);
  newobj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  if newobj.fObj=nil then
    raise ESMException.CreateUTF8('%.NewObjectWithClass',[self]);
end;

procedure TSMEngine.NewObjectWithClass(clasp: PJSClass; const prototype: TSMObject;
  const parent: TSMObject; var newobj: TSMObject);
begin
  newobj.fCx := cx;
  newobj.fObj := JS_NewObject(cx, clasp, prototype.obj, parent.obj);
  newobj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  if newobj.fObj=nil then
    raise ESMException.CreateUTF8('%.NewObjectWithClass(parent)',[self]);
end;

procedure TSMEngine.InitClass(clasp: PJSClass; ps: PJSPropertySpec; var newobj: TSMObject);
begin
  newobj.fCx := cx;
  newobj.fObj :=
    JS_InitClass(cx, GlobalObj, nil, clasp, nil, 0, ps , nil, nil, nil);
  newobj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  if newobj.obj=nil then
    raise ESMException.CreateUTF8('%.InitClass',[self]);
end;

procedure TSMEngine.MakeObject(const value: TSMValue; out obj: TSMObject);
begin
  MakeObject(value.FValue,obj);
end;

procedure TSMEngine.MakeObject(const value: jsval; out obj: TSMObject);
begin
  if JSVAL_IS_OBJECT(value) then begin
    obj.fCx := cx;
    obj.fObj := JSVAL_TO_OBJECT(value);
    obj.FDefaultPropertyAttrs := self.FDefaultPropertyAttrs;
  end else
    raise ESMException.CreateUTF8('%.MakeObject(value: not an object)',[self]);
end;

procedure TSMEngine.MakeObject(jsobj: PJSObject; out obj: TSMObject);
begin
  obj.fCx := cx;
  obj.fObj := jsobj;
end;

function TSMEngine.NewSMVariant: variant;
begin
  TSMVariant.New(self,result);
end;

procedure TSMEngine.NewSMVariantRooted(out newobj: variant);
begin
  TSMVariant.New(self,newobj);
  TSMVariantData(newobj).VObject.Root;
end;

function TSMEngine.Evaluate(const script: SynUnicode; const scriptName: RawUTF8;
  lineNo: Cardinal): variant;
var res: TSMValue;
begin
  globalObject.Evaluate(script, scriptName, lineNo, res);
  res.ToVariant(cx,result);
end;

function nsm_methodDelphi(cx: PJSContext; argc: uintN; vp: Pjsval): JSBool; cdecl;
var engine: TSMEngine;
    argv: PSMValues;
    f: PJSFunction;
    instance: TSMVariantData;
    callee,res: TSMValue;
    method: integer;
  procedure RunAsVariant(const CallbackVariant: TSMEngineMethodEventVariant);
  var a: integer;
      Args: TVariantDynArray;
  begin
    SetLength(Args,argc);
    for a := 0 to argc-1 do
      argv^[a].ToVariant(cx,Args[a]);
    res.SetVariant(cx,CallbackVariant(Variant(instance),Args));
  end;
  procedure RunAsJson(const CallbackJSON: TSMEngineMethodEventJSON);
  var a: integer;
      W: TTextWriter;
  begin
    W := TTextWriter.CreateOwnedStream(4096);
    try
      W.Add('[');
      for a := 0 to argc-1 do begin
        argv^[a].AddJSON(cx,W);
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add(']');
      res.SetJSON(cx,CallbackJSON(instance.SMObject,W.Text));
    finally
      W.Free;
    end;
  end;
  procedure RunError(E: Exception);
  begin // avoid temporary allocation of strings on the stack
    JSError(cx, E, FormatUTF8(' for function %()',
      [callee.ToNativeFunctionName(cx)]));
  end;
begin
  {$ifdef RESETFPUEXCEPTION}
  TSynFPUException.ForDelphiCode; // ensure we are back in Delphi FPU mask
  {$endif}
  try
    engine := cx.PrivateData;
    callee.FValue := vp^;
    f := callee.ToNativeFunction(cx);
    if f=nil then
      method := -1 else
      method := engine.fNativeMethods.FindHashed(f);
    if method<0 then
      raise ESMException.Create('nsm_methodDelphi: No callback defined');
    argv := pointer(JS_ARGV(cx,vp));
    instance.Init(cx,JS_THIS_OBJECT(cx,vp));
    with engine.fNativeMethod[method] do
      case EventKind of
      meVariant: RunAsVariant(CallbackVariant);
      meJSON:    RunAsJson(CallbackJSON);
      else raise ESMException.CreateUTF8('nsm_methodDelphi: Unknown EventKind=%',
        [ord(EventKind)]);
      end;
    JS_SET_RVAL(cx,vp,res.FValue);
    result := JS_TRUE;
  except
    on E: Exception do begin
      RunError(E);
      JS_SET_RVAL(cx,vp,JSVAL_VOID);
      result := JS_FALSE;
    end;
  end;
end;

function TSMEngine.InternalRegisterMethod(obj: PJSObject;
  const MethodName: SynUnicode; const Event: TMethod; Kind: TSMEngineMethodEventKind;
  ArgumentsCount: integer): PJSFunction;
var added: boolean;
    i: integer;
begin
  result := JS_DefineUCFunction(cx, obj, pointer(MethodName), Length(MethodName),
    nsm_methodDelphi, ArgumentsCount, JSPROP_ENUMERATE);
  if result=nil then
    raise ESMException.CreateUTF8(
      '%.InternalRegisterMethod(%): Defining native function',[self,MethodName]);
  i := fNativeMethods.FindHashedForAdding(result,added);
  if added then
  with fNativeMethod[i] do begin
    Func := result;
    EventKind := Kind;
    TMethod(CallbackVariant) := Event;
  end else
    raise ESMException.CreateUTF8(
      '%.InternalRegisterMethod(%): Duplicated name',[self,MethodName]);
end;

function TSMEngine.RegisterMethod(obj: PJSObject; const MethodName: SynUnicode;
  const Event: TSMEngineMethodEventVariant; ArgumentsCount: integer): PJSFunction;
begin
  result := InternalRegisterMethod(obj,MethodName,
    TMethod(Event),meVariant,ArgumentsCount);
end;

function TSMEngine.RegisterMethod(obj: PJSObject;
  const MethodName: SynUnicode; const Event: TSMEngineMethodEventJSON;
  ArgumentsCount: integer): PJSFunction;
begin
  result := InternalRegisterMethod(obj,MethodName,
    TMethod(Event),meJSON,ArgumentsCount);
end;

procedure TSMEngine.UnRegisterMethod(JSFunction: PJSFunction);
var i: integer;
begin
  i := fNativeMethods.FindHashed(JSFunction);
  if i<0 then
    raise ESMException.CreateUTF8(
      '%.UnRegisterMethod(%): Method not previously registered',[self,JSFunction]);
  fNativeMethods.Delete(i);
end;


{ TSMEngineManager }

constructor TSMEngineManager.Create;
begin
  FMaxPerEngineMemory := 8*1024*1024;
  FMaxRecursionDepth := 32;
  FEnginePool := TObjectList.Create;
  InitializeCriticalSection(fEngineCS);
end;

procedure TSMEngineManager.SetMaxPerEngineMemory(AMaxMem: Cardinal);
begin
  if aMaxMem<STACK_CHUNK_SIZE*MaxRecursionDepth then
    raise ESMException.CreateUTF8(
      '%.MaxPerEngineMemory := %, but must be >= STACK_CHUNK_SIZE*%, i.e. %',
      [self,aMaxMem,MaxRecursionDepth,STACK_CHUNK_SIZE*MaxRecursionDepth]);
  FMaxPerEngineMemory := AMaxMem;
end;

function TSMEngineManager.ThreadEngineIndex(ThreadID: TThreadID): Integer;
begin
  if self<>nil then
    for result := 0 to FEnginePool.Count-1 do
      if TSMEngine(FEnginePool.List[result]).fThreadID=ThreadID then
        exit;
  result := -1;
end;

destructor TSMEngineManager.Destroy;
begin
  FEnginePool.Free;
  inherited;
  DeleteCriticalSection(fEngineCS);
end;

procedure TSMEngineManager.DoOnNewEngine(const Engine: TSMEngine);
begin
  if Assigned(FOnNewEngine) then
     FOnNewEngine(Engine);
end;

function TSMEngineManager.ThreadSafeEngine: TSMEngine;
var i: integer;
    ThreadID: TThreadID;
begin
  EnterCriticalSection(fEngineCS);
  try
    ThreadID := GetCurrentThreadId;
    i := ThreadEngineIndex(ThreadID); // inlined CurrentThreadEngine
    if i<0 then
      result := nil else
      result := FEnginePool.List[i];
    if result<>nil then
      if result.EngineContentVersion=Self.ContentVersion then
        // return existing Engine corresponding to the current thread
        exit else begin
        // content version changed -> force recreate thread Engine
        {$ifdef SM_DEBUG}
        SynSMLog.Add.Log(sllDebug,
          'Drop SpiderMonkey Engine for thread % - modification found',ThreadID);
        {$endif}
        FEnginePool.Delete(i); // as in ReleaseCurrentThreadEngine
      end;
    // here result=nil or to be ignored (just dropped)
    {$ifdef SM_DEBUG}
    SynSMLog.Add.Log(sllDebug, 'Create new JavaScript Engine for thread %',ThreadID);
    {$endif}
    result := CreateNewEngine;
    result.fThreadID := ThreadID;
    FEnginePool.Add(result);
  finally
    LeaveCriticalSection(fEngineCS);
  end;
end;

procedure TSMEngineManager.ReleaseCurrentThreadEngine;
var
  i: integer;
begin
  EnterCriticalSection(fEngineCS);
  try
    i := ThreadEngineIndex(GetCurrentThreadId);
    if i>=0 then begin
      (FEnginePool[i] as TSMEngine).GarbageCollect;
      FEnginePool.Delete(i);
    end;
  finally
    LeaveCriticalSection(fEngineCS);
  end;
end;

function TSMEngineManager.CurrentThreadEngine: TSMEngine;
var
  i: integer;
begin
  EnterCriticalSection(fEngineCS);
  try
    i := ThreadEngineIndex(GetCurrentThreadId);
    if i < 0 then
      result := nil else
      result := FEnginePool.List[i];
  finally
    LeaveCriticalSection(fEngineCS);
  end;
end;

function TSMEngineManager.CreateNewEngine: TSMEngine;
begin
  Result := TSMEngine.Create(Self);
  if Assigned(FOnNewEngine) then begin
    {$ifdef JS_THREADSAFE}
    JS_BeginRequest(Result.cx);
    try
    {$endif}
    FOnNewEngine(Result);
    {$ifdef JS_THREADSAFE}
    finally
      JS_EndRequest(Result.cx);
    end;
    {$endif}
  end;
end;


{ TSMValue }

function VariantToJSVal(cx: PJSContext; const Value: Variant): jsval;
begin
  TSMValue(result).SetVariant(cx,Value);
end;

function TSMValue.ToInteger: integer;
begin
  {$ifndef WITHASSERT}
  if not JSVAL_IS_INT(FValue) then
    raise ESMException.Create('TSMValue.ToInteger!');
  {$endif}
  Result := JSVAL_TO_INT(FValue);
end;

procedure TSMValue.SetInteger(const Value: integer);
begin
  FValue := INT_TO_JSVAL(Value);
end;

function TSMValue.ToDouble: double;
begin
  {$ifndef WITHASSERT}
  if not JSVAL_IS_DOUBLE(FValue) then
    raise ESMException.Create('TSMValue.ToDouble!');
  {$endif}
  Result := JSVAL_TO_DOUBLE(FValue);
end;

procedure TSMValue.SetDouble(const Value: double);
begin
  FValue := DOUBLE_TO_JSVAL(Value);
end;

function TSMValue.ToBoolean: boolean;
begin
  Result := (FValue=JSVAL_TRUE);
end;

procedure TSMValue.SetBoolean(const Value: boolean);
begin
  if Value then
    FValue := JSVAL_TRUE else
    FValue := JSVAL_FALSE;
end;

function TSMValue.ToInt64: int64;
begin
  if JSVAL_IS_INT(FValue) then
    result := JSVAL_TO_INT(FValue) else
  {$ifndef WITHASSERT}
  if not JSVAL_IS_DOUBLE(FValue) then
    raise ESMException.Create('TSMValue.ToInt64!') else
  {$endif}
    result := trunc(JSVAL_TO_DOUBLE(FValue));
end;

procedure TSMValue.SetInt64(const Value: int64);
begin
  if (Value>=Low(integer)) and (Value<=High(integer)) then
    FValue := INT_TO_JSVAL(Value) else
    FValue := DOUBLE_TO_JSVAL(Value);
end;

function TSMValue.ValType(cx: PJSContext): JSType;
begin
  Result := JS_TypeOfValue(cx, FValue);
end;

function TSMValue.ToVariant(cx: PJSContext): Variant;
begin
  ToVariant(cx,result);
end;

procedure TSMValue.ToVariant(cx: PJSContext; var result: Variant);
begin
  case ValType(cx) of
    JSTYPE_VOID:
      VarClear(result);
    JSTYPE_NULL:
      SetVariantNull(result);
    JSTYPE_OBJECT:
      TSMVariant.New(cx,JSVAL_TO_OBJECT(FValue),result);
    JSTYPE_STRING:
      JSVAL_TO_STRING(FValue).ToVariant(cx,result);
    JSTYPE_NUMBER:
      if JSVAL_IS_INT(FValue) then
        result := JSVAL_TO_INT(FValue) else
        result := JSVAL_TO_DOUBLE(FValue);
    JSTYPE_BOOLEAN:
      result := JSVAL_TO_BOOLEAN(FValue)=JS_TRUE;
    JSTYPE_FUNCTION:
      result := TransformToSynUnicode(cx);
    else
      raise ESMException.CreateUTF8('Unhandled ToVariant(%)',[ord(ValType(cx))]);
  end;
end;

procedure TSMValue.SetVariant(cx: PJSContext; const Value: Variant);
var CustomVariantType: TCustomVariantType;
begin
  with TVarData(Value) do
  case VType of
  varNull:
    FValue := JSVAL_NULL;
  varEmpty:
    FValue := JSVAL_VOID;
  varBoolean:
    if VBoolean then
      FValue := JSVAL_TRUE else
      FValue := JSVAL_FALSE;
  varSmallint:
    FValue := INT_TO_JSVAL(VSmallInt);
  {$ifndef DELPHI5OROLDER}
  varShortInt:
    FValue := INT_TO_JSVAL(VShortInt);
  varWord:
    FValue := INT_TO_JSVAL(VWord);
  varLongWord:
    if VLongWord<=cardinal(high(Integer)) then
      FValue := INT_TO_JSVAL(VLongWord) else
      FValue := DOUBLE_TO_JSVAL(VLongWord);
  {$endif}
  varByte:
    FValue := INT_TO_JSVAL(VByte);
  varInteger:
    FValue := INT_TO_JSVAL(VInteger);
  varInt64:
    SetInt64(VInt64);
  varSingle:
    FValue := DOUBLE_TO_JSVAL(VSingle);
  varDouble:
    FValue := DOUBLE_TO_JSVAL(VDouble);
  varCurrency:
    FValue := DOUBLE_TO_JSVAL(VCurrency);
  varDate:
    SetDateTime(cx,VDate);
  varOleStr:
    SetWideString(cx,WideString(VAny));
  varString:
    SetAnsiChar(cx,VAny,length(RawByteString(VAny)),
  {$ifndef HASVARUSTRING} CP_UTF8);
  {$else}                 StringCodePage(RawByteString(VAny)));
  varUString:
    SetSynUnicode(cx,UnicodeString(VAny));
  {$endif}
  else
  if VType=varByRef or varVariant then
    SetVariant(cx,PVariant(VPointer)^) else
  if VType=varByRef or varOleStr then
    SetWideString(cx,PWideString(VAny)^) else
  {$ifdef HASVARUSTRING}
  if VType=varByRef or varUString then
    SetSynUnicode(cx,PUnicodeString(VAny)^) else
  {$endif}
  if (SMVariantType<>nil) and (VType=SMVariantType.VarType) then
    FValue := OBJECT_TO_JSVAL(TSMVariantData(Value).obj) else
  if FindCustomVariantType(VType,CustomVariantType) and
     CustomVariantType.InheritsFrom(TSynInvokeableVariantType) then
       SetJSON(cx,VariantSaveJSON(Value)) else
    raise ESMException.CreateUTF8('Unhandled variant type %',[VType]);
  end;
end;

procedure TSMValue.SetTVarRec(cx: PJSContext; const V: TVarRec);
begin
  case V.VType of
    vtPointer:
      FValue := JSVAL_VOID;
    vtBoolean:
      if V.VBoolean then
        FValue := JSVAL_TRUE else
        FValue := JSVAL_FALSE;
    vtInteger:
      FValue := INT_TO_JSVAL(V.VInteger);
    vtInt64{$ifdef FPC},vtQWord{$endif}:
      SetInt64(V.VInt64^);
    vtCurrency:
      FValue := DOUBLE_TO_JSVAL(V.VCurrency^);
    vtExtended:
      FValue := DOUBLE_TO_JSVAL(V.VExtended^);
    vtVariant:
      SetVariant(cx,V.VVariant^);
    vtWideString:
      SetWideString(cx,WideString(V.VPointer));
    vtAnsiString:
      SetAnsiChar(cx,V.VPointer,length(RawByteString(V.VAnsiString)),
{$ifndef HASCODEPAGE} CP_UTF8);
{$else}               StringCodePage(RawByteString(V.VAnsiString)));
    vtUnicodeString:
      SetSynUnicode(cx,UnicodeString(V.VPointer));
{$endif}
    vtString:
      SetAnsiChar(cx,PAnsiChar(@V.VString^[1]),ord(V.VString^[0]),0);
    vtPChar:
      SetAnsiChar(cx,V.VPChar,StrLen(V.VPointer),0);
    vtChar:
      SetAnsiChar(cx,@V.VChar,1,0);
    vtWideChar:
      FValue := STRING_TO_JSVAL(cx^.NewJSString(PWideChar(@V.VWideChar),1));
    else raise ESMException.CreateUTF8('Unhandled TVarRec.VType=%',[V.VType]);
  end;
end;

function TSMValue.ToSynUnicode(cx: PJSContext): SynUnicode;
begin
  ToSynUnicode(cx,result);
end;

procedure TSMValue.ToSynUnicode(cx: PJSContext; var result: SynUnicode);
begin
  Result := JSVAL_TO_STRING(FValue).ToSynUnicode(cx);
end;

procedure TSMValue.SetSynUnicode(cx: PJSContext; const aStr: SynUnicode);
begin
  SetWideChar(cx,pointer(aStr),length(aStr));
end;

function TSMValue.ToWideString(cx: PJSContext): WideString;
begin
  Result := JSVAL_TO_STRING(FValue).ToWideString(cx);
end;

procedure TSMValue.SetWideString(cx: PJSContext; const aStr: WideString);
begin
  SetWideChar(cx,pointer(aStr),length(aStr));
end;

procedure TSMValue.SetWideChar(cx: PJSContext; Text: PWideChar; TextLen: integer);
begin
  if (Text=nil) or (TextLen=0) then
    FValue := JS_GetEmptyStringValue(cx) else
    FValue := STRING_TO_JSVAL(cx^.NewJSString(Text,TextLen));
end;

procedure TSMValue.SetAnsiChar(cx: PJSContext; Text: PAnsiChar; TextLen,
  CodePage: integer);
begin
  if (Text=nil) or (TextLen=0) then
    FValue := JS_GetEmptyStringValue(cx) else
    FValue := STRING_TO_JSVAL(cx^.NewJSString(Text,TextLen,CodePage));
end;

function TSMValue.ToUTF8(cx: PJSContext): RawUTF8;
begin
  Result := JSVAL_TO_STRING(FValue).ToUTF8(cx);
end;

procedure TSMValue.SetUTF8(cx: PJSContext; const aStr: RawUTF8);
begin
  FValue := STRING_TO_JSVAL(cx^.NewJSString(aStr));
end;

procedure TSMValue.SetNativeString(cx: PJSContext; const aStr: SynUnicode);
begin
  FValue := STRING_TO_JSVAL(JS_NewExternalString(cx,
    pointer(aStr), length(aStr), @TSMEngine(cx.PrivateData).FStringFinalizer));
end;

function TSMValue.ToDateTime(cx: PJSContext): TDateTime;
var oDate: PJSObject;
{$ifdef CONSIDER_TIME_IN_Z} // as defined in SynSM.inc
    ms: double;
    ms64: Int64;
    fval: jsval;
{$else}
    d, m, Y, h, mn, s, ml: Integer;
    v, fval: jsval;
  function GetIntFuncPropVal(funcName: PWideChar): Integer;
  begin
    Result := 0;
    if JS_GetUCProperty(cx, oDate, pointer(funcName), Length(funcName), fval) = JS_TRUE then
      if JS_CallFunctionValue(cx, oDate, fval, 0, nil, v)  = JS_TRUE then
        Result := JSVAL_TO_INT(v);
  end;
{$endif}
begin
  oDate := JSVAL_TO_OBJECT(FValue);
  if JS_ObjectIsDate(cx, oDate) = JS_FALSE then
    raise ESMException.Create('TSMValue.ToDateTime: not a DateTime object');
{$ifdef CONSIDER_TIME_IN_Z}
  ms := 0;
  if JS_CallFunctionName(cx, oDate, PCChar('getTime'), 0, nil, fval) = JS_TRUE then
    ms := JSVAL_TO_DOUBLE(fval);
  if ms = 0 then
    raise ESMException.Create('TSMValue.ToDateTime: no getTime() in Date object');
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

procedure TSMValue.SetDateTime(cx: PJSContext; const Value: TDateTime);
var dmsec: double;
    unixTime: Int64;
  {$ifdef CONSIDER_TIME_IN_Z} // as defined in SynSM.inc
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
  oDate := JS_NewDateObjectMsec(cx, dmsec);
  if JS_ObjectIsDate(cx, oDate)<>JS_TRUE then
    raise ESMException.CreateUTF8('TSMValue.SetDateTime(%): not a valid date',[Value]);
  FValue := oDate.ToJSValue;
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
  FValue := JS_NewDateObjectMsec(cx, dmsec).ToJSValue;
{$endif}
end;

function TSMValue.TransformToSynUnicode(cx: PJSContext): SynUnicode;
begin
  Result := JS_ValueToString(cx, FValue).ToSynUnicode(cx);
end;

function TSMValue.TransformToUTF8(cx: PJSContext): RawUTF8;
begin
  Result := JS_ValueToString(cx, FValue).ToUTF8(cx);
end;

function TSMValue.ToNativeFunction(cx: PJSContext): PJSFunction;
begin
  if (not JSVAL_IS_OBJECT(FValue)) or
    (JS_ObjectIsFunction(cx,JSVAL_TO_OBJECT(FValue))=JS_FALSE) then
    result := nil else
    result := JS_ValueToFunction(cx,FValue);
end;

function TSMValue.ToNativeFunctionName(cx: PJSContext): RawUTF8;
var str: PJSString;
    buf,name: PWideChar;
    len: size_t;
begin
  result := '';
  if (@self=nil) or (not JSVAL_IS_OBJECT(FValue)) or
    (JS_ObjectIsFunction(cx,JSVAL_TO_OBJECT(FValue))=JS_FALSE) then
    exit;
  str := JS_ValueToString(cx, FValue);
  if str=nil then
    exit;
  buf := PWideChar(JS_GetStringCharsAndLength(cx, str, len));
  if (len<10) or not IdemPCharW(buf,'FUNCTION ') then
    exit;
  dec(len,9);
  inc(buf,9);
  name := buf;
  while (len>0) and (buf^<>'(') do begin
    dec(len);
    inc(buf);
  end;
  RawUnicodeToUtf8(name,buf-name,result);
end;

function writeCallback(const buf: Pjschar; len: uint32; data: pointer): JSBool; cdecl;
begin
  TTextWriter(data).AddNoJSONEscapeW(pointer(buf),len);
  result := JS_TRUE;
end;

procedure TSMValue.AddJSON(cx: PJSContext; W: TTextWriter);
begin
  if @self=nil then
    W.AddShort('null') else
  case ValType(cx) of
    JSTYPE_VOID,
    JSTYPE_NULL:
      W.AddShort('null');
    JSTYPE_STRING:
      JSVAL_TO_STRING(FValue).ToJSONString(cx,W);
    JSTYPE_NUMBER:
      if JSVAL_IS_INT(FValue) then
        W.Add(JSVAL_TO_INT(FValue)) else
        W.AddDouble(JSVAL_TO_DOUBLE(FValue));
    JSTYPE_BOOLEAN:
      W.Add(JSVAL_TO_BOOLEAN(FValue)=JS_TRUE);
    JSTYPE_OBJECT,
    JSTYPE_FUNCTION: begin
      if JS_Stringify(cx, @FValue, nil, JSVAL_NULL, writeCallback, pointer(W))<>JS_TRUE then begin
        TSMEngine(cx.PrivateData).CheckJSError(JS_FALSE);
        TSMEngine(cx.PrivateData).ClearLastError;
      end
    end
    else raise ESMException.CreateUTF8(
      'Unhandled TSMValue.AddJSON(%)',[ord(ValType(cx))]);
  end;
end;

function TSMValue.ToJSON(cx: PJSContext): RawUTF8;
var W: TJSONWriter;
    tmp: TTextWriterStackBuffer;
begin
  W := TJSONWriter.CreateOwnedStream(tmp);
  try
    AddJSON(cx,W);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TSMValue.SetJSON(cx: PJSContext; const aJSON: RawUTF8): boolean;
var tmp: RawUnicode;
    len: integer;
begin
  if aJSON='' then begin
    SetVoid;
    result := true;
  end else begin
    len := Utf8DecodeToRawUnicodeUI(aJSON,tmp);
    result := JS_ParseJSON(cx,pointer(tmp),len shr 1,@self)<>JS_FALSE;
  end;
end;

procedure TSMValue.SetNull;
begin
  FValue := JSVAL_NULL;
end;

procedure TSMValue.SetVoid;
begin
  FValue := JSVAL_VOID;
end;


{ TSMObject }

function TSMObject.Engine: TSMEngine;
begin
  if @self=nil then
    Result := nil else
    Result := TSMEngine(cx.PrivateData);
end;

function TSMObject.AsSMValue: TSMValue;
begin
  if (@self=nil) or (obj=nil) then
    Result.FValue := JSVAL_NULL else
    Result.FValue := OBJECT_TO_JSVAL(obj);
end;

procedure TSMObject.DefineProperty(const name: SynUnicode;
  const value: TSMValue; attrs: TJSPropertyAttrs);
begin
  if (@self=nil) or (cx=nil) or (obj=nil) or
     (JS_DefineUCProperty(cx, Obj, pointer(name), length(name),
      value.AsJSVal, nil, nil, word(attrs))<>JS_TRUE) then
    raise ESMException.CreateUTF8('TSMObject.DefineProperty(%)', [name]);
end;

procedure TSMObject.DefineProperty(const name: SynUnicode;
  const value: variant; attrs: TJSPropertyAttrs);
begin
  DefineProperty(name,TSMValue(VariantToJsVal(cx,value)),attrs);
end;

procedure TSMObject.DefineProperty(const name: SynUnicode;
  const value: variant);
begin
  DefineProperty(name, value, FDefaultPropertyAttrs);
end;

procedure TSMObject.SetPropVariant(const propName: SynUnicode;
  const Value: variant);
begin
  DefineProperty(propName,Value);
end;

function TSMObject.HasProperty(const propName: SynUnicode): Boolean;
var has: JSBool;
begin
  Result := (JS_HasUCProperty(cx, obj,
    pointer(propName), length(propName), has)=JS_TRUE) and (has=JS_TRUE);
end;

function TSMObject.HasOwnProperty(const propName: SynUnicode): Boolean;
var has: JSBool;
begin
  Result := (JS_AlreadyHasOwnUCProperty(cx, obj,
    pointer(propName), length(propName), has)=JS_TRUE) and (has=JS_TRUE);
end;

function TSMObject.GetPropValue(const propName: SynUnicode): TSMValue;
begin
  if JS_GetUCProperty(cx, obj,
      pointer(propName), length(propName), Result.FValue)=JS_FALSE then
    raise ESMException.CreateUTF8('TSMObject.GetPropValue(%)',[propName]);
end;

function TSMObject.GetPropVariant(const propName: SynUnicode): variant;
var res: TSMValue; // need a temp. var to compile with latest Delphi! :(
begin
  res := GetPropValue(propName);
  res.ToVariant(cx,result);
end;

procedure TSMObject.Evaluate(const script: SynUnicode; const scriptName: RawUTF8;
   lineNo: Cardinal; out result: TSMValue);
var r: JSBool;
    eng: TSMEngine;
begin
  {$ifdef RESETFPUEXCEPTION}
  TSynFPUException.ForLibraryCode;
  {$endif}
  eng := Engine;
  eng.ClearLastError;
  eng.ScheduleWatchdog(eng.fTimeoutInterval);
  r := JS_EvaluateUCScript(cx, obj,
     pointer(script), length(script), pointer(scriptName), lineNo, Result.FValue);
  eng.ScheduleWatchdog(-1);
  eng.CheckJSError(r);
end;

procedure TSMObject.RunMethod(const methodName: AnsiString;
  const argv: SMValArray; out rval: TSMValue);
var r: JSBool;
    eng: TSMEngine;
begin
  {$ifdef RESETFPUEXCEPTION}
  TSynFPUException.ForLibraryCode;
  {$endif}
  eng := Engine;
  eng.ClearLastError;
  eng.ScheduleWatchdog(Engine.fTimeoutInterval);
  r := JS_CallFunctionName(cx, obj, pointer(methodName),
    Length(argv), pointer(argv), rval.FValue);
  eng.ScheduleWatchdog(-1);
  eng.CheckJSError(r);
end;

procedure TSMObject.RunMethod(const methodName: AnsiString;
  const argv: array of const; out rval: TSMValue);
var args: SMValArray;
    a: integer;
begin
  SetLength(args,length(argv));
  for a := 0 to high(argv) do
    args[a].SetTVarRec(cx,argv[a]);
  RunMethod(methodName,args,rval);
end;

function TSMObject.Run(const methodName: AnsiString; const argv: array of variant): variant;
var args: SMValArray;
    a: integer;
    res: TSMValue;
begin
  SetLength(args,length(argv));
  for a := 0 to high(argv) do
    args[a].SetVariant(cx,argv[a]);
  RunMethod(methodName,args,res);
  res.ToVariant(cx,result);
end;

procedure TSMObject.Root;
begin
  if obj<>nil then
    JS_AddObjectRoot(cx, @obj);
end;

procedure TSMObject.UnRoot;
begin
  if obj<>nil then
    JS_RemoveObjectRoot(cx, @obj);
end;

function TSMObject.DefineNativeMethod(const methodName: SynUnicode; func: JSNative; nargs: uintN;
  attrs: TJSPropertyAttrs): PJSFunction;
begin
  Result := JS_DefineUCFunction(cx, obj,
      Pjschar(methodName), Length(methodName), func, nargs, word(attrs));
  if Result=nil then
    raise ESMException.CreateUTF8('TSMObject.DefineNativeMethod(%)',[methodName]);
end;

procedure TSMObject.Clear;
begin
  fCx := nil;
  fObj := nil;
end;

function TSMObject.DefineNativeMethod(const methodName: AnsiString; func: JSNative; nargs: uintN;
  attrs: TJSPropertyAttrs): PJSFunction;
begin
  Result := JS_DefineFunction(cx, obj,
      PCChar(methodName), func, nargs, word(attrs));
  if Result=nil then
    raise ESMException.CreateUTF8('TSMObject.DefineNativeMethod(%)',[methodName]);
end;

function TSMObject.DefineNativeMethod(const methodName: AnsiString;
  func: JSNative; nargs: uintN): PJSFunction;
begin
  result := DefineNativeMethod(methodName, func, nargs, DefaultPropertyAttrs);
end;

function TSMObject.DefineNativeMethod(const methodName: SynUnicode;
  func: JSNative; nargs: uintN): PJSFunction;
begin
  result := DefineNativeMethod(methodName, func, nargs, DefaultPropertyAttrs);
end;

procedure TSMObject.DefineProperty(const name: SynUnicode;
  const value: TSMValue);
begin
  DefineProperty(name, value, FDefaultPropertyAttrs);
end;

function TSMObject.Parent: TSMObject;
begin
  result.fCx := cx;
  if obj=nil then
    result.fObj := nil else
    result.fObj := JS_GetParent(obj);
end;

function TSMObject.Prototype: TSMObject;
begin
  result.fCx := cx;
  if obj=nil then
    result.fObj := nil else
    JS_GetPrototype(cx, obj, result.fObj);
end;

function TSMEngine.DoProcessOperationCallback: JSBool;
begin
  if fTimedOut then
    Result := JS_FALSE else
    Result := JS_TRUE;
end;

procedure TSMEngine.CancelExecution;
begin
  fTimedOut := True;
  FTimeOutAborted := True;
  FErrorExist := True;
  FLastErrorFileName := '(w/o name)';
  FLastErrorLine := 0;
  FLastErrorMsg := FormatUTF8('JSError. Filename: %. Line %. Message: %',
    [FLastErrorFileName, FLastErrorLine, 'Script runs for too long, terminating']);
  JS_TriggerOperationCallback(rt);
end;

function TSMEngine.InitWatchdog: boolean;
begin
  Assert(not Assigned(fWatchdogThread));
  fWatchdogLock := PR_NewLock;
  if Assigned(fWatchdogLock) then begin
    fWatchdogWakeup := PR_NewCondVar(fWatchdogLock);
    if Assigned(fWatchdogWakeup) then begin
      fSleepWakeup := PR_NewCondVar(fWatchdogLock);
      if Assigned(fSleepWakeup) then begin
        result := True;
        exit;
      end;
      PR_DestroyCondVar(fWatchdogWakeup);
    end;
  end;
  result := False;
end;

procedure TSMEngine.KillWatchdog;
var thread: PRThread;
begin
  PR_Lock(fWatchdogLock);
  thread := fWatchdogThread;
  if Assigned(thread) then begin
    // The watchdog thread is running, tell it to terminate waking it up
    // if necessary.
    fWatchdogThread := nil;
    PR_NotifyCondVar(fWatchdogWakeup);
  end;
  PR_Unlock(fWatchdogLock);
  if Assigned(thread) then
    PR_JoinThread(thread);
  PR_DestroyCondVar(fSleepWakeup);
  PR_DestroyCondVar(fWatchdogWakeup);
  PR_DestroyLock(fWatchdogLock);
end;

function IsBefore( t1, t2: int64): Boolean;
begin
  Result := int32(t1 - t2) < 0;
end;

procedure WatchdogMain(arg: pointer); cdecl;
var eng: TSMEngine;
    rt: PJSRuntime;
    now_: int64;
    sleepDuration: PRIntervalTime;
    status: PRStatus;
begin
  PR_SetCurrentThreadName('JS Watchdog');
  eng := TSMEngine(arg);
  rt := eng.rt;
  PR_Lock(eng.fWatchdogLock);
  while Assigned(eng.fWatchdogThread) do begin
    now_ := JS_Now();
    if (eng.fWatchdogHasTimeout and not IsBefore(now_, eng.fWatchdogTimeout)) then begin
      // The timeout has just expired. Trigger the operation callback outside the lock
      eng.fWatchdogHasTimeout := false;
      PR_Unlock(eng.fWatchdogLock);
      eng.CancelExecution;
      PR_Lock(eng.fWatchdogLock);
      // Wake up any threads doing sleep
      PR_NotifyAllCondVar(eng.fSleepWakeup);
    end else begin
      if (eng.fWatchdogHasTimeout) then begin
        // Time hasn't expired yet. Simulate an operation callback
        // which doesn't abort execution.
        JS_TriggerOperationCallback(rt);
      end;
      sleepDuration := PR_INTERVAL_NO_TIMEOUT;
      if (eng.fWatchdogHasTimeout) then
        sleepDuration := PR_TicksPerSecond() div 10;
      status := PR_WaitCondVar(eng.fWatchdogWakeup, sleepDuration);
      Assert(status = PR_SUCCESS);
    end
  end;
  PR_Unlock(eng.fWatchdogLock);
end;

function TSMEngine.ScheduleWatchdog(t: Double): Boolean;
var interval: Int64;
    timeout: Int64;
begin
  if (t <= 0) then begin
    PR_Lock(fWatchdogLock);
    fWatchdogHasTimeout := false;
    PR_Unlock(fWatchdogLock);
    result := true;
    exit;
  end;
  interval := int64(ceil(t * PRMJ_USEC_PER_SEC));
  timeout := JS_Now() + interval;
  PR_Lock(fWatchdogLock);
  if not Assigned(fWatchdogThread) then begin
    Assert(not fWatchdogHasTimeout);
    fWatchdogThread := PR_CreateThread(PR_USER_THREAD,
                                       @WatchdogMain,
                                       Self,
                                       PR_PRIORITY_NORMAL,
                                       PR_LOCAL_THREAD,
                                       PR_JOINABLE_THREAD,
                                       0);
    if not Assigned(fWatchdogThread) then begin
      PR_Unlock(fWatchdogLock);
      Result := false;
      Exit;
   end
  end else if (not fWatchdogHasTimeout or IsBefore(timeout, fWatchdogTimeout)) then begin
    PR_NotifyCondVar(fWatchdogWakeup);
  end;
  fWatchdogHasTimeout := true;
  fWatchdogTimeout := timeout;
  PR_Unlock(fWatchdogLock);
  Result := true;
end;

procedure TSMEngine.SetDefaultPropertyAttrs(const Value: TJSPropertyAttrs);
begin
  FDefaultPropertyAttrs := Value;
end;

procedure TSMEngine.SetTimeoutValue(const Value: Double);
begin
  fTimeoutInterval := Value;
  ScheduleWatchdog(Value);
end;

function TSMObject.GetPrivate: pointer;
{$ifdef WITHASSERT}
var C: PJSClass;
{$endif}
begin
  if obj=nil then
    result := nil else
{$ifdef WITHASSERT}
// JS_GetPrivate can return some not-nil pointer when we call JS_GetPrivate for object
// with class which has no flag JSCLASS_HAS_PRIVATE
  begin
    C := JS_GetClass(obj);
    if C.flags and JSCLASS_HAS_PRIVATE = 0 then
      result := nil // May be need to raise exception
    else
{$endif}
    result := JS_GetPrivate(obj);
{$ifdef WITHASSERT}
  end;
{$endif}
end;

procedure TSMObject.SetPrivate(const Value: pointer);
{$ifdef WITHASSERT}
var C: PJSClass;
{$endif}
begin
  if obj<>nil then
{$ifdef WITHASSERT} begin
    // If we set private data into object with class which has no flag JSCLASS_HAS_PRIVATE
    // SM don't raise exception, but we can get AV in any other place
    C := JS_GetClass(obj);
    if C.flags and JSCLASS_HAS_PRIVATE = 0 then
      exit // May be need to raise exception
    else
{$endif}
    JS_SetPrivate(obj, Value);
{$ifdef WITHASSERT}
  end;
{$endif}
end;

function TSMObject.GetPrivateData(expectedClass: PJSClass): pointer;
begin
  if obj=nil then
    result := nil else
    result := JS_GetInstancePrivate(cx, obj, expectedClass, nil);
end;

function TSMObject.ItemsCount: cardinal;
begin
  JS_GetArrayLength(cx,obj,result)
end;

function TSMObject.IsArray: boolean;
begin
  result := JS_IsArrayObject(cx,obj)=JS_TRUE;
end;

function TSMObject.GetItem(aIndex: integer): variant;
var res: TSMValue;
begin
  if JS_GetElement(cx,obj,aIndex,res.FValue)=JS_FALSE then
    raise ESMException.CreateUTF8('get TSMObject.Items[%]',[aIndex]) else
    res.ToVariant(cx,result);
end;

procedure TSMObject.SetDefaultPropertyAttrs(const Value: TJSPropertyAttrs);
begin
  FDefaultPropertyAttrs := Value;
end;

procedure TSMObject.SetItem(aIndex: integer; const Value: variant);
var val: TSMValue;
begin
  val.SetVariant(cx,Value);
  if JS_SetElement(cx,obj,aIndex,val.FValue)=JS_FALSE then
    raise ESMException.CreateUTF8('set TSMObject.Items[%]',[aIndex]);
end;

procedure TSMObject.DeleteItem(aIndex: integer);
begin
  if JS_DeleteElement(cx,obj,aIndex)=JS_FALSE then
    raise ESMException.CreateUTF8('TSMObject.DeleteItem(%)',[aIndex]);
end;


{ TSMVariant }

function TSMVariant.IntGet(var Dest: TVarData; const Instance: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var res: TSMValue;
begin
  //Assert(Instance.VType=SMVariantType.VarType);
  with TSMVariantData(Instance) do
    if JS_GetProperty(cx,obj,Name,res.FValue)=JS_FALSE then
      raise ESMException.CreateUTF8('Unexpected %.%',[self,Name]) else
      res.ToVariant(cx,variant(Dest));
  result := true;
end;

function TSMVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var args: SMValArray;
    a: integer;
    res: TSMValue;
    {$ifdef UNICODE}
    nam: array[byte] of AnsiChar;
    {$endif}
begin
  //Assert(V.VType=SMVariantType.VarType);
  result := true;
  with TSMVariantData(V).VObject do begin
    if (Arguments=nil) and (Name[1]='_') then begin
      {$ifdef UNICODE}
      UpperCopy255W(nam,Name)^ := #0;
      case IdemPCharArray(@nam[1],
      {$else}
      case IdemPCharArray(@Name[2],
      {$endif} ['ROOT','UNROOT']) of
      0: begin
        Root;
        exit;
      end;
      1: begin
        UnRoot;
        exit;
      end;
      end;
    end;
    SetLength(args,length(Arguments));
    for a := 0 to high(args) do
      args[a].SetVariant(cx,Variant(Arguments[a]));
    RunMethod(AnsiString(Name),args,res);
    res.ToVariant(cx,variant(Dest));
  end;
end;

function TSMVariant.IntSet(const Instance, Value: TVarData;
  Name: PAnsiChar; NameLen: PtrInt): boolean;
var smValue: TSMValue;
begin
  //Assert(Instance.VType=SMVariantType.VarType);
  with TSMVariantData(Instance) do begin
    smValue.SetVariant(cx,Variant(Value));
    result := JS_SetProperty(cx,obj,Name,smValue.FValue)<>JS_FALSE;
  end;
  if not result then
    raise ESMException.CreateUTF8('Error setting %.%',[self,Name]);
end;

procedure TSMVariant.ToJSON(W: TTextWriter; const Value: variant;
  Escape: TTextWriterKind);
var val: jsval;
begin
  with TSMVariantData(Value) do
  if VType=VarType then begin
    val := OBJECT_TO_JSVAL(obj);
    if JS_Stringify(cx, @val, nil, JSVAL_NULL, writeCallback, pointer(W))<>JS_TRUE then begin
      TSMEngine(cx.PrivateData).CheckJSError(JS_FALSE);
      TSMEngine(cx.PrivateData).ClearLastError;
    end;
  end else raise ESMException.CreateUTF8(
    '%.ToJSON: Unexpected variant type %',[self,VType]);
end;

class procedure TSMVariant.New(const aObject: TSMObject;
  out aValue: variant);
begin
  VarClear(aValue);
  TSMVariantData(aValue).Init(aObject);
end;

class procedure TSMVariant.New(cx: PJSContext; obj: PJSObject;
  out aValue: variant);
begin
  VarClear(aValue);
  TSMVariantData(aValue).Init(cx,obj);
end;

class procedure TSMVariant.New(engine: TSMEngine; out aValue: variant);
begin
  VarClear(aValue);
  TSMVariantData(aValue).InitNew(engine);
end;

procedure TSMVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest,Source,VarType);
end;

procedure TSMVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
var tmp: RawUTF8;
begin
  if Source.VType<>VarType then
    RaiseCastError;
  tmp := VariantToUTF8(variant(Source));
  if tmp='' then
    Variant(Dest) := '<<JavaScript TSMVariant>>' else
    RawUTF8ToVariant(tmp,Variant(Dest));
end;


{ TSMVariantData }

procedure TSMVariantData.GetGlobal(out global: variant);
begin
  global := VObject.Engine.Global;
end;

procedure TSMVariantData.Init(const aObject: TSMObject);
begin
  Init(aObject.cx,aObject.obj);
end;

procedure TSMVariantData.Init(aCx: PJSContext; aObj: PJSObject);
begin
  if SMVariantType=nil then
    SMVariantType := SynRegisterCustomVariantType(TSMVariant);
  ZeroFill(@self);
  VType := SMVariantType.VarType;
  VObject.fCx := aCx;
  VObject.fObj := aObj;
end;

procedure TSMVariantData.InitNew(engine: TSMEngine);
var aObj: TSMObject;
begin
  engine.NewObject(aObj);
  Init(aObj.cx,aObj.obj);
end;

initialization
  Assert(sizeof(TSMVariantData)=sizeof(variant));
end.
