/// automated tests for SpiderMonkey MORmot (Monkey On Rails) related units
// of the Synopse mORMot Framework
// - this unit is a part of freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSMSelfTest;
{
    Synopse framework. Copyright (C) 2013 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

    Scripting support MORmot (Monkey On Rails) Copyright (C) 2013 Pavel Mashlyakovsky
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
  Pavel Mashlyakovsky.
  Portions created by the Initial Developer are Copyright (C) 2013
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  
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

  Version 0.1
  - initial release. Use SpiderMonkey 24

}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64
{$I SynSM.inc}   // define SM_DEBUG JS_THREADSAFE CONSIDER_TIME_IN_Z

/// this is the main entry point of the tests
// - this procedure will create a console, then run all available tests
procedure SynSMConsoleTests;


implementation

uses
  Windows,
  Classes,
  SysUtils,
  Math,
  SynCommons,
  SynLog,
  SynTests,
  SynSMAPI,
  SynSM,
  SynCrtSock,
  DateUtils;

const
  jsglobal_class: JSClass = (name: 'global';
    flags: JSCLASS_HAS_PRIVATE or JSCLASS_GLOBAL_FLAGS { or JSCLASS_NEW_RESOLVE };
    addProperty: JS_PropertyStub;
    delProperty: JS_DeletePropertyStub;
    getProperty: JS_PropertyStub;
    setProperty: JS_StrictPropertyStub;
    enumerate: @JS_EnumerateStub;
    resolve: JS_ResolveStub;
    convert: JS_ConvertStub;
    //finalize is Optionally non-null member
    //in source it marked as Mandatory, but it doesn't
    //use in tests and there is no exported function JS_FinalizeStub
    );

type
  /// MORmot scritping engine unitary testing
  // - this class will launch all available scripting engine tests 
  TTestsSynSM = class(TSynTestsLogged)
  published
    /// test the SpiderMonkey API
    // - low level API function call
    procedure _SynSMAPI;
    /// test the SpiderMonkey classes
    // - high level objects
    procedure _SynSM;
  end;

  /// this test case will test used low-level SpiderMonkey functions
  TTestSynSMAPI = class(TSynTestCase)
  private
    rt: PJSRuntime;
    cx: PJSContext;
    global: PJSObject;
    comp: PJSCompartment;
    scriptDir: TFileName;
    obj: PJSObject;
  public
    destructor Destroy; override;
  published
    /// create SM runtime, context, global object and InitStandartClasses
    procedure RuntimeCreation;
    /// basic script compilation
    procedure CompilationRoutines;
    /// test Delphi<->SM interop of basic types
    procedure BasicTypeDelphiSmInterop;
    /// test creating SM Object
    procedure SpidermonkeyObjects;
    /// JSString and GarbageCollection
    procedure StringAndGarbageCollection;
    ///TODO test native (implemented in delphi) function and object
    procedure NativeFunctionsAndObject;
    ///TODO test multiple thread
    // we implement our HI level delphi wraper in "runtime per context" manner
    // to allow per thread garbage collection
    // http://blog.mozilla.org/luke/2012/01/24/jsruntime-is-now-officially-single-threaded
    procedure MultipleThreads;
    /// test Dephi object corresponding to low-level JS types
    // - e.g. JSRuntime, JSString, JSObject, jsval
    procedure BasicSmStructureWrappers;
    /// test Jslint support
    procedure JslintSupport;
    /// test external objects access
    procedure ExternalObject;
    /// destroy context here for future test
    procedure Finalize;
  end;

  /// this test case will test used high-level SpiderMonkey functions
  TTestSynSM = class(TSynTestCase)
  private
    FManager: TSMEngineManager;
  protected
  public
    destructor Destroy; override;
  published
    procedure CreateSmEngine;
    procedure ExternalObject;
    procedure SmValueTransformation;
    procedure ScriptEvaluation;
    procedure LoadMustacheTemplate;
    procedure RunMustacheTemplate;
    procedure RunTechempowerTemplate;
    procedure ArrayBufferSupport;

    procedure AbortOnTimeout;
  end;


threadvar
  strFinalizerCircles: Cardinal;
  finalizer: JSStringFinalizer;

procedure SynSMConsoleTests;
begin
  AllocConsole;
  //if false then
  with TSynLog.Family do begin
    Level := LOG_VERBOSE;
    //HighResolutionTimeStamp := true;
    PerThreadLog := ptIdentifiedInOnFile;
    TSynLogTestLog := TSynLog;
  end;

  // testing is performed by some dedicated classes defined in the above units
  with TTestsSynSM.Create('SynSM Automated tests') do
  try
    if ParamCount<>0 then begin
      SaveToFile(paramstr(1)); // DestPath on command line -> export to file
      Writeln(Ident,#13#10#13#10' Running tests... please wait');
    end;
    Run;
    if ParamCount<>0 then
      exit; // direct exit if an external file was generated
  finally
    Free;
  end;
  WriteLn('Done - Press ENTER to Exit');
  ReadLn;
end;


{ TTestSynSMAPI }
destructor TTestSynSMAPI.Destroy;
begin
{  cx.Destroy;
  rt.Destroy;}
  inherited;
end;

procedure TTestSynSMAPI.RuntimeCreation;
const
  cMaxMem = 8 * 1024 * 1024;
  Opt: CompartmentOptions =
    (
      zoneSpec: zsFreshZone;
      hasVersion: True;
      version: JSVERSION_LATEST;
    );
begin
  rt := JS_NewRuntime(cMaxMem, JS_USE_HELPER_THREADS);
  if CheckFailed(rt <> nil, 'Create Runtime') then Exit;
  Check(JS_GetGCParameter(rt, JSGC_MAX_BYTES) = cMaxMem);
  Check(JS_GetGCParameter(rt, JSGC_MAX_MALLOC_BYTES) = cMaxMem);

  cx := JS_NewContext(rt, 8192);
  if CheckFailed(cx <> nil, 'Create Context') then Exit;

//  You must set jsoBaseLine,jsoTypeInference,jsoIon for the enabling ION
//  ION is disabled without this options
  {$ifdef FIXBUGXE3}
  cx.SetOptions([jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs]);
  Check(cx.GetOptions=[jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs]);
  {$else}
  cx.Options := [jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs];
  Check(cx.Options=[jsoVarObjFix,jsoBaseLine,jsoTypeInference,jsoIon,jsoAsmJs]);
  {$endif}
  Check(JS_GetOptions(cx) =  JSOPTION_VAROBJFIX or JSOPTION_BASELINE or JSOPTION_TYPE_INFERENCE or JSOPTION_ION or JSOPTION_ASMJS);

  global := JS_NewGlobalObject(cx, @jsglobal_class, nil, @Opt);
  if CheckFailed(global <> nil, 'Create global object') then Exit;
  comp := JS_EnterCompartment(cx,global);
  Check(cx.Version=JSVERSION_ECMA_5,'Invalid ECMA version');

  if CheckFailed(cx.InitStandardClasses(global), 'InitStandardClasses') then Exit;
  Check( JS_DefineProperty(cx, global, 'global', OBJECT_TO_JSVAL(global), nil, nil, JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT) = JS_TRUE);
end;


procedure TTestSynSMAPI.CompilationRoutines;
const
  ansiScript: AnsiString = 'var __testVar = 1; __testVar += 1; __testVar = 1;';
  ansiScriptWErr: AnsiString = 'var i; {';
  ansiECMA5: AnsiString = 'var k = 0; for(let i = 0; i<1; i++){k+=i}; k;';
  globalAccess: AnsiString = 'global.myGlobalVar = 10;';
  ansiScriptStrConcatUndefPlusNum: AnsiString = '''fuck me tender '' + (undefined + 1)';
var
  scrObj: PJSScript;
  rval: jsval;
//  fn: AnsiString;
  uString: SynUnicode;
begin
  scriptDir := ExeVersion.ProgramFilePath + 'js';
  Check(DirectoryExists(scriptDir), scriptDir + ' dose not exist');
  if CheckFailed(IsAnsiCompatible(PChar(scriptDir)), ' Path to test directory must be Ansi Compatible') then Exit;

  // check global accessible from script
  Check( JS_EvaluateScript(cx, global, PCChar(globalAccess), length(globalAccess), 'test', 1, rval) = JS_TRUE, 'direct assign global.var');

  // check error, which was in SM17
  Check( JS_EvaluateScript(cx, global, PCChar(ansiScriptStrConcatUndefPlusNum), length(ansiScriptStrConcatUndefPlusNum), 'test', 1, rval) = JS_TRUE, 'ansiScriptStrConcatUndefPlusNum');

  scrObj := JS_CompileScript(cx, global, PCChar(ansiScript), length(ansiScript), 'test', 1);
  if not CheckFailed(scrObj <> nil) then
    Check(JS_ExecuteScript(cx, global, scrObj, rval) = JS_TRUE);
  // wrong script
  Check(JS_CompileScript(cx, global, PCChar(ansiScriptWErr), length(ansiScriptWErr), 'test', 1) = nil, 'script with errors don''t compiled');

  // test ECMA5 features
  if not CheckFailed(JS_EvaluateScript(cx, global, PCChar(ansiECMA5), length(ansiECMA5), 'test', 1, rval) = JS_TRUE, 'ECMA5 compartibility') then
    Check(JS_TypeOfValue(cx, rval) = JSTYPE_NUMBER, 'ECMA5 script result' );

//  We don't use file compilation    
//  fn := scriptDir + '\testCompileAnsi.js';
//  Check(JS_CompileUTF8File(cx, global, PCChar(fn)) <> nil, 'Compile file with ansi content');
  //TODO!!! not compiled
  //fn := scriptDir + '\testCompileUTF8.js';
  //Check(JS_CompileUTF8File(cx, global, PCChar(fn)) <> nil, 'Compile file with UTF8 content');

  uString := AnyTextFileToSynUnicode(scriptDir + '\testCompileUnicode.js');
  Check(JS_CompileUCScript(cx, global, Pjschar(uString), length(uString), 'test', 1) <> nil, 'compile unicode file');

  uString := AnyTextFileToSynUnicode(scriptDir + '\testCompileUTF8.js');
  Check(JS_CompileUCScript(cx, global, Pjschar(uString), length(uString), 'test', 1) <> nil, 'compile utf8 file');
end;

procedure TTestSynSMAPI.BasicTypeDelphiSMInterop;
const
  ansiScript: AnsiString =
    'var i = 10, dbl = 1.2,'+         //JSTYPE_NUMBER
    't_bool = true, f_bool = false,'+ //JSTYPE_BOOLEAN
    'str = "string value", '+       //JSTYPE_STRING
// xml is not supported now
    'v_void,' + // JSTYPE_VOID { undefined }
    'obj = {a: 10, s: "str10"}, '+ //JSTYPE_OBJECT,
    'fn = function(){}, '+ //JSTYPE_FUNCTION
    'null_val = null;'; //JSTYPE_NULL
var
  rval: jsval;
  L: size_t;
  pstr: Pjschar;
  rawStr: RawUnicode;
  obj: PJSObject;
  d: Double;
  i: integer;
begin
  //check jvlas layout is valid
  Randomize;

  for i := 1 to 100000 do begin
    D := Random * i;
    rval := JS_NumberValue(d);
    if not CheckFailed(JSVAL_IS_DOUBLE(rval), 'not jsdouble: ' + DoubleToString(d)) then
      CheckSame(d, JSVAL_TO_DOUBLE(rval), 1E-12, 'jsdouble<->double fail for ' + DoubleToString(d));

    rval := JS_NumberValue(i - 100);
    if not CheckFailed(JSVAL_IS_INT(rval), 'not int: ' + IntToString(i)) then
      Check(i - 100 = JSVAL_TO_Int(rval), 'js_int<->int fail for ' + IntToString(i));
  end;

  if CheckFailed(JS_EvaluateScript(cx, global, PCChar(ansiScript), length(ansiScript), 'test', 1, rval) = JS_TRUE) then exit;

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'i', rval) = JS_TRUE);
  Check(JSVAL_IS_INT(rval), 'integer value');
  Check(JSVAL_TO_INT(rval) = 10, 'get int');

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'dbl', rval) = JS_TRUE);
  Check(JSVAL_IS_DOUBLE(rval));
  CheckSame(JSVAL_TO_DOUBLE(rval), 1.2);

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 't_bool', rval) = JS_TRUE);
  Check(rval = JSVAL_TRUE, 'JSVAL_TRUE const definition');
  Check(JSVAL_IS_BOOLEAN(rval));
  Check(JSVAL_TO_BOOLEAN(rval) = JS_TRUE, 'get true bool');

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'f_bool', rval) = JS_TRUE);
  Check(JSVAL_IS_BOOLEAN(rval));
  Check(JSVAL_TO_BOOLEAN(rval) = JS_FALSE, 'get false bool');

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'str', rval) = JS_TRUE);
  Check(JSVAL_IS_STRING(rval)); L := 0;
  pstr := JS_GetStringCharsAndLength(cx, JSVAL_TO_STRING(rval), L);
  Check(pstr <> nil);
  if not CheckNot(L=0) then begin
    rawStr := WinAnsiConvert.AnsiToRawUnicode('string value');
    Check( CompareMem(pstr, pointer(rawStr), L shl 1), 'string value');
  end;

  // xml is not supported now

  rval := JSVAL_ONE;
  Check(JS_GetProperty(cx, global, 'v_void', rval) = JS_TRUE);
  Check(JSVAL_IS_VOID(rval));

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'obj', rval) = JS_TRUE);
  if not CheckFailed(JSVAL_IS_OBJECT(rval)) then begin
    obj := JSVAL_TO_OBJECT(rval);
    Check(obj <> nil);
    if not CheckFailed(JS_GetProperty(cx, obj, 'a', rval) = JS_TRUE) then begin
      Check(JSVAL_IS_INT(rval));
      Check(JSVAL_TO_INT(rval) = 10);
    end;
  end;

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'fn', rval) = JS_TRUE);
  Check(JS_TypeOfValue(cx, rval) = JSTYPE_FUNCTION);

  rval := JSVAL_VOID;
  Check(JS_GetProperty(cx, global, 'null_val', rval) = JS_TRUE);
  Check(JSVAL_IS_NULL(rval));
end;

procedure TTestSynSMAPI.SpidermonkeyObjects;
const
  ucName: SynUnicode = 'ПриветМедвед';
var
  val: jsval;
  attr: uintN;
  propFound: JSBool;
begin
  // create ordinary JavaScript Object
  obj := JS_NewObject(cx, nil{class}, nil{proto}, nil{parent} );
  Check(obj <> nil);
  Check(JS_AddObjectRoot(cx, @obj) = JS_TRUE);

  val := OBJECT_TO_JSVAL(obj);
  Check(val <> JSVAL_NULL);
  Check(JSVAL_IS_OBJECT(val));

  if not CheckFailed(JS_DefineProperty(cx, obj, 'myProp', INT_TO_JSVAL(100500), nil, nil, 0) = JS_TRUE) then begin
    Check(JS_GetProperty(cx, obj, 'myProp', val) = JS_TRUE);
    Check(JSVAL_TO_INT(val) = 100500);
  end;

  if not CheckFailed(JS_DefineUCProperty(cx, obj, Pjschar(ucName), length(ucName), DOUBLE_TO_JSVAL(100.500), nil, nil, JSPROP_ENUMERATE or JSPROP_READONLY) = JS_TRUE) then begin
    Check(JS_GetUCProperty(cx, obj, Pjschar(ucName), length(ucName), val) = JS_TRUE);
    CheckSame(JSVAL_TO_DOUBLE(val), 100.500);
    Check(JS_GetUCPropertyAttributes(cx, obj, Pjschar(ucName), length(ucName), attr, propFound)=JS_TRUE);
    Check(propFound=JS_TRUE);
    Check(attr = (JSPROP_ENUMERATE or JSPROP_READONLY));
    // read only property - set property is true but value not changed. As in ECMA
    val := INT_TO_JSVAL(1);
    Check(JS_SetUCProperty(cx, obj, Pjschar(ucName), length(ucName), val) = JS_TRUE);
    Check(JS_GetUCProperty(cx, obj, Pjschar(ucName), length(ucName), val) = JS_TRUE);
    CheckNot( JSVAL_IS_INT(val));
  end;
end;

// SMStringFinalize signature was changed
procedure SMStringFinalize(fin: PJSStringFinalizer; chars: Pjschar); cdecl;
begin
  Inc(strFinalizerCircles);
end;


procedure TTestSynSMAPI.StringAndGarbageCollection;
var
  uVar: SynUnicode;
  pStr: Pjschar;
  jsStr: PJSString;
  val: jsval;
  rndStr: SynUnicode;
  i: integer;
  len: size_t;
begin
  // obj from prev. test rooted, so it must not destroyed during GC;
  JS_GC(rt);
  Check(JS_GetProperty(cx, obj, 'myProp', val) = JS_TRUE);
  Check(JSVAL_TO_INT(val) = 100500);
  //TODO - how to check obj is alive? It like pointer to nothere...
  //TODO - how to check amount of memory used by SM runtime?

  Check( JS_GetGCParameter(rt, JSGC_NUMBER) = 1);
  strFinalizerCircles := 0;
  // actually we do not need string finalizer because Delphi free Pjschar = ^RawUnicode
  // just for testing
  finalizer.finalize := SMStringFinalize;

  uVar := 'this is Delphi string directly passed to SpiderMonkey.'#13#10'It MUST be unchanged during alive in SM!';

  jsStr := JS_NewExternalString(cx, pjschar(uVar), length(uVar), @finalizer);
  Check(jsStr <> nil);
  // root string so it not destroyed by GC
  Check( JS_AddStringRoot(cx, @jsStr) = JS_TRUE );
  JS_GC(rt);
  Check(strFinalizerCircles = 0);
  Check(JS_GetStringLength(jsStr) = cardinal(length(uVar)));

  // unroot string  - now it destroyed by GC
  Check( JS_RemoveStringRoot(cx, @jsStr) = JS_TRUE );
  JS_GC(rt);
  Check(strFinalizerCircles = 1);

  uVar := 'new string';
  jsStr := JS_NewExternalString(cx, pjschar(uVar), length(uVar), @finalizer);
  Check(jsStr <> nil);
  Check(JS_GetStringLength(jsStr) = 10);
  Check(JS_DefineProperty(cx, obj, 'strProp', STRING_TO_JSVAL(jsStr), nil, nil, 0) = JS_TRUE);
  // now string is rooted (it is a value of object);
  JS_GC(rt);
  Check(strFinalizerCircles = 1);
  JS_RemoveObjectRoot(cx, @obj);
  // and now object unrooted, so string unrooted to
  JS_GC(rt);
  Check(strFinalizerCircles = 2);

  // now test random Unicode string.
  for i := 0 to 1000 do begin
    rndStr := RawUnicodeToSynUnicode(RandomString(i*5));
    jsStr := JS_NewUCStringCopyN(cx, PjsChar(rndStr), length(rndStr));
    Check(jsStr <> nil);
    pStr := JS_GetStringCharsAndLength(cx, jsStr, len);
    Check(len = cardinal(length(rndStr)));
    SetString(uVar, PWideChar(pStr), len);
    Check(uVar = rndStr);
  end;
  JS_GC(rt);
end;

procedure TTestSynSMAPI.NativeFunctionsAndObject;
//var
//  val: jsval;
begin
  obj := JS_NewObject(cx, nil{class}, nil{proto}, nil{parent} );
//  val := OBJECT_TO_JSVAL(obj);

end;

procedure TTestSynSMAPI.MultipleThreads;
begin
  {TODO}
end;

procedure TTestSynSMAPI.BasicSmStructureWrappers;
begin
  {TODO}
end;

procedure TTestSynSMAPI.JslintSupport;
var
  uString: SynUnicode;
  rVal: jsval;
  jsLintFN: TFileName;
  jsLint: RawByteString;
begin
  jsLintFN := scriptDir + '\jslint.js';
  uString := AnyTextFileToSynUnicode(jsLintFN);
  if uString='' then begin
    jsLint := TWinINet.Get('https://github.com/douglascrockford/JSLint/raw/master/jslint.js');
    FileFromString(jsLint,jsLintFN);
    uString := SynUnicode(jsLint);
  end;
  Check(JS_EvaluateUCScript(cx, global, Pjschar(uString), length(uString), 'jslint', 1, rVal) = JS_TRUE, 'compile jslint');
end;

type
  TTestRec = record
    a: string;
    b: integer;
  end;
  PTestRec = ^TTestRec;
var
  IsPropertyCalled: boolean;
  
function TestClassProperty(cx: PJSContext; var obj: PJSObject; var id: jsid;
    vp: pjsval): JSBool; cdecl;
var
  js_Val: jsval;
  s: SynUnicode;
  TR: PTestRec;
begin
  IsPropertyCalled := true;
  JS_IdToValue(cx, id, js_Val);
  s := JS_ValueToString(cx, js_Val).ToSynUnicode(cx);
  TR := JS_GetPrivate(obj);
  Result := JS_TRUE;
  if s = 'a' then
    vp^ := cx.NewJSString(TR.a).ToJSVal
  else if s = 'b' then
    vp^ := JS_NumberValue(TR.b)
  else if s = 'c' then
    vp^ := JSVAL_NULL
  else
    Result := JS_FALSE;
end;

var
  IsDelPropertyCalled: boolean;
  
function TestClassDelProperty(cx: PJSContext; var obj: PJSObject; var id: jsid;
    succeeded: PJSBool):JSBool; cdecl;
var
  js_Val: jsval;
  s: SynUnicode;
  TR: PTestRec;
begin
  IsDelPropertyCalled := true;
  JS_IdToValue(cx, id, js_Val);
  s := JS_ValueToString(cx, js_Val).ToSynUnicode(cx);
  TR := JS_GetPrivate(obj);
  succeeded^ := JS_TRUE;
  Result := JS_TRUE;
  if s = 'a' then
    TR.a := ''
  else if s = 'b' then
    TR.b := 0
  else if s = 'c' then
    succeeded^ := JS_FALSE
  else
    Result := JS_FALSE;
end;

var
  IsSetPropertyCalled: boolean;
function TestClassSetProperty(cx: PJSContext; var obj: PJSObject; var id: jsid;
                                strict: JSBool; vp: pjsval): JSBool; cdecl;
var
  js_Val: jsval;
  s: SynUnicode;
  TR: PTestRec;
begin
  IsSetPropertyCalled := true;
  JS_IdToValue(cx, id, js_Val);
  s := JS_ValueToString(cx, js_Val).ToSynUnicode(cx);
  TR := JS_GetPrivate(obj);
  Result := JS_TRUE;
  if s = 'a' then
    TR.a := JS_ValueToString(cx, vp^).ToSynUnicode(cx)
  else if s = 'b' then
    TR.b := JSVAL_TO_INT(vp^)
  else
    Result := JS_FALSE;
end;

function TestClassEnumerate(cx: PJSContext; var obj: PJSObject; enum_op: JSIterateOp;
                              statep: pjsval; idp: pjsid): JSBool; cdecl;
begin
  Result := JS_TRUE; //ToDo
end;

var
  IsResolveCalled: boolean;
function TestClassResolve(cx: PJSContext; var obj: PJSObject;
    var id: jsid): JSBool; cdecl;
var
  js_Val: jsval;
  s: SynUnicode;
begin
  IsResolveCalled := true;
  JS_IdToValue(cx, id, js_Val);
  s := JS_ValueToString(cx, js_Val).ToSynUnicode(cx);
  if (s = 'a') or (s = 'b') or (s = 'c') then
    Result := JS_TRUE
  else
    Result := JS_FALSE;
end;

function TestClassConvert(cx: PJSContext; var obj: PJSObject; typ: JSType;
    vp: pjsval): JSBool; cdecl;
begin
  Result := JS_TRUE;//ToDo
end;

var
  IsFinalizeCalled: boolean;
  
procedure TestClassFinalize(cx: PJSContext; obj: PJSObject); cdecl;
var
  TR: PTestRec;
begin
  IsFinalizeCalled := true;
  TR := JS_GetPrivate(obj);
  if TR<>nil then
    Dispose(TR);
end;

var
  IsConstructCalled: boolean;
  
function TestClassConstruct(cx: PJSContext; argc: uintN; vp: Pjsval): JSBool; cdecl;
var
  obj: PJSObject;
  c: PJSClass;
  TR: PTestRec;
begin
  IsConstructCalled := true;
  obj := JSVAL_TO_OBJECT(vp^);
  c := JS_GetClass(obj);
  obj := JS_NewObject(cx, C, nil, nil);
  New(TR);
  JS_SetPrivate(obj,TR);
  vp^ := obj.ToJSValue;
  Result := JS_TRUE;
end;

const
  TestClass: JSClass = (name: 'TestClass';
    flags: JSCLASS_HAS_PRIVATE { or JSCLASS_NEW_RESOLVE };
    addProperty: TestClassProperty;
    delProperty: TestClassDelProperty;
    getProperty: TestClassProperty;
    setProperty: TestClassSetProperty;
    enumerate: @TestClassEnumerate;
    resolve: TestClassResolve;
    convert: TestClassConvert;
    finalize: TestClassFinalize;
    construct: TestClassConstruct;
    );

procedure TTestSynSMAPI.ExternalObject;
var
  obj: PJSObject;
  Val: jsval;
  TR: PTestRec;
const
  Script1: AnsiString = 'var obj = new TestClass();';
  
  Script2: AnsiString = 'obj.a ="555"';
  Script3: AnsiString = 'obj.a;';
  Script4: AnsiString = 'obj.a ="777"; obj;';
  Script5: AnsiString = 'delete obj.a; obj;';

  Script6: AnsiString = 'obj.b =555';
  Script7: AnsiString = 'obj.b;';
  Script8: AnsiString = 'obj.b =777; obj;';
  Script9: AnsiString = 'delete obj.b; obj;';

  Script10: AnsiString = 'obj.c =555;';
  Script11: AnsiString = 'obj.c;';
  Script12: AnsiString = 'obj.c ="777"';
  Script13: AnsiString = 'delete obj.c';

  Script14: AnsiString = 'obj.d =555;';

begin
  obj := JS_InitClass(cx, global ,nil, @TestClass, nil, 0, nil, nil, nil, nil);
  IsConstructCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script1),length(Script1),nil,0, Val)=JS_TRUE);
  Check(IsConstructCalled);

  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script2),length(Script2),nil,0, Val)=JS_TRUE);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script3),length(Script3),nil,0, Val)=JS_TRUE);
  Check(JSVAL_TO_STRING(Val).ToSynUnicode(cx)='555');
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script4),length(Script4),nil,0, Val)=JS_TRUE);
  Check(IsSetPropertyCalled);
  TR := JS_GetPrivate(JSVAL_TO_OBJECT(Val));
  Check(TR.a = '777');

  IsDelPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script5),length(Script5),nil,0, Val)=JS_TRUE);
  Check(IsDelPropertyCalled);
  TR := JS_GetPrivate(JSVAL_TO_OBJECT(Val));
  Check(TR.a = '');


  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script6),length(Script6),nil,0, Val)=JS_TRUE);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script7),length(Script7),nil,0, Val)=JS_TRUE);
  Check(JSVAL_TO_INT(Val)=555);
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script8),length(Script8),nil,0, Val)=JS_TRUE);
  Check(IsSetPropertyCalled);
  TR := JS_GetPrivate(JSVAL_TO_OBJECT(Val));
  Check(TR.b = 777);

  IsDelPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script9),length(Script9),nil,0, Val)=JS_TRUE);
  Check(IsDelPropertyCalled);
  TR := JS_GetPrivate(JSVAL_TO_OBJECT(Val));
  Check(TR.b = 0);


  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  CheckNot(JS_EvaluateScript(cx,global,pointer(Script10),length(Script10),nil,0, Val)=JS_TRUE);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script11),length(Script11),nil,0, Val)=JS_TRUE);
  Check(Val=JSVAL_NULL);
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  CheckNot(JS_EvaluateScript(cx,global,pointer(Script12),length(Script12),nil,0, Val)=JS_TRUE);
  Check(IsSetPropertyCalled);

  IsDelPropertyCalled := false;
  Check(JS_EvaluateScript(cx,global,pointer(Script13),length(Script13),nil,0, Val)=JS_TRUE);
  Check(IsDelPropertyCalled);
  Check(JSVAL_TO_BOOLEAN(Val)=JS_FALSE);

  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  CheckNot(JS_EvaluateScript(cx,global,pointer(Script14),length(Script14),nil,0, Val)=JS_TRUE);
  Check(IsResolveCalled);
  CheckNot(IsPropertyCalled);
  CheckNot(IsSetPropertyCalled);

end;

procedure TTestSynSMAPI.Finalize;
begin
  IsFinalizeCalled := false;
  JS_LeaveCompartment(cx,comp);
  cx.Destroy;
  Check(IsFinalizeCalled);
  rt.Destroy;
end;


{ TTestsSynSM }

procedure TTestsSynSM._SynSMAPI;
begin
  AddCase([TTestSynSMAPI]);
end;

procedure TTestsSynSM._SynSM;
begin
  AddCase([TTestSynSM]);
end;

{ TTestSynSM }

procedure TTestSynSM.ArrayBufferSupport;
var
  engine: TSMEngine;
  SMObj: TSMObject;
  SMVal: TSMValue;

  JSObj: PJsObject;
  cx: PJSContext;

  procedure CheckArrayType(T: JSArrayBufferViewType; aNeedSetVal: boolean = true);
  var
    JSObj1: PJSObject;
    L: uint32;
    int8Vector: Pint8Vector;
    uint8Vector: Puint8Vector;
    int16Vector: Pint16Vector;
    uint16Vector: Puint16Vector;
    int32Vector: Pint32Vector;
    uint32Vector: Puint32Vector;
    float32Vector: Pfloat32Vector;
    float64Vector: Pfloat64Vector;

  begin

    Check(JS_GetArrayBufferViewType(JSObj)=T);

    engine.MakeObject(JSObj, SMObj);
    engine.GlobalObject.DefineProperty('testAB', SMObj.AsSMValue);

    if aNeedSetVal then begin
      engine.GlobalObject.Evaluate('testAB;','s',0,SMVal);

      if T = jsabTYPE_DATAVIEW then begin
        Check(SMVal.ToJSON(cx)='{}');
        engine.GlobalObject.Evaluate(
          'var dv=new DataView(testAB);'+
          'dv.setUint8(0,11);'+
          'dv.setUint8(1,22);'+
          'dv.setUint8(2,33);'+
          'dv.setUint8(3,44);'+
          'dv.setUint8(4,55);'
          ,'s',0,SMVal);

      end else begin
        Check(SMVal.ToJSON(cx)='{"0":0,"1":0,"2":0,"3":0,"4":0}');

        SMObj.Items[0] := 11;
        SMObj.Items[1] := 22;
        SMObj.Items[2] := 33;
        SMObj.Items[3] := 44;
        SMObj.Items[4] := 55;
      end;
    end;


    engine.GlobalObject.Evaluate('testAB;','s',0,SMVal);

    L := JS_GetArrayBufferByteLength(JSObj);
    uint8Vector := JS_GetArrayBufferData(JSObj);

    Check(JS_GetTypedArrayByteOffset(JSObj) = 0);

    if T = jsabTYPE_DATAVIEW then begin
      CheckNot(JS_IsTypedArrayObject(JSObj)=JS_TRUE);
      CheckNot(JS_IsArrayBufferViewObject(JSObj)=JS_TRUE);
      Check(JS_IsArrayBufferObject(JSObj)=JS_TRUE);
      Check(L=5);
      Check(uint8Vector[0]=11);
      Check(uint8Vector[1]=22);
      Check(uint8Vector[2]=33);
      Check(uint8Vector[3]=44);
      Check(uint8Vector[4]=55);

      Check(JS_GetInt8ArrayData(JSObj)=nil);
      Check(JS_GetUint8ArrayData(JSObj)=nil);
      Check(JS_GetUint8ClampedArrayData(JSObj)=nil);
      Check(JS_GetInt16ArrayData(JSObj)=nil);
      Check(JS_GetUint16ArrayData(JSObj)=nil);
      Check(JS_GetInt32ArrayData(JSObj)=nil);
      Check(JS_GetUint32ArrayData(JSObj)=nil);
      Check(JS_GetFloat32ArrayData(JSObj)=nil);
      Check(JS_GetFloat64ArrayData(JSObj)=nil);

      L := JS_GetTypedArrayByteOffset(JSObj);

    end else begin
      Check(JS_IsTypedArrayObject(JSObj)=JS_TRUE);
      Check(JS_IsArrayBufferViewObject(JSObj)=JS_TRUE);
      CheckNot(JS_IsArrayBufferObject(JSObj)=JS_TRUE);
      Check(L=0);
      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(JS_GetTypedArrayLength(JSObj)=5);

    end;

    L := 0;
    int8Vector := nil;
    JSObj1 := JS_GetObjectAsInt8Array(JSObj, L, int8Vector);
    if T = jsabTYPE_INT8 then begin
      Check(JS_IsInt8Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(int8Vector));
      Check(int8Vector[0]=11);
      Check(int8Vector[1]=22);
      Check(int8Vector[2]=33);
      Check(int8Vector[3]=44);
      Check(int8Vector[4]=55);

      int8Vector[3] := 4;
      int8Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 130;'+
      'testAB[1] = -150;'+
      'testAB[2] = 3;'+
      ';testAB;'
      ,'s',0,SMVal);

      Check(SMVal.ToJSON(cx)='{"0":-126,"1":106,"2":3,"3":4,"4":5}');

      int8Vector[0] := 11;
      int8Vector[1] := 22;
      int8Vector[2] := 33;
      int8Vector[3] := 44;
      int8Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(int8Vector = JS_GetInt8ArrayData(JSObj));
      Check(int8Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*1);
      
    end else begin
      CheckNot(JS_IsInt8Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(int8Vector));
    end;

    L := 0;
    uint8Vector := nil;
    JSObj1 := JS_GetObjectAsUint8Array(JSObj, L, uint8Vector);
    if T = jsabTYPE_UINT8 then begin
      Check(JS_IsUint8Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(uint8Vector));
      Check(uint8Vector[0]=11);
      Check(uint8Vector[1]=22);
      Check(uint8Vector[2]=33);
      Check(uint8Vector[3]=44);
      Check(uint8Vector[4]=55);

      uint8Vector[3] := 4;
      uint8Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 300;'+
      'testAB[1] = -1;'+
      'testAB[2] = 3;'+
      ';testAB;'
      ,'s',0,SMVal);

      Check(SMVal.ToJSON(cx)='{"0":44,"1":255,"2":3,"3":4,"4":5}');

      uint8Vector[0] := 11;
      uint8Vector[1] := 22;
      uint8Vector[2] := 33;
      uint8Vector[3] := 44;
      uint8Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(uint8Vector = JS_GetUint8ArrayData(JSObj));
      Check(uint8Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*1);

    end else begin
      CheckNot(JS_IsUint8Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(uint8Vector));
    end;

    L := 0;
    uint8Vector := nil;
    JSObj1 := JS_GetObjectAsUint8ClampedArray(JSObj, L, uint8Vector);
    if T = jsabTYPE_UINT8_CLAMPED then begin
      Check(JS_IsUint8ClampedArray(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(uint8Vector));
      Check(uint8Vector[0]=11);
      Check(uint8Vector[1]=22);
      Check(uint8Vector[2]=33);
      Check(uint8Vector[3]=44);
      Check(uint8Vector[4]=55);

      uint8Vector[3] := 4;
      uint8Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 300;'+
      'testAB[1] = -1;'+
      'testAB[2] = 3;'+
      ';testAB;'
      ,'s',0,SMVal);

      Check(SMVal.ToJSON(cx)='{"0":255,"1":0,"2":3,"3":4,"4":5}');

      uint8Vector[0] := 11;
      uint8Vector[1] := 22;
      uint8Vector[2] := 33;
      uint8Vector[3] := 44;
      uint8Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(uint8Vector = JS_GetUint8ClampedArrayData(JSObj));
      Check(uint8Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*1);

    end else begin
      CheckNot(JS_IsUint8ClampedArray(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(uint8Vector));
    end;

    L := 0;
    int16Vector := nil;
    JSObj1 := JS_GetObjectAsInt16Array(JSObj, L, int16Vector);
    if T = jsabTYPE_INT16 then begin
      Check(JS_IsInt16Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(int16Vector));
      Check(int16Vector[0]=11);
      Check(int16Vector[1]=22);
      Check(int16Vector[2]=33);
      Check(int16Vector[3]=44);
      Check(int16Vector[4]=55);

      int16Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 40000;'+
      'testAB[1] = -40000;'+
      'testAB[2] = 300;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);

      Check(SMVal.ToJSON(cx)='{"0":-25536,"1":25536,"2":300,"3":4,"4":5}');

      int16Vector[0] := 11;
      int16Vector[1] := 22;
      int16Vector[2] := 33;
      int16Vector[3] := 44;
      int16Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(int16Vector = JS_GetInt16ArrayData(JSObj));
      Check(int16Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*2);

    end else begin
      CheckNot(JS_IsInt16Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(int16Vector));
    end;
    
    L := 0;
    uint16Vector := nil;
    JSObj1 := JS_GetObjectAsUint16Array(JSObj, L, uint16Vector);
    if T = jsabTYPE_UINT16 then begin
      Check(JS_IsUint16Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(uint16Vector));
      Check(uint16Vector[0]=11);
      Check(uint16Vector[1]=22);
      Check(uint16Vector[2]=33);
      Check(uint16Vector[3]=44);
      Check(uint16Vector[4]=55);

      uint16Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 70000;'+
      'testAB[1] = -40000;'+
      'testAB[2] = 1300;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);

      Check(SMVal.ToJSON(cx)='{"0":4464,"1":25536,"2":1300,"3":4,"4":5}');

      uint16Vector[0] := 11;
      uint16Vector[1] := 22;
      uint16Vector[2] := 33;
      uint16Vector[3] := 44;
      uint16Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(uint16Vector = JS_GetUint16ArrayData(JSObj));
      Check(uint16Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*2);

    end else begin
      CheckNot(JS_IsUint16Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(uint16Vector));
    end;

    L := 0;
    int32Vector := nil;
    JSObj1 := JS_GetObjectAsInt32Array(JSObj, L, int32Vector);
    if T = jsabTYPE_INT32 then begin
      Check(JS_IsInt32Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(int32Vector));
      Check(int32Vector[0]=11);
      Check(int32Vector[1]=22);
      Check(int32Vector[2]=33);
      Check(int32Vector[3]=44);
      Check(int32Vector[4]=55);

      int32Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 3000000000;'+
      'testAB[1] = -3000000000;'+
      'testAB[2] = 300000;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);
      Check(SMVal.ToJSON(cx)='{"0":-1294967296,"1":1294967296,"2":300000,"3":4,"4":5}');

      int32Vector[0] := 11;
      int32Vector[1] := 22;
      int32Vector[2] := 33;
      int32Vector[3] := 44;
      int32Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(int32Vector = JS_GetInt32ArrayData(JSObj));
      Check(int32Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*4);

    end else begin
      CheckNot(JS_IsInt32Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(int32Vector));
    end;

    L := 0;
    uint32Vector := nil;
    JSObj1 := JS_GetObjectAsUint32Array(JSObj, L, uint32Vector);
    if T = jsabTYPE_UINT32 then begin
      Check(JS_IsUint32Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(uint32Vector));
      Check(uint32Vector[0]=11);
      Check(uint32Vector[1]=22);
      Check(uint32Vector[2]=33);
      Check(uint32Vector[3]=44);
      Check(uint32Vector[4]=55);

      uint32Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 5000000000;'+
      'testAB[1] = -5000000000;'+
      'testAB[2] = 300000;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);
      Check(SMVal.ToJSON(cx)='{"0":705032704,"1":3589934592,"2":300000,"3":4,"4":5}');

      uint32Vector[0] := 11;
      uint32Vector[1] := 22;
      uint32Vector[2] := 33;
      uint32Vector[3] := 44;
      uint32Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(uint32Vector = JS_GetUint32ArrayData(JSObj));
      Check(uint32Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*4);

    end else begin
      CheckNot(JS_IsUint32Array(JSObj)=JS_TRUE);

      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(uint32Vector));
    end;

    L := 0;
    float32Vector := nil;
    JSObj1 := JS_GetObjectAsFloat32Array(JSObj, L, float32Vector);
    if T = jsabTYPE_FLOAT32 then begin
      Check(JS_IsFloat32Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(float32Vector));
      Check(float32Vector[0]=11);
      Check(float32Vector[1]=22);
      Check(float32Vector[2]=33);
      Check(float32Vector[3]=44);
      Check(float32Vector[4]=55);

      float32Vector[4] := 5.5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 0.5;'+
      'testAB[1] = 1e100;'+
      'testAB[2] = 33.42e5;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);
      Check(SMVal.ToJSON(cx)='{"0":0.5,"1":null,"2":3342000,"3":4,"4":5.5}');
      Check(float32Vector[1] = Infinity);
      float32Vector[0] := 11;
      float32Vector[1] := 22;
      float32Vector[2] := 33;
      float32Vector[3] := 44;
      float32Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(float32Vector = JS_GetFloat32ArrayData(JSObj));
      Check(float32Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*4);

    end else begin
      CheckNot(JS_IsFloat32Array(JSObj)=JS_TRUE);
      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(float32Vector));
    end;
      

    L := 0;
    float64Vector := nil;
    JSObj1 := JS_GetObjectAsFloat64Array(JSObj, L, float64Vector);
    if T = jsabTYPE_FLOAT64 then begin
      Check(JS_IsFloat64Array(JSObj)=JS_TRUE);

      Check(Assigned(JSObj1));
      Check(L=5);
      Check(Assigned(float64Vector));
      Check(float64Vector[0]=11);
      Check(float64Vector[1]=22);
      Check(float64Vector[2]=33);
      Check(float64Vector[3]=44);
      Check(float64Vector[4]=55);

      float64Vector[4] := 5;
      engine.GlobalObject.Evaluate(
      'testAB[0] = 0.5;'+
      'testAB[1] = 1e1000;'+
      'testAB[2] = 3.7e100;'+
      'testAB[3] = 4;'+
      ';testAB;'
      ,'s',0,SMVal);
      Check(SMVal.ToJSON(cx)='{"0":0.5,"1":null,"2":3.7e+100,"3":4,"4":5}');
      Check(float64Vector[1] = Infinity);

      float64Vector[0] := 11;
      float64Vector[1] := 22;
      float64Vector[2] := 33;
      float64Vector[3] := 44;
      float64Vector[4] := 55;

      Check(SMVal.ToJSON(cx)='{"0":11,"1":22,"2":33,"3":44,"4":55}');

      Check(float64Vector = JS_GetFloat64ArrayData(JSObj));
      Check(float64Vector = JS_GetArrayBufferViewData(JSObj));
      Check(JS_GetTypedArrayByteLength(JSObj)=5*8);

    end else begin
      CheckNot(JS_IsFloat64Array(JSObj)=JS_TRUE);
      CheckNot(Assigned(JSObj1));
      Check(L=0);
      CheckNot(Assigned(float64Vector));
    end;
  end;

begin
  engine := FManager.ThreadSafeEngine;
  cx := engine.cx;

//  jsabTYPE_INT8
  JSObj := JS_NewInt8Array(cx,5);
  CheckArrayType(jsabTYPE_INT8);
  JSObj := JS_NewInt8ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_INT8,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewInt8ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_INT8,false);

//  jsabTYPE_UINT8
  JSObj := JS_NewUint8Array(cx,5);
  CheckArrayType(jsabTYPE_UINT8);
  JSObj := JS_NewUint8ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_UINT8,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewUint8ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_UINT8,false);

//  jsabTYPE_UINT8_CLAMPED
  JSObj := JS_NewUint8ClampedArray(cx,5);
  CheckArrayType(jsabTYPE_UINT8_CLAMPED);
  JSObj := JS_NewUint8ClampedArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_UINT8_CLAMPED,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewUint8ClampedArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_UINT8_CLAMPED,false);

//  jsabTYPE_INT16
  JSObj := JS_NewInt16Array(cx,5);
  CheckArrayType(jsabTYPE_INT16);
  JSObj := JS_NewInt16ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_INT16,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewInt16ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_INT16,false);

//  jsabTYPE_UINT16
  JSObj := JS_NewUint16Array(cx,5);
  CheckArrayType(jsabTYPE_UINT16);
  JSObj := JS_NewUint16ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_UINT16,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewUint16ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_UINT16,false);

//  jsabTYPE_INT32
  JSObj := JS_NewInt32Array(cx,5);
  CheckArrayType(jsabTYPE_INT32);
  JSObj := JS_NewInt32ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_INT32,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewInt32ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_INT32,false);

//  jsabTYPE_UINT32
  JSObj := JS_NewUint32Array(cx,5);
  CheckArrayType(jsabTYPE_UINT32);
  JSObj := JS_NewUint32ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_UINT32,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewUint32ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_UINT32,false);

//  jsabTYPE_FLOAT32
  JSObj := JS_NewFloat32Array(cx,5);
  CheckArrayType(jsabTYPE_FLOAT32);
  JSObj := JS_NewFloat32ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_FLOAT32,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewFloat32ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_FLOAT32,false);

//  jsabTYPE_FLOAT64
  JSObj := JS_NewFloat64Array(cx,5);
  CheckArrayType(jsabTYPE_FLOAT64);
  JSObj := JS_NewFloat64ArrayFromArray(cx, JSObj);
  CheckArrayType(jsabTYPE_FLOAT64,false);
  JSObj := JS_GetArrayBufferViewBuffer(JSObj);
//  CheckArrayType(jsabTYPE_DATAVIEW,false);
  JSObj := JS_NewFloat64ArrayWithBuffer(cx, JSObj, 0, 5);
  CheckArrayType(jsabTYPE_FLOAT64,false);

  JSObj := JS_NewArrayBuffer(cx,5);
  CheckArrayType(jsabTYPE_DATAVIEW);

end;

procedure TTestSynSM.ExternalObject;
var
  engine: TSMEngine;
  obj: TSMObject;
  Val: TSMValue;
  TR: PTestRec;
  ScriptFailed: boolean;
begin
  engine := FManager.ThreadSafeEngine;
  engine.InitClass(@TestClass, nil, obj);

  IsConstructCalled := false;
  engine.GlobalObject.Evaluate('var obj = new TestClass(); obj;', '', 0, Val);
  Check(IsConstructCalled);

  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a ="555";', '', 0, Val);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a;', '', 0, Val);
  Check(Val.ToSynUnicode(engine.cx) ='555');
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a ="777"; obj;', '', 0, Val);
  Check(IsSetPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.a = '777');

  IsDelPropertyCalled := false;
  engine.GlobalObject.Evaluate('delete obj.a; obj;', '', 0, Val);
  Check(IsDelPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.a = '');

  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a ="555";', '', 0, Val);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a;', '', 0, Val);
  Check(Val.ToSynUnicode(engine.cx) ='555');
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.a ="777"; obj;', '', 0, Val);
  Check(IsSetPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.a = '777');

  IsDelPropertyCalled := false;
  engine.GlobalObject.Evaluate('delete obj.a; obj;', '', 0, Val);
  Check(IsDelPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.a = '');


  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.b =555;', '', 0, Val);
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);

  IsPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.b;', '', 0, Val);
  Check(Val.ToInteger = 555);
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.b =777; obj;', '', 0, Val);
  Check(IsSetPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.b = 777);

  IsDelPropertyCalled := false;
  engine.GlobalObject.Evaluate('delete obj.b; obj;', '', 0, Val);
  Check(IsDelPropertyCalled);
  engine.MakeObject(val, obj);
  TR := obj.PrivateData;
  Check(TR.b = 0);


  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  ScriptFailed := false;
  try
    engine.GlobalObject.Evaluate('obj.c =555;', '', 0, Val);
  except
    ScriptFailed := true;
  end;
  Check(IsResolveCalled);
  Check(IsPropertyCalled);
  Check(IsSetPropertyCalled);
  Check(ScriptFailed);

  IsPropertyCalled := false;
  engine.GlobalObject.Evaluate('obj.c;', '', 0, Val);
  Check(Val.AsJSVal = JSVAL_NULL);
  Check(IsPropertyCalled);

  IsSetPropertyCalled := false;
  ScriptFailed := false;
  try
    engine.GlobalObject.Evaluate('obj.c ="777"', '', 0, Val);
  except
    ScriptFailed := true;
  end;
  Check(IsSetPropertyCalled);
  Check(ScriptFailed);

  IsDelPropertyCalled := false;
  engine.GlobalObject.Evaluate('delete obj.c', '', 0, Val);
  Check(IsDelPropertyCalled);
  CheckNot(Val.AsBoolean);

  IsResolveCalled := false;
  IsPropertyCalled := false;
  IsSetPropertyCalled := false;
  ScriptFailed := false;
  try
    engine.GlobalObject.Evaluate('obj.d =555;', '', 0, Val);
  except
    ScriptFailed := true;
  end;
  Check(IsResolveCalled);
  CheckNot(IsPropertyCalled);
  CheckNot(IsSetPropertyCalled);
  Check(ScriptFailed);

  IsFinalizeCalled := false;
  FManager.ContentVersion := FManager.ContentVersion+1;
  FManager.ThreadSafeEngine;
  Check(IsFinalizeCalled);
end;

{ TTestSynSM }
procedure TTestSynSM.AbortOnTimeout;
var
  engine: TSMEngine;
begin
  engine := FManager.ThreadSafeEngine;
  engine.TimeoutValue := 2;

  engine.Evaluate('for(var i=0;i<2;i++){}');

  CheckNot(engine.TimeOutAborted);
  CheckNot(engine.ErrorExist);

  try
    engine.Evaluate('for(;;){}');
  except
  end;

  Check(engine.TimeOutAborted);
  Check(engine.ErrorExist);

  engine.TimeoutValue := -1;

end;

procedure TTestSynSM.CreateSmEngine;
var
  engine: TSMEngine;
  smv: TSMValue;
  v: jsval;
  obj: TSMObject;
  propName: SynUnicode;
begin
  FManager := TSMEngineManager.Create;
  // test GC in when small memory accessible to context
  // in production beter to set this property at last to 8*1024*1024 = 8 mb.
  FManager.MaxPerEngineMemory := STACK_CHUNK_SIZE * 128;
  Check(FManager <> nil);
  engine := FManager.ThreadSafeEngine;
  check(engine <> nil);

  smv.AsInteger := 100500;
  engine.GlobalObject.DefineProperty('testInt', smv);
  Check( JS_GetProperty(engine.cx, engine.GlobalObj, 'testInt', v) <> JS_FALSE);
  Check( JSVAL_TO_INT(v) = 100500);
  Check(TSMValue(v).AsInteger = 100500);

  engine.NewObject(obj);
  obj.DefineProperty('testInt2', smv);
  Check( JS_GetProperty(engine.cx, obj.obj, 'testInt2', v) <> JS_FALSE, 'get ansi prop');
  Check (v = smv.AsJSVal);

  propName := 'Медвед';
  obj.DefineProperty(propName, smv);
  Check( JS_GetUCProperty(engine.cx, obj.obj, PjsChar(propName), length(propName), v) <> JS_FALSE, 'get unicode prop');
  Check (v = smv.AsJSVal);
end;

procedure TTestSynSM.SmValueTransformation;
const
  nString: SynUnicode = 'native string';
var
  smv: TSMValue;
  v: jsval;
  dbl: double;
  i, iVal: integer;
  engine: TSMEngine;
  str: SynUnicode;
  u8Str: RawUTF8;
  dt: TDateTime;
begin
  engine := FManager.ThreadSafeEngine;
  for i := 0 to 10000 do begin
    iVal := RandomRange(1, MaxInt-1);

    //double
    dbl := Random * iVal;
    v := DOUBLE_TO_JSVAL(dbl);
    smv.AsDouble := dbl;
    CheckSame(smv.AsDouble, dbl);
    Check(v = smv.AsJSVal);

    // TODO - check only first min(length1, lenght2)-1 chars
    //Check(smv.TransformToString(engine.cx) = DoubleToString(dbl), 'double SM = ' + smv.TransformToString(engine.cx) + ' value = ' + DoubleToString(dbl));

    // integer
    smv.AsInteger := iVal;
    v := INT_TO_JSVAL(iVal);
    Check(smv.AsInteger = iVal);
    Check(v = smv.AsJSVal);
    Check(smv.TransformToSynUnicode(engine.cx) = intToString(iVal));

    // string
    str := RawUnicodeToSynUnicode(RandomString(i));
    smv.SetSynUnicode(engine.cx, str);
    Check(smv.ToSynUnicode(engine.cx) = str);

    // UFT8
    u8Str := WinAnsiConvert.AnsiToUtf8(RandomString(i));
    smv.SetUTF8(engine.cx, u8Str);
    Check(smv.ToUTF8(engine.cx) = u8Str);

    //DateTime
    dt := Now+((iVal mod 100)*0.1);
    smv.SetDateTime(engine.cx, dt);
    CheckSame(smv.ToDateTime(engine.cx),dt,1E-4);

    //TODO Int64

    //TODO variant
  end;
  engine.GarbageCollect;
  for I := 0 to 10000 do begin
    smv.SetNativeString(engine.cx, nString);
    Check(smv.ToSynUnicode(engine.cx) = nString);
  end;
  // free native string reference from SM
  engine.GarbageCollect;

  //boolean
  smv.AsBoolean := True;
  v := BOOLEAN_TO_JSVAL(JS_TRUE);
  Check(smv.AsBoolean);
  Check(v = smv.AsJSVal);
  Check(v = JSVAL_TRUE);

  smv.AsBoolean := False;
  v := BOOLEAN_TO_JSVAL(JS_FALSE);
  Check(not smv.AsBoolean);
  Check(v = smv.AsJSVal);
  Check(v = JSVAL_FALSE);

  Check(smv.SetJSON(engine.cx, '"test\r\nline"'));
  Check(smv.ToUTF8(engine.cx) = 'test'#13#10'line');
  Check(smv.ToJSON(engine.cx) = '"test\r\nline"');

  Check(smv.SetJSON(engine.cx, '{"prop":"strVal"}'));
  Check(smv.ToJSON(engine.cx) = '{"prop":"strVal"}');

end;

procedure TTestSynSM.ScriptEvaluation;
var
  engine: TSMEngine;
  smv: TSMValue;
  obj: TSMObject;
const
  ScriptJSTYPE_NUMBER_WithNaN: String = 'var obj = {};var str, date;str = "aq";date = new Date(str + "Z");date.getTime();obj.caption = date ? date.getDate(): "";';
  TestCaller =
    'var o = {' + #13#10 +
    'f1: function(){ var o = ''f1''+this.f1.caller},' + #13#10 +
    'f2: function(){ var o = ''f2''+this.f2.caller; this.f1()}' + #13#10 +
    '}' + #13#10 +
    'o.f1()' + #13#10 +
    'o.f2()';

begin
  engine := FManager.ThreadSafeEngine;
  engine.GarbageCollect;
  engine.Evaluate('var VaR = "Привет!"');
  Check(engine.GlobalObject.HasProperty('VaR'));
  smv := engine.GlobalObject.GetPropValue('VaR');
  Check( smv.ToSynUnicode(engine.cx) = 'Привет!');
  Check(engine.Global.VaR = 'Привет!');

  if DebugHook=0 then begin // do not trigger unexpected exception within IDE
    try
      engine.Evaluate('this is wrong script');
    except
    end;
    Check( engine.ErrorExist );
    Check (engine.LastErrorMsg = '[JSError 109] script (1): missing ; before statement');

    //check strict mode
    {$ifdef FIXBUGXE3}
    engine.cx.SetOptions(engine.cx.GetOptions - [jsoExtraWarning]);
    {$else}
    engine.cx.Options := engine.cx.Options - [jsoExtraWarning];
    {$endif}
    try
      engine.Evaluate('function sum(a, a, c){ return a + b + c; }');
    except
    end;
    CheckNot( engine.ErrorExist );
    Check (engine.LastErrorMsg = '[JSError 109] script (1): missing ; before statement');
    
    // This test fails without JS_SetNativeStackQuota
  	try
      engine.GlobalObject.Evaluate(TestCaller,'s1',0,smv);
    except
    end;
    CheckNot( engine.ErrorExist );

    {$ifdef FIXBUGXE3}
    engine.cx.SetOptions(engine.cx.GetOptions - [jsoExtraWarning]);
    {$else}
    engine.cx.Options := engine.cx.Options - [jsoExtraWarning];
    {$endif}
    try
      engine.Evaluate(ScriptJSTYPE_NUMBER_WithNaN);
    except
    end;
    CheckNot( engine.ErrorExist );

    {$ifdef FIXBUGXE3}
    engine.cx.SetOptions(engine.cx.GetOptions + [jsoExtraWarning, jsoWError]);
    {$else}
    engine.cx.Options := engine.cx.Options + [jsoExtraWarning, jsoWError];
    {$endif}
    engine.NewObject(obj);
    try
      obj.Evaluate('mistypedVariable = 17;','',111,smv);
    except
    end;
    Check( engine.ErrorExist );
    Check (engine.LastErrorMsg = '[JSError 156] (w/o name) (111): assignment to undeclared variable mistypedVariable');
  end;

  // check JSOPTION_VAROBJFIX
  {$ifdef FIXBUGXE3}
  engine.cx.SetOptions(engine.cx.GetOptions - [jsoVarObjFix]);
  {$else}
  engine.cx.Options := engine.cx.Options - [jsoVarObjFix];
  {$endif}
  engine.NewObject(obj);
  obj.Evaluate('var objProp = 1;','',0,smv);
  CheckNot( engine.GlobalObject.HasOwnProperty('objProp'), 'global has OWN property globalProp');
  Check(    obj.HasOwnProperty('objProp'), 'obj has OWN property objProp');

  {$ifdef FIXBUGXE3}
  engine.cx.SetOptions(engine.cx.GetOptions + [jsoVarObjFix]);
  {$else}
  engine.cx.Options := engine.cx.Options + [jsoVarObjFix];
  {$endif}
  obj.Evaluate('var objPropWFix = 2;','',0,smv);
  //TODO WHY it not work??????
  //Check( engine.globalObject.HasOwnProperty('PropWFix'), 'global has OWN property globalPropWFix');
  Check( obj.HasProperty('objPropWFix'), 'obj has property objPropWFix');
end;


procedure TTestSynSM.LoadMustacheTemplate;
var
  engine: TSMEngine;
  mSource: SynUnicode;
  mustacheFN: TFileName;
  mustache: RawByteString;
  i: integer;
begin
  engine := FManager.ThreadSafeEngine;
  // mustache.js is not strict mode friendly :(
  {$ifdef FIXBUGXE3}
  engine.cx.SetOptions(engine.cx.GetOptions - [jsoExtraWarning]);
  {$else}
  engine.cx.Options := engine.cx.Options - [jsoExtraWarning];
  {$endif}
  mustacheFN := ExeVersion.ProgramFilePath + 'js\mustache.js';
  mSource := AnyTextFileToSynUnicode(mustacheFN);
  if mSource='' then begin
    mustache := TWinINet.Get('https://github.com/janl/mustache.js/raw/master/mustache.js');
    if PosEx('return send(result);',mustache)=0 then begin
      i := PosEx('send(result);',mustache);
      if i>0 then
        insert('return ',mustache,i); // fix syntax error in official libary! :)
    end;
    FileFromString(mustache,mustacheFN);
    mSource := SynUnicode(mustache);
  end;
  Check(mSource <> '', 'exist js\mustache.js');
  engine.Evaluate(mSource, 'mustache.js');
end;

procedure TTestSynSM.RunMustacheTemplate;
var
  engine: TSMEngine;
  mObj, inObj: TSMObject;
  obj,mustache,result: variant;
  inp, outp: TSMValue;
  inArr: SMValArray;
  i: Integer;

begin // try  Mustache w/o compile
  engine := FManager.ThreadSafeEngine;
  Check(engine.GlobalObject.HasProperty('Mustache'), 'mustache exist');
  engine.MakeObject(engine.GlobalObject.GetPropValue('Mustache'), mObj);

  // check first with manual binding
  engine.NewObject(inObj);
  inObj.Root; // must be rooted because it can be destoryed by GC between calls to runMethod
  try
    SetLength(inArr, 2);
    inArr[0].SetSynUnicode(engine.cx, '{{title}} spends {{calc}}');
    inp.SetSynUnicode(engine.cx, 'titleValue');
    inObj.DefineProperty('title', inp);
    for i := 0 to 1000 do begin
      inp.AsInteger := i;
      inObj.DefineProperty('calc', inp);
      inArr[1] := inObj.AsSMValue;
      mObj.RunMethod('render', inArr, outp);
      Check(outp.ToSynUnicode(engine.cx) = 'titleValue spends ' + IntToStr(i));
    end;
  finally
    inObj.UnRoot;
  end;
  // then check with late binding
  TSMVariant.New(mObj,mustache);
  engine.NewSMVariantRooted(obj);
  try
    obj.title := 'titleValue';
    for i := 0 to 1000 do begin
      obj.calc := i;
      result := mustache.render('{{title}} spends {{calc}}',obj);
      Check(result = 'titleValue spends ' + IntToStr(i));
    end;
  finally
    obj._UnRoot; // using pseudo-method
  end;
end;

procedure TTestSynSM.RunTechempowerTemplate;
var
  engine: TSMEngine;
  mSource: SynUnicode;
  inArr: SMValArray;
  outv: TSMValue;
  i: Integer;
  resultFromFortunes, rendered: RawUTF8; // n frtune test here must be resultFromFortunes of database query
begin
  engine := FManager.ThreadSafeEngine;
  mSource := AnyTextFileToSynUnicode(ExeVersion.ProgramFilePath + 'js\precompiledMustache.js');
  if mSource='' then
    exit;
  CheckFailed(mSource <> '', 'exist js\precompiledMustache.js');
  engine.Evaluate(mSource, 'precompiledMustache.js');
  setLength(inArr, 1);
  for i := 0 to 1000 do begin
    resultFromFortunes := FormatUTF8('[{"id": %, "message": "message%"},{"id": %, "message": "message%"}]', [i, i, i+1, i+1]);
    inArr[0].SetUTF8(engine.cx, resultFromFortunes);
    engine.GlobalObject.RunMethod('precompiledMustache', inArr, outv);

    rendered := FormatUTF8('<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>'+
      '<tr><td>%</td><td>message%</td></tr><tr><td>%</td><td>message%</td></tr></table></body></html>', [i, i, i+1, i+1]);
    Check(rendered = outv.ToUTF8(engine.cx));
  end;
end;


destructor TTestSynSM.Destroy;
begin
  FManager.Free;
  inherited;
end;

end.
