{#(@)$Id: TestFramework.pas,v 1.116 2005/06/06 22:36:52 skaug Exp $ }
{  DUnit: An XTreme testing framework for Delphi programs. }
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)
{$IFDEF VER180}
  //VER180 = Delphi 2006 for Win32
  //Don't define DELPHI71_OR_LATER for Delphi 2006 for Win32.
  {$UNDEF DELPHI71_OR_LATER}
  {$DEFINE DELPHI6_OR_LATER}
  {$DEFINE DELPHI7_OR_LATER}
{$ENDIF}
{$IFDEF VER170}
  //VER170 = Delphi 2005 for Win32
  //Don't define DELPHI71_OR_LATER for Delphi 2005 for Win32.
  {$UNDEF DELPHI71_OR_LATER}
  {$DEFINE DELPHI6_OR_LATER}
  {$DEFINE DELPHI7_OR_LATER}
{$ENDIF}
{$IFDEF VER150}
  {$IFNDEF DELPHI70_MODE}
    {$DEFINE DELPHI71_OR_LATER}
    //If you are using Delphi 7.0 (not 7.1), then specify DELPHI70_MODE symbol in "Project/Options/Conditional defines" - Delphi 7.1 has build no. 4.453
  {$ENDIF}
  {$DEFINE DELPHI7_OR_LATER}
  {$DEFINE DELPHI6_OR_LATER}
  {$WARNINGS OFF}	//We probably don't want to hear about warnings - Not sure about that
{$ENDIF}
{$IFDEF VER140}
	{$DEFINE DELPHI6_OR_LATER}
{$ENDIF}
{$IFDEF DELPHI6_OR_LATER}
	{$WARN UNIT_PLATFORM OFF}	//NOT certified for Kylix
	{$WARN SYMBOL_PLATFORM OFF}
	{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF CLR}
  {$UNSAFECODE ON}
{$ENDIF}
{$BOOLEVAL OFF}
unit TestFramework;

interface
uses
{$IFDEF CLR}System.Reflection,{$ENDIF}
  SysUtils,
  Classes,
  IniFiles;

const
  rcs_id: string = '#(@)$Id: TestFramework.pas,v 1.116 2005/06/06 22:36:52 skaug Exp $';
  rcs_verion : string = '$Revision: 1.116 $';

type
{$IFDEF CLR}
//  Pointer = Borland.Delphi.System.Pointer;
  IUnknown = interface(IInterface)
  end;

  TestAttribute = class(TCustomAttribute)
  end;
{$ENDIF}

{$IFDEF CLR}
  TTestMethod  = string;
{$ELSE}
  TTestMethod  = procedure of object;
{$ENDIF}
  TTestProc    = procedure;

  TTestCaseClass  = class of TTestCase;

  ITestListener   = interface;
  IStatusListener = interface;

  TTestResult   = class;
  TAbstractTest = class;
  TTestCase     = class;
  TTestSuite    = class;
  TTestFailure  = class;

  ExceptionClass = class of Exception;

  ETestFailure = class(EAbort)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;

  EDunitException = class(Exception);
  ETestError = class(EDunitException);
  EStopTestsFailure = class(ETestFailure);


  { thrown to force a debugger break on a test failure }
  EBreakingTestFailure = class(EDunitException)
     constructor Create;               overload;
     constructor Create(msg :string);  overload;
  end;


  ITest = interface(IUnknown)
    ['{89CCD557-7DE1-4814-B033-ABAFE0870EC7}']
    function GetName: string;

    function  CountTestCases: integer;
    function  CountEnabledTestCases: integer;
    function  Tests: IInterfaceList;


    procedure SetUp;
    procedure TearDown;

    function  Run : TTestResult;  overload;
    procedure Run(testResult: TTestResult); overload;

    procedure RunWithFixture(testResult: TTestResult);
    procedure RunTest(testResult: TTestResult);

    function  GetEnabled: boolean;
    procedure SetEnabled(Value: boolean);

    procedure SetStartTime(Value :Int64);
    function  GetStartTime : Int64;

    procedure SetStopTime(Value :Int64);
    function  GetStopTime : Int64;
    function  ElapsedTestTime: Cardinal;


    procedure SetStatusListener(Listener :IStatusListener);
    function  GetStatus :string;

    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;
    procedure LoadConfiguration(const fileName: string; const useRegistry, useMemIni: boolean); overload;

    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload;
    procedure SaveConfiguration(const fileName: string; const useRegistry, useMemIni: boolean); overload;

    procedure SetGUIObject(const guiObject: TObject);
    function  GetGUIObject: TObject;

    property Name:    string  read GetName;
    property Enabled: boolean read GetEnabled write SetEnabled;
    property GUIObject: TObject read GetGUIObject write SetGUIObject;
    property Status:  string  read GetStatus;

    property StartTime: Int64 read GetStartTime write SetStartTime;
    property StopTime:  Int64 read GetStopTime  write SetStopTime;
  end;


  {: General interface for test decorators}
  ITestDecorator = interface(ITest)
    ['{8B3FC229-3033-4590-AD5C-01914C6E2C9F}']
    {: Get the decorated test
    @return The decorated test }
    function GetTest: ITest;
    property Test: ITest read GetTest;
  end;

  { IStatusListeners are notified of test status messages }
  IStatusListener = interface
  ['{8681DC88-033C-4A42-84F4-4C52EF9ABAC0}']
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test: ITest; const Msg: string);
  end;

  { ITestListeners get notified of testing events.
    See TTestResult.AddListener()
  }
  ITestListener = interface(IStatusListener)
    ['{114185BC-B36B-4C68-BDAB-273DBD450F72}']

    procedure TestingStarts;
    procedure StartTest(test: ITest);

    procedure AddSuccess(test: ITest);
    procedure AddError(error: TTestFailure);
    procedure AddFailure(Failure: TTestFailure);

    procedure EndTest(test: ITest);
    procedure TestingEnds(testResult :TTestResult);

    function  ShouldRunTest(test :ITest):boolean;
  end;


  ITestListenerX = interface(ITestListener)
    ['{5C28B1BE-38B5-4D6F-AA96-A04E9302C317}']

    procedure StartSuite(suite: ITest);
    procedure EndSuite(suite: ITest);
  end;

  // a named collection of tests
  ITestSuite = interface(ITest)
    ['{C20E38EF-7369-44D9-9D84-08E84EC1DCF0}']

    procedure AddTest(test: ITest);
    procedure AddSuite(suite : ITestSuite);
  end;

  {  Adapter to allow a TTestResult to receive status messages
     from the running test }
  TStatusToResultAdapter = class(TInterfacedObject, IStatusListener)
  protected
    FTestResult :TTestResult;
  public
    constructor Create(TestResult :TTestResult);
    procedure   Status(Test :ITest; const Msg :string);
    procedure   Warning(Test: ITest; const Msg: string);
  end;

  { A TTestResult collects the results of executing a test case.
  And notifies registered ITestListener of testing events. }
  TTestResult = class(TObject)
  private
    FTotalTime: Int64;
  protected
    fFailures: TList;
    fErrors: TList;
    fWarnings: TStrings;
    fListeners: IInterfaceList;
    FRootTest: ITest;
    fRunTests: integer;
    fStop: boolean;
    FBreakOnFailures :boolean;

    FStatusAdapter :IStatusListener;

    procedure Run(test: ITest); virtual;
    function  RunTestSetup(test: ITest):boolean; virtual;
    procedure RunTestTearDown(test: ITest); virtual;
    function  RunTestRun(test: ITest) : boolean; virtual;

    procedure TestingStarts;                           virtual;
    procedure StartSuite(suite: ITest);                virtual;
    procedure StartTest(test: ITest);                  virtual;
    function  ShouldRunTest(test :ITest) :boolean;     virtual;
    procedure Status(test :ITest; const Msg :string);  virtual;
    procedure Warning(test: ITest; const Msg: string); virtual;
    procedure EndSuite(suite: ITest);                  virtual;
    procedure EndTest(test: ITest);                    virtual;
    procedure TestingEnds;                             virtual;
  public

    constructor Create;
    destructor  Destroy; override;

    procedure AddListener(listener: ITestListener); virtual;

    procedure RunSuite(test: ITest);  overload;
    procedure AddSuccess(test: ITest);                                                              virtual;
    function  AddFailure(test: ITest; e: Exception; addr :Pointer): TTestFailure;                   virtual;
    function  AddError(  test: ITest; e: Exception; addr :Pointer; msg :string = ''): TTestFailure; virtual;


    procedure Stop; virtual;
    function  ShouldStop: boolean; virtual;

    function RunCount: integer;     virtual;
    function ErrorCount: integer;   virtual;
    function FailureCount: integer; virtual;
    function WarningCount: integer; virtual;

    function  GetError(Index :Integer) :TTestFailure;
    function  GetFailure(Index :Integer) :TTestFailure;
    function  GetWarning(Index :Integer) :string;

    function  WasStopped :boolean; virtual;
    function  WasSuccessful: boolean; virtual;

    property  BreakOnFailures :boolean read  FBreakOnFailures write FBreakOnFailures;
    property  TotalTime: Int64 read FTotalTime;

    property Errors[i :Integer] :TTestFailure read GetError;
    property Failures[i :Integer] :TTestFailure read GetFailure;
    property Warnings[i :Integer] :string read GetWarning;
  end;


  TAbstractTest = class(TInterfacedObject, ITest)
  protected
    FTestName: string;
    fEnabled: boolean;

    fStartTime: Int64;
    fStopTime:  Int64;

    FStatusListener :IStatusListener;
    FStatusStrings  :TStrings;

    FExpectedException: ExceptionClass;

    // Object used by the GUI to map the test onto a GUI object such as a tree node
    FGUIObject: TObject;

    procedure Invoke(AMethod: TTestMethod); virtual;
    procedure RunWithFixture(testResult: TTestResult); virtual;
    procedure RunTest(testResult: TTestResult); virtual;

    procedure SetUp; virtual;
    procedure TearDown; virtual;

    procedure SetStartTime(Value :Int64); virtual;
    function  GetStartTime : Int64;       virtual;

    procedure SetStopTime(Value :Int64);  virtual;
    function  GetStopTime : Int64;        virtual;

    procedure SetGUIObject(const guiObject: TObject);
    function  GetGUIObject: TObject;

    {$IFNDEF CLR} // related to Check(Not)EqualsMem, pointer based, unsuitable for .NET
    function GetMemDiffStr(expected, actual: pointer; size:longword; msg:string):string;
    {$ENDIF}

  public
    constructor Create(Name: string);
    destructor Destroy; override;

    function GetName: string; virtual;

    function  GetEnabled: boolean; virtual;
    procedure SetEnabled(value: boolean); virtual;

    function  Tests: IInterfaceList; virtual;

    function  CountTestCases: integer; virtual;
    function  CountEnabledTestCases: integer; virtual;

    function  Run: TTestResult; overload;
    procedure Run(testResult: TTestResult); overload;

    function  ElapsedTestTime: Cardinal; virtual;

    procedure SetStatusListener(Listener :IStatusListener);
    procedure Status(const Msg :string);
    function  GetStatus :string;

    procedure LoadConfiguration(const fileName: string; const useRegistry, useMemIni: boolean); overload;
    procedure LoadConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;
    procedure SaveConfiguration(const fileName: string; const useRegistry, useMemIni: boolean); overload;
    procedure SaveConfiguration(const iniFile :TCustomIniFile; const section :string);  overload; virtual;


    property Name:    string  read GetName;
    property Enabled: boolean read GetEnabled write SetEnabled;


    function  BoolToStr(ABool: boolean): string;

    procedure Check(condition: boolean; msg: string = ''); virtual;
    procedure CheckTrue(condition: boolean; msg: string = ''); virtual;
    procedure CheckFalse(condition: boolean; msg: string = ''); virtual;
    procedure CheckEquals(expected, actual: extended; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: extended; delta: extended; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckEqualsString(expected, actual: string; msg: string = ''); virtual;
{$IFNDEF CLR}
{$IFDEF DELPHI6_OR_LATER}
    procedure CheckEquals(expected, actual: WideString; msg: string = ''); overload; virtual;
{$ENDIF}
    procedure CheckEqualsWideString(expected, actual: WideString; msg: string = ''); virtual;
    procedure CheckEqualsMem(expected, actual: pointer; size:longword; msg:string=''); virtual;
{$ENDIF}
    procedure CheckEquals(expected, actual: boolean; msg: string = ''); overload; virtual;
    procedure CheckEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotEquals(expected, actual: integer; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = ''); overload; virtual;
    procedure CheckNotEquals(expected, actual: string; msg: string = ''); overload; virtual;
    procedure CheckNotEqualsString(expected, actual: string; msg: string = ''); virtual;
{$IFNDEF CLR}
{$IFDEF DELPHI6_OR_LATER}
    procedure CheckNotEquals(const expected, actual: WideString; msg: string = ''); overload; virtual;
{$ENDIF}
    procedure CheckNotEqualsWideString(const expected, actual: WideString; msg: string = ''); virtual;
    procedure CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string=''); virtual;
{$ENDIF}
    procedure CheckNotEquals(expected, actual: boolean; msg: string = ''); overload; virtual;
    procedure CheckNotEqualsBin(expected, actual: longword; msg: string = ''; digits: integer=32); virtual;
    procedure CheckNotEqualsHex(expected, actual: longword; msg: string = ''; digits: integer=8); virtual;

    procedure CheckNotNull(obj :IUnknown; msg :string = ''); overload; virtual;
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckSame(expected, actual: TObject; msg: string = ''); overload; virtual;

    procedure CheckNotNull(obj: TObject; msg: string = ''); overload; virtual;
    procedure CheckNull(obj: TObject; msg: string = ''); overload; virtual;

    procedure CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string = '');
    procedure CheckEquals(  expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckInherits(expected, actual: TClass; msg: string = ''); overload; virtual;
    procedure CheckIs(AObject :TObject; AClass: TClass; msg: string = ''); overload; virtual;

    procedure Fail(msg: sTring; errorAddr: Pointer = nil); overload; virtual;
    procedure FailEquals(expected, actual: WideString; msg: string = ''; errorAddr: Pointer = nil); virtual;
    procedure FailNotEquals(expected, actual: WideString; msg: string = ''; errorAddr: Pointer = nil); virtual;
    procedure FailNotSame(expected, actual: WideString; msg: string = ''; errorAddr: Pointer = nil); virtual;

    function EqualsErrorMessage(expected, actual :WideString; msg: string): WideString;
    function NotEqualsErrorMessage(expected, actual :WideString; msg: string): WideString;
    function NotSameErrorMessage(expected, actual, msg: string): WideString;

    procedure StopTests(msg: string = ''); virtual;

{$IFNDEF CLR}
    procedure CheckMethodIsNotEmpty(MethodPointer: pointer);
{$ENDIF}    

    procedure StartExpectingException(e: ExceptionClass);
    procedure StopExpectingException(msg :string = '');

    property ExpectedException :ExceptionClass
      read  fExpectedException
      write StartExpectingException;
  end;


  TTestCase = class(TAbstractTest, ITest)
  protected
    fMethod:    TTestMethod;

    procedure Invoke(AMethod: TTestMethod); override;
    procedure RunWithFixture(testResult: TTestResult); override;
    procedure RunTest(testResult: TTestResult); override;

  public
    constructor Create(MethodName: string); virtual;

    class function Suite: ITestSuite; virtual;

    procedure Run(testResult: TTestResult); overload;
  published
  end;


  TTestSuite = class(TAbstractTest, ITestSuite, ITest)
  protected
    fTests: IInterfaceList;
  public
    constructor Create; overload;
    constructor Create(Name: string); overload;
    constructor Create(TestClass: TTestCaseClass); overload;
    constructor Create(Name: string; const Tests: array of ITest); overload;

    function CountTestCases: integer;         override;
    function CountEnabledTestCases: integer;  override;

    function Tests: IInterfaceList;                 override;
    procedure AddTest(ATest: ITest);                virtual;
    procedure AddTests(testClass: TTestCaseClass);  virtual;
    procedure AddSuite(suite:  ITestSuite);         virtual;

    procedure RunTest(testResult: TTestResult); override;

    procedure LoadConfiguration(const iniFile: TCustomIniFile; const section: string);  override;
    procedure SaveConfiguration(const iniFile: TCustomIniFile; const section: string);  override;
  end;


  TTestFailure = class(TObject)
  protected
    fFailedTest: ITest;
    fThrownExceptionClass: TClass;
    fThrownExceptionMessage: string;
    FThrownExceptionAddress: Pointer;
    FStackTrace:             string;

    procedure CaptureStackTrace;
  public
    constructor Create(failedTest: ITest; thrownException: Exception; Addr: Pointer; msg: string = '');

    function FailedTest: ITest; virtual;
    function ThrownExceptionClass: TClass; virtual;
    function ThrownExceptionName: string; virtual;
    function ThrownExceptionMessage: string; virtual;
    function ThrownExceptionAddress: pointer; virtual;

    function LocationInfo: string; virtual;
    function AddressInfo:  string; virtual;

    function StackTrace:   string; virtual;
  end;


  TMethodEnumerator = class
  protected
    FMethodNameList:  array of string;
    function GetNameOfMethod(Index: integer):  string;
    function GetMethodCount: Integer;
  public
    constructor Create(AClass: TClass);
    property MethodCount: integer read GetMethodCount;
    property NameOfMethod[index:  integer]: string read GetNameOfMethod;
  end;


// creating suites
function  TestSuite(name: string; const Tests: array of ITest): ITestSuite;

// test registry
procedure RegisterTest(SuitePath: string; test: ITest); overload;
procedure RegisterTest(test: ITest);                    overload;
procedure RegisterTests(SuitePath: string; const Tests: array of ITest);  overload;
procedure RegisterTests(const Tests: array of ITest);                     overload;
function  RegisteredTests: ITestSuite;
procedure ClearRegistry;

// running tests
function RunTest(suite: ITest; listeners: array of ITestListener): TTestResult; overload;
function RunRegisteredTests(listeners: array of ITestListener): TTestResult;


// utility routines
function CallerAddr: Pointer; {$IFNDEF CLR} assembler; {$ENDIF}
function PtrToStr(p: Pointer): string;
function PointerToLocationInfo(Addr: Pointer): string;
function PointerToAddressInfo(Addr: Pointer):  string;
function IsTestMethod(aTest: ITest): Boolean;
function IsDecorator(aTest: ITest): Boolean;
function GetDUnitRegistryKey: string;
procedure SetDUnitRegistryKey(const NewKey: string);
{$IFNDEF CLR}  // - unsuitable for .NET, pointer magic
function FirstByteDiff(p1, p2: pointer; size: longword; out b1, b2: byte): integer;
{$ENDIF}


//  strings, used in TAbstractTestCase.EqualsErrorMessage etc.:
const sExpButWasFmt    = '%sexpected: <%s> but was: <%s>';
      sExpAndActualFmt = '%sexpected and actual were: <%s>';


///////////////////////////////////////////////////////////////////////////
implementation
uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
  Registry,
{$ENDIF}
{$IFDEF USE_JEDI_JCL}
 JclDebug,
 {$ENDIF}
 TypInfo;

{$STACKFRAMES ON} //required to retreive caller's address

type
  TMemIniFileTrimmed = class(TMemIniFile)
  public
    // Override the read string method to trim the string for compatibility with TIniFile
    function ReadString(const Section, Ident, Default: string): string; override;
  end;

var
  // SubKey of HKEY_CURRENT_USER for storing configurations in the registry (end with \)
  DUnitRegistryKey: string = ''; // How about 'Software\DUnitTests\';

{$IFDEF LINUX}

var
  PerformanceCounterInitValue: Int64;

procedure InitPerformanceCounter;
var
  TV : TTimeVal;
  TZ : TTimeZone;
begin
  gettimeofday(TV, TZ);
  PerformanceCounterInitValue :=
    LongWord(TV.tv_sec mod (24*60*60) * 1000) + (LongWord(TV.tv_usec) div 1000);
end;

function QueryPerformanceCounter(var PerformanceCounter: Int64): LongBool;
var
  TV : TTimeVal;
  TZ : TTimeZone;
begin
  gettimeofday(TV, TZ);
  PerformanceCounter := (TV.tv_sec mod (24*60*60) * 1000) +
            (TV.tv_usec div 1000);
  PerformanceCounter := PerformanceCounter - PerformanceCounterInitValue;
  Result := true;
end;

function QueryPerformanceFrequency(var Frequency: Int64): LongBool;
begin
  Frequency := 1000;
  Result := true;
end;
{$ENDIF}

{: Convert a pointer into its string representation }
function PtrToStr(p: Pointer): string;
begin
   Result := Format('%p', [p])
end;

function IsBadPointer(P: Pointer):boolean; {$IFNDEF CLR} register; {$ENDIF}
begin
  try
    Result  := (p = nil)
{$IFNDEF CLR}
              or ((Pointer(P^) <> P) and (Pointer(P^) = P));
{$ENDIF}
  except
    Result := true;
  end
end;


function CallerAddr: Pointer; {$IFNDEF CLR} assembler; {$ENDIF}
{$IFDEF CLR}
begin
  Result := nil;
end;
{$ELSE}
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ENDIF}

{$IFNDEF USE_JEDI_JCL}
function PointerToLocationInfo(Addr: Pointer): string;
begin
 Result := ''
end;

function PointerToAddressInfo(Addr: Pointer): string;
begin
 Result := '$'+PtrToStr(Addr);
end;

{$ELSE}
function PointerToLocationInfo(Addr: Pointer): string;
var
  _file,
  _module,
  _proc: AnsiString;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addr, _file, _module, _proc, _line);

  if _file <> '' then
    Result   := Format('%s:%d', [_file, _line])
  else
    Result   := _module;
end;

function PointerToAddressInfo(Addr: Pointer): string;
var
  _file,
  _module,
  _proc: AnsiString;
  _line: integer;
begin
  JclDebug.MapOfAddr(Addr, _file, _module, _proc, _line);
  Result := Format('%s$%p', [_proc, Addr]);
end;
{$ENDIF}

function IsTestMethod(aTest: ITest): Boolean;
var
  aTestSuite: ITestSuite;
  aTestDecorator: ITestDecorator;
begin
  Assert(Assigned(aTest));

  // Initialize to be sure
  aTestSuite := nil;
  aTestDecorator := nil;

  { The test should be a normal testmethod
    when the testcount = 1 }
  Result := (aTest.CountTestCases = 1);

  // But not when the test is a suite? (It could have one test.)
{$IFDEF CLR}
  if Supports(aTest, ITestSuite) or Supports(aTest, ITestDecorator) then
    Result := false;
{$ELSE}
  aTest.QueryInterface(ITestSuite, aTestSuite);
  if Assigned(aTestSuite) then
    Result := false;

  // And not when the test is a decorator?
  aTest.QueryInterface(ITestDecorator, aTestDecorator);
  if Assigned(aTestDecorator) then
    Result := false;
{$ENDIF}
end;

function IsDecorator(aTest: ITest): Boolean;
var
  aTestDecorator: ITestDecorator;
begin
  Assert(Assigned(aTest));

  // Initialize to be sure
  aTestDecorator := nil;

{$IFDEF CLR}
  Result := Supports(aTest, ItestDecorator);
{$ELSE}
  aTest.QueryInterface(ITestDecorator, aTestDecorator);
  Result := Assigned(aTestDecorator);
{$ENDIF}
end;

function GetDUnitRegistryKey: string;
begin
  Result := DUnitRegistryKey;
end;

procedure SetDUnitRegistryKey(const NewKey: string);
begin
  DUnitRegistryKey := NewKey;
end;

{$IFNDEF CLR} // KGS: not expected to work in .NET, pointer magic follows
function ByteAt(p: pointer; const Offset: integer): byte;
begin
  Result:=pByte(integer(p)+Offset)^;
end;

function FirstByteDiff(p1, p2: pointer; size: longword; out b1, b2: byte): integer;
// Returns offset of first byte pair (left to right, incrementing address) that is unequal
// Returns -1 if no difference found, or if size=0
var
  i: integer;
begin
  Result:=-1;
  if size>0 then
  for i:=0 to size-1 do // Subject to optimisation for sure:
    if ByteAt(p1,i)<>ByteAt(p2,i) then
    begin
      Result:=i;
      b1:=ByteAt(p1,i);
      b2:=ByteAt(p2,i);
      break;
    end;
end;
{$ENDIF}


{ TTestResult }

constructor TTestResult.Create;
begin
  inherited Create;
  fFailures := TList.Create;
  fErrors := TList.Create;
  fListeners := TInterfaceList.Create;
  fRunTests := 0;
  fStop := false;

  FStatusAdapter := TStatusToResultAdapter.Create(Self);
end;

destructor TTestResult.destroy;
var
  i: Integer;
begin
  fListeners := nil;
  for i := 0 to fErrors.Count - 1 do
  begin
    TTestFailure(fErrors[i]).Free;
  end;
  fErrors.Free;
  for i := 0 to fFailures.Count - 1 do
  begin
    TTestFailure(fFailures[i]).Free;
  end;
  fFailures.Free;
  inherited Destroy;
end;

procedure TTestResult.AddSuccess(test: ITest);
var
  i: integer;
begin
  assert(assigned(test));
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddSuccess(test);
  end;
end;

function TTestResult.AddError(test: ITest; e: Exception; addr: Pointer; msg: string): TTestFailure;
var
  i: integer;
  error:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fErrors));

  error := TTestFailure.Create(test, e, addr, msg);
  fErrors.add(error);
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddError(error);
  end;

  assert(assigned(error));
  Result := error;
end;

function TTestResult.AddFailure(test: ITest; e: Exception; addr: Pointer): TTestFailure;
var
  i: integer;
  Failure:  TTestFailure;
begin
  assert(assigned(test));
  assert(assigned(e));
  assert(assigned(fFailures));

  Failure := TTestFailure.Create(test, e, addr);
  fFailures.add(Failure);
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).AddFailure(Failure);
  end;

  assert(assigned(Failure));
  Result := Failure;
end;

procedure TTestResult.addListener(listener: ITestListener);
begin
  assert(assigned(listener), 'listener is nil');
  fListeners.add(listener);
end;

procedure TTestResult.EndTest(test: ITest);
var
  i: integer;
begin
  assert(assigned(fListeners));

  try
    for i := 0 to fListeners.count - 1 do
    begin
      (fListeners[i] as ITestListener).EndTest(test);
    end;
  finally
    test.SetStatusListener(nil);
  end;
end;

procedure TTestResult.Status(test: ITest; const Msg: string);
var
  i: integer;
begin
  assert(assigned(fListeners));

  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).Status(test, Msg);
  end;
end;

procedure TTestResult.Warning(test: ITest; const Msg: string);
begin
  assert(assigned(fWarnings));
  fWarnings.Add(Msg);
end;

function TTestResult.GetError(Index :Integer): TTestFailure;
begin
  Result := TObject(FErrors[Index]) as TTestFailure;
end;

function TTestResult.GetFailure(Index :Integer): TTestFailure;
begin
  Result := TObject(FFailures[Index]) as TTestFailure;
end;

function TTestResult.GetWarning(Index: Integer): string;
begin
  Result := fWarnings[Index];
end;

function TTestResult.RunTestSetup(test: ITest):boolean;
var
  Time :Int64;
begin
  try
    test.StopTime  := 0;
    QueryPerformanceCounter(Time);
    test.StartTime := Time;
    test.SetUp;
    Result := true;
  except
    on e: Exception do
    begin
      AddError(test, e, ExceptAddr, 'SetUp FAILED: ');
      Result := false;
    end
  end;
end;

procedure TTestResult.RunTestTearDown(test: ITest);
var
  Time :Int64;
begin
  try
    test.TearDown;
  except
    on e: Exception do
      AddError(test, e, ExceptAddr, 'TearDown FAILED: ');
  end;
  QueryPerformanceCounter(Time);
  test.StopTime := Time;
end;

function TTestResult.RunTestRun(test: ITest) : boolean;
var
  failure: TTestFailure;
begin
  Result := false;
  failure := nil;
  {$IFDEF USE_JEDI_JCL}
  try
    JclStartExceptionTracking;
  {$ENDIF}
    try
      test.RunTest(self);
      fTotalTime := FRootTest.ElapsedTestTime;
      AddSuccess(test);
      Result := true;
    except
      on e: EStopTestsFailure do
      begin
        failure := AddFailure(test, e, ExceptAddr);
        FStop := True;
      end;
      on e: ETestFailure do
      begin
        failure := AddFailure(test, e, ExceptAddr);
      end;
      on e: EBreakingTestFailure do
      begin
        failure := AddFailure(test, e, ExceptAddr);
      end;
      on e: Exception do
      begin
        failure := AddError(test, e, ExceptAddr);
      end;
    end;
  {$IFDEF USE_JEDI_JCL}
  finally
    JclStopExceptionTracking;
  end;
  {$ENDIF}
  if BreakOnFailures
  and (failure <> nil)
  and (failure.FThrownExceptionClass.InheritsFrom(ETestFailure))
  then
  begin
    try
       raise EBreakingTestFailure.Create(failure.ThrownExceptionMessage)
          {$IFNDEF CLR}at failure.ThrownExceptionAddress{$ENDIF};
    except
    end;
  end;
end;

procedure TTestResult.Run(test: ITest);
begin
  assert(assigned(test));
  if not ShouldStop and ShouldRunTest(test) then
  begin
    StartTest(test);
    try
      if RunTestSetUp(test) then
      begin
        RunTestRun(test);
      end;
      RunTestTearDown(test);
    finally
      EndTest(test);
    end;
  end;
end;

function TTestResult.RunCount: integer;
begin
  result := fRunTests;
end;

function TTestResult.ShouldStop: boolean;
begin
  result := fStop;
end;

procedure TTestResult.StartTest(test: ITest);
var
  i: integer;
begin
  assert(assigned(test));
  assert(assigned(fListeners));

  test.SetStatusListener(FStatusAdapter);

  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).StartTest(test);
  end;
end;

procedure TTestResult.Stop;
begin
  fStop := true;
end;

function TTestResult.ErrorCount: integer;
begin
  assert(assigned(fErrors));

  result := fErrors.count;
end;

function TTestResult.FailureCount: integer;
begin
  assert(assigned(fFailures));

  result := fFailures.count;
end;

function TTestResult.WarningCount: integer;
begin
  assert(assigned(fWarnings));

  result := fWarnings.count;
end;

function TTestResult.WasSuccessful: boolean;
begin
  result := (FailureCount = 0) and (ErrorCount() = 0) and not WasStopped;
end;

procedure TTestResult.TestingStarts;
var
  i: Integer;
begin
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).TestingStarts;
  end;
end;

procedure TTestResult.TestingEnds;
var
  i: Integer;
begin
  for i := 0 to fListeners.count - 1 do
  begin
    (fListeners[i] as ITestListener).TestingEnds(self);
  end;
end;

function TTestResult.ShouldRunTest(test: ITest): boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to fListeners.count - 1 do
  begin
    if not (fListeners[i] as ITestListener).ShouldRunTest(test) then
    begin
      Result := false;
      break;
    end;
  end;
end;


function TTestResult.WasStopped: boolean;
begin
  result := fStop;
end;

procedure TTestResult.RunSuite(test: ITest);
begin
  TestingStarts;
  try
    FRootTest := test;
    test.RunWithFixture(self);
  finally
    TestingEnds
  end
end;

procedure TTestResult.EndSuite(suite: ITest);
var
  i: Integer;
  l: ITestListenerX;
begin
  for i := 0 to fListeners.count - 1 do
  begin
{$IFDEF CLR}
    if Supports(fListeners[i], ITestListenerX, l) then
{$ELSE}
    if fListeners[i].QueryInterface(ITestListenerX, l) = 0 then
{$ENDIF}
       l.EndSuite(suite);
  end;
end;

procedure TTestResult.StartSuite(suite: ITest);
var
  i: Integer;
  l: ITestListenerX;
begin
  for i := 0 to fListeners.count - 1 do
  begin
{$IFDEF CLR}
    if Supports(fListeners[i], ITestListenerX, l) then
{$ELSE}
    if fListeners[i].QueryInterface(ITestListenerX, l) = 0 then
{$ENDIF}
      l.StartSuite(suite);
  end;
end;

{ TStatusToResultAdapter }

constructor TStatusToResultAdapter.Create(TestResult: TTestResult);
begin
  Assert(TestResult <> nil, 'Expected non nil TestResult');
  inherited Create;

  FTestResult := TestResult;
end;

procedure TStatusToResultAdapter.Status(Test: ITest; const Msg: string);
begin
  FTestResult.Status(Test, Msg);
end;

procedure TStatusToResultAdapter.Warning(Test: ITest; const Msg: string);
begin
  FTestResult.Warning(Test, Msg);
end;

{ TAbstractTest }

constructor TAbstractTest.Create(Name: string);
begin
  inherited Create;
  FTestName := Name;
  FEnabled  := true;
end;

destructor TAbstractTest.Destroy;
begin
  FStatusStrings.Free;
  inherited;
end;

procedure TAbstractTest.Invoke(AMethod: TTestMethod);
begin
end;

procedure TAbstractTest.Run(testResult: TTestResult);
begin
  testResult.RunSuite(self);
end;

function TAbstractTest.CountEnabledTestCases: integer;
begin
  if GetEnabled then
    Result := 1
  else
    Result := 0
end;

function TAbstractTest.CountTestCases: integer;
begin
  Result := 1;
end;

function TAbstractTest.getEnabled: boolean;
begin
  Result := fEnabled
end;

function TAbstractTest.GetName: string;
begin
  Result := fTestName
end;

procedure TAbstractTest.LoadConfiguration(const fileName: string; const useRegistry, useMemIni: boolean);
var
  f: TCustomIniFile;
begin
{$IFNDEF LINUX}
  if useRegistry then
    f := TRegistryIniFile.Create(DUnitRegistryKey + fileName)
  else
{$ENDIF}
    if useMemIni then
      f := TMemIniFileTrimmed.Create(fileName)
    else
      f := TIniFile.Create(fileName);

  try
    LoadConfiguration(f, 'Tests')
  finally
    f.free
  end
end;

procedure TAbstractTest.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  self.setEnabled(iniFile.readBool(section, self.GetName, True));
end;

procedure TAbstractTest.SaveConfiguration(const fileName: string; const useRegistry, useMemIni: boolean);
var
  f: TCustomIniFile;
begin
{$IFNDEF LINUX}
  if useRegistry then
    f := TRegistryIniFile.Create(DUnitRegistryKey + fileName)
  else
{$ENDIF}
    if useMemIni then
      f := TMemIniFileTrimmed.Create(fileName)
    else
      f := TIniFile.Create(fileName);

  try
    SaveConfiguration(f, 'Tests');
    f.UpdateFile;
  finally
    f.free
  end
end;

procedure TAbstractTest.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
begin
  if self.GetEnabled then
    iniFile.deleteKey(section, self.GetName)
  else
    iniFile.writeBool(section, self.GetName, False);
end;

function TAbstractTest.Run: TTestResult;
var
  testResult:  TTestResult;
begin
  testResult := TTestResult.Create;
  try
    testResult.RunSuite(self);
  except
    testResult.Free;
    raise;
  end;
  Result := testResult;
end;

procedure TAbstractTest.setEnabled(value: boolean);
begin
  fEnabled := value;
end;

var
  EmptyTestList: IInterfaceList = nil;

function TAbstractTest.Tests: IInterfaceList;
begin
   if EmptyTestList = nil then
     EmptyTestList := TInterfaceList.Create;
   Result := EmptyTestList;
end;


function TAbstractTest.GetStartTime: Int64;
begin
  Result := FStartTime
end;

procedure TAbstractTest.SetStartTime(Value: Int64);
begin
  FStartTime := Value;
end;

procedure TAbstractTest.SetStopTime(Value: Int64);
begin
  FStopTime := Value;
end;

function TAbstractTest.GetStopTime: Int64;
begin
  Result := FStopTime;
end;

procedure TAbstractTest.SetUp;
begin
 // do nothing
end;

procedure TAbstractTest.TearDown;
begin
  // do nothing
end;

procedure TAbstractTest.RunTest(testResult: TTestResult);
begin
  // do nothing
end;

function TAbstractTest.ElapsedTestTime: Cardinal;
var
  Freq, Time: Int64;
begin
  // returns TestTime in millisecs
  if fStopTime > 0 then
    Time := fStopTime
  else if fStartTime > 0 then
    QueryPerformanceCounter(Time)
  else
    Time := 0;

  Time := Time - fStartTime;

  if QueryPerformanceFrequency(Freq) then
    Result := (1000*Time) div Freq
  else
    Result := 0;
end;


procedure TAbstractTest.SetStatusListener(Listener: IStatusListener);
begin
  FStatusListener := Listener;
end;

function TAbstractTest.GetStatus: string;
begin
  if FStatusStrings = nil then
    Result := ''
  else
    Result := FStatusStrings.Text;
end;

procedure TAbstractTest.Status(const Msg: string);
begin
  if FStatusStrings = nil then
    FStatusStrings := TStringList.Create;
  FStatusStrings.Add(Msg);
  if FStatusListener <> nil then
    FStatusListener.Status(self, Msg);
end;

procedure TAbstractTest.RunWithFixture(testResult: TTestResult);
begin
  assert(assigned(testResult));
  if testResult.ShouldRunTest(self) then
    testResult.Run(self);
end;

procedure TAbstractTest.Check(condition: boolean; msg: string);
begin
    if (not condition) then
        Fail(msg, CallerAddr);
end;

procedure TAbstractTest.CheckTrue(condition: boolean; msg: string);
begin
  if (not condition) then
      FailNotEquals(BoolToStr(true), BoolToStr(false), msg, CallerAddr);
end;

procedure TAbstractTest.CheckFalse(condition: boolean; msg: string);
begin
  if (condition) then
      FailNotEquals(BoolToStr(false), BoolToStr(true), msg, CallerAddr);
end;


procedure TAbstractTest.Fail(msg: string; errorAddr: Pointer = nil);
begin
{$IFDEF CLR}
  raise ETestFailure.Create(msg);
{$ELSE}
  if errorAddr = nil then
    raise ETestFailure.Create(msg) at CallerAddr
  else
    raise ETestFailure.Create(msg) at errorAddr;
{$ENDIF}
end;

procedure TAbstractTest.StopTests(msg: string);
begin
  raise EStopTestsFailure.Create(msg);
end;

procedure TAbstractTest.FailNotEquals( expected,
                                       actual   : WideString;
                                       msg      : string = '';
                                       errorAddr: Pointer = nil);
begin
    Fail(notEqualsErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TAbstractTest.FailEquals(       expected,
                                          actual   : WideString;
                                          msg      : string = '';
                                          errorAddr: Pointer = nil);
begin
    Fail(EqualsErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TAbstractTest.FailNotSame( expected,
                                     actual   : WideString;
                                     msg      : string = '';
                                     errorAddr: Pointer = nil);
begin
    Fail(NotSameErrorMessage(expected, actual, msg), errorAddr);
end;

procedure TAbstractTest.CheckEquals( expected,
                                 actual   : extended;
                                 delta    : extended;
                                 msg      : string = '');
begin
    if (abs(expected-actual) > delta) then
        FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, CallerAddr);
end;

procedure TAbstractTest.CheckEquals(expected, actual: extended; msg: string);
begin
  CheckEquals(expected, actual, 0, msg);
end;

procedure TAbstractTest.CheckNotNull(obj: IUnknown; msg: string);
begin
    if obj = nil then
      Fail(msg, CallerAddr);
end;

procedure TAbstractTest.CheckNull(obj: IUnknown; msg: string);
begin
    if obj <>  nil then
      Fail(msg, CallerAddr);
end;

procedure TAbstractTest.CheckSame(expected, actual: IUnknown; msg: string = '');
begin
    if (expected <> actual) then
      FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, CallerAddr);
end;

procedure TAbstractTest.CheckEquals(expected, actual: string; msg: string = '');
begin
  if expected <> actual then
    FailNotEquals(expected, actual, msg, CallerAddr);
end;

procedure TAbstractTest.CheckEqualsString(expected, actual: string; msg: string = '');
begin
  if expected <> actual then
    FailNotEquals(expected, actual, msg, CallerAddr);
end;

{$IFNDEF CLR}
{$IFDEF DELPHI6_OR_LATER}
procedure TAbstractTest.CheckEquals(expected, actual: WideString; msg: string = '');
begin
  if expected <> actual then
    FailNotEquals(expected, actual, msg, CallerAddr);
end;
{$ENDIF}

procedure TAbstractTest.CheckEqualsWideString(expected, actual: WideString; msg: string = '');
begin
  if expected <> actual then
    FailNotEquals(expected, actual, msg, CallerAddr);
end;

function TAbstractTest.GetMemDiffStr(expected, actual: pointer; size:longword; msg:string):string;
var
  db1, db2: byte;
  Offset: integer;
begin
  Offset:=FirstByteDiff(expected,actual,size,db1,db2);
  Result:=NotEqualsErrorMessage(IntToHex(db1,2),IntToHex(db2,2),msg);
  Result:=Result+' at Offset = '+IntToHex(Offset,4)+'h';
end;

procedure TAbstractTest.CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  if not CompareMem(expected, actual, size) then
    Fail(GetMemDiffStr(expected, actual, size, msg), CallerAddr);
end;
{$ENDIF}

procedure TAbstractTest.CheckNotEquals(expected, actual: string; msg: string = '');
begin
  if expected = actual then
    FailEquals(expected, actual, msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEqualsString(expected, actual: string; msg: string = '');
begin
  if expected = actual then
    FailEquals(expected, actual, msg, CallerAddr);
end;

{$IFNDEF CLR}
{$IFDEF DELPHI6_OR_LATER}
procedure TAbstractTest.CheckNotEquals(const expected, actual: WideString; msg: string = '');
begin
  if expected = actual then
    FailEquals(expected, actual, msg, CallerAddr);
end;
{$ENDIF}
procedure TAbstractTest.CheckNotEqualsWideString(const expected, actual: WideString; msg: string = '');
begin
  if expected = actual then
    FailEquals(expected, actual, msg, CallerAddr);
end;

// Expected not to work under CLR (pointer based) - KGS
procedure TAbstractTest.CheckNotEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  if CompareMem(expected, actual, size) then
  begin
    if msg <>'' then msg := msg + ', ';
    Fail(msg+'Memory content was identical', CallerAddr);
  end;
end;
{$ENDIF}

procedure TAbstractTest.CheckEquals(expected, actual: integer; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(IntToStr(expected), IntToStr(actual), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: integer; msg: string = '');
begin
  if expected = actual then
    FailEquals(IntToStr(expected), IntToStr(actual), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEquals(expected: extended; actual: extended; delta: extended = 0; msg: string = '');
begin
    if (abs(expected-actual) <= delta) then
        FailNotEquals(FloatToStr(expected), FloatToStr(actual), msg, CallerAddr);
end;

procedure TAbstractTest.CheckEquals(expected, actual: boolean; msg: string);
begin
  if (expected <> actual) then
    FailNotEquals(BoolToStr(expected), BoolToStr(actual), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEquals(expected, actual: boolean; msg: string);
begin
  if (expected = actual) then
    FailEquals(BoolToStr(expected), BoolToStr(actual), msg, CallerAddr);
end;

{ [KGS] IntToBin: Elected not to add to TestFrameWork interface,
        many people already have a self made version: }
function IntToBin(const value, digits: longword): string;
const 
  ALL_32_BIT_0 = '00000000000000000000000000000000';
var
  counter: integer;
  pow:     integer;
begin
  Result := ALL_32_BIT_0;
  SetLength(Result, digits);
  pow := 1 shl (digits - 1);
  if value <> 0 then
  for counter := 0 to digits - 1 do
  begin
    if (value and (pow shr counter)) <> 0 then
      Result[counter+1] := '1';
  end;
end;

procedure TAbstractTest.CheckEqualsBin(expected, actual: longword;
                                       msg: string = ''; digits: integer=32);
begin
  if expected <> actual then
    FailNotEquals(IntToBin(expected, digits), IntToBin(actual, digits), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEqualsBin(expected, actual: longword;
                                       msg: string = ''; digits: integer=32);
begin
  if (expected = actual) then
    FailEquals(IntToBin(expected, digits), IntToBin(actual, digits), msg, CallerAddr);
end;

procedure TAbstractTest.CheckEqualsHex(expected, actual: longword;
                                       msg: string = ''; digits: integer=8);
begin
  if expected <> actual then
    FailNotEquals(IntToHex(expected, digits), IntToHex(actual, digits), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotEqualsHex(expected, actual: longword;
                                       msg: string = ''; digits: integer=8);
begin
  if (expected = actual) then
    FailEquals(IntToHex(expected, digits), IntToHex(actual, digits), msg, CallerAddr);
end;

procedure TAbstractTest.CheckSame(expected, actual: TObject; msg: string);
begin
    if (expected <> actual) then
      FailNotSame(PtrToStr(Pointer(expected)), PtrToStr(Pointer(actual)), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNotNull(obj: TObject; msg: string);
begin
    if obj = nil then
       FailNotSame('object', PtrToStr(Pointer(obj)), msg, CallerAddr);
end;

procedure TAbstractTest.CheckNull(obj: TObject; msg: string);
begin
    if obj <> nil then
       FailNotSame('nil', PtrToStr(Pointer(obj)), msg, CallerAddr);
end;

function TAbstractTest.NotEqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format( sExpButWasFmt , [msg, expected, actual])
end;

function TAbstractTest.EqualsErrorMessage(expected, actual: WideString; msg: string): WideString;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format( sExpAndActualFmt, [msg, expected])
end;

function TAbstractTest.NotSameErrorMessage(expected, actual, msg: string): WideString;
begin
    if (msg <> '') then
        msg := msg + ', ';
    Result := Format( sExpButWasFmt, [msg, expected, actual])
end;

function TAbstractTest.BoolToStr(ABool: boolean): string;
begin
  Result := BooleanIdents[aBool];
end;

procedure TAbstractTest.StartExpectingException(e: ExceptionClass);
begin
  StopExpectingException;
  fExpectedException := e;
end;

procedure TAbstractTest.StopExpectingException(msg :string);
begin
  try
    if fExpectedException <> nil then
    begin
      Fail( Format( 'Expected exception "%s" but there was none. %s',
                                        [fExpectedException.ClassName,
                                        Msg]),
                                        CallerAddr);
    end;
  finally
    fExpectedException := nil;
  end;
end;

{$IFNDEF CLR}
procedure TAbstractTest.CheckMethodIsNotEmpty(MethodPointer: pointer);
const
  AssemblerRet = $C3;
begin
  if byte(MethodPointer^) = AssemblerRet then
    fail('Empty test', MethodPointer);
end;
{$ENDIF}

procedure TAbstractTest.CheckException(AMethod: TTestMethod; AExceptionClass: TClass; msg :string);
begin
  try
    Invoke(AMethod);
  except
    on e :Exception do
    begin
      if  not Assigned(AExceptionClass) then
        raise
      else if not e.ClassType.InheritsFrom(AExceptionClass) then
        FailNotEquals(AExceptionClass.ClassName, e.ClassName, msg, CallerAddr)
      else
        AExceptionClass := nil;
    end;
  end;
  if Assigned(AExceptionClass) then
    FailNotEquals(AExceptionClass.ClassName, 'nothing', msg, CallerAddr)
end;

procedure TAbstractTest.CheckEquals(expected, actual: TClass; msg: string);
begin
 if expected <> actual then
 begin
   if expected = nil then
     FailNotEquals('nil', actual.ClassName, msg, CallerAddr)
   else if actual = nil then
     FailNotEquals(expected.ClassName, 'nil', msg, CallerAddr)
   else
     FailNotEquals(expected.ClassName, actual.ClassName, msg, CallerAddr)
 end;
end;

procedure TAbstractTest.CheckInherits(expected, actual: TClass; msg: string);
begin
 if expected = nil then
   FailNotEquals('nil', actual.ClassName, msg, CallerAddr)
 else if actual = nil then
   FailNotEquals(expected.ClassName, 'nil', msg, CallerAddr)
 else if not actual.InheritsFrom(expected) then
   FailNotEquals(expected.ClassName, actual.ClassName, msg, CallerAddr)
end;

procedure TAbstractTest.CheckIs(AObject: TObject; AClass: TClass; msg: string);
begin
 Assert(AClass <> nil);
 if AObject = nil then
   FailNotEquals(AClass.ClassName, 'nil', msg, CallerAddr)
 else if not AObject.ClassType.InheritsFrom(AClass) then
   FailNotEquals(AClass.ClassName, AObject.ClassName, msg, CallerAddr)
end;

function TAbstractTest.GetGUIObject: TObject;
begin
  Result := FGUIObject;
end;

procedure TAbstractTest.SetGUIObject(const guiObject: TObject);
begin
  FGUIObject := guiObject;
end;

{ TTestCase }

constructor TTestCase.Create(MethodName: string);
{$IFNDEF CLR}
var
  RunMethod: TMethod;
{$ENDIF}
begin
  assert(length(MethodName) >0);
{$IFNDEF CLR}
  assert(assigned(MethodAddress(MethodName)));
{$ELSE}
  assert(MethodName <> '');
{$ENDIF}

  inherited Create(MethodName);
{$IFDEF CLR}
  FMethod := MethodName;
{$ELSE}
  RunMethod.code := MethodAddress(MethodName);
  RunMethod.Data := self;
  fMethod := TTestMethod(RunMethod);

  assert(assigned(fMethod));
{$ENDIF}
end;

procedure TTestCase.Invoke(AMethod: TTestMethod);
{$IFDEF CLR}
var
  TestType: System.type;
  Args: array of System.Object;
  Flags: BindingFlags;
{$ENDIF}
begin
{$IFDEF CLR}
  try
    GetType.InvokeMember(AMethod, BindingFlags.Public or BindingFlags.Instance or BindingFlags.InvokeMethod, nil, Self, Args);
  except
    on E:TargetInvocationException do
      raise E.InnerException;
  end;
{$ELSE}
  AMethod;
{$ENDIF}
end;

procedure TTestCase.RunWithFixture(testResult: TTestResult);
begin
  assert(assigned(testResult));
  if testResult.ShouldRunTest(self) then
  begin
    inc(testResult.fRunTests);
    inherited;
  end;
end;

procedure TTestCase.RunTest(testResult: TTestResult);
begin
  assert(assigned(fMethod), 'Method "' + FTestName + '" not found');
  fExpectedException := nil;
  try
    try
{$IFNDEF CLR}
      CheckMethodIsNotEmpty(tMethod(fMethod).Code);
{$ENDIF}
      Invoke(fMethod);
      StopExpectingException;
    except
      on E: ETestFailure  do
      begin
        raise;
      end;
      on E: Exception  do
      begin
        if  not Assigned(fExpectedException) then
          raise
        else if not E.ClassType.InheritsFrom(fExpectedException) then
           FailNotEquals(fExpectedException.ClassName, E.ClassName, 'unexpected exception', ExceptAddr);
      end
    end;
  finally
    fExpectedException := nil;
  end;
end;

procedure TTestCase.Run(testResult: TTestResult);
begin
  testResult.RunSuite(self);
end;

class function TTestCase.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(self);
end;

{ TTestFailure }

constructor TTestFailure.Create(FailedTest: ITest; thrownException: Exception; Addr: Pointer; msg: string);
begin
  assert(assigned(thrownException));

  inherited Create;
  fFailedTest := FailedTest;
  fThrownExceptionClass := thrownException.ClassType;
  fThrownExceptionMessage := msg + thrownException.message;
  FThrownExceptionAddress := Addr;
  CaptureStackTrace;
end;

function TTestFailure.FailedTest: ITest;
begin
  result := fFailedTest;
end;

function TTestFailure.ThrownExceptionName: string;
begin
  result := fThrownExceptionClass.ClassName;
end;

function TTestFailure.ThrownExceptionMessage: string;
begin
  result := fThrownExceptionMessage;
end;

function TTestFailure.ThrownExceptionAddress: pointer;
begin
  Result := FThrownExceptionAddress;
end;

function TTestFailure.ThrownExceptionClass: TClass;
begin
  Result := FThrownExceptionClass;
end;

function TTestFailure.LocationInfo: string;
begin
  Result := PointerToLocationInfo(ThrownExceptionAddress);
end;

function TTestFailure.AddressInfo: string;
begin
  Result := PointerToAddressInfo(ThrownExceptionAddress);
end;

function TTestFailure.StackTrace: string;
begin
  Result := FStackTrace;
end;

procedure TTestFailure.CaptureStackTrace;
var
  Trace :TStrings;
begin
  Trace := TStringList.Create;
  try
    {$IFDEF USE_JEDI_JCL}
      JclDebug.JclLastExceptStackListToStrings(Trace, true);
    {$ENDIF}
    FStackTrace := Trace.Text;
  finally
    Trace.Free;
  end;
end;

{ TTestSuite }

constructor TTestSuite.Create;
begin
  Create(TObject.ClassName);
end;

constructor TTestSuite.Create(name: string);
begin
  assert(length(name) > 0);

  inherited Create(name);

  fTests := TInterfaceList.Create;
end;

constructor TTestSuite.Create( testClass: TTestCaseClass);
begin
  self.Create(testClass.ClassName);
  AddTests(testClass);
end;

constructor TTestSuite.Create(Name: string; const Tests: array of ITest);
var
  i: Integer;
begin
  self.Create(Name);
  for i := Low(Tests) to High(Tests) do begin
    Self.addTest(Tests[i])
  end;
end;

procedure TTestSuite.AddTest(ATest: ITest);
begin
  Assert(Assigned(ATest));

  fTests.Add(ATest);
end;

procedure TTestSuite.AddSuite(suite: ITestSuite);
begin
  AddTest(suite);
end;


procedure TTestSuite.AddTests(testClass: TTestCaseClass);
var
  MethodIter     :  Integer;
  NameOfMethod   :  string;
  MethodEnumerator:  TMethodEnumerator;
begin
  { call on the method enumerator to get the names of the test
    cases in the testClass }
  MethodEnumerator := nil;
  try
    MethodEnumerator := TMethodEnumerator.Create(testClass);
    { make sure we add each test case  to the list of tests }
    for MethodIter := 0 to MethodEnumerator.Methodcount-1 do
      begin
        NameOfMethod := MethodEnumerator.nameOfMethod[MethodIter];
        self.addTest(testClass.Create(NameOfMethod) as ITest);
      end;
  finally
    MethodEnumerator.free;
  end;
end;

function TTestSuite.CountTestCases: integer;
var
  test: ITest;
  i: Integer;
  Total:  integer;
begin
  assert(assigned(fTests));

  Total := 0;
  for i := 0 to fTests.Count - 1 do
  begin
    test := fTests[i] as ITest;
    Total := Total + test.CountTestCases;
  end;
  Result := Total;
end;

function TTestSuite.CountEnabledTestCases: integer;
var
  i: Integer;
  test: ITest;
  Total:  Integer;
begin
  assert(assigned(fTests));

  Total := 0;
  if getEnabled then
  begin
    for i := 0 to fTests.Count - 1 do
    begin
      test := fTests[i] as ITest;
      Total := Total + test.CountEnabledTestCases;
    end;
  end;
  Result := Total;
end;

procedure TTestSuite.RunTest(testResult: TTestResult);
var
  i: Integer;
  test: ITest;
begin
  assert(assigned(testResult));
  assert(assigned(fTests));

  testResult.StartSuite(self);
  for i := 0 to fTests.Count - 1 do
  begin
    if testResult.ShouldStop then
      BREAK;
    test := fTests[i] as ITest;
    test.RunWithFixture(testResult);
  end;
  testResult.EndSuite(self);
end;

function TTestSuite.Tests: IInterfaceList;
begin
  result := fTests;
end;

procedure TTestSuite.LoadConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  Tests: IInterfaceList;
  TestSection: string;
begin
  inherited LoadConfiguration(iniFile, section);
  Tests := self.Tests;
  TestSection := section + '.' + self.GetName;
  for i := 0 to Tests.count-1 do
    (Tests[i] as ITest).LoadConfiguration(iniFile, TestSection);
end;

procedure TTestSuite.SaveConfiguration(const iniFile: TCustomIniFile; const section: string);
var
  i    : integer;
  Tests: IInterfaceList;
  TestSection: string;
begin
  inherited SaveConfiguration(iniFile, section);
  Tests := self.Tests;
  TestSection := section + '.' + self.GetName;
  for i := 0 to Tests.count-1 do
    (Tests[i] as ITest).SaveConfiguration(iniFile, TestSection);
end;


{ ETestFailure }

constructor ETestFailure.Create;
begin
   inherited Create('')
end;

constructor ETestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ EBreakingTestFailure }

constructor EBreakingTestFailure.Create;
begin
   inherited Create('')
end;

constructor EBreakingTestFailure.Create(msg: string);
begin
   inherited Create(msg)
end;

{ TMemIniFileTrimmed }

function TMemIniFileTrimmed.ReadString(const Section, Ident,
  Default: string): string;
begin
  // Trim the result for compatibility with TIniFile
  Result := Trim(inherited ReadString(Section, Ident, Default));
end;

{ TMethodEnumerator }

constructor TMethodEnumerator.Create(AClass: TClass);
{$IFDEF CLR}
var
  I, L: integer;
  T: System.Type;
  Methods: array of MethodInfo;

  function IsTest(AMethod: MethodInfo): boolean;
  var
    CustomAttr: array of System.Object;
    I: integer;
  begin
    Result := false;
    if AMethod.IsPublic then
    begin
      CustomAttr := AMethod.GetCustomAttributes(false);

      for I := 0 to System.Array(CustomAttr).Length - 1 do
      begin
        if CustomAttr[I].ClassNameIs('TestAttribute') then
        begin
          Result := true;
          Break;
        end;;
      end;
    end;
  end;
{$ELSE}
type
  TMethodTable = packed record
    count: SmallInt;
  //[...methods...]
  end;
var
  table: ^TMethodTable;
  name:  ^ShortString;
  i, j:  Integer;
{$ENDIF}
begin
  inherited Create;
{$IFDEF CLR}
  T := AClass.ClassInfo;
  Methods := T.GetMethods();
  L := 0;
  SetLength(FMethodNameList, L);
  for I := 0 to System.Array(Methods).Length - 1 do
    if IsTest(Methods[I]) then
    begin
      L := L + 1;
      SetLength(FMethodNameList, L);
      FMethodNameList[L-1] := Methods[I].Name;
    end;
{$ELSE}
  while aclass <> nil do
  begin
    // *** HACK ALERT *** !!!
    // Review System.MethodName to grok how this method works
    asm
      mov  EAX, [aclass]
      mov  EAX,[EAX].vmtMethodTable { fetch pointer to method table }
      mov  [table], EAX
    end;
    if table <> nil then
    begin
      name  := Pointer(PChar(table) + 8);
      for i := 1 to table.count do
      begin
        // check if we've seen the method name
        j := Low(FMethodNameList);
        while (j <= High(FMethodNameList))
        and (name^ <> FMethodNameList[j]) do
          inc(j);
        // if we've seen the name, then the method has probably been overridden
        if j > High(FMethodNameList) then
        begin
          SetLength(FMethodNameList,length(FMethodNameList)+1);
          FMethodNameList[j] := name^;
        end;
        name := Pointer(PChar(name) + length(name^) + 7)
      end;
    end;
    aclass := aclass.ClassParent;
  end;
{$ENDIF}
end;

function TMethodEnumerator.GetMethodCount: Integer;
begin
  Result := Length(FMethodNameList);
end;

function TMethodEnumerator.GetNameOfMethod(Index: integer): string;
begin
  Result := FMethodNameList[Index];
end;

{ Convenience routines }

function  TestSuite(name: string; const Tests: array of ITest): ITestSuite;
begin
   result := TTestSuite.Create(name, Tests);
end;

{ test registry }

var
  __TestRegistry: ITestSuite = nil;

procedure RegisterTestInSuite(rootSuite: ITestSuite; path: string; test: ITest);
var
  pathRemainder:  string;
  suiteName:  string;
  targetSuite:  ITestSuite;
  suite:  ITestSuite;
  currentTest:  ITest;
  Tests:  IInterfaceList;
  dotPos:  Integer;
  i: Integer;
begin
  if (path = '') then
  begin
    // End any recursion
    rootSuite.addTest(test);
  end
  else
  begin
    // Split the path on the dot (.)
    dotPos := Pos('.', Path);
    if (dotPos <= 0) then dotPos := Pos('\', Path);
    if (dotPos <= 0) then dotPos := Pos('/', Path);
    if (dotPos > 0) then
    begin
      suiteName := Copy(path, 1, dotPos - 1);
      pathRemainder := Copy(path, dotPos + 1, length(path) - dotPos);
    end
    else
    begin
      suiteName := path;
      pathRemainder := '';
    end;
    Tests := rootSuite.Tests;

    // Check to see if the path already exists
    targetSuite := nil;
    Tests := rootSuite.Tests;
    for i := 0 to Tests.count -1 do
    begin
      currentTest := Tests[i] as ITest;
{$IFDEF CLR}
      if Supports(currentTest, ITestSuite, suite) then
{$ELSE}
      currentTest.queryInterface(ITestSuite, suite);
      if Assigned(suite) then
{$ENDIF}
      begin
        if (currentTest.GetName = suiteName) then
        begin
          targetSuite := suite;
          break;
        end;
      end;
    end;

    if not assigned(targetSuite) then
    begin
      targetSuite := TTestSuite.Create(suiteName);
      rootSuite.addTest(targetSuite);
    end;

    RegisterTestInSuite(targetSuite, pathRemainder, test);
  end;
end;

procedure CreateRegistry;
var
  MyName :AnsiString;
begin
{$IFDEF CLR}
  MyName := ExtractFileName(ParamStr(0));
{$ELSE}
  SetLength(MyName, 1024);
  GetModuleFileName(hInstance, PChar(MyName), Length(MyName));
  MyName := Trim(PChar(MyName));
  MyName := ExtractFileName(MyName);
{$ENDIF}
  __TestRegistry := TTestSuite.Create(MyName);
end;

procedure RegisterTest(SuitePath: string; test: ITest);
begin
  assert(assigned(test));
  if __TestRegistry = nil then CreateRegistry;
  RegisterTestInSuite(__TestRegistry, SuitePath, test);
end;

procedure RegisterTest(test: ITest);
begin
  RegisterTest('', test);
end;

procedure RegisterTests(SuitePath: string; const Tests: array of ITest);
var
  i: Integer;
begin
  for i := Low(Tests) to High(Tests) do begin
    TestFramework.RegisterTest(SuitePath, Tests[i])
  end
end;

procedure RegisterTests(const Tests: array of ITest);
begin
  RegisterTests('', Tests);
end;

function RegisteredTests: ITestSuite;
begin
  result := __TestRegistry;
end;

function RunTest(suite: ITest; listeners: array of ITestListener): TTestResult; overload;
var
  i        : Integer;
begin
  result := TTestResult.Create;
  for i := low(listeners) to high(listeners) do
      result.addListener(listeners[i]);
  if suite <> nil then
    suite.Run(result);
end;

function RunRegisteredTests(listeners: array of ITestListener): TTestResult;
begin
  result := RunTest(RegisteredTests, listeners);
end;

procedure ClearRegistry;
begin
  __TestRegistry := nil;
end;

initialization
{$IFDEF LINUX}
  InitPerformanceCounter;
{$ENDIF}
finalization
  ClearRegistry;
end.
