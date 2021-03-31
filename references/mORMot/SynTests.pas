/// Unit test functions used by Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynTests;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

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

*)


{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
  Messages,
{$endif}
{$ifdef KYLIX3}
  Types,
{$endif}
  Classes,
{$ifndef LVCL}
{$ifdef HASINLINE}
  Types,
{$endif}
{$endif}
{$ifndef NOVARIANTS}
  Variants,
{$endif}
  SynCommons,
  SynTable,
  SynLog,
  SysUtils;


{ ************ Unit-Testing classes and functions }

type
  /// the prototype of an individual test
  // - to be used with TSynTest descendants
  TSynTestEvent = procedure of object;

  /// allows to tune TSynTest process
  // - tcoLogEachCheck will log as sllCustom4 each non void Check() message
  TSynTestOption = (tcoLogEachCheck);
  /// set of options to tune TSynTest process
  TSynTestOptions = set of TSynTestOption;

  TSynTest = class;

  /// how published method information is stored within TSynTest
  TSynTestMethodInfo = record
    /// the uncamelcased method name
    TestName: string;
    /// ready-to-be-displayed 'Ident - TestName' text, as UTF-8
    IdentTestName: RawUTF8;
    /// raw method name, as defined in pascal code (not uncamelcased)
    MethodName: RawUTF8;
    /// direct access to the method execution
    Method: TSynTestEvent;
    /// the test case holding this method
    Test: TSynTest;
    /// the index of this method in the TSynTestCase
    MethodIndex: integer;
  end;
  /// pointer access to published method information
  PSynTestMethodInfo = ^TSynTestMethodInfo;

  /// abstract parent class for both tests suit (TSynTests) and cases (TSynTestCase)
  // - purpose of this ancestor is to have RTTI for its published methods,
  // and to handle a class text identifier, or uncamelcase its class name
  // if no identifier was defined
  // - sample code about how to use this test framework is available in
  // the "Sample\07 - SynTest" folder
  TSynTest = class(TSynPersistent)
  protected
    fTests: array of TSynTestMethodInfo;
    fIdent: string;
    fInternalTestsCount: integer;
    fOptions: TSynTestOptions;
    function GetCount: Integer;
    function GetIdent: string;
  public
    /// create the test instance
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = ''); reintroduce; virtual;
    /// register a specified test to this class instance
    // - Create will register all published methods of this class, but
    // your code may initialize its own set of methods on need
    procedure Add(const aMethod: TSynTestEvent; const aMethodName: RawUTF8;
      const aIdent: string);
    /// the test name
    // - either the Ident parameter supplied to the Create() method, either
    // a uncameled text from the class name
    property Ident: string read GetIdent;
    /// return the number of tests associated with this class
    // - i.e. the number of registered tests by the Register() method PLUS
    // the number of published methods defined within this class
    property Count: Integer read GetCount;
    /// return the number of published methods defined within this class as tests
    // - i.e. the number of tests added by the Create() constructor from RTTI
    // - any TestName/TestMethod[] index higher or equal to this value has been
    // added by a specific call to the Add() method
    property InternalTestsCount: integer read fInternalTestsCount;
    /// allows to tune the test case process
    property Options: TSynTestOptions read fOptions write fOptions;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter }
  end;

  TSynTests = class;

  /// a class implementing a test case
  // - should handle a test unit, i.e. one or more tests
  // - individual tests are written in the published methods of this class
  TSynTestCase = class(TSynTest)
  protected
    fOwner: TSynTests;
    fAssertions: integer;
    fAssertionsFailed: integer;
    fAssertionsBeforeRun: integer;
    fAssertionsFailedBeforeRun: integer;
    /// any number not null assigned to this field will display a "../s" stat
    fRunConsoleOccurenceNumber: cardinal;
    /// any number not null assigned to this field will display a "using .. MB" stat
    fRunConsoleMemoryUsed: Int64;
    /// any text assigned to this field will be displayed on console
    fRunConsole: string;
    fCheckLogTime: TPrecisionTimer;
    fCheckLastMsg: cardinal;
    fCheckLastTix: cardinal;
    /// called before all published methods are executed
    procedure Setup; virtual;
    /// called after all published methods are executed
    // - WARNING: this method should be re-entrant - so using FreeAndNil() is
    // a good idea in this method :)
    procedure CleanUp; virtual;
    /// called before each published methods execution
    procedure MethodSetup; virtual;
    /// called after each published methods execution
    procedure MethodCleanUp; virtual;
    procedure AddLog(condition: Boolean; const msg: string);
  public
    /// create the test case instance
    // - must supply a test suit owner
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    constructor Create(Owner: TSynTests; const Ident: string = ''); reintroduce; virtual;
    /// clean up the instance
    // - will call CleanUp, even if already done before
    destructor Destroy; override;
    /// used by the published methods to run a test assertion
    // - condition must equals TRUE to pass the test
    procedure Check(condition: Boolean; const msg: string = '');
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion
    // - condition must equals TRUE to pass the test
    // - function return TRUE if the condition failed, in order to allow the
    // caller to stop testing with such code:
    // ! if CheckFailed(A=10) then exit;
    function CheckFailed(condition: Boolean; const msg: string = ''): Boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion
    // - condition must equals FALSE to pass the test
    // - function return TRUE if the condition failed, in order to allow the
    // caller to stop testing with such code:
    // ! if CheckNot(A<>10) then exit;
    function CheckNot(condition: Boolean; const msg: string = ''): Boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against integers
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(a,b: Int64; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against UTF-8 strings
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(const a,b: RawUTF8; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against pointers/classes
    // - if a<>b, will fail and include '#<>#' text before the supplied msg
    function CheckEqual(a,b: pointer; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against integers
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(a,b: Int64; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against UTF-8 strings
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(const a,b: RawUTF8; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run test assertion against pointers/classes
    // - if a=b, will fail and include '#=#' text before the supplied msg
    function CheckNotEqual(a,b: pointer; const msg: RawUTF8 = ''): Boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by the published methods to run a test assertion about two double values
    // - includes some optional precision argument
    function CheckSame(const Value1,Value2: double;
      const Precision: double=DOUBLE_SAME; const msg: string = ''): Boolean;
    /// perform a string comparison with several value
    // - test passes if (Value=Values[0]) or (Value=Value[1]) or (Value=Values[2])...
    // and ExpectedResult=true
    function CheckMatchAny(const Value: RawUTF8; const Values: array of RawUTF8;
      CaseSentitive: Boolean=true; ExpectedResult: Boolean=true; const msg: string = ''): Boolean;
    /// used by the published methods to run a test assertion, with an UTF-8 error message
    // - condition must equals TRUE to pass the test
    procedure CheckUTF8(condition: Boolean; const msg: RawUTF8); overload;
    /// used by the published methods to run a test assertion, with a error
    // message computed via FormatUTF8()
    // - condition must equals TRUE to pass the test
    procedure CheckUTF8(condition: Boolean; const msg: RawUTF8; const args: array of const); overload;
    /// used by published methods to start some timing on associated log
    // - call this once, before one or several consecutive CheckLogTime()
    // - warning: this method is not thread-safe
    procedure CheckLogTimeStart;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by published methods to write some timing on associated log
    // - at least one CheckLogTimeStart method call should happen to reset the
    // internal timer
    // - condition must equals TRUE to pass the test
    // - the supplied message would be appended, with its timing
    // - warning: this method is not thread-safe
    procedure CheckLogTime(condition: boolean;
      const msg: RawUTF8; const args: array of const; level: TSynLogInfo=sllTrace);
    /// create a temporary string random content, WinAnsi (code page 1252) content
    class function RandomString(CharCount: Integer): RawByteString;
    /// create a temporary UTF-8 string random content, using WinAnsi
    // (code page 1252) content
    class function RandomUTF8(CharCount: Integer): RawUTF8;
    /// create a temporary UTF-16 string random content, using WinAnsi
    // (code page 1252) content
    class function RandomUnicode(CharCount: Integer): SynUnicode;
    /// create a temporary string random content, using ASCII 7 bit content
    class function RandomAnsi7(CharCount: Integer): RawByteString;
    /// create a temporary string random content, using A..Z,_,0..9 chars only
    class function RandomIdentifier(CharCount: Integer): RawByteString;
    /// create a temporary string random content, using uri-compatible chars only
    class function RandomURI(CharCount: Integer): RawByteString;
    /// create a temporary string, containing some fake text, with paragraphs
    class function RandomTextParagraph(WordCount: Integer;
      LastPunctuation: AnsiChar='.'; const RandomInclude: RawUTF8=''): RawUTF8;
    /// add containing some "bla bli blo blu" fake text, with paragraphs
    class procedure AddRandomTextParagraph(WR: TTextWriter; WordCount: Integer;
      LastPunctuation: AnsiChar='.'; const RandomInclude: RawUTF8=''; NoLineFeed: boolean=false);
    /// this method is triggered internaly - e.g. by Check() - when a test failed
    procedure TestFailed(const msg: string);
    /// will add to the console a message with a speed estimation
    // - speed is computed from the method start
    // - returns the number of microsec of the (may be specified) timer
    // - OnlyLog will compute and append the info to the log, but not on the console
    // - warning: this method is not thread-safe if a local Timer is not specified
    function NotifyTestSpeed(const ItemName: string; ItemCount: integer;
      SizeInBytes: cardinal=0; Timer: PPrecisionTimer=nil; OnlyLog: boolean=false): TSynMonitorOneMicroSec; overload;
    /// will add to the console a formatted message with a speed estimation
    function NotifyTestSpeed(const ItemNameFmt: RawUTF8; const ItemNameArgs: array of const;
      ItemCount: integer; SizeInBytes: cardinal=0; Timer: PPrecisionTimer=nil; OnlyLog: boolean=false): TSynMonitorOneMicroSec; overload;
    /// append some text to the current console
    // - OnlyLog will compute and append the info to the log, but not on the console
    procedure AddConsole(const msg: string; OnlyLog: boolean=false);
    /// the test suit which owns this test case
    property Owner: TSynTests read fOwner;
    /// the test name
    // - either the Ident parameter supplied to the Create() method, either
    // an uncameled text from the class name
    property Ident: string read GetIdent;
    /// the number of assertions (i.e. Check() method call) for this test case
    property Assertions: integer read fAssertions;
    /// the number of assertions (i.e. Check() method call) for this test case
    property AssertionsFailed: integer read fAssertionsFailed;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter
      - the method name will be used, after "uncamelcasing", for display }
  end;

  /// class-reference type (metaclass) of a test case
  TSynTestCaseClass = class of TSynTestCase;

  /// information about a failed test
  TSynTestFailed = record
    /// the contextual message associated with this failed test
    Error: string;
    /// the uncamelcased method name
    TestName: string;
    /// ready-to-be-displayed 'TestCaseIdent - TestName' text, as UTF-8
    IdentTestName: RawUTF8;
  end;
  TSynTestFaileds = array of TSynTestFailed;

  /// a class used to run a suit of test cases
  TSynTests = class(TSynTest)
  protected
    /// any number not null assigned to this field will display a "../sec" stat
    fRunConsoleOccurenceNumber: cardinal;
    fTestCase: TSynList; // stores TSynTestCaseClass during Run
    fAssertions: integer;
    fAssertionsFailed: integer;
    fCurrentMethodInfo: PSynTestMethodInfo;
    fSaveToFile: Text;
    fSafe: TSynLocker;
    fFailed: TSynTestFaileds;
    fFailedCount: integer;
    function GetFailedCount: integer;
    function GetFailed(Index: integer): TSynTestFailed;
    procedure CreateSaveToFile; virtual;
    procedure Color(aColor: TConsoleColor);
    /// called when a test case failed: default is to add item to fFailed[]
    procedure AddFailed(const msg: string); virtual;
    /// this method is called before every run
    // - default implementation will just return nil
    // - can be overridden to implement a per-test case logging for instance
    function BeforeRun: IUnknown; virtual;
    /// this method is called during the run, after every testcase
    // - this implementation just report some minimal data to the console
    // by default, but may be overridden to update a real UI or reporting system
    // - method implementation can use fCurrentMethodInfo^ to get run context
    procedure AfterOneRun; virtual;
  public
    /// you can put here some text to be displayed at the end of the messages
    // - some internal versions, e.g.
    // - every line of text must explicitly BEGIN with #13#10
    CustomVersions: string;
    /// contains the run elapsed time
    RunTimer, TestTimer, TotalTimer: TPrecisionTimer;
    /// create the test suit
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = ''); override;
    /// finalize the class instance
    // - release all registered Test case instance
    destructor Destroy; override;
    /// you can call this class method to perform all the tests on the Console
    // - it will create an instance of the corresponding class, with the
    // optional identifier to be supplied to its constructor
    // - if the executable was launched with a parameter, it will be used as
    // file name for the output - otherwise, tests information will be written
    // to the console
    // - it will optionally enable full logging during the process
    // - a typical use will first assign the same log class for the whole
    // framework, if the mORMot.pas unit is to be used - in such case, before
    // calling RunAsConsole(), the caller should execute:
    // ! TSynLogTestLog := TSQLLog;
    // ! TMyTestsClass.RunAsConsole('My Automated Tests',LOG_VERBOSE);
    class procedure RunAsConsole(const CustomIdent: string='';
      withLogs: TSynLogInfos=[sllLastError,sllError,sllException,sllExceptionOS];
      options: TSynTestOptions=[]); virtual;
    /// save the debug messages into an external file
    // - if no file name is specified, the current Ident is used
    procedure SaveToFile(const DestPath: TFileName; const FileName: TFileName='');
    /// register a specified Test case from its class name
    // - an instance of the supplied class will be created during Run
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase(TOneTestCase);
    procedure AddCase(TestCase: TSynTestCaseClass); overload;
    /// register a specified Test case from its class name
    // - an instance of the supplied classes will be created during Run
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase([TOneTestCase]);
    procedure AddCase(const TestCase: array of TSynTestCaseClass); overload;
    /// call of this method will run all associated tests cases
    // - function will return TRUE if all test passed
    // - all failed test cases will be added to the Failed[] list - which is
    // cleared at the beginning of the run
    // - Assertions and AssertionsFailed counter properties are reset and
    // computed during the run
    // - you may override this method to provide additional information, e.g.
    // ! function TMySynTests.Run: Boolean;
    // ! begin // need SynSQLite3.pas unit in the uses clause
    // !   CustomVersions := format(#13#10#13#10'%s'#13#10'    %s'#13#10 +
    // !     'Using mORMot %s'#13#10'    %s %s', [OSVersionText, CpuInfoText,
    // !      SYNOPSE_FRAMEWORK_FULLVERSION, sqlite3.ClassName, sqlite3.Version]);
    // !   result := inherited Run;
    // ! end;
    function Run: Boolean; virtual;
    /// method information currently running
    // - is set by Run and available within TTestCase methods
    property CurrentMethodInfo: PSynTestMethodInfo read fCurrentMethodInfo;
    /// number of failed tests after the last call to the Run method
    property FailedCount: integer read GetFailedCount;
    /// retrieve the information associated with a failure
    property Failed[Index: integer]: TSynTestFailed read GetFailed;
    /// the number of assertions (i.e. Check() method call) in all tests
    // - this property is set by the Run method above
    property Assertions: integer read fAssertions;
    /// the number of assertions (i.e. Check() method call) which failed in all tests
    // - this property is set by the Run method above
    property AssertionsFailed: integer read fAssertionsFailed;
  published
    { all published methods of the children will be run as test cases registering
      - these methods must be declared as procedure with no parameter
      - every method should create a customized TSynTestCase instance,
        which will be registered with the AddCase() method, then automaticaly
        destroyed during the TSynTests destroy  }
  end;

  /// this overridden class will create a .log file in case of a test case failure
  // - inherits from TSynTestsLogged instead of TSynTests in order to add
  // logging to your test suite (via a dedicated TSynLogTest instance)
  TSynTestsLogged = class(TSynTests)
  protected
    fLogFile: TSynLog;
    fConsoleDup: TTextWriter;
    procedure CreateSaveToFile; override;
    /// called when a test case failed: log into the file
    procedure AddFailed(const msg: string); override;
    /// this method is called before every run
    // - overridden implementation to implement a per-test case logging
    function BeforeRun: IUnknown; override;
  public
    /// create the test instance and initialize associated LogFile instance
    // - this will allow logging of all exceptions to the LogFile
    constructor Create(const Ident: string = ''); override;
    /// release associated memory
    destructor Destroy; override;
    /// the .log file generator created if any test case failed
    property LogFile: TSynLog read fLogFile;
  end;


const
  EQUAL_MSG = '%<>% %';
  NOTEQUAL_MSG = '%=% %';


implementation

{$ifdef FPC}
{$ifndef MSWINDOWS}
uses
  SynFPCLinux;
{$endif}
{$endif}

{ TSynTest }

procedure TSynTest.Add(const aMethod: TSynTestEvent; const aMethodName: RawUTF8;
  const aIdent: string);
var n: integer;
begin
  if self=nil then
    exit; // avoid GPF
  n := Length(fTests);
  SetLength(fTests,n+1);
  with fTests[n] do begin
    TestName := aIdent;
    IdentTestName := StringToUTF8(fIdent+' - '+TestName);
    Method := aMethod;
    MethodName := aMethodName;
    Test := self;
    MethodIndex := n;
  end;
end;

constructor TSynTest.Create(const Ident: string);
var id: RawUTF8;
    s: string;
    methods: TPublishedMethodInfoDynArray;
    i: integer;
begin
  inherited Create;
  if Ident<>'' then
    fIdent := Ident else begin
    ToText(ClassType,id);
    if IdemPChar(Pointer(id),'TSYN') then
      if IdemPChar(Pointer(id),'TSYNTEST') then
        Delete(id,1,8) else
        Delete(id,1,4) else
    if IdemPChar(Pointer(id),'TTEST') then
      Delete(id,1,5) else
    if id[1]='T' then
      Delete(id,1,1);
    fIdent := string(UnCamelCase(id));
  end;
  for i := 0 to GetPublishedMethods(self,methods)-1 do
    with methods[i] do begin
      inc(fInternalTestsCount);
      if Name[1]='_' then
        s := Ansi7ToString(copy(Name,2,100)) else
        s := Ansi7ToString(UnCamelCase(Name));
      Add(TSynTestEvent(Method),Name,s);
    end;
end;

function TSynTest.GetCount: Integer;
begin
  if self=nil then
    result := 0 else
    result := length(fTests);
end;

function TSynTest.GetIdent: string;
begin
  if self=nil then
    result := '' else
    result := fIdent;
end;


{ TSynTestCase }

constructor TSynTestCase.Create(Owner: TSynTests; const Ident: string);
begin
  inherited Create(Ident);
  fOwner := Owner;
  fOptions := Owner.Options;
end;

procedure TSynTestCase.Setup;
begin
  // do nothing by default
end;

procedure TSynTestCase.CleanUp;
begin
  // do nothing by default
end;

procedure TSynTestCase.MethodSetup;
begin
  // do nothing by default
end;

procedure TSynTestCase.MethodCleanUp;
begin
  // do nothing by default
end;

destructor TSynTestCase.Destroy;
begin
  CleanUp;
  inherited;
end;

procedure TSynTestCase.AddLog(condition: Boolean; const msg: string);
const LEV: array[boolean] of TSynLogInfo = (sllFail, sllCustom4);
var tix, crc: cardinal; // use a crc since strings are not thread-safe
begin
  if condition then begin
    crc := Hash32(pointer(msg),length(msg)*SizeOf(msg[1]));
    if crc=fCheckLastMsg then begin // no need to be too much verbose
      tix := GetTickCount64 shr 8; // also avoid to use a lock
      if tix=fCheckLastTix then
        exit;
      fCheckLastTix := tix;
    end;
    fCheckLastMsg := crc;
  end else
    fCheckLastMsg := 0;
  if fOwner.fCurrentMethodInfo<>nil then
    TSynLogTestLog.Add.Log(LEV[condition],'% % [%]',
      [ClassType,fOwner.fCurrentMethodInfo^.TestName,msg]);
end;

procedure TSynTestCase.Check(condition: Boolean; const msg: string);
begin
  if self=nil then
    exit;
  if (msg<>'') and (tcoLogEachCheck in fOptions) then
    AddLog(condition,msg);
  InterlockedIncrement(fAssertions);
  if not condition then
    TestFailed(msg);
end;

function TSynTestCase.CheckFailed(condition: Boolean; const msg: string): Boolean;
begin
  if self=nil then begin
    result := false;
    exit;
  end;
  if (msg<>'') and (tcoLogEachCheck in fOptions) then
    AddLog(condition,msg);
  InterlockedIncrement(fAssertions);
  if condition then
    result := false else begin
    TestFailed(msg);
    result := true;
  end;
end;

function TSynTestCase.CheckNot(condition: Boolean; const msg: string): Boolean;
begin
  result := CheckFailed(not condition, msg);
end;

function TSynTestCase.CheckEqual(a,b: Int64; const msg: RawUTF8): Boolean;
begin
  result := a=b;
  CheckUTF8(result,EQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckEqual(const a, b: RawUTF8; const msg: RawUTF8): Boolean;
begin
  result := a=b;
  CheckUTF8(result,EQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckEqual(a,b: pointer; const msg: RawUTF8): Boolean;
begin
  result := a=b;
  CheckUTF8(result,EQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckNotEqual(a,b: Int64; const msg: RawUTF8): Boolean;
begin
  result := a<>b;
  CheckUTF8(result,NOTEQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckNotEqual(const a, b: RawUTF8; const msg: RawUTF8): Boolean;
begin
  result := a<>b;
  CheckUTF8(result,NOTEQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckNotEqual(a,b: pointer; const msg: RawUTF8): Boolean;
begin
  result := a<>b;
  CheckUTF8(result,NOTEQUAL_MSG,[a,b,msg]);
end;

function TSynTestCase.CheckSame(const Value1, Value2: double;
  const Precision: double; const msg: string): Boolean;
begin
  result := SameValue(Value1,Value2,Precision);
  CheckUTF8(result,EQUAL_MSG,[Value1,Value2,msg]);
end;

function TSynTestCase.CheckMatchAny(const Value: RawUTF8;
  const Values: array of RawUTF8; CaseSentitive: Boolean;
  ExpectedResult: Boolean; const msg: string): Boolean;
begin
  result := (FindRawUTF8(Values,Value,CaseSentitive)>=0)=ExpectedResult;
  Check(result);
end;

procedure TSynTestCase.CheckUTF8(condition: Boolean; const msg: RawUTF8);
begin
  InterlockedIncrement(fAssertions);
  if not condition or (tcoLogEachCheck in fOptions) then
    CheckUTF8(condition,'%',[msg]);
end;

procedure TSynTestCase.CheckUTF8(condition: Boolean; const msg: RawUTF8;
  const args: array of const);
var str: string; // using a sub-proc may be faster, but unstable on Android
begin
  InterlockedIncrement(fAssertions);
  if not condition or (tcoLogEachCheck in fOptions) then begin
    if msg<>'' then begin
      FormatString(msg,args,str);
      if tcoLogEachCheck in fOptions then
        AddLog(condition,str);
    end;
    if not condition then
      TestFailed(str);
  end;
end;

procedure TSynTestCase.CheckLogTimeStart;
begin
  fCheckLogTime.Start;
end;

procedure TSynTestCase.CheckLogTime(condition: boolean; const msg: RawUTF8;
  const args: array of const; level: TSynLogInfo);
var str: string;
begin
  FormatString(msg,args,str);
  Check(condition,str);
  TSynLogTestLog.Add.Log(level,'% %',[str,fCheckLogTime.Stop],self);
  fCheckLogTime.Start;
end;

class function TSynTestCase.RandomString(CharCount: Integer): RawByteString;
var i: PtrInt;
    R: PByteArray;
    tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  SetString(result,nil,CharCount);
  for i := 0 to CharCount-1 do
    PByteArray(result)[i] := 32+R[i] and 127;
  tmp.Done;
end;

class function TSynTestCase.RandomAnsi7(CharCount: Integer): RawByteString;
var i: PtrInt;
    R: PByteArray;
    tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  SetString(result,nil,CharCount);
  for i := 0 to CharCount-1 do
    PByteArray(result)[i] := 32+R[i] mod 94;
  tmp.Done;
end;

procedure InitRandom64(chars64: PAnsiChar; count: integer; var result: RawByteString);
var i: PtrInt;
    R: PByteArray;
    tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(count);
  SetString(result,nil,count);
  for i := 0 to count-1 do
    PByteArray(result)[i] := ord(chars64[PtrInt(R[i]) and 63]);
  tmp.Done;
end;

class function TSynTestCase.RandomIdentifier(CharCount: Integer): RawByteString;
const IDENT_CHARS: array[0..63] of AnsiChar =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
begin
  InitRandom64(@IDENT_CHARS, CharCount, result);
end;

class function TSynTestCase.RandomURI(CharCount: Integer): RawByteString;
const URL_CHARS: array[0..63] of AnsiChar =
  'abcdefghijklmnopqrstuvwxyz0123456789-abCdEfGH.JKlmnOP.RsTuVWxyz.';
begin
  InitRandom64(@URL_CHARS, CharCount, result);
end;

class function TSynTestCase.RandomUTF8(CharCount: Integer): RawUTF8;
begin
  result := WinAnsiToUtf8(WinAnsiString(RandomString(CharCount)));
end;

class function TSynTestCase.RandomUnicode(CharCount: Integer): SynUnicode;
begin
  result := WinAnsiConvert.AnsiToUnicodeString(RandomString(CharCount));
end;

class function TSynTestCase.RandomTextParagraph(WordCount: Integer;
  LastPunctuation: AnsiChar; const RandomInclude: RawUTF8): RawUTF8;
var
  tmp: TTextWriterStackBuffer;
  WR: TTextWriter;
begin
  WR := TTextWriter.CreateOwnedStream(tmp);
  try
    AddRandomTextParagraph(WR, WordCount, LastPunctuation, RandomInclude);
    WR.SetText(result);
  finally
    WR.Free;
  end;
end;

class procedure TSynTestCase.AddRandomTextParagraph(WR: TTextWriter; WordCount: Integer;
  LastPunctuation: AnsiChar; const RandomInclude: RawUTF8; NoLineFeed: boolean);
type TKind = (space, comma, dot, question, paragraph);
const bla: array[0..7] of string[3]=('bla','ble','bli','blo','blu','bla','bli','blo');
      endKind = [dot,paragraph,question];
var n: integer;
    s: string[3];
    last: TKind;
    rnd: cardinal;
begin
  last := paragraph;
  while WordCount>0 do begin
    rnd := Random32gsl; // get 32-bit of randomness
    for n := 0 to rnd and 3 do begin // consume up to 20-bit from rnd
      rnd := rnd shr 2;
      s := bla[rnd and 7];
      rnd := rnd shr 3;
      if last in endKind then begin
        last := space;
        s[1] := NormToUpper[s[1]];
      end;
      WR.AddShort(s);
      WR.Add(' ');
      dec(WordCount);
    end;
    WR.CancelLastChar(' ');
    case rnd and 127 of // consume 7-bit
    0..4: begin
      if RandomInclude<>'' then begin
        WR.Add(' ');
        WR.AddString(RandomInclude);
      end;
      last := space;
    end;
    5..65:  last := space;
    66..90: last := comma;
    91..105: last := dot;
    106..115: last := question;
    116..127: if NoLineFeed then last := dot else last := paragraph;
    end;
    case last of
    space: WR.Add(' ');
    comma: WR.Add(',',' ');
    dot:   WR.Add('.',' ');
    question:  WR.Add('?',' ');
    paragraph: WR.AddShort('.'#13#10);
    end;
  end;
  if not(last in endKind) and (LastPunctuation<>' ') then begin
    WR.AddShort('bla');
    WR.Add(LastPunctuation);
  end;
end;

procedure TSynTestCase.TestFailed(const msg: string);
begin
  fOwner.fSafe.Lock; // protect when Check() is done from multiple threads
  try
    TSynLogTestLog.DebuggerNotify(sllFail,'#% %',[fAssertions-fAssertionsBeforeRun,msg]);
    if Owner<>nil then // avoid GPF
      Owner.AddFailed(msg);
    inc(fAssertionsFailed);
  finally
    fOwner.fSafe.UnLock;
  end;   
end;

procedure TSynTestCase.AddConsole(const msg: string; OnlyLog: boolean);
begin
  TSynLogTestLog.Add.Log(sllMonitoring, '% %', [ClassType, msg]);
  if OnlyLog then
    exit;
  fOwner.fSafe.Lock;
  try
    if fRunConsole<>'' then
      fRunConsole := fRunConsole+#13#10'     '+msg else
      fRunConsole := fRunConsole+msg;
  finally
    fOwner.fSafe.UnLock;
  end;
end;

function TSynTestCase.NotifyTestSpeed(const ItemName: string; ItemCount: integer;
  SizeInBytes: cardinal; Timer: PPrecisionTimer; OnlyLog: boolean): TSynMonitorOneMicroSec;
var Temp: TPrecisionTimer;
    msg: string;
begin
  if Timer=nil then
    Temp := Owner.TestTimer else
    Temp := Timer^;
  if ItemCount<=1 then
    FormatString('% in %',[ItemName,Temp.Stop],msg) else
    FormatString('% % in % i.e. %/s, aver. %',[ItemCount,ItemName,Temp.Stop,
      IntToThousandString(Temp.PerSec(ItemCount)),Temp.ByCount(ItemCount)],msg);
  if SizeInBytes>0 then
    msg := FormatString('%, %/s',[msg,KB(Temp.PerSec(SizeInBytes))]);
  AddConsole(msg,OnlyLog);
  result := Temp.TimeInMicroSec;
end;

function TSynTestCase.NotifyTestSpeed(const ItemNameFmt: RawUTF8; const ItemNameArgs: array of const;
  ItemCount: integer; SizeInBytes: cardinal; Timer: PPrecisionTimer; OnlyLog: boolean): TSynMonitorOneMicroSec;
var str: string;
begin
  FormatString(ItemNameFmt,ItemNameArgs,str);
  result := NotifyTestSpeed(str,ItemCount,SizeInBytes,Timer,OnlyLog);
end;


{ TSynTests }

procedure TSynTests.AddCase(const TestCase: array of TSynTestCaseClass);
var i: integer;
begin
  for i := low(TestCase) to high(TestCase) do
    fTestCase.Add(TestCase[i]);
end;

procedure TSynTests.AddCase(TestCase: TSynTestCaseClass);
begin
  fTestCase.Add(TestCase);
end;

function TSynTests.BeforeRun: IUnknown;
begin
  result := nil;
end;

constructor TSynTests.Create(const Ident: string);
begin
  inherited Create(Ident);
  fTestCase := TSynList.Create;
  fSafe.Init;
end;

destructor TSynTests.Destroy;
begin
  fTestCase.Free;
  if TTextRec(fSaveToFile).Handle<>0 then
    Close(fSaveToFile);
  inherited Destroy;
  fSafe.Done;
end;

{$I-}

procedure TSynTests.Color(aColor: TConsoleColor);
begin
  if (StdOut<>0) and (THandle(TTextRec(fSaveToFile).Handle)=StdOut) then
    TextColor(aColor);
end;

procedure TSynTests.CreateSaveToFile;
begin
  System.Assign(fSaveToFile,'');
  Rewrite(fSaveToFile);
  {$ifndef MSWINDOWS}
  TTextRec(fSaveToFile).LineEnd := #13#10;
  {$endif}
  StdOut := TTextRec(fSaveToFile).Handle;
end;

procedure TSynTests.AddFailed(const msg: string);
begin
  if fFailedCount=length(fFailed) then
    SetLength(fFailed,NextGrow(fFailedCount));
  with fFailed[fFailedCount] do begin
    Error := msg;
    if fCurrentMethodInfo<>nil then begin
      TestName := fCurrentMethodInfo^.TestName;
      IdentTestName := fCurrentMethodInfo^.IdentTestName;
    end;
  end;
  inc(fFailedCount);
end;

function TSynTests.GetFailed(Index: integer): TSynTestFailed;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fFailedCount)) then
    Finalize(result) else
    result := fFailed[index];
end;

function TSynTests.GetFailedCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fFailedCount;
end;

function TSynTests.Run: Boolean;
var i,t,m: integer;
    Elapsed, Version: RawUTF8;
    err: string;
    C: TSynTestCase;
    log: IUnknown;
begin
  if TTextRec(fSaveToFile).Handle=0 then
    CreateSaveToFile;
  Color(ccLightCyan);
  Writeln(fSaveToFile,#13#10'   ',Ident,#13#10'  ',StringOfChar('-',length(Ident)+2));
  RunTimer.Start;
  Randomize;
  fFailed := nil;
  fAssertions := 0;
  fAssertionsFailed := 0;
  for m := 0 to Count-1 do
  try
    Color(ccWhite);
    writeln(fSaveToFile,#13#10#13#10,m+1,'. ',fTests[m].TestName);
    Color(ccLightGray);
    fTests[m].Method(); // call AddCase() to add instances into fTestCase
    try
      for i := 0 to fTestCase.Count-1 do begin
        C := TSynTestCaseClass(fTestCase[i]).Create(self);
        try
          Color(ccWhite);
          writeln(fSaveToFile,#13#10' ',m+1,'.',i+1,'. ',C.Ident,': ');
          Color(ccLightGray);
          C.fAssertions := 0; // reset assertions count
          C.fAssertionsFailed := 0;
          TotalTimer.Start;
          C.Setup;
          for t := 0 to C.Count-1 do
          try
            C.fAssertionsBeforeRun := C.fAssertions;
            C.fAssertionsFailedBeforeRun := C.fAssertionsFailed;
            C.fRunConsoleOccurenceNumber := fRunConsoleOccurenceNumber;
            fCurrentMethodInfo := @C.fTests[t];
            log := BeforeRun;
            TestTimer.Start;
            C.MethodSetup;
            try
              fCurrentMethodInfo^.Method(); // run tests + Check()
              AfterOneRun;
            finally
              C.MethodCleanUp;
            end;
            log := nil; // will trigger logging leave method e.g.
          except
            on E: Exception do begin
              Color(ccLightRed);
              AddFailed(E.ClassName+': '+E.Message);
              write(fSaveToFile,'! ',fCurrentMethodInfo^.IdentTestName);
              if E.InheritsFrom(EControlC) then
                raise; // Control-C should just abort whole test
              writeln(fSaveToFile,#13#10'! Exception ',E.ClassName,
                ' raised with messsage:'#13#10'!  ',E.Message);
              Color(ccLightGray);
            end;
          end;
          C.CleanUp; // should be done before Destroy call
          if C.AssertionsFailed=0 then
            Color(ccLightGreen) else
            Color(ccLightRed);
          Write(fSaveToFile,'  Total failed: ',IntToThousandString(C.AssertionsFailed),
            ' / ',IntToThousandString(C.Assertions),'  - ',C.Ident);
          if C.AssertionsFailed=0 then
            Write(fSaveToFile,' PASSED') else
            Write(fSaveToFile,' FAILED');
          Writeln(fSaveToFile,'  ',TotalTimer.Stop);
          Color(ccLightGray);
          inc(fAssertions,C.fAssertions); // compute global assertions count
          inc(fAssertionsFailed,C.fAssertionsFailed);
        finally
          C.Free;
        end;
      end;
    finally
      fTestCase.Clear;
      fCurrentMethodInfo := nil;
    end;
  except
    on E: Exception do begin
      // assume any exception not intercepted above is a failure
      Color(ccLightRed);
      err := E.ClassName+': '+E.Message;
      AddFailed(err);
      write(fSaveToFile,'! ',err);
    end;
  end;
  Color(ccLightCyan);
  result := (fFailedCount=0);
  if Exeversion.Version.Major<>0 then
    Version := FormatUTF8(#13#10'Software version tested: % (%)',
      [ExeVersion.Version.Detailed, ExeVersion.Version.BuildDateTimeString]);
  FormatUTF8(#13#10#13#10'Time elapsed for all tests: %'#13#10'Performed % by % on %',
    [RunTimer.Stop,NowToString,Exeversion.User,Exeversion.Host],Elapsed);
  Writeln(fSaveToFile,#13#10,Version,CustomVersions,
    #13#10'Generated with: ',GetDelphiCompilerVersion,' compiler',
    Utf8ToConsole(Elapsed));
  if result then
    Color(ccWhite) else
    Color(ccLightRed);
  write(fSaveToFile,#13#10'Total assertions failed for all test suits:  ',
    IntToThousandString(AssertionsFailed),' / ', IntToThousandString(Assertions));
  if result then begin
    Color(ccLightGreen);
    Writeln(fSaveToFile,#13#10'! All tests passed successfully.');
  end else
    Writeln(fSaveToFile,#13#10'! Some tests FAILED: please correct the code.');
  Color(ccLightGray);
end;

procedure TSynTests.AfterOneRun;
var Run,Failed: integer;
    C: TSynTestCase;
begin
  if fCurrentMethodInfo=nil then
    exit;
  C := fCurrentMethodInfo^.Test as TSynTestCase;
  Run := C.Assertions-C.fAssertionsBeforeRun;
  Failed := C.AssertionsFailed-C.fAssertionsFailedBeforeRun;
  if Failed=0 then begin
    Color(ccGreen);
    Write(fSaveToFile,'  - ',fCurrentMethodInfo^.TestName,': ');
    if Run=0 then
      Write(fSaveToFile,'no assertion') else
    if Run=1 then
      Write(fSaveToFile,'1 assertion passed') else
      Write(fSaveToFile,IntToThousandString(Run),' assertions passed');
  end else begin
    Color(ccLightRed);   // ! to highlight the line
    Write(fSaveToFile,'!  - ',fCurrentMethodInfo^.TestName,': ',
      IntToThousandString(Failed),' / ',IntToThousandString(Run),' FAILED');
  end;
  Write(fSaveToFile,'  ',TestTimer.Stop);
  if C.fRunConsoleOccurenceNumber>0 then
    Write(fSaveToFile,'  ',
      IntToThousandString(TestTimer.PerSec(C.fRunConsoleOccurenceNumber)),'/s');
  if C.fRunConsoleMemoryUsed>0 then begin
    Write(fSaveToFile,'  ',KB(C.fRunConsoleMemoryUsed));
    C.fRunConsoleMemoryUsed := 0; // display only once
  end;
  Writeln(fSaveToFile);
  if C.fRunConsole<>'' then begin
    Writeln(fSaveToFile,'     ',C.fRunConsole);
    C.fRunConsole := '';
  end;
  Color(ccLightGray);
end;

procedure TSynTests.SaveToFile(const DestPath, FileName: TFileName);
var FN: TFileName;
begin
  if TTextRec(fSaveToFile).Handle<>0 then
    Close(fSaveToFile);
  if FileName='' then
    FN := DestPath+Ident+'.txt' else
    FN := DestPath+FileName;
  if ExtractFilePath(FN)='' then
    FN := ExeVersion.ProgramFilePath+FN;
  system.assign(fSaveToFile,FN);
  rewrite(fSaveToFile);
  if IOResult<>0 then
    fillchar(fSaveToFile,sizeof(fSaveToFile),0);
end;

{$I+}

class procedure TSynTests.RunAsConsole(const CustomIdent: string;
  withLogs: TSynLogInfos; options: TSynTestOptions);
var tests: TSynTests;
begin
  if self=TSynTests then
    raise ESynException.Create('You should inherit from TSynTests');
  {$ifdef MSWINDOWS}
  AllocConsole;
  {$endif}
  with TSynLogTestLog.Family do begin
    Level := withLogs;
    //DestinationPath := ExtractFilePath(paramstr(0))+'logs'; folder should exist
    PerThreadLog := ptIdentifiedInOnFile;
    //HighResolutionTimestamp := true;
    //RotateFileCount := 5; RotateFileSizeKB := 20*1024; // rotate by 20 MB logs
  end;
  // testing is performed by some dedicated classes defined in the caller units
  tests := Create(CustomIdent);
  try
    tests.Options := options;
    if ParamCount<>0 then begin
      tests.SaveToFile(paramstr(1)); // DestPath on command line -> export to file
      Writeln(tests.Ident,#13#10#13#10' Running tests... please wait');
    end;
    tests.Run;
  finally
    tests.Free;
  end;
  {$ifndef LINUX}
  if ParamCount=0 then begin // direct exit if an external file was generated
    WriteLn(#13#10'Done - Press ENTER to Exit');
    ReadLn;
  end;
  {$endif}
end;


{ TSynTestsLogged }

function TSynTestsLogged.BeforeRun: IUnknown;
begin
  with fCurrentMethodInfo^ do
    result := TSynLogTestLog.Enter(Test,pointer(TestName));
end;

constructor TSynTestsLogged.Create(const Ident: string);
begin
  inherited Create(Ident);
  with TSynLogTestLog.Family do begin
    if integer(Level)=0 then // if no exception is set
      Level := [sllException,sllExceptionOS,sllFail];
    if AutoFlushTimeOut=0 then
      AutoFlushTimeOut := 2; // flush any pending text into .log file every 2 sec
    fLogFile := SynLog;
  end;
end;

type
  PTTextWriter = ^TTextWriter;

function SynTestsTextOut(var t: TTextRec): Integer;
begin
  if t.BufPos = 0 then
    Result := 0 else begin
    if FileWrite(t.Handle,t.BufPtr^,t.BufPos)<>integer(t.BufPos) then
      Result := GetLastError else
      Result := 0;
    if PTTextWriter(@t.UserData)^<>nil then
      PTTextWriter(@t.UserData)^.AddJSONEscape(t.BufPtr,t.Bufpos);
    t.BufPos := 0;
  end;
end;

procedure TSynTestsLogged.CreateSaveToFile;
begin
  inherited;
  with TTextRec(fSaveToFile) do begin
    InOutFunc := @SynTestsTextOut;
    FlushFunc := @SynTestsTextOut;
    fConsoleDup := TTextWriter.CreateOwnedStream;
    fConsoleDup.AddShort('{"Msg"="');
    PTTextWriter(@UserData)^ := fConsoleDup;
  end;
end;

destructor TSynTestsLogged.Destroy;
begin
  if (fLogFile<>nil) and (fLogFile.Writer<>nil) and (fConsoleDup<>nil) then begin
    fConsoleDup.Add('"','}');
    fLogFile.Log(sllCustom1,fConsoleDup.Text);
  end;
  inherited Destroy;
  fConsoleDup.Free;
end;

procedure TSynTestsLogged.AddFailed(const msg: string);
begin
  inherited;
  with fCurrentMethodInfo^ do
    fLogFile.Log(sllFail,'% [%]',[IdentTestName,msg],Test);
end;


end.
