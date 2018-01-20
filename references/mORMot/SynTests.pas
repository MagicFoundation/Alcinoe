/// Unit test functions used by Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynTests;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2018 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2018
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

  Version 1.18
  - first public release, extracted from SynCommons.pas unit
  - added class function TSynTestCase.RandomTextParagraph
  - TSynTests will now write the tests summary with colored console output
  - added TSynTestCase.CleanUp virtual method for proper cleaning before Destroy
  - added TSynTestCase.CheckMatchAny() method for multi-value checks
  - TSynTestCase.TestFailed now triggers a debugger breakpoint when run from IDE
  - added TSynTestCase.NotifyTestSpeed() method
  - extraction of TTestLowLevelCommon code into SynSelfTests.pas unit
  - added TSynTests.RunAsConsole() class method to ease console test app writing

*)


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

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
  SyncObjs, // for TEvent
  Contnrs,  // for TObjectList
{$ifdef HASINLINE}
  Types,
{$endif}
{$endif}
{$ifndef NOVARIANTS}
  Variants,
{$endif}
  SynLZ, // needed e.g. for TSynMapFile .mab format
  SynCommons,
  SynLog,
  SysUtils;


{ ************ Unit-Testing classes and functions }

type
  /// the prototype of an individual test
  // - to be used with TSynTest descendants
  TSynTestEvent = procedure of object;

{$M+} { we need the RTTI for the published methods of this object class }
  /// a generic class for both tests suit and cases
  // - purpose of this ancestor is to have RTTI for its published methods,
  // and to handle a class text identifier, or uncamelcase its class name
  // if no identifier was defined
  // - sample code about how to use this test framework is available in
  // the "Sample\07 - SynTest" folder
  // - see @https://synopse.info/forum/viewtopic.php?pid=277
  TSynTest = class
  protected
    fTests: array of record
      TestName: string;
      TestNameUTF8: RawUTF8;
      Method: TSynTestEvent;
    end;
    fIdent: string;
    fInternalTestsCount: integer;
    function GetTestName(Index: integer): string; {$ifdef HASINLINE}inline;{$endif}
    function GetTestMethod(Index: integer): TSynTestEvent; {$ifdef HASINLINE}inline;{$endif}
    function GetCount: Integer;
    function GetIdent: string;
  public
    /// create the test instance
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = '');
    /// register a specified test to this class instance
    procedure Add(const aMethod: TSynTestEvent; const aName: string);
    /// the test name
    // - either the Ident parameter supplied to the Create() method, either
    // a uncameled text from the class name
    property Ident: string read GetIdent;
    /// return the number of tests associated with this class
    // - i.e. the number of registered tests by the Register() method PLUS
    // the number of published methods defined within this class
    property Count: Integer read GetCount;
    /// get the name of a specified test
    // - Index range is from 0 to Count-1 (including)
    property TestName[Index: integer]: string read GetTestName;
    /// get the event of a specified test
    // - Index range is from 0 to Count-1 (including)
    property TestMethod[Index: integer]: TSynTestEvent read GetTestMethod;
    /// return the number of published methods defined within this class as tests
    // - i.e. the number of tests added by the Create() constructor from RTTI
    // - any TestName/TestMethod[] index higher or equal to this value has been
    // added by a specific call to the Add() method
    property InternalTestsCount: integer read fInternalTestsCount;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter }
  end;
{$M-}

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
    fMethodIndex: integer;
    fTestCaseIndex: integer;
    /// any number not null assigned to this field will display a "../s" stat
    fRunConsoleOccurenceNumber: cardinal;
    /// any number not null assigned to this field will display a "using .. MB" stat
    fRunConsoleMemoryUsed: Int64;
    /// any text assigned to this field will be displayed on console
    fRunConsole: string;
    fCheckLogTime: TPrecisionTimer;
    /// override this method to process some clean-up before Destroy call
    // - WARNING: this method should be re-entrant - so using FreeAndNil() is
    // a good idea in this method :)
    procedure CleanUp; virtual;
  public
    /// create the test case instance
    // - must supply a test suit owner
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    constructor Create(Owner: TSynTests; const Ident: string = ''); virtual;
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
    /// used by the published methods to run a test assertion about two double values
    function CheckSame(const Value1,Value2: double;
      const Precision: double=1E-12; const msg: string = ''): Boolean;
    /// perform a string comparison with several value
    // - test passes if (Value=Values[0]) or (Value=Value[1]) or (Value=Values[...
    // and ExpectedResult=true
    procedure CheckMatchAny(const Value: RawUTF8; const Values: array of RawUTF8;
      CaseSentitive: Boolean=true; ExpectedResult: Boolean=true; const msg: string = '');
    /// used by the published methods to run a test assertion, with a error
    // message computed via FormatUTF8()
    // - condition must equals TRUE to pass the test
    procedure CheckUTF8(condition: Boolean; const msg: RawUTF8; const args: array of const);
    /// used by published methods to start some timing on associated log
    // - call this once, before one or several consecutive CheckLogTime()
    procedure CheckLogTimeStart;
      {$ifdef HASINLINE}inline;{$endif}
    /// used by published methods to write some timing on associated log
    // - at least one CheckLogTimeStart method call should happen to reset the
    // internal timer
    // - condition must equals TRUE to pass the test
    // - the supplied message would be appended, with its timing
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
    /// create a temporary string, containing some fake text, with paragraphs
    class function RandomTextParagraph(WordCount: Integer;
      LastPunctuation: AnsiChar='.'; const RandomInclude: RawUTF8=''): RawUTF8;
    /// this method is triggered internaly - e.g. by Check() - when a test failed
    procedure TestFailed(const msg: string);
    /// will add to the console a message with a speed estimation
    // - speed is computed from the method start
    procedure NotifyTestSpeed(const ItemName: string; ItemCount: integer;
      SizeInBytes: cardinal=0; Timer: PPrecisionTimer=nil);
    /// append some text to the current console
    procedure AddConsole(const msg: string);
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
    /// the index of the associated Owner.TestMethod[] which created this test
    property MethodIndex: integer read fMethodIndex;
    /// the index of the test case, starting at 0 for the associated MethodIndex
    property TestCaseIndex: integer read fTestCaseIndex;
  published
    { all published methods of the children will be run as individual tests
      - these methods must be declared as procedure with no parameter
      - the method name will be used, after "uncamelcasing", for display }
  end;

  /// class-reference type (metaclass) of a test case
  TSynTestCaseClass = class of TSynTestCase;

  /// a class used to run a suit of test cases
  TSynTests = class(TSynTest)
  protected
    /// any number not null assigned to this field will display a "../sec" stat
    fRunConsoleOccurenceNumber: cardinal;
    /// a list containing all failed tests after a call to the Run method
    // - if integer(Objects[]) is equal or higher than InternalTestsCount,
    // the Objects[] points to the TSynTestCase, and the Strings[] to the
    // associated failure message
    // - if integer(Objects[]) is lower than InternalTestsCount, then
    // it is an index to the corresponding published method, and the Strings[]
    // contains the associated failure message
    fFailed: TStringList;
    fTestCase: TObjectList;
    fAssertions: integer;
    fAssertionsFailed: integer;
    fCurrentMethod, fCurrentMethodIndex: integer;
    fSaveToFile: Text;
    function GetTestCase(Index: integer): TSynTestCase;
    function GetTestCaseCount: Integer;
    function GetFailedCaseIdent(Index: integer): string;
    function GetFailedCount: integer;
    function GetFailedMessage(Index: integer): string;
    function GetFailedCase(Index: integer): TSynTestCase;
    procedure CreateSaveToFile; virtual;
    procedure Color(aColor: TConsoleColor);
    /// called when a test case failed: default is to add item to fFailed[]
    procedure Failed(const msg: string; aTest: TSynTestCase); virtual;
    /// this method is called before every run
    // - default implementation will just return nil
    // - can be overridden to implement a per-test case logging for instance
    function BeforeRun(const TestName: RawUTF8): IUnknown; virtual;
    /// this method is called during the run, for every testcase
    // - this implementation just report some minimal data to the console
    // by default, but may be overridden to update a real UI or reporting system
    // - the TestMethodIndex is first -1 before any TestMethod[] method call,
    // then called once after every TestMethod[] run
    procedure DuringRun(TestCaseIndex, TestMethodIndex: integer); virtual;
  public
    /// you can put here some text to be displayed at the end of the messages
    // - some internal versions, e.g.
    // - every line of text must explicitly BEGIN with #13#10
    CustomVersions: string;
    /// contains the run elapsed time
    RunTimer, TestTimer, TotalTimer: TPrecisionTimer;
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
      withLogs: TSynLogInfos=[sllLastError,sllError,sllException,sllExceptionOS]); virtual;
    /// create the test instance
    // - if an identifier is not supplied, the class name is used, after
    // T[Syn][Test] left trim and un-camel-case
    // - this constructor will add all published methods to the internal
    // test list, accessible via the Count/TestName/TestMethod properties
    constructor Create(const Ident: string = '');
    /// finalize the class instance
    // - release all registered Test case instance
    destructor Destroy; override;
    /// save the debug messages into an external file
    // - if no file name is specified, the current Ident is used
    procedure SaveToFile(const DestPath: TFileName; const FileName: TFileName='');
    /// register a specified Test case instance
    // - all these instances will be freed by the TSynTests.Destroy
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase(TOneTestCase.Create(self));
    procedure AddCase(TestCase: TSynTestCase); overload;
    /// register a specified Test case from its class name
    // - all these instances will be freed by the TSynTests.Destroy
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase(TOneTestCase);
    procedure AddCase(TestCase: TSynTestCaseClass); overload;
    /// register a specified Test case from its class name
    // - an instance of the supplied class is created, and will be freed by
    // TSynTests.Destroy
    // - the published methods of the children must call this method in order
    // to add test cases
    // - example of use (code from a TSynTests published method):
    // !  AddCase([TOneTestCase]);
    procedure AddCase(const TestCase: array of TSynTestCaseClass); overload;
    /// call of this method will run all associated tests cases
    // - function will return TRUE if all test passed
    // - all failed test cases will be added to the Failed[] list
    // - the TestCase[] list is created first, by running all published methods,
    // which must call the AddCase() method above to register test cases
    // - the Failed[] list is cleared at the beginning of the run
    // - Assertions and AssertionsFailed properties are reset and computed during
    // the run
    function Run: Boolean; virtual;
    /// the number of items in the TestCase[] array
    property TestCaseCount: Integer read GetTestCaseCount;
    /// an array containing all registered Test case instances
    // - Test cases are registered by the AddCase() method above, mainly
    // by published methods of the children
    // - Test cases instances are freed by TSynTests.Destroy
    property TestCase[Index: integer]: TSynTestCase read GetTestCase;
    /// number of failed tests after the last call to the Run method
    property FailedCount: integer read GetFailedCount;
    /// retrieve the TSynTestCase instance associated with this failure
    // - returns nil if this failure was not triggered by a TSynTestCase,
    // but directly by a method
    property FailedCase[Index: integer]: TSynTestCase read GetFailedCase;
    /// retrieve the ident of the case test associated with this failure
    property FailedCaseIdent[Index: integer]: string read GetFailedCaseIdent;
    /// retrieve the error message associated with this failure
    property FailedMessage[Index: integer]: string read GetFailedMessage;
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
    procedure Failed(const msg: string; aTest: TSynTestCase); override;
    /// this method is called before every run
    // - overridden implementation to implement a per-test case logging
    function BeforeRun(const TestName: RawUTF8): IUnknown; override;
  public
    /// create the test instance and initialize associated LogFile instance
    // - this will allow logging of all exceptions to the LogFile
    constructor Create(const Ident: string = '');
    /// release associated memory
    destructor Destroy; override;
    /// the .log file generator created if any test case failed
    property LogFile: TSynLog read fLogFile;
  end;


implementation

{$ifdef FPC}
{$ifndef MSWINDOWS}
uses
  SynFPCLinux;
{$endif}
{$endif}

{ TSynTest }

procedure TSynTest.Add(const aMethod: TSynTestEvent; const aName: string);
var i: integer;
begin
  if self=nil then
    exit; // avoid GPF
  i := Length(fTests);
  SetLength(fTests,i+1);
  fTests[i].TestName := aName;
  fTests[i].TestNameUTF8 := StringToUTF8(fIdent+' - '+aName);
  fTests[i].Method := aMethod;
end;

constructor TSynTest.Create(const Ident: string);
var id: RawUTF8;
    methods: TPublishedMethodInfoDynArray;
    i: integer;
begin
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
        delete(Name,1,1) else
        Name := UnCamelCase(Name);
      Add(TSynTestEvent(Method),Ansi7ToString(Name));
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

function TSynTest.GetTestMethod(Index: integer): TSynTestEvent;
begin
  if (self=nil) or (Cardinal(Index)>=Cardinal(length(fTests))) then
    result := nil else
    result := fTests[Index].Method;
end;

function TSynTest.GetTestName(Index: integer): string;
begin
  if (self=nil) or (Cardinal(Index)>=Cardinal(length(fTests))) then
    result := '' else
    result := fTests[Index].TestName;
end;


{ TSynTestCase }

{.$define CHECKDOLOG}

procedure TSynTestCase.Check(condition: Boolean; const msg: string);
begin
  if self=nil then
    exit;
  {$ifdef CHECKDOLOG}
  if msg<>'' then
    TSynLogTestLog.Add.Log(sllTrace,msg);
  {$endif}
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
  {$ifdef CHECKDOLOG}
  if msg<>'' then
    TSynLogTestLog.Add.Log(sllTrace,msg);
  {$endif}
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

function TSynTestCase.CheckSame(const Value1, Value2, Precision: double;
  const msg: string): Boolean;
begin
  result := CheckFailed(SameValue(Value1,Value2,Precision),msg);
end;

procedure TSynTestCase.CheckMatchAny(const Value: RawUTF8;
  const Values: array of RawUTF8; CaseSentitive,ExpectedResult: Boolean; const msg: string);
begin
  Check((FindRawUTF8(Values,Value,CaseSentitive)>=0)=ExpectedResult);
end;

procedure TSynTestCase.CheckUTF8(condition: Boolean; const msg: RawUTF8;
  const args: array of const);
  procedure PerformFail;
  var utf8: RawUTF8;
  begin
    FormatUTF8(msg,args,utf8);
    TestFailed(UTF8ToString(utf8));
  end;
begin
  if condition then
    InterlockedIncrement(fAssertions) else
    PerformFail;
end;

procedure TSynTestCase.CheckLogTimeStart;
begin
  fCheckLogTime.Start;
end;

procedure TSynTestCase.CheckLogTime(condition: boolean; const msg: RawUTF8;
  const args: array of const; level: TSynLogInfo);
var utf8: RawUTF8;
begin
  FormatUTF8(msg,args,utf8);
  Check(condition,UTF8ToString(utf8));
  TSynLogTestLog.Add.Log(level,utf8+' '+fCheckLogTime.Stop);
  fCheckLogTime.Start;
end;

constructor TSynTestCase.Create(Owner: TSynTests; const Ident: string);
begin
  inherited Create(Ident);
  fOwner := Owner;
end;

procedure TSynTestCase.CleanUp;
begin
  // do nothing by default
end;

destructor TSynTestCase.Destroy;
begin
  CleanUp;
  inherited;
end;

class function TSynTestCase.RandomString(CharCount: Integer): RawByteString;
var i: integer;
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
var i: integer;
    R: PByteArray;
    tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  SetString(result,nil,CharCount);
  for i := 0 to CharCount-1 do
    PByteArray(result)[i] := 32+R[i] mod 94;
  tmp.Done;
end;

class function TSynTestCase.RandomIdentifier(CharCount: Integer): RawByteString;
const CHARS: array[0..63] of AnsiChar =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
var i: integer;
    R: PByteArray;
    tmp: TSynTempBuffer;
begin
  R := tmp.InitRandom(CharCount);
  SetString(result,nil,CharCount);
  for i := 0 to CharCount-1 do
    PByteArray(result)[i] := ord(CHARS[integer(R[i]) and 63]);
  tmp.Done;
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
type TKind = (space, comma, dot, question, paragraph);
const bla: array[0..4] of string[3]=('bla','ble','bli','blo','blu');
      endKind = [dot,paragraph,question];
var n: integer;
    WR: TTextWriter;
    s: string[3];
    last: TKind;
begin
  WR := TTextWriter.CreateOwnedStream;
  try
    last := paragraph;
    while WordCount>0 do begin
      for n := 0 to random(4) do begin
         s := bla[random(5)];
        if last in endKind then
          s[1] := upcase(s[1]);
        WR.AddShort(s);
        WR.Add(' ');
        dec(WordCount);
        last := space;
      end;
      WR.CancelLastChar(' ');
      case random(100) of
      0..2: begin
        if RandomInclude<>'' then begin
          WR.Add(' ');
          WR.AddString(RandomInclude);
        end;
        last := space;
      end;
      3..40:  last := space;
      41..70: last := comma;
      71..85: last := dot;
      86..90: last := question;
      91..99: last := paragraph;
      end;
      case last of
      space: WR.Add(' ');
      comma: WR.Add(',',' ');
      dot:   WR.Add('.',' ');
      question:  WR.Add('?',' ');
      paragraph: WR.AddShort('.'#13#10);
      end;
    end;
    if not (last in endKind) then begin
      WR.AddShort('bla');
      if LastPunctuation<>' ' then
        WR.Add(LastPunctuation);
    end;
    WR.SetText(result);
  finally
    WR.Free;
  end;
end;

procedure TSynTestCase.TestFailed(const msg: string);
begin
  TSynLogTestLog.DebuggerNotify(sllFail,'#% %',[fAssertions-fAssertionsBeforeRun,msg]);
  if Owner<>nil then // avoid GPF
    Owner.Failed(msg,self);
  InterlockedIncrement(fAssertionsFailed);
end;

procedure TSynTestCase.AddConsole(const msg: string);
begin
  if fRunConsole<>'' then
    fRunConsole := fRunConsole+#13#10'     '+msg else
    fRunConsole := fRunConsole+msg;
end;

procedure TSynTestCase.NotifyTestSpeed(const ItemName: string;
  ItemCount: integer; SizeInBytes: cardinal; Timer: PPrecisionTimer);
var Temp: TPrecisionTimer;
    msg: string;
begin
  if Timer=nil then
    Temp := Owner.TestTimer else
    Temp := Timer^;
  msg := format('%d %s in %s i.e. %d/s, aver. %s',
    [ItemCount,ItemName,Temp.Stop,Temp.PerSec(ItemCount),Temp.ByCount(ItemCount)]);
  if SizeInBytes>0 then
    msg := format('%s, %s/s',[msg,KB(Temp.PerSec(SizeInBytes))]);
  AddConsole(msg);
end;


{ TSynTests }

procedure TSynTests.AddCase(TestCase: TSynTestCase);
begin
  TestCase.fMethodIndex := fCurrentMethod;
  TestCase.fTestCaseIndex := fTestCase.Count-fCurrentMethodIndex;
  fTestCase.Add(TestCase);
end;

procedure TSynTests.AddCase(const TestCase: array of TSynTestCaseClass);
var i: integer;
begin
  for i := low(TestCase) to high(TestCase) do
    AddCase(TestCase[i].Create(self));
end;

procedure TSynTests.AddCase(TestCase: TSynTestCaseClass);
begin
  AddCase(TestCase.Create(self));
end;

function TSynTests.BeforeRun(const TestName: RawUTF8): IUnknown;
begin
  result := nil;
end;

constructor TSynTests.Create(const Ident: string);
begin
  inherited;
  fFailed := TStringList.Create;
  fTestCase := TObjectList.Create;
end;

{$I-}

procedure TSynTests.Color(aColor: TConsoleColor);
begin
  if (StdOut<>0) and (THandle(TTextRec(fSaveToFile).Handle)=StdOut) then
    TextColor(aColor);
end;

procedure TSynTests.CreateSaveToFile;
begin
  Assign(fSaveToFile,'');
  Rewrite(fSaveToFile);
  StdOut := TTextRec(fSaveToFile).Handle;
end;

destructor TSynTests.Destroy;
begin
  fFailed.Free;
  fTestCase.Free; // TObjectList will free all TSynTestCase instance
  if TTextRec(fSaveToFile).Handle<>0 then
    Close(fSaveToFile);
  inherited Destroy;
end;

procedure TSynTests.DuringRun(TestCaseIndex, TestMethodIndex: integer);
var C: TSynTestCase;
    Run,Failed: integer;
begin
  C := TestCase[TestCaseIndex];
  if C=nil then
    Exit;
  if TestMethodIndex<0 then begin
    Color(ccWhite);
    writeln(fSaveToFile,#13#10' ',C.MethodIndex+1,'.',C.TestCaseIndex+1,
      '. ',C.Ident,': '{$ifdef LINUX},#13{$endif});
  end else begin
    Run := C.Assertions-C.fAssertionsBeforeRun;
    Failed := C.AssertionsFailed-C.fAssertionsFailedBeforeRun;
    if Failed=0 then begin
      Color(ccGreen);
      Write(fSaveToFile,'  - ',C.TestName[TestMethodIndex],': ');
      if Run=0 then
        Write(fSaveToFile,'no assertion') else
      if Run=1 then
        Write(fSaveToFile,'1 assertion passed') else
        Write(fSaveToFile,IntToThousandString(Run),' assertions passed');
    end else begin
      Color(ccLightRed);
      Write(fSaveToFile,'!  - ',C.TestName[TestMethodIndex],': ',
        IntToThousandString(Failed),' / ',
        IntToThousandString(Run),' FAILED'); // ! to highlight the line
    end;
    Write(fSaveToFile,'  ',TestTimer.Stop);
    if C.fRunConsoleOccurenceNumber>0 then
      Write(fSaveToFile,'  ',
        IntToThousandString(TestTimer.PerSec(C.fRunConsoleOccurenceNumber)),'/s');
    if C.fRunConsoleMemoryUsed>0 then begin
      Write(fSaveToFile,'  ',KB(C.fRunConsoleMemoryUsed));
      C.fRunConsoleMemoryUsed := 0; // display only once
    end;
    Writeln(fSaveToFile{$ifdef LINUX},#13{$endif});
    if C.fRunConsole<>'' then begin
      Writeln(fSaveToFile,'     ',C.fRunConsole{$ifdef LINUX},#13{$endif});
      C.fRunConsole := '';
    end;
    if TestMethodIndex=C.Count-1 then begin
      if C.AssertionsFailed=0 then
        Color(ccLightGreen) else
        Color(ccLightRed);
      Write(fSaveToFile,'  Total failed: ',IntToThousandString(C.AssertionsFailed),
        ' / ',IntToThousandString(C.Assertions),'  - ',C.Ident);
      if C.AssertionsFailed=0 then
        Write(fSaveToFile,' PASSED') else
        Write(fSaveToFile,' FAILED');
      Writeln(fSaveToFile,'  ',TotalTimer.Stop{$ifdef LINUX},#13{$endif});
    end;
    Color(ccLightGray);
  end;
end;

procedure TSynTests.Failed(const msg: string; aTest: TSynTestCase);
begin
  fFailed.AddObject(msg,aTest);
end;

function TSynTests.GetFailedCase(Index: integer): TSynTestCase;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fFailed.Count)) then
    Result := nil else begin
    Index := integer(fFailed.Objects[index]);
    if Index<InternalTestsCount then
      Result := nil else
      Result := TSynTestCase(Index);
  end;
end;

function TSynTests.GetFailedCaseIdent(Index: integer): string;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fFailed.Count)) then
    Result := '' else begin
    Index := integer(fFailed.Objects[index]);
    if Index<InternalTestsCount then
      // if integer(Objects[]) is lower than InternalTestsCount, then
      // it is an index to the corresponding published method, and the Strings[]
      // contains the associated failure message
      Result := TestName[Index] else
      // if integer(Objects[]) is equal or higher than InternalTestsCount,
      // the Objects[] points to the TSynTestCase, and the Strings[] to the
      // associated failure message
      Result := TSynTestCase(Index).Ident;
  end;
end;

function TSynTests.GetFailedCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fFailed.Count;
end;

function TSynTests.GetFailedMessage(Index: integer): string;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fFailed.Count)) then
    result := '' else
    result := fFailed.Strings[Index];
end;

function TSynTests.GetTestCase(Index: integer): TSynTestCase;
begin
  if (self=nil) or (cardinal(Index)>=cardinal(fTestCase.Count)) then
    Result := nil else
    Result := TSynTestCase(fTestCase[Index]);
end;

function TSynTests.GetTestCaseCount: Integer;
begin
  if self=nil then
    result := 0 else
    result := fTestCase.Count;
end;

function TSynTests.Run: Boolean;
var i,t,m: integer;
    Elapsed, Version: RawUTF8;
    C: TSynTestCase;
    ILog: IUnknown;
begin
  if TTextRec(fSaveToFile).Handle=0 then
    CreateSaveToFile;
  Color(ccLightCyan);
  Writeln(fSaveToFile,#13#10'   ',Ident,#13#10'  ',StringOfChar('-',length(Ident)+2));
  RunTimer.Start;
  try
    // 1. register all test cases
    fTestCase.Clear;
    for m := 0 to Count-1 do begin
      fCurrentMethod := m;
      fCurrentMethodIndex := fTestCase.Count;
      // published methods will call AddCase() to register tests in fTestCase[]
      TSynTestEvent(TestMethod[m])();
    end;
    // 2. launch the tests
    Randomize;
    fFailed.Clear;
    fAssertions := 0;
    fAssertionsFailed := 0;
    m := -1;
    for i := 0 to fTestCase.Count-1 do begin
      C := TestCase[i];
      if C.MethodIndex<>m then begin
        m := C.MethodIndex;
        Color(ccWhite);
        writeln(fSaveToFile,#13#10#13#10,m+1,'. ',TestName[m]);
        Color(ccLightGray);
      end;
      DuringRun(i,-1);
      C.fAssertions := 0; // reset assertions count
      C.fAssertionsFailed := 0;
      TotalTimer.Start;
      for t := 0 to C.Count-1 do
      try
        C.fAssertionsBeforeRun := C.fAssertions;
        C.fAssertionsFailedBeforeRun := C.fAssertionsFailed;
        C.fRunConsoleOccurenceNumber := fRunConsoleOccurenceNumber;
        fCurrentMethod := i;
        fCurrentMethodIndex := t;
        ILog := BeforeRun(C.fTests[t].TestNameUTF8);
        TestTimer.Start;
        TSynTestEvent(C.TestMethod[t])(); // run tests + Check() and TestFailed()
        ILog := nil; // will trigger logging leave method e.g.
        DuringRun(i,t);
      except
        on E: Exception do begin
          Color(ccLightRed);
          fFailed.AddObject(E.ClassName+': '+E.Message,C);
          write(fSaveToFile,'! ',C.fTests[t].TestNameUTF8);
          if E.InheritsFrom(EControlC) then
            raise; // Control-C should just abort whole test
          writeln(fSaveToFile,#13#10'! Exception ',E.ClassName,
            ' raised with messsage:'#13#10'!  ',E.Message);
          Color(ccLightGray);
        end;
      end;
      C.CleanUp; // should be done before Destroy call
      inc(fAssertions,C.fAssertions); // compute global assertions count
      inc(fAssertionsFailed,C.fAssertionsFailed);
    end;
  except
    on E: Exception do begin
      // assume any exception not intercepted above is a failure
      Color(ccLightRed);
      fFailed.AddObject(E.ClassName+': '+E.Message,nil);
      writeln(fSaveToFile,#13#10'! Exception ',E.ClassName,
        ' raised with messsage:'#13#10'!  ',E.Message);
    end;
  end;
  Color(ccLightCyan);
  result := (fFailed.Count=0);
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
  assign(fSaveToFile,FN);
  rewrite(fSaveToFile);
  if IOResult<>0 then
    fillchar(fSaveToFile,sizeof(fSaveToFile),0);
end;

{$I+}

class procedure TSynTests.RunAsConsole(const CustomIdent: string;
  withLogs: TSynLogInfos);
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

function TSynTestsLogged.BeforeRun(const TestName: RawUTF8): IUnknown;
begin
  result := TSynLogTestLog.Enter(TObject(nil),pointer(TestName));
end;

constructor TSynTestsLogged.Create(const Ident: string);
begin
  inherited;
  with TSynLogTestLog.Family do begin
    if integer(Level)=0 then // if no exception is set
      Level := [sllException,sllExceptionOS,sllFail];
    {$ifdef MSWINDOWS}
    if AutoFlushTimeOut=0 then
      AutoFlushTimeOut := 2; // flush any pending text into .log file every 2 sec
    {$endif}
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

procedure TSynTestsLogged.Failed(const msg: string; aTest: TSynTestCase);
begin
  inherited;
  with TestCase[fCurrentMethod] do begin
    fLogFile.Log(sllFail,'%: % "%"',[Ident,TestName[fCurrentMethodIndex],msg],aTest);
    {$ifdef KYLIX3}
    fLogFile.Flush(true);
    // we do not have a debugger for CrossKylix -> stop here!
    TextColor(ccLightRed);
    writeln('!!! ',Ident,' - ',TestName[fCurrentMethodIndex],' "',msg,'" failed !!!');
    write('Press [Enter] to continue, or Ctrl+C to abort ');
    readln;
    {$endif}
  end;
end;

end.
