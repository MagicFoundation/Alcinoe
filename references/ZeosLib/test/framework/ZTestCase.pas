{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Classes for Testing Framework         }
{                                                         }
{ Originally written by Sergey Merkuriev, Sergey Seroukhov}
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZTestCase;

interface

{$I ZTestFramework.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, {$IFDEF FPC}fpcunit{$ELSE}TestFramework{$ENDIF}, ZCompatibility;

type
  {$IFDEF FPC}
  CTZAbstractTestCase=Class of TZAbstractTestCase;
  {$ENDIF}

  TDatePart = (dpYear, dpMonth, dpDay, dpHour, dpMin, dpSec, dpMSec);
  TDateParts = set of TDatePart;

  {** Implements an abstract class for all test cases. }

  { TZAbstractTestCase }

  TZAbstractTestCase = class(TTestCase)
  private
    FDecimalSeparator: Char;
    FSuppressTestOutput: Boolean;

  protected
    {$IFDEF FPC}
    frefcount : longint;
    { implement methods of IUnknown }
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
    procedure CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');
    {$ENDIF}
    property DecimalSeparator: Char read FDecimalSeparator
      write FDecimalSeparator;
    property SuppressTestOutput: Boolean read FSuppressTestOutput
      write FSuppressTestOutput;

    { Test configuration methods. }
    procedure LoadConfiguration; virtual;

    { Configuration properties accessing methods. }
    function ReadProperty(const Group, Key, Default: string): string;
    function ReadGroupProperty(const Key, Default: string): string;
    function ReadInheritProperty(const Key, Default: string): string;

    { Visual output methods. }
    procedure Print(_Message: string); virtual;
    procedure PrintLn(_Message: string = ''); virtual;

    { Additional checking methods. }
    procedure CheckEquals(Array1, Array2: TByteDynArray;
      _Message: string = ''); overload;
    procedure CheckEquals(Expected, Actual: TStream;
      const Msg: string = ''); overload;
    procedure CheckEqualsDate(const Expected, Actual: TDateTime;
      Parts: TDateParts = []; const Msg: string = '');

    { Measurement methods. }
    function GetTickCount: Cardinal;

  public
    constructor Create(MethodName: string); {$IFNDEF FPC} override; {$ELSE} overload;{$ENDIF}
    destructor Destroy; override;
    {$IFDEF FPC}
    constructor Create; override; overload;
    function GetName: string;
    procedure Fail(msg: string; errorAddr: Pointer = nil);
    procedure CheckNotNull(obj: IUnknown; msg: string = ''); overload; virtual;
    procedure CheckNull(obj: IUnknown; msg: string = ''); overload; virtual;
    class function Suite : CTZAbstractTestCase;
    {$ENDIF}
  end;

  {** Implements a generic test case. }
  TZGenericTestCase = class (TZAbstractTestCase);

implementation

uses
{$IFDEF FPC}
  LCLIntf,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, ZSysUtils, ZTestConfig;

{$IFDEF FPC}
function CallerAddr: Pointer;
begin
  Result := nil;
end;
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

function GetMemDiffStr(expected, actual: pointer; size:longword; msg:string):string;
var
  db1, db2: byte;
  Offset: integer;
begin
  Offset:=FirstByteDiff(expected,actual,size,db1,db2);
  Result:=Format('%s expected: <%s> but was: <%s>',[msg,IntToHex(db1,2),IntToHex(db2,2)]);
  Result:=Result+' at Offset = '+IntToHex(Offset,4)+'h';
end;
{$ENDIF}

{ TZAbstractTestCase }

{$IFDEF FPC}

function TZAbstractTestCase.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult;
  stdcall;
begin
  if getinterface(iid,obj) then
   result:=0
  else
   result:=longint(E_NOINTERFACE);
end;

function TZAbstractTestCase._AddRef: longint; stdcall;
begin
  _addref:=interlockedincrement(frefcount);
end;

function TZAbstractTestCase._Release: longint; stdcall;
begin
  _Release:=interlockeddecrement(frefcount);
  if _Release=0 then self.destroy;
end;

function TZAbstractTestCase.GetName: string;
begin
   Result := GetTestName;
end;

procedure TZAbstractTestCase.Fail(msg: string; errorAddr: Pointer = nil);
begin
  if errorAddr = nil then
    raise EAssertionFailedError.Create(msg) at CallerAddr
  else
    raise EAssertionFailedError.Create(msg) at errorAddr;
end;

class function TZAbstractTestCase.Suite: CTZAbstractTestCase;
begin
  result := Self;
end;

procedure TZAbstractTestCase.CheckNotNull(obj: IUnknown; msg: string);
begin
    if obj = nil then
      Fail(msg, CallerAddr);
end;

procedure TZAbstractTestCase.CheckNull(obj: IUnknown; msg: string);
begin
    if obj <>  nil then
      Fail(msg, CallerAddr);
end;

procedure TZAbstractTestCase.CheckEqualsMem(expected, actual: pointer; size:longword; msg:string='');
begin
  if not CompareMem(expected, actual, size) then
    Fail(GetMemDiffStr(expected, actual, size, msg), CallerAddr);
end;

constructor TZAbstractTestCase.Create;
begin
  inherited Create;
  LoadConfiguration;
end;
{$ENDIF}

{**
  Creates the abstract test case and initialize global parameters.
  @param MethodName a name of the test case.
}
constructor TZAbstractTestCase.Create(MethodName: string);
begin
  {$IFNDEF FPC}
  inherited Create(MethodName);
  LoadConfiguration;
  {$ELSE}
  inherited CreateWithName(MethodName);
  {$ENDIF}
end;

{**
  Destroys this test case and cleanups the memory.
}
destructor TZAbstractTestCase.Destroy;
begin
  inherited Destroy;
end;

{**
  Loads a configuration from the configuration file.
}
procedure TZAbstractTestCase.LoadConfiguration;
var
  Temp: string;
begin
  { Defines a decimal separator for the tests. }
  Temp := ReadInheritProperty(DECIMAL_SEPARATOR_KEY,
    DEFAULT_DECIMAL_SEPARATOR);
  if Temp <> '' then
    FDecimalSeparator := Temp[1]
  else FDecimalSeparator :=  DEFAULT_DECIMAL_SEPARATOR;
  SysUtils.DecimalSeparator := FDecimalSeparator;

  { Defines a 'suppress test output' setting. }
  Temp := ReadInheritProperty(SUPPRESS_TEST_OUTPUT_KEY, TRUE_VALUE);
  FSuppressTestOutput := StrToBoolEx(Temp);
end;

{**
  Reads a configuration property from test config file.
  @param Key a property key.
  @param Default a property default value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZAbstractTestCase.ReadProperty(
  const Group, Key, Default: string): string;
begin
  Result := TestConfig.ReadProperty(Group, Key, Default);
end;

{**
  Reads a configuration property from test config file
  for the current test group defined by TestGroup global variable.
  @param Key a property key.
  @param Default a property default value.
  @returns a read property value or default value if property
    was not found in the config file.
}
function TZAbstractTestCase.ReadGroupProperty(
  const Key, Default: string): string;
begin
  Result := TestConfig.ReadProperty(TestGroup, Key, Default);
end;

{**
  Reads a configuration property for the current test group
  defined by TestGroup global variable. If key is not defined
  in the current group it reads the same key from "common" group.
  @param Key a property key.
  @param Default a property default value.
  @returns an interited property value or default value if property
    was not found in the config file neither in the current
    nor in the "common" group.
}
function TZAbstractTestCase.ReadInheritProperty(
  const Key, Default: string): string;
const
  UNKNOWN_VALUE = '';
begin
  Result := TestConfig.ReadProperty(TestGroup, Key, UNKNOWN_VALUE);
  if Result = UNKNOWN_VALUE then
    Result := TestConfig.ReadProperty(COMMON_GROUP, Key, Default);
end;

{**
  Function compare two arrays. If arrays not equals raise exception.
  @param the first array for compare
  @param the secon array for compare
}
procedure TZAbstractTestCase.CheckEquals(Array1, Array2: TByteDynArray;
  _Message: string = '');
var
  Size1, Size2: Integer;
  P1, P2: Pointer;
begin
  Size1 := Length(Array1);
  Size2 := Length(Array2);
  if Size1 <> Size2 then
    FailNotEquals(IntToStr(Size1), IntToStr(Size1), _Message, CallerAddr);

  P1 := Addr(Array1);
  P2 := Addr(Array2);
  if CompareMem(P1, P2, High(Array1)) then
    Fail('Arrays not equal.' +  _Message)
end;

{**
   Function compare two streams. If streams not equals raise exception.
   @param Expected the first stream for compare
   @param Actual the second stream for compare
}
procedure TZAbstractTestCase.CheckEquals(Expected, Actual: TStream;
  const Msg: string = '');
var
  EBuf, ABuf: PByteArray;
  Size, ERead, ARead: Integer;
begin
  if Expected = Actual then Exit;
  if not Assigned(Actual) and Assigned(Expected) then
    Fail('Expected stream, but NIL receved.' + Msg);
  if Assigned(Actual) and not Assigned(Expected) then
    Fail('Expected NIL stream, but real stream receved.' + Msg);
  Size := Expected.Size;
  if Size <> Actual.Size then
    Fail(Format('Different stream size. Expected: %d. Actual: %d.', [Size, Actual.Size]) + Msg);
  GetMem(EBuf, Size);
  GetMem(ABuf, Size);
  try
    Expected.Position := 0;
    Actual.Position := 0;
    ERead := Expected.Read(EBuf^, Size);
    ARead := Actual.Read(ABuf^, Size);
    CheckEquals(ERead, ARead, Format('Stream read different. Expected: %d. Actual: %d.', [ERead, ARead]) + Msg);
    CheckEqualsMem(EBuf, ABuf, Size, 'Stream data different.' + Msg);
  finally
    FreeMem(EBuf);
    FreeMem(ABuf);
  end;
end;

procedure TZAbstractTestCase.CheckEqualsDate(const Expected, Actual: TDateTime;
  Parts: TDateParts; const Msg: string);
const
  fmt = 'YYYY-MM-DD HH:NN:SS.ZZZ';
var
  EYear, EMonth, EDay, EHour, EMin, ESec, EMSec: Word;
  AYear, AMonth, ADay, AHour, AMin, ASec, AMSec: Word;
  s: string;
begin
  if Parts = [] then
    Parts := [dpYear..dpMSec];
  s := Msg + Format(' DateTime: Expected: %s, Actual: %s - ',
    [FormatDateTime(fmt, Expected), FormatDateTime(fmt, Actual)]);
  DecodeDate(Expected, EYear, EMonth, EDay);
  DecodeTime(Expected, EHour, EMin, ESec, EMSec);
  DecodeDate(Actual, AYear, AMonth, ADay);
  DecodeTime(Actual, AHour, AMin, ASec, AMSec);
  if dpYear in Parts then CheckEquals(EYear, AYear, s + '(DateTime.Year)');
  if dpMonth in Parts then CheckEquals(EMonth, AMonth, s + '(DateTime.Month)');
  if dpDay in Parts then CheckEquals(EDay, ADay, s + '(DateTime.Day)');
  if dpHour in Parts then CheckEquals(EHour, AHour, s + '(DateTime.Hour)');
  if dpMin in Parts then CheckEquals(EMin, AMin, s + '(DateTime.Min)');
  if dpSec in Parts then CheckEquals(ESec, ASec, s + '(DateTime.Sec)');
  if dpMSec in Parts then CheckEquals(EMSec, AMSec, s + '(DateTime.MSec)');
end;


{**
  Prints a debug message to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.Print(_Message: string);
begin
  if not SuppressTestOutput then
    System.Write(_Message);
end;

{**
  Prints a debug message ends with EOL to standard output.
  @param Message a message string to be printed.
}
procedure TZAbstractTestCase.PrintLn(_Message: string);
begin
  if not SuppressTestOutput then
    System.WriteLn(_Message);
end;

{**
  Gets a system specific number of ticks.
  @return the current number of system ticks.
}
function TZAbstractTestCase.GetTickCount: Cardinal;
begin
{$IFDEF FPC}
  Result := LCLIntf.GetTickCount;
{$ELSE}
  Result := Windows.GetTickCount;
{$ENDIF}
end;

end.

