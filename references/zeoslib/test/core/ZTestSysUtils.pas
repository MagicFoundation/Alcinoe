{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Utility Functions              }
{                                                         }
{         Originally written by Sergey Merkuriev          }
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

unit ZTestSysUtils;

interface

{$I ZCore.inc}

uses {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions, ZSysUtils, SysUtils, ZClasses, ZVariant,
  ZMatchPattern;

type

  {** Implements a test case for Utilities. }
  TZTestSysUtilsCase = class(TZCoreGenericTestCase)
  published
    procedure TestBufferToStr;
    procedure TestFirstDelimiter;
    procedure TestIsIpAddr;
    procedure TestSqlStrToFloatDef;
    procedure TestStrToBoolEx;
    procedure TestObjectComparison;
    procedure TestReplaceChar;
    procedure TestMatch;
  end;

implementation

{ TZTestSysUtilsCase }

{**
  Runs a test for BufferToStr function.
}
procedure TZTestSysUtilsCase.TestBufferToStr;
var
  Value: PChar;
begin
  Value := nil;
  CheckEquals('', BufferToStr(Value, 1000));
end;

{**
  Runs a test for first delimiter function.
}
procedure TZTestSysUtilsCase.TestFirstDelimiter;
var
  SourceStr: string;
  DelimiterStr: string;
begin
  { Position should exist }
  DelimiterStr := '098g';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(7, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 1');

  { Position should not exist }
  DelimiterStr := 'klmn';
  SourceStr := 'abcdefg1234567890';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 2');

  { Check with empty string }
  DelimiterStr := '';
  SourceStr := '';
  CheckEquals(0, FirstDelimiter(DelimiterStr, SourceStr), 'FirstDelimiter 3');
end;

{**
  Runs a test for IsIpAddr function.
}
procedure TZTestSysUtilsCase.TestIsIpAddr;
var
  IP: string;
begin
 // Correct IP
 IP := '10.0.5.15';
 Check(IsIpAddr(IP), 'ip is 10.0.5.15');

 // Correct IP
 IP := '150.150.14.33';
 Check(IsIpAddr(IP), 'ip is 150.150.14.33');

 // Incorrect IP
 IP := '1001.2220.775.1544';
 Check(not IsIpAddr(IP), 'ip is 1001.2220.775.1544');

 // Incorrect IP
 IP := '10.0.5.15:3306';
 Check(not IsIpAddr(IP), 'ip is 10.0.5.15:3306');

 // Incorrect IP
 IP := '999.999.999.999';
 Check(not IsIpAddr(IP), 'ip is 999.999.999.999');

 // Incorrect IP
 IP := 'localhost';
 Check(not IsIpAddr(IP), 'ip is 10.0.5.15');

 // Incorrect IP - clear string
 IP := '';
 Check(not IsIpAddr(IP), 'ip is ''''');
end;

{**
  Runs a test for object and interface comparison.
}
procedure TZTestSysUtilsCase.TestObjectComparison;
var
  Value1: TZAnyValue;
  Value2: IZAnyValue;
begin
  Value1 := TZAnyValue.CreateWithInteger(1);
  Value2 := TZAnyValue.CreateWithInteger(1);

  try
    Check(Value1 = Value1);
    Check(IZInterface(Value1) <> Value2);

    Check(Pointer(Value1) = Pointer(Value1));
    Check(IZInterface(Value1) = IZInterface(Value1));
    Check(Pointer(Value1) <> Pointer(Value2));
{
    Check((Value1 as IZInterface) = (Value1 as IZInterface));
    Check((Value1 as IZInterface) <> (Value2 as IZInterface));
}
  finally
    Value1.Free;
  end;
end;

{**
  Runs a test for ReplaceChar function.
}
procedure TZTestSysUtilsCase.TestReplaceChar;
const
  Source = '1234,7567,567';
  SourceChar = ',';
  Target = '1234.7567.567';
  TargetChar = '.';
begin
  CheckEquals(Target, ReplaceChar(SourceChar, TargetChar, Source));
end;

{**
  Runs a test for StrToFloatDef function.
}
procedure TZTestSysUtilsCase.TestSqlStrToFloatDef;
begin
  CheckEquals(11.11, SqlStrToFloatDef('12,75', 11.11));
  CheckEquals(12.75, SqlStrToFloatDef('12.75', 11.11));
  CheckEquals(0.1275, SqlStrToFloatDef('12.75e-2', 11.11));
  CheckEquals(11.11, SqlStrToFloatDef('12.75float', 11.11));
  CheckEquals(11.11, SqlStrToFloatDef('', 11.11));
  CheckEquals(11.11, SqlStrToFloatDef('111,125.33', 11.11));
end;

{**
  Runs a test for StrToBoolEx function.
}
procedure TZTestSysUtilsCase.TestStrToBoolEx;
begin
  CheckEquals(True, StrToBoolEx('YES'));
  CheckEquals(True, StrToBoolEx('Yes'));
  CheckEquals(True, StrToBoolEx('Y'));
  CheckEquals(True, StrToBoolEx('TRUE'));
  CheckEquals(True, StrToBoolEx('True'));
  CheckEquals(True, StrToBoolEx('T'));
  CheckEquals(False, StrToBoolEx('FALSE'));
  CheckEquals(False, StrToBoolEx('False'));
  CheckEquals(False, StrToBoolEx('F'));
  CheckEquals(False, StrToBoolEx('NO'));
  CheckEquals(False, StrToBoolEx('No'));
  CheckEquals(False, StrToBoolEx('N'));
end;

{**
  Runs a test for IsMatch function.
}
procedure TZTestSysUtilsCase.TestMatch;
begin
  CheckEquals(True, IsMatch('2*', '23'));
  CheckEquals(True, IsMatch('qwe*', 'qwerty'));
  CheckEquals(True, IsMatch('qwe*', 'qwe'));
  CheckEquals(False, IsMatch('qwe*', 'xyz'));
  CheckEquals(False, IsMatch('qwe*', 'xyzqweabc'));

  CheckEquals(True, IsMatch('*qwe', 'xyzqwe'));
  CheckEquals(True, IsMatch('*qwe', 'qwe'));
  CheckEquals(False, IsMatch('*qwe', 'xyz'));
  CheckEquals(False, IsMatch('*qwe', 'xyzqweabc'));

  CheckEquals(True, IsMatch('*qwe*', 'xyzqwe'));
  CheckEquals(True, IsMatch('*qwe*', 'qwe'));
  CheckEquals(False, IsMatch('*qwe*', 'xyz'));
  CheckEquals(True, IsMatch('*qwe*', 'xyzqweabc'));
end;

initialization
  RegisterTest('core',TZTestSysUtilsCase.Suite);
end.
