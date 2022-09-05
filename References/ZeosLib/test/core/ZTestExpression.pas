{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Test Case for Expression Classes             }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZTestExpression;

interface

{$I ZCore.inc}

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  {$IFDEF FPC}testregistry{$ELSE}TestFramework{$ENDIF}, ZTestDefinitions, SysUtils, Classes, ZExpression, ZVariables;

type

  {** Implements a test case for Utilities. }

  { TZTestExpressionCase }

  TZTestExpressionCase = class(TZCoreGenericTestCase)
  published
    procedure TestRegularExpressions;
    procedure TestArrays;
    procedure TestFunctionsConvert;
    procedure TestFunctionsDateTime;
    procedure TestFunctionsMath;
    procedure TestFunctionsOther;
    procedure TestFunctionsStrings;
    procedure TestVariables;
    procedure TestPerformance;
  end;

implementation

uses Math, DateUtils, ZVariant, ZCompatibility;

const
  RUN_COUNT = 100000;

{ TZTestExpressionCase }

{**
  Runs a test for arrays processing.
}
procedure TZTestExpressionCase.TestArrays;
var
  Expression: IZExpression;
begin
  Expression := TZExpression.Create;
(*
  Variant[] values1 = new Variant[10];
  Integer[] values2 = new Integer[10];
  for (int i = 0; i < 10; i++) {
    values1[i] = expression.createVariant();
    values1[i].setInt(i + 1);
    values2[i] = new Integer(i + 1);
  }
  Variant n = expression.createVariant();
  Variant a = expression.createVariant();
  expression.setExpression("IN(N, A)");

  n.setInt(3);
  expression.getDefaultVariables().set("N", n);
  a.setArray(values1);
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  n.setInt(-100);
  expression.getDefaultVariables().set("N", n);
  assertTrue(!expression.evaluate().getBoolean());

  n.setInt(3);
  expression.getDefaultVariables().set("N", n);
  a.setObjectArray(values2);
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  n.setInt(-100);
  expression.getDefaultVariables().set("N", n);
  assertTrue(!expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(A, 3)");
  expression.getDefaultVariables().set("A", a);
  assertTrue(expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(A, -100)");
  expression.getDefaultVariables().set("A", a);
  assertTrue(!expression.evaluate().getBoolean());

  expression.setExpression("CONTAINS(0.0, '0')");
  assertTrue(expression.evaluate().getBoolean());
*)
end;

{**
  Runs a test for build-in conversion functions.
}
procedure TZTestExpressionCase.TestFunctionsConvert;
var
  Expression: IZExpression;
  v1, v2, v3: TZVariant;
  i         : Int64;
begin
  Expression := TZExpression.Create;
  with Expression do
  begin
    Expression := 'VAL(''  -011'')';
    CheckEquals(-11, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
    Expression := 'VAL(''  -023.25'')';
    CheckEquals(-23.25, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

    Expression := 'DTOS(ENCODEDATE(2009,07,13))';
    CheckEquals('20090713', SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CTOD(''' + DateToStr(Date) + ''')';
    CheckEquals(Date, DefVarManager.GetAsDateTime(Evaluate),Expression+' failed, ');
  end;
end;

{**
  Runs a test for build-in date and time functions.
}
procedure TZTestExpressionCase.TestFunctionsDateTime;
var
  Expression: IZExpression;
  v1, v2, v3: TZVariant;
  i         : Int64;
begin
  Expression := TZExpression.Create;
  with Expression do
  begin
    Expression := 'DATE()';
    CheckEquals(FormatDateTime('yyyy-mm-dd hh":"nn":"ss',Date), SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'TIME()';
    CheckEquals(FormatDateTime('yyyy-mm-dd hh":"nn":"ss',Time), SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'NOW()';
    CheckEquals(FormatDateTime('yyyy-mm-dd hh":"nn":"ss',Now), SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'ENCODEDATE(2009,07,13)';
    v1 := Evaluate;
    CheckEquals('2009-07-13 00:00:00', SoftVarManager.GetAsString(V1),Expression+' failed, ');
    Expression := 'ENCODETIME(13,7,3)';
    v2 := Evaluate;
    CheckEquals('1899-12-30 13:07:03', SoftVarManager.GetAsString(V2),Expression+' failed, ');

    DefaultVariables.Add('V1',V1);
    DefaultVariables.Add('V2',V2);

    Expression := 'COMPOSEDATETIME(V1,V2)';
    v3 := Evaluate;
    DefaultVariables.Add('V3',V3);
    CheckEquals('2009-07-13 13:07:03', SoftVarManager.GetAsString(V3),Expression+' failed, ');

    Expression := 'INCDATE(V2,110,-5,1,-24)';
    CheckEquals('2009-07-13 13:07:03', SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'INCTIME(V1,13,7,3)';
    CheckEquals('2009-07-13 13:07:03', SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'DATEOF(V3)';
    CheckEquals('2009-07-13 00:00:00', SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'TIMEOF(V3)';
    CheckEquals('1899-12-30 13:07:03', SoftVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'ISLEAPYEAR(2008)';
    CheckEquals(True,DefVarManager.GetAsBoolean(Evaluate),Expression+' failed, ');
    Expression := 'ISLEAPYEAR(2009)';
    CheckEquals(False,DefVarManager.GetAsBoolean(Evaluate),Expression+' failed, ');

    Expression := 'YEAROF(V3)';
    CheckEquals(2009, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MONTHOF(V3)';
    CheckEquals(7, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'DAYOF(V3)';
    CheckEquals(13, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'HOUROF(V3)';
    CheckEquals(13, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MINUTEOF(V3)';
    CheckEquals(7, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDOF(V3)';
    CheckEquals(3, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOF(V3)';
    CheckEquals(0, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'WEEKOFTHEYEAR(V3)';
    CheckEquals(29, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'DAYOFTHEYEAR(V3)';
    i          := 31+28+31+30+31+30+13;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'HOUROFTHEYEAR(V3)';
    i          := (i-1)*24+13;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MINUTEOFTHEYEAR(V3)';
    i          := i*60+7;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDOFTHEYEAR(V3)';
    i          := i*60+3;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOFTHEYEAR(V3)';
    i          := i*1000;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'WEEKOFTHEMONTH(V3)';
    CheckEquals(3, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'HOUROFTHEMONTH(V3)';
    i          := 12*24+13;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MINUTEOFTHEMONTH(V3)';
    i          := i*60+7;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDOFTHEMONTH(V3)';
    i          := i*60+3;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOFTHEMONTH(V3)';
    i          := i*1000;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'DAYOFTHEWEEK(V3)';
    i          := 1;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'HOUROFTHEWEEK(V3)';
    i          := (i-1)*24+13;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MINUTEOFTHEWEEK(V3)';
    i          := i*60+7;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDOFTHEWEEK(V3)';
    i          := i*60+3;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOFTHEWEEK(V3)';
    i          := i*1000;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'MINUTEOFTHEDAY(V3)';
    i          := 13*60+7;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDOFTHEDAY(V3)';
    i          := i*60+3;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOFTHEDAY(V3)';
    i          := i*1000;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'SECONDOFTHEHOUR(V3)';
    i          := 7*60+3;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDOFTHEHOUR(V3)';
    i          := i*1000;
    CheckEquals(i, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'MILLISECONDOFTHEMINUTE(V3)';
    CheckEquals(3000, SoftVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    DefaultVariables.NamedValues['V2'] := ZVariant.EncodeDateTime(DateUtils.EncodeDateTime(2000,2,4,22,31,43,20));
//  V2 = 2000-02-04 22:21:43.20
//  V3 = 2009-07-13 13:07:03.00
    Expression := 'YEARSBETWEEN(V2,V3)';
    i := 9;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MONTHSBETWEEN(V2,V3)';
    i := i*12+5;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'WEEKSBETWEEN(V2,V3)';
    i := 492;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'DAYSBETWEEN(V2,V3)';
    i := i*7+2;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'HOURSBETWEEN(V2,V3)';
    i := i*24+14;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MINUTESBETWEEN(V2,V3)';
    i := i*60+35;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'SECONDSBETWEEN(V2,V3)';
    i := i*60+19;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'MILLISECONDSBETWEEN(V2,V3)';
    i := (i+1)*1000-20;
    CheckEquals(i, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
  end;
end;

{**
  Runs a test for build-in math functions.
}
procedure TZTestExpressionCase.TestFunctionsMath;
var
  Expression: IZExpression;
  v1, v2, v3: TZVariant;
  i         : Int64;
begin
  Expression := TZExpression.Create;
  with Expression do
  begin
//    Expression := 'E()';
//    CheckEquals(2.7182818284590451, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'PI()';
//    CheckEquals(3.1415926535897931, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

//    Expression := 'EXP(2)';
//    CheckEquals(7.3890560989306504, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'LOG(0.5)';
//    CheckEquals(-0.69314718055994529, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'LOG10(0.9)';
//    CheckEquals(-0.045757490560675122, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

//    Expression := 'COS(0.5)';
//    CheckEquals(0.87758256189037276, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'SIN(0.5)';
//    CheckEquals(0.47942553860420301, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'TAN(0.5)';
//    CheckEquals(0.54630248984379048, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'COT(0.5)';
//    CheckEquals(1.830487721712452, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

//    Expression := 'ACOS(0.5)';
//    CheckEquals(1.0471975511965979, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'ASIN(0.5)';
//    CheckEquals(0.52359877559829893, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
//    Expression := 'ATAN(0.5)';
//    CheckEquals(0.46364760900080609, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

    Expression := 'CEIL(0.4)';
    CheckEquals(1, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'FLOOR(-0.5)';
    CheckEquals(-1, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'ROUND(0.4)';
    CheckEquals(0, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'ROUND(0.6)';
    CheckEquals(1, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'TRUNC(0.6)';
    CheckEquals(0, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'INT(1.5)';
    CheckEquals(1.0, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
    Expression := 'FRAC(1.5)';
    CheckEquals(0.5, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

    Expression := 'SQR(16)';
    CheckEquals(4, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
    Expression := 'SQRT(25)';
    CheckEquals(5, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
  end;
end;

{**
  Runs a test for other build-in functions.
}
procedure TZTestExpressionCase.TestFunctionsOther;
var
  Expression: IZExpression;
  v1, v2, v3: TZVariant;
  i         : Int64;
begin
  Expression := TZExpression.Create;
  with Expression do
  begin
    Expression := 'MIN(2,5,1,10,8)';
    CheckEquals(1, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'MAX(2.1,5.3,1.0,10.4,8.9)';
    CheckEquals(10.4, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');

    Expression := 'Sum(''A'',''B'',3)';
    CheckEquals('AB3', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'Sum(''A'',''B'',''3'') + (28 + 2)';
    CheckEquals('AB330', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'Sum(''A'',''B'',''3'') + (Max(10, -33, 28) + Min(2, 100, 5))';
    CheckEquals('AB330', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'IIF(TRUE,20,33)';
    CheckEquals(20, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'IIF(FALSE,20,33)';
    CheckEquals(33, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'CASEF(0,''V0'',33,22.5)';
    CheckEquals('V0', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CASEF(1,''V0'',33,22.5)';
    CheckEquals(33, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'CASEF(2,''V0'',33,22.5)';
    CheckEquals(22.5, DefVarManager.GetAsFloat(Evaluate),Expression+' failed, ');
  end;
end;

procedure TZTestExpressionCase.TestFunctionsStrings;
var
  Expression: IZExpression;
  v1, v2, v3: TZVariant;
  i         : Int64;
begin
  Expression := TZExpression.Create;
  with Expression do
  begin
    Expression := 'CONCAT(''ABC'',''DEF'',''geh'')';
    CheckEquals('ABCDEFgeh', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CONCAT(''ABC'',123,45.67)';
    CheckEquals('ABC12345.67', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'SUBSTR(''ABCDEFGH'',3,4)';
    CheckEquals('CDEF', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'LEFT(''ABCDEFGH'',3)';
    CheckEquals('ABC', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'RIGHT(''ABCDEFGH'',3)';
    CheckEquals('FGH', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'STRPOS(''CDE'',''ABCDEFGH'')';
    CheckEquals(3, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LENGTH(''ABCDEFGH'')';
    CheckEquals(8, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LENGTH('''')';
    CheckEquals(0, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');

    Expression := 'UPPER(''abcdefgh'')';
    CheckEquals('ABCDEFGH', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'LOWER(''ABCDEFGH'')';
    CheckEquals('abcdefgh', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CAPITALIZE(''abc def,gh'')';
    CheckEquals('Abc Def,Gh', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CAPITALIZE(''ABC;DEF [GH]'')';
    CheckEquals('Abc;Def [Gh]', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CAPITALIZE(''ABC;DEF [GH],ijk'','' ,'')';
    CheckEquals('Abc;def [gh],Ijk', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'CAP(''ABC;DEF [GH],ijk'','' ,'')';
    CheckEquals('Abc;def [gh],Ijk', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'TRIM('' more spaces  '')';
    CheckEquals('more spaces', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'LTRIM('' more spaces  '')';
    CheckEquals('more spaces  ', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'RTRIM('' more spaces  '')';
    CheckEquals(' more spaces', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'SOUNDEX(''Smith'')';
    CheckEquals('S530', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    Expression := 'SOUNDEX(''Smith'',6)';
    CheckEquals('S53000', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');
    //Expression := 'SOUNDEX(''Schmittchen'',6)';
    //CheckEquals('S25325', DefVarManager.GetAsString(Evaluate),Expression+' failed, ');

    Expression := 'LEVENSHTEINDISTANCE(''Smith'',''Smith'')';
    CheckEquals(0, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LEVENSHTEINDISTANCE(''smith'',''SMITH'',FALSE)';
    CheckEquals(5, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LEVDIST(''Smith'',''Shmid'')';
    CheckEquals(3, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LEVDIST(''Smither'',''Shmider'')';
    CheckEquals(3, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LEVDIST(''Billy the Kid'',''The big brown fox'')';
    CheckEquals(14, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
    Expression := 'LEVDIST(''Billy the kid'',''Billy and the young kid'')';
    CheckEquals(10, DefVarManager.GetAsInteger(Evaluate),Expression+' failed, ');
  end;
end;

{**
  Runs a test for regular (simple) expressions.
}
procedure TZTestExpressionCase.TestRegularExpressions;
var
  Expression: IZExpression;
begin
  Expression := TZExpression.Create;

  Expression.Expression := '2+2';
  CheckEquals(4, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(4, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '3-1';
  CheckEquals(2, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(2, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '2+2*2+2';
  CheckEquals(8, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(8, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '(2+2)*(2+2)';
  CheckEquals(16, DefVarManager.GetAsInteger(Expression.Evaluate));
  CheckEquals(16, DefVarManager.GetAsInteger(Expression.Evaluate));

  Expression.Expression := '''ABBA'' LIKE ''A*B?''';
  CheckEquals(True, DefVarManager.GetAsBoolean(Expression.Evaluate));

  Expression.Expression := '''ABC'' + 123';
  CheckEquals('ABC123', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := '''ABC'' + (100 + 23)';
  CheckEquals('ABC123', DefVarManager.GetAsString(Expression.Evaluate));

  Expression.Expression := '''Result='' + (23.5 + 11 / 23 * 2^2 - 3) + True';
  CheckEquals('Result=20.5TRUE', DefVarManager.GetAsString(Expression.Evaluate));
end;

{**
  Runs a test for variables.
}
procedure TZTestExpressionCase.TestVariables;
var
  Expression: TZExpression;
  Variables: IZVariablesList;
begin
  Expression := TZExpression.Create;

  with Expression do
  begin
    DefaultVariables.Add('a', EncodeInteger(100));
    DefaultVariables.Add('B C', EncodeInteger(23));
    Expression := 'A + "B C"';
    CheckEquals(123, DefVarManager.GetAsInteger(Evaluate), Expression+' failed, ');

    Expression := '"B C" LIKE ''2*''';
    Check(DefVarManager.GetAsBoolean(Evaluate), Expression+' failed, ');

    Expression := '"B C" NOT LIKE ''2*''';
    CheckEquals(False, DefVarManager.GetAsBoolean(Evaluate), Expression+' failed, ');

    DefaultVariables.Add('A_B', EncodeInteger(123));
    Expression := 'A_B + 321';
    CheckEquals(444, DefVarManager.GetAsInteger(Evaluate), Expression+' failed, ');

    Clear;
    AutoVariables := True;
    Expression := 'A = 123';
    CheckEquals(0, DefaultVariables.FindByName('a'), Expression+' failed, ');
    DefaultVariables.Values[0] := EncodeInteger(123);
    Check(DefVarManager.GetAsBoolean(Evaluate), Expression+' failed, ');

    try
      AutoVariables := False;
      Clear;
      Expression := 'B + 321';
      Evaluate;
      Fail('Wrong behaviour with unknown variable.');
    except
    end;

    Expression := 'A + B';
    Variables := TZVariablesList.Create;
    Variables.Add('a', EncodeInteger(123));
    Variables.Add('B', EncodeString('321'));
    CheckEquals(444, DefVarManager.GetAsInteger(Evaluate2(Variables)), Expression+' failed, ');

    Expression := 'A=1 OR A=2';
    Variables := TZVariablesList.Create;
    Variables.Add('a', EncodeInteger(3));
    CheckEquals(False, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');

    Variables := TZVariablesList.Create;
    Variables.Add('a', EncodeNull);

    Expression := 'A IS NULL';
    CheckEquals(True, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');

    Expression := 'A IS NOT NULL';
    CheckEquals(False, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');

    Expression := 'A Is Null Or a Not Like ''AB*''';
    CheckEquals(True, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');

    Expression := 'A Is Not Null or a Like ''C?''';
    CheckEquals(False, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');

    Expression := 'Upper(''Abc'')';
    CheckEquals('ABC', DefVarManager.GetAsString(Evaluate), Expression+' failed, ');

    Expression := 'Lower(''Abc'')';
    CheckEquals('abc', DefVarManager.GetAsString(Evaluate), Expression+' failed, ');

    Expression := 'Concat(''Ab'', ''cd'', ''efG'')';
    CheckEquals('AbcdefG', DefVarManager.GetAsString(Evaluate), Expression+' failed, ');

    Expression := 'SubStr(''AbcdefG'', 3, 3)';
    CheckEquals('cde', DefVarManager.GetAsString(Evaluate), Expression+' failed, ');

    Expression := 'StrPos(''cde'',''AbcdefG'')';
    CheckEquals(3, DefVarManager.GetAsInteger(Evaluate), Expression+' failed, ');

    Variables := TZVariablesList.Create;
    Variables.Add('a', EncodeString(''));
    Expression := 'A = ''''';
    CheckEquals(True, DefVarManager.GetAsBoolean(Evaluate2(Variables)), Expression+' failed, ');
  end;
  Expression.Destroy;
end;

{**
  Run tests for expression performance.
}
procedure TZTestExpressionCase.TestPerformance;
var
  I: Integer;
  StartTicks: Cardinal;
  Expression: IZExpression;
  Stack: TZExecutionStack;
  Temp: TZVariant;
  OldDecimalSeparator: Char;

  procedure EvaluateExpression(const s : string);
  var
    i : integer;
    Diff : double;
  begin
    With Expression do
    begin
      Expression := s;
      try
        StartTicks := GetTickCount;
        for I := 0 to RUN_COUNT - 1 do
          Evaluate4(DefaultVariables, DefaultFunctions, Stack);
        Diff := (GetTickCount - StartTicks) / 1000;
        System.WriteLn(Format('Evaluating '+Expression+' %d times, Time: %0.3f [s]',
          [RUN_COUNT,Diff]));
      finally
      end;
      Stack.Clear;
    end;
  end;

begin
  { Tests speed of repeatable calculations with variables. }
  Expression := TZExpression.Create;
  Stack := TZExecutionStack.Create;
  with Expression do
  begin
    Expression := 'A-100>100';
  try
    StartTicks := GetTickCount;
    for I := 0 to RUN_COUNT - 1 do
    begin
        DefaultVariables.Values[0] := EncodeInteger(I);
        Evaluate4(DefaultVariables, DefaultFunctions, Stack);
    end;
    PrintLn(Format('Evaluating expression, Time: %d',
      [GetTickCount - StartTicks]));
  finally
  end;
    Stack.Clear;

  { Tests comparison of float values. }
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := ',';
    Expression := 'A=123.4567890123456';
  try
      DefaultVariables.Values[0] := EncodeFloat(123.4567890123456);
      Temp := Evaluate4(DefaultVariables, DefaultFunctions, Stack);
    CheckEquals(Ord(vtBoolean), Ord(Temp.VType));
    CheckEquals(True, Temp.VBoolean);
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
    Stack.Clear;

  { Tests alternative comparison of float values. }
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := ',';
    Expression := 'Abs(A - 123.4567890123456)<0.001';
  try
      DefaultVariables.Values[0] := EncodeFloat(123.4567890123456);
      Temp := Evaluate4(DefaultVariables, DefaultFunctions, Stack);
    CheckEquals(Ord(vtBoolean), Ord(Temp.VType));
    CheckEquals(True, Temp.VBoolean);
  finally
    DecimalSeparator := OldDecimalSeparator;
  end;
    Stack.Clear;

    DefaultVariables.Clear;
    DefaultVariables.Add('A',EncodeDateTime(EncodeDate(1999,10,3)+EncodeTime(22,4,35,88)));
    DefaultVariables.Add('B',EncodeDateTime(Now));
    DefaultVariables.Add('I',EncodeInteger(-1));
    DefaultVariables.Add('F',EncodeFloat(-3.4532));
    EvaluateExpression('-I');
    EvaluateExpression('-F');
    EvaluateExpression('ABS(I)');
    EvaluateExpression('ABS(-1)');
    EvaluateExpression('ABS(F)');
    EvaluateExpression('ABS(-3.4532)');
    EvaluateExpression('ABS(F)-ABS(I)');
    EvaluateExpression('ABS(-3.4532)-ABS(-1)');
    EvaluateExpression('ABS(ABS(F)-ABS(I))');
    EvaluateExpression('ABS(ABS(-3.4532)-ABS(-1))');
    EvaluateExpression('MILLISECONDSBETWEEN(A,b)');
    EvaluateExpression('MILLISECONDSBETWEEN(A,b)-SECONDSBETWEEN(B,A)-MINUTESBETWEEN(A,B)');
    Stack.Clear;
  end;
  Stack.Free;
end;

initialization
  RegisterTest('core',TZTestExpressionCase.Suite);
end.
