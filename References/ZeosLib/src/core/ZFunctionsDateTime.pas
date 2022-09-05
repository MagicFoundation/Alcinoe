{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Variables classes and interfaces            }
{                                                         }
{           Originally written by Sergey Seroukhov        }
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

unit ZFunctionsDateTime;

interface

{$I ZCore.inc}

uses
  SysUtils, ZClasses, ZFunctions, ZExpression, ZVariant;

{** Date & time functions}

type
  {** Implements a DATE function. }
  TZDateFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TIME function. }
  TZTimeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a NOW function. }
  TZNowFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ENCODEDATE function. }
  TZEncodeDateFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ENCODETIME function. }
  TZEncodeTimeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a COMPOSEDATETIME function. }

  { TZComposeDateTimeFunction }

  TZComposeDateTimeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a INCDATE function. }
  TZIncDateFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a INCTIME function. }
  TZIncTimeFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a ISLEAPYEAR function. }
  TZIsLeapYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{-------------------- Extracting functions ----------------------------}
  {** Implements a DATEOF function. }
  TZDateOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a TIMEOF function. }
  TZTimeOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a YEAROF function. }
  TZYearOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MONTHOF function. }
  TZMonthOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a DAYOF function. }
  TZDayOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a HOUROF function. }
  TZHourOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MINUTEOF function. }
  TZMinuteOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDOF function. }
  TZSecondOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOF function. }
  TZMilliSecondOfFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{-------------------- *OFTHEYEAR Extracting functions ----------------------------}
  {** Implements a WEEKOFTHEYEAR function. }
  TZWeekOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a DAYOFTHEYEAR function. }
  TZDayOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a HOUROFTHEYEAR function. }
  TZHourOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MINUTEOFTHEYEAR function. }
  TZMinuteOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDOFTHEYEAR function. }
  TZSecondOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOFTHEYEAR function. }
  TZMilliSecondOfTheYearFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{-------------------- *OFTHEMONTH Extracting functions ----------------------------}
  {** Implements a WEEKOFTHEMONTH function. }
  TZWeekOfTheMonthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a HOUROFTHEMONTH function. }
  TZHourOfTheMonthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MINUTEOFTHEMONTH function. }
  TZMinuteOfTheMonthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDOFTHEMONTH function. }
  TZSecondOfTheMonthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOFTHEMONTH function. }
  TZMilliSecondOfTheMonthFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{-------------------- *OFTHEWEEK Extracting functions ----------------------------}
  {** Implements a DAYOfTheWeek function. }
  TZDayOfTheWeekFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a HOUROfTheWeek function. }
  TZHourOfTheWeekFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MINUTEOfTheWeek function. }
  TZMinuteOfTheWeekFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDOfTheWeek function. }
  TZSecondOfTheWeekFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOfTheWeek function. }
  TZMilliSecondOfTheWeekFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;


{---------------- *OFTHEDAY Extracting functions --------------------}
  {** Implements a MINUTEOFTHEDAY function. }
  TZMinuteOfTheDayFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDOFTHEDAY function. }
  TZSecondOfTheDayFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOFTHEDAY function. }
  TZMilliSecondOfTheDayFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{---------------- *OfTheHour Extracting functions --------------------}
  {** Implements a SECONDOFTHEHOUR function. }
  TZSecondOfTheHourFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDOFTHEHOUR function. }
  TZMilliSecondOfTheHourFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{---------------- *OFTHEMINUTE Extracting functions --------------------}
  {** Implements a MILLISECONDOfTheHour function. }
  TZMilliSecondOfTheMinuteFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

{---------------- *BETWEEN functions --------------------}
  {** Implements a YEARSBETWEEN function. }
  TZYearsBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MONTHSBETWEEN function. }
  TZMonthsBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a WEEKSBETWEEN function. }
  TZWeeksBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a DAYSBETWEEN function. }
  TZDaysBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a HOURSBETWEEN function. }
  TZHoursBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MINUTESBETWEEN function. }
  TZMinutesBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a SECONDSBETWEEN function. }
  TZSecondsBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

  {** Implements a MILLISECONDSBETWEEN function. }
  TZMillisecondsBetweenFunction = class (TZAbstractFunction)
  public
    function Execute(Stack: TZExecutionStack;
      VariantManager: IZVariantManager): TZVariant; override;
  end;

procedure AddDateTimeFunctions(Functions : TZFunctionsList);

implementation

uses
  ZMessages, DateUtils;

Function IncDate(const aDate : TDateTime; const aYear, aMonth, aWeek, aDay : LongInt) : TDateTime;
begin
  Result := aDate;
  if aYear  <> 0 then Result := IncYear(Result, aYear);
  if aMonth <> 0 then Result := IncMonth(Result, aMonth);
  if aWeek  <> 0 then Result := IncWeek(Result, aWeek);
  if aDay   <> 0 then Result := IncDay(Result, aDay);
end;

Function IncTime(const aDate : TDateTime; const aHour, aMinute, aSecond, aMillisec : LongInt) : TDateTime;
begin
  Result := IncHour(IncMinute(IncSecond(IncMillisecond(aDate, aMilliSec),aSecond),aMinute),aHour);
end;

{ TZDateFunction }

function TZDateFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Date);
end;

{ TZTimeFunction }

{**
  Executes this function.
  @param Stack the stack object.
  @param VariantManager a reference to variant processor object.
  @returns a function value.
}
function TZTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Time);
end;

{ TZNowFunction }

function TZNowFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 0);
  VariantManager.SetAsDateTime(Result, Now);
end;

{ TZEncodeDateFunction }

function TZEncodeDateFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Year , Month, Day : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  Year  := 0;
  Month := 1;
  Day   := 1;

  if ParamsCount > 0 then
    Year := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount));
  if ParamsCount > 1 then
    Month := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  if ParamsCount > 2 then
    Day := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));

  VariantManager.SetAsDateTime(Result, EncodeDate(Year,Month,Day));
end;

{ TZEncodeDateFunction }

function TZEncodeTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Hour , Minute, Second, MilliSecond : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  Hour        := 0;
  Minute      := 0;
  Second      := 0;
  MilliSecond := 0;

  if ParamsCount > 0 then
    Hour := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount));
  if ParamsCount > 1 then
    Minute := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  if ParamsCount > 2 then
    Second := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
    MilliSecond := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));

  VariantManager.SetAsDateTime(Result, EncodeTime(Hour,Minute,Second,MilliSecond));
end;

{ TZComposeDateTimeFunction }

function TZComposeDateTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsDateTime(Result, VariantManager.GetAsDateTime(Stack.GetParameter(2))+
    VariantManager.GetAsDateTime(Stack.GetParameter(1)));
end;

{ TZIncDateFunction }

function TZIncDateFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Date : TDateTime;
  Year , Month, Week, Day : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  if (ParamsCount <= 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Date  := VariantManager.GetAsDateTime(Stack.GetParameter(ParamsCount));
  Year  := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));
  Month := 0;
  Week  := 0;
  Day   := 0;
  if ParamsCount > 2 then
     Month := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
     Week := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));
  if ParamsCount > 4 then
     Day := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-4));

  VariantManager.SetAsDateTime(Result, IncDate(Date,Year,Month,Week,Day));
end;

{ TZIncTimeFunction }

function TZIncTimeFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
var
  ParamsCount: Integer;
  Date : TDateTime;
  Hour , Minute, Second, MilliSecond : LongInt;
begin
  ParamsCount := VariantManager.GetAsInteger(Stack.GetParameter(0));

  if (ParamsCount <= 2) then
    raise TZExpressionError.Create(SExpectedMoreParams);

  Date := VariantManager.GetAsDateTime(Stack.GetParameter(ParamsCount));
  Hour := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-1));

  Minute      := 0;
  Second      := 0;
  MilliSecond := 0;

  if ParamsCount > 2 then
    Minute := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-2));
  if ParamsCount > 3 then
    Second := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-3));
  if ParamsCount > 4 then
    MilliSecond := VariantManager.GetAsInteger(Stack.GetParameter(ParamsCount-4));

  VariantManager.SetAsDateTime(Result, IncTime(Date, Hour,Minute,Second,MilliSecond));
end;

{ TZIsLeapYearFunction }

function TZIsLeapYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsBoolean(Result, IsLeapYear(
    VariantManager.GetAsInteger(Stack.GetParameter(1))));
end;

{ TZDateOfFunction }

function TZDateOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsDateTime(Result, DateOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZTimeOfFunction }

function TZTimeOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsDateTime(Result, TimeOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZYearOfFunction }

function TZYearOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, YearOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMonthOfFunction }

function TZMonthOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MonthOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfFunction }

function TZDayOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfFunction }

function TZHourOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfFunction }

function TZMinuteOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfFunction }

function TZSecondOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfFunction }

function TZMilliSecondOfFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOf(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeekOfTheYearFunction }

function TZWeekOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, WeekOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfTheYearFunction }

function TZDayOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheYearFunction }

function TZHourOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheYearFunction }

function TZMinuteOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheYearFunction }

function TZSecondOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheYearFunction }

function TZMilliSecondOfTheYearFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheYear(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeekOfTheMonthFunction }

function TZWeekOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, WeekOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheMonthFunction }

function TZHourOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheMonthFunction }

function TZMinuteOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheMonthFunction }

function TZSecondOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheMonthFunction }

function TZMilliSecondOfTheMonthFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheMonth(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDayOfTheWeekFunction }

function TZDayOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, DayOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHourOfTheWeekFunction }

function TZHourOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, HourOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheWeekFunction }

function TZMinuteOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheWeekFunction }

function TZSecondOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheWeekFunction }

function TZMilliSecondOfTheWeekFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheWeek(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinuteOfTheDayFunction }

function TZMinuteOfTheDayFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MinuteOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheDayFunction }

function TZSecondOfTheDayFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheDayFunction }

function TZMilliSecondOfTheDayFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheDay(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondOfTheHourFunction }

function TZSecondOfTheHourFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, SecondOfTheHour(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheHourFunction }

function TZMilliSecondOfTheHourFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheHour(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMilliSecondOfTheMinuteFunction }

function TZMilliSecondOfTheMinuteFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 1);
  VariantManager.SetAsInteger(Result, MilliSecondOfTheMinute(
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZYearsBetweenFunction }

function TZYearsBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, YearsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMonthsBetweenFunction }

function TZMonthsBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MonthsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZWeeksBetweenFunction }

function TZWeeksBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, WeeksBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZDaysBetweenFunction }

function TZDaysBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, DaysBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHoursBetweenFunction }

function TZHoursBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, HoursBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZMinutesBetweenFunction }

function TZMinutesBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MinutesBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZSecondsBetweenFunction }

function TZSecondsBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, SecondsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

{ TZHoursBetweenFunction }

function TZMillisecondsBetweenFunction.Execute(Stack: TZExecutionStack;
  VariantManager: IZVariantManager): TZVariant;
begin
  CheckParamsCount(Stack, 2);
  VariantManager.SetAsInteger(Result, MillisecondsBetween(
    VariantManager.GetAsDateTime(Stack.GetParameter(2)),
    VariantManager.GetAsDateTime(Stack.GetParameter(1))));
end;

procedure AddDateTimeFunctions(Functions : TZFunctionsList);
begin
  Functions.Add(TZDateFunction.Create('DATE'));
  Functions.Add(TZTimeFunction.Create('TIME'));
  Functions.Add(TZNowFunction.Create('NOW'));

// First the Aliases

  Functions.Add(TZEncodeDateFunction.Create('ENCD'));
  Functions.Add(TZEncodeTimeFunction.Create('ENCT'));
//  Functions.Add(TZComposeDateTimeFunction.Create('COMPDT'));

  Functions.Add(TZIncDateFunction.Create('INCD'));
  Functions.Add(TZIncTimeFunction.Create('INCT'));

  Functions.Add(TZIsLeapYearFunction.Create('LEAPY'));

//  Functions.Add(TZDateOfFunction.Create('DATEOF'));
//  Functions.Add(TZTimeOfFunction.Create('TIMEOF'));

//  Functions.Add(TZYearOfFunction.Create('YEAROF'));
//  Functions.Add(TZMonthOfFunction.Create('MONTHOF'));
//  Functions.Add(TZDayOfFunction.Create('DAYOF'));
//  Functions.Add(TZHourOfFunction.Create('HOUROF'));

  Functions.Add(TZMinuteOfFunction.Create('MINOF'));
  Functions.Add(TZSecondOfFunction.Create('SECOF'));
  Functions.Add(TZMilliSecondOfFunction.Create('MSECOF'));

  Functions.Add(TZWeekOfTheYearFunction.Create('WofY'));
  Functions.Add(TZDayOfTheYearFunction.Create('DofY'));
  Functions.Add(TZHourOfTheYearFunction.Create('HofY'));
  Functions.Add(TZMinuteOfTheYearFunction.Create('MINofY'));
  Functions.Add(TZSecondOfTheYearFunction.Create('SECofY'));
  Functions.Add(TZMilliSecondOfTheYearFunction.Create('MSECofY'));

  Functions.Add(TZWeekOfTheMonthFunction.Create('WofM'));
  Functions.Add(TZHourOfTheMonthFunction.Create('HofM'));
  Functions.Add(TZMinuteOfTheMonthFunction.Create('MINofM'));
  Functions.Add(TZSecondOfTheMonthFunction.Create('SECofM'));
  Functions.Add(TZMilliSecondOfTheMonthFunction.Create('MSECofM'));

  Functions.Add(TZDayOfTheWeekFunction.Create('DofW'));
  Functions.Add(TZHourOfTheWeekFunction.Create('HofW'));
  Functions.Add(TZMinuteOfTheWeekFunction.Create('MINofW'));
  Functions.Add(TZSecondOfTheWeekFunction.Create('SECofW'));
  Functions.Add(TZMilliSecondOfTheWeekFunction.Create('MSECofW'));

  Functions.Add(TZMinuteOfTheDayFunction.Create('MINofD'));
  Functions.Add(TZSecondOfTheDayFunction.Create('SECofD'));
  Functions.Add(TZMilliSecondOfTheDayFunction.Create('MSECofD'));

  Functions.Add(TZSecondOfTheHourFunction.Create('SECofH'));
  Functions.Add(TZMilliSecondOfTheHourFunction.Create('MSECofH'));

  Functions.Add(TZMilliSecondOfTheMinuteFunction.Create('MSECofMIN'));

  Functions.Add(TZYearsBetweenFunction.Create('YBTW'));
  Functions.Add(TZMonthsBetweenFunction.Create('MBTW'));
  Functions.Add(TZWeeksBetweenFunction.Create('WBTW'));
  Functions.Add(TZDaysBetweenFunction.Create('DBTW'));
  Functions.Add(TZHoursBetweenFunction.Create('HBTW'));
  Functions.Add(TZMinutesBetweenFunction.Create('MINBTW'));
  Functions.Add(TZSecondsBetweenFunction.Create('SECBTW'));
  Functions.Add(TZMilliSecondsBetweenFunction.Create('MSECBTW'));

// End of Aliases

  Functions.Add(TZEncodeDateFunction.Create('ENCODEDATE'));
  Functions.Add(TZEncodeTimeFunction.Create('ENCODETIME'));
  Functions.Add(TZComposeDateTimeFunction.Create('COMPOSEDATETIME'));

  Functions.Add(TZIncDateFunction.Create('INCDATE'));
  Functions.Add(TZIncTimeFunction.Create('INCTIME'));

  Functions.Add(TZIsLeapYearFunction.Create('ISLEAPYEAR'));

  Functions.Add(TZDateOfFunction.Create('DATEOF'));
  Functions.Add(TZTimeOfFunction.Create('TIMEOF'));

  Functions.Add(TZYearOfFunction.Create('YEAROF'));
  Functions.Add(TZMonthOfFunction.Create('MONTHOF'));
  Functions.Add(TZDayOfFunction.Create('DAYOF'));
  Functions.Add(TZHourOfFunction.Create('HOUROF'));

  Functions.Add(TZMinuteOfFunction.Create('MINUTEOF'));
  Functions.Add(TZSecondOfFunction.Create('SECONDOF'));
  Functions.Add(TZMilliSecondOfFunction.Create('MILLISECONDOF'));

  Functions.Add(TZWeekOfTheYearFunction.Create('WEEKOFTHEYEAR'));
  Functions.Add(TZDayOfTheYearFunction.Create('DAYOFTHEYEAR'));
  Functions.Add(TZHourOfTheYearFunction.Create('HOUROFTHEYEAR'));
  Functions.Add(TZMinuteOfTheYearFunction.Create('MINUTEOFTHEYEAR'));
  Functions.Add(TZSecondOfTheYearFunction.Create('SECONDOFTHEYEAR'));
  Functions.Add(TZMilliSecondOfTheYearFunction.Create('MILLISECONDOFTHEYEAR'));

  Functions.Add(TZWeekOfTheMonthFunction.Create('WEEKOFTHEMONTH'));
  Functions.Add(TZHourOfTheMonthFunction.Create('HOUROFTHEMONTH'));
  Functions.Add(TZMinuteOfTheMonthFunction.Create('MINUTEOFTHEMONTH'));
  Functions.Add(TZSecondOfTheMonthFunction.Create('SECONDOFTHEMONTH'));
  Functions.Add(TZMilliSecondOfTheMonthFunction.Create('MILLISECONDOFTHEMONTH'));

  Functions.Add(TZDayOfTheWeekFunction.Create('DAYOFTHEWEEK'));
  Functions.Add(TZHourOfTheWeekFunction.Create('HOUROFTHEWEEK'));
  Functions.Add(TZMinuteOfTheWeekFunction.Create('MINUTEOFTHEWEEK'));
  Functions.Add(TZSecondOfTheWeekFunction.Create('SECONDOFTHEWEEK'));
  Functions.Add(TZMilliSecondOfTheWeekFunction.Create('MILLISECONDOFTHEWEEK'));

  Functions.Add(TZMinuteOfTheDayFunction.Create('MINUTEOFTHEDAY'));
  Functions.Add(TZSecondOfTheDayFunction.Create('SECONDOFTHEDAY'));
  Functions.Add(TZMilliSecondOfTheDayFunction.Create('MILLISECONDOFTHEDAY'));

  Functions.Add(TZSecondOfTheHourFunction.Create('SECONDOFTHEHOUR'));
  Functions.Add(TZMilliSecondOfTheHourFunction.Create('MILLISECONDOFTHEHOUR'));

  Functions.Add(TZMilliSecondOfTheMinuteFunction.Create('MILLISECONDOFTHEMINUTE'));

  Functions.Add(TZYearsBetweenFunction.Create('YEARSBETWEEN'));
  Functions.Add(TZMonthsBetweenFunction.Create('MONTHSBETWEEN'));
  Functions.Add(TZWeeksBetweenFunction.Create('WEEKSBETWEEN'));
  Functions.Add(TZDaysBetweenFunction.Create('DAYSBETWEEN'));
  Functions.Add(TZHoursBetweenFunction.Create('HOURSBETWEEN'));
  Functions.Add(TZMinutesBetweenFunction.Create('MINUTESBETWEEN'));
  Functions.Add(TZSecondsBetweenFunction.Create('SECONDSBETWEEN'));
  Functions.Add(TZMilliSecondsBetweenFunction.Create('MILLISECONDSBETWEEN'));

end;

end.
