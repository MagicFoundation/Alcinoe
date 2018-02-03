{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclSchedule.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created Marcel Bestebroer are Copyright (C) Marcel Bestebroer. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel Bestebroer (marcelb)                                                                    }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains scheduler classes.                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSchedule;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

type
  TScheduleRecurringKind = (srkOneShot, srkDaily, srkWeekly, srkMonthly, srkYearly);
  TScheduleEndKind = (sekNone, sekDate, sekTriggerCount, sekDayCount);
  TScheduleWeekDay = (swdMonday, swdTuesday, swdWednesday, swdThursday, swdFriday, swdSaturday,
    swdSunday);
  TScheduleWeekDays = set of TScheduleWeekDay;
  TScheduleIndexKind = (sikNone, sikDay, sikWeekDay, sikWeekendDay, sikMonday, sikTuesday,
    sikWednesday, sikThursday, sikFriday, sikSaturday, sikSunday);

const
  sivFirst = 1;
  sivSecond = 2;
  sivThird = 3;
  sivFourth = 4;
  sivLast = -1;

type
  // Forwards
  IJclSchedule = interface;
  IJclDailySchedule = interface;
  IJclWeeklySchedule = interface;
  IJclMonthlySchedule = interface;
  IJclYearlySchedule = interface;

  EJclScheduleError = class(EJclError);
  ESchedule = EJclScheduleError;

  IJclSchedule = interface(IUnknown)
    ['{1CC54450-7F84-4F27-B1C1-418C451DAD80}']
    function GetStartDate: TTimeStamp;
    function GetRecurringType: TScheduleRecurringKind;
    function GetEndType: TScheduleEndKind;
    function GetEndDate: TTimeStamp;
    function GetEndCount: Cardinal;
    procedure SetStartDate(const Value: TTimeStamp);
    procedure SetRecurringType(Value: TScheduleRecurringKind);
    procedure SetEndType(Value: TScheduleEndKind);
    procedure SetEndDate(const Value: TTimeStamp);
    procedure SetEndCount(Value: Cardinal);

    function TriggerCount: Cardinal;
    function DayCount: Cardinal;
    function LastTriggered: TTimeStamp;

    procedure InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
      LastDayCount: Cardinal);
    procedure Reset;
    function NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
    function NextEventFrom(const FromEvent: TTimeStamp; CountMissedEvent: Boolean = False): TTimeStamp;
    function NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;

    property StartDate: TTimeStamp read GetStartDate write SetStartDate;
    property RecurringType: TScheduleRecurringKind read GetRecurringType write SetRecurringType;
    property EndType: TScheduleEndKind read GetEndType write SetEndType;
    property EndDate: TTimeStamp read GetEndDate write SetEndDate;
    property EndCount: Cardinal read GetEndCount write SetEndCount;
  end;

  IJclScheduleDayFrequency = interface(IUnknown)
    ['{6CF37F0D-56F4-4AE6-BBCA-7B9DFE60F50D}']
    function GetStartTime: Cardinal;
    function GetEndTime: Cardinal;
    function GetInterval: Cardinal;
    procedure SetStartTime(Value: Cardinal);
    procedure SetEndTime(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property StartTime: Cardinal read GetStartTime write SetStartTime;
    property EndTime: Cardinal read GetEndTime write SetEndTime;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclDailySchedule = interface(IUnknown)
    ['{540E22C5-BE14-4539-AFB3-E24A67C58D8A}']
    function GetEveryWeekDay: Boolean;
    function GetInterval: Cardinal;
    procedure SetEveryWeekDay(Value: Boolean);
    procedure SetInterval(Value: Cardinal);

    property EveryWeekDay: Boolean read GetEveryWeekDay write SetEveryWeekDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclWeeklySchedule = interface(IUnknown)
    ['{73F15D99-C6A1-4526-8DE3-A2110E099BBC}']
    function GetDaysOfWeek: TScheduleWeekDays;
    function GetInterval: Cardinal;
    procedure SetDaysOfWeek(Value: TScheduleWeekDays);
    procedure SetInterval(Value: Cardinal);

    property DaysOfWeek: TScheduleWeekDays read GetDaysOfWeek write SetDaysOfWeek;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclMonthlySchedule = interface(IUnknown)
    ['{705E17FC-83E6-4385-8D2D-17013052E9B3}']
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

  IJclYearlySchedule = interface(IUnknown)
    ['{3E5303B0-FFA0-495A-96BB-14A718A01C1B}']
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetMonth: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal);
    procedure SetMonth(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Month: Cardinal read GetMonth write SetMonth;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

function CreateSchedule: IJclSchedule;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclDateTime, JclResources;

//=== { TJclScheduleAggregate } ==============================================

type
  TJclScheduleAggregate = class(TAggregatedObject)
  protected
    procedure CheckInterfaceAllowed;
    function InterfaceAllowed: Boolean;
    function Schedule: IJclSchedule;
    class function RecurringType: TScheduleRecurringKind; virtual;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; virtual; abstract;
    procedure MakeValidStamp(var Stamp: TTimeStamp); virtual; abstract;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; virtual; abstract;
  end;

procedure TJclScheduleAggregate.CheckInterfaceAllowed;
begin
  if not InterfaceAllowed then
    RunError(23); // reIntfCastError
end;

function TJclScheduleAggregate.InterfaceAllowed: Boolean;
begin
  Result := Schedule.RecurringType = RecurringType;
end;

function TJclScheduleAggregate.Schedule: IJclSchedule;
begin
  Result := Controller as IJclSchedule;
end;

class function TJclScheduleAggregate.RecurringType: TScheduleRecurringKind;
begin
  Result := srkOneShot;
end;

//=== { TJclDayFrequency } ===================================================

type
  TJclDayFrequency = class(TAggregatedObject, IJclScheduleDayFrequency, IInterface)
  private
    FStartTime: Cardinal;
    FEndTime: Cardinal;
    FInterval: Cardinal;
  protected
    function ValidStamp(const Stamp: TTimeStamp): Boolean;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
  public
    constructor Create(const Controller: IUnknown);
    { IJclScheduleDayFrequency }
    function GetStartTime: Cardinal;
    function GetEndTime: Cardinal;
    function GetInterval: Cardinal;
    procedure SetStartTime(Value: Cardinal);
    procedure SetEndTime(Value: Cardinal);
    procedure SetInterval(Value: Cardinal);

    property StartTime: Cardinal read GetStartTime write SetStartTime;
    property EndTime: Cardinal read GetEndTime write SetEndTime;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TJclDayFrequency.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FStartTime := 0;
  FEndTime := HoursToMSecs(24) - 1;
  FInterval := 500;
end;

function TJclDayFrequency.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (Cardinal(Stamp.Time) >= FStartTime) and (Cardinal(Stamp.Time) <= FEndTime) and
    ((Cardinal(Stamp.Time) - FStartTime) mod FInterval = 0);
end;

function TJclDayFrequency.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  if Stamp.Time < Integer(FStartTime) then
    Result.Time := FStartTime
  else
  if ((Cardinal(Stamp.Time) - FStartTime) mod FInterval) <> 0 then
    Result.Time := Stamp.Time + Integer(FInterval-(Cardinal(Stamp.Time) - FStartTime) mod FInterval)
  else
    Result.Time := Stamp.Time + Integer(FInterval);
  if (Result.Time < 0) or (Cardinal(Result.Time) > FEndTime) then
    Result := NullStamp;
end;

function TJclDayFrequency.GetStartTime: Cardinal;
begin
  Result := FStartTime;
end;

function TJclDayFrequency.GetEndTime: Cardinal;
begin
  Result := FEndTime;
end;

function TJclDayFrequency.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

procedure TJclDayFrequency.SetStartTime(Value: Cardinal);
begin
  if Value <> FStartTime then
  begin
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise EJclScheduleError.CreateRes(@RsScheduleInvalidTime);
    FStartTime := Value;
    if EndTime < StartTime then
      FEndTime := Value;
  end;
end;

procedure TJclDayFrequency.SetEndTime(Value: Cardinal);
begin
  if Value <> FEndTime then
  begin
    if Value < FStartTime then
      raise EJclScheduleError.CreateRes(@RsScheduleEndBeforeStart);
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise EJclScheduleError.CreateRes(@RsScheduleInvalidTime);
    FEndTime := Value;
  end;
end;

procedure TJclDayFrequency.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    if Value >= Cardinal(HoursToMSecs(24)) then
      raise EJclScheduleError.CreateRes(@RsScheduleInvalidTime);
    if Value = 0 then
    begin
      FEndTime := FStartTime;
      FInterval := 1;
    end
    else
      FInterval := Value;
  end;
end;

//=== { TJclDailySchedule } ==================================================

type
  TJclDailySchedule = class(TJclScheduleAggregate, IJclDailySchedule, IInterface)
  private
    FEveryWeekDay: Boolean;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    { IJclDailySchedule }
    function GetEveryWeekDay: Boolean;
    function GetInterval: Cardinal;
    procedure SetEveryWeekDay(Value: Boolean);
    procedure SetInterval(Value: Cardinal);

    property EveryWeekDay: Boolean read GetEveryWeekDay write SetEveryWeekDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TJclDailySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FEveryWeekDay := True;
  FInterval := 1;
end;

class function TJclDailySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkDaily;
end;

function TJclDailySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (FEveryWeekDay and (TimeStampDOW(Stamp) < 6)) or
    (not FEveryWeekDay and (Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval = 0));
end;

procedure TJclDailySchedule.MakeValidStamp(var Stamp: TTimeStamp);
begin
  if FEveryWeekDay and (TimeStampDOW(Stamp) >= 6) then
    Inc(Stamp.Date, 2 - (TimeStampDOW(Stamp) - 6))
  else
  if not FEveryWeekDay and (Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval <> 0) then
    Inc(Stamp.Date, Interval - Cardinal(Stamp.Date - Schedule.StartDate.Date) mod Interval);
end;

function TJclDailySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    if FEveryWeekDay then
    begin
      Inc(Result.Date);
      MakeValidStamp(Result);     // Skip over the weekend.
    end
    else
      Inc(Result.Date, Interval); // always valid as we started with a valid stamp
  end;
end;

function TJclDailySchedule.GetEveryWeekDay: Boolean;
begin
  CheckInterfaceAllowed;
  Result := FEveryWeekDay;
end;

function TJclDailySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  if EveryWeekDay then
    Result := 0
  else
    Result := FInterval;
end;

procedure TJclDailySchedule.SetEveryWeekDay(Value: Boolean);
begin
  CheckInterfaceAllowed;
  FEveryWeekDay := Value;
end;

procedure TJclDailySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise EJclScheduleError.CreateRes(@RsScheduleIntervalZero);
  if FEveryWeekDay then
    FEveryWeekDay := False;
  if Value <> FInterval then
    FInterval := Value;
end;

//=== { TJclWeeklySchedule } =================================================

type
  TJclWeeklySchedule = class(TJclScheduleAggregate, IJclWeeklySchedule, IInterface)
  private
    FDaysOfWeek: TScheduleWeekDays;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    { IJclWeeklySchedule }
    function GetDaysOfWeek: TScheduleWeekDays;
    function GetInterval: Cardinal;
    procedure SetDaysOfWeek(Value: TScheduleWeekDays);
    procedure SetInterval(Value: Cardinal);

    property DaysOfWeek: TScheduleWeekDays read GetDaysOfWeek write SetDaysOfWeek;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TJclWeeklySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FDaysOfWeek := [swdMonday];
  FInterval := 1;
end;

class function TJclWeeklySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkWeekly;
end;

function TJclWeeklySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
begin
  Result := (TScheduleWeekDay(TimeStampDOW(Stamp)) in DaysOfWeek) and
    (Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval = 0);
end;

procedure TJclWeeklySchedule.MakeValidStamp(var Stamp: TTimeStamp);
begin
  while not (TScheduleWeekDay(TimeStampDOW(Stamp) - 1) in DaysOfWeek) do
    Inc(Stamp.Date);
  if (Stamp.Date - Schedule.StartDate.Date) <> 0 then
  begin
    if Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval <> 0 then
      Inc(Stamp.Date, 7 * (Interval -
        (Cardinal((Stamp.Date - Schedule.StartDate.Date) div 7) mod Interval)));
  end;
end;

function TJclWeeklySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and weeks
  end;
end;

function TJclWeeklySchedule.GetDaysOfWeek: TScheduleWeekDays;
begin
  CheckInterfaceAllowed;
  Result := FDaysOfWeek;
end;

function TJclWeeklySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FInterval;
end;

procedure TJclWeeklySchedule.SetDaysOfWeek(Value: TScheduleWeekDays);
begin
  CheckInterfaceAllowed;
  if Value = [] then
    raise EJclScheduleError.CreateRes(@RsScheduleNoDaySpecified);
  FDaysOfWeek := Value;
end;

procedure TJclWeeklySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise EJclScheduleError.CreateRes(@RsScheduleIntervalZero);
  FInterval := Value;
end;

//=== { TJclMonthlySchedule } ================================================

type
  TJclMonthlySchedule = class(TJclScheduleAggregate, IJclMonthlySchedule, IInterface)
  private
    FIndexKind: TScheduleIndexKind;
    FIndexValue: Integer;
    FDay: Cardinal;
    FInterval: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;

    function ValidStampMonthIndex(const TYear, TMonth, TDay: Word): Boolean;
    procedure MakeValidStampMonthIndex(var TYear, TMonth, TDay: Word);
  public
    constructor Create(const Controller: IUnknown);
    { IJclMonthlySchedule }
    function GetIndexKind: TScheduleIndexKind;
    function GetIndexValue: Integer;
    function GetDay: Cardinal;
    function GetInterval: Cardinal;
    procedure SetIndexKind(Value: TScheduleIndexKind);
    procedure SetIndexValue(Value: Integer);
    procedure SetDay(Value: Cardinal); 
    procedure SetInterval(Value: Cardinal);

    property IndexKind: TScheduleIndexKind read GetIndexKind write SetIndexKind;
    property IndexValue: Integer read GetIndexValue write SetIndexValue;
    property Day: Cardinal read GetDay write SetDay;
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

constructor TJclMonthlySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FIndexKind := sikNone;
  FIndexValue := sivFirst;
  FDay := 1;
  FInterval := 1;
end;

class function TJclMonthlySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkMonthly;
end;

function TJclMonthlySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
begin
  DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  Result := (((TYear * 12 + TMonth) - (SYear * 12 + SMonth)) mod Integer(Interval) = 0) and
    ValidStampMonthIndex(TYear, TMonth, TDay);
end;

procedure TJclMonthlySchedule.MakeValidStamp(var Stamp: TTimeStamp);
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
  MonthDiff: Integer;
begin
  DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  MonthDiff := (TYear * 12 + TMonth) - (SYear * 12 + SMonth);
  if MonthDiff mod Integer(Interval) <> 0 then
  begin
    Inc(TMonth, Integer(Interval) - (MonthDiff mod Integer(Interval)));
    if TMonth > 12 then
    begin
      Inc(TYear, TMonth div 12);
      TMonth := TMonth mod 12;
    end;
    TDay := 1;
  end;
  MakeValidStampMonthIndex(TYear, TMonth, TDay);
  while DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date < Stamp.Date do
  begin
    Inc(TMonth, Integer(Interval));
    if TMonth > 12 then
    begin
      Inc(TYear, TMonth div 12);
      TMonth := TMonth mod 12;
    end;
    MakeValidStampMonthIndex(TYear, TMonth, TDay);
  end;
  Stamp.Date := DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date;
end;

function TJclMonthlySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and months
  end;
end;

function TJclMonthlySchedule.ValidStampMonthIndex(const TYear, TMonth, TDay: Word): Boolean;
var
  DIM: Integer;
  TempDay: Integer;
begin
  DIM := DaysInMonth(JclDateTime.EncodeDate(TYear, TMonth, 1));
  case IndexKind of
    sikNone:
      Result := (TDay = Day) or ((Integer(Day) > DIM) and (TDay = DIM));
    sikDay:
      Result :=
        ((IndexValue = sivLast) and (TDay = DIM)) or
        ((IndexValue <> sivLast) and (
          (TDay = IndexValue) or (
            (IndexValue > DIM) and
            (TDay = DIM)
          ) or (
            (IndexValue < 0) and (
              (TDay = DIM + 1 + IndexValue) or (
                (-IndexValue > DIM) and
                (TDay = 1)
              )
            )
          )
        ));
    sikWeekDay:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstWeekDay(TYear, TMonth);
          sivLast:
            TempDay := LastWeekDay(TYear, TMonth);
          else
            TempDay := IndexedWeekDay(TYear, TMonth, IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastWeekDay(TYear, TMonth)
              else
              if IndexValue < 0 then
                TempDay := FirstWeekDay(TYear, TMonth);
            end;
        end;
        Result := TDay = TempDay;
      end;
    sikWeekendDay:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstWeekendDay(TYear, TMonth);
          sivLast:
            TempDay := LastWeekendDay(TYear, TMonth);
          else
            TempDay := IndexedWeekendDay(TYear, TMonth, IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastWeekendDay(TYear, TMonth)
              else
              if IndexValue < 0 then
                TempDay := FirstWeekendDay(TYear, TMonth);
            end;
        end;
        Result := TDay = TempDay;
      end;
    sikMonday..sikSunday:
      begin
        case IndexValue of
          sivFirst:
            TempDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          sivLast:
            TempDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          else
            TempDay := IndexedDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay),
              IndexValue);
            if TempDay = 0 then
            begin
              if IndexValue > 0 then
                TempDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay))
              else
              if IndexValue < 0 then
                TempDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
            end;
        end;
        Result := TDay = TempDay;
      end;
    else
      Result := False;
  end;
end;

procedure TJclMonthlySchedule.MakeValidStampMonthIndex(var TYear, TMonth, TDay: Word);
var
  DIM: Integer;
begin
  DIM := DaysInMonth(JclDateTime.EncodeDate(TYear, TMonth, 1));
  case IndexKind of
    sikNone:
      begin
        TDay := Day;
        if Integer(Day) > DIM then
          TDay := DIM;
      end;
    sikDay:
      begin
        if (IndexValue = sivLast) or (Integer(IndexValue) > DIM) then
          TDay := DIM
        else
        if IndexValue > 0 then
          TDay := IndexValue
        else
        begin
          if -IndexValue > DIM then
            TDay := 1
          else
            TDay := DIM + 1 + IndexValue;
        end;
      end;
    sikWeekDay:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstWeekDay(TYear, TMonth);
          sivLast:
            TDay := LastWeekDay(TYear, TMonth);
          else
            begin
              TDay := IndexedWeekDay(TYear, TMonth, IndexValue);
              if TDay = 0 then
              begin
                if IndexValue > 0 then
                  TDay := LastWeekDay(TYear, TMonth)
                else
                if IndexValue < 0 then
                  TDay := FirstWeekDay(TYear, TMonth);
              end;
            end;
        end;
      end;
    sikWeekendDay:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstWeekendDay(TYear, TMonth);
          sivLast:
            TDay := LastWeekendDay(TYear, TMonth);
          else
            begin
              TDay := IndexedWeekendDay(TYear, TMonth, IndexValue);
              if TDay = 0 then
              begin
                if IndexValue > 0 then
                  TDay := LastWeekendDay(TYear, TMonth)
                else
                if IndexValue < 0 then
                  TDay := FirstWeekendDay(TYear, TMonth);
              end;
            end;
        end;
      end;
    sikMonday..sikSunday:
      begin
        case IndexValue of
          sivFirst:
            TDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          sivLast:
            TDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
          else
            TDay := IndexedDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay),
              IndexValue);
            if TDay = 0 then
            begin
              if IndexValue > 0 then
                TDay := LastDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay))
              else
              if IndexValue < 0 then
                TDay := FirstDayOfWeek(TYear, TMonth, Ord(IndexKind) - Ord(sikWeekendDay));
            end;
        end;
      end;
  end;
end;

function TJclMonthlySchedule.GetIndexKind: TScheduleIndexKind;
begin
  CheckInterfaceAllowed;
  Result := FIndexKind;
end;

function TJclMonthlySchedule.GetIndexValue: Integer;
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikDay .. sikSunday]) then
    raise EJclScheduleError.CreateRes(@RsScheduleIndexValueSup);
  Result := FIndexValue;
end;

function TJclMonthlySchedule.GetDay: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FDay;
end;

function TJclMonthlySchedule.GetInterval: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FInterval;
end;

procedure TJclMonthlySchedule.SetIndexKind(Value: TScheduleIndexKind);
begin
  CheckInterfaceAllowed;
  FIndexKind := Value;
end;

procedure TJclMonthlySchedule.SetIndexValue(Value: Integer);
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikDay .. sikSunday]) then
    raise EJclScheduleError.CreateRes(@RsScheduleIndexValueSup);
  if Value = 0 then
    raise EJclScheduleError.CreateRes(@RsScheduleIndexValueZero);
  FIndexValue := Value;
end;

procedure TJclMonthlySchedule.SetDay(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if not (FIndexKind in [sikNone]) then
    raise EJclScheduleError.CreateRes(@RsScheduleDayNotSupported);
  if (Value = 0) or (Value > 31) then
    raise EJclScheduleError.CreateRes(@RsScheduleDayInRange);
  FDay := Value;
end;

procedure TJclMonthlySchedule.SetInterval(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if Value = 0 then
    raise EJclScheduleError.CreateRes(@RsScheduleIntervalZero);
  FInterval := Value;
end;

//=== { TJclYearlySchedule } =================================================

type
  TJclYearlySchedule = class(TJclMonthlySchedule, IJclYearlySchedule, IInterface)
  private
    FMonth: Cardinal;
  protected
    class function RecurringType: TScheduleRecurringKind; override;

    function ValidStamp(const Stamp: TTimeStamp): Boolean; override;
    procedure MakeValidStamp(var Stamp: TTimeStamp); override;
    function NextValidStamp(const Stamp: TTimeStamp): TTimeStamp; override;
  public
    constructor Create(const Controller: IUnknown);
    { IJclYearlySchedule }
    function GetMonth: Cardinal;
    procedure SetMonth(Value: Cardinal);
    
    property Month: Cardinal read GetMonth write SetMonth;
  end;

constructor TJclYearlySchedule.Create(const Controller: IUnknown);
begin
  inherited Create(Controller);
  FMonth := 1;
end;

class function TJclYearlySchedule.RecurringType: TScheduleRecurringKind;
begin
  Result := srkYearly;
end;

function TJclYearlySchedule.ValidStamp(const Stamp: TTimeStamp): Boolean;
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
begin
  JclDateTime.DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  JclDateTime.DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  Result := ((TYear - SYear) mod Integer(Interval) = 0) and (TMonth = Month) and
    ValidStampMonthIndex(TYear, TMonth, TDay);
end;

procedure TJclYearlySchedule.MakeValidStamp(var Stamp: TTimeStamp);
var
  SYear, SMonth, SDay: Word;
  TYear, TMonth, TDay: Word;
  YearDiff: Integer;
begin
  JclDateTime.DecodeDate(TimeStampToDateTime(Schedule.StartDate), SYear, SMonth, SDay);
  JclDateTime.DecodeDate(TimeStampToDateTime(Stamp), TYear, TMonth, TDay);
  YearDiff := TYear - SYear;
  if YearDiff mod Integer(Interval) <> 0 then
  begin
    Inc(TYear, Integer(Interval) - (YearDiff mod Integer(Interval)));
    TMonth := Month;
    TDay := 1;
  end;
  MakeValidStampMonthIndex(TYear, TMonth, TDay);
  while DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date < Stamp.Date do
  begin
    Inc(TYear, Integer(Interval));
    TMonth := Month;
    TDay := 1;
    MakeValidStampMonthIndex(TYear, TMonth, TDay);
  end;
  Stamp.Date := DateTimeToTimeStamp(JclDateTime.EncodeDate(TYear, TMonth, TDay)).Date;
end;

function TJclYearlySchedule.NextValidStamp(const Stamp: TTimeStamp): TTimeStamp;
begin
  Result := Stamp;
  MakeValidStamp(Result);
  if EqualTimeStamps(Stamp, Result) then
  begin
    // Time stamp has not been adjusted (it was valid). Determine the next time stamp
    Inc(Result.Date);
    MakeValidStamp(Result);    // Skip over unwanted days and months
  end;
end;

function TJclYearlySchedule.GetMonth: Cardinal;
begin
  CheckInterfaceAllowed;
  Result := FMonth;
end;

procedure TJclYearlySchedule.SetMonth(Value: Cardinal);
begin
  CheckInterfaceAllowed;
  if (Value < 1) or (Value > 12) then
    raise EJclScheduleError.CreateRes(@RsScheduleMonthInRange);
  FMonth := Value;
end;

//=== { TJclSchedule } =======================================================

type
  TJclSchedule = class(TInterfacedObject, IJclSchedule, IJclScheduleDayFrequency, IJclDailySchedule,
    IJclWeeklySchedule, IJclMonthlySchedule, IJclYearlySchedule)
  private
    FStartDate: TTimeStamp;
    FRecurringType: TScheduleRecurringKind;
    FEndType: TScheduleEndKind;
    FEndDate: TTimeStamp;
    FEndCount: Cardinal;
    FDayFrequency: TJclDayFrequency;
    FDailySchedule: TJclDailySchedule;
    FWeeklySchedule: TJclWeeklySchedule;
    FMonthlySchedule: TJclMonthlySchedule;
    FYearlySchedule: TJclYearlySchedule;
  protected
    FTriggerCount: Cardinal;
    FDayCount: Cardinal;
    FLastEvent: TTimeStamp;
    function GetNextEventStamp(const From: TTimeStamp): TTimeStamp;
  public
    constructor Create;
    destructor Destroy; override;

    { IJclSchedule }
    function GetStartDate: TTimeStamp;
    function GetRecurringType: TScheduleRecurringKind;
    function GetEndType: TScheduleEndKind;
    function GetEndDate: TTimeStamp;
    function GetEndCount: Cardinal;
    procedure SetStartDate(const Value: TTimeStamp);
    procedure SetRecurringType(Value: TScheduleRecurringKind);
    procedure SetEndType(Value: TScheduleEndKind);
    procedure SetEndDate(const Value: TTimeStamp);
    procedure SetEndCount(Value: Cardinal);

    function TriggerCount: Cardinal;
    function DayCount: Cardinal;
    function LastTriggered: TTimeStamp;

    procedure InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
      LastDayCount: Cardinal);
    procedure Reset;
    function NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
    function NextEventFrom(const FromEvent: TTimeStamp;
      CountMissedEvent: Boolean = False): TTimeStamp;
    function NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;

    property StartDate: TTimeStamp read GetStartDate write SetStartDate;
    property RecurringType: TScheduleRecurringKind read GetRecurringType write SetRecurringType;
    property EndType: TScheduleEndKind read GetEndType write SetEndType;
    property EndDate: TTimeStamp read GetEndDate write SetEndDate;
    property EndCount: Cardinal read GetEndCount write SetEndCount;

    { IJclScheduleDayFrequency }
    function GetDayFrequency: IJclScheduleDayFrequency;
    property DayFrequency: IJclScheduleDayFrequency read GetDayFrequency implements IJclScheduleDayFrequency;
    { IJclDailySchedule }
    function GetDailySchedule: IJclDailySchedule;
    property DailySchedule: IJclDailySchedule read GetDailySchedule implements IJclDailySchedule;
    { IJclWeeklySchedule }
    function GetWeeklySchedule: IJclWeeklySchedule;
    property WeeklySchedule: IJclWeeklySchedule read GetWeeklySchedule implements IJclWeeklySchedule;
    { IJclMonthlySchedule }
    function GetMonthlySchedule: IJclMonthlySchedule;
    property MonthlySchedule: IJclMonthlySchedule read GetMonthlySchedule implements IJclMonthlySchedule;
    { IJclYearlySchedule }
    function GetYearlySchedule: IJclYearlySchedule;
    property YearlySchedule: IJclYearlySchedule read GetYearlySchedule implements IJclYearlySchedule;
  end;

constructor TJclSchedule.Create;
var
  InitialStamp: TTimeStamp;
begin
  inherited Create;
  FDayFrequency := TJclDayFrequency.Create(Self);
  FDailySchedule := TJclDailySchedule.Create(Self);
  FWeeklySchedule := TJclWeeklySchedule.Create(Self);
  FMonthlySchedule := TJclMonthlySchedule.Create(Self);
  FYearlySchedule := TJclYearlySchedule.Create(Self);
  InitialStamp := DateTimeToTimeStamp(Now);
  InitialStamp.Time := 1000 * (InitialStamp.Time div 1000); // strip of milliseconds
  StartDate := InitialStamp;
  EndType := sekNone;
  RecurringType := srkOneShot;
end;

destructor TJclSchedule.Destroy;
begin
  FreeAndNil(FYearlySchedule);
  FreeAndNil(FMonthlySchedule);
  FreeAndNil(FWeeklySchedule);
  FreeAndNil(FDailySchedule);
  FreeAndNil(FDayFrequency);
  inherited Destroy;
end;

function TJclSchedule.GetDayFrequency: IJclScheduleDayFrequency;
begin
  Result := FDayFrequency;
end;

function TJclSchedule.GetDailySchedule: IJclDailySchedule;
begin
  Result := FDailySchedule;
end;

function TJclSchedule.GetWeeklySchedule: IJclWeeklySchedule;
begin
  Result := FWeeklySchedule;
end;

function TJclSchedule.GetMonthlySchedule: IJclMonthlySchedule;
begin
  Result := FMonthlySchedule;
end;

function TJclSchedule.GetYearlySchedule: IJclYearlySchedule;
begin
  Result := FYearlySchedule;
end;

function TJclSchedule.GetNextEventStamp(const From: TTimeStamp): TTimeStamp;
var
  UseFrom: TTimeStamp;
begin
  Result := NullStamp;
  UseFrom := From;
  if (From.Date = 0) or (From.Date < StartDate.Date) then
  begin
    UseFrom := StartDate;
    Dec(UseFrom.Time);
  end;
  case RecurringType of
    srkOneShot:
      if TriggerCount = 0 then
        Result := StartDate;
    srkDaily:
      begin
        Result := FDayFrequency.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := FDayFrequency.StartTime;
          Result := FDailySchedule.NextValidStamp(Result);
        end
        else
          FDailySchedule.MakeValidStamp(Result);
      end;
    srkWeekly:
      begin
        Result := FDayFrequency.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := FDayFrequency.StartTime;
          Result := FWeeklySchedule.NextValidStamp(Result);
        end
        else
          FWeeklySchedule.MakeValidStamp(Result);
      end;
    srkMonthly:
      begin
        Result := FDayFrequency.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := FDayFrequency.StartTime;
          Result := FMonthlySchedule.NextValidStamp(Result);
        end
        else
          FMonthlySchedule.MakeValidStamp(Result);
      end;
    srkYearly:
      begin
        Result := FDayFrequency.NextValidStamp(UseFrom);
        if IsNullTimeStamp(Result) then
        begin
          Result.Date := UseFrom.Date;
          Result.Time := FDayFrequency.StartTime;
          Result := FYearlySchedule.NextValidStamp(Result);
        end
        else
          FYearlySchedule.MakeValidStamp(Result);
      end;
  end;
  if CompareTimeStamps(Result, UseFrom) < 0 then
    Result := NullStamp;
  if not IsNullTimeStamp(Result) then
  begin
    if ((EndType = sekDate) and (CompareTimeStamps(Result, EndDate) > 0)) or
        ((EndType = sekDayCount) and (DayCount = EndCount) and (UseFrom.Date <> Result.Date)) or
        ((EndType = sekTriggerCount) and (TriggerCount = EndCount)) then
      Result := NullStamp
    else
    begin
      Inc(FTriggerCount);
      if (UseFrom.Date <> Result.Date) or (DayCount = 0) then
        Inc(FDayCount);
      FLastEvent := Result;
    end;
  end;
end;

function TJclSchedule.GetStartDate: TTimeStamp;
begin
  Result := FStartDate;
end;

function TJclSchedule.GetRecurringType: TScheduleRecurringKind;
begin
  Result := FRecurringType;
end;

function TJclSchedule.GetEndType: TScheduleEndKind;
begin
  Result := FEndType;
end;

function TJclSchedule.GetEndDate: TTimeStamp;
begin
  Result := FEndDate;
end;

function TJclSchedule.GetEndCount: Cardinal;
begin
  Result := FEndCount;
end;

procedure TJclSchedule.SetStartDate(const Value: TTimeStamp);
begin
  FStartDate := Value;
end;

procedure TJclSchedule.SetRecurringType(Value: TScheduleRecurringKind);
begin
  FRecurringType := Value;
end;

procedure TJclSchedule.SetEndType(Value: TScheduleEndKind);
begin
  FEndType := Value;
end;

procedure TJclSchedule.SetEndDate(const Value: TTimeStamp);
begin
  FEndDate := Value;
end;

procedure TJclSchedule.SetEndCount(Value: Cardinal);
begin
  FEndCount := Value;
end;

function TJclSchedule.TriggerCount: Cardinal;
begin
  Result := FTriggerCount;
end;

function TJclSchedule.DayCount: Cardinal;
begin
  Result := FDayCount;
end;

function TJclSchedule.LastTriggered: TTimeStamp;
begin
  Result := FLastEvent;
end;

procedure TJclSchedule.InitToSavedState(const LastTriggerStamp: TTimeStamp; const LastTriggerCount,
  LastDayCount: Cardinal);
begin
  FLastEvent := LastTriggerStamp;
  FTriggerCount := LastTriggerCount;
  FDayCount := LastDayCount;
end;

procedure TJclSchedule.Reset;
begin
  FLastEvent := NullStamp;
  FTriggerCount := 0;
  FDayCount := 0;
end;

function TJclSchedule.NextEvent(CountMissedEvents: Boolean = False): TTimeStamp;
begin
  Result := NextEventFrom(FLastEvent, CountMissedEvents);
end;

function TJclSchedule.NextEventFrom(const FromEvent: TTimeStamp;
  CountMissedEvent: Boolean = False): TTimeStamp;
begin
  if CountMissedEvent then
  begin
    Result := FLastEvent;
    repeat
      Result := GetNextEventStamp(Result);
    until IsNullTimeStamp(Result) or (CompareTimeStamps(FromEvent, Result) <= 0);
  end
  else
    Result := GetNextEventStamp(FromEvent);
end;

function TJclSchedule.NextEventFromNow(CountMissedEvents: Boolean = False): TTimeStamp;
begin
  Result := NextEventFrom(DateTimeToTimeStamp(Now), CountMissedEvents);
end;

function CreateSchedule: IJclSchedule;
begin
  Result := TJclSchedule.Create;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
