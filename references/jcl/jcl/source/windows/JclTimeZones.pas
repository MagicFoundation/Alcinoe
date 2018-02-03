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
{ The Original Code is TimeZones.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rik Barker                                         }
{ <rik dott barker att visionsoft dott com>.                                                       }
{ Portions created by Rik Barker are Copyright (C) Rik Barker.  All Rights Reserved.               }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines and classes for working with Timezones and UTC dates.                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}


{**************************************************************************************************}
{ Important:                                                                                       }
{ Information on Timezones is pretty sparse and often vague.                                       }
{ According to MSDN GetTimeZoneInformation will return TIME_ZONE_ID_DAYLIGHT if                    }
{                                                                                                  }
{    "the system is operating in the range covered by the DaylightDate                             }
{     member of the TIME_ZONE_INFORMATION structure."                                              }
{                                                                                                  }
{ What it fails to mention is that it will take liberties with the TIME_ZONE_INFORMATION structure.}
{ Unless "Automatically Adjust clock for Daylight savings changes" is enabled, DayLightDate        }
{ will contain StandardDate and DayLightName will contain StandardName.                            }
{ So you know you're supposed to be in daylight savings, but you can't find out when it started or }
{ what the Daylight Name is.                                                                       }
{                                                                                                  }
{ Where possible use the functions/classes that read the data direct from the registry,            }
{ these will always return the correct daylight savings date and name                              }
{**************************************************************************************************}

unit JclTimeZones;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Windows, System.SysUtils, System.Contnrs, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  Windows, SysUtils, Contnrs, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

type
  // Contents of the TZI value in the Time Zones section of the registry
  TJclTZIValueInfo = record
    Bias: Longint;
    StandardBias: Integer;
    DaylightBias: Integer;
    StandardDate: TSystemTime;
    DaylightDate: TSystemTime;
  end;

  // All other Time Zone values in the registry
  PJclTimeZoneRegInfo = ^TJclTimeZoneRegInfo;
  TJclTimeZoneRegInfo = record
    DisplayDesc: string;
    StandardName: string;
    DaylightName: string;
    SortIndex: Integer;
    MapID: string;
    TZI: TJclTZIValueInfo;
  end;

type
  // Callback prototype for EnumTimeZones
  TJclTimeZoneCallBackFunc = function(const TimeZoneRec: TJclTimeZoneRegInfo): Boolean of object;

type
  TJclTimeZoneInfo = class(TObject)
  private
    FStandardName: string;
    FDaylightName: string;
    FTZDescription: string;
    FSortIndex: Integer; // Order to sort the timezones into
    FMapID: string; // Coordinates for the Date/Time properties map
    FBiasInfo: TJclTZIValueInfo; // Bias information from the registry
    function GetActiveBias: Integer;
    function GetCurrentDateTime: TDateTime;
    function GetDaylightSavingsStartDate: TDateTime;
    function GetGMTOffset: string;
    function GetStandardStartDate: TDateTime;
    function GetSupportsDaylightSavings: Boolean;
    function GetTimeZoneType(TZI: TJclTZIValueInfo): Cardinal;
  public
    procedure Assign(Source: TJclTimeZoneRegInfo);
    procedure ApplyTimeZone;
    function DayLightSavingsPeriod: string;
    function DateTimeIsInDaylightSavings(ADateTime: TDateTime):Boolean;
    function StandardStartDateInYear(const AYear: Integer): TDateTime;
    function DaylightStartDateInYear(const AYear: Integer): TDateTime;
    // These are all the values we want to be able to stream
    property ActiveBias: Integer read GetActiveBias;
    property CurrentDateTime: TDateTime read GetCurrentDateTime;
    property DaylightName: string read FDaylightName;
    property DaylightSavingsStartDate: TDateTime read GetDaylightSavingsStartDate;
    property DisplayDescription: string read FTZDescription;
    property GMTOffset: string read GetGMTOffset;
    property MapID: string read FMapID;
    property SortIndex: Integer read FSortIndex;
    property StandardName: string read FStandardName;
    property StandardStartDate: TDateTime read GetStandardStartDate;
    property SupportsDaylightSavings: Boolean read GetSupportsDaylightSavings;
  end;

  TJclTimeZones = class(TObject)
  private
    FActiveTimeZoneIndex: Integer;
    FTimeZones: TObjectList;
    FAutoAdjustEnabled: Boolean;
    function GetAutoAdjustEnabled: Boolean;
    function GetActiveTimeZoneInfo: TJclTimeZoneInfo;
    function GetCount: Integer;
    function GetItem(Index: Integer): TJclTimeZoneInfo;
    procedure LoadTimeZones;
    function TimeZoneCallback(const TimeZoneRec: TJclTimeZoneRegInfo): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function SetDateTime(DateTime: TDateTime): Boolean;
    procedure SetAutoAdjustEnabled(Value: Boolean);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJclTimeZoneInfo read GetItem; default;
    property ActiveTimeZone: TJclTimeZoneInfo read GetActiveTimeZoneInfo;
    property AutoAdjustEnabled: Boolean read FAutoAdjustEnabled write FAutoAdjustEnabled;
  end;

type
  EDaylightSavingsNotSupported = class(EJclError);
  EAutoAdjustNotEnabled = class(EJclError);

// Enumerate all time zones from the registry
function EnumTimeZones(CallBackFunc: TJclTimeZoneCallBackFunc): Boolean;

// Functions that read from the current time zone
function IsAutoAdjustEnabled: Boolean;
function CurrentTimeZoneSupportsDaylightSavings: Boolean;
function DateCurrentTimeZoneClocksChangeToStandard: TDateTime;
function DateCurrentTimeZoneClocksChangeToDaylightSavings: TDateTime;
function GetCurrentTimeZoneDescription: string;
function GetCurrentTimeZoneDaylightSavingsPeriod: string;
function GetCurrentTimeZoneGMTOffset: string;
function GetCurrentTimeZoneUTCBias: Integer;

// Misc UTC related functions
function GetWMIScheduledJobUTCTime(Time: TDateTime): string;
function UTCNow: TDateTime;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNITSCOPE}
  Winapi.Messages,
  {$ELSE ~HAS_UNITSCOPE}
  Messages,
  {$ENDIF ~HAS_UNITSCOPE}
  JclResources,
  JclDateTime, JclRegistry;

const
  cAutoAdjustKey = '\System\CurrentControlSet\Control\TimeZoneInformation';
  cAutoAdjustValue = 'DisableAutoDaylightTimeSet';

const
  UTCDays: array [0..6] of PChar =
    ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');
  UTCDaysOffset: array [1..5] of PChar =
    ('first', 'second', 'third', 'fourth', 'last');
  UTCMonths: array [1..12] of string =
   ('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December');

//--------------------------------------------------------------------------------------------------
// Callback enumerator of timezone information.  Return false to stop enumeration

function EnumTimeZones(CallBackFunc: TJclTimeZoneCallBackFunc): Boolean;
var
  TimeZoneRootName: string;
  TimeZones: TStringList;
  I: Integer;
  TimeZoneRegInfo: TJclTimeZoneRegInfo;
begin
  Result := True;

  if not Assigned(CallBackFunc) then
    raise Exception.Create(RsENoCallbackFunc);

  TimeZones := TStringList.Create;
  try
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      TimeZoneRootName := '\Software\Microsoft\Windows\CurrentVersion\Time Zones\'
    else
      TimeZoneRootName := '\Software\Microsoft\Windows NT\CurrentVersion\Time Zones\';

    if not RegGetKeyNames(HKEY_LOCAL_MACHINE, TimeZoneRootName, TimeZones) then
      Exit;

    for I := 0 to TimeZones.Count - 1 do
    begin
      FillChar(TimeZoneRegInfo, SizeOf(TJclTimeZoneRegInfo), #0);

      TimeZoneRegInfo.DisplayDesc := RegReadString(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'Display');
      TimeZoneRegInfo.StandardName := RegReadString(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'Std');
      TimeZoneRegInfo.DaylightName := RegReadString(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'Dlt');
      TimeZoneRegInfo.SortIndex := RegReadIntegerDef(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'Index', -1);
      TimeZoneRegInfo.MapID := RegReadStringDef(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'MapID', '');
      RegReadBinary(HKEY_LOCAL_MACHINE, TimeZoneRootName + TimeZones[I], 'TZI', TimeZoneRegInfo.TZI,
        SizeOf(TJclTZIValueInfo));

      // Allow the callback function to stop the enumeration
      if not CallBackFunc(TimeZoneRegInfo) then
        Break;
    end;
  finally
    TimeZones.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------
// Translates dates like "the third thursday in March" into the actual date

function CalculateTransitionDate(TransitionInfo: TSystemTime; CalculateForYear: Integer= -1): TDateTime;
var
  CheckDate, FirstOfMonth: TDateTime;
  MonthStartDOW: Integer;
  TransitionDOW: Word;
  TransitionDay: Word;
  PartialDays: Integer;
  I, MonthDays: Integer;
  Year, Month, Day: Word;
  ActiveYear: Integer;
begin
  if CalculateForYear= -1 then
    ActiveYear:=YearOfDate(Now)
  else
    ActiveYear:=CalculateForYear;

  // If the date is empty, we've been passed a system date for a timezone that has no daylight support
  if (TransitionInfo.wYear = 0) and (TransitionInfo.wMonth = 0) and (TransitionInfo.wDay = 0) then
  begin
    Result := 0;
    Exit;
  end;

  // Work out what day the first of the month is
  FirstOfMonth := EncodeDate(ActiveYear, TransitionInfo.wMonth, 1);

  // System time is 0 based, Day of Week is 1 based
  MonthStartDOW := DayOfWeek(FirstOfMonth);
  TransitionDOW := TransitionInfo.wDayOfWeek + 1;

  if MonthStartDOW > TransitionDOW then
    PartialDays := 7 - (MonthStartDOW - TransitionDOW) + 1
  else
    PartialDays := (TransitionDOW - MonthStartDOW) + 1;

  // According to MSDN, wDay member is a value in the range 1 through 5.
  // Using this notation, the first Sunday in April can be specified,
  // as can the last Thursday in October (5 is equal to "the last").
  TransitionDay := 1;
  case TransitionInfo.wDay of
    1:
      TransitionDay := PartialDays;
    2, 3, 4:
      TransitionDay := (7 * (TransitionInfo.wDay - 1)) + PartialDays;
    5:
      begin
        // Work out the date of the last X day in the month
        MonthDays := DaysInMonth(FirstOfMonth);
        DecodeDate(FirstOfMonth, Year, Month, Day);

        for I := MonthDays downto MonthDays - 7 do
        begin
          CheckDate := EncodeDate(Year, Month, I);
          if DayOfWeek(CheckDate) = TransitionDOW then
          begin
            TransitionDay := I;
            Break;
          end;
        end;
      end;
  end;

  Result := EncodeDate(ActiveYear, TransitionInfo.wMonth, TransitionDay);
end;

//--------------------------------------------------------------------------------------------------
//Returns the date range in friendly format "From the 1st sunday in March to the last Sunday in October"

function GetDayLightSavingsPeriod(StandardDate, DayLightDate: TSystemTime): string;
begin
  if (DaylightDate.wMonth = 0) and (StandardDate.wMonth = 0) then
    Result := '' // There is no daylight savings period for this timezone
  else
  begin
    if (DaylightDate.wMonth = StandardDate.wMonth) and (DaylightDate.wDay = StandardDate.wDay) then
      Result := '' // AutoAdjust for Daylight savings is not enabled, so Windows returns the entire year
    else
    begin
      Result := 'From the ' + UTCDaysOffset[DaylightDate.wDay] + ' ' + UTCDays[DaylightDate.wDayOfWeek] + ' of ' +
        UTCMonths[DaylightDate.wMonth] +
        ' to the ' + UTCDaysOffset[StandardDate.wDay] + ' ' + UTCDays[StandardDate.wDayOfWeek] + ' of ' +
          UTCMonths[StandardDate.wMonth];
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------
// Functions relating to the current time zone
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
// Returns true if the current timezone supports daylight savings - regardless of whether
// AutoAdjust is enabled or not

function CurrentTimeZoneSupportsDaylightSavings: Boolean;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  Result := False;

  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := TimeZoneInfo.StandardDate.wMonth <> 0;
    TIME_ZONE_ID_DAYLIGHT:
      Result := True;
  end;
end;

//-----------------------------------------------------------------------------
// Returns the exact date that clocks switch to daylight savings in the current
// year for the current time zone

function DateCurrentTimeZoneClocksChangeToDaylightSavings: TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
   // The daylight savings time is only returned if AutoAdjust is enabled and the
   // timezone supports Daylight savings.
  if not CurrentTimeZoneSupportsDaylightSavings then
    raise EDaylightSavingsNotSupported.Create(RsEDaylightSavingsNotSupported);

  if not IsAutoAdjustEnabled then
    raise EAutoAdjustNotEnabled.Create(RsEAutoAdjustNotEnabled);

  Result := 0;
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) in [TIME_ZONE_ID_DAYLIGHT, TIME_ZONE_ID_STANDARD] then
    Result := CalculateTransitionDate(TimeZoneInfo.DaylightDate);
end;

//-----------------------------------------------------------------------------
// Returns the exact date that clocks switch to standard time in the current year
// for the current time zone

function DateCurrentTimeZoneClocksChangeToStandard: TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  if not CurrentTimeZoneSupportsDaylightSavings then
    raise EDaylightSavingsNotSupported.Create(RsEDaylightSavingsNotSupported);

  Result := 0;
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) in [TIME_ZONE_ID_DAYLIGHT, TIME_ZONE_ID_STANDARD] then
    Result := CalculateTransitionDate(TimeZoneInfo.StandardDate);
end;

//----------------------------------------------------------------------------
// Returns the name of the current time zone

function GetCurrentTimeZoneDescription: string;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_INVALID:
      Result := '';
    TIME_ZONE_ID_STANDARD:
      Result := TimeZoneInfo.StandardName;
    TIME_ZONE_ID_UNKNOWN:
      Result := TimeZoneInfo.StandardName;
    TIME_ZONE_ID_DAYLIGHT:
      Result := TimeZoneInfo.DaylightName;
  end;
end;

//-----------------------------------------------------------------------------
// Returns the date range in friendly format for the current timezone

function GetCurrentTimeZoneDaylightSavingsPeriod: string;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  Result := '';
  if not IsAutoAdjustEnabled then
    Exit;

  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_INVALID then
    Exit;

  Result := GetDaylightSavingsPeriod(TimeZoneInfo.StandardDate, TimeZoneInfo.DaylightDate);
end;

//-----------------------------------------------------------------------------
// Returns the current offset from GMT as a string "GMT+03:00"

function GetCurrentTimeZoneGMTOffset: string;
var
  Bias: Integer;
  Hours, Minutes: Integer;
begin
  Bias := GetCurrentTimeZoneUTCBias;

  Hours := Bias div 60;
  Minutes := Abs(Bias) mod 60;
  if Bias >= 0 then
    Result := Format('GMT+%.2d:%.2d', [Hours, Minutes])
  else
    // (rom) not GMT- here?
    Result := Format('GMT%.2d:%.2d', [Hours, Minutes]);
end;

//-----------------------------------------------------------------------------
// Returns the Current Time Zone UTC Bias from GMT in minutes

function GetCurrentTimeZoneUTCBias: Integer;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  Result := 0;
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := -(TimeZoneInfo.Bias + TimeZoneInfo.StandardBias);
    TIME_ZONE_ID_UNKNOWN:
      Result := -(TimeZoneInfo.Bias + TimeZoneInfo.StandardBias);
    TIME_ZONE_ID_DAYLIGHT:
      Result := -(TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias);
  end;
end;

//-----------------------------------------------------------------------------
// Miscellaneous UTC functions
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Returns the current UTC time

function UTCNow: TDateTime;
var
  SystemTime: TSystemTime;
begin
  GetSystemTime(SystemTime);
  Result := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay) +
    EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
end;

//-----------------------------------------------------------------------------
// Returns Time in the UTC string format expected by root\cimv2\Win32_ScheduledJob
// TODO: This works for scheduling local jobs, but fails on remote jobs - looks
// like Windows does something a bit odd with remote scheduled jobs, it's not
// handling the time correctly at all.  So much for UTC.

function GetWMIScheduledJobUTCTime(Time: TDateTime): string;
var
  TimeBias: Integer;
  Hour, Min, Sec, MSec: Word;
begin
  Result := '';

  TimeBias := GetCurrentTimeZoneUTCBias;

  DecodeTime(Time, Hour, Min, Sec, MSec);

  Result := '********' + Format('%.2d%.2d%.2d.000000', [Hour, Min, Sec]);

  if TimeBias >= 0 then
    Result := Result + Format('+%.3d', [TimeBias])
  else
    Result := Result + Format('%.3d', [TimeBias]);
end;

//-----------------------------------------------------------------------------
// Returns true if "Automatically Adjust clock for daylight saving changes" is checked

function IsAutoAdjustEnabled: Boolean;
begin
  Result := RegReadIntegerDef(HKEY_LOCAL_MACHINE, cAutoAdjustKey, cAutoAdjustValue, 0) = 0;
end;

//=== { TJclTimeZoneInfo } ====================================================

procedure TJclTimeZoneInfo.ApplyTimeZone;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  TimeZoneInfo.Bias := FBiasInfo.Bias;

  StringToWideChar(FStandardName, TimeZoneInfo.StandardName, 32);
  TimeZoneInfo.StandardDate := FBiasInfo.StandardDate;
  TimeZoneInfo.StandardBias := FBiasInfo.StandardBias;
  StringToWideChar(FDaylightName, TimeZoneInfo.DaylightName, 32);
  TimeZoneInfo.DaylightDate := FBiasInfo.DaylightDate;
  TimeZoneInfo.DaylightBias := FBiasInfo.DaylightBias;

  if not SetTimeZoneInformation({$IFDEF FPC}@{$ENDIF FPC}TimeZoneInfo) then
    RaiseLastOSError;

  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, 0);
end;

procedure TJclTimeZoneInfo.Assign(Source: TJclTimeZoneRegInfo);
begin
  FStandardName := Source.StandardName;
  FDaylightName := Source.DaylightName;
  FTZDescription := Source.DisplayDesc;
  FSortIndex := Source.SortIndex;
  FMapID := Source.MapID;
  FBiasInfo := Source.TZI;
end;

function TJclTimeZoneInfo.GetActiveBias: Integer;
begin
  // Return the active bias (including any skew derived from auto-adjust being enabled)
  Result := 0;

  case GetTimeZoneType(FBiasInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := -(FBiasInfo.Bias + FBiasInfo.StandardBias);
    TIME_ZONE_ID_UNKNOWN:
      Result := -(FBiasInfo.Bias + FBiasInfo.StandardBias);
    TIME_ZONE_ID_DAYLIGHT:
      // the bias is different if autoadjust is turned off
      if IsAutoAdjustEnabled then
        Result := -(FBiasInfo.Bias + FBiasInfo.DaylightBias)
      else
        Result := -(FBiasInfo.Bias + FBiasInfo.StandardBias);
  end;
end;

//-----------------------------------------------------------------------------
// Returns the exact date that clocks switch to daylight savings in the current
// year for the specified standard date

function TJclTimeZoneInfo.DayLightSavingsPeriod: string;
begin
  Result := GetDayLightSavingsPeriod(FBiasInfo.StandardDate, FBiasInfo.DayLightDate);
end;

function TJclTimeZoneInfo.GetDaylightSavingsStartDate: TDateTime;
begin
  Result := CalculateTransitionDate(FBiasInfo.DaylightDate);
end;

//-----------------------------------------------------------------------------
// Returns the offset from GMT as a string "GMT+03:00"

function TJclTimeZoneInfo.GetGMTOffset: string;
var
  Hours, Minutes: Integer;
begin
  Hours := -FBiasInfo.Bias div 60;
  Minutes := Abs(FBiasInfo.Bias) mod 60;
  if Hours >= 0 then
    Result := Format('GMT+%.2d:%.2d', [Hours, Minutes])
  else
    // (rom) No GMT- here?
    Result := Format('GMT%.2d:%.2d', [Hours, Minutes]);
end;

function TJclTimeZoneInfo.GetStandardStartDate: TDateTime;
begin
  Result := CalculateTransitionDate(FBiasInfo.StandardDate);
end;

function TJclTimeZoneInfo.GetSupportsDaylightSavings: Boolean;
begin
  Result := False;

  // TODO: Check this is correct on 9x
  case GetTimeZoneType(FBiasInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := FBiasInfo.StandardDate.wMonth <> 0;
    TIME_ZONE_ID_DAYLIGHT:
      Result := True;
  end;
end;

function TJclTimeZoneInfo.GetCurrentDateTime: TDateTime;
begin
  // Return the current date time in this time zone
  Result := UTCNow + (GetActiveBias / 1440);
end;

// Returns the TimeZone type based on StandardDate and DaylightDate

function TJclTimeZoneInfo.GetTimeZoneType(TZI: TJclTZIValueInfo): Cardinal;
var
  StandardDate: TDateTime;
  DaylightSavingsDate: TDateTime;
begin
  StandardDate := CalculateTransitionDate(TZI.StandardDate);
  DaylightSavingsDate := CalculateTransitionDate(TZI.DaylightDate);

  if (StandardDate = 0) and (DayLightSavingsDate = 0) then
    Result := TIME_ZONE_ID_UNKNOWN
  else
  begin
    // In places like Australia, Daylight savings is after Standard, in the UK it's the other way round
    if DayLightSavingsDate > StandardDate then
    begin
      if (Now >= StandardDate) and (Now < DaylightSavingsDate) then
        Result := TIME_ZONE_ID_STANDARD
      else
        Result := TIME_ZONE_ID_DAYLIGHT;
    end
    else
    begin
      if (Now >= DaylightSavingsDate) and (Now < StandardDate) then
        Result := TIME_ZONE_ID_DAYLIGHT
      else
        Result := TIME_ZONE_ID_STANDARD;
    end;
  end;
end;

//=== { TJclTimeZones } =======================================================

function TimeZoneSort(Item1, Item2: Pointer): Integer;
begin
  Result := TJclTimeZoneInfo(Item1).SortIndex - TJclTimeZoneInfo(Item2).SortIndex;
  if (Result = 0) or (TJclTimeZoneInfo(Item1).SortIndex = -1) or (TJclTimeZoneInfo(Item2).SortIndex = -1) then
    Result := CompareText(TJclTimeZoneInfo(Item1).DisplayDescription, TJclTimeZoneInfo(Item2).DisplayDescription);
end;

constructor TJclTimeZones.Create;
begin
  inherited Create;
  FAutoAdjustEnabled := GetAutoAdjustEnabled;
  FTimeZones := TObjectList.Create(True);
  LoadTimeZones;
end;

destructor TJclTimeZones.Destroy;
begin
  FreeAndNil(FTimeZones);
  inherited Destroy;
end;

function TJclTimeZones.GetAutoAdjustEnabled: Boolean;
begin
  Result := IsAutoAdjustEnabled;
end;

function TJclTimeZones.GetActiveTimeZoneInfo: TJclTimeZoneInfo;
begin
  Result := GetItem(FActiveTimeZoneIndex);
end;

function TJclTimeZones.GetCount: Integer;
begin
  Result := FTimeZones.Count;
end;

function TJclTimeZones.GetItem(Index: Integer): TJclTimeZoneInfo;
begin
  if (Index >= 0) or (Index < FTimeZones.Count) then
    Result := TJclTimeZoneInfo(FTimeZones[Index])
  else
    Result := nil;
end;

procedure TJclTimeZones.LoadTimeZones;
var
  CurrentTimeZoneDesc: string;
  I: Integer;
begin
  EnumTimeZones(TimeZoneCallback);
  FTimeZones.Sort(@TimeZoneSort);

  CurrentTimeZoneDesc := GetCurrentTimeZoneDescription;

  FActiveTimeZoneIndex := -1;
  for I := 0 to FTimeZones.Count - 1 do
    if (TJclTimeZoneInfo(FTimeZones[I]).StandardName = CurrentTimeZoneDesc) or
      (TJclTimeZoneInfo(FTimeZones[I]).DayLightName = CurrentTimeZoneDesc) then
    begin
      FActiveTimeZoneIndex := I;
      Break;
    end;
end;

procedure TJclTimeZones.SetAutoAdjustEnabled(Value: Boolean);
begin
  // TODO: PC isn't being notified correctly of the change
  if FAutoAdjustEnabled <> Value then
  begin
    if not Value then
      RegWriteInteger(HKEY_LOCAL_MACHINE, cAutoAdjustKey, cAutoAdjustValue, 1)
    else
      RegDeleteEntry(HKEY_LOCAL_MACHINE, cAutoAdjustKey, cAutoAdjustValue);

    FAutoAdjustEnabled := Value;
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar('intl')));
    SendMessage(HWND_TOPMOST, WM_TIMECHANGE, 0, 0);
  end;
end;

function TJclTimeZones.TimeZoneCallback(const TimeZoneRec: TJclTimeZoneRegInfo): Boolean;
var
  TimeZone: TJclTimeZoneInfo;
begin
  Result := True;
  TimeZone := TJclTimeZoneInfo.Create;
  TimeZone.Assign(TimeZoneRec);
  FTimeZones.Add(TimeZone)
end;

function TJclTimeZones.SetDateTime(DateTime: TDateTime): Boolean;
var
  SystemTime: TSystemTime;
begin
  // The date time we have is local.  Convert it to UTC
  DateTime := LocalDateTimeToDateTime(DateTime);

  DecodeDate(DateTime, SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay);
  DecodeTime(DateTime, SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliSeconds);

  Result := SetSystemTime(SystemTime);

  if not Result then
    RaiseLastOSError;

  SendMessage(HWND_TOPMOST, WM_TIMECHANGE, 0, 0);
end;

function TJclTimeZoneInfo.DateTimeIsInDaylightSavings(ADateTime: TDateTime): Boolean;
var
  dsStartDate: TDateTime;
  stdStartDate: TDateTime;
  Year: Integer;
begin
  //Return whether the specified date time is DaylightSavings or not
  Result:=False;

  if not SupportsDaylightSavings then
    Exit;

  Year:=YearOfDate(ADateTime);
  dsStartDate := CalculateTransitionDate(FBiasInfo.DaylightDate, Year);
  stdStartDate:= CalculateTransitionDate(FBiasInfo.StandardDate, Year);

  // In places like Australia, Daylight savings is after Standard, in the UK it's the other way round
  if dsStartDate > stdStartDate then
  begin
    if (ADateTime >= stdStartDate) and (ADateTime < dsStartDate) then
      Result := False
    else
      Result := True;
  end
  else
  begin
    if (ADateTime >= dsStartDate) and (ADateTime < stdStartDate) then
      Result := True
    else
      Result := False;
  end;
end;

function TJclTimeZoneInfo.DaylightStartDateInYear(
  const AYear: Integer): TDateTime;
begin
  Result:= CalculateTransitionDate(FBiasInfo.DaylightDate, AYear);
end;

function TJclTimeZoneInfo.StandardStartDateInYear(
  const AYear: Integer): TDateTime;
begin
  Result:= CalculateTransitionDate(FBiasInfo.StandardDate, AYear);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

