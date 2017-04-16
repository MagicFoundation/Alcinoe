{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      Alcinoe Common functions
Version:      4.00

Description:  Alcinoe Common Functions

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     09/01/2005: correct then AlEmptyDirectory function
              25/05/2006: Move some function to AlFcnFile
              25/02/2008: Update AlIsValidEmail
              06/10/3008: Update AlIsValidEmail
              03/03/2010: add ALIsInt64
              26/06/2012: Add xe2 support
Link :

**************************************************************}
unit ALCommon;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Type TalLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
type TALCustomDelayedFreeObjectProc = procedure(var aObject: Tobject) of object;
var ALCustomDelayedFreeObjectProc: TALCustomDelayedFreeObjectProc;
{$IFDEF DEBUG}
var ALFreeAndNilRefCountWarn: boolean;
threadvar ALCurThreadFreeAndNilNORefCountWarn: boolean;
{$ENDIF}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean = false); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean); overload; {$IFNDEF DEBUG}inline;{$ENDIF}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Function AlBoolToInt(Value:Boolean):Integer;
Function AlIntToBool(Value:integer):boolean;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
function AlLocalDateTimeToUTCDateTime(Const aLocalDateTime: TDateTime): TdateTime;
function AlUTCDateTimeToLocalDateTime(Const aUTCDateTime: TDateTime): TdateTime;
function ALUTCNow: TDateTime;
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
Function ALInc(var x: integer; Count: integer): Integer;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
const ALMAXUInt64: UInt64 = 18446744073709551615;
      ALMAXInt64: Int64 = 9223372036854775807;
      ALMAXUInt: cardinal = 4294967295;
      ALMAXInt: int32 = 2147483647;
      ALNullDate = -0.5; // There are no TDateTime values from –1 through 0
                         // dt := -0.5;
                         // writeln(formatFloat('0.0', dt));                    => -0.5
                         // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000
                         //
                         // dt := encodedatetime(1899,12,30,12,00,00,000);
                         // writeln(formatFloat('0.0', dt));                    => 0.5
                         // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000

implementation

uses system.Classes,
     {$IFDEF MSWINDOWS}
     Winapi.Windows,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.JavaTypes,
     Androidapi.Helpers,
     ALAndroidApi,
     {$IFEND}
     {$IF defined(IOS)}
     iOSapi.Foundation,
     Macapi.Helpers,
     {$IFEND}
     system.sysutils,
     system.DateUtils;

{***********************************************************************************************}
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);
{$IF defined(IOS) or defined(MSWINDOWS)}
var aMsg: String;
{$IFEND}
begin
  {$IF defined(ANDROID)}
  case _type of
    TalLogType.VERBOSE: TJLog.JavaClass.v(StringToJString(Tag), StringToJString(msg));
    TalLogType.DEBUG: TJLog.JavaClass.d(StringToJString(Tag), StringToJString(msg));
    TalLogType.INFO: TJLog.JavaClass.i(StringToJString(Tag), StringToJString(msg));
    TalLogType.WARN: TJLog.JavaClass.w(StringToJString(Tag), StringToJString(msg));
    TalLogType.ERROR: TJLog.JavaClass.e(StringToJString(Tag), StringToJString(msg));
    TalLogType.ASSERT: TJLog.JavaClass.wtf(StringToJString(Tag), StringToJString(msg)); // << wtf for What a Terrible Failure but everyone know that it's for what the fuck !
  end;
  {$ELSEIF defined(IOS)}
  // https://forums.developer.apple.com/thread/4685
  //if _type <> TalLogType.VERBOSE  then begin // because log on ios slow down the app so skip verbosity
    if msg <> '' then aMsg := ' => ' + msg
    else aMsg := '';
    case _type of
      TalLogType.VERBOSE: NSLog(StringToID('[V] ' + Tag + aMsg));
      TalLogType.DEBUG:   NSLog(StringToID('[D][V] ' + Tag + aMsg));
      TalLogType.INFO:    NSLog(StringToID('[I][D][V] ' + Tag + aMsg));
      TalLogType.WARN:    NSLog(StringToID('[W][I][D][V] ' + Tag + aMsg));
      TalLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V] ' + Tag + aMsg));
      TalLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V] ' + Tag + aMsg));
    end;
  //end;
  {$ELSEIF defined(MSWINDOWS)}
  if _type <> TalLogType.VERBOSE  then begin // because log on windows slow down the app so skip verbosity
    if msg <> '' then aMsg := ' => ' + stringReplace(msg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
    else aMsg := '';
    case _type of
      TalLogType.VERBOSE: OutputDebugString(pointer('[V] ' + Tag + aMsg + ' |'));
      TalLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + Tag + aMsg + ' |'));
      TalLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + Tag + aMsg + ' |'));
    end;
  end;
  {$IFEND}
end;

{******************************************}
Function AlBoolToInt(Value:Boolean):Integer;
Begin
  If Value then result := 1
  else result := 0;
end;

{******************************************}
Function AlIntToBool(Value:integer):boolean;
begin
  result := Value <> 0;
end;

{***************************************************************}
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
Begin
  result := (LTotal - (LBorder*2) - LObject) div 2 + LBorder;
End;

{********************************************************************************}
function AlLocalDateTimeToUTCDateTime(Const aLocalDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(aLocalDateTime);
end;

{******************************************************************************}
function AlUTCDateTimeToLocalDateTime(Const aUTCDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToLocalTime(aUTCDateTime);
end;

{*************************}
{The same like Now but used
 UTC-time not local time.}
function ALUTCNow: TDateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(NOW);
end;

{******************************************************}
Function ALInc(var x: integer; Count: integer): Integer;
begin
  inc(X, count);
  result := X;
end;

{*******************************************************}
{Accepts number of milliseconds in the parameter aValue,
 provides 1000 times more precise value of TDateTime}
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
begin
  Result := IncMilliSecond(UnixDateDelta, aValue);
end;

{********************************************************}
{Returns UNIX-time as the count of milliseconds since the
 UNIX epoch. Can be very useful for the purposes of
 special precision.}
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
begin
  result := MilliSecondsBetween(UnixDateDelta, aValue);
  if aValue < UnixDateDelta then result := -result;
end;

{***************************************************************}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean = false);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  if temp = nil then exit;
  TObject(Obj) := nil;
  if adelayed and assigned(ALCustomDelayedFreeObjectProc) then ALCustomDelayedFreeObjectProc(Temp)
  else begin
    {$IF defined(AUTOREFCOUNT)}
    if AtomicCmpExchange(temp.refcount{Target}, 0{NewValue}, 0{Compareand}) = 1 then begin // it's seam it's not an atomic operation (http://stackoverflow.com/questions/39987850/is-reading-writing-an-integer-4-bytes-atomic-on-ios-android-like-on-win32-win6)
      temp.Free;
      temp := nil;
    end
    else begin
      Temp.DisposeOf; // TComponent Free Notification mechanism notifies registered components that particular
                      // component instance is being freed. Notified components can handle that notification inside
                      // virtual Notification method and make sure that they clear all references they may hold on
                      // component being destroyed.
                      //
                      // Free Notification mechanism is being triggered in TComponent destructor and without DisposeOf
                      // and direct execution of destructor, two components could hold strong references to each
                      // other keeping themselves alive during whole application lifetime.
      {$IF defined(DEBUG)}
      if ALFreeAndNilRefCountWarn and
        (not ALCurThreadFreeAndNilNORefCountWarn) then begin
        if (Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag}) <> 0 then
          ALLog('ALFreeAndNil', Temp.ClassName + ' | Refcount is not null (' + Inttostr((Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag})) + ')', TalLogType.warn);
      end;
      {$IFEND}
      temp := nil;
    end;
    {$ELSE}
    temp.Free;
    temp := nil;
    {$IFEND}
  end;
end;

{*************************************************************************************}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean);
{$IFDEF DEBUG}
var aOldCurThreadFreeAndNilNoRefCountWarn: boolean;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aOldCurThreadFreeAndNilNoRefCountWarn := ALCurThreadFreeAndNilNORefCountWarn;
  ALCurThreadFreeAndNilNORefCountWarn := not aRefCountWarn;
  try
  {$ENDIF}

    ALFreeAndNil(Obj, adelayed);

  {$IFDEF DEBUG}
  finally
    ALCurThreadFreeAndNilNORefCountWarn := aOldCurThreadFreeAndNilNORefCountWarn;
  end;
  {$ENDIF}
end;

initialization
  ALCustomDelayedFreeObjectProc := nil;
  {$IFDEF DEBUG}
  ALFreeAndNilRefCountWarn := False;
  {$ENDIF}

end.
