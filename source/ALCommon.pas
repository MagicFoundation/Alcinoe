unit ALCommon;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

{$IFDEF IOS}
uses iOSapi.Foundation;
{$ENDIF}

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
{$IFDEF IOS}
function ALNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
{$ENDIF}
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
                         //
                         // also -0.5 have the advantage to be in the form
                         // m*2^e (-1*2^-1) with mean we don't need to use
                         // samevalue to compare
                         // https://stackoverflow.com/questions/41779801/single-double-and-precision

{$IFNDEF NEXTGEN}

type
  /// the potential features, retrieved from an Intel CPU
  // - see https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  TALIntelCpuFeature =
   ( { in EDX }
   cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA64, cfPBE,
   { in ECX }
   cfSSE3, cfCLMUL, cfDS64, cfMON, cfDSCPL, cfVMX, cfSMX, cfEST,
   cfTM2, cfSSSE3, cfCID, cfSDBG, cfFMA, cfCX16, cfXTPR, cfPDCM,
   cf_c16, cfPCID, cfDCA, cfSSE41, cfSSE42, cfX2A, cfMOVBE, cfPOPCNT,
   cfTSC2, cfAESNI, cfXS, cfOSXS, cfAVX, cfF16C, cfRAND, cfHYP,
   { extended features in EBX, ECX }
   cfFSGS, cf_b01, cfSGX, cfBMI1, cfHLE, cfAVX2, cf_b06, cfSMEP, cfBMI2,
   cfERMS, cfINVPCID, cfRTM, cfPQM, cf_b13, cfMPX, cfPQE, cfAVX512F,
   cfAVX512DQ, cfRDSEED, cfADX, cfSMAP, cfAVX512IFMA, cfPCOMMIT,
   cfCLFLUSH, cfCLWB, cfIPT, cfAVX512PF, cfAVX512ER, cfAVX512CD,
   cfSHA, cfAVX512BW, cfAVX512VL, cfPREFW1, cfAVX512VBMI);

  /// all features, as retrieved from an Intel CPU
  TALIntelCpuFeatures = set of TALIntelCpuFeature;

var
  /// the available CPU features, as recognized at program startup
  ALCpuFeatures: TALIntelCpuFeatures;

{$ENDIF}

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

{**********}
{$IFDEF IOS}
function ALNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
begin
  if ADateTime <> nil then
    Result := ADateTime.TimeIntervalSince1970 / SecsPerDay + EncodeDate(1970, 1, 1)
  else
    Result := 0.0;
end;
{$ENDIF}

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


//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//

{$IF CompilerVersion > 32} // tokyo
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
{$IFEND}

{$IFNDEF NEXTGEN}

{**}
type
 TRegisters = record
   eax,ebx,ecx,edx: cardinal;
 end;

{*************************************************************}
procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
{$IF defined(CPU64BITS)}
asm // ecx=param, rdx=Registers (Linux: edi,rsi)
        .NOFRAME
        mov     eax, ecx
        mov     r9, rdx
        mov     r10, rbx // preserve rbx
        xor     ebx, ebx
        xor     ecx, ecx
        xor     edx, edx
        cpuid
        mov     TRegisters(r9).&eax, eax
        mov     TRegisters(r9).&ebx, ebx
        mov     TRegisters(r9).&ecx, ecx
        mov     TRegisters(r9).&edx, edx
        mov     rbx, r10
end;
{$else}
asm
        push    esi
        push    edi
        mov     esi, edx
        mov     edi, eax
        pushfd
        pop     eax
        mov     edx, eax
        xor     eax, $200000
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax, edx
        jz      @nocpuid
        push    ebx
        mov     eax, edi
        xor     ecx, ecx
        cpuid
        mov     TRegisters(esi).&eax, eax
        mov     TRegisters(esi).&ebx, ebx
        mov     TRegisters(esi).&ecx, ecx
        mov     TRegisters(esi).&edx, edx
        pop     ebx
@nocpuid:
        pop     edi
        pop     esi
end;
{$ifend}

{*****************************}
procedure TestIntelCpuFeatures;
var regs: TRegisters;
begin
  regs.edx := 0;
  regs.ecx := 0;
  GetCPUID(1,regs);
  PIntegerArray(@ALCpuFeatures)^[0] := regs.edx;
  PIntegerArray(@ALCpuFeatures)^[1] := regs.ecx;
  GetCPUID(7,regs);
  PIntegerArray(@ALCpuFeatures)^[2] := regs.ebx;
  PIntegerArray(@ALCpuFeatures)^[3] := regs.ecx;
  PByte(@PIntegerArray(@ALCpuFeatures)^[4])^ := regs.edx;
end;

{$ENDIF}

initialization

  {$IFNDEF NEXTGEN}
  TestIntelCpuFeatures;
  {$ENDIF}

  ALCustomDelayedFreeObjectProc := nil;
  {$IFDEF DEBUG}
  ALFreeAndNilRefCountWarn := False;
  {$ENDIF}

end.
