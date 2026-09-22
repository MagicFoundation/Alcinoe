unit Alcinoe.CodeProfiler;

interface

{$I Alcinoe.inc}
{$I Alcinoe.CodeProfiler.inc}

{$IF defined(ALCodeProfilerIgnoreThreadID) and (not defined(ALCodeProfilerHistoryGroupByProcID))}
  {$MESSAGE ERROR 'ALCodeProfilerIgnoreThreadID is only available with ALCodeProfilerHistoryGroupByProcID'}
{$ENDIF}

type
  TALProcMetrics = record
  public
    {$IF defined(ALCodeProfilerHistoryGroupNone)}
    ExecutionID: Cardinal;
    ParentExecutionID: Cardinal;
    ProcID: Cardinal;
    ThreadID: Cardinal;
    StartTimeStamp: Int64;
    ElapsedTicks: Int64;
    {$ELSEIF defined(ALCodeProfilerHistoryGroupByProcID)}
    ProcID: Cardinal;
    // Always 0 when ALCodeProfilerIgnoreThreadID is defined. The field is kept
    // in all cases so that the layout of the .dat file never changes.
    ThreadID: Cardinal;
    CallCount: Cardinal;
    ElapsedTicks: Int64;
    {$ELSEIF defined(ALCodeProfilerHistoryGroupByCallStack)}
    HashCode: Integer;
    ProcID: Cardinal;
    MetricsID: Cardinal;
    ParentMetricsID: Cardinal;
    ThreadID: Cardinal;
    CallCount: Cardinal;
    ElapsedTicks: Int64;
    {$ENDIF}
  end;
  PALProcMetrics = ^TALProcMetrics;

procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);
procedure ALCodeProfilerStart;
procedure ALCodeProfilerStop;
function ALCodeProfilerIsrunning: Boolean;

const
  ALCodeProfilerProcIDMapFilename = 'ALCodeProfilerProcIDMap.txt';
  {$IF defined(ALCodeProfilerHistoryGroupNone)}
  ALCodeProfilerProcMetricsFilename: String = 'ALCodeProfilerProcMetrics.None.dat';
  {$ELSEIF defined(ALCodeProfilerHistoryGroupByProcID)}
  ALCodeProfilerProcMetricsFilename: String = 'ALCodeProfilerProcMetrics.ByProcID.dat';
  {$ELSEIF defined(ALCodeProfilerHistoryGroupByCallStack)}
  ALCodeProfilerProcMetricsFilename: String = 'ALCodeProfilerProcMetrics.ByCallStack.dat';
  {$ENDIF}
  ALCodeProfilerRegistryPath = 'Software\MagicFoundation\Alcinoe\CodeProfiler';
  ALCodeProfilerDataStoragePathKey = 'DataStoragePath';
  ALCodeProfilerMillisecondsPerTick = 0.0001;

var
  ALCodeProfilerAppStartTimeStamp: Int64;


implementation

uses
  {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
  System.Hash,
  {$ENDIF}
  {$IF defined(MSWindows)}
  System.Win.Registry,
  Winapi.Windows,
  {$ENDIF}
  {$IF defined(IOS) or defined(ANDROID)}
  System.Messaging,
  FMX.Platform,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.Helpers,
  Androidapi.jni.JavaTypes,
  Androidapi.JNI.Util,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.Foundation,
  Macapi.Helpers,
  {$ENDIF}
  System.Net.URLClient,
  System.net.HttpClientComponent,
  System.SyncObjs,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Diagnostics,
  System.IOUtils;

{**}
Type
  TALStopWatchProcMetrics = record
  private
    {$IF defined(ALCodeProfilerHistoryGroupNone)}
    class var ExecutionIDSequence: cardinal;
    {$ENDIF}
  public
    {$IF defined(ALCodeProfilerHistoryGroupNone)}
    ExecutionID: Cardinal;
    ParentExecutionID: Cardinal;
    {$ENDIF}
    ProcID: Cardinal;
    {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
    ParentMetricsID: Cardinal;
    {$ENDIF}
    {$IF not defined(ALCodeProfilerIgnoreThreadID)}
    ThreadID: Cardinal;
    {$ENDIF}
    StopWatch: TStopWatch;
  end;

  TALStopWatchProcMetricsArray = array of TALStopWatchProcMetrics;
  TALProcMetricsStack = class(TObject)
  protected
    FArray: TALStopWatchProcMetricsArray;
    FCount: NativeInt;
    FCapacity: NativeInt;
    procedure Grow;
    procedure SetCapacity(NewCapacity: NativeInt);
  end;

  TALProcMetricsArray = array of TALProcMetrics;
  TALProcMetricsHistory = class(TObject)
  protected
    FArray: TALProcMetricsArray;
    FCount: NativeInt;
    FCapacity: NativeInt;
    {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
    FGrowThreshold: NativeInt;
    procedure Rehash(NewCapPow2: NativeInt);
    function GetBucketIndex(const AProcID, AParentMetricsID: Cardinal; const AHashCode: Integer): NativeInt;
    function Hash(const AProcID, AParentMetricsID: Cardinal): Integer;
    {$ENDIF}
    procedure Grow;
    procedure SetCapacity(NewCapacity: NativeInt);
    procedure Clear;
  end;

{*******}
threadvar
  ALProcMetricsStack: TALProcMetricsStack;

{*******}
{$IF defined(ALCodeProfilerIgnoreThreadID)}
// All the threads share the same history, so that the metrics of a procedure
// are merged together whatever the thread it was called from.
var
  ALProcMetricsHistory: TALProcMetricsHistory;
{$ELSE}
threadvar
  ALProcMetricsHistory: TALProcMetricsHistory;
{$ENDIF}

{*}
var
  ALProcMetricsHistories: TList<TALProcMetricsHistory>;
  ALProcMetricsLock: TLightweightMREW;
  ALProcMetricsFilename: String;
  {$IF defined(IOS) or defined(ANDROID)}
  ALCodeProfilerAppActivatedBefore: Boolean;
  {$ENDIF}

{**}
Type
  TALCodeProfilerLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);

{**************************************************}
{$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
const
  EMPTY_HASH = -1;
{$ENDIF}

{**************************}
procedure ALCodeProfilerLog(
            Const Tag: String;
            Const msg: String;
            Const &Type: TALCodeProfilerLogType);
begin
  var LMsg: String := msg;
  {$IF defined(ANDROID)}
  if LMsg = '' then LMsg := '<empty>';
  if TThread.Current.ThreadID <> MainThreadID then LMsg := '['+IntToStr(TThread.Current.ThreadID)+'] ' + LMsg;
  case &Type of
    TALCodeProfilerLogType.VERBOSE: TJutil_Log.JavaClass.v(StringToJString(Tag), StringToJString(LMsg));
    TALCodeProfilerLogType.DEBUG: TJutil_Log.JavaClass.d(StringToJString(Tag), StringToJString(LMsg));
    TALCodeProfilerLogType.INFO: TJutil_Log.JavaClass.i(StringToJString(Tag), StringToJString(LMsg));
    TALCodeProfilerLogType.WARN: TJutil_Log.JavaClass.w(StringToJString(Tag), StringToJString(LMsg));
    TALCodeProfilerLogType.ERROR: TJutil_Log.JavaClass.e(StringToJString(Tag), StringToJString(LMsg));
    TALCodeProfilerLogType.ASSERT: TJutil_Log.JavaClass.wtf(StringToJString(Tag), StringToJString(LMsg)); // << wtf for What a Terrible Failure but everyone know that it's for what the fuck !
  end;
  {$ELSEIF defined(IOS)}
  if LMsg <> '' then LMsg := Tag + ' | ' + LMsg
  else LMsg := Tag;
  var LThreadID: String;
  if TThread.Current.ThreadID <> MainThreadID then LThreadID := '['+IntToStr(TThread.Current.ThreadID)+']'
  else LThreadID := '';
  //On iOS NSLog is limited to 1024 Bytes so if the
  //message is > 1024 bytes split it
  var P: integer := 1;
  while P <= length(LMsg) do begin
    var LMsgPart := Copy(LMsg, P, 950); // to stay safe
    inc(P, 950);
    case &Type of
      TALCodeProfilerLogType.VERBOSE: NSLog(StringToID('[V]'+LThreadID+' ' + LMsgPart));
      TALCodeProfilerLogType.DEBUG:   NSLog(StringToID('[D][V]'+LThreadID+' ' + LMsgPart));
      TALCodeProfilerLogType.INFO:    NSLog(StringToID('[I][D][V]'+LThreadID+' ' + LMsgPart));
      TALCodeProfilerLogType.WARN:    NSLog(StringToID('[W][I][D][V]'+LThreadID+' ' + LMsgPart));
      TALCodeProfilerLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V]'+LThreadID+' ' + LMsgPart));
      TALCodeProfilerLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V]'+LThreadID+' ' + LMsgPart));
    end;
  end;
  {$ELSEIF defined(MSWINDOWS)}
  if LMsg <> '' then LMsg := Tag + ' | ' + stringReplace(LMsg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
  else LMsg := Tag;
  case &Type of
    TALCodeProfilerLogType.VERBOSE: OutputDebugString(pointer('[V] ' + LMsg + ' |'));
    TALCodeProfilerLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + LMsg + ' |'));
    TALCodeProfilerLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + LMsg + ' |'));
    TALCodeProfilerLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + LMsg + ' |'));
    TALCodeProfilerLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + LMsg + ' |'));
    TALCodeProfilerLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + LMsg + ' |'));
  end;
  {$ENDIF}
end;

{*********************************}
procedure TALProcMetricsStack.Grow;
begin
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
end;

{****************************************************************}
procedure TALProcMetricsStack.SetCapacity(NewCapacity: NativeInt);
begin
  if NewCapacity <> FCapacity then begin
    SetLength(FArray, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{***********************************}
procedure TALProcMetricsHistory.Grow;
begin
  {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}

  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.Grow was not updated and adjust the IFDEF'}
  {$ENDIF}

  var LNewCap: NativeInt := Length(FArray) * 2;
  if LNewCap = 0 then
    LNewCap := 4;
  Rehash(LNewCap);

  {$ELSE}

  SetCapacity(GrowCollection(FCapacity, FCount + 1));

  {$ENDIF}
end;

{******************************************************************}
procedure TALProcMetricsHistory.SetCapacity(NewCapacity: NativeInt);
begin
  {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}

  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.SetCapacity was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.InternalSetCapacity was not updated and adjust the IFDEF'}
  {$ENDIF}

  // Ensure at least one empty slot for GetBucketIndex to terminate.
  Inc(NewCapacity);
  if FCapacity <> NewCapacity then begin
    if NewCapacity < FCount then
      ErrorArgumentOutOfRange;

    if NewCapacity = 0 then Rehash(0)
    else begin
      var LNewCap: NativeInt := 4;
      while LNewCap shr 1 <= NewCapacity do // 50%
        LNewCap := LNewCap shl 1;
      Rehash(LNewCap);
    end
  end;

  {$ELSE}

  if NewCapacity <> FCapacity then begin
    SetLength(FArray, NewCapacity);
    FCapacity := NewCapacity;
  end;

  {$ENDIF}
end;

{************************************}
procedure TALProcMetricsHistory.Clear;
begin

  {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}

  FCount := 0;
  SetLength(FArray, 0);
  FCapacity := 0;
  FGrowThreshold := 0;

  {$ELSE}

  FCount := 0;

  {$ENDIF}

end;

{**************************************************}
{$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
procedure TALProcMetricsHistory.Rehash(NewCapPow2: NativeInt);
begin

  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.Rehash was not updated and adjust the IFDEF'}
  {$ENDIF}

  if NewCapPow2 = Length(FArray) then
    Exit
  else if NewCapPow2 < 0 then
    OutOfMemoryError;

  var LOldArray: TALProcMetricsArray := FArray;
  var LNewArray: TALProcMetricsArray;

  SetLength(LNewArray, NewCapPow2);
  var P: PALProcMetrics := PALProcMetrics(LNewArray);
  for var i := 0 to Length(LNewArray) - 1 do begin
    P^.HashCode := EMPTY_HASH;
    Inc(P);
  end;
  FArray := LNewArray;
  FGrowThreshold := NewCapPow2 shr 1; // 50%

  P := PALProcMetrics(LOldArray);
  for var i := 0 to Length(LOldArray) - 1 do begin
    raise Exception.Create(
      'Rehash is not implemented right now because MetricsID and ParentMetricsID ' +
      'reference positions in the array, which would become invalid after rehashing. ' +
      'The array is currently sized large enough to avoid calling Rehash.');
    if P^.HashCode <> EMPTY_HASH then begin
      var j := not GetBucketIndex(P^.ProcID, P^.ParentMetricsID, P^.HashCode);
      FArray[j] := P^;
    end;
    Inc(P);
  end;

end;
{$ENDIF}

{********************************************************}
{$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
function TALProcMetricsHistory.GetBucketIndex(const AProcID, AParentMetricsID: Cardinal; const AHashCode: Integer): NativeInt;
begin

  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.GetBucketIndex was not updated and adjust the IFDEF'}
  {$ENDIF}

  var L: NativeInt := Length(FArray);
  if L = 0 then
    Exit(not High(NativeInt));

  Result := AHashCode and (L - 1);
  var P: PALProcMetrics := @FArray[Result];
  while True do begin
    var LHashCode := P^.HashCode;

    // Not found: return complement of insertion point.
    if LHashCode = EMPTY_HASH then
      Exit(not Result);

    // Found: return location.
    if (LHashCode = AHashCode) and (P^.ProcID = AProcID) and (P^.ParentMetricsID = AParentMetricsID) then
      Exit(Result);

    Inc(Result);
    Inc(P);
    if Result >= L then begin
      Result := 0;
      P := @FArray[0];
    end;
  end;

end;
{$ENDIF}

{********************************************************}
{$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
function TALProcMetricsHistory.Hash(const AProcID, AParentMetricsID: Cardinal): Integer;
const
  PositiveMask = Integer.MaxValue;
begin

  {$IFNDEF ALCompilerVersionSupported131}
    {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.GetBucketIndex was not updated and adjust the IFDEF'}
  {$ENDIF}

  {$IFOPT Q+}
    {$DEFINE Q_ON}
    {$Q-}
  {$ENDIF}
  var LKey: UInt64 := (UInt64(AProcID) shl 32) or UInt64(AParentMetricsID);
  // Double-Abs to avoid -MaxInt and MinInt problems.
  // Not using compiler-Abs because we *must* get a positive integer;
  // for compiler, Abs(Low(Integer)) is a null op.
  Result := PositiveMask and ((PositiveMask and THashFNV1a32.GetHashValue(LKey, SizeOf(LKey))) + 1);
  {$IFDEF Q_ON}
    {$Q+}
    {$UNDEF Q_ON}
  {$ENDIF}

end;
{$ENDIF}

{**********************************************************************************************}
procedure ALCodeProfilerSaveHistory(const AProcMetricsHistory: TALProcMetricsHistory); overload;
begin
  {$IF defined(ALCodeProfilerHistoryGroupNone) or defined(ALCodeProfilerHistoryGroupByCallStack)}
  if AProcMetricsHistory.FCount = 0 then exit;
  {$ENDIF}
  //--
  If ALProcMetricsFilename = '' then begin
    {$IF defined(MSWindows)}
    var LRegistry := TRegistry.Create(KEY_READ);
    try
      LRegistry.RootKey := HKEY_CURRENT_USER;
      if LRegistry.OpenKeyReadOnly(ALCodeProfilerRegistryPath) then begin
        if LRegistry.ValueExists(ALCodeProfilerDataStoragePathKey) then
          ALProcMetricsFilename := LRegistry.ReadString(ALCodeProfilerDataStoragePathKey);
        LRegistry.CloseKey;
      end;
    finally
      LRegistry.Free;
    end;
    If ALProcMetricsFilename <> '' then begin
      ALProcMetricsFilename := TPath.Combine(ALProcMetricsFilename, ALCodeProfilerProcMetricsFilename);
      ALCodeProfilerServerName := '';
    end
    else
    {$ENDIF}
      ALProcMetricsFilename := TPath.Combine(System.IOUtils.TPath.GetTempPath, ALCodeProfilerProcMetricsFilename);
    if TFile.Exists(ALProcMetricsFilename) then TFile.Delete(ALProcMetricsFilename);
  end;
  //--
  var LfileStream: TFileStream;
  {$IF defined(ALCodeProfilerHistoryGroupByProcID) or defined(ALCodeProfilerHistoryGroupByCallStack)}
  if Tfile.Exists(ALProcMetricsFilename) then Tfile.Delete(ALProcMetricsFilename);
  LfileStream := TFileStream.Create(ALProcMetricsFilename, fmCreate);
  {$ELSE}
  if Tfile.Exists(ALProcMetricsFilename) then LfileStream := TFileStream.Create(ALProcMetricsFilename, fmOpenWrite)
  else LfileStream := TFileStream.Create(ALProcMetricsFilename, fmCreate);
  {$ENDIF}
  try
    LfileStream.Position := LfileStream.Size;
    {$IF defined(ALCodeProfilerHistoryGroupNone)}
    LfileStream.WriteBuffer(AProcMetricsHistory.FArray[0], AProcMetricsHistory.FCount * SizeOf(TALProcMetrics));
    {$ELSEIF defined(ALCodeProfilerHistoryGroupByProcID)}
    for var I := Low(AProcMetricsHistory.FArray) to High(AProcMetricsHistory.FArray) do
      if AProcMetricsHistory.FArray[I].CallCount <> 0 then
        LfileStream.WriteBuffer(AProcMetricsHistory.FArray[I], SizeOf(TALProcMetrics));
    {$ELSEIF defined(ALCodeProfilerHistoryGroupByCallStack)}
    for var I := Low(AProcMetricsHistory.FArray) to High(AProcMetricsHistory.FArray) do
      if AProcMetricsHistory.FArray[I].HashCode <> EMPTY_HASH then
        LfileStream.WriteBuffer(AProcMetricsHistory.FArray[I], SizeOf(TALProcMetrics));
    {$ENDIF}
  finally
    LFileStream.Free;
  end;
end;

{*************************************}
procedure ALCodeProfilerPurgeHistories;
begin
  ALProcMetricsLock.BeginWrite;
  try

    for var I := ALProcMetricsHistories.Count - 1 downto 0 do begin
      ALCodeProfilerSaveHistory(ALProcMetricsHistories[i]);
      {$IF defined(ALCodeProfilerHistoryGroupNone)}
      ALProcMetricsHistories[i].Clear;
      {$ENDIF}
    end;

    if ALCodeProfilerServerName <> '' then begin
      var LGuid: TGUID;
      if CreateGUID(LGuid) <> S_OK then RaiseLastOSError;
      var LGuidStr: String;
      SetLength(LGuidStr, 32);
      StrLFmt(
        PChar(LGuidStr), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
        [LGuid.D1, LGuid.D2, LGuid.D3, LGuid.D4[0], LGuid.D4[1], LGuid.D4[2], LGuid.D4[3],
        LGuid.D4[4], LGuid.D4[5], LGuid.D4[6], LGuid.D4[7]]);
      var LTmpProcMetricsFilename := ALProcMetricsFilename + '~' + LGuidStr;
      TFile.Move(ALProcMetricsFilename, LTmpProcMetricsFilename);
      {$IF defined(IOS) or defined(ANDROID)}
      TThread.CreateAnonymousThread(
        procedure
        begin
        {$ENDIF}
          var LHTTPClient := TNetHTTPClient.Create(nil);
          try
            Try
              var LFileStream := TFileStream.Create(LTmpProcMetricsFilename, fmOpenRead or fmShareDenyWrite);
              try
                var LHeaders: TNetHeaders;
                setlength(LHeaders, 1);
                LHeaders[0].Name := 'Content-Type';
                LHeaders[0].Value := 'application/octet-stream';
                LHTTPClient.Post(ALCodeProfilerServerName, LFileStream, nil{AResponseContent}, LHeaders);
              finally
                LFileStream.Free;
              end;
            Except
              On E: Exception do
                ALCodeProfilerLog('ALCodeProfiler', E.Message, TALCodeProfilerLogType.ERROR);
            End;
          finally
            TFile.Delete(LTmpProcMetricsFilename);
            LHTTPClient.Free;
          end;
        {$IF defined(IOS) or defined(ANDROID)}
        end).Start;
      {$ENDIF}
    end;

  finally
    ALProcMetricsLock.EndWrite;
  end;
end;

{**********************************************************}
procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
begin
  if ALCodeProfilerEnabled then begin
    var LProcMetricsStack := ALProcMetricsStack;
    if LProcMetricsStack = nil then begin
      ALProcMetricsStack := TALProcMetricsStack.Create;
      ALProcMetricsStack.SetCapacity(100);
      LProcMetricsStack := ALProcMetricsStack;
    end;
    //--
    if LProcMetricsStack.FCount = LProcMetricsStack.FCapacity then LProcMetricsStack.Grow;
    inc(LProcMetricsStack.FCount);
    With LProcMetricsStack.FArray[LProcMetricsStack.FCount - 1] do begin
      {$IF defined(ALCodeProfilerHistoryGroupNone)}
      ExecutionID := AtomicIncrement(TALStopWatchProcMetrics.ExecutionIDSequence);
      {$ENDIF}
      if LProcMetricsStack.FCount > 1 then begin
        {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
        var LProcMetricsHistory := ALProcMetricsHistory;
        if LProcMetricsHistory = nil then begin
          ALProcMetricsHistory := TALProcMetricsHistory.Create;
          ALProcMetricsHistory.SetCapacity(ALCodeProfilerHistoryCapacity); {with the default capacity: 1 000 000 * 32 Bytes = 32 MB or with gap = 2 097 152 * 32 Bytes = 67.11 MB}
          LProcMetricsHistory := ALProcMetricsHistory;
          ALProcMetricsLock.BeginWrite;
          try
            ALProcMetricsHistories.Add(LProcMetricsHistory);
          finally
            ALProcMetricsLock.EndWrite;
          end;
        end;
        ALProcMetricsLock.BeginRead;
        try
          var LParentProcID: Cardinal := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ProcID;
          var LParentParentMetricsID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ParentMetricsID;
          var LHashCode: Integer := LProcMetricsHistory.Hash(LParentProcID, LParentParentMetricsID);
          var LParentMetricsID: NativeInt := LProcMetricsHistory.GetBucketIndex(LParentProcID, LParentParentMetricsID, LHashCode);
          if LParentMetricsID < 0 then begin
            if LProcMetricsHistory.FCount >= LProcMetricsHistory.FGrowThreshold then begin
              LProcMetricsHistory.Grow;
              LParentMetricsID := LProcMetricsHistory.GetBucketIndex(LParentProcID, LParentParentMetricsID, LHashCode);
            end;
            inc(LProcMetricsHistory.FCount);
            LParentMetricsID := not LParentMetricsID;
            With LProcMetricsHistory.FArray[LParentMetricsID] do begin
              HashCode := LHashCode;
              ProcID := LParentProcID;
              MetricsID := LParentMetricsID;
              ParentMetricsID := LParentParentMetricsID;
              ThreadID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ThreadID;
              {$IFNDEF ALCompilerVersionSupported131}
                {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
              {$ENDIF}
              CallCount := 0;
              ElapsedTicks := 0;
            end;
          end;
          ParentMetricsID := LParentMetricsID;
        finally
          ALProcMetricsLock.EndRead;
        end;
        {$ELSEIF defined(ALCodeProfilerHistoryGroupNone)}
        ParentExecutionID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ExecutionID;
        {$ENDIF}
        {$IF not defined(ALCodeProfilerIgnoreThreadID)}
        ThreadID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ThreadID;
        {$ENDIF}
      end
      else begin
        {$IF defined(ALCodeProfilerHistoryGroupByCallStack)}
        ParentMetricsID := 0;
        {$ELSEIF defined(ALCodeProfilerHistoryGroupNone)}
        ParentExecutionID := 0;
        {$ENDIF}
        {$IF not defined(ALCodeProfilerIgnoreThreadID)}
        var LCurrentThreadID := TThread.CurrentThread.ThreadID;
        if LCurrentThreadID = MainThreadID then ThreadID := 0
        else begin
          ThreadID := LCurrentThreadID mod 4294967295;
          if ThreadID = 0 then ThreadID := 1;
        end;
        {$ENDIF}
      end;
      ProcID := AProcID;
      StopWatch := TStopWatch.StartNew;
    end;
  end;
end;

{*********************************************************}
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);

  type
    {$IFNDEF ALCompilerVersionSupported131}
      {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch was not updated and adjust the IFDEF'}
    {$ENDIF}
    TStopwatchAccessPrivate = record
    public
      FElapsed: Int64;
      FRunning: Boolean;
      FStartTimeStamp: Int64;
    end;

begin
  var LProcMetricsStack := ALProcMetricsStack;
  if LProcMetricsStack <> nil then begin
    if not ALCodeProfilerEnabled then begin
      ALProcMetricsStack.Free;
      ALProcMetricsStack := nil;
    end
    else if LProcMetricsStack.FCount <> 0 then begin
      var LProcMetricsStackLastIndex: integer := LProcMetricsStack.FCount - 1;
      LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Stop;
      //--
      var LProcMetricsHistory := ALProcMetricsHistory;
      {$IF not defined(ALCodeProfilerIgnoreThreadID)}
      if LProcMetricsHistory = nil then begin
        ALProcMetricsHistory := TALProcMetricsHistory.Create;
        ALProcMetricsHistory.SetCapacity(ALCodeProfilerHistoryCapacity); {with the default capacity: 1 000 000 * 32 Bytes = 32 MB or with gap = 2 097 152 * 32 Bytes = 67.11 MB}
        LProcMetricsHistory := ALProcMetricsHistory;
        ALProcMetricsLock.BeginWrite;
        try
          ALProcMetricsHistories.Add(LProcMetricsHistory);
        finally
          ALProcMetricsLock.EndWrite;
        end;
      end;
      {$ENDIF}
      //--
      ALProcMetricsLock.BeginRead;
      try
        {$IF defined(ALCodeProfilerHistoryGroupNone)}
        if (LProcMetricsHistory.FCount = LProcMetricsHistory.FCapacity) then begin
          if (LProcMetricsHistory.FCount >= 100_000_000) {100_000_000 * 32 Bytes = 3.2 GB} then begin
            ALProcMetricsLock.EndRead;
            try
              ALCodeProfilerPurgeHistories;
            finally
              ALProcMetricsLock.BeginRead;
            end;
          end;
          if LProcMetricsHistory.FCount = LProcMetricsHistory.FCapacity then
            LProcMetricsHistory.Grow;
        end;
        inc(LProcMetricsHistory.FCount);
        With LProcMetricsHistory.FArray[LProcMetricsHistory.FCount - 1] do begin
          ExecutionID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ExecutionID;
          ParentExecutionID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ParentExecutionID;
          ProcID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ProcID;
          ThreadID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ThreadID;
          {$IFNDEF ALCompilerVersionSupported131}
            {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
          {$ENDIF}
          {$IF defined(MSWINDOWS)}
          var LTickFrequency: Double;
          if not TStopwatch.IsHighResolution then LTickFrequency := 1.0
          else LTickFrequency := 10000000.0 / LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Frequency;
          StartTimeStamp := Trunc((TStopwatchAccessPrivate(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch).FStartTimeStamp - ALCodeProfilerAppStartTimeStamp) * LTickFrequency);
          ElapsedTicks := Trunc(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks * LTickFrequency);
          {$ELSEIF defined(POSIX)}
          StartTimeStamp := TStopwatchAccessPrivate(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch).FStartTimeStamp - ALCodeProfilerAppStartTimeStamp;
          ElapsedTicks := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks;
          {$ELSE}
          Raise Exception.create('Error 55533349-EC72-404D-B113-CA32C518012F')
          {$ENDIF}
        end;
        {$ELSEIF defined(ALCodeProfilerHistoryGroupByProcID)}
        var LProcID: Cardinal := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ProcID;
        {$IFNDEF ALCompilerVersionSupported131}
          {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.TryAdd was not updated and adjust the IFDEF'}
        {$ENDIF}
        {$IF defined(ALCodeProfilerIgnoreThreadID)}
        // All the threads update the very same record, so the metrics must be
        // updated atomically.
        With LProcMetricsHistory.FArray[LProcID] do begin
          ProcID := LProcID;
          ThreadID := 0;
          AtomicIncrement(CallCount);
          {$IFNDEF ALCompilerVersionSupported131}
            {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
          {$ENDIF}
          {$IF defined(MSWINDOWS)}
          var LTickFrequency: Double;
          if not TStopwatch.IsHighResolution then LTickFrequency := 1.0
          else LTickFrequency := 10000000.0 / LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Frequency;
          AtomicIncrement(ElapsedTicks, Trunc(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks * LTickFrequency));
          {$ELSEIF defined(POSIX)}
          AtomicIncrement(ElapsedTicks, LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks);
          {$ELSE}
          Raise Exception.create('Error 5FE96C7E-ABFA-4AE2-84E5-39EFF2E83BBB')
          {$ENDIF}
        end;
        {$ELSE}
        With LProcMetricsHistory.FArray[LProcID] do begin
          ProcID := LProcID;
          ThreadID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ThreadID;
          Inc(CallCount);
          {$IFNDEF ALCompilerVersionSupported131}
            {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
          {$ENDIF}
          {$IF defined(MSWINDOWS)}
          var LTickFrequency: Double;
          if not TStopwatch.IsHighResolution then LTickFrequency := 1.0
          else LTickFrequency := 10000000.0 / LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Frequency;
          ElapsedTicks := ElapsedTicks + Trunc(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks * LTickFrequency);
          {$ELSEIF defined(POSIX)}
          ElapsedTicks := ElapsedTicks + LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks;
          {$ELSE}
          Raise Exception.create('Error 8CB28339-29A0-4276-80AA-F8CD5E447CE5')
          {$ENDIF}
        end;
        {$ENDIF}
        {$ELSEIF defined(ALCodeProfilerHistoryGroupByCallStack)}
        var LProcID: Cardinal := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ProcID;
        var LParentMetricsID: Cardinal := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ParentMetricsID;
        {$IFNDEF ALCompilerVersionSupported131}
          {$MESSAGE WARN 'Check if System.Generics.Collections.TDictionary<K,V>.TryAdd was not updated and adjust the IFDEF'}
        {$ENDIF}
        var LHashCode: Integer := LProcMetricsHistory.Hash(LProcID, LParentMetricsID);
        var LIndex: NativeInt := LProcMetricsHistory.GetBucketIndex(LProcID, LParentMetricsID, LHashCode);
        if LIndex >= 0 then begin
          With LProcMetricsHistory.FArray[LIndex] do begin
            Inc(CallCount);
            {$IFNDEF ALCompilerVersionSupported131}
              {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
            {$ENDIF}
            {$IF defined(MSWINDOWS)}
            var LTickFrequency: Double;
            if not TStopwatch.IsHighResolution then LTickFrequency := 1.0
            else LTickFrequency := 10000000.0 / LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Frequency;
            ElapsedTicks := ElapsedTicks + Trunc(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks * LTickFrequency);
            {$ELSEIF defined(POSIX)}
            ElapsedTicks := ElapsedTicks + LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks;
            {$ELSE}
            Raise Exception.create('Error 8CB28339-29A0-4276-80AA-F8CD5E447CE5')
            {$ENDIF}
          end;
        end
        else begin
          if LProcMetricsHistory.FCount >= LProcMetricsHistory.FGrowThreshold then begin
            LProcMetricsHistory.Grow;
            LIndex := LProcMetricsHistory.GetBucketIndex(LProcID, LParentMetricsID, LHashCode);
          end;
          inc(LProcMetricsHistory.FCount);
          With LProcMetricsHistory.FArray[not LIndex] do begin
            HashCode := LHashCode;
            ProcID := LProcID;
            MetricsID := not LIndex;
            ParentMetricsID := LParentMetricsID;
            ThreadID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ThreadID;
            CallCount := 1;
            {$IFNDEF ALCompilerVersionSupported131}
              {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch.InitStopwatchType was not updated and adjust the IFDEF'}
            {$ENDIF}
            {$IF defined(MSWINDOWS)}
            var LTickFrequency: Double;
            if not TStopwatch.IsHighResolution then LTickFrequency := 1.0
            else LTickFrequency := 10000000.0 / LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Frequency;
            ElapsedTicks := Trunc(LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks * LTickFrequency);
            {$ELSEIF defined(POSIX)}
            ElapsedTicks := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.ElapsedTicks;
            {$ELSE}
            Raise Exception.create('Error 8CB28339-29A0-4276-80AA-F8CD5E447CE5')
            {$ENDIF}
          end;
        end;
        {$ENDIF}
        dec(LProcMetricsStack.FCount);
      finally
        ALProcMetricsLock.EndRead;
      end;
      if (LProcMetricsStack.FCount = 0) and
         (TThread.CurrentThread.ThreadID <> MainThreadID) then begin
        ALProcMetricsStack.Free;
        ALProcMetricsStack := nil;
      end;
    end;
  end;
end;

{****************************}
procedure ALCodeProfilerStart;
begin
  ALCodeProfilerEnabled := True;
end;

{***************************}
procedure ALCodeProfilerStop;
Begin
  ALCodeProfilerEnabled := False;
End;

{****************************************}
function ALCodeProfilerIsrunning: Boolean;
begin
  Result := ALCodeProfilerEnabled;
end;

{************************************}
{$IF defined(IOS) or defined(ANDROID)}
procedure ALCodeProfilerApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if (M is TApplicationEventMessage) and
     ((M as TApplicationEventMessage).value.Event = TApplicationEvent.BecameActive) then begin
    if ALCodeProfilerAppActivatedBefore then
      ALCodeProfilerPurgeHistories;
    else
      ALCodeProfilerAppActivatedBefore := True;
  end;
end;
{$ENDIF}

initialization
  ALCodeProfilerAppStartTimeStamp := TStopWatch.GetTimeStamp;
  {$IF defined(ALCodeProfilerHistoryGroupNone)}
  TALStopWatchProcMetrics.ExecutionIDSequence := 0;
  {$ENDIF}
  //ALProcMetricsLock := ?? There is no TLightweightMREW.Create; initialization is done through the TLightweightMREW.Initialize class operator instead
  ALProcMetricsFilename := '';
  //--
  ALProcMetricsHistory := TALProcMetricsHistory.Create;
  {$IF defined(ALCodeProfilerHistoryGroupNone)}
  ALProcMetricsHistory.SetCapacity(25000000); {25 000 000 * 32 Bytes = 800MB}
  {$ELSE}
  ALProcMetricsHistory.SetCapacity(ALCodeProfilerHistoryCapacity); {with the default capacity: 1 000 000 = 2 097 152 (with gap) * 32 Bytes = 67.11 MB}
  {$ENDIF}
  //--
  ALProcMetricsHistories := TList<TALProcMetricsHistory>.Create;
  ALProcMetricsHistories.Add(ALProcMetricsHistory);
  //--
  {$IF defined(IOS) or defined(ANDROID)}
  ALCodeProfilerAppActivatedBefore := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ALCodeProfilerApplicationEventHandler);
  {$ENDIF}

finalization
  {$IF (not defined(IOS)) and (not defined(ANDROID))}
  // At this point, all background threads must have completed.
  ALCodeProfilerPurgeHistories;
  {$ENDIF}
  ALCodeProfilerEnabled := False;
  //--
  ALProcMetricsHistories.Free;
  ALProcMetricsHistories := nil;
  //--
  if ALProcMetricsStack <> nil then begin
    ALProcMetricsStack.Free;
    ALProcMetricsStack := nil;
  end;
  //--
  if ALProcMetricsHistory <> nil then begin
    ALProcMetricsHistory.Free;
    ALProcMetricsHistory := nil;
  end;
  //--
  {$IF defined(IOS) or defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ALCodeProfilerApplicationEventHandler);
  {$ENDIF}

end.