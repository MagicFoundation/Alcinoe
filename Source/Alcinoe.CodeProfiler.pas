unit Alcinoe.CodeProfiler;

{$I Alcinoe.inc}

interface

type
  TALProcMetrics = record
  public
    ExecutionID: Cardinal;
    ParentExecutionID: Cardinal;
    ProcID: Cardinal;
    ThreadID: Cardinal;
    StartTimeStamp: Int64;
    ElapsedTicks: Int64;
  end;

procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);
procedure ALCodeProfilerStart;
procedure ALCodeProfilerStop(Const ASaveHistories: Boolean = True);
function ALCodeProfilerIsrunning: Boolean;

const
  ALCodeProfilerProcMetricsFilename = 'ALCodeProfilerProcMetrics.dat';
  ALCodeProfilerProcIDMapFilename = 'ALCodeProfilerProcIDMap.txt';
  ALCodeProfilerRegistryPath = 'Software\MagicFoundation\Alcinoe\CodeProfiler';
  ALCodeProfilerDataStoragePathKey = 'DataStoragePath';
  ALCodeProfilerMillisecondsPerTick = 0.0001;

var
  ALCodeProfilerAppStartTimeStamp: Int64;
  ALCodeProfilerServerName: String;

implementation

uses
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
    class var ExecutionIDSequence: cardinal;
  public
    ExecutionID: Cardinal;
    ParentExecutionID: Cardinal;
    ProcID: Cardinal;
    ThreadID: Cardinal;
    StopWatch: TStopWatch;
  end;

  TALStopWatchProcMetricsArray = array of TALStopWatchProcMetrics;
  TALProcMetricsStack = class(TObject)
  protected
    FArray: TALStopWatchProcMetricsArray;
    FCount: NativeInt;
    FCapacity: NativeInt;
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: NativeInt);
  public
    destructor Destroy; override;
  end;

  TALProcMetricsArray = array of TALProcMetrics;
  TALProcMetricsHistory = class(TObject)
  protected
    FArray: TALProcMetricsArray;
    FCount: NativeInt;
    FCapacity: NativeInt;
    FIsOrphaned: Boolean;
    procedure Grow; virtual;
    procedure SetCapacity(NewCapacity: NativeInt);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

{*******}
threadvar
  ALProcMetricsStack: TALProcMetricsStack;
  ALProcMetricsHistory: TALProcMetricsHistory;
  ALIsInCodeProfiler: Boolean;

{*}
var
  ALProcMetricsHistories: TList<TALProcMetricsHistory>;
  ALCodeProfilerEnabled: Boolean;
  ALProcMetricsLock: TLightweightMREW;
  ALProcMetricsFilename: String;
  {$IF defined(IOS) or defined(ANDROID)}
  ALCodeProfilerAppActivatedBefore: Boolean;
  {$ENDIF}

{**}
Type
  TALCodeProfilerLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);

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

{*************************************}
destructor TALProcMetricsStack.Destroy;
begin
  SetCapacity(0);
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

{***************************************}
constructor TALProcMetricsHistory.Create;
begin
  inherited;
  FIsOrphaned := False;
end;

{***************************************}
destructor TALProcMetricsHistory.Destroy;
begin
  SetCapacity(0);
end;

{***********************************}
procedure TALProcMetricsHistory.Grow;
begin
  SetCapacity(GrowCollection(FCapacity, FCount + 1));
end;

{******************************************************************}
procedure TALProcMetricsHistory.SetCapacity(NewCapacity: NativeInt);
begin
  if NewCapacity <> FCapacity then begin
    SetLength(FArray, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

{**********************************************************************************************}
procedure ALCodeProfilerSaveHistory(const AProcMetricsHistory: TALProcMetricsHistory); overload;
begin
  if AProcMetricsHistory.FCount = 0 then exit;
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
      ALProcMetricsFilename := TPath.Combine(TPath.GetDocumentsPath, ALCodeProfilerProcMetricsFilename);
    if TFile.Exists(ALProcMetricsFilename) then TFile.Delete(ALProcMetricsFilename);
  end;
  //--
  var LfileStream: TFileStream;
  if Tfile.Exists(ALProcMetricsFilename) then LfileStream := TFileStream.Create(ALProcMetricsFilename, fmOpenWrite)
  else LfileStream := TFileStream.Create(ALProcMetricsFilename, fmCreate);
  try
    LfileStream.Position := LfileStream.Size;
    LfileStream.WriteBuffer(AProcMetricsHistory.FArray[0], AProcMetricsHistory.FCount * SizeOf(TALProcMetrics));
  finally
    LFileStream.Free;
  end;
  //--
  AProcMetricsHistory.FCount := 0;
end;

{********************************************************************}
procedure ALCodeProfilerPurgeHistories(const ASaveHistories: boolean);
begin
  ALProcMetricsLock.BeginWrite;
  try
    for var I := ALProcMetricsHistories.Count - 1 downto 0 do begin
      if ASaveHistories then ALCodeProfilerSaveHistory(ALProcMetricsHistories[i]);
      if ALProcMetricsHistories[i].FIsOrphaned then ALProcMetricsHistories.ExtractAt(i).Free
      else ALProcMetricsHistories[i].FCount := 0;
    end;
  finally
    ALProcMetricsLock.EndWrite;
  end;
  //--
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
end;

{**********************************************************}
procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
begin
  if ALIsInCodeProfiler then exit;
  ALIsInCodeProfiler := True;
  //--
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
      ExecutionID := AtomicIncrement(TALStopWatchProcMetrics.ExecutionIDSequence);
      if LProcMetricsStack.FCount > 1 then begin
        ParentExecutionID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ExecutionID;
        ThreadID := LProcMetricsStack.FArray[LProcMetricsStack.FCount - 2].ThreadID;
      end
      else begin
        ParentExecutionID := 0;
        var LCurrentThreadID := TThread.CurrentThread.ThreadID;
        if LCurrentThreadID = MainThreadID then ThreadID := 0
        else begin
          ThreadID := LCurrentThreadID mod 4294967295;
          if ThreadID = 0 then ThreadID := 1;
        end;
      end;
      ProcID := AProcID;
      StopWatch := TStopWatch.StartNew;
    end;
  end;
  //--
  ALIsInCodeProfiler := False;
end;

{*********************************************************}
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);

  type
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if System.Diagnostics.TStopwatch was not updated and adjust the IFDEF'}
    {$ENDIF}
    TStopwatchAccessPrivate = record
    public
      FElapsed: Int64;
      FRunning: Boolean;
      FStartTimeStamp: Int64;
    end;

begin
  if ALIsInCodeProfiler then exit;
  ALIsInCodeProfiler := True;
  //--
  var LProcMetricsStack := ALProcMetricsStack;
  if LProcMetricsStack <> nil then begin
    if not ALCodeProfilerEnabled then begin
      ALProcMetricsStack.Free;
      ALProcMetricsStack := nil;
      if ALProcMetricsHistory <> nil then begin
        ALProcMetricsLock.BeginRead;
        try
          ALProcMetricsHistory.FIsOrphaned := true;
          ALProcMetricsHistory := nil;
        finally
          ALProcMetricsLock.EndRead;
        end;
      end;
    end
    else if LProcMetricsStack.FCount <> 0 then begin
      var LProcMetricsStackLastIndex: integer := LProcMetricsStack.FCount - 1;
      LProcMetricsStack.FArray[LProcMetricsStackLastIndex].StopWatch.Stop;
      //--
      var LProcMetricsHistory := ALProcMetricsHistory;
      if LProcMetricsHistory = nil then begin
        ALProcMetricsHistory := TALProcMetricsHistory.Create;
        ALProcMetricsHistory.SetCapacity(1000000); {1 000 000 * 32 Bytes = 32 MB}
        LProcMetricsHistory := ALProcMetricsHistory;
        ALProcMetricsLock.BeginWrite;
        try
          ALProcMetricsHistories.Add(LProcMetricsHistory);
        finally
          ALProcMetricsLock.EndWrite;
        end;
      end;
      //--
      ALProcMetricsLock.BeginRead;
      try
        if LProcMetricsHistory.FCount = LProcMetricsHistory.FCapacity then LProcMetricsHistory.Grow;
        inc(LProcMetricsHistory.FCount);
        With LProcMetricsHistory.FArray[LProcMetricsHistory.FCount - 1] do begin
          ExecutionID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ExecutionID;
          ParentExecutionID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ParentExecutionID;
          ProcID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ProcID;
          ThreadID := LProcMetricsStack.FArray[LProcMetricsStackLastIndex].ThreadID;
          {$IFNDEF ALCompilerVersionSupported122}
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
        dec(LProcMetricsStack.FCount);
      finally
        ALProcMetricsLock.EndRead;
      end;
      if (LProcMetricsStack.FCount = 0) and
         (TThread.CurrentThread.ThreadID <> MainThreadID) then begin
        ALProcMetricsStack.Free;
        ALProcMetricsStack := nil;
        If ALProcMetricsHistory <> nil then begin
          ALProcMetricsLock.BeginRead;
          try
            ALProcMetricsHistory.FIsOrphaned := true;
            ALProcMetricsHistory := nil;
          finally
            ALProcMetricsLock.EndRead;
          end;
        end;
      end;
    end;
  end;
  //--
  ALIsInCodeProfiler := False;
end;

{****************************}
procedure ALCodeProfilerStart;
begin
  ALCodeProfilerEnabled := True;
end;

{*****************************************************************}
procedure ALCodeProfilerStop(Const ASaveHistories: Boolean = True);
Begin
  ALCodeProfilerEnabled := False;
  ALCodeProfilerPurgeHistories(ASaveHistories);
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
      ALCodeProfilerPurgeHistories(ALCodeProfilerEnabled{ASaveHistories})
    else
      ALCodeProfilerAppActivatedBefore := True;
  end;
end;
{$ENDIF}

initialization
  ALIsInCodeProfiler := False;
  ALCodeProfilerAppStartTimeStamp := TStopWatch.GetTimeStamp;
  TALStopWatchProcMetrics.ExecutionIDSequence := 0;
  //ALProcMetricsLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(
  ALCodeProfilerEnabled := True;
  ALProcMetricsFilename := '';
  ALCodeProfilerServerName := '';
  //--
  ALProcMetricsHistory := TALProcMetricsHistory.Create;
  ALProcMetricsHistory.SetCapacity(25000000); {25 000 000 * 32 Bytes = 800MB}
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
  ALCodeProfilerPurgeHistories(ALCodeProfilerEnabled{ASaveHistories});
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
