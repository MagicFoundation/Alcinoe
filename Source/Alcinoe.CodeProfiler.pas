unit Alcinoe.CodeProfiler;

{$I Alcinoe.inc}

interface

type
  TALProcMetrics = record
  public
    ExecutionID: Cardinal;
    ThreadID: Cardinal;
    ProcID: Cardinal;
    ParentExecutionID: Cardinal;
    StartTimeStamp: Int64;
    ElapsedTicks: Int64;
  end;

procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);
procedure ALCodeProfilerStart;
procedure ALCodeProfilerStop(Const ASaveHistory: Boolean = True);

const
  ALCodeProfilerProcMetricsFilename = 'ALCodeProfilerProcMetrics.dat';
  ALCodeProfilerProcIDMapFilename = 'ALCodeProfilerProcIDMap.txt';
  ALCodeProfilerRegistryPath = 'Software\MagicFoundation\Alcinoe\CodeProfiler';
  ALCodeProfilerDataStoragePathKey = 'DataStoragePath';

implementation

uses
  {$IF defined(MSWindows)}
  System.Win.Registry,
  winapi.Windows,
  {$ENDIF}
  system.SysUtils,
  System.Classes,
  system.Generics.Collections,
  System.Diagnostics,
  System.IOUtils,
  Alcinoe.StringUtils;

{**}
Type
  TALStopWatchProcMetrics = record
  private
    class var ExecutionIDSequence: cardinal;
  public
    ExecutionID: Cardinal;
    ThreadID: Cardinal;
    ProcID: Cardinal;
    ParentExecutionID: Cardinal;
    StopWatch: TStopWatch;
    constructor Create(const AProcID: Cardinal);
  end;

{*******}
threadvar
  ALProcMetricsStack: TStack<TALStopWatchProcMetrics>;
  ALProcMetricsHistory: TList<TALStopWatchProcMetrics>;

{*}
var
  ALCodeProfilerEnabled: Boolean;
  ALProcMetricsLock: TObject;
  ALProcMetricsFilename: String;

{*********************************************************}
constructor TALStopWatchProcMetrics.Create(const AProcID: Cardinal);
begin
  Self.ExecutionID := AtomicIncrement(ExecutionIDSequence);
  Self.ThreadID := TThread.CurrentThread.ThreadID;
  if Self.ThreadID = MainThreadID then Self.ThreadID := 0;
  Self.ProcID := AProcID;
  Self.ParentExecutionID := 0;
  Self.StopWatch := TStopWatch.StartNew;
end;

{**********************************}
procedure ALCodeProfilerSaveHistory;

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
  var LProcMetricsHistory := ALProcMetricsHistory;
  if LProcMetricsHistory = nil then exit;
  var LMemoryStream := TMemoryStream.Create;
  try
    LMemoryStream.Size := LProcMetricsHistory.Count * SizeOf(TALProcMetrics);
    For var I := 0 to LProcMetricsHistory.Count - 1 do begin
      var LStopWatchProcMetrics := LProcMetricsHistory[i];
      var LProcMetrics: TALProcMetrics;
      LProcMetrics.ExecutionID := LStopWatchProcMetrics.ExecutionID;
      LProcMetrics.ThreadID := LStopWatchProcMetrics.ThreadID;
      LProcMetrics.ProcID := LStopWatchProcMetrics.ProcID;
      LProcMetrics.ParentExecutionID := LStopWatchProcMetrics.ParentExecutionID;
      LProcMetrics.StartTimeStamp := TStopwatchAccessPrivate(LStopWatchProcMetrics.StopWatch).FStartTimeStamp;
      LProcMetrics.ElapsedTicks := LStopWatchProcMetrics.StopWatch.ElapsedTicks;
      LMemoryStream.WriteData(LProcMetrics);
    end;
    Tmonitor.Enter(ALProcMetricsLock);
    try
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
        If ALProcMetricsFilename <> '' then
          ALProcMetricsFilename := TPath.Combine(ALProcMetricsFilename, ALCodeProfilerProcMetricsFilename)
        else
        {$ENDIF}
          ALProcMetricsFilename := TPath.Combine(TPath.GetDownloadsPath, ALCodeProfilerProcMetricsFilename);
        if TFile.Exists(ALProcMetricsFilename) then TFile.Delete(ALProcMetricsFilename);
      end;
      var LfileStream: TFileStream;
      if Tfile.Exists(ALProcMetricsFilename) then LfileStream := TFileStream.Create(ALProcMetricsFilename, fmOpenWrite)
      else LfileStream := TFileStream.Create(ALProcMetricsFilename, fmCreate);
      try
        LfileStream.Position := LfileStream.Size;
        LfileStream.CopyFrom(LMemoryStream);
      finally
        LFileStream.Free;
      end;
    finally
      Tmonitor.Exit(ALProcMetricsLock);
    end;
  finally
    LMemoryStream.free;
  end;
  ALProcMetricsHistory.Free;
  ALProcMetricsHistory := nil;
end;

{**********************************************************}
procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
begin
  if not ALCodeProfilerEnabled then exit;
  var LProcMetricsStack := ALProcMetricsStack;
  if LProcMetricsStack = nil then begin
    ALProcMetricsStack := TStack<TALStopWatchProcMetrics>.Create;
    LProcMetricsStack := ALProcMetricsStack;
  end;
  var LProcMetrics: TALStopWatchProcMetrics;
  if LProcMetricsStack.Count > 0 then begin
    LProcMetrics := LProcMetricsStack.Peek;
    LProcMetrics.ParentExecutionID := LProcMetrics.ExecutionID;
    LProcMetrics.ExecutionID := AtomicIncrement(TALStopWatchProcMetrics.ExecutionIDSequence);
    LProcMetrics.ProcID := aProcID;
    LProcMetrics.StopWatch := TStopWatch.StartNew;
  end
  else LProcMetrics := TALStopWatchProcMetrics.create(aProcID);
  LProcMetricsStack.Push(LProcMetrics);
end;

{*********************************************************}
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);
begin
  var LProcMetricsStack := ALProcMetricsStack;
  if LProcMetricsStack = nil then exit;
  if not ALCodeProfilerEnabled then begin
    ALProcMetricsStack.Free;
    ALProcMetricsStack := nil;
    ALCodeProfilerSaveHistory;
    exit;
  end;
  if LProcMetricsStack.Count > 0 then begin
    var LProcMetrics := LProcMetricsStack.Pop;
    LProcMetrics.StopWatch.Stop;
    var LProcMetricsHistory := ALProcMetricsHistory;
    if LProcMetricsHistory = nil then begin
      ALProcMetricsHistory := TList<TALStopWatchProcMetrics>.Create;
      LProcMetricsHistory := ALProcMetricsHistory;
    end;
    LProcMetricsHistory.Add(LProcMetrics);
    if (LProcMetricsStack.Count = 0) and
       (TThread.CurrentThread.ThreadID <> MainThreadID) then begin
      ALProcMetricsStack.Free;
      ALProcMetricsStack := nil;
      ALCodeProfilerSaveHistory;
    end;
  end;
end;

{****************************}
procedure ALCodeProfilerStart;
begin
  ALCodeProfilerEnabled := True;
end;

{***************************}
procedure ALCodeProfilerStop(Const ASaveHistory: Boolean = True);
Begin
  ALCodeProfilerEnabled := False;
  if ALProcMetricsStack <> nil then begin
    ALProcMetricsStack.Free;
    ALProcMetricsStack := nil;
  end;
  if ASaveHistory then ALCodeProfilerSaveHistory
  else if ALProcMetricsHistory <> nil then begin
    ALProcMetricsHistory.Free;
    ALProcMetricsHistory := nil;
  end;
End;

initialization
  TALStopWatchProcMetrics.ExecutionIDSequence := 0;
  ALProcMetricsLock := TObject.Create;
  ALCodeProfilerEnabled := True;
  ALProcMetricsFilename := '';

finalization
  ALCodeProfilerEnabled := False;
  ALCodeProfilerSaveHistory;
  if ALProcMetricsStack <> nil then begin
    ALProcMetricsStack.Free;
    ALProcMetricsStack := nil;
  end;
  ALProcMetricsLock.Free;
  ALProcMetricsLock := nil;

end.
