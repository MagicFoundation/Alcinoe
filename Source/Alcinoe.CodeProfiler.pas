unit Alcinoe.CodeProfiler;

{$I Alcinoe.inc}

interface

type
  TALProcMetrics = record
  public
    RunID: Cardinal;
    ThreadID: Cardinal;
    ProcID: Cardinal;
    ParentRunID: Cardinal;
    TimeTaken: Double;
  end;

procedure ALCodeProfilerEnterProc(const aProcID : Cardinal);
procedure ALCodeProfilerExitProc(const aProcID : Cardinal);

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
    class var RunIDSequence: cardinal;
  public
    RunID: Cardinal;
    ThreadID: Cardinal;
    ProcID: Cardinal;
    ParentRunID: Cardinal;
    StopWatch: TStopWatch;
    constructor Create(const AProcID: Cardinal);
  end;

{*******}
threadvar
  ALProcMetricsStack: TStack<TALStopWatchProcMetrics>;
  ALProcMetricsHistory: TList<TALStopWatchProcMetrics>;

{*}
var
  ALProcMetricsLock: TObject;
  ALDataFilename: String;

{*********************************************************}
constructor TALStopWatchProcMetrics.Create(const AProcID: Cardinal);
begin
  Self.RunID := AtomicIncrement(RunIDSequence);
  Self.ThreadID := TThread.CurrentThread.ThreadID;
  if Self.ThreadID = MainThreadID then Self.ThreadID := 0;
  Self.ProcID := AProcID;
  Self.ParentRunID := 0;
  Self.StopWatch := TStopWatch.StartNew;
end;

{**********************************}
procedure ALCodeProfilerSaveHistory;
begin
  var LProcMetricsHistory := ALProcMetricsHistory;
  if LProcMetricsHistory = nil then exit;
  var LMemoryStream := TMemoryStream.Create;
  try
    LMemoryStream.Size := LProcMetricsHistory.Count * ({ThreadID}sizeOf(Cardinal) + {LoopID}sizeOf(Cardinal) + {ProcID}sizeOf(Cardinal) + {ParentProcID}sizeOf(Cardinal) + {TimeTaken}sizeOf(Double));
    For var I := 0 to LProcMetricsHistory.Count - 1 do begin
      var LStopWatchProcMetrics := LProcMetricsHistory[i];
      var LProcMetrics: TALProcMetrics;
      LProcMetrics.RunID := LStopWatchProcMetrics.RunID;
      LProcMetrics.ThreadID := LStopWatchProcMetrics.ThreadID;
      LProcMetrics.ProcID := LStopWatchProcMetrics.ProcID;
      LProcMetrics.ParentRunID := LStopWatchProcMetrics.ParentRunID;
      LProcMetrics.TimeTaken := LStopWatchProcMetrics.StopWatch.Elapsed.TotalMilliseconds;
      LMemoryStream.WriteData(LProcMetrics);
    end;
    Tmonitor.Enter(ALProcMetricsLock);
    try
      If ALDataFilename = '' then begin
        {$IF defined(MSWindows)}
        var LRegistry := TRegistry.Create(KEY_READ);
        try
          LRegistry.RootKey := HKEY_CURRENT_USER;
          if LRegistry.OpenKeyReadOnly(ALCodeProfilerRegistryPath) then begin
            if LRegistry.ValueExists(ALCodeProfilerDataStoragePathKey) then
              ALDataFilename := LRegistry.ReadString(ALCodeProfilerDataStoragePathKey);
            LRegistry.CloseKey;
          end;
        finally
          LRegistry.Free;
        end;
        If ALDataFilename <> '' then
          ALDataFilename := TPath.Combine(ALDataFilename, ALCodeProfilerProcMetricsFilename)
        else
        {$ENDIF}
          ALDataFilename := TPath.Combine(TPath.GetDownloadsPath, ALCodeProfilerProcMetricsFilename);
        if TFile.Exists(ALDataFilename) then TFile.Delete(ALDataFilename);
      end;
      var LfileStream: TFileStream;
      if Tfile.Exists(ALDataFilename) then LfileStream := TFileStream.Create(ALDataFilename, fmOpenWrite)
      else LfileStream := TFileStream.Create(ALDataFilename, fmCreate);
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
  var LProcMetricsStack := ALProcMetricsStack;
  if LProcMetricsStack = nil then begin
    ALProcMetricsStack := TStack<TALStopWatchProcMetrics>.Create;
    LProcMetricsStack := ALProcMetricsStack;
  end;
  var LProcMetrics: TALStopWatchProcMetrics;
  if LProcMetricsStack.Count > 0 then begin
    LProcMetrics := LProcMetricsStack.Peek;
    LProcMetrics.ParentRunID := LProcMetrics.RunID;
    LProcMetrics.RunID := AtomicIncrement(TALStopWatchProcMetrics.RunIDSequence);
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

initialization
  TALStopWatchProcMetrics.RunIDSequence := 0;
  ALProcMetricsLock := TObject.Create;
  ALDataFilename := '';

finalization
  ALCodeProfilerSaveHistory;
  if ALProcMetricsStack <> nil then begin
    ALProcMetricsStack.Free;
    ALProcMetricsStack := nil;
  end;
  ALProcMetricsLock.Free;
  ALProcMetricsLock := nil;

end.
