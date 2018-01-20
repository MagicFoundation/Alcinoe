// test logging when 6000 threads are created (per chunks of 20)
program thread512;

{$APPTYPE CONSOLE}

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SynCommons,
  SynLog,
  mORMot,
  Classes,
  SysUtils;

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  n: integer;

procedure SubProc;
begin
  InterlockedIncrement(n);
  TSynLog.Enter(nil,'SubProc').Log(sllDebug,'Thread #% (%)',[n,pointer(GetCurrentThreadID)]);
  sleep(0);
end;

procedure TMyThread.Execute;
var n: TThreadID;
    log: TSynLog;
begin
  n := GetCurrentThreadId;
  log := TSynLog.Add;
  log.Log(sllTrace,'Entering thread %',[pointer(n)]);
  SubProc;
  log.Log(sllTrace,'Leaving thread %',[pointer(n)]);
  log.NotifyThreadEnded;
end;

procedure Test;
var i,j: Integer;
    t: array[1..20] of TMyThread;
begin
  TSynLog.Enter;
  for i := 1 to 300 do begin
    for j := Low(t) to high(t) do
      t[j] := TMyThread.Create(false);
    for j := high(t) downto low(t) do
      t[j].WaitFor;
    for j := Low(t) to high(t) do
      t[j].Free;
  end;
end;

var Timer: TPrecisionTimer;
begin
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSynLog.Family.EchoToConsole := LOG_STACKTRACE;
  Timer.Start;
  Test;
  writeln(n,' threads created in ',Timer.Stop);
  {$ifdef MSWINDOWS}
  readln;
  {$endif}
end.
