{
  Synopse mORMot framework

  Sample 11 - Exception logging
    purpose of this sample is to show basic logging mechanism of the framework

  TO HAVE LINE NUMBERS IN THE LOG FILE:
  - Go to Project/Options then set the Linker/File map setting to "Detailed"

  Version 1.0 - April 14, 2011
    - Initial Release

  Version 1.18
    - Kylix support

}
program LoggingTest;

{$AppType console}

{$I Synopse.inc} // all expected conditionals

{$ifndef DELPHI5OROLDER} // mORMot.pas doesn't compile under Delphi 5
  {$define WITHMORMOT}
{$endif}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  {$ifdef MSWINDOWS}
  Windows,
  ComObj,
  {$endif}
  SysUtils,
  {$ifdef WITHMORMOT}
  mORMot,
  {$endif}
  SynCommons,
  SynLog;

type
  /// a class just to show how methods are handled
  TTestLogClass = class
  protected
    procedure TestLog;
  end;

  /// a custom exception used to show how Delphi exception are handled and
  // can be ignored on request
  ECustomException = class(Exception);

{$ifdef WITHMORMOT}
  TSQLRecordPeople = class(TSQLRecord)
  private
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;
{$else}
  // mORMot.pas doesn't compile under Delphi 5 (yet)
  TSQLLog = TSynLog;
{$endif}

var
  TestLevel: TSynLogInfo = high(TSynLogInfo);

procedure TTestLogClass.TestLog;
var ILog: ISynLog;
    S: TSynLogInfos;
begin
  ILog := TSQLLog.Enter(self);
  // do some stuff
  ILog.Log(sllCustom1);
  ILog.Log(sllInfo,'TestLevel',TypeInfo(TSynLogInfo),TestLevel,nil);
  ILog.Log(sllInfo,'set',TypeInfo(TSynLogInfos),S,nil);
  ILog.Log(sllDebug,ILog.Instance);
  ILog.Log(sllExceptionOS, 'Some error with stacktrace from %', [ExeVersion.ProgramName], self);
  if TestLevel=low(TestLevel) then
    TTestLogClass(nil).ClassName; // will raise an access violation
  dec(TestLevel);
  TestLog;
end;

procedure TestLogProc;
var ILog: ISynLog;
begin
  ILog := TSQLLog.Enter;
  ILog.Log(sllDebug,'GarbageCollector',GarbageCollector);
  ILog.Log(sllDebug,GarbageCollector);
end;


procedure TestsLog;

{$ifdef WITHMORMOT}
  procedure TestPeopleProc;
  var People: TSQLRecordPeople;
      Log: ISynLog;
  begin
    Log := TSQLLog.Enter;
    People := TSQLRecordPeople.Create;
    try
      People.IDValue := 16;
      People.FirstName := 'Louis';
      People.LastName := 'Croivebaton';
      People.YearOfBirth := 1754;
      People.YearOfDeath := 1793;
      Log.Log(sllInfo,People);
    finally
      People.Free;
    end;
  end;
{$endif}

  procedure Proc2(n1, n2: Integer); forward; // test nested

  procedure Proc1(n1, n2: Integer);
  begin
    if n1 = 0 then
    try
      TTestLogClass(nil).ClassName; // will raise an access violation
    except
      on E: Exception do
        TSQLLog.Add.Log(sllInfo,'^^^^^^^^ recursion test Proc1',e);
    end else
    Proc2(n1 - 1, n2);
  end;

  procedure Proc2(n1, n2: Integer);
  begin
    if n2 = 0 then
      try
        TTestLogClass(nil).ClassName; // will raise an access violation
      except
        on E: Exception do
          TSQLLog.Add.Log(sllInfo,'^^^^^^^^ recursion test Proc2',e);
      end else
      Proc1(n1, n2 - 1);
  end;

var i: integer;
    f: system.TextFile;
    info: TSynLogExceptionInfoDynArray;
begin
  i := 1; // we need this to circumvent the FPC compiler :)
  // first, set the TSQLLog family parameters
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    //Level := [sllException,sllExceptionOS];
    //PerThreadLog := true;
    //HighResolutionTimeStamp := true;
    //AutoFlushTimeOut := 5;
    OnArchive := EventArchiveSynLZ;
    //OnArchive := EventArchiveZip;
    ArchiveAfterDays := 1; // archive after one day
  end;
  TSQLLog.Add.Log(sllInfo,'Starting');
  writeln(' try some low-level common exceptions');
  try
    dec(i);
    if 10 div i=0 then; // will raise EDivByZero
  except
    on E: exception do
      TSQLLog.Add.Log(sllStackTrace,'^^^^^^^^ the first sample, divide by 0',E);
  end;
  try
    closefile(f);
    readln(f); // will raise EIOError (no console is available to read from)
  except
    on E: exception do
      TSQLLog.Add.Log(sllStackTrace,'^^^^^^^^ the next sample, I/O error',E);
  end;
  writeln(' try EAccessViolation in nested procedure calls (see stack trace)');
  Proc1(5,7);
  Proc2(7,5);
  writeln(' try a method recursive call, with an EAccessViolation raised within');
  with TTestLogClass.Create do
  try
    try
      TestLog;
    except
      on Exception do; // just ignore now
    end;
  finally
    Free;
  end;
  writeln(' try a procedure call with Enter/Auto-Leave');
  TestLogProc;
  {$ifdef WITHMORMOT}
  writeln(' try a procedure call with Enter/Auto-Leave and a TSQLRecordPeople logging');
  TestPeopleProc;
  {$endif}
  writeln(' try a custom Delphi exception');
  try
    raise ECustomException.Create('Test exception'); // logged to TSQLLog
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  custom exception type',E);
  end;
  writeln(' try a custom Delphi exception after been marked as to be ignored');
  TSQLLog.Family.ExceptionIgnore.Add(ECustomException);
  try
    raise ECustomException.Create('Test exception');
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  nothing should be logged just above',E);
  end;
  writeln(' try an Exception with message='' - see ticket [388c2768b6]');
  try
    raise Exception.Create('');
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  Exception.Message=""',E);
  end;
  writeln(' try an ESynException');
  try
    raise ESynException.CreateUTF8('testing %.CreateUTF8',[ESynException]);
  except
    on E: ESynException do begin
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  ESynException',E);
      {$ifdef WITHMORMOT}
      TSQLLog.Add.Log(sllDebug,'ObjectToJSONDebug(E) = %',[ObjectToJSONDebug(E)],E);
      {$endif}
      TSQLLog.Add.Log(sllDebug,'FindLocation(E) = %',[TSynMapFile.FindLocation(E)],E);
    end;
  end;
  {$ifdef MSWINDOWS}
  writeln(' try a EOleSysError, as if it was triggered from the .Net CLR');
  try
    raise EOleSysError.Create('Test',HRESULT($80004003),0);
  except
    on E: Exception do
      TSQLLog.Add.Log(sllInfo,'^^^^^^^^  should be recognized as NullReferenceException',E);
  end;
  {$endif}
  writeln('GetLastExceptions = ');
  GetLastExceptions(info);
  for i := 0 to high(info) do
    writeln(ToText(info[i]));
end;

begin
  TestsLog;
  writeln('------ finished');
end.

