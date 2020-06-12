/// test access to a local MongoDB instance
program MongoDBTests;

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

uses
  {$I SynDprUses.inc}
  SynCommons,
  SynLog,
  mORMot,
  MongoDBTestCases;

begin
  {$ifdef WITHLOG}
  // SQLite3Log.Family.Level := LOG_VERBOSE;
  TSynLogTestLog := SQLite3Log;
  {$endif}
  with TTestMongoDB.Create do
  try
    Run;
    {$ifdef MSWINDOWS}
    readln;
    {$endif}
  finally
    Free;
  end;
  {$ifdef FPC_X64MM}
  WriteHeapStatus(#13#10'Memory Usage Report:', 16, 12, {flags=}true);
  {$endif FPC_X64MM}
end.
