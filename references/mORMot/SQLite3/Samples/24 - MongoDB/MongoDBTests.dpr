/// test access to a local MongoDB instance
program MongoDBTests;

{$APPTYPE CONSOLE}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  {$I SynDprUses.inc}
  SynCommons,
  SynLog,
  mORMot,
  MongoDBTestCases;

begin
  {$ifdef WITHLOG}
  //SQLite3Log.Family.Level := LOG_VERBOSE;
  TSynLogTestLog := SQLite3Log;
  {$endif}
  with TTestMongoDB.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
