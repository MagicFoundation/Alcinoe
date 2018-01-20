program DDDPersistence;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // includes FastMM4
  SynLog,
  mORMot,
  SysUtils,
  SynCommons,
  DDDPersistenceMain;

begin
  TSynLogTestLog := SQLite3Log; // share the same log file with the whole mORMot
  SQLite3Log.Family.HighResolutionTimeStamp := true;
  TTestSuit.RunAsConsole('Automated Tests',LOG_VERBOSE);
end.

