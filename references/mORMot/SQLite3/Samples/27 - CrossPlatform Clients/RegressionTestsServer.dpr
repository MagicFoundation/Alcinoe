program RegressionTestsServer;

{$i Synopse.inc} // define e.g. HASINLINE

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}
  PeopleServer,
  SynCommons,
  SynLog,
  mORMot,
  SysUtils;

begin
  // define the log level
  if false then
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE; // LOG_STACKTRACE;
    //EchoToConsole := LOG_VERBOSE; // events to the console
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  Writeln('Running Cross-Platform mORMot Server on port 888'#13#10+
          'Using TSQLRestServerAuthenticationDefault'#13#10#10+
          'You can now run FPC or SMS client applications'#13#10':)');
  StartServer(psaDefault);
  writeln(#13#10'Press [Enter] to quit');
  readln;
  StopServer;
end.

