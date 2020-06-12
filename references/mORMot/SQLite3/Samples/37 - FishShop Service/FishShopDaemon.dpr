program FishShopDaemon;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons,          // framework core
  SynTable,
  SynLog,              // logging features
  mORMot,              // RESTful server & ORM
  SynSQLite3Static,    // staticaly linked SQLite3 engine
  mORMotHttpServer,    // HTTP server for RESTful server
  ServFishShopTypes,   // definitions shared by Server and Clients
  ServFishShopMain;    // the main implementation unit of our daemon

var
  daemon: TFishShopDaemon;
begin
  // set logging abilities
  SQLite3Log.Family.Level := LOG_VERBOSE;
  //SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
  SQLite3Log.Family.PerThreadLog := ptIdentifiedInOnFile;
  // initialize the daemon
  daemon := TFishShopDaemon.Create(TFishShopSettings, ExeVersion.ProgramFilePath, '', '');
  try
    daemon.CommandLine;
  finally
    daemon.Free;
  end;
end.

