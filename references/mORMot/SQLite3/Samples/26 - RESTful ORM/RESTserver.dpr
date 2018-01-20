/// RESTful ORM server
program RESTserver;

{$APPTYPE CONSOLE}

// first line after uses clause should be  {$I SynDprUses.inc}  for FastMM4
uses
  {$I SynDprUses.inc}
  Classes,
  SysUtils,
  SynCommons,
  SynLog,
  mORMot,
  SynCrtSock,
  mORMotHTTPServer,
  RESTData,
  RESTServerClass;

var ORMServer: TNoteServer;
    HTTPServer: TSQLHttpServer;
begin
  ORMServer := TNoteServer.Create(ExeVersion.ProgramFilePath+'data','root');
  try
    TSQLLog.Family.EchoToConsole := LOG_VERBOSE;
    HTTPServer := TSQLHttpServer.Create(HTTP_PORT,[ORMServer]);
    try
      sleep(300); // let the HTTP server start (for the console log refresh)
      writeln(#13#10'Background server is running at http://localhost:888'#13#10+
              #13#10'Press [Enter] to close the server.');
      ConsoleWaitForEnterKey;
    finally
      HTTPServer.Free;
    end;
  finally
    ORMServer.Free;
  end;
end.
