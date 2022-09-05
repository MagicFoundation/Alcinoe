program AppServer;

{$IFDEF CONSOLEAPP}
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
uses
  PDGService in '..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\src\PDGSocketStub.pas',
  WebServer in 'WebServer.pas',
  PDGHTTPStub in '..\..\src\PDGHTTPStub.pas',
  myapp_controller in 'myapp_controller.pas',
  mypool in 'mypool.pas',
  myapp_view in 'myapp_view.pas',
  PDGUtils in '..\..\src\PDGUtils.pas';

begin
  Application.Name := 'PDGWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
