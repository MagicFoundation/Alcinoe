program AppServer;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}
uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFNDEF FPC}
  PDGZlib in '..\..\src\PDGZlib.pas',
{$ENDIF}
  PDGService in '..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\src\PDGSocketStub.pas',
  TestLib_Server in 'TestLib_Server.pas',
  TestLib_Intf in 'TestLib_Intf.pas',
  TestLib in 'TestLib.pas';

begin
  Application.DisplayName := 'Progdigy Application Server';
{$IFNDEF CONSOLEAPP}
  Application.Name := 'PDGAPPSRV';
  Application.Dependencies := '';
  Application.ServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
{$ENDIF}
  Application.Run;
end.
