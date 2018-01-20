/// manage (install/start/stop/uninstall) HttpService sample
program HttpServiceSetup;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Windows,
  Classes,
  SysUtils,
  SynCommons,
  SynLog,
  mORMot,
  mORMotService;

/// if we will run the service with administrator rights
// - otherwise, ensure you registered the URI /root:8080
{$R ..\..\..\VistaAdm.res}

const
  HTTPSERVICE_NAME = 'mORMotHttpServerService';
  HTTPSERVICE_DISPLAYNAME = 'mORMot Http Server Service';
  HTTPSERVICE_DESCRIPTION = 'This is a sample mORMot HTTP Server running as Service';

begin
  ServiceLog := TSQLLog; // explicitely enable logging
  ServiceLog.Family.Level := LOG_VERBOSE;
  TServiceController.CheckParameters(ExeVersion.ProgramFilePath+'HttpService.exe',
    HTTPSERVICE_NAME,HTTPSERVICE_DISPLAYNAME,HTTPSERVICE_DESCRIPTION);
  TSQLLog.Add.Log(sllTrace,'Quitting command line');
  with TServiceController.CreateOpenService('','',HTTPSERVICE_NAME) do
  try
    State; // just to log the service state after handling the /parameters
  finally
    Free;
  end;
end.
