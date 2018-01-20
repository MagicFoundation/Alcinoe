program ServBook;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // includes FastMM4
  SysUtils,
  SynLog,
  mORMot,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotService,     // cross-platform service/daemon skeleton with settings
  // domain
  DomConferenceTypes in '..\dom\DomConferenceTypes.pas',
  DomConferenceInterfaces in '..\dom\DomConferenceInterfaces.pas',
  DomConferenceDepend in '..\dom\DomConferenceDepend.pas',
  DomConferenceServices in '..\dom\DomConferenceServices.pas',
  // infrastructure
  InfraConferenceRepository in '..\infra\InfraConferenceRepository.pas',
  // servers
  ServBookMain in '..\serv\ServBookMain.pas';

type
  TBookSettings = class(TSynDaemonSettings)
  private
    fProcess: TBookProcessSettings;
  published
    property Process: TBookProcessSettings read fProcess;
  end;

  TBookDaemon = class(TSynDaemon)
  protected
    fProcess: TBookProcess;
  public
    procedure Start; override;
    procedure Stop; override;
  end;


{ TBookDaemon }

procedure TBookDaemon.Start;
begin
  if fProcess = nil then
    fProcess := TBookProcess.Create((fSettings as TBookSettings).Process);
end;

procedure TBookDaemon.Stop;
begin
  FreeAndNil(fProcess);
end;

begin
  with TBookDaemon.Create(TBookSettings, '', '', '') do
  try
    CommandLine(true);
  finally
    Free;
  end;
end.
