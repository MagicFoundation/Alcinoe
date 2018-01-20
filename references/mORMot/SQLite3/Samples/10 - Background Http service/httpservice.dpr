/// implements a background Service serving HTTP pages
program HttpService;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Windows,
  Classes,
  SysUtils,
  SynCommons,
  SynLog,
  mORMotService,
  mORMot,
  mORMotSQLite3, SynSQLite3Static,
  mORMotHTTPServer,
  SampleData in '..\01 - In Memory ORM\SampleData.pas';



/// if we will run the service with administrator rights
// - otherwise, ensure you registered the URI /root:8080
{$R ..\..\..\VistaAdm.res}

type
  /// class implementing the background Service
  TSQLite3HttpService = class(TServiceSingle)
  public
    /// the associated database model
    Model: TSQLModel;
    /// the associated DB
    DB: TSQLRestServerDB;
    /// the background Server processing all requests
    Server: TSQLHttpServer;

    /// event triggered to start the service
    // - e.g. create the Server instance
    procedure DoStart(Sender: TService);
    /// event triggered to stop the service
    // - e.g. destroy the Server instance
    procedure DoStop(Sender: TService);

    /// initialize the background Service
    constructor Create; reintroduce;
    /// launch as Console application
    constructor CreateAsConsole; reintroduce;
    /// release memory
    destructor Destroy; override;
  end;


const
  HTTPSERVICENAME = 'mORMotHttpServerService';
  HTTPSERVICEDISPLAYNAME = 'mORMot Http Server Service';


{ TSQLite3HttpService }

constructor TSQLite3HttpService.Create;
begin
  inherited Create(HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
  TSQLLog.Family.Level := LOG_VERBOSE;
  TSQLLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSQLLog.Enter(self);
  OnStart := {$ifdef FPC}@{$endif}DoStart;
  OnStop := {$ifdef FPC}@{$endif}DoStop;
  OnResume := {$ifdef FPC}@{$endif}DoStart; // trivial Pause/Resume actions
  OnPause := {$ifdef FPC}@{$endif}DoStop;
end;

constructor TSQLite3HttpService.CreateAsConsole;
begin
  // manual switch to console mode
  AllocConsole;
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_STACKTRACE;
  end;
end;

destructor TSQLite3HttpService.Destroy;
begin
  TSQLLog.Enter(self);
  if Server<>nil then
    DoStop(nil); // should not happen
  inherited Destroy;
end;

procedure TSQLite3HttpService.DoStart(Sender: TService);
begin
  TSQLLog.Enter(self);
  if Server<>nil then
    DoStop(nil); // should never happen
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(ExeVersion.ProgramFileName,'.db3'));
  DB.CreateMissingTables;
  Server := TSQLHttpServer.Create('8080',[DB],'+',useHttpApiRegisteringURI);
  TSQLLog.Add.Log(sllInfo,'Server % started by %',[Server.HttpServer,Server]);
end;

procedure TSQLite3HttpService.DoStop(Sender: TService);
begin
  TSQLLog.Enter(self);
  if Server=nil then
    exit;
  TSQLLog.Add.Log(sllInfo,'Server % stopped by %',[Server.HttpServer,Server]);
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

begin
  if (ParamCount<>0) and
     (SameText(ParamStr(1),'-c') or SameText(ParamStr(1),'/c')) then
    with TSQLite3HttpService.CreateAsConsole do
    try
      DoStart(nil);
      TextColor(ccLightGray);
      writeln(#10'Background server is running.'#10);
      writeln('Press [Enter] to close the server.'#10);
      ConsoleWaitForEnterKey; // ReadLn if you do not use main thread execution
      exit;
    finally
      Free;
    end else
    with TSQLite3HttpService.Create do
    try
      // launches the registered Services execution = do all the magic
      ServicesRun;
    finally
      Free;
    end;
end.
