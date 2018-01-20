/// simple SOA server using a callback for long process ending notification
program Project31LongWorkServer;

uses
  {$I SynDprUses.inc} // use FastMM4 on older versions of Delphi
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  SynBidirSock,
  mORMotHttpServer,
  Project31LongWorkCallbackInterface in 'Project31LongWorkCallbackInterface.pas';

{$APPTYPE CONSOLE}

type
  TLongWorkServiceThread = class(TThread)
  protected
    fCallback: ILongWorkCallback;
    fWorkName: string;
    procedure Execute; override;
  public
    constructor Create(const workName: string; const callback: ILongWorkCallback);
  end;

  TLongWorkService = class(TInterfacedObject,ILongWorkService)
  protected
    fTotalWorkCount: Integer;
  public
    procedure StartWork(const workName: string; const onFinish: ILongWorkCallback);
    function TotalWorkCount: Integer;
  end;

procedure TLongWorkService.StartWork(const workName: string;
  const onFinish: ILongWorkCallback);
begin
  InterlockedIncrement(fTotalWorkCount);
  TLongWorkServiceThread.Create(workName,onFinish);
end;

function TLongWorkService.TotalWorkCount: Integer;
begin
  result := fTotalWorkCount;
end;

constructor TLongWorkServiceThread.Create(const workName: string;
  const callback: ILongWorkCallback);
begin
  inherited Create(false);
  fCallback := Callback;
  fWorkName := workName;
  FreeOnTerminate := true;
end;

procedure TLongWorkServiceThread.Execute;
var tix: Int64;
begin
  TSQLLog.Add.Log(sllInfo,'%.Execute(%) started',[self,fWorkName]);
  tix := GetTickCount64;
  Sleep(5000+Random(1000)); // some hard work
  if Random(100)>20 then
    fCallback.WorkFinished(fWorkName,GetTickCount64-tix) else
    fCallback.WorkFailed(fWorkName,'expected random failure');
  TSQLLog.Add.Log(sllInfo,'%.Execute(%) notified',[self,fWorkName]);
end;

procedure Run;
var HttpServer: TSQLHttpServer;
    Server: TSQLRestServerFullMemory;
begin
  Server := TSQLRestServerFullMemory.CreateWithOwnModel([]);
  try
    Server.CreateMissingTables;
    Server.ServiceDefine(TLongWorkService,[ILongWorkService],sicShared).
      ByPassAuthentication := true;
    HttpServer := TSQLHttpServer.Create('8888',[Server],'+',useBidirSocket);
    try
      HttpServer.WebSocketsEnable(Server,PROJECT31_TRANSMISSION_KEY).
        Settings.SetFullLog; // full verbose logs for this demo
      TextColor(ccLightGreen);
      writeln('WebSockets Long Work Server running on localhost:8888'#13#10);
      TextColor(ccWhite);
      writeln('Please compile and run Project31LongWorkClient.exe'#13#10);
      TextColor(ccLightGray);
      writeln('Press [Enter] to quit'#13#10);
      TextColor(ccCyan);
      readln;
    finally
      HttpServer.Free;
    end;
  finally
    Server.Free;
  end;
end;


begin
  with TSQLLog.Family do begin // enable logging to file and to console
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOnFile;
  end;
  WebSocketLog := TSQLLog; // verbose log of all WebSockets activity
  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.