/// simple SOA server using callbacks for a chat room
program Project31ChatServer;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older versions of Delphi
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  SynBidirSock,
  mORMotHttpServer,
  Project31ChatCallbackInterface in 'Project31ChatCallbackInterface.pas';

type
  TChatService = class(TInterfacedObject,IChatService)
  protected
    fConnected: array of IChatCallback;
  public
    procedure Join(const pseudo: string; const callback: IChatCallback);
    procedure BlaBla(const pseudo,msg: string);
    procedure CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
  end;

procedure TChatService.Join(const pseudo: string;
  const callback: IChatCallback);
begin
  InterfaceArrayAdd(fConnected,callback);
end;

procedure TChatService.BlaBla(const pseudo,msg: string);
var i: integer;
begin
  for i := high(fConnected) downto 0 do // downwards for InterfaceArrayDelete()
    try
      fConnected[i].NotifyBlaBla(pseudo,msg);
    except
      InterfaceArrayDelete(fConnected,i); // unsubscribe the callback on failure
    end;
end;

procedure TChatService.CallbackReleased(const callback: IInvokable; const interfaceName: RawUTF8);
begin
  if interfaceName='IChatCallback' then
    InterfaceArrayDelete(fConnected,callback);
end;


procedure Run;
var HttpServer: TSQLHttpServer;
    Server: TSQLRestServerFullMemory;
begin
  Server := TSQLRestServerFullMemory.CreateWithOwnModel([]);
  try
    Server.CreateMissingTables;
    Server.ServiceDefine(TChatService,[IChatService],sicShared).
      SetOptions([],[optExecLockedPerInterface]). // thread-safe fConnected[]
      ByPassAuthentication := true;
    HttpServer := TSQLHttpServer.Create('8888',[Server],'+',useBidirSocket);
    try
      HttpServer.WebSocketsEnable(Server,PROJECT31_TRANSMISSION_KEY).
        Settings.SetFullLog; // full verbose logs for this demo
      TextColor(ccLightGreen);
      writeln('WebSockets Chat Server running on localhost:8888'#13#10);
      TextColor(ccWhite);
      writeln('Please compile and run Project31ChatClient.exe'#13#10);
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
