/// simple SOA client using a callback for long process ending notification
program Project31LongWorkClient;

uses
  {$I SynDprUses.inc} // use FastMM4 on older versions of Delphi
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  mORMotHttpClient,
  Project31LongWorkCallbackInterface in 'Project31LongWorkCallbackInterface.pas';

{$APPTYPE CONSOLE}

type
  TLongWorkCallback = class(TInterfacedCallback,ILongWorkCallback)
  protected
    procedure WorkFinished(const workName: string; timeTaken: integer);
    procedure WorkFailed(const workName, error: string);
  end;

procedure TLongWorkCallback.WorkFailed(const workName, error: string);
begin
  TextColor(ccLightRed);
  writeln(#13'Received callback WorkFailed(',workName,') with message "',error,'"');
  TextColor(ccLightGray);
  write('>');
end;

procedure TLongWorkCallback.WorkFinished(const workName: string;
  timeTaken: integer);
begin
  TextColor(ccLightBlue);
  writeln(#13'Received callback WorkFinished(',workName,') in ',timeTaken,'ms');
  TextColor(ccLightGray);
  write('>');
end;


procedure Run;
var Client: TSQLHttpClientWebsockets;
    workName: string;
    Service: ILongWorkService;
    callback: ILongWorkCallback;
begin
  writeln('Connecting to the local Websockets server...');
  Client := TSQLHttpClientWebsockets.Create('127.0.0.1','8888',TSQLModel.Create([]));
  try
    Client.Model.Owner := Client;
    Client.WebSocketsUpgrade(PROJECT31_TRANSMISSION_KEY);
    if not Client.ServerTimeStampSynchronize then
      raise EServiceException.Create(
        'Error connecting to the server: please run Project31LongWorkServer.exe');
    Client.ServiceDefine([ILongWorkService],sicShared);
    if not Client.Services.Resolve(ILongWorkService,Service) then
      raise EServiceException.Create('Service ILongWorkService unavailable');
    TextColor(ccWhite);
    writeln('Please type a work name, then press [Enter]');
    writeln('Enter a void line to quit');
    callback := TLongWorkCallback.Create(Client,ILongWorkCallback);
    try
      repeat
        TextColor(ccLightGray);
        write('>');
        readln(workName);
        if workName='' then
          break;
        Service.StartWork(workName,callback);
        TextColor(ccBrown);
        writeln('Service.TotalWorkCount=',Service.TotalWorkCount);
      until false;
    finally
      callback := nil;
      Service := nil; // release the service local instance BEFORE Client.Free
    end;
  finally
    Client.Free;
  end;
end;



begin
  try
    Run;
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
end.