{ This file is generated automaticaly, do not modify }
unit TestLib_Client;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes, {$IFDEF FPC}sockets{$ELSE}WinSock{$ENDIF}, PDGUtils, TestLib_Intf;

type
  TMyObjectClient = class(TInterfacedObject, IMyObject)
  private
    FSocketHandle: longint;  
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
  public
    property SocketHandle: longint read FSocketHandle;
    constructor Create(const server: string; port: word); virtual;
    destructor Destroy; override;
    procedure ExecuteScript(const script: string; out data: TMemoryStream); virtual; stdcall;
  end;

  TMyObject2Client = class(TInterfacedObject, IMyObject2)
  private
    FSocketHandle: longint;  
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
  public
    property SocketHandle: longint read FSocketHandle;
    constructor Create(const server: string; port: word); virtual;
    destructor Destroy; override;
    function GetString: string; virtual; stdcall;
  end;

implementation
uses SysUtils;

{ TMyObjectClient }

function TMyObjectClient.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := CompressStream(stream, socket);
end;

function TMyObjectClient.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := DecompressStream(socket, stream);
end;

constructor TMyObjectClient.Create(const server: string; port: word);
var
  p: PHostEnt;
  Address: TSockAddr;
  uid: TGUID;
  SO_True: Integer;
begin
  SO_True := -1;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PChar(@SO_True), SizeOf(SO_True));
  Address.sin_family := AF_INET;
  Address.sin_port := htons(port);
  p := gethostbyname(PChar(server));
  if p = nil then
    raise Exception.Create('Can''t find remote host.') else
    Address.sin_addr.S_addr := PInteger(p^.h_addr^)^;
  if connect(FSocketHandle, Address, sizeof(Address)) = SOCKET_ERROR then
    raise Exception.Create('Can''t connect to remote host.');
  uid := CLSID_MyObject;
  send(FSocketHandle, uid, sizeOf(uid), 0);
end;

destructor TMyObjectClient.Destroy;
begin
  closesocket(FSocketHandle);
  inherited;
end;

procedure TMyObjectClient.ExecuteScript(const script: string; out data: TMemoryStream);
var
  fn_stream: TPooledMemoryStream;
  fn_id: integer;
  fn_error: string;
  script_len: integer;
  data_len: integer;
begin
  fn_stream := TPooledMemoryStream.Create;
  try
    fn_id := 0;
    fn_stream.write(fn_id, sizeof(fn_id));
    script_len := Length(script);
    fn_stream.Write(script_len, sizeof(script_len));
    fn_stream.Write(PChar(script)^, script_len);
    fn_stream.Seek(0, soFromBeginning);
    if not EncodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Size := 0;
    if not DecodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Seek(0, soFromBeginning);
    fn_stream.read(data_len, sizeof(data_len));
    data.size := data_len;
    data.Seek(0, soFromBeginning);
    fn_stream.read(data.Memory^, data_len);
    fn_error := fn_stream.ReadString;
    if not(fn_error = '') then
      raise ERemoteError.Create(fn_error);
  finally
    fn_stream.Free;
  end;
end;

{ TMyObject2Client }

function TMyObject2Client.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := CompressStream(stream, socket);
end;

function TMyObject2Client.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := DecompressStream(socket, stream);
end;

constructor TMyObject2Client.Create(const server: string; port: word);
var
  p: PHostEnt;
  Address: TSockAddr;
  uid: TGUID;
  SO_True: Integer;
begin
  SO_True := -1;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PChar(@SO_True), SizeOf(SO_True));
  Address.sin_family := AF_INET;
  Address.sin_port := htons(port);
  p := gethostbyname(PChar(server));
  if p = nil then
    raise Exception.Create('Can''t find remote host.') else
    Address.sin_addr.S_addr := PInteger(p^.h_addr^)^;
  if connect(FSocketHandle, Address, sizeof(Address)) = SOCKET_ERROR then
    raise Exception.Create('Can''t connect to remote host.');
  uid := CLSID_MyObject2;
  send(FSocketHandle, uid, sizeOf(uid), 0);
end;

destructor TMyObject2Client.Destroy;
begin
  closesocket(FSocketHandle);
  inherited;
end;

function TMyObject2Client.GetString: string;
var
  fn_stream: TPooledMemoryStream;
  fn_id: integer;
  fn_error: string;
  Result_len: integer;
begin
  fn_stream := TPooledMemoryStream.Create;
  try
    fn_id := 0;
    fn_stream.write(fn_id, sizeof(fn_id));
    fn_stream.Seek(0, soFromBeginning);
    if not EncodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Size := 0;
    if not DecodeStream(fn_stream, FSocketHandle) then
      raise ERemoteError.Create('Communication error.');
    fn_stream.Seek(0, soFromBeginning);
    fn_stream.read(Result_len, sizeof(Result_len));
    SetLength(Result, Result_len);
    fn_stream.Read(PChar(Result)^, Result_len);
    fn_error := fn_stream.ReadString;
    if not(fn_error = '') then
      raise ERemoteError.Create(fn_error);
  finally
    fn_stream.Free;
  end;
end;

{$IFNDEF FPC}
var
  Data: TWSAData ;
initialization
  WSAStartup($0202, Data);
finalization
  WSACleanup;
{$ENDIF}
end.
