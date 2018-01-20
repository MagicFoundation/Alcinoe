program Project31WinHTTPEchoServer;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  Windows,
  SynZip,
  SynCrtSock,
  SynCommons;

{$APPTYPE CONSOLE}

type
  TSimpleWebsocketServer = class
  private
    fServer: THttpApiWebSocketServer;
//    fProtocols: THttpApiWebSocketServerProtocolDynArray;
    function onHttpRequest(Ctxt: THttpServerRequest): cardinal;
    function onAccept(Ctxt: THttpServerRequest; var Conn: THttpApiWebSocketConnection): Boolean;
    procedure onConnect(const Conn: THttpApiWebSocketConnection );
    procedure onMessage(const Conn: THttpApiWebSocketConnection; aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);
    procedure onDisconnect(const Conn: THttpApiWebSocketConnection ; aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG);
  public
    constructor Create;
    destructor Destroy; override;
  end;
{ TSimpleWebsocketServer }

constructor TSimpleWebsocketServer.Create;
begin
  fServer := THttpApiWebSocketServer.Create(false, 8, 10000);
  fServer.AddUrl('','8888', False, 'localhost');
  fServer.AddUrlWebSocket('whatever', '8888', False, 'localhost');
  // ManualFragmentManagement = false - so Server will join all packet fragments
  // automatically and call onMessage with full message content
  fServer.RegisterProtocol('meow', False, onAccept, onMessage, onConnect, onDisconnect);
  fServer.RegisterCompress(CompressDeflate);
  fServer.OnRequest := onHttpRequest;
  fServer.Clone(8);
end;

destructor TSimpleWebsocketServer.Destroy;
begin
  fServer.Free;
  inherited;
end;

function TSimpleWebsocketServer.onAccept(Ctxt: THttpServerRequest; var Conn: THttpApiWebSocketConnection): Boolean;
begin
// You can check some Ctxt parameters here
  Result := true;
end;

procedure TSimpleWebsocketServer.onConnect(const Conn: THttpApiWebSocketConnection);
begin
  Writeln('Connected ', Conn.index);
end;

procedure TSimpleWebsocketServer.onDisconnect(const Conn: THttpApiWebSocketConnection;
  aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG);
var
  str: RawUTF8;
begin
  SetString(str, pUtf8Char(aBuffer), aBufferSize);

  Writeln('Disconnected ', Conn.index,' ',aStatus,' ',str);
end;

function TSimpleWebsocketServer.onHttpRequest(Ctxt: THttpServerRequest): cardinal;
begin
  Writeln('HTTP request to ', Ctxt.URL);
  if Ctxt.URL = '/' then
    Ctxt.OutContent := 'Project31SimpleEchoServer.html'
  else if Ctxt.URL = '/favicon.ico' then
     Ctxt.OutContent := 'favicon.ico';
  Ctxt.OutContentType := HTTP_RESP_STATICFILE;
  Result := 200;
end;

procedure TSimpleWebsocketServer.onMessage(const Conn: THttpApiWebSocketConnection;
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);
var
  str: RawUTF8;
begin
  Conn.Send(aBufferType, aBuffer, aBufferSize);
//  Conn.Protocol.Send(Conn.index, aBufferType, aBuffer, aBufferSize); //also work
  SetString(str, pUtf8Char(aBuffer), aBufferSize);
  if aBufferType = WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE then
    Writeln('UTF8 message from ', Conn.index, ' ',str)
  else if aBufferType = WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE then
    Writeln('UTF8 fragment from ', Conn.index, ' ',str)
  else if (aBufferType = WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE)
    or (aBufferType = WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) then
    Writeln(aBufferType, ' from ', Conn.index, ' of length ', aBufferSize)
  else begin
    Writeln(aBufferType, ' from ', Conn.index, ' ',str);
  end;
end;

var
  _Server: TSimpleWebsocketServer;
  s: string;
  MsgBuffer: RawUTF8;
  CloseReasonBuffer: RawUTF8;
begin
  MsgBuffer := 'Manual Message!';
  CloseReasonBuffer := 'Manual Close!';
  try
    _Server := TSimpleWebsocketServer.Create;
    try
      Writeln('WebSocket server is now listen on ws://localhost:8888/whatever');
      Writeln('HTTP server is now listen on http://localhost:8888/');
      Writeln(' - point your browser to http://localhost:8888/ for initial page');
      Writeln(' - type "close connectionID" to close existing webSocket connection');
      Writeln(' - type "send  connectionID" to send text to specified WebCocket');
      Writeln(' - press [Enter] to quit');
      repeat
        Readln(s);
        if Pos('close ', s) = 1 then begin
          s := SysUtils.Trim(Copy(s, 7, Length(s)));
          _Server.fServer.Protocols[0].Close(StrToIntDef(s, -1), WEB_SOCKET_SUCCESS_CLOSE_STATUS,
            Pointer(CloseReasonBuffer), length(CloseReasonBuffer));
        end else if Pos('send ', s) = 1 then begin
          s := SysUtils.Trim(Copy(s, 6, Length(s)));
          _Server.fServer.Protocols[0].Send(StrToIntDef(s, -1), WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE,
             Pointer(MsgBuffer), length(MsgBuffer));
        end;
      until s = '';
    finally
      _Server.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.