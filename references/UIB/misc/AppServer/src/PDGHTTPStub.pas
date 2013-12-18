(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit PDGHTTPStub;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
{$I PDGAppServer.inc}
interface
uses
  PDGSocketStub,
  {$IFDEF FPC}sockets,{$ELSE}Winsock,{$ENDIF}
  PDGUtils, classes, superobject, uiblib;

type

  THttpMethod = (mUNKNOW, mOPTIONS, mGET, mHEAD, mPOST, mPUT, mDELETE, mTRACE, mCONNECT);

  TConnectionField = (conClose, conKeepAlive);

  THTTPMessage = class(TSuperObject)
  private
    FContent: TPooledMemoryStream;
    function GetContentString: SOString;
  public
    property Content: TPooledMemoryStream read FContent;
    property ContentString: SOString read GetContentString;

    constructor Create(jt: TSuperType = stObject); override;
    destructor Destroy; override;

    procedure Clear(all: boolean = false); override;
  end;

  THTTPStub = class(TSocketStub)
  private
    FRequest: THTTPMessage;
    FResponse: THTTPMessage;
    FMVC: ISuperObject;
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
  protected
    function CreateMVC: ISuperObject; virtual;
    function DecodeContent: boolean; virtual;
    procedure doBeforeProcessRequest(ctx: ISuperObject); virtual;
    procedure doAfterProcessRequest(ctx: ISuperObject); virtual;
    procedure ProcessRequest(ctx: ISuperObject); virtual;
  public
    property Request: THTTPMessage read FRequest;
    property Response: THTTPMessage read FResponse;
    property MVC: ISuperObject read FMVC;
    function Run: Cardinal; override;
    procedure WriteLine(str: RawByteString);
    procedure WriteString(const str: RawByteString);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    procedure SendString(const data: RawByteString);
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); override;
    destructor Destroy; override;
  end;

const
  CR = #13;
  LF = #10;
  SP = #32; // space
  HT = #9;  // backspace
  NL = #0;  // NULL
  SL = '/';
  PT = '.';
  CRLF = CR+LF;

const
(* default limit on bytes in Request-Line (Method+URI+HTTP-version) *)
  DEFAULT_LIMIT_REQUEST_LINE = 8190;
(* default limit on bytes in any one header field  *)
  DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;
(* default limit on number of request header fields *)
  DEFAULT_LIMIT_REQUEST_FIELDS = 100;

function HTTPInterprete(src: PSOChar; named: boolean = false; sep: SOChar = ';'; StrictSep: boolean = false; codepage: Integer = 0): ISuperObject;
function HTTPDecode(const AStr: string; codepage: Integer = 0): string;
function HttpResponseStrings(code: integer): RawByteString;

implementation
uses
{$IFDEF MSWINDOWS}
 windows,
{$ENDIF}
SysUtils, StrUtils {$ifdef madExcept}, madexcept {$endif}
{$IFDEF UNICODE}, AnsiStrings{$ENDIF}
{$IFDEF UNIX}, baseunix{$ENDIF}
;


function HttpResponseStrings(code: integer): RawByteString;
begin
  case code of
    100: Result := '100 Continue';
    101: Result := '101 Switching Protocols';

    200: Result := 'HTTP/1.1 200 OK';
    201: Result := 'HTTP/1.1 201 Created';
    202: Result := 'HTTP/1.1 202 Accepted';
    203: Result := 'HTTP/1.1 203 Non-Authoritative Information';
    204: Result := 'HTTP/1.1 204 No Content';
    205: Result := 'HTTP/1.1 205 Reset Content';
    206: Result := 'HTTP/1.1 206 Partial Content';

    300: Result := 'HTTP/1.1 300 Multiple Choices';
    301: Result := 'HTTP/1.1 301 Moved Permanently';
    302: Result := 'HTTP/1.1 302 Found';
    303: Result := 'HTTP/1.1 303 See Other';
    304: Result := 'HTTP/1.1 304 Not Modified';
    305: Result := 'HTTP/1.1 305 Use Proxy';
    306: Result := 'HTTP/1.1 306 unused';
    307: Result := 'HTTP/1.1 307 Temporary Redirect';
    400: Result := 'HTTP/1.1 400 Bad Request';
    401: Result := 'HTTP/1.1 401 Authorization Required';
    402: Result := 'HTTP/1.1 402 Payment Required';
    403: Result := 'HTTP/1.1 403 Forbidden';
    404: Result := 'HTTP/1.1 404 Not Found';
    405: Result := 'HTTP/1.1 405 Method Not Allowed';
    406: Result := 'HTTP/1.1 406 Not Acceptable';
    407: Result := 'HTTP/1.1 407 Proxy Authentication Required';
    408: Result := 'HTTP/1.1 408 Request Time-out';
    409: Result := 'HTTP/1.1 409 Conflict';
    410: Result := 'HTTP/1.1 410 Gone';
    411: Result := 'HTTP/1.1 411 Length Required';
    412: Result := 'HTTP/1.1 412 Precondition Failed';
    413: Result := 'HTTP/1.1 413 Request Entity Too Large';
    414: Result := 'HTTP/1.1 414 Request-URI Too Large';
    415: Result := 'HTTP/1.1 415 Unsupported Media Type';
    416: Result := 'HTTP/1.1 416 Requested Range Not Satisfiable';
    417: Result := 'HTTP/1.1 417 Expectation Failed';

    500: Result := 'HTTP/1.1 500 Internal Server Error';
    501: Result := 'HTTP/1.1 501 Method Not Implemented';
    502: Result := 'HTTP/1.1 502 Bad Gateway';
    503: Result := 'HTTP/1.1 503 Service Temporarily Unavailable';
    504: Result := 'HTTP/1.1 504 Gateway Time-out';
    505: Result := 'HTTP/1.1 505 HTTP Version Not Supported';
  else
    Result := 'HTTP/1.1 ' + RawByteString(inttostr(code));
  end;
end;


function HTTPInterprete(src: PSOChar; named: boolean; sep: SOChar; StrictSep: boolean; codepage: Integer): ISuperObject;
var
  P1: PSOChar;
  S: SOString;
  i: integer;
  obj, obj2, value: ISuperObject;
begin
    if named then
      Result := TSuperObject.create(stObject) else
      Result := TSuperObject.create(stArray);
    if not StrictSep then
      while {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']) do
        Inc(src);
    while src^ <> #0 do
    begin
      P1 := src;
      while ((not StrictSep and (src^ >= ' ')) or
            (StrictSep and (src^ <> #0))) and (src^ <> sep) do
        Inc(src);
      SetString(S, P1, src - P1);
      if codepage > 0 then
        S := HTTPDecode(S, codepage);
      if named then
      begin
        i := pos('=', S);
        // named
        if i > 1 then
        begin
          S[i] := #0;
          obj := Result[S];
          value := TSuperObject.ParseString(PSOChar(@S[i+1]), false);
          if value = nil then
            value := TSuperObject.Create(PSOChar(@S[i+1]));
          if obj = nil then
            Result[S] := value else
            begin
              if obj.IsType(stArray) then
                obj.AsArray.Add(value) else
                begin
                  obj2 := TSuperObject.Create(stArray);
                  Result[S] := obj2;
                  obj2.AsArray.Add(obj);
                  obj2.AsArray.Add(value);
                end;
            end;
        end else
        begin
          // unamed value ignored
        end;
      end else
      begin
        value := TSuperObject.ParseString(PSOChar(S), false);
        if value = nil then
          value := TSuperObject.Create(s);
        Result.AsArray.Add(value);
      end;
      if not StrictSep then
        while {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']) do
          Inc(src);
      if src^ = sep then
      begin
        P1 := src;
        Inc(P1);
        if (P1^ = #0) and not named then
          Result.AsArray.Add(TSuperObject.Create(''));
        repeat
          Inc(src);
        until not (not StrictSep and {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']));
      end;
    end;
end;

function HTTPDecode(const AStr: string; codepage: Integer): String;
var
  Sp, Rp, Cp: PChar;
  S: String;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Inc(Sp);
             if Sp^ = '%' then
               Rp^ := '%'
             else
             begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then
               begin
                 S := '$' + Cp^ + Sp^;
                 Rp^ := chr(StrToInt(S));
               end
               else
               begin
                 Result := '';
                 Exit;
               end;
             end;
           end;
    else
      Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
  if (codepage > 0) then
    Result := MBUDecode(RawByteString(Result), codepage)
end;

function HTTPGetAuthorization(const str: string): ISuperObject;
var
  i: integer;
  data: string;
begin
  Result := nil;
  if str <> '' then
  begin
    i := pos('Basic ', str);
    if i = 1  then
    begin
      data := Base64ToStr(Copy(str, 7, Length(str) - 6));
      i := pos(':', data);
      if i > 0 then
      begin
        Result := TSuperObject.Create;
        Result.AsObject.S['user'] := copy(data, 1, i-1);
        Result.AsObject.S['pass'] := copy(data, i+1, Length(data)-i);
      end;
    end;
  end;
end;

{ THTTPMessage }

procedure THTTPMessage.Clear(all: boolean);
begin
  inherited;
  FContent.Clear;
end;

constructor THTTPMessage.Create(jt: TSuperType);
begin
  Inherited create(jt);
  FContent := TPooledMemoryStream.Create;
  DataPtr := Self;
end;

destructor THTTPMessage.Destroy;
begin
  inherited;
  FContent.Free;
end;

function THTTPMessage.GetContentString: SOString;
var
  Data: RawByteString;
begin
  FContent.Seek(0, soFromBeginning);
  SetLength(Data, FContent.Size);
  FContent.Read(PAnsiChar(Data)^, FContent.Size);
  Result := SOString(Data);
end;

{ THTTPStub }

function THTTPStub.DecodeFields(str: PChar): boolean;
var
  p: PChar;
  prop: string;
begin
  p := StrScan(str, ':');
  if p = nil then
    Result := false else
    with FRequest.ForcePath('env') do
    begin
      prop := LowerCase(Copy(str, 1, p-str));
      AsObject.S[prop] := p+2;
      Result := true;
    end;
end;

function THTTPStub.DecodeContent: boolean;
var
  ContentLength: integer;
begin
  ContentLength := FRequest.I['env.content-length'];
  if ContentLength > 0 then
  begin
    FRequest.FContent.Size := ContentLength;
    FRequest.FContent.LoadFromSocket(SocketHandle, false);
  end;
  result := true;
end;

function THTTPStub.DecodeCommand(str: PChar): boolean;
  function DecodeURI(uri: PChar; len: integer; out data: string): boolean;
  const hexcodes = ['0'..'9', 'A'..'F', 'a'..'f'];
  var i: integer;
  begin
    data := '';
    while len > 0 do
    begin
      if (uri^ = '%') then
      begin
        // PARANOIA !!
        if (len > 2) and
          {$IFDEF UNICODE}(uri[1] < #256) and {$ENDIF}(AnsiChar(uri[1]) in hexcodes) and
          {$IFDEF UNICODE}(uri[2] < #256) and {$ENDIF}(AnsiChar(uri[2]) in hexcodes) and
          TryStrToInt('$' + uri[1] + uri[2], i) and
          (i in [32..255]) then
            begin
              data := data + char(i);
              inc(uri, 3);
              dec(len, 3);
            end else
            begin
              Result := False;
              Exit;
            end;
      end else
      begin
        data := data + uri^;
        inc(uri, 1);
        dec(len, 1);
      end;
    end;
    Result := true;
  end;
var
  marker: PChar;
  param, value: string;
  i: integer;
begin
  result := false;
  marker := StrScan(str, SP);
  if marker = nil then exit;
  FRequest.S['method'] := copy(str, 0, marker - str);
  str := marker;

  // SP
  if (str^ <> SP) then
    exit;

  // URI
  inc(str);
  marker := Str;
  while not ({$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(Str^) in [SP, NL, '?'])) do
    inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if DecodeURI(marker, str - marker, value) then
      FRequest.S['uri'] := value else
      exit;
  end else
    exit;

  // decode parametters
  if str^ = '?' then
  begin
    inc(str);
    marker := Str;
    param := '';
    value := '';
    while true do
      case str^ of
        '&', SP, NL: begin
               if (param <> '') and (str > marker) then
               begin
                 if not DecodeURI(marker, str - marker, value) then exit;
                 FRequest.S['params.'+HTTPDecode(param)] := HTTPDecode(value);
               end;
               if {$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(str^) in [SP, NL]) then
                 Break;
               param := '';
               value := '';
               inc(Str);
               marker := Str;
             end;
        '=': begin
               if (str > marker) then
                 if not DecodeURI(marker, str - marker, param) then
                   Exit;
               inc(Str);
               marker := Str;
             end;
      else
        inc(Str);
        continue;
      end;

  end;

  // SP expected
  if (str^ <> SP) then exit;

  // HTTP/
  inc(str);
  if not ((str[0] = 'H') and (str[1] = 'T') and (str[2] = 'T') and
     (str[3] = 'P') and (str[4] = SL)) then
    exit;
  str := @str[5];

  // version major
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF}(AnsiChar(str^) in ['0'..'9']) do inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.major'] := i else
      exit;
  end else
    exit;

  // .
  if (str^ <> PT) then
    exit;
  inc(str);

  // version minor
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF} (AnsiChar(str^) in ['0'..'9']) do inc(str);
  if (str > marker) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.minor']  := i else
      exit;
  end else
    exit;

  if (str^ <> NL) then exit;

  result := true;
end;

function THTTPStub.Run: Cardinal;
var
  buffer: string;
  cursor, line, len: integer;
  c: char;
  ctx: ISuperObject;
{$IFDEF UNIX}
  FDSet: TFDSet;
  TimeOut: TTimeVal;
  r: integer;
{$ENDIF}
begin
  result := 0;
  cursor := 0;
  len := 0;
  line := 0;
  c := #0;

  while not Stopped do
  begin
    inc(cursor);
    if cursor > len then
    begin
      inc(len, 255);
      SetLength(buffer, len);
    end;

    // check sizes
    if ((line = 0) and (cursor >= DEFAULT_LIMIT_REQUEST_LINE)) or
       ((line > 0) and (cursor >= DEFAULT_LIMIT_REQUEST_FIELDSIZE)) or
       (line > DEFAULT_LIMIT_REQUEST_FIELDS) then
      Exit;

{$IFDEF UNIX}
    repeat
      // stop listening when stoped
      fpFD_ZERO(FDSet);
      fpFD_SET(SocketHandle, FDSet);
      TimeOut.tv_sec := 1; //1 sec
      TimeOut.tv_usec := 0;
      r := fpSelect(SocketHandle + 1, @FDSet, nil, nil, @TimeOut);
      if Stopped then exit;
    until r > 0;
{$ENDIF}
    if receive(SocketHandle, c, 1, 0) <> 1 then exit;
    case c of
    CR: dec(cursor){->LF};
    LF: begin
          if cursor = 1 then
          begin
            if not DecodeContent then
              exit;

              ctx := TSuperObject.Create;
              try
                doBeforeProcessRequest(ctx);
                try
                  try
                    ProcessRequest(ctx); // <<<<<<<<<<<<<<<
                  except
                    on E: Exception do
                    begin
                      FResponse.I['response'] := 500;
                      FResponse.Content.WriteString(E.Message, false);
                    {$ifdef madExcept}
                      HandleException(etNormal, E);
                    {$endif}
                    end;
                  end;
                finally
                  doAfterProcessRequest(ctx);
                end;
              finally
                ctx := nil;
              end;

            line := 0;
            cursor := 0;
          end else
          begin
            buffer[cursor] := NL;
            if line = 0 then
            begin
              if not DecodeCommand(Pointer(Buffer)) then
                exit;
            end else
            begin
              if not DecodeFields(Pointer(Buffer)) then
                exit;
            end;
            cursor := 0;
            inc(line);
          end;
        end;
    else
      buffer[cursor] := c;
    end;
  end;
end;

function THTTPStub.CreateMVC: ISuperObject;
begin
  Result := TSuperObject.Create;
end;

constructor THTTPStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FRequest := THTTPMessage.Create;
  FRequest._AddRef;
  FResponse := THTTPMessage.Create;
  FResponse._AddRef;
  FMVC := CreateMVC;
end;

destructor THTTPStub.Destroy;
begin
  FRequest._Release;
  FResponse._Release;
  FMVC := nil;
  inherited;
end;

procedure THTTPStub.doAfterProcessRequest(ctx: ISuperObject);
var
  ite: TSuperObjectIter;
begin
   WriteLine(HttpResponseStrings(Response.I['response']));

   if ObjectFindFirst(Response['env'], ite) then
   repeat
     WriteLine(RawByteString(ite.key + ': ' + ite.val.AsString));
   until not ObjectFindNext(ite);
   ObjectFindClose(ite);

   if Response['sendfile'] <> nil then
     SendFile(Response.S['sendfile']) else
     SendStream(Response.Content);

//   Response.Clear;
//   Request.Clear;
   ctx.Clear(true);
end;

procedure THTTPStub.doBeforeProcessRequest(ctx: ISuperObject);
begin
  FRequest.I['tickcount'] := GetTickCount;
  FRequest['cookies'] := HTTPInterprete(PSOChar(Request.S['env.cookie']), true);
  FRequest['content-type'] := HTTPInterprete(PSOChar(Request.S['env.content-type']));
  FRequest['authorization'] := HTTPGetAuthorization(Request.S['env.authorization']);
  FRequest['accept'] := HTTPInterprete(PSOChar(Request.S['env.accept']), false, ',');


  FResponse.I['response'] :=  200;

  ctx['request'] := FRequest;
  ctx['response'] := FResponse;
end;

procedure THTTPStub.ProcessRequest(ctx: ISuperObject);
begin

end;

procedure THTTPStub.SendEmpty;
begin
  WriteLine('Content-Length: 0');
  WriteLine('');
end;

procedure THTTPStub.SendFile(const filename: string);
var
  stream: TFileStream;
begin
  if FileExists(filename) then
  begin
    stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
      SendStream(stream);
    finally
      stream.Free;
    end;
  end else
    SendEmpty;
end;

procedure THTTPStub.SendString(const data: RawByteString);
begin
  WriteLine(format(AnsiString('Content-Length: %d'), [length(data)]));
  WriteLine('');
  WriteString(data);
end;

procedure THTTPStub.WriteLine(str: RawByteString);
begin
  str := str + CRLF;
{$IFDEF FPC}
  fpsend(SocketHandle, PChar(str), length(str), 0);
{$ELSE}
  send(SocketHandle, PAnsiChar(str)^, length(str), 0);
{$ENDIF}
end;

procedure THTTPStub.WriteString(const str: RawByteString);
begin
{$IFDEF FPC}
  fpsend(SocketHandle, PChar(str), length(str), 0);
{$ELSE}
  send(SocketHandle, PAnsiChar(str)^, length(str), 0);
{$ENDIF}
end;

procedure THTTPStub.SendStream(Stream: TStream);
  procedure SendIt(s: TSTream);
  var
    size: Integer;
    buffer: array[0..1023] of byte;
  begin
    //WriteLine(format('Content-Length: %d', [s.size]));
    WriteLine('');
    //s.Seek(0, soFromBeginning);
    size := s.Read(buffer, sizeof(buffer));
    while size > 0 do
    begin
{$IFDEF FPC}
      fpsend(SocketHandle, @buffer, size, 0);
{$ELSE}
      send(SocketHandle, buffer, size, 0);
{$ENDIF}
      size := s.Read(buffer, sizeof(buffer));
    end;
  end;
var
  streamout: TPooledMemoryStream;
begin
  if Response.B['compress'] then
  begin
    streamout := TPooledMemoryStream.Create;
    try
      CompressStream(stream, streamout, Response.I['compresslevel']);
      // don't send first 2 bytes !
      WriteLine(format(AnsiString('Content-Length: %d'), [streamout.size - 2]));
      streamout.Seek(2, soFromBeginning);
      SendIt(streamout);
    finally
      streamout.Free;
    end;
  end else
  begin
    WriteLine(format(AnsiString('Content-Length: %d'), [Stream.size]));
    Stream.Seek(0, soFromBeginning);
    SendIt(Stream);
  end;
end;

end.


