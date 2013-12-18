{ This file is generated automaticaly, do not modify }
unit TestLib_Server;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
interface
uses Classes, {$IFDEF FPC}sockets{$ELSE}WinSock{$ENDIF}, PDGUtils, PDGSocketStub, TestLib_Intf {$ifdef madExcept}, madExcept {$endif};

type
  TMyObjectStub = class(TSocketStub, IMyObject)
  private
    procedure Dec_ExecuteScript(stream: TPooledMemoryStream);
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function Run: Cardinal; override;
    procedure ExecuteScript(const script: string; out data: TMemoryStream); virtual; stdcall; abstract;
  end;

  TMyObject2Stub = class(TSocketStub, IMyObject2)
  private
    procedure Dec_GetString(stream: TPooledMemoryStream);
  protected
    function EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean; virtual;
    function Run: Cardinal; override;
    function GetString: string; virtual; stdcall; abstract;
  end;

implementation
uses SysUtils;

{$ifndef madExcept}
// work around Delphi Bug
const
  CMadExceptVersion = 'dummy';
{$endif}

{ TMyObjectStub }

function TMyObjectStub.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := DecompressStream(socket, stream);
end;

function TMyObjectStub.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := CompressStream(stream, socket);
end;

function TMyObjectStub.Run: Cardinal;
var
  fn: Integer;
  stream: TPooledMemoryStream;
  procedure SendException(e: Exception);
  begin
    stream.WriteString('[' + e.ClassName + '] ' + E.Message, true);
  end;
begin
  Result := 0;
  while not Stopped do
  begin
    stream := TPooledMemoryStream.Create;
    try
      if not DecodeStream(stream, SocketHandle) then exit;
      stream.Seek(0, soFromBeginning);
      if not(stream.Read(fn, sizeof(fn)) = SizeOf(fn)) then exit;
      try
        case fn of
          0 : Dec_ExecuteScript(stream);
        else
          exit;
        end;
        stream.WriteInteger(0);
      except
        on E: Exception do
        begin
          SendException(E);
        {$ifdef madExcept}
          {$if (CMadExceptVersion = '3.0a') or (CMadExceptVersion = '3.0b')}
            HandleException(etNormal, E);
          {$else}
            HandleException(false, E);
          {$ifend}
        {$endif}
        end;
      end;
      stream.Seek(0, soFromBeginning);
      if not EncodeStream(stream, SocketHandle) then exit;
    finally
      stream.Free;
    end;
  end;
end;

procedure TMyObjectStub.Dec_ExecuteScript(stream: TPooledMemoryStream);
var
  script_len: integer;
  script: string;
  data_len: integer;
  data: TMemoryStream;
begin
  data := TMemoryStream.Create;
  stream.Read(script_len, sizeof(script_len));
  SetLength(script, script_len);
  if script_len > 0 then
    stream.Read(script[1], script_len);
  try
    ExecuteScript(script, data);
  finally
    stream.Size := 0;
    data_len := data.Size;
    stream.Write(data_len, SizeOf(data_len));
    data.SaveToStream(stream);
    data.free;
  end;
end;

{ TMyObject2Stub }

function TMyObject2Stub.DecodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := DecompressStream(socket, stream);
end;

function TMyObject2Stub.EncodeStream(stream: TPooledMemoryStream; socket: longint): boolean;
begin
  Result := CompressStream(stream, socket);
end;

function TMyObject2Stub.Run: Cardinal;
var
  fn: Integer;
  stream: TPooledMemoryStream;
  procedure SendException(e: Exception);
  begin
    stream.WriteString('[' + e.ClassName + '] ' + E.Message, true);
  end;
begin
  Result := 0;
  while not Stopped do
  begin
    stream := TPooledMemoryStream.Create;
    try
      if not DecodeStream(stream, SocketHandle) then exit;
      stream.Seek(0, soFromBeginning);
      if not(stream.Read(fn, sizeof(fn)) = SizeOf(fn)) then exit;
      try
        case fn of
          0 : Dec_GetString(stream);
        else
          exit;
        end;
        stream.WriteInteger(0);
      except
        on E: Exception do
        begin
          SendException(E);
        {$ifdef madExcept}
          {$if (CMadExceptVersion = '3.0a') or (CMadExceptVersion = '3.0b')}
            HandleException(etNormal, E);
          {$else}
            HandleException(false, E);
          {$ifend}
        {$endif}
        end;
      end;
      stream.Seek(0, soFromBeginning);
      if not EncodeStream(stream, SocketHandle) then exit;
    finally
      stream.Free;
    end;
  end;
end;

procedure TMyObject2Stub.Dec_GetString(stream: TPooledMemoryStream);
var
  Return_len: integer;
  Return: string;
begin
  try
    Return := GetString;
  finally
    stream.Size := 0;
    Return_len := length(Return);
    stream.Write(Return_len, SizeOf(Return_len));
    if Return_len > 0 then
      stream.Write(Return[1], Return_len);
  end;
end;

end.
