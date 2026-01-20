unit Alcinoe.Net.Socket.Client.Win;

interface

{$I Alcinoe.inc}

uses
  System.SysUtils,
  WinApi.Winsock2,
  Alcinoe.Net.Socket.Client;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWinSocketClient = class(TALSocketClient)
  private
    FSocket: TSocket;
  protected
    class procedure Connect(var ASocket: TSocket; const AHost: String; const APort: Integer); reintroduce; overload;
    class procedure Disconnect(var ASocket: TSocket); reintroduce; overload;
    class function GetSendTimeout(const ASocket: TSocket): integer; reintroduce; overload;
    class procedure SetSendTimeout(const ASocket: TSocket; const Value: integer); reintroduce; overload;
    class function GetReceiveTimeout(const ASocket: TSocket): integer; reintroduce; overload;
    class procedure SetReceiveTimeout(const ASocket: TSocket; const Value: integer); reintroduce; overload;
    class function GetKeepAlive(const ASocket: TSocket): boolean; reintroduce; overload;
    class procedure SetKeepAlive(const ASocket: TSocket; const Value: boolean); reintroduce; overload;
    class function GetTCPNoDelay(const ASocket: TSocket): boolean; reintroduce; overload;
    class procedure SetTCPNoDelay(const ASocket: TSocket; const Value: boolean); reintroduce; overload;
    class function Send(const ASocket: TSocket; const Buf; Len: Integer): Integer; reintroduce; overload;
    class procedure SendAll(const ASocket: TSocket; const Buf; Len: Integer); reintroduce; overload;
    class function Receive(const ASocket: TSocket; var Buf; Len: Integer): Integer; reintroduce; overload;
    class procedure ReceiveAll(const ASocket: TSocket; var Buf: TBytes); reintroduce; overload;
    class procedure ReceiveAll(const ASocket: TSocket; var Buf: AnsiString); reintroduce; overload;
    procedure SetSendTimeout(const Value: integer); overload; override;
    procedure SetReceiveTimeout(const Value: integer); overload; override;
    procedure SetKeepAlive(const Value: boolean); overload; override;
    procedure SetTCPNoDelay(const Value: boolean); overload; override;
    function GetConnected: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect(const AHost: String; const APort: Integer); overload; override;
    procedure Disconnect; overload; override;
    function Send(const Buf; Len: Integer): Integer; overload; override;
    function Receive(var Buf; Len: Integer): Integer; overload; override;
  end;

implementation

uses
  System.Math,
  WinApi.Windows,
  Alcinoe.StringUtils,
  Alcinoe.WinApi.Windows;

{************************************}
constructor TALWinSocketClient.Create;
begin
  inherited;
  FSocket := INVALID_SOCKET;
end;

{************************************}
destructor TALWinSocketClient.Destroy;
begin
  Disconnect;
  Inherited;
end;

{**********************************************************************************************************}
class procedure TALWinSocketClient.Connect(var ASocket: TSocket; const AHost: String; const APort: Integer);
begin
  if ASocket <> INVALID_SOCKET then
    raise Exception.Create('Socket is already connected. Call Disconnect first.');

  Try

    var LHints: addrinfoW;
    ZeroMemory(@LHints, SizeOf(LHints));
    LHints.ai_family := AF_UNSPEC; // PF_xxx
    LHints.ai_socktype := SOCK_STREAM; // SOCK_xxx
    LHints.ai_protocol := IPPROTO_TCP; // 0 or IPPROTO_xxx for IPv4 and IPv6

    var LResult: PaddrinfoW := nil;
    var LPort := ALIntToStrW(APort);

    ALCheckWinApiWSAErrorCode(
      'GetAddrInfoW',
      GetAddrInfoW(
        PChar(AHost), // [in, optional] PCWSTR pNodeName,
        PChar(LPort), // [in, optional] PCWSTR pServiceName,
        LHints, // [in, optional] const ADDRINFOW *pHints,
        LResult)); // [out] PADDRINFOW *ppResult
    try

      var LPAddrinfoW: PaddrinfoW := LResult;
      while LPAddrinfoW <> nil do begin
        ASocket := Winapi.WinSock2.socket(LPAddrinfoW.ai_family, LPAddrinfoW.ai_socktype, LPAddrinfoW.ai_protocol);
        if ASocket <> INVALID_SOCKET then begin
          if Winapi.WinSock2.connect(ASocket, LPAddrinfoW.ai_addr^, LPAddrinfoW.ai_addrlen) = 0 then Exit;
          closesocket(ASocket);
          ASocket := INVALID_SOCKET;
        end;
        LPAddrinfoW := LPAddrinfoW.ai_next;
      end;

      raise Exception.CreateFmt('Unable to establish a TCP connection to %s:%d using any resolved address', [AHost, APort]);

    finally
      if LResult <> nil then
        FreeAddrInfoW(LResult^);
    end;

  except
    On E: Exception do begin
      ASocket := INVALID_SOCKET;
      Raise;
    end;
  end
end;

{******************************************************************}
class procedure TALWinSocketClient.Disconnect(var ASocket: TSocket);
begin
  if ASocket <> INVALID_SOCKET then begin
    Try
      shutdown(ASocket, SD_BOTH);
      closesocket(ASocket);
    except
      // best-effort shutdown; ignore errors
    End;
    ASocket := INVALID_SOCKET;
  end;
end;

{********************************************************************************}
class function TALWinSocketClient.GetSendTimeout(const ASocket: TSocket): integer;
begin
  Result := 0;
  var LOptLen: Integer := SizeOf(Result);
  ALCheckWinApiWSAErrorCode(
    'getsockopt(SO_SNDTIMEO)',
    Winapi.Winsock2.getsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_SNDTIMEO, // [in] int optname,
      PAnsiChar(@Result), // [out] char *optval,
      LOptLen)); // [in, out] int *optlen
end;

{**********************************************************************************************}
class procedure TALWinSocketClient.SetSendTimeout(const ASocket: TSocket; const Value: integer);
begin
  ALCheckWinApiWSAErrorCode(
    'setsockopt(SO_SNDTIMEO)',
    Winapi.Winsock2.setsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_SNDTIMEO, // [in] int optname,
      PAnsiChar(@Value), // in] const char *optval,
      SizeOf(Value))); // [in] int optlen
end;

{***********************************************************************************}
class function TALWinSocketClient.GetReceiveTimeout(const ASocket: TSocket): integer;
begin
  Result := 0;
  var LOptLen: Integer := SizeOf(Result);
  ALCheckWinApiWSAErrorCode(
    'getsockopt(SO_RCVTIMEO)',
    Winapi.Winsock2.getsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_RCVTIMEO, // [in] int optname,
      PAnsiChar(@Result), // [out] char *optval,
      LOptLen)); // [in, out] int *optlen
end;

{*************************************************************************************************}
class procedure TALWinSocketClient.SetReceiveTimeout(const ASocket: TSocket; const Value: integer);
begin
  ALCheckWinApiWSAErrorCode(
    'setsockopt(SO_RCVTIMEO)',
    Winapi.Winsock2.setsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_RCVTIMEO, // [in] int optname,
      PAnsiChar(@Value), // in] const char *optval,
      SizeOf(Value))); // [in] int optlen
end;


{******************************************************************************}
class function TALWinSocketClient.GetKeepAlive(const ASocket: TSocket): boolean;
begin
  var LOpt: Integer := 0;
  var LOptLen: Integer := SizeOf(LOpt);
  ALCheckWinApiWSAErrorCode(
    'getsockopt(SO_KEEPALIVE)',
    Winapi.Winsock2.getsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_KEEPALIVE, // [in] int optname,
      PAnsiChar(@LOpt), // in] const char *optval,
      LOptLen)); // [in] int optlen
  Result := LOpt <> 0;
end;

{********************************************************************************************}
class procedure TALWinSocketClient.SetKeepAlive(const ASocket: TSocket; const Value: boolean);
begin
  var LOpt: Integer := Ord(Value);
  ALCheckWinApiWSAErrorCode(
    'setsockopt(SO_KEEPALIVE)',
    Winapi.Winsock2.setsockopt(
      ASocket, // [in] SOCKET s,
      SOL_SOCKET, // [in] int level,
      SO_KEEPALIVE, // [in] int optname,
      PAnsiChar(@LOpt), // in] const char *optval,
      SizeOf(LOpt))); // [in] int optlen
end;

{*******************************************************************************}
class function TALWinSocketClient.GetTCPNoDelay(const ASocket: TSocket): boolean;
begin
  var LOpt: Integer := 0;
  var LOptLen: Integer := SizeOf(LOpt);
  ALCheckWinApiWSAErrorCode(
    'getsockopt(TCP_NODELAY)',
    Winapi.Winsock2.getsockopt(
      ASocket, // [in] SOCKET s,
      IPPROTO_TCP, // [in] int level,
      TCP_NODELAY, // [in] int optname,
      PAnsiChar(@LOpt), // in] const char *optval,
      LOptLen)); // [in] int optlen
  Result := LOpt <> 0;
end;

{*********************************************************************************************}
class procedure TALWinSocketClient.SetTCPNoDelay(const ASocket: TSocket; const Value: boolean);
begin
  var LOpt: Integer := Ord(Value);
  ALCheckWinApiWSAErrorCode(
    'setsockopt(TCP_NODELAY)',
    Winapi.Winsock2.setsockopt(
      ASocket, // [in] SOCKET s,
      IPPROTO_TCP, // [in] int level,
      TCP_NODELAY, // [in] int optname,
      PAnsiChar(@LOpt), // in] const char *optval,
      SizeOf(LOpt))); // [in] int optlen
end;

{***********************************************************************************************}
class function TALWinSocketClient.Send(const ASocket: TSocket; const Buf; Len: Integer): Integer;
begin
  if Len <= 0 then Exit(0);
  Result := Winapi.WinSock2.send(ASocket, Buf, Len, 0);
  if Result = SOCKET_ERROR then begin
    var LLastError := WSAGetLastError;
    if LLastError <> 0 then ALCheckWinApiWSAErrorCode('send', LLastError)
    else Raise Exception.Create('Error 8B8D4DFB-3ECE-4B51-A4A2-5901BEBB0490')
  end;
end;

{******************************************************************************************}
class procedure TALWinSocketClient.SendAll(const ASocket: TSocket; const Buf; Len: Integer);
begin

  //
  // duplicated in TALSocketClient.SendAll(const Buf; Len: Integer);
  //

  if Len <= 0 then Exit;
  var PBuf: PByte := @Buf;
  var LRemaining: Integer := Len;
  while LRemaining > 0 do begin
    var LSent := Send(ASocket, PBuf^, LRemaining);
    if LSent <= 0 then raise Exception.Create('SendAll failed');
    Inc(PBuf, LSent);
    Dec(LRemaining, LSent);
  end;

end;

{************************************************************************************************}
class function TALWinSocketClient.Receive(const ASocket: TSocket; var Buf; Len: Integer): Integer;
begin
  if Len <= 0 then Exit(0);
  Result := Winapi.WinSock2.recv(ASocket, Buf, Len, 0);
  if Result = SOCKET_ERROR then begin
    var LLastError := WSAGetLastError;
    if LLastError <> 0 then ALCheckWinApiWSAErrorCode('recv', LLastError)
    else Raise Exception.Create('Error 87ECF408-4E50-47E9-B7F2-78381EC2F84C')
  end;
end;

{*************************************************************************************}
class procedure TALWinSocketClient.ReceiveAll(const ASocket: TSocket; var Buf: TBytes);
begin

  //
  // duplicated in TALSocketClient.ReceiveAll(var Buf: TBytes);
  //

  const CMinChunk = 8 * 1024;
  var LUsed: Integer := 0;
  while True do begin

    var LBufLn := Length(Buf);
    if LUsed >= LBufLn then begin
      SetLength(Buf, LBufLn + max(LBufLn div 2{~1.5x growth}, CMinChunk));
      LBufLn := Length(Buf);
    end;

    var LGot := Receive(ASocket, Buf[LUsed], LBufLn - LUsed);

    // 0 = orderly close
    if LGot = 0 then begin
      if LBufLn <> LUsed then SetLength(Buf, LUsed);
      Exit;
    end
    // < 0 = error
    else if LGot < 0 then
      raise Exception.Create('ReceiveAll failed (socket error)');

    Inc(LUsed, LGot);
  end;

end;

{*****************************************************************************************}
class procedure TALWinSocketClient.ReceiveAll(const ASocket: TSocket; var Buf: AnsiString);
begin

  //
  // duplicated in TALSocketClient.ReceiveAll(var Buf: AnsiString);
  //

  const CMinChunk = 8 * 1024;
  var LUsed: Integer := 0;
  while True do begin

    var LBufLn := Length(Buf);
    if LUsed >= LBufLn then begin
      SetLength(Buf, LBufLn + max(LBufLn div 2{~1.5x growth}, CMinChunk));
      LBufLn := Length(Buf);
    end;

    var LGot := Receive(ASocket, Buf[low(Buf) + LUsed], LBufLn - LUsed);

    // 0 = orderly close
    if LGot = 0 then begin
      if LBufLn <> LUsed then SetLength(Buf, LUsed);
      Exit;
    end
    // < 0 = error
    else if LGot < 0 then
      raise Exception.Create('ReceiveAll failed (socket error)');

    Inc(LUsed, LGot);
  end;

end;

{******************************************************************************}
procedure TALWinSocketClient.Connect(const AHost: String; const APort: Integer);
begin
  Connect(FSocket, AHost, APort);
  SetSendTimeout(FSocket, GetSendTimeout);
  SetReceiveTimeout(FSocket, GetReceiveTimeout);
  SetKeepAlive(FSocket, GetKeepAlive);
  SetTCPNoDelay(FSocket, GetTCPNoDelay);
end;

{**************************************}
procedure TALWinSocketClient.Disconnect;
begin
  Disconnect(FSocket);
end;

{****************************************************************}
procedure TALWinSocketClient.SetSendTimeout(const Value: integer);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    SetSendTimeout(FSocket, Value);
end;

{*******************************************************************}
procedure TALWinSocketClient.SetReceiveTimeout(const Value: integer);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    SetReceiveTimeout(FSocket, Value);
end;

{**************************************************************}
procedure TALWinSocketClient.SetKeepAlive(const Value: boolean);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    SetKeepAlive(FSocket, Value);
end;

{***************************************************************}
procedure TALWinSocketClient.SetTCPNoDelay(const Value: boolean);
begin
  inherited;
  if FSocket <> INVALID_SOCKET then
    SetTCPNoDelay(FSocket, Value);
end;

{************************************************}
function TALWinSocketClient.GetConnected: Boolean;
begin
  Result := FSocket <> INVALID_SOCKET;
end;

{*****************************************************************}
function TALWinSocketClient.Send(const Buf; Len: Integer): Integer;
begin
  Result := Send(FSocket, Buf, Len);
end;

{******************************************************************}
function TALWinSocketClient.Receive(var Buf; Len: Integer): Integer;
begin
  Result := Receive(FSocket, Buf, Len);
end;

initialization
  var LWsData: TWSAData;
  ALCheckWinApiWSAErrorCode(
    'WSAStartup',
    WSAStartup(WINSOCK_VERSION, LWsData));

finalization
  WSACleanup;

end.