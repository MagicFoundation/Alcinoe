unit Alcinoe.Net.Socket.Client;

interface

{$I Alcinoe.inc}

uses
  system.SysUtils;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSocketClient = class(TObject)
  private
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    fKeepAlive: Boolean;
    fTCPNoDelay: Boolean;
  protected
    function GetSendTimeout: integer; virtual;
    procedure SetSendTimeout(const Value: integer); virtual;
    function GetReceiveTimeout: integer; virtual;
    procedure SetReceiveTimeout(const Value: integer); virtual;
    function GetKeepAlive: boolean; virtual;
    procedure SetKeepAlive(const Value: boolean); virtual;
    function GetTCPNoDelay: boolean; virtual;
    procedure SetTCPNoDelay(const Value: boolean); virtual;
    function GetConnected: Boolean; virtual; abstract;
  public
    constructor Create; virtual;
    /// <summary>
    ///   Connects to <paramref name="AHost"/>:<paramref name="APort"/> using TCP.
    ///   Raises an exception on failure.
    /// </summary>
    procedure Connect(const AHost: String; const APort: Integer); virtual; abstract;
    /// <summary>
    ///   Closes the connection (best-effort) and releases any underlying resources.
    ///   Safe to call multiple times.
    /// </summary>
    procedure Disconnect; virtual; abstract;
    property Connected: Boolean read GetConnected;
    /// <summary>
    ///   Sends up to <paramref name="Len"/> bytes from <paramref name="Buf"/>.
    ///   Returns the number of bytes actually sent (may be less than Len).
    ///   Raises an exception on error.
    /// </summary>
    function Send(const Buf; Len: Integer): Integer; virtual; abstract;
    /// <summary>
    ///   Sends exactly <paramref name="Len"/> bytes from <paramref name="Buf"/>,
    ///   looping until all bytes are sent or an error occurs.
    /// </summary>
    procedure SendAll(const Buf; Len: Integer); virtual;
    /// <summary>
    ///   Receives up to <paramref name="Len"/> bytes into <paramref name="Buf"/>.
    ///   Returns the number of bytes received.
    ///   Returns 0 when the peer has performed an orderly shutdown.
    ///   Raises an exception on error.
    /// </summary>
    function Receive(var Buf; Len: Integer): Integer; virtual; abstract;
    /// <summary>
    ///   Receives data until the peer closes the connection or an error occurs,
    ///   growing/shrinking <paramref name="Buf"/> as needed, then returns.
    ///   Use this when you want to read “everything available until close”
    ///   (e.g. protocols with an orderly close at end of message).
    /// </summary>
    procedure ReceiveAll(var Buf: TBytes); overload; virtual;
    /// <summary>
    ///   Receives data until the peer closes the connection or an error occurs,
    ///   growing/shrinking <paramref name="Buf"/> as needed, then returns.
    ///   Use this when you want to read “everything available until close”.
    /// </summary>
    procedure ReceiveAll(var Buf: AnsiString); overload; virtual;
    /// <summary>
    ///   Send timeout in milliseconds
    /// </summary>
    Property SendTimeout: integer read GetSendTimeout write SetSendTimeout;
    /// <summary>
    ///   Receive timeout in milliseconds
    /// </summary>
    Property ReceiveTimeout: integer read GetReceiveTimeout write SetReceiveTimeout;
    /// <summary>
    ///   Enables or disables TCP keep-alive probes
    /// </summary>
    Property KeepAlive: boolean read GetKeepAlive write SetKeepAlive;
    /// <summary>
    ///   Enables or disables Nagle's algorithm (TCP_NODELAY).
    ///   True = disable Nagle (lower latency, potentially more small packets).
    ///   False = enable Nagle (better throughput for many small writes).
    /// </summary>
    Property TCPNoDelay: boolean read GetTCPNoDelay write SetTCPNoDelay;
  end;

implementation

uses
  System.Math;

{*********************************}
constructor TALSocketClient.Create;
begin
  inherited;
  // The timeout, in milliseconds, for blocking send calls. The default for
  // this option is zero, which indicates that a send operation will not
  // time out.
  FSendTimeout := 0;
  // The timeout, in milliseconds, for blocking receive calls. The default for this option is zero,
  // which indicates that a receive operation will not time out.
  FReceiveTimeout := 0;
  // The setsockopt function called with the SO_KEEPALIVE socket option allows
  // an application to enable keep-alive packets for a socket connection.
  // The SO_KEEPALIVE option for a socket is disabled (set to FALSE) by default.
  FKeepAlive := False;
  // Enables or disables the Nagle algorithm for TCP sockets. This option is
  // disabled (set to FALSE) by default.
  fTCPNoDelay := False;
end;

{*********************************}
function TALSocketClient.GetSendTimeout: integer;
begin
  result := FSendTimeout;
end;

{*********************************}
procedure TALSocketClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
end;

{*********************************}
function TALSocketClient.GetReceiveTimeout: integer;
begin
  result := FReceiveTimeout;
end;

{*********************************}
procedure TALSocketClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
end;

{*********************************}
function TALSocketClient.GetKeepAlive: boolean;
begin
  result := FKeepAlive;
end;

{*********************************}
procedure TALSocketClient.SetKeepAlive(const Value: boolean);
begin
  FKeepAlive := Value;
end;

{*********************************}
function TALSocketClient.GetTCPNoDelay: boolean;
begin
  result := FTCPNoDelay;
end;

{*********************************}
procedure TALSocketClient.SetTCPNoDelay(const Value: boolean);
begin
  FTCPNoDelay := Value;
end;

{*********************************************************}
procedure TALSocketClient.SendAll(const Buf; Len: Integer);
begin

  //
  // duplicated in TALWinClientSocket.SendAll(const ASocket: TSocket; const Buf; Len: Integer);
  //

  if Len <= 0 then Exit;
  var PBuf: PByte := @Buf;
  var LRemaining: Integer := Len;
  while LRemaining > 0 do begin
    var LSent := Send(PBuf^, LRemaining);
    if LSent <= 0 then raise Exception.Create('SendAll failed');
    Inc(PBuf, LSent);
    Dec(LRemaining, LSent);
  end;

end;

{****************************************************}
procedure TALSocketClient.ReceiveAll(var Buf: TBytes);
begin

  //
  // duplicated in TALWinClientSocket.ReceiveAll(const ASocket: TSocket; var Buf: TBytes);
  //

  const CMinChunk = 8 * 1024;
  var LUsed: Integer := 0;
  while True do begin

    var LBufLn := Length(Buf);
    if LUsed >= LBufLn then begin
      SetLength(Buf, LBufLn + max(LBufLn div 2{~1.5x growth}, CMinChunk));
      LBufLn := Length(Buf);
    end;

    var LGot := Receive(Buf[LUsed], LBufLn - LUsed);

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

{********************************************************}
procedure TALSocketClient.ReceiveAll(var Buf: AnsiString);
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

    var LGot := Receive(Buf[low(Buf) + LUsed], LBufLn - LUsed);

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

end.
