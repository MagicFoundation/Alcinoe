/// implements asynchronous RTSP stream tunnelling over HTTP
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynProtoRTSPHTTP;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):


  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


   RTSP Stream Tunnelling Over HTTP
  ----------------------------------
  as defined by Apple at https://goo.gl/CX6VA3

 It will encapsulate a RTSP TCP/IP duplex video stream into two HTTP links,
 one POST for upgoing commands, and one GET for downloaded video.

 Thanks to TAsynchServer, it can handle thousands on concurrent streams,
 with minimal resources, in a cross-platform way.

 This unit illustrates use of TAsynchConnections, on both Windows and Linux.
 See "Sample 34 - RTSP over HTTP proxy" for some numbers on actual hardware.

}


{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  {$endif}
  SynFPCSock, // shared with Kylix
  {$endif}
  SysUtils,
  SynCommons,
  SynCrtSock,
  SynBidirSock,
  SynLog,
  SynTests;

type
  /// holds a HTTP POST connection for RTSP proxy
  // - as used by the TRTSPOverHTTPServer class
  TPostConnection = class(TAsynchConnection)
  protected
    fRtspTag: TPollSocketTag;
    // redirect the POST base-64 encoded command to the RTSP socket
    function OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead; override;
    // will release the associated TRtspConnection instance
    procedure BeforeDestroy(Sender: TAsynchConnections); override;
  end;

  /// holds a RTSP connection for HTTP GET proxy
  // - as used by the TRTSPOverHTTPServer class
  TRtspConnection = class(TAsynchConnection)
  protected
    fGetBlocking: TCrtSocket;
    // redirect the RTSP socket input to the GET content
    function OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead; override;
    // will release the associated blocking GET socket
    procedure BeforeDestroy(Sender: TAsynchConnections); override;
  end;

  /// implements RTSP over HTTP asynchronous proxy
  // - the HTTP transport is built from two separate HTTP GET and POST requests
  // initiated by the client; the server then binds the connections to form a
  // virtual full-duplex connection - see https://goo.gl/CX6VA3 for reference
  // material about this horrible, but widely accepted, Apple hack
  TRTSPOverHTTPServer = class(TAsynchServer)
  protected
    fRtspServer, fRtspPort: SockString;
    fPendingGet: TRawUTF8List;
    function GetHttpPort: SockString;
    // creates TPostConnection and TRtspConnection instances for a given stream
    function ConnectionCreate(aSocket: TSocket; const aRemoteIp: RawUTF8;
      out aConnection: TAsynchConnection): boolean; override;
  public
    /// initialize the proxy HTTP server forwarding specified RTSP server:port
    constructor Create(const aRtspServer, aRtspPort, aHttpPort: SockString;
      aLog: TSynLogClass; aOnStart, aOnStop: TNotifyThreadEvent;
      aOptions: TAsynchConnectionsOptions = []); reintroduce;
    /// shutdown and finalize the server
    destructor Destroy; override;
    /// convert a rtsp://.... URI into a http://... proxy URI
    // - will reuse the rtsp public server name, but change protocol to http://
    // and set the port to RtspPort
    function RtspToHttp(const RtspURI: RawUTF8): RawUTF8;
    /// convert a http://... proxy URI into a rtsp://.... URI
    function HttpToRtsp(const HttpURI: RawUTF8): RawUTF8;
    /// perform some basic regression and benchmark testing on a running server
    procedure RegressionTests(test: TSynTestCase; clientcount, steps: integer);
    /// the associated RTSP server address
    property RtspServer: SockString read fRtspServer;
    /// the associated RTSP server port
    property RtspPort: SockString read fRtspPort;
    /// the bound HTTP port
    property HttpPort: SockString read GetHttpPort;
  end;


implementation


{ TRtspConnection }

function TRtspConnection.OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead;
begin
  if acoVerboseLog in Sender.Options then
    Sender.LogVerbose(self, 'Frame forwarded', fSlot.readbuf);
  if fGetBlocking.TrySndLow(pointer(fSlot.readbuf), length(fSlot.readbuf)) then begin
    Sender.Log.Add.Log(sllDebug, 'OnRead % RTSP forwarded % bytes to GET',
      [Handle, length(fSlot.readbuf)], self);
    result := sorContinue;
  end
  else begin
    Sender.Log.Add.Log(sllDebug, 'OnRead % RTSP failed send to GET -> close % connection',
      [Handle, RemoteIP], self);
    result := sorClose;
  end;
  fSlot.readbuf := '';
end;

procedure TRtspConnection.BeforeDestroy(Sender: TAsynchConnections);
begin
  fGetBlocking.Free;
  inherited BeforeDestroy(Sender);
end;


{ TPostConnection }

function TPostConnection.OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead;
var
  decoded: RawByteString;
  rtsp: TAsynchConnection;
begin
  result := sorContinue;
  decoded := Base64ToBinSafe(TrimControlChars(fSlot.readbuf));
  if decoded = '' then
    exit; // maybe some pending command chars
  fSlot.readbuf := '';
  rtsp := Sender.ConnectionFindLocked(fRtspTag);
  if rtsp <> nil then
    try
      Sender.Write(rtsp, decoded); // asynch sending to RTSP server
      Sender.Log.Add.Log(sllDebug, 'OnRead % POST forwarded RTSP command [%]',
        [Handle, decoded], self);
    finally
      Sender.Unlock;
    end
  else begin
    Sender.Log.Add.Log(sllDebug, 'OnRead % POST found no rtsp=%', [Handle, fRtspTag], self);
    result := sorClose;
  end;
end;

procedure TPostConnection.BeforeDestroy(Sender: TAsynchConnections);
begin
  Sender.ConnectionRemove(fRtspTag); // disable associated RTSP and GET sockets
  inherited BeforeDestroy(Sender);
end;


{ TRTSPOverHTTPServer }

constructor TRTSPOverHTTPServer.Create(const aRtspServer, aRtspPort, aHttpPort: SockString;
  aLog: TSynLogClass; aOnStart, aOnStop: TNotifyThreadEvent; aOptions: TAsynchConnectionsOptions);
begin
  fLog := aLog;
  fRtspServer := aRtspServer;
  fRtspPort := aRtspPort;
  fPendingGet := TRawUTF8List.Create([fObjectsOwned,fCaseSensitive]);
  inherited Create(aHttpPort, aOnStart, aOnStop, TPostConnection,
    'rtsp/http', aLog, aOptions);
end;

destructor TRTSPOverHTTPServer.Destroy;
var log: ISynLog;
begin
  log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'Destroy'{$endif});
  inherited Destroy;
  fPendingGet.Free;
end;

type
  TProxySocket = class(THttpServerSocket)
  protected
    fExpires: cardinal;
  published
    property Method;
    property URL;
    property RemoteIP;
  end;

const
  RTSP_MIME = 'application/x-rtsp-tunnelled';

function TRTSPOverHTTPServer.ConnectionCreate(aSocket: TSocket; const aRemoteIp: RawUTF8;
  out aConnection: TAsynchConnection): boolean;
var
  log: ISynLog;
  sock, get, old: TProxySocket;
  cookie: RawUTF8;
  rtsp: TSocket;
  i, found: integer;
  postconn: TPostConnection;
  rtspconn: TRtspConnection;
  now: cardinal;

  procedure PendingDelete(i: integer; const reason: RawUTF8);
  begin
    if log<>nil then
      log.Log(sllDebug, 'ConnectionCreate rejected %', [reason], self);
    fPendingGet.Delete(i);
  end;

begin
  aConnection := nil;
  get := nil;
  result := false;
  log := fLog.Enter('ConnectionCreate(%)', [aSocket], self);
  try
    sock := TProxySocket.Create(nil);
    try
      sock.AcceptRequest(aSocket,nil);
      sock.RemoteIP := aRemoteIP;
      sock.CreateSockIn; // faster header process (released below once not needed)
      if (sock.GetRequest({withBody=}false, {headertix=}0)=grHeaderReceived) and
         (sock.URL <> '') then begin
        if log<>nil then
          log.Log(sllTrace, 'ConnectionCreate received % % %', [sock.Method, sock.URL,
            sock.HeaderGetText], self);
        cookie := sock.HeaderGetValue('X-SESSIONCOOKIE');
        if cookie = '' then
          exit;
        fPendingGet.Safe.Lock;
        try
          found := -1;
          now := SynCommons.GetTickCount64 shr 10;
          for i := fPendingGet.Count - 1 downto 0 do begin
            old := fPendingGet.ObjectPtr[i];
            if now > old.fExpires then begin
              if log<>nil then
                log.Log(sllTrace, 'ConnectionCreate deletes deprecated %', [old], self);
              fPendingGet.Delete(i);
            end
            else if fPendingGet[i]=cookie then
              found := i;
          end;
          if IdemPropNameU(sock.Method, 'GET') then begin
            if found >= 0 then
              PendingDelete(found, 'duplicated')
            else begin
              sock.Write(FormatUTF8('HTTP/1.0 200 OK'#13#10 +
                'Server: % %'#13#10 +
                'Connection: close'#13#10 +
                'Date: Thu, 19 Aug 1982 18:30:00 GMT'#13#10 +
                'Cache-Control: no-store'#13#10 +
                'Pragma: no-cache'#13#10 +
                'Content-Type: ' + RTSP_MIME + #13#10#13#10,
                [ExeVersion.ProgramName, ExeVersion.Version.DetailedOrVoid]));
              sock.fExpires := now + 60 * 15; // deprecated after 15 minutes
              sock.CloseSockIn; // we won't use it any more
              fPendingGet.AddObject(cookie, sock);
              sock := nil; // will be in fPendingGet until POST arrives
              result := true;
            end;
          end
          else if IdemPropNameU(sock.Method, 'POST') then begin
            if found < 0 then begin
              if log<>nil then
                log.Log(sllDebug, 'ConnectionCreate rejected on unknonwn %', [sock], self)
            end else if not IdemPropNameU(sock.ContentType, RTSP_MIME) then
              PendingDelete(found, sock.ContentType)
            else begin
              get := fPendingGet.Objects[found];
              fPendingGet.Objects[found] := nil; // will be owned by rtspinstance
              fPendingGet.Delete(found);
              sock.Sock := -1; // disable Close on sock.Free -> handled in pool
            end;
          end;
        finally
          fPendingGet.Safe.UnLock;
        end;
      end
      else if log<>nil then
        log.Log(sllDebug, 'ConnectionCreate: ignored invalid %', [sock], self);
    finally
      sock.Free;
    end;
    if get = nil then
      exit;
    if not get.SockConnected then begin
      if log<>nil then
        log.Log(sllDebug, 'ConnectionCreate: GET disconnected %', [get.Sock], self);
      exit;
    end;
    rtsp := CallServer(fRtspServer, fRtspPort, false, cslTCP, 1000);
    if rtsp <= 0 then // ECrtSocket to include WSAGetLastError
      raise ECrtSocket.CreateFmt('No RTSP server on %s:%s', [fRtspServer, fRtspPort], -1);
    postconn := TPostConnection.Create(aRemoteIP);
    rtspconn := TRtspConnection.Create(aRemoteIP);
    if not inherited ConnectionAdd(aSocket, postconn) or
       not inherited ConnectionAdd(rtsp, rtspconn) then
      raise EAsynchConnections.CreateUTF8('inherited %.ConnectionAdd(%) % failed',
        [self, aSocket, cookie]);
    aConnection := postconn;
    postconn.fRtspTag := rtspconn.Handle;
    rtspconn.fGetBlocking := get;
    if not fClients.Start(rtspconn) then
      exit;
    get := nil;
    result := true;
    if log<>nil then
      log.Log(sllTrace, 'ConnectionCreate added get=% post=%/% and rtsp=%/% for %',
        [rtspconn.fGetBlocking.Sock, aSocket, aConnection.Handle,
         rtsp, rtspconn.Handle, cookie], self);
  except
    if log<>nil then
      log.Log(sllDebug, 'ConnectionCreate(%) failed', [aSocket], self);
    get.Free;
  end;
end;

procedure TRTSPOverHTTPServer.RegressionTests(test: TSynTestCase;
  clientcount, steps: integer);
type
  TReq = record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUTF8;
  end;
var
  streamer: TCrtSocket;
  req: array of TReq;
  rmax, r, i: integer;
  text: SockString;
  log: ISynLog;
begin // here we follow the steps and content stated by https://goo.gl/CX6VA3
  log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'Tests'{$endif});
  if (self = nil) or (fRtspServer <> '127.0.0.1') then
    test.Check(false, 'expect a running proxy on 127.0.0.1')
  else
  try
    rmax := clientcount - 1;
    streamer := TCrtSocket.Bind(fRtspPort);
    try
      if log<>nil then
        log.Log(sllCustom1, 'RegressionTests % GET', [clientcount], self);
      SetLength(req, clientcount);
      for r := 0 to rmax do
      with req[r] do begin
        session := TSynTestCase.RandomIdentifier(20 + r and 15);
        get := THttpSocket.Open('localhost', fServer.Port);
        get.Write(
          'GET /sw.mov HTTP/1.0'#13#10 +
          'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
          'x-sessioncookie: ' + session + #13#10 +
          'Accept: ' + RTSP_MIME + #13#10 +
          'Pragma: no-cache'#13#10 +
          'Cache-Control: no-cache'#13#10#13#10);
        get.SockRecvLn(text);
        test.Check(text = 'HTTP/1.0 200 OK');
        get.GetHeader(false);
        test.Check(connectionClose in get.HeaderFlags);
        test.Check(get.SockConnected);
        test.Check(get.ContentType = RTSP_MIME);
      end;
      if log<>nil then
        log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], self);
      for r := 0 to rmax do
      with req[r] do begin
        post := TCrtSocket.Open('localhost', fServer.Port);
        post.Write(
          'POST /sw.mov HTTP/1.0'#13#10 +
          'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
          'x-sessioncookie: ' + session + #13#10 +
          'Content-Type: ' + RTSP_MIME + #13#10 +
          'Pragma: no-cache'#13#10 +
          'Cache-Control: no-cache'#13#10 +
          'Content-Length: 32767'#13#10 +
          'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
        stream := streamer.AcceptIncoming;
        if stream = nil then begin
          test.Check(false);
          exit;
        end;
        test.Check(get.SockConnected);
        test.Check(post.SockConnected);
      end;
      for i := 0 to steps do begin
        if log<>nil then
          log.Log(sllCustom1, 'RegressionTests % RUN #%', [clientcount, i], self);
        // send a RTSP command once in a while to the POST request
        if i and 7 = 0 then begin
          for r := 0 to rmax do
            req[r].post.Write(
              'REVTQ1JJQkUgcnRzcDovL3R1Y2tydS5hcHBsZS5jb20vc3cubW92IFJUU1AvMS4w'#13#10 +
              'DQpDU2VxOiAxDQpBY2NlcHQ6IGFwcGxpY2F0aW9uL3NkcA0KQmFuZHdpZHRoOiAx'#13#10 +
              'NTAwMDAwDQpBY2NlcHQtTGFuZ3VhZ2U6IGVuLVVTDQpVc2VyLUFnZW50OiBRVFMg'#13#10 +
              'KHF0dmVyPTQuMTtjcHU9UFBDO29zPU1hYyA4LjYpDQoNCg==');
          for r := 0 to rmax do
            test.check(req[r].stream.SockReceiveString =
              'DESCRIBE rtsp://tuckru.apple.com/sw.mov RTSP/1.0'#13#10 +
              'CSeq: 1'#13#10 +
              'Accept: application/sdp'#13#10 +
              'Bandwidth: 1500000'#13#10 +
              'Accept-Language: en-US'#13#10 +
              'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10#13#10);
        end;
        // stream output should be redirected to the GET request
        for r := 0 to rmax do
          req[r].stream.Write(req[r].session);
        for r := 0 to rmax do
          with req[r] do
            test.check(get.SockReceiveString = session);
      end;
      if log<>nil then
        log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], self);
    finally
      // first half deletes POST first, second half deletes GET first
      for r := 0 to rmax shr 1 do
        req[r].post.Free;
      req[0].stream.Free; // validates remove POST when RTSP already down
      for r := (rmax shr 1)+1 to rmax do
        req[r].get.Free;
      for r := 0 to rmax shr 1 do
        req[r].get.Free;
      for r := (rmax shr 1)+1 to rmax do
        req[r].post.Free;
      sleep(10);
      for r := 1 to rmax do
        req[r].stream.Free;
      streamer.Free;
    end;
  except
    on E: Exception do
      test.Check(false, E.ClassName);
  end;
end;

function TRTSPOverHTTPServer.GetHttpPort: SockString;
begin
  if self <> nil then
    result := fServer.Port
  else
    result := '';
end;

function TRTSPOverHTTPServer.RtspToHttp(const RtspURI: RawUTF8): RawUTF8;
var
  uri: TUri;
begin
  if (self <> nil) and IdemPChar(pointer(RtspURI), 'RTSP://') and
     uri.From(copy(RtspURI, 8, maxInt), fRtspPort) and
     IdemPropNameU(uri.Port, fRtspPort) then
    FormatUTF8('http://%:%/%', [uri.Server, fServer.Port, uri.Address], result)
  else
    result := RtspURI;
end;

function TRTSPOverHTTPServer.HttpToRtsp(const HttpURI: RawUTF8): RawUTF8;
var
  uri: TUri;
begin
  if (self <> nil) and uri.From(HttpURI, fServer.Port) and
     IdemPropNameU(uri.Port, fServer.Port) then
    FormatUTF8('rtsp://%:%/%', [uri.Server, fRtspPort, uri.Address], result)
  else
    result := HttpURI;
end;

end.

