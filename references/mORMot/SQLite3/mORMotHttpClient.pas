/// HTTP/1.1 RESTful JSON Client classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpClient;

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
  - Eva Freimann (EVaF)
  - Maciej Izak (hnb)

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


      HTTP/1.1 RESTful JSON Client for mORMot
    ******************************************

   - use internaly the JSON format for content communication
   - can be called by any JSON-aware AJAX application
   - can optionaly compress the returned data to optimize Internet bandwidth
   - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
     i.e. 400Mb/sec of duplex raw IP data, with about 200 us only elapsed
     by request (direct call is 50 us, so bottle neck is the Win32 API),
     i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
     of JSON data each)... try to find a faster JSON HTTP server! ;)

}

interface

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - not defined by default - should be set globally to the project conditionals }

{.$define NOHTTPCLIENTWEBSOCKETS}
{ if defined, TSQLHttpClientWebSockets won't be declared
  - will avoid to link SynBidirSock and SynEcc units }

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 WITHLOG

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef KYLIX3}
  Types,
  LibC,
  {$else}
  SynFPCLinux,
  {$endif}
{$endif}
  SysUtils,
  Classes,
  SynZip,
  SynLZ,
  SynCrtSock,
  {$ifndef NOHTTPCLIENTWEBSOCKETS}
  SynBidirSock, // for WebSockets
  {$endif}
  SynCrypto,    // for hcSynShaAes
  SynCommons,
  SynTable,
  SynLog,
  mORMot;

type
  /// available compression algorithms for transmission
  // - SynLZ is faster then Deflate, but not standard: use hcSynLZ for Delphi
  // clients, but hcDeflate for AJAX or any HTTP clients
  // - with hcSynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLHttpClient
  // is compressed into 106 KB with no speed penalty (it's even a bit faster)
  // whereas hcDeflate with its level set to 1 (fastest), is 25 % slower
  // - hcSynShaAes will use SHA-256/AES-256-CFB to encrypt the content (after
  // SynLZ compression), via SynCrypto.CompressShaAes() function
  // - here hcDeflate will use in fact gzip content encoding, since deflate
  // is inconsistent between browsers: http://stackoverflow.com/a/9186091
  // - TSQLHttpClientGeneric.Compression default property is [hcSynLZ]
  TSQLHttpCompression = (hcSynLZ, hcDeflate, hcSynShaAes);

  /// set of available compressions schemes
  TSQLHttpCompressions = set of TSQLHttpCompression;

  /// abstract HTTP/1.1 RESTful JSON mORMot Client class
  // - this class, and other inherited classes defined in this unit, are
  // thread-safe, since each of their URI() method is protected by a giant lock
  TSQLHttpClientGeneric = class(TSQLRestClientURI)
  protected
    fKeepAliveMS: cardinal;
    fCompression: TSQLHttpCompressions;
    /// connection parameters as set by Create()
    fServer, fPort: AnsiString;
    fHttps: boolean;
    fProxyName, fProxyByPass: AnsiString;
    fSendTimeout, fReceiveTimeout, fConnectTimeout: DWORD;
    fConnectRetrySeconds: integer; // used by InternalCheckOpen
    fExtendedOptions: THttpRequestExtendedOptions;
    procedure SetCompression(Value: TSQLHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
    constructor RegisteredClassCreateFrom(aModel: TSQLModel;
      aDefinition: TSynConnectionDefinition); override;
    /// process low-level HTTP/1.1 request
    // - called by InternalURI(), therefore by URI() public method
    // - returns 200,202,204 if OK, http status error otherwise in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    procedure InternalURI(var Call: TSQLRestURIParams); override;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy - note
    // that proxy parameters are currently not available for TSQLHttpClientWinSock
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms) - if
    // you left the 0 default parameters, it would use global
    // HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT and
    // HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      aHttps: boolean=false; const aProxyName: AnsiString='';
      const aProxyByPass: AnsiString=''; aSendTimeout: DWORD=0;
      aReceiveTimeout: DWORD=0; aConnectTimeout: DWORD=0); reintroduce; overload; virtual;
    /// connect to TSQLHttpServer via 'address:port/root' URI format
    // - if port is not specified, aDefaultPort is used
    // - if root is not specified, aModel.Root is used
    constructor Create(const aServer: TSQLRestServerURIString; aModel: TSQLModel;
      aDefaultPort: integer; aHttps: boolean=false); reintroduce; overload;
    /// connnect to a LogView HTTP Server for remote logging
    // - will associate the EchoCustom callback of the log class to this server
    // - the aLogClass.Family will manage this TSQLHttpClientGeneric instance
    // life time, until application is closed or Family.EchoRemoteStop is called
    constructor CreateForRemoteLogging(const aServer: AnsiString;
      aLogClass: TSynLogClass; aPort: Integer=8091; const aRoot: RawUTF8='LogService');
    /// save the TSQLHttpClientGeneric properties into a persistent storage object
    // - CreateFrom() will expect Definition.ServerName to store the URI as
    // 'server:port' or 'https://server:port', Definition.User/Password to store
    // the TSQLRestClientURI.SetUser() information, and Definition.DatabaseName
    // to store the extended options as an URL-encoded string
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;
    /// returns 'Server:Port' current value
    function HostName: AnsiString;
    /// optional custom HTTP "User Agent:" header value
    property UserAgent: SockString
      read fExtendedOptions.UserAgent write fExtendedOptions.UserAgent;
  published
    /// the Server IP address
    property Server: AnsiString read fServer;
    /// the Server IP port
    property Port: AnsiString read fPort;
    /// the time (in milliseconds) to keep the connection alive with the
    // TSQLHttpServer
    // - default is 20000, i.e. 20 seconds
    property KeepAliveMS: cardinal read fKeepAliveMS write SetKeepAliveMS;
    /// the compression algorithms usable with this client
    // - equals [hcSynLZ] by default, since our SynLZ algorithm provides a good
    // compression, with very low CPU use on server side
    // - you may include hcDeflate, which will have a better compression ratio,
    // be recognized by all browsers and libraries, but would consume much
    // more CPU resources than hcSynLZ
    // - if you include hcSynShaAes, it will use SHA-256/AES-256-CFB to encrypt
    // the content (after SynLZ compression), if it is enabled on the server side:
    // ! MyServer := TSQLHttpServer.Create('888',[DataBase],'+',useHttpApi,32,secSynShaAes);
    // - for fast and safe communication between stable mORMot nodes, consider
    // using TSQLHttpClientWebSockets, leaving hcDeflate for AJAX or non mORMot
    // clients, and hcSynLZ if you expect to have mORMot client(s)
    property Compression: TSQLHttpCompressions read fCompression write SetCompression;
    /// how many seconds the client may try to connect after open socket failure
    // - is disabled to 0 by default, but you may set some seconds here e.g. to
    // let the server start properly, and let the client handle exceptions to
    // wait and retry until the specified timeout is reached
    // - this property is used only once at startup, then flushed to 0 once connected
    property ConnectRetrySeconds: integer read fConnectRetrySeconds write fConnectRetrySeconds;
  end;

  TSQLHttpClientGenericClass = class of TSQLHttpClientGeneric;

  /// HTTP/1.1 RESTful JSON mORMot Client class using SynCrtSock's Sockets
  // - will give the best performance on a local computer, but has been found
  // out to be slower over a network
  // - is not able to use secure HTTPS protocol
  // - note that, in its current implementation, this class is not thread-safe:
  // you need either to lock its access via a critical section, or initialize
  // one client instance per thread
  TSQLHttpClientWinSock = class(TSQLHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocketClass: THttpClientSocketClass;
    /// either THttpClientSocket or THttpClientWebSockets
    fSocket: THttpClientSocket;
    /// call fSocket.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
  published
    /// internal HTTP/1.1 compatible client
    // - can be used e.g. to access SendTimeout and ReceiveTimeout properties
    property Socket: THttpClientSocket read fSocket;
  end;

  {$ifndef NOHTTPCLIENTWEBSOCKETS}

  /// HTTP/1.1 RESTful JSON mORMot Client able to upgrade to WebSockets
  // - in addition to TSQLHttpClientWinSock, this client class is able
  // to upgrade its HTTP connection to the WebSockets protocol, so that the
  // server may be able to notify the client via a callback
  // - the internal Socket class will be in fact a THttpClientWebSockets
  // instance, as defined in the SynBidirSock unit
  TSQLHttpClientWebsockets = class(TSQLHttpClientWinSock)
  protected
    fWebSocketParams: record
      AutoUpgrade: boolean;
      Key: RawUTF8;
      Compression: boolean;
      Ajax: boolean;
    end;
    fOnWebSocketsUpgraded: TOnRestClientNotify;
    fOnWebSocketsClosed: TNotifyEvent;
    fWebSocketLoopDelay: integer;
    fDefaultWebSocketProcessSettings : TWebSocketProcessSettings;
    function InternalCheckOpen: boolean; override;
    function FakeCallbackRegister(Sender: TServiceFactoryClient;
      const Method: TServiceMethod; const ParamInfo: TServiceMethodArgument;
      ParamValue: Pointer): integer; override;
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: integer; Instance: pointer): boolean; override;
    function CallbackRequest(Ctxt: THttpServerRequest): cardinal; virtual;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    // - this overriden method will handle properly WebSockets settings
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      aHttps: boolean=false; const aProxyName: AnsiString='';
      const aProxyByPass: AnsiString=''; aSendTimeout: DWORD=0;
      aReceiveTimeout: DWORD=0; aConnectTimeout: DWORD=0); override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - the Model.Root URI will be used for upgrade
    // - if aWebSocketsAJAX equals default FALSE, it will use 'synopsebinary'
    // i.e. TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAJAX is TRUE, it will register the slower and less secure
    // 'synopsejson' mode, i.e. TWebSocketProtocolJSON (to be used for AJAX
    // debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - once upgraded, the client would automatically re-upgrade any new
    // HTTP client link on automatic reconnection, so that use of this class
    // should be not tied to a particular TCP/IP socket - use OnWebsocketsUpgraded
    // event to perform any needed initialization set, e.g. SOA real-time
    // callbacks registration
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompression: boolean=true): RawUTF8;
    /// connect using a specified WebSockets protocol
    // - this method would call WebSocketsUpgrade, then ServerTimestampSynchronize
    // - it therefore expects SetUser() to have been previously called
    function WebSocketsConnect(const aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompression: boolean=true): RawUTF8;
    /// internal HTTP/1.1 and WebSockets compatible client
    // - you could use its properties after upgrading the connection to WebSockets
    function WebSockets: THttpClientWebSockets;
    /// returns true if the connection is a running WebSockets
    // - may be false even if fSocket<>nil, e.g. when gracefully disconnected
    function WebSocketsConnected: boolean;
    /// will set the HTTP header as expected by THttpClientWebSockets.Request to
    // perform the Callback() query in wscNonBlockWithoutAnswer mode
    procedure CallbackNonBlockingSetHeader(out Header: RawUTF8); override;
    /// this event will be executed just after the HTTP client has been
    // upgraded to the expected WebSockets protocol
    // - supplied Sender parameter will be this TSQLHttpClientWebsockets instance
    // - it will be executed the first time, and also on each reconnection
    // occuring when the HTTP-TCP/IP link is re-created, and user re-authenticated
    // - this event handler is the right place to setup link-driven connection,
    // e.g. SOA real-time callbacks registration (using Sender.Services)
    property OnWebSocketsUpgraded: TOnRestClientNotify
      read fOnWebSocketsUpgraded write fOnWebSocketsUpgraded;
    /// this event handler will be executed when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
    /// customize the internal REST loop delay
    // - to be defined before WebSocketsUpdate/WebSocketsConnect
    // - will set TWebSocketProcessSettings.LoopDelay value at WebSocketsUpgrade
    // - will override LoopDelay from DefaultWebSocketProcessSettings
    property WebSocketLoopDelay: integer read fWebSocketLoopDelay write fWebSocketLoopDelay;
    /// returns a reference to default settings for every new WebSocket process
    function DefaultWebSocketProcessSettings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
  end;

  {$endif NOHTTPCLIENTWEBSOCKETS}

  /// HTTP/1.1 RESTful JSON mORMot Client abstract class using either WinINet,
  // WinHTTP or libcurl API
  // - not to be called directly, but via TSQLHttpClientWinINet or (even
  // better) TSQLHttpClientWinHTTP overridden classes under Windows
  TSQLHttpClientRequest = class(TSQLHttpClientGeneric)
  protected
    fRequest: THttpRequest;
    fRequestClass: THttpRequestClass;
    /// call fWinAPI.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// set the fWinAPI class
    // - the overridden implementation should set the expected fWinAPIClass
    procedure InternalSetClass; virtual; abstract;
  public
    /// internal class instance used for the connection
    // - will return either a TWinINet, a TWinHTTP or a TCurlHTTP class instance
    property Request: THttpRequest read fRequest;
    /// allows to ignore untrusted SSL certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreSSLCertificateErrors: boolean
      read fExtendedOptions.IgnoreSSLCertificateErrors
      write fExtendedOptions.IgnoreSSLCertificateErrors;
    /// optional Authentication Scheme
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme write fExtendedOptions.Auth.Scheme;
    /// optional User Name for Authentication
    property AuthUserName: SynUnicode
      read fExtendedOptions.Auth.UserName write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SynUnicode
      read fExtendedOptions.Auth.Password write fExtendedOptions.Auth.Password;
  end;

  /// meta-class of TSQLHttpClientRequest types
  TSQLHttpClientRequestClass = class of TSQLHttpClientRequest;

  {$ifdef USEWININET}
  /// HTTP/1.1 RESTful JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TSQLHttpClient using SynCrtSock
  // on a local machine, but was found to be faster throughout local networks
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TSQLHttpClientWinINet = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

  /// HTTP/1.1 RESTful JSON Client class using WinHTTP API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure HTTPS connection - so it seems to be used
  // in your client programs: TSQLHttpClient will therefore map to this class
  // - WinHTTP does not share directly any proxy settings with Internet Explorer.
  // The default WinHTTP proxy configuration is set by either
  // proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
  // netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
  // the current user's proxy settings for Internet Explorer (under 64 bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinHTTP API can be used from a service or a server
  // - is implemented by creating a TWinHTTP internal class instance
  TSQLHttpClientWinHTTP = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USEWININET}

  {$ifdef USELIBCURL}
  /// HTTP/1.1 RESTful JSON Client class using libculr
  // - will handle HTTP and HTTPS, if OpenSSL or similar libray is available
  TSQLHttpClientCurl = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USELIBCURL}

  {$ifdef ONLYUSEHTTPSOCKET}
  /// HTTP/1.1 RESTful JSON default mORMot Client class
  // -  maps the raw socket implementation class
  TSQLHttpClient = TSQLHttpClientWinSock;
  {$ifdef USELIBCURL}
  TSQLHttpsClient = TSQLHttpClientCurl;
  {$else}
  {$ifdef USEWININET}
  TSQLHttpsClient = TSQLHttpClientWinHTTP;
  {$else}
  TSQLHttpsClient = TSQLHttpClientWinSock; // (Android) fallback to non-TLS class
  {$endif USEWININET}
  {$endif USELIBCURL}
  {$else ONLYUSEHTTPSOCKET}
  {$ifdef USEWININET}
  /// HTTP/1.1 RESTful JSON default mORMot Client class
  // - under Windows, maps the TSQLHttpClientWinHTTP class
  TSQLHttpClient = TSQLHttpClientWinHTTP;
  /// HTTP/HTTPS RESTful JSON default mORMot Client class
  // - under Windows, maps the TSQLHttpClientWinHTTP class, or TSQLHttpClientCurl
  // under Linux
  TSQLHttpsClient = TSQLHttpClientWinHTTP;
  {$else}
  TSQLHttpClient = TSQLHttpClientWinSock;
  TSQLHttpsClient = TSQLHttpClientWinSock; // wouls use SChannel if available
  {$endif USEWININET}
  {$endif ONLYUSEHTTPSOCKET}

var
  /// a global hook variable, able to set WebSockets logging to full verbose
  // - checked by TSQLHttpClientWebsockets.WebSocketsConnect()
  HttpClientFullWebSocketsLog: Boolean;


implementation

{ TSQLHttpClientGeneric }

procedure TSQLHttpClientGeneric.InternalURI(var Call: TSQLRestURIParams);
var Head, Content, ContentType: RawUTF8;
    P, PBeg: PUTF8Char;
    res: Int64Rec;
{$ifdef WITHLOG}
    log: ISynLog;
begin
  log := fLogClass.Enter('InternalURI %', [Call.Method], self);
{$else}
begin
{$endif}
  if InternalCheckOpen then begin
    Head := Call.InHead;
    Content := Call.InBody;
    ContentType := JSON_CONTENT_TYPE_VAR; // consider JSON by default
    P := pointer(Head);
    while P<>nil do begin
      PBeg := P;
      if IdemPChar(PBeg,'CONTENT-TYPE:') then begin
        ContentType := GetNextLine(PBeg+14,P); // retrieve customized type
        if P=nil then // last entry in header
          SetLength(Head,PBeg-pointer(Head)) else
          system.delete(Head,PBeg-pointer(Head)+1,P-PBeg);
        Head := trim(Head);
        break;
      end;
      P := GotoNextLine(P);
    end;
    if Content<>'' then // always favor content type from binary
      ContentType := GetMimeContentTypeFromBuffer(pointer(Content),Length(Content),ContentType);
    fSafe.Enter;
    try
      res := InternalRequest(Call.Url,Call.Method,Head,Content,ContentType);
    finally
      fSafe.Leave;
    end;
    Call.OutStatus := res.Lo;
    Call.OutInternalState := res.Hi;
    Call.OutHead := Head;
    Call.OutBody := Content;
  end else
    Call.OutStatus := HTTP_NOTIMPLEMENTED; // 501 indicates not socket closed
{$ifdef WITHLOG}
  if log<>nil then
    with Call do
      log.Log(sllClient,'% % status=% len=% state=%',
        [method,url,OutStatus,length(OutBody),OutInternalState],self);
{$endif}
end;

procedure TSQLHttpClientGeneric.SetCompression(Value: TSQLHttpCompressions);
begin
  fCompression := Value;
  InternalClose; // force re-create connection at next request
end;

procedure TSQLHttpClientGeneric.SetKeepAliveMS(Value: cardinal);
begin
  fKeepAliveMS := Value;
  InternalClose; // force re-create connection at next request
end;

constructor TSQLHttpClientGeneric.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; aHttps: boolean; const aProxyName, aProxyByPass: AnsiString;
  aSendTimeout,aReceiveTimeout,aConnectTimeout: DWORD);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fHttps := aHttps;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
  fCompression := []; // may add hcSynLZ or hcDeflate for AJAX clients
  if aConnectTimeout=0 then
    fConnectTimeout := HTTP_DEFAULT_CONNECTTIMEOUT else
    fConnectTimeout := aConnectTimeout;
  if aSendTimeout=0 then
    fSendTimeout := HTTP_DEFAULT_SENDTIMEOUT else
    fSendTimeout := aSendTimeout;
  if aReceiveTimeout=0 then
    fReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT else
    fReceiveTimeout := aReceiveTimeout;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
end;

constructor TSQLHttpClientGeneric.CreateForRemoteLogging(const aServer: AnsiString;
  aLogClass: TSynLogClass; aPort: Integer; const aRoot: RawUTF8);
var aModel: TSQLModel;
begin
  if not Assigned(aLogClass) then
    raise ECommunicationException.CreateUTF8('%.CreateForRemoteLogging(LogClass=nil)',[self]);
  aModel := TSQLModel.Create([],aRoot);
  Create(aServer,AnsiString(UInt32ToUtf8(aPort)),aModel,aPort=443);
  aModel.Owner := self;
  ServerRemoteLogStart(aLogClass,true);
  fRemoteLogClass.Log(sllTrace,
    'Echoing to remote server http://%/%/RemoteLog:%',[aServer,aRoot,aPort]);
end;

procedure TSQLHttpClientGeneric.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition=nil then
    exit;
  inherited DefinitionTo(Definition); // save Kind + User/Password
  if fHttps then
    Definition.ServerName := 'https://';
  Definition.ServerName := FormatUTF8('%%:%',[Definition.ServerName,fServer,fPort]);
  Definition.DatabaseName := UrlEncode([
   'IgnoreSSLCertificateErrors',ord(fExtendedOptions.IgnoreSSLCertificateErrors),
   'ConnectTimeout',fConnectTimeout,'SendTimeout',fSendTimeout,'ReceiveTimeout',fReceiveTimeout,
   'ProxyName',fProxyName,'ProxyByPass',fProxyByPass]);
  Definition.DatabaseName := copy(Definition.DatabaseName,2,MaxInt); // trim leading '?'
end;

constructor TSQLHttpClientGeneric.RegisteredClassCreateFrom(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition);
var URI: TURI;
    P: PUTF8Char;
    V: cardinal;
    tmp: RawUTF8;
begin
  URI.From(aDefinition.ServerName);
  Create(URI.Server,URI.Port,aModel,URI.Https);
  P := Pointer(aDefinition.DataBaseName);
  while P<>nil do begin
    if UrlDecodeCardinal(P,'CONNECTTIMEOUT',V) then
      fConnectTimeout := V else
    if UrlDecodeCardinal(P,'SENDTIMEOUT',V) then
      fSendTimeout := V else
    if UrlDecodeCardinal(P,'RECEIVETIMEOUT',V) then
      fReceiveTimeout := V else
    if UrlDecodeValue(P,'PROXYNAME',tmp) then
      fProxyName := CurrentAnsiConvert.UTF8ToAnsi(tmp) else
    if UrlDecodeValue(P,'PROXYBYPASS',tmp) then
      fProxyByPass := CurrentAnsiConvert.UTF8ToAnsi(tmp);
    if UrlDecodeCardinal(P,'IGNORESSLCERTIFICATEERRORS',V,@P) then
      fExtendedOptions.IgnoreSSLCertificateErrors := Boolean(V);
  end;
  inherited RegisteredClassCreateFrom(aModel,aDefinition); // call SetUser()
end;

constructor TSQLHttpClientGeneric.Create(const aServer: TSQLRestServerURIString;
  aModel: TSQLModel; aDefaultPort: integer; aHttps: boolean);
var URI: TSQLRestServerURI;
begin
  URI.URI := aServer;
  if URI.Root<>'' then
    aModel.Root := URI.Root;
  if (URI.Port='') and (aDefaultPort<>0) then
    URI.Port := Int32ToUtf8(aDefaultPort);
  Create(SockString(URI.Address),SockString(URI.Port),aModel,aHttps);
end;

function TSQLHttpClientGeneric.HostName: AnsiString;
begin
  if fServer<>'' then
    if fPort<>'' then
      result := fServer+':'+fPort else
      result := fServer else
    result := '';
end;


{ TSQLHttpClientWinSock }

function TSQLHttpClientWinSock.InternalCheckOpen: boolean;
var started, elapsed: Int64;
    wait, retry: integer;
begin
  result := fSocket<>nil;
  if result or (isDestroying in fInternalState) then
    exit;
  fSafe.Enter;
  try
    if fSocket=nil then begin
      if fSocketClass=nil then
        fSocketClass := THttpClientSocket;
      retry := 0;
      if fConnectRetrySeconds=0 then
        started := 0 else
        started := GetTickCount64;
      repeat
        try
          fSocket := fSocketClass.Open(fServer,fPort,cslTCP,fConnectTimeout,fHttps);
        except
          on E: Exception do begin
            FreeAndNil(fSocket);
            if started=0 then
              exit;
            elapsed := GetTickCount64-started;
            if elapsed>=fConnectRetrySeconds shl 10 then
              exit;
            inc(retry);
            if elapsed<500 then
              wait := 100 else
              wait := 1000; // checking every second is enough
            {$ifdef WITHLOG}
            fLogClass.Add.Log(sllTrace, 'InternalCheckOpen: % on %:% after %' +
              ' -> wait % and retry #% up to % seconds',
              [E.ClassType,fServer,fPort,MicroSecToString(elapsed*1000),
               MicroSecToString(wait*1000),retry,fConnectRetrySeconds], self);
            {$endif}
            SleepHiRes(wait);
          end;
        end;
      until fSocket<>nil;
      fConnectRetrySeconds := 0; // retry done once at startup
      if fExtendedOptions.UserAgent<>'' then
        fSocket.UserAgent := fExtendedOptions.UserAgent;
      if fModel<>nil then
        fSocket.ProcessName := FormatUTF8('%/%',[fPort,fModel.Root]);
      if fSendTimeout>0 then
        fSocket.SendTimeout := fSendTimeout;
      if fReceiveTimeout>0 then
        fSocket.ReceiveTimeout := fReceiveTimeout;
      {$ifdef USETCPPREFIX}
      fSocket.TCPPrefix := 'magic';
      {$endif}
      // note that first registered algo will be the prefered one
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CFB encryption + SynLZ compression
        fSocket.RegisterCompress(CompressShaAes,{CompressMinSize=}0);
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fSocket.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP gzip compression
        fSocket.RegisterCompress(CompressGZip);
    end;
    result := true;
  finally
    fSafe.Leave;
  end;
end;

procedure TSQLHttpClientWinSock.InternalClose;
begin
  if fSocket<>nil then
    try
      InternalLog('InternalClose: fSocket.Free', sllTrace);
      FreeAndNil(fSocket);
    except
      ; // ignore any error here
    end;
end;

function TSQLHttpClientWinSock.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
begin
  {$ifdef WITHLOG}
  fLogFamily.SynLog.Log(sllTrace,'InternalRequest % calling %(%).Request',
     [method,fSocket.ClassType,pointer(fSocket)],self);
  {$endif}
  result.Lo := fSocket.Request(SockString(url),SockString(method),
    KeepAliveMS,SockString(Header),SockString(Data),SockString(DataType),false);
  result.Hi := fSocket.ServerInternalState;
  Header := fSocket.HeaderGetText;
  Data := fSocket.Content;
end;


{$ifndef NOHTTPCLIENTWEBSOCKETS}

{ TSQLHttpClientWebsockets }

function TSQLHttpClientWebsockets.InternalCheckOpen: boolean;
begin
  result := WebSocketsConnected;
  if result or (isDestroying in fInternalState) then
    exit; // already connected
  fSafe.Enter;
  try
    if fSocket=nil then
    try
      if fSocketClass=nil then
        fSocketClass := THttpClientWebSockets;
      InternalLog('InternalCheckOpen: calling %.Open', [fSocketClass]);
      result := inherited InternalCheckOpen;
      if result then begin
        include(fInternalState,isOpened);
        with fWebSocketParams do
          if AutoUpgrade then
            result := WebSocketsUpgrade(Key,Ajax,Compression)='';
      end;
    except
      result := false;
    end;
  finally
    fSafe.Leave;
  end;
end;

function TSQLHttpClientWebsockets.FakeCallbackRegister(Sender: TServiceFactoryClient;
  const Method: TServiceMethod; const ParamInfo: TServiceMethodArgument;
  ParamValue: Pointer): integer;
begin
  if WebSockets=nil then
    raise EServiceException.CreateUTF8('Missing %.WebSocketsUpgrade() call '+
      'to enable interface parameter callbacks for %.%(%: %)',
      [self,Sender.InterfaceTypeInfo^.Name,Method.URI,
       ParamInfo.ParamName^,ParamInfo.ArgTypeName^]);
  if ParamValue=nil then
    result := 0 else
    result := fFakeCallbacks.DoRegister(
      ParamValue,TInterfaceFactory.Get(ParamInfo.ArgTypeInfo));
end;

function TSQLHttpClientWebsockets.FakeCallbackUnregister(Factory: TInterfaceFactory;
  FakeCallbackID: integer; Instance: pointer): boolean;
var body,head,resp: RawUTF8;
begin
  if (FakeCallbackID=0) or not WebSocketsConnected then begin
    result := true; // nothing to notify
    exit;
  end;
  if WebSockets=nil then
    raise EServiceException.CreateUTF8('Missing %.WebSocketsUpgrade() call',[self]);
  FormatUTF8('{"%":%}',[Factory.InterfaceTypeInfo^.Name,FakeCallbackID],body);
  CallbackNonBlockingSetHeader(head); // frames gathering + no wait
  result := CallBack(mPOST,'CacheFlush/_callback_',body,resp,nil,0,@head) in
    [HTTP_SUCCESS,HTTP_NOCONTENT];
end;

function TSQLHttpClientWebsockets.CallbackRequest(Ctxt: THttpServerRequest): cardinal;
var params: TSQLRestURIParams;
begin
  if (Ctxt=nil) or
     ((Ctxt.InContentType<>'') and
      not IdemPropNameU(Ctxt.InContentType,JSON_CONTENT_TYPE)) then begin
    result := HTTP_BADREQUEST;
    exit;
  end;
  params.Init(Ctxt.URL,Ctxt.Method,Ctxt.InHeaders,Ctxt.InContent);
  InternalNotificationMethodExecute(params);
  Ctxt.OutContent := params.OutBody;
  Ctxt.OutCustomHeaders := params.OutHead;
  Ctxt.OutContentType := params.OutBodyType;
  result := params.OutStatus;
end;

constructor TSQLHttpClientWebsockets.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; aHttps: boolean; const aProxyName, aProxyByPass: AnsiString;
  aSendTimeout, aReceiveTimeout,aConnectTimeout: DWORD);
begin
  inherited;
  fDefaultWebSocketProcessSettings.SetDefaults;
end;

function TSQLHttpClientWebsockets.DefaultWebSocketProcessSettings: PWebSocketProcessSettings;
begin
  if self=nil then
    result := nil else
    result := @fDefaultWebSocketProcessSettings;
end;

function TSQLHttpClientWebsockets.WebSocketsConnected: boolean;
begin
  result := (self<>nil) and (fSocket<>nil) and
    fSocket.InheritsFrom(THttpClientWebSockets) and
    (THttpClientWebSockets(fSocket).WebSockets.State<=wpsRun);
end;

procedure TSQLHttpClientWebsockets.CallbackNonBlockingSetHeader(out Header: RawUTF8);
begin
  Header := 'Sec-WebSocket-REST: NonBlocking'; // frames gathering + no wait
end;

function TSQLHttpClientWebsockets.WebSockets: THttpClientWebSockets;
begin
  if fSocket=nil then
    if not InternalCheckOpen then begin
      result := nil;
      exit;
    end;
  result := fSocket as THttpClientWebSockets;
  if not Assigned(result.OnCallbackRequestProcess) then
    result.OnCallbackRequestProcess := CallbackRequest;
  if not Assigned(result.OnWebSocketsClosed) then
    result.OnWebSocketsClosed := OnWebSocketsClosed;
  result.Settings^ := fDefaultWebSocketProcessSettings;
end;

function TSQLHttpClientWebsockets.WebSocketsUpgrade(
  const aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean;
  aWebSocketsCompression: boolean): RawUTF8;
var sockets: THttpClientWebSockets;
{$ifdef WITHLOG}
    log: ISynLog;
begin
  log := fLogFamily.SynLog.Enter(self, 'WebSocketsUpgrade');
{$else}
begin
{$endif}
  sockets := WebSockets;
  if sockets=nil then
    result := 'Impossible to connect to the Server'
  else begin    
    if fWebSocketLoopDelay>0 then
      sockets.Settings^.LoopDelay := fWebSocketLoopDelay;
    result := sockets.WebSocketsUpgrade(Model.Root,
      aWebSocketsEncryptionKey,aWebSocketsAJAX,aWebSocketsCompression);
    if result='' then // no error message = success
      with fWebSocketParams do begin // store parameters for auto-reconnection
        AutoUpgrade := true;
        Key := aWebSocketsEncryptionKey;
        Compression := aWebSocketsCompression;
        Ajax := aWebSocketsAJAX;
        if Assigned(fOnWebSocketsUpgraded) then
          fOnWebSocketsUpgraded(self);
      end;
  end;
{$ifdef WITHLOG}
  if log<>nil then
    if result<>'' then
      log.Log(sllWarning,'[%] error upgrading %',[result,sockets],self) else
      log.Log(sllHTTP,'HTTP link upgraded to WebSockets using %',[sockets],self);
{$endif}
end;

function TSQLHttpClientWebsockets.WebSocketsConnect(
  const aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean;
  aWebSocketsCompression: boolean): RawUTF8;
begin
  if WebSockets = nil then
    result := 'WebSockets=nil'
  else begin
    if HttpClientFullWebSocketsLog then
      WebSockets.Settings.SetFullLog;
    result := WebSocketsUpgrade(aWebSocketsEncryptionKey,aWebSocketsAJAX,aWebSocketsCompression);
    if result='' then
      if not ServerTimestampSynchronize then
        result := 'ServerTimestampSynchronize';
  end;
  if result<>'' then
    raise ECommunicationException.CreateUTF8('%.WebSocketsConnect failed on %:%/% -> %',
      [self,Server,Port,Model.Root,result]);
end;

{$endif NOHTTPCLIENTWEBSOCKETS}


{ TSQLHttpClientRequest }

function TSQLHttpClientRequest.InternalCheckOpen: boolean;
var timeout: Int64;
begin
  result := fRequest<>nil;
  if result or (isDestroying in fInternalState) then
    exit;
  fSafe.Enter;
  try
    if fRequest=nil then begin
      InternalSetClass;
      if fRequestClass=nil then
        raise ECommunicationException.CreateUTF8('fRequestClass=nil for %',[self]);
      timeout := GetTickCount64+fConnectRetrySeconds shl 10;
      repeat
        try
          fRequest := fRequestClass.Create(fServer,fPort,fHttps,
            fProxyName,fProxyByPass,fConnectTimeout,fSendTimeout,fReceiveTimeout);
        except
          on E: Exception do begin
            FreeAndNil(fRequest);
            if GetTickCount64>=timeout then
              exit;
            {$ifdef WITHLOG}
            fLogClass.Add.Log(sllTrace,
              'InternalCheckOpen: % on %:% -> wait and retry up to % seconds',
              [E.ClassType,fServer,fPort,fConnectRetrySeconds], self);
            {$endif}
            SleepHiRes(250);
          end;
        end;
      until fRequest<>nil;
      fRequest.ExtendedOptions := fExtendedOptions;
      // note that first registered algo will be the prefered one
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CFB encryption + SynLZ compression
        fRequest.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fRequest.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP zip/deflate compression
        fRequest.RegisterCompress(CompressGZip);
    end;
    result := true;
  finally
    fSafe.Leave;
  end;
end;

procedure TSQLHttpClientRequest.InternalClose;
begin
  FreeAndNil(fRequest);
end;

function TSQLHttpClientRequest.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
var OutHeader, OutData: RawByteString;
begin
  if fRequest=nil then
    result.Lo := HTTP_NOTIMPLEMENTED else begin
    result.Lo := fRequest.Request(SockString(url),SockString(method),
      KeepAliveMS,SockString(Header),SockString(Data),SockString(DataType),
      SockString(OutHeader),SockString(OutData));
    result.Hi := GetCardinal(FindNameValue(pointer(OutHeader),'SERVER-INTERNALSTATE:'));
    Header := OutHeader;
    Data := OutData;
  end;
end;


{$ifdef USEWININET}

{ TSQLHttpClientWinINet }

procedure TSQLHttpClientWinINet.InternalSetClass;
begin
  fRequestClass := TWinINet;
  inherited;
end;


{ TSQLHttpClientWinHTTP }

procedure TSQLHttpClientWinHTTP.InternalSetClass;
begin
  fRequestClass := TWinHTTP;
  inherited;
end;

{$endif}

{$ifdef USELIBCURL}

{ TSQLHttpClientCurl}

procedure TSQLHttpClientCurl.InternalSetClass;
begin
  fRequestClass := TCurlHTTP;
  inherited;
end;

{$endif USELIBCURL}

procedure StatusCodeToErrorMsgInternal(Code: integer; var result: RawUTF8);
begin
  result := SynCrtSock.StatusCodeToReason(Code); // faster and more complete
end;

initialization
  StatusCodeToErrorMessage := StatusCodeToErrorMsgInternal; // as in mORMotHttpServer
  TSQLHttpClientWinSock.RegisterClassNameForDefinition;
{$ifndef NOHTTPCLIENTWEBSOCKETS}
  TSQLHttpClientWebsockets.RegisterClassNameForDefinition;
{$endif}
{$ifdef USELIBCURL}
  TSQLHttpClientCurl.RegisterClassNameForDefinition;
{$endif}
{$ifdef USEWININET}
  TSQLHttpClientWinINet.RegisterClassNameForDefinition;
  TSQLHttpClientWinHTTP.RegisterClassNameForDefinition;
{$endif}
end.
