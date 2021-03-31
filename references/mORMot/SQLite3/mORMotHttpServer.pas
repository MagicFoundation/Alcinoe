/// HTTP/1.1 RESTFUL JSON Server classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpServer;

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
  - DigDiver (for HTTPS support)
  - cheemeng

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

   HTTP/1.1 RESTFUL JSON Client/Server for mORMot
  ************************************************

 - use internaly the JSON format for content communication
 - can be called by TSQLHttpClient class from direct Delphi clients
   (see mORMotHttpClient unit)
 - can be called by any JSON-aware AJAX application
 - can optionaly compress the returned data to optimize Internet bandwidth
 - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
   i.e. 400Mb/sec of duplex raw IP data, with about 200 µs only elapsed
   by request (direct call is 50 µs, so bottle neck is the Win32 API),
   i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
   of JSON data each)... try to find a faster JSON HTTP server! ;)

}

interface

{$define COMPRESSSYNLZ}
{ if defined, will use SynLZ for content compression
  - SynLZ is much faster than deflate/zip, so is preferred
  - can be set global for Client and Server applications
  - with SynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLHttpClient
    is compressed into 106 KB with no speed penalty (it's even a bit faster)
    whereas deflate, even with its level set to 1 (fastest), is 25 % slower
  - TSQLHttpClientGeneric.Compression shall contain hcSynLZ to handle it }

{$define COMPRESSDEFLATE}
{ if defined, will use gzip (and not deflate/zip) for content compression
  - can be set global for Client and Server applications
  - deflate/zip is just broken between browsers and client, and should be
    avoided: see http://stackoverflow.com/a/9186091
  - SynLZ is faster but only known by Delphi clients: you can enable deflate
    when the server is connected an AJAX application (not defined by default)
  - if you define both COMPRESSSYNLZ and COMPRESSDEFLATE, the server will use
    SynLZ if available, and deflate if not called from a Delphi client
  - TSQLHttpClientGeneric.Compression shall contain hcDeflate to handle it }

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - will work only with our THttpServer pure Delphi code, of course
  - not defined by default - should be set globally to the project conditionals,
  to be defined in both mORMotHttpClient and mORMotHttpServer units }

{$I Synopse.inc} // define HASINLINE WITHLOG ONLYUSEHTTPSOCKET


uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef KYLIX3}
  Types,
  LibC,
{$endif}
  SysUtils,
  Classes,
{$ifdef COMPRESSDEFLATE}
  SynZip,
{$endif}
{$ifdef COMPRESSSYNLZ}
  SynLZ,
{$endif}
  SynCrtSock,
  SynBidirSock, // for WebSockets
  SynCrypto,    // for CompressShaAes()
  SynCommons,
  SynTable,
  SynLog,
  mORMot;

type
  /// exception raised in case of a HTTP Server error
  EHttpServerException = class(ECommunicationException);

  /// available running options for TSQLHttpServer.Create() constructor
  // - useHttpApi to use kernel-mode HTTP.SYS server (THttpApiServer) with an
  // already registered URI (default way, similar to IIS/WCF security policy
  // as specified by Microsoft) - you would need to register the URI by hand,
  // e.g. in the Setup program, via code similar to this one:
  // ! THttpApiServer.AddUrlAuthorize('root','888',false,'+'))
  // - useHttpApiRegisteringURI will first registry the given URI, then use
  // kernel-mode HTTP.SYS server (THttpApiServer) - will need Administrator
  // execution rights at least one time (e.g. during setup); note that if
  // the URI is already registered, the server will still be launched, even if
  // the program does not run as Administrator - it is therefore sufficient
  // to run such a program once as Administrator to register the URI, when this
  // useHttpApiRegisteringURI option is set
  // - useHttpSocket will use the standard Sockets library (i.e. socket-based
  // THttpServer) - it will trigger the Windows firewall popup UAC window at
  // first execution
  // - useBidirSocket will use the standard Sockets library but via the
  // TWebSocketServerRest class, allowing HTTP connection upgrade to the
  // WebSockets protocol, allowing immediate event callbacks in addition to the
  // standard RESTful mode
  // - the first item should be the preferred one (see HTTP_DEFAULT_MODE)
  TSQLHttpServerOptions =
    ({$ifndef ONLYUSEHTTPSOCKET}useHttpApi, useHttpApiRegisteringURI, {$endif}
     useHttpSocket, useBidirSocket);

  /// available security options for TSQLHttpServer.Create() constructor
  // - default secNone will use plain HTTP connection
  // - secSSL will use HTTPS secure connection
  // - secSynShaAes will use our proprietary SHA-256 / AES-256-CTR encryption
  // identified as 'synshaaes' as ACCEPT-ENCODING: header parameter
  TSQLHttpServerSecurity =
    (secNone, secSSL, secSynShaAes);

const
  /// the default access rights used by the HTTP server if none is specified
  HTTP_DEFAULT_ACCESS_RIGHTS: PSQLAccessRights = @SUPERVISOR_ACCESS_RIGHTS;

  /// the kind of HTTP server to be used by default
  // - will define the best available server class, depending on the platform
  HTTP_DEFAULT_MODE = {$ifdef ONLYUSEHTTPSOCKET}useHttpSocket{$else}useHttpApiRegisteringURI{$endif};

type
  /// HTTP/1.1 RESTFUL JSON mORMot Server class
  // - this server is multi-threaded and not blocking
  // - under Windows, it will first try to use fastest http.sys kernel-mode
  // server (i.e. create a THttpApiServer instance); it should work OK under XP
  // or WS 2K3 - but you need to have administrator rights under Vista or Seven:
  // if http.sys fails to initialize, it will use the socket-based THttpServer;
  // a solution is to call the THttpApiServer.AddUrlAuthorize class method during
  // program setup for the desired port, or define a useHttpApiRegisteringURI
  // kind of server, in order to allow it for every user
  // - under Linux, only THttpServer is available
  // - you can specify useBidirSocket kind of server (i.e. TWebSocketServerRest)
  // if you want the HTTP protocol connection to be upgraded to a WebSockets
  // mode, to allow immediate callbacks from the server to the client
  // - just create it and it will serve SQL statements as UTF-8 JSON
  // - for a true AJAX server, expanded data is prefered - your code may contain:
  // ! DBServer.NoAJAXJSON := false;
  TSQLHttpServer = class(TSynPersistentLock)
  protected
    fOnlyJSONRequests: boolean;
    fShutdownInProgress: boolean;
    fHttpServer: THttpServerGeneric;
    fPort, fDomainName: AnsiString;
    fPublicAddress, fPublicPort: RawUTF8;
    /// internal servers to compute responses (protected by inherited fSafe)
    fDBServers: array of record
      Server: TSQLRestServer;
      RestAccessRights: PSQLAccessRights;
      Security: TSQLHttpServerSecurity;
    end;
    fHosts: TSynNameValue;
    fAccessControlAllowOrigin: RawUTF8;
    fAccessControlAllowOriginsMatch: TMatchs;
    fAccessControlAllowCredential: boolean;
    fRootRedirectToURI: array[boolean] of RawUTF8;
    fRedirectServerRootUriForExactCase: boolean;
    fHttpServerKind: TSQLHttpServerOptions;
    fLog: TSynLogClass;
    procedure SetAccessControlAllowOrigin(const Value: RawUTF8);
    procedure ComputeAccessControlHeader(Ctxt: THttpServerRequest);
    // assigned to fHttpServer.OnHttpThreadStart/Terminate e.g. to handle connections
    procedure HttpThreadStart(Sender: TThread); virtual;
    procedure HttpThreadTerminate(Sender: TThread); virtual;
    /// implement the server response - must be thread-safe
    function Request(Ctxt: THttpServerRequest): cardinal; virtual;
    function GetDBServerCount: integer; {$ifdef HASINLINE}inline;{$endif}
    function GetDBServer(Index: Integer): TSQLRestServer; {$ifdef HASINLINE}inline;{$endif}
    procedure SetDBServerAccessRight(Index: integer; Value: PSQLAccessRights);
    procedure SetDBServer(aIndex: integer; aServer: TSQLRestServer;
      aSecurity: TSQLHttpServerSecurity; aRestAccessRights: PSQLAccessRights);
    function GetDBServerNames: RawUTF8;
    function HttpApiAddUri(const aRoot,aDomainName: RawByteString;
      aSecurity: TSQLHttpServerSecurity; aRegisterURI,aRaiseExceptionOnError: boolean): RawUTF8;
    function NotifyCallback(aSender: TSQLRestServer; const aInterfaceDotMethodName,aParams: RawUTF8;
      aConnectionID: THttpServerConnectionID; aFakeCallID: integer; aResult, aErrorMsg: PRawUTF8): boolean;
  public
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a EHttpServer exception if binding failed
    // - specify one or more TSQLRestServer server class to be used: each
    // class must have an unique Model.Root value, to identify which TSQLRestServer
    // instance must handle a particular request from its URI
    // - port is an AnsiString, as expected by the WinSock API - in case of
    // useHttpSocket or useBidirSocket kind of server, you should specify the
    // public server address to bind to: e.g. '1.2.3.4:1234' - even for http.sys,
    // the public address could be used e.g. for TSQLRestServer.SetPublicURI()
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call:
    // it could be either a fully qualified case-insensitive domain name
    // an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port) - this
    // parameter is ignored by the TSQLHttpApiServer instance
    // - aHttpServerKind defines how the HTTP server itself will be implemented:
    // it will use by default optimized kernel-based http.sys server (useHttpApi),
    // optionally registering the URI (useHttpApiRegisteringURI) if needed,
    // or using the standard Sockets library (useHttpSocket), possibly in its
    // WebSockets-friendly version (useBidirSocket - you shoud call the
    // WebSocketsEnable method to initialize the available protocols)
    // - by default, the PSQLAccessRights will be set to nil
    // - the ServerThreadPoolCount parameter will set the number of threads
    // to be initialized to handle incoming connections (default is 32, which
    // may be sufficient for most cases, maximum is 256)
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalURL parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TSQLHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    // - for THttpServer, you can force aHeadersUnFiltered flag
    constructor Create(const aPort: AnsiString;
      const aServers: array of TSQLRestServer; const aDomainName: AnsiString='+';
      aHttpServerKind: TSQLHttpServerOptions=HTTP_DEFAULT_MODE; ServerThreadPoolCount: Integer=32;
      aHttpServerSecurity: TSQLHttpServerSecurity=secNone; const aAdditionalURL: AnsiString='';
      const aQueueName: SynUnicode=''; aHeadersUnFiltered: boolean=false); reintroduce; overload;
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a EHttpServer exception if binding failed
    // - specify one TSQLRestServer server class to be used
    // - port is an AnsiString, as expected by the WinSock API - in case of
    // useHttpSocket or useBidirSocket kind of server, you can specify the
    // public server address to bind to: e.g. '1.2.3.4:1234' - even for http.sys,
    // the public address could be used e.g. for TSQLRestServer.SetPublicURI()
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalURL parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TSQLHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    constructor Create(const aPort: AnsiString; aServer: TSQLRestServer;
      const aDomainName: AnsiString='+';
      aHttpServerKind: TSQLHttpServerOptions=HTTP_DEFAULT_MODE; aRestAccessRights: PSQLAccessRights=nil;
      ServerThreadPoolCount: Integer=32; aHttpServerSecurity: TSQLHttpServerSecurity=secNone;
      const aAdditionalURL: AnsiString=''; const aQueueName: SynUnicode=''); reintroduce; overload;
    /// create a Server instance, binded and listening on a TCP port to HTTP requests
    // - raise a EHttpServer exception if binding failed
    // - specify one TSQLRestServer instance to be published, and the associated
    // transmission definition; other parameters would be the standard one
    // - only the supplied aDefinition.Authentication will be defined
    // - under Windows, will use http.sys with automatic URI registration, unless
    // aDefinition.WebSocketPassword is set and binary WebSockets would be
    // expected with the corresponding encryption, or aForcedKind is overriden
    // - optional aWebSocketsLoopDelay parameter could be set for tuning
    // WebSockets responsiveness
    constructor Create(aServer: TSQLRestServer; aDefinition: TSQLHttpServerDefinition;
      aForcedKind: TSQLHttpServerOptions=HTTP_DEFAULT_MODE; aWebSocketsLoopDelay: integer=0); reintroduce; overload;
    /// release all memory, internal mORMot server and HTTP handlers
    destructor Destroy; override;
    /// you can call this method to prepare the HTTP server for shutting down
    // - it will call all associated TSQLRestServer.Shutdown methods, unless
    // noRestServerShutdown is true
    // - note that Destroy won't call this method on its own, since the
    // TSQLRestServer instances may have a life-time uncoupled from HTTP process
    procedure Shutdown(noRestServerShutdown: boolean=false);
    /// try to register another TSQLRestServer instance to the HTTP server
    // - each TSQLRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - an optional aRestAccessRights parameter is available to override the
    // default HTTP_DEFAULT_ACCESS_RIGHTS access right setting - but you shall
    // better rely on the authentication feature included in the framework
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - return true on success, false on error (e.g. duplicated Root value)
    function AddServer(aServer: TSQLRestServer; aRestAccessRights: PSQLAccessRights=nil;
      aHttpServerSecurity: TSQLHttpServerSecurity=secNone): boolean;
    /// un-register a TSQLRestServer from the HTTP server
    // - each TSQLRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - return true on success, false on error (e.g. specified server not found)
    function RemoveServer(aServer: TSQLRestServer): boolean;
    /// register a domain name to be redirected to a given Model.Root
    // - i.e. can be used to support some kind of virtual hosting
    // - by default, the URI would be used to identify which TSQLRestServer
    // instance to use, and the incoming HOST value would just be ignored
    // - you can specify here domain names which would be checked against
    // the incoming HOST header, to redirect to a given URI, as such:
    // ! DomainHostRedirect('project1.com','root1');
    // ! DomainHostRedirect('project2.com','root2');
    // ! DomainHostRedirect('blog.project2.com','root2/blog');
    // for the last entry, you may have for instance initialized a MVC web
    // server on the 'blog' sub-URI of the 'root2' TSQLRestServer via:
    // !constructor TMyMVCApplication.Create(aRestModel: TSQLRest; aInterface: PTypeInfo);
    // ! ...
    // ! fMainRunner := TMVCRunOnRestServer.Create(self,nil,'blog');
    // ! ...
    // - if aURI='' is given, the corresponding host redirection will be disabled
    // - note: by design, 'something.localhost' is likely to be not recognized
    // as aDomain, since 'localhost' can not be part of proper DNS resolution
    procedure DomainHostRedirect(const aDomain,aURI: RawUTF8);
    /// allow to temporarly redirect ip:port root URI to a given sub-URI
    // - by default, only sub-URI, as defined by TSQLRestServer.Model.Root, are
    // registered - you can define here a sub-URI to reach when the main server
    // is directly accessed from a browser, e.g. localhost:port will redirect to
    // localhost:port/RedirectedURI
    // - for http.sys server, would try to register '/' if aRegisterURI is TRUE
    // - by default, will redirect http://localhost:port unless you set
    // aHttpServerSecurity=secSSL so that it would redirect https://localhost:port
    procedure RootRedirectToURI(const aRedirectedURI: RawUTF8;
      aRegisterURI: boolean=true; aHttps: boolean=false);
    /// defines the WebSockets protocols to be used for useBidirSocket
    // - i.e. 'synopsebinary' and optionally 'synopsejson' protocols
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single REST server
    // - TWebSocketProtocolBinary will always be registered by this method
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    // - this method does nothing if the associated HttpServer class is not a
    // TWebSocketServerRest (i.e. this instance was not created as useBidirSocket)
    function WebSocketsEnable(const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompressed: boolean=true): TWebSocketServerRest; overload;
    /// defines the useBidirSocket WebSockets protocol to be used for a REST server
    // - same as the overloaded WebSocketsEnable() method, but the URI will be
    // forced to match the aServer.Model.Root value, as expected on the client
    // side by TSQLHttpClientWebsockets.WebSocketsUpgrade()
    function WebSocketsEnable(aServer: TSQLRestServer;
      const aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean=false;
      aWebSocketsCompressed: boolean=true): TWebSocketServerRest; overload;
    /// the associated running HTTP server instance
    // - either THttpApiServer (available only under Windows), THttpServer or
    // TWebSocketServerRest (on any system)
    property HttpServer: THttpServerGeneric read fHttpServer;
    /// the TCP/IP (address and) port on which this server is listening to
    // - may contain the public server address to bind to: e.g. '1.2.3.4:1234'
    // - see PublicAddress and PublicPort properties if you want to get the
    // true IP port or address
    property Port: AnsiString read fPort;
    /// the TCP/IP public address on which this server is listening to
    // - equals e.g. '1.2.3.4' if Port = '1.2.3.4:1234'
    // - if Port does not contain an explicit address (e.g. '1234'), the current
    // computer host name would be assigned as PublicAddress
    property PublicAddress: RawUTF8 read fPublicAddress;
    /// the TCP/IP public port on which this server is listening to
    // - equals e.g. '1234' if Port = '1.2.3.4:1234'
    property PublicPort: RawUTF8 read fPublicPort;
    /// the URLprefix used for internal HttpAddUrl API call
    property DomainName: AnsiString read fDomainName;
    /// read-only access to the number of registered internal servers
    property DBServerCount: integer read GetDBServerCount;
    /// read-only access to all internal servers
    property DBServer[Index: integer]: TSQLRestServer read GetDBServer;
    /// write-only access to all internal servers access right
    // - can be used to override the default HTTP_DEFAULT_ACCESS_RIGHTS setting
    property DBServerAccessRight[Index: integer]: PSQLAccessRights write SetDBServerAccessRight;
    /// find the first instance of a registered REST server
    // - note that the same REST server may appear several times in this HTTP
    // server instance, e.g. with diverse security options
    function DBServerFind(aServer: TSQLRestServer): integer;
    /// set this property to TRUE if the server must only respond to
    // request of MIME type APPLICATION/JSON
    // - the default is false, in order to allow direct view of JSON from
    // any browser
    property OnlyJSONRequests: boolean read fOnlyJSONRequests write fOnlyJSONRequests;
    /// enable cross-origin resource sharing (CORS) for proper AJAX process
    // - see @https://developer.mozilla.org/en-US/docs/HTTP/Access_control_CORS
    // - can be set e.g. to '*' to allow requests from any site/domain; or
    // specify an CSV white-list of URI to be allowed as origin e.g. as
    // 'https://foo.example1,https://foo.example2' or 'https://*.foo.example' or
    // (faster) '*.foo.example1,*.foo.example2' following the TMatch syntax
    // - see also AccessControlAllowCredential property
    property AccessControlAllowOrigin: RawUTF8
      read fAccessControlAllowOrigin write SetAccessControlAllowOrigin;
    /// enable cookies, authorization headers or TLS client certificates CORS exposition
    // - this option works with the AJAX XMLHttpRequest.withCredentials property
    // on client/JavaScript side, as stated by
    // @https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials
    // - see @https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Access-Control-Allow-Credentials
    property AccessControlAllowCredential: boolean
      read fAccessControlAllowCredential write fAccessControlAllowCredential;
    /// enable redirectoin to fix any URI for a case-sensitive match of Model.Root
    // - by default, TSQLRestServer.Model.Root would be accepted with case
    // insensitivity; but it may induce errors for HTTP cookies, since they
    // are bound with '; Path=/ModelRoot', which is case-sensitive on the
    // browser side
    // - set this property to TRUE so that only exact case URI would be handled
    // by TSQLRestServer.URI(), and any case-sensitive URIs (e.g. /Root/... or
    // /ROOT/...) would be temporary redirected to Model.Root (e.g. /root/...)
    // via a HTTP 307 command
    property RedirectServerRootUriForExactCase: boolean
      read fRedirectServerRootUriForExactCase write fRedirectServerRootUriForExactCase;
  end;

  /// callback expected by TSQLHTTPRemoteLogServer to notify about a received log
  TRemoteLogReceivedOne = procedure(const Text: RawUTF8) of object;

  {$M+}
  /// limited HTTP server which is will receive remote log notifications
  // - this will create a simple in-memory mORMot server, which will trigger
  // a supplied callback when a remote log is received
  // - see TSQLHttpClientWinGeneric.CreateForRemoteLogging() for the client side
  // - used e.g. by the LogView tool
  TSQLHTTPRemoteLogServer = class(TSQLHttpServer)
  protected
    fServer: TSQLRestServerFullMemory;
    fEvent: TRemoteLogReceivedOne;
  public
    /// initialize the HTTP server and an internal mORMot server
    // - you can share several HTTP log servers on the same port, if you use
    // a dedicated root URI and use the http.sys server (which is the default)
    constructor Create(const aRoot: RawUTF8; aPort: integer;
      const aEvent: TRemoteLogReceivedOne); reintroduce;
    /// release the HTTP server and its internal mORMot server
    destructor Destroy; override;
    /// the associated mORMot server instance running with this HTTP server
    property Server: TSQLRestServerFullMemory read fServer;
  published
    /// this HTTP server will publish a 'RemoteLog' method-based service
    // - expecting PUT with text as body, at http://server/root/RemoteLog
    procedure RemoteLog(Ctxt: TSQLRestServerURIContext);
  end;
  {$M-}


var
  /// a global hook variable, able to enhance WebSockets logging
  // - when a TSQLHttpServer is created from a TSQLHttpServerDefinition
  HttpServerFullWebSocketsLog: Boolean;


function ToText(opt: TSQLHttpServerOptions): PShortString; overload;
function ToText(sec: TSQLHttpServerSecurity): PShortString; overload;


implementation

function ToText(opt: TSQLHttpServerOptions): PShortString;
begin
  result := GetEnumName(TypeInfo(TSQLHttpServerOptions),ord(opt));
end;

function ToText(sec: TSQLHttpServerSecurity): PShortString;
begin
  result := GetEnumName(TypeInfo(TSQLHttpServerSecurity),ord(sec));
end;

{ TSQLHttpServer }

function TSQLHttpServer.AddServer(aServer: TSQLRestServer;
  aRestAccessRights: PSQLAccessRights; aHttpServerSecurity: TSQLHttpServerSecurity): boolean;
var i,n: integer;
    log: ISynLog;
begin
  result := False;
  if (self=nil) or (aServer=nil) or (aServer.Model=nil) then
    exit;
  log := fLog.Enter(self, 'AddServer');
  fSafe.Lock; // protect fDBServers[]
  try
    n := length(fDBServers);
    for i := 0 to n-1 do
      if (fDBServers[i].Server.Model.URIMatch(aServer.Model.Root)<>rmNoMatch) and
         (fDBServers[i].Security=aHttpServerSecurity) then
        exit; // register only once per URI Root address and per protocol
    {$ifndef ONLYUSEHTTPSOCKET}
    if fHttpServerKind in [useHttpApi,useHttpApiRegisteringURI] then
      if HttpApiAddUri(aServer.Model.Root,fDomainName,aHttpServerSecurity,
         fHttpServerKind=useHttpApiRegisteringURI,false)<>'' then
        exit;
    {$endif}
    SetLength(fDBServers,n+1);
    SetDBServer(n,aServer,aHttpServerSecurity,aRestAccessRights);
    fHttpServer.ProcessName := GetDBServerNames;
    result := true;
  finally
    fSafe.UnLock;
    if log<>nil then
      log.Log(sllHttp,'AddServer(%,Root=%,Port=%,Public=%:%)=%',
        [aServer,aServer.Model.Root,fPort,fPublicAddress,fPublicPort,
         BOOL_STR[result]],self);
  end;
end;

function TSQLHttpServer.DBServerFind(aServer: TSQLRestServer): integer;
begin
  fSafe.Lock; // protect fDBServers[]
  try
    for result := 0 to Length(fDBServers)-1 do
      if fDBServers[result].Server=aServer then
        exit;
    result := -1;
  finally
    fSafe.UnLock;
  end;
end;

function TSQLHttpServer.RemoveServer(aServer: TSQLRestServer): boolean;
var i,j,n: integer;
    log: ISynLog;
begin
  result := False;
  if (self=nil) or (aServer=nil) or (aServer.Model=nil) then
    exit;
  log := fLog.Enter(self, 'RemoveServer');
  fSafe.Lock; // protect fDBServers[]
  try
    n := high(fDBServers);
    for i := n downto 0 do // may appear several times, with another Security
    if fDBServers[i].Server=aServer then begin
      {$ifndef ONLYUSEHTTPSOCKET}
      if fHttpServer.InheritsFrom(THttpApiServer) then
        if THttpApiServer(fHttpServer).RemoveUrl(aServer.Model.Root,fPublicPort,
           fDBServers[i].Security=secSSL,fDomainName)<>NO_ERROR then
          log.Log(sllLastError,'%.RemoveUrl(%)',[self,aServer.Model.Root],self);
      {$endif}
      for j := i to n-1 do
        fDBServers[j] := fDBServers[j+1];
      SetLength(fDBServers,n);
      dec(n);
      aServer.OnNotifyCallback := nil;
      aServer.SetPublicURI('','');
      result := true; // don't break here: may appear with another Security
    end;
  finally
    fSafe.UnLock;
    if log<>nil then
      log.Log(sllHttp,'%.RemoveServer(Root=%)=%',
        [self,aServer.Model.Root,BOOL_STR[result]],self);
  end;
end;

procedure TSQLHttpServer.DomainHostRedirect(const aDomain,aURI: RawUTF8);
var uri: TURI;
begin
  if uri.From(aDomain) and EndWith(uri.Server,'.LOCALHOST') then
      fLog.Add.Log(sllWarning, 'DomainHostRedirect(%) is very likely to be unresolved'+
        ': consider using a real host name instead of the loopback',[aDomain],self);
  if aURI='' then
    fHosts.Delete(aDomain) else
    fHosts.Add(aDomain,aURI); // e.g. Add('project1.com','root1')
end;

constructor TSQLHttpServer.Create(const aPort: AnsiString;
  const aServers: array of TSQLRestServer; const aDomainName: AnsiString;
  aHttpServerKind: TSQLHttpServerOptions; ServerThreadPoolCount: Integer;
  aHttpServerSecurity: TSQLHttpServerSecurity; const aAdditionalURL: AnsiString;
  const aQueueName: SynUnicode; aHeadersUnFiltered: boolean);
var i,j: integer;
    ServersRoot: RawUTF8;
    ErrMsg: RawUTF8;
    log: ISynLog;
begin
  {$ifdef WITHLOG} // this is the only place where we check for WITHLOG
  if high(aServers)<0 then
    fLog := TSQLLog else
    fLog := aServers[0].LogClass;
  log := fLog.Enter('Create % (%) on port %',[ToText(aHttpServerKind)^,
    ToText(aHttpServerSecurity)^,aPort],self);
  {$endif}
  inherited Create;
  SetAccessControlAllowOrigin(''); // deny CORS by default
  fHosts.Init(false);
  fDomainName := aDomainName;
  fPort := aPort;
  Split(RawUTF8(fPort),':',fPublicAddress,fPublicPort);
  if fPublicPort='' then begin // you should better set aPort='publicip:port'
    fPublicPort := fPublicAddress;
    fPublicAddress := ExeVersion.Host;
  end;
  fHttpServerKind := aHttpServerKind;
  if high(aServers)>=0 then begin
    for i := 0 to high(aServers) do
      if (aServers[i]=nil) or (aServers[i].Model=nil) then
        ErrMsg := 'Invalid TSQLRestServer';
    if ErrMsg='' then
      for i := 0 to high(aServers) do
      with aServers[i].Model do begin
        ServersRoot := ServersRoot+' '+Root;
        for j := i+1 to high(aServers) do
          if aServers[j].Model.URIMatch(Root)<>rmNoMatch then
            FormatUTF8('Duplicated Root URI: % and %',[Root,aServers[j].Model.Root],ErrMsg);
      end;
    if ErrMsg<>'' then
       raise EHttpServerException.CreateUTF8('%.Create(% ): %',[self,ServersRoot,ErrMsg]);
    // associate before HTTP server is started, for TSQLRestServer.BeginCurrentThread
    SetLength(fDBServers,length(aServers));
    for i := 0 to high(aServers) do
      SetDBServer(i,aServers[i],aHttpServerSecurity,HTTP_DEFAULT_ACCESS_RIGHTS);
  end;
  {$ifndef USETCPPREFIX}
  {$ifndef ONLYUSEHTTPSOCKET}
  if aHttpServerKind in [useHttpApi,useHttpApiRegisteringURI] then
  try
    if PosEx('Wine',OSVersionInfoEx)>0 then
      log.Log(sllWarning,'%: httpapi probably not supported on % -> try useHttpSocket',
        [ToText(aHttpServerKind)^,OSVersionInfoEx]);
    // first try to use fastest http.sys
    fHttpServer := THttpApiServer.Create(false,
      aQueueName,HttpThreadStart,HttpThreadTerminate,GetDBServerNames);
    for i := 0 to high(aServers) do
      HttpApiAddUri(aServers[i].Model.Root,fDomainName,aHttpServerSecurity,
        fHttpServerKind=useHttpApiRegisteringURI,true);
    if aAdditionalURL<>'' then
      HttpApiAddUri(aAdditionalURL,fDomainName,aHttpServerSecurity,
        fHttpServerKind=useHttpApiRegisteringURI,true);
  except
    on E: Exception do begin
      log.Log(sllError,'% for % % at%  -> fallback to socket-based server',
        [E,ToText(aHttpServerKind)^,fHttpServer,ServersRoot],self);
      FreeAndNil(fHttpServer); // if http.sys initialization failed
    end;
  end;
  {$endif}
  {$endif}
  if fHttpServer=nil then begin
    // http.sys not running -> create one instance of our pure socket server
    if aHttpServerKind=useBidirSocket then
      fHttpServer := TWebSocketServerRest.Create(
        fPort,HttpThreadStart,HttpThreadTerminate,GetDBServerNames) else
      fHttpServer := THttpServer.Create(fPort,HttpThreadStart,HttpThreadTerminate,
        GetDBServerNames,ServerThreadPoolCount,30000,aHeadersUnFiltered);
    THttpServer(fHttpServer).WaitStarted;
    {$ifdef USETCPPREFIX}
    THttpServer(fHttpServer).TCPPrefix := 'magic';
    {$endif}
  end;
  fHttpServer.OnRequest := Request;
  if aHttpServerSecurity=secSynShaAes then
    fHttpServer.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
{$ifdef COMPRESSSYNLZ} // SynLZ registered first, since will be prefered
  fHttpServer.RegisterCompress(CompressSynLZ);
{$endif}
{$ifdef COMPRESSDEFLATE}
  fHttpServer.RegisterCompress(CompressGZip);
{$endif}
{$ifndef ONLYUSEHTTPSOCKET}
  if fHttpServer.InheritsFrom(THttpApiServer) then
    // allow fast multi-threaded requests
    if ServerThreadPoolCount>1 then
      THttpApiServer(fHttpServer).Clone(ServerThreadPoolCount-1);
{$endif}
  // last HTTP server handling callbacks would be set for the TSQLRestServer(s)
  if fHttpServer.CanNotifyCallback then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.OnNotifyCallback := NotifyCallback;
  log.Log(sllHttp,'% initialized for%',[fHttpServer,ServersRoot],self);
end;

constructor TSQLHttpServer.Create(const aPort: AnsiString;
  aServer: TSQLRestServer; const aDomainName: AnsiString;
  aHttpServerKind: TSQLHttpServerOptions; aRestAccessRights: PSQLAccessRights;
  ServerThreadPoolCount: Integer; aHttpServerSecurity: TSQLHttpServerSecurity;
  const aAdditionalURL: AnsiString; const aQueueName: SynUnicode);
begin
  Create(aPort,[aServer],aDomainName,aHttpServerKind,ServerThreadPoolCount,
    aHttpServerSecurity,aAdditionalURL,aQueueName);
  if aRestAccessRights<>nil then
    DBServerAccessRight[0] := aRestAccessRights;
end;

destructor TSQLHttpServer.Destroy;
var log: ISynLog;
begin
  log := fLog.Enter(self,'Destroy');
  if log<>nil then
    log.Log(sllHttp,'% finalized for %',[fHttpServer,Plural('server',length(fDBServers))],self);
  Shutdown(true); // but don't call fDBServers[i].Server.Shutdown
  FreeAndNil(fHttpServer);
  inherited Destroy;
  fAccessControlAllowOriginsMatch.Free;
end;

procedure TSQLHttpServer.Shutdown(noRestServerShutdown: boolean);
var i: integer;
    log: ISynLog;
begin
  if (self<>nil) and not fShutdownInProgress then begin
    log := fLog.Enter('Shutdown(%)',[BOOL_STR[noRestServerShutdown]],self);
    fShutdownInProgress := true;
    fHttpServer.Shutdown;
    fSafe.Lock; // protect fDBServers[]
    try
      for i := 0 to high(fDBServers) do begin
        if not noRestServerShutdown then
          fDBServers[i].Server.Shutdown;
        if TMethod(fDBServers[i].Server.OnNotifyCallback).Data=self then
          fDBServers[i].Server.OnNotifyCallback := nil; // avoid unexpected GPF
      end;
    finally
      fSafe.UnLock;
    end;
  end;
end;

function TSQLHttpServer.GetDBServer(Index: Integer): TSQLRestServer;
begin
  result := nil;
  if self=nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    if cardinal(Index)<cardinal(length(fDBServers)) then
      result := fDBServers[Index].Server;
  finally
    fSafe.UnLock;
  end;
end;

function TSQLHttpServer.GetDBServerCount: integer;
begin
  result := length(fDBServers);
end;

function TSQLHttpServer.GetDBServerNames: RawUTF8;
var i: integer;
begin
  result := '';
  if self=nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      result := result+fDBServers[i].Server.Model.Root+' ';
  finally
    fSafe.UnLock;
  end;
end;

procedure TSQLHttpServer.SetDBServerAccessRight(Index: integer;
  Value: PSQLAccessRights);
begin
  if self=nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    if Value=nil then
      Value := HTTP_DEFAULT_ACCESS_RIGHTS;
    if cardinal(Index)<cardinal(length(fDBServers)) then
      fDBServers[Index].RestAccessRights := Value;
  finally
    fSafe.UnLock;
  end;
end;

procedure TSQLHttpServer.SetDBServer(aIndex: integer; aServer: TSQLRestServer;
  aSecurity: TSQLHttpServerSecurity; aRestAccessRights: PSQLAccessRights);
begin // caller should have made fSafe.Lock
  if (self<>nil) and (cardinal(aIndex)<cardinal(length(fDBServers))) then
    with fDBServers[aIndex] do begin
      Server := aServer;
      if (fHttpServer<>nil) and fHttpServer.CanNotifyCallback then
        Server.OnNotifyCallback := NotifyCallback;
      Server.SetPublicURI(fPublicAddress,fPublicPort);
      Security := aSecurity;
      if aRestAccessRights=nil then
        RestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS else
        RestAccessRights := aRestAccessRights;
    end;
end;

const
  HTTPS_TEXT: array[boolean] of string[1] = ('','s');
  HTTPS_SECURITY: array[boolean] of TSQLHttpServerSecurity = (secNone, secSSL);

procedure TSQLHttpServer.RootRedirectToURI(const aRedirectedURI: RawUTF8;
  aRegisterURI: boolean; aHttps: boolean);
begin
  if fRootRedirectToURI[aHttps]=aRedirectedURI then
    exit;
  fLog.Add.Log(sllHttp,'Redirect http%://localhost:% to http%://localhost:%/%',
    [HTTPS_TEXT[aHttps],fPublicPort,HTTPS_TEXT[aHttps],fPublicPort,aRedirectedURI],self);
  fRootRedirectToURI[aHttps] := aRedirectedURI;
  if aRedirectedURI<>'' then
    HttpApiAddUri('/','+',HTTPS_SECURITY[aHttps],aRegisterURI,true);
end;

function TSQLHttpServer.HttpApiAddUri(const aRoot,aDomainName: RawByteString;
  aSecurity: TSQLHttpServerSecurity; aRegisterURI,aRaiseExceptionOnError: boolean): RawUTF8;
{$ifndef ONLYUSEHTTPSOCKET}
var err: integer;
    https: boolean;
{$endif}
begin
  result := ''; // no error
  {$ifndef ONLYUSEHTTPSOCKET}
  if not fHttpServer.InheritsFrom(THttpApiServer) then
    exit;
  https := aSecurity=secSSL;
  fLog.Add.Log(sllHttp,'http.sys registration of http%://%:%/%',
    [HTTPS_TEXT[https],aDomainName,fPublicPort,aRoot],self);
  // try to register the URL to http.sys
  err := THttpApiServer(fHttpServer).AddUrl(aRoot,fPublicPort,https,aDomainName,aRegisterURI);
  if err=NO_ERROR then
    exit;
  FormatUTF8('http.sys URI registration error #% for http%://%:%/%',
    [err,HTTPS_TEXT[https],aDomainName,fPublicPort,aRoot],result);
  if err=ERROR_ACCESS_DENIED then
    if aRegisterURI then
      result := result+' (administrator rights needed, at least once to register the URI)' else
      result := result+' (you need to register the URI - try to use useHttpApiRegisteringURI)';
  fLog.Add.Log(sllLastError,result,self);
  if aRaiseExceptionOnError then
    raise EHttpServerException.CreateUTF8('%: %',[self,result]);
  {$endif}
end;

function TSQLHttpServer.Request(Ctxt: THttpServerRequest): cardinal;
var call: TSQLRestURIParams;
    i,hostlen: integer;
    P: PUTF8Char;
    headers,hostroot,redirect: RawUTF8;
    match: TSQLRestModelMatch;
    serv: TSQLRestServer;
begin
  if (self=nil) or fShutdownInProgress then
    result := HTTP_NOTFOUND else
  if ((Ctxt.URL='') or (Ctxt.URL='/')) and (Ctxt.Method='GET') then
    if fRootRedirectToURI[Ctxt.UseSSL]<>'' then begin
      Ctxt.OutCustomHeaders := 'Location: '+fRootRedirectToURI[Ctxt.UseSSL];
      result := HTTP_TEMPORARYREDIRECT;
    end else
      result := HTTP_BADREQUEST else
  if (Ctxt.Method='') or (OnlyJSONRequests and
     not IdemPChar(pointer(Ctxt.InContentType),JSON_CONTENT_TYPE_UPPER)) then
    // wrong Input parameters or not JSON request: 400 BAD REQUEST
    result := HTTP_BADREQUEST else
  if Ctxt.Method='OPTIONS' then begin // handle CORS
    if fAccessControlAllowOrigin='' then
      Ctxt.OutCustomHeaders := 'Access-Control-Allow-Origin:' else begin
      FindNameValue(Ctxt.InHeaders,'ACCESS-CONTROL-REQUEST-HEADERS:',headers);
      Ctxt.OutCustomHeaders := 'Access-Control-Allow-Headers: '+headers;
      ComputeAccessControlHeader(Ctxt);
    end;
    result := HTTP_NOCONTENT;
  end else begin
    // compute URI, handling any virtual host domain
    call.Init;
    call.LowLevelConnectionID := Ctxt.ConnectionID;
    if Ctxt.UseSSL then
      call.LowLevelFlags := call.LowLevelFlags+[llfHttps,llfSecured];
    if Ctxt.ConnectionThread<>nil then
      if PClass(Ctxt.ConnectionThread)^=TWebSocketServerResp then begin
        include(call.LowLevelFlags,llfWebsockets);
        if TWebSocketServerResp(Ctxt.ConnectionThread).WebSocketProtocol.Encrypted then
          include(call.LowLevelFlags,llfSecured);
    end;
    if fHosts.Count>0 then begin
      FindNameValue(Ctxt.InHeaders,'HOST: ',hostroot);
      i := PosExChar(':',hostroot);
      if i>0 then
        SetLength(hostroot,i-1); // trim any port
      if hostroot<>'' then // e.g. 'Host: project1.com' -> 'root1'
        hostroot := fHosts.Value(hostroot);
    end;
    if hostroot<>'' then
      if (Ctxt.URL='') or (Ctxt.URL='/') then
        call.Url := hostroot else
        if Ctxt.URL[1]='/' then
          call.Url := hostroot+Ctxt.URL else
          call.Url := hostroot+'/'+Ctxt.URL else
      if (Ctxt.URL<>'') and (Ctxt.URL[1]='/') then
          call.Url := copy(Ctxt.URL,2,maxInt) else
          call.Url := Ctxt.URL;
    // search and call any matching TSQLRestServer instance
    result := HTTP_NOTFOUND; // page not found by default (in case of wrong URL)
    serv := nil;
    match := rmNoMatch;
    fSafe.Lock; // protect fDBServers[]
    try
      for i := 0 to length(fDBServers)-1 do
        with fDBServers[i] do
        if Ctxt.UseSSL=(Security=secSSL) then begin // registered for http or https
          match := Server.Model.URIMatch(call.Url);
          if match=rmNoMatch then
            continue;
          call.RestAccessRights := RestAccessRights;
          serv := Server;
          break;
        end;
    finally
      fSafe.UnLock;
    end;
    if (match=rmNoMatch) or (serv=nil) then
      exit;
    if fRedirectServerRootUriForExactCase and (match=rmMatchWithCaseChange) then begin
      // force redirection to exact Server.Model.Root case sensitivity
      call.OutStatus := HTTP_TEMPORARYREDIRECT;
      call.OutHead := 'Location: '+serv.Model.Root+
        copy(call.Url,length(serv.Model.Root)+1,maxInt);
    end else begin
      // call matching TSQLRestServer.URI()
      call.Method := Ctxt.Method;
      call.InHead := Ctxt.InHeaders;
      call.InBody := Ctxt.InContent;
      serv.URI(call);
    end;
    // set output content
    result := call.OutStatus;
    Ctxt.OutContent := call.OutBody;
    Ctxt.OutContentType := call.OutBodyType;
    P := pointer(call.OutHead);
    if IdemPChar(P,'CONTENT-TYPE: ') then begin
      // change mime type if modified in HTTP header (e.g. GET blob fields)
      Ctxt.OutContentType := GetNextLine(P+14,P);
      call.OutHead := P;
    end else
      // default content type is JSON
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    // handle HTTP redirection and cookies over virtual hosts
    if hostroot<>'' then begin
      if ((result=HTTP_MOVEDPERMANENTLY) or (result=HTTP_TEMPORARYREDIRECT)) then begin
        redirect := FindIniNameValue(P,'LOCATION: ');
        if (redirect<>'') and (redirect[1]='/') then
          delete(redirect,1,1); // what is needed for real URI doesn't help here
        hostlen := length(hostroot);
        if (length(redirect)>hostlen) and (redirect[hostlen+1]='/') and
           IdemPropNameU(hostroot,pointer(redirect),hostlen) then
          // hostroot/method -> method on same domain
          call.OutHead := 'Location: '+copy(redirect,hostlen+1,maxInt);
      end else
      if ExistsIniName(P,'SET-COOKIE:') then
        Call.OutHead := StringReplaceAll(
          Call.OutHead,'; Path=/'+serv.Model.Root,'; Path=/')
    end;
    Ctxt.OutCustomHeaders := Trim(call.OutHead);
    if call.OutInternalState<>0 then
      Ctxt.OutCustomHeaders := FormatUTF8('%'#13#10'Server-InternalState: %',
        [Ctxt.OutCustomHeaders,call.OutInternalState]);
    // handle optional CORS origin
    if fAccessControlAllowOrigin<>'' then
      ComputeAccessControlHeader(Ctxt);
    Ctxt.OutCustomHeaders := trim(Ctxt.OutCustomHeaders);
  end;
end;

procedure TSQLHttpServer.HttpThreadTerminate(Sender: TThread);
var i: integer;
begin
  if self=nil then
    exit;
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.EndCurrentThread(Sender);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSQLHttpServer.HttpThreadStart(Sender: TThread);
var i: integer;
begin
  if self=nil then
    exit;
  SetCurrentThreadName('% %/%%',[self,fPort,GetDBServerNames,Sender]);
  fSafe.Lock; // protect fDBServers[]
  try
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.BeginCurrentThread(Sender);
  finally
    fSafe.UnLock;
  end;
end;

procedure TSQLHttpServer.SetAccessControlAllowOrigin(const Value: RawUTF8);
var patterns: TRawUTF8DynArray;
begin
  fAccessControlAllowOrigin := Value;
  FreeAndNil(fAccessControlAllowOriginsMatch);
  if (Value='') or (Value='*') then
    exit;
  CSVToRawUTF8DynArray(pointer(Value),patterns);
  if patterns=nil then
    exit;
  fAccessControlAllowOriginsMatch := TMatchs.Create(patterns,{caseinsensitive=}true);
end;

procedure TSQLHttpServer.ComputeAccessControlHeader(Ctxt: THttpServerRequest);
var origin: RawUTF8;
begin // caller did ensure that fAccessControlAllowOrigin<>''
  FindNameValue(Ctxt.InHeaders,'ORIGIN: ',origin);
  if origin='' then
    exit;
  if fAccessControlAllowOrigin='*' then
    origin := fAccessControlAllowOrigin else
    if fAccessControlAllowOriginsMatch.Match(origin)<0 then
      exit;
  Ctxt.OutCustomHeaders := Ctxt.OutCustomHeaders+
    #13#10'Access-Control-Allow-Methods: POST, PUT, GET, DELETE, LOCK, OPTIONS'+
    #13#10'Access-Control-Max-Age: 1728000'+
    // see http://blog.import.io/tech-blog/exposing-headers-over-cors-with-access-control-expose-headers
    #13#10'Access-Control-Expose-Headers: content-length,location,server-internalstate'+
    #13#10'Access-Control-Allow-Origin: '+origin;
  if fAccessControlAllowCredential then
    Ctxt.OutCustomHeaders := Ctxt.OutCustomHeaders+
      #13#10'Access-Control-Allow-Credentials: true';
end;

function TSQLHttpServer.WebSocketsEnable(
  const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
  aWebSocketsAJAX,aWebSocketsCompressed: boolean): TWebSocketServerRest;
begin
  if fHttpServer.InheritsFrom(TWebSocketServerRest) then begin
    result := TWebSocketServerRest(fHttpServer);
    result.WebSocketsEnable(aWebSocketsURI,aWebSocketsEncryptionKey,
      aWebSocketsAJAX,aWebSocketsCompressed);
  end else
    raise EWebSockets.CreateUTF8(
      '%.WebSocketEnable(%): expected useBidirSocket',
      [self,GetEnumName(TypeInfo(TSQLHttpServerOptions),ord(fHttpServerKind))^]);
end;

function TSQLHttpServer.WebSocketsEnable(aServer: TSQLRestServer;
  const aWebSocketsEncryptionKey: RawUTF8;
  aWebSocketsAJAX,aWebSocketsCompressed: boolean): TWebSocketServerRest;
begin
  if (aServer=nil) or (DBServerFind(aServer)<0) then
    raise EWebSockets.CreateUTF8('%.WebSocketEnable(aServer=%?)',[self,aServer]);
  result := WebSocketsEnable(aServer.Model.Root,
    aWebSocketsEncryptionKey,aWebSocketsAJAX,aWebSocketsCompressed);
end;

function TSQLHttpServer.NotifyCallback(aSender: TSQLRestServer;
  const aInterfaceDotMethodName, aParams: RawUTF8; aConnectionID: THttpServerConnectionID;
  aFakeCallID: integer; aResult, aErrorMsg: PRawUTF8): boolean;
var ctxt: THttpServerRequest;
    status: cardinal;
begin
  result := false;
  if (self<>nil) and not fShutdownInProgress then
  try
    if fHttpServer<>nil then begin
      // aConnection.InheritsFrom(TSynThread) may raise an exception
      // -> checked in WebSocketsCallback/IsActiveWebSocket
      ctxt := THttpServerRequest.Create(nil,aConnectionID,nil);
      try
        ctxt.Prepare(FormatUTF8('%/%/%',[aSender.Model.Root,aInterfaceDotMethodName,
          aFakeCallID]),'POST','','['+aParams+']','','',{ssl=}false);
        status := fHttpServer.Callback(ctxt,aResult=nil);
        if status=HTTP_SUCCESS then begin
          if aResult<>nil then
            if IdemPChar(pointer(ctxt.OutContent),'{"RESULT":') then
              aResult^ := copy(ctxt.OutContent,11,maxInt) else
              aResult^ := ctxt.OutContent;
          result := true;
        end else
          if aErrorMsg<>nil then
            FormatUTF8('%.Callback(%) received status=% from %',
              [fHttpServer,aConnectionID,status,ctxt.URL],aErrorMsg^);
      finally
        ctxt.Free;
      end;
    end else
      if aErrorMsg<>nil then
        FormatUTF8('%.NotifyCallback with fHttpServer=nil',[self],aErrorMsg^);
  except
    on E: Exception do
      if aErrorMsg<>nil then
        aErrorMsg^ := ObjectToJSONDebug(E);
  end;
end;

constructor TSQLHttpServer.Create(aServer: TSQLRestServer;
  aDefinition: TSQLHttpServerDefinition; aForcedKind: TSQLHttpServerOptions;
  aWebSocketsLoopDelay: integer);
const AUTH: array[TSQLHttpServerRestAuthentication] of TSQLRestServerAuthenticationClass = (
  // adDefault, adHttpBasic, adWeak, adSSPI
  TSQLRestServerAuthenticationDefault, TSQLRestServerAuthenticationHttpBasic,
  TSQLRestServerAuthenticationNone,TSQLRestServerAuthenticationSSPI{may be nil});
var a: TSQLHttpServerRestAuthentication;
    thrdCnt: integer;
    websock: TWebSocketServerRest;
begin
  if aDefinition=nil then
    raise EHttpServerException.CreateUTF8('%.Create(aDefinition=nil)',[self]);
  if aDefinition.WebSocketPassword<>'' then
    aForcedKind := useBidirSocket;
  if aDefinition.ThreadCount=0 then
    thrdCnt := 32 else
    thrdCnt := aDefinition.ThreadCount;
  Create(aDefinition.BindPort,aServer,'+',aForcedKind,nil,thrdCnt,
    HTTPS_SECURITY[aDefinition.Https],'',aDefinition.HttpSysQueueName);
  if aDefinition.EnableCORS<>'' then begin
    AccessControlAllowOrigin := aDefinition.EnableCORS;
    AccessControlAllowCredential := true;
  end;
  if fHttpServer<>nil then
    fHttpServer.RemoteIPHeader := aDefinition.RemoteIPHeader;
  a := aDefinition.Authentication;
  if aServer.HandleAuthentication then
    if AUTH[a]=nil then
      fLog.Add.Log(sllWarning,'Ignored',TypeInfo(TSQLHttpServerRestAuthentication),a,self) else begin
      aServer.AuthenticationUnregisterAll;
      aServer.AuthenticationRegister(AUTH[a]);
    end;
  if aDefinition.WebSocketPassword<>'' then begin
    websock := WebSocketsEnable(aServer,aDefinition.PasswordPlain);
    if HttpServerFullWebSocketsLog then
      websock.Settings.SetFullLog;
    websock.Settings^.LoopDelay := aWebSocketsLoopDelay;
  end;
end;


{ TSQLHTTPRemoteLogServer }

constructor TSQLHTTPRemoteLogServer.Create(const aRoot: RawUTF8;
  aPort: integer; const aEvent: TRemoteLogReceivedOne);
var aModel: TSQLModel;
begin
  aModel := TSQLModel.Create([],aRoot);
  fServer := TSQLRestServerFullMemory.Create(aModel);
  aModel.Owner := fServer;
  fServer.ServiceMethodRegisterPublishedMethods('',self);
  fServer.AcquireExecutionMode[execSOAByMethod] := amLocked; // protect aEvent
  inherited Create(AnsiString(UInt32ToUtf8(aPort)),fServer,'+',HTTP_DEFAULT_MODE,nil,1);
  fEvent := aEvent;
  SetAccessControlAllowOrigin('*'); // e.g. when called from AJAX/SMS
end;

destructor TSQLHTTPRemoteLogServer.Destroy;
begin
  try
    inherited Destroy;
  finally
    fServer.Free;
  end;
end;

procedure TSQLHTTPRemoteLogServer.RemoteLog(Ctxt: TSQLRestServerURIContext);
begin
  if Assigned(fEvent) and (Ctxt.Method=mPUT) then begin
    fEvent(Ctxt.Call^.InBody);
    Ctxt.Success;
  end;
end;

procedure StatusCodeToErrorMsgInternal(Code: integer; var result: RawUTF8);
begin
  result := SynCrtSock.StatusCodeToReason(Code); // faster and more complete
end;

initialization
  StatusCodeToErrorMessage := StatusCodeToErrorMsgInternal; // as in mORMotHttpClient
end.
