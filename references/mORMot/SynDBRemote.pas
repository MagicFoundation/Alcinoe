/// remote access to any RDBMS via HTTP using our SynDB architecture
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBRemote;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
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

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  Types,
  SynKylix,
  {$endif}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  SynCommons,
  SynCrtSock,
  SynTable,
  SynDB;

{ -------------- HTTP Server classes for SynDB remote access }

const
  /// default HTTP port to be used for SynDB remote access if none is specified
  SYNDB_DEFAULT_HTTP_PORT = '8092';

type
  /// used to define the HTTP server class for publishing a SynDB connection
  TSQLDBServerClass = class of TSQLDBServerAbstract;

  /// implements a generic HTTP server, able to publish any SynDB connection
  // - do not instantiate this class, but rather use TSQLDBServerHttpApi or
  // TSQLDBServerSockets - this abstract class won't set any HTTP server
  TSQLDBServerAbstract = class
  protected
    fServer: THttpServerGeneric;
    fThreadPoolCount: integer;
    fPort, fDatabaseName: RawUTF8;
    fHttps: boolean;
    fProperties: TSQLDBConnectionProperties;
    fProtocol: TSQLDBProxyConnectionProtocol;
    fSafe: TSynLocker;
    fProcessLocked: boolean;
    // this is where the process would take place
    function Process(Ctxt: THttpServerRequest): cardinal;
  public
    /// publish the SynDB connection on a given HTTP port and URI
    // - this generic constructor won't initialize the HTTP server itself:
    // use overriden constructors instead
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential, or change the
    // transmission Protocol which is TSQLDBRemoteConnectionProtocol by default
    // - aProperties.ThreadingMode will be set to the optional aThreadMode
    // parameter tmMainConnection by default, which would also set ProcessLocked
    // to TRUE - in fact, you should better use a single thread for the process,
    // but you may define a small thread pool for the process IF the provider
    // supports it
    constructor Create(aProperties: TSQLDBConnectionProperties;
      const aDatabaseName: RawUTF8; const aPort: RawUTF8=SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8=''; const aPassword: RawUTF8='';
      aHttps: boolean=false; aThreadPoolCount: integer=1;
      aProtocol: TSQLDBProxyConnectionProtocolClass=nil;
      aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode=tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract=nil); virtual;
    /// released used memory
    destructor Destroy; override;
    /// the associated database connection properties
    property Properties: TSQLDBConnectionProperties read fProperties write fProperties;
    /// the associated port number
    property Port: RawUTF8 read fPort;
    /// the associated database name
    property DatabaseName: RawUTF8 read fDatabaseName;
    /// the associated communication protocol
    // - to manage user authentication, use AuthenticateUser/DisauthenticateUser
    // methods of Protocol.Authenticate
    property Protocol: TSQLDBProxyConnectionProtocol read fProtocol write fProtocol;
    /// if the internal Process() method would be protected by a critical section
    // - set to TRUE if constructor's aThreadMode is left to its default
    // tmMainConnection value
    property ProcessLocked: boolean read fProcessLocked write fProcessLocked;
  end;

  /// implements a SynDB HTTP server via the user-land Sockets API
  TSQLDBServerSockets = class(TSQLDBServerAbstract)
  protected
  public
    /// publish the SynDB connection on a given HTTP port and URI using sockets
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential
    // - parameter aHttps is ignored by this class
    constructor Create(aProperties: TSQLDBConnectionProperties;
      const aDatabaseName: RawUTF8; const aPort: RawUTF8=SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8=''; const aPassword: RawUTF8='';
      aHttps: boolean=false; aThreadPoolCount: integer=1;
      aProtocol: TSQLDBProxyConnectionProtocolClass=nil;
      aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode=tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract=nil); override;
  end;

  {$ifdef ONLYUSEHTTPSOCKET}

  TSQLDBServerRemote = TSQLDBServerSockets;

  {$else}

  /// implements a SynDB HTTP server using fast http.sys kernel-mode server
  // - under Windows, this class is faster and more stable than TSQLDBServerSockets
  TSQLDBServerHttpApi = class(TSQLDBServerAbstract)
  protected
  public
    /// publish the SynDB connection on a given HTTP port and URI using http.sys
    // - URI would follow the supplied aDatabaseName parameter on the given port
    // e.g. http://serverip:8092/remotedb for
    // ! Create(aProps,'remotedb');
    // - you can optionally register one user credential
    constructor Create(aProperties: TSQLDBConnectionProperties;
      const aDatabaseName: RawUTF8; const aPort: RawUTF8=SYNDB_DEFAULT_HTTP_PORT;
      const aUserName: RawUTF8=''; const aPassword: RawUTF8='';
      aHttps: boolean=false; aThreadPoolCount: integer=1;
      aProtocol: TSQLDBProxyConnectionProtocolClass=nil;
      aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode=tmMainConnection;
      aAuthenticate: TSynAuthenticationAbstract=nil); override;
  end;

  /// the default SynDB HTTP server class on each platform
  TSQLDBServerRemote = TSQLDBServerHttpApi;

  {$endif ONLYUSEHTTPSOCKET}


{ -------------- HTTP Client classes for SynDB remote access }

type
  /// implements a generic HTTP client, able to access remotely any SynDB
  // - do not instantiate this class, but rather use TSQLDBSocketConnectionProperties
  //  TSQLDBWinHTTPConnectionProperties TSQLDBWinINetConnectionProperties
  TSQLDBHTTPConnectionPropertiesAbstract = class(TSQLDBRemoteConnectionPropertiesAbstract)
  protected
    fKeepAliveMS: cardinal;
    fURI: TURI;
    function GetServer: RawByteString; {$ifdef HASINLINE}inline;{$endif}
    function GetPort: RawByteString;   {$ifdef HASINLINE}inline;{$endif}
    /// you could inherit from it and set your custom fProtocol instance
    procedure SetInternalProperties; override;
    procedure SetServerName(const aServerName: RawUTF8);
    // this overriden method will just call InternalRequest
    procedure ProcessMessage(const Input: RawByteString; out Output: RawByteString); override;
    /// to be overriden to process low-level HTTP/1.1 request
    function InternalRequest(var Data,DataType: RawByteString): integer; virtual; abstract;
  published
    /// the associated server IP address or name
    property Server: RawByteString read GetServer;
    /// the associated port number
    property Port: RawByteString read GetPort;
    /// time (in milliseconds) to keep the connection alive with the server
    // - default is 60000, i.e. one minute
    property KeepAliveMS: cardinal read fKeepAliveMS write fKeepAliveMS;
  end;

  /// implements a HTTP client via sockets, able to access remotely any SynDB
  TSQLDBSocketConnectionProperties = class(TSQLDBHTTPConnectionPropertiesAbstract)
  protected
    fSocket: THttpClientSocket;
    function InternalRequest(var Data,DataType: RawByteString): integer; override;
  public
    /// initialize the properties for remote access via HTTP using sockets
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSQLDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the Socket implementation instance
    property Socket: THttpClientSocket read fSocket;
  end;


  /// implements an abstract HTTP client via THttpRequest abstract class,
  // able to access remotely any SynDB
  // - never instantiate this class, but rather TSQLDBWinHTTPConnectionProperties
  // or TSQLDBWinINetConnectionProperties
  TSQLDBHttpRequestConnectionProperties = class(TSQLDBHTTPConnectionPropertiesAbstract)
  protected
    fClient: THttpRequest;
    function InternalRequest(var Data,DataType: RawByteString): integer; override;
  public
    /// released used memory
    destructor Destroy; override;
    /// low-level direct access to the WinHTTP implementation instance
    property Client: THttpRequest read fClient;
  end;

  {$ifdef USELIBCURL}

  /// implements a HTTP client via the libcurl API, able to access remotely any SynDB
  TSQLDBCurlConnectionProperties = class(TSQLDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using libcurl
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSQLDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
  end;

  {$endif USELIBCURL}

  {$ifdef USEWININET}

  /// implements a HTTP client via WinHTTP API, able to access remotely any SynDB
  TSQLDBWinHTTPConnectionProperties = class(TSQLDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinHTTP
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSQLDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
  end;

  /// implements a HTTP client via WinINet API, able to access remotely any SynDB
  TSQLDBWinINetConnectionProperties = class(TSQLDBHttpRequestConnectionProperties)
  public
    /// initialize the properties for remote access via HTTP using WinINet
    // - aServerName should be the HTTP server address as 'server:port'
    // - aDatabaseName would be used to compute the URI as in TSQLDBServerAbstract
    // - the user/password credential should match server-side authentication
    constructor Create(const aServerName,aDatabaseName, aUserID,aPassWord: RawUTF8); override;
  end;

  {$endif USEWININET}


implementation

{ TSQLDBServerAbstract }

constructor TSQLDBServerAbstract.Create(aProperties: TSQLDBConnectionProperties;
  const aDatabaseName, aPort, aUserName,aPassword: RawUTF8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSQLDBProxyConnectionProtocolClass;
  aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
begin
  fProperties := aProperties;
  if fProperties.InheritsFrom(TSQLDBConnectionPropertiesThreadSafe) then begin
    TSQLDBConnectionPropertiesThreadSafe(fProperties).ThreadingMode := aThreadMode;
    if aThreadMode=tmMainConnection then
      fProcessLocked := true;
  end;
  fDatabaseName := aDatabaseName;
  fSafe.Init;
  fPort := aPort;
  fHttps := aHttps;
  fThreadPoolCount := aThreadPoolCount;
  if aProtocol=nil then
    aProtocol := TSQLDBRemoteConnectionProtocol;
  if aAuthenticate=nil then
    aAuthenticate := TSynAuthentication.Create(aUserName,aPassword);
  fProtocol := aProtocol.Create(aAuthenticate);
end;

destructor TSQLDBServerAbstract.Destroy;
begin
  inherited;
  fServer.Free;
  fProtocol.Free;
  fSafe.Done;
end;

function TSQLDBServerAbstract.Process(Ctxt: THttpServerRequest): cardinal;
var o: RawByteString;
begin
  if (Ctxt.Method<>'POST') or (Ctxt.InContent='') or
     not IdemPropNameU(trim(Ctxt.InContentType),BINARY_CONTENT_TYPE) then begin
    result := STATUS_NOTFOUND;
    exit;
  end;
  try
    if fProcessLocked then
      fSafe.Lock;
    fProperties.ThreadSafeConnection.RemoteProcessMessage(Ctxt.InContent,o,fProtocol);
  finally
    if fProcessLocked then
      fSafe.UnLock;
  end;
  Ctxt.OutContent := o;
  Ctxt.OutContentType := BINARY_CONTENT_TYPE;
  result := STATUS_SUCCESS;
end;


{ TSQLDBHTTPConnectionPropertiesAbstract }

function TSQLDBHTTPConnectionPropertiesAbstract.GetServer: RawByteString;
begin
  result := fURI.Server;
end;

function TSQLDBHTTPConnectionPropertiesAbstract.GetPort: RawByteString;
begin
  result := fURI.Port;
end;

procedure TSQLDBHTTPConnectionPropertiesAbstract.SetServerName(
  const aServerName: RawUTF8);
begin
  fKeepAliveMS := 60000;
  if not fURI.From(aServerName) then
    raise ESQLDBRemote.CreateUTF8(
      '%.Create: expect a valid URI in aServerName="%"',[self,aServerName]);
  if fURI.Port='' then
    fURI.Port:= SYNDB_DEFAULT_HTTP_PORT;
end;

procedure TSQLDBHTTPConnectionPropertiesAbstract.ProcessMessage(
  const Input: RawByteString; out Output: RawByteString);
var Content, ContentType: RawByteString;
    status: integer;
begin
  Content := Input;
  ContentType := BINARY_CONTENT_TYPE;
  status := InternalRequest(Content,ContentType);
  if status<>STATUS_SUCCESS then
    raise ESQLDBRemote.CreateUTF8(
      '%.ProcessMessage: Error % from %',[self,status,fURI.URI]);
  if ContentType<>BINARY_CONTENT_TYPE then
    raise ESQLDBRemote.CreateUTF8(
      '%.ProcessMessage: Invalid content type [%] from %',[self,ContentType,fURI.URI]);
  Output := Content;
end;

procedure TSQLDBHTTPConnectionPropertiesAbstract.SetInternalProperties;
begin
  if fProtocol=nil then
    fProtocol := TSQLDBRemoteConnectionProtocol.Create(
      TSynAuthentication.Create(UserID,PassWord));
  inherited;
end;


{ TSQLDBSocketConnectionProperties }

constructor TSQLDBSocketConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fSocket := THttpClientSocket.Open(Server,Port);
  inherited;
end;

destructor TSQLDBSocketConnectionProperties.Destroy;
begin
  try
    inherited;
  finally
    fSocket.Free;
  end;
end;

function TSQLDBSocketConnectionProperties.InternalRequest(
  var Data,DataType: RawByteString): integer;
begin
  result := fSocket.Request(fDatabaseName,'POST',fKeepAliveMS,'',Data,DataType,false);
  Data := fSocket.Content;
  DataType := fSocket.ContentType;
end;


{ TSQLDBHttpRequestConnectionProperties }

destructor TSQLDBHttpRequestConnectionProperties.Destroy;
begin
  try
    inherited;
  finally
    fClient.Free;
  end;
end;

function TSQLDBHttpRequestConnectionProperties.InternalRequest(
  var Data,DataType: RawByteString): integer;
var inData,inDataType,head: RawByteString;
begin
  inData := Data;
  inDataType := DataType;
  result := fClient.Request(fDatabaseName,'POST',fKeepAliveMS,'',inData,inDataType,
    SockString(head),SockString(Data));
  FindNameValue(head,HEADER_CONTENT_TYPE_UPPER,RawUTF8(DataType));
end;


{$ifdef USEWININET}

{ TSQLDBWinHTTPConnectionProperties }

constructor TSQLDBWinHTTPConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TWinHTTP.Create(Server,Port,fURI.Https);
  inherited;
end;

{ TSQLDBWinINetConnectionProperties }

constructor TSQLDBWinINetConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TWinINet.Create(Server,Port,fURI.Https);
  inherited;
end;

{$endif USEWININET}

{$ifdef USELIBCURL}

{ TSQLDBCurlConnectionProperties }

constructor TSQLDBCurlConnectionProperties.Create(const aServerName,
  aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  SetServerName(aServerName);
  fClient := TCurlHTTP.Create(Server,Port,fURI.Https);
  inherited;
end;

{$endif USELIBCURL}


{$ifndef ONLYUSEHTTPSOCKET}

{ TSQLDBServerHttpApi }

constructor TSQLDBServerHttpApi.Create(aProperties: TSQLDBConnectionProperties;
  const aDatabaseName, aPort, aUserName,aPassword: RawUTF8; aHttps: boolean;
  aThreadPoolCount: integer; aProtocol: TSQLDBProxyConnectionProtocolClass;
  aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var status: integer;
begin
  inherited;
  fServer := THttpApiServer.Create(false,'');
  status := THttpApiServer(fServer).AddUrl(fDatabaseName,fPort,fHttps,'+',true);
  if status<>NO_ERROR then
    if status=ERROR_ACCESS_DENIED then
      raise ESQLDBRemote.CreateUTF8(
        '%.Create: administrator rights needed to register URI % on port %',
        [self,fDatabaseName,fPort]) else
      raise ESQLDBRemote.CreateUTF8(
        '%.Create: error registering URI % on port %: is not another server '+
        'instance running on this port?',[self,fDatabaseName,fPort]);
  fServer.OnRequest := Process;
  if fThreadPoolCount>1 then
    THttpApiServer(fServer).Clone(fThreadPoolCount-1);
end;

{$endif ONLYUSEHTTPSOCKET}


{ TSQLDBServerSockets }

constructor TSQLDBServerSockets.Create(aProperties: TSQLDBConnectionProperties;
  const aDatabaseName, aPort, aUserName, aPassword: RawUTF8;
  aHttps: boolean; aThreadPoolCount: integer; aProtocol: TSQLDBProxyConnectionProtocolClass;
  aThreadMode: TSQLDBConnectionPropertiesThreadSafeThreadingMode;
  aAuthenticate: TSynAuthenticationAbstract);
var
  ident: RawUTF8;
begin
  inherited;
  FormatUTF8('DBRemote %',[aDatabaseName],ident);
  fServer := THttpServer.Create(aPort,nil,nil,ident,fThreadPoolCount);
  THttpServer(fServer).WaitStarted;
  fServer.OnRequest := Process;
end;


initialization
  TSQLDBSocketConnectionProperties.RegisterClassNameForDefinition;
  {$ifdef USEWININET}
  TSQLDBWinHTTPConnectionProperties.RegisterClassNameForDefinition;
  TSQLDBWinINetConnectionProperties.RegisterClassNameForDefinition;
  {$endif}
  {$ifdef USELIBCURL}
  TSQLDBCurlConnectionProperties.RegisterClassNameForDefinition;
  {$endif}
end.
