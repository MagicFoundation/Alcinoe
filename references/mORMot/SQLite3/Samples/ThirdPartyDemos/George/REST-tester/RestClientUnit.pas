unit RestClientUnit;

interface

uses
  // RTL
  SysUtils,
  Classes,
  StrUtils,
  Dialogs,
  // mORMot
  mORMot,
  mORMotHttpClient,
  SynCommons,
  // Custom
  RestMethodsInterfaceUnit;

type
  {$ifndef MSWINDOWS}
  TSQLHttpClientWinHTTP=TSQLHttpClientWinSock;
  {$endif}

  lProtocol = (HTTP_Socket, HTTP_HTTPsys{$ifdef MSWINDOWS}, HTTPsys_SSL{$endif}, HTTPsys_AES, HTTP_WebSocket, WebSocketBidir_JSON, WebSocketBidir_Binary, WebSocketBidir_BinaryAES{$ifdef MSWINDOWS}, NamedPipe{$endif});
  lAuthenticationMode = (NoAuthentication, Default, None, HttpBasic{$ifdef MSWINDOWS}, SSPI{$endif});

  rClientSettings = record
    Protocol: lProtocol;
    AuthMode: lAuthenticationMode;
    HostOrIP: string;
    Port: string;
    UserLogin: RawUTF8;
    UserPassword: RawUTF8;
  end;

  rConnectionSettings = record
    SendTimeout: Cardinal;
    ReceiveTimeout: Cardinal;
    ConnectTimeout: Cardinal;
    procedure LanNetworkPreset();
  end;

  tRestClient = class
  private
    fModel: TSQLModel;
    fClientSettings: rClientSettings;
    fConnectionSettings: rConnectionSettings;
    fInitialized: boolean;
  public
    fClient: TSQLRestClientURI;
    RestMethods: IRestMethods;
    property Initialized: boolean read fInitialized;
    constructor Create();
    destructor Destroy(); override;
    function Initialize(ClSettings: rClientSettings; ConSettings: rConnectionSettings): boolean; overload;
    function Initialize(ClSettings: rClientSettings): boolean; overload;
    procedure DeInitialize();
  end;

var
  RestClient: tRestClient;

implementation

{ rConnectionSettings }

procedure rConnectionSettings.LanNetworkPreset();
begin
  SendTimeout := 5000;
  ReceiveTimeout := 5000;
  ConnectTimeout := 10000;
end;

{ tRestClient }

constructor tRestClient.Create();
begin
  fConnectionSettings.LanNetworkPreset();
end;

destructor tRestClient.Destroy();
begin
  DeInitialize();
  inherited;
end;

function tRestClient.Initialize(ClSettings: rClientSettings; ConSettings: rConnectionSettings): boolean;
begin
  fConnectionSettings := ConSettings;
  Result := Initialize(ClSettings);
end;

function tRestClient.Initialize(ClSettings: rClientSettings): boolean;
begin
  Result := False;
  // Destroy current object
  DeInitialize();
  // Client initialization (for better understanding, each section contain separate code, later should be refactored)
  fClientSettings := ClSettings;
  fModel := TSQLModel.Create([], ROOT_NAME);
  case fClientSettings.Protocol of
    HTTP_Socket:
      begin
        fClient := TSQLHttpClientWinSock.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWinSock(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
      end;
    HTTP_HTTPsys:
      begin
        fClient := TSQLHttpClientWinHTTP.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWinHTTP(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        TSQLHttpClientWinHTTP(fClient).Compression := [hcSynShaAes];
      end;
   {$ifdef MSWINDOWS}
    HTTPsys_SSL:
      begin
        fClient := TSQLHttpClientWinHTTP.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, True, '', '', fConnectionSettings.SendTimeout,
          fConnectionSettings.ReceiveTimeout, fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWinHTTP(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        TSQLHttpClientWinHTTP(fClient).Compression := [hcSynShaAes];
      end;
   {$endif}
    HTTPsys_AES:
      begin
        fClient := TSQLHttpClientWinHTTP.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWinHTTP(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        TSQLHttpClientWinHTTP(fClient).Compression := [hcSynShaAes];
      end;
    HTTP_WebSocket:
      begin
        fClient := TSQLHttpClientWebsockets.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWebsockets(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
      end;
    WebSocketBidir_JSON:
      begin
        fClient := TSQLHttpClientWebsockets.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWebsockets(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        (fClient as TSQLHttpClientWebsockets).WebSocketsUpgrade('', True);
      end;
    WebSocketBidir_Binary:
      begin
        fClient := TSQLHttpClientWebsockets.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWebsockets(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        (fClient as TSQLHttpClientWebsockets).WebSocketsUpgrade('', False);
      end;
    WebSocketBidir_BinaryAES:
      begin
        fClient := TSQLHttpClientWebsockets.Create(AnsiString(fClientSettings.HostOrIP), AnsiString(fClientSettings.Port), fModel, fConnectionSettings.SendTimeout, fConnectionSettings.ReceiveTimeout,
          fConnectionSettings.ConnectTimeout);
        TSQLHttpClientWebsockets(fClient).KeepAliveMS := CONNECTION_TIMEOUT;
        (fClient as TSQLHttpClientWebsockets).WebSocketsUpgrade('2141D32ADAD54D9A9DB56000CC9A4A70', False);
      end;
    {$ifdef MSWINDOWS}
    NamedPipe:
      begin
        fClient := TSQLRestClientURINamedPipe.Create(fModel, '\\' + fClientSettings.HostOrIP + '\pipe\mORMot_' + NAMED_PIPE_NAME);
      end;
    {$endif}
  else
    begin
      DeInitialize();
      raise Exception.Create('Selected protocol not available in this build.');
    end;
  end;
  case fClientSettings.AuthMode of
    // NoAuthentication
    NoAuthentication:
      begin
        // nothing to do here
      end;
    // TSQLRestServerAuthenticationDefault
    Default:
      begin
        fClient.SetUser(fClientSettings.UserLogin, fClientSettings.UserPassword);
      end;
    // TSQLRestServerAuthenticationNone
    None:
      begin
        TSQLRestServerAuthenticationNone.ClientSetUser(fClient, fClientSettings.UserLogin, fClientSettings.UserPassword);
      end;
    // TSQLRestServerAuthenticationHttpBasic
    HttpBasic:
      begin
        TSQLRestServerAuthenticationHttpBasic.ClientSetUser(fClient, fClientSettings.UserLogin, fClientSettings.UserPassword);
      end;
    // TSQLRestServerAuthenticationSSPI
    {$ifdef MSWINDOWS}
    SSPI:
      begin
        TSQLRestServerAuthenticationSSPI.ClientSetUser(fClient, fClientSettings.UserLogin, fClientSettings.UserPassword);
      end;
    {$endif}
  else
    begin
      DeInitialize();
      raise Exception.Create('Selected Authentication mode not available in this build.');
    end;
  end;
  // Preparing
  if not fClient.ServerTimeStampSynchronize() then
    begin
      ShowMessage(UTF8ToString(fClient.LastErrorMessage));
      exit;
    end;
  // Service initialization
  fClient.ServiceDefine([IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
  // Result := Client.Services['RestMethods'].Get(RestServerMethods);
  Result := fClient.Services.Resolve(IRestMethods, RestMethods); // same result, but no chance to make mistake with service name
  fInitialized := Result;
end;

procedure tRestClient.DeInitialize();
begin
  RestMethods := nil;
  if Assigned(fClient) then
    FreeAndNil(fClient);
  if Assigned(fModel) then
    FreeAndNil(fModel);
  fInitialized := False;
end;

initialization

RestClient := tRestClient.Create();

finalization

if Assigned(RestClient) then
  FreeAndNil(RestClient);

end.
