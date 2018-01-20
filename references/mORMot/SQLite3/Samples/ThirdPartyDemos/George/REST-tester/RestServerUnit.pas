unit RestServerUnit;

interface

uses
  // RTL
  SysUtils,
  Classes,
  StrUtils,
  Generics.Collections,
  Dialogs,
  // mORMot
  mORMot,
  mORMotHttpServer,
  SynBidirSock,
  SynCommons,
  SynCrtSock,
  // Custom
  RestMethodsInterfaceUnit,
  RestServerMethodsUnit;

type
  lProtocol = (HTTP_Socket, HTTPsys, HTTPsys_SSL, HTTPsys_AES, HTTP_WebSocket, WebSocketBidir_JSON, WebSocketBidir_Binary, WebSocketBidir_BinaryAES{$IFDEF MSWINDOWS}, NamedPipe{$ENDIF});
  lAuthenticationMode = (NoAuthentication, Default, None, HttpBasic{$IFDEF MSWINDOWS},SSPI{$ENDIF});
  lAuthorizationPolicy = (AllowAll, DenyAll, FollowGroupsSettings);

  rAuthGroup = record
    Name: RawUTF8;
    SessionTimeout: integer;
    SQLAccessRights: TSQLAccessRights;
  end;

  rAuthUser = record
    LogonName: RawUTF8;
    DisplayName: RawUTF8;
    PasswordPlain: RawUTF8;
    PasswordHashHexa: RawUTF8;
    Group: RawUTF8;
  end;

  rMethodAuthorizationSettings = record
    MethodName: RawUTF8;
    AllowedGroups: array of RawUTF8;
    DeniedGroups: array of RawUTF8;
  end;

  TRestServerSettings = class
  private
    fGroupList: TList<rAuthGroup>;
    fUserList: TList<rAuthUser>;
    fMethodAuthorizationSettings: TList<rMethodAuthorizationSettings>;
  public
    Protocol: lProtocol;
    Port: string;
    AuthenticationMode: lAuthenticationMode;
    AuthorizationPolicy: lAuthorizationPolicy;
    constructor Create();
    destructor Destroy(); override;
    function AddGroup(AuthGroup: rAuthGroup): boolean;
    function AddUser(AuthUser: rAuthUser): boolean;
    function AddMethodAuthorizationSettings(MethodAuthorizationSettings: rMethodAuthorizationSettings): boolean;
  end;

  tRestServer = class
  private
    fModel: TSQLModel;
    fRestServer: TSQLRestServer;
    fHTTPServer: TSQLHttpServer;
    fServerSettings: TRestServerSettings;
    fInitialized: boolean;
    procedure ApplyAuthorizationRules(ServiceFactoryServer: TServiceFactoryServer; RestServerSettings: TRestServerSettings);
  public
    property Initialized: boolean read fInitialized;
    constructor Create();
    destructor Destroy(); override;
    function Initialize(SrvSettings: TRestServerSettings): boolean;
    function DeInitialize(): boolean;
  end;

const
  AppID = '{AA4AC37D-B812-46A7-BEFB-A68167A05BA7}';

var
  RestServer: tRestServer;

implementation

{ TServerSettings }

constructor TRestServerSettings.Create();
begin
  AuthenticationMode := lAuthenticationMode.NoAuthentication;
  AuthorizationPolicy := lAuthorizationPolicy.AllowAll;
  fGroupList := TList<rAuthGroup>.Create();
  fUserList := TList<rAuthUser>.Create();
  fMethodAuthorizationSettings := TList<rMethodAuthorizationSettings>.Create();
end;

destructor TRestServerSettings.Destroy();
begin
  fGroupList.Free;
  fUserList.Free;
  fMethodAuthorizationSettings.Free;
  inherited;
end;

function TRestServerSettings.AddGroup(AuthGroup: rAuthGroup): boolean;
begin
  Result := True;
  // some checks must be implemented here
  // ...
  fGroupList.Add(AuthGroup);
end;

function TRestServerSettings.AddUser(AuthUser: rAuthUser): boolean;
begin
  Result := True;
  // some checks must be implemented here
  // ...
  fUserList.Add(AuthUser);
end;

function TRestServerSettings.AddMethodAuthorizationSettings(MethodAuthorizationSettings: rMethodAuthorizationSettings): boolean;
begin
  Result := True;
  // some checks must be implemented here
  // ...
  fMethodAuthorizationSettings.Add(MethodAuthorizationSettings);
end;

{ tRestServer }

constructor tRestServer.Create();
begin
  //
end;

destructor tRestServer.Destroy();
begin
  DeInitialize();
  fServerSettings.Free();
  inherited;
end;

procedure tRestServer.ApplyAuthorizationRules(ServiceFactoryServer: TServiceFactoryServer; RestServerSettings: TRestServerSettings);
var
  User: TSQLAuthUser;
  Group: TSQLAuthGroup;
  GroupID: TID;
  i, j: integer;
  AuthGroup: rAuthGroup;
  AuthUser: rAuthUser;
  MethodAuthorizationSettings: rMethodAuthorizationSettings;
  // IDs: TIDDynArray;
begin
  // ID := fRestServer.MainFieldID(TSQLAuthGroup, 'User');

  { TSQLAuthGroup }
  // Clear default groups
  fRestServer.Delete(TSQLAuthGroup, '');
  for i := 0 to RestServerSettings.fGroupList.Count - 1 do
    begin
      AuthGroup := RestServerSettings.fGroupList.Items[i];
      Group := TSQLAuthGroup.Create();
      Group.Ident := AuthGroup.Name;
      Group.SQLAccessRights := AuthGroup.SQLAccessRights;
      Group.SessionTimeout := AuthGroup.SessionTimeout;
      // Save object to ORM
      fRestServer.Add(Group, True);
      // Cleanup
      Group.Free;
    end;

  { TSQLAuthUser }
  // Clear default users
  fRestServer.Delete(TSQLAuthUser, '');
  for i := 0 to RestServerSettings.fUserList.Count - 1 do
    begin
      AuthUser := RestServerSettings.fUserList.Items[i];
      User := TSQLAuthUser.Create();
      User.DisplayName := AuthUser.DisplayName;
      User.LogonName := AuthUser.LogonName;
      User.PasswordPlain := AuthUser.PasswordPlain;
      if AuthUser.PasswordHashHexa <> '' then
        User.PasswordHashHexa := AuthUser.PasswordHashHexa;
      if AuthUser.Group <> '' then
        begin
          GroupID := fRestServer.MainFieldID(TSQLAuthGroup, AuthUser.Group);
          User.GroupRights := pointer(GroupID);
        end;
      // Save object to ORM
      fRestServer.Add(User, True);
      // Cleanup
      User.Free;
    end;

  {
    // DEBUG
    fRestServer.MainFieldIDs(TSQLAuthGroup, ['Administrators', 'Users'], IDs);
    if Length(IDs) = 0 then
    ShowMessage('why IDs = []? (((');
    // WHY IDs empty?
    Group := TSQLAuthGroup.CreateAndFillPrepare(fRestServer, '');
    try
    while Group.FillOne do
    ShowMessage(Group.Ident);
    finally
    Group.Free;
    end;
    // DEBUG
  }

  // Apply Authorization Rules
  case RestServerSettings.AuthorizationPolicy of
    AllowAll:
      ServiceFactoryServer.AllowAll();
    DenyAll:
      ServiceFactoryServer.DenyAll();
    FollowGroupsSettings:
      begin
        ServiceFactoryServer.DenyAll();
        for i := 0 to RestServerSettings.fMethodAuthorizationSettings.Count - 1 do
          begin
            MethodAuthorizationSettings := RestServerSettings.fMethodAuthorizationSettings.Items[i];
            // ServiceFactoryServer.AllowByName([MethodAuthorizationSettings.MethodName], MethodAuthorizationSettings.AllowedGroups); // not work for some reason :(
            // ServiceFactoryServer.DenyByName([MethodAuthorizationSettings.MethodName], MethodAuthorizationSettings.DeniedGroups);
            for j := 0 to Length(MethodAuthorizationSettings.AllowedGroups) - 1 do
              ServiceFactoryServer.AllowByName([MethodAuthorizationSettings.MethodName], MethodAuthorizationSettings.AllowedGroups[j]);
            for j := 0 to Length(MethodAuthorizationSettings.DeniedGroups) - 1 do
              ServiceFactoryServer.DenyByName([MethodAuthorizationSettings.MethodName], MethodAuthorizationSettings.DeniedGroups[j]);
          end;
      end;
  end;
end;

function tRestServer.Initialize(SrvSettings: TRestServerSettings): boolean;
var
  ServiceFactoryServer: TServiceFactoryServer;
  { WebSocketServerRest: TWebSocketServerRest; }
begin
  Result := false;
  // Destroy current object
  if DeInitialize() then
    try
      // Server initialization (!!! for better understanding, each section contain separate code, later should be refactored)
      fServerSettings.Free;
      fServerSettings := SrvSettings;
      case fServerSettings.AuthenticationMode of
        // NoAuthentication
        NoAuthentication:
          begin
            fModel := TSQLModel.Create([], ROOT_NAME);
            fRestServer := TSQLRestServerFullMemory.Create(fModel, false);
            ServiceFactoryServer := fRestServer.ServiceDefine(TRestMethods, [IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
            ServiceFactoryServer.SetOptions([], [optErrorOnMissingParam]);
          end;
        // TSQLRestServerAuthenticationDefault
        Default:
          begin
            fModel := TSQLModel.Create([], ROOT_NAME);
            fRestServer := TSQLRestServerFullMemory.Create(fModel, false { make AuthenticationSchemesCount = 0 } );
            ServiceFactoryServer := fRestServer.ServiceDefine(TRestMethods, [IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
            fRestServer.AuthenticationRegister(TSQLRestServerAuthenticationDefault); // register single authentication mode
            ApplyAuthorizationRules(ServiceFactoryServer, fServerSettings);
            ServiceFactoryServer.SetOptions([], [optErrorOnMissingParam]);
          end;
        // TSQLRestServerAuthenticationNone
        None:
          begin
            fModel := TSQLModel.Create([], ROOT_NAME);
            fRestServer := TSQLRestServerFullMemory.Create(fModel, false { make AuthenticationSchemesCount = 0 } );
            ServiceFactoryServer := fRestServer.ServiceDefine(TRestMethods, [IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
            fRestServer.AuthenticationRegister(TSQLRestServerAuthenticationNone); // register single authentication mode
            ApplyAuthorizationRules(ServiceFactoryServer, fServerSettings);
            ServiceFactoryServer.SetOptions([], [optErrorOnMissingParam]);
          end;
        // TSQLRestServerAuthenticationHttpBasic
        HttpBasic:
          begin
            fModel := TSQLModel.Create([], ROOT_NAME);
            fRestServer := TSQLRestServerFullMemory.Create(fModel, false { make AuthenticationSchemesCount = 0 } );
            ServiceFactoryServer := fRestServer.ServiceDefine(TRestMethods, [IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
            fRestServer.AuthenticationRegister(TSQLRestServerAuthenticationHttpBasic); // register single authentication mode
            ApplyAuthorizationRules(ServiceFactoryServer, fServerSettings);
            ServiceFactoryServer.SetOptions([], [optErrorOnMissingParam]);
          end;
        // TSQLRestServerAuthenticationSSPI
        {$IFDEF MSWINDOWS}
        SSPI:
          begin
            fModel := TSQLModel.Create([], ROOT_NAME);
            fRestServer := TSQLRestServerFullMemory.Create(fModel, false { make AuthenticationSchemesCount = 0 } );
            ServiceFactoryServer := fRestServer.ServiceDefine(TRestMethods, [IRestMethods], SERVICE_INSTANCE_IMPLEMENTATION);
            fRestServer.AuthenticationRegister(TSQLRestServerAuthenticationSSPI); // register single authentication mode
            ApplyAuthorizationRules(ServiceFactoryServer, fServerSettings);
            ServiceFactoryServer.SetOptions([], [optErrorOnMissingParam]);
          end;
        {$endif}
      else
        begin
          DeInitialize();
          raise Exception.Create('Selected Authentication mode not available in this build.');
        end;
      end;
      // protocol initialization
      case fServerSettings.Protocol of
        HTTP_Socket:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', useHttpSocket);
            THttpServer(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
          end;
        {
          // require manual URI registration, we will not use this option in this test project, because this option
          // should be used with installation program that will unregister all used URIs during sofware uninstallation.
          HTTPsys:
          begin
          HTTPServer := TSQLHttpServer.Create(AnsiString(Options.Port), [RestServer], '+', useHttpApi);
          THttpServer(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := SERVER_CONNECTION_TIMEOUT;
          end;
        }
        HTTPsys:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', HTTP_DEFAULT_MODE);
            THttpServer(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
          end;
        HTTPsys_SSL:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', HTTP_DEFAULT_MODE, 32, TSQLHttpServerSecurity.secSSL);
            THttpServer(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
          end;
        HTTPsys_AES:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', HTTP_DEFAULT_MODE, 32, TSQLHttpServerSecurity.secSynShaAes);
            THttpServer(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
          end;
        HTTP_WebSocket:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', useBidirSocket);
            TWebSocketServerRest(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
          end;
        WebSocketBidir_JSON:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', useBidirSocket);
            TWebSocketServerRest(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
            { WebSocketServerRest := } fHTTPServer.WebSocketsEnable(fRestServer, '', True);
          end;
        WebSocketBidir_Binary:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', useBidirSocket);
            TWebSocketServerRest(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
            { WebSocketServerRest := } fHTTPServer.WebSocketsEnable(fRestServer, '', false);
          end;
        WebSocketBidir_BinaryAES:
          begin
            fHTTPServer := TSQLHttpServer.Create(AnsiString(fServerSettings.Port), [fRestServer], '+', useBidirSocket);
            TWebSocketServerRest(fHTTPServer.HttpServer).ServerKeepAliveTimeOut := CONNECTION_TIMEOUT;
            { WebSocketServerRest := } fHTTPServer.WebSocketsEnable(fRestServer, '2141D32ADAD54D9A9DB56000CC9A4A70', false);
          end;
        {$IFDEF MSWINDOWS}
        NamedPipe:
          begin
            if not fRestServer.ExportServerNamedPipe(NAMED_PIPE_NAME) then
              Exception.Create('Unable to register server with named pipe channel.');
          end;
        {$ENDIF}
      else
        begin
          DeInitialize();
          raise Exception.Create('Selected protocol not available in this build.');
        end;
      end;
      Result := True;
    except
      on E: Exception do
        begin
          ShowMessage(E.ToString);
          DeInitialize();
        end;
    end;
  fInitialized := Result;
end;

function tRestServer.DeInitialize(): boolean;
begin
  Result := True;
  try
    // if used HttpApiRegisteringURI then remove registration (require run as admin), but seems not work from here
    {$IFDEF MSWINDOWS}
    if Assigned(fHTTPServer) and (fHTTPServer.HttpServer.ClassType = THttpApiServer) then
      THttpApiServer(fHTTPServer.HttpServer).RemoveUrl(ROOT_NAME, fHTTPServer.Port, fServerSettings.Protocol = HTTPsys_SSL, '+');
    {$ENDIF}
    if Assigned(fHTTPServer) then
      FreeAndNil(fHTTPServer);
    if Assigned(fRestServer) then
      FreeAndNil(fRestServer);
    if Assigned(fModel) then
      FreeAndNil(fModel);
    fInitialized := false;
  except
    on E: Exception do
      begin
        ShowMessage(E.ToString);
        Result := false;
      end;
  end;
end;

initialization

RestServer := tRestServer.Create();

finalization

if Assigned(RestServer) then
  FreeAndNil(RestServer);

end.
