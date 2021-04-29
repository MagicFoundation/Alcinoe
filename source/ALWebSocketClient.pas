{*******************************************************************************
TALWebSocketClient is a ancestor of class like TALWinHTTPWebSocketClient
*******************************************************************************}

unit ALWebSocketClient;

interface

uses
  system.Classes,
  ALHttpClient;

type

  {---------------------------------------------------------}
  EALWebSocketClientException = class(EALHTTPClientException)
  end;

  {--------------------------------------------------------------------------------------------------------------------}
  TALWebSocketClientReceiveEvent  = procedure(sender: Tobject; const Data: AnsiString; const IsUTF8: Boolean) of object;
  TALWebSocketClientErrorEvent  = procedure(sender: Tobject; const &Message: AnsiString) of object;

  {---------------------------------}
  TALWebSocketClient = class(TObject)
  private
    FProxyParams: TALHttpClientProxyParams;
    FRequestHeader: TALHTTPRequestHeader;
    FProtocolVersion: TALHTTPProtocolVersion;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnReceive: TALWebSocketClientReceiveEvent;
    fOnError: TALWebSocketClientErrorEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FBufferSize: cardinal;
    FAutoReconnect: Boolean;
  protected
    procedure SetUsername(const NameValue: AnsiString); virtual;
    procedure SetPassword(const PasswordValue: AnsiString); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetBufferSize(const Value: Cardinal); virtual;
    procedure SetOnReceive(const Value: TALWebSocketClientReceiveEvent); virtual;
    procedure SetOnError(const Value: TALWebSocketClientErrorEvent); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //-----
    procedure Connect(const aUrl:AnsiString); virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure Send(const aData: AnsiString;
                   const aIsUTF8: Boolean = True); virtual; abstract;
    //-----
    property  ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property  SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property  ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property  BufferSize: cardinal read FBufferSize write SetBufferSize default $8000;
    property  ProxyParams: TALHttpClientProxyParams read FProxyParams;
    property  RequestHeader: TALHTTPRequestHeader read FRequestHeader;
    Property  ProtocolVersion: TALHTTPProtocolVersion read FProtocolVersion write FProtocolVersion default HTTPpv_1_1;
    property  UserName: AnsiString read FUserName write SetUserName;
    property  Password: AnsiString read FPassword write SetPassword;
    property  OnReceive: TALWebSocketClientReceiveEvent read FOnReceive write SetOnReceive;
    property  OnError: TALWebSocketClientErrorEvent read FOnError write SetOnError;
    property  AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;
  end;

implementation

uses
  AlCommon;

{************************************}
constructor TALWebSocketClient.Create;
begin
  inherited;
  FBufferSize := $8000;
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
  FUserName := '';
  FPassword := '';
  FOnReceive := nil;
  FOnError := nil;
  FOnConnected := nil;
  FOnDisconnected := nil;
  FProxyParams := TALHttpClientProxyParams.Create;
  FProxyParams.OnChange := OnProxyParamsChange;
  FRequestHeader := TALHTTPRequestHeader.Create;
  FRequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALWebSocketClient)';
  FProtocolVersion := HTTPpv_1_1;
  fAutoReconnect := True;
end;

{************************************}
destructor TALWebSocketClient.Destroy;
begin
  AlFreeAndNil(FProxyParams);
  AlFreeAndNil(FRequestHeader);
  inherited;
end;

{****************************************************************}
procedure TALWebSocketClient.SetBufferSize(const Value: Cardinal);
begin
  FBufferSize := Value;
end;

{********************************************************************}
procedure TALWebSocketClient.SetUsername(const NameValue: AnsiString);
begin
  FUserName := NameValue;
end;

{************************************************************************}
procedure TALWebSocketClient.SetPassword(const PasswordValue: AnsiString);
begin
  FPassword := PasswordValue;
end;

{*************************************************************************************}
procedure TALWebSocketClient.SetOnReceive(const Value: TALWebSocketClientReceiveEvent);
begin
  FOnReceive := Value;
end;

{*********************************************************************************}
procedure TALWebSocketClient.SetOnError(const Value: TALWebSocketClientErrorEvent);
begin
  FOnError := Value;
end;

{**********************************************************************************************}
procedure TALWebSocketClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

end.
