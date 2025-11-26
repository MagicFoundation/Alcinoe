unit Alcinoe.HTTP.Client;

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  Alcinoe.Common,
  Alcinoe.StringList,
  Alcinoe.Mime.Multipart,
  Alcinoe.HTTP,
  System.Classes;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientRequestHeadersA = Class(TALHttpRequestHeadersA)
  private
    FKnownHeaders: array[0..39] of AnsiString;
    FCookies: TALStringsA;
    FUnknownHeaders: TALStringsA;
  protected
    function GetCookies: TALStringsA; override;
    function GetUnknownHeaders: TALStringsA; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); override;
    Function GetRawHeaderText: AnsiString; override;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientRequestHeadersW = Class(TALHttpRequestHeadersW)
  private
    FKnownHeaders: array[0..39] of String;
    FCookies: TALStringsW;
    FUnknownHeaders: TALStringsW;
  protected
    function GetCookies: TALStringsW; override;
    function GetUnknownHeaders: TALStringsW; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): String; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String); override;
    Function GetRawHeaderText: String; override;
    procedure SetRawHeaderText(const ARawHeaderText: String); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientResponseHeadersA = Class(TALHttpResponseHeadersA)
  private
    FKnownHeaders: array[0..28] of AnsiString;
    FCookies: TObjectList<TALHttpCookieA>;
    FUnknownHeaders: TALStringsA;
  protected
    function GetCookies: TObjectList<TALHttpCookieA>; override;
    function GetUnknownHeaders: TALStringsA; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); override;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); override;
    Function GetRawHeaderText: AnsiString; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientResponseHeadersW = Class(TALHttpResponseHeadersW)
  private
    FKnownHeaders: array[0..28] of String;
    FCookies: TObjectList<TALHttpCookieW>;
    FUnknownHeaders: TALStringsW;
  protected
    function GetCookies: TObjectList<TALHttpCookieW>; override;
    function GetUnknownHeaders: TALStringsW; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): String; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String); override;
    procedure SetRawHeaderText(const ARawHeaderText: String); override;
    Function GetRawHeaderText: String; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientResponseA = Class(TALHTTPResponseA)
  private
    FVersion: TALHttpVersion;
    FStatusCode: Integer;
    FReason: AnsiString;
    FHeaders: TALHttpClientResponseHeadersA;
    FBodyStream: TStream;
    FOwnsBodyStream: Boolean;
  protected
    function GetVersion: TALHttpVersion; override;
    procedure SetVersion(const AValue: TALHttpVersion); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const AValue: Integer); override;
    function GetReason: AnsiString; override;
    procedure SetReason(const AValue: AnsiString); override;
    function GetHeaders: TALHttpResponseHeadersA; override;
    function GetBodyStream: TStream; override;
    procedure SetBodyStream(const AValue: TStream); override;
    function GetOwnsBodyStream: Boolean; override;
    procedure SetOwnsBodyStream(const AValue: Boolean); override;
    function GetBodyString: AnsiString; override;
    procedure SetBodyString(const AValue: AnsiString); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientResponseW = Class(TALHTTPResponseW)
  private
    FVersion: TALHttpVersion;
    FStatusCode: Integer;
    FReason: String;
    FHeaders: TALHttpClientResponseHeadersW;
    FBodyStream: TStream;
    FOwnsBodyStream: Boolean;
  protected
    function GetVersion: TALHttpVersion; override;
    procedure SetVersion(const AValue: TALHttpVersion); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const AValue: Integer); override;
    function GetReason: String; override;
    procedure SetReason(const AValue: String); override;
    function GetHeaders: TALHttpResponseHeadersW; override;
    function GetBodyStream: TStream; override;
    procedure SetBodyStream(const AValue: TStream); override;
    function GetOwnsBodyStream: Boolean; override;
    procedure SetOwnsBodyStream(const AValue: Boolean); override;
    function GetBodyString: String; override;
    procedure SetBodyString(const AValue: String); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientProxyParamsA = Class(Tobject)
  public
    type
      TPropertyChangeEvent = procedure(Sender: TObject; Const PropertyIndex: Integer) of object;
  Private
    FProxyBypass: AnsiString;
    FproxyServer: AnsiString;
    FProxyUserName: AnsiString;
    FProxyPassword: AnsiString;
    FproxyPort: integer;
    FOnChange: TPropertyChangeEvent;
    procedure SetProxyBypass(const Value: AnsiString);
    procedure SetProxyPassword(const Value: AnsiString);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: AnsiString);
    procedure SetProxyUserName(const Value: AnsiString);
    Procedure DoChange(propertyIndex: Integer);
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    Property ProxyBypass: AnsiString read FProxyBypass write SetProxyBypass; //index 0
    property ProxyServer: AnsiString read FProxyServer write SetProxyServer; //index 1
    property ProxyPort: integer read FProxyPort write SetProxyPort default 0; //index 2
    property ProxyUserName: AnsiString read FProxyUserName write SetProxyUserName; //index 3
    property ProxyPassword: AnsiString read FProxyPassword write SetProxyPassword; //index 4
    property OnChange: TPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientProxyParamsW = Class(Tobject)
  public
    type
      TPropertyChangeEvent = procedure(Sender: TObject; Const PropertyIndex: Integer) of object;
  Private
    FProxyBypass: String;
    FproxyServer: String;
    FProxyUserName: String;
    FProxyPassword: String;
    FproxyPort: integer;
    FOnChange: TPropertyChangeEvent;
    procedure SetProxyBypass(const Value: String);
    procedure SetProxyPassword(const Value: String);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: String);
    procedure SetProxyUserName(const Value: String);
    Procedure DoChange(propertyIndex: Integer);
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    Property ProxyBypass: String read FProxyBypass write SetProxyBypass; //index 0
    property ProxyServer: String read FProxyServer write SetProxyServer; //index 1
    property ProxyPort: integer read FProxyPort write SetProxyPort default 0; //index 2
    property ProxyUserName: String read FProxyUserName write SetProxyUserName; //index 3
    property ProxyPassword: String read FProxyPassword write SetProxyPassword; //index 4
    property OnChange: TPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientA = class(TObject)
  public
    type
      TRedirectEvent = procedure(Sender: Tobject; const NewURL: AnsiString) of object;
      TUploadProgressEvent = procedure(Sender: Tobject; Sent: Int64; Total: Int64) of object;
      TDownloadProgressEvent = procedure(Sender: Tobject; Read: Int64; Total: Int64) of object;
  private
    FProxyParams: TALHttpClientProxyParamsA;
    FRequestHeaders: TALHttpClientRequestHeadersA;
    FProtocolVersion: TALHTTPVersion;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TUploadProgressEvent;
    FOnDownloadProgress: TDownloadProgressEvent;
    FOnRedirect: TRedirectEvent;
  protected
    procedure SetUsername(const AValue: AnsiString); virtual;
    procedure SetPassword(const AValue: AnsiString); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetOnRedirect(const Value: TRedirectEvent); virtual;
    function Execute(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHttpClientResponseA; virtual; abstract;
    function ExecuteFormUrlEncoded(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const AFormParams: TALStringsA;
               Const AEncodeParams: Boolean;
               const AAddParamsToUrl: Boolean;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHttpClientResponseA;
    function ExecuteMultipartFormData(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHttpClientResponseA;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Get(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA; overload;
    function Get(
               const AUrl: AnsiString;
               const AQueryParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA; overload;
    function Post(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PostFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PostMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Head(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Trace(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Put(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PutFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PutMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Patch(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PatchFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function PatchMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Delete(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    function Options(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property ProxyParams: TALHttpClientProxyParamsA read FProxyParams;
    property RequestHeaders: TALHttpClientRequestHeadersA read FRequestHeaders;
    Property ProtocolVersion: TALHTTPVersion read FProtocolVersion write FProtocolVersion default TALHTTPVersion.v1_1;
    property UserName: AnsiString read FUserName write SetUserName;
    property Password: AnsiString read FPassword write SetPassword;
    property OnUploadProgress: TUploadProgressEvent read FOnUploadProgress write FOnUploadProgress;
    property OnDownloadProgress: TDownloadProgressEvent read FonDownloadProgress write FonDownloadProgress;
    property OnRedirect: TRedirectEvent read FOnRedirect write SetOnRedirect;
  end;

var
  ALMaxKeepAliveHttpClientPerHost: integer = 16;
  ALCreateHttpClientReceiveTimeout: integer = 20000;
  ALCreateHttpClientSendTimeout: integer = 20000;
  ALCreateHttpClientConnectTimeout: integer = 20000;

implementation

uses
  System.SysUtils,
  system.AnsiStrings,
  Alcinoe.Url,
  Alcinoe.StringUtils;

{**********************************************}
constructor TALHttpClientRequestHeadersA.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{**********************************************}
destructor TALHttpClientRequestHeadersA.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{*******************************************}
procedure TALHttpClientRequestHeadersA.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{************************************************************}
function TALHttpClientRequestHeadersA.GetCookies: TALStringsA;
begin
  if FCookies = nil then begin
    FCookies := TALNVStringListA.Create;
    FCookies.LineBreak := '; ';
    FCookies.TrailingLineBreak := False;
  end;
  Result := FCookies;
end;

{*******************************************************************}
function TALHttpClientRequestHeadersA.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.TrailingLineBreak := False;
  end;
  Result := FUnknownHeaders;
end;

{****************************************************************************************************}
function TALHttpClientRequestHeadersA.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{******************************************************************************************************************}
procedure TALHttpClientRequestHeadersA.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{*****************************************************************}
Function TALHttpClientRequestHeadersA.GetRawHeaderText: AnsiString;
begin
  var SB := TALStringBuilderA.Create(2048);
  try
    // 1) Known headers
    for var I := Low(FKnownHeaders) to High(FKnownHeaders) do begin
      if FKnownHeaders[i] <> '' then begin
        SB.Append(PropertyIndexToName(i));
        SB.Append(': ');
        SB.AppendLine(FKnownHeaders[i]);
      end;
    end;

    // 2) Cookies
    If (FCookies <> nil) and (FCookies.Count > 0) then begin
      SB.Append('Cookie: ');
      SB.AppendLine(FCookies.text);
    end;

    // 3) Unknown headers
    for var I := 0 to UnknownHeaders.Count - 1 do begin
      SB.Append(UnknownHeaders.Names[I]);
      SB.Append(': ');
      SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
    end;

    // 4) Produce the final string
    Result := SB.ToString(true{AUpdateCapacity});
  finally
    ALFreeAndNil(SB);
  end;
end;

{****************************************************************************************}
procedure TALHttpClientRequestHeadersA.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  Clear;
  var LRawHeaders := TALNVStringListA.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do
      Values[ALTrim(LRawHeaders.Names[I])] := alTrim(LRawHeaders.ValueFromIndex[I]);
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

{**********************************************}
constructor TALHttpClientRequestHeadersW.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{**********************************************}
destructor TALHttpClientRequestHeadersW.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{*******************************************}
procedure TALHttpClientRequestHeadersW.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{************************************************************}
function TALHttpClientRequestHeadersW.GetCookies: TALStringsW;
begin
  if FCookies = nil then begin
    FCookies := TALNVStringListW.Create;
    FCookies.LineBreak := '; ';
    FCookies.TrailingLineBreak := False;
  end;
  Result := FCookies;
end;

{*******************************************************************}
function TALHttpClientRequestHeadersW.GetUnknownHeaders: TALStringsW;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListW.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.TrailingLineBreak := False;
  end;
  Result := FUnknownHeaders;
end;

{************************************************************************************************}
function TALHttpClientRequestHeadersW.GetHeaderValueByPropertyIndex(const Index: Integer): String;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{**************************************************************************************************************}
procedure TALHttpClientRequestHeadersW.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{*************************************************************}
Function TALHttpClientRequestHeadersW.GetRawHeaderText: String;
begin
  var SB := TALStringBuilderW.Create(2048);
  try
    // 1) Known headers
    for var I := Low(FKnownHeaders) to High(FKnownHeaders) do begin
      if FKnownHeaders[i] <> '' then begin
        SB.Append(PropertyIndexToName(i));
        SB.Append(': ');
        SB.AppendLine(FKnownHeaders[i]);
      end;
    end;

    // 2) Cookies
    If (FCookies <> nil) and (FCookies.Count > 0) then begin
      SB.Append('Cookie: ');
      SB.AppendLine(FCookies.text);
    end;

    // 3) Unknown headers
    for var I := 0 to UnknownHeaders.Count - 1 do begin
      SB.Append(UnknownHeaders.Names[I]);
      SB.Append(': ');
      SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
    end;

    // 4) Produce the final string
    Result := SB.ToString(true{AUpdateCapacity});
  finally
    ALFreeAndNil(SB);
  end;
end;

{************************************************************************************}
procedure TALHttpClientRequestHeadersW.SetRawHeaderText(const ARawHeaderText: String);
begin
  Clear;
  var LRawHeaders := TALNVStringListW.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do
      Values[ALTrim(LRawHeaders.Names[I])] := alTrim(LRawHeaders.ValueFromIndex[I]);
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

{***********************************************}
constructor TALHttpClientResponseHeadersA.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{***********************************************}
destructor TALHttpClientResponseHeadersA.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{********************************************}
procedure TALHttpClientResponseHeadersA.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{*****************************************************************************}
function TALHttpClientResponseHeadersA.GetCookies: TObjectList<TALHttpCookieA>;
begin
  if FCookies = nil then
    FCookies := TObjectList<TALHttpCookieA>.Create(True{AOwnsObjects});
  Result := FCookies;
end;

{********************************************************************}
function TALHttpClientResponseHeadersA.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.TrailingLineBreak := False;
  end;
  Result := FUnknownHeaders;
end;

{*****************************************************************************************************}
function TALHttpClientResponseHeadersA.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{*******************************************************************************************************************}
procedure TALHttpClientResponseHeadersA.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{******************************************************************}
Function TALHttpClientResponseHeadersA.GetRawHeaderText: AnsiString;
begin
  var SB := TALStringBuilderA.Create(2048);
  try
    // 1) Known headers
    for var I := Low(FKnownHeaders) to High(FKnownHeaders) do begin
      if FKnownHeaders[i] <> '' then begin
        SB.Append(PropertyIndexToName(i));
        SB.Append(': ');
        SB.AppendLine(FKnownHeaders[i]);
      end;
    end;

    // 2) Cookies
    If (FCookies <> nil) and (FCookies.Count > 0) then begin
      for var I := 0 to FCookies.Count - 1 do begin
        SB.Append('Set-Cookie: ');
        SB.AppendLine(FCookies[i].HeaderValue);
      end;
    end;

    // 3) Unknown headers
    for var I := 0 to UnknownHeaders.Count - 1 do begin
      SB.Append(UnknownHeaders.Names[I]);
      SB.Append(': ');
      SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
    end;

    // 4) Produce the final string
    Result := SB.ToString(true{AUpdateCapacity});
  finally
    ALFreeAndNil(SB);
  end;
end;

{*****************************************************************************************}
procedure TALHttpClientResponseHeadersA.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  Clear;
  var LRawHeaders := TALNVStringListA.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do begin
      var LName := ALTrim(LRawHeaders.Names[I]);
      If ALSameTextA(LName, 'Set-Cookie') then begin
        Var LCookie := TALHttpCookieA.Create;
        LCookie.HeaderValue := alTrim(LRawHeaders.ValueFromIndex[I]);
        Cookies.Add(LCookie);
      end
      else Values[LName] := alTrim(LRawHeaders.ValueFromIndex[I]);
    end;
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

{***********************************************}
constructor TALHttpClientResponseHeadersW.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{***********************************************}
destructor TALHttpClientResponseHeadersW.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{********************************************}
procedure TALHttpClientResponseHeadersW.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{*****************************************************************************}
function TALHttpClientResponseHeadersW.GetCookies: TObjectList<TALHttpCookieW>;
begin
  if FCookies = nil then
    FCookies := TObjectList<TALHttpCookieW>.Create(True{AOwnsObjects});
  Result := FCookies;
end;

{********************************************************************}
function TALHttpClientResponseHeadersW.GetUnknownHeaders: TALStringsW;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListW.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.TrailingLineBreak := False;
  end;
  Result := FUnknownHeaders;
end;

{*************************************************************************************************}
function TALHttpClientResponseHeadersW.GetHeaderValueByPropertyIndex(const Index: Integer): String;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{***************************************************************************************************************}
procedure TALHttpClientResponseHeadersW.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{**************************************************************}
Function TALHttpClientResponseHeadersW.GetRawHeaderText: String;
begin
  var SB := TALStringBuilderW.Create(2048);
  try
    // 1) Known headers
    for var I := Low(FKnownHeaders) to High(FKnownHeaders) do begin
      if FKnownHeaders[i] <> '' then begin
        SB.Append(PropertyIndexToName(i));
        SB.Append(': ');
        SB.AppendLine(FKnownHeaders[i]);
      end;
    end;

    // 2) Cookies
    If (FCookies <> nil) and (FCookies.Count > 0) then begin
      for var I := 0 to FCookies.Count - 1 do begin
        SB.Append('Set-Cookie: ');
        SB.AppendLine(FCookies[i].HeaderValue);
      end;
    end;

    // 3) Unknown headers
    for var I := 0 to UnknownHeaders.Count - 1 do begin
      SB.Append(UnknownHeaders.Names[I]);
      SB.Append(': ');
      SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
    end;

    // 4) Produce the final string
    Result := SB.ToString(true{AUpdateCapacity});
  finally
    ALFreeAndNil(SB);
  end;
end;

{*************************************************************************************}
procedure TALHttpClientResponseHeadersW.SetRawHeaderText(const ARawHeaderText: String);
begin
  Clear;
  var LRawHeaders := TALNVStringListW.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do begin
      var LName := ALTrim(LRawHeaders.Names[I]);
      If ALSameTextW(LName, 'Set-Cookie') then begin
        Var LCookie := TALHttpCookieW.Create;
        LCookie.HeaderValue := alTrim(LRawHeaders.ValueFromIndex[I]);
        Cookies.Add(LCookie);
      end
      else Values[LName] := alTrim(LRawHeaders.ValueFromIndex[I]);
    end;
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

{****************************************}
constructor TALHttpClientResponseA.Create;
begin
  inherited;
  FVersion := TALHttpVersion.Unspecified;
  FStatusCode := 0;
  FReason := '';
  FHeaders := nil;
  FBodyStream := nil;
  FOwnsBodyStream := True;
end;

{****************************************}
destructor TALHttpClientResponseA.Destroy;
begin
  ALFreeAndNil(FHeaders);
  if OwnsBodyStream then ALFreeAndNil(FBodyStream);
  inherited;
end;

{*********************************************************}
function TALHttpClientResponseA.GetVersion: TALHttpVersion;
begin
  Result := FVersion;
end;

{************************************************************************}
procedure TALHttpClientResponseA.SetVersion(const AValue: TALHttpVersion);
begin
  FVersion := AValue;
end;

{*****************************************************}
function TALHttpClientResponseA.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

{********************************************************************}
procedure TALHttpClientResponseA.SetStatusCode(const AValue: Integer);
begin
  If AValue <> FStatusCode then begin
    FStatusCode := AValue;
    Reason := ALGetHttpReasonPhraseA(FStatusCode);
  end;
end;

{****************************************************}
function TALHttpClientResponseA.GetReason: AnsiString;
begin
  Result := FReason;
end;

{*******************************************************************}
procedure TALHttpClientResponseA.SetReason(const AValue: AnsiString);
begin
  FReason := AValue;
end;

{******************************************************************}
function TALHttpClientResponseA.GetHeaders: TALHttpResponseHeadersA;
begin
  if FHeaders = nil then
    FHeaders := TALHttpClientResponseHeadersA.Create;
  Result := FHeaders;
end;

{*****************************************************}
function TALHttpClientResponseA.GetBodyStream: TStream;
begin
  if FBodyStream = nil then
    FBodyStream := TALStringStreamA.Create('');
  Result := FBodyStream;
end;

{********************************************************************}
procedure TALHttpClientResponseA.SetBodyStream(const AValue: TStream);
begin
  if AValue <> FBodyStream then begin
    if OwnsBodyStream then ALFreeAndNil(FBodyStream);
    FBodyStream := AValue;
  end;
end;

{*********************************************************}
function TALHttpClientResponseA.GetOwnsBodyStream: Boolean;
begin
  Result := FOwnsBodyStream;
end;

{************************************************************************}
procedure TALHttpClientResponseA.SetOwnsBodyStream(const AValue: Boolean);
begin
  FOwnsBodyStream := AValue;
end;

{********************************************************}
function TALHttpClientResponseA.GetBodyString: AnsiString;
begin
  if FBodyStream <> nil then begin
    if FBodyStream is TALStringStreamA then Result := TALStringStreamA(FBodyStream).DataString
    else if FBodyStream.Size > 0 then begin
      SetLength(Result, FBodyStream.Size);
      FBodyStream.Position := 0;
      FBodyStream.ReadBuffer(PAnsiChar(Result)^, FBodyStream.Size);
    end
    else Result := '';
  end
  else Result := '';
end;

{***********************************************************************}
procedure TALHttpClientResponseA.SetBodyString(const AValue: AnsiString);
begin
  if FBodyStream <> nil then begin
    if FBodyStream is TALStringStreamA then begin
      TALStringStreamA(FBodyStream).DataString := AValue;
      Exit;
    end
    else if not OwnsBodyStream then begin
      FBodyStream.Position := 0;
      FBodyStream.Size := Length(AValue);
      if Length(AValue) > 0 then FBodyStream.WriteBuffer(PAnsiChar(AValue)^, Length(AValue));
      Exit;
    end
    else
      ALFreeAndNil(FBodyStream)
  end;
  FBodyStream := TALStringStreamA.Create(AValue);
end;

{****************************************}
constructor TALHttpClientResponseW.Create;
begin
  inherited;
  FVersion := TALHttpVersion.Unspecified;
  FStatusCode := 0;
  FReason := '';
  FHeaders := nil;
  FBodyStream := nil;
  FOwnsBodyStream := True;
end;

{****************************************}
destructor TALHttpClientResponseW.Destroy;
begin
  ALFreeAndNil(FHeaders);
  if OwnsBodyStream then ALFreeAndNil(FBodyStream);
  inherited;
end;

{*********************************************************}
function TALHttpClientResponseW.GetVersion: TALHttpVersion;
begin
  Result := FVersion;
end;

{************************************************************************}
procedure TALHttpClientResponseW.SetVersion(const AValue: TALHttpVersion);
begin
  FVersion := AValue;
end;

{*****************************************************}
function TALHttpClientResponseW.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

{********************************************************************}
procedure TALHttpClientResponseW.SetStatusCode(const AValue: Integer);
begin
  If AValue <> FStatusCode then begin
    FStatusCode := AValue;
    Reason := ALGetHttpReasonPhraseW(FStatusCode);
  end;
end;

{************************************************}
function TALHttpClientResponseW.GetReason: String;
begin
  Result := FReason;
end;

{***************************************************************}
procedure TALHttpClientResponseW.SetReason(const AValue: String);
begin
  FReason := AValue;
end;

{******************************************************************}
function TALHttpClientResponseW.GetHeaders: TALHttpResponseHeadersW;
begin
  if FHeaders = nil then
    FHeaders := TALHttpClientResponseHeadersW.Create;
  Result := FHeaders;
end;

{*****************************************************}
function TALHttpClientResponseW.GetBodyStream: TStream;
begin
  if FBodyStream = nil then begin
    var LCharset := Headers.ContentCharset;
    if (LCharSet = '') or (ALSameTextW(LCharSet, 'utf-8')) then
      FBodyStream := TALStringStreamW.Create('', TEncoding.UTF8, False{AOwnsEncoding})
    else
      FBodyStream := TALStringStreamW.Create('', TEncoding.GetEncoding(LCharSet), True{AOwnsEncoding});
  end;
  Result := FBodyStream;
end;

{********************************************************************}
procedure TALHttpClientResponseW.SetBodyStream(const AValue: TStream);
begin
  if AValue <> FBodyStream then begin
    if OwnsBodyStream then ALFreeAndNil(FBodyStream);
    FBodyStream := AValue;
  end;
end;

{*********************************************************}
function TALHttpClientResponseW.GetOwnsBodyStream: Boolean;
begin
  Result := FOwnsBodyStream;
end;

{************************************************************************}
procedure TALHttpClientResponseW.SetOwnsBodyStream(const AValue: Boolean);
begin
  FOwnsBodyStream := AValue;
end;

{****************************************************}
function TALHttpClientResponseW.GetBodyString: String;
begin
  if FBodyStream <> nil then begin
    if FBodyStream is TALStringStreamW then Result := TALStringStreamW(FBodyStream).DataString
    else if FBodyStream.Size > 0 then begin

      var LBytes: TBytes;
      setlength(LBytes, FBodyStream.Size);
      FBodyStream.Position := 0;
      FBodyStream.ReadBuffer(LBytes[0], FBodyStream.Size);

      var LEncoding: TEncoding;
      var LFreeEncoding: boolean;
      var LCharset := Headers.ContentCharset;
      if (LCharSet = '') or (ALSameTextW(LCharSet, 'utf-8')) then begin
        LEncoding := TEncoding.UTF8;
        LFreeEncoding := False;
      end
      else begin
        LEncoding := TEncoding.GetEncoding(LCharSet);
        LFreeEncoding := True;
      end;
      try
        var LPreamble := TEncoding.GetBufferEncoding(LBytes, LEncoding);
        Result := LEncoding.GetString(LBytes, LPreamble, length(LBytes) - LPreamble);
      finally
        if LFreeEncoding then
          ALFreeAndNil(LEncoding);
      end;

    end
    else Result := '';
  end
  else Result := '';
end;

{*******************************************************************}
procedure TALHttpClientResponseW.SetBodyString(const AValue: String);
begin
  if FBodyStream <> nil then begin
    if not OwnsBodyStream then begin
      FBodyStream.Position := 0;
      if Length(AValue) > 0 then begin
        var LEncoding: TEncoding;
        var LFreeEncoding: boolean;
        var LCharset := Headers.ContentCharset;
        if (LCharSet = '') or (ALSameTextW(LCharSet, 'utf-8')) then begin
          LEncoding := TEncoding.UTF8;
          LFreeEncoding := False;
        end
        else begin
          LEncoding := TEncoding.GetEncoding(LCharSet);
          LFreeEncoding := True;
        end;
        try
          var LBytes := LEncoding.GetBytes(AValue);
          FBodyStream.Size := Length(LBytes);
          if Length(LBytes) > 0 then
            FBodyStream.WriteBuffer(LBytes[0], Length(LBytes));
        finally
          if LFreeEncoding then
            ALFreeAndNil(LEncoding);
        end;
      end
      else
        FBodyStream.Size := 0;
      Exit;
    end
    else
      ALFreeAndNil(FBodyStream);
  end;

  var LCharset := Headers.ContentCharset;
  if (LCharSet = '') or (ALSameTextW(LCharSet, 'utf-8')) then
    FBodyStream := TALStringStreamW.Create(AValue, TEncoding.UTF8, False{AOwnsEncoding})
  else
    FBodyStream := TALStringStreamW.Create(AValue, TEncoding.GetEncoding(LCharSet), True{AOwnsEncoding});
end;

{*******************************************}
constructor TALHttpClientProxyParamsA.Create;
Begin
  inherited create;
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  FOnchange := nil;
end;

{****************************************}
procedure TALHttpClientProxyParamsA.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{*******************************************************************}
procedure TALHttpClientProxyParamsA.DoChange(propertyIndex: Integer);
begin
  if assigned(FOnChange) then
    FOnChange(Self,propertyIndex);
end;

{**************************************************************************}
procedure TALHttpClientProxyParamsA.SetProxyBypass(const Value: AnsiString);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{****************************************************************************}
procedure TALHttpClientProxyParamsA.SetProxyPassword(const Value: AnsiString);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{*********************************************************************}
procedure TALHttpClientProxyParamsA.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{**************************************************************************}
procedure TALHttpClientProxyParamsA.SetProxyServer(const Value: AnsiString);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{****************************************************************************}
procedure TALHttpClientProxyParamsA.SetProxyUserName(const Value: AnsiString);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

{*******************************************}
constructor TALHttpClientProxyParamsW.Create;
Begin
  inherited create;
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  FOnchange := nil;
end;

{****************************************}
procedure TALHttpClientProxyParamsW.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{*******************************************************************}
procedure TALHttpClientProxyParamsW.DoChange(propertyIndex: Integer);
begin
  if assigned(FOnChange) then
    FOnChange(Self,propertyIndex);
end;

{**********************************************************************}
procedure TALHttpClientProxyParamsW.SetProxyBypass(const Value: String);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{************************************************************************}
procedure TALHttpClientProxyParamsW.SetProxyPassword(const Value: String);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{*********************************************************************}
procedure TALHttpClientProxyParamsW.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{**********************************************************************}
procedure TALHttpClientProxyParamsW.SetProxyServer(const Value: String);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{************************************************************************}
procedure TALHttpClientProxyParamsW.SetProxyUserName(const Value: String);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

{********************************}
constructor TALHttpClientA.Create;
begin
  inherited;
  FProxyParams := TALHttpClientProxyParamsA.Create;
  FProxyParams.OnChange := OnProxyParamsChange;
  FRequestHeaders := TALHttpClientRequestHeadersA.Create;
  FRequestHeaders.UserAgent := 'ALHTTPClient/1.0';
  FProtocolVersion := TALHTTPVersion.v1_1;
  FUserName := '';
  FPassword := '';
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
  FOnUploadProgress := nil;
  FOnDownloadProgress := nil;
  FOnRedirect := nil;
end;

{********************************}
destructor TALHttpClientA.Destroy;
begin
  AlFreeAndNil(FProxyParams);
  AlFreeAndNil(FRequestHeaders);
  inherited;
end;

{*************************************************************}
procedure TALHttpClientA.SetUsername(const AValue: AnsiString);
begin
  FUserName := AValue;
end;

{*************************************************************}
procedure TALHttpClientA.SetPassword(const AValue: AnsiString);
begin
  FPassword := AValue;
end;

{******************************************************************************************}
procedure TALHttpClientA.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  //virtual
end;

{******************************************************************}
procedure TALHttpClientA.SetOnRedirect(const Value: TRedirectEvent);
begin
  FOnRedirect := Value;
end;

{********************************************}
function TALHttpClientA.ExecuteFormUrlEncoded(
           const AUrl: AnsiString;
           const AVerb: AnsiString;
           const AFormParams: TALStringsA;
           Const AEncodeParams: Boolean;
           const AAddParamsToUrl: Boolean;
           const AHeaders: TALNameValueArrayA;
           const AResponseBodyStream: TStream): TALHttpClientResponseA;
Begin
  Var LLength: Integer := 0;
  if AAddParamsToUrl then LLength := LLength + length(AUrl) + 1{?};
  If AFormParams is TALNVStringListA then begin
    For var I := 0 to AFormParams.Count - 1 do
      LLength := LLength + Length(AFormParams.Names[I]) + Length(AFormParams.ValueFromIndex[I]) + 1{NameValueSeparator};
  end
  else begin
    For var I := 0 to AFormParams.Count - 1 do
      LLength := LLength + Length(AFormParams[i]);
  end;
  if AEncodeParams then LLength := LLength * 3;
  var SB := TALStringBuilderA.Create(LLength);
  try

    if AAddParamsToUrl then
      SB.Append(AUrl);

    if AFormParams.Count <> 0 then begin
      if AAddParamsToUrl then SB.Append('?');
      If AEncodeParams then begin
        for var I := 0 to AFormParams.Count - 1 do begin
          if I > 0 then SB.Append('&');
          if AFormParams.ItemHasNameValue(I) then begin
            SB.Append(ALUrlEncode(AFormParams.Names[I], True{ASpacesAsPlus}));
            SB.Append('=');
            SB.Append(ALUrlEncode(AFormParams.ValueFromIndex[I], True{ASpacesAsPlus}));
          end
          else
            SB.Append(ALUrlEncode(AFormParams[I], True{ASpacesAsPlus}));
        end;
      end
      else begin
        var LPrevNameValueSeparator := AFormParams.NameValueSeparator;
        var LPrevLineBreak := AFormParams.LineBreak;
        var LPrevTrailingLineBreak := AFormParams.TrailingLineBreak;
        AFormParams.NameValueSeparator := '=';
        AFormParams.LineBreak := '&';
        AFormParams.TrailingLineBreak := False;
        try
          SB.Append(AFormParams.Text);
        finally
          AFormParams.NameValueSeparator := LPrevNameValueSeparator;
          AFormParams.LineBreak := LPrevLineBreak;
          AFormParams.TrailingLineBreak := LPrevTrailingLineBreak;
        end;
      end;
    end;

    if AAddParamsToUrl then begin
      Result := Execute(
                  SB.ToString(true{AUpdateCapacity}), // const AUrl: AnsiString;
                  AVerb, // const AVerb: AnsiString;
                  nil, // const ABodyStream: TStream;
                  AHeaders, // const AHeaders: TALNameValueArrayA
                  AResponseBodyStream); // const AResponseBodyStream: TStream)
    end
    else begin
      var LURLEncodedBodyStream := TALStringStreamA.create(SB.ToString(true{AUpdateCapacity}));
      var LPrevContentType := FRequestHeaders.ContentType;
      Try
        FRequestHeaders.ContentType := 'application/x-www-form-urlencoded';
        Result := Execute(
                    AUrl, // const AUrl: AnsiString;
                    AVerb, // const AVerb: AnsiString;
                    LURLEncodedBodyStream, // const ABodyStream: TStream;
                    AHeaders, // const AHeaders: TALNameValueArrayA
                    AResponseBodyStream); // const AResponseBodyStream: TStream)
      finally
        AlFreeAndNil(LURLEncodedBodyStream);
        FRequestHeaders.ContentType := LPrevContentType;
      end;
    end;

  finally
    ALFreeAndNil(SB);
  end;
End;

{***********************************************}
function TALHttpClientA.ExecuteMultipartFormData(
           const AUrl: AnsiString;
           const AVerb: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA;
           const AResponseBodyStream: TStream): TALHttpClientResponseA;
Begin
  var LPrevContentType := FRequestHeaders.ContentType;
  try
    FRequestHeaders.ContentType := AFormDataEncoder.MimeTypeHeader;
    Result := Execute(
                AUrl, // const AUrl: AnsiString;
                AVerb, // const AVerb: AnsiString;
                AFormDataEncoder.Stream, // const ABodyStream: TStream;
                AHeaders, // const AHeaders: TALNameValueArrayA
                AResponseBodyStream); // const AResponseBodyStream: TStream)
  finally
    FRequestHeaders.ContentType := LPrevContentType;
  end;
End;

{**************************}
function TALHttpClientA.Get(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'GET', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{**************************}
function TALHttpClientA.Get(
           const AUrl: AnsiString;
           const AQueryParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteFormUrlEncoded(
              AUrl, // const AUrl: AnsiString;
              'GET', // const AVerb: AnsiString;
              AQueryParams, // const AFormParams: TALStringsA;
              AEncodeParams, // Const AEncodeParams: Boolean;
              True, // const AAddParamsToUrl: Boolean;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{***************************}
function TALHttpClientA.Post(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'POST', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*****************************************}
function TALHttpClientA.PostFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteFormUrlEncoded(
              AUrl, // const AUrl: AnsiString;
              'POST', // const AVerb: AnsiString;
              AFormParams, // const AFormParams: TALStringsA;
              AEncodeParams, // Const AEncodeParams: Boolean;
              False, // const AAddParamsToUrl: Boolean;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{********************************************}
function TALHttpClientA.PostMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'POST', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{***************************}
function TALHttpClientA.Head(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'HEAD', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{****************************}
function TALHttpClientA.Trace(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'TRACE', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{**************************}
function TALHttpClientA.Put(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'PUT', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{****************************************}
function TALHttpClientA.PutFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteFormUrlEncoded(
              AUrl, // const AUrl: AnsiString;
              'PUT', // const AVerb: AnsiString;
              AFormParams, // const AFormParams: TALStringsA;
              AEncodeParams, // Const AEncodeParams: Boolean;
              False, // const AAddParamsToUrl: Boolean;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*******************************************}
function TALHttpClientA.PutMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'PUT', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{****************************}
function TALHttpClientA.Patch(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'PATCH', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{******************************************}
function TALHttpClientA.PatchFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteFormUrlEncoded(
              AUrl, // const AUrl: AnsiString;
              'PATCH', // const AVerb: AnsiString;
              AFormParams, // const AFormParams: TALStringsA;
              AEncodeParams, // Const AEncodeParams: Boolean;
              False, // const AAddParamsToUrl: Boolean;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*********************************************}
function TALHttpClientA.PatchMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'PATCH', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*****************************}
function TALHttpClientA.Delete(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'DELETE', // const AVerb: AnsiString;
              ABodyStream,  // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{******************************}
function TALHttpClientA.Options(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHttpClientResponseA;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'OPTIONS', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

end.