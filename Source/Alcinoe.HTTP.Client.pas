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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpClientRequestHeaders = Class(TALHTTPRequestHeaders)
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
  TALHttpClientResponseHeaders = Class(TALHTTPResponseHeaders)
  private
    FKnownHeaders: array[0..28] of AnsiString;
    FCookies: TObjectList<TALHTTPCookie>;
    FUnknownHeaders: TALStringsA;
  protected
    function GetCookies: TObjectList<TALHTTPCookie>; override;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHTTPClientResponse = Class(TALHTTPResponse)
  private
    FVersion: TALHttpVersion;
    FStatusCode: Integer;
    FReason: AnsiString;
    FHeaders: TALHttpClientResponseHeaders;
    FBodyStream: TStream;
    FOwnsBodyStream: Boolean;
  protected
    function GetVersion: TALHttpVersion; override;
    procedure SetVersion(const AValue: TALHttpVersion); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const AValue: Integer); override;
    function GetReason: AnsiString; override;
    procedure SetReason(const AValue: AnsiString); override;
    function GetHeaders: TALHTTPResponseHeaders; override;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHTTPClientProxyParams = Class(Tobject)
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHTTPClient = class(TObject)
  public
    type
      TRedirectEvent = procedure(Sender: Tobject; const NewURL: AnsiString) of object;
      TUploadProgressEvent = procedure(Sender: Tobject; Sent: Int64; Total: Int64) of object;
      TDownloadProgressEvent = procedure(Sender: Tobject; Read: Int64; Total: Int64) of object;
  private
    FProxyParams: TALHTTPClientProxyParams;
    FRequestHeaders: TALHttpClientRequestHeaders;
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
               const AResponseBodyStream: TStream): TALHTTPClientResponse; virtual; abstract;
    function ExecuteFormUrlEncoded(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const AFormParams: TALStringsA;
               Const AEncodeParams: Boolean;
               const AAddParamsToUrl: Boolean;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHTTPClientResponse;
    function ExecuteMultipartFormData(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHTTPClientResponse;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Get(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse; overload;
    function Get(
               const AUrl: AnsiString;
               const AQueryParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse; overload;
    function Post(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PostFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PostMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Head(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Trace(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Put(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PutFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PutMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Patch(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PatchFormUrlEncoded(
               const AUrl: AnsiString;
               const AFormParams: TALStringsA;
               const AHeaders: TALNameValueArrayA = nil;
               Const AEncodeParams: Boolean=True;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function PatchMultipartFormData(
               const AUrl: AnsiString;
               const AFormDataEncoder: TALMultipartFormDataEncoderA;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Delete(
               const AUrl: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    function Options(
               const AUrl: AnsiString;
               const AHeaders: TALNameValueArrayA = nil;
               const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property ProxyParams: TALHTTPClientProxyParams read FProxyParams;
    property RequestHeaders: TALHttpClientRequestHeaders read FRequestHeaders;
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

{**************************************}
constructor TALHttpClientRequestHeaders.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{**************************************}
destructor TALHttpClientRequestHeaders.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{***********************************}
procedure TALHttpClientRequestHeaders.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{***********************************************************}
function TALHttpClientRequestHeaders.GetCookies: TALStringsA;
begin
  if FCookies = nil then begin
    FCookies := TALNVStringListA.Create;
    FCookies.LineBreak := '; ';
    FCookies.IncludeTrailingLineBreakInText := False;
  end;
  Result := FCookies;
end;

{******************************************************************}
function TALHttpClientRequestHeaders.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.IncludeTrailingLineBreakInText := False;
  end;
  Result := FUnknownHeaders;
end;

{********************************************************************************************}
function TALHttpClientRequestHeaders.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{**********************************************************************************************************}
procedure TALHttpClientRequestHeaders.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{*********************************************************}
Function TALHttpClientRequestHeaders.GetRawHeaderText: AnsiString;
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

{********************************************************************************}
procedure TALHttpClientRequestHeaders.SetRawHeaderText(const ARawHeaderText: AnsiString);
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

{**************************************}
constructor TALHttpClientResponseHeaders.Create;
Begin
  inherited;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{**************************************}
destructor TALHttpClientResponseHeaders.Destroy;
begin
  AlFreeAndNil(Fcookies);
  AlFreeAndNil(FUnknownHeaders);
  inherited;
end;

{***********************************}
procedure TALHttpClientResponseHeaders.Clear;
begin
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
end;

{***************************************************************************}
function TALHttpClientResponseHeaders.GetCookies: TObjectList<TALHTTPCookie>;
begin
  if FCookies = nil then
    FCookies := TObjectList<TALHTTPCookie>.Create(True{AOwnsObjects});
  Result := FCookies;
end;

{******************************************************************}
function TALHttpClientResponseHeaders.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.IncludeTrailingLineBreakInText := False;
  end;
  Result := FUnknownHeaders;
end;

{********************************************************************************************}
function TALHttpClientResponseHeaders.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  Result := FKnownHeaders[Index];
end;

{**********************************************************************************************************}
procedure TALHttpClientResponseHeaders.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  {$IF defined(DEBUG)}
  if (Index < Low(FKnownHeaders)) or (Index > High(FKnownHeaders)) then
    raise EArgumentOutOfRangeException.CreateFmt('Header index (%d) out of range [0..%d]', [Index, High(FKnownHeaders)]);
  {$ENDIF}
  FKnownHeaders[Index] := Value;
end;

{*********************************************************}
Function TALHttpClientResponseHeaders.GetRawHeaderText: AnsiString;
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

{********************************************************************************}
procedure TALHttpClientResponseHeaders.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  Clear;
  var LRawHeaders := TALNVStringListA.create;
  try
    LRawHeaders.NameValueSeparator := ':';
    LRawHeaders.Text := ARawHeaderText;
    For var I := 0 to LRawHeaders.count - 1 do begin
      var LName := ALTrim(LRawHeaders.Names[I]);
      If ALSameTextA(LName, 'Set-Cookie') then begin
        Var LCookie := TALHttpCookie.Create;
        LCookie.HeaderValue := alTrim(LRawHeaders.ValueFromIndex[I]);
        Cookies.Add(LCookie);
      end
      else Values[LName] := alTrim(LRawHeaders.ValueFromIndex[I]);
    end;
  finally
    AlFreeAndNil(LRawHeaders);
  end;
end;

{*****************************************}
constructor TALHttpClientResponse.Create;
begin
  inherited;
  FVersion := TALHttpVersion.v1_1;
  FStatusCode := 200;
  FReason := 'OK';
  FHeaders := nil;
  FBodyStream := nil;
  FOwnsBodyStream := True;
end;

{*****************************************}
destructor TALHttpClientResponse.Destroy;
begin
  ALFreeAndNil(FHeaders);
  if OwnsBodyStream then ALFreeAndNil(FBodyStream);
  inherited;
end;

{*****************************************}
function TALHttpClientResponse.GetVersion: TALHttpVersion;
begin
  Result := FVersion;
end;

{*****************************************}
procedure TALHttpClientResponse.SetVersion(const AValue: TALHttpVersion);
begin
  FVersion := AValue;
end;

{*****************************************}
function TALHttpClientResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

{*****************************************}
procedure TALHttpClientResponse.SetStatusCode(const AValue: Integer);
begin
  If AValue <> FStatusCode then begin
    FStatusCode := AValue;
    Reason := ALGetHttpReasonPhraseA(FStatusCode);
  end;
end;

{*****************************************}
function TALHttpClientResponse.GetReason: AnsiString;
begin
  Result := FReason;
end;

{*****************************************}
procedure TALHttpClientResponse.SetReason(const AValue: AnsiString);
begin
  FReason := AValue;
end;

{*****************************************}
function TALHttpClientResponse.GetHeaders: TALHTTPResponseHeaders;
begin
  if FHeaders = nil then
    FHeaders := TALHttpClientResponseHeaders.Create;
  Result := FHeaders;
end;

{*****************************************}
function TALHttpClientResponse.GetBodyStream: TStream;
begin
  if FBodyStream = nil then
    FBodyStream := TALStringStreamA.Create('');
  Result := FBodyStream;
end;

{*****************************************}
procedure TALHttpClientResponse.SetBodyStream(const AValue: TStream);
begin
  if AValue <> FBodyStream then begin
    if OwnsBodyStream then ALFreeAndNil(FBodyStream);
    FBodyStream := AValue;
  end;
end;

{*****************************************}
function TALHttpClientResponse.GetOwnsBodyStream: Boolean;
begin
  Result := FOwnsBodyStream;
end;

{*****************************************}
procedure TALHttpClientResponse.SetOwnsBodyStream(const AValue: Boolean);
begin
  FOwnsBodyStream := AValue;
end;

{*****************************************}
function TALHttpClientResponse.GetBodyString: AnsiString;
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

{*****************************************}
procedure TALHttpClientResponse.SetBodyString(const AValue: AnsiString);
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

{******************************************}
constructor TALHTTPClientProxyParams.Create;
Begin
  inherited create;
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  FOnchange := nil;
end;

{***************************************}
procedure TALHTTPClientProxyParams.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{******************************************************************}
procedure TALHTTPClientProxyParams.DoChange(propertyIndex: Integer);
begin
  if assigned(FOnChange) then
    FOnChange(Self,propertyIndex);
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyBypass(const Value: AnsiString);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPassword(const Value: AnsiString);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyServer(const Value: AnsiString);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyUserName(const Value: AnsiString);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

{*******************************}
constructor TALHTTPClient.Create;
begin
  inherited;
  FProxyParams := TALHTTPClientProxyParams.Create;
  FProxyParams.OnChange := OnProxyParamsChange;
  FRequestHeaders := TALHttpClientRequestHeaders.Create;
  FRequestHeaders.Accept := '*/*';
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

{*******************************}
destructor TALHTTPClient.Destroy;
begin
  AlFreeAndNil(FProxyParams);
  AlFreeAndNil(FRequestHeaders);
  inherited;
end;

{***************************************************************}
procedure TALHTTPClient.SetUsername(const AValue: AnsiString);
begin
  FUserName := AValue;
end;

{*******************************************************************}
procedure TALHTTPClient.SetPassword(const AValue: AnsiString);
begin
  FPassword := AValue;
end;

{*****************************************************************************************}
procedure TALHTTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  //virtual
end;

{*****************************************************************************}
procedure TALHTTPClient.SetOnRedirect(const Value: TRedirectEvent);
begin
  FOnRedirect := Value;
end;

{**************************************}
function TALHTTPClient.ExecuteFormUrlEncoded(
           const AUrl: AnsiString;
           const AVerb: AnsiString;
           const AFormParams: TALStringsA;
           Const AEncodeParams: Boolean;
           const AAddParamsToUrl: Boolean;
           const AHeaders: TALNameValueArrayA;
           const AResponseBodyStream: TStream): TALHTTPClientResponse;
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
        var LPrevIncludeTrailingLineBreakInText := AFormParams.IncludeTrailingLineBreakInText;
        AFormParams.NameValueSeparator := '=';
        AFormParams.LineBreak := '&';
        AFormParams.IncludeTrailingLineBreakInText := False;
        try
          SB.Append(AFormParams.Text);
        finally
          AFormParams.NameValueSeparator := LPrevNameValueSeparator;
          AFormParams.LineBreak := LPrevLineBreak;
          AFormParams.IncludeTrailingLineBreakInText := LPrevIncludeTrailingLineBreakInText;
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

{**********************************************}
function TALHTTPClient.ExecuteMultipartFormData(
           const AUrl: AnsiString;
           const AVerb: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA;
           const AResponseBodyStream: TStream): TALHTTPClientResponse;
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

{*************************}
function TALHTTPClient.Get(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'GET', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*************************}
function TALHTTPClient.Get(
           const AUrl: AnsiString;
           const AQueryParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
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

{**************************}
function TALHTTPClient.Post(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'POST', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{************************************}
function TALHTTPClient.PostFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
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

{*******************************************}
function TALHTTPClient.PostMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'POST', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{**************************}
function TALHTTPClient.Head(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'HEAD', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{***************************}
function TALHTTPClient.Trace(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'TRACE', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*************************}
function TALHTTPClient.Put(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'PUT', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{************************************}
function TALHTTPClient.PutFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
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
function TALHTTPClient.PutMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'PUT', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*************************}
function TALHTTPClient.Patch(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'PATCH', // const AVerb: AnsiString;
              ABodyStream, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{************************************}
function TALHTTPClient.PatchFormUrlEncoded(
           const AUrl: AnsiString;
           const AFormParams: TALStringsA;
           const AHeaders: TALNameValueArrayA = nil;
           Const AEncodeParams: Boolean=True;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
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

{*******************************************}
function TALHTTPClient.PatchMultipartFormData(
           const AUrl: AnsiString;
           const AFormDataEncoder: TALMultipartFormDataEncoderA;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := ExecuteMultipartFormData(
              AUrl, // const AUrl: AnsiString;
              'PATCH', // const AVerb: AnsiString;
              AFormDataEncoder, // const AFormDataEncoder: TALMultipartFormDataEncoderA;
              AHeaders, // const AHeaders: TALNameValueArrayA;
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{****************************}
function TALHTTPClient.Delete(
           const AUrl: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'DELETE', // const AVerb: AnsiString;
              ABodyStream,  // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

{*****************************}
function TALHTTPClient.Options(
           const AUrl: AnsiString;
           const AHeaders: TALNameValueArrayA = nil;
           const AResponseBodyStream: TStream = nil): TALHTTPClientResponse;
begin
  Result := Execute(
              AUrl, // const AUrl: AnsiString;
              'OPTIONS', // const AVerb: AnsiString;
              nil, // const ABodyStream: TStream;
              AHeaders, // const AHeaders: TALNameValueArrayA
              AResponseBodyStream); // const AResponseBodyStream: TStream)
end;

end.
