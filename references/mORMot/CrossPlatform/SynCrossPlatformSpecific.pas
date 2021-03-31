/// system-specific cross-platform units
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformSpecific;

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
  - danielkuettner
  - Stefan (itSDS)

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


  Each operating system will have its own API calls in this single unit
  Should compile with Delphi for any platform (including NextGen for mobiles),
    with FPC 2.7 or Kylix, and with SmartMobileStudio 2.2

}

{$ifdef DWSCRIPT} // always defined since SMS 1.1.2
  {$define ISDWS}           // e.g. for SmartMobileStudio or Delphi Web Script
  {$define ISSMS}           // for SmartMobileStudio
  {$define HASINLINE}
{$else}           // Delphi or FPC: select a single USE* conditional
  {$I SynCrossPlatform.inc} // define e.g. HASINLINE
  {$ifdef MSWINDOWS}
    {$ifdef FPC}
      {$define USESYNCRT}       // sounds to be the best choice under Windows
      {.$define USEFCL}         // for debugging the FCL within Lazarus
    {$else}
      {$define USESYNCRT}       // sounds to be the best choice under Windows
      {.$define USEINDY}        // for debugging Indy within Delphi
      {.$define USEHTTPCLIENT}  // for debugging XE8+ HttpClient within Delphi
    {$endif}
    {$define USECRITICALSECTION}
  {$else}
    {$ifdef FPC}
      {$define USEFCL}
      {$define USECRITICALSECTION}
    {$else}
      {$ifdef ISDELPHIXE8}     // use new XE8+ System.Net.HttpClient
        {$ifdef ANDROID}
          {$define USEHTTPCLIENT}
          {.$define USEINDY}    // for debugging Indy within Android
        {$else}
          {$define USEINDY} // HttpClient has still issues with https under iOS
        {$endif ANDROID}
      {$else}
        {$define USEINDY}
      {$endif ISDELPHIXE8}
    {$endif FPC}
  {$endif MSWINDOWS}
{$endif}

interface

{$ifdef ISDWS}
uses
  SmartCL.System,
  System.Types,
  ECMA.Date,
  ECMA.Json;
{$else}
uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$endif}
  SysUtils,
  Classes;
{$endif}

type
  {$ifdef ISDWS}
  JDateHelper = helper for JDate
  private
    function GetAsDateTime : TDateTime;
    function GetAsLocalDateTime : TDateTime;
    procedure SetAsDateTime(dt : TDateTime);
    procedure SetAsLocalDateTime(dt : TDateTime);
  public
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsLocalDateTime : TDateTime read GetAsLocalDateTime write SetAsLocalDateTime;
  end;
  // HTTP body may not match the string type, and could be binary
  THttpBody = string;

  // define some Delphi types not supported natively by DWS/SMS
  char = string;
  byte = integer;
  word = integer;
  cardinal = integer;
  // warning: JavaScript truncates integer to its mantissa resolution + sign!
  Int53 = integer;
  Int64 = integer;
  currency = float;
  TPersistent = TObject;
  TObjectList = array of TObject;
  TStringList = array of string;
  TVariantDynArray = array of variant;
  TIntegerDynArray = array of integer;

  // as defined in SmartCL.Inet and expected by XMLHttpRequest
  THttpRequestReadyState = (rrsUnsent          = 0,
                            rrsOpened          = 1,
                            rrsHeadersReceived = 2,
                            rrsLoading         = 3,
                            rrsDone            = 4);
  {$else}

  /// will store input and output HTTP body content
  // - HTTP body may not match the string type, and could be binary
  // - this kind of variable is compatible with NextGen version of the compiler
  THttpBody = array of byte;

  /// cross-platform thread safe locking
  // - will use TMonitor on the newest Delphi platforms
  TMutex = class
  {$ifdef USECRITICALSECTION}
  protected
    fLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
  {$endif}
  public
    procedure Enter;
    procedure Leave;
  end;

  {$ifdef NEXTGEN}
  /// see TUTF8Buffer = TBytes in SynCrossPlatformJSON
  AnsiChar = byte;
  {$endif NEXTGEN}

  {$endif ISDWS}

  /// used to store the request of a REST call
  {$ifdef USEOBJECTINSTEADOFRECORD}
  TSQLRestURIParams = object
  {$else}
  TSQLRestURIParams = record
  {$endif}
    /// input parameter containing the caller URI
    Url: string;
    /// caller URI, without any appended signature
    UrlWithoutSignature: string;
    /// input parameter containing the caller method
    Verb: string;
    /// input parameter containing the caller message headers
    InHead: string;
    /// input parameter containing the caller message body
    InBody: THttpBody;
    /// output parameter to be set to the response message header
    OutHead: string;
    /// output parameter to be set to the response message body
    OutBody: THttpBody;
    /// output parameter to be set to the HTTP status integer code
    OutStatus: cardinal;
    {$ifdef ISDWS}
    /// the associated TXMLHttpRequest instance
    XHR: THandle;
    /// callback events for asynchronous call
    // - will be affected to the corresponding XHR events
    OnSuccess: TProcedureRef;
    OnError: TProcedureRef;
    {$endif}
    /// set the caller content
    procedure Init(const aUrl,aVerb,aUTF8Body: string);
    /// get the response message body as UTF-8
    function OutBodyUtf8: string;
  end;

  /// the connection parameters, as stored and used by TAbstractHttpConnection
  TSQLRestConnectionParams = record
    /// the server name or IP address
    Server: string;
    /// the server port
    Port: integer;
    /// if the connection should be HTTPS
    Https: boolean;
    {$ifndef ISSMS}
    /// the optional proxy name to be used
    ProxyName: string;
    /// the optional proxy password to be used
    ProxyByPass: string;
    /// the connection timeout, in ms
    ConnectionTimeOut: integer;
    /// the timeout when sending data, in ms
    SendTimeout: cardinal;
    /// the timeout when receiving data, in ms
    ReceiveTimeout: cardinal
    {$endif}
  end;

  /// abstract class for HTTP client connection
  TAbstractHttpConnection = class
  protected
    fParameters: TSQLRestConnectionParams;
    fURL: string;
    fOpaqueConnection: TObject; 
  public
    /// this is the main entry point for all HTTP clients
    // - connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    constructor Create(const aParameters: TSQLRestConnectionParams); virtual;
    /// perform the request
    // - this is the main entry point of this class
    // - inherited classes should override this abstract method
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); virtual; abstract;

    /// the remote server full URI
    // - e.g. 'http://myserver:888/'
    property Server: string read fURL;
    /// the connection parameters
    property Parameters: TSQLRestConnectionParams read fParameters;
    /// opaque access to the effective connection class instance
    // - which may be a TFPHttpClient, a TIdHTTP or a TWinHttpAPI
    property ActualConnection: TObject read fOpaqueConnection;
  end;
   
  /// define the inherited class for HTTP client connection
  TAbstractHttpConnectionClass = class of TAbstractHttpConnection;


const
  /// MIME content type used for JSON communication
  JSON_CONTENT_TYPE = 'application/json; charset=UTF-8';

  /// HTTP Status Code for "Continue"
  HTTP_CONTINUE = 100;
  /// HTTP Status Code for "Switching Protocols"
  HTTP_SWITCHINGPROTOCOLS = 101;
  /// HTTP Status Code for "Success"
  HTTP_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  HTTP_CREATED = 201;
  /// HTTP Status Code for "Accepted"
  HTTP_ACCEPTED = 202;
  /// HTTP Status Code for "Non-Authoritative Information"
  HTTP_NONAUTHORIZEDINFO = 203;
  /// HTTP Status Code for "No Content"
  HTTP_NOCONTENT = 204;
  /// HTTP Status Code for "Partial Content"
  HTTP_PARTIALCONTENT = 206;
  /// HTTP Status Code for "Multiple Choices"
  HTTP_MULTIPLECHOICES = 300;
  /// HTTP Status Code for "Moved Permanently"
  HTTP_MOVEDPERMANENTLY = 301;
  /// HTTP Status Code for "Found"
  HTTP_FOUND = 302;
  /// HTTP Status Code for "See Other"
  HTTP_SEEOTHER = 303;
  /// HTTP Status Code for "Not Modified"
  HTTP_NOTMODIFIED = 304;
  /// HTTP Status Code for "Use Proxy"
  HTTP_USEPROXY = 305;
  /// HTTP Status Code for "Temporary Redirect"
  HTTP_TEMPORARYREDIRECT = 307;
  /// HTTP Status Code for "Bad Request"
  HTTP_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  HTTP_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  HTTP_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  HTTP_NOTFOUND = 404;
  // HTTP Status Code for "Method Not Allowed"
  HTTP_NOTALLOWED = 405;
  // HTTP Status Code for "Not Acceptable"
  HTTP_NOTACCEPTABLE = 406;
  // HTTP Status Code for "Proxy Authentication Required"
  HTTP_PROXYAUTHREQUIRED = 407;
  /// HTTP Status Code for "Request Time-out"
  HTTP_TIMEOUT = 408;
  /// HTTP Status Code for "Internal Server Error"
  HTTP_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  HTTP_NOTIMPLEMENTED = 501;
  /// HTTP Status Code for "Bad Gateway"
  HTTP_BADGATEWAY = 502;
  /// HTTP Status Code for "Service Unavailable"
  HTTP_UNAVAILABLE = 503;
  /// HTTP Status Code for "Gateway Timeout"
  HTTP_GATEWAYTIMEOUT = 504;
  /// HTTP Status Code for "HTTP Version Not Supported"
  HTTP_HTTPVERSIONNONSUPPORTED = 505;


/// gives access to the class type to implement a HTTP connection
// - will use WinHTTP API (from our SynCrtSock) under Windows
// - will use Indy for Delphi on other platforms
// - will use fcl-web (fphttpclient) with FreePascal
function HttpConnectionClass: TAbstractHttpConnectionClass;

  
/// convert a text into UTF-8 binary buffer
function TextToHttpBody(const Text: string): THttpBody;

/// convert a UTF-8 binary buffer into texts
procedure HttpBodyToText(const Body: THttpBody; var Text: string);

/// will return the next CSV value from the supplied text 
function GetNextCSV(const str: string; var index: Integer; var res: string;
  Sep: char=','; resultTrim: boolean=false): boolean;

{$ifdef ISDWS}
// some definitions implemented in SynCrossPlatformJSON.pas for Delphi+FPC

procedure DoubleQuoteStr(var text: string);
function IdemPropName(const PropName1,PropName2: string): boolean;
function StartWithPropName(const PropName1,PropName2: string): boolean;
function VarRecToValue(const VarRec: variant; var tmpIsString: boolean): string;
procedure DecodeTime(Value: TDateTime; var HH,MM,SS,MS: word);
procedure DecodeDate(Value: TDateTime; var Y,M,D: word);
function TryEncodeDate(Y,M,D: integer; UTC: DateTimeZone; var Value: TDateTime): boolean;
function TryEncodeTime(HH,MM,SS,MS: integer; var Value: TDateTIme): boolean;
function NowToIso8601: string;
function DateTimeToIso8601(Value: TDateTime): string;
function Iso8601ToDateTime(const Value: string): TDateTime;
function TryStrToInt(const S: string; var Value: integer): Boolean;
function TryStrToInt64(const S: string; var Value: Int64): Boolean;
function StrToInt64Def(const S: string; const def: Int64): Int64;
function UpCase(ch: Char): Char; inline;

type
  /// which kind of document the TJSONVariantData contains
  TJSONVariantKind = (jvUndefined, jvObject, jvArray);

  /// stores any JSON object or array as variant
  TJSONVariantData = class
  public
    Kind: TJSONVariantKind;
    Names: TStrArray;
    Values: TVariantDynArray;
    /// initialize the low-level memory structure with a given JSON content
    constructor Create(const aJSON: string);
    /// initialize the low-level memory structure with a given object
    constructor CreateFrom(const document: variant);
    /// number of items in this jvObject or jvArray
    property Count: integer read (Values.Count);
  end;

/// guess the type of a supplied variant
function VariantType(const Value: variant): TJSONVariantKind;

/// faster than chr(c) when you are sure that c<=$ffff
function DirectChr(c: Integer): string; external 'String.fromCharCode';

/// compute the JSON representation of a variant value
// - match function signature as defined in SynCrossPlatformJSON
function ValueToJSON(Value: variant): string; external 'JSON.stringify';

/// compute a variant from its JSON representation
// - match function signature as defined in SynCrossPlatformJSON
function JSONToValue(JSON: string): variant; external 'JSON.parse';

{$endif}


implementation

{$ifdef USEFCL}
uses
  fphttpclient;
{$endif}

{$ifdef USEINDY}
uses
  IdHTTP, IdCoderMIME,
  {$ifdef MACOS}
  {$ifdef CPUARM}
  IdSSLOpenSSLHeaders_Static, // for iOS ARM
  {$else}
  IdSSLOpenSSLHeaders,        // for OSX and iOS x86
  {$endif}
  {$endif}
  IdSSLOpenSSL;
  // for SSL support with iOS and Android client, please follow instructions at
  // http://blog.marcocantu.com/blog/using_ssl_delphi_ios.html and you may
  // download the *.a files from http://indy.fulgan.com/SSL/OpenSSLStaticLibs.7z
  // see also https://synopse.info/forum/viewtopic.php?id=2325
{$endif}

{$ifdef USESYNCRT}
uses
  SynCrtSock;
{$endif}

{$ifdef USEHTTPCLIENT}
uses
  System.Net.UrlClient,
  System.Net.HttpClient;
{$endif}

{$ifdef ISDWS}
function JDateHelper.GetAsDateTime : TDateTime;
begin
  Result := Self.getTime / 864e5 + 25569;
end;

procedure JDateHelper.SetAsDateTime(dt : TDateTime);
begin
  Self.setTime(round((dt - 25569) * 864e5));
end;

function JDateHelper.GetAsLocalDateTime: TDateTime;
begin
  Result := (Self.getTime - 60000 * Self.getTimezoneOffset) / 864e5 + 25569;
end;

procedure JDateHelper.SetAsLocalDateTime(dt: TDateTime);
begin
  Self.setTime(round((dt - 25569) * 864e5) + 60000 * Self.getTimezoneOffset);
end;
{$endif}

function TextToHttpBody(const Text: string): THttpBody;
{$ifdef ISSMS}
begin
  // http://ecmanaut.blogspot.fr/2006/07/encoding-decoding-utf8-in-javascript.html
  asm
    @result=unescape(encodeURIComponent(@Text));
  end;
end;
{$else}
{$ifdef NEXTGEN}
begin
  result := THttpBody(TEncoding.UTF8.GetBytes(Text));
end;
{$else}
var utf8: UTF8String;
    n: integer;
begin
  utf8 := UTF8Encode(Text);
  n := length(utf8);
  SetLength(result,n);
  move(pointer(utf8)^,pointer(result)^,n);
end;
{$endif}
{$endif}

function GetNextCSV(const str: string; var index: Integer; var res: string;
  Sep: char=','; resultTrim: boolean=false): boolean;
var i,j,L: integer;
begin
  L := length(str);
  if index<=L then begin
    i := index;
    while i<=L do
      if str[i]=Sep then
        break else
        inc(i);
    j := index;
    index := i+1;
    if resultTrim then begin
      while (j<L) and (ord(str[j])<=32) do inc(j);
      while (i>j) and (ord(str[i-1])<=32) do dec(i);
    end;
    res := copy(str,j,i-j);
    result := true;
  end else
    result := false;
end;

procedure HttpBodyToText(const Body: THttpBody; var Text: string);
{$ifdef ISSMS}
begin
  asm
    @Text=decodeURIComponent(escape(@Body));
  end;
end;
{$else}
{$ifdef NEXTGEN}
begin
  Text := TEncoding.UTF8.GetString(TBytes(Body));
end;
{$else}
var utf8: UTF8String;
    L: integer;
begin
  L := length(Body);
  SetLength(utf8,L);
  move(pointer(Body)^,pointer(utf8)^,L);
  {$ifdef UNICODE}
  Text := UTF8ToString(utf8);
  {$else}
  Text := UTF8Decode(utf8);
  {$endif}
end;
{$endif}
{$endif}


{ TAbstractHttpConnection }

const
  INTERNET_DEFAULT_HTTP_PORT = 80;
  INTERNET_DEFAULT_HTTPS_PORT = 443; 

constructor TAbstractHttpConnection.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited Create;
  fParameters := aParameters;
  if fParameters.Port=0 then
    if fParameters.Https then
      fParameters.Port := INTERNET_DEFAULT_HTTPS_PORT else
      fParameters.Port := INTERNET_DEFAULT_HTTP_PORT;
  if fParameters.Https then
    fURL := 'https://' else
    fURL := 'http://';
  fURL := fURL+fParameters.Server+':'+IntToStr(fParameters.Port)+'/';
end;


{$ifdef USEFCL}

type
  TFclHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TFPHttpClient;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string; KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TFclHttpConnectionClass }

constructor TFclHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited Create(aParameters);
  fConnection := TFPHttpClient.Create(nil);
  fOpaqueConnection := fConnection;
end;

procedure TFclHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var InStr,OutStr: TBytesStream;
begin
  InStr := TBytesStream.Create(Call.InBody);
  OutStr := TBytesStream.Create;
  try
    fConnection.RequestHeaders.Text := Call.InHead;
    fConnection.RequestBody := InStr;
    fConnection.HTTPMethod(Call.Verb,fURL+Call.Url,OutStr,[]);
    Call.OutStatus := fConnection.ResponseStatusCode;
    Call.OutHead := fConnection.ResponseHeaders.Text;
    Call.OutBody := OutStr.Bytes;
    SetLength(Call.OutBody,OutStr.Position);
  finally
    OutStr.Free;
    InStr.Free;
  end;
end;

destructor TFclHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  inherited Destroy;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TFclHttpConnectionClass;
end;

{$endif}

{$ifdef USEINDY}

type
  TIndyHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TIdHTTP;
    fIOHandler: TIdSSLIOHandlerSocketOpenSSL; // here due to NextGen ARC model
    fLock : TMutex;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TIndyHttpConnectionClass }

constructor TIndyHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited;
  fLock := TMutex.Create;
  fConnection := TIdHTTP.Create(nil);
  fOpaqueConnection := fConnection;
  fConnection.UseNagle := False; 
  fConnection.HTTPOptions := fConnection.HTTPOptions+[hoKeepOrigProtocol];
  fConnection.ConnectTimeout := fParameters.ConnectionTimeOut;
  fConnection.ReadTimeout := fParameters.ReceiveTimeout;
  if fParameters.Https then begin
    fIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
    fConnection.IOHandler := fIOHandler;
  end;
  if fParameters.ProxyName<>'' then
    fConnection.ProxyParams.ProxyServer := fParameters.ProxyName;
end;

destructor TIndyHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  fIOHandler.Free;
  fLock.Free;
  inherited;
end;

procedure TIndyHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var InStr, OutStr: TStream;
    OutLen,i: integer;
    Auth: string;
begin
  fLock.Enter;
  try
    InStr := TMemoryStream.Create;
    OutStr := TMemoryStream.Create;
    try
      fConnection.Request.RawHeaders.Text := Call.InHead;
      Auth := fConnection.Request.RawHeaders.Values['Authorization'];
      if (Auth<>'') and SameText(Copy(Auth,1,6),'Basic ') then begin
        // see https://synopse.info/forum/viewtopic.php?pid=11761#p11761
        with TIdDecoderMIME.Create do
        try
          Auth := DecodeString(copy(Auth,7,maxInt));
        finally
          Free;
        end;
        i := Pos(':',Auth);
        if i>0 then begin
          fConnection.Request.BasicAuthentication := true;
          fConnection.Request.Username := copy(Auth,1,i-1);
          fConnection.Request.Password := Copy(Auth,i+1,maxInt);
        end;
      end;
      if Call.InBody<>nil then begin
        InStr.Write(Call.InBody[0],length(Call.InBody));
        InStr.Seek(0,soBeginning);
        fConnection.Request.Source := InStr;
      end;
      if Call.Verb='GET' then // allow 404 as valid Call.OutStatus
        fConnection.Get(fURL+Call.Url,OutStr,[HTTP_SUCCESS,HTTP_NOTFOUND]) else
      if Call.Verb='POST' then
        fConnection.Post(fURL+Call.Url,InStr,OutStr) else
      if Call.Verb='PUT' then
        fConnection.Put(fURL+Call.Url,InStr) else
      if Call.Verb='DELETE' then
        fConnection.Delete(fURL+Call.Url) else
        raise Exception.CreateFmt('Indy does not know method %s',[Call.Verb]);
      Call.OutStatus := fConnection.Response.ResponseCode;
      Call.OutHead := fConnection.Response.RawHeaders.Text;
      OutLen := OutStr.Size;
      if OutLen>0 then begin
        SetLength(Call.OutBody,OutLen);
        OutStr.Seek(0,soBeginning);
        OutStr.Read(Call.OutBody[0],OutLen);
      end;
    finally
      OutStr.Free;
      InStr.Free;
    end;
  finally
    fLock.Leave;
  end;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TIndyHttpConnectionClass;
end;
{$endif}

{$ifdef USEHTTPCLIENT}
type
  THttpClientHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: THttpClient;
    procedure DoValidateServerCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string; KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TFclHttpConnectionClass }

constructor THttpClientHttpConnectionClass.Create(const aParameters: TSQLRestConnectionParams);
begin
  inherited Create(aParameters);
  fConnection := THttpClient.Create;
  {$ifdef ISDELPHI102} // this basic settings are available only since Berlin!
  fConnection.ConnectionTimeout := aParameters.ConnectionTimeOut;
  fConnection.ResponseTimeout := aParameters.ReceiveTimeout;
  {$endif}
  fConnection.OnValidateServerCertificate := DoValidateServerCertificate;
  fOpaqueConnection := fConnection;
end;

function NetHeadersToText(const AHeaders: TNetHeaders): string;
var i: integer;
begin
  result := '';
  for i := 0 to High(AHeaders) do
    with AHeaders[i] do
      result := result+Name+': '+Value+#13#10;
end;

procedure THttpClientHttpConnectionClass.URI(var Call: TSQLRestURIParams; const InDataType: string; KeepAlive: integer);
var
  InStr, OutStr: TStream;
  OutLen: integer;
  LResponse : IHTTPResponse;
begin
  InStr := TMemoryStream.Create;
  OutStr := TMemoryStream.Create;
  try
    if Call.InBody<>nil then begin
      InStr.Write(Call.InBody[0],length(Call.InBody));
      InStr.Seek(0,soBeginning);
    end;
    LResponse := nil;
    if Call.Verb='GET' then // allow 404 as valid Call.OutStatus
      LResponse := fConnection.Get(fURL+Call.Url,OutStr)
    else if Call.Verb='POST' then
      LResponse := fConnection.Post(fURL+Call.Url,InStr,OutStr)
    else if Call.Verb='PUT' then
      LResponse := fConnection.Put(fURL+Call.Url,InStr)
    else if Call.Verb='DELETE' then
      LResponse := fConnection.Delete(fURL+Call.Url)
    else
      raise Exception.CreateFmt('Indy does not know method %s',[Call.Verb]);
    if LResponse <> nil then begin
      Call.OutStatus := LResponse.StatusCode;
      Call.OutHead := NetHeadersToText(LResponse.Headers);
      OutLen := OutStr.Size;
      if OutLen>0 then begin
        SetLength(Call.OutBody,OutLen);
        OutStr.Seek(0,soBeginning);
        OutStr.Read(Call.OutBody[0],OutLen);
      end;
    end;
  finally
    OutStr.Free;
    InStr.Free;
  end;
end;

destructor THttpClientHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  inherited Destroy;
end;

procedure THttpClientHttpConnectionClass.DoValidateServerCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const Certificate: TCertificate; var Accepted: Boolean);
begin
  Accepted := True;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := THttpClientHttpConnectionClass;
end;

{$endif}

{$ifdef USESYNCRT}
type
  TWinHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TWinHttpAPI;
    fLock: TRTLCriticalSection;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TWinHttpConnectionClass }

constructor TWinHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited;
  InitializeCriticalSection(fLock);
  fConnection := TWinHTTP.Create(SockString(fParameters.Server),
    SockString(IntToStr(fParameters.Port)),fParameters.Https,
    SockString(fParameters.ProxyName),SockString(fParameters.ProxyByPass),
    fParameters.ConnectionTimeOut,fParameters.SendTimeout,fParameters.ReceiveTimeout);
  fOpaqueConnection := fConnection;
  fConnection.IgnoreSSLCertificateErrors := true; // do not be paranoid here
end;

destructor TWinHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  DeleteCriticalSection(fLock);
  inherited;
end;

procedure TWinHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var inb,outb,outh: SockString;
    n: integer;
begin
  EnterCriticalSection(fLock);
  try
    SetString(inb,PAnsiChar(Call.InBody),length(Call.InBody));
    Call.OutStatus := fConnection.Request(SockString(Call.Url),
      SockString(Call.Verb),KeepAlive,SockString(Call.InHead),
      inb,SockString(InDataType),outh,outb);
    Call.OutHead := string(outh);
    n := length(outb);
    SetLength(Call.OutBody,n);
    Move(pointer(outb)^,pointer(Call.OutBody)^,n);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TWinHttpConnectionClass;
end;

{$endif}


{$ifdef ISDWS} // some definitions usually made in SynCrossPlatformJSON.pas

procedure DoubleQuoteStr(var text: string);
var i,j: integer;
    tmp: string;
begin
  i := pos('"',text);
  if i=0 then begin
    text := '"'+text+'"';
    exit;
  end;
  tmp := '"'+copy(text,1,i)+'"';
  for j := i+1 to length(text) do
    if text[j]='"' then
      tmp := tmp+'""' else
      tmp := tmp+text[j];
  text := tmp+'"';
end;

function IdemPropName(const PropName1,PropName2: string): boolean;
begin
  result := uppercase(PropName1)=uppercase(PropName2);
end;

function StartWithPropName(const PropName1,PropName2: string): boolean;
var L: integer;
begin
  L := length(PropName2);
  if length(PropName1)<L then
    result := false else
    result := IdemPropName(copy(PropName1,1,L),PropName2);
end;

function VarRecToValue(const VarRec: variant; var tmpIsString: boolean): string;
begin
  tmpIsString := TVariant.IsString(VarRec);
  if TVariant.IsNull(VarRec) then
    result := 'null' else
    result := TVariant.AsString(VarRec);
end;

procedure DecodeTime(Value: TDateTime; var HH,MM,SS,MS: word);
var date := new JDate;
begin
  date.AsDateTime := Value;
  HH := date.getUTCHours;
  MM := date.getUTCMinutes;
  SS := date.getUTCSeconds;
end;

procedure DecodeDate(Value: TDateTime; var Y,M,D: word);
var date := new JDate;
begin
  date.AsDateTime := Value;
  Y := date.getUTCFullYear;
  M := date.getUTCMonth+1;
  D := date.getUTCDate;
end;

function TryEncodeDate(Y,M,D: integer; UTC: DateTimeZone; var Value: TDateTime): boolean;
begin
  try
    Value := EncodeDate(Y,M,D, DateTimeZone.UTC);
    result := true
  except
    result := false;
  end;
end;

function TryEncodeTime(HH,MM,SS,MS: integer; var Value: TDateTime): boolean;
begin
  try
    Value := EncodeTime(HH,MM,SS,MS);
    result := true
  except
    result := false;
  end;
end;

function UpCase(ch: Char): Char; inline;
begin
  result := ch.UpperCase;
end;

function TryStrToInt(const S: string; var Value: Integer): Boolean;
begin
  try
    Value := StrToInt(S);
    result := true;
  except
    on E: Exception do
      result := false;
  end;
end;

function TryStrToInt64(const S: string; var Value: Int64): Boolean; inline;
begin
  result := TryStrToInt(S,Value);
end;

function StrToInt64Def(const S: string; const def: Int64): Int64;
begin
  if not TryStrToInt(S,result) then
      result := def;
end;

function NowToIso8601: string;
begin
  result := DateTimeToIso8601(Now);
end;

function DateTimeToIso8601(Value: TDateTime): string;
begin // e.g. YYYY-MM-DD Thh:mm:ss or YYYY-MM-DDThh:mm:ss
  if Value<=0 then
    result := '' else
  if frac(Value)=0 then
    result := FormatDateTime('yyyy-mm-dd',Value,DateTimeZone.UTC) else
  if trunc(Value)=0 then
    result := FormatDateTime('Thh:nn:ss',Value,DateTimeZone.UTC) else
    result := FormatDateTime('yyyy-mm-ddThh:nn:ss',Value,DateTimeZone.UTC);
end;

function Iso8601ToDateTime(const Value: string): TDateTime;
var Y,M,D, HH,MI,SS: cardinal;
begin //  YYYY-MM-DD   Thh:mm:ss  or  YYYY-MM-DDThh:mm:ss
      //  1234567890   123456789      1234567890123456789
  result := 0;
  case Length(Value) of
  9: if (Value[1]='T') and (Value[4]=':') and (Value[7]=':') then begin
    HH := ord(Value[2])*10+ord(Value[3])-(48+480);
    MI := ord(Value[5])*10+ord(Value[6])-(48+480);
    SS := ord(Value[8])*10+ord(Value[9])-(48+480);
    TryEncodeTime(HH,MI,SS,0,result);
  end;
  10: if (Value[5]=Value[8]) and (ord(Value[8]) in [ord('-'),ord('/')]) then begin
    Y := ord(Value[1])*1000+ord(Value[2])*100+
         ord(Value[3])*10+ord(Value[4])-(48+480+4800+48000);
    M := ord(Value[6])*10+ord(Value[7])-(48+480);
    D := ord(Value[9])*10+ord(Value[10])-(48+480);
    TryEncodeDate(Y,M,D,DateTimeZone.UTC,result);
  end;
  19: if (Value[5]=Value[8]) and (ord(Value[8]) in [ord('-'),ord('/')]) and
         (ord(Value[11]) in [ord(' '),ord('T')]) and (Value[14]=':') and (Value[17]=':') then begin
    Y := ord(Value[1])*1000+ord(Value[2])*100+
         ord(Value[3])*10+ord(Value[4])-(48+480+4800+48000);
    M := ord(Value[6])*10+ord(Value[7])-(48+480);
    D := ord(Value[9])*10+ord(Value[10])-(48+480);
    HH := ord(Value[12])*10+ord(Value[13])-(48+480);
    MI := ord(Value[15])*10+ord(Value[16])-(48+480);
    SS := ord(Value[18])*10+ord(Value[19])-(48+480);
    if (Y<=9999) and ((M-1)<12) and ((D-1)<31) and
       (HH<24) and (MI<60) and (SS<60) then
      result := EncodeDate(Y,M,D,DateTimeZone.UTC)+EncodeTime(HH,MI,SS,0);
  end;
  end;
end;


{ TJSONVariantData }

{$HINTS OFF}
function VariantType(const Value: variant): TJSONVariantKind;
begin
  asm
    if (@Value === null) return 0;
    if (typeof(@Value) !== "object") return 0;
    if (Object.prototype.toString.call(@Value) === "[object Array]") return 2;
    return 1;
  end;
end;
{$HINTS ON}

constructor TJSONVariantData.Create(const aJSON: string);
begin
  CreateFrom(JSON.Parse(aJSON));
end;

constructor TJSONVariantData.CreateFrom(const document: Variant);
var name: string;
begin
  Kind := VariantType(document);
  case Kind of
  jvObject: begin
    Names := TVariant.Properties(document);
    for name in Names do
      Values.Add(document[name]);
  end;
  jvArray: asm
    @Values=@document;
  end;
  end;
end;


type
  TSMSHttpConnectionClass = class(TAbstractHttpConnection)
  protected  // see http://www.w3.org/TR/XMLHttpRequest
  public
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
  end;

{ TSMSHttpConnectionClass }

procedure TSMSHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
begin
  asm
    @Call.XHR = new XMLHttpRequest();
  end;
  if Assigned(Call.OnSuccess) then begin // asynchronous call
    Call.XHR.onreadystatechange := lambda
      if Call.XHR.readyState=rrsDone then begin
        Call.XHR.onreadystatechange := nil; // avoid any further trigger
        Call.OutStatus := Call.XHR.status;
        Call.OutHead := Call.XHR.getAllResponseHeaders();
        Call.OutBody := Call.XHR.responseText;
        Call.OnSuccess;
      end;
    end;
    Call.XHR.onerror := Call.OnError;
    Call.XHR.open(Call.Verb,fURL+Call.Url,true);  // true for asynch call
  end else
    Call.XHR.open(Call.Verb,fURL+Call.Url,false); // false for synch call
  if Call.InHead<>'' then begin
    var i = 1;
    var line: string;
    while GetNextCSV(Call.InHead,i,line,#10) do begin
      var l := pos(':',line );
      if l=0 then
        continue;
      var head := trim(copy(line,1,l-1));
      var value := trim(copy(line,l+1,length(line)));
      if (head<>'') and (value<>'') then
        Call.XHR.setRequestHeader(head,value);
    end;
  end;
  if Call.InBody='' then
    Call.XHR.send(null) else
    Call.XHR.send(Call.InBody);
  if not Assigned(Call.OnSuccess) then begin // synchronous call
    Call.OutStatus := Call.XHR.status;
    Call.OutHead := Call.XHR.getAllResponseHeaders();
    Call.OutBody := Call.XHR.responseText;
  end;
end;


function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TSMSHttpConnectionClass;
end;

{$endif ISDWS}


{ TSQLRestURIParams }

procedure TSQLRestURIParams.Init(const aUrl,aVerb,aUTF8Body: string);
begin
  Url := aUrl;
  Verb := aVerb;
  if aUTF8Body='' then
    exit;
  {$ifdef ISSMS}
  InBody := aUTF8Body;
  {$else}
  InBody := TextToHttpBody(aUTF8Body);
  {$endif}
end;

function TSQLRestURIParams.OutBodyUtf8: String;
begin
  {$ifdef ISSMS}
  result := OutBody; // XMLHttpRequest did convert UTF-8 into DomString
  {$else}
  HttpBodyToText(OutBody,result);
  {$endif}
end;


{$ifndef ISDWS}

{ TMutex }

{$ifdef USETMONITOR}

procedure TMutex.Enter;
begin
  TMonitor.Enter(self);
end;

procedure TMutex.Leave;
begin
  TMonitor.Exit(self);
end;

{$else}

constructor TMutex.Create;
begin
  {$ifdef FPC}
  InitCriticalSection(fLock);
  {$else}
  InitializeCriticalSection(fLock);
  {$endif}
end;

destructor TMutex.Destroy;
begin
  {$ifdef FPC}
  DoneCriticalSection(fLock);
  {$else}
  DeleteCriticalSection(fLock);
  {$endif}
end;

procedure TMutex.Enter;
begin
  EnterCriticalSection(fLock);
end;

procedure TMutex.Leave;
begin
  LeaveCriticalSection(fLock);
end;

{$endif}

{$endif ISDWS}

initialization
{$ifdef USEINDY}
  // see http://www.monien.net/delphi-xe5-ssl-https-on-different-platforms-with-tidhttp-and-trestclient
  {$ifdef MACOS} // for OSX, iOS ARM and iOS x86
  {$ifndef CPUARM}
  IdOpenSSLSetLibPath('/usr/lib/');  // for OSX and iOS x86
  {$endif}
  {$endif}
{$endif USEINDY}
end.
