{*******************************************************************************
Description:  TALWinHttpClient is a is easy to use WinHTTP-based
HTTP client component which allows to post and get
any data from the Web via HTTP protocol.

The TALWinHttpClient use WinHTTP and is recommended in
server process. In client process (like a browser for
exemple), you can use TALWinInetHttpClient instead.

Microsoft® Windows® HTTP Services (WinHTTP) provides
developers with a server-supported, high-level interface
to the HTTP/1.1 Internet protocol. WinHTTP is designed
to be used primarily in server-based scenarios by server
applications that communicate with HTTP servers. WinHTTP
is also designed for use in system services and HTTP-based
client applications. WinHTTP is more secure and robust than
WinInet. However, single-user applications that need FTP or
gopher functionality, cookie persistence, caching, automatic
credential dialog handling, Internet Explorer compatibility,
or downlevel platform support should still consider
using WinInet.

Know bug :
UserName and password not work (but you can use the header instead)
*******************************************************************************}

unit ALWinHttpClient;

interface

uses
  Winapi.Windows,
  System.Classes,
  ALHttpClient,
  ALWinHttpWrapper;

type

  {---------------------------------------------------------}
  TALWinHttpClientInternetOpenAccessType = (wHttpAt_NO_PROXY,                    {Resolves all host names locally.}
                                            wHttpAt_DEFAULT_PROXY,               {Retrieves the static proxy or direct configuration from the registry.
                                                                                  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY does not inherit browser proxy settings.
                                                                                  WinHTTP does not share any proxy settings with Internet Explorer. This option
                                                                                  picks up the WinHTTP proxy configuration set by the WinHTTP Proxycfg.exe utility.}
                                            wHttpAt_NAMED_PROXY);                {Passes requests to the proxy unless a proxy bypass list is supplied and the name
                                                                                  to be resolved bypasses the proxy. In this case, this function uses
                                                                                  WINHTTP_ACCESS_TYPE_NAMED_PROXY.}


  {----------------------------------------------}
  TAlWinHttpClientInternetOption = (wHttpIo_Async,                     {NOT SUPPORTED YET!
                                                                        Use the WinHTTP functions asynchronously. By default, all WinHTTP functions that use
                                                                        the returned HINTERNET handle are performed synchronously.}
                                    wHttpIo_BYPASS_PROXY_CACHE,        {This flag provides the same behavior as WINHTTP_FLAG_REFRESH.}
                                    wHttpIo_ESCAPE_DISABLE,            {Unsafe characters in the URL passed in for pwszObjectName are not converted to
                                                                        escape sequences.}
                                    wHttpIo_ESCAPE_DISABLE_QUERY,      {Unsafe characters in the query component of the URL passed in for pwszObjectName are not
                                                                        converted to escape sequences.}
                                    wHttpIo_ESCAPE_PERCENT,            {The string passed in for pwszObjectName is converted from an LPCWSTR to an LPSTR.
                                                                        All unsafe characters are converted to an escape sequence including the percent symbol.
                                                                        By default, all unsafe characters except the percent symbol are converted to an escape
                                                                        sequence.}
                                    wHttpIo_NULL_CODEPAGE,             {The string passed in for pwszObjectName is assumed to consist of valid ANSI characters
                                                                        represented by WCHARs. No check are done for unsafe characters.}
                                    wHttpIo_REFRESH,                    {Indicates that the request should be forwarded to the originating server rather than
                                                                        sending a cached version of a resource from a proxy server. When this flag is used,
                                                                        a "Pragma: no-cache" header is added to the request handle. When creating an HTTP/1.1
                                                                        request header, a "Cache-Control: no-cache" is also added.}
                                    wHttpIo_SECURE,                    {Uses secure transaction semantics. This translates to using Secure Sockets Layer
                                                                        (SSL)/Transport Layer Security (TLS).}
                                    wHttpIo_Keep_connection,           {Uses keep-alive semantics, if available, for the connection. This flag is required for Microsoft
                                                                        Network (MSN), NT LAN Manager (NTLM), and other types of authentication.}
                                    wHttpIo_No_cookies,                {Does not automatically add cookie headers to requests, and does not automatically add returned
                                                                        cookies to the cookie database.}
                                    wHttpIo_No_auto_redirect);         {Does not automatically handle redirection in HttpSendRequest.}

  {------------------------------------------------------------------------}
  TALWinHttpClientInternetOptionSet = Set of TALWinHttpClientInternetOption;

  {--------------------------------}
  {TALWinHttpClientStatusChangeEvent

   InternetStatus
   can be one of the following value:

      WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION     Closing the connection to the server. The lpvStatusInformation parameter is NULL.
                                                     This flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER    Successfully connected to the server. The lpvStatusInformation parameter contains a
                                                     pointer to an LPWSTR that indicates the IP address of the server in dotted notation.
                                                     This flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER   Connecting to the server. The lpvStatusInformation parameter contains a pointer to
                                                     an LPWSTR that indicates the IP address of the server in dotted notation. This flag
                                                     has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED      Successfully closed the connection to the server. The lpvStatusInformation parameter is
                                                     NULL. This flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE         Data is available to be retrieved with WinHttpReadData. The lpvStatusInformation parameter
                                                     points to a DWORD that contains the number of bytes of data available. The dwStatusInformationLength
                                                     parameter itself is 4 (the size of a DWORD).
      WINHTTP_CALLBACK_STATUS_HANDLE_CREATED         An HINTERNET handle has been created. The lpvStatusInformation parameter contains a pointer to
                                                     the HINTERNET handle. This flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING         This handle value has been terminated. The lpvStatusInformation parameter contains a
                                                     pointer to the HINTERNET handle.
      WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE      The response header has been received and is available with WinHttpQueryHeaders.
                                                     The lpvStatusInformation parameter is NULL.
      WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE  Received an intermediate (100 level) status code message from the server. The lpvStatusInformation
                                                     parameter contains a pointer to a DWORD that indicates the status code.
      WINHTTP_CALLBACK_STATUS_NAME_RESOLVED          Successfully found the IP address of the server. The lpvStatusInformation parameter contains a
                                                     pointer to an LPWSTR that indicates the name that was resolved. This flag has been deprecated
                                                     and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_READ_COMPLETE          Data was successfully read from the server. The lpvStatusInformation parameter contains a pointer
                                                     to the buffer specified in the call to WinHttpReadData. The dwStatusInformationLength parameter
                                                     contains the number of bytes read.
      WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE     Waiting for the server to respond to a request. The lpvStatusInformation parameter is NULL. This
                                                     flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_REDIRECT               An HTTP request is about to automatically redirect the request. The lpvStatusInformation parameter
                                                     contains a pointer to an LPWSTR indicating the new URL. At this point, the application can read any
                                                     data returned by the server with the redirect response and can query the response headers. It can
                                                     also cancel the operation by closing the handle.
      WINHTTP_CALLBACK_STATUS_REQUEST_ERROR          An error occurred while sending an HTTP request. The lpvStatusInformation parameter contains a pointer
                                                     to a WINHTTP_ASYNC_RESULT structure, of which the dwResult member indicates the return value of the
                                                     function and any error codes that apply.
      WINHTTP_CALLBACK_STATUS_REQUEST_SENT           Successfully sent the information request to the server. The lpvStatusInformation parameter contains a
                                                     pointer to a DWORD indicating the number of bytes sent. This flag has been deprecated and may be
                                                     removed in a future release.
      WINHTTP_CALLBACK_STATUS_RESOLVING_NAME         Looking up the IP address of a server name. The lpvStatusInformation parameter contains a pointer to
                                                     the server name being resolved. This flag has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED      Successfully received a response from the server. The lpvStatusInformation parameter contains a pointer
                                                     to a DWORD indicating the number of bytes received. This flag has been deprecated and may be removed
                                                     in a future release.
      WINHTTP_CALLBACK_STATUS_SECURE_FAILURE         One or more errors were encountered while retrieving a Secure Sockets Layer (SSL) certificate from the
                                                     server. The lpvStatusInformation parameter contains a flag. For more information, see the description
                                                     for lpvStatusInformation.
      WINHTTP_CALLBACK_STATUS_SENDING_REQUEST        Sending the information request to the server. The lpvStatusInformation parameter is NULL. This flag
                                                     has been deprecated and may be removed in a future release.
      WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE   The request completed successfully. The lpvStatusInformation parameter is NULL.
      WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE         Data was successfully written to the server. The lpvStatusInformation parameter contains a pointer to
                                                     a DWORD that indicates the number of bytes written.

   StatusInformation
   A pointer to a buffer that specifies information pertinent to this call to the callback function. The format of this data depends on
   the value of the InternetStatus parameter. For more information, see InternetStatus.

   If the InternetStatus parameter is WINHTTP_CALLBACK_STATUS_SECURE_FAILURE, this parameter can be one of the following values.

      WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED          Certification revocation checking has been enabled, but the revocation check failed to verify whether a
                                                            certificate has been revoked. The server used to check for revocation might be unreachable.
      WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT SSL         certificate is invalid.
      WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED SSL         certificate was revoked.
      WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA The           function is unfamiliar with the Certificate Authority that generated the server's certificate.
      WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID SSL      certificate common name (host name field) is incorrect, for example, if you entered www.microsoft.com
                                                            and the common name on the certificate says www.msn.com.
      WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID SSL    certificate date that was received from the server is bad. The certificate is expired.
      WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR   The application experienced an internal error loading the SSL libraries.


   StatusInformationLength
   Size of the data pointed to by StatusInformation.}
  TALWinHttpClientStatusChangeEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: LPVOID; StatusInformationLength: DWord) of object;

  {-------------------------------------}
  TALWinHttpClient = class(TALHTTPClient)
  private
    FAccessType: TALWinHttpClientInternetOpenAccessType;
    FInternetOptions: TALWinHttpClientInternetOptionSet;
    FConnected: Boolean;
    FURL: AnsiString;
    FURLHost: AnsiString;
    FURLSite: AnsiString;
    FURLPort: INTERNET_PORT;
    FURLScheme: INTERNET_SCHEME;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatusChange: TALWinHttpClientStatusChangeEvent;
    procedure InitURL(const Value: AnsiString);
    procedure SetAccessType(const Value: TALWinHttpClientInternetOpenAccessType);
    procedure SetOnStatusChange(const Value: TALWinHttpClientStatusChangeEvent);
  protected
    procedure CheckError(ErrCode: DWORD); overload;
    procedure CheckError(Error: Boolean); overload;
    procedure SetURL(const Value: AnsiString);
    procedure SetUsername(const NameValue: AnsiString); override;
    procedure SetPassword(const PasswordValue: AnsiString); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure SetOnRedirect(const Value: TAlHTTPClientRedirectEvent); override;
    procedure Execute(const aUrl:AnsiString;
                      const aRequestMethod: TALHTTPMethod;
                      const aRequestDataStream: TStream;
                      const ARequestHeaderValues: TALNameValueArray;
                      const aResponseContent: TStream;
                      const aResponseHeader: TALHTTPResponseHeader); override;
    function  Send(const aRequestMethod: TALHTTPMethod;
                   const aRequestDataStream: TStream): HINTERNET; virtual;
    procedure Receive(const aContext: HINTERNET;
                      const aResponseContent: TStream;
                      const aResponseHeader: TALHTTPResponseHeader); virtual;
    property  URL: AnsiString read FURL write SetURL;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    property  AccessType: TALWinHttpClientInternetOpenAccessType read FAccessType write SetAccessType default wHttpAt_NO_PROXY;
    property  InternetOptions: TALWinHttpClientInternetOptionSet read FInternetOptions write FInternetOptions default [wHttpIo_Keep_connection];
    property  OnStatusChange: TALWinHttpClientStatusChangeEvent read FOnStatusChange write SetOnStatusChange;
  end;

implementation

uses
  System.SysUtils,
  System.Ansistrings,
  ALCommon,
  ALString;

{***********************************************************}
procedure ALWinHTTPCLientStatusCallback(hInternet: HINTERNET;
                                        dwContext: DWORD_PTR;
                                        dwInternetStatus: DWORD;
                                        lpvStatusInformation: LPVOID;
                                        dwStatusInformationLength: DWORD); stdcall;
begin
  if dwContext = 0 then exit;
  with TALWinHttpClient(dwContext) do begin

    {fire the On Status change event}
    if Assigned(FOnStatusChange) then
      FOnStatusChange(TALWinHttpClient(dwContext),
                      dwInternetStatus,
                      lpvStatusInformation,
                      dwStatusInformationLength);

    {fire the On redirect event}
    If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_REDIRECT) and Assigned(OnRedirect) then
      OnRedirect(TALWinHttpClient(dwContext), AnsiString(PWideChar(lpvStatusInformation)));

  end;
end;

{**********************************}
constructor TALWinHttpClient.Create;
begin
  inherited;
  FInetRoot := nil;
  FInetConnect := nil;
  FConnected := False;
  FURL:= '';
  FURLHost := '';
  FURLSite := '';
  FURLPort := INTERNET_DEFAULT_HTTP_PORT;
  FURLScheme := INTERNET_SCHEME_HTTP;
  FOnStatusChange := nil;
  FAccessType := wHttpAt_NO_PROXY;
  FInternetOptions := [wHttpIo_Keep_connection];
  RequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALWinHttpClient)';
end;

{**********************************}
destructor TALWinHttpClient.Destroy;
begin
  Disconnect;
  inherited;
end;

{****************************************************}
procedure TALWinHttpClient.CheckError(ErrCode: DWORD);
var ln: DWORD;
    S: AnsiString;
begin
  if ErrCode <> 0 then begin
    SetLength(S, 256);
    ln := FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE, //  _In_     DWORD   dwFlags,
                         Pointer(GetModuleHandle('winhttp.dll')), // _In_opt_ LPCVOID lpSource,
                         ErrCode, // _In_     DWORD   dwMessageId,
                         0, // _In_     DWORD   dwLanguageId,
                         PAnsiChar(S), // _Out_    LPTSTR  lpBuffer,
                         Length(S), // _In_     DWORD   nSize,
                         nil); // _In_opt_ va_list *Arguments
    if ln = 0 then raiseLastOsError;
    SetLength(S, ln);
    raise EALHTTPClientException.CreateFmt('%s - URL:%s', [ALTrim(S), URL]);      { Do not localize }
  end;
end;

{****************************************************}
procedure TALWinHttpClient.CheckError(Error: Boolean);
begin
  if Error then
    CheckError(GetLastError);
end;

{*********************************************************}
procedure TALWinHttpClient.SetURL(const Value: AnsiString);
Var LOldURLHost: AnsiString;
    LOldUrlPort: INTERNET_PORT;
    LOldURLScheme: INTERNET_SCHEME;
begin
  If Value <> Url then Begin
    LOldURLHost := FURLHost;
    LOldUrlPort := FURLPort;
    LOldURLScheme := FURLScheme;
    InitURL(Value);

    { Here we disconnect if a new URL comes in with a
      new host...this ensures that we don't keep a
      connection to a wrong host }
    If (LOldURLHost <> FURLHost) or
       (LOldUrlPort <> FURLPort) or
       (LOldURLScheme <> FURLScheme) then Disconnect;

    Furl := Value;
  end;
end;

{**********************************************************}
procedure TALWinHttpClient.InitURL(const Value: AnsiString);
var URLComp: TURLComponents;
    P: PWideChar;
begin
  if Value <> '' then begin
    FillChar(URLComp, SizeOf(URLComp), 0);
    URLComp.dwStructSize := SizeOf(URLComp);
    URLComp.dwSchemeLength := 1;
    URLComp.dwHostNameLength := 1;
    URLComp.dwURLPathLength := 1;
    P := PWideChar(String(Value));
    if (not WinHttpCrackUrl(P, 0, 0, URLComp)) or
       (not (URLComp.nScheme in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS])) then
      raise EALHTTPClientException.CreateFmt(CALHTTPCLient_MsgInvalidURL, [Value]);
    FURLScheme := URLComp.nScheme;
    FURLPort := URLComp.nPort;
    FURLHost := ALCopyStr(Value, URLComp.lpszHostName - P + 1, URLComp.dwHostNameLength);
    FURLSite := ALCopyStr(Value, URLComp.lpszUrlPath - P + 1, URLComp.dwUrlPathLength);
  end
  else begin
    FURLPort := INTERNET_DEFAULT_HTTP_PORT;
    FURLHost := '';
    FURLSite := '';
    FURLScheme := INTERNET_SCHEME_HTTP;
  end;
end;

{******************************************************************}
procedure TALWinHttpClient.SetUsername(const NameValue: AnsiString);
begin
  If UserName <> NameValue then Disconnect;
  inherited;
end;

{**********************************************************************}
procedure TALWinHttpClient.SetPassword(const PasswordValue: AnsiString);
begin
  IF Password <> PasswordValue then Disconnect;
  inherited;
end;

{*********************************}
procedure TALWinHttpClient.Connect;

  {---------------------------------------------}
  Function InternalGetProxyServerName: PWideChar;
  Begin
    If (FaccessType <> wHTTPat_NAMED_PROXY) or
       (ProxyParams.ProxyServer = '') then result := WINHTTP_NO_PROXY_NAME
    else result := PWideChar(string(ProxyParams.ProxyServer) + ':' + String(ALIntToStr(ProxyParams.ProxyPort)));
  end;

  {-----------------------------------------}
  Function InternalGetProxyBypass: PWideChar;
  Begin
    {We should not use empty string for ProxyBypass because
     InternetOpen will use it as the proxy bypass list}
    if (FaccessType <> wHTTPat_NAMED_PROXY) or
       (ProxyParams.ProxyBypass = '') then result := WINHTTP_NO_PROXY_BYPASS
    else result := PWidechar(String(ProxyParams.ProxyBypass));
  end;

const AccessTypeArr: Array[TALWinHttpClientInternetOpenAccessType] of DWord = (WINHTTP_ACCESS_TYPE_NO_PROXY,
                                                                               WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                                                                               WINHTTP_ACCESS_TYPE_NAMED_PROXY);

var WinHttpSetStatusCallbackResult: TWinHttpStatusCallback;

begin
  { Yes, but what if we're connected to a different Host/Port?? }
  { So take advantage of a cached handle, we'll assume that
    Connect(False) will be called explicitly when we're switching
    Host. To that end, SetURL always disconnects }
  if (FConnected) then Exit;

  {init FInetRoot}
  FInetRoot := WinHttpOpen(nil, // _In_opt_ LPCWSTR pwszUserAgent,
                           AccessTypeArr[FAccessType], // _In_     DWORD   dwAccessType,
                           InternalGetProxyServerName, // _In_     LPCWSTR pwszProxyName,
                           InternalGetProxyBypass, // _In_     LPCWSTR pwszProxyBypass,
                           0); // _In_     DWORD   dwFlags
  CheckError(not Assigned(FInetRoot));

  try

    {Register the callback function}
    if assigned(OnStatusChange) or assigned(OnRedirect) then begin
      WinHttpSetStatusCallbackResult := WinHttpSetStatusCallback(FInetRoot, // _In_       HINTERNET               hInternet,
                                                                 @ALWinHTTPCLientStatusCallback, // _In_       WINHTTP_STATUS_CALLBACK lpfnInternetCallback,
                                                                 WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, // _In_       DWORD                   dwNotificationFlags,
                                                                 0); // _Reserved_ DWORD_PTR               dwReserved
      CheckError(@WinHttpSetStatusCallbackResult = @WINHTTP_INVALID_STATUS_CALLBACK);
    end;

    {init FInetConnect}
    FInetConnect := WinHttpConnect(FInetRoot, // _In_       HINTERNET     hSession,
                                   PWideChar(String(FURLHost)), // _In_       LPCWSTR       pswzServerName,
                                   FURLPort, // _In_       INTERNET_PORT nServerPort,
                                   0); //  _Reserved_ DWORD         dwReserved
    CheckError(not Assigned(FInetConnect));

    {Set FConnected to true}
    FConnected := True;

  except
    WinHttpCloseHandle(FInetRoot);
    FInetRoot := nil;
    raise;
  end;
end;

{************************************}
procedure TALWinHttpClient.Disconnect;
begin
  if Assigned(FInetConnect) then WinHttpCloseHandle(FInetConnect);
  FInetConnect := nil;
  if Assigned(FInetRoot) then WinHttpCloseHandle(FInetRoot);
  FInetRoot := nil;
  FConnected := False;
end;

{*****************************************************************}
function TALWinHttpClient.Send(const aRequestMethod: TALHTTPMethod;
                               const aRequestDataStream: TStream): HINTERNET;

  {----------------------------------------------}
  Function InternalGetHttpOpenRequestFlags: DWord;
  Begin
    Result := 0;
    If wHttpIo_BYPASS_PROXY_CACHE in InternetOptions then Result := result or WINHTTP_FLAG_BYPASS_PROXY_CACHE;
    If wHttpIo_ESCAPE_DISABLE in InternetOptions then Result := result or WINHTTP_FLAG_ESCAPE_DISABLE;
    If wHttpIo_ESCAPE_DISABLE_QUERY in InternetOptions then Result := result or WINHTTP_FLAG_ESCAPE_DISABLE_QUERY;
    If wHttpIo_ESCAPE_PERCENT in InternetOptions then Result := result or WINHTTP_FLAG_ESCAPE_PERCENT;
    If wHttpIo_NULL_CODEPAGE in InternetOptions then Result := result or WINHTTP_FLAG_NULL_CODEPAGE;
    If wHttpIo_REFRESH in InternetOptions then Result := result or WINHTTP_FLAG_REFRESH;
    if (whttpIo_SECURE in InternetOptions) or
       (FURLScheme = INTERNET_SCHEME_HTTPS) then Result := result or WINHTTP_FLAG_SECURE;
  end;

var BuffSize, Len: DWord;
    Buffer: TMemoryStream;
    LNumberOfBytesWritten: DWord;
    LHeader: String;
    LOption: DWord;
    StrProtocolVersion: String;
    StrVerb: String;
    AcceptTypes: array of PWideChar;
    CertContextSet: Boolean;
    ErrCode: DWORD;

begin

  { Connect }
  Connect;

  If ProtocolVersion = HTTPpv_1_1 then StrProtocolVersion := 'HTTP/1.1'
  else StrProtocolVersion := 'HTTP/1.0';

  if      aRequestMethod = HTTPmt_Post then    StrVerb := 'POST'
  else if aRequestMethod = HTTPmt_Get then     StrVerb := 'GET'
  else if aRequestMethod = HTTPmt_head then    StrVerb := 'HEAD'
  else if aRequestMethod = HTTPmt_trace then   StrVerb := 'TRACE'
  else if aRequestMethod = HTTPmt_Put then     StrVerb := 'PUT'
  else if aRequestMethod = HTTPmt_Delete then  StrVerb := 'DELETE'
  else if aRequestMethod = HTTPmt_Options then StrVerb := 'OPTIONS'
  else raise Exception.Create('Unknown Request Method');

  SetLength(AcceptTypes, 2);
  AcceptTypes[0] := PWideChar(String(RequestHeader.Accept));
  AcceptTypes[1] := nil;

  Result := WinHttpOpenRequest(FInetConnect, // _In_ HINTERNET hConnect,
                               PWideChar(StrVerb), //  _In_ LPCWSTR   pwszVerb,
                               PWideChar(String(FURLSite)), // _In_ LPCWSTR   pwszObjectName,
                               PWideChar(StrProtocolVersion), // _In_ LPCWSTR   pwszVersion,
                               PWideChar(String(requestHeader.Referer)), // _In_ LPCWSTR   pwszReferrer,
                               pointer(AcceptTypes), // _In_ LPCWSTR   *ppwszAcceptTypes,
                               InternalGetHttpOpenRequestFlags); // _In_ DWORD     dwFlags
  CheckError(not Assigned(Result));

  try

    { Timeouts }
    if ConnectTimeout > 0 then begin
      LOption := ConnectTimeout;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_CONNECT_TIMEOUT , Pointer(@LOption), SizeOf(LOption)));
    end;
    if SendTimeout > 0 then begin
      LOption := SendTimeout;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_SEND_TIMEOUT , Pointer(@LOption), SizeOf(LOption)));
    end;
    if ReceiveTimeout > 0 then begin
      LOption := ReceiveTimeout;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_RECEIVE_TIMEOUT, Pointer(@LOption), SizeOf(LOption)));
    end;

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_PROXY_USERNAME, PWidechar(String(ProxyParams.ProxyUserName)), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_PROXY_PASSWORD, PWidechar(String(ProxyParams.ProxyPassword)), length(ProxyParams.ProxyPassword)));

    {wHttpIo_Keep_connection, wHttpIo_No_cookies and wHttpIo_No_auto_redirect}
    If not (wHttpIo_Keep_connection in InternetOptions) then begin
      LOption := WINHTTP_DISABLE_KEEP_ALIVE;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;
    If (wHttpIo_No_cookies in InternetOptions) then begin
      LOption := WINHTTP_DISABLE_COOKIES;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;
    If (wHttpIo_No_auto_redirect in InternetOptions) then begin
      LOption := WINHTTP_DISABLE_REDIRECTS;
      CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;

    {set the header}
    LHeader := String(requestHeader.RawHeaderText);
    CheckError(not WinHttpAddRequestHeaders(Result, // _In_ HINTERNET hRequest,
                                            PWideChar(LHeader), // _In_ LPCWSTR   pwszHeaders,
                                            Length(LHeader),  // _In_ DWORD     dwHeadersLength,
                                            WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD)); // _In_ DWORD     dwModifiers

    If assigned(aRequestDataStream) then begin
      aRequestDataStream.Position := 0;
      BuffSize := aRequestDataStream.Size;
    end
    else BuffSize := 0;

    if BuffSize > UploadBufferSize then begin
      Buffer := TMemoryStream.Create;
      try
        Buffer.SetSize(UploadBufferSize);
        CertContextSet := False;
        repeat
          if not WinHttpSendRequest(Result, // _In_     HINTERNET hRequest,
                                    WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                                    0, // _In_     DWORD     dwHeadersLength,
                                    nil, // _In_opt_ LPVOID    lpOptional,
                                    0,  //  _In_     DWORD     dwOptionalLength,
                                    BuffSize, // _In_     DWORD     dwTotalLength,
                                    DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
            ErrCode := GetLastError;
            if (not CertContextSet) and (ErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
              CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
              CertContextSet := True;
            end
            else
              checkError(ErrCode);
          end
          else ErrCode := 0;
        until ErrCode = 0;
        try

          while True do begin
            Len := BuffSize - aRequestDataStream.Position;
            if Len > UploadBufferSize then Len := UploadBufferSize;
            if Len = 0 then break;
            Len := aRequestDataStream.Read(Buffer.Memory^, Len);
            if Len = 0 then raise EALHTTPClientException.Create(CALHTTPCLient_MsgInvalidHTTPRequest);
            CheckError(not WinHttpWriteData(Result, // _In_  HINTERNET hRequest,
                                            Buffer.Memory^, // _In_  LPCVOID   lpBuffer,
                                            Len, // _In_  DWORD     dwNumberOfBytesToWrite,
                                            @LNumberOfBytesWritten)); // _Out_ LPDWORD   lpdwNumberOfBytesWritten
            if LNumberOfBytesWritten < Len then
              aRequestDataStream.Position := aRequestDataStream.Position - (Len - LNumberOfBytesWritten);
            if Assigned(OnUploadProgress) then
              OnUploadProgress(self, aRequestDataStream.Position, BuffSize);
          end;

        finally
          CheckError(not WinHttpReceiveResponse(Result, nil));
        end;
      finally
        AlFreeAndNil(Buffer);
      end;
    end

    else if buffsize > 0 then begin
      Buffer := TMemoryStream.Create;
      try
        Buffer.CopyFrom(aRequestDataStream, 0);
        CertContextSet := False;
        repeat
          if not WinHttpSendRequest(Result, // _In_     HINTERNET hRequest,
                                    WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                                    0, // _In_     DWORD     dwHeadersLength,
                                    Buffer.Memory, // _In_opt_ LPVOID    lpOptional,
                                    Buffer.Size, //  _In_     DWORD     dwOptionalLength,
                                    Buffer.Size, // _In_     DWORD     dwTotalLength,
                                    DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
            ErrCode := GetLastError;
            if (not CertContextSet) and (ErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
              CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
              CertContextSet := True;
            end
            else
              checkError(ErrCode);
          end
          else ErrCode := 0;
        until ErrCode = 0;
        CheckError(not WinHttpReceiveResponse(Result, nil));
      finally
        AlFreeAndNil(Buffer);
      end;
    end

    else begin
      CertContextSet := False;
      repeat
        if not WinHttpSendRequest(Result, // _In_     HINTERNET hRequest,
                                  WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                                  0, // _In_     DWORD     dwHeadersLength,
                                  nil, // _In_opt_ LPVOID    lpOptional,
                                  0, //  _In_     DWORD     dwOptionalLength,
                                  0, // _In_     DWORD     dwTotalLength,
                                  DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
          ErrCode := GetLastError;
          if (not CertContextSet) and (ErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
            CheckError(not WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
            CertContextSet := True;
          end
          else
            checkError(ErrCode);
        end
        else ErrCode := 0;
      until ErrCode = 0;
      CheckError(not WinHttpReceiveResponse(Result, nil));
    end;

  except
    WinHttpCloseHandle(Result);
    raise;
  end;

end;

{************************************************************}
procedure  TALWinHttpClient.Receive(const aContext: HINTERNET;
                                    const aResponseContent: TStream;
                                    const aResponseHeader: TALHTTPResponseHeader);
var Size: DWord;
    Downloaded: DWord;
    Status: DWord;
    Len: DWord;
    ContentlengthDownloaded: DWord;
    ContentLength: DWord;
    LWideStr: String;
    LBuffer: Tbytes;
begin

  {read the header}
  If assigned(aResponseHeader) then begin
    Size := 4096 * sizeof(Char);
    While true do begin
      SetLength(LWideStr, Size div sizeof(Char));
      If WinHttpQueryHeaders(aContext, // _In_     HINTERNET hRequest,
                             WINHTTP_QUERY_RAW_HEADERS_CRLF, // _In_     DWORD     dwInfoLevel,
                             WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
                             pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
                             Size, // _Inout_  LPDWORD   lpdwBufferLength,
                             WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
        SetLength(LWideStr, Size div sizeof(Char));
        aResponseHeader.RawHeaderText := AnsiString(LWideStr);
        break;
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { read content-length }
  if Assigned(onDownloadProgress) then begin
    Len := SizeOf(ContentLength);
    if not WinHttpQueryHeaders(aContext, // _In_     HINTERNET hRequest,
                               WINHTTP_QUERY_CONTENT_LENGTH or WINHTTP_QUERY_FLAG_NUMBER, // _In_     DWORD     dwInfoLevel,
                               WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
                               @ContentLength, // _Out_    LPVOID    lpBuffer,
                               Len, // _Inout_  LPDWORD   lpdwBufferLength,
                               WINHTTP_NO_HEADER_INDEX) then ContentLength := 0; // _Inout_  LPDWORD   lpdwIndex
  end;

  { Read data }
  Len := 0;
  ContentlengthDownloaded := 0;
  repeat
    CheckError(not WinHttpQueryDataAvailable(aContext, @Size));
    if Size > 0 then begin
      if dword(length(LBuffer)) <> Size then SetLength(LBuffer, Size);
      CheckError(not WinHttpReadData(aContext, // _In_  HINTERNET hRequest,
                                     LBuffer[0], // _Out_ LPVOID    lpBuffer,
                                     Size, // _In_  DWORD     dwNumberOfBytesToRead,
                                     @Downloaded)); // _Out_ LPDWORD   lpdwNumberOfBytesRead
      aResponseContent.WriteBuffer(pointer(LBuffer)^, Downloaded);
      if Assigned(onDownloadProgress) then begin
        inc(ContentlengthDownloaded, Downloaded);
        onDownloadProgress(self, ContentlengthDownloaded, ContentLength);
      end;
    end;
  until Size = 0;

  { Handle error from status}
  Len := SizeOf(Status);
  if WinHttpQueryHeaders(aContext, // _In_     HINTERNET hRequest,
                         WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER, // _In_     DWORD     dwInfoLevel,
                         WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
                         @Status, // _Out_    LPVOID    lpBuffer,
                         Len, // _Inout_  LPDWORD   lpdwBufferLength,
                         WINHTTP_NO_HEADER_INDEX) and // _Inout_  LPDWORD   lpdwIndex
     (Status >= 300) then begin
    Size := 256 * sizeof(Char);
    While true do begin
      SetLength(LWideStr, Size div sizeof(Char));
      if WinHttpQueryHeaders(aContext, // _In_     HINTERNET hRequest,
                             WINHTTP_QUERY_STATUS_TEXT, // _In_     DWORD     dwInfoLevel,
                             WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
                             pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
                             Size, // _Inout_  LPDWORD   lpdwBufferLength,
                             WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
        SetLength(LWideStr, Size div sizeof(Char));
        raise EALHTTPClientException.CreateFmt('%s (%d) - ''%s''', [LWideStr, Status, URL], Status);
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

end;

{*******************************************************}
procedure TALWinHttpClient.Execute(const aUrl:AnsiString;
                                   const aRequestMethod: TALHTTPMethod;
                                   const aRequestDataStream: TStream;
                                   const ARequestHeaderValues: TALNameValueArray;
                                   const aResponseContent: TStream;
                                   const aResponseHeader: TALHTTPResponseHeader);
var LContext: HINTERNET;
    LOldRawRequestHeaderText: AnsiString;
    LRestoreRequestHeader: Boolean;
    I: integer;
begin
  SetURL(aURL);
  if URL = '' then raise EALHTTPClientException.Create(CALHTTPCLient_MsgEmptyURL);

  LRestoreRequestHeader := False;
  if length(ARequestHeaderValues) > 0 then begin
    LRestoreRequestHeader := True;
    LOldRawRequestHeaderText := requestHeader.RawHeaderText;
    for I := Low(ARequestHeaderValues) to High(ARequestHeaderValues) do
      requestHeader.setHeaderValue(alTrim(ARequestHeaderValues[I].Name),
                                   alTrim(ARequestHeaderValues[I].Value));
  end;

  try

    LContext := Send(aRequestMethod, aRequestDataStream);
    try
      Receive(LContext, aResponseContent, aResponseHeader);
    finally
      WinHttpCloseHandle(LContext);
    end;

  finally
    if LRestoreRequestHeader then
      requestHeader.RawHeaderText := LOldRawRequestHeaderText;
  end;
end;

{********************************************************************************************}
procedure TALWinHttpClient.SetAccessType(const Value: TALWinHttpClientInternetOpenAccessType);
begin
  If (value <> AccessType) then begin
    Disconnect;
    FaccessType := value;
  end;
end;

{*******************************************************************************************}
procedure TALWinHttpClient.SetOnStatusChange(const Value: TALWinHttpClientStatusChangeEvent);
begin
  Disconnect;
  FOnStatusChange := Value;
end;

{********************************************************************************}
procedure TALWinHttpClient.SetOnRedirect(const Value: TAlHTTPClientRedirectEvent);
begin
  disconnect;
  inherited;
end;

{********************************************************************************************}
procedure TALWinHttpClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

end.


