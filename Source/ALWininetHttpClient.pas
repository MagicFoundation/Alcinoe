{*******************************************************************************
TALWinInetHttpClient is a is easy to use WinInet-based
HTTP client component which allows to post and get
any data from the Web via HTTP protocol. TALWinInetHttpClient
component can retrieve documents or files using HTTP protocol;
that is connect to a HTTP server also known as a webserver.
It can also trigger a CGI/ISAPI/NSAPI script and post data
using either GET or POST method.

The TALWinInetHttpClient can grab web contents both in binary
and text formats, supports cache of Internet Explorer,
can resume broken downloads, read data from password
protected directories and supports basic proxy
authentication scheme.
*******************************************************************************}

unit ALWininetHttpClient;

interface

uses
  Winapi.Windows,
  System.Classes,
  WinApi.WinInet,
  ALHttpClient;

const
  INTERNET_STATUS_COOKIE_SENT      = 320;
  INTERNET_STATUS_COOKIE_RECEIVED  = 321;
  INTERNET_STATUS_PRIVACY_IMPACTED = 324;
  INTERNET_STATUS_P3P_HEADER       = 325;
  INTERNET_STATUS_P3P_POLICYREF    = 326;
  INTERNET_STATUS_COOKIE_HISTORY   = 327;

type

  {-----------------------------------------------------}
  TALWinInetHttpInternetOpenAccessType = (wHttpAt_Direct,                       {Resolves all host names locally.}
                                          wHttpAt_Preconfig,                    {Retrieves the proxy or direct configuration from the registry.}
                                          wHttpAt_Preconfig_with_no_autoproxy,  {Retrieves the proxy or direct configuration from the registry and
                                                                                 prevents the use of a startup Microsoft JScript or Internet Setup
                                                                                 (INS) file.}
                                          wHttpAt_Proxy);                       {Passes requests to the proxy unless a proxy bypass list is supplied
                                                                                 and the name to be resolved bypasses the proxy. In this case, the
                                                                                 function uses Ioat_Direct.}


  {--------------------------------------------------}
  TAlWininetHttpClientInternetOption = (wHttpIo_Async,                     {NOT SUPPORTED YET!
                                                                            Makes only asynchronous requests on handles descended from the handle
                                                                            returned from InternetOpen function.}
                                        wHttpIo_From_Cache,                {Does not make network requests. All entities are returned from the cache.
                                                                            If the requested item is not in the cache, a suitable error, such as
                                                                            ERROR_FILE_NOT_FOUND, is returned.}
                                        wHttpIo_Offline,                   {Identical to Hcio_From_Cache. Does not make network requests.
                                                                            All entities are returned from the cache. If the requested item is not
                                                                            in the cache, a suitable error, such as ERROR_FILE_NOT_FOUND, is returned.}
                                        wHttpIo_Cache_if_net_fail,         {Returns the resource from the cache if the network request for the resource
                                                                            fails due to an ERROR_INTERNET_CONNECTION_RESET (the connection with the
                                                                            server has been reset) or ERROR_INTERNET_CANNOT_CONNECT (the attempt to
                                                                            connect to the server failed).}
                                        wHttpIo_Hyperlink,                 {Forces a reload if there was no Expires time and no LastModified time returned
                                                                            from the server when determining whether to reload the item from the network.}
                                        wHttpIo_Ignore_cert_cn_invalid,    {Disables checking of SSL/PCT-based certificates that are returned from the server
                                                                            against the host name given in the request. WinINet functions use a simple check
                                                                            against certificates by comparing for matching host names and simple wildcarding rules.}
                                        wHttpIo_Ignore_cert_date_invalid,  {Disables checking of SSL/PCT-based certificates for proper validity dates.}
                                        wHttpIo_Ignore_redirect_to_http,   {Disables detection of this special type of redirect. When this flag is used, WinINet functions
                                                                            transparently allow redirects from HTTPS to HTTP URLs.}
                                        wHttpIo_Ignore_redirect_to_https,  {Disables detection of this special type of redirect. When this flag is used, WinINet functions
                                                                            transparently allow redirects from HTTP to HTTPS URLs.}
                                        wHttpIo_Keep_connection,           {Uses keep-alive semantics, if available, for the connection. This flag is required for Microsoft
                                                                            Network (MSN), NT LAN Manager (NTLM), and other types of authentication.}
                                        wHttpIo_Need_file,                 {Causes a temporary file to be created if the file cannot be cached.}
                                        wHttpIo_No_auth,                   {Does not attempt authentication automatically.}
                                        wHttpIo_No_auto_redirect,          {Does not automatically handle redirection in HttpSendRequest.}
                                        wHttpIo_No_cache_write,            {Does not add the returned entity to the cache.}
                                        wHttpIo_No_cookies,                {Does not automatically add cookie headers to requests, and does not automatically add returned
                                                                            cookies to the cookie database.}
                                        wHttpIo_No_ui,                     {Disables the cookie dialog box.}
                                        wHttpIo_Pragma_nocache,            {Forces the request to be resolved by the origin server, even if a cached copy exists on the proxy.}
                                        wHttpIo_Reload,                    {Forces a download of the requested file, object, or directory listing from the origin
                                                                           server, not from the cache.}
                                        wHttpIo_Resynchronize,             {Reloads HTTP resources if the resource has been modified since the last time it was downloaded.
                                                                           All FTP and Gopher resources are reloaded.}
                                        wHttpIo_Secure);                   {Uses secure transaction semantics. This translates to using Secure Sockets Layer/Private
                                                                           Communications Technology (SSL/PCT) and is only meaningful in HTTP requests.}

  {--------------------------------------------------------------------------------}
  TALWininetHttpClientInternetOptionSet = Set of TAlWininetHttpClientInternetOption;

  {------------------------------}
  {TAlWinInetHTTPClientStatusEvent

   InternetStatus
   can be one of the following value:

     INTERNET_STATUS_CLOSING_CONNECTION     Closing the connection to the server. The lpvStatusInformation parameter is NULL.
     INTERNET_STATUS_CONNECTED_TO_SERVER    Successfully connected to the socket address (SOCKADDR) pointed to by StatusInformation.
     INTERNET_STATUS_CONNECTING_TO_SERVER   Connecting to the socket address (SOCKADDR) pointed to by StatusInformation.
     INTERNET_STATUS_CONNECTION_CLOSED      Successfully closed the connection to the server. The StatusInformation parameter is NULL.
     INTERNET_STATUS_CTL_RESPONSE_RECEIVED  Not implemented.
     INTERNET_STATUS_DETECTING_PROXY        Notifies the client application that a proxy has been detected.
     INTERNET_STATUS_HANDLE_CLOSING         This handle value has been terminated.
     INTERNET_STATUS_HANDLE_CREATED         Used by InternetConnect to indicate it has created the new handle. This lets the application call
                                            InternetCloseHandle from another thread, if the connect is taking too long. The StatusInformation
                                            parameter contains the address of an INTERNET_ASYNC_RESULT structure.
     INTERNET_STATUS_INTERMEDIATE_RESPONSE  Received an intermediate (100 level) status code message from the server.
     INTERNET_STATUS_NAME_RESOLVED          Successfully found the IP address of the name contained in StatusInformation.
     INTERNET_STATUS_PREFETCH               Not implemented.
     INTERNET_STATUS_RECEIVING_RESPONSE     Waiting for the server to respond to a request. The StatusInformation parameter is NULL.
     INTERNET_STATUS_REDIRECT               An HTTP request is about to automatically redirect the request. The StatusInformation parameter points
                                            to the new URL. At this point, the application can read any data returned by the server with the
                                            redirect response and can query the response headers. It can also cancel the operation by closing
                                            the handle. This callback is not made if the original request specified INTERNET_FLAG_NO_AUTO_REDIRECT.
     INTERNET_STATUS_REQUEST_COMPLETE       An asynchronous operation has been completed. The StatusInformation parameter contains the
                                            address of an INTERNET_ASYNC_RESULT structure.
     INTERNET_STATUS_REQUEST_SENT           Successfully sent the information request to the server. The StatusInformation parameter points to a
                                            DWORD value that contains the number of bytes sent.
     INTERNET_STATUS_RESOLVING_NAME         Looking up the IP address of the name contained in StatusInformation.
     INTERNET_STATUS_RESPONSE_RECEIVED      Successfully received a response from the server. The StatusInformation parameter points to a
                                            DWORD value that contains the number of bytes received.
     INTERNET_STATUS_SENDING_REQUEST        Sending the information request to the server. The StatusInformation parameter is NULL.
     INTERNET_STATUS_STATE_CHANGE           Moved between a secure (HTTPS) and a nonsecure (HTTP) site. The user must be informed of this change; otherwise,
                                            the user is at risk of disclosing sensitive information involuntarily. When this flag is set, the
                                            StatusInformation parameter points to a status DWORD that contains additonal flags.
     INTERNET_STATUS_COOKIE_SENT            Not documented yet!
     INTERNET_STATUS_COOKIE_RECEIVED        Not documented yet!
     INTERNET_STATUS_PRIVACY_IMPACTED       Not documented yet!
     INTERNET_STATUS_P3P_HEADER             Not documented yet!
     INTERNET_STATUS_P3P_POLICYREF          Not documented yet!
     INTERNET_STATUS_COOKIE_HISTORY         Not documented yet!

   StatusInformation
   Additional status information (see below for more information).
   For the INTERNET_STATUS_STATE_CHANGE flag, StatusInformation Can be one of the following value (Dword) :

     INTERNET_STATE_CONNECTED               Connected state (mutually exclusive with disconnected state).
     INTERNET_STATE_DISCONNECTED            Disconnected state. No network connection could be established.
     INTERNET_STATE_DISCONNECTED_BY_USER    Disconnected by user request.
     INTERNET_STATE_IDLE                    No network requests are being made by Windows Internet.
     INTERNET_STATE_BUSY                    Network requests are being made by Windows Internet.
     INTERNET_STATUS_USER_INPUT_REQUIRED    The request requires user input to be completed.

   StatusInformationLength
   Size of the data pointed to by StatusInformation.}
  TAlWinInetHTTPClientStatusEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: LPVOID; StatusInformationLength: DWord) of object;

  {-----------------------------------------}
  TALWinInetHTTPClient = class(TALHTTPClient)
  private
    FAccessType: TALWinInetHttpInternetOpenAccessType;
    FInternetOptions: TAlWininetHTTPClientInternetOptionSet;
    FConnected: Boolean;
    FURL: AnsiString;
    FURLHost: AnsiString;
    FURLPath: AnsiString;
    FURLPort: INTERNET_PORT;
    FURLScheme: TInternetScheme;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatus: TAlWinInetHTTPClientStatusEvent;
    FIgnoreSecurityErrors: Boolean;
    procedure SetAccessType(const Value: TALWinInetHttpInternetOpenAccessType);
    procedure SetInternetOptions(const Value: TAlWininetHTTPClientInternetOptionSet);
    procedure SetOnStatus(const Value: TAlWinInetHTTPClientStatusEvent);
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
    property  AccessType: TALWinInetHttpInternetOpenAccessType read FAccessType write SetAccessType default wHttpAt_Preconfig;
    property  InternetOptions: TAlWininetHTTPClientInternetOptionSet read FInternetOptions write SetInternetOptions default [wHttpIo_Keep_connection];
    property  OnStatus: TAlWinInetHTTPClientStatusEvent read FOnStatus write SetOnStatus;
    property  IgnoreSecurityErrors: Boolean read FIgnoreSecurityErrors write FIgnoreSecurityErrors;
  end;

implementation

uses
  System.SysUtils,
  System.Ansistrings,
  ALCommon,
  ALString;

{***************************************************************}
procedure ALWininetHTTPCLientStatusCallback(hInternet: HINTERNET;
                                            dwContext: DWORD_PTR;
                                            dwInternetStatus: DWORD;
                                            lpvStatusInformation: LPVOID;
                                            dwStatusInformationLength: DWORD); stdcall;
begin
  if dwContext = 0 then exit;
  with TALWinInetHTTPClient(dwContext) do begin

    {fire the OnStatus event}
    if Assigned(FOnStatus) then
      FOnStatus(
        TALWininetHttpClient(dwContext),
        dwInternetStatus,
        lpvStatusInformation,
        dwStatusInformationLength);

    {fire the OnRedirect event}
    If (dwInternetStatus = INTERNET_STATUS_REDIRECT) and Assigned(OnRedirect) then
      OnRedirect(TALWinInetHttpClient(dwContext), AnsiString(PAnsiChar(lpvStatusInformation)));

  end;
end;

{**************************************}
constructor TALWinInetHTTPClient.Create;
begin
  inherited;
  FInetRoot := nil;
  FInetConnect := nil;
  FConnected := False;
  FURL:= '';
  FURLHost := '';
  FURLPath := '';
  FURLPort := INTERNET_DEFAULT_HTTP_PORT;
  FURLScheme := INTERNET_SCHEME_DEFAULT;
  FOnStatus := nil;
  FAccessType := wHttpAt_Preconfig;
  FInternetOptions := [wHttpIo_Keep_connection];
  RequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALWinInetHTTPClient)';
  FIgnoreSecurityErrors := False;
end;

{**************************************}
destructor TALWinInetHTTPClient.Destroy;
begin
  Disconnect;
  inherited;
end;

{********************************************************}
procedure TALWinInetHTTPClient.CheckError(ErrCode: DWORD);
var ln: DWORD;
    S: AnsiString;
begin
  if ErrCode <> 0 then begin
    SetLength(S, 256);
    ln := FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE,
                         Pointer(GetModuleHandle('wininet.dll')),
                         ErrCode,
                         0,
                         PAnsiChar(S),
                         Length(S),
                         nil);
    if ln = 0 then raiseLastOsError;
    SetLength(S, ln);
    raise EALHTTPClientException.CreateFmt('%s - URL:%s', [ALtrim(S), URL]);      { Do not localize }
  end;
end;

{********************************************************}
procedure TALWinInetHTTPClient.CheckError(Error: Boolean);
begin
  if Error then
    CheckError(GetLastError);
end;

{*************************************************************}
procedure TALWinInetHTTPClient.SetURL(const Value: AnsiString);
Var LSchemeName,
    LHostName,
    LUserName,
    LPassword,
    LUrlPath,
    LExtraInfo: AnsiString;
    LScheme: TInternetScheme;
    LPortNumber: integer;
begin
  If Value <> Url then Begin

    if Value <> '' then begin
      if not AlInternetCrackUrl(
               Value, // aUrl: AnsiString;
               LSchemeName, // Var LSchemeName: AnsiString;
               LHostName, // Var LHostName: AnsiString;
               LUserName, // Var LUserName: AnsiString;
               LPassword, // Var LPassword: AnsiString;
               LUrlPath, // Var LUrlPath: AnsiString;
               LExtraInfo, // Var LExtraInfo: AnsiString;
               LPortNumber) then
        raise EALHTTPClientException.CreateFmt(CALHTTPCLient_MsgInvalidURL, [Value]);
      //-----
      if ALSameText(LSchemeName, 'https') then LScheme := INTERNET_SCHEME_HTTPS
      else if ALSameText(LSchemeName, 'http') then LScheme := INTERNET_SCHEME_HTTP
      else raise Exception.Createfmt('Unknown scheme (%s)',[LSchemeName]);
      //-----
      { Here we disconnect if a new URL comes in with a
        new host...this ensures that we don't keep a
        connection to a wrong host }
      If (LHostName <> FURLHost) or
         (LPortNumber <> FURLPort) or
         (LScheme <> FURLScheme) then Disconnect;
      //-----
      FURLPort := LPortNumber;
      FURLHost := LHostName;
      FURLPath := LUrlPath + LExtraInfo;
      FURLScheme := LScheme;
    end
    else begin
      disconnect;
      FURLPort := INTERNET_DEFAULT_HTTP_PORT;
      FURLHost := '';
      FURLPath := '';
      FURLScheme := INTERNET_SCHEME_HTTP;
    end;

    Furl := Value;

  end;
end;

{**********************************************************************}
procedure TALWinInetHTTPClient.SetUsername(const NameValue: AnsiString);
begin
  If UserName <> NameValue then Disconnect;
  inherited;
end;

{**************************************************************************}
procedure TALWinInetHTTPClient.SetPassword(const PasswordValue: AnsiString);
begin
  IF Password <> PasswordValue then Disconnect;
  inherited;
end;

{*************************************}
procedure TALWinInetHTTPClient.Connect;

  {---------------------------------------------}
  Function InternalGetProxyServerName: AnsiString;
  Begin
    If (ProxyParams.ProxyServer = '') then result := ''
    else result := ProxyParams.ProxyServer + ':' + ALIntToStr(ProxyParams.ProxyPort);
  end;

  {-----------------------------------------}
  Function InternalGetProxyBypass: PAnsiChar;
  Begin
    {We should not use empty string for ProxyBypass because
     InternetOpen will use it as the proxy bypass list}
    if (ProxyParams.ProxyBypass = '') then result := nil
    else result := PAnsiChar(ProxyParams.ProxyBypass);
  end;

  {-------------------------------------------}
  Function InternalGetInternetOpenFlags: DWord;
  Begin
    Result := 0;
    if whttpIo_From_Cache in InternetOptions then Result := result or INTERNET_FLAG_FROM_CACHE;
    if whttpIo_Offline in InternetOptions then Result := result or INTERNET_FLAG_OFFLINE;
  end;

const
  AccessTypeArr: Array[TALWinInetHttpInternetOpenAccessType] of DWord = (INTERNET_OPEN_TYPE_DIRECT,
                                                                         INTERNET_OPEN_TYPE_PRECONFIG,
                                                                         INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY,
                                                                         INTERNET_OPEN_TYPE_PROXY);

var
  LSetStatusCallbackResult: PFNInternetStatusCallback;
  ProxyStr: AnsiString;
  ProxyPtr: PAnsiChar;

begin
  { Yes, but what if we're connected to a different Host/Port?? }
  { So take advantage of a cached handle, we'll assume that
    Connect(False) will be called explicitly when we're switching
    Host. To that end, SetURL always disconnects }
  if (FConnected) then Exit;

  { Also, could switch to new API introduced in IE4/Preview2}
  if InternetAttemptConnect(0) <> ERROR_SUCCESS then System.SysUtils.Abort;

  ProxyStr := InternalGetProxyServerName;
  if ProxyStr <> '' then begin
    ProxyPtr := PAnsiChar(ProxyStr);
  end else begin
    ProxyPtr := nil;
  end;

  {init FInetRoot}
  FInetRoot := InternetOpenA(PAnsiChar(RequestHeader.UserAgent),
                             AccessTypeArr[FAccessType],
                             ProxyPtr,
                             InternalGetProxyBypass,
                             InternalGetInternetOpenFlags);
  CheckError(not Assigned(FInetRoot));

  try

    {Register the callback function}
    if assigned(OnStatus) or assigned(OnRedirect) then begin
      LSetStatusCallbackResult := InternetSetStatusCallback(FInetRoot, @ALWininetHTTPCLientStatusCallback);
      CheckError(LSetStatusCallbackResult = pointer(INTERNET_INVALID_STATUS_CALLBACK));
    end;

    {init FInetConnect}
    FInetConnect := InternetConnectA(FInetRoot,
                                     PAnsiChar(FURLHost),
                                     FURLPort,
                                     PAnsiChar(UserName),
                                     PAnsiChar(Password),
                                     INTERNET_SERVICE_HTTP,
                                     0,
                                     DWORD_PTR(Self));
    CheckError(not Assigned(FInetConnect));

    {Set FConnected to true}
    FConnected := True;

  except
    InternetCloseHandle(FInetRoot);
    FInetRoot := nil;
    raise;
  end;
end;

{****************************************}
procedure TALWinInetHTTPClient.Disconnect;
begin
  if Assigned(FInetConnect) then InternetCloseHandle(FInetConnect);
  FInetConnect := nil;
  if Assigned(FInetRoot) then InternetCloseHandle(FInetRoot);
  FInetRoot := nil;
  FConnected := False;
end;

{*********************************************************************}
function TALWinInetHTTPClient.Send(const aRequestMethod: TALHTTPMethod;
                                   const aRequestDataStream: TStream): HINTERNET;

  {----------------------------------------------}
  Function InternalGetHttpOpenRequestFlags: DWord;
  Begin
    Result := 0;
    if whttpIo_CACHE_IF_NET_FAIL in InternetOptions then Result := result or INTERNET_FLAG_CACHE_IF_NET_FAIL;
    if whttpIo_HYPERLINK in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if whttpIo_IGNORE_CERT_CN_INVALID in InternetOptions then Result := result or INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
    if whttpIo_IGNORE_CERT_DATE_INVALID in InternetOptions then Result := result or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
    if whttpIo_IGNORE_REDIRECT_TO_HTTP in InternetOptions then Result := result or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;
    if whttpIo_IGNORE_REDIRECT_TO_HTTPS in InternetOptions then Result := result or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
    if whttpIo_KEEP_CONNECTION in InternetOptions then Result := result or INTERNET_FLAG_KEEP_CONNECTION;
    if whttpIo_NEED_FILE in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if whttpIo_NO_AUTH in InternetOptions then Result := result or INTERNET_FLAG_NO_AUTH;
    if whttpIo_NO_AUTO_REDIRECT in InternetOptions then Result := result or INTERNET_FLAG_NO_AUTO_REDIRECT;
    if whttpIo_NO_CACHE_WRITE in InternetOptions then Result := result or INTERNET_FLAG_NO_CACHE_WRITE;
    if whttpIo_NO_COOKIES in InternetOptions then Result := result or INTERNET_FLAG_NO_COOKIES;
    if whttpIo_NO_UI in InternetOptions then Result := result or INTERNET_FLAG_NO_UI;
    if whttpIo_PRAGMA_NOCACHE in InternetOptions then Result := result or INTERNET_FLAG_PRAGMA_NOCACHE;
    if whttpIo_RELOAD in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if whttpIo_RESYNCHRONIZE in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
    if (whttpIo_SECURE in InternetOptions) or
       (FURLScheme = INTERNET_SCHEME_HTTPS) then Result := result or INTERNET_FLAG_SECURE;
  end;

  {-------------------------------------------------}
  Function InternalGetHttpProtocolVersion: PAnsiChar;
  Begin
    If ProtocolVersion = HTTPpv_1_1 then result := 'HTTP/1.1'
    else result := 'HTTP/1.0'
  end;

  {-------------------------------------------------}
  Function InternalGetHttpOpenRequestVerb: PAnsiChar;
  Begin
    Case aRequestMethod of
      HTTPmt_Get:     Result := 'GET';
      HTTPmt_Post:    Result := 'POST';
      HTTPmt_Head:    Result := 'HEAD';
      HTTPmt_Delete:  Result := 'DELETE';
      HTTPmt_Put:     Result := 'PUT';
      HTTPmt_Trace:   Result := 'TRACE'; // <= seam to not work on Wininet ! but work on winhttp
      HTTPmt_Options: Result := 'OPTIONS'
      else raise Exception.Create('Unknown Request Method');
    end;
  end;

var LNumberOfBytesWritten: DWord;
    LDwFlags: DWORD;
    LBuffLen: Cardinal;
    LBuffSize, LLen: cardinal;
    LINBuffer: INTERNET_BUFFERSA;
    LBuffer: TMemoryStream;
    LAcceptTypes: array of PAnsiChar;
    LHeader: AnsiString;
    LOption: DWord;

begin

  { Connect }
  Connect;

  SetLength(LAcceptTypes, 2);
  LAcceptTypes[0] := PAnsiChar(RequestHeader.Accept);
  LAcceptTypes[1] := nil;

  Result := HttpOpenRequestA(FInetConnect,
                             InternalGetHttpOpenRequestVerb,
                             PAnsiChar(FURLPath),
                             InternalGetHttpProtocolVersion,
                             PAnsiChar(requestHeader.Referer),
                             Pointer(LAcceptTypes),
                             InternalGetHttpOpenRequestFlags,
                             DWORD_PTR(Self));
  CheckError(not Assigned(Result));
  try

    if FIgnoreSecurityErrors and (FURLScheme = INTERNET_SCHEME_HTTPS) then begin
      LBuffLen := SizeOf(LDwFlags);
      CheckError(not InternetQueryOptionA(Result, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@LDwFlags), LBuffLen));
      LDwFlags := LDwFlags or
        SECURITY_FLAG_IGNORE_REVOCATION or
        SECURITY_FLAG_IGNORE_UNKNOWN_CA or
        SECURITY_FLAG_IGNORE_WRONG_USAGE;
      CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_SECURITY_FLAGS, Pointer(@LDwFlags), SizeOf(LDwFlags)));
    end;

    { Timeouts }
    if ConnectTimeout > 0 then begin
      LOption := ConnectTimeout;
      CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_CONNECT_TIMEOUT, Pointer(@LOption), SizeOf(LOption)));
    end;
    if SendTimeout > 0 then begin
      LOption := SendTimeout;
      CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_SEND_TIMEOUT, Pointer(@LOption), SizeOf(LOption)));
    end;
    if ReceiveTimeout > 0 then begin
      LOption := ReceiveTimeout;
      CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_RECEIVE_TIMEOUT, Pointer(@LOption), SizeOf(LOption)));
    end;

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_PROXY_USERNAME, PAnsiChar(ProxyParams.ProxyUserName), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not InternetSetOptionA(Result, INTERNET_OPTION_PROXY_PASSWORD, PAnsiChar(ProxyParams.ProxyPassword), length(ProxyParams.ProxyPassword)));

    {set the header}
    LHeader := requestHeader.RawHeaderText;
    HttpAddRequestHeadersA(Result,
                           PAnsiChar(LHeader),
                           Length(LHeader),
                           HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);

    If assigned(aRequestDataStream) then begin
      aRequestDataStream.Position := 0;
      LBuffSize := aRequestDataStream.Size;
    end
    else LBuffSize := 0;

    if LBuffSize > UploadBufferSize then begin
      LBuffer := TMemoryStream.Create;
      try
        LBuffer.SetSize(UploadBufferSize);

        { Init Input Buffer }
        LINBuffer.dwStructSize := SizeOf(LINBuffer);
        LINBuffer.Next := nil;
        LINBuffer.lpcszHeader := nil;
        LINBuffer.dwHeadersLength := 0;
        LINBuffer.dwHeadersTotal := 0;
        LINBuffer.lpvBuffer := nil;
        LINBuffer.dwBufferLength := 0;
        LINBuffer.dwBufferTotal := LBuffSize;
        LINBuffer.dwOffsetLow := 0;
        LINBuffer.dwOffsetHigh := 0;

        { Start POST }
        CheckError(not HttpSendRequestExA(Result,
                                          @LINBuffer,
                                          nil,
                                          HSR_INITIATE or HSR_SYNC,
                                          DWORD_PTR(Self)));
        try
          while True do begin
            { Calc length of data to send }
            LLen := LBuffSize - aRequestDataStream.Position;
            if LLen > UploadBufferSize then LLen := UploadBufferSize;
            { Bail out if zip.. }
            if LLen = 0 then break;
            { Read data in buffer and write out}
            LLen := aRequestDataStream.Read(LBuffer.Memory^, LLen);
            if LLen = 0 then raise EALHTTPClientException.Create(CALHTTPCLient_MsgInvalidHTTPRequest);

            CheckError(not InternetWriteFile(Result,
                                             LBuffer.Memory,
                                             LLen,
                                             LNumberOfBytesWritten));
            if LNumberOfBytesWritten < LLen then
              aRequestDataStream.Position := aRequestDataStream.Position - (LLen - LNumberOfBytesWritten);

            { Posting Data Event }
            if Assigned(OnUploadProgress) then
              OnUploadProgress(self, aRequestDataStream.Position, LBuffSize);
          end;
        finally
          CheckError(not HttpEndRequestA(Result,
                                         nil,
                                         0,
                                         0));
        end;
      finally
        AlFreeAndNil(LBuffer);
      end;
    end

    else if LBuffSize > 0 then begin
      LBuffer := TMemoryStream.Create;
      try
        LBuffer.CopyFrom(aRequestDataStream, 0);
        CheckError(not HttpSendRequestA(Result,
                                        nil,
                                        0,
                                        LBuffer.Memory,
                                        LBuffer.size));
      finally
        AlFreeAndNil(LBuffer);
      end;
    end

    else begin
      CheckError(not HttpSendRequestA(Result,
                                      nil,
                                      0,
                                      nil,
                                      0));
    end;

  except
    InternetCloseHandle(Result);
    raise;
  end;

end;

{****************************************************************}
procedure  TALWinInetHTTPClient.Receive(const aContext: HINTERNET;
                                        const aResponseContent: TStream;
                                        const aResponseHeader: TALHTTPResponseHeader);

var LSize: DWord;
    LDownloaded: DWord;
    LStatus: DWord;
    LLen: DWord;
    LIndex: DWord;
    LContentlengthDownloaded,
    LContentLength: DWord;
    LStr: AnsiString;
    LBuffer: Tbytes;

begin

  {read the header}
  If assigned(aResponseHeader) then begin
    LSize := 4096;
    While true do begin
      LIndex := 0;
      SetLength(LStr, LSize);
      If HttpQueryInfoA(aContext,
                        HTTP_QUERY_RAW_HEADERS_CRLF,
                        pointer(LStr),
                        LSize,
                        LIndex) then begin
        SetLength(LStr, LSize);
        aResponseHeader.RawHeaderText := LStr;
        break;
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { read content-length }
  if assigned(onDownloadProgress) then begin
    LIndex := 0;
    LLen := SizeOf(LContentLength);
    if not HttpQueryInfoA(aContext,
                          HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
                          @LContentLength,
                          LLen,
                          LIndex) then LContentLength := 0;
  end;

  { Read data }
  LLen := 0;
  LContentlengthDownloaded := 0;
  repeat
    CheckError(not InternetQueryDataAvailable(aContext,
                                              LSize,
                                              0,
                                              0));

    if LSize > 0 then begin
      if dWord(length(LBuffer)) <> LSize then SetLength(LBuffer, LSize);
      CheckError(not InternetReadFile(aContext,
                                      @LBuffer[0],
                                      LSize,
                                      LDownloaded));
      aResponseContent.Writebuffer(pointer(LBuffer)^, LDownloaded);
      if Assigned(onDownloadProgress) then begin
        inc(LContentlengthDownloaded, LDownloaded);
        onDownloadProgress(self, LContentlengthDownloaded, LContentLength);
      end;
    end;
  until LSize = 0;

  { Handle error from status}
  LIndex := 0;
  LLen := SizeOf(LStatus);
  if HttpQueryInfoA(aContext,
                    HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                    @LStatus,
                    LLen,
                    LIndex) and
     (LStatus >= 300) then begin
    LSize := 256;
    While true do begin
      LIndex := 0;
      SetLength(LStr, LSize);
      if HttpQueryInfoA(aContext,
                        HTTP_QUERY_STATUS_TEXT,
                        pointer(LStr),
                        LSize,
                        LIndex) then begin
        SetLength(LStr, LSize);
        raise EALHTTPClientException.CreateFmt('%s (%d) - ''%s''', [LStr, LStatus, URL], LStatus);
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

end;

{***********************************************************}
procedure TALWinInetHTTPClient.Execute(const aUrl:AnsiString;
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
      InternetCloseHandle(LContext);
    end;

  finally
    if LRestoreRequestHeader then
      requestHeader.RawHeaderText := LOldRawRequestHeaderText;
  end;
end;

{**********************************************************************************************}
procedure TALWinInetHTTPClient.SetAccessType(const Value: TALWinInetHttpInternetOpenAccessType);
begin
  If (value <> AccessType) then begin
    Disconnect;
    FaccessType := value;
  end;
end;

{***************************************************************************************}
procedure TALWinInetHTTPClient.SetOnStatus(const Value: TAlWinInetHTTPClientStatusEvent);
begin
  Disconnect;
  FOnStatus := Value;
end;

{************************************************************************************}
procedure TALWinInetHTTPClient.SetOnRedirect(const Value: TAlHTTPClientRedirectEvent);
begin
  Disconnect;
  inherited;
end;

{************************************************************************************************}
procedure TALWinInetHTTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

{****************************************************************************************************}
procedure TALWinInetHTTPClient.SetInternetOptions(const Value: TAlWininetHTTPClientInternetOptionSet);
begin
  If Value <> FInternetOptions then begin
    If (whttpIo_From_Cache in Value) and (not (whttpIo_From_Cache in FInternetOptions)) or
       (whttpIo_From_Cache in FInternetOptions) and (not (whttpIo_From_Cache in Value)) or
       (whttpIo_Offline in Value) and (not (whttpIo_Offline in FInternetOptions)) or
       (whttpIo_Offline in FInternetOptions) and (not (whttpIo_Offline in Value)) then disconnect;
    FInternetOptions := Value;
  end;
end;

end.


