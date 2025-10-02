unit Alcinoe.HTTP.Client.WinHTTP;

interface

{$I Alcinoe.inc}

uses
  Winapi.Windows,
  Winapi.WinHTTP,
  System.Classes,
  System.Net.URLClient,
  Alcinoe.HTTP,
  Alcinoe.HTTP.Client,
  Alcinoe.Common;

type

  {-------------------------------------}
  TALWinHttpClient = class(TALHTTPClient)
  public
    type
      // ---------------
      // TOption
      THttpOption = (
        /// <summary>
        ///   Indicates that the request should be forwarded to the originating server rather than
        ///   sending a cached version of a resource from a proxy server. When this flag is used,
        ///   a "Pragma: no-cache" header is added to the request handle. When creating an HTTP/1.1
        ///   request header, a "Cache-Control: no-cache" is also added.
        /// </summary>
        Refresh,
        /// <summary>
        ///   Uses keep-alive semantics, if available, for the connection. This flag is required for Microsoft
        ///   Network (MSN), NT LAN Manager (NTLM), and other types of authentication.
        /// </summary>
        Keep_connection,
        /// <summary>
        ///   Does not automatically add cookie headers to requests, and does not automatically add returned
        ///   cookies to the cookie database.
        /// </summary>
        No_cookies,
        /// <summary>
        ///   Does not automatically handle redirection in HttpSendRequest.
        /// </summary>
        No_auto_redirect,
        /// <summary>
        ///   determine whether WinHTTP will automatically decompress response bodies with compressed Content-
        ///   Encodings. WinHTTP will also set an appropriate Accept-Encoding header, overriding any supplied
        ///   by the caller
        /// </summary>
        Decompression);
      // ------------------
      // TOptionSet
      THttpOptionSet = Set of THttpOption;
      // -----------------------
      // TAccessType
      TAccessType = (
        /// <summary>
        ///   Resolves all host names locally.
        /// </summary>
        NO_PROXY,
        /// <summary>
        ///   Retrieves the static proxy or direct configuration from the registry.
        ///   WINHTTP_ACCESS_TYPE_DEFAULT_PROXY does not inherit browser proxy settings.
        ///   WinHTTP does not share any proxy settings with Internet Explorer. This option
        ///   picks up the WinHTTP proxy configuration set by the WinHTTP Proxycfg.exe utility.
        /// </summary>
        DEFAULT_PROXY,
        /// <summary>
        ///   Passes requests to the proxy unless a proxy bypass list is supplied and the name
        ///   to be resolved bypasses the proxy. In this case, this function uses
        ///   WINHTTP_ACCESS_TYPE_NAMED_PROXY.
        /// </summary>
        NAMED_PROXY);
      // ------------
      // TStatusEvent
      /// <summary>
      ///   InternetStatus
      ///   can be one of the following value:
      ///      WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION
      ///        Closing the connection to the server. The lpvStatusInformation parameter is NULL.
      ///        This flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER
      ///        Successfully connected to the server. The lpvStatusInformation parameter contains a
      ///        pointer to an LPWSTR that indicates the IP address of the server in dotted notation.
      ///        This flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER
      ///        Connecting to the server. The lpvStatusInformation parameter contains a pointer to
      ///        an LPWSTR that indicates the IP address of the server in dotted notation. This flag
      ///        has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED
      ///        Successfully closed the connection to the server. The lpvStatusInformation parameter is
      ///        NULL. This flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
      ///        Data is available to be retrieved with WinHttpReadData. The lpvStatusInformation parameter
      ///        points to a DWORD that contains the number of bytes of data available. The dwStatusInformationLength
      ///        parameter itself is 4 (the size of a DWORD).
      ///      WINHTTP_CALLBACK_STATUS_HANDLE_CREATED
      ///        An HINTERNET handle has been created. The lpvStatusInformation parameter contains a pointer to
      ///        the HINTERNET handle. This flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING
      ///        This handle value has been terminated. The lpvStatusInformation parameter contains a
      ///        pointer to the HINTERNET handle.
      ///      WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
      ///        The response header has been received and is available with WinHttpQueryHeaders.
      ///        The lpvStatusInformation parameter is NULL.
      ///      WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE
      ///        Received an intermediate (100 level) status code message from the server. The lpvStatusInformation
      ///        parameter contains a pointer to a DWORD that indicates the status code.
      ///      WINHTTP_CALLBACK_STATUS_NAME_RESOLVED
      ///        Successfully found the IP address of the server. The lpvStatusInformation parameter contains a
      ///        pointer to an LPWSTR that indicates the name that was resolved. This flag has been deprecated
      ///        and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_READ_COMPLETE
      ///        Data was successfully read from the server. The lpvStatusInformation parameter contains a pointer
      ///        to the buffer specified in the call to WinHttpReadData. The dwStatusInformationLength parameter
      ///        contains the number of bytes read.
      ///      WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE
      ///        Waiting for the server to respond to a request. The lpvStatusInformation parameter is NULL. This
      ///        flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_REDIRECT
      ///        An HTTP request is about to automatically redirect the request. The lpvStatusInformation parameter
      ///        contains a pointer to an LPWSTR indicating the new URL. At this point, the application can read any
      ///        data returned by the server with the redirect response and can query the response headers. It can
      ///        also cancel the operation by closing the handle.
      ///      WINHTTP_CALLBACK_STATUS_REQUEST_ERROR
      ///        An error occurred while sending an HTTP request. The lpvStatusInformation parameter contains a pointer
      ///        to a WINHTTP_ASYNC_RESULT structure, of which the dwResult member indicates the return value of the
      ///        function and any error codes that apply.
      ///      WINHTTP_CALLBACK_STATUS_REQUEST_SENT
      ///        Successfully sent the information request to the server. The lpvStatusInformation parameter contains a
      ///        pointer to a DWORD indicating the number of bytes sent. This flag has been deprecated and may be
      ///        removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_RESOLVING_NAME
      ///        Looking up the IP address of a server name. The lpvStatusInformation parameter contains a pointer to
      ///        the server name being resolved. This flag has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED
      ///        Successfully received a response from the server. The lpvStatusInformation parameter contains a pointer
      ///        to a DWORD indicating the number of bytes received. This flag has been deprecated and may be removed
      ///        in a future release.
      ///      WINHTTP_CALLBACK_STATUS_SECURE_FAILURE
      ///        One or more errors were encountered while retrieving a Secure Sockets Layer (SSL) certificate from the
      ///        server. The lpvStatusInformation parameter contains a flag. For more information, see the description
      ///        for lpvStatusInformation.
      ///      WINHTTP_CALLBACK_STATUS_SENDING_REQUEST
      ///        Sending the information request to the server. The lpvStatusInformation parameter is NULL. This flag
      ///        has been deprecated and may be removed in a future release.
      ///      WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
      ///        The request completed successfully. The lpvStatusInformation parameter is NULL.
      ///      WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
      ///        Data was successfully written to the server. The lpvStatusInformation parameter contains a pointer to
      ///        a DWORD that indicates the number of bytes written.
      ///
      ///   StatusInformation
      ///   A pointer to a buffer that specifies information pertinent to this call to the callback function. The format of this data depends on
      ///   the value of the InternetStatus parameter. For more information, see InternetStatus.
      ///
      ///   If the InternetStatus parameter is WINHTTP_CALLBACK_STATUS_SECURE_FAILURE, this parameter can be one of the following values:
      ///      WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED
      ///        Certification revocation checking has been enabled, but the revocation check failed to verify whether a
      ///        certificate has been revoked. The server used to check for revocation might be unreachable.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT SSL
      ///        certificate is invalid.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED SSL
      ///        certificate was revoked.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA The
      ///        function is unfamiliar with the Certificate Authority that generated the server's certificate.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID SSL
      ///        certificate common name (host name field) is incorrect, for example, if you entered www.microsoft.com
      ///        and the common name on the certificate says www.msn.com.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID SSL
      ///        certificate date that was received from the server is bad. The certificate is expired.
      ///      WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR
      ///        The application experienced an internal error loading the SSL libraries.
      ///
      ///   StatusInformationLength
      ///   Size of the data pointed to by StatusInformation.
      /// </summary>
      TStatusEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: LPVOID; StatusInformationLength: DWord) of object;
  private
    class var FWinHttpModuleHandle: HMODULE;
  private
    FAccessType: TAccessType;
    FHttpOptions: THttpOptionSet;
    FConnected: Boolean;
    FRawURL: AnsiString;
    FURLScheme: INTERNET_SCHEME;
    FURLHost: AnsiString;
    FURLPort: INTERNET_PORT;
    FURLPath: AnsiString;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatus: TStatusEvent;
    procedure SetAccessType(const Value: TAccessType);
    procedure SetOnStatus(const Value: TStatusEvent);
  protected
    procedure CheckApiErrorCode(const AExtraInfo: String; const AErrorCode: DWORD); overload;
    procedure CheckApiBoolean(const AExtraInfo: String; const ABoolean: Boolean); overload;
    function  CheckApiPointer(const AExtraInfo: String; const APointer: Pointer): Pointer; overload;
    procedure SetURL(const AValue: AnsiString);
    procedure SetUsername(const AValue: AnsiString); override;
    procedure SetPassword(const AValue: AnsiString); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure SetOnRedirect(const Value: TALHTTPClient.TRedirectEvent); override;
    function Execute(
               const AUrl: AnsiString;
               const AVerb: AnsiString;
               const ABodyStream: TStream;
               const AHeaders: TALNameValueArrayA;
               const AResponseBodyStream: TStream): TALHTTPClientResponse; override;
    function Send(
               const AVerb: AnsiString;
               const ABodyStream: TStream): HINTERNET; virtual;
    procedure Receive(
                const AContext: HINTERNET;
                const AResponse: TALHTTPClientResponse); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    property AccessType: TAccessType read FAccessType write SetAccessType;
    property HttpOptions: THttpOptionSet read FHttpOptions write FHttpOptions;
    property OnStatus: TStatusEvent read FOnStatus write SetOnStatus;
  end;

Function ALCreateWinHttpClient(Const AAllowcookies: Boolean = False): TALWinHttpClient;
function ALAcquireKeepAliveWinHttpClient(const AURI: TUri): TALWinHttpClient;
procedure ALReleaseKeepAliveWinHttpClient(const AURI: TUri; var AHTTPClient: TALWinHttpClient);
procedure ALReleaseAllKeepAliveWinHttpClients;

const
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if WINHTTP_PROTOCOL_FLAG_HTTP3 was not added in Winapi.WinHTTP.pas and adjust the IFDEF'}
  {$ENDIF}
  WINHTTP_PROTOCOL_FLAG_HTTP3 = $2;

implementation

uses
  System.SysUtils,
  System.Ansistrings,
  System.Types,
  System.Generics.Collections,
  Alcinoe.WinApi.Windows,
  Alcinoe.Url,
  Alcinoe.StringUtils;

{*}
var
  ALWinHttpClientKeepAlives: TObjectDictionary<AnsiString, TobjectList<TALWinHttpClient>>;

{*************************************************************************************}
Function ALCreateWinHttpClient(Const AAllowcookies: Boolean = False): TALWinHttpClient;
Begin
  Result := TALWinHttpClient.Create;
  With Result do begin
    If AAllowcookies then HttpOptions := HttpOptions - [TALWinHttpClient.THttpOption.No_cookies];
    ReceiveTimeout := ALCreateHttpClientReceiveTimeout;
    sendTimeout := ALCreateHttpClientSendTimeout;
    ConnectTimeout := ALCreateHttpClientConnectTimeout;
  end;
end;

{***************************************************************************}
function ALAcquireKeepAliveWinHttpClient(const AURI: TUri): TALWinHttpClient;
begin
  ALMonitorEnter(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveWinHttpClient'{$ENDIF});
  try
    var LList: TobjectList<TALWinHttpClient>;
    if ALWinHttpClientKeepAlives.TryGetValue(AlLowerCase(ansiString(AURI.Scheme)) + '://' + AlLowerCase(ansiString(AURI.Host)) + ':' + ALIntToStrA(AURI.port), LList) then begin
      if LList.Count > 0 then result := LList.ExtractItem(LList.Last, TDirection.FromEnd)
      else result := ALCreateWinHttpClient;
    end
    else result := ALCreateWinHttpClient;
  finally
    ALMonitorExit(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALAcquireKeepAliveWinHttpClient'{$ENDIF});
  end;
end;

{*********************************************************************************************}
procedure ALReleaseKeepAliveWinHttpClient(const AURI: TUri; var AHTTPClient: TALWinHttpClient);
begin
  ALMonitorEnter(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveWinHttpClient'{$ENDIF});
  try
    var LList: TobjectList<TALWinHttpClient>;
    if ALWinHttpClientKeepAlives.TryGetValue(AlLowerCase(ansiString(AURI.Scheme)) + '://' + AlLowerCase(ansiString(AURI.Host)) + ':' + ALIntToStrA(AURI.port), LList) then begin
      while LList.Count >= ALMaxKeepAliveHttpClientPerHost do
        LList.Delete(0);
      LList.Add(AHTTPClient);
      AHTTPClient := nil;
    end
    else begin
      LList := TobjectList<TALWinHttpClient>.create(true{aOwnObject});
      try
        LList.Add(AHTTPClient);
        AHTTPClient := nil;
        if not ALWinHttpClientKeepAlives.TryAdd(AlLowerCase(ansiString(AURI.Scheme)) + '://' + AlLowerCase(ansiString(AURI.Host)) + ':' + ALIntToStrA(AURI.port), LList) then ALFreeAndNil(LList);
      except
        ALFreeAndNil(LList);
        raise;
      end;
    end;
  finally
    ALMonitorExit(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseKeepAliveWinHttpClient'{$ENDIF});
  end;
end;

{********************************************}
procedure ALReleaseAllKeepAliveWinHttpClients;
begin
  ALMonitorEnter(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveWinHttpClients'{$ENDIF});
  try
    ALWinHttpClientKeepAlives.Clear;
  finally
    ALMonitorExit(ALWinHttpClientKeepAlives{$IF defined(DEBUG)}, 'ALReleaseAllKeepAliveWinHttpClients'{$ENDIF});
  end;
end;

{**************************************}
procedure ALWinHTTPClientStatusCallback(
            hInternet: HINTERNET;
            dwContext: DWORD_PTR;
            dwInternetStatus: DWORD;
            lpvStatusInformation: LPVOID;
            dwStatusInformationLength: DWORD); stdcall;
begin
  if dwContext = 0 then exit;
  with TALWinHttpClient(dwContext) do begin

    // fire the OnStatus event
    if Assigned(FOnStatus) then
      FOnStatus(
        TALWinHttpClient(dwContext),
        dwInternetStatus,
        lpvStatusInformation,
        dwStatusInformationLength);

    // fire the OnRedirect event
    If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_REDIRECT) and Assigned(OnRedirect) then
      OnRedirect(TALWinHttpClient(dwContext), AnsiString(PWideChar(lpvStatusInformation)));

  end;
end;

{**********************************}
constructor TALWinHttpClient.Create;
begin
  inherited;
  FAccessType := TAccessType.NO_PROXY;
  FHttpOptions := [THttpOption.Keep_connection, THttpOption.No_cookies, THttpOption.No_auto_redirect, THttpOption.Decompression];
  FConnected := False;
  FRawURL:= '';
  FURLScheme := INTERNET_SCHEME_HTTP;
  FURLHost := '';
  FURLPort := INTERNET_DEFAULT_HTTP_PORT;
  FURLPath := '';
  FInetRoot := nil;
  FInetConnect := nil;
  FOnStatus := nil;
  RequestHeaders.UserAgent := 'ALWinHttpClient/1.0';
end;

{**********************************}
destructor TALWinHttpClient.Destroy;
begin
  Disconnect;
  inherited;
end;

{**********************************************************************************************}
procedure TALWinHttpClient.CheckApiErrorCode(const AExtraInfo: String; const AErrorCode: DWORD);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _doCheckApiErrorCode;
  Begin
    ALCheckWinApiErrorCode(
      AExtraInfo + ' - ' + String(FRawUrl), // const AExtraInfo: String;
      AErrorCode, // const AErrorCode: DWORD;
      FWinHttpModuleHandle); // const AModuleHandle: HMODULE = 0)
  End;

begin
  if AErrorCode <> 0 then
    _doCheckApiErrorCode;
end;

{********************************************************************************************}
procedure TALWinHttpClient.CheckApiBoolean(const AExtraInfo: String; const ABoolean: Boolean);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _doCheckApiBoolean;
  Begin
    ALCheckWinApiBoolean(
      AExtraInfo + ' - ' + String(FRawUrl), // const AExtraInfo: String;
      ABoolean, // const ABoolean: Boolean;
      FWinHttpModuleHandle); // const AModuleHandle: HMODULE = 0)
  End;

begin
  If not ABoolean then
    _doCheckApiBoolean
end;

{****************************************************************************************************}
function TALWinHttpClient.CheckApiPointer(const AExtraInfo: String; const APointer: Pointer): Pointer;
begin
  CheckApiBoolean(AExtraInfo, APointer <> nil);
  Result := APointer;
end;

{**********************************************************}
procedure TALWinHttpClient.SetURL(const AValue: AnsiString);
begin
  If AValue <> FRawURL then Begin

    if AValue <> '' then begin
      var LCookedUrl := TALCookedUrlA.create(AValue);
      Try
        if (LCookedUrl.Scheme = '') or
           (LCookedUrl.Host = '') or
           (LCookedUrl.Port = 0) then
          raise Exception.CreateFmt('Invalid url "%s"', [AValue]);
        //--
        var LScheme: INTERNET_SCHEME;
        if ALSameTextA(LCookedUrl.Scheme, 'https') then LScheme := INTERNET_SCHEME_HTTPS
        else if ALSameTextA(LCookedUrl.Scheme, 'http') then LScheme := INTERNET_SCHEME_HTTP
        else raise Exception.Createfmt('Unknown scheme (%s)',[LCookedUrl.Scheme]);
        //--
        // Here we disconnect if a new URL comes in with a
        // new host...this ensures that we don't keep a
        // connection to a wrong host }
        If (LCookedUrl.Host <> FURLHost) or
           (LCookedUrl.Port <> FURLPort) or
           (LScheme <> FURLScheme) then Disconnect;
        //--
        FURLScheme := LScheme;
        FURLHost := LCookedUrl.Host;
        FURLPort := LCookedUrl.Port;
        FURLPath := LCookedUrl.Path;
        if LCookedUrl.QueryString <> '' then FURLPath := FURLPath + '?' + LCookedUrl.QueryString;
      finally
        ALFreeAndNil(LCookedUrl);
      End;
    end
    else begin
      disconnect;
      FURLScheme := INTERNET_SCHEME_HTTP;
      FURLHost := '';
      FURLPort := INTERNET_DEFAULT_HTTP_PORT;
      FURLPath := '';
    end;

    FRawURL := AValue;

  end;
end;

{***************************************************************}
procedure TALWinHttpClient.SetUsername(const AValue: AnsiString);
begin
  If UserName <> AValue then Disconnect;
  inherited;
end;

{***************************************************************}
procedure TALWinHttpClient.SetPassword(const AValue: AnsiString);
begin
  IF Password <> AValue then Disconnect;
  inherited;
end;

{*********************************}
procedure TALWinHttpClient.Connect;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function GetProxyServerName: String;
  Begin
    If (FaccessType <> TAccessType.NAMED_PROXY) or
       (ProxyParams.ProxyServer = '') then result := '' // WINHTTP_NO_PROXY_NAME = nil
    else result := string(ProxyParams.ProxyServer) + ':' + String(ALIntToStrA(ProxyParams.ProxyPort));
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function GetProxyBypass: String;
  Begin
    // We should not use empty string for ProxyBypass because
    // InternetOpen will use it as the proxy bypass list
    if (FaccessType <> TAccessType.NAMED_PROXY) or
       (ProxyParams.ProxyBypass = '') then result := '' // WINHTTP_NO_PROXY_BYPASS = nil
    else result := String(ProxyParams.ProxyBypass);
  end;

const
  AccessTypeArr: Array[TAccessType] of DWord = (
                   WINHTTP_ACCESS_TYPE_NO_PROXY,
                   WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                   WINHTTP_ACCESS_TYPE_NAMED_PROXY);

begin
  // Yes, but what if we're connected to a different Host/Port??
  // So take advantage of a cached handle, we'll assume that
  // Connect(False) will be called explicitly when we're switching
  // Host. To that end, SetURL always disconnects
  if (FConnected) then Exit;

  // Init FInetRoot
  FInetRoot := CheckApiPointer(
                 'WinHttpOpen',
                 WinHttpOpen(
                   nil, // _In_opt_ LPCWSTR pwszUserAgent,
                   AccessTypeArr[FAccessType], // _In_     DWORD   dwAccessType,
                   PWideChar(GetProxyServerName), // _In_     LPCWSTR pwszProxyName,
                   PWideChar(GetProxyBypass), // _In_     LPCWSTR pwszProxyBypass,
                   0)); // _In_     DWORD   dwFlags
  try

    // Register the callback function
    if assigned(OnStatus) or assigned(OnRedirect) then begin
      var LSetStatusCallbackResult := WinHttpSetStatusCallback(
                                        FInetRoot, // _In_       HINTERNET               hInternet,
                                        @ALWinHTTPClientStatusCallback, // _In_       WINHTTP_STATUS_CALLBACK lpfnInternetCallback,
                                        WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, // _In_       DWORD                   dwNotificationFlags,
                                        0); // _Reserved_ DWORD_PTR               dwReserved
      CheckApiBoolean('WinHttpSetStatusCallback', @LSetStatusCallbackResult <> @WINHTTP_INVALID_STATUS_CALLBACK);
    end;

    // Init FInetConnect
    FInetConnect := CheckApiPointer(
                      'WinHttpConnect',
                      WinHttpConnect(
                        FInetRoot, // _In_       HINTERNET     hSession,
                        PWideChar(String(FURLHost)), // _In_       LPCWSTR       pswzServerName,
                        FURLPort, // _In_       INTERNET_PORT nServerPort,
                        0)); //  _Reserved_ DWORD         dwReserved

    // Set FConnected to true
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

{*****************************}
function TALWinHttpClient.Send(
           const AVerb: AnsiString;
           const ABodyStream: TStream): HINTERNET;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function GetHttpOpenRequestFlags: DWord;
  Begin
    Result := WINHTTP_FLAG_ESCAPE_DISABLE or WINHTTP_FLAG_ESCAPE_DISABLE_QUERY;
    If THttpOption.REFRESH in HttpOptions then Result := result or WINHTTP_FLAG_REFRESH;
    if FURLScheme = INTERNET_SCHEME_HTTPS then Result := result or WINHTTP_FLAG_SECURE;
  end;

begin

  // Connect
  Connect;

  var LStrProtocolVersion: String;
  case ProtocolVersion of
    TALHTTPVersion.v3,
    TALHTTPVersion.v2,
    TALHTTPVersion.v1_1: LStrProtocolVersion := ''; // If this parameter is NULL, the function uses HTTP/1.1
    TALHTTPVersion.v1_0: LStrProtocolVersion := 'HTTP/1.0';
    else raise Exception.Create('Unsupported protocol version');
  end;

  Result := CheckApiPointer(
              'WinHttpOpenRequest',
              WinHttpOpenRequest(
                FInetConnect, // _In_ HINTERNET hConnect,
                PWideChar(String(AVerb)), //  _In_ LPCWSTR   pwszVerb,
                PWideChar(String(FURLPath)), // _In_ LPCWSTR   pwszObjectName,
                PWideChar(LStrProtocolVersion), // _In_ LPCWSTR   pwszVersion,
                WINHTTP_NO_REFERER, // _In_ LPCWSTR   pwszReferrer,
                WINHTTP_DEFAULT_ACCEPT_TYPES, // _In_ LPCWSTR   *ppwszAcceptTypes,
                GetHttpOpenRequestFlags)); // _In_ DWORD     dwFlags

  try

    // Allow HTTP/2 (keeps HTTP/1.1 as fallback)
    if ProtocolVersion = TALHTTPVersion.v2 then begin
      var LOption: DWord := WINHTTP_PROTOCOL_FLAG_HTTP2;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_ENABLE_HTTP_PROTOCOL , Pointer(@LOption), SizeOf(LOption)));
    end

    // Allow HTTP/3 (keeps HTTP/1.1 as fallback)
    else if ProtocolVersion = TALHTTPVersion.v3 then begin
      var LOption: DWord := WINHTTP_PROTOCOL_FLAG_HTTP3;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_ENABLE_HTTP_PROTOCOL , Pointer(@LOption), SizeOf(LOption)));
    end;

    // Timeouts
    if ConnectTimeout > 0 then begin
      var LOption: DWord := ConnectTimeout;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_CONNECT_TIMEOUT , Pointer(@LOption), SizeOf(LOption)));
    end;
    if SendTimeout > 0 then begin
      var LOption: DWord := SendTimeout;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_SEND_TIMEOUT , Pointer(@LOption), SizeOf(LOption)));
    end;
    if ReceiveTimeout > 0 then begin
      var LOption: DWord := ReceiveTimeout;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_RECEIVE_TIMEOUT, Pointer(@LOption), SizeOf(LOption)));
    end;

    // proxy user name and password
    If proxyParams.ProxyUserName <> '' then CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_PROXY_USERNAME, PWidechar(String(ProxyParams.ProxyUserName)), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_PROXY_PASSWORD, PWidechar(String(ProxyParams.ProxyPassword)), length(ProxyParams.ProxyPassword)));

    // Keep_connection, No_cookies, No_auto_redirect and Decompression
    If not (THttpOption.Keep_connection in HttpOptions) then begin
      var LOption: DWord := WINHTTP_DISABLE_KEEP_ALIVE;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;
    If (THttpOption.No_cookies in HttpOptions) then begin
      var LOption: DWord := WINHTTP_DISABLE_COOKIES;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;
    If (THttpOption.No_auto_redirect in HttpOptions) then begin
      var LOption: DWord := WINHTTP_DISABLE_REDIRECTS;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LOption), sizeof(LOption)));
    end;
    If (THttpOption.Decompression in HttpOptions) then begin
      var LOption: DWord := WINHTTP_DECOMPRESSION_FLAG_ALL;
      CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_DECOMPRESSION, Pointer(@LOption), sizeof(LOption)));
    end;

    // Set the header
    var LHeader: String := String(RequestHeaders.RawHeaderText);
    CheckApiBoolean(
      'WinHttpAddRequestHeaders',
      WinHttpAddRequestHeaders(
        Result, // _In_ HINTERNET hRequest,
        PWideChar(LHeader), // _In_ LPCWSTR   pwszHeaders,
        Length(LHeader),  // _In_ DWORD     dwHeadersLength,
        WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD)); // _In_ DWORD     dwModifiers

    var LBuffSizeInt64: Int64;
    If assigned(ABodyStream) then begin
      ABodyStream.Position := 0;
      LBuffSizeInt64 := ABodyStream.Size;
    end
    else LBuffSizeInt64 := 0;

    const LUploadBufferSize = 64 * 1024;  // Usual TCP Window Size
    if LBuffSizeInt64 > LUploadBufferSize then begin
      var LBuffer := TMemoryStream.Create;
      try
        LBuffer.SetSize(LUploadBufferSize);
        var LCertContextSet := False;
        var LErrCode: DWORD;
        repeat
          var LBuffSizeDWord: Dword;
          if LBuffSizeInt64 > High(DWORD) then LBuffSizeDWord := WINHTTP_IGNORE_REQUEST_TOTAL_LENGTH
          else LBuffSizeDWord := LBuffSizeInt64;
          if not WinHttpSendRequest(
                   Result, // _In_     HINTERNET hRequest,
                   WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                   0, // _In_     DWORD     dwHeadersLength,
                   nil, // _In_opt_ LPVOID    lpOptional,
                   0,  //  _In_     DWORD     dwOptionalLength,
                   LBuffSizeDWord, // _In_     DWORD     dwTotalLength,
                   DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
            LErrCode := GetLastError;
            if (not LCertContextSet) and (LErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
              CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
              LCertContextSet := True;
            end
            else
              CheckApiErrorCode('WinHttpSendRequest', LErrCode);
          end
          else LErrCode := 0;
        until LErrCode = 0;
        try

          while True do begin
            var LLenInt64: Int64 := LBuffSizeInt64 - ABodyStream.Position;
            if LLenInt64 > LUploadBufferSize then LLenInt64 := LUploadBufferSize;
            if LLenInt64 = 0 then break;
            LLenInt64 := ABodyStream.Read(LBuffer.Memory^, LLenInt64);
            if LLenInt64 = 0 then raise Exception.Create('Unexpected EOF reading request body');
            var LNumberOfBytesWritten: DWord;
            var LLenDword: DWord := LLenInt64;
            CheckApiBoolean(
              'WinHttpWriteData',
              WinHttpWriteData(
                Result, // _In_  HINTERNET hRequest,
                LBuffer.Memory^, // _In_  LPCVOID   lpBuffer,
                LLenDWord, // _In_  DWORD     dwNumberOfBytesToWrite,
                @LNumberOfBytesWritten)); // _Out_ LPDWORD   lpdwNumberOfBytesWritten
            if LNumberOfBytesWritten < LLenInt64 then
              ABodyStream.Position := ABodyStream.Position - (LLenInt64 - LNumberOfBytesWritten);
            if Assigned(OnUploadProgress) then
              OnUploadProgress(self, ABodyStream.Position, LBuffSizeInt64);
          end;

        finally
          CheckApiBoolean('WinHttpReceiveResponse', WinHttpReceiveResponse(Result, nil));
        end;
      finally
        AlFreeAndNil(LBuffer);
      end;
    end

    else if LBuffSizeInt64 > 0 then begin
      var LBuffer := TMemoryStream.Create;
      try
        LBuffer.CopyFrom(ABodyStream, 0);
        var LCertContextSet := False;
        var LErrCode: DWORD;
        repeat
          if not WinHttpSendRequest(
                   Result, // _In_     HINTERNET hRequest,
                   WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                   0, // _In_     DWORD     dwHeadersLength,
                   LBuffer.Memory, // _In_opt_ LPVOID    lpOptional,
                   LBuffer.Size, //  _In_     DWORD     dwOptionalLength,
                   LBuffer.Size, // _In_     DWORD     dwTotalLength,
                   DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
            LErrCode := GetLastError;
            if (not LCertContextSet) and (LErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
              CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
              LCertContextSet := True;
            end
            else
              CheckApiErrorCode('WinHttpSendRequest', LErrCode);
          end
          else LErrCode := 0;
        until LErrCode = 0;
        CheckApiBoolean('WinHttpReceiveResponse', WinHttpReceiveResponse(Result, nil));
      finally
        AlFreeAndNil(LBuffer);
      end;
    end

    else begin
      var LCertContextSet := False;
      var LErrCode: DWORD;
      repeat
        if not WinHttpSendRequest(
                 Result, // _In_     HINTERNET hRequest,
                 WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                 0, // _In_     DWORD     dwHeadersLength,
                 nil, // _In_opt_ LPVOID    lpOptional,
                 0, //  _In_     DWORD     dwOptionalLength,
                 0, // _In_     DWORD     dwTotalLength,
                 DWORD_PTR(self)) then begin // _In_     DWORD_PTR dwContext
          LErrCode := GetLastError;
          if (not LCertContextSet) and (LErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
            CheckApiBoolean('WinHttpSetOption', WinHttpSetOption(Result, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
            LCertContextSet := True;
          end
          else
            CheckApiErrorCode('WinHttpSendRequest', LErrCode);
        end
        else LErrCode := 0;
      until LErrCode = 0;
      CheckApiBoolean('WinHttpReceiveResponse', WinHttpReceiveResponse(Result, nil));
    end;

  except
    WinHttpCloseHandle(Result);
    raise;
  end;

end;

{*********************************}
procedure TALWinHttpClient.Receive(
            const AContext: HINTERNET;
            const AResponse: TALHTTPClientResponse);
begin

  // Read the header
  var LSize: DWord := 4096 * sizeof(Char);
  While true do begin
    var LWideStr: String;
    SetLength(LWideStr, LSize div sizeof(Char));
    If WinHttpQueryHeaders(
         AContext, // _In_     HINTERNET hRequest,
         WINHTTP_QUERY_RAW_HEADERS_CRLF, // _In_     DWORD     dwInfoLevel,
         WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
         pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
         LSize, // _Inout_  LPDWORD   lpdwBufferLength,
         WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
      SetLength(LWideStr, LSize div sizeof(Char));
      AResponse.Headers.RawHeaderText := AnsiString(LWideStr);
      break;
    end
    else CheckApiBoolean('WinHttpQueryHeaders', GetLastError = ERROR_INSUFFICIENT_BUFFER);
  end;

  // Read status code
  var LStatus: DWORD;
  var LLen: DWord := SizeOf(LStatus);
  CheckApiBoolean(
    'WinHttpQueryHeaders',
    WinHttpQueryHeaders(
      AContext, // hRequest: HINTERNET;
      WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER, // dwInfoLevel: DWORD;
      WINHTTP_HEADER_NAME_BY_INDEX, // pwszName: LPCWSTR;
      @LStatus, // lpBuffer: Pointer;
      LLen, // var lpdwBufferLength: DWORD;
      WINHTTP_NO_HEADER_INDEX)); // lpdwIndex: LPDWORD
  AResponse.StatusCode := LStatus;

  // Read the reason
  LSize := 32  * sizeof(Char);
  While true do begin
    var LWideStr: String;
    SetLength(LWideStr, LSize div sizeof(Char));
    If WinHttpQueryHeaders(
         AContext, // _In_     HINTERNET hRequest,
         WINHTTP_QUERY_STATUS_TEXT, // _In_     DWORD     dwInfoLevel,
         WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
         pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
         LSize, // _Inout_  LPDWORD   lpdwBufferLength,
         WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
      SetLength(LWideStr, LSize div sizeof(Char));
      AResponse.Reason := AnsiString(LWideStr);
      break;
    end
    else CheckApiBoolean('WinHttpQueryHeaders', GetLastError = ERROR_INSUFFICIENT_BUFFER);
  end;

  // Read the protocol version
  var LUsed: DWord;
  LLen := SizeOf(LUsed);
  CheckApiBoolean(
    'WinHttpQueryOption',
    WinHttpQueryOption(
      AContext, // hInternet: HINTERNET;
      WINHTTP_OPTION_HTTP_PROTOCOL_USED, // dwOption: DWORD;
      LUsed, // out lpBuffer;
      LLen)); // var lpdwBufferLength: DWORD
  if (LUsed and WINHTTP_PROTOCOL_FLAG_HTTP3) <> 0 then AResponse.Version := TALHttpVersion.v3
  else if (LUsed and WINHTTP_PROTOCOL_FLAG_HTTP2) <> 0 then AResponse.Version := TALHttpVersion.v2
  else begin
    LSize := 9  * sizeof(Char);
    While true do begin
      var LWideStr: String;
      SetLength(LWideStr, LSize div sizeof(Char));
      If WinHttpQueryHeaders(
           AContext, // _In_     HINTERNET hRequest,
           WINHTTP_QUERY_VERSION, // _In_     DWORD     dwInfoLevel,
           WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
           pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
           LSize, // _Inout_  LPDWORD   lpdwBufferLength,
           WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
        SetLength(LWideStr, LSize div sizeof(Char));
        if ALSameTextW(LWideStr, 'HTTP/1.1') then AResponse.Version := TALHttpVersion.v1_1
        else if ALSameTextW(LWideStr, 'HTTP/1.0') then AResponse.Version := TALHttpVersion.v1_0
        else AResponse.Version := TALHttpVersion.v0_9; // extremely rare; fallback
        break;
      end
      else CheckApiBoolean('WinHttpQueryHeaders', GetLastError = ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  // Read content-length
  var LContentLengthInt64: Int64 := 0;
  if Assigned(onDownloadProgress) then begin
    var LContentLengthDWord: DWord;
    LLen := SizeOf(LContentLengthDWord);
    if not WinHttpQueryHeaders(
             AContext, // _In_     HINTERNET hRequest,
             WINHTTP_QUERY_CONTENT_LENGTH or WINHTTP_QUERY_FLAG_NUMBER, // _In_     DWORD     dwInfoLevel,
             WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
             @LContentLengthDWord, // _Out_    LPVOID    lpBuffer,
             LLen, // _Inout_  LPDWORD   lpdwBufferLength,
             WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
      LSize := 19  * sizeof(Char);
      While true do begin
        var LWideStr: String;
        SetLength(LWideStr, LSize div sizeof(Char));
        If WinHttpQueryHeaders(
             AContext, // _In_     HINTERNET hRequest,
             WINHTTP_QUERY_CONTENT_LENGTH, // _In_     DWORD     dwInfoLevel,
             WINHTTP_HEADER_NAME_BY_INDEX, // _In_opt_ LPCWSTR   pwszName,
             pointer(LWideStr), // _Out_    LPVOID    lpBuffer,
             LSize, // _Inout_  LPDWORD   lpdwBufferLength,
             WINHTTP_NO_HEADER_INDEX) then begin // _Inout_  LPDWORD   lpdwIndex
          SetLength(LWideStr, LSize div sizeof(Char));
          If not ALTryStrToInt64(LWideStr, LContentLengthInt64) then LContentLengthInt64 := 0;
          break;
        end
        else if GetLastError = ERROR_WINHTTP_HEADER_NOT_FOUND then begin
          LContentLengthInt64 := 0;
          break;
        end
        else CheckApiBoolean('WinHttpQueryHeaders', GetLastError = ERROR_INSUFFICIENT_BUFFER);
      end;
    end
    else LContentLengthInt64 := LContentLengthDWord;
  end;

  // Read data
  LLen := 0;
  var LContentlengthDownloaded: DWord := 0;
  var LBuffer: Tbytes := [];
  repeat
    CheckApiBoolean('WinHttpQueryDataAvailable', WinHttpQueryDataAvailable(AContext, @LSize));
    if LSize > 0 then begin
      if dword(length(LBuffer)) < LSize then SetLength(LBuffer, LSize);
      var LDownloaded: DWord;
      CheckApiBoolean(
        'WinHttpReadData',
        WinHttpReadData(
          AContext, // _In_  HINTERNET hRequest,
          LBuffer[0], // _Out_ LPVOID    lpBuffer,
          LSize, // _In_  DWORD     dwNumberOfBytesToRead,
          @LDownloaded)); // _Out_ LPDWORD   lpdwNumberOfBytesRead
      AResponse.BodyStream.WriteBuffer(pointer(LBuffer)^, LDownloaded);
      if Assigned(onDownloadProgress) then begin
        inc(LContentlengthDownloaded, LDownloaded);
        onDownloadProgress(self, LContentlengthDownloaded, LContentLengthInt64);
      end;
    end;
  until LSize = 0;

end;

{********************************}
function TALWinHttpClient.Execute(
           const AUrl: AnsiString;
           const AVerb: AnsiString;
           const ABodyStream: TStream;
           const AHeaders: TALNameValueArrayA;
           const AResponseBodyStream: TStream): TALHTTPClientResponse;
begin
  if AURL = '' then raise Exception.Create('Empty URL');
  SetURL(AUrl);

  Var LPrevHeaders: TALNameValueArrayA;
  Setlength(LPrevHeaders, length(AHeaders));
  for var I := Low(AHeaders) to High(AHeaders) do begin
    LPrevHeaders[I] := TALNameValuePairA.Create(AHeaders[I].Name, RequestHeaders.Values[AHeaders[I].Name]);
    RequestHeaders.Values[AHeaders[I].Name] := AHeaders[I].Value;
  end;
  // Starting in Windows Vista and Windows Server 2008, WinHttp supports
  // uploading files up to the size of a LARGE_INTEGER (2^64 bytes) using the
  // Content-Length header. Payload lengths specified in the call to
  // WinHttpSendRequest are limited to the size of a DWORD (2^32 bytes). To
  // upload data to a URL larger than a DWORD, the application must provide the
  // length in the Content-Length header of the request.
  var LPrevContentLength := RequestHeaders.ContentLength;
  if ABodyStream = nil then RequestHeaders.ContentLength := '0'
  else begin
    ABodyStream.Position := 0;
    RequestHeaders.ContentLength := ALintToStrA(ABodyStream.Size);
  end;

  try

    Result := TALHTTPClientResponse.Create;
    Try

      if AResponseBodyStream <> nil then begin
        Result.BodyStream := AResponseBodyStream;
        Result.ownsBodyStream := False;
      end;

      var LContext: HINTERNET := Send(AVerb, ABodyStream);
      try
        Receive(LContext, Result);
      finally
        WinHttpCloseHandle(LContext);
      end;

    except
      ALFreeAndNil(Result);
      Raise;
    End;

  finally
    for var I := Low(LPrevHeaders) to High(LPrevHeaders) do
      RequestHeaders.Values[LPrevHeaders[I].Name] := LPrevHeaders[I].Value;
    RequestHeaders.ContentLength := LPrevContentLength;
  end;
end;

{*****************************************************************}
procedure TALWinHttpClient.SetAccessType(const Value: TAccessType);
begin
  If (value <> AccessType) then begin
    Disconnect;
    FaccessType := value;
  end;
end;

{****************************************************************}
procedure TALWinHttpClient.SetOnStatus(const Value: TStatusEvent);
begin
  Disconnect;
  FOnStatus := Value;
end;

{**********************************************************************************}
procedure TALWinHttpClient.SetOnRedirect(const Value: TALHTTPClient.TRedirectEvent);
begin
  disconnect;
  inherited;
end;

{********************************************************************************************}
procedure TALWinHttpClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.WinHTTP','initialization');
  {$ENDIF}
  ALWinHttpClientKeepAlives := TObjectDictionary<AnsiString, TobjectList<TALWinHttpClient>>.create([doOwnsValues]);
  TALWinHttpClient.FWinHttpModuleHandle := GetModuleHandle('winhttp.dll');

finalization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Client.WinHTTP','finalization');
  {$ENDIF}
  ALFreeAndNil(ALWinHttpClientKeepAlives);

end.