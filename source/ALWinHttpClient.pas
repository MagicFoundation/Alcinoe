{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      ALWinHttpClient
Version:      3.50

Description:  Description:  TALWinHttpClient is a is easy to use WinHTTP-based
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

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :    10/10/2008: UserName and password not work (but you can use the header instead)

History :     28/11/2005: add Component in delphi;

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALWinHttpClient;

interface

uses Windows,
     SysUtils,
     Classes,
     ALHttpCommon,
     ALHttpClient,
     ALWinHttpWrapper;

(*$HPPEMIT '#pragma link "winhttp.lib"' *)

type

  {----------------------------------------}
  TALWinHttpClientInternetOpenAccessType = (
                                            wHttpAt_NO_PROXY,                    {Resolves all host names locally.}
                                            wHttpAt_DEFAULT_PROXY,               {Retrieves the static proxy or direct configuration from the registry.
                                                                                  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY does not inherit browser proxy settings.
                                                                                  WinHTTP does not share any proxy settings with Internet Explorer. This option
                                                                                  picks up the WinHTTP proxy configuration set by the WinHTTP Proxycfg.exe utility.}
                                            wHttpAt_NAMED_PROXY                  {Passes requests to the proxy unless a proxy bypass list is supplied and the name
                                                                                  to be resolved bypasses the proxy. In this case, this function uses
                                                                                  WINHTTP_ACCESS_TYPE_NAMED_PROXY.}
                                           );

  {--------------------------------}
  TAlWinHttpClientInternetOption = (
                                    wHttpIo_Async,                     {NOT SUPPORTED YET!
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
                                    wHttpIo_No_auto_redirect           {Does not automatically handle redirection in HttpSendRequest.}
                                   );

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
  TALWinHttpClientStatusChangeEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord) of object;

  {-------------------------------------}
  TALWinHttpClient = class(TALHTTPClient)
  private
    FAccessType: TALWinHttpClientInternetOpenAccessType;
    FInternetOptions: TALWinHttpClientInternetOptionSet;
    FConnected: Boolean;
    FURLHost: string;
    FURLSite: string;
    FURLPort: Integer;
    FURLScheme: Integer;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatusChange: TALWinHttpClientStatusChangeEvent;
    procedure InitURL(const Value: string);
    procedure SetAccessType(const Value: TALWinHttpClientInternetOpenAccessType);
  protected
    procedure CheckError(Error: Boolean);
    procedure SetURL(const Value: string); override;
    procedure SetUsername(const NameValue: string); override;
    procedure SetPassword(const PasswordValue: string); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function  Send(const aRequestDataStream: TStream): Integer; virtual;
    procedure Receive(aContext: Dword; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); virtual;
    procedure Execute(const aRequestDataStream: TStream; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); override;
  published
    property  AccessType: TALWinHttpClientInternetOpenAccessType read FAccessType write SetAccessType default wHttpAt_NO_PROXY;
    property  InternetOptions: TALWinHttpClientInternetOptionSet read FInternetOptions write FInternetOptions default [wHttpIo_Keep_connection];
    property  OnStatusChange: TALWinHttpClientStatusChangeEvent read FOnStatusChange write FOnStatusChange;
  end;

procedure Register;

implementation

{$R ..\resource\ALWinHttpClient.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALWinHttpClient]);
end;




////////////////////////////////////////////////////////////////////////////////
////////// TALWinHttpClient ////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{*****************************************************************}
procedure ALWinHTTPCLientStatusCallback(InternetSession: hInternet;
                                        Context,
                                        InternetStatus: DWord;
                                        StatusInformation: Pointer;
                                        StatusInformationLength: DWord); stdcall;
begin
  if Context=0 then exit;
  with TALWinHttpClient(Context) do begin

    {fire the On Status change event}
    if Assigned(FOnStatusChange) then
      FOnStatusChange(
                      TALWinHttpClient(Context),
                      InternetStatus,
                      StatusInformation,
                      StatusInformationLength
                     );

    {fire the On redirect event}
    If (InternetStatus = WINHTTP_CALLBACK_STATUS_REDIRECT) and Assigned(OnRedirect) then begin
      OnRedirect(
                 TALWinHttpClient(Context),
                 String(PWideChar(InternetStatus))
                );
    end;

  end;
end;

{*****************************************************}
constructor TALWinHttpClient.Create(Owner: TComponent);
begin
  inherited;
  FInetRoot := nil;
  FInetConnect := nil;
  FConnected := False;
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
  if Assigned(FInetConnect) then WinHttpSetStatusCallback(FInetConnect, nil, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, 0);
  if Assigned(FInetRoot) then WinHttpSetStatusCallback(FInetRoot, nil, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, 0);
  Disconnect;
  inherited;
end;

{****************************************************}
procedure TALWinHttpClient.CheckError(Error: Boolean);
var ErrCode: Integer;
    S: string;
begin
  ErrCode := GetLastError;
  if Error and (ErrCode <> 0) then begin
    SetLength(S, 256);
    FormatMessage(
                  FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE,
                  Pointer(GetModuleHandle('winhttp.dll')),
                  ErrCode,
                  0,
                  PChar(S),
                  Length(S),
                  nil
                 );
    SetLength(S, StrLen(PChar(S)));
    while (Length(S) > 0) and (S[Length(S)] in [#10, #13]) do SetLength(S, Length(S) - 1);
    raise EALHTTPClientException.CreateFmt('%s - URL:%s', [S, URL]);      { Do not localize }
  end;
end;

{*****************************************************}
procedure TALWinHttpClient.SetURL(const Value: string);
Var FOldURLHost: String;
    FoldUrlPort: integer;
begin
  If Value <> Url then Begin
    FOldURLHost := FURLHost;
    FOldURLPort := FURLPort;
    InitURL(Value);

    { Here we disconnect if a new URL comes in with a
      new host...this ensures that we don't keep a
      connection to a wrong host }
    If (FOldURLHost <> FURLHost) or
       (FOldURLPort <> FURLPort) then Disconnect;

    inherited SetUrl(Value);
  end;
end;

{******************************************************}
procedure TALWinHttpClient.InitURL(const Value: string);
var URLComp: TURLComponents;
    P: PWideChar;
begin
  if Value <> '' then begin
    FillChar(URLComp, SizeOf(URLComp), 0);
    URLComp.dwStructSize := SizeOf(URLComp);
    URLComp.dwSchemeLength := 1;
    URLComp.dwHostNameLength := 1;
    URLComp.dwURLPathLength := 1;
    P := PWideChar(WideString(Value));
    WinHttpCrackUrl(P, 0, 0, URLComp);
    if not (URLComp.nScheme in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then
      raise EALHTTPClientException.CreateFmt(CALHTTPCLient_MsgInvalidURL, [Value]);
    FURLScheme := URLComp.nScheme;
    FURLPort := URLComp.nPort;
    FURLHost := Copy(Value, URLComp.lpszHostName - P + 1, URLComp.dwHostNameLength);
    FURLSite := Copy(Value, URLComp.lpszUrlPath - P + 1, URLComp.dwUrlPathLength);
  end
  else begin
    FURLPort := INTERNET_DEFAULT_HTTP_PORT;
    FURLHost := '';
    FURLSite := '';
    FURLScheme := INTERNET_SCHEME_HTTP;
  end;
end;

{**************************************************************}
procedure TALWinHttpClient.SetUsername(const NameValue: string);
begin
  If UserName <> NameValue then Disconnect;
  inherited;
end;

{******************************************************************}
procedure TALWinHttpClient.SetPassword(const PasswordValue: string);
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
    else result := PWideChar(widestring(ProxyParams.ProxyServer) + ':' + WideString(IntToStr(ProxyParams.ProxyPort)));
  end;

  {-----------------------------------------}
  Function InternalGetProxyBypass: PWideChar;
  Begin
    {We should not use empty string for ProxyBypass because
     InternetOpen will use it as the proxy bypass list}
    if (FaccessType <> wHTTPat_NAMED_PROXY) or
       (ProxyParams.ProxyBypass = '') then result := WINHTTP_NO_PROXY_BYPASS
    else result := PWidechar(WideString(ProxyParams.ProxyBypass));
  end;

const AccessTypeArr: Array[TALWinHttpClientInternetOpenAccessType] of DWord = (
                                                                               WINHTTP_ACCESS_TYPE_NO_PROXY,
                                                                               WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                                                                               WINHTTP_ACCESS_TYPE_NAMED_PROXY
                                                                              );

begin
  { Yes, but what if we're connected to a different Host/Port?? }
  { So take advantage of a cached handle, we'll assume that
    Connect(False) will be called explicitly when we're switching
    Host. To that end, SetURL always disconnects }
  if (FConnected) then Exit;

  FInetRoot := WinHttpOpen(
                           PWideChar(widestring(RequestHeader.UserAgent)),
                           AccessTypeArr[FAccessType],
                           InternalGetProxyServerName,
                           InternalGetProxyBypass,
                           0
                          );

  CheckError(not Assigned(FInetRoot));

  try
    {Register the callback function}
    WinHttpSetStatusCallback(FInetRoot, @ALWinHTTPCLientStatusCallback, WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, 0);

    FInetConnect := WinHttpConnect(
                                   FInetRoot,
                                   PWideChar(wideString(FURLHost)),
                                   FURLPort,
                                   0
                                  );


    CheckError(not Assigned(FInetConnect));
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

{*************************************************************************}
function TALWinHttpClient.Send(const aRequestDataStream: TStream): Integer;

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

var Request: HINTERNET;
    BuffSize, Len: Integer;
    Buffer: TMemoryStream;
    StrStr: TStringStream;
    RetVal: DWord;
    aHeader: WideString;
    aDWORDOptions: DWord;
    StrProtocolVersion: String;
    StrVerb: String;

begin
  { Connect }
  Connect;

  If ProtocolVersion = HTTPpv_1_1 then StrProtocolVersion := 'HTTP/1.1'
  else StrProtocolVersion := 'HTTP/1.0';

  if RequestMethod = HTTPrm_Post then StrVerb := 'POST'
  else StrVerb := 'GET';

  Request := nil;
  try
    Request := WinHttpOpenRequest(
                                  FInetConnect,
                                  PWideChar(WideString(StrVerb)),
                                  PWideChar(WideString(FURLSite)),
                                  PWideChar(WideString(StrProtocolVersion)),
                                  PWideChar(WideString(requestHeader.Referer)),
                                  WINHTTP_DEFAULT_ACCEPT_TYPES,  // we try before to put accept type here but it's
                                                                 // work good under delphi 7 / XP but have access
                                                                 // violation under Vista / delphi 2007 !
                                                                 // i thing i not understand very weel how to
                                                                 // use this parameter, so i prefere to not use it !
                                                                 // anyway the accept type is pass after with WinHttpAddRequestHeaders !
                                  InternalGetHttpOpenRequestFlags
                                 );

    CheckError(not Assigned(Request));

    { Timeouts }
    if ConnectTimeout > 0 then CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_CONNECT_TIMEOUT , Pointer(@ConnectTimeout), SizeOf(ConnectTimeout)));
    if SendTimeout > 0 then CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_SEND_TIMEOUT , Pointer(@SendTimeout), SizeOf(SendTimeout)));
    if ReceiveTimeout > 0 then CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_RECEIVE_TIMEOUT, Pointer(@ReceiveTimeout), SizeOf(ReceiveTimeout)));

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_PROXY_USERNAME, PWidechar(WideString(ProxyParams.ProxyUserName)), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_PROXY_PASSWORD, PWidechar(WideString(ProxyParams.ProxyPassword)), length(ProxyParams.ProxyPassword)));

    {wHttpIo_Keep_connection, wHttpIo_No_cookies and wHttpIo_No_auto_redirect}
    If not (wHttpIo_Keep_connection in InternetOptions) then begin
      aDWORDOptions := WINHTTP_DISABLE_KEEP_ALIVE;
      CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@aDWORDOptions), sizeof(aDWORDOptions)));
    end;
    If (wHttpIo_No_cookies in InternetOptions) then begin
      aDWORDOptions := WINHTTP_DISABLE_COOKIES;
      CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@aDWORDOptions), sizeof(aDWORDOptions)));
    end;
    If (wHttpIo_No_auto_redirect in InternetOptions) then begin
      aDWORDOptions := WINHTTP_DISABLE_REDIRECTS;
      CheckError(not WinHttpSetOption(Request, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@aDWORDOptions), sizeof(aDWORDOptions)));
    end;

    {set the header}
    aHeader := requestHeader.RawHeaderText;
    WinHttpAddRequestHeaders(
                             Request,
                             PWideChar(aHeader),
                             Length(aHeader),
                             WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD
                            );

    If assigned(aRequestDataStream) then begin
      aRequestDataStream.Position := 0;
      BuffSize := aRequestDataStream.Size;
    end
    else BuffSize := 0;

    if BuffSize > UploadBufferSize then begin
      Buffer := TMemoryStream.Create;
      try
        Buffer.SetSize(UploadBufferSize);

        { Start POST }
        CheckError(
                   not WinHttpSendRequest(
                                          Request,
                                          WINHTTP_NO_ADDITIONAL_HEADERS,
                                          0,
                                          nil,
                                          0,
                                          BuffSize,
                                          Dword(self)
                                         )
                  );

        try
          while True do begin
            { Calc length of data to send }
            Len := BuffSize - aRequestDataStream.Position;
            if Len > UploadBufferSize then Len := UploadBufferSize;
            { Bail out if zip.. }
            if Len = 0 then break;
            { Read data in buffer and write out}
            Len := aRequestDataStream.Read(Buffer.Memory^, Len);
            if Len = 0 then raise EALHTTPClientException.Create(CALHTTPCLient_MsgInvalidHTTPRequest);

            CheckError(
                       not WinHttpWriteData(
                                            Request,
                                            @Buffer.Memory^,
                                            Len,
                                            RetVal
                                           )
                      );

            { Posting Data Event }
            if Assigned(OnUploadProgress) then
              OnUploadProgress(self, aRequestDataStream.Position, BuffSize);
          end;
        finally
          CheckError(not WinHttpReceiveResponse(Request, nil));
        end;
      finally
        Buffer.Free;
      end;
    end

    else begin
      StrStr := TStringStream.Create('');
      try
        If assigned(aRequestDataStream) then StrStr.CopyFrom(aRequestDataStream, 0);
        CheckError(
                   not WinHttpSendRequest(
                                          Request,
                                          WINHTTP_NO_ADDITIONAL_HEADERS,
                                          0,
                                          @StrStr.DataString[1],
                                          Length(StrStr.DataString),
                                          Length(StrStr.DataString),
                                          Dword(self)
                                         )
                  );

        CheckError(not WinHttpReceiveResponse(Request, nil));
      finally
        StrStr.Free;
      end;
    end;
  except
    if (Request <> nil) then
      WinHttpCloseHandle(Request);
    Disconnect;
    raise;
  end;
  Result := Integer(Request);
end;

{**************************************************}
procedure  TALWinHttpClient.Receive(aContext: Dword;
                                    aResponseContentStream: TStream;
                                    aResponseContentHeader: TALHTTPResponseHeader);
var Size,
    Downloaded,
    Status,
    Len,
    ContentlengthDownloaded,
    ContentLength: DWord;
    aWideStr: Widestring;
    aStr: String;
begin

  {read the header}
  If assigned(aResponseContentHeader) then begin
    Size := 4096;
    While true do begin
      SetLength(aWideStr, Size div sizeof(wideChar));
      If Boolean(WinHttpQueryHeaders(
                                     Pointer(aContext),
                                     WINHTTP_QUERY_RAW_HEADERS_CRLF,
                                     WINHTTP_HEADER_NAME_BY_INDEX,
                                     @aWideStr[1],
                                     Size,
                                     WINHTTP_NO_HEADER_INDEX
                                    )) then begin
        SetLength(aWideStr, Size div sizeof(wideChar));
        aResponseContentHeader.RawHeaderText := aWideStr;
        break;
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { Handle error from status}
  Len := SizeOf(Status);
  if boolean(WinHttpQueryHeaders(
                                 Pointer(aContext),
                                 WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
                                 WINHTTP_HEADER_NAME_BY_INDEX,
                                 @Status,
                                 Len,
                                 WINHTTP_NO_HEADER_INDEX
                                )) and
           (Status >= 300) then begin

    Size := 4096;
    While true do begin
      SetLength(aWideStr, Size div sizeof(wideChar));
      if boolean(WinHttpQueryHeaders(
                                     Pointer(aContext),
                                     WINHTTP_QUERY_STATUS_TEXT,
                                     WINHTTP_HEADER_NAME_BY_INDEX,
                                     @aWideStr[1],
                                     Size,
                                     WINHTTP_NO_HEADER_INDEX
                                    )) then begin
        SetLength(aWideStr, Size div sizeof(wideChar));
        raise EALHTTPClientException.CreateFmt('%s (%d) - ''%s''', [aWideStr, Status, URL], Status);
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { read content-length }
  Len := SizeOf(ContentLength);
  if not WinHttpQueryHeaders(
                             Pointer(aContext),
                             WINHTTP_QUERY_CONTENT_LENGTH or WINHTTP_QUERY_FLAG_NUMBER,
                             WINHTTP_HEADER_NAME_BY_INDEX,
                             @ContentLength,
                             Len,
                             WINHTTP_NO_HEADER_INDEX
                            ) then ContentLength := 0;

  { Read data }
  Len := 0;
  ContentlengthDownloaded := 0;
  repeat
    CheckError(
               not WinHttpQueryDataAvailable(
                                              Pointer(aContext),
                                              Size
                                             )
              );

    if Size > 0 then begin
      SetLength(aStr, Size);
      CheckError(
                 not WinHttpReadData(
                                     Pointer(aContext),
                                     @aStr[1],
                                     Size,
                                     Downloaded
                                    )
                );
      aResponseContentStream.Write(aStr[1], Size);

      { Receiving Data event }
      inc(ContentlengthDownloaded, Downloaded);
      if Assigned(onDownloadProgress) then onDownloadProgress(self, ContentlengthDownloaded, ContentLength)
    end;
  until Size = 0;
end;

{*******************************************************************}
procedure TALWinHttpClient.Execute(const aRequestDataStream: TStream;
                                   aResponseContentStream: TStream;
                                   aResponseContentHeader: TALHTTPResponseHeader);
var Context: Integer;
begin
  if URL = '' then raise EALHTTPClientException.Create(CALHTTPCLient_MsgEmptyURL);
  Context := Send(aRequestDataStream);
  try
    try
      Receive(Context, aResponseContentStream, aResponseContentHeader);
    except
      on Ex: EALHTTPClientException do begin
        Disconnect;
        raise;
      end;
    end;
  finally
    if Context <> 0  then
      WinHttpCloseHandle(Pointer(Context));
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

{********************************************************************************************}
procedure TALWinHttpClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

{**************************************************************************************************}
procedure TALWinHttpClient.OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex = 35) then Disconnect; //clear, UserAgent
end;

end.


