{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)
							
product:      ALWinInetHttpClient
Version:      4.00

Description:  TALWinInetHttpClient is a is easy to use WinInet-based
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

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

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

Know bug :

History :     28/11/2005: add Component in delphi;
              12/09/2007: rename TALWinInetInternetOpenAccessType in TALWinInetHttpInternetOpenAccessType
              11/08/2011: add property DisconnectOnError => this to not auto
                          disconnect on error because if so, we loose the cookies
                          if any http error are encountered
              26/06/2012: Add xe2 support

Link :        http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
              http://www.ietf.org/rfc/rfc1867.txt
              http://www.ietf.org/rfc/rfc2388.txt
              http://www.w3.org/MarkUp/html-spec/html-spec_8.html
              http://www.cs.tut.fi/~jkorpela/forms/methods.html
              http://support.microsoft.com/kb/q194700/
              http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
              http://msdn.microsoft.com/library/default.asp?url=/library/en-us/wininet/wininet/about_wininet.asp

**************************************************************}
unit ALWininetHttpClient;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.Classes,
     WinApi.WinInet,
     {$ELSE}
     Windows,
     Classes,
     WinInet,
     {$IFEND}
     ALHttpClient;

(*$HPPEMIT '#pragma link "wininet.lib"' *)

{$IF CompilerVersion < 18.5}
Type
  DWORD_PTR = DWORD;
{$IFEND}

const
  INTERNET_STATUS_COOKIE_SENT      = 320;
  {$EXTERNALSYM INTERNET_STATUS_COOKIE_SENT}
  INTERNET_STATUS_COOKIE_RECEIVED  = 321;
  {$EXTERNALSYM INTERNET_STATUS_COOKIE_RECEIVED}
  INTERNET_STATUS_PRIVACY_IMPACTED = 324;
  {$EXTERNALSYM INTERNET_STATUS_PRIVACY_IMPACTED}
  INTERNET_STATUS_P3P_HEADER       = 325;
  {$EXTERNALSYM INTERNET_STATUS_P3P_HEADER}
  INTERNET_STATUS_P3P_POLICYREF    = 326;
  {$EXTERNALSYM INTERNET_STATUS_P3P_POLICYREF}
  INTERNET_STATUS_COOKIE_HISTORY   = 327;
  {$EXTERNALSYM INTERNET_STATUS_COOKIE_HISTORY}

type

  {-------------------------------------------------}
  //http://qc.embarcadero.com/wc/qcmain.aspx?d=106424
  _URL_COMPONENTSA = record
    dwStructSize: DWORD;        { size of this structure. Used in version check }
    lpszScheme: LPSTR;          { pointer to scheme name }
    dwSchemeLength: DWORD;      { length of scheme name }
    nScheme: TInternetScheme;   { enumerated scheme type (if known) }
    lpszHostName: LPSTR;        { pointer to host name }
    dwHostNameLength: DWORD;    { length of host name }
    nPort: INTERNET_PORT;       { converted port number }
    //pad: WORD;                { force correct allignment regardless of comp. flags}
    lpszUserName: LPSTR;        { pointer to user name }
    dwUserNameLength: DWORD;    { length of user name }
    lpszPassword: LPSTR;        { pointer to password }
    dwPasswordLength: DWORD;    { length of password }
    lpszUrlPath: LPSTR;         { pointer to URL-path }
    dwUrlPathLength: DWORD;     { length of URL-path }
    lpszExtraInfo: LPSTR;       { pointer to extra information (e.g. ?foo or #foo) }
    dwExtraInfoLength: DWORD;   { length of extra information }
  end;
  _TURLComponentsA = _URL_COMPONENTSA;

function _InternetCrackUrlA(lpszUrl: PAnsiChar; dwUrlLength, dwFlags: DWORD;
  var lpUrlComponents: _TURLComponentsA): BOOL; stdcall; external {$IFDEF MSWINDOWS}'wininet.dll'{$ENDIF}{$IFDEF LINUX}'libwininet.borland.so'{$ENDIF} name 'InternetCrackUrlA';

type

  {--------------------------------------------------}
  //http://qc.embarcadero.com/wc/qcmain.aspx?d=106689
  _PInternetBuffersA = ^_INTERNET_BUFFERSA;
  _INTERNET_BUFFERSA = record
    dwStructSize: DWORD;      { used for API versioning. Set to sizeof(INTERNET_BUFFERS) }
    Next: _PInternetBuffersA;  { chain of buffers }
    lpcszHeader: PAnsiChar;   { pointer to headers (may be NULL) }
    dwHeadersLength: DWORD;   { length of headers if not NULL }
    dwHeadersTotal: DWORD;    { size of headers if not enough buffer }
    lpvBuffer: Pointer;       { pointer to data buffer (may be NULL) }
    dwBufferLength: DWORD;    { length of data buffer if not NULL }
    dwBufferTotal: DWORD;     { total size of chunk, or content-length if not chunked }
    dwOffsetLow: DWORD;       { used for read-ranges (only used in HttpSendRequest2) }
    dwOffsetHigh: DWORD;
  end;
  _LP_INTERNET_BUFFERSA = _PInternetBuffersA;

function _HttpSendRequestExA(hRequest: HINTERNET; lpBuffersIn: _LP_INTERNET_BUFFERSA;
    lpBuffersOut: _LP_INTERNET_BUFFERSA;
    dwFlags: DWORD; dwContext: DWORD_PTR): BOOL; stdcall; external {$IFDEF MSWINDOWS}'wininet.dll'{$ENDIF}{$IFDEF LINUX}'libwininet.borland.so'{$ENDIF} name 'HttpSendRequestExA';
function _HttpEndRequestA(hRequest: HINTERNET;
  lpBuffersOut: _LP_INTERNET_BUFFERSA; dwFlags: DWORD;
  dwContext: DWORD_PTR): BOOL; stdcall; external {$IFDEF MSWINDOWS}'wininet.dll'{$ENDIF}{$IFDEF LINUX}'libwininet.borland.so'{$ENDIF} name 'HttpEndRequestA';

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

  {------------------------------------}
  {TAlWinInetHTTPClientStatusChangeEvent

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
  TAlWinInetHTTPClientStatusChangeEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord) of object;

  {-----------------------------------------}
  TALWinInetHTTPClient = class(TALHTTPClient)
  private
    FAccessType: TALWinInetHttpInternetOpenAccessType;
    FInternetOptions: TAlWininetHTTPClientInternetOptionSet;
    FConnected: Boolean;
    FURLHost: AnsiString;
    FURLSite: AnsiString;
    FURLPort: Integer;
    FURLScheme: Integer;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatusChange: TAlWinInetHTTPClientStatusChangeEvent;
    FDisconnectOnError: Boolean;
    procedure InitURL(const Value: AnsiString);
    procedure SetAccessType(const Value: TALWinInetHttpInternetOpenAccessType);
    procedure SetInternetOptions(const Value: TAlWininetHTTPClientInternetOptionSet);
    procedure SetOnStatusChange(const Value: TAlWinInetHTTPClientStatusChangeEvent);
  protected
    procedure CheckError(Error: Boolean);
    procedure SetURL(const Value: AnsiString); override;
    procedure SetUsername(const NameValue: AnsiString); override;
    procedure SetPassword(const PasswordValue: AnsiString); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure SetOnRedirect(const Value: TAlHTTPClientRedirectEvent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    function  Send(aRequestDataStream: TStream): Integer; virtual;
    procedure Receive(aContext: Dword; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); virtual;
    procedure Execute(aRequestDataStream: TStream; aResponseContentStream: TStream; aResponseContentHeader: TALHTTPResponseHeader); override;
    property  AccessType: TALWinInetHttpInternetOpenAccessType read FAccessType write SetAccessType default wHttpAt_Preconfig;
    property  InternetOptions: TAlWininetHTTPClientInternetOptionSet read FInternetOptions write SetInternetOptions default [wHttpIo_Keep_connection];
    property  DisconnectOnError: Boolean read FDisconnectOnError write FDisconnectOnError default False; // WinInethttp seam to handle internally the disconnection/reconnection !
    property  OnStatusChange: TAlWinInetHTTPClientStatusChangeEvent read FOnStatusChange write SetOnStatusChange;
  end;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     System.SysUtils,
     {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings,{$IFEND}
     {$ELSE}
     SysUtils,
     {$IFEND}
     ALString;

{********************************************************************}
{this procedure produce some strange bug under windows Server 2008 R2}
{when we download url from a site that is a little "slow" (when the site is fast seam to work ok,
{sometime we receive an AppCrash here. i thing it's simple a Windows Server 2008 R2 BUG}
{NB: i see this bug under WINHTTP, i don't know if it's the same under wininet}
procedure ALWininetHTTPCLientStatusCallback(InternetSession: hInternet;
                                            Context,
                                            InternetStatus: DWord;
                                            StatusInformation: Pointer;
                                            StatusInformationLength: DWord); stdcall;
var NewURL: AnsiString;
begin
  with TALWinInetHTTPClient(Context) do begin

    {fire the On Status change event}
    if Assigned(FOnStatusChange) then
      FOnStatusChange(TALWininetHttpClient(Context),
                      InternetStatus,
                      StatusInformation,
                      StatusInformationLength);

    {fire the On redirect event}
    If (InternetStatus = INTERNET_STATUS_REDIRECT) and Assigned(OnRedirect) then begin
      SetLength(NewURL, StatusInformationLength - 1);
      ALMove(StatusInformation^, NewURL[1], StatusInformationLength - 1);
      OnRedirect(TALWinInetHttpClient(Context), NewURL);
    end;

  end;
end;

{**************************************}
constructor TALWinInetHTTPClient.Create;
begin
  inherited;
  FInetRoot := nil;
  FInetConnect := nil;
  FConnected := False;
  FURLHost := '';
  FURLSite := '';
  FURLPort := INTERNET_DEFAULT_HTTP_PORT;
  FURLScheme := INTERNET_SCHEME_DEFAULT;
  FOnStatusChange := nil;
  FAccessType := wHttpAt_Preconfig;
  FInternetOptions := [wHttpIo_Keep_connection];
  RequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALWinInetHTTPClient)';
  FDisconnectOnError := False;
end;

{**************************************}
destructor TALWinInetHTTPClient.Destroy;
begin
  Disconnect;
  inherited;
end;

{********************************************************}
procedure TALWinInetHTTPClient.CheckError(Error: Boolean);
var ErrCode: Integer;
    S: AnsiString;
begin
  ErrCode := GetLastError;
  if Error and (ErrCode <> 0) then begin
    SetLength(S, 256);
    FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE,
                   Pointer(GetModuleHandle('wininet.dll')),
                   ErrCode,
                   0,
                   PAnsiChar(S),
                   Length(S),
                   nil);
    SetLength(S, {$IF CompilerVersion >= 24}{Delphi XE3}System.Ansistrings.{$IFEND}StrLen(PAnsiChar(S)));
    raise EALHTTPClientException.CreateFmt('%s - URL:%s', [ALtrim(S), URL]);      { Do not localize }
  end;
end;

{*************************************************************}
procedure TALWinInetHTTPClient.SetURL(const Value: AnsiString);
Var FOldURLHost: AnsiString;
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

{**************************************************************}
procedure TALWinInetHTTPClient.InitURL(const Value: AnsiString);
var URLComp: _TURLComponentsA;
    P: PAnsiChar;
begin
  if Value <> '' then begin
    FillChar(URLComp, SizeOf(URLComp), 0);
    URLComp.dwStructSize := SizeOf(URLComp);
    URLComp.dwSchemeLength := 1;
    URLComp.dwHostNameLength := 1;
    URLComp.dwURLPathLength := 1;
    P := PAnsiChar(Value);
    _InternetCrackUrlA(P, 0, 0, URLComp);
    if not (URLComp.nScheme in [INTERNET_SCHEME_HTTP, INTERNET_SCHEME_HTTPS]) then
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
    FURLScheme := INTERNET_SCHEME_DEFAULT;
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
  Function InternalGetProxyServerName: PAnsiChar;
  Begin
    If (ProxyParams.ProxyServer = '') then result := nil
    else result := PAnsiChar(ProxyParams.ProxyServer + ':' + ALIntToStr(ProxyParams.ProxyPort));
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

const AccessTypeArr: Array[TALWinInetHttpInternetOpenAccessType] of DWord = (INTERNET_OPEN_TYPE_DIRECT,
                                                                             INTERNET_OPEN_TYPE_PRECONFIG,
                                                                             INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY,
                                                                             INTERNET_OPEN_TYPE_PROXY);

var InternetSetStatusCallbackResult: PFNInternetStatusCallback;

begin
  { Yes, but what if we're connected to a different Host/Port?? }
  { So take advantage of a cached handle, we'll assume that
    Connect(False) will be called explicitly when we're switching
    Host. To that end, SetURL always disconnects }
  if (FConnected) then Exit;

  { Also, could switch to new API introduced in IE4/Preview2}
  if InternetAttemptConnect(0) <> ERROR_SUCCESS then {$IF CompilerVersion >= 23}{Delphi XE2}System.{$IFEND}SysUtils.Abort;

  {init FInetRoot}
  FInetRoot := InternetOpenA(PAnsiChar(RequestHeader.UserAgent),
                             AccessTypeArr[FAccessType],
                             InternalGetProxyServerName,
                             InternalGetProxyBypass,
                             InternalGetInternetOpenFlags);
  CheckError(not Assigned(FInetRoot));

  try

    {Register the callback function}
    if assigned(OnStatusChange) or assigned(OnRedirect) then begin
      InternetSetStatusCallbackResult := InternetSetStatusCallback(FInetRoot, @ALWininetHTTPCLientStatusCallback);
      CheckError(InternetSetStatusCallbackResult = pointer(INTERNET_INVALID_STATUS_CALLBACK));
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

{***********************************************************************}
function TALWinInetHTTPClient.Send(aRequestDataStream: TStream): Integer;

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
    Case RequestMethod of
      HTTPmt_Get:    Result := 'GET';
      HTTPmt_Post:   Result := 'POST';
      HTTPmt_Head:   Result := 'HEAD';
      HTTPmt_Delete: Result := 'DELETE';
      HTTPmt_Put:    Result := 'PUT';
      HTTPmt_Trace:  Result := 'TRACE'; // <= seam to not work on Wininet ! but work on winhttp
      else raise Exception.Create('Unknown Request Method');
    end;
  end;

var Request: HINTERNET;
    RetVal: DWord;
    BuffSize, Len: Integer;
    INBuffer: _INTERNET_BUFFERSA;
    Buffer: TMemoryStream;
    StrStr: TALStringStream;
    AcceptTypes: array of PAnsiChar;
    aHeader: AnsiString;

begin
  { Connect }
  Connect;

  SetLength(AcceptTypes, 2);
  AcceptTypes[0] := PAnsiChar(RequestHeader.Accept);
  AcceptTypes[1] := nil;

  Request := nil;
  try
    Request := HttpOpenRequestA(FInetConnect,
                                InternalGetHttpOpenRequestVerb,
                                PAnsiChar(FURLSite),
                                InternalGetHttpProtocolVersion,
                                PAnsiChar(requestHeader.Referer),
                                Pointer(AcceptTypes),
                                InternalGetHttpOpenRequestFlags,
                                DWORD_PTR(Self));

    CheckError(not Assigned(Request));

    { Timeouts }
    if ConnectTimeout > 0 then CheckError(not InternetSetOptionA(Request, INTERNET_OPTION_CONNECT_TIMEOUT, Pointer(@ConnectTimeout), SizeOf(ConnectTimeout)));
    if SendTimeout > 0 then CheckError(not InternetSetOptionA(Request, INTERNET_OPTION_SEND_TIMEOUT, Pointer(@SendTimeout), SizeOf(SendTimeout)));
    if ReceiveTimeout > 0 then CheckError(not InternetSetOptionA(Request, INTERNET_OPTION_RECEIVE_TIMEOUT, Pointer(@ReceiveTimeout), SizeOf(ReceiveTimeout)));

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not InternetSetOptionA(Request, INTERNET_OPTION_PROXY_USERNAME, PAnsiChar(ProxyParams.ProxyUserName), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not InternetSetOptionA(Request, INTERNET_OPTION_PROXY_PASSWORD, PAnsiChar(ProxyParams.ProxyPassword), length(ProxyParams.ProxyPassword)));

    {set the header}
    aHeader := requestHeader.RawHeaderText;
    HttpAddRequestHeadersA(Request,
                           PAnsiChar(aHeader),
                           Length(aHeader),
                           HTTP_ADDREQ_FLAG_REPLACE or HTTP_ADDREQ_FLAG_ADD);

    If assigned(aRequestDataStream) then begin
      aRequestDataStream.Position := 0;
      BuffSize := aRequestDataStream.Size;
    end
    else BuffSize := 0;

    if BuffSize > UploadBufferSize then begin
      Buffer := TMemoryStream.Create;
      try
        Buffer.SetSize(UploadBufferSize);

        { Init Input Buffer }
        INBuffer.dwStructSize := SizeOf(INBuffer);
        INBuffer.Next := nil;
        INBuffer.lpcszHeader := nil;
        INBuffer.dwHeadersLength := 0;
        INBuffer.dwHeadersTotal := 0;
        INBuffer.lpvBuffer := nil;
        INBuffer.dwBufferLength := 0;
        INBuffer.dwBufferTotal := BuffSize;
        INBuffer.dwOffsetLow := 0;
        INBuffer.dwOffsetHigh := 0;

        { Start POST }
        CheckError(not _HttpSendRequestExA(Request,
                                           @INBuffer,
                                           nil,
                                           HSR_INITIATE or HSR_SYNC,
                                           DWORD_PTR(Self)));
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

            CheckError(not InternetWriteFile(Request,
                                             @Buffer.Memory^,
                                             Len,
                                             RetVal));

            { Posting Data Event }
            if Assigned(OnUploadProgress) then
              OnUploadProgress(self, aRequestDataStream.Position, BuffSize);
          end;
        finally
          CheckError(not _HttpEndRequestA(Request,
                                          nil,
                                          0,
                                          0));
        end;
      finally
        Buffer.Free;
      end;
    end

    else if BuffSize > 0 then begin
      StrStr := TALStringStream.Create('');
      try
        StrStr.CopyFrom(aRequestDataStream, 0);
        CheckError(not HttpSendRequestA(Request,
                                        nil,
                                        0,
                                        @StrStr.DataString[1],
                                        Length(StrStr.DataString)));
      finally
        StrStr.Free;
      end;
    end

    else begin
      CheckError(not HttpSendRequestA(Request,
                                      nil,
                                      0,
                                      nil,
                                      0));
    end;

  except
    if (Request <> nil) then
      InternetCloseHandle(Request);
    If fDisconnectOnError then Disconnect;
    raise;
  end;
  Result := Integer(Request);
end;

{******************************************************}
procedure  TALWinInetHTTPClient.Receive(aContext: Dword;
                                        aResponseContentStream: TStream;
                                        aResponseContentHeader: TALHTTPResponseHeader);

var Size,
    Downloaded,
    Status,
    Len,
    Index,
    ContentlengthDownloaded,
    ContentLength: DWord;
    S: AnsiString;

begin

  {read the header}
  If assigned(aResponseContentHeader) then begin
    Size := 4096;
    While true do begin
      Index := 0;
      SetLength(s, Size);
      If HttpQueryInfoA(Pointer(aContext),
                        HTTP_QUERY_RAW_HEADERS_CRLF,
                        @s[1],
                        Size,
                        Index) then begin
        SetLength(s, Size);
        aResponseContentHeader.RawHeaderText := s;
        break;
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { Handle error from status}
  Index := 0;
  Len := SizeOf(Status);
  if HttpQueryInfoA(Pointer(aContext),
                    HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                    @Status,
                    Len,
                    Index) and
     (Status >= 300) then begin

    Size := 4096;
    While true do begin
      Index := 0;
      SetLength(s, Size);
      if HttpQueryInfoA(Pointer(aContext),
                        HTTP_QUERY_STATUS_TEXT,
                        @S[1],
                        Size,
                        Index) then begin
        SetLength(S, Size);
        raise EALHTTPClientException.CreateFmt('%s (%d) - ''%s''', [S, Status, URL], Status);
      end
      else checkError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    end;
  end;

  { read content-length }
  Index := 0;
  Len := SizeOf(ContentLength);
  if not HttpQueryInfoA(Pointer(aContext),
                        HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
                        @ContentLength,
                        Len,
                        Index) then ContentLength := 0;

  { Read data }
  Len := 0;
  ContentlengthDownloaded := 0;
  repeat
    CheckError(not InternetQueryDataAvailable(Pointer(aContext),
                                              Size,
                                              0,
                                              0));

    if Size > 0 then begin
      SetLength(S, Size);
      CheckError(not InternetReadFile(Pointer(aContext),
                                      @S[1],
                                      Size,
                                      Downloaded));
      aResponseContentStream.Writebuffer(pointer(S)^, Size);

      { Receiving Data event }
      inc(ContentlengthDownloaded, Downloaded);
      if Assigned(onDownloadProgress) then onDownloadProgress(self, ContentlengthDownloaded, ContentLength)
    end;
  until Size = 0;
end;

{*****************************************************************}
procedure TALWinInetHTTPClient.Execute(aRequestDataStream: TStream;
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
      If fDisconnectOnError then Disconnect;
      raise;
    end;
  finally
    if Context <> 0  then
      InternetCloseHandle(Pointer(Context));
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

{***************************************************************************************************}
procedure TALWinInetHTTPClient.SetOnStatusChange(const Value: TAlWinInetHTTPClientStatusChangeEvent);
begin
  Disconnect;
  FOnStatusChange := Value;
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

{**************************************************************************************************}
procedure TALWinInetHTTPClient.OnRequestHeaderChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex = 35) then Disconnect; //clear, UserAgent
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


