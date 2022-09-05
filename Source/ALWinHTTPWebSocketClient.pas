{*******************************************************************************
Description:  TALWinHTTPWebSocketClient is a easy to use WinHTTP-based
Web Socket client component
*******************************************************************************}

unit ALWinHTTPWebSocketClient;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.WinHTTP,
  ALStringList,
  ALWinHttpClient,
  ALWebSocketClient,
  ALHttpClient;

type

  {---------------------------------------------}
  TALWinHTTPWebSocketClientSendQueueItem = Record
    Data: AnsiString;
    IsUTF8: Boolean;
    class function Create(const aData: AnsiString;
                          const aIsUTF8: Boolean = True): TALWinHTTPWebSocketClientSendQueueItem; static; inline;
  end;

  {--------------------------------}
  TALWinHTTPWebSocketClient = class;

  {-----------------------------------------------}
  TALWinHTTPWebSocketClientContext = class(Tobject)
  private
    fWebSocketClient: TALWinHTTPWebSocketClient;
  public
    constructor Create(const aWebSocketClient: TALWinHTTPWebSocketClient);
    property WebSocketClient: TALWinHTTPWebSocketClient read fWebSocketClient;
  end;

  {---------------------------------------------------}
  TALWinHTTPWebSocketClient = class(TALWebSocketClient)
  private
    class var ActiveWebSocketClient: TDictionary<DWORD_PTR, Boolean>;
  private
    fContext: TALWinHTTPWebSocketClientContext;
    FAccessType: TALWinHTTPClientInternetOpenAccessType;
    FInternetOptions: TALWinHTTPClientInternetOptionSet;
    FConnected: Boolean;
    FWebSocketReady: boolean;
    FURL: AnsiString;
    FURLHost: AnsiString;
    FURLPath: AnsiString;
    FURLPort: INTERNET_PORT;
    FURLScheme: INTERNET_SCHEME;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FInetRequest: HINTERNET;
    FInetWebSocket: HINTERNET;
    FOnStatus: TALWinHttpClientStatusEvent;
    FReceiveBuffer: AnsiString;
    FFragmentReceiveBuffer: AnsiString;
    FSendqueue: Tqueue<TALWinHTTPWebSocketClientSendQueueItem>;
    procedure SetAccessType(const Value: TALWinHTTPClientInternetOpenAccessType);
    procedure Reconnect;
    function DoSend(const aData: AnsiString;
                    const aIsUTF8: Boolean = True;
                    const aEnqueue: Boolean = True): boolean;
    procedure DoDisconnect(Const ClearSendQueue: Boolean = True);
  protected
    function Receive: boolean; virtual;
    procedure CheckError(ErrCode: DWORD); overload;
    procedure CheckError(Error: Boolean); overload;
    procedure SetURL(const Value: AnsiString);
    procedure SetUsername(const NameValue: AnsiString); override;
    procedure SetPassword(const PasswordValue: AnsiString); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
    procedure SetBufferSize(const Value: Cardinal); override;
    property  URL: AnsiString read FURL write SetURL;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Connect(const aUrl:AnsiString); override;
    procedure Disconnect; override;
    procedure Send(const aData: AnsiString;
                   const aIsUTF8: Boolean = True); override;
    property  AccessType: TALWinHTTPClientInternetOpenAccessType read FAccessType write SetAccessType default wHttpAt_NO_PROXY;
    property  InternetOptions: TALWinHTTPClientInternetOptionSet read FInternetOptions write FInternetOptions default [wHttpIo_Keep_connection];
    property  OnStatus: TALWinHttpClientStatusEvent read FOnStatus write fOnStatus;
  end;


{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if Winapi.WinHTTP.pas was not updated with the lines below and adjust the IFDEF'}
{$ENDIF}

function WinHttpWebSocketCompleteUpgrade(hRequest: HINTERNET; pContext: DWORD_PTR): HINTERNET; stdcall; external 'winhttp.dll';
function WinHttpWebSocketSend(hWebSocket: HINTERNET;
                              eBufferType: DWORD;
                              pvBuffer: PVOID;
                              dwBufferLength: DWORD): DWORD; stdcall; external 'winhttp.dll';
function WinHttpWebSocketReceive(hWebSocket: HINTERNET;
                                 pvBuffer: PVOID;
                                 dwBufferLength: DWORD;
                                 var pdwBytesRead: DWORD;
                                 var peBufferType: DWORD): DWORD; stdcall; external 'winhttp.dll';

const
  WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;
  WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE = 0;
  WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE = 1;
  WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE = 2;
  WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE = 3;
  WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE = 4;

type
  _WINHTTP_WEB_SOCKET_STATUS = record
    dwBytesTransferred: DWORD;
    eBufferType: DWORD;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  ALCommon,
  ALString;

{*********************************************************************************}
procedure ALWinHTTPWebSocketClientInetWebSocketStatusCallback(hInternet: HINTERNET;
                                                              dwContext: DWORD_PTR;
                                                              dwInternetStatus: DWORD;
                                                              lpvStatusInformation: LPVOID;
                                                              dwStatusInformationLength: DWORD); stdcall;
begin

  TThread.Synchronize(nil, // << As we use WINHTTP_FLAG_ASYNC we must synch the event with the main thread
    procedure
    var LWebSocketClient: TALWinHTTPWebSocketClient;
        LBool: Boolean;
        LStatus: _WINHTTP_WEB_SOCKET_STATUS;
        LData: ansiString;
    begin

      {check if dwContext is still active}
      if (TALWinHTTPWebSocketClient.ActiveWebSocketClient = nil) or
         (not TALWinHTTPWebSocketClient.ActiveWebSocketClient.TryGetValue(dwContext, LBool)) then exit;

      {init LWebSocketClient}
      LWebSocketClient := TALWinHTTPWebSocketClientContext(dwContext).WebSocketClient;

      Try

        {fire the OnStatusChange event}
        if Assigned(LWebSocketClient.FOnStatus) then
          LWebSocketClient.FOnStatus(
            LWebSocketClient,
            dwInternetStatus,
            lpvStatusInformation,
            dwStatusInformationLength);

        {WINHTTP_CALLBACK_STATUS_REQUEST_ERROR}
        If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR) then begin
          if Assigned(LWebSocketClient.OnError) then
            LWebSocketClient.OnError(
              LWebSocketClient,
              'An error occurred while sending an HTTP request');
          if LWebSocketClient.AutoReconnect then
            LWebSocketClient.Reconnect;
        end

        {WINHTTP_CALLBACK_STATUS_READ_COMPLETE}
        else If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_READ_COMPLETE) then begin

          LStatus := _WINHTTP_WEB_SOCKET_STATUS(lpvStatusInformation^);
          case LStatus.eBufferType of

            //-----
            WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE,
            WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE: begin
              LData := LWebSocketClient.FFragmentReceiveBuffer + ALCopyStr(LWebSocketClient.FReceiveBuffer, 1, LStatus.dwBytesTransferred);
              LWebSocketClient.FFragmentReceiveBuffer := '';
              if Assigned(LWebSocketClient.OnReceive) then
                LWebSocketClient.OnReceive(
                  LWebSocketClient, // sender: Tobject;
                  LData, // const Data: AnsiString;
                  LStatus.eBufferType = WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE) // const IsUTF8: Boolean
            end;

            //-----
            WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE,
            WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE: begin
              LWebSocketClient.FFragmentReceiveBuffer := LWebSocketClient.FFragmentReceiveBuffer + ALCopyStr(LWebSocketClient.FReceiveBuffer, 1, LStatus.dwBytesTransferred);
            end;

            //-----
            WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE: exit;

            //-----
            else raise Exception.CreateFmt('Unknown Buffer Type (%d)', [LStatus.eBufferType]);

          end;

          //start the listener again
          LWebSocketClient.Receive;

        end

        {WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE}
        else If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE) then begin
          if LWebSocketClient.FSendqueue.Count > 0 then
            LWebSocketClient.FSendqueue.Dequeue;
        end;

      Except
        on E: Exception do begin
          if assigned(LWebSocketClient.OnError) then LWebSocketClient.OnError(LWebSocketClient, AnsiString(E.Message));
          if LWebSocketClient.AutoReconnect then LWebSocketClient.Reconnect
          else raise;
        end;
      end;

    end);

end;

{****************************************************************************}
procedure ALWinHTTPWebSocketClientInetRootStatusCallback(hInternet: HINTERNET;
                                                         dwContext: DWORD_PTR;
                                                         dwInternetStatus: DWORD;
                                                         lpvStatusInformation: LPVOID;
                                                         dwStatusInformationLength: DWORD); stdcall;
begin

  TThread.Synchronize(nil, // << As we use WINHTTP_FLAG_ASYNC we must synch the event with the main thread
    procedure
    var LWebSocketClient: TALWinHTTPWebSocketClient;
        LBool: Boolean;
        LSetStatusCallbackResult: TWinHttpStatusCallback;
        LDWordPtrOption: DWord_ptr;
        LSendQueueItems: TArray<TALWinHTTPWebSocketClientSendQueueItem>;
        I: integer;
    begin

      {check if dwContext is still active}
      if (TALWinHTTPWebSocketClient.ActiveWebSocketClient = nil) or
         (not TALWinHTTPWebSocketClient.ActiveWebSocketClient.TryGetValue(dwContext, LBool)) then exit;

      {init LWebSocketClient}
      LWebSocketClient := TALWinHTTPWebSocketClientContext(dwContext).WebSocketClient;

      Try

        {fire the OnStatusChange event}
        if Assigned(LWebSocketClient.FOnStatus) then
          LWebSocketClient.FOnStatus(
            LWebSocketClient,
            dwInternetStatus,
            lpvStatusInformation,
            dwStatusInformationLength);

        {WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE}
        If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE) then begin
          LWebSocketClient.CheckError(not WinHttpReceiveResponse(LWebSocketClient.FInetRequest, nil));
        end

        {WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE}
        else if dwInternetStatus = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE then begin

          {Upgrade to WebSocket}
          LWebSocketClient.FInetWebSocket := WinHttpWebSocketCompleteUpgrade(LWebSocketClient.FInetRequest, 0);
          LWebSocketClient.CheckError(not Assigned(LWebSocketClient.FInetWebSocket));

          {Register the callback function}
          LDWordPtrOption := DWORD_PTR(LWebSocketClient.fContext);
          LWebSocketClient.CheckError(not WinHttpSetOption(LWebSocketClient.FInetWebSocket, WINHTTP_OPTION_CONTEXT_VALUE, Pointer(@LDWordPtrOption), sizeof(LDWordPtrOption)));
          LSetStatusCallbackResult := WinHttpSetStatusCallback(LWebSocketClient.FInetWebSocket, // _In_       HINTERNET               hInternet,
                                                               @ALWinHTTPWebSocketClientInetWebSocketStatusCallback, // _In_       WINHTTP_STATUS_CALLBACK lpfnInternetCallback,
                                                               WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, // _In_       DWORD                   dwNotificationFlags,
                                                               0); // _Reserved_ DWORD_PTR               dwReserved
          LWebSocketClient.CheckError(@LSetStatusCallbackResult = @WINHTTP_INVALID_STATUS_CALLBACK);

          {start to read the socket}
          if not LWebSocketClient.Receive then exit;

          {init FWebSocketReady}
          LWebSocketClient.FWebSocketReady := True;

          {Send the item in queue}
          LSendQueueItems := LWebSocketClient.FSendqueue.ToArray;
          for I := low(LSendQueueItems) to high(LSendQueueItems) do
            if not LWebSocketClient.DoSend(LSendQueueItems[i].Data, LSendQueueItems[i].IsUTF8, False{aEnqueue}) then exit;

        end

        {WINHTTP_CALLBACK_STATUS_REQUEST_ERROR}
        else If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR) then begin
          if Assigned(LWebSocketClient.OnError) then
            LWebSocketClient.OnError(
              LWebSocketClient,
              'An error occurred while sending an HTTP request');
          if LWebSocketClient.AutoReconnect then
            LWebSocketClient.Reconnect;
        end

        {WINHTTP_CALLBACK_STATUS_SECURE_FAILURE}
        else If (dwInternetStatus = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE) then begin
          if Assigned(LWebSocketClient.OnError) then
            LWebSocketClient.OnError(
              LWebSocketClient,
              'One or more errors were encountered while retrieving a Secure Sockets Layer (SSL) certificate from the server');
          if LWebSocketClient.AutoReconnect then
            LWebSocketClient.Reconnect;
        end

      Except
        on E: Exception do begin
          if assigned(LWebSocketClient.OnError) then LWebSocketClient.OnError(LWebSocketClient, AnsiString(E.Message));
          if LWebSocketClient.AutoReconnect then LWebSocketClient.Reconnect
          else raise;
        end;
      end;

    end);

end;

{***********************************************************************************}
class function TALWinHTTPWebSocketClientSendQueueItem.Create(const aData: AnsiString;
                                                             const aIsUTF8: Boolean = True): TALWinHTTPWebSocketClientSendQueueItem;
begin
  result.Data := aData;
  result.IsUTF8 := aIsUTF8;
end;

{*****************************************************************************************************}
constructor TALWinHTTPWebSocketClientContext.Create(const aWebSocketClient: TALWinHTTPWebSocketClient);
begin
  FWebSocketClient := aWebSocketClient;
end;

{*******************************************}
constructor TALWinHTTPWebSocketClient.Create;
begin
  inherited;
  fContext := nil;
  FInetRoot := nil;
  FInetConnect := nil;
  FInetRequest := nil;
  FInetWebSocket := nil;
  FConnected := False;
  FWebSocketReady := False;
  FURL:= '';
  FURLHost := '';
  FURLPath := '';
  FURLPort := INTERNET_DEFAULT_HTTP_PORT;
  FURLScheme := INTERNET_SCHEME_HTTP;
  FOnStatus := nil;
  FAccessType := wHttpAt_NO_PROXY;
  FInternetOptions := [wHttpIo_Keep_connection];
  RequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALWinHTTPWebSocketClient)';
  setlength(FReceiveBuffer,BufferSize);
  FFragmentReceiveBuffer := '';
  FSendqueue := TQueue<TALWinHTTPWebSocketClientSendQueueItem>.Create;
end;

{*******************************************}
destructor TALWinHTTPWebSocketClient.Destroy;
begin
  Disconnect;
  ALFreeAndNil(FSendqueue);
  inherited;
end;

{*************************************************************}
procedure TALWinHTTPWebSocketClient.CheckError(ErrCode: DWORD);
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
    raise EALWebSocketClientException.CreateFmt('%s - URL:%s', [ALTrim(S), URL]);      { Do not localize }
  end;
end;

{*************************************************************}
procedure TALWinHTTPWebSocketClient.CheckError(Error: Boolean);
begin
  if Error then
    CheckError(GetLastError);
end;

{******************************************************************}
procedure TALWinHTTPWebSocketClient.SetURL(const Value: AnsiString);
Var LSchemeName,
    LHostName,
    LUserName,
    LPassword,
    LUrlPath,
    LExtraInfo: AnsiString;
    LPortNumber: integer;
begin
  If Value <> Url then Begin
    Disconnect;
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
      if ALSameText(LSchemeName, 'wss') then FURLScheme := INTERNET_SCHEME_HTTPS
      else if ALSameText(LSchemeName, 'ws') then FURLScheme := INTERNET_SCHEME_HTTP
      else raise Exception.Createfmt('Unknown scheme (%s)',[LSchemeName]);
      FURLPort := LPortNumber;
      FURLHost := LHostName;
      FURLPath := LUrlPath + LExtraInfo;
    end
    else begin
      FURLPort := INTERNET_DEFAULT_HTTP_PORT;
      FURLHost := '';
      FURLPath := '';
      FURLScheme := INTERNET_SCHEME_HTTP;
    end;
    Furl := Value;
  end;
end;

{*****************************************************************}
procedure TALWinHTTPWebSocketClient.Connect(const aUrl:AnsiString);

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

const
  AccessTypeArr: Array[TALWinHttpClientInternetOpenAccessType] of DWord = (WINHTTP_ACCESS_TYPE_NO_PROXY,
                                                                           WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                                                                           WINHTTP_ACCESS_TYPE_NAMED_PROXY);

var
  LSetStatusCallbackResult: TWinHttpStatusCallback;
  LHeader: String;
  LDWordOption: DWord;
  LStrProtocolVersion: String;
  LAcceptTypes: array of PWideChar;
  LCertContextSet: Boolean;
  LErrCode: DWORD;

begin

  {init URL}
  SetURL(aUrl);
  if URL = '' then raise EALHTTPClientException.Create(CALHTTPCLient_MsgEmptyURL);
  if (FConnected) then Exit;

  try

    {update the ALActiveWinHTTPWebSocketClient}
    ALFreeAndNil(fContext);
    fContext := TALWinHTTPWebSocketClientContext.Create(self);
    ActiveWebSocketClient.TryAdd(DWORD_PTR(fContext), True);

    {init FInetRoot}
    FInetRoot := WinHttpOpen(nil, // _In_opt_ LPCWSTR pwszUserAgent,
                             AccessTypeArr[FAccessType], // _In_     DWORD   dwAccessType,
                             InternalGetProxyServerName, // _In_     LPCWSTR pwszProxyName,
                             InternalGetProxyBypass, // _In_     LPCWSTR pwszProxyBypass,
                             WINHTTP_FLAG_ASYNC); // _In_     DWORD   dwFlags
    CheckError(not Assigned(FInetRoot));

    {Register the callback function}
    LSetStatusCallbackResult := WinHttpSetStatusCallback(FInetRoot, // _In_       HINTERNET               hInternet,
                                                         @ALWinHTTPWebSocketClientInetRootStatusCallback, // _In_       WINHTTP_STATUS_CALLBACK lpfnInternetCallback,
                                                         WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, // _In_       DWORD                   dwNotificationFlags,
                                                         0); // _Reserved_ DWORD_PTR               dwReserved
    CheckError(@LSetStatusCallbackResult = @WINHTTP_INVALID_STATUS_CALLBACK);

    {init FInetConnect}
    FInetConnect := WinHttpConnect(FInetRoot, // _In_       HINTERNET     hSession,
                                   PWideChar(String(FURLHost)), // _In_       LPCWSTR       pswzServerName,
                                   FURLPort, // _In_       INTERNET_PORT nServerPort,
                                   0); //  _Reserved_ DWORD         dwReserved
    CheckError(not Assigned(FInetConnect));

    {init ProtocolVersion}
    If ProtocolVersion = HTTPpv_1_1 then LStrProtocolVersion := 'HTTP/1.1'
    else LStrProtocolVersion := 'HTTP/1.0';

    {init AcceptTypes}
    SetLength(LAcceptTypes, 2);
    LAcceptTypes[0] := PWideChar(String(RequestHeader.Accept));
    LAcceptTypes[1] := nil;

    {init AcceptTypes}
    FInetRequest := WinHttpOpenRequest(FInetConnect, // _In_ HINTERNET hConnect,
                                       PWideChar('GET'), //  _In_ LPCWSTR   pwszVerb,
                                       PWideChar(String(FURLPath)), // _In_ LPCWSTR   pwszObjectName,
                                       PWideChar(LStrProtocolVersion), // _In_ LPCWSTR   pwszVersion,
                                       PWideChar(String(requestHeader.Referer)), // _In_ LPCWSTR   pwszReferrer,
                                       pointer(LAcceptTypes), // _In_ LPCWSTR   *ppwszAcceptTypes,
                                       InternalGetHttpOpenRequestFlags); // _In_ DWORD     dwFlags
    CheckError(not Assigned(FInetRequest));

    { Timeouts }
    if ConnectTimeout > 0 then begin
      LDWordOption := ConnectTimeout;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_CONNECT_TIMEOUT , Pointer(@LDWordOption), SizeOf(LDWordOption)));
    end;
    if SendTimeout > 0 then begin
      LDWordOption := SendTimeout;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_SEND_TIMEOUT , Pointer(@LDWordOption), SizeOf(LDWordOption)));
    end;
    if ReceiveTimeout > 0 then begin
      LDWordOption := ReceiveTimeout;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_RECEIVE_TIMEOUT, Pointer(@LDWordOption), SizeOf(LDWordOption)));
    end;

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_PROXY_USERNAME, PWidechar(String(ProxyParams.ProxyUserName)), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_PROXY_PASSWORD, PWidechar(String(ProxyParams.ProxyPassword)), length(ProxyParams.ProxyPassword)));

    {wHttpIo_Keep_connection, wHttpIo_No_cookies and wHttpIo_No_auto_redirect}
    If not (wHttpIo_Keep_connection in InternetOptions) then begin
      LDWordOption := WINHTTP_DISABLE_KEEP_ALIVE;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LDWordOption), sizeof(LDWordOption)));
    end;
    If (wHttpIo_No_cookies in InternetOptions) then begin
      LDWordOption := WINHTTP_DISABLE_COOKIES;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LDWordOption), sizeof(LDWordOption)));
    end;
    If (wHttpIo_No_auto_redirect in InternetOptions) then begin
      LDWordOption := WINHTTP_DISABLE_REDIRECTS;
      CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_DISABLE_FEATURE, Pointer(@LDWordOption), sizeof(LDWordOption)));
    end;

    { WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET }
    CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET, nil, 0));

    {set the header}
    LHeader := String(requestHeader.RawHeaderText);
    CheckError(not WinHttpAddRequestHeaders(FInetRequest, // _In_ HINTERNET hRequest,
                                            PWideChar(LHeader), // _In_ LPCWSTR   pwszHeaders,
                                            Length(LHeader),  // _In_ DWORD     dwHeadersLength,
                                            WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD)); // _In_ DWORD     dwModifiers

    LCertContextSet := False;
    repeat
      if not WinHttpSendRequest(FInetRequest, // _In_     HINTERNET hRequest,
                                WINHTTP_NO_ADDITIONAL_HEADERS, // _In_opt_ LPCWSTR   pwszHeaders,
                                0, // _In_     DWORD     dwHeadersLength,
                                nil, // _In_opt_ LPVOID    lpOptional,
                                0, //  _In_     DWORD     dwOptionalLength,
                                0, // _In_     DWORD     dwTotalLength,
                                DWORD_PTR(fContext)) then begin // _In_     DWORD_PTR dwContext
        LErrCode := GetLastError;
        if (not LCertContextSet) and (LErrCode = ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) then begin
          CheckError(not WinHttpSetOption(FInetRequest, WINHTTP_OPTION_CLIENT_CERT_CONTEXT, WINHTTP_NO_CLIENT_CERT_CONTEXT, 0));
          LCertContextSet := True;
        end
        else
          checkError(LErrCode);
      end
      else LErrCode := 0;
    until LErrCode = 0;

    {Set FConnected to true}
    FConnected := True;

  Except
    on E: Exception do begin
      if assigned(OnError) then OnError(Self, AnsiString(E.Message));
      if AutoReconnect then Reconnect
      else begin
        Disconnect;
        raise;
      end;
    end;
  end;

end;

{*************************************************************************************}
procedure TALWinHTTPWebSocketClient.DoDisconnect(Const ClearSendQueue: Boolean = True);
begin
  ActiveWebSocketClient.Remove(DWORD_PTR(fContext));
  ALFreeAndNil(fContext);
  //----
  if assigned(FInetRoot) then WinHttpCloseHandle(FInetRoot);
  FInetRoot := nil;
  //----
  if assigned(FInetConnect) then WinHttpCloseHandle(FInetConnect);
  FInetConnect := nil;
  //----
  if assigned(FInetRequest) then WinHttpCloseHandle(FInetRequest);
  FInetRequest := nil;
  //----
  if assigned(FInetWebSocket) then WinHttpCloseHandle(FInetWebSocket);
  FInetWebSocket := nil;
  //----
  if ClearSendQueue then FSendQueue.Clear;
  //----
  FWebSocketReady := False;
  FConnected := False;
end;

{*********************************************}
procedure TALWinHTTPWebSocketClient.Disconnect;
begin
  DoDisconnect(true{ClearSendQueue});
end;

{********************************************}
procedure TALWinHTTPWebSocketClient.Reconnect;
begin
  {$IF CompilerVersion >= 34} // sydney
  TThread.ForceQueue(
    nil, // const AThread: TThread;
    procedure
    var LBool: Boolean;
    begin
      if (not ActiveWebSocketClient.TryGetValue(DWORD_PTR(fContext), LBool)) then exit;
      DoDisconnect(false{ClearSendQueue});
      connect(fUrl);
    end, // const AMethod: TThreadMethod;
    1000); // ADelay: Integer = 0
  {$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      sleep(1000);
      TThread.Synchronize(nil,
        procedure
        var LBool: Boolean;
        begin
          if (not ActiveWebSocketClient.TryGetValue(DWORD_PTR(fContext), LBool)) then exit;
          DoDisconnect(false{ClearSendQueue});
          connect(fUrl);
        end);
    end).Start;
  {$ENDIF}
end;

{****************************************************************}
function TALWinHTTPWebSocketClient.DoSend(const aData: AnsiString;
                                          const aIsUTF8: Boolean = True;
                                          const aEnqueue: Boolean = True): Boolean;
begin
  result := True;
  //-----
  if aEnqueue then
    FSendqueue.Enqueue(TALWinHTTPWebSocketClientSendQueueItem.Create(aData, aIsUTF8));
  if not FWebSocketReady then exit;
  //-----
  try
    CheckError(
      WinHttpWebSocketSend(
        FInetWebSocket, // hWebSocket: HINTERNET;
        ALifThen(
          aIsUTF8,
          WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE,
          WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE), // eBufferType: DWORD;
        Pansichar(aData), // pvBuffer: PVOID;
        length(aData))); // dwBufferLength: DWORD
  except
    on E: Exception do begin
      Result := False;
      if assigned(OnError) then OnError(Self, ansiString(E.Message));
      if AutoReconnect then Reconnect
      else raise;
    end;
  end;
end;

{***************************************************************}
procedure TALWinHTTPWebSocketClient.Send(const aData: AnsiString;
                                         const aIsUTF8: Boolean = True);
begin
  doSend(aData, aIsUTF8, true{aEnqueue});
end;

{***************************************************}
function  TALWinHTTPWebSocketClient.Receive: boolean;
var LPdwBytesRead: DWORD;
    LPeBufferType: DWORD;
begin
  Result := True;
  try
    setlength(FReceiveBuffer,BufferSize);
    CheckError(
      WinHttpWebSocketReceive(
        FInetWebSocket, // hWebSocket: HINTERNET;
        pansiChar(FReceiveBuffer), // pvBuffer: PVOID;
        length(FReceiveBuffer), // dwBufferLength: DWORD;
        LPdwBytesRead, // var pdwBytesRead: DWORD;
        LPeBufferType)); // var peBufferType: DWORD;
  except
    on E: Exception do begin
      Result := False;
      if assigned(OnError) then OnError(Self, ansiString(E.Message));
      if AutoReconnect then Reconnect
      else raise;
    end;
  end;
end;

{***********************************************************************}
procedure TALWinHTTPWebSocketClient.SetBufferSize(const Value: Cardinal);
begin
  if Value <> BufferSize then Disconnect;
  inherited;
end;

{***************************************************************************}
procedure TALWinHTTPWebSocketClient.SetUsername(const NameValue: AnsiString);
begin
  If UserName <> NameValue then Disconnect;
  inherited;
end;

{*******************************************************************************}
procedure TALWinHTTPWebSocketClient.SetPassword(const PasswordValue: AnsiString);
begin
  IF Password <> PasswordValue then Disconnect;
  inherited;
end;

{*****************************************************************************************************}
procedure TALWinHTTPWebSocketClient.SetAccessType(const Value: TALWinHTTPClientInternetOpenAccessType);
begin
  If (value <> AccessType) then begin
    Disconnect;
    FaccessType := value;
  end;
end;

{*****************************************************************************************************}
procedure TALWinHTTPWebSocketClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

initialization
  TALWinHTTPWebSocketClient.ActiveWebSocketClient := TDictionary<DWORD_PTR, Boolean>.create;

finalization
  ALFreeAndNil(TALWinHTTPWebSocketClient.ActiveWebSocketClient);

end.
