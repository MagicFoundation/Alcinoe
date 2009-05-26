{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALWinInetFTPClient
Version:      3.50

Description:  TALWinInetFTPClient is a is easy to use WinInet-based
              FTP client component.

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

Know bug :

History :

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALWinInetFTPClient;

interface

uses Windows,
     SysUtils,
     Classes,
     ALFTPClient,
     WinInet;

type

  {-------------------------------------}
  TALWinInetFTPInternetOpenAccessType = (
                                         wFtpAt_Direct,                       {Resolves all host names locally.}
                                         wFtpAt_Preconfig,                    {Retrieves the proxy or direct configuration from the registry.}
                                         wFtpAt_Preconfig_with_no_autoproxy,  {Retrieves the proxy or direct configuration from the registry and
                                                                               prevents the use of a startup Microsoft JScript or Internet Setup
                                                                               (INS) file.}
                                         wFtpAt_Proxy                         {Passes requests to the proxy unless a proxy bypass list is supplied
                                                                               and the name to be resolved bypasses the proxy. In this case, the
                                                                               function uses Ioat_Direct.}
                                        );

  {-----------------------------------}
  TAlWinInetFTPClientInternetOption = (
                                       wFtpIo_Async,                     {NOT SUPPORTED YET!
                                                                           Makes only asynchronous requests on handles descended from the handle
                                                                           returned from InternetOpen function.}
                                       wFtpIo_From_Cache,                {Does not make network requests. All entities are returned from the cache.
                                                                          If the requested item is not in the cache, a suitable error, such as
                                                                          ERROR_FILE_NOT_FOUND, is returned.}
                                       wFtpIo_Offline,                   {Identical to Hcio_From_Cache. Does not make network requests.
                                                                          All entities are returned from the cache. If the requested item is not
                                                                          in the cache, a suitable error, such as ERROR_FILE_NOT_FOUND, is returned.}
                                       wFtpIo_Passive,                   {Uses passive FTP semantics}
                                       wFtpIo_Hyperlink,                 {Forces a reload if there was no Expires time and no LastModified time returned
                                                                          from the server when determining whether to reload the item from the network.}
                                       wFtpIo_Need_file,                 {Causes a temporary file to be created if the file cannot be cached.}
                                       wftpIo_No_cache_write,            {Does not add the returned entity to the cache.}
                                       wftpIo_Reload,                    {Forces a download of the requested file, object, or directory listing from the origin
                                                                          server, not from the cache.}
                                       wftpIo_Resynchronize              {Reloads HTTP resources if the resource has been modified since the last time it was downloaded.
                                                                          All FTP and Gopher resources are reloaded.}
                                      );

  {--------------------------------------------------------}
  TALWinInetFtpTransferType = (wFtpTt_ASCII, wFtpTt_BINARY);

  {------------------------------------------------------------------------------}
  TALWinInetFTPClientInternetOptionSet = Set of TAlWinInetFTPClientInternetOption;

  {------------------------------------}
  {TAlWinInetFTPClientStatusChangeEvent

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
  TAlWinInetFTPClientStatusChangeEvent  = procedure(sender: Tobject; InternetStatus: DWord; StatusInformation: Pointer; StatusInformationLength: DWord) of object;

  {---------------------------------------}
  TALWinInetFTPClient = class(TALFTPClient)
  private
    FTranferType: TALWinInetFtpTransferType;
    FAccessType: TALWinInetFtpInternetOpenAccessType;
    FInternetOptions: TAlWininetFtpClientInternetOptionSet;
    FConnected: Boolean;
    FInetRoot: HINTERNET;
    FInetConnect: HINTERNET;
    FOnStatusChange: TAlWinInetFtpClientStatusChangeEvent;
    procedure SetAccessType(const Value: TALWinInetFtpInternetOpenAccessType);
    procedure SetInternetOptions(const Value: TAlWininetFtpClientInternetOptionSet);
  protected
    procedure CheckError(Error: Boolean);
    function  FindMatchingFile(var F: TALFtpclientSearchRec): Integer;
    procedure SetServerName(const Value: string); override;
    procedure SetServerPort(const Value: Integer); override;
    procedure SetUsername(const NameValue: string); override;
    procedure SetPassword(const PasswordValue: string); override;
    function  GetConnected: Boolean; override;
    procedure SetConnected(const Value: Boolean); override;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure CreateDirectory(Directory: String); override;
    procedure DeleteFile(FileName: String); override;
    Function  FindFirst(const Path: string; Attr: Integer; var F: TALFtpclientSearchRec): Integer; override;
    Function  FindNext(var F: TALFtpclientSearchRec): Integer; override;
    procedure FindClose(var F: TALFtpclientSearchRec); override;
    Function  GetCurrentDirectory: String; override;
    Procedure GetFile(RemoteFile: String; LocalFile: String; FailIfExists: Boolean); overload; override;
    Procedure GetFile(RemoteFile: String; DataStream: Tstream); overload; override;
    Function  GetFileSize(filename: String): Longword; override;
    Procedure PutFile(LocalFile: String; Remotefile: String); overload; override;
    Procedure PutFile(DataStream: TStream; Remotefile: String); overload; override;
    Procedure RemoveDirectory(Directory: String); override;
    Procedure RenameFile(ExistingFile, NewFile: String); override;
    Procedure SetCurrentDirectory(Directory: String); override;
    procedure Connect; override;
    procedure Disconnect; override;
  published
    property  TransferType: TALWinInetFtpTransferType read FTranferType write FTranferType default wFtpTt_BINARY;
    property  AccessType: TALWinInetFtpInternetOpenAccessType read FAccessType write SetAccessType default wFtpAt_Preconfig;
    property  InternetOptions: TAlWininetFtpClientInternetOptionSet read FInternetOptions write SetInternetOptions default [];
    property  OnStatusChange: TAlWinInetFtpClientStatusChangeEvent read FOnStatusChange write FOnStatusChange;
  end;

procedure Register;

implementation

 {$R ..\resource\ALWinInetFTPClient.dcr}

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALWinInetFTPClient]);
end;




////////////////////////////////////////////////////////////////////////////////
////////// TALWinInetFTPClient ////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{********************************************************************}
procedure ALWininetFTPCLientStatusCallback(InternetSession: hInternet;
                                           Context,
                                           InternetStatus: DWord;
                                           StatusInformation: Pointer;
                                           StatusInformationLength: DWord); stdcall;
begin
  with TALWinInetFTPClient(Context) do begin

    {fire the On Status change event}
    if Assigned(FOnStatusChange) then
      FOnStatusChange(
                      TALWininetFTPClient(Context),
                      InternetStatus,
                      StatusInformation,
                      StatusInformationLength
                     );

  end;
end;

{********************************************************}
constructor TALWinInetFTPClient.Create(Owner: TComponent);
begin
  inherited;
  FInetRoot := nil;
  FInetConnect := nil;
  FConnected := False;
  FAccessType := wftpAt_Preconfig;
  FOnStatusChange := nil;
  FInternetOptions := [];
  TransferType := wFtpTt_BINARY;
end;

{*************************************}
destructor TALWinInetFTPClient.Destroy;
begin
  if Assigned(FInetConnect) then InternetSetStatusCallback(FInetConnect, nil);
  if Assigned(FInetRoot) then InternetSetStatusCallback(FInetRoot, nil);
  Disconnect;
  inherited;
end;

{*******************************************************}
procedure TALWinInetFTPClient.CheckError(Error: Boolean);
var ErrCode: DWord;
    ErrMsg: string;
    ErrMsgln: Dword;

    {-----------------------------------------}
    Procedure internalFormatMessageFromErrCode;
    Begin
      SetLength(ErrMsg, 256);
      FormatMessage(
                    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_FROM_HMODULE,
                    Pointer(GetModuleHandle('wininet.dll')),
                    ErrCode,
                    0,
                    PChar(ErrMsg),
                    Length(ErrMsg),
                    nil
                   );
      SetLength(ErrMsg, StrLen(PChar(ErrMsg)));
      while (Length(ErrMsg) > 0) and (ErrMsg[Length(ErrMsg)] in [#10, #13]) do SetLength(ErrMsg, Length(ErrMsg) - 1);
    end;

begin
  ErrCode := GetLastError;
  if Error and (ErrCode <> 0) then begin
    If ErrCode = ERROR_INTERNET_EXTENDED_ERROR then begin
      ErrMsgLn := 0;
      ErrMsg := '';
      if not InternetGetLastResponseInfo(
                                         ErrCode,
                                         Pchar(ErrMsg),
                                         ErrMsgln
                                        ) then begin
        ErrCode := GetLastError;
        if ErrCode=ERROR_INSUFFICIENT_BUFFER then begin
          SetLength(ErrMsg,ErrMsgLn);
          InternetGetLastResponseInfo(
                                      ErrCode,
                                      Pchar(ErrMsg),
                                      ErrMsgln
                                     );
        end
        else internalFormatMessageFromErrCode;
      end;
    end
    else internalFormatMessageFromErrCode;

    raise EALFTPClientException.Create(ErrMsg);
  end;
end;

{***************************************************************}
procedure TALWinInetFTPClient.SetServerName(const Value: string);
begin
  If ServerName <> Value then Disconnect;
  inherited;
end;

{****************************************************************}
procedure TALWinInetFTPClient.SetServerPort(const Value: Integer);
begin
  If ServerPort <> Value then Disconnect;
  inherited;
end;

{*****************************************************************}
procedure TALWinInetFTPClient.SetUsername(const NameValue: string);
begin
  If UserName <> NameValue then Disconnect;
  inherited;
end;

{*********************************************************************}
procedure TALWinInetFTPClient.SetPassword(const PasswordValue: string);
begin
  IF Password <> PasswordValue then Disconnect;
  inherited;
end;

{*************************************}
procedure TALWinInetFTPClient.Connect;

  {-----------------------------------------}
  Function InternalGetProxyServerName: PChar;
  Begin
    If (ProxyParams.ProxyServer = '') then result := nil
    else result := PChar(ProxyParams.ProxyServer + ':' + IntToStr(ProxyParams.ProxyPort));
  end;

  {-------------------------------------}
  Function InternalGetProxyBypass: PChar;
  Begin
    {We should not use empty string for ProxyBypass because
     InternetOpen will use it as the proxy bypass list}
    if (ProxyParams.ProxyBypass = '') then result := nil
    else result := Pchar(ProxyParams.ProxyBypass);
  end;

  {-------------------------------------------}
  Function InternalGetInternetOpenFlags: DWord;
  Begin
    Result := 0;
    if wFTPIo_From_Cache in InternetOptions then Result := result or INTERNET_FLAG_FROM_CACHE;
    if wFTPIo_Offline in InternetOptions then Result := result or INTERNET_FLAG_OFFLINE;
  end;

  {----------------------------------------------}
  Function InternalGetInternetConnectFlags: DWord;
  Begin
    Result := 0;
    if wFtpIo_Passive in InternetOptions then Result := result or INTERNET_FLAG_PASSIVE;
  end;

const AccessTypeArr: Array[TALWinInetFTPInternetOpenAccessType] of DWord = (
                                                                             INTERNET_OPEN_TYPE_DIRECT,
                                                                             INTERNET_OPEN_TYPE_PRECONFIG,
                                                                             INTERNET_OPEN_TYPE_PRECONFIG_WITH_NO_AUTOPROXY,
                                                                             INTERNET_OPEN_TYPE_PROXY
                                                                            );

begin
  { Yes, but what if we're connected to a different Host/Port?? }
  { So take advantage of a cached handle, we'll assume that
    Connect(False) will be called explicitly when we're switching
    Host. To that end, SetURL always disconnects }
  if (FConnected) then Exit;

  { Also, could switch to new API introduced in IE4/Preview2}
  if InternetAttemptConnect(0) <> ERROR_SUCCESS then SysUtils.Abort;

  FInetRoot := InternetOpen(
                            PChar(''),
                            AccessTypeArr[FAccessType],
                            InternalGetProxyServerName,
                            InternalGetProxyBypass,
                            InternalGetInternetOpenFlags
                           );

  CheckError(not Assigned(FInetRoot));

  try
    {Register the callback function}
    InternetSetStatusCallback(FInetRoot, @ALWininetFTPCLientStatusCallback);

    FInetConnect := InternetConnect(
                                    FInetRoot,
                                    PChar(ServerName),
                                    ServerPort,
                                    PChar(UserName),
                                    PChar(Password),
                                    INTERNET_SERVICE_FTP,
                                    InternalGetInternetConnectFlags,
                                    Dword(Self)
                                   );

    CheckError(not Assigned(FInetConnect));
    FConnected := True;
  except
    InternetCloseHandle(FInetRoot);
    FInetRoot := nil;
    raise;
  end;

  Try
    { Timeouts }
    if ConnectTimeout > 0 then CheckError(not InternetSetOption(FInetConnect, INTERNET_OPTION_CONNECT_TIMEOUT, Pointer(@ConnectTimeout), SizeOf(ConnectTimeout)));
    if SendTimeout > 0 then CheckError(not InternetSetOption(FInetConnect, INTERNET_OPTION_SEND_TIMEOUT, Pointer(@SendTimeout), SizeOf(SendTimeout)));
    if ReceiveTimeout > 0 then CheckError(not InternetSetOption(FInetConnect, INTERNET_OPTION_RECEIVE_TIMEOUT, Pointer(@ReceiveTimeout), SizeOf(ReceiveTimeout)));

    { proxy user name and password }
    If proxyParams.ProxyUserName <> '' then CheckError(not InternetSetOption(FInetConnect, INTERNET_OPTION_PROXY_USERNAME, Pchar(ProxyParams.ProxyUserName), length(ProxyParams.ProxyUserName)));
    If proxyParams.ProxyPassword <> '' then CheckError(not InternetSetOption(FInetConnect, INTERNET_OPTION_PROXY_PASSWORD, Pchar(ProxyParams.ProxyPassword), length(ProxyParams.ProxyPassword)));
  except
    Disconnect;
    raise;
  end;

end;

{***************************************************************}
procedure TALWinInetFTPClient.CreateDirectory(Directory: String);
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpCreateDirectory(
                                    FInetConnect,
                                    Pchar(Directory)
                                   )
           );
end;

{*********************************************************}
procedure TALWinInetFTPClient.DeleteFile(FileName: String);
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpDeleteFile(
                               FInetConnect,
                               Pchar(FileName)
                              )
           );
end;

{***********************************************************************************}
function TALWinInetFTPClient.FindMatchingFile(var F: TALFtpclientSearchRec): Integer;
var LocalFileTime: TFileTime;
begin
  with F do begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not InternetFindNextFile(F.FindHandle, @F.FindData) then begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := FindData.nFileSizeLow;
    Attr := FindData.dwFileAttributes;
    Name := FindData.cFileName;
  end;
  Result := 0;
end;

{***************************************************************************************************************}
function TALWinInetFTPClient.FindFirst(const Path: string; Attr: Integer; var F: TALFtpclientSearchRec): Integer;

  {-----------------------------------------------}
  Function InternalGetFtpFindFirstFileFlags: DWord;
  Begin
    Result := 0;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_No_cache_write in InternetOptions then Result := result or INTERNET_FLAG_NO_CACHE_WRITE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

const faSpecial = faDirectory;
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FtpFindFirstFile(
                                   FInetConnect,
                                   Pchar(Path),
                                   F.FindData,
                                   InternalGetFtpFindFirstFileFlags,
                                   Dword(Self)
                                  );

  if F.FindHandle <> nil then begin
    Result := FindMatchingFile(F);
    if Result <> 0 then FindClose(F);
  end
  else Result := GetLastError;
end;

{***************************************************************************}
function TALWinInetFTPClient.FindNext(var F: TALFtpclientSearchRec): Integer;
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  if InternetFindNextFile(F.FindHandle, @F.FindData) then Result := FindMatchingFile(F)
  else Result := GetLastError;
end;

{********************************************************************}
Procedure TALWinInetFTPClient.FindClose(var F: TALFtpclientSearchRec);
begin
  if F.FindHandle <> nil then begin
    InternetCloseHandle(F.FindHandle);
    F.FindHandle := nil;
  end;
end;

{*******************************************************}
function TALWinInetFTPClient.GetCurrentDirectory: String;
var Len: Dword;
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  Len := 0;
  Result := '';
  If not FtpGetCurrentDirectory(
                                FInetConnect,
                                Pchar(Result),
                                Len
                               ) then begin
    CheckError(GetLastError <> ERROR_INSUFFICIENT_BUFFER);
    SetLength(Result,Len);
    CheckError(
               not FtpGetCurrentDirectory(
                                          FInetConnect,
                                          Pchar(Result),
                                          Len
                                         )
              );
  end;
end;

{*****************************************************************************}
procedure TALWinInetFTPClient.GetFile(RemoteFile: String; DataStream: Tstream);

  {------------------------------------------}
  Function InternalGetFtpOpenFileFlags: DWord;
  Begin
    Result := 0;
    if FTranferType = wFtpTt_ASCII then Result := result or INTERNET_FLAG_TRANSFER_ASCII
    else Result := result or INTERNET_FLAG_TRANSFER_BINARY;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

var Size,
    Downloaded,
    ContentlengthDownloaded,
    ContentLength: DWord;
    S: string;
    hFile: HINTERNET;
    nFileSizeHigh: LongWord;

begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  HFile := FtpOpenFile(
                       FInetConnect,
                       Pchar(RemoteFile),
                       GENERIC_READ,
                       InternalGetFtpOpenFileFlags,
                       Dword(Self)
                      );
  CheckError(not assigned(Hfile));
  Try

    {get the file size}
    ContentLength := FtpGetFileSize(
                                    hFile,
                                    @nFileSizeHigh
                                   );

    { Read data }
    ContentlengthDownloaded := 0;
    repeat
      CheckError(
                 not InternetQueryDataAvailable(
                                                hFile,
                                                Size,
                                                0,
                                                0
                                               )
                );

      if Size > 0 then begin
        SetLength(S, Size);
        CheckError(
                   not InternetReadFile(
                                        HFile,
                                        @S[1],
                                        Size,
                                        Downloaded
                                       )
                  );
        DataStream.Write(S[1], Size);

        { Receiving Data event }
        inc(ContentlengthDownloaded, Downloaded);
        if Assigned(onDownloadProgress) then onDownloadProgress(self, ContentlengthDownloaded, ContentLength)
      end;
    until Size = 0;

  finally
    InternetCloseHandle(hFile);
  end;

end;

{******************************************************************************************}
procedure TALWinInetFTPClient.GetFile(RemoteFile, LocalFile: String; FailIfExists: Boolean);

  {-----------------------------------------}
  Function InternalGetFtpGetFileFlags: DWord;
  Begin
    Result := 0;
    if FTranferType = wFtpTt_ASCII then Result := result or INTERNET_FLAG_TRANSFER_ASCII
    else Result := result or INTERNET_FLAG_TRANSFER_BINARY;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpGetFile(
                            FInetConnect,
                            Pchar(RemoteFile),
                            Pchar(LocalFile),
                            FailIfExists,
                            FILE_ATTRIBUTE_NORMAL,
                            InternalGetFtpGetFileFlags,
                            Dword(Self)
                           )
           );
end;

{*******************************************************************}
function TALWinInetFTPClient.GetFileSize(filename: String): Longword;

  {------------------------------------------}
  Function InternalGetFtpOpenFileFlags: DWord;
  Begin
    Result := 0;
    if FTranferType = wFtpTt_ASCII then Result := result or INTERNET_FLAG_TRANSFER_ASCII
    else Result := result or INTERNET_FLAG_TRANSFER_BINARY;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

Var hFile: HINTERNET;
    nFileSizeHigh: LongWord;
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  HFile := FtpOpenFile(
                       FInetConnect,
                       Pchar(filename),
                       GENERIC_READ,
                       InternalGetFtpOpenFileFlags,
                       Dword(Self)
                      );
  CheckError(not assigned(Hfile));
  Try
    Result := FtpGetFileSize(
                             hFile,
                             @nFileSizeHigh
                            );
  finally
    InternetCloseHandle(hFile);
  end;
end;

{*****************************************************************************}
procedure TALWinInetFTPClient.PutFile(DataStream: TStream; Remotefile: String);

  {------------------------------------------}
  Function InternalGetFtpOpenFileFlags: DWord;
  Begin
    Result := 0;
    if FTranferType = wFtpTt_ASCII then Result := result or INTERNET_FLAG_TRANSFER_ASCII
    else Result := result or INTERNET_FLAG_TRANSFER_BINARY;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

var RetVal: DWord;
    BuffSize, Len: Integer;
    Buffer: TMemoryStream;
    hFile: HINTERNET;

begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  HFile := FtpOpenFile(
                       FInetConnect,
                       Pchar(RemoteFile),
                       GENERIC_WRITE,
                       InternalGetFtpOpenFileFlags,
                       Dword(Self)
                      );
  CheckError(not assigned(Hfile));
  Buffer := TMemoryStream.Create;
  Try

    Buffer.SetSize(UploadBufferSize);
    DataStream.Position := 0;
    BuffSize := DataStream.Size;

    while True do begin
      { Calc length of data to send }
      Len := BuffSize - DataStream.Position;
      if Len > UploadBufferSize then Len := UploadBufferSize;
      { Bail out if zip.. }
      if Len = 0 then break;
      { Read data in buffer and write out}
      Len := DataStream.Read(Buffer.Memory^, Len);
      if Len = 0 then raise EALFtpClientException.Create(CALFtpCLient_MsgInvalidFtpRequest);

      CheckError(
                 not InternetWriteFile(
                                       HFile,
                                       @Buffer.Memory^,
                                       Len,
                                       RetVal
                                      )
                );

      { Posting Data Event }
      if Assigned(OnUploadProgress) then
        OnUploadProgress(self, DataStream.Position, BuffSize);
    end;

  finally
    Buffer.Free;
    InternetCloseHandle(hFile);
  end;

end;

{*******************************************************************}
procedure TALWinInetFTPClient.PutFile(LocalFile, Remotefile: String);

  {-----------------------------------------}
  Function InternalGetFtpPutFileFlags: DWord;
  Begin
    Result := 0;
    if FTranferType = wFtpTt_ASCII then Result := result or INTERNET_FLAG_TRANSFER_ASCII
    else Result := result or INTERNET_FLAG_TRANSFER_BINARY;
    if wFtpIo_Hyperlink in InternetOptions then Result := result or INTERNET_FLAG_HYPERLINK;
    if wFtpIo_Need_file in InternetOptions then Result := result or INTERNET_FLAG_NEED_FILE;
    if wftpIo_Reload in InternetOptions then Result := result or INTERNET_FLAG_RELOAD;
    if wftpIo_Resynchronize in InternetOptions then Result := result or INTERNET_FLAG_RESYNCHRONIZE;
  end;

begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpPutFile(
                            FInetConnect,
                            Pchar(LocalFile),
                            Pchar(RemoteFile),
                            InternalGetFtpPutFileFlags,
                            Dword(Self)
                           )
           );
end;

{***************************************************************}
procedure TALWinInetFTPClient.RemoveDirectory(Directory: String);
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpRemoveDirectory(
                                    FInetConnect,
                                    Pchar(Directory)
                                   )
           );
end;

{**********************************************************************}
procedure TALWinInetFTPClient.RenameFile(ExistingFile, NewFile: String);
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpRenameFile(
                               FInetConnect,
                               Pchar(ExistingFile),
                               Pchar(NewFile)
                              )
           );
end;

{*******************************************************************}
procedure TALWinInetFTPClient.SetCurrentDirectory(Directory: String);
begin
  If Not fconnected then raise EALFTPClientException.Create(CALFTPCLient_MsgNotConnected);
  CheckError(
             not FtpSetCurrentDirectory(
                                        FInetConnect,
                                        Pchar(Directory)
                                       )
           );
end;

{****************************************}
procedure TALWinInetFTPClient.Disconnect;
begin
  if Assigned(FInetConnect) then InternetCloseHandle(FInetConnect);
  FInetConnect := nil;
  if Assigned(FInetRoot) then InternetCloseHandle(FInetRoot);
  FInetRoot := nil;
  FConnected := False;
end;

{**********************************************************************************************}
procedure TALWinInetFTPClient.SetAccessType(const Value: TALWinInetFTPInternetOpenAccessType);
begin
  If (value <> AccessType) then begin
    Disconnect;
    FaccessType := value;
  end;
end;

{************************************************************************************************}
procedure TALWinInetFTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
  if (PropertyIndex = -1) or (PropertyIndex in [0, 1, 2]) then Disconnect; //clear, ProxyBypass, ProxyServer, ProxyPort
end;

{****************************************************************************************************}
procedure TALWinInetFTPClient.SetInternetOptions(const Value: TAlWininetFTPClientInternetOptionSet);
begin
  If Value <> FInternetOptions then begin
    If (wFTPIo_From_Cache in Value) and (not (wFTPIo_From_Cache in FInternetOptions)) or
       (wFTPIo_From_Cache in FInternetOptions) and (not (wFTPIo_From_Cache in Value)) or
       (wFTPIo_Passive in Value) and (not (wFTPIo_Passive in FInternetOptions)) or
       (wFTPIo_Passive in FInternetOptions) and (not (wFTPIo_Passive in Value)) or
       (wFTPIo_Offline in Value) and (not (wFTPIo_Offline in FInternetOptions)) or
       (wFTPIo_Offline in FInternetOptions) and (not (wFTPIo_Offline in Value)) then disconnect;
    FInternetOptions := Value;
  end;
end;

{*************************************************}
function TALWinInetFTPClient.GetConnected: Boolean;
begin
  Result := fConnected;
end;

{***************************************************************}
procedure TALWinInetFTPClient.SetConnected(const Value: Boolean);
begin
  if Value <> Fconnected then begin
    if value then connect
    else disconnect;
  end;
end;

end.

