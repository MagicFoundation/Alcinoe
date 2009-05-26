{*****************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)
							
product:      ALPhpRunner
Version:      3.53

Description:  TALPHPRunnerEngine is a simple but useful component for
              easily use php (any version) as a scripting language
              in Delphi applications. TALPhpRunnerEngine allows to
              execute the PHP scripts within the Delphi program without
              a WebServer. TALPHPRunnerEngine use the ISAPI DLL
              interface of PHP to communicate with PHP engine.

Legal issues: Copyright (C) 1999-2007 by Arkadia Software Engineering

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

History :     29/01/2007: correct status missed in ALPhpRunnerECBServerSupportFunction
                          Add ALL_HTTP in servervariables object
              30/01/2007: Add fconnectioncount to not unload bug when action is processing

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALPhpRunner;

interface

uses Windows,
     Classes,
     sysutils,
     ISAPI2,
     HttpApp,     
     ALHttpCommon,
     ALIsapiHttp;

Const

  {------------------------------------------------------------}
  HSE_REQ_SEND_RESPONSE_HEADER_EX = (HSE_REQ_END_RESERVED + 16);
  HSE_REQ_MAP_URL_TO_PATH_EX      = (HSE_REQ_END_RESERVED + 12);

type

  {------------------------------}
  HSE_SEND_HEADER_EX_INFO = record
    pszStatus : LPCSTR;
    pszHeader : LPCSTR;
    cchStatus : DWORD;
    cchHeader : DWORD;
    fKeepConn : BOOL;
  end;
  LPHSE_SEND_HEADER_EX_INFO = ^HSE_SEND_HEADER_EX_INFO;
  THSE_SEND_HEADER_EX_INFO = HSE_SEND_HEADER_EX_INFO;

  {-------------------------}
  HSE_URL_MAPEX_INFO = record
    lpszPath : array[0..MAX_PATH - 1] of CHAR;
    dwFlags : DWORD;
    cchMatchingPath : DWORD;
    cchMatchingURL : DWORD;
    dwReserved1 : DWORD;
    dwReserved2 : DWORD;
  end;
  LPHSE_URL_MAPEX_INFO = ^HSE_URL_MAPEX_INFO;
  THSE_URL_MAPEX_INFO = HSE_URL_MAPEX_INFO;


{***************************************
Below the list of some server variables.
It's important to init them correctly
before to call the execute methode of
the TALPhpRunnerEngine because they
give to the engine all the neccessary
params

  URL                    (URL=/scripts/rooter.php)
  PATH_INFO              (PATH_INFO=/scripts/rooter.php)
  PATH_TRANSLATED        (PATH_TRANSLATED=C:\InetPub\scripts\rooter.php)
  SCRIPT_NAME            (SCRIPT_NAME=/scripts/rooter.php)
  SCRIPT_FILENAME        (SCRIPT_FILENAME=C:\InetPub\scripts\rooter.php)   => init by php to PATH_TRANSLATED if empty
  DOCUMENT_ROOT      	   (DOCUMENT_ROOT=C:\InetPub\)                       => Server variable introduced in PHP
                                                                              if set, It will be use to calculate
                                                                              Path_Translated (from SCRIPT_NAME first, and
                                                                              if not set them from PATH_INFO)

  REQUEST_METHOD         (REQUEST_METHOD=GET)
  SERVER_PROTOCOL        (SERVER_PROTOCOL=HTTP/1.1)
  QUERY_STRING           (QUERY_STRING=goto=newpost&t=1)                   => Don't forget that Query_string need to be
                                                                              url_encoded
  HTTP_CACHE_CONTROL     (HTTP_CACHE_CONTROL=)
  HTTP_DATE              (HTTP_DATE=)
  HTTP_ACCEPT            (HTTP_ACCEPT=*/*)
  HTTP_FROM              (HTTP_FROM=)
  HTTP_HOST              (HTTP_HOST=127.0.0.1)
  HTTP_IF_MODIFIED_SINCE (HTTP_IF_MODIFIED_SINCE=)
  HTTP_REFERER           (HTTP_REFERER=http://www.yahoo.fr)
  HTTP_USER_AGENT        (HTTP_USER_AGENT=Mozilla/4.0)
  HTTP_CONTENT_ENCODING  (HTTP_CONTENT_ENCODING=)
  CONTENT_TYPE           (CONTENT_TYPE=)
  CONTENT_LENGTH         (CONTENT_LENGTH=0)                                => set automatiquely with the size of the
                                                                              contentstream provided
  HTTP_CONTENT_VERSION   (HTTP_CONTENT_VERSION=)
  HTTP_DERIVED_FROM      (HTTP_DERIVED_FROM=)
  HTTP_EXPIRES	  			 (HTTP_EXPIRES=)
  HTTP_TITLE						 (HTTP_TITLE=)
  REMOTE_ADDR            (REMOTE_ADDR=127.0.0.1)
  REMOTE_HOST            (REMOTE_HOST=127.0.0.1)
  SERVER_PORT            (SERVER_PORT=80)
  HTTP_CONNECTION        (HTTP_CONNECTION=Keep-Alive)
  HTTP_COOKIE						 (HTTP_COOKIE=cookie1=value1; cookie2=Value2)
  HTTP_AUTHORIZATION     (HTTP_AUTHORIZATION=)

  SERVER_SOFTWARE      	 (SERVER_SOFTWARE=Microsoft-IIS/5.1)
  SERVER_NAME            (SERVER_NAME=127.0.0.1)
  AUTH_TYPE              (AUTH_TYPE=)
  REMOTE_USER            (REMOTE_USER=)
  REMOTE_IDENT           (REMOTE_IDENT=)


Below the list of all server variable found in the
code of Php5Isapi.dll

  ALL_HTTP
  APPL_MD_PATH
  APPL_PHYSICAL_PATH
  AUTH_PASSWORD
  AUTH_TYPE
  AUTH_USER
  CERT_COOKIE
  CERT_FLAGS
  CERT_ISSUER
  CERT_KEYSIZE
  CERT_SECRETKEYSIZE
  CERT_SERIALNUMBER
  CERT_SERVER_ISSUER
  CERT_SERVER_SUBJECT
  CERT_SUBJECT
  CONTENT_LENGTH
  CONTENT_TYPE
  DOCUMENT_ROOT
  HTTP_AUTHORIZATION
  HTTP_COOKIE
  HTTPS
  HTTPS_KEYSIZE
  HTTPS_SECRETKEYSIZE
  HTTPS_SERVER_ISSUER
  HTTPS_SERVER_SUBJECT
  INSTANCE_ID
  INSTANCE_META_PATH
  LOGON_USER
  ORIG_PATH_INFO
  ORIG_PATH_TRANSLATED
  PATH_INFO
  PATH_TRANSLATED
  PHP_AUTH_PW
  PHP_AUTH_USER
  PHP_SELF
  QUERY_STRING
  REMOTE_ADDR
  REMOTE_HOST
  REMOTE_USER
  REQUEST_METHOD
  REQUEST_URI
  SCRIPT_FILENAME
  SCRIPT_NAME
  SERVER_NAME
  SERVER_PORT
  SERVER_PORT_SECURE
  SERVER_PROTOCOL
  SERVER_SIGNATURE
  SERVER_SOFTWARE
  SSL_CLIENT_C
  SSL_CLIENT_CN
  SSL_CLIENT_DN
  SSL_CLIENT_EMAIL
  SSL_CLIENT_I_C
  SSL_CLIENT_I_CN
  SSL_CLIENT_I_DN
  SSL_CLIENT_I_EMAIL
  SSL_CLIENT_I_L
  SSL_CLIENT_I_O
  SSL_CLIENT_I_OU
  SSL_CLIENT_I_ST
  SSL_CLIENT_L
  SSL_CLIENT_O
  SSL_CLIENT_OU
  SSL_CLIENT_ST
  URL

***************************************}

  {---------------------------------}
  TALPhpRunnerEngine = class(Tobject)
  private
    fConnectionCount: Integer;
    fDLLhandle: THandle;
    fHttpExtensionProcFunct: THttpExtensionProc;
    function GetDllLoaded: Boolean;
  protected
    procedure CheckError(Error: Boolean);
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure LoadDLL(const DLLFileName: String); virtual;
    Procedure UnloadDLL; virtual;
    procedure Execute(WebRequest: TALIsapiRequest; ResponseContentStream: Tstream; ResponseHeader: TALHTTPResponseHeader); overload; virtual;
    function  Execute(WebRequest: TALIsapiRequest): String; overload; virtual;
    procedure Execute(ServerVariables: Tstrings; RequestContentStream: Tstream; ResponseContentStream: Tstream; ResponseHeader: TALHTTPResponseHeader); overload; virtual;
    function  Execute(ServerVariables: Tstrings; RequestContentStream: Tstream): String; overload; virtual;
    procedure Execute(ServerVariables: Tstrings; RequestContentString: String; ResponseContentStream: Tstream; ResponseHeader: TALHTTPResponseHeader); overload; virtual;
    function  Execute(ServerVariables: Tstrings; RequestContentString: String): String; overload; virtual;
    procedure ExecutePostUrlEncoded(ServerVariables: Tstrings; PostDataStrings: TStrings; ResponseContentStream: Tstream; ResponseHeader: TALHTTPResponseHeader; Const EncodeParams: Boolean=True); overload; virtual;
    function  ExecutePostUrlEncoded(ServerVariables: Tstrings; PostDataStrings: TStrings; Const EncodeParams: Boolean=True): String; overload; virtual;
    Property  DLLLoaded: Boolean read GetDllLoaded;
    property  Dllhandle: THandle read fDLLhandle;
  end;

  {----------------------------------}
  TALPhpRunnerBaseECB = class(Tobject)
  public
    ECB: TEXTENSION_CONTROL_BLOCK;
    ResponseContentStream: TStream;
    ResponseHeader: TALHTTPResponseHeader;
    constructor Create; virtual;
    Function GetServerVariableValue(aName: String): String; virtual; abstract;
  end;

  {----------------------------------------------------}
  TALPhpRunnerWebRequestECB = class(TALPhpRunnerBaseECB)
  public
    ServerVariablesObj: TWebRequest;
    constructor Create; override;
    Function GetServerVariableValue(aName: String): String; override;
  end;

  {--------------------------------------------------}
  TALPhpRunnerTstringsECB = class(TALPhpRunnerBaseECB)
  public
    ServerVariablesObj: Tstrings;
    constructor Create; override;
    Function GetServerVariableValue(aName: String): String; override;
  end;

{--------------------}
function ALPhpRunnerECBGetServerVariable(hConn: HCONN; VariableName: PChar; Buffer: Pointer; var Size: DWORD ): BOOL; stdcall;
function ALPhpRunnerECBWriteClient(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD): BOOL; stdcall;
function ALPhpRunnerECBReadClient(ConnID: HCONN; Buffer: Pointer; var Size: DWORD ): BOOL; stdcall;
function ALPhpRunnerECBServerSupportFunction(hConn: HCONN; HSERRequest: DWORD; Buffer: Pointer; Size: LPDWORD; DataType: LPDWORD ): BOOL; stdcall;

implementation

uses AlFcnString;

{****************************************************************************************************************************}
function ALPhpRunnerECBGetServerVariable(hConn: HCONN; VariableName: PChar; Buffer: Pointer; var Size: DWORD ): BOOL; stdcall;
Var TmpS: String;
    ln: Integer;
begin
  Try

    TmpS := TALPhpRunnerBaseECB(hConn).GetServerVariableValue(VariableName);
    LN := length(TmpS) + 1;
    If size < Dword(LN) then begin
      Result := False;
      SetLastError(ERROR_INSUFFICIENT_BUFFER);
      Size := Ln;
    end
    else begin
      Result := True;
      StrPCopy(PChar(buffer), tmpS);
      size:=Ln;
    end;

  Except
    Result := False;
  end;
end;

{*********************************************************************************************************************}
function ALPhpRunnerECBWriteClient(ConnID: HCONN; Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD): BOOL; stdcall;
begin
  Try
    TALPhpRunnerBaseECB(ConnID).ResponseContentStream.Write(Buffer^,Bytes);
    Result := True;
  except
    Result := False;
  end;
end;

{*************************************************************************************************}
function ALPhpRunnerECBReadClient(ConnID: HCONN; Buffer: Pointer; var Size: DWORD ): BOOL; stdcall;
begin
  Result := True;
  Size := 0;
end;

{************************************************************************************************************************************************}
function ALPhpRunnerECBServerSupportFunction(hConn: HCONN; HSERRequest: DWORD; Buffer: Pointer; Size: LPDWORD; DataType: LPDWORD ): BOOL; stdcall;
Var HeaderInfoEx : THSE_SEND_HEADER_EX_INFO;
    MapInfo : LPHSE_URL_MAPEX_INFO;
    DocumentRoot: String;
    TmpPath: String;    
    Ln: integer;
begin
  Try

    Case HSERRequest of

      {----------}
      HSE_REQ_SEND_RESPONSE_HEADER_EX: begin
                                         With TALPhpRunnerBaseECB(HCONN).ResponseHeader do begin
                                           Result := true;
                                           HeaderInfoEx := HSE_SEND_HEADER_EX_INFO(Buffer^);
                                           RawHeaderText := AlCopyStr(HeaderInfoEx.pszStatus,1,HeaderInfoEx.cchStatus) + #13#10 +
                                                            AlCopyStr(HeaderInfoEx.pszHeader,1,HeaderInfoEx.cchHeader);
                                         end;
                                       end;

      {----------}
      HSE_REQ_MAP_URL_TO_PATH_EX: begin
                                    MapInfo := LPHSE_URL_MAPEX_INFO(DataType);
                                    TmpPath := String(Pchar(Buffer));
                                    If TmpPath = '' then result := false
                                    else begin
                                      DocumentRoot := TALPhpRunnerBaseECB(hConn).GetServerVariableValue('DOCUMENT_ROOT');
                                      If DocumentRoot = '' then result := False
                                      else begin
                                        TmpPath := ExpandFilename(IncludeTrailingPathDelimiter(DocumentRoot)+ TmpPath);
                                        Ln := length(TmpPath) + 1;
                                        If Ln < MAX_PATH then begin
                                          result := False;
                                          SetLastError(ERROR_INSUFFICIENT_BUFFER);
                                        end
                                        else begin
                                          Result := true;
                                          StrPCopy(MapInfo^.lpszPath,TmpPath);
                                        end;
                                      end;
                                    end;
                                  end;

      Else Begin
        Result := False;
        SetLastError(ERROR_NO_DATA);
      end;

    end

  except
    Result := False;
  end;
end;


//////////////////////////////
///// TALPhpRunnerEngine /////
//////////////////////////////

{******************************************************}
procedure TALPhpRunnerEngine.CheckError(Error: Boolean);
var ErrCode: Integer;
    S: string;
begin
  ErrCode := GetLastError;
  if Error and (ErrCode <> 0) then begin
    SetLength(S, 256);
    FormatMessage(
                  FORMAT_MESSAGE_FROM_SYSTEM,
                  nil,
                  ErrCode,
                  0,
                  PChar(S),
                  Length(S),
                  nil
                 );
    SetLength(S, StrLen(PChar(S)));
    while (Length(S) > 0) and (S[Length(S)] in [#10, #13]) do SetLength(S, Length(S) - 1);
    raise Exception.Create(S);
  end;
end;

{************************************}
constructor TALPhpRunnerEngine.Create;
begin
  fConnectionCount := 0;
  fDLLhandle:=0;
  fHttpExtensionProcFunct := nil;
end;

{************************************}
destructor TALPhpRunnerEngine.Destroy;
begin
  UnloadDLL;
  inherited;
end;

{************************************************}
function TALPhpRunnerEngine.GetDllLoaded: Boolean;
begin
  result := fDLLhandle <> 0;
end;

{**************************************************************}
procedure TALPhpRunnerEngine.LoadDLL(const DLLFileName: String);
Var GetExtensionVersionFunct: TGetExtensionVersion;
    Version: THSE_VERSION_INFO;
begin
  fDLLhandle := LoadLibrary(Pchar(DLLFileName));
  CheckError(fDLLhandle = 0);
  Try

    @fHttpExtensionProcFunct := GetProcAddress(fDLLHandle, 'HttpExtensionProc');
    CheckError(@fHttpExtensionProcFunct = nil);

    @GetExtensionVersionFunct := GetProcAddress(fDLLHandle, 'GetExtensionVersion');
    CheckError(@GetExtensionVersionFunct = nil);
    If not GetExtensionVersionFunct(Version) then raise exception.Create('Can not use the extension!');

  except
    UnloadDLL;
  end;
end;

{*************************************}
procedure TALPhpRunnerEngine.UnloadDLL;
Var TerminateExtensionFunct : TTerminateExtension;
begin
  If DLLLoaded then Begin
    while InterlockedCompareExchange(fconnectioncount, 0, 0) <> 0 do sleep(100);
    @TerminateExtensionFunct := GetProcAddress(fDLLHandle, 'TerminateExtension');
    If assigned(TerminateExtensionFunct) then TerminateExtensionFunct(HSE_TERM_MUST_UNLOAD);
    CheckError(not FreeLibrary(fDLLHandle));
    fHttpExtensionProcFunct := nil;
    fDLLHandle := 0;
  end;
end;

{***************************************************************}
procedure TALPhpRunnerEngine.Execute(WebRequest: TALIsapiRequest;
                                     ResponseContentStream: Tstream;
                                     ResponseHeader: TALHTTPResponseHeader);
Var aPhpRunnerECB: TALPhpRunnerWebRequestECB;
    aHttpExtensionProcResult: DWORD;
begin
  If not DLLLoaded then raise Exception.Create('DLL is not loaded!');

  aPhpRunnerECB := TALPhpRunnerWebRequestECB.Create;
  InterlockedIncrement(Fconnectioncount);
  Try

    With aPhpRunnerECB.ECB do begin
      lpszMethod := pchar(WebRequest.Method);
      lpszQueryString := pchar(WebRequest.Query);
      lpszPathInfo:= pchar(WebRequest.PathInfo);
      lpszPathTranslated:= pchar(WebRequest.PathTranslated);
      lpszContentType:= pchar(WebRequest.ContentType);

      cbTotalBytes:=WebRequest.ContentStream.Size;
      cbAvailable:=WebRequest.ContentStream.Size;
      If cbTotalBytes > 0 then begin
        GetMem(lpbData, cbTotalBytes);
        WebRequest.ContentStream.Position := 0;
        WebRequest.ContentStream.read(lpbData^, cbTotalBytes);
      end
      else lpbData := nil;
    end;

    aPhpRunnerECB.ServerVariablesObj := WebRequest;
    aPhpRunnerECB.ResponseContentStream := ResponseContentStream;
    aPhpRunnerECB.ResponseHeader := ResponseHeader;

    aHttpExtensionProcResult := fHttpExtensionProcFunct(aPhpRunnerECB.ECB);
    if aHttpExtensionProcResult <> HSE_STATUS_SUCCESS then raise exception.Create('The extension has encountered an error while processing the request!');

  finally
    With aPhpRunnerECB.ECB do
      if lpbData <> nil then FreeMem(lpbData, cbTotalBytes);
    aPhpRunnerECB.free;
    InterlockedDecrement(Fconnectioncount);
  end;
end;

{***********************************************************************}
function TALPhpRunnerEngine.Execute(WebRequest: TALIsapiRequest): String;
var ResponseContentStream: TStringStream;
    ResponseHeader: TALHTTPResponseHeader;
begin
  ResponseContentStream := TStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    Execute(
            WebRequest,
            ResponseContentStream,
            ResponseHeader
           );
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.Free;
    ResponseHeader.Free;
  end;
end;

{*************************************************************}
procedure TALPhpRunnerEngine.Execute(ServerVariables: Tstrings;
                                     RequestContentStream: Tstream;
                                     ResponseContentStream: Tstream;
                                     ResponseHeader: TALHTTPResponseHeader);
Var aPhpRunnerECB: TALPhpRunnerTstringsECB;
    aHttpExtensionProcResult: DWORD;
    WorkServerVariables: Tstrings;
    i: integer;
    S1, S2: String;
begin
  {exit if the dll is not loaded (off course)}
  If not DLLLoaded then raise Exception.Create('DLL is not loaded!');

  {Create local object}
  aPhpRunnerECB := TALPhpRunnerTstringsECB.Create;
  WorkServerVariables := TstringList.Create;
  InterlockedIncrement(Fconnectioncount);
  Try

    {init WorkServerVariables}
    WorkServerVariables.Assign(ServerVariables);

    {init aPhpRunnerECB.ECB}
    With aPhpRunnerECB.ECB do begin
      lpszMethod := pchar(WorkServerVariables.Values['REQUEST_METHOD']);
      lpszQueryString := pchar(WorkServerVariables.Values['QUERY_STRING']);
      lpszPathInfo:= pchar(WorkServerVariables.Values['PATH_INFO']);
      lpszPathTranslated:= pchar(WorkServerVariables.Values['PATH_TRANSLATED']);
      lpszContentType:= pchar(WorkServerVariables.Values['CONTENT_TYPE']);

      If assigned(RequestContentStream) then begin
        cbTotalBytes:=RequestContentStream.Size;
        cbAvailable:=RequestContentStream.Size;
        If cbTotalBytes > 0 then begin
          GetMem(lpbData, cbTotalBytes);
          RequestContentStream.Position := 0;
          RequestContentStream.read(lpbData^, cbTotalBytes);
        end
        else lpbData := nil;
      end
      else begin
        cbTotalBytes:=0;
        cbAvailable:=0;
        lpbData := nil;
      end;

      WorkServerVariables.Values['CONTENT_LENGTH'] := inttostr(cbTotalBytes);
    end;

    {update the ALL_HTTP value}
    S1 := '';
    For i := 0 to ServerVariables.Count - 1 do begin
      S2 := AlUpperCase(ServerVariables.Names[i])+': '+ServerVariables.ValueFromIndex[i];
      If AlPos('HTTP_',S2) = 1 then S1 := S1 + #13#10 + S2;
    end;
    If S1 <> '' then delete(S1,1,2);
    WorkServerVariables.Values['ALL_HTTP'] := S1;

    {init aPhpRunnerECB properties}
    aPhpRunnerECB.ServerVariablesObj := WorkServerVariables;
    aPhpRunnerECB.ResponseContentStream := ResponseContentStream;
    aPhpRunnerECB.ResponseHeader := ResponseHeader;

    aHttpExtensionProcResult := fHttpExtensionProcFunct(aPhpRunnerECB.ECB);
    if aHttpExtensionProcResult <> HSE_STATUS_SUCCESS then raise exception.Create('The extension has encountered an error while processing the request!');

  finally
    With aPhpRunnerECB.ECB do
      if lpbData <> nil then FreeMem(lpbData, cbTotalBytes);
    aPhpRunnerECB.free;
    WorkServerVariables.Free;
    InterlockedDecrement(Fconnectioncount);
  end;
end;

{************************************************************}
function TALPhpRunnerEngine.Execute(ServerVariables: Tstrings;
                                    RequestContentStream: Tstream): String;
var ResponseContentStream: TStringStream;
    ResponseHeader: TALHTTPResponseHeader;
begin
  ResponseContentStream := TStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    Execute(
            ServerVariables,
            RequestContentStream,
            ResponseContentStream,
            ResponseHeader
           );
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.Free;
    ResponseHeader.Free;
  end;
end;

{*************************************************************}
procedure TALPhpRunnerEngine.Execute(ServerVariables: Tstrings;
                                     RequestContentString: String;
                                     ResponseContentStream: Tstream;
                                     ResponseHeader: TALHTTPResponseHeader);
var RequestContentStream: TstringStream;
begin
  RequestContentStream := TStringStream.Create(RequestContentString);
  Try
    Execute(
            ServerVariables,
            RequestContentStream,
            ResponseContentStream,
            ResponseHeader
           );
  finally
    RequestContentStream.Free;
  end;
end;

{************************************************************}
function TALPhpRunnerEngine.Execute(ServerVariables: Tstrings;
                                    RequestContentString: String): String;
var ResponseContentStream: TStringStream;
    ResponseHeader: TALHTTPResponseHeader;
    RequestContentStream: TstringStream;
begin
  RequestContentStream := TStringStream.Create(RequestContentString);
  ResponseContentStream := TStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    Execute(
            ServerVariables,
            RequestContentStream,
            ResponseContentStream,
            ResponseHeader
           );
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.Free;
    ResponseHeader.Free;
    RequestContentStream.Free;
  end;
end;

{***************************************************************************}
procedure TALPhpRunnerEngine.ExecutePostUrlEncoded(ServerVariables: Tstrings;
                                                   PostDataStrings: TStrings;
                                                   ResponseContentStream: Tstream;
                                                   ResponseHeader: TALHTTPResponseHeader;
                                                   const EncodeParams: Boolean=True);
Var aURLEncodedContentStream: TstringStream;
    I: Integer;
begin
  aURLEncodedContentStream := TstringStream.create('');
  try

    if EncodeParams then ALHTTPEncodeParamNameValues(PostDataStrings);
    With PostDataStrings do
      for i := 0 to Count - 1 do
        If i < Count - 1 then aURLEncodedContentStream.WriteString(Strings[i] + '&')
        else aURLEncodedContentStream.WriteString(Strings[i]);

    ServerVariables.Values['REQUEST_METHOD'] := 'POST';
    ServerVariables.Values['CONTENT_TYPE'] := 'application/x-www-form-urlencoded';

    Execute(
            ServerVariables,
            aURLEncodedContentStream,
            ResponseContentStream,
            ResponseHeader
           );
  finally
    aURLEncodedContentStream.free;
  end;
end;

{**************************************************************************}
function TALPhpRunnerEngine.ExecutePostUrlEncoded(ServerVariables: Tstrings;
                                                  PostDataStrings: TStrings;
                                                  const EncodeParams: Boolean= True): String;
var ResponseContentStream: TStringStream;
    ResponseHeader: TALHTTPResponseHeader;
begin
  ResponseContentStream := TStringStream.Create('');
  ResponseHeader := TALHTTPResponseHeader.Create;
  Try
    ExecutePostUrlEncoded(
                          ServerVariables,
                          PostDataStrings,
                          ResponseContentStream,
                          ResponseHeader
                         );
    Result := ResponseContentStream.DataString;
  finally
    ResponseContentStream.Free;
    ResponseHeader.Free;
  end;
end;


///////////////////////////
///// TALPhpRunnerECB /////
///////////////////////////

{*************************************}
constructor TALPhpRunnerbaseECB.Create;
begin
  ECB.cbSize:=sizeof(TEXTENSION_CONTROL_BLOCK);
  ECB.dwVersion:= MAKELONG(HSE_VERSION_MINOR, HSE_VERSION_MAJOR);
  ECB.ConnID:= THandle(Self);
  ECB.GetServerVariable:=@ALPhpRunnerECBGetServerVariable;
  ECB.WriteClient:=@ALPhpRunnerECBWriteClient;
  ECB.ReadClient:=@ALPhpRunnerECBReadClient;
  ECB.ServerSupportFunction:=@ALPhpRunnerECBServerSupportFunction;
  ECB.lpszLogData:='';
  ECB.lpszMethod := nil;
  ECB.lpszQueryString := nil;
  ECB.lpszPathInfo:= nil;
  ECB.lpszPathTranslated:= nil;
  ECB.lpszContentType:=nil;
  ECB.cbTotalBytes:=0;
  ECB.cbAvailable:=0;
  ECB.lpbData:=nil;
  ECB.dwHttpStatusCode := 0;
  ResponseContentStream := nil;
  ResponseHeader := nil;
end;

/////////////////////////////////////
///// TALPhpRunnerWebRequestECB /////
/////////////////////////////////////

{*******************************************}
constructor TALPhpRunnerWebRequestECB.Create;
begin
  inherited;
  ServerVariablesObj := nil;
end;

{*******************************************************************************}
function TALPhpRunnerWebRequestECB.GetServerVariableValue(aName: String): String;
begin
  Result := ServerVariablesObj.GetFieldByName(aName);
end;

///////////////////////////////////
///// TALPhpRunnerTstringsECB /////
///////////////////////////////////

{*****************************************}
constructor TALPhpRunnerTstringsECB.Create;
begin
  inherited;
  ServerVariablesObj := nil;
end;

{*****************************************************************************}
function TALPhpRunnerTstringsECB.GetServerVariableValue(aName: String): String;
begin
  Result := ServerVariablesObj.Values[aName];
end;

end.
