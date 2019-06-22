{*************************************************************
product:      Alcinoe ISAPI Http WebRequest and WebResponse Objects
Description:  Inherited TWebRequest and TWebResponse for better handling
              of ISAPI Request
**************************************************************}

unit ALIsapiHTTP;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if Web.Win.IsapiHTTP / Web.HTTPApp was not updated and adjust the IFDEF'}
{$IFEND}

uses System.SysUtils,
     System.Classes,
     System.Types,
     Winapi.Isapi2,
     ALMultiPartParser,
     ALHttpClient,
     ALStringList,
     ALString;

type

  {--------------------------------------------------}
  EALIsapiRequestContentSizeTooBig = Class(Exception);
  EALIsapiRequestConnectionDropped = class(Exception);

  {----------------------------}
  TALWebRequest = class(TObject)
  private
    // [Deleted from TwebRequest] FContentParser: TALAbstractContentParser;
    // [Deleted from TwebRequest] FCookieFields: TALStrings;
    // [Deleted from TwebRequest] FQueryFields: TALStrings;
    // [Deleted from TwebRequest] function GetContentParser: TALAbstractContentParser;
    // [Deleted from TwebRequest] function GetFiles: TALAbstractWebRequestFiles;
    // [Deleted from TwebRequest] function GetContentFields: TALStrings;
    // [Deleted from TwebRequest] function GetCookieFields: TALStrings;
    // [Deleted from TwebRequest] function GetQueryFields: TALStrings;
    FMaxContentSize: integer; // [added from TwebRequest]
    function GetContent: AnsiString; // [added from TwebRequest]
    function GetMethodType: TALHTTPMethod; // [added from TwebRequest]
    function GetBytesRange: TInt64DynArray; // [added from TwebRequest]
  protected
    // [Deleted from TwebRequest] function GetRemoteIP: AnsiString; virtual;
    // [Deleted from TwebRequest] function GetRawPathInfo: AnsiString; virtual;
    // [Deleted from TwebRequest] function GetInternalPathInfo: AnsiString; virtual;
    // [Deleted from TwebRequest] function GetInternalScriptName: AnsiString; virtual;
    function GetStringVariable(Index: Integer): AnsiString; virtual; abstract;
    function GetDateVariable(Index: Integer): TDateTime; virtual; abstract;
    function GetIntegerVariable(Index: Integer): Integer; virtual; abstract;
    function GetContentStream: TALStringStream; virtual; abstract; // [added from TwebRequest]
  public
    // [Deleted from TwebRequest] procedure ExtractFields(Separators, _WhiteSpace: TSysCharSet; Content: PAnsiChar; Strings: TALStrings); overload; // Utility to extract fields from a given string buffer
    // [Deleted from TwebRequest] procedure ExtractFields(Separators, _WhiteSpace: TSysCharSet; const Content: AnsiString; Strings: TALStrings); overload;
    // [Deleted from TwebRequest] procedure ExtractContentFields(Strings: TALStrings); // Fills the given string list with the content fields as the result of a POST method
    // [Deleted from TwebRequest] procedure ExtractCookieFields(Strings: TALStrings); // Fills the given string list with values from the cookie header field
    // [Deleted from TwebRequest] procedure ExtractQueryFields(Strings: TALStrings); // Fills the given TStrings with the values from the Query data (ie: data following the "?" in the URL)
    // [Deleted from TwebRequest] function ReadUnicodeString(Count: Integer): AnsiString;
    // [Deleted from TwebRequest] property RemoteIP: AnsiString read GetRemoteIP;
    // [Deleted from TwebRequest] property ContentParser: TALAbstractContentParser read GetContentParser;
    // [Deleted from TwebRequest] property Files: TALAbstractWebRequestFiles read GetFiles;
    // [Deleted from TwebRequest] property ContentFields: TALStrings read GetContentFields;
    // [Deleted from TwebRequest] property CookieFields: TALStrings read GetCookieFields;
    // [Deleted from TwebRequest] property QueryFields: TALStrings read GetQueryFields;
    // [Deleted from TwebRequest] property RawPathInfo: AnsiString read GetRawPathInfo; // PathInfo that has not been decoded.
    // [Deleted from TwebRequest] property InternalPathInfo: AnsiString read GetInternalPathInfo; // Perform special processing to get normalized PathInfo when using WebAppDbg
    // [Deleted from TwebRequest] property InternalScriptName: AnsiString read GetInternalScriptName; // Perform special processing to get normalized ScriptName when using WebAppDbg
    // [Deleted from TwebRequest] property RawContent: AnsiString index 25 read GetStringVariable;
    constructor Create;
    procedure ExtractCookieFields(Fields: TALStrings);
    procedure ExtractQueryFields(Fields: TALStrings);
    procedure ExtractUrlEncodedContentFields(Fields: TALStrings);
    procedure ExtractMultipartFormDataFields(Fields: TALStrings; Files: TALMultiPartFormDataContents);
    function ReadClient(var Buffer; Count: Integer): Integer; virtual; abstract; // Read count bytes from client
    function ReadString(Count: Integer): AnsiString; virtual; abstract; // Read count characters as a string from client
    function TranslateURI(const URI: AnsiString): AnsiString; virtual; abstract; // Translate a relative URI to a local absolute path
    function WriteClient(var Buffer; Count: Integer): Integer; virtual; abstract; // Write count bytes back to client
    function WriteString(const AString: AnsiString): Boolean; virtual; abstract; // Write string contents back to client
    function WriteHeaders(StatusCode: Integer; const ReasonString, Headers: AnsiString): Boolean; virtual; abstract; // Write HTTP header string
    function GetFieldByName(const Name: AnsiString): AnsiString; virtual; abstract; // Read an arbitrary HTTP/Server Field not lists here
    property MethodType: TALHTTPMethod read GetMethodType; // The request method as an enumeration
    property Method: AnsiString index 0 read GetStringVariable;
    property ProtocolVersion: AnsiString index 1 read GetStringVariable;
    property URL: AnsiString index 2 read GetStringVariable;
    property Query: AnsiString index 3 read GetStringVariable;
    property PathInfo: AnsiString index 4 read GetStringVariable;
    property PathTranslated: AnsiString index 5 read GetStringVariable;
    property Authorization: AnsiString index 28 read GetStringVariable;
    property CacheControl: AnsiString index 6 read GetStringVariable;
    property Cookie: AnsiString index 27 read GetStringVariable;
    property Date: TDateTime index 7 read GetDateVariable; // the result is in GMT
    property Accept: AnsiString index 8 read GetStringVariable;
    property From: AnsiString index 9 read GetStringVariable;
    property Host: AnsiString index 10 read GetStringVariable;
    property IfModifiedSince: TDateTime index 11 read GetDateVariable; // the result is in GMT
    property Referer: AnsiString index 12 read GetStringVariable;
    property UserAgent: AnsiString index 13 read GetStringVariable;
    property ContentEncoding: AnsiString index 14 read GetStringVariable;
    property ContentType: AnsiString index 15 read GetStringVariable;
    property ContentLength: Integer index 16 read GetIntegerVariable;
    property ContentVersion: AnsiString index 17 read GetStringVariable;
    Property Content: AnsiString Read GetContent;
    Property ContentStream: TALStringStream Read GetContentStream; // [added from TwebRequest]
    Property MaxContentSize: Integer Read FMaxContentSize Write FMaxContentSize; // [added from TwebRequest]
    property Connection: AnsiString index 26 read GetStringVariable;
    property DerivedFrom: AnsiString index 18 read GetStringVariable;
    property Expires: TDateTime index 19 read GetDateVariable; // the result is in GMT
    property Title: AnsiString index 20 read GetStringVariable;
    property RemoteAddr: AnsiString index 21 read GetStringVariable;
    property RemoteHost: AnsiString index 22 read GetStringVariable;
    property ScriptName: AnsiString index 23 read GetStringVariable;
    property ServerPort: Integer index 24 read GetIntegerVariable;
    property BytesRange: TInt64DynArray read GetBytesRange; // [added from TwebRequest]
  end;

  {------------------------------------}
  TALISAPIRequest = class(TALWebRequest)
  private
    FECB: PEXTENSION_CONTROL_BLOCK;
    FcontentStream: TALStringStream;
    fConnectionClosed: boolean;
    fClientDataExhausted: Boolean;
    function GetHost: AnsiString;
  protected
    // [Deleted from TwebRequest] function GetRawPathInfo: AnsiString; override;
    function GetStringVariable(Index: Integer): AnsiString; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetContentStream: TALStringStream; override; // [added from TwebRequest]
  public
    constructor Create(AECB: PEXTENSION_CONTROL_BLOCK);
    destructor Destroy; override;
    procedure closeConnection;
    procedure ReadClientToStream(const aStream: TStream);
    function GetFieldByName(const Name: AnsiString): AnsiString; override;
    function ReadClient(var Buffer; Count: Integer): Integer; override; // if you use readClient then you need to avoid to use ContentStream
    function ReadString(Count: Integer): AnsiString; override;          // if you use ReadString then you need to avoid to use ContentStream
    function TranslateURI(const URI: AnsiString): AnsiString; override;
    function WriteClient(var Buffer; Count: Integer): Integer; override;
    function WriteString(const AString: AnsiString): Boolean; override;
    function WriteHeaders(StatusCode: Integer; const StatusString, Headers: AnsiString): Boolean; override;
    property ECB: PEXTENSION_CONTROL_BLOCK read FECB;
    property ConnectionClosed: boolean read fConnectionClosed;
    property ClientDataExhausted: boolean read fClientDataExhausted;
  end;

  {-----------------------------}
  TALWebResponse = class(TObject)
  private
    // [Deleted from TWebResponse] function GetUnicodeContent: AnsiString;
    // [Deleted from TWebResponse] procedure SetUnicodeContent(const AValue: AnsiString);
    // [Deleted from TWebResponse] FFreeContentStream: Boolean;
    FContentStream: TStream;
    FCookies: TALHttpCookieCollection;
    procedure SetCustomHeaders(Value: TALStrings);
  protected
    FHTTPRequest: TALWebRequest;
    FCustomHeaders: TALStrings;
    procedure AddCustomHeaders(var Headers: AnsiString);
    function GetStringVariable(Index: Integer): AnsiString; virtual; abstract;
    procedure SetStringVariable(Index: Integer; const Value: AnsiString); virtual; abstract;
    function GetDateVariable(Index: Integer): TDateTime; virtual; abstract;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); virtual; abstract;
    function GetIntegerVariable(Index: Integer): Integer; virtual; abstract;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); virtual; abstract;
    function GetContent: AnsiString; virtual; abstract;
    procedure SetContent(const Value: AnsiString); virtual; abstract;
    procedure SetContentStream(Value: TStream); virtual;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(Value: Integer); virtual; abstract;
    function GetLogMessage: AnsiString; virtual; abstract;
    procedure SetLogMessage(const Value: AnsiString); virtual; abstract;
    function FormatAuthenticate: AnsiString;
  public
    // [Deleted from TWebResponse] property RawContent: AnsiString read GetContent write SetContent;
    constructor Create(HTTPRequest: TALWebRequest);
    destructor Destroy; override;
    function GetCustomHeader(const Name: AnsiString): AnsiString;
    procedure SendResponse; virtual; abstract;
    procedure SendRedirect(const URI: AnsiString); virtual; abstract;
    procedure SendStream(AStream: TStream); virtual; abstract;
    function Sent: Boolean; virtual;
    procedure SetCookieField(Values: TALStrings;
                             const ADomain, APath: AnsiString;
                             AExpires: TDateTime;
                             ASecure: Boolean;
                             const AHttpOnly: Boolean = False);
    procedure SetCustomHeader(const Name, Value: AnsiString);
    property Cookies: TALHttpCookieCollection read FCookies;
    property HTTPRequest: TALWebRequest read FHTTPRequest;
    property ProtocolVersion: AnsiString index 0 read GetStringVariable write SetStringVariable; // [Renamed from TwebRequest] Version
    property ReasonString: AnsiString index 1 read GetStringVariable write SetStringVariable;
    property Server: AnsiString index 2 read GetStringVariable write SetStringVariable;
    property WWWAuthenticate: AnsiString index 3 read GetStringVariable write SetStringVariable;
    property Realm: AnsiString index 4 read GetStringVariable write SetStringVariable;
    property Allow: AnsiString index 5 read GetStringVariable write SetStringVariable;
    property Location: AnsiString index 6 read GetStringVariable write SetStringVariable;
    property ContentEncoding: AnsiString index 7 read GetStringVariable write SetStringVariable;
    property ContentType: AnsiString index 8 read GetStringVariable write SetStringVariable;
    property ContentVersion: AnsiString index 9 read GetStringVariable write SetStringVariable;
    property DerivedFrom: AnsiString index 10 read GetStringVariable write SetStringVariable;
    property Title: AnsiString index 11 read GetStringVariable write SetStringVariable;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    // [Deleted from TwebRequest] property ContentLength: Integer index 0 read GetIntegerVariable write SetIntegerVariable;
    property Date: TDateTime index 0 read GetDateVariable write SetDateVariable;
    property Expires: TDateTime index 1 read GetDateVariable write SetDateVariable;
    property LastModified: TDateTime index 2 read GetDateVariable write SetDateVariable;
    property Content: AnsiString read GetContent write SetContent;
    property ContentStream: TStream read FContentStream write SetContentStream;
    property LogMessage: AnsiString read GetLogMessage write SetLogMessage;
    property CustomHeaders: TALStrings read FCustomHeaders write SetCustomHeaders;
    // [Deleted from TWebResponse] property FreeContentStream: Boolean read FFreeContentStream write FFreeContentStream;
  end;

  {--------------------------------------}
  TALISAPIResponse = class(TALWebResponse)
  private
    FStatusCode: Integer;
    FStringVariables: array[0..11] of AnsiString;
    FIntegerVariables: array[0..0] of Integer;
    FDateVariables: array[0..2] of TDateTime;
    FContent: AnsiString;
    FTransmitFileInfo: THSE_TF_INFO;
    FSent: Boolean;
    fSentInAsync: Boolean;
    function getTransmitFileInfo: PHSE_TF_INFO;
  protected
    function GetContent: AnsiString; override;
    function GetDateVariable(Index: Integer): TDateTime; override;
    function GetIntegerVariable(Index: Integer): Integer; override;
    function GetLogMessage: AnsiString; override;
    function GetStatusCode: Integer; override;
    function GetStringVariable(Index: Integer): AnsiString; override;
    procedure SetContent(const Value: AnsiString); override;
    procedure SetDateVariable(Index: Integer; const Value: TDateTime); override;
    procedure SetIntegerVariable(Index: Integer; Value: Integer); override;
    procedure SetLogMessage(const Value: AnsiString); override;
    procedure SetStatusCode(Value: Integer); override;
    procedure SetStringVariable(Index: Integer; const Value: AnsiString); override;
    procedure InitResponse; virtual;
  public
    constructor Create(HTTPRequest: TALWebRequest);
    procedure SendResponse; override;
    procedure SendRedirect(const URI: AnsiString); override;
    procedure SendStream(AStream: TStream); override;
    function Sent: Boolean; override;
    property SentInAsync: Boolean read fSentInAsync;
    property TransmitFileInfo: PHSE_TF_INFO read getTransmitFileInfo;
  end;

function ALIsapiHttpStatusString(StatusCode: Integer): AnsiString;

//
// Flags for IO Functions, supported for IO Funcs.
//  TF means ServerSupportFunction( HSE_REQ_TRANSMIT_FILE)
//

const
  HSE_IO_SYNC                      = $00000001;   // for WriteClient
  HSE_IO_ASYNC                     = $00000002;   // for WriteClient/TF
  HSE_IO_DISCONNECT_AFTER_SEND     = $00000004;   // for TF
  HSE_IO_SEND_HEADERS              = $00000008;   // for TF

const
  HSE_REQ_CLOSE_CONNECTION = (HSE_REQ_END_RESERVED+17);

implementation

uses Winapi.Windows,
     System.DateUtils,
     System.Ansistrings,
     System.math,
     ALCommon;

const
  ALWebRequestServerVariables: array[0..28] of AnsiString = (
    '',
    'SERVER_PROTOCOL',
    'URL',
    '',
    '',
    '',
    'HTTP_CACHE_CONTROL',
    'HTTP_DATE',
    'HTTP_ACCEPT',
    'HTTP_FROM',
    'HTTP_HOST',
    'HTTP_IF_MODIFIED_SINCE',
    'HTTP_REFERER',
    'HTTP_USER_AGENT',
    'HTTP_CONTENT_ENCODING',
    'CONTENT_TYPE',
    'CONTENT_LENGTH',
    'HTTP_CONTENT_VERSION',
    'HTTP_DERIVED_FROM',
    'HTTP_EXPIRES',
    'HTTP_TITLE',
    'REMOTE_ADDR',
    'REMOTE_HOST',
    'SCRIPT_NAME',
    'SERVER_PORT',
    '',
    'HTTP_CONNECTION',
    'HTTP_COOKIE',
    'HTTP_AUTHORIZATION');

{*******************************}
constructor TALWebRequest.Create;
begin
  inherited Create;
  FMaxContentSize := -1;
end;

{**************************************************************}
procedure TALWebRequest.ExtractCookieFields(Fields: TALStrings);
begin
  ALExtractHeaderFields([';'],  // Separators
                        [' '],  // WhiteSpace
                        [],     // Quotes
                        PAnsiChar(Cookie), // Content
                        Fields,  // Strings
                        True);   // Decode - Cookie encoding:
                                 // There is some confusion over encoding of a cookie value. The commonly held belief
                                 // is that cookie values must be URL-encoded, but this is a fallacy even though it is
                                 // the de facto implementation. The original specification indicates that only three
                                 // types of characters must be encoded: semicolon, comma, and white space. The specification
                                 // indicates that URL encoding may be used but stops short of requiring it. The RFC makes
                                 // no mention of encoding whatsoever. Still, almost all implementations perform
                                 // some sort of URL encoding on cookie values. In the case of name=value formats, the
                                 // name and value are typically encoded separately while the equals sign is left as is.

end;

{*************************************************************}
procedure TALWebRequest.ExtractQueryFields(Fields: TALStrings);
begin
  ALExtractHTTPFields(['&'], // Separators
                      [],    // WhiteSpace
                      [],    // Quotes
                      PAnsiChar(Query), // Content
                      Fields,  // Strings
                      True);   // Decode
end;

{*************************************************************************}
procedure TALWebRequest.ExtractUrlEncodedContentFields(Fields: TALStrings);
begin
  ALExtractHTTPFields(['&'], // Separators
                      [],    // WhiteSpace
                      [],    // Quotes
                      PAnsiChar(Content), // Content
                      Fields,  // Strings
                      True);   // Decode
end;

{**************************************************************************************************************}
procedure TALWebRequest.ExtractMultipartFormDataFields(Fields: TALStrings; Files: TALMultiPartFormDataContents);
var aBoundary: AnsiString;
    aMultipartFormDataDecoder: TALMultipartFormDataDecoder;
begin
  aBoundary := ALMultipartExtractBoundaryFromContentType(ContentType);
  If aBoundary='' then raise Exception.Create('Bad multipart/form-data Content-Type');
  aMultipartFormDataDecoder := TALMultipartFormDataDecoder.Create;
  Try
    aMultipartFormDataDecoder.Decode(Content,
                                     aBoundary,
                                     Fields,
                                     Files);
  Finally
    AlFreeAndNil(aMultipartFormDataDecoder);
  End;
end;

{********************************************}
function TALWebRequest.GetContent: AnsiString;
begin
  result := contentStream.DataString;
end;

{**************************************************}
function TALWebRequest.GetMethodType: TALHTTPMethod;
var aMethodStr : AnsiString;
begin
  aMethodStr := Method;
       if ALSameText(aMethodStr, 'GET')     then result := HTTPmt_Get
  else if ALSameText(aMethodStr, 'POST')    then result := HTTPmt_Post
  else if ALSameText(aMethodStr, 'PUT')     then result := HTTPmt_Put
  else if ALSameText(aMethodStr, 'HEAD')    then result := HTTPmt_Head
  else if ALSameText(aMethodStr, 'TRACE')   then result := HTTPmt_Trace
  else if ALSameText(aMethodStr, 'DELETE')  then result := HTTPmt_Delete
  else if ALSameText(aMethodStr, 'OPTIONS') then result := HTTPmt_Options
  else raise Exception.Create('Unknown method type');
end;

{***************************************************}
function TALWebRequest.GetBytesRange: TInt64DynArray;
var aRangeHeader: ansiString;
    aList: TalStringList;
    aStr: ansiString;
    i: integer;
begin

  //
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Range
  //

  //clear the result
  setlength(Result, 0);

  //get aRangeHeader
  aRangeHeader := GetFieldByName('Range');
  if aRangeHeader = '' then Exit;

  //check that unit is bytes
  //Range: bytes=200-1000, 2000-6576, 19000-
  i := ALPos('=', aRangeHeader); // bytes=200-1000, 2000-6576, 19000-
  if i <= 0 then exit;
  if not ALSameText(alTrim(alcopyStr(aRangeHeader,1,i-1)), 'bytes') then exit;  // bytes
  aRangeHeader := alcopyStr(aRangeHeader, i+1, maxint); // 200-1000, 2000-6576, 19000-

  //move all ranges in result
  //200-1000, 2000-6576, 19000- => [200,1000,2000,6576,19000,-1]
  //200-1000, 2000-6576, -19000 => [200,1000,2000,6576,-1,19000]
  aList := TalStringList.Create;
  try
    aList.LineBreak := ',';
    aList.NameValueSeparator := '-';
    aList.Text := aRangeHeader;
    setlength(result, aList.Count * 2);
    for I := 0 to aList.Count - 1 do begin

      aStr := alTrim(aList.Names[i]);
      if aStr = '' then result[i*2] := -1
      else if not alTryStrToint64(aStr, result[i*2]) then raise Exception.Create('Bad range header');
      //----
      aStr := alTrim(aList.ValueFromIndex[i]);
      if aStr = '' then result[(i*2)+1] := -1
      else if not alTryStrToint64(aStr, result[(i*2)+1]) then raise Exception.Create('Bad range header');
      //----
      if ((result[i*2] = -1) and
          (result[(i*2)+1] = -1)) or
         ((result[(i*2)+1] <> -1) and
          (result[(i*2)+1] < result[i*2])) then raise Exception.Create('Bad range header');

    end;
  finally
    aList.Free;
  end;

end;

{*****************************************************************}
constructor TALISAPIRequest.Create(AECB: PEXTENSION_CONTROL_BLOCK);
begin
  FECB := AECB;
  FcontentStream := nil;
  fConnectionClosed := False;
  fClientDataExhausted := ECB.cbTotalBytes <= ECB.cbAvailable;
  inherited Create;
end;

{*********************************}
destructor TALISAPIRequest.Destroy;
begin
  AlFreeAndNil(FcontentStream);
  inherited;
end;

{**************************************************************************}
function TALISAPIRequest.GetFieldByName(const Name: AnsiString): AnsiString;
var Buffer: array[0..4095] of AnsiChar;
    Size: DWORD;
begin
  Size := SizeOf(Buffer);
  if ECB.GetServerVariable(ECB.ConnID,
                           PAnsiChar(Name),
                           @Buffer,
                           Size) or
     ECB.GetServerVariable(ECB.ConnID,
                           PAnsiChar(AnsiString('HTTP_') + Name),
                           @Buffer,
                           Size)
  then begin
    if Size > 0 then Dec(Size);
    SetString(Result, Buffer, Size);
  end
  else Result := '';
end;

{*********************************************************************}
function TALISAPIRequest.GetStringVariable(Index: Integer): AnsiString;
begin
  case Index of
    0: Result := ECB.lpszMethod;
    3: Result := ECB.lpszQueryString;
    4: Result := ECB.lpszPathInfo;
    5: Result := ECB.lpszPathTranslated;
    10: Result := GetHost;
    1..2, 6..9, 11..24, 26..28: Result := GetFieldByName(ALWebRequestServerVariables[Index]);
    else Result := '';
  end;
end;

{**********************************}
// Strip the port from the host name
function TALISAPIRequest.GetHost: AnsiString;
var I, J: Integer;
begin
  Result := GetFieldByName('HTTP_HOST');
  J := ALPos(']', Result); // Handle Ipv6 host like [::1]:80
  I := ALPosEx(':', Result, J+1);
  if I > 0 then Delete(Result, I, MaxInt);
end;

{****************************************}
procedure TALISAPIRequest.closeConnection;
begin

  // HSE_REQ_CLOSE_CONNECTION closes the client socket connection immediately,
  // but IIS takes a small amount of time to handle the threads in the thread pool
  // before the connection can be completely removed.
  if fConnectionClosed then exit;
  if not ECB.ServerSupportFunction(ECB.ConnID,
                                   HSE_REQ_CLOSE_CONNECTION,
                                   nil,
                                   nil,
                                   nil) then raiseLastOsError;
  fConnectionClosed := true;

end;

{*******************************************************************}
procedure TALISAPIRequest.ReadClientToStream(const aStream: TStream);
var aByteRead: Integer;
    aBuffer: array[0..8191] of Byte;
begin

  If (FMaxContentSize > -1) and (ECB.cbTotalBytes > DWord(FMaxContentSize)) then begin
    closeConnection; // if we don't close the connection IIS will continue to read all the bytes send by the client
    Raise EALIsapiRequestContentSizeTooBig.createFmt('Content size (%d bytes) is bigger than the maximum allowed size (%d bytes)', [ECB.cbTotalBytes, FMaxContentSize]);
  end;

  aStream.Size := ECB.cbTotalBytes; // cbTotalBytes The total number of bytes to be received from the client.
                                    // This is equivalent to the CGI variable CONTENT_LENGTH
  aStream.Position := 0;
  if ECB.cbAvailable > 0 then aStream.WriteBuffer(ECB.lpbData^, ECB.cbAvailable); // The available number of bytes (out of a total of cbTotalBytes) in the buffer pointed to by lpbData.
                                                                                  // If cbTotalBytes is the same as cbAvailable, the lpbData variable will point to a buffer that contains
                                                                                  // all the data as sent by the client. Otherwise, cbTotalBytes will contain the total number of bytes
                                                                                  // of data received. The ISAPI extensions will then need to use the callback function ReadClient to read
                                                                                  // the rest of the data (beginning from an offset of cbAvailable).
  if aStream is TALStringStream then begin
    while aStream.Position < aStream.Size do begin
      aByteRead := ReadClient(Pbyte(TALStringStream(aStream).DataString)[aStream.Position], aStream.Size - aStream.Position);
      if aByteRead <= 0 then break;  // The doc of Delphi say "If no more content is available, ReadClient returns -1."
                                     // but it's false !!
                                     // http://msdn.microsoft.com/en-us/library/ms525214(v=vs.90).aspx
                                     // If the socket on which the server is listening to the client is closed, ReadClient will return TRUE, but with zero bytes read.
      aStream.Position := aStream.Position + aByteRead;
    end;
  end
  else begin
    while aStream.Position < aStream.Size do begin
      aByteRead := ReadClient(aBuffer[0], length(aBuffer));
      if aByteRead <= 0 then break;  // The doc of Delphi say "If no more content is available, ReadClient returns -1."
                                     // but it's false !!
                                     // http://msdn.microsoft.com/en-us/library/ms525214(v=vs.90).aspx
                                     // If the socket on which the server is listening to the client is closed, ReadClient will return TRUE, but with zero bytes read.
      aStream.WriteBuffer(aBuffer[0], aByteRead);
    end;
  end;
  fClientDataExhausted := True;
  aStream.Size := aStream.Position;
  aStream.Position := 0;

  if ContentLength > aStream.Size then
    raise EALIsapiRequestConnectionDropped.Createfmt('Client Dropped Connection.'#13#10 +
      'Total Bytes indicated by Header: %d' + #13#10 +
      'Total Bytes Read: %d',
      [ContentLength, aStream.Size]);

end;

{*********************************************************}
function TALISAPIRequest.GetContentStream: TALStringStream;
begin
  if not assigned(FcontentStream) then begin
    FcontentStream := TALStringStream.Create('');
    ReadClientToStream(FcontentStream);
  end;
  Result := FcontentStream;
end;

{******************************************************************}
function TALISAPIRequest.GetDateVariable(Index: Integer): TDateTime;
var Value: AnsiString;
begin
  Value := GetStringVariable(Index);
  if Value <> '' then Result := ALRfc822StrToGMTDateTime(Value)
  else Result := -1;
end;

{*******************************************************************}
function TALISAPIRequest.GetIntegerVariable(Index: Integer): Integer;
var Value: AnsiString;
begin
  Value := GetStringVariable(Index);
  if Value <> '' then Result := ALStrToInt(Value)
  else Result := -1;
end;

{***********************************************************************}
function TALISAPIRequest.ReadClient(var Buffer; Count: Integer): Integer;
begin
  Result := Count;
  if not ECB.ReadClient(ECB.ConnID, @Buffer, DWORD(Result)) then RaiseLastOsError;
  if Result <= 0 then fClientDataExhausted := True;
end;

{**************************************************************}
function TALISAPIRequest.ReadString(Count: Integer): AnsiString;
var Len: Integer;
begin
  SetLength(Result, Count);
  Len := ReadClient(Pointer(Result)^, Count);
  if Len > 0 then SetLength(Result, Len)
  else Result := '';
end;

{***********************************************************************}
function TALISAPIRequest.TranslateURI(const URI: AnsiString): AnsiString;
var PathBuffer: array[0..1023] of AnsiChar;
    Size: Integer;
begin
  System.Ansistrings.StrCopy(PathBuffer, PAnsiChar(URI));
  Size := SizeOf(PathBuffer);
  if not ECB.ServerSupportFunction(ECB.ConnID,
                                   HSE_REQ_MAP_URL_TO_PATH,
                                   @PathBuffer,
                                   @Size,
                                   nil) then raiseLastOsError;
  Result := PathBuffer;
end;

{************************************************************************}
function TALISAPIRequest.WriteClient(var Buffer; Count: Integer): Integer;
begin
  Result := Count;
  if not ECB.WriteClient(ECB.ConnID, @Buffer, DWORD(Result), 0) then raiseLastOsError;
end;

{***********************************************************************}
function TALISAPIRequest.WriteString(const AString: AnsiString): Boolean;
begin
  Result := WriteClient(Pointer(AString)^, Length(AString)) = Length(AString);
end;

{********************************************************}
function TALISAPIRequest.WriteHeaders(StatusCode: Integer;
                                      const StatusString, Headers: AnsiString): Boolean;
begin
  TALISAPIRequest(Self).ECB.dwHttpStatusCode := StatusCode;
  with TALISAPIRequest(Self) do
    if not ECB.ServerSupportFunction(ECB.ConnID,
                                     HSE_REQ_SEND_RESPONSE_HEADER,
                                     PAnsiChar(StatusString),
                                     nil,
                                     LPDWORD(Headers)) then raiseLastOsError;
  Result := True;
end;

{************************************************************}
constructor TALWebResponse.Create(HTTPRequest: TALWebRequest);
begin
  inherited Create;
  FHTTPRequest := HTTPRequest;
  FCustomHeaders := TALStringList.Create;
  FCookies := TALHttpCookieCollection.Create(TALHttpCookie);
end;

{********************************}
destructor TALWebResponse.Destroy;
begin
  AlFreeAndNil(FContentStream);
  AlFreeAndNil(FCustomHeaders);
  AlFreeAndNil(FCookies);
  inherited Destroy;
end;

{*****************************************************************}
procedure TALWebResponse.AddCustomHeaders(var Headers: AnsiString);
var I: Integer;
    Name, Value: AnsiString;
begin
  for I := 0 to FCustomHeaders.Count - 1 do begin
    Name := FCustomHeaders.Names[I];
    Value := FCustomHeaders.values[Name];
    Headers := Headers + Name + ': ' + Value + #13#10;
  end;
end;

{**************************************************************************}
function TALWebResponse.GetCustomHeader(const Name: AnsiString): AnsiString;
begin
  Result := FCustomHeaders.Values[Name];
end;

{************************************}
function TALWebResponse.Sent: Boolean;
begin
  Result := False;
end;

{********************************************************}
procedure TALWebResponse.SetContentStream(Value: TStream);
begin
  if Value <> FContentStream then begin
    AlFreeAndNil(FContentStream);
    FContentStream := Value;
  end;
end;

{*********************************************************}
procedure TALWebResponse.SetCookieField(Values: TALStrings;
                                        const ADomain, APath: AnsiString;
                                        AExpires: TDateTime;
                                        ASecure: Boolean;
                                        const AHttpOnly: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to Values.Count - 1 do
    with Cookies.Add do begin
      Name := Values.Names[I];
      Value := Values.Values[Values.Names[I]];
      Domain := ADomain;
      Path := APath;
      Expires := AExpires;
      Secure := ASecure;
      HttpOnly := AHttpOnly;
    end;
end;

{**********************************************************************}
procedure TALWebResponse.SetCustomHeader(const Name, Value: AnsiString);
begin
  FCustomHeaders.Values[Name] := Value;
end;

{***********************************************************}
procedure TALWebResponse.SetCustomHeaders(Value: TALStrings);
begin
  FCustomHeaders.Assign(Value);
end;

{*****************************************************}
function TALWebResponse.FormatAuthenticate: AnsiString;
begin
  if Realm <> '' then Result := ALFormat('%s Realm=%s', [WWWAuthenticate, Realm])
  else Result := WWWAuthenticate;
end;

{****************************************************************}
function ALIsapiHttpStatusString(StatusCode: Integer): AnsiString;
begin
  case StatusCode of
    100: Result := 'Continue';                                 {do not localize}
    101: Result := 'Switching Protocols';                      {do not localize}
    200: Result := 'OK';                                       {do not localize}
    201: Result := 'Created';                                  {do not localize}
    202: Result := 'Accepted';                                 {do not localize}
    203: Result := 'Non-Authoritative Information';            {do not localize}
    204: Result := 'No Content';                               {do not localize}
    205: Result := 'Reset Content';                            {do not localize}
    206: Result := 'Partial Content';                          {do not localize}
    300: Result := 'Multiple Choices';                         {do not localize}
    301: Result := 'Moved Permanently';                        {do not localize}
    302: Result := 'Moved Temporarily';                        {do not localize}
    303: Result := 'See Other';                                {do not localize}
    304: Result := 'Not Modified';                             {do not localize}
    305: Result := 'Use Proxy';                                {do not localize}
    400: Result := 'Bad Request';                              {do not localize}
    401: Result := 'Unauthorized';                             {do not localize}
    402: Result := 'Payment Required';                         {do not localize}
    403: Result := 'Forbidden';                                {do not localize}
    404: Result := 'Not Found';                                {do not localize}
    405: Result := 'Method Not Allowed';                       {do not localize}
    406: Result := 'None Acceptable';                          {do not localize}
    407: Result := 'Proxy Authentication Required';            {do not localize}
    408: Result := 'Request Timeout';                          {do not localize}
    409: Result := 'Conflict';                                 {do not localize}
    410: Result := 'Gone';                                     {do not localize}
    411: Result := 'Length Required';                          {do not localize}
    412: Result := 'Unless True';                              {do not localize}
    500: Result := 'Internal Server Error';                    {do not localize}
    501: Result := 'Not Implemented';                          {do not localize}
    502: Result := 'Bad Gateway';                              {do not localize}
    503: Result := 'Service Unavailable';                      {do not localize}
    504: Result := 'Gateway Timeout';                          {do not localize}
  else
    Result := '';
  end
end;

{**************************************************************}
constructor TALISAPIResponse.Create(HTTPRequest: TALWebRequest);
begin
  inherited Create(HTTPRequest);
  FTransmitFileInfo.hFile := INVALID_HANDLE_VALUE;
  fSentInAsync := False;
  InitResponse;
end;

{**************************************}
procedure TALISAPIResponse.InitResponse;
begin
  if FHTTPRequest.ProtocolVersion = '' then ProtocolVersion := '1.0';
  StatusCode := 200;
  LastModified := -1;
  Expires := -1;
  Date := -1;
  ContentType := 'text/html';  { do not localize }
end;

{***********************************************}
function TALISAPIResponse.GetContent: AnsiString;
begin
  Result := FContent;
end;

{*******************************************************************}
function TALISAPIResponse.GetDateVariable(Index: Integer): TDateTime;
begin
  if (Index >= Low(FDateVariables)) and (Index <= High(FDateVariables)) then Result := FDateVariables[Index]
  else Result := 0.0;
end;

{********************************************************************}
function TALISAPIResponse.GetIntegerVariable(Index: Integer): Integer;
begin
  if (Index >= Low(FIntegerVariables)) and (Index <= High(FIntegerVariables)) then Result := FIntegerVariables[Index]
  else Result := -1;
end;

{**************************************************}
function TALISAPIResponse.GetLogMessage: AnsiString;
begin
  Result := TALISAPIRequest(HTTPRequest).ECB.lpszLogData;
end;

{***********************************************}
function TALISAPIResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

{**********************************************************************}
function TALISAPIResponse.GetStringVariable(Index: Integer): AnsiString;
begin
  if (Index >= Low(FStringVariables)) and (Index <= High(FStringVariables)) then
    Result := FStringVariables[Index];
end;

{**************************************}
function TALISAPIResponse.Sent: Boolean;
begin
  Result := FSent;
end;

{*************************************************************}
procedure TALISAPIResponse.SetContent(const Value: AnsiString);
begin
  FContent := Value;
end;

{*********************************************************************************}
procedure TALISAPIResponse.SetDateVariable(Index: Integer; const Value: TDateTime);
begin
  if (Index >= Low(FDateVariables)) and (Index <= High(FDateVariables)) then
    if Value <> FDateVariables[Index] then
      FDateVariables[Index] := Value;
end;

{****************************************************************************}
procedure TALISAPIResponse.SetIntegerVariable(Index: Integer; Value: Integer);
begin
  if (Index >= Low(FIntegerVariables)) and (Index <= High(FIntegerVariables)) then
    if Value <> FIntegerVariables[Index] then
      FIntegerVariables[Index] := Value;
end;

{****************************************************************}
procedure TALISAPIResponse.SetLogMessage(const Value: AnsiString);
var LLen: Integer;
begin
  LLen := Length(TALISAPIRequest(HTTPRequest).ECB.lpszLogData);
  LLen := Min(LLen, Length(Value) + 1);    // + 1 to include null terminator
  Move(Value[Low(Value)], TALISAPIRequest(HTTPRequest).ECB.lpszLogData[0], LLen);
  TALISAPIRequest(HTTPRequest).ECB.lpszLogData[LLen-1] := Char(0);
end;

{*******************************************************}
procedure TALISAPIResponse.SetStatusCode(Value: Integer);
begin
  if FStatusCode <> Value then begin
    FStatusCode := Value;
    ReasonString := ALIsapiHttpStatusString(Value);
  end;
end;

{************************************************************************************}
procedure TALISAPIResponse.SetStringVariable(Index: Integer; const Value: AnsiString);
begin
  if (Index >= Low(FStringVariables)) and (Index <= High(FStringVariables)) then
    FStringVariables[Index] := Value;
end;

{**************************************}
procedure TALISAPIResponse.SendResponse;

var StatusString: AnsiString;
    Headers: AnsiString;
    I: Integer;

  {---------------------------------------------------------------------------}
  procedure AddHeaderItem(const Item: AnsiString; const FormatStr: AnsiString);
  begin
    if Item <> '' then Headers := Headers + ALFormat(FormatStr, [Item]);
  end;

begin

  if fSent or TALISAPIRequest(FHTTPRequest).ConnectionClosed then exit;

  if HTTPRequest.ProtocolVersion <> '' then begin
    if (ReasonString <> '') and (StatusCode > 0) then StatusString := ALFormat('%d %s', [StatusCode, ReasonString])
    else StatusString := '200 OK';
    AddHeaderItem(Location, 'Location: %s'#13#10);
    AddHeaderItem(Allow, 'Allow: %s'#13#10);
    for I := 0 to Cookies.Count - 1 do
      AddHeaderItem(Cookies[I].HeaderValue, 'Set-Cookie: %s'#13#10);
    AddHeaderItem(DerivedFrom, 'Derived-From: %s'#13#10);
    if Expires > 0 then
      AddHeaderItem(ALFormat(ALFormatDateTime('"%s", dd "%s" yyyy hh":"nn":"ss "GMT"',
                                              Expires,
                                              ALDefaultFormatSettings),
                             [AlRfc822DayOfWeekNames[DayOfWeek(Expires)],
                              ALRfc822MonthOfTheYearNames[MonthOf(Expires)]]),
                    'Expires: %s'#13#10);
    if LastModified > 0 then
      AddHeaderItem(ALFormat(ALFormatDateTime('"%s", dd "%s" yyyy hh":"nn":"ss "GMT"',
                                              LastModified,
                                              ALDefaultFormatSettings),
                             [AlRfc822DayOfWeekNames[DayOfWeek(LastModified)],
                              ALRfc822MonthOfTheYearNames[MonthOf(LastModified)]]),
                    'Last-Modified: %s'#13#10);
    AddHeaderItem(Title, 'Title: %s'#13#10);
    AddHeaderItem(FormatAuthenticate, 'WWW-Authenticate: %s'#13#10);
    AddCustomHeaders(Headers);
    AddHeaderItem(ContentVersion, 'Content-Version: %s'#13#10);
    AddHeaderItem(ContentEncoding, 'Content-Encoding: %s'#13#10);
    AddHeaderItem(ContentType, 'Content-Type: %s'#13#10);
    Headers := Headers + #13#10;
    HTTPRequest.WriteHeaders(StatusCode, StatusString, Headers);
  end;

  fSentInAsync := False;
  if fTransmitFileInfo.hFile <> Invalid_handle_value then begin
    with TALISAPIRequest(FHTTPRequest) do
      if not ECB.ServerSupportFunction(ECB.ConnID,
                                       HSE_REQ_TRANSMIT_FILE,
                                       @fTransmitFileInfo,
                                       nil,
                                       nil) then raiseLastOsError;
    fSentInAsync := True;
  end
  else if ContentStream = nil then HTTPRequest.WriteString(Content)
  else if ContentStream <> nil then begin
    SendStream(ContentStream);
    ContentStream := nil; // Drop the stream
  end;

  FSent := True;

end;

{*************************************************************}
procedure TALISAPIResponse.SendRedirect(const URI: AnsiString);
begin
  with TALISAPIRequest(FHTTPRequest) do
    if not ECB.ServerSupportFunction(ECB.ConnID,
                                     HSE_REQ_SEND_URL_REDIRECT_RESP,
                                     PAnsiChar(URI),
                                     nil,
                                     nil) then raiseLastOsError;
  FSent := True;
end;

{******************************************************}
procedure TALISAPIResponse.SendStream(AStream: TStream);
var Buffer: array[0..8191] of Byte;
    BytesToSend: Integer;
begin
  while AStream.Position < AStream.Size do begin
    BytesToSend := AStream.Read(Buffer, SizeOf(Buffer));
    FHTTPRequest.WriteClient(Buffer, BytesToSend);
  end;
end;

{**********************************************************}
function TALISAPIResponse.getTransmitFileInfo: PHSE_TF_INFO;
begin
  result := @FTransmitFileInfo;
end;

end.
