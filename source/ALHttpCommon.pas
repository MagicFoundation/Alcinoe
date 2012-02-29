{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Common Http Functions
Version:      3.53

Description:  Common http functions that can be use by HTTP Components

Legal issues: Copyright (C) 1999-2010 by Arkadia Software Engineering

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

History :     04/10/2005: * Move ALTryGMTHeaderHttpStrToDateTime to
                            ALTryGMTRFC822StrToGmtDateTime in AlFcnRFC
                          * Move ALGMTHeaderHttpStrToDateTime to
                            ALGMTRFC822StrToGmtDateTime in AlFcnRFC
                          * Move ALGetDefaultFileExtFromHTTPMIMEContentType to
                            ALGetDefaultFileExtFromMIMEContentType in AlFcnMime
                          * Move ALGetDefaultHTTPMIMEContentTypeFromExt to
                            ALGetDefaultMIMEContentTypeFromExt in alFcnMime
              04/10/2005: * add some new procedures
                          * Update TALHTTPRequestCookie and TALHTTPRequestCookieCollection
                            and TALHTTPRequestHeader in the way they can be use in
                            the object inspector
              11/02/2006: * correct TALHTTPRequestCookie and TALHTTPRequestCookieCollection
                            and TALHTTPRequestHeader to TALHTTPResponseCookie and
                            TALHTTPResponseCookieCollection and TALHTTPResponseHeader
              29/01/2008: * Update the handle of status line in TALHTTPResponseHeader
              03/01/2008: * Better handle of the status line in TALHTTPResponseHeader
              22/02/2008: * Update AlHttpEncode

Link :        http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
              http://wp.netscape.com/newsref/std/cookie_spec.html

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  voting on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALHttpCommon;

interface

uses Windows,
     Classes,
     sysutils,
     Wininet;

Type

  {------------------------------------------------}
  EALHttpClientConnectionDropped = class(Exception);

  {-- onchange Event that specify the property index that is just changed --}
  TALHTTPPropertyChangeEvent = procedure(sender: Tobject; Const PropertyIndex: Integer) of object;

  {--protocol version--}
  TALHTTPProtocolVersion = (
                            HTTPpv_1_0,
                            HTTPpv_1_1
                           );

  {--Request method--}
  TALHTTPRequestMethod = (
                          HTTPrm_Get,
                          HTTPrm_Post,
                          HTTPrm_Head,
                          HTTPrm_Trace
                         );

  {--Request header--}
  TALHTTPRequestHeader = Class(Tcomponent)
  Private
    fAccept: String;
    fAcceptCharSet: String;
    fAcceptEncoding: String;
    fAcceptLanguage: String;
    fAllow: String;
    fAuthorization: String;
    fCacheControl: String;
    fConnection: string;
    fContentEncoding: string;
    fContentLanguage: string;
    fContentLength: string;
    fContentLocation: string;
    fContentMD5: string;
    fContentRange: string;
    fContentType: string;
    fDate: string;
    fExpect: string;
    fExpires: string;
    fFrom: String;
    fHost: String;
    fIfMatch: string;
    fIfModifiedSince: string;
    fIfNoneMatch: string;
    fIfRange: string;
    fIfUnmodifiedSince: string;
    fLastModified: string;
    fMaxForwards: string;
    fPragma: string;
    fProxyAuthorization: string;
    fRange: string;
    fReferer: String;
    fTE: string;
    fTrailer: String;
    fTransferEncoding: String;
    fUpgrade: String;
    fUserAgent: String;
    fVia: String;
    fWarning: String;
    FCustomHeaders: Tstrings;
    FCookies: Tstrings;
    FOnChange: TALHTTPPropertyChangeEvent;
    Procedure DoChange(propertyIndex: Integer);
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: string);
    Function GetRawHeaderText: String;
    procedure SetCookies(const Value: TStrings);
    procedure SetRawHeaderText(const aRawHeaderText: string);
    procedure SetCustomHeaders(const Value: Tstrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    Property RawHeaderText: String read GetRawHeaderText write SetRawHeaderText;
  Published
    property Accept: String index 0 read FAccept write SetHeaderValueByPropertyIndex; {Accept: audio/*; q=0.2, audio/basic}
    property AcceptCharSet: String index 1 read FAcceptCharSet write SetHeaderValueByPropertyIndex; {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
    property AcceptEncoding: String index 2 read FAcceptEncoding write SetHeaderValueByPropertyIndex; {Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0}
    property AcceptLanguage: String index 3 read FAcceptLanguage write SetHeaderValueByPropertyIndex; {Accept-Language: da, en-gb;q=0.8, en;q=0.7}
    property Allow: String index 4 read FAllow write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property Authorization: String index 5 read FAuthorization write SetHeaderValueByPropertyIndex; {Authorization: BASIC d2VibWFzdGVyOnpycW1hNHY=}
    property CacheControl: String index 6 read FCacheControl write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: string index 7 read FConnection write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: string index 8 read FContentEncoding write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: string index 9 read FContentLanguage write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: string index 10 read FContentLength write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: string index 11 read FContentLocation write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: string index 12 read FContentMD5 write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: string index 13 read FContentRange write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: string index 14 read FContentType write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property Date: string index 15 read FDate write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property Expect: string index 16 read fExpect write SetHeaderValueByPropertyIndex; {Expect: 100-continue}
    property Expires: string index 17 read FExpires write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property From: String index 18 read FFrom write SetHeaderValueByPropertyIndex; {From: webmaster@w3.org}
    property Host: String index 19 read FHost write SetHeaderValueByPropertyIndex; {Host: www.w3.org}
    property IfMatch: string index 20 read FIfMatch write SetHeaderValueByPropertyIndex; {If-Match: entity_tag001}
    property IfModifiedSince: string index 21 read FIfModifiedSince write SetHeaderValueByPropertyIndex; {If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property IfNoneMatch: string index 22 read fIfNoneMatch write SetHeaderValueByPropertyIndex; {If-None-Match: entity_tag001}
    property IfRange: string index 23 read fIfRange write SetHeaderValueByPropertyIndex; {If-Range: entity_tag001}
    property IfUnmodifiedSince: string index 24 read fIfUnmodifiedSince write SetHeaderValueByPropertyIndex; {If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property LastModified: string index 25 read fLastModified write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property MaxForwards: string index 26 read fMaxForwards write SetHeaderValueByPropertyIndex; {Max-Forwards: 3}
    property Pragma: string index 27 read FPragma write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthorization: string index 28 read FProxyAuthorization write SetHeaderValueByPropertyIndex; {Proxy-Authorization: [credentials]}
    property Range: string index 29 read FRange write SetHeaderValueByPropertyIndex; {Range: bytes=100-599}
    property Referer: String index 30 read FReferer write SetHeaderValueByPropertyIndex; {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    property TE: string index 31 read fTE write SetHeaderValueByPropertyIndex; {TE: trailers, deflate;q=0.5}
    property Trailer: String index 32 read FTrailer write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property TransferEncoding: String index 33 read FTransferEncoding write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: String index 34 read FUpgrade write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property UserAgent: String index 35 read FUserAgent write SetHeaderValueByPropertyIndex; {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    property Via: String index 36 read FVia write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: String index 37 read FWarning write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property CustomHeaders: Tstrings read FCustomHeaders write SetCustomHeaders;
    property Cookies: TStrings read FCookies write SetCookies;
    property OnChange: TALHTTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {--TALHTTPCookie--}
  TALHTTPResponseCookie = class(TCollectionItem)
  private
    FName: string;
    FValue: string;
    FPath: string;
    FDomain: string;
    FExpires: TDateTime;
    FSecure: Boolean;
  protected
    function GetHeaderValue: string;
    procedure SetHeaderValue(Const aValue: string);
  public
    constructor Create(Collection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    property HeaderValue: string read GetHeaderValue write SetHeaderValue;
  Published
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property Domain: string read FDomain write FDomain;
    property Path: string read FPath write FPath;
    property Expires: TDateTime read FExpires write FExpires;
    property Secure: Boolean read FSecure write FSecure Default False;
  end;

  {--TALCookieCollection--}
  TALHTTPResponseCookieCollection = class(TOwnedCollection)
  private
  protected
    function GetCookie(Index: Integer): TALHTTPResponseCookie;
    procedure SetCookie(Index: Integer; Cookie: TALHTTPResponseCookie);
  public
    function Add: TALHTTPResponseCookie;
    property Items[Index: Integer]: TALHTTPResponseCookie read GetCookie write SetCookie; default;
  end;

  {--Response header--}
  TALHTTPResponseHeader = Class(TPersistent)
  Private
    FAcceptRanges: String;
    FAge: String;
    FAllow: String;
    FCacheControl: String;
    FConnection: string;
    FContentEncoding: string;
    FContentLanguage: string;
    FContentLength: string;
    FContentLocation: string;
    FContentMD5: string;
    FContentRange: string;
    FContentType: string;
    FDate: string;
    FETag: String;
    FExpires: string;
    FLastModified: string;
    FLocation: String;
    FPragma: string;
    FProxyAuthenticate: String;
    FRetryAfter: String;
    FServer: String;
    FTrailer: String;
    FTransferEncoding: String;
    FUpgrade: String;
    FVary: String;
    FVia: String;
    FWarning: String;
    FWWWAuthenticate: String;
    FRawHeaderText: string;
    FCustomHeaders: Tstrings;
    FCookies: TALHTTPResponseCookieCollection;
    FStatusCode: String;
    FHttpProtocolVersion: String;
    FReasonPhrase: String;
    procedure SetRawHeaderText(const aRawHeaderText: string);
    Function GetRawHeaderText: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property AcceptRanges: String read FAcceptRanges; {Accept-Ranges: bytes}
    property Age: String read FAge; {Age: 2147483648(2^31)}
    property Allow: String read FAllow; {Allow: GET, HEAD, PUT}
    property CacheControl: String read FCacheControl; {Cache-Control: no-cache}
    property Connection: string read FConnection; {Connection: close}
    property ContentEncoding: string read FContentEncoding; {Content-Encoding: gzip}
    property ContentLanguage: string read FContentLanguage; {Content-Language: mi, en}
    property ContentLength: string read FContentLength;  {Content-Length: 3495}
    property ContentLocation: string read FContentLocation;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: string read FContentMD5;  {Content-MD5: [md5-digest]}
    property ContentRange: string read FContentRange;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: string read FContentType; {Content-Type: text/html; charset=ISO-8859-4}
    property Date: string read FDate; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property ETag: String read FETag; {ETag: W/"xyzzy"}
    property Expires: string read FExpires; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property LastModified: string read FLastModified; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property Location: String read FLocation; {Location: http://www.w3.org/pub/WWW/People.html}
    property Pragma: string read FPragma; {Pragma: no-cache}
    property ProxyAuthenticate: String read FProxyAuthenticate; {Proxy-Authenticate: [challenge]}
    property RetryAfter: String read FRetryAfter; {Retry-After: Fri, 31 Dec 1999 23:59:59 GMT}
    property Server: String read FServer; {Server: CERN/3.0 libwww/2.17}
    property Trailer: String read FTrailer; {Trailer: Date}
    property TransferEncoding: String read FTransferEncoding; {Transfer-Encoding: chunked}
    property Upgrade: String read FUpgrade; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property Vary: String read FVary; {Vary: Date}
    property Via: String read FVia; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: String read FWarning; {Warning: 112 Disconnected Operation}
    property WWWAuthenticate: String read FWWWAuthenticate; {WWW-Authenticate: [challenge]}
    Property CustomHeaders: Tstrings read FCustomHeaders;
    Property Cookies: TALHTTPResponseCookieCollection read FCookies;
    property StatusCode: String read FStatusCode;
    property HttpProtocolVersion: String read FHttpProtocolVersion;
    Property ReasonPhrase: String read FReasonPhrase;
    property RawHeaderText: String read GetRawHeaderText write setRawHeaderText;
  end;

{Http Function}
function  ALHTTPDecode(const AStr: String): String;
procedure ALHTTPEncodeParamNameValues(ParamValues: TStrings);
procedure ALExtractHTTPFields(Separators,
                              WhiteSpace,
                              Quotes: TSysCharSet;
                              Content: PChar;
                              Strings: TStrings;
                              StripQuotes: Boolean = False);
Function  AlExtractShemeFromUrl(aUrl: String): TInternetScheme;
Function  AlExtractHostNameFromUrl(aUrl: String): String;
Function  AlExtractDomainNameFromUrl(aUrl: String): String;
Function  AlExtractUrlPathFromUrl(aUrl: String): String;
{*************
flags can be :
  ICU_DECODE Converts encoded characters back to their normal form.
  ICU_ESCAPE Converts all escape sequences (%xx) to their corresponding characters}
Function AlInternetCrackUrl(aUrl: String;
                            Var SchemeName,
                                HostName,
                                UserName,
                                Password,
                                UrlPath,
                                ExtraInfo: String;
                            var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(aUrl: String;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 Anchor: String; // not the anchor is never send to the server ! it's only used on client side
                             Query: TStrings;
                             var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(var Url: String; // if true return UrlPath
                             var Anchor: String;
                             Query: TStrings): Boolean; overload;
Function  AlRemoveAnchorFromUrl(aUrl: String; Var aAnchor: String): String; overload;
Function  AlRemoveAnchorFromUrl(aUrl: String): String; overload;
function  AlCombineUrl(RelativeUrl, BaseUrl: String): String; overload;
Function  AlCombineUrl(RelativeUrl,
                       BaseUrl,
                       Anchor: String;
                       Query: TStrings): String; overload;
Function ALIPV4StrToNumeric(aIPv4: String): Cardinal;
Function ALTryIPV4StrToNumeric(aIPv4Str: String; var aIPv4Num: Cardinal): Boolean;
Function ALNumericToIPv4Str(aIPv4: Cardinal): String;


ResourceString
  CALHTTPCLient_MsgInvalidURL         = 'Invalid url ''%s'' - only supports ''http'' and ''https'' schemes';
  CALHTTPCLient_MsgInvalidHTTPRequest = 'Invalid HTTP Request: Length is 0';
  CALHTTPCLient_MsgEmptyURL           = 'Empty URL';

implementation

uses HTTPapp,
     alFcnRFC,
     AlFcnMisc,
     AlFcnString;

{***********************************************************************}
function AlStringFetch(var AInput: string; const ADelim: string): string;
var
  LPos: Integer;
begin
  LPos := AlPos(ADelim, AInput);
  if LPos = 0 then begin
    Result := AInput;
    AInput := '';
  end
  else begin
    Result := AlCopyStr(AInput, 1, LPos - 1);
    AInput := AlCopyStr(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;


/////////////////////////////////////////////////////////////////////////////
////////// TALHTTPResponseCookie ////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

{****************************************************************}
constructor TALHTTPResponseCookie.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FExpires := -1;
  FSecure := False;
end;

{**********************************************************}
procedure TALHTTPResponseCookie.AssignTo(Dest: TPersistent);
begin
  if Dest is TALHTTPResponseCookie then
    with TALHTTPResponseCookie(Dest) do begin
      Name := Self.FName;
      Value := Self.FValue;
      Domain := Self.FDomain;
      Path := Self.FPath;
      Expires := Self.FExpires;
      Secure := Self.FSecure;
    end
    else inherited AssignTo(Dest);
end;

{****************************************************}
function TALHTTPResponseCookie.GetHeaderValue: string;
var aYear, aMonth, aDay: Word;
begin
  Result := Format('%s=%s; ', [HTTPEncode(FName), HTTPEncode(FValue)]);
  if Domain <> '' then Result := Result + Format('domain=%s; ', [Domain]);
  if Path <> '' then Result := Result + Format('path=%s; ', [Path]);
  if Expires > -1 then begin
    DecodeDate(Expires, aYear, aMonth, aDay);
    Result := Result + Format(
                              FormatDateTime(
                                             '"expires=%s, "dd"-%s-"yyyy" "hh":"nn":"ss" GMT; "',
                                             Expires
                                            ),
                              [
                               CAlRfc822DaysOfWeek[DayOfWeek(Expires)],
                               CAlRfc822MonthNames[aMonth]
                              ]
                             );
  end;
  if Secure then Result := Result + 'secure';
  if Copy(Result, Length(Result) - 1, MaxInt) = '; ' then SetLength(Result, Length(Result) - 2);
end;

{*******************************************************************}
procedure TALHTTPResponseCookie.SetHeaderValue(Const aValue: string);
Var aCookieProp: TStringList;
    aCookieStr: String;
begin
  FName:= '';
  FValue:= '';
  FPath:= '';
  FDomain:= '';
  FExpires:= -1;
  FSecure:= False;

  aCookieProp := TStringList.Create;
  try
    aCookieStr := AValue;

    while Pos(';', aCookieStr) > 0 do begin
      aCookieProp.Add(Trim(AlStringFetch(aCookieStr, ';')));
      if (Pos(';', aCookieStr) = 0) and (Length(aCookieStr) > 0) then aCookieProp.Add(Trim(aCookieStr));
    end;

    if aCookieProp.Count = 0 then aCookieProp.Text := aCookieStr;
    if aCookieProp.Count = 0 then exit;

    FName := aCookieProp.Names[0];
    FValue := aCookieProp.Values[aCookieProp.Names[0]];
    aCookieProp.Delete(0);

    FPath := aCookieProp.values['PATH'];
    { Tomcat can return SetCookie2 with path wrapped in " }
    if (Length(FPath) > 0) then begin
      if FPath[1] = '"' then Delete(FPath, 1, 1);
      if FPath[Length(FPath)] = '"' then SetLength(FPath, Length(FPath) - 1);
    end
    else FPath := '/';
    if not ALTryRfc822StrToGmtDateTime(aCookieProp.values['EXPIRES'], FExpires) then FExpires := -1;
    FDomain := aCookieProp.values['DOMAIN'];
    FSecure := aCookieProp.IndexOf('SECURE') <> -1;
  finally
    aCookieProp.free;
  end;
end;



/////////////////////////////////////////////////////////////////////////////
////////// TCookieCollection ////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

{******************************************************************}
function TALHTTPResponseCookieCollection.Add: TALHTTPResponseCookie;
begin
  Result := TALHTTPResponseCookie(inherited Add);
end;

{****************************************************************************************}
function TALHTTPResponseCookieCollection.GetCookie(Index: Integer): TALHTTPResponseCookie;
begin
  Result := TALHTTPResponseCookie(inherited Items[Index]);
end;

{*************************************************************************************************}
procedure TALHTTPResponseCookieCollection.SetCookie(Index: Integer; Cookie: TALHTTPResponseCookie);
begin
  Items[Index].Assign(Cookie);
end;




///////////////////////////////////////////////////////////////////////////////////////
////////// TALHTTPClientResponseHeader ////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

{***************************************}
constructor TALHTTPResponseHeader.Create;
begin
  inherited;
  FCustomHeaders := TstringList.create;
  FCustomHeaders.NameValueSeparator := ':';
  FCookies := TALHTTPResponseCookieCollection.Create(Self, TALHTTPResponseCookie);
  clear;
end;

{***************************************}
destructor TALHTTPResponseHeader.Destroy;
begin
  FCustomHeaders.free;
  FCookies.free;
  inherited;
end;

{************************************}
procedure TALHTTPResponseHeader.Clear;
begin
  FAcceptRanges:= '';
  FAge:= '';
  FAllow:= '';
  FCacheControl:= '';
  FConnection:= '';
  FContentEncoding:= '';
  FContentLanguage:= '';
  FContentLength:= '';
  FContentLocation:= '';
  FContentMD5:= '';
  FContentRange:= '';
  FContentType:= '';
  FDate:= '';
  FETag:= '';
  FExpires:= '';
  FLastModified:= '';
  FLocation:= '';
  FPragma:= '';
  FProxyAuthenticate:= '';
  FRetryAfter:= '';
  FServer:= '';
  FTrailer:= '';
  FTransferEncoding:= '';
  FUpgrade:= '';
  FVary:= '';
  FVia:= '';
  FWarning:= '';
  FWWWAuthenticate:= '';
  FRawHeaderText:= '';
  FCustomHeaders.clear;
  FCookies.Clear;
  FStatusCode:= '';
  FHttpProtocolVersion:= '';
  FReasonPhrase := '';
end;

{******************************************************}
function TALHTTPResponseHeader.GetRawHeaderText: String;
begin
  result := FRawHeaderText;
end;

{*****************************************************************************}
procedure TALHTTPResponseHeader.SetRawHeaderText(Const aRawHeaderText: string);
Var aRawHeaderLst: TstringList;
    j: integer;
    AStatusLine: String;

  {-------------------------------------}
  Function AlG001(aName: String): String;
  Var i: Integer;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
    end
    else result := '';
  end;

begin
  aRawHeaderLst := TstringList.create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    FAcceptRanges := Alg001('Accept-Ranges');
    FAge:= Alg001('Age');
    FAllow := Alg001('Allow');
    FCacheControl := Alg001('Cache-Control');
    FConnection := Alg001('Connection');
    FContentEncoding := Alg001('Content-Encoding');
    FContentLanguage := Alg001('Content-Language');
    FContentLength := Alg001('Content-Length');
    FContentLocation := Alg001('Content-Location');
    FContentMD5 := Alg001('Content-MD5');
    FContentRange := Alg001('Content-Range');
    FContentType := Alg001('Content-Type');
    FDate := Alg001('Date');
    FETag := Alg001('ETag');
    FExpires := Alg001('Expires');
    FLastModified := Alg001('Last-Modified');
    FLocation := Alg001('Location');
    FPragma := Alg001('Pragma');
    FProxyAuthenticate := Alg001('Proxy-Authenticate');
    FRetryAfter := Alg001('Retry-After');
    FServer := Alg001('Server');
    FTrailer := Alg001('Trailer');
    FTransferEncoding := Alg001('Transfer-Encoding');
    FUpgrade := Alg001('Upgrade');
    FVary := Alg001('Vary');
    FVia := Alg001('Via');
    FWarning := Alg001('Warning');
    FWWWAuthenticate := Alg001('WWW-Authenticate');

    FCookies.clear;
    J := aRawHeaderLst.IndexOfName('Set-Cookie');
    While J >= 0 do begin
      If trim(aRawHeaderLst.ValueFromIndex[j]) <> '' then
        Cookies.Add.HeaderValue := Trim(aRawHeaderLst.ValueFromIndex[j]);
      aRawHeaderLst.Delete(j);
      J := aRawHeaderLst.IndexOfName('Set-Cookie');
    end;

    If aRawHeaderLst.Count > 0 then begin
      AStatusLine := aRawHeaderLst[0]; //HTTP/1.1 200 OK | 200 OK | status: 200 OK
      FHttpProtocolVersion := trim(AlStringFetch(AstatusLine,' '));
      If AlIsInteger(FHttpProtocolVersion) then begin
        FStatusCode := FHttpProtocolVersion;
        FHttpProtocolVersion := '';
      end
      else FStatusCode := trim(AlStringFetch(AstatusLine,' '));
      FReasonPhrase := trim(AstatusLine);

      If not AlIsInteger(FStatusCode) then begin
        FHttpProtocolVersion := '';
        AStatusLine := Alg001('Status');
        FStatusCode := trim(AlStringFetch(AStatusLine,' '));
        FReasonPhrase := trim(AlStringFetch(AStatusLine,' '));
      end
      else aRawHeaderLst.Delete(0);
    end
    else begin
      FStatusCode := '';
      FHttpProtocolVersion := '';
      FReasonPhrase := '';
    end;

    FCustomHeaders.clear;
    For j := 0 to aRawHeaderLst.count - 1 do
      If trim(aRawHeaderLst[j]) <> '' then
        FCustomHeaders.Add(aRawHeaderLst[j]);

    FRawHeaderText := aRawHeaderText;
  finally
    aRawHeaderLst.Free;
  end;
end;

{**********************************************************}
procedure TALHTTPResponseHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TALHTTPResponseHeader then begin
    with Dest as TALHTTPResponseHeader do begin
      FAcceptRanges := Self.FAcceptRanges;
      FAge := Self.FAge;
      FAllow := Self.FAllow;
      FCacheControl := Self.FCacheControl;
      FConnection := Self.FConnection;
      FContentEncoding := Self.FContentEncoding;
      FContentLanguage := Self.FContentLanguage;
      FContentLength := Self.FContentLength;
      FContentLocation := Self.FContentLocation;
      FContentMD5 := Self.FContentMD5;
      FContentRange := Self.FContentRange;
      FContentType := Self.FContentType;
      FDate := Self.FDate;
      FETag := Self.FETag;
      FExpires := Self.FExpires;
      FLastModified := Self.FLastModified;
      FLocation := Self.FLocation;
      FPragma := Self.FPragma;
      FProxyAuthenticate := Self.FProxyAuthenticate;
      FRetryAfter := Self.FRetryAfter;
      FServer := Self.FServer;
      FTrailer := Self.FTrailer;
      FTransferEncoding := Self.FTransferEncoding;
      FUpgrade := Self.FUpgrade;
      FVary := Self.FVary;
      FVia := Self.FVia;
      FWarning := Self.FWarning;
      FWWWAuthenticate := Self.FWWWAuthenticate;
      FRawHeaderText := Self.FRawHeaderText;
      FStatusCode := Self.FStatusCode;
      FHttpProtocolVersion := Self.FHttpProtocolVersion;
      FReasonPhrase := Self.FReasonPhrase;
      FCustomHeaders.Assign(Self.FCustomHeaders);
      FCookies.Assign(Self.FCookies);
    end;
  end
  else inherited AssignTo(Dest);
end;




///////////////////////////////////////////////////////////////////////////////////////
////////// TALHTTPClientRequestHeader /////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

{**********************************************************}
constructor TALHTTPRequestHeader.Create(AOwner: TComponent);
Begin
  inherited create(AOwner);
  fCustomHeaders:= TstringList.create;
  fCustomHeaders.NameValueSeparator := ':';
  FCookies := TstringList.create;
  FOnchange := nil;
  clear;
  fAccept := 'text/html, */*';
end;

{**************************************}
destructor TALHTTPRequestHeader.Destroy;
begin
  fCustomHeaders.free;
  Fcookies.Free;
  inherited;
end;

{***********************************}
procedure TALHTTPRequestHeader.Clear;
begin
  fAccept := '';
  fAcceptCharSet := '';
  fAcceptEncoding := '';
  fAcceptLanguage := '';
  fAllow := '';
  fAuthorization := '';
  fCacheControl := '';
  fConnection := '';
  fContentEncoding := '';
  fContentLanguage := '';
  fContentLength := '';
  fContentLocation := '';
  fContentMD5 := '';
  fContentRange := '';
  fContentType := '';
  fDate := '';
  fExpect := '';
  fExpires := '';
  fFrom := '';
  fHost := '';
  fIfMatch := '';
  fIfModifiedSince := '';
  fIfNoneMatch := '';
  fIfRange := '';
  fIfUnmodifiedSince := '';
  fLastModified := '';
  fMaxForwards := '';
  fPragma := '';
  fProxyAuthorization := '';
  fRange := '';
  fReferer := '';
  fTE := '';
  fTrailer := '';
  fTransferEncoding := '';
  fUpgrade := '';
  fUserAgent := '';
  fVia := '';
  fWarning := '';
  fCustomHeaders.clear;
  FCookies.Clear;
  DoChange(-1);
end;

{**************************************************************}
procedure TALHTTPRequestHeader.DoChange(propertyIndex: Integer);
begin
  if assigned(FonChange) then FonChange(Self,propertyIndex);
end;

{******************************************************************************************************}
procedure TALHTTPRequestHeader.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: string);

  {--------------------------------------}
  procedure alg001(Var AProperty: String);
  Begin
    If AProperty <> Value then begin
      AProperty := Value;
      DoChange(Index);
    end;
  end;

begin
  Case index of
    0: alg001(FAccept);
    1: alg001(FAcceptCharSet);
    2: alg001(FAcceptEncoding);
    3: alg001(FAcceptLanguage);
    4: alg001(FAllow);
    5: alg001(FAuthorization);
    6: alg001(FCacheControl);
    7: alg001(FConnection);
    8: alg001(FContentEncoding);
    9: alg001(FContentLanguage);
    10: alg001(FContentLength);
    11: alg001(FContentLocation);
    12: alg001(FContentMD5);
    13: alg001(FContentRange);
    14: alg001(FContentType);
    15: alg001(FDate);
    16: alg001(fExpect);
    17: alg001(FExpires);
    18: alg001(FFrom);
    19: alg001(FHost);
    20: alg001(FIfMatch);
    21: alg001(FIfModifiedSince);
    22: alg001(fIfNoneMatch);
    23: alg001(fIfRange);
    24: alg001(fIfUnmodifiedSince);
    25: alg001(fLastModified);
    26: alg001(fMaxForwards);
    27: alg001(FPragma);
    28: alg001(FProxyAuthorization);
    29: alg001(FRange);
    30: alg001(FReferer);
    31: alg001(fTE);
    32: alg001(FTrailer);
    33: alg001(FTransferEncoding);
    34: alg001(FUpgrade);
    35: alg001(FUserAgent);
    36: alg001(FVia);
    37: alg001(FWarning);
  end;
end;

{*****************************************************}
Function TALHTTPRequestHeader.GetRawHeaderText: String;
Var i : integer;
begin
  Result := '';
  If trim(fAccept) <> '' then result := result + 'Accept: ' + trim(FAccept) + #13#10;
  If trim(fAcceptCharSet) <> '' then result := result + 'Accept-Charset: ' + trim(FAcceptCharSet) + #13#10;
  If trim(fAcceptEncoding) <> '' then result := result + 'Accept-Encoding: ' + trim(FAcceptEncoding) + #13#10;
  If trim(fAcceptLanguage) <> '' then result := result + 'Accept-Language: ' + trim(FAcceptLanguage) + #13#10;
  If trim(fAllow) <> '' then result := result + 'Allow: ' + trim(FAllow) + #13#10;
  If trim(fAuthorization) <> '' then result := result + 'Authorization: ' + trim(FAuthorization) + #13#10;
  If trim(fCacheControl) <> '' then result := result + 'Cache-Control: ' + trim(FCacheControl) + #13#10;
  If trim(fConnection) <> '' then result := result + 'Connection: ' + trim(FConnection) + #13#10;
  If trim(fContentEncoding) <> '' then result := result + 'Content-Encoding: ' + trim(FContentEncoding) + #13#10;
  If trim(fContentLanguage) <> '' then result := result + 'Content-Language: ' + trim(FContentLanguage) + #13#10;
  If trim(fContentLength) <> '' then result := result + 'Content-Length: ' + trim(FContentLength) + #13#10;
  If trim(fContentLocation) <> '' then result := result + 'Content-Location: ' + trim(FContentLocation) + #13#10;
  If trim(fContentMD5) <> '' then result := result + 'Content-MD5: ' + trim(FContentMD5) + #13#10;
  If trim(fContentRange) <> '' then result := result + 'Content-Range: ' + trim(FContentRange) + #13#10;
  If trim(fContentType) <> '' then result := result + 'Content-Type: ' + trim(FContentType) + #13#10;
  If trim(fDate) <> '' then result := result + 'Date: ' + trim(FDate) + #13#10;
  If trim(fExpect) <> '' then result := result + 'Expect: ' + trim(FExpect) + #13#10;
  If trim(fExpires) <> '' then result := result + 'Expires: ' + trim(FExpires) + #13#10;
  If trim(fFrom) <> '' then result := result + 'From: ' + trim(FFrom) + #13#10;
  If trim(fHost) <> '' then result := result + 'Host: ' + trim(FHost) + #13#10;
  If trim(fIfMatch) <> '' then result := result + 'If-Match: ' + trim(FIfMatch) + #13#10;
  If trim(fIfModifiedSince) <> '' then result := result + 'If-Modified-Since: ' + trim(FIfModifiedSince) + #13#10;
  If trim(fIfNoneMatch) <> '' then result := result + 'If-None-Match: ' + trim(fIfNoneMatch) + #13#10;
  If trim(fIfRange) <> '' then result := result + 'If-Range: ' + trim(fIfRange) + #13#10;
  If trim(fIfUnmodifiedSince) <> '' then result := result + 'If-Unmodified-Since: ' + trim(fIfUnmodifiedSince) + #13#10;
  If trim(fLastModified) <> '' then result := result + 'Last-Modified: ' + trim(fLastModified) + #13#10;
  If trim(fMaxForwards) <> '' then result := result + 'Max-Forwards: ' + trim(fMaxForwards) + #13#10;
  If trim(fPragma) <> '' then result := result + 'Pragma: ' + trim(FPragma) + #13#10;
  If trim(fProxyAuthorization) <> '' then result := result + 'Proxy-Authorization: ' + trim(FProxyAuthorization) + #13#10;
  If trim(fRange) <> '' then result := result + 'Range: ' + trim(FRange) + #13#10;
  If trim(fReferer) <> '' then result := result + 'Referer: ' + trim(FReferer) + #13#10;
  If trim(fTE) <> '' then result := result + 'TE: ' + trim(fTE) + #13#10;
  If trim(fTrailer) <> '' then result := result + 'Trailer: ' + trim(FTrailer) + #13#10;
  If trim(fTransferEncoding) <> '' then result := result + 'Transfer-Encoding: ' + trim(FTransferEncoding) + #13#10;
  If trim(fUpgrade) <> '' then result := result + 'Upgrade: ' + trim(FUpgrade) + #13#10;
  If trim(fUserAgent) <> '' then result := result + 'User-Agent: ' + trim(FUserAgent) + #13#10;
  If trim(fVia) <> '' then result := result + 'Via: ' + trim(FVia) + #13#10;
  If trim(fWarning) <> '' then result := result + 'Warning: ' + trim(FWarning) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (trim(FCustomHeaders.names[i]) <> '') and (trim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + trim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
  If FCookies.Count > 0 then
    result := result + 'Cookie: ' + AlStringReplace(Trim(FCookies.text),#13#10,'; ', [rfReplaceAll]) + #13#10;
end;

{****************************************************************************}
procedure TALHTTPRequestHeader.SetRawHeaderText(const aRawHeaderText: string);
Var aRawHeaderLst: TstringList;
    j: integer;

  {-------------------------------------}
  Function AlG001(aName: String): String;
  Var i: Integer;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := Trim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
    end
    else result := '';
  end;

begin
  aRawHeaderLst := TstringList.create;
  try
    aRawHeaderLst.NameValueSeparator := ':';
    aRawHeaderLst.Text := aRawHeaderText;

    fAccept := Alg001('Accept');
    fAcceptCharSet := Alg001('Accept-Charset');
    fAcceptEncoding := Alg001('Accept-Encoding');
    fAcceptLanguage := Alg001('Accept-Language');
    fAllow := Alg001('Allow');
    fAuthorization := Alg001('Authorization');
    fCacheControl := Alg001('Cache-Control');
    fConnection := Alg001('Connection');
    fContentEncoding := Alg001('Content-Encoding');
    fContentLanguage := Alg001('Content-Language');
    fContentLength := Alg001('Content-Length');
    fContentLocation := Alg001('Content-Location');
    fContentMD5 := Alg001('Content-MD5');
    fContentRange := Alg001('Content-Range');
    fContentType := Alg001('Content-Type');
    fDate := Alg001('Date');
    fExpect := Alg001('Expect');
    fExpires := Alg001('Expires');
    fFrom := Alg001('From');
    fHost := Alg001('Host');
    fIfMatch := Alg001('If-Match');
    fIfModifiedSince := Alg001('If-Modified-Since');
    fIfNoneMatch := Alg001('If-None-Match');
    fIfRange := Alg001('If-Range');
    fIfUnmodifiedSince := Alg001('If-Unmodified-Since');
    fLastModified := Alg001('Last-Modified');
    fMaxForwards := Alg001('Max-Forwards');
    fPragma := Alg001('Pragma');
    fProxyAuthorization := Alg001('Proxy-Authorization');
    fRange := Alg001('Range');
    fReferer := Alg001('Referer');
    fTE := Alg001('TE');
    fTrailer := Alg001('Trailer');
    fTransferEncoding := Alg001('Transfer-Encoding');
    fUpgrade := Alg001('Upgrade');
    fUserAgent := Alg001('User-Agent');
    fVia := Alg001('Via');
    fWarning := Alg001('Warning');

    FCookies.clear;
    J := aRawHeaderLst.IndexOfName('Cookie');
    If J >= 0 then begin
      ALExtractHTTPFields([';'], [' '], [], PChar(aRawHeaderLst.ValueFromIndex[j]), Cookies, True);
      aRawHeaderLst.Delete(j);
    end;

    FCustomHeaders.clear;
    For j := 0 to aRawHeaderLst.count - 1 do
      If trim(aRawHeaderLst[j]) <> '' then
        FCustomHeaders.Add(aRawHeaderLst[j]);

    DoChange(-1);
  finally
    aRawHeaderLst.Free;
  end;
end;

{*********************************************************}
procedure TALHTTPRequestHeader.AssignTo(Dest: TPersistent);
begin
  if Dest is TALHTTPRequestHeader then begin
    with Dest as TALHTTPRequestHeader do begin
      fAccept := self.fAccept;
      fAcceptCharSet := self.fAcceptCharSet;
      fAcceptEncoding := self.fAcceptEncoding;
      fAcceptLanguage := self.fAcceptLanguage;
      fAllow := self.fAllow;
      fAuthorization := self.fAuthorization;
      fCacheControl := self.fCacheControl;
      fConnection := self.fConnection;
      fContentEncoding := self.fContentEncoding;
      fContentLanguage := self.fContentLanguage;
      fContentLength := self.fContentLength;
      fContentLocation := self.fContentLocation;
      fContentMD5 := self.fContentMD5;
      fContentRange := self.fContentRange;
      fContentType := self.fContentType;
      fDate := self.fDate;
      fExpect := self.fExpect;
      fExpires := self.fExpires;
      fFrom := self.fFrom;
      fHost := self.fHost;
      fIfMatch := self.fIfMatch;
      fIfModifiedSince := self.fIfModifiedSince;
      fIfNoneMatch := self.fIfNoneMatch;
      fIfRange := self.fIfRange;
      fIfUnmodifiedSince := self.fIfUnmodifiedSince;
      fLastModified := self.fLastModified;
      fMaxForwards := self.fMaxForwards;
      fPragma := self.fPragma;
      fProxyAuthorization := self.fProxyAuthorization;
      fRange := self.fRange;
      fReferer := self.fReferer;
      fTE := self.fTE;
      fTrailer := self.fTrailer;
      fTransferEncoding := self.fTransferEncoding;
      fUpgrade := self.fUpgrade;
      fUserAgent := self.fUserAgent;
      fVia := self.fVia;
      fWarning := self.fWarning;
      fCustomHeaders.assign(self.fCustomHeaders);
      FCookies.Assign(self.fCookies);
      Dochange(-1);
    end;
  end
  else inherited AssignTo(Dest);
end;

{***************************************************************}
procedure TALHTTPRequestHeader.SetCookies(const Value: TStrings);
begin
  FCookies.Assign(Value);
end;

{*********************************************************************}
procedure TALHTTPRequestHeader.SetCustomHeaders(const Value: Tstrings);
begin
  FCustomHeaders.Assign(Value);
end;



///////////////////////////////////////////////////////////////////////////////////////
////////// Http Misc function /////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////

{************************************************}
function ALHTTPDecode(const AStr: String): String;
var Sp, Rp, Cp, Tp: PChar;
    int: integer;
    S: String;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Tp := Sp;
             Inc(Sp);

             //escaped % (%%)
             if Sp^ = '%' then Rp^ := '%'

             // %<hex> encoded character
             else begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then begin
                 S := '$' + Cp^ + Sp^;
                 if trystrtoint(s,int) then Rp^ := Chr(int)
                 else begin
                   Rp^ := '%';
                   Sp := Tp;
                 end;
               end
               else begin
                 Rp^ := '%';
                 Sp := Tp;
               end;
             end;
           end;
      else Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

{***********************************************************}
procedure ALHTTPEncodeParamNameValues(ParamValues: TStrings);
var i: Integer;
    LPos: integer;
    LStr: string;
begin
  for i := 0 to ParamValues.Count - 1 do begin
    LStr := ParamValues[i];
    LPos := AlPos(ParamValues.NameValueSeparator, LStr);
    if LPos > 0 then ParamValues[i] := HTTPEncode(AlCopyStr(LStr, 1, LPos-1)) + '=' + HTTPEncode(AlCopyStr(LStr, LPos+1, MAXINT));
  end;
end;

{********************************************************}
{Parses a multi-valued string into its constituent fields.
 ExtractHTTPFields is a general utility to parse multi-valued HTTP header strings into separate substrings.
 *Separators is a set of characters that are used to separate individual values within the multi-valued string.
 *WhiteSpace is a set of characters that are to be ignored when parsing the string.
 *Content is the multi-valued string to be parsed.
 *Strings is the TStrings object that receives the individual values that are parsed from Content.
 *StripQuotes determines whether the surrounding quotes are removed from the resulting items. When StripQuotes is true, surrounding quotes are
  removed before substrings are added to Strings.
 Note:	Characters contained in Separators or WhiteSpace are treated as part of a value substring if the substring is surrounded by single
 or double quote marks. HTTP escape characters are converted using the HTTPDecode function.}
procedure ALExtractHTTPFields(Separators,
                              WhiteSpace,
                              Quotes: TSysCharSet;
                              Content: PChar;
                              Strings: TStrings;
                              StripQuotes: Boolean = False);
begin
  ALExtractHeaderFields(Separators, WhiteSpace, Quotes, Content, Strings, True, StripQuotes);
end;

{************************************************************}
Function AlExtractShemeFromUrl(aUrl: String): TInternetScheme;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: String;
var PortNumber: integer;
begin
  if AlInternetCrackUrl(aUrl,
                        SchemeName,
                        HostName,
                        UserName,
                        Password,
                        UrlPath,
                        ExtraInfo,
                        PortNumber) then begin
    if sametext(SchemeName,'http') then result := INTERNET_SCHEME_HTTP
    else if sametext(SchemeName,'https') then result := INTERNET_SCHEME_HTTPS
    else if sametext(SchemeName,'ftp') then result := INTERNET_SCHEME_FTP
    else result := INTERNET_SCHEME_UNKNOWN;
  end
  else result := INTERNET_SCHEME_UNKNOWN;
end;

{******************************************************}
Function AlExtractHostNameFromUrl(aUrl: String): String;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: String;
    PortNumber: integer;
begin
  if AlInternetCrackUrl(aUrl,
                        SchemeName,
                        HostName,
                        UserName,
                        Password,
                        UrlPath,
                        ExtraInfo,
                        PortNumber) then result := HostName
  else result := '';
end;

{********************************************************}
Function AlExtractDomainNameFromUrl(aUrl: String): String;
begin
  Result := AlExtractHostNameFromUrl(aUrl);
  while length(AlStringReplace(Result,
                               '.',
                               '',
                               [rfReplaceALL])) < length(result) - 1 do delete(Result, 1, ALpos('.',result));
end;

{******************************************************}
Function  AlExtractUrlPathFromUrl(aUrl: String): String;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: String;
    PortNumber: integer;
begin
  if AlInternetCrackUrl(aUrl,
                        SchemeName,
                        HostName,
                        UserName,
                        Password,
                        UrlPath,
                        ExtraInfo,
                        PortNumber) then result := UrlPath
  else result := '';
end;

{*************
flags can be :
  ICU_DECODE Converts encoded characters back to their normal form. (Exemple: sam%40a%mple.xml => sam@a%mple.xml)
  ICU_ESCAPE Converts all escape sequences (%xx) to their corresponding characters (Exemple: sam%40a%mple.xml => error because %m is a wrong escape sequences !)}
Function AlInternetCrackUrl(aUrl: String;
                            Var SchemeName,
                                HostName,
                                UserName,
                                Password,
                                UrlPath,
                                ExtraInfo: String;
                            var PortNumber: integer): Boolean;
Var P1: Integer;
    S1: String;
begin
  SchemeName := '';
  HostName := '';
  UserName := '';
  Password := '';
  UrlPath := '';
  ExtraInfo := '';
  PortNumber := 0;
  Result := True;

  P1 := AlPos('://', aUrl);  // ftp://xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx?param1=value1
  if P1 > 0 then begin
    SchemeName := AlCopyStr(aUrl, 1, P1-1); // ftp
    delete(aUrl,1, P1+2);                   // xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx?param1=value1
    P1 := AlPos('?',aUrl);
    if P1 > 0 then begin
      ExtraInfo := AlCopyStr(aUrl, P1, Maxint); // ?param1=value1
      delete(aUrl, P1, Maxint);                 // xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx
    end;
    P1 := AlPos('/',aUrl);
    if P1 > 0 then begin
      UrlPath := AlCopyStr(aUrl, P1, Maxint); // /path/filename.xxx
      delete(aUrl, P1, Maxint);               // xxxx:yyyyy@ftp.yoyo.com:21
    end;
    P1 := lastdelimiter('@',aUrl);
    if P1 > 0 then begin
      S1 := AlCopyStr(aUrl, 1, P1-1); // xxxx:yyyyy
      delete(aUrl,1, P1);             // ftp.yoyo.com:21
      P1 := Alpos(':', S1);
      if P1 > 0 then begin
        UserName := AlCopyStr(S1,1,P1-1);      // xxxx
        Password := AlCopyStr(S1,P1+1,Maxint); // yyyyy
      end
      else begin
        UserName := S1;
        Password := '';
      end;
    end;
    P1 := AlPos(':',aUrl);
    if P1 > 0 then begin
      S1 := AlCopyStr(aUrl, P1+1, Maxint); // 21
      delete(aUrl, P1, Maxint);            // ftp.yoyo.com
      if not tryStrToInt(S1, PortNumber) then PortNumber := 0;
    end;
    if PortNumber = 0 then begin
      if sameText(SchemeName, 'http') then PortNumber := 80
      else if sameText(SchemeName, 'https') then PortNumber := 443
      else if sameText(SchemeName, 'ftp') then PortNumber := 21
      else result := False;
    end;
    if result then begin
      HostName := aUrl;
      result := HostName <> '';
    end;
  end
  else result := False;

  if not result then begin
    SchemeName := '';
    HostName := '';
    UserName := '';
    Password := '';
    UrlPath := '';
    ExtraInfo := '';
    PortNumber := 0;
  end;
end;


{***************************************}
Function AlInternetCrackUrl(aUrl: String;
                            Var SchemeName,
                                HostName,
                                UserName,
                                Password,
                                UrlPath,
                                Anchor: String; // not the anchor is never send to the server ! it's only used on client side
                            Query: TStrings;
                            var PortNumber: integer): Boolean;
var aExtraInfo: string;
    P1: integer;
begin
  Result := AlInternetCrackUrl(aUrl,
                               SchemeName,
                               HostName,
                               UserName,
                               Password,
                               UrlPath,
                               aExtraInfo,
                               PortNumber);
  if result then begin
    P1 := AlPos('#',aExtraInfo);
    if P1 > 0 then begin
      Anchor := AlCopyStr(aExtraInfo, P1+1, MaxInt);
      delete(aExtraInfo, P1, Maxint);
    end
    else Anchor := '';
    if (aExtraInfo <> '') and (aExtraInfo[1] = '?') then begin
      {$IF CompilerVersion >= 18.5}
        if AlPos('&amp;', aExtraInfo) > 0 then Query.LineBreak := '&amp;'
        else Query.LineBreak := '&';
        Query.text := AlCopyStr(aExtraInfo,2,Maxint);
      {$ELSE}
        if AlPos('&amp;', aExtraInfo) > 0 then Query.text := AlStringReplace(AlCopyStr(aExtraInfo,2,Maxint), '&amp;', #13#10, [rfReplaceAll])
        else                                   Query.text := AlStringReplace(AlCopyStr(aExtraInfo,2,Maxint), '&',     #13#10, [rfReplaceAll]);
      {$IFEND}
    end
    else Query.clear;
  end
  else begin
    Anchor := '';
    Query.clear;
  end;

end;

{*******************************************}
Function  AlInternetCrackUrl(var Url: String;  // if true return the relative url
                             var Anchor: String;
                             Query: TStrings): Boolean;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath: String;
    PortNumber: integer;
    tmpUrl: String;
begin

  //exit if no url
  if Url = '' then begin
    result := False;
    Exit;
  end;

  //first try with full url
  Result := AlInternetCrackUrl(Url,
                               SchemeName,
                               HostName,
                               UserName,
                               Password,
                               UrlPath,
                               Anchor,
                               Query,
                               PortNumber);
  if Result then Url := UrlPath

  //try with relative url
  else begin
    tmpUrl := Url;
    if tmpUrl[1] = '/' then tmpUrl := 'http://www.arkadia.com' + tmpUrl // we don't take care of the domaine name, it's will be skip, it's just to make the url valid
    else tmpUrl := 'http://www.arkadia.com/' + tmpUrl;
    Result := AlInternetCrackUrl(tmpUrl,
                                 SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 Anchor,
                                 Query,
                                 PortNumber);
    if Result then Url := UrlPath;
  end;

end;

{**********************************************************************************}
Function AlRemoveAnchorFromUrl(aUrl: String; Var aAnchor: String): String; overload;
Var P1: integer;
begin
  P1 := AlPos('#',aUrl);
  if P1 > 0 then begin
    aAnchor := AlCopyStr(aUrl, P1+1, MaxInt);
    delete(aUrl, P1, Maxint);
  end
  else aAnchor := '';
  Result := aUrl;
end;

{*************************************************************}
Function AlRemoveAnchorFromUrl(aUrl: String): String; overload;
var aAnchor: String;
begin
  result := AlRemoveAnchorFromUrl(aUrl,aAnchor);
end;

{**********************************************************}
function AlCombineUrl(RelativeUrl, BaseUrl: String): String;
var Size: Dword;
    Buffer: String;
begin
  case AlExtractShemeFromUrl(RelativeUrl) of

    {relative path.. so try to combine the url}
    INTERNET_SCHEME_PARTIAL,
    INTERNET_SCHEME_UNKNOWN,
    INTERNET_SCHEME_DEFAULT: begin
                               Size := INTERNET_MAX_URL_LENGTH;
                               SetLength(Buffer, Size);
                               if InternetCombineUrl(PChar(BaseUrl), PChar(RelativeUrl), @Buffer[1], size, ICU_BROWSER_MODE or ICU_no_encode) then Result := AlCopyStr(Buffer, 1, Size)
                               else result := RelativeUrl;
                             end;

    {not a relative path}
    else result := RelativeUrl;

  end;
end;

{*********************************}
Function  AlCombineUrl(RelativeUrl,
                       BaseUrl,
                       Anchor: String;
                       Query: TStrings): String;
Var S1 : String;
    aBool: Boolean;
begin
  if Query.Count > 0 then begin

    {$IF CompilerVersion >= 18.5}
      if Query.LineBreak = #13#10 then begin
        aBool := True;
        Query.LineBreak := '&';
      end
      else aBool := False;

      try

        S1 := trim(Query.Text);
        while alpos(Query.LineBreak, S1) = 1 do delete(S1,1,length(Query.LineBreak));
        while alposEx(Query.LineBreak,
                      S1,
                      length(S1) - length(Query.LineBreak) + 1) > 0 do delete(S1,
                                                                              length(S1) - length(Query.LineBreak) + 1,
                                                                              MaxInt);
        if S1 <> '' then S1 := '?' + S1;

      finally
        if aBool then Query.LineBreak := #13#10;
      end;
    {$ELSE}
      S1 := AlStringReplace(trim(Query.Text),#13#10,'&',[rfReplaceAll]);
      while alpos('&', S1) = 1 do delete(S1,1,1);
      while alposEx('&',
                    S1,
                    length(S1)) > 0 do delete(S1,
                                              length(S1),
                                              MaxInt);
      if S1 <> '' then S1 := '?' + S1;
    {$IFEND}

  end
  else S1 := '';

  if Anchor <> '' then S1 := S1 + '#' + Anchor;

  Result := AlCombineUrl(RelativeUrl + S1, BaseUrl);
end;

{********************************************************************************}
Function ALTryIPV4StrToNumeric(aIPv4Str: String; var aIPv4Num: Cardinal): Boolean;
Var P1, P2: Integer;
    I1, I2, I3, I4: integer;
Begin

  {----------}
  if aIPv4Str = '' then begin
    Result := False;
    Exit;
  end;

  {----------}
  P1 := 1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not TryStrtoInt(AlCopyStr(aIPv4Str,P1,P2-P1), I1)) or
     (not (I1 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  {----------}
  P1 := P2+1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not TryStrtoInt(AlCopyStr(aIPv4Str,P1,P2-P1), I2)) or
     (not (I2 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  {----------}
  P1 := P2+1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not TryStrtoInt(AlCopyStr(aIPv4Str,P1,P2-P1), I3)) or
     (not (I3 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  {----------}
  P1 := P2+1;
  P2 := length(aIPv4Str) + 1;
  if (P2 <= P1) or
     (not TryStrtoInt(AlCopyStr(aIPv4Str,P1,P2-P1), I4)) or
     (not (I4 in  [0..255])) then begin
    Result := False;
    Exit;
  end;

  {-------------}
  Result := True;
  aIPv4Num := (I1*256*256*256) + (I2*256*256) +  (I3*256) + (I4);

End;

{***************************************************}
Function ALIPV4StrToNumeric(aIPv4: String): Cardinal;
Begin
  if not ALTryIPV4StrToNumeric(aIPv4, Result) then Raise Exception.Create('Bad IPv4 string: ' + aIPv4);
End;

{***************************************************}
Function ALNumericToIPv4Str(aIPv4: Cardinal): String;
Var S1, S2, S3, S4: String;
    P1: extended;
Begin
  P1 := aIPv4 / 256;
  S4 := inttostr(trunc(256 * (P1 - INT(P1))));
  P1 := P1 / 256;
  S3 := inttostr(trunc(256 * (P1 - INT(P1))));
  P1 := P1 / 256;
  S2 := inttostr(trunc(256 * (P1 - INT(P1))));
  S1 := inttostr(trunc (P1));
  Result := S1 + '.' + S2 + '.' + S3 + '.' + S4;
End;

end.
