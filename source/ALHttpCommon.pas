{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe Common Http Functions
Version:      4.00

Description:  Common http functions that can be use by HTTP Components

Legal issues: Copyright (C) 1999-2012 by Arkadia Software Engineering

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
              26/06/2012: * Add xe2 support

Link :        http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
              http://wp.netscape.com/newsref/std/cookie_spec.html

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by
  promoting the sponsor on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALHttpCommon;

interface

uses Windows,
     Classes,
     sysutils,
     Wininet,
     ALFcnString,
     AlStringList;

Type

  {---------------------------------------------------}
  EALHttpClientConnectionDropped = class(EALException);

  {-- onchange Event that specify the property index that is just changed --}
  TALHTTPPropertyChangeEvent = procedure(sender: Tobject; Const PropertyIndex: Integer) of object;

  {--protocol version--}
  TALHTTPProtocolVersion = (HTTPpv_1_0, HTTPpv_1_1);

  {--Request method--}
  TALHTTPRequestMethod = (HTTPrm_Get,
                          HTTPrm_Post,
                          HTTPrm_Head,
                          HTTPrm_Trace,
                          HTTPrm_Put,
                          HTTPrm_Delete);

  {--Request header--}
  TALHTTPRequestHeader = Class(Tcomponent)
  Private
    fAccept: AnsiString;
    fAcceptCharSet: AnsiString;
    fAcceptEncoding: AnsiString;
    fAcceptLanguage: AnsiString;
    fAllow: AnsiString;
    fAuthorization: AnsiString;
    fCacheControl: AnsiString;
    fConnection: AnsiString;
    fContentEncoding: AnsiString;
    fContentLanguage: AnsiString;
    fContentLength: AnsiString;
    fContentLocation: AnsiString;
    fContentMD5: AnsiString;
    fContentRange: AnsiString;
    fContentType: AnsiString;
    fDate: AnsiString;
    fExpect: AnsiString;
    fExpires: AnsiString;
    fFrom: AnsiString;
    fHost: AnsiString;
    fIfMatch: AnsiString;
    fIfModifiedSince: AnsiString;
    fIfNoneMatch: AnsiString;
    fIfRange: AnsiString;
    fIfUnmodifiedSince: AnsiString;
    fLastModified: AnsiString;
    fMaxForwards: AnsiString;
    fPragma: AnsiString;
    fProxyAuthorization: AnsiString;
    fRange: AnsiString;
    fReferer: AnsiString;
    fTE: AnsiString;
    fTrailer: AnsiString;
    fTransferEncoding: AnsiString;
    fUpgrade: AnsiString;
    fUserAgent: AnsiString;
    fVia: AnsiString;
    fWarning: AnsiString;
    FCustomHeaders: TALStrings;
    FCookies: TALStrings;
    FOnChange: TALHTTPPropertyChangeEvent;
    Procedure DoChange(propertyIndex: Integer);
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
    Function GetRawHeaderText: AnsiString;
    procedure SetCookies(const Value: TALStrings);
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
    procedure SetCustomHeaders(const Value: TALStrings);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    Property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
  Published
    property Accept: AnsiString index 0 read FAccept write SetHeaderValueByPropertyIndex; {Accept: audio/*; q=0.2, audio/basic}
    property AcceptCharSet: AnsiString index 1 read FAcceptCharSet write SetHeaderValueByPropertyIndex; {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
    property AcceptEncoding: AnsiString index 2 read FAcceptEncoding write SetHeaderValueByPropertyIndex; {Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0}
    property AcceptLanguage: AnsiString index 3 read FAcceptLanguage write SetHeaderValueByPropertyIndex; {Accept-Language: da, en-gb;q=0.8, en;q=0.7}
    property Allow: AnsiString index 4 read FAllow write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property Authorization: AnsiString index 5 read FAuthorization write SetHeaderValueByPropertyIndex; {Authorization: BASIC d2VibWFzdGVyOnpycW1hNHY=}
    property CacheControl: AnsiString index 6 read FCacheControl write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: AnsiString index 7 read FConnection write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: AnsiString index 8 read FContentEncoding write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: AnsiString index 9 read FContentLanguage write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: AnsiString index 10 read FContentLength write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: AnsiString index 11 read FContentLocation write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: AnsiString index 12 read FContentMD5 write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: AnsiString index 13 read FContentRange write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: AnsiString index 14 read FContentType write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property Date: AnsiString index 15 read FDate write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property Expect: AnsiString index 16 read fExpect write SetHeaderValueByPropertyIndex; {Expect: 100-continue}
    property Expires: AnsiString index 17 read FExpires write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property From: AnsiString index 18 read FFrom write SetHeaderValueByPropertyIndex; {From: webmaster@w3.org}
    property Host: AnsiString index 19 read FHost write SetHeaderValueByPropertyIndex; {Host: www.w3.org}
    property IfMatch: AnsiString index 20 read FIfMatch write SetHeaderValueByPropertyIndex; {If-Match: entity_tag001}
    property IfModifiedSince: AnsiString index 21 read FIfModifiedSince write SetHeaderValueByPropertyIndex; {If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property IfNoneMatch: AnsiString index 22 read fIfNoneMatch write SetHeaderValueByPropertyIndex; {If-None-Match: entity_tag001}
    property IfRange: AnsiString index 23 read fIfRange write SetHeaderValueByPropertyIndex; {If-Range: entity_tag001}
    property IfUnmodifiedSince: AnsiString index 24 read fIfUnmodifiedSince write SetHeaderValueByPropertyIndex; {If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property LastModified: AnsiString index 25 read fLastModified write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property MaxForwards: AnsiString index 26 read fMaxForwards write SetHeaderValueByPropertyIndex; {Max-Forwards: 3}
    property Pragma: AnsiString index 27 read FPragma write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthorization: AnsiString index 28 read FProxyAuthorization write SetHeaderValueByPropertyIndex; {Proxy-Authorization: [credentials]}
    property Range: AnsiString index 29 read FRange write SetHeaderValueByPropertyIndex; {Range: bytes=100-599}
    property Referer: AnsiString index 30 read FReferer write SetHeaderValueByPropertyIndex; {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    property TE: AnsiString index 31 read fTE write SetHeaderValueByPropertyIndex; {TE: trailers, deflate;q=0.5}
    property Trailer: AnsiString index 32 read FTrailer write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property TransferEncoding: AnsiString index 33 read FTransferEncoding write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: AnsiString index 34 read FUpgrade write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property UserAgent: AnsiString index 35 read FUserAgent write SetHeaderValueByPropertyIndex; {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    property Via: AnsiString index 36 read FVia write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: AnsiString index 37 read FWarning write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property CustomHeaders: TALStrings read FCustomHeaders write SetCustomHeaders;
    property Cookies: TALStrings read FCookies write SetCookies;
    property OnChange: TALHTTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {--TALHTTPCookie--}
  TALHTTPResponseCookie = class(TCollectionItem)
  private
    FName: AnsiString;
    FValue: AnsiString;
    FPath: AnsiString;
    FDomain: AnsiString;
    FExpires: TDateTime;
    FSecure: Boolean;
  protected
    function GetHeaderValue: AnsiString;
    procedure SetHeaderValue(Const aValue: AnsiString);
  public
    constructor Create(Collection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    property HeaderValue: AnsiString read GetHeaderValue write SetHeaderValue;
  Published
    property Name: AnsiString read FName write FName;
    property Value: AnsiString read FValue write FValue;
    property Domain: AnsiString read FDomain write FDomain;
    property Path: AnsiString read FPath write FPath;
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
    FAcceptRanges: AnsiString;
    FAge: AnsiString;
    FAllow: AnsiString;
    FCacheControl: AnsiString;
    FConnection: AnsiString;
    FContentEncoding: AnsiString;
    FContentLanguage: AnsiString;
    FContentLength: AnsiString;
    FContentLocation: AnsiString;
    FContentMD5: AnsiString;
    FContentRange: AnsiString;
    FContentType: AnsiString;
    FDate: AnsiString;
    FETag: AnsiString;
    FExpires: AnsiString;
    FLastModified: AnsiString;
    FLocation: AnsiString;
    FPragma: AnsiString;
    FProxyAuthenticate: AnsiString;
    FRetryAfter: AnsiString;
    FServer: AnsiString;
    FTrailer: AnsiString;
    FTransferEncoding: AnsiString;
    FUpgrade: AnsiString;
    FVary: AnsiString;
    FVia: AnsiString;
    FWarning: AnsiString;
    FWWWAuthenticate: AnsiString;
    FRawHeaderText: AnsiString;
    FCustomHeaders: TALStrings;
    FCookies: TALHTTPResponseCookieCollection;
    FStatusCode: AnsiString;
    FHttpProtocolVersion: AnsiString;
    FReasonPhrase: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
    Function GetRawHeaderText: AnsiString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property AcceptRanges: AnsiString read FAcceptRanges; {Accept-Ranges: bytes}
    property Age: AnsiString read FAge; {Age: 2147483648(2^31)}
    property Allow: AnsiString read FAllow; {Allow: GET, HEAD, PUT}
    property CacheControl: AnsiString read FCacheControl; {Cache-Control: no-cache}
    property Connection: AnsiString read FConnection; {Connection: close}
    property ContentEncoding: AnsiString read FContentEncoding; {Content-Encoding: gzip}
    property ContentLanguage: AnsiString read FContentLanguage; {Content-Language: mi, en}
    property ContentLength: AnsiString read FContentLength;  {Content-Length: 3495}
    property ContentLocation: AnsiString read FContentLocation;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: AnsiString read FContentMD5;  {Content-MD5: [md5-digest]}
    property ContentRange: AnsiString read FContentRange;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: AnsiString read FContentType; {Content-Type: text/html; charset=ISO-8859-4}
    property Date: AnsiString read FDate; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property ETag: AnsiString read FETag; {ETag: W/"xyzzy"}
    property Expires: AnsiString read FExpires; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property LastModified: AnsiString read FLastModified; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property Location: AnsiString read FLocation; {Location: http://www.w3.org/pub/WWW/People.html}
    property Pragma: AnsiString read FPragma; {Pragma: no-cache}
    property ProxyAuthenticate: AnsiString read FProxyAuthenticate; {Proxy-Authenticate: [challenge]}
    property RetryAfter: AnsiString read FRetryAfter; {Retry-After: Fri, 31 Dec 1999 23:59:59 GMT}
    property Server: AnsiString read FServer; {Server: CERN/3.0 libwww/2.17}
    property Trailer: AnsiString read FTrailer; {Trailer: Date}
    property TransferEncoding: AnsiString read FTransferEncoding; {Transfer-Encoding: chunked}
    property Upgrade: AnsiString read FUpgrade; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property Vary: AnsiString read FVary; {Vary: Date}
    property Via: AnsiString read FVia; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: AnsiString read FWarning; {Warning: 112 Disconnected Operation}
    property WWWAuthenticate: AnsiString read FWWWAuthenticate; {WWW-Authenticate: [challenge]}
    Property CustomHeaders: TALStrings read FCustomHeaders;
    Property Cookies: TALHTTPResponseCookieCollection read FCookies;
    property StatusCode: AnsiString read FStatusCode;
    property HttpProtocolVersion: AnsiString read FHttpProtocolVersion;
    Property ReasonPhrase: AnsiString read FReasonPhrase;
    property RawHeaderText: AnsiString read GetRawHeaderText write setRawHeaderText;
  end;

{Http Function}
function  ALHTTPDecode(const AStr: AnsiString): AnsiString;
procedure ALHTTPEncodeParamNameValues(ParamValues: TALStrings);
procedure ALExtractHTTPFields(Separators,
                              WhiteSpace,
                              Quotes: TSysCharSet;
                              Content: PAnsiChar;
                              Strings: TALStrings;
                              StripQuotes: Boolean = False);
Function  AlExtractShemeFromUrl(aUrl: AnsiString): TInternetScheme;
Function  AlExtractHostNameFromUrl(aUrl: AnsiString): AnsiString;
Function  AlExtractDomainNameFromUrl(aUrl: AnsiString): AnsiString;
Function  AlExtractUrlPathFromUrl(aUrl: AnsiString): AnsiString;
Function  AlInternetCrackUrl(aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 ExtraInfo: AnsiString;
                             var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 Anchor: AnsiString; // not the anchor is never send to the server ! it's only used on client side
                             Query: TALStrings;
                             var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(var Url: AnsiString; // if true return UrlPath
                             var Anchor: AnsiString;
                             Query: TALStrings): Boolean; overload;
Function  AlRemoveAnchorFromUrl(aUrl: AnsiString; Var aAnchor: AnsiString): AnsiString; overload;
Function  AlRemoveAnchorFromUrl(aUrl: AnsiString): AnsiString; overload;
function  AlCombineUrl(RelativeUrl, BaseUrl: AnsiString): AnsiString; overload;
Function  AlCombineUrl(RelativeUrl,
                       BaseUrl,
                       Anchor: AnsiString;
                       Query: TALStrings): AnsiString; overload;

Function ALTryIPV4StrToNumeric(aIPv4Str: ansiString; var aIPv4Num: Cardinal): Boolean;
Function ALIPV4StrToNumeric(aIPv4: ansiString): Cardinal;
Function ALNumericToIPv4Str(aIPv4: Cardinal): ansiString;

type
  TALIPv6Binary = array[1..16] of ansiChar;

Function ALZeroIpV6: TALIPv6Binary;
Function ALTryIPV6StrToBinary(aIPv6Str: ansiString; var aIPv6Bin: TALIPv6Binary): Boolean;
Function ALIPV6StrTobinary(aIPv6: ansiString): TALIPv6Binary;
Function ALBinaryToIPv6Str(aIPv6: TALIPv6Binary): ansiString;
Function ALBinaryStrToIPv6Binary(aIPV6BinaryStr: ansiString): TALIPv6Binary;

ResourceString
  CALHTTPCLient_MsgInvalidURL         = 'Invalid url ''%s'' - only supports ''http'' and ''https'' schemes';
  CALHTTPCLient_MsgInvalidHTTPRequest = 'Invalid HTTP Request: Length is 0';
  CALHTTPCLient_MsgEmptyURL           = 'Empty URL';

implementation

uses HTTPapp,
     alFcnRFC,
     AlFcnMisc;

{***********************************************************************************}
function AlStringFetch(var AInput: AnsiString; const ADelim: AnsiString): AnsiString;
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

{********************************************************}
function TALHTTPResponseCookie.GetHeaderValue: AnsiString;
var aYear, aMonth, aDay: Word;
begin
  Result := ALFormat('%s=%s; ', [HTTPEncode(FName), HTTPEncode(FValue)]);
  if Domain <> '' then Result := Result + ALFormat('domain=%s; ', [Domain]);
  if Path <> '' then Result := Result + ALFormat('path=%s; ', [Path]);
  if Expires > -1 then begin
    DecodeDate(Expires, aYear, aMonth, aDay);
    Result := Result + ALFormat(ALFormatDateTime('"expires=%s, "dd"-%s-"yyyy" "hh":"nn":"ss" GMT; "',
                                                 Expires,
                                                 ALDefaultFormatSettings),
                                [CAlRfc822DaysOfWeek[DayOfWeek(Expires)],
                                 CAlRfc822MonthNames[aMonth]]);
  end;
  if Secure then Result := Result + 'secure';
  if ALCopyStr(Result, Length(Result) - 1, MaxInt) = '; ' then SetLength(Result, Length(Result) - 2);
end;

{***********************************************************************}
procedure TALHTTPResponseCookie.SetHeaderValue(Const aValue: AnsiString);
Var aCookieProp: TALStringList;
    aCookieStr: AnsiString;
begin
  FName:= '';
  FValue:= '';
  FPath:= '';
  FDomain:= '';
  FExpires:= -1;
  FSecure:= False;

  aCookieProp := TALStringList.Create;
  try
    aCookieStr := AValue;

    while ALPos(';', aCookieStr) > 0 do begin
      aCookieProp.Add(ALTrim(AlStringFetch(aCookieStr, ';')));
      if (ALPos(';', aCookieStr) = 0) and (Length(aCookieStr) > 0) then aCookieProp.Add(ALTrim(aCookieStr));
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

{***************************************}
constructor TALHTTPResponseHeader.Create;
begin
  inherited;
  FCustomHeaders := TALStringList.create;
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

{**********************************************************}
function TALHTTPResponseHeader.GetRawHeaderText: AnsiString;
begin
  result := FRawHeaderText;
end;

{*********************************************************************************}
procedure TALHTTPResponseHeader.SetRawHeaderText(Const aRawHeaderText: AnsiString);
Var aRawHeaderLst: TALStringList;
    j: integer;
    AStatusLine: AnsiString;

  {---------------------------------------------}
  Function AlG001(aName: AnsiString): AnsiString;
  Var i: Integer;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
    end
    else result := '';
  end;

begin
  aRawHeaderLst := TALStringList.create;
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
      If ALTrim(aRawHeaderLst.ValueFromIndex[j]) <> '' then
        Cookies.Add.HeaderValue := ALTrim(aRawHeaderLst.ValueFromIndex[j]);
      aRawHeaderLst.Delete(j);
      J := aRawHeaderLst.IndexOfName('Set-Cookie');
    end;

    If aRawHeaderLst.Count > 0 then begin
      AStatusLine := aRawHeaderLst[0]; //HTTP/1.1 200 OK | 200 OK | status: 200 OK
      FHttpProtocolVersion := ALTrim(AlStringFetch(AstatusLine,' '));
      If AlIsInteger(FHttpProtocolVersion) then begin
        FStatusCode := FHttpProtocolVersion;
        FHttpProtocolVersion := '';
      end
      else FStatusCode := ALTrim(AlStringFetch(AstatusLine,' '));
      FReasonPhrase := ALTrim(AstatusLine);

      If not AlIsInteger(FStatusCode) then begin
        FHttpProtocolVersion := '';
        AStatusLine := Alg001('Status');
        FStatusCode := ALTrim(AlStringFetch(AStatusLine,' '));
        FReasonPhrase := ALTrim(AlStringFetch(AStatusLine,' '));
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
      If ALTrim(aRawHeaderLst[j]) <> '' then
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

{**********************************************************}
constructor TALHTTPRequestHeader.Create(AOwner: TComponent);
Begin
  inherited create(AOwner);
  fCustomHeaders:= TALStringList.create;
  fCustomHeaders.NameValueSeparator := ':';
  FCookies := TALStringList.create;
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

{**********************************************************************************************************}
procedure TALHTTPRequestHeader.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);

  {------------------------------------------}
  procedure alg001(Var AProperty: AnsiString);
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

{*********************************************************}
Function TALHTTPRequestHeader.GetRawHeaderText: AnsiString;
Var i : integer;
begin
  Result := '';
  If ALTrim(fAccept) <> '' then result := result + 'Accept: ' + ALTrim(FAccept) + #13#10;
  If ALTrim(fAcceptCharSet) <> '' then result := result + 'Accept-Charset: ' + ALTrim(FAcceptCharSet) + #13#10;
  If ALTrim(fAcceptEncoding) <> '' then result := result + 'Accept-Encoding: ' + ALTrim(FAcceptEncoding) + #13#10;
  If ALTrim(fAcceptLanguage) <> '' then result := result + 'Accept-Language: ' + ALTrim(FAcceptLanguage) + #13#10;
  If ALTrim(fAllow) <> '' then result := result + 'Allow: ' + ALTrim(FAllow) + #13#10;
  If ALTrim(fAuthorization) <> '' then result := result + 'Authorization: ' + ALTrim(FAuthorization) + #13#10;
  If ALTrim(fCacheControl) <> '' then result := result + 'Cache-Control: ' + ALTrim(FCacheControl) + #13#10;
  If ALTrim(fConnection) <> '' then result := result + 'Connection: ' + ALTrim(FConnection) + #13#10;
  If ALTrim(fContentEncoding) <> '' then result := result + 'Content-Encoding: ' + ALTrim(FContentEncoding) + #13#10;
  If ALTrim(fContentLanguage) <> '' then result := result + 'Content-Language: ' + ALTrim(FContentLanguage) + #13#10;
  If ALTrim(fContentLength) <> '' then result := result + 'Content-Length: ' + ALTrim(FContentLength) + #13#10;
  If ALTrim(fContentLocation) <> '' then result := result + 'Content-Location: ' + ALTrim(FContentLocation) + #13#10;
  If ALTrim(fContentMD5) <> '' then result := result + 'Content-MD5: ' + ALTrim(FContentMD5) + #13#10;
  If ALTrim(fContentRange) <> '' then result := result + 'Content-Range: ' + ALTrim(FContentRange) + #13#10;
  If ALTrim(fContentType) <> '' then result := result + 'Content-Type: ' + ALTrim(FContentType) + #13#10;
  If ALTrim(fDate) <> '' then result := result + 'Date: ' + ALTrim(FDate) + #13#10;
  If ALTrim(fExpect) <> '' then result := result + 'Expect: ' + ALTrim(FExpect) + #13#10;
  If ALTrim(fExpires) <> '' then result := result + 'Expires: ' + ALTrim(FExpires) + #13#10;
  If ALTrim(fFrom) <> '' then result := result + 'From: ' + ALTrim(FFrom) + #13#10;
  If ALTrim(fHost) <> '' then result := result + 'Host: ' + ALTrim(FHost) + #13#10;
  If ALTrim(fIfMatch) <> '' then result := result + 'If-Match: ' + ALTrim(FIfMatch) + #13#10;
  If ALTrim(fIfModifiedSince) <> '' then result := result + 'If-Modified-Since: ' + ALTrim(FIfModifiedSince) + #13#10;
  If ALTrim(fIfNoneMatch) <> '' then result := result + 'If-None-Match: ' + ALTrim(fIfNoneMatch) + #13#10;
  If ALTrim(fIfRange) <> '' then result := result + 'If-Range: ' + ALTrim(fIfRange) + #13#10;
  If ALTrim(fIfUnmodifiedSince) <> '' then result := result + 'If-Unmodified-Since: ' + ALTrim(fIfUnmodifiedSince) + #13#10;
  If ALTrim(fLastModified) <> '' then result := result + 'Last-Modified: ' + ALTrim(fLastModified) + #13#10;
  If ALTrim(fMaxForwards) <> '' then result := result + 'Max-Forwards: ' + ALTrim(fMaxForwards) + #13#10;
  If ALTrim(fPragma) <> '' then result := result + 'Pragma: ' + ALTrim(FPragma) + #13#10;
  If ALTrim(fProxyAuthorization) <> '' then result := result + 'Proxy-Authorization: ' + ALTrim(FProxyAuthorization) + #13#10;
  If ALTrim(fRange) <> '' then result := result + 'Range: ' + ALTrim(FRange) + #13#10;
  If ALTrim(fReferer) <> '' then result := result + 'Referer: ' + ALTrim(FReferer) + #13#10;
  If ALTrim(fTE) <> '' then result := result + 'TE: ' + ALTrim(fTE) + #13#10;
  If ALTrim(fTrailer) <> '' then result := result + 'Trailer: ' + ALTrim(FTrailer) + #13#10;
  If ALTrim(fTransferEncoding) <> '' then result := result + 'Transfer-Encoding: ' + ALTrim(FTransferEncoding) + #13#10;
  If ALTrim(fUpgrade) <> '' then result := result + 'Upgrade: ' + ALTrim(FUpgrade) + #13#10;
  If ALTrim(fUserAgent) <> '' then result := result + 'User-Agent: ' + ALTrim(FUserAgent) + #13#10;
  If ALTrim(fVia) <> '' then result := result + 'Via: ' + ALTrim(FVia) + #13#10;
  If ALTrim(fWarning) <> '' then result := result + 'Warning: ' + ALTrim(FWarning) + #13#10;
  For i := 0 to FCustomHeaders.count - 1 do
    if (ALTrim(FCustomHeaders.names[i]) <> '') and (ALTrim(FCustomHeaders.ValueFromIndex[i]) <> '') then
      result := result + FCustomHeaders.names[i] + ': ' + ALTrim(FCustomHeaders.ValueFromIndex[i]) + #13#10;
  If FCookies.Count > 0 then
    result := result + 'Cookie: ' + AlStringReplace(ALTrim(FCookies.text),#13#10,'; ', [rfReplaceAll]) + #13#10;
end;

{********************************************************************************}
procedure TALHTTPRequestHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);
Var aRawHeaderLst: TALStringList;
    j: integer;

  {---------------------------------------------}
  Function AlG001(aName: AnsiString): AnsiString;
  Var i: Integer;
  Begin
    I := aRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(aRawHeaderLst.ValueFromIndex[i]);
      aRawHeaderLst.Delete(i);
    end
    else result := '';
  end;

begin
  aRawHeaderLst := TALStringList.create;
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
      ALExtractHTTPFields([';'], [' '], [], PAnsiChar(aRawHeaderLst.ValueFromIndex[j]), Cookies, True);
      aRawHeaderLst.Delete(j);
    end;

    FCustomHeaders.clear;
    For j := 0 to aRawHeaderLst.count - 1 do
      If ALTrim(aRawHeaderLst[j]) <> '' then
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

{*****************************************************************}
procedure TALHTTPRequestHeader.SetCookies(const Value: TALStrings);
begin
  FCookies.Assign(Value);
end;

{***********************************************************************}
procedure TALHTTPRequestHeader.SetCustomHeaders(const Value: TALStrings);
begin
  FCustomHeaders.Assign(Value);
end;

{********************************************************}
function ALHTTPDecode(const AStr: AnsiString): AnsiString;
var Sp, Rp, Cp, Tp: PAnsiChar;
    int: integer;
    S: AnsiString;
begin
  SetLength(Result, Length(AStr));
  Sp := PAnsiChar(AStr);
  Rp := PAnsiChar(Result);
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
                 S := AnsiChar('$') + AnsiChar(Cp^) + AnsiChar(Sp^);
                 if ALTryStrToInt(s,int) then Rp^ := ansiChar(int)
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
  SetLength(Result, Rp - PAnsiChar(Result));
end;

{*************************************************************}
procedure ALHTTPEncodeParamNameValues(ParamValues: TALStrings);
var i: Integer;
    LPos: integer;
    LStr: AnsiString;
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
                              Content: PAnsiChar;
                              Strings: TALStrings;
                              StripQuotes: Boolean = False);
begin
  ALExtractHeaderFields(Separators, WhiteSpace, Quotes, Content, Strings, True, StripQuotes);
end;

{****************************************************************}
Function AlExtractShemeFromUrl(aUrl: AnsiString): TInternetScheme;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: AnsiString;
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
    if ALSameText(SchemeName,'http') then result := INTERNET_SCHEME_HTTP
    else if ALSameText(SchemeName,'https') then result := INTERNET_SCHEME_HTTPS
    else if ALSameText(SchemeName,'ftp') then result := INTERNET_SCHEME_FTP
    else result := INTERNET_SCHEME_UNKNOWN;
  end
  else result := INTERNET_SCHEME_UNKNOWN;
end;

{**************************************************************}
Function AlExtractHostNameFromUrl(aUrl: AnsiString): AnsiString;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: AnsiString;
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

{****************************************************************}
Function AlExtractDomainNameFromUrl(aUrl: AnsiString): AnsiString;
begin
  Result := AlExtractHostNameFromUrl(aUrl);
  while length(AlStringReplace(Result,
                               '.',
                               '',
                               [rfReplaceALL])) < length(result) - 1 do delete(Result, 1, ALpos('.',result));
end;

{**************************************************************}
Function  AlExtractUrlPathFromUrl(aUrl: AnsiString): AnsiString;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath,
    ExtraInfo: AnsiString;
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

{********************************************}
Function  AlInternetCrackUrl(aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 ExtraInfo: AnsiString;
                             var PortNumber: integer): Boolean;
Var P1: Integer;
    S1: AnsiString;
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
    P1 := ALLastDelimiter('@',aUrl);
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
      if not ALTryStrToInt(S1, PortNumber) then PortNumber := 0;
    end;
    if PortNumber = 0 then begin
      if ALSameText(SchemeName, 'http') then PortNumber := 80
      else if ALSameText(SchemeName, 'https') then PortNumber := 443
      else if ALSameText(SchemeName, 'ftp') then PortNumber := 21
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


{*******************************************}
Function AlInternetCrackUrl(aUrl: AnsiString;
                            Var SchemeName,
                                HostName,
                                UserName,
                                Password,
                                UrlPath,
                                Anchor: AnsiString; // not the anchor is never send to the server ! it's only used on client side
                            Query: TALStrings;
                            var PortNumber: integer): Boolean;
var aExtraInfo: AnsiString;
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

{***********************************************}
Function  AlInternetCrackUrl(var Url: AnsiString;  // if true return the relative url
                             var Anchor: AnsiString;
                             Query: TALStrings): Boolean;
Var SchemeName,
    HostName,
    UserName,
    Password,
    UrlPath: AnsiString;
    PortNumber: integer;
    tmpUrl: AnsiString;
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

{************************************************************************************}
Function AlRemoveAnchorFromUrl(aUrl: AnsiString; Var aAnchor: AnsiString): AnsiString;
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

{***********************************************************}
Function AlRemoveAnchorFromUrl(aUrl: AnsiString): AnsiString;
var aAnchor: AnsiString;
begin
  result := AlRemoveAnchorFromUrl(aUrl,aAnchor);
end;

{******************************************************************}
function AlCombineUrl(RelativeUrl, BaseUrl: AnsiString): AnsiString;
var Size: Dword;
    Buffer: AnsiString;
begin
  case AlExtractShemeFromUrl(RelativeUrl) of

    {relative path.. so try to combine the url}
    INTERNET_SCHEME_PARTIAL,
    INTERNET_SCHEME_UNKNOWN,
    INTERNET_SCHEME_DEFAULT: begin
                               Size := INTERNET_MAX_URL_LENGTH;
                               SetLength(Buffer, Size);
                               if InternetCombineUrlA(PAnsiChar(BaseUrl), PAnsiChar(RelativeUrl), @Buffer[1], size, ICU_BROWSER_MODE or ICU_no_encode) then Result := AlCopyStr(Buffer, 1, Size)
                               else result := RelativeUrl;
                             end;

    {not a relative path}
    else result := RelativeUrl;

  end;
end;

{*********************************}
Function  AlCombineUrl(RelativeUrl,
                       BaseUrl,
                       Anchor: AnsiString;
                       Query: TALStrings): AnsiString;
Var S1 : AnsiString;
    {$IF CompilerVersion >= 18.5}
    aBool: Boolean;
    {$IFEND}
begin
  if Query.Count > 0 then begin

    {$IF CompilerVersion >= 18.5}
      if Query.LineBreak = #13#10 then begin
        aBool := True;
        Query.LineBreak := '&';
      end
      else aBool := False;

      try

        S1 := ALTrim(Query.Text);
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
      S1 := AlStringReplace(ALTrim(Query.Text),#13#10,'&',[rfReplaceAll]);
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

{************************************************************************************}
Function ALTryIPV4StrToNumeric(aIPv4Str: ansiString; var aIPv4Num: Cardinal): Boolean;
Var P1, P2: Integer;
    I1, I2, I3, I4: integer;
Begin

  //----------
  if aIPv4Str = '' then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := 1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(aIPv4Str,P1,P2-P1), I1)) or
     (not (I1 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(aIPv4Str,P1,P2-P1), I2)) or
     (not (I2 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := AlPosEx('.',aIPv4Str, P1);
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(aIPv4Str,P1,P2-P1), I3)) or
     (not (I3 in [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  P1 := P2+1;
  P2 := length(aIPv4Str) + 1;
  if (P2 <= P1) or
     (not ALTryStrToInt(AlCopyStr(aIPv4Str,P1,P2-P1), I4)) or
     (not (I4 in  [0..255])) then begin
    Result := False;
    Exit;
  end;

  //----------
  Result := True;
  aIPv4Num := (I1*256*256*256) + (I2*256*256) +  (I3*256) + (I4);

End;

{*******************************************************}
Function ALIPV4StrToNumeric(aIPv4: ansiString): Cardinal;
Begin
  if not ALTryIPV4StrToNumeric(aIPv4, Result) then Raise EALException.Create('Bad IPv4 string: ' + aIPv4);
End;

{*******************************************************}
Function ALNumericToIPv4Str(aIPv4: Cardinal): ansiString;
Var S1, S2, S3, S4: ansiString;
Begin

  S1 := ALIntToStr(( aIPv4 div (256*256*256) ) mod 256);
  S2 := ALIntToStr(( aIPv4 div (256*256)     ) mod 256);
  S3 := ALIntToStr(( aIPv4 div (256)         ) mod 256);
  S4 := ALIntToStr(( aIPv4                   ) mod 256);

  Result := S1 + '.' + S2 + '.' + S3 + '.' + S4;

End;

{*********************************}
Function ALZeroIpV6: TALIPv6Binary;
begin
  FillChar(result, 16, #0);
end;

{****************************************************************************************}
Function ALTryIPV6StrToBinary(aIPv6Str: ansiString; var aIPv6Bin: TALIPv6Binary): Boolean;
var aLstIpv6Part: TALStringList;
    S1: ansiString;
    P1: integer;
    i: integer;
begin

  //----------
  if aIPv6Str = '' then begin
    Result := False;
    Exit;
  end;

  // http://msdn.microsoft.com/en-us/library/aa921042.aspx
  // Zero compression can be used only once in an address, which enables you to determine
  // the number of 0 bits represented by each instance of a double-colon (::).
  P1 := Alpos('::',aIPv6Str);
  if (P1 > 0) and (AlPosEx('::', aIPv6Str, P1+1) > 0) then begin
    result := False;
    exit;
  end
  else if P1 = 1 then delete(aIPv6Str,1,1);  // with the exemple below, we have one extra ":"
                                             // ::D3:0000:2F3B:02AA:00FF:FE28:9C5A => 0:D3:0000:2F3B:02AA:00FF:FE28:9C5A
                                             // but with the exemple below ok because the last #13#10 will be trim when we will do
                                             // aLstIpv6Part.Text := AlStringReplace(aIPv6Str, ':', #13#10, [rfReplaceALL]);
                                             // 21DA:D3:0000:2F3B:02AA:00FF:FE28:: => 21DA:D3:0000:2F3B:02AA:00FF:FE28:0


  //----------
  aLstIpv6Part := TALStringList.Create;
  try

    //----------
    aLstIpv6Part.Text := AlStringReplace(aIPv6Str, ':', #13#10, [rfReplaceALL]);

    //----------
    if (aLstIpv6Part.Count > 8) then begin
      Result := False;
      Exit;
    end;

    // http://msdn.microsoft.com/en-us/library/aa921042.aspx
    // IPv6 representation can be further simplified by removing the leading zeros within each 16-bit block.
    // However, each block must have at least a single digit. The following example shows the address without
    // the leading zeros
    // 21DA:D3:0:2F3B:2AA:FF:FE28:9C5A => 21DA:D3:0000:2F3B:02AA:00FF:FE28:9C5A
    //
    // Some types of addresses contain long sequences of zeros. In IPv6 addresses, a contiguous sequence of
    // 16-bit blocks set to 0 in the colon-hexadecimal format can be compressed to :: (known as double-colon).
    // The following list shows examples of compressing zeros
    // FF02::2 => FF02:0:0:0:0:0:0:2
    I := 0;
    while i <= 7 do begin
      if i >= aLstIpv6Part.Count then begin
        result := False;
        Exit;
      end
      else if (aLstIpv6Part[i] = '') then begin
        aLstIpv6Part[i] := '0000';
        while aLstIpv6Part.Count < 8 do begin
          aLstIpv6Part.Insert(i,'0000');
          inc(i);
        end;
      end
      else begin
        if length(aLstIpv6Part[i]) > 4 then begin
          result := False;
          Exit;
        end
        else if length(aLstIpv6Part[i]) < 4 then begin
          SetLength(S1,4-length(aLstIpv6Part[i]));
          FillChar(S1[1], length(S1), '0');
          aLstIpv6Part[i] := S1 + aLstIpv6Part[i];
        end;
      end;
      inc(i);
    end;

    //----------
    for I := 0 to aLstIpv6Part.Count - 1 do begin

      S1 := AlUpperCase(AlCopyStr(aLstIpv6Part[i], 1, 2));
      if (not ALTryStrToInt('$' + S1, P1)) or
         (not (P1 in [0..255])) then begin
        Result := False;
        exit;
      end;
      aIPv6Bin[(i*2) + 1] := AnsiChar(P1);

      S1 := AlUpperCase(AlCopyStr(aLstIpv6Part[i], 3, 2));
      if (not ALTryStrToInt('$' + S1, P1)) or
         (not (P1 in [0..255])) then begin
        Result := False;
        exit;
      end;
      aIPv6Bin[(i*2) + 2] := AnsiChar(P1);

    end;

    Result := True;

  finally
    aLstIpv6Part.Free;
  end;

end;

{***********************************************************}
Function ALIPV6StrTobinary(aIPv6: ansiString): TALIPv6Binary;
Begin
  if not ALTryIPv6StrToBinary(aIPv6, Result) then Raise EALException.Create('Bad IPv6 string: ' + aIPv6);
End;

{***********************************************************}
Function ALBinaryToIPv6Str(aIPv6: TALIPv6Binary): ansiString;
Begin

  Result := ALIntToHex(ord(aIPv6[1]), 2)  + ALIntToHex(ord(aIPv6[2]), 2)   + ':' +
            ALIntToHex(ord(aIPv6[3]), 2)  + ALIntToHex(ord(aIPv6[4]), 2)   + ':' +
            ALIntToHex(ord(aIPv6[5]), 2)  + ALIntToHex(ord(aIPv6[6]), 2)   + ':' +
            ALIntToHex(ord(aIPv6[7]), 2)  + ALIntToHex(ord(aIPv6[8]), 2)   + ':' +
            ALIntToHex(ord(aIPv6[9]), 2)  + ALIntToHex(ord(aIPv6[10]), 2)   + ':' +
            ALIntToHex(ord(aIPv6[11]), 2) + ALIntToHex(ord(aIPv6[12]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[13]), 2) + ALIntToHex(ord(aIPv6[14]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[15]), 2) + ALIntToHex(ord(aIPv6[16]), 2);

End;

{**************************************************************************}
Function ALBinaryStrToIPv6Binary(aIPV6BinaryStr: ansiString): TALIPv6Binary;
Begin

  if length(aIPV6BinaryStr) <> 16 then Raise EALException.Create('Bad IPv6 binary string');
  result[1] := aIPV6BinaryStr[1];
  result[2] := aIPV6BinaryStr[2];
  result[3] := aIPV6BinaryStr[3];
  result[4] := aIPV6BinaryStr[4];
  result[5] := aIPV6BinaryStr[5];
  result[6] := aIPV6BinaryStr[6];
  result[7] := aIPV6BinaryStr[7];
  result[8] := aIPV6BinaryStr[8];
  result[9] := aIPV6BinaryStr[9];
  result[10] := aIPV6BinaryStr[10];
  result[11] := aIPV6BinaryStr[11];
  result[12] := aIPV6BinaryStr[12];
  result[13] := aIPV6BinaryStr[13];
  result[14] := aIPV6BinaryStr[14];
  result[15] := aIPV6BinaryStr[15];
  result[16] := aIPV6BinaryStr[16];

End;

end.
