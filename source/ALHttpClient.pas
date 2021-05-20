{*******************************************************************************
TALHttpClient is a ancestor of class like TALWinInetHttpClient or
TALWinHttpClient

http://www.w3.org/TR/REC-html40/interact/forms.html#h-17.1
http://www.ietf.org/rfc/rfc1867.txt
http://www.ietf.org/rfc/rfc2388.txt
http://www.w3.org/MarkUp/html-spec/html-spec_8.html
http://www.cs.tut.fi/~jkorpela/forms/methods.html
http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
http://wp.netscape.com/newsref/std/cookie_spec.html
*******************************************************************************}

unit ALHttpClient;

interface

{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if Web.HTTPApp was not updated and adjust the IFDEF'}
{$IFEND}

uses
  System.SysUtils,
  System.Classes,
  {$IFDEF MSWINDOWS}
  Winapi.Wininet,
  {$ENDIF}
  ALStringList,
  ALMultiPartParser;

type

  {-- onchange Event that specify the property index that is just changed --}
  TALHTTPPropertyChangeEvent = procedure(sender: Tobject; Const PropertyIndex: Integer) of object;

  {--protocol version--}
  TALHTTPProtocolVersion = (HTTPpv_1_0, HTTPpv_1_1);

  {--Request method--}
  TALHTTPMethod = (HTTPmt_Get,
                   HTTPmt_Post,
                   HTTPmt_Head,
                   HTTPmt_Trace,
                   HTTPmt_Put,
                   HTTPmt_Delete,
                   HTTPmt_Options);

  {--Request header--}
  TALHTTPRequestHeader = Class(TObject)
  Private
    fHeaders: TALStrings;
    FCookies: TALStrings;
    function GetHeaderValueByPropertyIndex(const Index: Integer): ansiString;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
    Function GetRawHeaderText: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure setHeaderValue(const aName: ansiString; const aValue: ansiString);
    Property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
    property Accept: AnsiString index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept: audio/*; q=0.2, audio/basic}
    property AcceptCharSet: AnsiString index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
    property AcceptEncoding: AnsiString index 2 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0}
    property AcceptLanguage: AnsiString index 3 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Language: da, en-gb;q=0.8, en;q=0.7}
    property Allow: AnsiString index 4 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property Authorization: AnsiString index 5 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Authorization: BASIC d2VibWFzdGVyOnpycW1hNHY=}
    property CacheControl: AnsiString index 6 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: AnsiString index 7 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: AnsiString index 8 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: AnsiString index 9 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: AnsiString index 10 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: AnsiString index 11 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: AnsiString index 12 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: AnsiString index 13 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: AnsiString index 14 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property Date: AnsiString index 15 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property Expect: AnsiString index 16 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expect: 100-continue}
    property Expires: AnsiString index 17 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property From: AnsiString index 18 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {From: webmaster@w3.org}
    property Host: AnsiString index 19 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Host: www.w3.org}
    property IfMatch: AnsiString index 20 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Match: entity_tag001}
    property IfModifiedSince: AnsiString index 21 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property IfNoneMatch: AnsiString index 22 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-None-Match: entity_tag001}
    property IfRange: AnsiString index 23 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Range: entity_tag001}
    property IfUnmodifiedSince: AnsiString index 24 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property LastModified: AnsiString index 25 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property MaxForwards: AnsiString index 26 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Max-Forwards: 3}
    property Pragma: AnsiString index 27 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthorization: AnsiString index 28 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Proxy-Authorization: [credentials]}
    property Range: AnsiString index 29 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Range: bytes=100-599}
    property Referer: AnsiString index 30 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    property TE: AnsiString index 31 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {TE: trailers, deflate;q=0.5}
    property Trailer: AnsiString index 32 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property TransferEncoding: AnsiString index 33 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: AnsiString index 34 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property UserAgent: AnsiString index 35 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    property Via: AnsiString index 36 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: AnsiString index 37 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property CustomHeaders: TALStrings read FHeaders;
    property Cookies: TALStrings read FCookies;
  end;

  {-----------------------}
  TALNameValuePair = record
    Name: ansistring;
    Value: ansistring;
    constructor Create(const AName, AValue: ansistring);
  end;
  TALNameValueArray = Array of TALNameValuePair;

  {--TALHTTPCookie--}
  TALHTTPCookie = class(TCollectionItem)
  private
    FName: AnsiString;
    FValue: AnsiString;
    FPath: AnsiString;
    FDomain: AnsiString;
    FExpires: TDateTime;
    FSameSite: AnsiString;
    FSecure: Boolean;
    FHttpOnly: Boolean;
  protected
    function GetHeaderValue: AnsiString;
    procedure SetHeaderValue(Const aValue: AnsiString);
  public
    constructor Create(Collection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
    property Name: AnsiString read FName write FName;
    property Value: AnsiString read FValue write FValue;
    property Path: AnsiString read FPath write FPath;
    property Domain: AnsiString read FDomain write FDomain;
    property Expires: TDateTime read FExpires write FExpires;
    property SameSite: AnsiString read FSameSite write FSameSite;
    property Secure: Boolean read FSecure write FSecure;
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    property HeaderValue: AnsiString read GetHeaderValue write SetHeaderValue;
  end;

  {--TALCookieCollection--}
  TALHTTPCookieCollection = class(TCollection)
  private
  protected
    function GetCookie(Index: Integer): TALHTTPCookie;
    procedure SetCookie(Index: Integer; Cookie: TALHTTPCookie);
  public
    function Add: TALHTTPCookie; overload;
    function Add(const Name: AnsiString;
                 const Value: AnsiString;
                 const Path: AnsiString;
                 const Domain: AnsiString;
                 const Expires: int64; // unixDateTime format
                 const SameSite: AnsiString;
                 const Secure: Boolean;
                 const HttpOnly: Boolean): TALHTTPCookie; overload;
    property Items[Index: Integer]: TALHTTPCookie read GetCookie write SetCookie; default;
  end;

  {--Response header--}
  TALHTTPResponseHeader = Class(TObject)
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
    FCookies: TALHTTPCookieCollection;
    FStatusCode: AnsiString;
    FHttpProtocolVersion: AnsiString;
    FReasonPhrase: AnsiString;
    procedure SetRawHeaderText(const aRawHeaderText: AnsiString);
    Function GetRawHeaderText: AnsiString;
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
    Property Cookies: TALHTTPCookieCollection read FCookies;
    property StatusCode: AnsiString read FStatusCode;
    property HttpProtocolVersion: AnsiString read FHttpProtocolVersion;
    Property ReasonPhrase: AnsiString read FReasonPhrase;
    property RawHeaderText: AnsiString read GetRawHeaderText write setRawHeaderText;
  end;

  {---------------------------------------}
  EALHTTPClientException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(const Msg: AnsiString; SCode: Integer = 0);
    constructor CreateFmt(const Msg: AnsiString; const Args: array of const; SCode: Integer = 0);
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  {---------------------------------------}
  TALHTTPClientProxyParams = Class(Tobject)
  Private
    FProxyBypass: AnsiString;
    FproxyServer: AnsiString;
    FProxyUserName: AnsiString;
    FProxyPassword: AnsiString;
    FproxyPort: integer;
    FOnChange: TALHTTPPropertyChangeEvent;
    procedure SetProxyBypass(const Value: AnsiString);
    procedure SetProxyPassword(const Value: AnsiString);
    procedure SetProxyPort(const Value: integer);
    procedure SetProxyServer(const Value: AnsiString);
    procedure SetProxyUserName(const Value: AnsiString);
    Procedure DoChange(propertyIndex: Integer);
  public
    constructor Create; virtual;
    procedure Clear;
    Property ProxyBypass: AnsiString read FProxyBypass write SetProxyBypass; //index 0
    property ProxyServer: AnsiString read FProxyServer write SetProxyServer; //index 1
    property ProxyPort: integer read FProxyPort write SetProxyPort default 0; //index 2
    property ProxyUserName: AnsiString read FProxyUserName write SetProxyUserName; //index 3
    property ProxyPassword: AnsiString read FProxyPassword write SetProxyPassword; //index 4
    property OnChange: TALHTTPPropertyChangeEvent read FOnChange write FOnChange;
  end;

  {--------------------------------------------------------------------------------------------------}
  TAlHTTPClientRedirectEvent         = procedure(sender: Tobject; const NewURL: AnsiString) of object;
  TALHTTPClientUploadProgressEvent   = procedure(sender: Tobject; Sent: Integer; Total: Integer) of object;
  TALHTTPClientDownloadProgressEvent = procedure(sender: Tobject; Read: Integer; Total: Integer) of object;

  {----------------------------}
  TALHTTPClient = class(TObject)
  private
    FProxyParams: TALHTTPClientProxyParams;
    FRequestHeader: TALHTTPRequestHeader;
    FProtocolVersion: TALHTTPProtocolVersion;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FConnectTimeout: Integer;
    FSendTimeout: Integer;
    FReceiveTimeout: Integer;
    FOnUploadProgress: TALHTTPClientUploadProgressEvent;
    FOnDownloadProgress: TALHTTPClientDownloadProgressEvent;
    FOnRedirect: TAlHTTPClientRedirectEvent;
    FUploadBufferSize: cardinal;
  protected
    procedure SetUsername(const NameValue: AnsiString); virtual;
    procedure SetPassword(const PasswordValue: AnsiString); virtual;
    procedure OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer); virtual;
    procedure SetOnRedirect(const Value: TAlHTTPClientRedirectEvent); virtual;
    procedure Execute(const aUrl:AnsiString;
                      const aRequestMethod: TALHTTPMethod;
                      const aRequestDataStream: TStream;
                      const ARequestHeaderValues: TALNameValueArray;
                      const aResponseContent: TStream;
                      const aResponseHeader: TALHTTPResponseHeader); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //-----
    Procedure Get(const aUrl:AnsiString;
                  const aRequestFields: TALStrings;
                  const aResponseContent: TStream;
                  const aResponseHeader: TALHTTPResponseHeader;
                  const ARequestHeaderValues: TALNameValueArray = nil;
                  Const aEncodeRequestFields: Boolean=True); overload;
    Procedure Get(const aUrl:AnsiString;
                  const aResponseContent: TStream;
                  const aResponseHeader: TALHTTPResponseHeader;
                  const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Function  Get(const aUrl:AnsiString;
                  const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    Function  Get(const aUrl:AnsiString;
                  const aRequestFields: TALStrings;
                  const ARequestHeaderValues: TALNameValueArray = nil;
                  Const aEncodeRequestFields: Boolean=True): AnsiString; overload;
    //-----
    Procedure Post(const aUrl:AnsiString;
                   const aResponseContent: TStream;
                   const aResponseHeader: TALHTTPResponseHeader;
                   const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Procedure Post(const aUrl:AnsiString;
                   const aPostDataStream: TStream;
                   const aResponseContent: TStream;
                   const aResponseHeader: TALHTTPResponseHeader;
                   const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Function  Post(const aUrl:AnsiString;
                   const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    Function  Post(const aUrl:AnsiString;
                   const aPostDataStream: TStream;
                   const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    Procedure PostUrlEncoded(const aUrl:AnsiString;
                             const aRequestFields: TALStrings;
                             const aResponseContent: TStream;
                             const aResponseHeader: TALHTTPResponseHeader;
                             const ARequestHeaderValues: TALNameValueArray = nil;
                             Const aEncodeRequestFields: Boolean=True); overload;
    Function  PostUrlEncoded(const aUrl:AnsiString;
                             const aRequestFields: TALStrings;
                             const ARequestHeaderValues: TALNameValueArray = nil;
                             Const aEncodeRequestFields: Boolean=True): AnsiString; overload;
    //-----
    Procedure PostMultipartFormData(const aUrl:AnsiString;
                                    const aRequestFields: TALStrings;
                                    const aRequestFiles: TALMultiPartFormDataContents;
                                    const aResponseContent: TStream;
                                    const aResponseHeader: TALHTTPResponseHeader;
                                    const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Function  PostMultiPartFormData(const aUrl:AnsiString;
                                    const aRequestFields: TALStrings;
                                    const aRequestFiles: TALMultiPartFormDataContents;
                                    const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    Procedure Head(const aUrl:AnsiString;
                   const aResponseContent: TStream;
                   const aResponseHeader: TALHTTPResponseHeader;
                   const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Function  Head(const aUrl:AnsiString;
                   const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    Procedure Trace(const aUrl:AnsiString;
                    const aResponseContent: TStream;
                    const aResponseHeader: TALHTTPResponseHeader;
                    const ARequestHeaderValues: TALNameValueArray = nil); overload;
    Function  trace(const aUrl:AnsiString;
                    const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    Procedure Put(const aUrl:AnsiString;
                  const aPutDataStream: TStream;
                  const aResponseContent: TStream;
                  const aResponseHeader: TALHTTPResponseHeader;
                  const ARequestHeaderValues: TALNameValueArray = nil); overload;
    function  Put(const aURL: Ansistring;
                  const aPutDataStream: TStream;
                  const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    procedure Delete(const aUrl:AnsiString;
                     const aResponseContent: TStream;
                     const aResponseHeader: TALHTTPResponseHeader;
                     const ARequestHeaderValues: TALNameValueArray = nil); overload;
    function  Delete(const aURL: Ansistring;
                     const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    procedure Options(const aUrl:AnsiString;
                      const aResponseContent: TStream;
                      const aResponseHeader: TALHTTPResponseHeader;
                      const ARequestHeaderValues: TALNameValueArray = nil); overload;
    function  Options(const aURL: Ansistring;
                      const ARequestHeaderValues: TALNameValueArray = nil): AnsiString; overload;
    //-----
    property  ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout default 0;
    property  SendTimeout: Integer read FSendTimeout write FSendTimeout default 0;
    property  ReceiveTimeout: Integer read FReceiveTimeout write FReceiveTimeout default 0;
    property  UploadBufferSize: cardinal read FUploadBufferSize write FUploadBufferSize default $8000;
    property  ProxyParams: TALHTTPClientProxyParams read FProxyParams;
    property  RequestHeader: TALHTTPRequestHeader read FRequestHeader;
    Property  ProtocolVersion: TALHTTPProtocolVersion read FProtocolVersion write FProtocolVersion default HTTPpv_1_1;
    property  UserName: AnsiString read FUserName write SetUserName;
    property  Password: AnsiString read FPassword write SetPassword;
    property  OnUploadProgress: TALHTTPClientUploadProgressEvent read FOnUploadProgress write FOnUploadProgress;
    property  OnDownloadProgress: TALHTTPClientDownloadProgressEvent read FonDownloadProgress write FonDownloadProgress;
    property  OnRedirect: TAlHTTPClientRedirectEvent read FOnRedirect write SetOnRedirect;
  end;

{Http Function}
procedure ALHTTPEncodeParamNameValues(const ParamValues: TALStrings);
procedure ALExtractHTTPFields(Separators,
                              WhiteSpace,
                              Quotes: TSysCharSet;
                              Content: PAnsiChar;
                              Strings: TALStrings;
                              StripQuotes: Boolean = False);
Function  AlRemoveShemeFromUrl(const aUrl: AnsiString): ansiString;
{$IFDEF MSWINDOWS}
Function  AlExtractShemeFromUrl(const aUrl: AnsiString): TInternetScheme;
{$ENDIF}
Function  AlExtractHostNameFromUrl(const aUrl: AnsiString): AnsiString;
Function  AlExtractDomainNameFromUrl(const aUrl: AnsiString): AnsiString;
Function  AlExtractUrlPathFromUrl(const aUrl: AnsiString): AnsiString;
Function  AlInternetCrackUrl(const aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 ExtraInfo: AnsiString;
                             var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(const aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 Anchor: AnsiString; // not the anchor is never send to the server ! it's only used on client side
                             const Query: TALStrings;
                             var PortNumber: integer): Boolean; overload;
Function  AlInternetCrackUrl(var Url: AnsiString; // if true return UrlPath
                             var Anchor: AnsiString;
                             const Query: TALStrings): Boolean; overload;
Function  AlRemoveAnchorFromUrl(aUrl: AnsiString; Var aAnchor: AnsiString): AnsiString; overload;
Function  AlRemoveAnchorFromUrl(const aUrl: AnsiString): AnsiString; overload;
{$IFDEF MSWINDOWS}
function  AlCombineUrl(const RelativeUrl, BaseUrl: AnsiString): AnsiString; overload;
Function  AlCombineUrl(const RelativeUrl,
                             BaseUrl,
                             Anchor: AnsiString;
                       const Query: TALStrings): AnsiString; overload;
{$ENDIF}

const
  AlRfc822DayOfWeekNames: array[1..7] of AnsiString = ('Sun',
                                                       'Mon',
                                                       'Tue',
                                                       'Wed',
                                                       'Thu',
                                                       'Fri',
                                                       'Sat');

  ALRfc822MonthOfTheYearNames: array[1..12] of AnsiString = ('Jan',
                                                             'Feb',
                                                             'Mar',
                                                             'Apr',
                                                             'May',
                                                             'Jun',
                                                             'Jul',
                                                             'Aug',
                                                             'Sep',
                                                             'Oct',
                                                             'Nov',
                                                             'Dec');

function ALGmtDateTimeToRfc822Str(const aValue: TDateTime): AnsiString;
{$IFDEF MSWINDOWS}
function ALDateTimeToRfc822Str(const aValue: TDateTime): AnsiString;
{$ENDIF}
function ALTryRfc822StrToGMTDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
function ALRfc822StrToGMTDateTime(const s: AnsiString): TDateTime;

function ALTryIPV4StrToNumeric(const aIPv4Str: AnsiString; var aIPv4Num: Cardinal): Boolean;
function ALIPV4StrToNumeric(const aIPv4: AnsiString): Cardinal;
function ALNumericToIPv4Str(const aIPv4: Cardinal): ansiString;
function ALIPv4EndOfRange(const aStartIPv4: Cardinal; aMaskLength: integer): Cardinal;

type
  TALIPv6Binary = array[1..16] of AnsiChar;

function ALZeroIpV6: TALIPv6Binary;
Function ALIsValidIPv6BinaryStr(const aIPV6BinaryStr: ansiString): boolean;
function ALTryIPV6StrToBinary(aIPv6Str: ansiString; var aIPv6Bin: TALIPv6Binary): Boolean;
function ALIPV6StrTobinary(const aIPv6: AnsiString): TALIPv6Binary;
function ALBinaryToIPv6Str(const aIPv6: TALIPv6Binary): ansiString;
function ALBinaryStrToIPv6Binary(const aIPV6BinaryStr: ansiString): TALIPv6Binary;
function ALBinaryStrToIPv6Str(const aIPV6BinaryStr: ansiString): ansiString;
function ALIPv6EndOfRange(const aStartIPv6: TALIPv6Binary; aMaskLength: integer): TALIPv6Binary;
procedure ALIPv6SplitParts(const aIPv6: TALIPv6Binary;
                           var aLowestPart: UInt64;
                           var aHigestPart: UInt64);

Const
  cALHTTPCLient_MsgInvalidURL         = 'Invalid url ''%s'' - only supports ''http'' and ''https'' schemes';
  cALHTTPCLient_MsgInvalidHTTPRequest = 'Invalid HTTP Request: Length is 0';
  cALHTTPCLient_MsgEmptyURL           = 'Empty URL';

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  Web.HttpApp,
  System.DateUtils,
  System.SysConst,
  System.Math,
  system.AnsiStrings,
  AlCommon,
  ALString;

{***********************************************************************************}
function AlStringFetch(var AInput: AnsiString; const ADelim: AnsiString): AnsiString;
var
  LPos: Integer;
begin
  LPos := AlPos(ADelim, AInput);
  if LPos <= 0 then begin
    Result := AInput;
    AInput := '';
  end
  else begin
    Result := AlCopyStr(AInput, 1, LPos - 1);
    AInput := AlCopyStr(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;

{********************************************************}
constructor TALHTTPCookie.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FValue := '';
  FPath := '';
  FDomain := '';
  FExpires := ALNullDate;
  FSameSite := '';
  FSecure := False;
  FHttpOnly := false;
end;

{**************************************************}
procedure TALHTTPCookie.AssignTo(Dest: TPersistent);
begin
  if Dest is TALHTTPCookie then
    with TALHTTPCookie(Dest) do begin
      Name := Self.FName;
      Value := Self.FValue;
      Path := Self.FPath;
      Domain := Self.FDomain;
      Expires := Self.FExpires;
      SameSite := Self.FSameSite;
      Secure := Self.FSecure;
      HttpOnly := Self.FHttpOnly;
    end
    else inherited AssignTo(Dest);
end;

{************************************************}
function TALHTTPCookie.GetHeaderValue: AnsiString;
begin
  Result := ALFormat('%s=%s; ', [ALHTTPEncode(FName), ALHTTPEncode(FValue)]);
  if Domain <> '' then Result := Result + ALFormat('domain=%s; ', [Domain]);
  if Path <> '' then Result := Result + ALFormat('path=%s; ', [Path]);
  if Expires <> ALNullDate then
    Result := Result + ALFormat(ALFormatDateTime('"expires=%s, "dd"-%s-"yyyy" "hh":"nn":"ss" GMT; "',
                                                 Expires,
                                                 ALDefaultFormatSettings),
                                [AlRfc822DayOfWeekNames[DayOfWeek(Expires)],
                                 ALRfc822MonthOfTheYearNames[MonthOf(Expires)]]);
  if SameSite <> '' then Result := Result + ALFormat('SameSite=%s; ', [SameSite]);
  if Secure then Result := Result + 'secure; ';
  if HttpOnly then Result := Result + 'httponly';
  if ALCopyStr(Result, Length(Result) - 1, MaxInt) = '; ' then SetLength(Result, Length(Result) - 2);
end;

{*****************}
//exemple of value:
// LSID=DQAAAK…Eaem_vYg; Domain=docs.foo.com; Path=/accounts; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
// HSID=AYQEVn….DKrdst; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; HttpOnly
// SSID=Ap4P….GTEq; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
procedure TALHTTPCookie.SetHeaderValue(Const aValue: AnsiString);
Var LCookieProp: TALStringList;
    LCookieStr: AnsiString;
begin

  FName:= '';
  FValue:= '';
  FPath:= '';
  FDomain:= '';
  FExpires:= ALNullDate;
  FSameSite := '';
  FSecure:= False;
  FHttpOnly := False;

  LCookieProp := TALStringList.Create;
  try

    LCookieStr := ALTrim(AValue);
    while LCookieStr <> '' do
      LCookieProp.Add(ALTrim(AlStringFetch(LCookieStr, ';')));
    if LCookieProp.Count = 0 then exit;

    // Exemple of aCookieProp content :
    //   LSID=DQAAAK…Eaem_vYg
    //   Domain=docs.foo.com
    //   Path=/accounts
    //   Expires=Wed, 13-Jan-2021 22:23:01 GMT
    //   SameSite=None
    //   Secure
    //   HttpOnly

    FName := LCookieProp.Names[0];
    FValue := LCookieProp.ValueFromIndex[0];
    FPath := LCookieProp.values['PATH'];
    if FPath = '' then FPath := '/';
    FDomain := LCookieProp.values['DOMAIN'];
    if not ALTryRfc822StrToGmtDateTime(LCookieProp.values['EXPIRES'], FExpires) then FExpires := ALNullDate;
    FSameSite := LCookieProp.values['SAMESITE'];
    FSecure := LCookieProp.IndexOf('SECURE') <> -1;
    FHttpOnly := LCookieProp.IndexOf('HTTPONLY') <> -1;

  finally
    AlFreeAndNil(LCookieProp);
  end;

end;

{**************************************************}
function TALHTTPCookieCollection.Add: TALHTTPCookie;
begin
  Result := TALHTTPCookie(inherited Add);
end;

{**********************************************************}
function TALHTTPCookieCollection.Add(const Name: AnsiString;
                                     const Value: AnsiString;
                                     const Path: AnsiString;
                                     const Domain: AnsiString;
                                     const Expires: int64; // unixDateTime format
                                     const SameSite: AnsiString;
                                     const Secure: Boolean;
                                     const HttpOnly: boolean): TALHTTPCookie;
begin
  result := TALHTTPCookie(inherited Add);
  result.Name := Name;
  result.Value := Value;
  result.Path := Path;
  result.Domain := Domain;
  result.Expires := unixtoDateTime(Expires);
  result.SameSite := SameSite;
  result.Secure := Secure;
  result.HttpOnly := HttpOnly;
end;

{************************************************************************}
function TALHTTPCookieCollection.GetCookie(Index: Integer): TALHTTPCookie;
begin
  Result := TALHTTPCookie(inherited Items[Index]);
end;

{*********************************************************************************}
procedure TALHTTPCookieCollection.SetCookie(Index: Integer; Cookie: TALHTTPCookie);
begin
  Items[Index].Assign(Cookie);
end;

{***************************************}
constructor TALHTTPResponseHeader.Create;
begin
  inherited;
  FCustomHeaders := TALStringList.create;
  FCustomHeaders.NameValueSeparator := ':';
  FCookies := TALHTTPCookieCollection.Create(TALHTTPCookie);
  clear;
end;

{***************************************}
destructor TALHTTPResponseHeader.Destroy;
begin
  AlFreeAndNil(FCustomHeaders);
  AlFreeAndNil(FCookies);
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

Var LRawHeaderLst: TALStringList;
    J: integer;
    LStatusLine: AnsiString;

  {-------------------------------------------------------------}
  Function internalGetValue(const aName: AnsiString): AnsiString;
  Var I: Integer;
  Begin
    I := LRawHeaderLst.IndexOfName(aName);
    If I >= 0 then Begin
      result := ALTrim(LRawHeaderLst.ValueFromIndex[I]);
      LRawHeaderLst.Delete(I);
    end
    else result := '';
  end;

begin
  LRawHeaderLst := TALStringList.create;
  try
    LRawHeaderLst.NameValueSeparator := ':';
    LRawHeaderLst.Text := aRawHeaderText;

    FAcceptRanges := internalGetValue('Accept-Ranges');
    FAge:= internalGetValue('Age');
    FAllow := internalGetValue('Allow');
    FCacheControl := internalGetValue('Cache-Control');
    FConnection := internalGetValue('Connection');
    FContentEncoding := internalGetValue('Content-Encoding');
    FContentLanguage := internalGetValue('Content-Language');
    FContentLength := internalGetValue('Content-Length');
    FContentLocation := internalGetValue('Content-Location');
    FContentMD5 := internalGetValue('Content-MD5');
    FContentRange := internalGetValue('Content-Range');
    FContentType := internalGetValue('Content-Type');
    FDate := internalGetValue('Date');
    FETag := internalGetValue('ETag');
    FExpires := internalGetValue('Expires');
    FLastModified := internalGetValue('Last-Modified');
    FLocation := internalGetValue('Location');
    FPragma := internalGetValue('Pragma');
    FProxyAuthenticate := internalGetValue('Proxy-Authenticate');
    FRetryAfter := internalGetValue('Retry-After');
    FServer := internalGetValue('Server');
    FTrailer := internalGetValue('Trailer');
    FTransferEncoding := internalGetValue('Transfer-Encoding');
    FUpgrade := internalGetValue('Upgrade');
    FVary := internalGetValue('Vary');
    FVia := internalGetValue('Via');
    FWarning := internalGetValue('Warning');
    FWWWAuthenticate := internalGetValue('WWW-Authenticate');

    FCookies.clear;
    J := LRawHeaderLst.IndexOfName('Set-Cookie');
    While J >= 0 do begin
      If ALTrim(LRawHeaderLst.ValueFromIndex[J]) <> '' then
        Cookies.Add.HeaderValue := ALTrim(LRawHeaderLst.ValueFromIndex[J]);
      LRawHeaderLst.Delete(J);
      J := LRawHeaderLst.IndexOfName('Set-Cookie');
    end;

    If LRawHeaderLst.Count > 0 then begin
      LStatusLine := LRawHeaderLst[0]; //HTTP/1.1 200 OK | 200 OK | status: 200 OK
      FHttpProtocolVersion := ALTrim(AlStringFetch(LStatusLine,' '));
      If AlIsInteger(FHttpProtocolVersion) then begin
        FStatusCode := FHttpProtocolVersion;
        FHttpProtocolVersion := '';
      end
      else FStatusCode := ALTrim(AlStringFetch(LStatusLine,' '));
      FReasonPhrase := ALTrim(LStatusLine);

      If not AlIsInteger(FStatusCode) then begin
        FHttpProtocolVersion := '';
        LStatusLine := internalGetValue('Status');
        FStatusCode := ALTrim(AlStringFetch(LStatusLine,' '));
        FReasonPhrase := ALTrim(AlStringFetch(LStatusLine,' '));
      end
      else LRawHeaderLst.Delete(0);
    end
    else begin
      FStatusCode := '';
      FHttpProtocolVersion := '';
      FReasonPhrase := '';
    end;

    FCustomHeaders.clear;
    For J := 0 to LRawHeaderLst.count - 1 do
      If ALTrim(LRawHeaderLst[J]) <> '' then
        FCustomHeaders.Add(LRawHeaderLst[J]);

    FRawHeaderText := aRawHeaderText;
  finally
    AlFreeAndNil(LRawHeaderLst);
  end;
end;

{**************************************}
constructor TALHTTPRequestHeader.Create;
Begin
  inherited;
  fHeaders:= TALNVStringList.create;
  fHeaders.NameValueSeparator := ':';
  FCookies := TALNVStringList.create;
  Accept := 'text/html, */*';
end;

{**************************************}
destructor TALHTTPRequestHeader.Destroy;
begin
  AlFreeAndNil(fHeaders);
  AlFreeAndNil(Fcookies);
  inherited;
end;

{***********************************}
procedure TALHTTPRequestHeader.Clear;
begin
  fHeaders.clear;
  FCookies.Clear;
end;

{********************************************************************************************}
function TALHTTPRequestHeader.GetHeaderValueByPropertyIndex(const Index: Integer): ansiString;
begin
  Case index of
    0: result := FHeaders.values['Accept'];
    1: result := FHeaders.values['Accept-Charset'];
    2: result := FHeaders.values['Accept-Encoding'];
    3: result := FHeaders.values['Accept-Language'];
    4: result := FHeaders.values['Allow'];
    5: result := FHeaders.values['Authorization'];
    6: result := FHeaders.values['Cache-Control'];
    7: result := FHeaders.values['Connection'];
    8: result := FHeaders.values['Content-Encoding'];
    9: result := FHeaders.values['Content-Language'];
    10: result := FHeaders.values['Content-Length'];
    11: result := FHeaders.values['Content-Location'];
    12: result := FHeaders.values['Content-MD5'];
    13: result := FHeaders.values['Content-Range'];
    14: result := FHeaders.values['Content-Type'];
    15: result := FHeaders.values['Date'];
    16: result := FHeaders.values['Expect'];
    17: result := FHeaders.values['Expires'];
    18: result := FHeaders.values['From'];
    19: result := FHeaders.values['Host'];
    20: result := FHeaders.values['If-Match'];
    21: result := FHeaders.values['If-Modified-Since'];
    22: result := FHeaders.values['If-None-Match'];
    23: result := FHeaders.values['If-Range'];
    24: result := FHeaders.values['If-Unmodified-Since'];
    25: result := FHeaders.values['Last-Modified'];
    26: result := FHeaders.values['Max-Forwards'];
    27: result := FHeaders.values['Pragma'];
    28: result := FHeaders.values['Proxy-Authorization'];
    29: result := FHeaders.values['Range'];
    30: result := FHeaders.values['Referer'];
    31: result := FHeaders.values['TE'];
    32: result := FHeaders.values['Trailer'];
    33: result := FHeaders.values['Transfer-Encoding'];
    34: result := FHeaders.values['Upgrade'];
    35: result := FHeaders.values['User-Agent'];
    36: result := FHeaders.values['Via'];
    37: result := FHeaders.values['Warning'];
    else raise Exception.Create('Error A24918E8-4580-43FC-B513-47D1D4483F72');
  end;
end;

{**********************************************************************************************************}
procedure TALHTTPRequestHeader.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  Case index of
    0: FHeaders.values['Accept'] := AlTrim(Value);
    1: FHeaders.values['Accept-Charset'] := AlTrim(Value);
    2: FHeaders.values['Accept-Encoding'] := AlTrim(Value);
    3: FHeaders.values['Accept-Language'] := AlTrim(Value);
    4: FHeaders.values['Allow'] := AlTrim(Value);
    5: FHeaders.values['Authorization'] := AlTrim(Value);
    6: FHeaders.values['Cache-Control'] := AlTrim(Value);
    7: FHeaders.values['Connection'] := AlTrim(Value);
    8: FHeaders.values['Content-Encoding'] := AlTrim(Value);
    9: FHeaders.values['Content-Language'] := AlTrim(Value);
    10: FHeaders.values['Content-Length'] := AlTrim(Value);
    11: FHeaders.values['Content-Location'] := AlTrim(Value);
    12: FHeaders.values['Content-MD5'] := AlTrim(Value);
    13: FHeaders.values['Content-Range'] := AlTrim(Value);
    14: FHeaders.values['Content-Type'] := AlTrim(Value);
    15: FHeaders.values['Date'] := AlTrim(Value);
    16: FHeaders.values['Expect'] := AlTrim(Value);
    17: FHeaders.values['Expires'] := AlTrim(Value);
    18: FHeaders.values['From'] := AlTrim(Value);
    19: FHeaders.values['Host'] := AlTrim(Value);
    20: FHeaders.values['If-Match'] := AlTrim(Value);
    21: FHeaders.values['If-Modified-Since'] := AlTrim(Value);
    22: FHeaders.values['If-None-Match'] := AlTrim(Value);
    23: FHeaders.values['If-Range'] := AlTrim(Value);
    24: FHeaders.values['If-Unmodified-Since'] := AlTrim(Value);
    25: FHeaders.values['Last-Modified'] := AlTrim(Value);
    26: FHeaders.values['Max-Forwards'] := AlTrim(Value);
    27: FHeaders.values['Pragma'] := AlTrim(Value);
    28: FHeaders.values['Proxy-Authorization'] := AlTrim(Value);
    29: FHeaders.values['Range'] := AlTrim(Value);
    30: FHeaders.values['Referer'] := AlTrim(Value);
    31: FHeaders.values['TE'] := AlTrim(Value);
    32: FHeaders.values['Trailer'] := AlTrim(Value);
    33: FHeaders.values['Transfer-Encoding'] := AlTrim(Value);
    34: FHeaders.values['Upgrade'] := AlTrim(Value);
    35: FHeaders.values['User-Agent'] := AlTrim(Value);
    36: FHeaders.values['Via'] := AlTrim(Value);
    37: FHeaders.values['Warning'] := AlTrim(Value);
    else raise Exception.Create('Error F230BBEE-7614-4CEF-AC06-0D190D4BD838');
  end;
end;

{*********************************************************}
Function TALHTTPRequestHeader.GetRawHeaderText: AnsiString;
Var LName: ansiString;
    LValue: ansiString;
    I : integer;
begin
  result := '';
  For I := 0 to FHeaders.count - 1 do begin
    LName := ALTrim(FHeaders.names[I]);
    LValue := alTrim(FHeaders.ValueFromIndex[I]);
    If (LName <> '') and (LValue <> '') then
      result := result + LName + ': ' + LValue + #13#10;
  end;
  If FCookies.Count > 0 then begin
    LValue := AlStringReplace(ALTrim(FCookies.text), #13#10, '; ', [rfReplaceAll]);
    if LValue <> '' then
      result := result + 'Cookie: ' + LValue + #13#10;
  end;
end;

{***********************************************************************************************}
procedure TALHTTPRequestHeader.setHeaderValue(const aName: ansiString; const aValue: ansiString);
begin
  If (aName <> '') then begin
    if AlSameText(aName, 'Cookie') then begin
      FCookies.Clear;
      if (aValue <> '') then ALExtractHTTPFields([';'], [' '], [], PAnsiChar(aValue), fCookies, True)
    end
    else FHeaders.Values[aName] := aValue;
  end;
end;

{********************************************************************************}
procedure TALHTTPRequestHeader.SetRawHeaderText(const aRawHeaderText: AnsiString);
Var LTmpHeaders: TALStringList;
    I: integer;
begin
  LTmpHeaders := TALStringList.create;
  try

    LTmpHeaders.NameValueSeparator := ':';
    LTmpHeaders.Text := aRawHeaderText;

    FHeaders.clear;
    FCookies.clear;
    For I := 0 to LTmpHeaders.count - 1 do
      setHeaderValue(ALTrim(LTmpHeaders.Names[I]),
                     alTrim(LTmpHeaders.ValueFromIndex[I]));

  finally
    AlFreeAndNil(LTmpHeaders);
  end;
end;

{*******************************************************************}
constructor TALNameValuePair.Create(const AName, AValue: ansiString);
begin
  Name := AName;
  Value := AValue;
end;

{*******************************************************************}
procedure ALHTTPEncodeParamNameValues(const ParamValues: TALStrings);
var I: Integer;
    LPos: integer;
    LStr: AnsiString;
begin
  for I := 0 to ParamValues.Count - 1 do begin
    LStr := ParamValues[I];
    LPos := AlPos(ParamValues.NameValueSeparator, LStr);
    if LPos > 0 then ParamValues[I] := ALHTTPEncode(AlCopyStr(LStr, 1, LPos-1)) + '=' + ALHTTPEncode(AlCopyStr(LStr, LPos+1, MAXINT));
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
  ALExtractHeaderFields(Separators,
                        WhiteSpace,
                        Quotes,
                        Content,
                        Strings,
                        True,
                        StripQuotes);
end;

{*****************************************************************}
Function  AlRemoveShemeFromUrl(const aUrl: AnsiString): ansiString;
Var P: integer;
begin
  P := AlPos('://', aUrl);
  if P > 0 then result := ALcopyStr(aUrl, P+3, maxint)
  else result := aUrl;
end;

{****************}
{$IFDEF MSWINDOWS}
Function AlExtractShemeFromUrl(const aUrl: AnsiString): TInternetScheme;
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
                        PortNumber) then begin
    if ALSameText(SchemeName,'http') then result := INTERNET_SCHEME_HTTP
    else if ALSameText(SchemeName,'https') then result := INTERNET_SCHEME_HTTPS
    else if ALSameText(SchemeName,'ftp') then result := INTERNET_SCHEME_FTP
    else result := INTERNET_SCHEME_UNKNOWN;
  end
  else result := INTERNET_SCHEME_UNKNOWN;
end;
{$ENDIF}

{********************************************************************}
Function AlExtractHostNameFromUrl(const aUrl: AnsiString): AnsiString;
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

{**********************************************************************}
Function AlExtractDomainNameFromUrl(const aUrl: AnsiString): AnsiString;
var LIPv4Num: Cardinal;
begin
  Result := AlExtractHostNameFromUrl(aUrl);
  if not ALTryIPV4StrToNumeric(Result, LIPv4Num) then begin
    while length(AlStringReplace(Result,
                                 '.',
                                 '',
                                 [rfReplaceALL])) < length(result) - 1 do delete(Result, 1, ALpos('.',result));
  end;
end;

{********************************************************************}
Function  AlExtractUrlPathFromUrl(const aUrl: AnsiString): AnsiString;
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

{**************************************************}
Function  AlInternetCrackUrl(const aUrl: AnsiString;
                             Var SchemeName,
                                 HostName,
                                 UserName,
                                 Password,
                                 UrlPath,
                                 ExtraInfo: AnsiString;
                             var PortNumber: integer): Boolean;
Var P1, P2: Integer;
    S1: AnsiString;
begin
  HostName := aUrl;
  Result := True;
  P1 := AlPos('://', HostName);  // ftp://xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx?param1=value1
  if P1 > 0 then begin
    SchemeName := AlCopyStr(HostName, 1, P1-1); // ftp
    delete(HostName,1, P1+2);                   // xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx?param1=value1
    P2 := AlPos('#',HostName);
    P1 := AlPos('?',HostName);
    if (P1 > 0) and (P2 > 0) then P1 := Min(P1,P2)
    else if (P2 > 0) then P1 := P2;
    if P1 > 0 then begin
      ExtraInfo := AlCopyStr(HostName, P1, Maxint); // ?param1=value1
      delete(HostName, P1, Maxint);                 // xxxx:yyyyy@ftp.yoyo.com:21/path/filename.xxx
    end
    else ExtraInfo := '';
    P1 := AlPos('/',HostName);
    if P1 > 0 then begin
      UrlPath := AlCopyStr(HostName, P1, Maxint); // /path/filename.xxx
      delete(HostName, P1, Maxint);               // xxxx:yyyyy@ftp.yoyo.com:21
    end
    else UrlPath := '';
    P1 := ALLastDelimiter('@',HostName);
    if P1 > 0 then begin
      S1 := AlCopyStr(HostName, 1, P1-1); // xxxx:yyyyy
      delete(HostName,1, P1);             // ftp.yoyo.com:21
      P1 := Alpos(':', S1);
      if P1 > 0 then begin
        UserName := AlCopyStr(S1,1,P1-1);      // xxxx
        Password := AlCopyStr(S1,P1+1,Maxint); // yyyyy
      end
      else begin
        UserName := S1;
        Password := '';
      end;
    end
    else begin
      UserName := '';
      Password := '';
    end;
    P2 := AlPos(']', HostName); // to handle ipV6 url like [::1]:8080
    P1 := AlPosEx(':',HostName, P2+1);
    if P1 > 0 then begin
      S1 := AlCopyStr(HostName, P1+1, Maxint); // 21
      delete(HostName, P1, Maxint);            // ftp.yoyo.com
      if not ALTryStrToInt(S1, PortNumber) then PortNumber := 0;
    end
    else PortNumber := 0;
    if PortNumber = 0 then begin
      if ALSameText(SchemeName, 'http') then PortNumber := 80
      else if ALSameText(SchemeName, 'https') then PortNumber := 443
      else if ALSameText(SchemeName, 'ws') then PortNumber := 80
      else if ALSameText(SchemeName, 'wss') then PortNumber := 443
      else if ALSameText(SchemeName, 'ftp') then PortNumber := 21
      else result := False;
    end;
    If result then result := HostName <> '';
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


{*************************************************}
Function AlInternetCrackUrl(const aUrl: AnsiString;
                            Var SchemeName,
                                HostName,
                                UserName,
                                Password,
                                UrlPath,
                                Anchor: AnsiString; // not the anchor is never send to the server ! it's only used on client side
                            const Query: TALStrings;
                            var PortNumber: integer): Boolean;
var LExtraInfo: AnsiString;
    P1: integer;
begin
  Result := AlInternetCrackUrl(aUrl,
                               SchemeName,
                               HostName,
                               UserName,
                               Password,
                               UrlPath,
                               LExtraInfo,
                               PortNumber);
  if result then begin
    P1 := AlPos('#',LExtraInfo);
    if P1 > 0 then begin
      Anchor := AlCopyStr(LExtraInfo, P1+1, MaxInt);
      delete(LExtraInfo, P1, Maxint);
    end
    else Anchor := '';
    if (LExtraInfo <> '') and (LExtraInfo[1] = '?') then begin
      if AlPos('&amp;', LExtraInfo) > 0 then Query.LineBreak := '&amp;'
      else Query.LineBreak := '&';
      Query.text := AlCopyStr(LExtraInfo,2,Maxint);
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
                             const Query: TALStrings): Boolean;
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

{*****************************************************************}
Function AlRemoveAnchorFromUrl(const aUrl: AnsiString): AnsiString;
var LAnchor: AnsiString;
begin
  result := AlRemoveAnchorFromUrl(aUrl,LAnchor);
end;

{****************}
{$IFDEF MSWINDOWS}
function AlCombineUrl(const RelativeUrl, BaseUrl: AnsiString): AnsiString;
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
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function  AlCombineUrl(const RelativeUrl,
                             BaseUrl,
                             Anchor: AnsiString;
                       const Query: TALStrings): AnsiString;
Var S1 : AnsiString;
    LBool: Boolean;
begin
  if Query.Count > 0 then begin

    if Query.LineBreak = #13#10 then begin
      LBool := True;
      Query.LineBreak := '&';
    end
    else LBool := False;

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
      if LBool then Query.LineBreak := #13#10;
    end;

  end
  else S1 := '';

  if Anchor <> '' then S1 := S1 + '#' + Anchor;

  Result := AlCombineUrl(RelativeUrl + S1, BaseUrl);
end;
{$ENDIF}

{*********************************************************************}
{aValue is a GMT TDateTime - result is "Sun, 06 Nov 1994 08:49:37 GMT"}
function  ALGMTDateTimeToRfc822Str(const aValue: TDateTime): AnsiString;
var LDay, LMonth, LYear: Word;
begin
  DecodeDate(aValue,
             LYear,
             LMonth,
             LDay);

  Result := ALFormat('%s, %.2d %s %.4d %s %s',
                     [AlRfc822DayOfWeekNames[DayOfWeek(aValue)],
                      LDay,
                      ALRfc822MonthOfTheYearNames[LMonth],
                      LYear,
                      ALFormatDateTime('hh":"nn":"ss', aValue, ALDefaultFormatSettings),
                      'GMT']);
end;

{****************}
{$IFDEF MSWINDOWS}
{aValue is a Local TDateTime - result is "Sun, 06 Nov 1994 08:49:37 GMT"}
function ALDateTimeToRfc822Str(const aValue: TDateTime): AnsiString;
begin
  Result := ALGMTDateTimeToRfc822Str(AlLocalDateTimeToUTC(aValue));
end;
{$ENDIF}

{************************************************************}
{Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
 to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)

 The "Date" line (formerly "Posted") is the date that the message was
 originally posted to the network.  Its format must be acceptable
 both in RFC-822 and to the getdate(3) routine that is provided with
 the Usenet software.  This date remains unchanged as the message is
 propagated throughout the network.  One format that is acceptable to
 both is:

                      Wdy, DD Mon YY HH:MM:SS TIMEZONE

 Several examples of valid dates appear in the sample message above.
 Note in particular that ctime(3) format:

                      Wdy Mon DD HH:MM:SS YYYY

  is not acceptable because it is not a valid RFC-822 date.  However,
  since older software still generates this format, news
  implementations are encouraged to accept this format and translate
  it into an acceptable format.

  There is no hope of having a complete list of timezones.  Universal
  Time (GMT), the North American timezones (PST, PDT, MST, MDT, CST,
  CDT, EST, EDT) and the +/-hhmm offset specifed in RFC-822 should be
  supported.  It is recommended that times in message headers be
  transmitted in GMT and displayed in the local time zone.}
Function  ALTryRfc822StrToGMTDateTime(const S: AnsiString; out Value: TDateTime): Boolean;

  {--------------------------------------------------------------------------}
  Function InternalMonthWithLeadingChar(const AMonth: AnsiString): AnsiString;
  Begin
    If Length(AMonth) = 1 then result := '0' + AMonth
    else result := aMonth;
  end;

Var P1,P2: Integer;
    LDateStr : AnsiString;
    LLst: TALStringList;
    LMonthLabel: AnsiString;
    LFormatSettings: TALformatSettings;
    LTimeZoneStr: AnsiString;
    LTimeZoneDelta: TDateTime;

Begin

  LDateStr := S; // Wdy, DD-Mon-YYYY HH:MM:SS GMT
                 // Wdy, DD-Mon-YYYY HH:MM:SS +0200
                 // 23 Aug 2004 06:48:46 -0700
  P1 := AlPos(',',LDateStr);
  If P1 > 0 then delete(LDateStr,1,P1); // DD-Mon-YYYY HH:MM:SS GMT
                                        // DD-Mon-YYYY HH:MM:SS +0200
                                        // 23 Aug 2004 06:48:46 -0700
  LDateStr := ALTrim(LDateStr); // DD-Mon-YYYY HH:MM:SS GMT
                                // DD-Mon-YYYY HH:MM:SS +0200
                                // 23 Aug 2004 06:48:46 -0700

  P1 := AlPos(':',LDateStr);
  P2 := AlPos('-',LDateStr);
  While (P2 > 0) and (P2 < P1) do begin
    LDateStr[P2] := ' ';
    P2 := AlPosEx('-',LDateStr,P2);
  end; // DD Mon YYYY HH:MM:SS GMT
       // DD Mon YYYY HH:MM:SS +0200
       // 23 Aug 2004 06:48:46 -0700
  While Alpos('  ',LDateStr) > 0 do LDateStr := AlStringReplace(LDateStr,'  ',' ',[RfReplaceAll]); // DD Mon YYYY HH:MM:SS GMT
                                                                                                   // DD Mon YYYY HH:MM:SS +0200
                                                                                                   // 23 Aug 2004 06:48:46 -0700

  LLst := TALStringList.create;
  Try

    LLst.Text :=  AlStringReplace(LDateStr,' ',#13#10,[RfReplaceall]);
    If LLst.Count < 5 then begin
      Result := False;
      Exit;
    end;

    LMonthLabel := ALTrim(LLst[1]); // Mon
                                    // Mon
                                    // Aug
    P1 := 1;
    While (p1 <= 12) and (not ALSameText(ALRfc822MonthOfTheYearNames[P1],LMonthLabel)) do inc(P1);
    If P1 > 12 then begin
      Result := False;
      Exit;
    end;

    LFormatSettings := ALDefaultFormatSettings;
    LFormatSettings.DateSeparator := '/';
    LFormatSettings.TimeSeparator := ':';
    LFormatSettings.ShortDateFormat := 'dd/mm/yyyy';
    LFormatSettings.ShortTimeFormat := 'hh:nn:zz';

    LTimeZoneStr := ALTrim(LLst[4]); // GMT
                                     // +0200
                                     // -0700
    LTimeZoneStr := AlStringReplace(LTimeZoneStr,'(','',[]);
    LTimeZoneStr := AlStringReplace(LTimeZoneStr,')','',[]);
    LTimeZoneStr := ALTrim(LTimeZoneStr);
    If LTimeZoneStr = '' then Begin
      Result := False;
      Exit;
    end
    else If (Length(LTimeZoneStr) >= 5) and
            (LTimeZoneStr[1] in ['+','-']) and
            (LTimeZoneStr[2] in ['0'..'9']) and
            (LTimeZoneStr[3] in ['0'..'9']) and
            (LTimeZoneStr[4] in ['0'..'9']) and
            (LTimeZoneStr[5] in ['0'..'9']) then begin
      LTimeZoneDelta := ALStrToDateTime(AlCopyStr(LTimeZoneStr,2,2) + ':' + AlCopyStr(LTimeZoneStr,4,2) + ':00', LFormatSettings);
      if LTimeZoneStr[1] = '+' then LTimeZoneDelta := -1*LTimeZoneDelta;
    end
    else If ALSameText(LTimeZoneStr,'GMT') then  LTimeZoneDelta := 0
    else If ALSameText(LTimeZoneStr,'UTC') then  LTimeZoneDelta := 0
    else If ALSameText(LTimeZoneStr,'UT')  then  LTimeZoneDelta := 0
    else If ALSameText(LTimeZoneStr,'EST') then LTimeZoneDelta := ALStrToDateTime('05:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'EDT') then LTimeZoneDelta := ALStrToDateTime('04:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'CST') then LTimeZoneDelta := ALStrToDateTime('06:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'CDT') then LTimeZoneDelta := ALStrToDateTime('05:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'MST') then LTimeZoneDelta := ALStrToDateTime('07:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'MDT') then LTimeZoneDelta := ALStrToDateTime('06:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'PST') then LTimeZoneDelta := ALStrToDateTime('08:00:00', LFormatSettings)
    else If ALSameText(LTimeZoneStr,'PDT') then LTimeZoneDelta := ALStrToDateTime('07:00:00', LFormatSettings)
    else begin
      Result := False;
      Exit;
    end;

    LDateStr := ALTrim(LLst[0]) + '/' + InternalMonthWithLeadingChar(ALIntToStr(P1)) + '/' + ALTrim(LLst[2]) + ' ' + ALTrim(LLst[3]); // DD/MM/YYYY HH:MM:SS
    Result := ALTryStrToDateTime(LDateStr,Value,LFormatSettings);
    If Result then Value := Value + LTimeZoneDelta;

  finally
    AlFreeAndNil(LLst);
  end;

end;

{*************************************************************}
{Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
 the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
 to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)}
function  ALRfc822StrToGMTDateTime(const s: AnsiString): TDateTime;
Begin
  if not ALTryRfc822StrToGMTDateTime(S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [S]);
end;

{******************************************************************************************}
Function ALTryIPV4StrToNumeric(const aIPv4Str: ansiString; var aIPv4Num: Cardinal): Boolean;
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
  aIPv4Num := (cardinal(I1)*256*256*256) + (cardinal(I2)*256*256) +  (cardinal(I3)*256) + (cardinal(I4));

End;

{*************************************************************}
Function ALIPV4StrToNumeric(const aIPv4: ansiString): Cardinal;
Begin
  if not ALTryIPV4StrToNumeric(aIPv4, Result) then Raise EALException.CreateFmt('Bad IPv4 string: %s', [aIPv4]);
End;

{*************************************************************}
Function ALNumericToIPv4Str(const aIPv4: Cardinal): ansiString;
Var S1, S2, S3, S4: ansiString;
Begin

  S1 := ALIntToStr(( aIPv4 div (256*256*256) ) mod 256);
  S2 := ALIntToStr(( aIPv4 div (256*256)     ) mod 256);
  S3 := ALIntToStr(( aIPv4 div (256)         ) mod 256);
  S4 := ALIntToStr(( aIPv4                   ) mod 256);

  Result := S1 + '.' + S2 + '.' + S3 + '.' + S4;

End;

{********************************************************************************}
{This function calculates ending IPv4-address for a given start of IPv4 range and
 length of subnetwork mask.
 Calculation is described in RFC: http://tools.ietf.org/html/rfc1878,
 calculator to test its behaviour - https://www.ultratools.com/tools/netMask.
 There are declared that length of subnetwork represents number of addresses
 like 2^(32 - n) - 2 where "n" is a length of subnetwork mask.

 This way for the address:
  1.0.1.0/26

 the length will be 2^(32-26) - 2 = 2^6 - 2 = 62 addresses + 1 reserved for
 the services purposes, so the last possible address in that range will be:
  1.0.1.63}
function ALIPv4EndOfRange(const aStartIPv4: Cardinal; aMaskLength: integer): Cardinal;
begin
  if (aMaskLength < 1) or
     (aMaskLength > 32) then raise Exception.CreateFmt('Wrong value for mask length IPv4: %d', [aMaskLength]);

  result := aStartIPv4 + Round(Power(2, (32 - aMaskLength)) - 1 {why not -2 it's that +1 address for service purposes})
end;

{*********************************}
Function ALZeroIpV6: TALIPv6Binary;
begin
  FillChar(result, 16, #0);
end;

{*************************************************************************}
Function ALIsValidIPv6BinaryStr(const aIPV6BinaryStr: ansiString): boolean;
begin
  result := (length(aIPV6BinaryStr) = 16) and (aIPV6BinaryStr <> ALZeroIpV6)
end;

{****************************************************************************************}
Function ALTryIPV6StrToBinary(aIPv6Str: ansiString; var aIPv6Bin: TALIPv6Binary): Boolean;
var LLstIpv6Part: TALStringList;
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

  //https://howdoesinternetwork.com/2013/ipv6-zone-id
  P1 := Alpos('%', aIPv6Str); // fe80:3438:7667:5c77:ce27%18
  if (P1 > 0) then delete(aIPv6Str,P1,maxint); // fe80:3438:7667:5c77:ce27

  //----------
  LLstIpv6Part := TALStringList.Create;
  try

    //----------
    LLstIpv6Part.Text := AlStringReplace(aIPv6Str, ':', #13#10, [rfReplaceALL]);

    //----------
    if (LLstIpv6Part.Count > 8) then begin
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
      if i >= LLstIpv6Part.Count then begin
        result := False;
        Exit;
      end
      else if (LLstIpv6Part[i] = '') then begin
        LLstIpv6Part[i] := '0000';
        while LLstIpv6Part.Count < 8 do begin
          LLstIpv6Part.Insert(i,'0000');
          inc(i);
        end;
      end
      else begin
        if length(LLstIpv6Part[i]) > 4 then begin
          result := False;
          Exit;
        end
        else if length(LLstIpv6Part[i]) < 4 then begin
          SetLength(S1,4-length(LLstIpv6Part[i]));
          FillChar(S1[1], length(S1), '0');
          LLstIpv6Part[i] := S1 + LLstIpv6Part[i];
        end;
      end;
      inc(i);
    end;

    //----------
    for I := 0 to LLstIpv6Part.Count - 1 do begin

      S1 := AlUpperCase(AlCopyStr(LLstIpv6Part[i], 1, 2));
      if (not ALTryStrToInt('$' + S1, P1)) or
         (not (P1 in [0..255])) then begin
        Result := False;
        exit;
      end;
      aIPv6Bin[(i*2) + 1] := AnsiChar(P1);

      S1 := AlUpperCase(AlCopyStr(LLstIpv6Part[i], 3, 2));
      if (not ALTryStrToInt('$' + S1, P1)) or
         (not (P1 in [0..255])) then begin
        Result := False;
        exit;
      end;
      aIPv6Bin[(i*2) + 2] := AnsiChar(P1);

    end;

    Result := True;

  finally
    AlFreeAndNil(LLstIpv6Part);
  end;

end;

{*****************************************************************}
Function ALIPV6StrTobinary(const aIPv6: ansiString): TALIPv6Binary;
Begin
  if not ALTryIPv6StrToBinary(aIPv6, Result) then Raise EALException.CreateFmt('Bad IPv6 string: %s', [aIPv6]);
End;

{*****************************************************************}
Function ALBinaryToIPv6Str(const aIPv6: TALIPv6Binary): ansiString;
Begin

  Result := ALIntToHex(ord(aIPv6[1]), 2)  + ALIntToHex(ord(aIPv6[2]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[3]), 2)  + ALIntToHex(ord(aIPv6[4]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[5]), 2)  + ALIntToHex(ord(aIPv6[6]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[7]), 2)  + ALIntToHex(ord(aIPv6[8]), 2)  + ':' +
            ALIntToHex(ord(aIPv6[9]), 2)  + ALIntToHex(ord(aIPv6[10]), 2) + ':' +
            ALIntToHex(ord(aIPv6[11]), 2) + ALIntToHex(ord(aIPv6[12]), 2) + ':' +
            ALIntToHex(ord(aIPv6[13]), 2) + ALIntToHex(ord(aIPv6[14]), 2) + ':' +
            ALIntToHex(ord(aIPv6[15]), 2) + ALIntToHex(ord(aIPv6[16]), 2);

End;

{********************************************************************************}
Function ALBinaryStrToIPv6Binary(const aIPV6BinaryStr: ansiString): TALIPv6Binary;
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

{**************************************************************************}
function ALBinaryStrToIPv6Str(const aIPV6BinaryStr: ansiString): ansiString;
begin
  result := ALBinaryToIPv6Str(ALBinaryStrToIPv6Binary(aIPV6BinaryStr));
end;

{********************************************************************************
{This function calculates ending IPv6-address for a given start of IPv6-range and
 length of subnetwork mask.
 RFC about IPv6 architecture is described here: https://tools.ietf.org/html/rfc4291.
 Check out the also the good table representing the rules to calculate this ending address:
  https://www.ripe.net/internet-coordination/press-centre/cidr_chart1.jpg
 Calculator to test its behaviour: https://www.ultratools.com/tools/ipv6CIDRToRange

 Assume that start IPv6: 2001:250:c20::/43,
 /43 means that 128 - 43 = 85 bits starting from the lowest bit must be set to 1 and
 we will get ending address.

 So this address can be written like:
  2001:250:c20:0:0:0:0:0

 Setting 85 first bits of this address to 1 we will get
  2001:250:c3f:ffff:ffff:ffff:ffff:ffff}
function ALIPv6EndOfRange(const aStartIPv6: TALIPv6Binary; aMaskLength: integer): TALIPv6Binary;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _setBitTo1(const aValue: byte;
                      const aBitNumber: byte): byte;
  begin
    if (aBitNumber > 8) or
       (aBitNumber < 1) then raise EALException.Create('Bad bit number for IPv6');
    result := aValue or (1 shl (aBitNumber - 1));
  end;

var LBitsCount: integer;
    LByteNumber: integer;
    LBitNumber: integer;
    I: byte;

begin
  if (aMaskLength < 1) or
     (aMaskLength > 128) then raise Exception.CreateFmt('Wrong value for mask length IPv6: %d', [aMaskLength]);

  result := aStartIPv6;
  LBitsCount := 128 - aMaskLength; // for example, 128 - 24 = 104

  // scroll all the required bits and set them to 1;
  // 1st bit to set actually represents 1st bit of the 16th byte,
  // ...
  // 10th bit represents 2nd bit of the 15th byte et cetera
  for I := 1 to LBitsCount do begin
    // NOTE: these variables are uneseccarily but the code becomes to be a little
    //       more readable, we see immediately what each formula represents.
    LByteNumber := 16 - (Ceil(I / 8)) + 1;
    LBitNumber := 8 - ((Ceil(I / 8) * 8) - I);
    result[LByteNumber] := AnsiChar(_setBitTo1(Ord(result[LByteNumber]), LBitNumber));
  end;
end;

{****************************************************}
procedure ALIPv6SplitParts(const aIPv6: TALIPv6Binary;
                           var aLowestPart: UInt64;
                           var aHigestPart: UInt64);
var LIntRec: Int64Rec;
    I: integer;
begin
  // get the Lowest Part
  LIntRec.Lo := 0;
  LIntRec.Hi := 0;
  for I := 8 downto 1 do begin
    LIntRec.Bytes[8 - I] := Ord(aIPv6[I]);
  end;
  aLowestPart := UInt64(LIntRec);

  // get the Higest Part
  LIntRec.Lo := 0;
  LIntRec.Hi := 0;
  for I := 16 downto 9 do begin
    LIntRec.Bytes[16 - I] := Ord(aIPv6[I]);
  end;
  aHigestPart := UInt64(LIntRec);
end;

{***********************************************************************************}
constructor EALHTTPClientException.Create(const Msg: AnsiString; SCode: Integer = 0);
begin
  inherited Create(String(Msg));
  FStatusCode := SCode;
end;

{******************************************************************************************************************}
constructor EALHTTPClientException.CreateFmt(const Msg: AnsiString; const Args: array of const; SCode: Integer = 0);
begin
  inherited CreateFmt(String(Msg), Args);
  FStatusCode := SCode;
end;

{*******************************}
constructor TALHTTPClient.Create;
begin
  inherited;
  FUploadBufferSize := $8000;
  FConnectTimeout := 0;
  FSendTimeout := 0;
  FReceiveTimeout := 0;
  FUserName := '';
  FPassword := '';
  FOnUploadProgress := nil;
  FOnDownloadProgress := nil;
  FOnRedirect := nil;
  FProxyParams := TALHTTPClientProxyParams.Create;
  FProxyParams.OnChange := OnProxyParamsChange;
  FRequestHeader := TALHTTPRequestHeader.Create;
  FRequestHeader.UserAgent := 'Mozilla/3.0 (compatible; TALHTTPClient)';
  FProtocolVersion := HTTPpv_1_1;
end;

{*******************************}
destructor TALHTTPClient.Destroy;
begin
  AlFreeAndNil(FProxyParams);
  AlFreeAndNil(FRequestHeader);
  inherited;
end;

{***************************************************************}
procedure TALHTTPClient.SetUsername(const NameValue: AnsiString);
begin
  FUserName := NameValue;
end;

{*******************************************************************}
procedure TALHTTPClient.SetPassword(const PasswordValue: AnsiString);
begin
  FPassword := PasswordValue;
end;

{*****************************************************************************}
procedure TALHTTPClient.SetOnRedirect(const Value: TAlHTTPClientRedirectEvent);
begin
  FOnRedirect := Value;
end;

{************************************************}
Procedure TALHTTPClient.Get(const aUrl:AnsiString;
                            const aRequestFields: TALStrings;
                            const aResponseContent: TStream;
                            const aResponseHeader: TALHTTPResponseHeader;
                            const ARequestHeaderValues: TALNameValueArray = nil;
                            Const aEncodeRequestFields: Boolean=True);
Var Query: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin

  Query := '';
  for i := 0 to aRequestFields.Count - 1 do begin
    Str := aRequestFields[i];
    P := AlPos(aRequestFields.NameValueSeparator, Str);
    if aEncodeRequestFields then begin
      if P > 0 then Query := Query + ALHTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + ALHTTPEncode(AlCopyStr(Str, P+1, MAXINT)) + ALIfThen(i < aRequestFields.Count - 1, '&')
      else Query := Query + ALHTTPEncode(Str) + ALIfThen(i < aRequestFields.Count - 1, '&')
    end
    else begin
      if P > 0 then Query := Query + AlCopyStr(Str, 1, P-1) + '=' + AlCopyStr(Str, P+1, MAXINT) + ALIfThen(i < aRequestFields.Count - 1, '&')
      else Query := Query + Str + ALIfThen(i < aRequestFields.Count - 1, '&')
    end;
  end;
  if Query <> '' then begin
    P := ALpos('?', aUrl);
    if P <= 0 then Query := '?' + Query
    else if P <> length(aUrl) then Query := '&' + Query;
  end;

  Get(aUrl + Query,
      aResponseContent,
      aResponseHeader,
      ARequestHeaderValues);

end;

{*************************************************}
procedure TALHTTPClient.Get(const aUrl: AnsiString;
                            const aResponseContent: TStream;
                            const aResponseHeader: TALHTTPResponseHeader;
                            const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Execute(aUrl,
          HTTPmt_get,
          nil,
          ARequestHeaderValues,
          aResponseContent,
          aResponseHeader);
end;

{***********************************************}
Function TALHTTPClient.Get(const aUrl:AnsiString;
                           const aRequestFields: TALStrings;
                           const ARequestHeaderValues: TALNameValueArray = nil;
                           Const aEncodeRequestFields: Boolean=True): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Get(aUrl,
        aRequestFields,
        LResponseContent,
        nil,
        ARequestHeaderValues,
        aEncodeRequestFields);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{************************************************}
function TALHTTPClient.Get(const aUrl: AnsiString;
                           const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Get(aUrl,
        LResponseContent,
        nil,
        ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{**************************************************}
procedure TALHTTPClient.Post(const aUrl: AnsiString;
                             const aPostDataStream: TStream;
                             const aResponseContent: TStream;
                             const aResponseHeader: TALHTTPResponseHeader;
                             const ARequestHeaderValues: TALNameValueArray = nil);
Var LOldContentLengthValue: AnsiString;
begin
  LOldContentLengthValue := FrequestHeader.ContentLength;
  try
    If assigned(aPostDataStream) then FrequestHeader.ContentLength := ALIntToStr(aPostDataStream.Size)
    else FrequestHeader.ContentLength := '0';
    Execute(aURL,
            HTTPmt_Post,
            aPostDataStream,
            ARequestHeaderValues,
            aResponseContent,
            aResponseHeader);
  finally
    FrequestHeader.ContentLength := LOldContentLengthValue;
  end;
end;

{**************************************************}
procedure TALHTTPClient.Post(const aUrl: AnsiString;
                             const aResponseContent: TStream;
                             const aResponseHeader: TALHTTPResponseHeader;
                             const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Post(aUrl,
       nil,
       aResponseContent,
       aResponseHeader,
       ARequestHeaderValues);
end;

{*************************************************}
function TALHTTPClient.Post(const aUrl: AnsiString;
                            const aPostDataStream: TStream;
                            const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    post(aUrl,
         aPostDataStream,
         LResponseContent,
         nil,
         ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{*************************************************}
function TALHTTPClient.Post(const aUrl: AnsiString;
                            const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
begin
  Result := Post(aUrl, nil, ARequestHeaderValues);
end;

{************************************************************}
procedure TALHTTPClient.PostUrlEncoded(const aUrl: AnsiString;
                                       const aRequestFields: TALStrings;
                                       const aResponseContent: TStream;
                                       const aResponseHeader: TALHTTPResponseHeader;
                                       const ARequestHeaderValues: TALNameValueArray = nil;
                                       Const aEncodeRequestFields: Boolean=True);
Var LURLEncodedContentStream: TALStringStream;
    LOldRequestContentType: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin
  LURLEncodedContentStream := TALStringStream.create('');
  LOldRequestContentType := FrequestHeader.ContentType;
  try

    if aEncodeRequestFields then begin
      for i := 0 to aRequestFields.Count - 1 do begin
        Str := aRequestFields[i];
        P := AlPos(aRequestFields.NameValueSeparator, Str);
        if P > 0 then Str := ALHTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + ALHTTPEncode(AlCopyStr(Str, P+1, MAXINT))
        else Str := ALHTTPEncode(Str);
        If i < aRequestFields.Count - 1 then LURLEncodedContentStream.WriteString(Str + '&')
        else LURLEncodedContentStream.WriteString(Str);
      end;
    end
    else begin
      for i := 0 to aRequestFields.Count - 1 do begin
        If i < aRequestFields.Count - 1 then LURLEncodedContentStream.WriteString(aRequestFields[i] + '&')
        else LURLEncodedContentStream.WriteString(aRequestFields[i]);
      end;
    end;

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    post(aUrl,
         LURLEncodedContentStream,
         aResponseContent,
         aResponseHeader,
         ARequestHeaderValues);

  finally
    AlFreeAndNil(LURLEncodedContentStream);
    FrequestHeader.ContentType := LOldRequestContentType;
  end;
end;

{***********************************************************}
function TALHTTPClient.PostUrlEncoded(const aUrl: AnsiString;
                                      const aRequestFields: TALStrings;
                                      const ARequestHeaderValues: TALNameValueArray = nil;
                                      Const aEncodeRequestFields: Boolean=True): AnsiString;
Var LURLEncodedContentStream: TALStringStream;
    LOldRequestContentType: AnsiString;
    Str: AnsiString;
    I, P: Integer;
begin
  LURLEncodedContentStream := TALStringStream.create('');
  LOldRequestContentType := FrequestHeader.ContentType;
  try

    if aEncodeRequestFields then begin
      for i := 0 to aRequestFields.Count - 1 do begin
        Str := aRequestFields[i];
        P := AlPos(aRequestFields.NameValueSeparator, Str);
        if P > 0 then Str := ALHTTPEncode(AlCopyStr(Str, 1, P-1)) + '=' + ALHTTPEncode(AlCopyStr(Str, P+1, MAXINT))
        else Str := ALHTTPEncode(Str);
        If i < aRequestFields.Count - 1 then LURLEncodedContentStream.WriteString(Str + '&')
        else LURLEncodedContentStream.WriteString(Str);
      end;
    end
    else begin
      for i := 0 to aRequestFields.Count - 1 do begin
        If i < aRequestFields.Count - 1 then LURLEncodedContentStream.WriteString(aRequestFields[i] + '&')
        else LURLEncodedContentStream.WriteString(aRequestFields[i]);
      end;
    end;

    FrequestHeader.ContentType := 'application/x-www-form-urlencoded';
    Result := post(aUrl, LURLEncodedContentStream, ARequestHeaderValues);

  finally
    AlFreeAndNil(LURLEncodedContentStream);
    FrequestHeader.ContentType := LOldRequestContentType;
  end;
end;

{*******************************************************************}
procedure TALHTTPClient.PostMultiPartFormData(const aUrl: AnsiString;
                                              const aRequestFields: TALStrings;
                                              const aRequestFiles: TALMultiPartFormDataContents;
                                              const aResponseContent: TStream;
                                              const aResponseHeader: TALHTTPResponseHeader;
                                              const ARequestHeaderValues: TALNameValueArray = nil);
Var LMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    LOldRequestContentType: AnsiString;
begin
  LMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  LOldRequestContentType := FrequestHeader.ContentType;
  try
    LMultipartFormDataEncoder.Encode(aRequestFields, aRequestFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+LMultipartFormDataEncoder.DataStream.Boundary;
    post(aUrl,
         LMultipartFormDataEncoder.DataStream,
         aResponseContent,
         aResponseHeader,
         ARequestHeaderValues);
  finally
    AlFreeAndNil(LMultipartFormDataEncoder);
    FrequestHeader.ContentType := LOldRequestContentType;
  end;
end;

{******************************************************************}
function TALHTTPClient.PostMultiPartFormData(const aUrl: AnsiString;
                                             const aRequestFields: TALStrings;
                                             const aRequestFiles: TALMultiPartFormDataContents;
                                             const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
Var LMultipartFormDataEncoder: TALMultipartFormDataEncoder;
    LOldRequestContentType: AnsiString;
begin
  LMultipartFormDataEncoder := TALMultipartFormDataEncoder.create;
  LOldRequestContentType := FrequestHeader.ContentType;
  try
    LMultipartFormDataEncoder.Encode(aRequestFields, aRequestFiles);
    FrequestHeader.ContentType := 'multipart/form-data; boundary='+LMultipartFormDataEncoder.DataStream.Boundary;
    Result := post(aUrl, LMultipartFormDataEncoder.DataStream, ARequestHeaderValues);
  finally
    AlFreeAndNil(LMultipartFormDataEncoder);
    FrequestHeader.ContentType := LOldRequestContentType;
  end;
end;

{**************************************************}
procedure TALHTTPClient.Head(const aUrl: AnsiString;
                             const aResponseContent: TStream;
                             const aResponseHeader: TALHTTPResponseHeader;
                             const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Execute(aURL,
          HTTPmt_head,
          nil,
          ARequestHeaderValues,
          aResponseContent,
          aResponseHeader);
end;

{************************************************}
function TALHTTPClient.Head(const aUrl:AnsiString;
                            const ARequestHeaderValues: TALNameValueArray = nil) : AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Head(aUrl,
         LResponseContent,
         nil,
         ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{***************************************************}
procedure TALHTTPClient.Trace(const aUrl: AnsiString;
                              const aResponseContent: TStream;
                              const aResponseHeader: TALHTTPResponseHeader;
                              const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Execute(aURL,
          HTTPmt_Trace,
          nil,
          ARequestHeaderValues,
          aResponseContent,
          aResponseHeader);
end;

{*************************************************}
function TALHTTPClient.Trace(const aUrl:AnsiString;
                             const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Trace(aUrl,
          LResponseContent,
          nil,
          ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{************************************************}
procedure TALHTTPClient.Put(const aUrl:AnsiString;
                            const aPutDataStream: TStream;
                            const aResponseContent: TStream;
                            const aResponseHeader: TALHTTPResponseHeader;
                            const ARequestHeaderValues: TALNameValueArray = nil);
Var LOldContentLengthValue: AnsiString;
begin
  LOldContentLengthValue := FrequestHeader.ContentLength;
  try
    If assigned(aPutDataStream) then FrequestHeader.ContentLength := ALIntToStr(aPutDataStream.Size)
    else FrequestHeader.ContentLength := '0';
    Execute(aURL,
            HTTPmt_Put,
            aPutDataStream,
            ARequestHeaderValues,
            aResponseContent,
            aResponseHeader);
  finally
    FrequestHeader.ContentLength := LOldContentLengthValue;
  end;
end;

{************************************************}
function TALHTTPClient.Put(const aURL: Ansistring;
                           const aPutDataStream: TStream;
                           const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    put(aUrl,
        aPutDataStream,
        LResponseContent,
        nil,
        ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{****************************************************}
procedure TALHTTPClient.Delete(const aUrl: AnsiString;
                               const aResponseContent: TStream;
                               const aResponseHeader: TALHTTPResponseHeader;
                               const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Execute(aURL,
          HTTPmt_Delete,
          nil,
          ARequestHeaderValues,
          aResponseContent,
          aResponseHeader);
end;

{***************************************************}
function TALHTTPClient.Delete(const aURL: Ansistring;
                              const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Delete(aUrl,
           LResponseContent,
           nil,
           ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{****************************************************}
procedure TALHTTPClient.Options(const aUrl: AnsiString;
                                const aResponseContent: TStream;
                                const aResponseHeader: TALHTTPResponseHeader;
                                const ARequestHeaderValues: TALNameValueArray = nil);
begin
  Execute(aURL,
          HTTPmt_Options,
          nil,
          ARequestHeaderValues,
          aResponseContent,
          aResponseHeader);
end;

{****************************************************}
function TALHTTPClient.Options(const aURL: Ansistring;
                               const ARequestHeaderValues: TALNameValueArray = nil): AnsiString;
var LResponseContent: TALStringStream;
begin
  LResponseContent := TALStringStream.Create('');
  try
    Options(aUrl,
            LResponseContent,
            nil,
            ARequestHeaderValues);
    result := LResponseContent.DataString;
  finally
    AlFreeAndNil(LResponseContent);
  end;
end;

{*****************************************************************************************}
procedure TALHTTPClient.OnProxyParamsChange(sender: Tobject; Const PropertyIndex: Integer);
begin
 //virtual
end;

{***************************************}
procedure TALHTTPClientProxyParams.Clear;
begin
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  DoChange(-1);
end;

{******************************************}
constructor TALHTTPClientProxyParams.Create;
Begin
  inherited create;
  FProxyBypass := '';
  FproxyServer := '';
  FProxyUserName := '';
  FProxyPassword := '';
  FproxyPort := 0;
  FOnchange := nil;
end;

{******************************************************************}
procedure TALHTTPClientProxyParams.DoChange(propertyIndex: Integer);
begin
  if assigned(FonChange) then FonChange(Self,propertyIndex);
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyBypass(const Value: AnsiString);
begin
  If (Value <> FProxyBypass) then begin
    FProxyBypass := Value;
    DoChange(0);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPassword(const Value: AnsiString);
begin
  If (Value <> FProxyPassword) then begin
    FProxyPassword := Value;
    DoChange(4);
  end;
end;

{********************************************************************}
procedure TALHTTPClientProxyParams.SetProxyPort(const Value: integer);
begin
  If (Value <> FProxyPort) then begin
    FProxyPort := Value;
    DoChange(2);
  end;
end;

{*************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyServer(const Value: AnsiString);
begin
  If (Value <> FProxyServer) then begin
    FProxyServer := Value;
    DoChange(1);
  end;
end;

{***************************************************************************}
procedure TALHTTPClientProxyParams.SetProxyUserName(const Value: AnsiString);
begin
  If (Value <> FProxyUserName) then begin
    FProxyUserName := Value;
    DoChange(3);
  end;
end;

end.

