unit Alcinoe.HTTP;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Generics.Collections,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.url;

type

  {-------------------------------------------------------}
  TALHttpVersion = (Unspecified, v0_9, v1_0, v1_1, v2, v3);

  {-----------------------------}
  TALHttpCookieA = class(TObject)
  public
    const
      MaxAgeUnset = -MaxInt;  // sentinel: do not emit Max-Age
  private
    FName: AnsiString;
    FValue: AnsiString;
    FPath: AnsiString;
    FDomain: AnsiString;
    FExpires: TDateTime;
    FMaxAge: Integer;
    FSameSite: AnsiString;
    FSecure: Boolean;
    FHttpOnly: Boolean;
    FPartitioned: Boolean;
  private
    function GetHeaderValue: AnsiString;
    procedure SetHeaderValue(Const aValue: AnsiString);
  public
    constructor Create; overload;
    procedure Reset;
    property Name: AnsiString read FName write FName;
    /// <summary>
    ///   Cookie value (MUST be pre–percent-encoded).
    ///   Assign an already URL/percent-encoded ASCII string; this class does not encode it.
    ///   Tip: use ALUrlEncode(Value, False{ASpacesAsPlus}) to produce a safe value
    ///   (spaces → %20, never '+').
    /// </summary>
    property Value: AnsiString read FValue write FValue;
    /// <summary>
    ///   Cookie Path attribute. This value MUST be pre–percent-encoded
    ///   (URL-encoded) by the caller if it contains spaces or other
    ///   non-token characters. No encoding is performed by this class;
    ///   the string is copied verbatim into the Set-Cookie header.
    ///   Examples: '/' or '/account%20settings'.
    /// </summary>
    property Path: AnsiString read FPath write FPath;
    property Domain: AnsiString read FDomain write FDomain;
    property Expires: TDateTime read FExpires write FExpires; // GMT
    property MaxAge: Integer read FMaxAge write FMaxAge;
    property SameSite: AnsiString read FSameSite write FSameSite;
    /// <summary>
    ///   Indicates that the cookie is sent to the server only when a
    ///   request is made with the https: scheme (except on localhost),
    ///   and therefore, is more resistant to man-in-the-middle attacks.
    /// </summary>
    property Secure: Boolean read FSecure write FSecure;
    /// <summary>
    ///   Forbids JavaScript from accessing the cookie, for example,
    ///   through the Document.cookie property.
    /// </summary>
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    /// <summary>
    ///   Indicates that the cookie should be stored using partitioned
    ///   storage. Note that if this is set, the Secure directive must
    ///   also be set.
    /// </summary>
    property Partitioned: Boolean read FPartitioned write FPartitioned;
    property HeaderValue: AnsiString read GetHeaderValue write SetHeaderValue;
  end;

  {-----------------------------}
  TALHttpCookieW = class(TObject)
  public
    const
      MaxAgeUnset = -MaxInt;  // sentinel: do not emit Max-Age
  private
    FName: String;
    FValue: String;
    FPath: String;
    FDomain: String;
    FExpires: TDateTime;
    FMaxAge: Integer;
    FSameSite: String;
    FSecure: Boolean;
    FHttpOnly: Boolean;
    FPartitioned: Boolean;
  private
    function GetHeaderValue: String;
    procedure SetHeaderValue(Const aValue: String);
  public
    constructor Create; overload;
    procedure Reset;
    property Name: String read FName write FName;
    /// <summary>
    ///   Cookie value (MUST be pre–percent-encoded).
    ///   Assign an already URL/percent-encoded ASCII string; this class does not encode it.
    ///   Tip: use ALUrlEncode(Value, False{ASpacesAsPlus}) to produce a safe value
    ///   (spaces → %20, never '+').
    /// </summary>
    property Value: String read FValue write FValue;
    /// <summary>
    ///   Cookie Path attribute. This value MUST be pre–percent-encoded
    ///   (URL-encoded) by the caller if it contains spaces or other
    ///   non-token characters. No encoding is performed by this class;
    ///   the string is copied verbatim into the Set-Cookie header.
    ///   Examples: '/' or '/account%20settings'.
    /// </summary>
    property Path: String read FPath write FPath;
    property Domain: String read FDomain write FDomain;
    property Expires: TDateTime read FExpires write FExpires; // GMT
    property MaxAge: Integer read FMaxAge write FMaxAge;
    property SameSite: String read FSameSite write FSameSite;
    /// <summary>
    ///   Indicates that the cookie is sent to the server only when a
    ///   request is made with the https: scheme (except on localhost),
    ///   and therefore, is more resistant to man-in-the-middle attacks.
    /// </summary>
    property Secure: Boolean read FSecure write FSecure;
    /// <summary>
    ///   Forbids JavaScript from accessing the cookie, for example,
    ///   through the Document.cookie property.
    /// </summary>
    property HttpOnly: Boolean read FHttpOnly write FHttpOnly;
    /// <summary>
    ///   Indicates that the cookie should be stored using partitioned
    ///   storage. Note that if this is set, the Secure directive must
    ///   also be set.
    /// </summary>
    property Partitioned: Boolean read FPartitioned write FPartitioned;
    property HeaderValue: String read GetHeaderValue write SetHeaderValue;
  end;

  {-------------------------------------}
  TALHttpRequestHeadersA = Class(TObject)
  protected
    function PropertyIndexToName(const AIndex: Integer): AnsiString; virtual;
    function NameToPropertyIndex(const AName: AnsiString): Integer; virtual;
    function GetCookies: TALStringsA; virtual; abstract;
    function GetUnknownHeaders: TALStringsA; virtual; abstract;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; virtual; abstract;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); virtual; abstract;
    function GetHeaderValueByName(const AName: AnsiString): AnsiString; virtual;
    procedure SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString); virtual;
    Function GetRawHeaderText: AnsiString; virtual; abstract;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); virtual; abstract;
    function GetContentCharset: AnsiString; virtual;
  public
    procedure Clear; virtual; abstract;
    property RawHeaderText: AnsiString read GetRawHeaderText write SetRawHeaderText;
    property Accept: AnsiString index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept: audio/*; q=0.2, audio/basic}
    property AcceptCharset: AnsiString index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
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
    property ContentCharset: AnsiString read GetContentCharset;
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
    property KeepAlive: AnsiString index 25 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Keep-Alive: timeout=5, max=1000}
    property LastModified: AnsiString index 26 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property MaxForwards: AnsiString index 27 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Max-Forwards: 3}
    property Pragma: AnsiString index 28 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthorization: AnsiString index 29 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Proxy-Authorization: [credentials]}
    property Range: AnsiString index 30 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Range: bytes=100-599}
    property Referer: AnsiString index 31 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    property TE: AnsiString index 32 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {TE: trailers, deflate;q=0.5}
    property Trailer: AnsiString index 33 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property Translate: AnsiString index 34 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Translate: f}
    property TransferEncoding: AnsiString index 35 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: AnsiString index 36 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property UserAgent: AnsiString index 37 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    property Via: AnsiString index 38 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: AnsiString index 39 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property Cookies: TALStringsA read GetCookies;
    property UnknownHeaders: TALStringsA read GetUnknownHeaders;
    property Values[const AName: AnsiString]: AnsiString read GetHeaderValueByName write SetHeaderValueByName; default;
  end;

  {-------------------------------------}
  TALHttpRequestHeadersW = Class(TObject)
  protected
    function PropertyIndexToName(const AIndex: Integer): String; virtual;
    function NameToPropertyIndex(const AName: String): Integer; virtual;
    function GetCookies: TALStringsW; virtual; abstract;
    function GetUnknownHeaders: TALStringsW; virtual; abstract;
    function GetHeaderValueByPropertyIndex(const Index: Integer): String; virtual; abstract;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String); virtual; abstract;
    function GetHeaderValueByName(const AName: String): String; virtual;
    procedure SetHeaderValueByName(const AName: String; const AValue: String); virtual;
    Function GetRawHeaderText: String; virtual; abstract;
    procedure SetRawHeaderText(const ARawHeaderText: String); virtual; abstract;
    function GetContentCharset: String; virtual;
  public
    procedure Clear; virtual; abstract;
    property RawHeaderText: String read GetRawHeaderText write SetRawHeaderText;
    property Accept: String index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept: audio/*; q=0.2, audio/basic}
    property AcceptCharset: String index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
    property AcceptEncoding: String index 2 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0}
    property AcceptLanguage: String index 3 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Language: da, en-gb;q=0.8, en;q=0.7}
    property Allow: String index 4 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property Authorization: String index 5 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Authorization: BASIC d2VibWFzdGVyOnpycW1hNHY=}
    property CacheControl: String index 6 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: String index 7 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: String index 8 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: String index 9 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: String index 10 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: String index 11 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: String index 12 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: String index 13 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: String index 14 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property ContentCharset: String read GetContentCharset;
    property Date: String index 15 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property Expect: String index 16 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expect: 100-continue}
    property Expires: String index 17 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property From: String index 18 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {From: webmaster@w3.org}
    property Host: String index 19 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Host: www.w3.org}
    property IfMatch: String index 20 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Match: entity_tag001}
    property IfModifiedSince: String index 21 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property IfNoneMatch: String index 22 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-None-Match: entity_tag001}
    property IfRange: String index 23 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Range: entity_tag001}
    property IfUnmodifiedSince: String index 24 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    property KeepAlive: String index 25 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Keep-Alive: timeout=5, max=1000}
    property LastModified: String index 26 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property MaxForwards: String index 27 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Max-Forwards: 3}
    property Pragma: String index 28 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthorization: String index 29 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Proxy-Authorization: [credentials]}
    property Range: String index 30 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Range: bytes=100-599}
    property Referer: String index 31 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    property TE: String index 32 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {TE: trailers, deflate;q=0.5}
    property Trailer: String index 33 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property Translate: String index 34 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Translate: f}
    property TransferEncoding: String index 35 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: String index 36 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property UserAgent: String index 37 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    property Via: String index 38 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: String index 39 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property Cookies: TALStringsW read GetCookies;
    property UnknownHeaders: TALStringsW read GetUnknownHeaders;
    property Values[const AName: String]: String read GetHeaderValueByName write SetHeaderValueByName; default;
  end;

  {--------------------------------------}
  TALHttpResponseHeadersA = Class(TObject)
  protected
    function PropertyIndexToName(const AIndex: Integer): AnsiString; virtual;
    function NameToPropertyIndex(const AName: AnsiString): Integer; virtual;
    function GetCookies: TObjectList<TALHttpCookieA>; virtual; abstract;
    function GetUnknownHeaders: TALStringsA; virtual; abstract;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; virtual; abstract;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); virtual; abstract;
    function GetHeaderValueByName(const AName: AnsiString): AnsiString; virtual;
    procedure SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString); virtual;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); virtual; abstract;
    Function GetRawHeaderText: AnsiString; virtual; abstract;
    function GetContentCharset: AnsiString; virtual;
  public
    procedure Clear; virtual; abstract;
    property AcceptRanges: AnsiString index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Ranges: bytes}
    property Age: AnsiString index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Age: 2147483648(2^31)}
    property Allow: AnsiString index 2 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property CacheControl: AnsiString index 3 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: AnsiString index 4 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: AnsiString index 5 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: AnsiString index 6 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: AnsiString index 7 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: AnsiString index 8 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: AnsiString index 9 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: AnsiString index 10 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: AnsiString index 11 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property ContentCharset: AnsiString read GetContentCharset;
    property Date: AnsiString index 12 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property ETag: AnsiString index 13 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {ETag: W/"xyzzy"}
    property Expires: AnsiString index 14 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property KeepAlive: AnsiString index 15 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Keep-Alive: timeout=5, max=1000}
    property LastModified: AnsiString index 16 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property Location: AnsiString index 17 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Location: http://www.w3.org/pub/WWW/People.html}
    property Pragma: AnsiString index 18 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthenticate: AnsiString index 19 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Proxy-Authenticate: [challenge]}
    property RetryAfter: AnsiString index 20 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Retry-After: Fri, 31 Dec 1999 23:59:59 GMT}
    property Server: AnsiString index 21 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Server: CERN/3.0 libwww/2.17}
    property Trailer: AnsiString index 22 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property TransferEncoding: AnsiString index 23 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: AnsiString index 24 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property Vary: AnsiString index 25 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Vary: Date}
    property Via: AnsiString index 26 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: AnsiString index 27 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property WWWAuthenticate: AnsiString index 28 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {WWW-Authenticate: [challenge]}
    property Cookies: TObjectList<TALHttpCookieA> read GetCookies;
    property UnknownHeaders: TALStringsA read GetUnknownHeaders;
    property Values[const AName: AnsiString]: AnsiString read GetHeaderValueByName write SetHeaderValueByName; default;
    property RawHeaderText: AnsiString read GetRawHeaderText write setRawHeaderText;
  end;

  {--------------------------------------}
  TALHttpResponseHeadersW = Class(TObject)
  protected
    function PropertyIndexToName(const AIndex: Integer): String; virtual;
    function NameToPropertyIndex(const AName: String): Integer; virtual;
    function GetCookies: TObjectList<TALHttpCookieW>; virtual; abstract;
    function GetUnknownHeaders: TALStringsW; virtual; abstract;
    function GetHeaderValueByPropertyIndex(const Index: Integer): String; virtual; abstract;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: String); virtual; abstract;
    function GetHeaderValueByName(const AName: String): String; virtual;
    procedure SetHeaderValueByName(const AName: String; const AValue: String); virtual;
    procedure SetRawHeaderText(const ARawHeaderText: String); virtual; abstract;
    Function GetRawHeaderText: String; virtual; abstract;
    function GetContentCharset: String; virtual;
  public
    procedure Clear; virtual; abstract;
    property AcceptRanges: String index 0 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Accept-Ranges: bytes}
    property Age: String index 1 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Age: 2147483648(2^31)}
    property Allow: String index 2 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Allow: GET, HEAD, PUT}
    property CacheControl: String index 3 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Cache-Control: no-cache}
    property Connection: String index 4 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Connection: close}
    property ContentEncoding: String index 5 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Encoding: gzip}
    property ContentLanguage: String index 6 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Language: mi, en}
    property ContentLength: String index 7 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Length: 3495}
    property ContentLocation: String index 8 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Location: http://localhost/page.asp}
    property ContentMD5: String index 9 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-MD5: [md5-digest]}
    property ContentRange: String index 10 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex;  {Content-Range: bytes 2543-4532/7898}
    property ContentType: String index 11 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Content-Type: text/html; charset=ISO-8859-4}
    property ContentCharset: String read GetContentCharset;
    property Date: String index 12 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    property ETag: String index 13 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {ETag: W/"xyzzy"}
    property Expires: String index 14 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    property KeepAlive: String index 15 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Keep-Alive: timeout=5, max=1000}
    property LastModified: String index 16 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    property Location: String index 17 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Location: http://www.w3.org/pub/WWW/People.html}
    property Pragma: String index 18 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Pragma: no-cache}
    property ProxyAuthenticate: String index 19 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Proxy-Authenticate: [challenge]}
    property RetryAfter: String index 20 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Retry-After: Fri, 31 Dec 1999 23:59:59 GMT}
    property Server: String index 21 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Server: CERN/3.0 libwww/2.17}
    property Trailer: String index 22 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Trailer: Date}
    property TransferEncoding: String index 23 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Transfer-Encoding: chunked}
    property Upgrade: String index 24 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    property Vary: String index 25 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Vary: Date}
    property Via: String index 26 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    property Warning: String index 27 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {Warning: 112 Disconnected Operation}
    property WWWAuthenticate: String index 28 read GetHeaderValueByPropertyIndex write SetHeaderValueByPropertyIndex; {WWW-Authenticate: [challenge]}
    property Cookies: TObjectList<TALHttpCookieW> read GetCookies;
    property UnknownHeaders: TALStringsW read GetUnknownHeaders;
    property Values[const AName: String]: String read GetHeaderValueByName write SetHeaderValueByName; default;
    property RawHeaderText: String read GetRawHeaderText write setRawHeaderText;
  end;

  {------------------------------}
  TALHTTPRequestA = Class(TObject)
  protected
    function GetVersion: TALHttpVersion; virtual; abstract;
    procedure SetVersion(const AValue: TALHttpVersion); virtual; abstract;
    function GetVerb: AnsiString; virtual; abstract;
    procedure SetVerb(const AValue: AnsiString); virtual; abstract;
    function GetRawUrl: AnsiString; virtual; abstract;
    procedure SetRawUrl(const AValue: AnsiString); virtual; abstract;
    function GetCookedUrl: TALCookedUrlA; virtual; abstract;
    function GetHeaders: TALHttpRequestHeadersA; virtual; abstract;
    function GetBodyStream: TStream; virtual; abstract;
    procedure SetBodyStream(const AValue: TStream); virtual; abstract;
    function GetOwnsBodyStream: Boolean; virtual; abstract;
    procedure SetOwnsBodyStream(const AValue: Boolean); virtual; abstract;
    function GetBodyString: AnsiString; virtual; abstract;
    procedure SetBodyString(const AValue: AnsiString); virtual; abstract;
  public
    property Version: TALHttpVersion read GetVersion write SetVersion;
    property Verb: AnsiString read GetVerb write SetVerb;
    property RawUrl: AnsiString read GetRawUrl write SetRawUrl;
    property CookedUrl: TALCookedUrlA read GetCookedUrl;
    property Headers: TALHttpRequestHeadersA read GetHeaders;
    property BodyStream: TStream read GetBodyStream write SetBodyStream;
    property BodyString: AnsiString read GetBodyString write SetBodyString;
    property OwnsBodyStream: Boolean read GetOwnsBodyStream write SetOwnsBodyStream;
  end;

  {------------------------------}
  TALHTTPRequestW = Class(TObject)
  protected
    function GetVersion: TALHttpVersion; virtual; abstract;
    procedure SetVersion(const AValue: TALHttpVersion); virtual; abstract;
    function GetVerb: String; virtual; abstract;
    procedure SetVerb(const AValue: String); virtual; abstract;
    function GetRawUrl: String; virtual; abstract;
    procedure SetRawUrl(const AValue: String); virtual; abstract;
    function GetCookedUrl: TALCookedUrlA; virtual; abstract;
    function GetHeaders: TALHttpRequestHeadersW; virtual; abstract;
    function GetBodyStream: TStream; virtual; abstract;
    procedure SetBodyStream(const AValue: TStream); virtual; abstract;
    function GetOwnsBodyStream: Boolean; virtual; abstract;
    procedure SetOwnsBodyStream(const AValue: Boolean); virtual; abstract;
    function GetBodyString: String; virtual; abstract;
    procedure SetBodyString(const AValue: String); virtual; abstract;
  public
    property Version: TALHttpVersion read GetVersion write SetVersion;
    property Verb: String read GetVerb write SetVerb;
    property RawUrl: String read GetRawUrl write SetRawUrl;
    property CookedUrl: TALCookedUrlA read GetCookedUrl;
    property Headers: TALHttpRequestHeadersW read GetHeaders;
    property BodyStream: TStream read GetBodyStream write SetBodyStream;
    property BodyString: String read GetBodyString write SetBodyString;
    property OwnsBodyStream: Boolean read GetOwnsBodyStream write SetOwnsBodyStream;
  end;

  {-------------------------------}
  TALHTTPResponseA = Class(TObject)
  protected
    function GetVersion: TALHttpVersion; virtual; abstract;
    procedure SetVersion(const AValue: TALHttpVersion); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const AValue: Integer); virtual; abstract;
    function GetReason: AnsiString; virtual; abstract;
    procedure SetReason(const AValue: AnsiString); virtual; abstract;
    function GetHeaders: TALHttpResponseHeadersA; virtual; abstract;
    function GetBodyStream: TStream; virtual; abstract;
    procedure SetBodyStream(const AValue: TStream); virtual; abstract;
    function GetOwnsBodyStream: Boolean; virtual; abstract;
    procedure SetOwnsBodyStream(const AValue: Boolean); virtual; abstract;
    function GetBodyString: AnsiString; virtual; abstract;
    procedure SetBodyString(const AValue: AnsiString); virtual; abstract;
  public
    property Version: TALHttpVersion read GetVersion write SetVersion;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property Reason: AnsiString read GetReason write SetReason;
    property Headers: TALHttpResponseHeadersA read GetHeaders;
    property BodyStream: TStream read GetBodyStream write SetBodyStream;
    property BodyString: AnsiString read GetBodyString write SetBodyString;
    property OwnsBodyStream: Boolean read GetOwnsBodyStream write SetOwnsBodyStream;
  end;

  {-------------------------------}
  TALHTTPResponseW = Class(TObject)
  protected
    function GetVersion: TALHttpVersion; virtual; abstract;
    procedure SetVersion(const AValue: TALHttpVersion); virtual; abstract;
    function GetStatusCode: Integer; virtual; abstract;
    procedure SetStatusCode(const AValue: Integer); virtual; abstract;
    function GetReason: String; virtual; abstract;
    procedure SetReason(const AValue: String); virtual; abstract;
    function GetHeaders: TALHttpResponseHeadersW; virtual; abstract;
    function GetBodyStream: TStream; virtual; abstract;
    procedure SetBodyStream(const AValue: TStream); virtual; abstract;
    function GetOwnsBodyStream: Boolean; virtual; abstract;
    procedure SetOwnsBodyStream(const AValue: Boolean); virtual; abstract;
    function GetBodyString: String; virtual; abstract;
    procedure SetBodyString(const AValue: String); virtual; abstract;
  public
    property Version: TALHttpVersion read GetVersion write SetVersion;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property Reason: String read GetReason write SetReason;
    property Headers: TALHttpResponseHeadersW read GetHeaders;
    property BodyStream: TStream read GetBodyStream write SetBodyStream;
    property BodyString: String read GetBodyString write SetBodyString;
    property OwnsBodyStream: Boolean read GetOwnsBodyStream write SetOwnsBodyStream;
  end;

const
  AlRfc822DayOfWeekNamesA: array[1..7] of AnsiString = (
    'Sun',
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat');
  AlRfc822DayOfWeekNamesW: array[1..7] of String = (
    'Sun',
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat');

  ALRfc822MonthOfTheYearNamesA: array[1..12] of AnsiString = (
    'Jan',
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
  ALRfc822MonthOfTheYearNamesW: array[1..12] of String = (
    'Jan',
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

function ALUtcDateTimeToRfc1123StrA(const aValue: TDateTime): AnsiString;
function ALUtcDateTimeToRfc1123StrW(const aValue: TDateTime): String;
function ALLocalDateTimeToRfc1123StrA(const aValue: TDateTime): AnsiString;
function ALLocalDateTimeToRfc1123StrW(const aValue: TDateTime): String;
function ALTryRfc822DateStrToUtcDateTime(const S: AnsiString; out Value: TDateTime): Boolean; overload;
function ALTryRfc822DateStrToUtcDateTime(const S: String; out Value: TDateTime): Boolean; overload;
function ALRfc822DateStrToUtcDateTime(const S: AnsiString): TDateTime; overload;
function ALRfc822DateStrToUtcDateTime(const S: String): TDateTime; overload;
function ALGetHttpReasonPhraseA(Const AStatusCode: Integer): Ansistring;
function ALGetHttpReasonPhraseW(Const AStatusCode: Integer): String;
procedure ALDecompressHttpResponseBody(const AContentEncoding: AnsiString; var ABodyStream: TMemoryStream); overload;
procedure ALDecompressHttpResponseBody(const AContentEncoding: String; var ABodyStream: TMemoryStream); overload;

implementation

uses
  {$if not defined(ALHttpGzipAuto)}
  System.zlib,
  {$endif}
  System.AnsiStrings,
  System.SysUtils,
  System.DateUtils,
  System.SysConst,
  Alcinoe.Common;

{********************************}
constructor TALHttpCookieA.Create;
begin
  inherited Create;
  Reset;
end;

{*****************************}
procedure TALHttpCookieA.Reset;
begin
  FName := '';
  FValue := '';
  FPath := '';
  FDomain := '';
  FExpires := ALNullDate;
  FMaxAge := MaxAgeUnset;
  FSameSite := '';
  FSecure := False;
  FHttpOnly := False;
  FPartitioned := False;
end;

{*************************************************}
function TALHttpCookieA.GetHeaderValue: AnsiString;
begin

  // Most cookies (88%) are <100 bytes, reflecting simple session IDs or tracking tokens
  var SB := TALStringBuilderA.Create(256);
  try

    SB.Append(FName);
    SB.Append('=');
    SB.Append(FValue);

    if Domain <> '' then begin
      SB.Append('; Domain=');
      SB.Append(Domain);
    end;

    if Path <> '' then begin
      SB.Append('; Path=');
      SB.Append(Path);
    end;

    if Expires <> ALNullDate then begin
      // Expires=day-name, DD Mon YYYY HH:MM:SS GMT
      var LYear, LMonth, LDay, LHour, LMin, LSec, LMSec: Word;
      DecodeDate(Expires, LYear, LMonth, LDay);
      DecodeTime(Expires, LHour, LMin, LSec, LMSec);
      SB.Append('; Expires=');
      SB.Append(AlRfc822DayOfWeekNamesA[DayOfWeek(Expires)]);
      SB.Append(', ');
      if LDay <= 9 then SB.Append('0'); SB.Append(ALIntToStrA(LDay));
      SB.Append(' ');
      SB.Append(ALRfc822MonthOfTheYearNamesA[LMonth]);
      SB.Append(' ');
      SB.Append(ALIntToStrA(LYear));
      SB.Append(' ');
      if LHour <= 9 then SB.Append('0'); SB.Append(ALIntToStrA(LHour));
      SB.Append(':');
      if LMin <= 9 then SB.Append('0'); SB.Append(ALIntToStrA(LMin));
      SB.Append(':');
      if LSec <= 9 then SB.Append('0'); SB.Append(ALIntToStrA(LSec));
      SB.Append(' GMT');
    end;

    if MaxAge <> MaxAgeUnset then begin
      SB.Append('; Max-Age=');
      SB.Append(ALIntToStrA(MaxAge));
    end;

    if SameSite <> '' then begin
      SB.Append('; SameSite=');
      SB.Append(SameSite);
    end;

    if Secure then
      SB.Append('; Secure');

    if HttpOnly then
      SB.Append('; HttpOnly');

    if Partitioned then
      SB.Append('; Partitioned');

    Result := SB.ToString(true{AUpdateCapacity});

  finally
    ALFreeAndNil(SB);
  end;

end;

{*****************}
//exemple of value:
// LSID=DQAAAK…Eaem_vYg; Domain=docs.foo.com; Path=/accounts; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
// HSID=AYQEVn….DKrdst; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; HttpOnly
// SSID=Ap4P….GTEq; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
procedure TALHttpCookieA.SetHeaderValue(Const aValue: AnsiString);
begin
  var LCookieParams := TALNVStringListA.Create;
  try
    ALExtractHeaderFields(
      [';'], // const ASeparators: TSysCharSet;,
      [' ', #9], // const AWhiteSpace: TSysCharSet;
      [], // const AQuoteChars: TSysCharSet;
      PAnsiChar(aValue), // const AContent: PAnsiChar;
      LCookieParams); // const AStrings: TALStringsA;
    FName := LCookieParams.Names[0];
    FValue := LCookieParams.ValueFromIndex[0];
    FPath := LCookieParams.values['Path'];
    FDomain := LCookieParams.values['Domain'];
    if not ALTryRfc822DateStrToUtcDateTime(LCookieParams.values['Expires'], FExpires) then FExpires := ALNullDate;
    If not ALTryStrToInt(LCookieParams.values['Max-Age'], FMaxAge) then FMaxAge := MaxAgeUnset;
    FSameSite := LCookieParams.values['SameSite'];
    FSecure := LCookieParams.IndexOfName('Secure') <> -1;
    FHttpOnly := LCookieParams.IndexOfName('HttpOnly') <> -1;
    FPartitioned := LCookieParams.IndexOfName('Partitioned') <> -1;
  finally
    AlFreeAndNil(LCookieParams);
  end;
end;

{********************************}
constructor TALHttpCookieW.Create;
begin
  inherited Create;
  Reset;
end;

{*****************************}
procedure TALHttpCookieW.Reset;
begin
  FName := '';
  FValue := '';
  FPath := '';
  FDomain := '';
  FExpires := ALNullDate;
  FMaxAge := MaxAgeUnset;
  FSameSite := '';
  FSecure := False;
  FHttpOnly := False;
  FPartitioned := False;
end;

{*********************************************}
function TALHttpCookieW.GetHeaderValue: String;
begin

  // Most cookies (88%) are <100 bytes, reflecting simple session IDs or tracking tokens
  var SB := TALStringBuilderW.Create(256);
  try

    SB.Append(FName);
    SB.Append('=');
    SB.Append(FValue);

    if Domain <> '' then begin
      SB.Append('; Domain=');
      SB.Append(Domain);
    end;

    if Path <> '' then begin
      SB.Append('; Path=');
      SB.Append(Path);
    end;

    if Expires <> ALNullDate then begin
      // Expires=day-name, DD Mon YYYY HH:MM:SS GMT
      var LYear, LMonth, LDay, LHour, LMin, LSec, LMSec: Word;
      DecodeDate(Expires, LYear, LMonth, LDay);
      DecodeTime(Expires, LHour, LMin, LSec, LMSec);
      SB.Append('; Expires=');
      SB.Append(AlRfc822DayOfWeekNamesW[DayOfWeek(Expires)]);
      SB.Append(', ');
      if LDay <= 9 then SB.Append('0'); SB.Append(ALIntToStrW(LDay));
      SB.Append(' ');
      SB.Append(ALRfc822MonthOfTheYearNamesW[LMonth]);
      SB.Append(' ');
      SB.Append(ALIntToStrW(LYear));
      SB.Append(' ');
      if LHour <= 9 then SB.Append('0'); SB.Append(ALIntToStrW(LHour));
      SB.Append(':');
      if LMin <= 9 then SB.Append('0'); SB.Append(ALIntToStrW(LMin));
      SB.Append(':');
      if LSec <= 9 then SB.Append('0'); SB.Append(ALIntToStrW(LSec));
      SB.Append(' GMT');
    end;

    if MaxAge <> MaxAgeUnset then begin
      SB.Append('; Max-Age=');
      SB.Append(ALIntToStrW(MaxAge));
    end;

    if SameSite <> '' then begin
      SB.Append('; SameSite=');
      SB.Append(SameSite);
    end;

    if Secure then
      SB.Append('; Secure');

    if HttpOnly then
      SB.Append('; HttpOnly');

    if Partitioned then
      SB.Append('; Partitioned');

    Result := SB.ToString(true{AUpdateCapacity});

  finally
    ALFreeAndNil(SB);
  end;

end;

{*****************}
//exemple of value:
// LSID=DQAAAK…Eaem_vYg; Domain=docs.foo.com; Path=/accounts; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
// HSID=AYQEVn….DKrdst; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; HttpOnly
// SSID=Ap4P….GTEq; Domain=.foo.com; Path=/; Expires=Wed, 13-Jan-2021 22:23:01 GMT; Secure; HttpOnly
procedure TALHttpCookieW.SetHeaderValue(Const aValue: String);
begin
  var LCookieParams := TALNVStringListW.Create;
  try
    ALExtractHeaderFields(
      [';'], // const ASeparators: TSysCharSet;,
      [' ', #9], // const AWhiteSpace: TSysCharSet;
      [], // const AQuoteChars: TSysCharSet;
      PChar(aValue), // const AContent: PChar;
      LCookieParams); // const AStrings: TALStringsW;
    FName := LCookieParams.Names[0];
    FValue := LCookieParams.ValueFromIndex[0];
    FPath := LCookieParams.values['Path'];
    FDomain := LCookieParams.values['Domain'];
    if not ALTryRfc822DateStrToUtcDateTime(LCookieParams.values['Expires'], FExpires) then FExpires := ALNullDate;
    If not ALTryStrToInt(LCookieParams.values['Max-Age'], FMaxAge) then FMaxAge := MaxAgeUnset;
    FSameSite := LCookieParams.values['SameSite'];
    FSecure := LCookieParams.IndexOfName('Secure') <> -1;
    FHttpOnly := LCookieParams.IndexOfName('HttpOnly') <> -1;
    FPartitioned := LCookieParams.IndexOfName('Partitioned') <> -1;
  finally
    AlFreeAndNil(LCookieParams);
  end;
end;

{*************************************************************************************}
function TALHttpRequestHeadersA.PropertyIndexToName(const AIndex: Integer): AnsiString;
begin
  Case AIndex of
    0: Result := 'Accept';
    1: Result := 'Accept-Charset';
    2: Result := 'Accept-Encoding';
    3: Result := 'Accept-Language';
    4: Result := 'Allow';
    5: Result := 'Authorization';
    6: Result := 'Cache-Control';
    7: Result := 'Connection';
    8: Result := 'Content-Encoding';
    9: Result := 'Content-Language';
    10: Result := 'Content-Length';
    11: Result := 'Content-Location';
    12: Result := 'Content-MD5';
    13: Result := 'Content-Range';
    14: Result := 'Content-Type';
    15: Result := 'Date';
    16: Result := 'Expect';
    17: Result := 'Expires';
    18: Result := 'From';
    19: Result := 'Host';
    20: Result := 'If-Match';
    21: Result := 'If-Modified-Since';
    22: Result := 'If-None-Match';
    23: Result := 'If-Range';
    24: Result := 'If-Unmodified-Since';
    25: Result := 'Keep-Alive';
    26: Result := 'Last-Modified';
    27: Result := 'Max-Forwards';
    28: Result := 'Pragma';
    29: Result := 'Proxy-Authorization';
    30: Result := 'Range';
    31: Result := 'Referer';
    32: Result := 'TE';
    33: Result := 'Trailer';
    34: Result := 'Translate';
    35: Result := 'Transfer-Encoding';
    36: Result := 'Upgrade';
    37: Result := 'User-Agent';
    38: Result := 'Via';
    39: Result := 'Warning';
    else
      Raise Exception.Create('Error 8FA2DFE9-805F-4547-8CC1-07B853445437')
  End;
end;

{************************************************************************************}
function TALHttpRequestHeadersA.NameToPropertyIndex(const AName: AnsiString): Integer;
begin
  var LLowerName := ALLowerCase(AName);
       if LLowerName = 'accept' then Result := 0
  else if LLowerName = 'accept-charset' then Result := 1
  else if LLowerName = 'accept-encoding' then Result := 2
  else if LLowerName = 'accept-language' then Result := 3
  else if LLowerName = 'allow' then Result := 4
  else if LLowerName = 'authorization' then Result := 5
  else if LLowerName = 'cache-control' then Result := 6
  else if LLowerName = 'connection' then Result := 7
  else if LLowerName = 'content-encoding' then Result := 8
  else if LLowerName = 'content-language' then Result := 9
  else if LLowerName = 'content-length' then Result := 10
  else if LLowerName = 'content-location' then Result := 11
  else if LLowerName = 'content-md5' then Result := 12
  else if LLowerName = 'content-range' then Result := 13
  else if LLowerName = 'content-type' then Result := 14
  else if LLowerName = 'date' then Result := 15
  else if LLowerName = 'expect' then Result := 16
  else if LLowerName = 'expires' then Result := 17
  else if LLowerName = 'from' then Result := 18
  else if LLowerName = 'host' then Result := 19
  else if LLowerName = 'if-match' then Result := 20
  else if LLowerName = 'if-modified-since' then Result := 21
  else if LLowerName = 'if-none-match' then Result := 22
  else if LLowerName = 'if-range' then Result := 23
  else if LLowerName = 'if-unmodified-since' then Result := 24
  else if LLowerName = 'keep-alive' then Result := 25
  else if LLowerName = 'last-modified' then Result := 26
  else if LLowerName = 'max-forwards' then Result := 27
  else if LLowerName = 'pragma' then Result := 28
  else if LLowerName = 'proxy-authorization' then Result := 29
  else if LLowerName = 'range' then Result := 30
  else if LLowerName = 'referer' then Result := 31
  else if LLowerName = 'te' then Result := 32
  else if LLowerName = 'trailer' then Result := 33
  else if LLowerName = 'translate' then Result := 34
  else if LLowerName = 'transfer-encoding' then Result := 35
  else if LLowerName = 'upgrade' then Result := 36
  else if LLowerName = 'user-agent' then Result := 37
  else if LLowerName = 'via' then Result := 38
  else if LLowerName = 'warning' then Result := 39
  else Result := -1;
end;

{****************************************************************************************}
function TALHttpRequestHeadersA.GetHeaderValueByName(const AName: AnsiString): AnsiString;
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then Result := GetHeaderValueByPropertyIndex(LIndex)
  else if ALSameTextA(AName, 'Cookie') then Result := Cookies.Text
  else Result := UnknownHeaders.Values[AName];
end;

{*******************************************************************************************************}
procedure TALHttpRequestHeadersA.SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString);
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then
    SetHeaderValueByPropertyIndex(LIndex, AValue)
  else if ALSameTextA(AName, 'Cookie') then
    ALExtractHeaderFields(
      [';'], // const ASeparators: TSysCharSet;,
      [' ', #9], // const AWhiteSpace: TSysCharSet;
      [], // const AQuoteChars: TSysCharSet;
      PAnsiChar(AValue), // const AContent: PAnsiChar;
      Cookies) // const AStrings: TALStringsA;
  else
    UnknownHeaders.Values[AName] := AValue;
end;

{************************************************************}
function TALHttpRequestHeadersA.GetContentCharset: AnsiString;
begin
  Result := ALExtractHeaderParamValue(ContentType, AnsiString('charset'));
end;

{*********************************************************************************}
function TALHttpRequestHeadersW.PropertyIndexToName(const AIndex: Integer): String;
begin
  Case AIndex of
    0: Result := 'Accept';
    1: Result := 'Accept-Charset';
    2: Result := 'Accept-Encoding';
    3: Result := 'Accept-Language';
    4: Result := 'Allow';
    5: Result := 'Authorization';
    6: Result := 'Cache-Control';
    7: Result := 'Connection';
    8: Result := 'Content-Encoding';
    9: Result := 'Content-Language';
    10: Result := 'Content-Length';
    11: Result := 'Content-Location';
    12: Result := 'Content-MD5';
    13: Result := 'Content-Range';
    14: Result := 'Content-Type';
    15: Result := 'Date';
    16: Result := 'Expect';
    17: Result := 'Expires';
    18: Result := 'From';
    19: Result := 'Host';
    20: Result := 'If-Match';
    21: Result := 'If-Modified-Since';
    22: Result := 'If-None-Match';
    23: Result := 'If-Range';
    24: Result := 'If-Unmodified-Since';
    25: Result := 'Keep-Alive';
    26: Result := 'Last-Modified';
    27: Result := 'Max-Forwards';
    28: Result := 'Pragma';
    29: Result := 'Proxy-Authorization';
    30: Result := 'Range';
    31: Result := 'Referer';
    32: Result := 'TE';
    33: Result := 'Trailer';
    34: Result := 'Translate';
    35: Result := 'Transfer-Encoding';
    36: Result := 'Upgrade';
    37: Result := 'User-Agent';
    38: Result := 'Via';
    39: Result := 'Warning';
    else
      Raise Exception.Create('Error 8FA2DFE9-805F-4547-8CC1-07B853445437')
  End;
end;

{********************************************************************************}
function TALHttpRequestHeadersW.NameToPropertyIndex(const AName: String): Integer;
begin
  var LLowerName := ALLowerCase(AName);
       if LLowerName = 'accept' then Result := 0
  else if LLowerName = 'accept-charset' then Result := 1
  else if LLowerName = 'accept-encoding' then Result := 2
  else if LLowerName = 'accept-language' then Result := 3
  else if LLowerName = 'allow' then Result := 4
  else if LLowerName = 'authorization' then Result := 5
  else if LLowerName = 'cache-control' then Result := 6
  else if LLowerName = 'connection' then Result := 7
  else if LLowerName = 'content-encoding' then Result := 8
  else if LLowerName = 'content-language' then Result := 9
  else if LLowerName = 'content-length' then Result := 10
  else if LLowerName = 'content-location' then Result := 11
  else if LLowerName = 'content-md5' then Result := 12
  else if LLowerName = 'content-range' then Result := 13
  else if LLowerName = 'content-type' then Result := 14
  else if LLowerName = 'date' then Result := 15
  else if LLowerName = 'expect' then Result := 16
  else if LLowerName = 'expires' then Result := 17
  else if LLowerName = 'from' then Result := 18
  else if LLowerName = 'host' then Result := 19
  else if LLowerName = 'if-match' then Result := 20
  else if LLowerName = 'if-modified-since' then Result := 21
  else if LLowerName = 'if-none-match' then Result := 22
  else if LLowerName = 'if-range' then Result := 23
  else if LLowerName = 'if-unmodified-since' then Result := 24
  else if LLowerName = 'keep-alive' then Result := 25
  else if LLowerName = 'last-modified' then Result := 26
  else if LLowerName = 'max-forwards' then Result := 27
  else if LLowerName = 'pragma' then Result := 28
  else if LLowerName = 'proxy-authorization' then Result := 29
  else if LLowerName = 'range' then Result := 30
  else if LLowerName = 'referer' then Result := 31
  else if LLowerName = 'te' then Result := 32
  else if LLowerName = 'trailer' then Result := 33
  else if LLowerName = 'translate' then Result := 34
  else if LLowerName = 'transfer-encoding' then Result := 35
  else if LLowerName = 'upgrade' then Result := 36
  else if LLowerName = 'user-agent' then Result := 37
  else if LLowerName = 'via' then Result := 38
  else if LLowerName = 'warning' then Result := 39
  else Result := -1;
end;

{********************************************************************************}
function TALHttpRequestHeadersW.GetHeaderValueByName(const AName: String): String;
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then Result := GetHeaderValueByPropertyIndex(LIndex)
  else if ALSameTextW(AName, 'Cookie') then Result := Cookies.Text
  else Result := UnknownHeaders.Values[AName];
end;

{***********************************************************************************************}
procedure TALHttpRequestHeadersW.SetHeaderValueByName(const AName: String; const AValue: String);
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then
    SetHeaderValueByPropertyIndex(LIndex, AValue)
  else if ALSameTextW(AName, 'Cookie') then
    ALExtractHeaderFields(
      [';'], // const ASeparators: TSysCharSet;,
      [' ', #9], // const AWhiteSpace: TSysCharSet;
      [], // const AQuoteChars: TSysCharSet;
      PChar(AValue), // const AContent: PChar;
      Cookies) // const AStrings: TALStringsW;
  else
    UnknownHeaders.Values[AName] := AValue;
end;

{********************************************************}
function TALHttpRequestHeadersW.GetContentCharset: String;
begin
  Result := ALExtractHeaderParamValue(ContentType, String('charset'));
end;

{**************************************************************************************}
function TALHttpResponseHeadersA.PropertyIndexToName(const AIndex: Integer): AnsiString;
begin
  Case AIndex of
    0: Result := 'Accept-Ranges';
    1: Result := 'Age';
    2: Result := 'Allow';
    3: Result := 'Cache-Control';
    4: Result := 'Connection';
    5: Result := 'Content-Encoding';
    6: Result := 'Content-Language';
    7: Result := 'Content-Length';
    8: Result := 'Content-Location';
    9: Result := 'Content-MD5';
    10: Result := 'Content-Range';
    11: Result := 'Content-Type';
    12: Result := 'Date';
    13: Result := 'ETag';
    14: Result := 'Expires';
    15: Result := 'Keep-Alive';
    16: Result := 'Last-Modified';
    17: Result := 'Location';
    18: Result := 'Pragma';
    19: Result := 'Proxy-Authenticate';
    20: Result := 'Retry-After';
    21: Result := 'Server';
    22: Result := 'Trailer';
    23: Result := 'Transfer-Encoding';
    24: Result := 'Upgrade';
    25: Result := 'Vary';
    26: Result := 'Via';
    27: Result := 'Warning';
    28: Result := 'WWW-Authenticate';
    else
      Raise Exception.Create('Error 83797A2F-ED2E-421F-8189-82242B253E11');
  End;
end;

{*************************************************************************************}
function TALHttpResponseHeadersA.NameToPropertyIndex(const AName: AnsiString): Integer;
begin
  var LLowerName := ALLowerCase(AName);
       if LLowerName = 'accept-ranges' then Result := 0
  else if LLowerName = 'age' then Result := 1
  else if LLowerName = 'allow' then Result := 2
  else if LLowerName = 'cache-control' then Result := 3
  else if LLowerName = 'connection' then Result := 4
  else if LLowerName = 'content-encoding' then Result := 5
  else if LLowerName = 'content-language' then Result := 6
  else if LLowerName = 'content-length' then Result := 7
  else if LLowerName = 'content-location' then Result := 8
  else if LLowerName = 'content-md5' then Result := 9
  else if LLowerName = 'content-range' then Result := 10
  else if LLowerName = 'content-type' then Result := 11
  else if LLowerName = 'date' then Result := 12
  else if LLowerName = 'etag' then Result := 13
  else if LLowerName = 'expires' then Result := 14
  else if LLowerName = 'keep-alive' then Result := 15
  else if LLowerName = 'last-modified' then Result := 16
  else if LLowerName = 'location' then Result := 17
  else if LLowerName = 'pragma' then Result := 18
  else if LLowerName = 'proxy-authenticate' then Result := 19
  else if LLowerName = 'retry-after' then Result := 20
  else if LLowerName = 'server' then Result := 21
  else if LLowerName = 'trailer' then Result := 22
  else if LLowerName = 'transfer-encoding' then Result := 23
  else if LLowerName = 'upgrade' then Result := 24
  else if LLowerName = 'vary' then Result := 25
  else if LLowerName = 'via' then Result := 26
  else if LLowerName = 'warning' then Result := 27
  else if LLowerName = 'www-authenticate' then Result := 28
  else Result := -1;
end;

{*****************************************************************************************}
function TALHttpResponseHeadersA.GetHeaderValueByName(const AName: AnsiString): AnsiString;
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then Result := GetHeaderValueByPropertyIndex(LIndex)
  else if ALSameTextA(AName, 'Set-Cookie') then begin
    If Cookies.Count > 0 then Result := Cookies[0].HeaderValue
    else Result := '';
  end
  else Result := UnknownHeaders.Values[AName];
end;

{********************************************************************************************************}
procedure TALHttpResponseHeadersA.SetHeaderValueByName(const AName: AnsiString; const AValue: AnsiString);
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then
    SetHeaderValueByPropertyIndex(LIndex, AValue)
  else if ALSameTextA(AName, 'Set-Cookie') then begin
    If Cookies.Count > 0 then Cookies[0].HeaderValue := AValue
    else begin
      var LCookie := TALHttpCookieA.Create;
      LCookie.HeaderValue := AValue;
      Cookies.Add(LCookie);
    end;
  end
  else
    UnknownHeaders.Values[AName] := AValue;
end;

{*************************************************************}
function TALHttpResponseHeadersA.GetContentCharset: AnsiString;
begin
  Result := ALExtractHeaderParamValue(ContentType, AnsiString('charset'));
end;

{**********************************************************************************}
function TALHttpResponseHeadersW.PropertyIndexToName(const AIndex: Integer): String;
begin
  Case AIndex of
    0: Result := 'Accept-Ranges';
    1: Result := 'Age';
    2: Result := 'Allow';
    3: Result := 'Cache-Control';
    4: Result := 'Connection';
    5: Result := 'Content-Encoding';
    6: Result := 'Content-Language';
    7: Result := 'Content-Length';
    8: Result := 'Content-Location';
    9: Result := 'Content-MD5';
    10: Result := 'Content-Range';
    11: Result := 'Content-Type';
    12: Result := 'Date';
    13: Result := 'ETag';
    14: Result := 'Expires';
    15: Result := 'Keep-Alive';
    16: Result := 'Last-Modified';
    17: Result := 'Location';
    18: Result := 'Pragma';
    19: Result := 'Proxy-Authenticate';
    20: Result := 'Retry-After';
    21: Result := 'Server';
    22: Result := 'Trailer';
    23: Result := 'Transfer-Encoding';
    24: Result := 'Upgrade';
    25: Result := 'Vary';
    26: Result := 'Via';
    27: Result := 'Warning';
    28: Result := 'WWW-Authenticate';
    else
      Raise Exception.Create('Error 83797A2F-ED2E-421F-8189-82242B253E11');
  End;
end;

{*********************************************************************************}
function TALHttpResponseHeadersW.NameToPropertyIndex(const AName: String): Integer;
begin
  var LLowerName := ALLowerCase(AName);
       if LLowerName = 'accept-ranges' then Result := 0
  else if LLowerName = 'age' then Result := 1
  else if LLowerName = 'allow' then Result := 2
  else if LLowerName = 'cache-control' then Result := 3
  else if LLowerName = 'connection' then Result := 4
  else if LLowerName = 'content-encoding' then Result := 5
  else if LLowerName = 'content-language' then Result := 6
  else if LLowerName = 'content-length' then Result := 7
  else if LLowerName = 'content-location' then Result := 8
  else if LLowerName = 'content-md5' then Result := 9
  else if LLowerName = 'content-range' then Result := 10
  else if LLowerName = 'content-type' then Result := 11
  else if LLowerName = 'date' then Result := 12
  else if LLowerName = 'etag' then Result := 13
  else if LLowerName = 'expires' then Result := 14
  else if LLowerName = 'keep-alive' then Result := 15
  else if LLowerName = 'last-modified' then Result := 16
  else if LLowerName = 'location' then Result := 17
  else if LLowerName = 'pragma' then Result := 18
  else if LLowerName = 'proxy-authenticate' then Result := 19
  else if LLowerName = 'retry-after' then Result := 20
  else if LLowerName = 'server' then Result := 21
  else if LLowerName = 'trailer' then Result := 22
  else if LLowerName = 'transfer-encoding' then Result := 23
  else if LLowerName = 'upgrade' then Result := 24
  else if LLowerName = 'vary' then Result := 25
  else if LLowerName = 'via' then Result := 26
  else if LLowerName = 'warning' then Result := 27
  else if LLowerName = 'www-authenticate' then Result := 28
  else Result := -1;
end;

{*********************************************************************************}
function TALHttpResponseHeadersW.GetHeaderValueByName(const AName: String): String;
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then Result := GetHeaderValueByPropertyIndex(LIndex)
  else if ALSameTextW(AName, 'Set-Cookie') then begin
    If Cookies.Count > 0 then Result := Cookies[0].HeaderValue
    else Result := '';
  end
  else Result := UnknownHeaders.Values[AName];
end;

{************************************************************************************************}
procedure TALHttpResponseHeadersW.SetHeaderValueByName(const AName: String; const AValue: String);
begin
  Var LIndex := NameToPropertyIndex(AName);
  If LIndex >= 0 then
    SetHeaderValueByPropertyIndex(LIndex, AValue)
  else if ALSameTextW(AName, 'Set-Cookie') then begin
    If Cookies.Count > 0 then Cookies[0].HeaderValue := AValue
    else begin
      var LCookie := TALHttpCookieW.Create;
      LCookie.HeaderValue := AValue;
      Cookies.Add(LCookie);
    end;
  end
  else
    UnknownHeaders.Values[AName] := AValue;
end;

{*********************************************************}
function TALHttpResponseHeadersW.GetContentCharset: String;
begin
  Result := ALExtractHeaderParamValue(ContentType, String('charset'));
end;

{***********}
/// <summary>
/// Formats a UTC (GMT) TDateTime as an RFC 1123/HTTP-date string
/// using English weekday/month abbreviations and a fixed "GMT" suffix,
/// e.g. "Sun, 06 Nov 1994 08:49:37 GMT". The input must already be UTC;
/// no time zone conversion is performed (milliseconds are ignored).
/// </summary>
function ALUtcDateTimeToRfc1123StrA(const aValue: TDateTime): AnsiString;
begin
  var LYear, LMonth, LDay, LHour, LMin, LSec, LMSec: Word;
  DecodeDate(aValue, LYear, LMonth, LDay);
  DecodeTime(aValue, LHour, LMin, LSec, LMSec);
  Result := ALFormatA(
              '%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
              [AlRfc822DayOfWeekNamesA[DayOfWeek(aValue)],
               LDay,
               ALRfc822MonthOfTheYearNamesA[LMonth],
               LYear,
               LHour,
               LMin,
               LSec]);
end;

{***********}
/// <summary>
/// Formats a UTC (GMT) TDateTime as an RFC 1123/HTTP-date string
/// using English weekday/month abbreviations and a fixed "GMT" suffix,
/// e.g. "Sun, 06 Nov 1994 08:49:37 GMT". The input must already be UTC;
/// no time zone conversion is performed (milliseconds are ignored).
/// </summary>
function ALUtcDateTimeToRfc1123StrW(const aValue: TDateTime): String;
begin
  var LYear, LMonth, LDay, LHour, LMin, LSec, LMSec: Word;
  DecodeDate(aValue, LYear, LMonth, LDay);
  DecodeTime(aValue, LHour, LMin, LSec, LMSec);
  Result := ALFormatW(
              '%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
              [AlRfc822DayOfWeekNamesW[DayOfWeek(aValue)],
               LDay,
               ALRfc822MonthOfTheYearNamesW[LMonth],
               LYear,
               LHour,
               LMin,
               LSec]);
end;

{***********}
/// <summary>
/// Converts a local TDateTime to an RFC 1123/HTTP-date string by first
/// converting to UTC (via AlLocalDateTimeToUTC) and formatting as
/// "Wdy, DD Mon YYYY HH:MM:SS GMT". Uses OS time zone/DST rules; milliseconds
/// are ignored.
/// </summary>
function ALLocalDateTimeToRfc1123StrA(const aValue: TDateTime): AnsiString;
begin
  Result := ALUtcDateTimeToRfc1123StrA(AlLocalDateTimeToUTC(aValue));
end;

{***********}
/// <summary>
/// Converts a local TDateTime to an RFC 1123/HTTP-date string by first
/// converting to UTC (via AlLocalDateTimeToUTC) and formatting as
/// "Wdy, DD Mon YYYY HH:MM:SS GMT". Uses OS time zone/DST rules; milliseconds
/// are ignored.
/// </summary>
function ALLocalDateTimeToRfc1123StrW(const aValue: TDateTime): String;
begin
  Result := ALUtcDateTimeToRfc1123StrW(AlLocalDateTimeToUTC(aValue));
end;

{**************************************************************}
// Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
// the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
// to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)
//
// The "Date" line (formerly "Posted") is the date that the message was
// originally posted to the network.  Its format must be acceptable
// both in RFC-822 and to the getdate(3) routine that is provided with
// the Usenet software.  This date remains unchanged as the message is
// propagated throughout the network.  One format that is acceptable to
// both is:
//
//                    Wdy, DD Mon YY HH:MM:SS TIMEZONE
//
// Several examples of valid dates appear in the sample message above.
// Note in particular that ctime(3) format:
//
//                    Wdy Mon DD HH:MM:SS YYYY
//
//  is not acceptable because it is not a valid RFC-822 date. However,
//  since older software still generates this format, news
//  implementations are encouraged to accept this format and translate
//  it into an acceptable format.
//
//  There is no hope of having a complete list of timezones.  Universal
//  Time (GMT), the North American timezones (PST, PDT, MST, MDT, CST,
//  CDT, EST, EDT) and the +/-hhmm offset specifed in RFC-822 should be
//  supported.  It is recommended that times in message headers be
//  transmitted in GMT and displayed in the local time zone.
Function ALTryRfc822DateStrToUtcDateTime(const S: AnsiString; out Value: TDateTime): Boolean;
Begin

  var LDateStr := S; // Wdy, DD-Mon-YYYY HH:MM:SS GMT
                     // Wdy, DD-Mon-YYYY HH:MM:SS +0200
                     // 23 Aug 2004 06:48:46 -0700
  var P1 := ALPosA(',',LDateStr);
  If P1 > 0 then delete(LDateStr,1,P1); // DD-Mon-YYYY HH:MM:SS GMT
                                        // DD-Mon-YYYY HH:MM:SS +0200
                                        // 23 Aug 2004 06:48:46 -0700
  LDateStr := ALTrim(LDateStr); // DD-Mon-YYYY HH:MM:SS GMT
                                // DD-Mon-YYYY HH:MM:SS +0200
                                // 23 Aug 2004 06:48:46 -0700

  P1 := ALPosA(':',LDateStr);
  var P2 := ALPosA('-',LDateStr);
  While (P2 > 0) and (P2 < P1) do begin
    LDateStr[P2] := ' ';
    P2 := ALPosA('-',LDateStr,P2);
  end; // DD Mon YYYY HH:MM:SS GMT
       // DD Mon YYYY HH:MM:SS +0200
       // 23 Aug 2004 06:48:46 -0700
  While ALPosA('  ',LDateStr) > 0 do
    LDateStr := ALStringReplaceA(LDateStr,'  ',' ',[RfReplaceAll]); // DD Mon YYYY HH:MM:SS GMT
                                                                    // DD Mon YYYY HH:MM:SS +0200
                                                                    // 23 Aug 2004 06:48:46 -0700

  var LDateTimeLst := TALStringListA.create;
  Try

    LDateTimeLst.LineBreak := ' ';
    LDateTimeLst.Text := LDateStr;
    If LDateTimeLst.Count < 5 then begin
      Result := False;
      Exit;
    end;

    var LMonthLabel := LDateTimeLst[1]; // Mon
                                        // Mon
                                        // Aug
    var LMonth: Word := 1;
    While (LMonth <= 12) and (not ALSameTextA(ALRfc822MonthOfTheYearNamesA[LMonth],LMonthLabel)) do inc(LMonth);
    If LMonth > 12 then begin
      Result := False;
      Exit;
    end;

    var LTimeZoneDelta: TDateTime;
    var LTimeZoneStr := LDateTimeLst[4]; // GMT
                                         // +0200
                                         // -0700
    If (Length(LTimeZoneStr) >= 5) and
       (LTimeZoneStr[1] in ['+','-']) and
       (LTimeZoneStr[2] in ['0'..'9']) and
       (LTimeZoneStr[3] in ['0'..'9']) and
       (LTimeZoneStr[4] in ['0'..'9']) and
       (LTimeZoneStr[5] in ['0'..'9']) then begin
      var LHour: Integer;
      var LMin: Integer;
      If (not ALTryStrToInt(AlCopyStr(LTimeZoneStr,2,2), LHour)) or
         (not ALTryStrToInt(AlCopyStr(LTimeZoneStr,4,2), LMin)) or
         (not TryEncodeTime(Word(LHour){Hour}, Word(LMin){Min}, 0{Sec}, 0{MSec}, LTimeZoneDelta)) then begin
        Result := False;
        Exit;
      end;
      if LTimeZoneStr[1] = '+' then LTimeZoneDelta := -1*LTimeZoneDelta;
    end
    else If ALSameTextA(LTimeZoneStr,'GMT') then LTimeZoneDelta := 0
    else If ALSameTextA(LTimeZoneStr,'UTC') then LTimeZoneDelta := 0
    else If ALSameTextA(LTimeZoneStr,'UT')  then LTimeZoneDelta := 0
    else If ALSameTextA(LTimeZoneStr,'EST') then LTimeZoneDelta := EncodeTime(5{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'EDT') then LTimeZoneDelta := EncodeTime(4{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'CST') then LTimeZoneDelta := EncodeTime(6{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'CDT') then LTimeZoneDelta := EncodeTime(5{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'MST') then LTimeZoneDelta := EncodeTime(7{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'MDT') then LTimeZoneDelta := EncodeTime(6{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'PST') then LTimeZoneDelta := EncodeTime(8{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextA(LTimeZoneStr,'PDT') then LTimeZoneDelta := EncodeTime(7{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else begin
      Result := False;
      Exit;
    end;

    var LTimeLst := TALStringListA.create;
    try

      LTimeLst.LineBreak := ':';
      LTimeLst.Text := LDateTimeLst[3]; // HH:MM:SS
      var LYear, LDay, LHour, LMinute, LSecond: Integer;
      If (LTimeLst.Count <> 3) or
         (not ALTryStrToInt(LDateTimeLst[0], LDay)) or
         (not ALTryStrToInt(LDateTimeLst[2], LYear)) or
         (not ALTryStrToInt(LTimeLst[0], LHour)) or
         (not ALTryStrToInt(LTimeLst[1], LMinute)) or
         (not ALTryStrToInt(LTimeLst[2], LSecond)) then begin
        Result := False;
        Exit;
      end;

      Result := TryEncodeDateTime(Word(LYear), Word(LMonth), Word(LDay), Word(LHour), Word(LMinute), Word(LSecond), 0{MSec}, Value);
      If Result then Value := Value + LTimeZoneDelta;

    finally
      ALFreeAndNil(LTimeLst);
    end;

  finally
    AlFreeAndNil(LDateTimeLst);
  end;

end;

{**************************************************************}
// Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
// the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
// to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)
//
// The "Date" line (formerly "Posted") is the date that the message was
// originally posted to the network.  Its format must be acceptable
// both in RFC-822 and to the getdate(3) routine that is provided with
// the Usenet software.  This date remains unchanged as the message is
// propagated throughout the network.  One format that is acceptable to
// both is:
//
//                    Wdy, DD Mon YY HH:MM:SS TIMEZONE
//
// Several examples of valid dates appear in the sample message above.
// Note in particular that ctime(3) format:
//
//                    Wdy Mon DD HH:MM:SS YYYY
//
//  is not acceptable because it is not a valid RFC-822 date. However,
//  since older software still generates this format, news
//  implementations are encouraged to accept this format and translate
//  it into an acceptable format.
//
//  There is no hope of having a complete list of timezones.  Universal
//  Time (GMT), the North American timezones (PST, PDT, MST, MDT, CST,
//  CDT, EST, EDT) and the +/-hhmm offset specifed in RFC-822 should be
//  supported.  It is recommended that times in message headers be
//  transmitted in GMT and displayed in the local time zone.
Function ALTryRfc822DateStrToUtcDateTime(const S: String; out Value: TDateTime): Boolean;
Begin

  var LDateStr := S; // Wdy, DD-Mon-YYYY HH:MM:SS GMT
                     // Wdy, DD-Mon-YYYY HH:MM:SS +0200
                     // 23 Aug 2004 06:48:46 -0700
  var P1 := ALPosW(',',LDateStr);
  If P1 > 0 then delete(LDateStr,1,P1); // DD-Mon-YYYY HH:MM:SS GMT
                                        // DD-Mon-YYYY HH:MM:SS +0200
                                        // 23 Aug 2004 06:48:46 -0700
  LDateStr := ALTrim(LDateStr); // DD-Mon-YYYY HH:MM:SS GMT
                                // DD-Mon-YYYY HH:MM:SS +0200
                                // 23 Aug 2004 06:48:46 -0700

  P1 := ALPosW(':',LDateStr);
  var P2 := ALPosW('-',LDateStr);
  While (P2 > 0) and (P2 < P1) do begin
    LDateStr[P2] := ' ';
    P2 := ALPosW('-',LDateStr,P2);
  end; // DD Mon YYYY HH:MM:SS GMT
       // DD Mon YYYY HH:MM:SS +0200
       // 23 Aug 2004 06:48:46 -0700
  While ALPosW('  ',LDateStr) > 0 do
    LDateStr := ALStringReplaceW(LDateStr,'  ',' ',[RfReplaceAll]); // DD Mon YYYY HH:MM:SS GMT
                                                                    // DD Mon YYYY HH:MM:SS +0200
                                                                    // 23 Aug 2004 06:48:46 -0700

  var LDateTimeLst := TALStringListW.create;
  Try

    LDateTimeLst.LineBreak := ' ';
    LDateTimeLst.Text := LDateStr;
    If LDateTimeLst.Count < 5 then begin
      Result := False;
      Exit;
    end;

    var LMonthLabel := LDateTimeLst[1]; // Mon
                                        // Mon
                                        // Aug
    var LMonth: Word := 1;
    While (LMonth <= 12) and (not ALSameTextW(ALRfc822MonthOfTheYearNamesW[LMonth],LMonthLabel)) do inc(LMonth);
    If LMonth > 12 then begin
      Result := False;
      Exit;
    end;

    var LTimeZoneDelta: TDateTime;
    var LTimeZoneStr := LDateTimeLst[4]; // GMT
                                         // +0200
                                         // -0700
    If (Length(LTimeZoneStr) >= 5) and
       (CharInSet(LTimeZoneStr[1], ['+','-'])) and
       (CharInSet(LTimeZoneStr[2], ['0'..'9'])) and
       (CharInSet(LTimeZoneStr[3], ['0'..'9'])) and
       (CharInSet(LTimeZoneStr[4], ['0'..'9'])) and
       (CharInSet(LTimeZoneStr[5], ['0'..'9'])) then begin
      var LHour: Integer;
      var LMin: Integer;
      If (not ALTryStrToInt(AlCopyStr(LTimeZoneStr,2,2), LHour)) or
         (not ALTryStrToInt(AlCopyStr(LTimeZoneStr,4,2), LMin)) or
         (not TryEncodeTime(Word(LHour){Hour}, Word(LMin){Min}, 0{Sec}, 0{MSec}, LTimeZoneDelta)) then begin
        Result := False;
        Exit;
      end;
      if LTimeZoneStr[1] = '+' then LTimeZoneDelta := -1*LTimeZoneDelta;
    end
    else If ALSameTextW(LTimeZoneStr,'GMT') then LTimeZoneDelta := 0
    else If ALSameTextW(LTimeZoneStr,'UTC') then LTimeZoneDelta := 0
    else If ALSameTextW(LTimeZoneStr,'UT')  then LTimeZoneDelta := 0
    else If ALSameTextW(LTimeZoneStr,'EST') then LTimeZoneDelta := EncodeTime(5{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'EDT') then LTimeZoneDelta := EncodeTime(4{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'CST') then LTimeZoneDelta := EncodeTime(6{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'CDT') then LTimeZoneDelta := EncodeTime(5{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'MST') then LTimeZoneDelta := EncodeTime(7{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'MDT') then LTimeZoneDelta := EncodeTime(6{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'PST') then LTimeZoneDelta := EncodeTime(8{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else If ALSameTextW(LTimeZoneStr,'PDT') then LTimeZoneDelta := EncodeTime(7{Hour}, 0{Min}, 0{Sec}, 0{MSec})
    else begin
      Result := False;
      Exit;
    end;

    var LTimeLst := TALStringListW.create;
    try

      LTimeLst.LineBreak := ':';
      LTimeLst.Text := LDateTimeLst[3]; // HH:MM:SS
      var LYear, LDay, LHour, LMinute, LSecond: Integer;
      If (LTimeLst.Count <> 3) or
         (not ALTryStrToInt(LDateTimeLst[0], LDay)) or
         (not ALTryStrToInt(LDateTimeLst[2], LYear)) or
         (not ALTryStrToInt(LTimeLst[0], LHour)) or
         (not ALTryStrToInt(LTimeLst[1], LMinute)) or
         (not ALTryStrToInt(LTimeLst[2], LSecond)) then begin
        Result := False;
        Exit;
      end;

      Result := TryEncodeDateTime(Word(LYear), Word(LMonth), Word(LDay), Word(LHour), Word(LMinute), Word(LSecond), 0{MSec}, Value);
      If Result then Value := Value + LTimeZoneDelta;

    finally
      ALFreeAndNil(LTimeLst);
    end;

  finally
    AlFreeAndNil(LDateTimeLst);
  end;

end;

{**************************************************************}
// Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
// the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
// to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)}
function ALRfc822DateStrToUtcDateTime(const S: AnsiString): TDateTime;
Begin
  if not ALTryRfc822DateStrToUtcDateTime(S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [S]);
end;

{**************************************************************}
// Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
// the function allow also date like "Sun, 06-Nov-1994 08:49:37 GMT"
// to be compatible with cookies field (http://wp.netscape.com/newsref/std/cookie_spec.html)}
function ALRfc822DateStrToUtcDateTime(const S: String): TDateTime;
Begin
  if not ALTryRfc822DateStrToUtcDateTime(S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDateTime, [S]);
end;

{**********************************************************************}
function ALGetHttpReasonPhraseA(Const AStatusCode: Integer): Ansistring;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Status
  case AStatusCode of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    102: Result := 'Processing';
    103: Result := 'Early Hints';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    207: Result := 'Multi-Status';
    208: Result := 'Already Reported';
    226: Result := 'IM Used';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    307: Result := 'Temporary Redirect';
    308: Result := 'Permanent Redirect';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Content Too Large';
    414: Result := 'URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    418: Result := 'I''m a teapot';
    421: Result := 'Misdirected Request';
    422: Result := 'Unprocessable Content';
    423: Result := 'Locked';
    424: Result := 'Failed Dependency';
    425: Result := 'Too Early';
    426: Result := 'Upgrade Required';
    428: Result := 'Precondition Required';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    451: Result := 'Unavailable For Legal Reasons';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    506: Result := 'Variant Also Negotiates';
    507: Result := 'Insufficient Storage';
    508: Result := 'Loop Detected';
    510: Result := 'Not Extended';
    511: Result := 'Network Authentication Required';
  else
    Result := '';
  end
end;

{******************************************************************}
function ALGetHttpReasonPhraseW(Const AStatusCode: Integer): String;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Status
  case AStatusCode of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    102: Result := 'Processing';
    103: Result := 'Early Hints';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    207: Result := 'Multi-Status';
    208: Result := 'Already Reported';
    226: Result := 'IM Used';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    307: Result := 'Temporary Redirect';
    308: Result := 'Permanent Redirect';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Content Too Large';
    414: Result := 'URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    418: Result := 'I''m a teapot';
    421: Result := 'Misdirected Request';
    422: Result := 'Unprocessable Content';
    423: Result := 'Locked';
    424: Result := 'Failed Dependency';
    425: Result := 'Too Early';
    426: Result := 'Upgrade Required';
    428: Result := 'Precondition Required';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    451: Result := 'Unavailable For Legal Reasons';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    506: Result := 'Variant Also Negotiates';
    507: Result := 'Insufficient Storage';
    508: Result := 'Loop Detected';
    510: Result := 'Not Extended';
    511: Result := 'Network Authentication Required';
  else
    Result := '';
  end
end;

{*********************************************************************************************************}
procedure ALDecompressHttpResponseBody(const AContentEncoding: AnsiString; var ABodyStream: TMemoryStream);
begin
  {$if not defined(ALHttpGzipAuto)}
  if ALSameTextA(AContentEncoding, 'gzip') then begin
    ABodyStream.position := 0;
    // 15 is the default mode.
    // 16 is to enable gzip mode.  http://www.zlib.net/manual.html#Advanced
    var LTmpStream := TmemoryStream.Create;
    try
      var LDecompressionStream := TDecompressionStream.Create(ABodyStream, 15 + 16);
      try
        LTmpStream.CopyFrom(LDecompressionStream, 0{Count});
      finally
        alFreeAndNil(LDecompressionStream);
      end;
      ALFreeAndNil(ABodyStream);
      ABodyStream := LTmpStream;
      LTmpStream := nil;
    except
      AlFreeAndNil(LTmpStream);
      raise;
    end;
  end;
  {$ENDIF}
  ABodyStream.Position := 0;
end;

{*****************************************************************************************************}
procedure ALDecompressHttpResponseBody(const AContentEncoding: String; var ABodyStream: TMemoryStream);
begin
  {$if not defined(ALHttpGzipAuto)}
  if ALSameTextW(AContentEncoding, 'gzip') then begin
    ABodyStream.position := 0;
    // 15 is the default mode.
    // 16 is to enable gzip mode.  http://www.zlib.net/manual.html#Advanced
    var LTmpStream := TmemoryStream.Create;
    try
      var LDecompressionStream := TDecompressionStream.Create(ABodyStream, 15 + 16);
      try
        LTmpStream.CopyFrom(LDecompressionStream, 0{Count});
      finally
        alFreeAndNil(LDecompressionStream);
      end;
      ALFreeAndNil(ABodyStream);
      ABodyStream := LTmpStream;
      LTmpStream := nil;
    except
      AlFreeAndNil(LTmpStream);
      raise;
    end;
  end;
  {$ENDIF}
  ABodyStream.Position := 0;
end;

end.