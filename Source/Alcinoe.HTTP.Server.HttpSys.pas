unit Alcinoe.HTTP.Server.HttpSys;

//
// Advantages of using http.sys
//
// * Kernel-mode HTTP stack → runs directly inside Windows networking core, giving you
//   IIS-level performance, stability, and security without needing a heavyweight web server.
// * Port sharing → multiple apps can listen on the same port with different URL prefixes.
// * I/O Completion Ports → your thread only spends time building the response.
//   Receiving requests and sending responses (even to very slow clients) is
//   handled in the kernel, so your thread never blocks. Concretely, this means
//   a single thread can handle thousands of simultaneous connections.
// * TLS/SSL handled in kernel → you don’t manage OpenSSL/Schannel yourself
// * Modern protocols → supports HTTP/1.1, HTTP/2, and HTTP/3 (QUIC) out of the box.
// * Automatic timeout handling → idle, header, and entity-body timeouts enforced by kernel.
// * Kernel-mode response caching → frequently requested responses can be cached directly
//   in kernel space, bypassing user-mode, which reduces latency and CPU usage.
// * Kernel-managed W3C logging → http.sys writes W3C logs for you and handles
//   file rotation and I/O off the request path, so your app avoids blocking.
// * Quality of Service (QoS) → bandwidth throttling and connection limits built in.
// * Widely tested → same engine IIS uses under the hood
//

//
// Enable HTTP on localhost:23456 with HTTP.sys (Windows 10+)
// Enable HTTPS on localhost:34567 with HTTP.sys (Windows 10+)
// -----------------------------------------------------------
//
// Run PowerShell (Admin)
//
// 1) Create a self-signed cert for localhost
//    $cert = New-SelfSignedCertificate -DnsName "localhost" -FriendlyName "Dev Localhost" -CertStoreLocation "Cert:\LocalMachine\My" -NotAfter (Get-Date).AddYears(99)
//
// 2) (Optional but nice) Trust it to avoid browser warnings
//    $store = New-Object System.Security.Cryptography.X509Certificates.X509Store("Root","LocalMachine")
//    $store.Open("ReadWrite"); $store.Add($cert); $store.Close()
//
// 3) Grab the thumbprint (no spaces)
//    $tp = ($cert.Thumbprint -replace ' ')
//    $appid = [guid]::NewGuid().Guid
//
// 4) Bind the cert to port 34567 (IPv4 + IPv6)
//    netsh http add sslcert ipport=0.0.0.0:34567 certhash=$tp appid="{$appid}" certstorename=MY
//    netsh http add sslcert ipport=[::]:34567 certhash=$tp appid="{$appid}" certstorename=MY
//
// Open the Local Computer certificate console: certlm.msc
// You should see the cert in:
//   - Personal → Certificates
//   - Trusted Root Certification Authorities → Certificates
//
// 5) Allow your user to reserve the HTTPS URL
//    netsh http add urlacl url="https://+:34567/" user="YourWindowsUser"
//
// 5) Allow your user to reserve the HTTP URL
//    netsh http add urlacl url="https://+:23456/" user="YourWindowsUser"
//

//
// High-performance HTTP Server code example
// https://learn.microsoft.com/en-us/windows/win32/http/http-server-hp-code-example
//

interface

{$I Alcinoe.inc}

uses
  System.Generics.Collections,
  System.Classes,
  Alcinoe.HTTP,
  Alcinoe.StringUtils,
  Alcinoe.StringList,
  Alcinoe.Net,
  Alcinoe.Url,
  Alcinoe.HTTP.Server,
  Alcinoe.Winapi.HttpApi;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpSysServerRequestHeaders = Class(TALHTTPRequestHeaders)
  private
    FHeaders: PHTTP_REQUEST_HEADERS;
    FKnownHeaders: array[0..Ord(HTTP_HEADER_ID.HttpHeaderRequestMaximum)-1] of AnsiString;
    FCookies: TALStringsA;
    FUnknownHeaders: TALStringsA;
    function PropertyIndexToHeaderID(const APropertyIndex: Integer): HTTP_HEADER_ID;
  protected
    function GetCookies: TALStringsA; override;
    function GetUnknownHeaders: TALStringsA; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); override;
    Function GetRawHeaderText: AnsiString; override;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); override;
  public
    constructor Create(const AHeaders: PHTTP_REQUEST_HEADERS); virtual;
    destructor Destroy; override;
    procedure Clear; override;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpSysServerResponseHeaders = Class(TALHTTPResponseHeaders)
  private
    FKnownHeaders: array[0..Ord(HTTP_HEADER_ID.HttpHeaderResponseMaximum)-1] of AnsiString;
    FCookies: TObjectList<TALHTTPCookie>;
    FUnknownHeaders: TALStringsA;
    function PropertyIndexToHeaderID(const APropertyIndex: Integer): HTTP_HEADER_ID;
  protected
    function GetCookies: TObjectList<TALHTTPCookie>; override;
    function GetUnknownHeaders: TALStringsA; override;
    function GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString; override;
    procedure SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString); override;
    procedure SetRawHeaderText(const ARawHeaderText: AnsiString); override;
    Function GetRawHeaderText: AnsiString; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpSysServerRequest = class(TALHttpServerRequest)
  private
    FHttpRequest: PHTTP_REQUEST;
    FRawUrl: AnsiString;
    FCookedUrl: TALCookedUrlA;
    FVerb: AnsiString;
    FHeaders: TALHttpSysServerRequestHeaders;
    FBodyStream: TALStringStreamA;
    FRemoteAddress: TALNetEndpoint;
    FLocalAddress: TALNetEndpoint;
  protected
    function GetVersion: TALHttpVersion; override;
    procedure SetVersion(const AValue: TALHttpVersion); override;
    function GetVerb: AnsiString; override;
    procedure SetVerb(const AValue: AnsiString); override;
    function GetRawUrl: AnsiString; override;
    procedure SetRawUrl(const AValue: AnsiString); override;
    function GetCookedUrl: TALCookedUrlA; override;
    function GetHeaders: TALHTTPRequestHeaders; override;
    function GetBodyStream: TStream; override;
    procedure SetBodyStream(const AValue: TStream); override;
    function GetOwnsBodyStream: Boolean; override;
    procedure SetOwnsBodyStream(const AValue: Boolean); override;
    function GetBodyString: AnsiString; override;
    procedure SetBodyString(const AValue: AnsiString); override;
    function GetRemoteAddress: TALNetEndpoint; override;
    procedure SetRemoteAddress(const AValue: TALNetEndpoint); override;
    function GetLocalAddress: TALNetEndpoint; override;
    procedure SetLocalAddress(const AValue: TALNetEndpoint); override;
    function GetIsSecure: Boolean; override;
  public
    constructor Create(const AHttpRequest: PHTTP_REQUEST); virtual;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpSysServerResponse = class(TALHttpServerResponse)
  private
    FVersion: TALHttpVersion;
    FStatusCode: Integer;
    FReason: AnsiString;
    FHeaders: TALHttpSysServerResponseHeaders;
    FBodyStream: TStream;
    FOwnsBodyStream: Boolean;
    FBodyFileHandle: THandle;
    FBodyByteRangeStartingOffset: UInt64;
    FBodyByteRangeLength: UInt64;
    FCachePolicyType: TALHttpServerResponse.TCachePolicyType;
    FCacheSecondsToLive: Cardinal;
  protected
    function GetVersion: TALHttpVersion; override;
    procedure SetVersion(const AValue: TALHttpVersion); override;
    function GetStatusCode: Integer; override;
    procedure SetStatusCode(const AValue: Integer); override;
    function GetReason: AnsiString; override;
    procedure SetReason(const AValue: AnsiString); override;
    function GetHeaders: TALHTTPResponseHeaders; override;
    function GetBodyStream: TStream; override;
    procedure SetBodyStream(const AValue: TStream); override;
    function GetOwnsBodyStream: Boolean; override;
    procedure SetOwnsBodyStream(const AValue: Boolean); override;
    function GetBodyString: AnsiString; override;
    procedure SetBodyString(const AValue: AnsiString); override;
    function GetBodyFileHandle: THandle; override;
    procedure SetBodyFileHandle(const AValue: THandle); override;
    function GetBodyByteRangeStartingOffset: UInt64; override;
    procedure SetBodyByteRangeStartingOffset(const AValue: UInt64); override;
    function GetBodyByteRangeLength: UInt64; override;
    procedure SetBodyByteRangeLength(const AValue: UInt64); override;
    function GetCachePolicyType: TALHttpServerResponse.TCachePolicyType; override;
    procedure SetCachePolicyType(const AValue: TALHttpServerResponse.TCachePolicyType); override;
    function GetCacheSecondsToLive: Cardinal; override;
    procedure SetCacheSecondsToLive(const AValue: Cardinal); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALHttpSysServer = class(TALHttpServer)
  private
    class var FHttpApiModuleHandle: HMODULE;
  public
    type
      // ------------
      // TLoggingInfo
      TLoggingInfo = Class(TObject)
      private
        FEnabled: Boolean;
        FLoggingFlags: Cardinal;
        FSoftwareName: String;
        FDirectoryName: String;
        FFormat: HTTP_LOGGING_TYPE;
        FFields: Cardinal;
        FRolloverType: HTTP_LOGGING_ROLLOVER_TYPE;
        FRolloverSize: Cardinal;
      public
        constructor Create; virtual;
        property Enabled: Boolean read FEnabled write FEnabled;
        property LoggingFlags: Cardinal read FLoggingFlags write FLoggingFlags;
        property SoftwareName: String read FSoftwareName write FSoftwareName;
        property DirectoryName: String read FDirectoryName write FDirectoryName;
        property Format: HTTP_LOGGING_TYPE read FFormat write FFormat;
        property Fields: Cardinal read FFields write FFields;
        property RolloverType: HTTP_LOGGING_ROLLOVER_TYPE read FRolloverType write FRolloverType;
        property RolloverSize: Cardinal read FRolloverSize write FRolloverSize;
      End;
  private
    type
      // -------------
      // TWorkerThread
      TWorkerThread = Class(TThread)
      private
        FOwner: TALHttpSysServer;
      protected
        procedure Execute; override;
      public
        constructor Create(const AOwner: TALHttpSysServer);
      End;
  private
    FUrlPrefixes: TALStringListW;
    FLoggingInfo: TLoggingInfo;
    FWorkerThreads: TObjectList<TWorkerThread>;
    FMinWorkerThreadCount: Integer;
    FMaxWorkerThreadCount: Integer;
    FWorkerThreadCount: Integer;
    FBusyWorkerThreadCount: Integer;
    FPeakWorkerThreadCount: Integer;
    FPeakBusyWorkerThreadCount: Integer;
    FRequestQueueHandle: HANDLE;
    FIoCompletionPort: THandle;
    FServerSessionId: HTTP_SERVER_SESSION_ID;
    FUrlGroupId: HTTP_URL_GROUP_ID;
    FIOCPPendingInitialReceiveCount: Integer;
    FIOCPPendingContinuationCount: Integer;
    FMinInitialReceiveCount: Integer;
    FMaxInitialReceiveCount: Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    /// <summary>
    ///   Minimum number of worker threads to start with.
    ///   If set to 0, defaults to (CPU core count * 4).
    /// </summary>
    property MinWorkerThreadCount: Integer read FMinWorkerThreadCount write FMinWorkerThreadCount;
    /// <summary>
    ///   Maximum number of worker threads the pool can grow to.
    ///   Set to 0 (or any value <= MinWorkerThreadCount) to disable dynamic growth.
    /// </summary>
    property MaxWorkerThreadCount: Integer read FMaxWorkerThreadCount write FMaxWorkerThreadCount;
    property WorkerThreadCount: Integer read FWorkerThreadCount;
    property BusyWorkerThreadCount: Integer read FBusyWorkerThreadCount;
    property PeakWorkerThreadCount: Integer read FPeakWorkerThreadCount;
    property PeakBusyWorkerThreadCount: Integer read FPeakBusyWorkerThreadCount;
    property IOCPPendingInitialReceiveCount: Integer read FIOCPPendingInitialReceiveCount;
    property IOCPPendingContinuationCount: Integer read FIOCPPendingContinuationCount;
    property UrlPrefixes: TALStringListW read FUrlPrefixes;
    property LoggingInfo: TLoggingInfo read FLoggingInfo;
  end;

function ALHttpRequestHeaderIdToNameA(const AHeaderID: HTTP_HEADER_ID): AnsiString;
function ALHttpResponseHeaderIdToNameA(const AHeaderID: HTTP_HEADER_ID): AnsiString;
function ALHttpVerbToStringA(const AVERB: HTTP_VERB): AnsiString;

implementation

uses
  System.SysUtils,
  System.Math,
  Winapi.Windows,
  Alcinoe.WinApi.Windows,
  Alcinoe.Common;

{*********************************************************************************}
function ALHttpRequestHeaderIdToNameA(const AHeaderID: HTTP_HEADER_ID): AnsiString;
begin
  case AHeaderID of
    HTTP_HEADER_ID.HttpHeaderAccept: Result := 'Accept';
    HTTP_HEADER_ID.HttpHeaderAcceptCharset: Result := 'Accept-Charset';
    HTTP_HEADER_ID.HttpHeaderAcceptEncoding: Result := 'Accept-Encoding';
    HTTP_HEADER_ID.HttpHeaderAcceptLanguage: Result := 'Accept-Language';
    HTTP_HEADER_ID.HttpHeaderAllow: Result := 'Allow';
    HTTP_HEADER_ID.HttpHeaderAuthorization: Result := 'Authorization';
    HTTP_HEADER_ID.HttpHeaderCacheControl: Result := 'Cache-Control';
    HTTP_HEADER_ID.HttpHeaderConnection: Result := 'Connection';
    HTTP_HEADER_ID.HttpHeaderContentEncoding: Result := 'Content-Encoding';
    HTTP_HEADER_ID.HttpHeaderContentLanguage: Result := 'Content-Language';
    HTTP_HEADER_ID.HttpHeaderContentLength: Result := 'Content-Length';
    HTTP_HEADER_ID.HttpHeaderContentLocation: Result := 'Content-Location';
    HTTP_HEADER_ID.HttpHeaderContentMD5: Result := 'Content-MD5';
    HTTP_HEADER_ID.HttpHeaderContentRange: Result := 'Content-Range';
    HTTP_HEADER_ID.HttpHeaderContentType: Result := 'Content-Type';
    HTTP_HEADER_ID.HttpHeaderDate: Result := 'Date';
    HTTP_HEADER_ID.HttpHeaderExpect: Result := 'Expect';
    HTTP_HEADER_ID.HttpHeaderExpires: Result := 'Expires';
    HTTP_HEADER_ID.HttpHeaderFrom: Result := 'From';
    HTTP_HEADER_ID.HttpHeaderHost: Result := 'Host';
    HTTP_HEADER_ID.HttpHeaderIfMatch: Result := 'If-Match';
    HTTP_HEADER_ID.HttpHeaderIfModifiedSince: Result := 'If-Modified-Since';
    HTTP_HEADER_ID.HttpHeaderIfNoneMatch: Result := 'If-None-Match';
    HTTP_HEADER_ID.HttpHeaderIfRange: Result := 'If-Range';
    HTTP_HEADER_ID.HttpHeaderIfUnmodifiedSince: Result := 'If-Unmodified-Since';
    HTTP_HEADER_ID.HttpHeaderKeepAlive: Result := 'Keep-Alive';
    HTTP_HEADER_ID.HttpHeaderLastModified: Result := 'Last-Modified';
    HTTP_HEADER_ID.HttpHeaderMaxForwards: Result := 'Max-Forwards';
    HTTP_HEADER_ID.HttpHeaderPragma: Result := 'Pragma';
    HTTP_HEADER_ID.HttpHeaderProxyAuthorization: Result := 'Proxy-Authorization';
    HTTP_HEADER_ID.HttpHeaderRange: Result := 'Range';
    HTTP_HEADER_ID.HttpHeaderReferer: Result := 'Referer';
    HTTP_HEADER_ID.HttpHeaderTE: Result := 'TE';
    HTTP_HEADER_ID.HttpHeaderTrailer: Result := 'Trailer';
    HTTP_HEADER_ID.HttpHeaderTranslate: Result := 'Translate';
    HTTP_HEADER_ID.HttpHeaderTransferEncoding: Result := 'Transfer-Encoding';
    HTTP_HEADER_ID.HttpHeaderUpgrade: Result := 'Upgrade';
    HTTP_HEADER_ID.HttpHeaderUserAgent: Result := 'User-Agent';
    HTTP_HEADER_ID.HttpHeaderVia: Result := 'Via';
    HTTP_HEADER_ID.HttpHeaderWarning: Result := 'Warning';
    HTTP_HEADER_ID.HttpHeaderCookie: Result := 'Cookie';
    else
      Raise Exception.Create('Error D689854E-728D-4069-840D-4003E5842719')
  end;
end;

{**********************************************************************************}
function ALHttpResponseHeaderIdToNameA(const AHeaderID: HTTP_HEADER_ID): AnsiString;
begin
  case AHeaderID of
    HTTP_HEADER_ID.HttpHeaderAcceptRanges: Result := 'Accept-Ranges';
    HTTP_HEADER_ID.HttpHeaderAge: Result := 'Age';
    HTTP_HEADER_ID.HttpHeaderAllow: Result := 'Allow';
    HTTP_HEADER_ID.HttpHeaderCacheControl: Result := 'Cache-Control';
    HTTP_HEADER_ID.HttpHeaderConnection: Result := 'Connection';
    HTTP_HEADER_ID.HttpHeaderContentEncoding: Result := 'Content-Encoding';
    HTTP_HEADER_ID.HttpHeaderContentLanguage: Result := 'Content-Language';
    HTTP_HEADER_ID.HttpHeaderContentLength: Result := 'Content-Length';
    HTTP_HEADER_ID.HttpHeaderContentLocation: Result := 'Content-Location';
    HTTP_HEADER_ID.HttpHeaderContentMD5: Result := 'Content-MD5';
    HTTP_HEADER_ID.HttpHeaderContentRange: Result := 'Content-Range';
    HTTP_HEADER_ID.HttpHeaderContentType: Result := 'Content-Type';
    HTTP_HEADER_ID.HttpHeaderDate: Result := 'Date';
    HTTP_HEADER_ID.HttpHeaderETag: Result := 'ETag';
    HTTP_HEADER_ID.HttpHeaderExpires: Result := 'Expires';
    HTTP_HEADER_ID.HttpHeaderKeepAlive: Result := 'Keep-Alive';
    HTTP_HEADER_ID.HttpHeaderLastModified: Result := 'Last-Modified';
    HTTP_HEADER_ID.HttpHeaderLocation: Result := 'Location';
    HTTP_HEADER_ID.HttpHeaderPragma: Result := 'Pragma';
    HTTP_HEADER_ID.HttpHeaderProxyAuthenticate: Result := 'Proxy-Authenticate';
    HTTP_HEADER_ID.HttpHeaderRetryAfter: Result := 'Retry-After';
    HTTP_HEADER_ID.HttpHeaderServer: Result := 'Server';
    HTTP_HEADER_ID.HttpHeaderTrailer: Result := 'Trailer';
    HTTP_HEADER_ID.HttpHeaderTransferEncoding: Result := 'Transfer-Encoding';
    HTTP_HEADER_ID.HttpHeaderUpgrade: Result := 'Upgrade';
    HTTP_HEADER_ID.HttpHeaderVary: Result := 'Vary';
    HTTP_HEADER_ID.HttpHeaderVia: Result := 'Via';
    HTTP_HEADER_ID.HttpHeaderWarning: Result := 'Warning';
    HTTP_HEADER_ID.HttpHeaderWWWAuthenticate: Result := 'WWW-Authenticate';
    HTTP_HEADER_ID.HttpHeaderSetCookie: Result := 'Set-Cookie';
    else
      Raise Exception.Create('Error 6E31EEDC-AD7E-4B1B-AE99-F5B3B0CC60C3')
  end;
end;

{***************************************************************}
function ALHttpVerbToStringA(const AVERB: HTTP_VERB): AnsiString;
begin
  case AVERB of
    HTTP_VERB.HttpVerbUnparsed: Result := '';
    HTTP_VERB.HttpVerbUnknown: Result := '';
    HTTP_VERB.HttpVerbInvalid: Result := '';
    HTTP_VERB.HttpVerbOPTIONS: Result := 'OPTIONS';
    HTTP_VERB.HttpVerbGET: Result := 'GET';
    HTTP_VERB.HttpVerbHEAD: Result := 'HEAD';
    HTTP_VERB.HttpVerbPOST: Result := 'POST';
    HTTP_VERB.HttpVerbPUT: Result := 'PUT';
    HTTP_VERB.HttpVerbDELETE: Result := 'DELETE';
    HTTP_VERB.HttpVerbTRACE: Result := 'TRACE';
    HTTP_VERB.HttpVerbCONNECT: Result := 'CONNECT';
    HTTP_VERB.HttpVerbTRACK: Result := 'TRACK';
    HTTP_VERB.HttpVerbMOVE: Result := 'MOVE';
    HTTP_VERB.HttpVerbCOPY: Result := 'COPY';
    HTTP_VERB.HttpVerbPROPFIND: Result := 'PROPFIND';
    HTTP_VERB.HttpVerbPROPPATCH: Result := 'PROPPATCH';
    HTTP_VERB.HttpVerbMKCOL: Result := 'MKCOL';
    HTTP_VERB.HttpVerbLOCK: Result := 'LOCK';
    HTTP_VERB.HttpVerbUNLOCK: Result := 'UNLOCK';
    HTTP_VERB.HttpVerbSEARCH: Result := 'SEARCH';
    else
      Raise Exception.Create('Error F6EADBFC-A8AB-4DFF-AC13-F1C524E5BED6')
  end
end;

{***************************************************************************************}
constructor TALHttpSysServerRequestHeaders.Create(const AHeaders: PHTTP_REQUEST_HEADERS);
begin
  inherited Create;
  FHeaders := AHeaders;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies:= nil;
  FUnknownHeaders := nil;
end;

{************************************************}
destructor TALHttpSysServerRequestHeaders.Destroy;
begin
  ALFreeAndNil(FCookies);
  ALFreeAndNil(FUnknownHeaders);
  inherited;
end;

{*************************************************************************************************************}
function TALHttpSysServerRequestHeaders.PropertyIndexToHeaderID(const APropertyIndex: Integer): HTTP_HEADER_ID;
begin
  case APropertyIndex of
    0: Result := HTTP_HEADER_ID.HttpHeaderAccept; // {Accept: audio/*; q=0.2, audio/basic}
    1: Result := HTTP_HEADER_ID.HttpHeaderAcceptCharset; // {Accept-Charset: iso-8859-5, unicode-1-1;q=0.8}
    2: Result := HTTP_HEADER_ID.HttpHeaderAcceptEncoding; // {Accept-Encoding: gzip;q=1.0, identity; q=0.5, *;q=0}
    3: Result := HTTP_HEADER_ID.HttpHeaderAcceptLanguage; // {Accept-Language: da, en-gb;q=0.8, en;q=0.7}
    4: Result := HTTP_HEADER_ID.HttpHeaderAllow; // {Allow: GET, HEAD, PUT}
    5: Result := HTTP_HEADER_ID.HttpHeaderAuthorization; // {Authorization: BASIC d2VibWFzdGVyOnpycW1hNHY=}
    6: Result := HTTP_HEADER_ID.HttpHeaderCacheControl; // {Cache-Control: no-cache}
    7: Result := HTTP_HEADER_ID.HttpHeaderConnection; // {Connection: close}
    8: Result := HTTP_HEADER_ID.HttpHeaderContentEncoding; // {Content-Encoding: gzip}
    9: Result := HTTP_HEADER_ID.HttpHeaderContentLanguage; // {Content-Language: mi, en}
    10: Result := HTTP_HEADER_ID.HttpHeaderContentLength; // {Content-Length: 3495}
    11: Result := HTTP_HEADER_ID.HttpHeaderContentLocation; // {Content-Location: http://localhost/page.asp}
    12: Result := HTTP_HEADER_ID.HttpHeaderContentMD5; // {Content-MD5: [md5-digest]}
    13: Result := HTTP_HEADER_ID.HttpHeaderContentRange; // {Content-Range: bytes 2543-4532/7898}
    14: Result := HTTP_HEADER_ID.HttpHeaderContentType; // {Content-Type: text/html; charset=ISO-8859-4}
    15: Result := HTTP_HEADER_ID.HttpHeaderDate; // {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    16: Result := HTTP_HEADER_ID.HttpHeaderExpect; // {Expect: 100-continue}
    17: Result := HTTP_HEADER_ID.HttpHeaderExpires; // {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    18: Result := HTTP_HEADER_ID.HttpHeaderFrom; // {From: webmaster@w3.org}
    19: Result := HTTP_HEADER_ID.HttpHeaderHost; // {Host: www.w3.org}
    20: Result := HTTP_HEADER_ID.HttpHeaderIfMatch; // {If-Match: entity_tag001}
    21: Result := HTTP_HEADER_ID.HttpHeaderIfModifiedSince; // {If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    22: Result := HTTP_HEADER_ID.HttpHeaderIfNoneMatch; // {If-None-Match: entity_tag001}
    23: Result := HTTP_HEADER_ID.HttpHeaderIfRange; // {If-Range: entity_tag001}
    24: Result := HTTP_HEADER_ID.HttpHeaderIfUnmodifiedSince; // {If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT}
    25: Result := HTTP_HEADER_ID.HttpHeaderKeepAlive; // {Keep-Alive: timeout=5, max=1000}
    26: Result := HTTP_HEADER_ID.HttpHeaderLastModified; // {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    27: Result := HTTP_HEADER_ID.HttpHeaderMaxForwards; // {Max-Forwards: 3}
    28: Result := HTTP_HEADER_ID.HttpHeaderPragma; // {Pragma: no-cache}
    29: Result := HTTP_HEADER_ID.HttpHeaderProxyAuthorization; // {Proxy-Authorization: [credentials]}
    30: Result := HTTP_HEADER_ID.HttpHeaderRange; // {Range: bytes=100-599}
    31: Result := HTTP_HEADER_ID.HttpHeaderReferer; // {Referer: http://www.w3.org/hypertext/DataSources/Overview.html}
    32: Result := HTTP_HEADER_ID.HttpHeaderTE; // {TE: trailers, deflate;q=0.5}
    33: Result := HTTP_HEADER_ID.HttpHeaderTrailer; // {Trailer: Date}
    34: Result := HTTP_HEADER_ID.HttpHeaderTranslate; // {Translate: f}
    35: Result := HTTP_HEADER_ID.HttpHeaderTransferEncoding; // {Transfer-Encoding: chunked}
    36: Result := HTTP_HEADER_ID.HttpHeaderUpgrade; // {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    37: Result := HTTP_HEADER_ID.HttpHeaderUserAgent; // {User-Agent: CERN-LineMode/2.15 libwww/2.17b3}
    38: Result := HTTP_HEADER_ID.HttpHeaderVia; // {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    39: Result := HTTP_HEADER_ID.HttpHeaderWarning; // {Warning: 112 Disconnected Operation}
    else
      Raise Exception.Create('Error D689854E-728D-4069-840D-4003E5842719')
  end;
end;

{**************************************************************}
function TALHttpSysServerRequestHeaders.GetCookies: TALStringsA;
begin
  if FCookies = nil then begin
    FCookies := TALNVStringListA.Create;
    FCookies.LineBreak := '; ';
    FCookies.IncludeTrailingLineBreakInText := False;
    var LRawCookiesStr: AnsiString;
    SetString(
      LRawCookiesStr,
      FHeaders^.KnownHeaders[Ord(HTTP_HEADER_ID.HttpHeaderCookie)].pRawValue,
      FHeaders^.KnownHeaders[Ord(HTTP_HEADER_ID.HttpHeaderCookie)].RawValueLength);
    ALExtractHeaderFields(
      [';'], // const ASeparators: TSysCharSet;,
      [' ', #9], // const AWhiteSpace: TSysCharSet;
      [], // const AQuoteChars: TSysCharSet;
      PAnsiChar(LRawCookiesStr), // const AContent: PAnsiChar;
      FCookies); // const AStrings: TALStringsA;
  end;
  Result := FCookies;
end;

{*********************************************************************}
function TALHttpSysServerRequestHeaders.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.IncludeTrailingLineBreakInText := False;
    for var I := 0 to FHeaders^.UnknownHeaderCount - 1 do begin
      var LUnknownHeader := PHTTP_UNKNOWN_HEADER(NativeUInt(FHeaders^.pUnknownHeaders) + (NativeUInt(I) * SizeOf(HTTP_UNKNOWN_HEADER)));
      var LName: AnsiString;
      SetString(
        LName,
        LUnknownHeader.pName,
        LUnknownHeader.NameLength);
      var LValue: AnsiString;
      SetString(
        LValue,
        LUnknownHeader.pRawValue,
        LUnknownHeader.RawValueLength);
      FUnknownHeaders.AddNameValue(LName,LValue);
    end;
  end;
  Result := FUnknownHeaders;
end;

{******************************************************************************************************}
function TALHttpSysServerRequestHeaders.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  var LHttpHeaderID := Ord(PropertyIndexToHeaderID(Index));
  if FKnownHeaders[LHttpHeaderID] <> '' then Result := FKnownHeaders[LHttpHeaderID]
  else begin
    SetString(
      Result,
      FHeaders^.KnownHeaders[LHttpHeaderID].pRawValue,
      FHeaders^.KnownHeaders[LHttpHeaderID].RawValueLength);
    FKnownHeaders[LHttpHeaderID] := Result;
  end;
end;

{********************************************************************************************************************}
procedure TALHttpSysServerRequestHeaders.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  raise Exception.Create('Cannot modify request headers: http.sys exposes a read-only HTTP_REQUEST');
end;

{*******************************************************************}
Function TALHttpSysServerRequestHeaders.GetRawHeaderText: AnsiString;
begin
  var SB := TALStringBuilderA.Create(2048);
  try
    // 1) Known headers
    var LValue: AnsiString;
    for var LHeaderId := 0 to ord(HTTP_HEADER_ID.HttpHeaderRequestMaximum) - 1 do begin
      if FHeaders^.KnownHeaders[LHeaderId].RawValueLength > 0 then begin
        SetString(
          LValue,
          FHeaders^.KnownHeaders[LHeaderId].pRawValue,
          FHeaders^.KnownHeaders[LHeaderId].RawValueLength);
        SB.Append(ALHttpRequestHeaderIdToNameA(HTTP_HEADER_ID(LHeaderId)));
        SB.Append(': ');
        SB.AppendLine(LValue);
      end;
    end;

    // 2) Unknown headers
    for var I := 0 to UnknownHeaders.Count - 1 do begin
      SB.Append(UnknownHeaders.Names[I]);
      SB.Append(': ');
      SB.AppendLine(UnknownHeaders.ValueFromIndex[I]);
    end;

    // 3) Produce the final string
    Result := SB.ToString(true{AUpdateCapacity});
  finally
    ALFreeAndNil(SB);
  end;
end;

{******************************************************************************************}
procedure TALHttpSysServerRequestHeaders.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  raise Exception.Create('Cannot modify request headers: http.sys exposes a read-only HTTP_REQUEST');
end;

{*********************************************}
procedure TALHttpSysServerRequestHeaders.Clear;
begin
  raise Exception.Create('Cannot modify request headers: http.sys exposes a read-only HTTP_REQUEST');
end;

{*************************************************}
constructor TALHttpSysServerResponseHeaders.Create;
begin
  inherited Create;
  //for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
  //  FKnownHeaders[i] := '';
  FCookies := nil;
  FUnknownHeaders := nil;
end;

{*************************************************}
destructor TALHttpSysServerResponseHeaders.Destroy;
begin
  ALFreeAndNil(FCookies);
  ALFreeAndNil(FUnknownHeaders);
  inherited;
end;

{**************************************************************************************************************}
function TALHttpSysServerResponseHeaders.PropertyIndexToHeaderID(const APropertyIndex: Integer): HTTP_HEADER_ID;
begin
  case APropertyIndex of
    0: Result := HTTP_HEADER_ID.HttpHeaderAcceptRanges; // {Accept-Ranges: bytes}
    1: Result := HTTP_HEADER_ID.HttpHeaderAge; // {Age: 2147483648(2^31)}
    2: Result := HTTP_HEADER_ID.HttpHeaderAllow; // {Allow: GET, HEAD, PUT}
    3: Result := HTTP_HEADER_ID.HttpHeaderCacheControl; // {Cache-Control: no-cache}
    4: Result := HTTP_HEADER_ID.HttpHeaderConnection; // {Connection: close}
    5: Result := HTTP_HEADER_ID.HttpHeaderContentEncoding; // {Content-Encoding: gzip}
    6: Result := HTTP_HEADER_ID.HttpHeaderContentLanguage; // {Content-Language: mi, en}
    7: Result := HTTP_HEADER_ID.HttpHeaderContentLength; // {Content-Length: 3495}
    8: Result := HTTP_HEADER_ID.HttpHeaderContentLocation; // {Content-Location: http://localhost/page.asp}
    9: Result := HTTP_HEADER_ID.HttpHeaderContentMD5; // {Content-MD5: [md5-digest]}
    10: Result := HTTP_HEADER_ID.HttpHeaderContentRange; // {Content-Range: bytes 2543-4532/7898}
    11: Result := HTTP_HEADER_ID.HttpHeaderContentType; // {Content-Type: text/html; charset=ISO-8859-4}
    12: Result := HTTP_HEADER_ID.HttpHeaderDate; // {Date: Tue, 15 Nov 1994 08:12:31 GMT}
    13: Result := HTTP_HEADER_ID.HttpHeaderETag; // {ETag: W/"xyzzy"}
    14: Result := HTTP_HEADER_ID.HttpHeaderExpires; // {Expires: Thu, 01 Dec 1994 16:00:00 GMT}
    15: Result := HTTP_HEADER_ID.HttpHeaderKeepAlive; // {Keep-Alive: timeout=5, max=1000}
    16: Result := HTTP_HEADER_ID.HttpHeaderLastModified; // {Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT}
    17: Result := HTTP_HEADER_ID.HttpHeaderLocation; // {Location: http://www.w3.org/pub/WWW/People.html}
    18: Result := HTTP_HEADER_ID.HttpHeaderPragma; // {Pragma: no-cache}
    19: Result := HTTP_HEADER_ID.HttpHeaderProxyAuthenticate; // {Proxy-Authenticate: [challenge]}
    20: Result := HTTP_HEADER_ID.HttpHeaderRetryAfter; // {Retry-After: Fri, 31 Dec 1999 23:59:59 GMT}
    21: Result := HTTP_HEADER_ID.HttpHeaderServer; // {Server: CERN/3.0 libwww/2.17}
    22: Result := HTTP_HEADER_ID.HttpHeaderTrailer; // {Trailer: Date}
    23: Result := HTTP_HEADER_ID.HttpHeaderTransferEncoding; // {Transfer-Encoding: chunked}
    24: Result := HTTP_HEADER_ID.HttpHeaderUpgrade; // {Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11}
    25: Result := HTTP_HEADER_ID.HttpHeaderVary; // {Vary: Date}
    26: Result := HTTP_HEADER_ID.HttpHeaderVia; // {Via: 1.0 ricky, 1.1 mertz, 1.0 lucy}
    27: Result := HTTP_HEADER_ID.HttpHeaderWarning; // {Warning: 112 Disconnected Operation}
    28: Result := HTTP_HEADER_ID.HttpHeaderWWWAuthenticate; // {WWW-Authenticate: [challenge]}
    else
      Raise Exception.Create('Error 1BDF78DD-E23D-4120-9CBF-ED52052BA4B6')
  end;
end;

{******************************************************************************}
function TALHttpSysServerResponseHeaders.GetCookies: TObjectList<TALHTTPCookie>;
begin
  if FCookies = nil then
    FCookies := TObjectList<TALHTTPCookie>.Create(True{AOwnsObjects});
  Result := FCookies;
end;

{**********************************************************************}
function TALHttpSysServerResponseHeaders.GetUnknownHeaders: TALStringsA;
begin
  if FUnknownHeaders = nil then begin
    FUnknownHeaders := TALNVStringListA.Create;
    FUnknownHeaders.NameValueSeparator := ':';
    FUnknownHeaders.IncludeTrailingLineBreakInText := False;
  end;
  Result := FUnknownHeaders;
end;

{*******************************************************************************************************}
function TALHttpSysServerResponseHeaders.GetHeaderValueByPropertyIndex(const Index: Integer): AnsiString;
begin
  Result := FKnownHeaders[Ord(PropertyIndexToHeaderID(Index))];
end;

{*********************************************************************************************************************}
procedure TALHttpSysServerResponseHeaders.SetHeaderValueByPropertyIndex(const Index: Integer; const Value: AnsiString);
begin
  FKnownHeaders[Ord(PropertyIndexToHeaderID(Index))] := Value;
end;

{*******************************************************************************************}
procedure TALHttpSysServerResponseHeaders.SetRawHeaderText(const ARawHeaderText: AnsiString);
begin
  raise ENotImplemented.Create('TALHttpSysServerResponseHeaders.SetRawHeaderText is not yet implemented');
end;

{********************************************************************}
Function TALHttpSysServerResponseHeaders.GetRawHeaderText: AnsiString;
begin
  raise ENotImplemented.Create('TALHttpSysServerResponseHeaders.GetRawHeaderText is not yet implemented');
end;

{**********************************************}
procedure TALHttpSysServerResponseHeaders.Clear;
begin
  if FCookies <> nil then FCookies.Clear;
  if FUnknownHeaders <> nil then FUnknownHeaders.Clear;
  for var i := Low(FKnownHeaders) to High(FKnownHeaders) do
    FKnownHeaders[i] := '';
end;

{****************************************************************************}
constructor TALHttpSysServerRequest.Create(const AHttpRequest: PHTTP_REQUEST);
begin
  inherited Create;
  FHttpRequest := AHttpRequest;
  FRawUrl := '';
  FCookedUrl := nil;
  FVerb := '';
  FHeaders := TALHttpSysServerRequestHeaders.Create(@FHttpRequest^.Headers);
  FBodyStream := TALStringStreamA.Create('');
  FRemoteAddress := nil;
  FLocalAddress := nil;
end;

{*****************************************}
destructor TALHttpSysServerRequest.Destroy;
begin
  ALFreeAndNil(FCookedUrl);
  ALFreeAndNil(FHeaders);
  ALFreeAndNil(FBodyStream);
  ALFreeAndNil(FRemoteAddress);
  ALFreeAndNil(FLocalAddress);
  inherited;
end;

{**********************************************************}
function TALHttpSysServerRequest.GetVersion: TALHttpVersion;
begin
  case FHttpRequest^.Version.MajorVersion of
    0: begin
      if FHttpRequest^.Version.MinorVersion = 9 then Result := TALHttpVersion.v0_9
      else Result := TALHttpVersion.Unspecified;
    end;
    //--
    1: begin
      if FHttpRequest^.Version.MinorVersion = 0 then Result := TALHttpVersion.v1_0
      else Result := TALHttpVersion.v1_1;
    end;
    //--
    2: Result := TALHttpVersion.v2;
    //--
    3: Result := TALHttpVersion.v3;
    //--
    else Result := TALHttpVersion.Unspecified;
  end;
end;

{*************************************************************************}
procedure TALHttpSysServerRequest.SetVersion(const AValue: TALHttpVersion);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{***************************************************}
function TALHttpSysServerRequest.GetVerb: AnsiString;
begin
  if FVerb <> '' then Result := FVerb
  else begin
    Result := ALHttpVerbToStringA(FHttpRequest^.Verb);
    if Result = '' then
      SetString(
        Result,
        FHttpRequest^.pUnknownVerb,
        FHttpRequest^.UnknownVerbLength);
    FVerb := Result;
  end;
end;

{******************************************************************}
procedure TALHttpSysServerRequest.SetVerb(const AValue: AnsiString);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{*****************************************************}
function TALHttpSysServerRequest.GetRawUrl: AnsiString;
begin
  if FRawUrl <> '' then Result := FRawUrl
  else begin
    SetString(
      Result,
      FHttpRequest^.pRawUrl,
      FHttpRequest^.RawUrlLength);
    FRawUrl := Result;
  end;
end;

{********************************************************************}
procedure TALHttpSysServerRequest.SetRawUrl(const AValue: AnsiString);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{***********************************************************}
function TALHttpSysServerRequest.GetCookedUrl: TALCookedUrlA;
begin
  If FCookedUrl = nil then begin
    if IsSecure then FCookedUrl := TALCookedUrlA.Create('https://' + Headers.Host + RawUrl)
    else FCookedUrl := TALCookedUrlA.Create('http://' + Headers.Host + RawUrl);
  end;
  Result := FCookedUrl;
end;

{*****************************************************************}
function TALHttpSysServerRequest.GetHeaders: TALHTTPRequestHeaders;
begin
  Result := FHeaders;
end;

{******************************************************}
function TALHttpSysServerRequest.GetBodyStream: TStream;
begin
  Result := FBodyStream;
end;

{*********************************************************************}
procedure TALHttpSysServerRequest.SetBodyStream(const AValue: TStream);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{**********************************************************}
function TALHttpSysServerRequest.GetOwnsBodyStream: Boolean;
begin
  Result := True;
end;

{*************************************************************************}
procedure TALHttpSysServerRequest.SetOwnsBodyStream(const AValue: Boolean);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{*********************************************************}
function TALHttpSysServerRequest.GetBodyString: AnsiString;
begin
  Result := FBodyStream.DataString;
end;

{************************************************************************}
procedure TALHttpSysServerRequest.SetBodyString(const AValue: AnsiString);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{****************************************************************}
function TALHttpSysServerRequest.GetRemoteAddress: TALNetEndpoint;
begin
  if FRemoteAddress = nil then
    FRemoteAddress := TALNetEndpoint.Create(FHttpRequest^.Address.pRemoteAddress);
  Result := FRemoteAddress;
end;

{*******************************************************************************}
procedure TALHttpSysServerRequest.SetRemoteAddress(const AValue: TALNetEndpoint);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{***************************************************************}
function TALHttpSysServerRequest.GetLocalAddress: TALNetEndpoint;
begin
  if FLocalAddress = nil then
    FLocalAddress := TALNetEndpoint.Create(FHttpRequest^.Address.pLocalAddress);
  Result := FLocalAddress;
end;

{******************************************************************************}
procedure TALHttpSysServerRequest.SetLocalAddress(const AValue: TALNetEndpoint);
begin
  raise Exception.Create('Cannot modify request: http.sys exposes a read-only HTTP_REQUEST');
end;

{****************************************************}
function TALHttpSysServerRequest.GetIsSecure: Boolean;
begin
  // If this request has SSL info, it means the connection is HTTPS
  Result := Assigned(FHttpRequest^.pSslInfo);
end;

{******************************************}
constructor TALHttpSysServerResponse.Create;
begin
  inherited;
  FVersion := TALHttpVersion.v1_1;
  FStatusCode := 200;
  FReason := 'OK';
  FHeaders := nil;
  FBodyStream := nil;
  FOwnsBodyStream := True;
  FBodyFileHandle := INVALID_HANDLE_VALUE;
  FBodyByteRangeStartingOffset := 0;
  FBodyByteRangeLength := HTTP_BYTE_RANGE_TO_EOF;
  FCachePolicyType := TALHttpServerResponse.TCachePolicyType.Nocache;
  FCacheSecondsToLive := 0;
end;

{******************************************}
destructor TALHttpSysServerResponse.Destroy;
begin
  if FBodyFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FBodyFileHandle); // best-effort; do not raise on failure
  ALFreeAndNil(FHeaders);
  if OwnsBodyStream then ALFreeAndNil(FBodyStream);
  inherited;
end;

{***********************************************************}
function TALHttpSysServerResponse.GetVersion: TALHttpVersion;
begin
  Result := FVersion;
end;

{**************************************************************************}
procedure TALHttpSysServerResponse.SetVersion(const AValue: TALHttpVersion);
begin
  FVersion := AValue;
end;

{*******************************************************}
function TALHttpSysServerResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

{**********************************************************************}
procedure TALHttpSysServerResponse.SetStatusCode(const AValue: Integer);
begin
  If AValue <> FStatusCode then begin
    FStatusCode := AValue;
    Reason := ALGetHttpReasonPhraseA(FStatusCode);
  end;
end;

{******************************************************}
function TALHttpSysServerResponse.GetReason: AnsiString;
begin
  Result := FReason;
end;

{*********************************************************************}
procedure TALHttpSysServerResponse.SetReason(const AValue: AnsiString);
begin
  FReason := AValue;
end;

{*******************************************************************}
function TALHttpSysServerResponse.GetHeaders: TALHTTPResponseHeaders;
begin
  if FHeaders = nil then
    FHeaders := TALHttpSysServerResponseHeaders.Create;
  Result := FHeaders;
end;

{*******************************************************}
function TALHttpSysServerResponse.GetBodyStream: TStream;
begin
  if FBodyStream = nil then
    FBodyStream := TALStringStreamA.Create('');
  Result := FBodyStream;
end;

{**********************************************************************}
procedure TALHttpSysServerResponse.SetBodyStream(const AValue: TStream);
begin
  if AValue <> FBodyStream then begin
    if OwnsBodyStream then ALFreeAndNil(FBodyStream);
    FBodyStream := AValue;
  end;
end;

{***********************************************************}
function TALHttpSysServerResponse.GetOwnsBodyStream: Boolean;
begin
  Result := FOwnsBodyStream;
end;

{**************************************************************************}
procedure TALHttpSysServerResponse.SetOwnsBodyStream(const AValue: Boolean);
begin
  FOwnsBodyStream := AValue;
end;

{**********************************************************}
function TALHttpSysServerResponse.GetBodyString: AnsiString;
begin
  if FBodyStream <> nil then begin
    if FBodyStream is TALStringStreamA then Result := TALStringStreamA(FBodyStream).DataString
    else if FBodyStream.Size > 0 then begin
      SetLength(Result, FBodyStream.Size);
      FBodyStream.Position := 0;
      FBodyStream.ReadBuffer(PAnsiChar(Result)^, FBodyStream.Size);
    end
    else Result := '';
  end
  else Result := '';
end;

{*************************************************************************}
procedure TALHttpSysServerResponse.SetBodyString(const AValue: AnsiString);
begin
  if FBodyStream <> nil then begin
    if FBodyStream is TALStringStreamA then begin
      TALStringStreamA(FBodyStream).DataString := AValue;
      Exit;
    end
    else if not OwnsBodyStream then begin
      FBodyStream.Position := 0;
      FBodyStream.Size := Length(AValue);
      if Length(AValue) > 0 then FBodyStream.WriteBuffer(PAnsiChar(AValue)^, Length(AValue));
      Exit;
    end
    else
      ALFreeAndNil(FBodyStream)
  end;
  FBodyStream := TALStringStreamA.Create(AValue);
end;

{***********************************************************}
function TALHttpSysServerResponse.GetBodyFileHandle: THandle;
begin
  Result := FBodyFileHandle;
end;

{**************************************************************************}
procedure TALHttpSysServerResponse.SetBodyFileHandle(const AValue: THandle);
begin
  if AValue <> FBodyFileHandle then begin
    if FBodyFileHandle <> INVALID_HANDLE_VALUE then
      ALCheckWinApiBoolean('CloseHandle(FBodyFileHandle)', CloseHandle(FBodyFileHandle));
    FBodyFileHandle := AValue;
  end;
end;

{***********************************************************************}
function TALHttpSysServerResponse.GetBodyByteRangeStartingOffset: UInt64;
begin
  Result := FBodyByteRangeStartingOffset;
end;

{**************************************************************************************}
procedure TALHttpSysServerResponse.SetBodyByteRangeStartingOffset(const AValue: UInt64);
begin
  FBodyByteRangeStartingOffset := AValue;
end;

{***************************************************************}
function TALHttpSysServerResponse.GetBodyByteRangeLength: UInt64;
begin
  Result := FBodyByteRangeLength;
end;

{******************************************************************************}
procedure TALHttpSysServerResponse.SetBodyByteRangeLength(const AValue: UInt64);
begin
  FBodyByteRangeLength := AValue;
end;

{*******************************************************************************************}
function TALHttpSysServerResponse.GetCachePolicyType: TALHttpServerResponse.TCachePolicyType;
begin
  Result := FCachePolicyType;
end;

{**********************************************************************************************************}
procedure TALHttpSysServerResponse.SetCachePolicyType(const AValue: TALHttpServerResponse.TCachePolicyType);
begin
  FCachePolicyType := AValue;
end;

{****************************************************************}
function TALHttpSysServerResponse.GetCacheSecondsToLive: Cardinal;
begin
  Result := FCacheSecondsToLive;
end;

{*******************************************************************************}
procedure TALHttpSysServerResponse.SetCacheSecondsToLive(const AValue: Cardinal);
begin
  FCacheSecondsToLive := AValue;
end;

{***********************************************}
constructor TALHttpSysServer.TLoggingInfo.Create;
begin
  Inherited Create;
  FEnabled := False;
  FLoggingFlags := HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION;
  FSoftwareName := '';
  FDirectoryName := '';
  FFormat := HTTP_LOGGING_TYPE.HttpLoggingTypeW3C;
  FFields := HTTP_LOG_FIELD_DATE or
             HTTP_LOG_FIELD_TIME or
             HTTP_LOG_FIELD_CLIENT_IP or
             //HTTP_LOG_FIELD_USER_NAME or
             //HTTP_LOG_FIELD_SITE_NAME or
             //HTTP_LOG_FIELD_COMPUTER_NAME or
             //HTTP_LOG_FIELD_SERVER_IP or
             HTTP_LOG_FIELD_METHOD or
             HTTP_LOG_FIELD_URI_STEM or
             HTTP_LOG_FIELD_URI_QUERY or
             HTTP_LOG_FIELD_STATUS or
             //HTTP_LOG_FIELD_WIN32_STATUS or
             HTTP_LOG_FIELD_BYTES_SENT or
             HTTP_LOG_FIELD_BYTES_RECV or
             HTTP_LOG_FIELD_TIME_TAKEN or
             //HTTP_LOG_FIELD_SERVER_PORT or
             HTTP_LOG_FIELD_USER_AGENT or
             //HTTP_LOG_FIELD_COOKIE or
             HTTP_LOG_FIELD_REFERER or
             HTTP_LOG_FIELD_VERSION or
             HTTP_LOG_FIELD_HOST;
             //HTTP_LOG_FIELD_SUB_STATUS or
             //HTTP_LOG_FIELD_STREAM_ID or
             //HTTP_LOG_FIELD_STREAM_ID_EX or
             //HTTP_LOG_FIELD_TRANSPORT_TYPE;
  FRolloverType := HTTP_LOGGING_ROLLOVER_TYPE.HttpLoggingRolloverDaily;
  FRolloverSize := HTTP_LIMIT_INFINITE;
End;

{********************************************************************************}
constructor TALHttpSysServer.TWorkerThread.Create(const AOwner: TALHttpSysServer);
begin
  FOwner := AOwner;
  inherited Create(False{CreateSuspended});
end;

{***********************************************}
procedure TALHttpSysServer.TWorkerThread.Execute;

type
  POverlappedContext = ^TOverlappedContext;
  TOverlappedContext = record
    Overlapped: TOverlapped;
    Operation: Integer;
    CompletedInline: Boolean;
    InitialReceive: Boolean;
    EOFProbeBuffer: Byte;
    HttpApiStatus: ULONG;
    HttpApiRequest: PHTTP_REQUEST;
    HttpApiRequestLength: ULONG;
    HttpApiResponse: PHTTP_RESPONSE;
    HttpApiCookieStrings: TArray<AnsiString>;
    HttpApiUnknownHeaders: TArray<HTTP_UNKNOWN_HEADER>;
    HttpApiCachePolicy: PHTTP_CACHE_POLICY;
    HttpApiLogFieldsData: PHTTP_LOG_FIELDS_DATA;
    HttpApiLogFieldStringsA: Array[0..2] of AnsiString;
    HttpApiLogFieldStringsW: Array[0..1] of String;
    HttpApiDataChunk: PHTTP_DATA_CHUNK;
    HttpSysRequest: TALHttpSysServerRequest;
    HttpSysResponse: TALHttpSysServerResponse;
    NumberOfBytesTransferred: DWORD;
  end;

const
  OP_RECEIVE_HTTP_REQUEST = 0;
  OP_RECEIVE_REQUEST_ENTITY_BODY = 1;
  OP_SEND_HTTP_RESPONSE = 2;

const
  HttpApiRequestDefaultBufferLength = SizeOf(HTTP_REQUEST) + (16 * 1024);
  HttpSysRequestBodyStreamDefaultSize = 16384 * 4;

begin

  var LBusyWorkerThreadCount := AtomicIncrement(FOwner.FBusyWorkerThreadCount);
  if FOwner.FPeakBusyWorkerThreadCount < LBusyWorkerThreadCount then AtomicExchange(FOwner.FPeakBusyWorkerThreadCount, LBusyWorkerThreadCount);
  var LOverlappedContext: POverlappedContext := nil;
  while true do begin

    Try

      {$REGION 'Post initial HttpReceiveHttpRequest'}
      if (LOverlappedContext = nil) or (not LOverlappedContext^.CompletedInline) then begin
        If (not Terminated) and
           (((LOverlappedContext = nil) and (Fowner.FIOCPPendingInitialReceiveCount < FOwner.FMinInitialReceiveCount)) or
            ((LOverlappedContext <> nil) and (Fowner.FIOCPPendingInitialReceiveCount < FOwner.FMaxInitialReceiveCount))) then begin
          if LOverlappedContext = nil then begin
            LOverlappedContext := AllocMem(SizeOf(TOverlappedContext));
            // https://learn.microsoft.com/en-us/windows/win32/api/http/nf-http-httpreceivehttprequest
            // To be sure of receiving at least part of the request, it is recommended
            // that an application provide at least a buffer of 4 KB, which accommodates
            // most HTTP requests. Alternately, authentication headers, parsed as
            // unknown headers, can add up to 12 KB to that, so if
            // authentication/authorization is used, a buffer size of at least 16 KB is
            // recommended.
            // ------------
            // Note: Windows Server enforces system-wide header size limits through
            // HTTP.sys registry settings:
            //   MaxRequestBytes   (default 16 KB)
            //   MaxFieldLength    (default 8 KB)
            // Requests exceeding these limits are rejected with
            // "400 Bad Request – Request Too Long".
            // The limits can be increased under:
            //   HKLM\SYSTEM\CurrentControlSet\Services\HTTP\Parameters
            // followed by a system reboot.
            GetMem(LOverlappedContext^.HttpApiRequest, HttpApiRequestDefaultBufferLength);
            LOverlappedContext^.HttpApiRequestLength := HttpApiRequestDefaultBufferLength;
            GetMem(LOverlappedContext^.HttpApiResponse, SizeOf(HTTP_RESPONSE));
            GetMem(LOverlappedContext^.HttpApiCachePolicy, SizeOf(HTTP_CACHE_POLICY));
            GetMem(LOverlappedContext^.HttpApiLogFieldsData, SizeOf(HTTP_LOG_FIELDS_DATA));
            GetMem(LOverlappedContext^.HttpApiDataChunk, SizeOf(HTTP_DATA_CHUNK));
            //LOverlappedContext^.Overlapped = (Internal=0,InternalHigh=0,Offset=0,OffsetHigh=0,hEvent=0)
            //LOverlappedContext^.Operation = 0
            //LOverlappedContext^.CompletedInline = False
            //LOverlappedContext^.InitialReceive = False
            //LOverlappedContext^.EOFProbeBuffer = 0
            //LOverlappedContext^.HttpApiStatus = 0
            //LOverlappedContext^.HttpApiRequest = (pointer)
            //LOverlappedContext^.HttpApiRequestLength = HttpApiRequestDefaultBufferLength
            //LOverlappedContext^.HttpApiResponse = (pointer)
            //LOverlappedContext^.HttpApiCookieStrings = []
            //LOverlappedContext^.HttpApiUnknownHeaders = []
            //LOverlappedContext^.HttpApiCachePolicy = (pointer)
            //LOverlappedContext^.HttpApiLogFieldsData = (pointer)
            //LOverlappedContext^.HttpApiLogFieldStringsA = ['', '', '']
            //LOverlappedContext^.HttpApiLogFieldStringsW = ['', '']
            //LOverlappedContext^.HttpApiDataChunk = (pointer)
            //LOverlappedContext^.HttpSysRequest = nil
            //LOverlappedContext^.HttpSysResponse = nil
            //LOverlappedContext^.NumberOfBytesTransferred = 0
          end
          else ZeroMemory(@LOverlappedContext^.Overlapped, SizeOf(TOverlapped));
          LOverlappedContext^.Operation := OP_RECEIVE_HTTP_REQUEST;
          LOverlappedContext^.CompletedInline := False;
          LOverlappedContext^.InitialReceive := True;
          if LOverlappedContext^.HttpApiRequestLength <> HttpApiRequestDefaultBufferLength then begin
            LOverlappedContext^.HttpApiRequestLength := HttpApiRequestDefaultBufferLength;
            ReallocMem(LOverlappedContext^.HttpApiRequest, LOverlappedContext^.HttpApiRequestLength);
          end;
          var LBytesReturned: ULONG;
          var LStatus := HttpReceiveHttpRequest(
                           FOwner.FRequestQueueHandle, // RequestQueueHandle: HANDLE;
                           0, // RequestId: HTTP_REQUEST_ID;
                           0, // Flags: ULONG;
                              // 0 (zero):
                              //   Only the request headers are retrieved; the entity body is not copied.
                              // HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY:
                              //   The available entity body is copied along with the request headers.
                              //   The pEntityChunks member of the HTTP_REQUEST structure points to
                              //   the entity body.
                              // HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY:
                              //   All of the entity bodies are copied along with the request headers.
                              //   The pEntityChunks member of the HTTP_REQUEST structure points to
                              //   the entity body.
                           LOverlappedContext^.HttpApiRequest, // RequestBuffer: PHTTP_REQUEST;
                           LOverlappedContext^.HttpApiRequestLength, // RequestBufferLength: ULONG;
                           @LBytesReturned, // BytesReturned: PULONG; // OPTIONAL
                           @LOverlappedContext^.Overlapped); // Overlapped: LPOVERLAPPED)
          if LStatus <> ERROR_IO_PENDING then begin
            LOverlappedContext^.CompletedInline := True;
            LOverlappedContext^.HttpApiStatus := LStatus;
            LOverlappedContext^.NumberOfBytesTransferred := LBytesReturned;
          end
          else AtomicIncrement(Fowner.FIOCPPendingInitialReceiveCount);
        end
        else if LOverlappedContext <> nil then begin
          FreeMem(LOverlappedContext^.HttpApiRequest);
          FreeMem(LOverlappedContext^.HttpApiResponse);
          FreeMem(LOverlappedContext^.HttpApiCachePolicy);
          FreeMem(LOverlappedContext^.HttpApiLogFieldsData);
          FreeMem(LOverlappedContext^.HttpApiDataChunk);
          ALFreeAndNil(LOverlappedContext^.HttpSysRequest);
          ALFreeAndNil(LOverlappedContext^.HttpSysResponse);
          Dispose(LOverlappedContext);
          LOverlappedContext := nil;
        end;
      end;
      {$ENDREGION}

      {$REGION 'Dequeue an I/O completion packet'}
      if (LOverlappedContext = nil) or (not LOverlappedContext^.CompletedInline) then begin

        var LNumberOfBytesTransferred: DWORD;
        var LCompletionKey: ULONG_PTR;
        var LOverlapped: lpOverlapped;

        AtomicDecrement(FOwner.FBusyWorkerThreadCount);

        if Terminated then begin
          TMonitor.Enter(Fowner);
          Try
            If (FOwner.FWorkerThreadCount > 1) or
               ((FOwner.FWorkerThreadCount = 1) and
                (Fowner.FIOCPPendingInitialReceiveCount = 0) and
                (Fowner.FIOCPPendingContinuationCount = 0)) then begin
              AtomicIncrement(FOwner.FBusyWorkerThreadCount);
              AtomicDecrement(FOwner.FWorkerThreadCount);
              Break;
            end;
          Finally
            TMonitor.Exit(Fowner);
          End;
        end;

        GetQueuedCompletionStatus(
          FOwner.FIoCompletionPort, // CompletionPort: THandle;
          lNumberOfBytesTransferred, // var lpNumberOfBytesTransferred: DWORD;
          LCompletionKey, // var lpCompletionKey: ULONG_PTR;
          LOverlapped, // var lpOverlapped: POverlapped
          INFINITE); // dwMilliseconds: DWORD

        LBusyWorkerThreadCount := AtomicIncrement(FOwner.FBusyWorkerThreadCount);
        if FOwner.FPeakBusyWorkerThreadCount < LBusyWorkerThreadCount then AtomicExchange(FOwner.FPeakBusyWorkerThreadCount, LBusyWorkerThreadCount);

        If LOverlapped = nil then begin
          LOverlappedContext := nil;
          Continue;
        end;

        // GetQueuedCompletionStatus(...) gives you back the same lpOverlapped pointer
        // that you originally passed into the async API. In our design, the field
        // `Overlapped` is the *first* field of TOverlappedContext, i.e. it lives at
        // offset 0 of the record. That means:
        //
        //   AddressOf(Context^)            = @Context^
        //   AddressOf(Context^.Overlapped) = @Context^ + 0 = @Context^
        //
        // Therefore, lpOverlapped (the pointer we get from the IOCP) is *exactly*
        // the address of the containing TOverlappedContext. Reinterpreting it as
        // POverlappedContext is correct on both Win32 and Win64—no pointer-size or
        // alignment pitfalls—because offset 0 is offset 0 on all architectures.
        //
        // ⚠ Important: This is only true if `Overlapped` remains the first field.
        // If you ever add a field before it, this cast becomes invalid. In that
        // case use the CONTAINING_RECORD pattern (subtract the field offset).

        LOverlappedContext := POverlappedContext(LOverlapped);
        LOverlappedContext^.HttpApiStatus := RtlNtStatusToDosError(NTSTATUS(LOverlappedContext^.Overlapped.Internal));
        LOverlappedContext^.NumberOfBytesTransferred := LNumberOfBytesTransferred;
        if LOverlappedContext^.InitialReceive then begin
          LOverlappedContext^.InitialReceive := False;
          AtomicDecrement(Fowner.FIOCPPendingInitialReceiveCount);
        end
        else AtomicDecrement(Fowner.FIOCPPendingContinuationCount);

      end;
      {$ENDREGION}

      {$REGION 'ERROR_CONNECTION_INVALID'}
      If LOverlappedContext^.HttpApiStatus = ERROR_CONNECTION_INVALID then begin
        // Meaning: Client disappeared during receive.
        // Do: Free the per-operation/request context; do not attempt a response.
        LOverlappedContext^.CompletedInline := False;
        Continue;
      end;
      {$ENDREGION}

      {$REGION 'ERROR_OPERATION_ABORTED'}
      If LOverlappedContext^.HttpApiStatus = ERROR_OPERATION_ABORTED then begin
        // Meaning: You (or shutdown) cancelled the I/O, or the queue/handle is closing.
        // Do: Clean up the context for this overlapped, and don’t reply.
        LOverlappedContext^.CompletedInline := False;
        Continue;
      end;
      {$ENDREGION}

      case LOverlappedContext^.Operation of

        {$REGION 'OP_RECEIVE_HTTP_REQUEST'}
        OP_RECEIVE_HTTP_REQUEST: begin

          {$REGION 'ERROR_MORE_DATA'}
          If LOverlappedContext^.HttpApiStatus = ERROR_MORE_DATA then begin
            ZeroMemory(@LOverlappedContext^.Overlapped, SizeOf(TOverlapped));
            LOverlappedContext^.Operation := OP_RECEIVE_HTTP_REQUEST;
            LOverlappedContext^.CompletedInline := False;
            if LOverlappedContext^.NumberOfBytesTransferred <= LOverlappedContext^.HttpApiRequestLength then
              Raise Exception.Create('Error A73E0415-BBA0-4402-B410-1973A56CF2E0');
            LOverlappedContext^.HttpApiRequestLength := LOverlappedContext^.NumberOfBytesTransferred;
            ReallocMem(LOverlappedContext^.HttpApiRequest, LOverlappedContext^.HttpApiRequestLength);
            var LBytesReturned: ULONG;
            var LStatus := HttpReceiveHttpRequest(
                             FOwner.FRequestQueueHandle, // RequestQueueHandle: HANDLE;
                             LOverlappedContext^.HttpApiRequest^.RequestId, // RequestId: HTTP_REQUEST_ID;
                             0, // Flags: ULONG;
                                // 0 (zero):
                                //   Only the request headers are retrieved; the entity body is not copied.
                                // HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY:
                                //   The available entity body is copied along with the request headers.
                                //   The pEntityChunks member of the HTTP_REQUEST structure points to
                                //   the entity body.
                                // HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY:
                                //   All of the entity bodies are copied along with the request headers.
                                //   The pEntityChunks member of the HTTP_REQUEST structure points to
                                //   the entity body.
                             LOverlappedContext^.HttpApiRequest, // RequestBuffer: PHTTP_REQUEST;
                             LOverlappedContext^.HttpApiRequestLength, // RequestBufferLength: ULONG;
                             @LBytesReturned, // BytesReturned: PULONG; // OPTIONAL
                             @LOverlappedContext^.Overlapped); // Overlapped: LPOVERLAPPED)
            if LStatus <> ERROR_IO_PENDING then begin
              LOverlappedContext^.CompletedInline := True;
              LOverlappedContext^.HttpApiStatus := LStatus;
              LOverlappedContext^.NumberOfBytesTransferred := LBytesReturned;
            end
            else begin
              LOverlappedContext := nil;
              AtomicIncrement(Fowner.FIOCPPendingContinuationCount);
            end;
            Continue;
          end;
          {$ENDREGION}

          {$REGION 'Check HttpApiStatus'}
          ALCheckWinApiErrorCode('HttpReceiveHttpRequest', LOverlappedContext^.HttpApiStatus, FHttpApiModuleHandle);
          {$ENDREGION}

          {$REGION 'Create HttpSysRequest'}
          ALFreeAndNil(LOverlappedContext^.HttpSysRequest);
          LOverlappedContext^.HttpSysRequest := TALHttpSysServerRequest.Create(LOverlappedContext^.HttpApiRequest);
          {$ENDREGION}

          {$REGION 'Body present'}
          var LContentLength: integer;
          if not ALTryStrToInt(LOverlappedContext^.HttpSysRequest.Headers.ContentLength, LContentLength) then LContentLength := 0;
          if (LContentLength > 0) or (AlposIgnoreCaseA('chunked', LOverlappedContext^.HttpSysRequest.Headers.TransferEncoding) > 0) then begin
            if (LContentLength > FOwner.MaxBodySize) and (FOwner.MaxBodySize > 0) then
              raise EALHttpServerBodySizeTooBig.CreateFmt('Body size (%d bytes) is bigger than the maximum allowed size (%d bytes)', [LContentLength, FOwner.MaxBodySize]);
            var LBodyStream := TALStringStreamA(LOverlappedContext^.HttpSysRequest.BodyStream);
            ZeroMemory(@LOverlappedContext^.Overlapped, SizeOf(TOverlapped));
            LOverlappedContext^.Operation := OP_RECEIVE_REQUEST_ENTITY_BODY;
            LOverlappedContext^.CompletedInline := False;
            if LContentLength > 0 then LBodyStream.Size := LContentLength
            else LBodyStream.Size := HttpSysRequestBodyStreamDefaultSize;
            LBodyStream.Position := 0;
            var LBytesReturned: ULONG;
            var LStatus := HttpReceiveRequestEntityBody(
                             Fowner.FRequestQueueHandle, // RequestQueueHandle: HANDLE;
                             LOverlappedContext^.HttpApiRequest^.RequestId, // RequestId: HTTP_REQUEST_ID;
                             HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER, // Flags: ULONG;
                                                                                // HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER:
                                                                                //   Specifies that the buffer will be filled with one or more entity bodies,
                                                                                //   unless there are no remaining entity bodies to copy.
                             @LBodyStream.DataString[low(AnsiString)], // EntityBuffer: PVOID;
                             LBodyStream.Size, // EntityBufferLength: ULONG;
                             @LBytesReturned, // BytesReturned: PULONG; // OPTIONAL
                             @LOverlappedContext^.Overlapped); // Overlapped: LPOVERLAPPED);
            if LStatus <> ERROR_IO_PENDING then begin
              LOverlappedContext^.CompletedInline := True;
              LOverlappedContext^.HttpApiStatus := LStatus;
              LOverlappedContext^.NumberOfBytesTransferred := LBytesReturned;
            end
            else begin
              LOverlappedContext := nil;
              AtomicIncrement(Fowner.FIOCPPendingContinuationCount);
            end;
            Continue;
          end;
          {$ENDREGION}

          {$REGION 'No body present'}
          LOverlappedContext^.Operation := OP_RECEIVE_REQUEST_ENTITY_BODY;
          LOverlappedContext^.CompletedInline := True;
          LOverlappedContext^.HttpApiStatus := NO_ERROR;
          LOverlappedContext^.NumberOfBytesTransferred := 0;
          Continue;
          {$ENDREGION}

        end;
        {$ENDREGION}

        {$REGION 'OP_RECEIVE_REQUEST_ENTITY_BODY'}
        OP_RECEIVE_REQUEST_ENTITY_BODY: begin

          {$REGION 'NO_ERROR/ERROR_MORE_DATA'}
          If (LOverlappedContext^.HttpSysRequest.BodyStream.Size > 0) and
             (LOverlappedContext^.HttpApiStatus in [NO_ERROR, ERROR_MORE_DATA]) then begin
            //-
            var LContentLength: integer;
            if not ALTryStrToInt(LOverlappedContext^.HttpSysRequest.Headers.ContentLength, LContentLength) then LContentLength := 0;
            //-
            var LBodyStream := TALStringStreamA(LOverlappedContext^.HttpSysRequest.BodyStream);
            If LBodyStream.Position + LOverlappedContext^.NumberOfBytesTransferred > LBodyStream.size then
              Raise Exception.Create('Error CDA0E7BC-32EF-4AA0-A2C4-D0672B538427');
            LBodyStream.Position := LBodyStream.Position + LOverlappedContext^.NumberOfBytesTransferred;
            //-
            var LEntityBuffer: PVOID;
            var LEntityBufferLength: ULONG;
            if LContentLength > 0 then begin
              if LBodyStream.Position = LBodyStream.Size then begin
                LEntityBuffer := @LOverlappedContext^.EOFProbeBuffer;
                LEntityBufferLength := SizeOf(LOverlappedContext^.EOFProbeBuffer);
              end
              else begin
                LEntityBuffer := @LBodyStream.DataString[low(AnsiString) + LBodyStream.Position];
                LEntityBufferLength := LBodyStream.Size - LBodyStream.Position;
              end;
            end
            else begin
              var LBodyStreamSize := LBodyStream.Size + HttpSysRequestBodyStreamDefaultSize - (LBodyStream.Size - LBodyStream.Position);
              if (LBodyStreamSize > FOwner.MaxBodySize) and (FOwner.MaxBodySize > 0) then
                raise EALHttpServerBodySizeTooBig.CreateFmt('Body size (%d bytes) is bigger than the maximum allowed size (%d bytes)', [LBodyStreamSize, FOwner.MaxBodySize]);
              LBodyStream.Size := LBodyStreamSize;
              LEntityBuffer := @LBodyStream.DataString[low(AnsiString) + LBodyStream.Position];
              LEntityBufferLength := LBodyStream.Size - LBodyStream.Position;
            end;
            //-
            ZeroMemory(@LOverlappedContext^.Overlapped, SizeOf(TOverlapped));
            LOverlappedContext^.Operation := OP_RECEIVE_REQUEST_ENTITY_BODY;
            LOverlappedContext^.CompletedInline := False;
            var LBytesReturned: ULONG;
            var LStatus := HttpReceiveRequestEntityBody(
                             Fowner.FRequestQueueHandle, // RequestQueueHandle: HANDLE;
                             LOverlappedContext^.HttpApiRequest^.RequestId, // RequestId: HTTP_REQUEST_ID;
                             HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER, // Flags: ULONG;
                                                                                // HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER:
                                                                                //   Specifies that the buffer will be filled with one or more entity bodies,
                                                                                //   unless there are no remaining entity bodies to copy.
                             LEntityBuffer, // EntityBuffer: PVOID;
                             LEntityBufferLength, // EntityBufferLength: ULONG;
                             @LBytesReturned, // BytesReturned: PULONG; // OPTIONAL
                             @LOverlappedContext^.Overlapped); // Overlapped: LPOVERLAPPED);
            if LStatus <> ERROR_IO_PENDING then begin
              LOverlappedContext^.CompletedInline := True;
              LOverlappedContext^.HttpApiStatus := LStatus;
              LOverlappedContext^.NumberOfBytesTransferred := LBytesReturned;
            end
            else begin
              LOverlappedContext := nil;
              AtomicIncrement(Fowner.FIOCPPendingContinuationCount);
            end;
            Continue;
          end;
          {$ENDREGION}

          {$REGION 'ERROR_HANDLE_EOF'}
          If LOverlappedContext^.HttpApiStatus = ERROR_HANDLE_EOF then begin
            var LBodyStream := TALStringStreamA(LOverlappedContext^.HttpSysRequest.BodyStream);
            var LContentLength: integer;
            if not ALTryStrToInt(LOverlappedContext^.HttpSysRequest.Headers.ContentLength, LContentLength) then LContentLength := 0;
            if LContentLength > 0 then begin
              if LBodyStream.Position <> LBodyStream.Size then
                raise EALHttpServerConnectionDropped.Createfmt('Client Dropped Connection. Total Bytes indicated by Header: %d. Total Bytes Read: %d', [LContentLength, LBodyStream.Position]);
            end
            else begin
              if (LBodyStream.Position > FOwner.MaxBodySize) and (FOwner.MaxBodySize > 0) then
                raise EALHttpServerBodySizeTooBig.CreateFmt('Body size (%d bytes) is bigger than the maximum allowed size (%d bytes)', [LBodyStream.Position, FOwner.MaxBodySize]);
              LBodyStream.Size := LBodyStream.Position;
            end;
            //-
            LOverlappedContext^.HttpApiStatus := NO_ERROR;
          end;
          {$ENDREGION}

          {$REGION 'Check HttpApiStatus'}
          ALCheckWinApiErrorCode('HttpReceiveRequestEntityBody', LOverlappedContext^.HttpApiStatus, FHttpApiModuleHandle);
          {$ENDREGION}

          {$REGION 'Sanity check: no unread body data'}
          if LOverlappedContext^.HttpSysRequest.BodyStream.Position <> LOverlappedContext^.HttpSysRequest.BodyStream.Size then
            Raise Exception.Create('Error 274506B5-AB35-4CB0-921C-ABDC07FB88EC');
          {$ENDREGION}

          {$REGION 'Create HttpSysResponse'}
          ALFreeAndNil(LOverlappedContext^.HttpSysResponse);
          LOverlappedContext^.HttpSysResponse := TALHttpSysServerResponse.Create;
          {$ENDREGION}

          {$REGION 'Call OnRequest'}
          Try
            If Assigned(FOwner.OnRequest) then
              FOwner.OnRequest(LOverlappedContext^.HttpSysRequest, LOverlappedContext^.HttpSysResponse);
          Except
            On E: Exception do begin
              ALLog('TALHttpSysServer.TWorkerThread.Execute(OnRequest)', E);
              LOverlappedContext^.HttpSysResponse.StatusCode := 500;
              LOverlappedContext^.HttpSysResponse.Reason := 'Internal Server Error';
            end;
          End;
          {$ENDREGION}

          {$REGION 'Build HttpApiResponse'}
          ZeroMemory(LOverlappedContext^.HttpApiResponse, SizeOf(HTTP_RESPONSE));
          LOverlappedContext^.HttpApiResponse^.StatusCode := LOverlappedContext^.HttpSysResponse.StatusCode;
          LOverlappedContext^.HttpApiResponse^.pReason := PAnsiChar(LOverlappedContext^.HttpSysResponse.Reason);
          LOverlappedContext^.HttpApiResponse^.ReasonLength := length(LOverlappedContext^.HttpSysResponse.Reason);
          case LOverlappedContext^.HttpSysResponse.Version of
            TALHttpVersion.Unspecified: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 0;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 0;
            end;
            TALHttpVersion.v0_9: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 0;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 9;
            end;
            TALHttpVersion.v1_0: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 1;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 0;
            end;
            TALHttpVersion.v1_1: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 1;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 1;
            end;
            TALHttpVersion.v2: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 2;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 0;
            end;
            TALHttpVersion.v3: begin
              LOverlappedContext^.HttpApiResponse^.Version.MajorVersion := 3;
              LOverlappedContext^.HttpApiResponse^.Version.MinorVersion := 0;
            end
            else
              Raise Exception.Create('Error E24B2C84-121F-4D47-991F-521C1B2681AE');
          end;

          If LOverlappedContext^.HttpSysResponse.FHeaders <> nil then begin
            var LHeaders := TALHttpSysServerResponseHeaders(LOverlappedContext^.HttpSysResponse.FHeaders);
            for var I := low(LHeaders.FKnownHeaders) to High(LHeaders.FKnownHeaders) do begin
              if LHeaders.FKnownHeaders[I] <> '' then begin
                LOverlappedContext^.HttpApiResponse^.Headers.KnownHeaders[I].pRawValue := PAnsiChar(LHeaders.FKnownHeaders[I]);
                LOverlappedContext^.HttpApiResponse^.Headers.KnownHeaders[I].RawValueLength := Length(LHeaders.FKnownHeaders[I]);
              end;
            end;
            //--
            var LHttpSysCookiesCount: Integer := 0;
            if LHeaders.FCookies <> nil then begin
              setlength(LOverlappedContext^.HttpApiCookieStrings, LHeaders.FCookies.Count);
              for var I := 0 to LHeaders.FCookies.Count - 1 do begin
                if LHeaders.FCookies[i].Name <> '' then begin
                  LOverlappedContext^.HttpApiCookieStrings[LHttpSysCookiesCount] := LHeaders.FCookies[i].HeaderValue;
                  inc(LHttpSysCookiesCount);
                end;
              end;
            end;
            //--
            if (LHeaders.FUnknownHeaders <> nil) or (LHttpSysCookiesCount > 1) then begin
              if LHeaders.FUnknownHeaders <> nil then setlength(LOverlappedContext^.HttpApiUnknownHeaders, LHeaders.FUnknownHeaders.Count + Max(0, LHttpSysCookiesCount - 1))
              else setlength(LOverlappedContext^.HttpApiUnknownHeaders, Max(0, LHttpSysCookiesCount - 1));
              LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount := 0;
              if LHeaders.FUnknownHeaders <> nil then begin
                for var I := 0 to LHeaders.FUnknownHeaders.Count - 1 do begin
                  var LName := LHeaders.FUnknownHeaders.Names[I];
                  if LName <> '' then begin
                    LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].pName := PAnsiChar(LName);
                    LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].NameLength := Length(LName);
                    var LValue := LHeaders.FUnknownHeaders.ValueFromIndex[I];
                    LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].pRawValue := PAnsiChar(LValue);
                    LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].RawValueLength := Length(LValue);
                    inc(LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount);
                  end;
                end;
              end;
              for var I := 1 to LHttpSysCookiesCount - 1 do begin
                LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].pName := PAnsiChar('Set-Cookie');
                LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].NameLength := 10; {Length('Set-Cookie')};
                LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].pRawValue := PAnsiChar(LOverlappedContext^.HttpApiCookieStrings[I]);
                LOverlappedContext^.HttpApiUnknownHeaders[LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount].RawValueLength := Length(LOverlappedContext^.HttpApiCookieStrings[I]);
                inc(LOverlappedContext^.HttpApiResponse^.Headers.UnknownHeaderCount);
              end;
              LOverlappedContext^.HttpApiResponse^.Headers.pUnknownHeaders := @LOverlappedContext^.HttpApiUnknownHeaders[0];
            end;
            //--
            if LHttpSysCookiesCount > 0 then begin
              LOverlappedContext^.HttpApiResponse^.Headers.KnownHeaders[Ord(HTTP_HEADER_ID.HttpHeaderSetCookie)].pRawValue := PAnsiChar(LOverlappedContext^.HttpApiCookieStrings[0]);
              LOverlappedContext^.HttpApiResponse^.Headers.KnownHeaders[Ord(HTTP_HEADER_ID.HttpHeaderSetCookie)].RawValueLength := Length(LOverlappedContext^.HttpApiCookieStrings[0]);
            end;
          end;

          If LOverlappedContext^.HttpSysRequest.Verb <> 'HEAD' then begin
            if LOverlappedContext^.HttpSysResponse.BodyFileHandle <> INVALID_HANDLE_VALUE then begin
              ZeroMemory(LOverlappedContext^.HttpApiDataChunk, SizeOf(HTTP_DATA_CHUNK));
              LOverlappedContext^.HttpApiDataChunk^.DataChunkType := HTTP_DATA_CHUNK_TYPE.HttpDataChunkFromFileHandle;
              LOverlappedContext^.HttpApiDataChunk^.FromFileHandle.ByteRange.StartingOffset.QuadPart := LOverlappedContext^.HttpSysResponse.BodyByteRangeStartingOffset;
              LOverlappedContext^.HttpApiDataChunk^.FromFileHandle.ByteRange.Length.QuadPart := LOverlappedContext^.HttpSysResponse.BodyByteRangeLength;
              LOverlappedContext^.HttpApiDataChunk^.FromFileHandle.FileHandle := LOverlappedContext^.HttpSysResponse.BodyFileHandle;
              LOverlappedContext^.HttpApiResponse^.EntityChunkCount := 1;
              LOverlappedContext^.HttpApiResponse^.pEntityChunks := LOverlappedContext^.HttpApiDataChunk;
            end
            else if LOverlappedContext^.HttpSysResponse.FBodyStream <> nil then begin
              ZeroMemory(LOverlappedContext^.HttpApiDataChunk, SizeOf(HTTP_DATA_CHUNK));
              LOverlappedContext^.HttpApiDataChunk^.DataChunkType := HTTP_DATA_CHUNK_TYPE.HttpDataChunkFromMemory;
              if (LOverlappedContext^.HttpSysResponse.FBodyStream is TALStringStreamA) then begin
                LOverlappedContext^.HttpApiDataChunk^.fromMemory.pBuffer := PAnsiChar(TALStringStreamA(LOverlappedContext^.HttpSysResponse.FBodyStream).DataString);
                LOverlappedContext^.HttpApiDataChunk^.fromMemory.BufferLength := LOverlappedContext^.HttpSysResponse.FBodyStream.Size;
              end
              else if (LOverlappedContext^.HttpSysResponse.FBodyStream is TCustomMemoryStream) then begin
                LOverlappedContext^.HttpApiDataChunk^.fromMemory.pBuffer := TCustomMemoryStream(LOverlappedContext^.HttpSysResponse.FBodyStream).Memory;
                LOverlappedContext^.HttpApiDataChunk^.fromMemory.BufferLength := LOverlappedContext^.HttpSysResponse.FBodyStream.Size;
              end
              else
                raise Exception.Create('Unsupported stream type. Only TALStringStreamA and TCustomMemoryStream are supported.');
              LOverlappedContext^.HttpApiResponse^.EntityChunkCount := 1;
              LOverlappedContext^.HttpApiResponse^.pEntityChunks := LOverlappedContext^.HttpApiDataChunk;
            end;
          end;
          {$ENDREGION}

          {$REGION 'Build HttpApiCachePolicy'}
          ZeroMemory(LOverlappedContext^.HttpApiCachePolicy, SizeOf(HTTP_CACHE_POLICY));
          case LOverlappedContext^.HttpSysResponse.CachePolicyType of
            TALHttpServerResponse.TCachePolicyType.Nocache: LOverlappedContext^.HttpApiCachePolicy^.Policy := HTTP_CACHE_POLICY_TYPE.HttpCachePolicyNocache;
            TALHttpServerResponse.TCachePolicyType.UserInvalidates: LOverlappedContext^.HttpApiCachePolicy^.Policy := HTTP_CACHE_POLICY_TYPE.HttpCachePolicyUserInvalidates;
            TALHttpServerResponse.TCachePolicyType.TimeToLive: LOverlappedContext^.HttpApiCachePolicy^.Policy := HTTP_CACHE_POLICY_TYPE.HttpCachePolicyTimeToLive;
            else raise Exception.Create('Error 0F4DCF43-E7A9-47F8-A308-3146865D13BA');
          end;
          LOverlappedContext^.HttpApiCachePolicy^.SecondsToLive := LOverlappedContext^.HttpSysResponse.CacheSecondsToLive;
          {$ENDREGION}

          {$REGION 'Build LHttpLogData'}
          var LHttpLogData: PHTTP_LOG_DATA;
          If FOwner.LoggingInfo.Enabled then begin
            ZeroMemory(LOverlappedContext^.HttpApiLogFieldsData, SizeOf(HTTP_LOG_FIELDS_DATA));
            LOverlappedContext^.HttpApiLogFieldsData^.Base.&Type := HTTP_LOG_DATA_TYPE.HttpLogDataTypeFields;
            if (HTTP_LOG_FIELD_USER_NAME and FOwner.LoggingInfo.Fields) <> 0 then begin
              // HttpSysRequest.CookedUrl.UserName is one of the few mutable fields in HttpSysRequest,
              // so the application can set/update it during OnRequest (e.g., after authentication).
              LOverlappedContext^.HttpApiLogFieldStringsW[0] := String(LOverlappedContext^.HttpSysRequest.CookedUrl.UserName);
              LOverlappedContext^.HttpApiLogFieldsData^.UserName := PWideChar(LOverlappedContext^.HttpApiLogFieldStringsW[0]);
              LOverlappedContext^.HttpApiLogFieldsData^.UserNameLength := Length(LOverlappedContext^.HttpApiLogFieldStringsW[0]) * SizeOf(WideChar);
            end;
            if (HTTP_LOG_FIELD_URI_STEM and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldStringsW[1] := String(LOverlappedContext^.HttpSysRequest.CookedUrl.Path);
              LOverlappedContext^.HttpApiLogFieldsData^.UriStem := PWideChar(LOverlappedContext^.HttpApiLogFieldStringsW[1]);
              LOverlappedContext^.HttpApiLogFieldsData^.UriStemLength := Length(LOverlappedContext^.HttpApiLogFieldStringsW[1]) * SizeOf(WideChar);
            end;
            if (HTTP_LOG_FIELD_CLIENT_IP and FOwner.LoggingInfo.Fields) <> 0 then begin
              IF not ALIsZeroIPv4(LOverlappedContext^.HttpSysRequest.RemoteAddress.IpV4) then
                LOverlappedContext^.HttpApiLogFieldStringsA[0] := ALNumericToIPv4StrA(LOverlappedContext^.HttpSysRequest.RemoteAddress.IpV4)
              else IF not ALIsZeroIPv6(LOverlappedContext^.HttpSysRequest.RemoteAddress.IpV6) then
                LOverlappedContext^.HttpApiLogFieldStringsA[0] := ALBinaryToIPv6StrA(LOverlappedContext^.HttpSysRequest.RemoteAddress.IpV6)
              else
                LOverlappedContext^.HttpApiLogFieldStringsA[0] := '';
              LOverlappedContext^.HttpApiLogFieldsData^.ClientIp := PAnsiChar(LOverlappedContext^.HttpApiLogFieldStringsA[0]);
              LOverlappedContext^.HttpApiLogFieldsData^.ClientIpLength := length(LOverlappedContext^.HttpApiLogFieldStringsA[0]);
            end;
            //if (HTTP_LOG_FIELD_COMPUTER_NAME and FOwner.LoggingInfo.Fields) <> 0 then begin
              //LOverlappedContext^.HttpApiLogFieldsData^.ServerNameLength: USHORT;
              //LOverlappedContext^.HttpApiLogFieldsData^.ServerName: PCHAR;
            //end;
            //if (HTTP_LOG_FIELD_SITE_NAME and FOwner.LoggingInfo.Fields) <> 0 then begin
              //LOverlappedContext^.HttpApiLogFieldsData^.ServiceName: PCHAR;
              //LOverlappedContext^.HttpApiLogFieldsData^.ServiceNameLength: USHORT;
            //end;
            if (HTTP_LOG_FIELD_SERVER_IP and FOwner.LoggingInfo.Fields) <> 0 then begin
              IF not ALIsZeroIPv4(LOverlappedContext^.HttpSysRequest.LocalAddress.IpV4) then
                LOverlappedContext^.HttpApiLogFieldStringsA[1] := ALNumericToIPv4StrA(LOverlappedContext^.HttpSysRequest.LocalAddress.IpV4)
              else IF not ALIsZeroIPv6(LOverlappedContext^.HttpSysRequest.LocalAddress.IpV6) then
                LOverlappedContext^.HttpApiLogFieldStringsA[1] := ALBinaryToIPv6StrA(LOverlappedContext^.HttpSysRequest.LocalAddress.IpV6)
              else
                LOverlappedContext^.HttpApiLogFieldStringsA[1] := '';
              LOverlappedContext^.HttpApiLogFieldsData^.ServerIp := PAnsiChar(LOverlappedContext^.HttpApiLogFieldStringsA[1]);
              LOverlappedContext^.HttpApiLogFieldsData^.ServerIpLength := length(LOverlappedContext^.HttpApiLogFieldStringsA[1]);
            end;
            if (HTTP_LOG_FIELD_METHOD and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldsData^.Method := PAnsiChar(LOverlappedContext^.HttpSysRequest.Verb);
              LOverlappedContext^.HttpApiLogFieldsData^.MethodLength := length(LOverlappedContext^.HttpSysRequest.Verb);
              LOverlappedContext^.HttpApiLogFieldsData^.MethodNum := HTTP_VERB.HttpVerbUnparsed;
            end;
            if (HTTP_LOG_FIELD_URI_QUERY and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldsData^.UriQuery := PAnsiChar(LOverlappedContext^.HttpSysRequest.CookedUrl.QueryString);
              LOverlappedContext^.HttpApiLogFieldsData^.UriQueryLength := Length(LOverlappedContext^.HttpSysRequest.CookedUrl.QueryString);
            end;
            if (HTTP_LOG_FIELD_HOST and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldsData^.Host := PAnsiChar(LOverlappedContext^.HttpSysRequest.CookedUrl.Host);
              LOverlappedContext^.HttpApiLogFieldsData^.HostLength := Length(LOverlappedContext^.HttpSysRequest.CookedUrl.Host);
            end;
            if (HTTP_LOG_FIELD_USER_AGENT and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldsData^.UserAgent := PAnsiChar(LOverlappedContext^.HttpSysRequest.Headers.UserAgent);
              LOverlappedContext^.HttpApiLogFieldsData^.UserAgentLength := Length(LOverlappedContext^.HttpSysRequest.Headers.UserAgent);
            end;
            if (HTTP_LOG_FIELD_COOKIE and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldStringsA[2] := LOverlappedContext^.HttpSysRequest.Headers.Cookies.Text;
              LOverlappedContext^.HttpApiLogFieldsData^.Cookie := PAnsiChar(LOverlappedContext^.HttpApiLogFieldStringsA[2]);
              LOverlappedContext^.HttpApiLogFieldsData^.CookieLength := Length(LOverlappedContext^.HttpApiLogFieldStringsA[2]);
            end;
            if (HTTP_LOG_FIELD_REFERER and FOwner.LoggingInfo.Fields) <> 0 then begin
              LOverlappedContext^.HttpApiLogFieldsData^.Referrer := PAnsiChar(LOverlappedContext^.HttpSysRequest.Headers.Referer);
              LOverlappedContext^.HttpApiLogFieldsData^.ReferrerLength := Length(LOverlappedContext^.HttpSysRequest.Headers.Referer);
            end;
            if (HTTP_LOG_FIELD_SERVER_PORT and FOwner.LoggingInfo.Fields) <> 0 then
              LOverlappedContext^.HttpApiLogFieldsData^.ServerPort := USHORT(LOverlappedContext^.HttpSysRequest.CookedUrl.Port);
            if (HTTP_LOG_FIELD_STATUS and FOwner.LoggingInfo.Fields) <> 0 then
              LOverlappedContext^.HttpApiLogFieldsData^.ProtocolStatus := USHORT(LOverlappedContext^.HttpSysResponse.StatusCode);
            //if (HTTP_LOG_FIELD_WIN32_STATUS and FOwner.LoggingInfo.Fields) <> 0 then LOverlappedContext^.HttpApiLogFieldsData^.Win32Status: ULONG;
            //if (HTTP_LOG_FIELD_SUB_STATUS and FOwner.LoggingInfo.Fields) <> 0 then LOverlappedContext^.HttpApiLogFieldsData^.SubStatus: USHORT;
            LHttpLogData := PHTTP_LOG_DATA(@LOverlappedContext^.HttpApiLogFieldsData^);
          end
          else
            LHttpLogData := nil;
          {$ENDREGION}

          {$REGION 'Send response'}
          ZeroMemory(@LOverlappedContext^.Overlapped, SizeOf(TOverlapped));
          LOverlappedContext^.Operation := OP_SEND_HTTP_RESPONSE;
          LOverlappedContext^.CompletedInline := False;
          var LStatus := HttpSendHttpResponse(
                           FOwner.FRequestQueueHandle, // RequestQueueHandle: HANDLE;
                           LOverlappedContext^.HttpApiRequest^.RequestId, // RequestId: HTTP_REQUEST_ID;
                           0, // Flags: ULONG;
                              // HTTP_SEND_RESPONSE_FLAG_DISCONNECT:
                              //   The network connection should be disconnected after sending this response,
                              //   overriding any persistent connection features associated with the version
                              //   of HTTP in use.
                              // HTTP_SEND_RESPONSE_FLAG_MORE_DATA:
                              //   Additional entity body data for this response is sent by the application
                              //   through one or more subsequent calls to HttpSendResponseEntityBody. The last
                              //   call sending entity-body data then sets this flag to zero.
                              // HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA:
                              //   This flag enables buffering of data in the kernel on a per-response basis.
                              //   It should be used by an application doing synchronous I/O or by an application
                              //   doing asynchronous I/O with no more than one outstanding send at a time.
                              //   Applications that use asynchronous I/O and that may have more than one send
                              //   outstanding at a time should not use this flag.
                              //   When this flag is set, it should also be used consistently in calls to the
                              //   HttpSendResponseEntityBody function.
                              // HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING:
                              //   Enables the TCP nagling algorithm for this send only.
                              // HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES
                              //   Specifies that for a range request, the full response content is
                              //   passed and the caller wants the HTTP API to process ranges appropriately.
                              // HTTP_SEND_RESPONSE_FLAG_OPAQUE
                              //   Specifies that the request/response is not HTTP compliant and all subsequent
                              //   bytes should be treated as entity-body. Applications specify this flag when
                              //   it is accepting a Web Socket upgrade request and informing HTTP.sys to treat
                              //   the connection data as opaque data.
                              //   This flag is only allowed when the StatusCode member of pHttpResponse is 101,
                              //   switching protocols. HttpSendHttpResponse returns ERROR_INVALID_PARAMETER for
                              //   all other HTTP response types if this flag is used.
                           LOverlappedContext^.HttpApiResponse, // HttpResponse: PHTTP_RESPONSE;
                           LOverlappedContext^.HttpApiCachePolicy, // CachePolicy: PHTTP_CACHE_POLICY; // OPTIONAL
                           nil, // BytesSent: PULONG; // OPTIONAL
                           nil, // Reserved1: PVOID; // Reserved
                           0, // Reserved2: ULONG; // Reserved
                           @LOverlappedContext^.Overlapped, // Overlapped: LPOVERLAPPED; // OPTIONAL
                           LHttpLogData); // LogData: PHTTP_LOG_DATA
          if LStatus <> ERROR_IO_PENDING then begin
            LOverlappedContext^.CompletedInline := True;
            LOverlappedContext^.HttpApiStatus := LStatus;
            LOverlappedContext^.NumberOfBytesTransferred := 0;
          end
          else begin
            LOverlappedContext := nil;
            AtomicIncrement(Fowner.FIOCPPendingContinuationCount);
          end;
          Continue;
          {$ENDREGION}

        end;
        {$ENDREGION}

        {$REGION 'OP_SEND_HTTP_RESPONSE'}
        OP_SEND_HTTP_RESPONSE: begin

          {$REGION 'Check HttpApiStatus'}
          ALCheckWinApiErrorCode('HttpSendHttpResponse', LOverlappedContext^.HttpApiStatus, FHttpApiModuleHandle);
          {$ENDREGION}

          {$REGION 'Break loop to post new initial HttpReceiveHttpRequest'}
          LOverlappedContext^.CompletedInline := False;
          Continue;
          {$ENDREGION}

        end;
        {$ENDREGION}

        {$REGION 'Unexpected case'}
        Else
          Raise Exception.Create('Error 45E57B42-4FB2-4ACB-A674-546A85F5B93A')
        {$ENDREGION}

      end;

    Except

      {$REGION 'Exception handling and logging'}
      On E: Exception do begin
        ALLog('TALHttpSysServer.TWorkerThread.Execute', E);
        if LOverlappedContext <> nil then LOverlappedContext^.CompletedInline := False;
      end;
      {$ENDREGION}

    End;

  end;
  AtomicDecrement(FOwner.FBusyWorkerThreadCount);

end;

{**********************************}
constructor TALHttpSysServer.Create;
begin
  inherited Create;
  FUrlPrefixes := TALStringListW.Create;
  FLoggingInfo := TLoggingInfo.Create;
  FWorkerThreads := TObjectList<TWorkerThread>.Create(true{AOwnsObjects});
  FMinWorkerThreadCount := 0;
  FMaxWorkerThreadCount := 0;
  FWorkerThreadCount := 0;
  FBusyWorkerThreadCount := 0;
  FPeakWorkerThreadCount := 0;
  FPeakBusyWorkerThreadCount := 0;
  FRequestQueueHandle := 0;
  FIoCompletionPort := 0;
  FServerSessionId := 0;
  FUrlGroupId := 0;
  FIOCPPendingInitialReceiveCount := 0;
  FIOCPPendingContinuationCount := 0;
  FMinInitialReceiveCount := 0;
  FMaxInitialReceiveCount := 0;
end;

{**********************************}
destructor TALHttpSysServer.Destroy;
begin
  inherited; // Will call stop
  ALFreeAndNil(FUrlPrefixes);
  ALFreeAndNil(FLoggingInfo);
  ALFreeAndNil(FWorkerThreads);
end;

{*******************************}
procedure TALHttpSysServer.Start;
begin

  // Validate configuration: at least one HTTP.SYS URL prefix is required.
  // Example: 'http://+:23456/' (note the trailing slash).
  if UrlPrefixes.Count = 0 then
    Raise Exception.Create('No URL prefixes configured. Add at least one entry to UrlPrefixes (e.g. "http://+:23456/") before calling Start.');

  // Initializes the HTTP Server API driver
  if FRunning then Exit;
  ALCheckWinApiErrorCode(
    'HttpInitialize',
    HttpInitialize(
      HTTPAPI_VERSION_2, // Version: HTTPAPI_VERSION;
      HTTP_INITIALIZE_SERVER, // Flags: ULONG;
      nil), // pReserved: PVOID
    FHttpApiModuleHandle);
  FRunning := True;
  try

    // Creates a server session
    ALCheckWinApiErrorCode(
      'HttpCreateServerSession',
      HttpCreateServerSession(
        HTTPAPI_VERSION_2, // Version: HTTPAPI_VERSION;
        @FServerSessionId, // ServerSessionId: PHTTP_SERVER_SESSION_ID;
        0), // Reserved: ULONG
      FHttpApiModuleHandle);

    // Creates a URL Group
    ALCheckWinApiErrorCode(
      'HttpCreateUrlGroup',
      HttpCreateUrlGroup(
        FServerSessionId, // ServerSessionId: HTTP_SERVER_SESSION_ID;
        @FUrlGroupId, // pUrlGroupId: PHTTP_URL_GROUP_ID;
        0), // Reserved: ULONG
      FHttpApiModuleHandle);

    // Adds the specified URL to the URL Group
    for var I := 0 to FUrlPrefixes.Count - 1 do begin
      var LRes := HttpAddUrlToUrlGroup(
                    FUrlGroupId, // UrlGroupId: HTTP_URL_GROUP_ID;
                    PWideChar(FUrlPrefixes[I]), // pFullyQualifiedUrl: PCWSTR;
                    0, // UrlContext: HTTP_URL_CONTEXT; // OPTIONAL
                    0); // Reserved: ULONG
      If LRes = ERROR_ACCESS_DENIED then
        Raise Exception.CreateFmt(
                'Access denied when adding URL prefix "%s".' + #13#10 +
                'Run as administrator OR reserve the prefix for this account:' + #13#10 +
                'netsh http add urlacl url="%s" user="{account}"' + #13#10 +
                'If using HTTPS, also bind a certificate (by IP:port):' + #13#10 +
                'netsh http add sslcert ipport=0.0.0.0:{port} certhash={thumbprint} appid={GUID}' + #13#10 +
                'Note: URL prefixes must end with a slash (e.g. "http://+:8080/").',
                [FUrlPrefixes[I], FUrlPrefixes[I]]);
      ALCheckWinApiErrorCode('HttpAddUrlToUrlGroup', LRes, FHttpApiModuleHandle);
    end;

    // Creates a new request queue
    ALCheckWinApiErrorCode(
      'HttpCreateRequestQueue',
      HttpCreateRequestQueue(
        HTTPAPI_VERSION_2, // Version: HTTPAPI_VERSION;
        nil, // Name: PCWSTR; // OPTIONAL
        nil, // SecurityAttributes: PSECURITY_ATTRIBUTES; // OPTIONAL
        0, // Flags: ULONG; // OPTIONAL
           // HTTP_CREATE_REQUEST_QUEUE_FLAG_CONTROLLER:
           //   The handle to the request queue created using this flag cannot
           //   be used to perform I/O operations. This flag can be set only
           //   when the request queue handle is created.
           // HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING
           //   The HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING flag allows
           //   applications to open an existing request queue by name and
           //   retrieve the request queue handle. The pName parameter must
           //   contain a valid request queue name; it cannot be NULL.
        @FRequestQueueHandle), // RequestQueueHandle: PHANDLE)
      FHttpApiModuleHandle);

    // Sets the URL Group association with a request queue.
    var LHttpBindingInfo: HTTP_BINDING_INFO;
    LHttpBindingInfo.Flags.Present := 1;
    LHttpBindingInfo.RequestQueueHandle := FRequestQueueHandle;
    ALCheckWinApiErrorCode(
      'HttpSetUrlGroupProperty',
      HttpSetUrlGroupProperty(
        FUrlGroupId, // UrlGroupId: HTTP_URL_GROUP_ID;
        HTTP_SERVER_PROPERTY.HttpServerBindingProperty, // &Property: HTTP_SERVER_PROPERTY;
        @LHttpBindingInfo, // PropertyInformation: PVOID;
        SizeOf(LHttpBindingInfo)), // PropertyInformationLength: ULONG
      FHttpApiModuleHandle);

    // Sets logging for the URL Group.
    if FLoggingInfo.Enabled then begin
      var LHttpLoggingInfo: HTTP_LOGGING_INFO;
      ZeroMemory(@LHttpLoggingInfo, sizeof(HTTP_LOGGING_INFO));
      LHttpLoggingInfo.Flags.Present := 1;
      LHttpLoggingInfo.LoggingFlags := FLoggingInfo.LoggingFlags;
      LHttpLoggingInfo.SoftwareName := PWideChar(FLoggingInfo.SoftwareName);
      LHttpLoggingInfo.SoftwareNameLength := Length(FLoggingInfo.SoftwareName) * sizeof(WideChar);
      LHttpLoggingInfo.DirectoryNameLength := Length(FLoggingInfo.DirectoryName) * sizeof(WideChar);
      LHttpLoggingInfo.DirectoryName := PWideChar(FLoggingInfo.DirectoryName);
      LHttpLoggingInfo.Format := FLoggingInfo.Format;
      LHttpLoggingInfo.Fields := FLoggingInfo.Fields;
      LHttpLoggingInfo.RolloverType := FLoggingInfo.RolloverType;
      LHttpLoggingInfo.RolloverSize := FLoggingInfo.RolloverSize;
      ALCheckWinApiErrorCode(
        'HttpSetUrlGroupProperty',
        HttpSetUrlGroupProperty(
          FUrlGroupId, // UrlGroupId: HTTP_URL_GROUP_ID;
          HTTP_SERVER_PROPERTY.HttpServerLoggingProperty, // &Property: HTTP_SERVER_PROPERTY;
          @LHttpLoggingInfo, // PropertyInformation: PVOID;
          SizeOf(LHttpLoggingInfo)), // PropertyInformationLength: ULONG
        FHttpApiModuleHandle);
    end;

    // Configures I/O completion behavior on the request queue handle.
    // FILE_SKIP_COMPLETION_PORT_ON_SUCCESS means that if an I/O completes
    // synchronously (returns NO_ERROR), http.sys will NOT post a completion
    // packet to the IOCP. The application must handle the inline completion
    // immediately. This reduces spurious IOCP wakeups.
    ALCheckWinApiBoolean(
      'SetFileCompletionNotificationModes',
      SetFileCompletionNotificationModes(
        FRequestQueueHandle, // FileHandle: THandle
        FILE_SKIP_COMPLETION_PORT_ON_SUCCESS)); // Flags: UCHAR
                                                // FILE_SKIP_COMPLETION_PORT_ON_SUCCESS:
                                                //   If the following three conditions are true, the I/O
                                                //   Manager does not queue a completion entry to the port,
                                                //   when it would ordinarily do so. The conditions are:
                                                //     * A completion port is associated with the file handle.
                                                //     * The file is opened for asynchronous I/O.
                                                //     * A request returns success immediately without returning
                                                //       ERROR_PENDING.
                                                // FILE_SKIP_SET_EVENT_ON_HANDLE:
                                                //   The I/O Manager does not set the event for the file object
                                                //   if a request returns with a success code, or the error
                                                //   returned is ERROR_PENDING and the function that is called
                                                //   is not a synchronous function.
                                                //   If an explicit event is provided for the request,
                                                //   it is still signaled.

    // Create IOCP and bind the request queue to it
    FIoCompletionPort := ALCheckWinApiHandle(
                           'CreateIoCompletionPort',
                           CreateIoCompletionPort(
                             FRequestQueueHandle, // FileHandle: THandle
                             0, // ExistingCompletionPort: THandle
                             0, // CompletionKey: ULONG_PTR
                             0)); // NumberOfConcurrentThreads: DWORD

    // Create all WorkerThreads
    var LWorkerThreadCount := FMinWorkerThreadCount;
    if LWorkerThreadCount = 0 then LWorkerThreadCount := Max(1, TThread.ProcessorCount * 4);
    {$IF defined(DEBUG)}
    //LWorkerThreadCount := 1;
    {$ENDIF}
    AtomicExchange(FMinInitialReceiveCount, LWorkerThreadCount * 2);
    AtomicExchange(FMaxInitialReceiveCount, LWorkerThreadCount * 4);
    AtomicExchange(FWorkerThreadCount, LWorkerThreadCount);
    AtomicExchange(FPeakWorkerThreadCount, LWorkerThreadCount);
    For var I := 1 to LWorkerThreadCount do
      FWorkerThreads.Add(TWorkerThread.Create(Self{AOwner}));

  except
    Stop;
    raise;
  end;
end;

{******************************}
procedure TALHttpSysServer.Stop;
begin

  // If not Running do nothing
  If not FRunning then Exit;
  FRunning := False;

  // Terminate all worker threads
  for var I := FWorkerThreads.Count - 1 downto 0 do
    FWorkerThreads[i].Terminate;

  // Stop new requests & cancel outstanding I/O
  if FRequestQueueHandle <> 0 then begin
    ALCheckWinApiErrorCode(
      'HttpShutdownRequestQueue',
      HttpShutdownRequestQueue(FRequestQueueHandle), // RequestQueueHandle: HANDLE
      FHttpApiModuleHandle);
  end;

  // Wait until all worker threads have exited
  while FWorkerThreads.Count > 0 do begin
    if FIoCompletionPort <> 0 then
      ALCheckWinApiBoolean(
        'PostQueuedCompletionStatus',
        PostQueuedCompletionStatus(
          FIoCompletionPort, // CompletionPort: THandle;
          0, // dwNumberOfBytesTransferred: DWORD;
          0, // dwCompletionKey: UIntPtr;
          nil)); // lpOverlapped: POverlapped
    for var I := FWorkerThreads.Count - 1 downto 0 do
      if FWorkerThreads[I].Finished then
        FWorkerThreads.Delete(i);
    sleep(10);
  end;

  // Update counts
  AtomicExchange(FWorkerThreadCount, 0);
  AtomicExchange(FBusyWorkerThreadCount, 0);
  AtomicExchange(FPeakWorkerThreadCount, 0);
  AtomicExchange(FPeakBusyWorkerThreadCount, 0);
  AtomicExchange(FIOCPPendingInitialReceiveCount, 0);
  AtomicExchange(FIOCPPendingContinuationCount, 0);
  AtomicExchange(FMinInitialReceiveCount, 0);
  AtomicExchange(FMaxInitialReceiveCount, 0);

  // Closes the request queue
  if FRequestQueueHandle <> 0 then begin
    ALCheckWinApiErrorCode(
      'HttpCloseRequestQueue',
      HttpCloseRequestQueue(FRequestQueueHandle), // RequestQueueHandle: HANDLE
      FHttpApiModuleHandle);
    FRequestQueueHandle := 0;
  end;

  // Closes the IOCP
  if FIoCompletionPort <> 0 then begin
    ALCheckWinApiBoolean('CloseHandle(FIoCompletionPort)', CloseHandle(FIoCompletionPort));
    FIoCompletionPort := 0;
  end;

  // Removes the specified URL from the group
  // Closes the URL Group
  if FUrlGroupId <> 0 then begin
    ALCheckWinApiErrorCode(
      'HttpRemoveUrlFromUrlGroup',
      HttpRemoveUrlFromUrlGroup(
        FUrlGroupId, // UrlGroupId: HTTP_URL_GROUP_ID;
        nil, // pFullyQualifiedUrl: PCWSTR;
        HTTP_URL_FLAG_REMOVE_ALL), // Flags: ULONG
      FHttpApiModuleHandle);
    ALCheckWinApiErrorCode(
      'HttpCloseUrlGroup',
      HttpCloseUrlGroup(FUrlGroupId), // UrlGroupId: HTTP_URL_GROUP_ID
      FHttpApiModuleHandle);
    FUrlGroupId := 0;
  end;

  // Deletes the server session
  if FServerSessionId <> 0 then begin
    ALCheckWinApiErrorCode(
      'HttpCloseServerSession',
      HttpCloseServerSession(FServerSessionId), // ServerSessionId: HTTP_SERVER_SESSION_ID
      FHttpApiModuleHandle);
    FServerSessionId := 0;
  end;

  // Cleans up resources used by the HTTP Server API
  ALCheckWinApiErrorCode(
    'HttpTerminate',
    HttpTerminate(
      HTTP_INITIALIZE_SERVER, // Flags: ULONG;
      nil), // pReserved: PVOID
    FHttpApiModuleHandle);

end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.HTTP.Server.HttpSys','initialization');
  {$ENDIF}
  TALHttpSysServer.FHttpApiModuleHandle := GetModuleHandle(HTTPAPI_DLL);

end.
