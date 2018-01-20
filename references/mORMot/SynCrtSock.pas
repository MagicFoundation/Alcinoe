/// classes implementing HTTP/1.1 client and server protocol
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrtSock;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2018 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alfred Glaenzer (alf)
  - Cybexr
  - EMartin
  - Eric Grange
  - EvaF
  - Maciej Izak (hnb)
  - Marius Maximus
  - Pavel (mpv)
  - Willo vd Merwe

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


   TCP/IP and HTTP/1.1 Client and Server
  ***************************************

  Initial version: 2009 May, by Arnaud Bouchez

  Version 1.4 - February 8, 2010
  - whole Synopse SQLite3 database framework released under the GNU Lesser
    General Public License version 3, instead of generic "Public Domain"
  - fix a bug happening when multiple HTTP connections were opened and
    closed in the same program

  Version 1.5 - March 1, 2010
  - new generic unix implementation, using libc sockets, in SynLibcSock.pas

  Version 1.9
  - avoid some GPF during client deconnection when the server shut down
  - rewrite HTTP Server handle request loop keep alive timing
  - HTTP Server now use a Thread Pool to speed up multi-connections: this
    speed up a lot HTTP/1.0 requests, by creating a Thread only if
    necessary

  Version 1.9.2
  - deleted deprecated DOS related code (formerly used with DWPL Dos Extender)
  - a dedicated thread is now used if the incoming HTTP request has
    POSTed a body content of more than 16 KB (to avoid Deny Of Service, and
    preserve the Thread Pool to only real small processes)
  - new CROnly parameter for TCrtSocket.SockRecvLn, to handle #13 as
    line delimiter: by default, #10 or #13#10 are line delimiters
    (as for normal Window/Linux text files)

  Version 1.12
  - added connection check and exception handling in
    THttpServerSocket.GetRequest, which now is a function returning a boolean
  - added DOS / TCP SYN Flood detection if THttpServerSocket.GetRequest
    spent more than 2 seconds to get header from HTTP Client

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - new THttpApiServer class, using fast http.sys kernel-mode server
    for better performance and less resource usage
  - DOS / TCP SYN Flood detection time enhanced to 5 seconds
  - fixed HTTP client stream layout (to be more RFC compliant)
  - new generic compression handling mechanism: can handle gzip, deflate
    or custom synlz / synlzo algorithms via THttpSocketCompress functions
  - new THttpServerGeneric.Request virtual abstract method prototype
  - new TWinINet class, using WinINet API (very slow, do not use)
  - new TWinHTTP class, using WinHTTP API (faster than THttpClientSocket):
    this is the class to be used

  Version 1.15
  - unit now tested with Delphi XE2 (32 Bit)
  - fixed issue in HTTP_RESPONSE.SetHeaders()

  Version 1.16
  - fixed issue in case of wrong void parameter e.g. in THttpApiServer.AddUrl
  - circumvent some bugs of Delphi XE2 background compiler (main compiler is OK)
  - added 'RemoteIP: 127.0.0.1' to the retrieved HTTP headers
  - major speed up of THttpApiServer for Windows Vista and up, by processing
    huge content in chunks: upload of 100Mb file take 25 sec before and 6 sec
    after changes, according to feedback by MPV - ticket 711247b998
  - new THttpServerGeneric.OnHttpThreadTerminate event, available to clean-up
    any process in the thread context, when it is terminated (to call e.g.
    TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread in order to call
    CoUnInitialize from thread in which CoInitialize was initialy made) - see
    https://synopse.info/fossil/tktview?name=213544b2f5

  Version 1.17
  - replaced TSockData string type to the generic RawByteString type (and
    the default AnsiString for non-Unicode version of Delphi)
  - added optional aProxyName, aProxyByPass parameters to TWinHttpAPI /
    TWinInet and TWinHTTP constructors
  - added THttpServerGeneric.OnHttpThreadStart property, and associated
    TNotifyThreadEvent event prototype
  - handle 'Range: bytes=***-***' request in THttpApiServer

  Version 1.18
  - replaced RawByteString type (defined locally for non-Unicode version of
    Delphi) by a dedicated SockString type, used for data storage and for
    simple ASCII content (like URIs or port number)
  - added and tested Linux support (FPC/CrossKylix), via sockets or libcurl API
  - introducing THttpServerRequest class for HTTP server context and THttpRequest
    (replacing TWinHttpAPI) as abstract parent for HTTP client classes
  - http.sys kernel-mode server now handles HTTP API 2.0 (available since
    Windows Vista / Server 2008), or fall back to HTTP API 1.0 (for Windows XP
    or Server 2003) - thanks pavel for the feedback and initial patch!
  - deep code refactoring of thread process, especially for TSynThreadPool as
    used by THttpServer: introducing TSynThread and TSynThreadPoolSubThread;
    as a result, it fixes OnHttpThreadStart and OnHttpThreadTerminate to be
    triggered from every thread, and propagated from THttpApiServer's clones
  - added TCrtSocket.TCPNoDelay/SendTimeout/ReceiveTimeout/KeepAlive properties
  - added optional parameter to set buffer size for TCrtSocket.CreateSockIn
    and TCrtSocket.CreateSockOut methods
  - renamed misleading TCrtSocket.Snd method as overloaded SockSend, and
    refactored public fields as read/only properties
  - fixed long-standing random bug occuring when a TCrtSocket connection is
    closed then reopened (e.g. when THttpClientSocket.Request does its retrial);
    thanks hnb for the patch!
  - added THttpServerRequest.UseSSL property to check if connection is secured
  - added optional queue name for THttpApiServer.Create constructor [149cf42383]
  - added THttpApiServer.RemoveUrl() method
  - introduced THttpApiServer.ReceiveBufferSize property
  - added THttpApiServer.HTTPQueueLength property (for HTTP API 2.0 only)
  - added THttpApiServer.MaxBandwidth and THttpApiServer.MaxConnections
    properties (for HTTP API 2.0 only) - thanks mpv for the proposal!
  - added THttpApiServer.LogStart/LogStop for HTTP API 2.0 integrated logging
  - introducing new THttpApiServer.SetAuthenticationSchemes() method for HTTP
    API 2.0, and the corresponding THttpServerRequest.AuthenticationStatus and
    AuthenticatedUser properties
  - added THttpApiServer.SetTimeOutLimits() method for HTTP API 2.0
  - added THttpApiServer.ServerSessionID and UrlGroupID read-only properties
  - let HTTP_RESPONSE.AddCustomHeader() recognize all known headers
  - THttpApiServer won't try to send an error message when connection is broken
  - added error check for HttpSendHttpResponse() API call
  - added EWinHTTP exception, raised when TWinHttp client fails to connect
  - added aTimeOut optional parameter to TCrtSocket.Open() constructor
  - added function HtmlEncode()
  - some code cleaning about 64-bit compilation (including [540628f498])
  - refactored HTTP_DATA_CHUNK record definition into HTTP_DATA_CHUNK_* records
    to circumvent XE3 alignemnt issue
  - WinSock-based THttpServer will avoid creating a thread per connection,
    when the maximum of 256 threads is reached in the pool, with an exception
    of kept-alife or huge body requets (avoiding DoS attacks by limiting the
    total number of created threads)
  - allow WinSock-based THttpServer to set a server address ('1.2.3.4:1234')
  - let WinSock-based THttpServer.Process() handle HTTP_RESP_STATICFILE
  - force disable range checking and other compiler options for this unit
  - included more detailed information to HTTP client User-Agent header
  - added ConnectionTimeOut, SendTimeout and ReceiveTimeout optional parameters
    to THttpRequest constructors - feature request [bfe485b678]
  - added optional aCompressMinSize parameter to RegisterCompress() methods
  - added THttpRequest.Get/Post/Put/Delete() class functions for easy remote
    resource retrieval using either WinHTTP or WinINet APIs
  - added TURI structure, ready to parse a supplied HTTP URI
  - added ConnectionID the HTTP server context - see request [0636eeec54]
  - added THttpRequest.IgnoreSSLCertificateErrors property (proposal by EMartin)
  - added THttpRequest AuthScheme and AuthUserName/AuthPassword properties, for
    authentication - only implemented at TWinHttp level (thanks Eric Grange)
  - fixed TCrtSocket.BytesIn and TCrtSocket.BytesOut properties
  - fixed ticket [82df275784] THttpRequest with responses without Content-Length
  - fixed ticket [f0749956af] TWinINet does not work with HTTPS servers
  - fixed ticket [842a5ae15a] THttpApiServer.Execute/SendError message
  - fixed ticket [f2ae4022a4] EWinINet error handling
  - fixed ticket [73da2c17b1] about Accept-Encoding header in THttpApiServer
  - fixed ticket [cbcbb3b2fc] about PtrInt definition
  - fixed ticket [91f8f3ec6f] about error retrieving unknown headers
  - fixed ticket [f79ff5714b] about potential finalization issues as .bpl in IDE
  - fixed ticket [2d53fc43e3] about unneeded port 80
  - fixed ticket [11b327bd77] about TCrtSocket not working with Delphi 2009+
  - fixed ticket [0f6ecdaf55] for better compatibility with HTTP/1.1 cache
  - fixed ticket [814f6bd65a] about missing OnHttpThreadStart in CreateClone
  - fixed ticket [51a9c086f3] about THttpApiServer.SetHTTPQueueLength()
  - fixed potential Access Violation error at THttpServerResp shutdown
  - removed several compilation hints when assertions are set to off
  - added aRegisterURI optional parameter to THttpApiServer.AddUrl() method
  - made exception error messages more explicit (tuned per module)
  - fixed several issues when releasing THttpApiServer and THttpServer instances
  - allow to use any Unicode content for SendEmail() - also includes
    SendEmailSubject() function, for feature request [0a5fdf9129]
  - added support for TLS1.1 & TLS1.2 for TWinHTTP
  - added advanced exception text if case of HTTPS connection problems
  - added HTTP.SYS 2.0 web socket API TWebSocketAPI
  - added HTTP.SYS 2.0 based WebSocket server THttpApiWebSocketServer
  - added direct SChannel API TLS support on Windows 

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$ifdef USEWININET}
  WinInet,
  {$endif}
  {$ifndef DELPHI5OROLDER}
  Types,
  {$endif}
{$else MSWINDOWS}
  {$undef USEWININET}
  {$ifdef FPC}
  Sockets,
  SynFPCSock,
  SynFPCLinux,
  BaseUnix, // for fpgetrlimit/fpsetrlimit
  {$else}
  {$ifndef DELPHI5OROLDER}
  Types,
  {$endif}
  {$endif}
  {$ifdef KYLIX3}
  KernelIoctl, // for IoctlSocket/ioctl FION* constants
  LibC,
  SynFPCSock,  // shared with Kylix
  SynKylix,
  {$endif}
{$endif MSWINDOWS}
{$ifndef LVCL}
  Contnrs,
  SyncObjs, // for TEvent (in Classes.pas for LVCL)
{$endif}
  SysUtils,
  Classes;

const
  /// the full text of the current Synopse mORMot framework version
  // - match the value defined in SynCommons.pas and SynopseCommit.inc
  // - we don't supply full version number with build revision, to reduce
  // potential attack surface
  XPOWEREDPROGRAM = 'mORMot 1.18';

  /// the running Operating System
  XPOWEREDOS = {$ifdef MSWINDOWS} 'Windows' {$else}
                 {$ifdef LINUXNOTBSD} 'Linux' {$else} 'Posix' {$endif LINUXNOTBSD}
               {$endif MSWINDOWS};

  /// used by THttpApiServer.Request for http.sys to send a static file
  // - the OutCustomHeader should contain the proper 'Content-type: ....'
  // corresponding to the file (e.g. by calling GetMimeContentType() function
  // from SynCommons supplyings the file name)
  // - should match HTML_CONTENT_STATICFILE constant defined in mORMot.pas unit
  HTTP_RESP_STATICFILE = '!STATICFILE';
  /// used to notify e.g. the THttpServerRequest not to wait for any response
  // from the client
  // - is not to be used in normal HTTP process, but may be used e.g. by
  // TWebSocketProtocolRest.ProcessFrame() to avoid to wait for an incoming
  // response from the other endpoint
  // - should match NORESPONSE_CONTENT_TYPE constant defined in mORMot.pas unit
  HTTP_RESP_NORESPONSE = '!NORESPONSE';

var
  /// THttpRequest timeout default value for DNS resolution
  // - leaving to 0 will let system default value be used
  HTTP_DEFAULT_RESOLVETIMEOUT: integer = 0;
  /// THttpRequest timeout default value for remote connection
  // - default is 60 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  HTTP_DEFAULT_CONNECTTIMEOUT: integer = 60000;
  /// THttpRequest timeout default value for data sending
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_SENDTIMEOUT: integer = 30000;
  /// THttpRequest timeout default value for data receiving
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_RECEIVETIMEOUT: integer = 30000;

type
{$ifdef UNICODE}
  /// define the fastest Unicode string type of the compiler
  SynUnicode = UnicodeString;
  /// define a raw 8-bit storage string type, used for data buffer management
  SockString = type RawByteString;
{$else}
  /// define the fastest 16-bit Unicode string type of the compiler
  SynUnicode = WideString;
  {$ifdef HASCODEPAGE} // FPC may expect a CP, e.g. to compare two string constants
  SockString = type RawByteString;
  {$else}
  /// define a 8-bit raw storage string type, used for data buffer management
  SockString = type AnsiString;
  {$endif}
{$endif}
  /// points to a 8-bit raw storage variable, used for data buffer management
  PSockString = ^SockString;

  /// defines a dynamic array of SockString
  TSockStringDynArray = array of SockString;

{$ifdef DELPHI5OROLDER}
  // not defined in Delphi 5 or older
  PPointer = ^Pointer;
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);
  UTF8String = AnsiString;
  UTF8Encode = AnsiString;
{$endif}

{$ifndef FPC}

  /// FPC 64-bit compatibility integer type
  {$ifdef CPU64}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  {$else}
  PtrInt = integer;
  PtrUInt = cardinal;
  {$endif}
  /// FPC 64-bit compatibility pointer type
  PPtrInt = ^PtrInt;
  PPtrUInt = ^PtrUInt;

{$endif FPC}

  {$M+}
  /// exception thrown by the classes of this unit
  ECrtSocket = class(Exception)
  protected
    fLastError: integer;
  public
    /// will concat the message with the WSAGetLastError information
    constructor Create(const Msg: string); overload;
    /// will concat the message with the supplied WSAGetLastError information
    constructor Create(const Msg: string; Error: integer); overload;
    /// will concat the message with the supplied WSAGetLastError information
    constructor CreateFmt(const Msg: string; const Args: array of const; Error: integer); overload;
  published
    /// the associated WSAGetLastError value
    property LastError: integer read fLastError;
  end;
  {$M-}

  TCrtSocketClass = class of TCrtSocket;

  /// the available available network transport layer
  // - either TCP/IP, UDP/IP or Unix sockets
  TCrtSocketLayer = (cslTCP, cslUDP, cslUNIX);

  /// identify the incoming data availability in TCrtSocket.SockReceivePending
  TCrtSocketPending = (cspSocketError, cspNoData, cspDataAvailable);

  PTextFile = ^TextFile;

  {$M+}
  /// Fast low-level Socket implementation
  // - direct access to the OS (Windows, Linux) network layer API
  // - use Open constructor to create a client to be connected to a server
  // - use Bind constructor to initialize a server
  // - use SockIn and SockOut (after CreateSock*) to read/readln or write/writeln
  //  as with standard Delphi text files (see SendEmail implementation)
  // - even if you do not use read(SockIn^), you may call CreateSockIn then
  // read the (binary) content via SockInRead/SockInPending methods, which would
  // benefit of the SockIn^ input buffer to maximize reading speed
  // - to write data, CreateSockOut and write(SockOut^) is not mandatory: you
  // rather may use SockSend() overloaded methods, followed by a SockFlush call
  // - in fact, you can decide whatever to use none, one or both SockIn/SockOut
  // - since this class rely on its internal optimized buffering system,
  // TCP_NODELAY is set to disable the Nagle algorithm
  // - our classes are (much) faster than the Indy or Synapse implementation
  TCrtSocket = class
  protected
    fSock: TSocket;
    fServer: SockString;
    fPort: SockString;
    fSockIn: PTextFile;
    fSockOut: PTextFile;
    fTimeOut: cardinal;
    fBytesIn: Int64;
    fBytesOut: Int64;
    fSocketLayer: TCrtSocketLayer;
    fSockInEof, fTLS: boolean;
    // updated by every SockSend() call
    fSndBuf: SockString;
    fSndBufLen: integer;
    // updated during UDP connection, accessed via PeerAddress/PeerPort
    fPeerAddr: TSockAddr;
    {$ifdef MSWINDOWS}
    fSecure: TSChannelClient;
    {$endif MSWINDOWS}
    procedure SetInt32OptionByIndex(OptName, OptVal: integer); virtual;
    procedure AcceptRequest(aClientSock: TSocket; aRemoteIP: PSockString);
  public
    /// common initialization of all constructors
    // - do not call directly, but use Open / Bind constructors instead
    constructor Create(aTimeOut: cardinal=10000); reintroduce; virtual;
    /// connect to aServer:aPort
    // - you may ask for a TLS secured client connection (only available under
    // Windows by now, using the SChanell API - and not fully tested)
    constructor Open(const aServer, aPort: SockString; aLayer: TCrtSocketLayer=cslTCP;
      aTimeOut: cardinal=10000; aTLS: boolean=false);
    /// bind to a Port
    // - expects the port to be specified as Ansi string, e.g. '1234'
    // - you can optionally specify a server address to bind to, e.g.
    // '1.2.3.4:1234'
    constructor Bind(const aPort: SockString; aLayer: TCrtSocketLayer=cslTCP);
    /// low-level internal method called by Open() and Bind() constructors
    // - raise an ECrtSocket exception on error
    // - you may ask for a TLS secured client connection (only available under
    // Windows by now, using the SChanell API - and not fully tested)
    procedure OpenBind(const aServer, aPort: SockString; doBind: boolean;
      aSock: integer=-1; aLayer: TCrtSocketLayer=cslTCP; aTLS: boolean=false);
    /// initialize SockIn for receiving with read[ln](SockIn^,...)
    // - data is buffered, filled as the data is available
    // - read(char) or readln() is indeed very fast
    // - multithread applications would also use this SockIn pseudo-text file
    // - by default, expect CR+LF as line feed (i.e. the HTTP way)
    procedure CreateSockIn(LineBreak: TTextLineBreakStyle=tlbsCRLF;
      InputBufferSize: Integer=1024);
    /// initialize SockOut for sending with write[ln](SockOut^,....)
    // - data is sent (flushed) after each writeln() - it's a compiler feature
    // - use rather SockSend() + SockSendFlush to send headers at once e.g.
    // since writeln(SockOut^,..) flush buffer each time
    procedure CreateSockOut(OutputBufferSize: Integer=1024);
    /// finalize SockIn receiving buffer
    // - you may call this method when you are sure that you don't need the
    // input buffering feature on this connection any more (e.g. after having
    // parsed the HTTP header, then rely on direct socket comunication)
    procedure CloseSockIn;
    /// finalize SockOut receiving buffer
    // - you may call this method when you are sure that you don't need the
    // output buffering feature on this connection any more (e.g. after having
    // parsed the HTTP header, then rely on direct socket comunication)
    procedure CloseSockOut;
    /// close and shutdown the connection (called from Destroy)
    procedure Close;
    /// close the opened socket, and corresponding SockIn/SockOut
    destructor Destroy; override;
    /// read Length bytes from SockIn buffer + Sock if necessary
    // - if SockIn is available, it first gets data from SockIn^.Buffer,
    // then directly receive data from socket if UseOnlySockIn=false
    // - if UseOnlySockIn=true, it will return the data available in SockIn^,
    // and returns the number of bytes
    // - can be used also without SockIn: it will call directly SockRecv()
    // in such case (assuming UseOnlySockin=false)
    function SockInRead(Content: PAnsiChar; Length: integer;
      UseOnlySockIn: boolean=false): integer;
    /// returns the number of bytes in SockIn buffer or pending in Sock
    // - if SockIn is available, it first check from any data in SockIn^.Buffer,
    // then call InputSock to try to receive any pending data if the buffer is
    // void - unless aSocketForceCheck is TRUE, and both the buffer and the
    // socket are asked for pending bytes (slower, but sometimes needed, e.g.
    // if you are currently waiting for a whole header block to be available)
    // - will wait up to the specified aTimeOutMS value (in milliseconds) for
    // incoming data
    // - returns -1 in case of a socket error (e.g. broken connection); you
    // can raise a ECrtSocket exception to propagate the error
    function SockInPending(aTimeOutMS: integer; aSocketForceCheck: boolean=false): integer;
    /// check the connection status of the socket
    function SockConnected: boolean;
    /// simulate writeln() with direct use of Send(Sock, ..)
    // - useful on multi-treaded environnement (as in THttpServer.Process)
    // - no temp buffer is used
    // - handle SockString, ShortString, Char, Integer parameters
    // - raise ECrtSocket exception on socket error
    procedure SockSend(const Values: array of const); overload;
    /// simulate writeln() with a single line
    procedure SockSend(const Line: SockString=''); overload;
    /// append P^ data into SndBuf (used by SockSend(), e.g.)
    // - call SockSendFlush to send it through the network via SndLow()
    procedure SockSend(P: pointer; Len: integer); overload;
    /// flush all pending data to be sent
    procedure SockSendFlush;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - raise ECrtSocket exception on socket error
    procedure SockRecv(Buffer: pointer; Length: integer);
    /// check if there are some pending bytes in the input sockets API buffer
    function SockReceivePending(TimeOutMS: cardinal): TCrtSocketPending;
    /// returns the socket input stream as a string
    function SockReceiveString: SockString;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - return false on any fatal socket error, true on success
    // - call Close if the socket is identified as shutdown from the other side 
    // - you may optionally set StopBeforeLength=true, then the read bytes count
    // are set in Length, even if not all expected data has been received - in
    // this case, Close method won't be called 
    function TrySockRecv(Buffer: pointer; var Length: integer; StopBeforeLength: boolean=false): boolean;
    /// call readln(SockIn^,Line) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - by default, will handle #10 or #13#10 as line delimiter (as normal text
    // files), but you can delimit lines using #13 if CROnly is TRUE
    procedure SockRecvLn(out Line: SockString; CROnly: boolean=false); overload;
    /// call readln(SockIn^) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - line content is ignored
    procedure SockRecvLn; overload;
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SockSend() or SockOut^ buffers
    procedure SndLow(P: pointer; Len: integer);
    /// direct send data through network
    // - return false on any error, true on success
    // - bypass the SndBuf or SockOut^ buffers
    function TrySndLow(P: pointer; Len: integer): boolean;
    /// returns the low-level error number
    // - i.e. returns WSAGetLastError
    function LastLowSocketError: Integer;
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SndBuf or SockOut^ buffers
    // - raw Data is sent directly to OS: no CR/CRLF is appened to the block
    procedure Write(const Data: SockString);
    /// direct accept an new incoming connection on a bound socket
    // - instance should have been setup as a server via a previous Bind() call
    // - returns nil on error or a ResultClass instance on success
    // - if ResultClass is nil, will return a plain TCrtSocket, but you may
    // specify e.g. THttpServerSocket if you expect incoming HTTP requests  
    function AcceptIncoming(RemoteIP: PSockString=nil;
      ResultClass: TCrtSocketClass=nil): TCrtSocket;
    /// remote IP address of the last packet received (SocketLayer=slUDP only)
    function PeerAddress: SockString;
    /// remote IP port of the last packet received (SocketLayer=slUDP only)
    function PeerPort: integer;
    /// set the TCP_NODELAY option for the connection
    // - default 1 (true) will disable the Nagle buffering algorithm; it should
    // only be set for applications that send frequent small bursts of information
    // without getting an immediate response, where timely delivery of data
    // is required - so it expects buffering before calling Write() or SndLow()
    // - you can set 0 (false) here to enable the Nagle algorithm, if needed
    // - see http://www.unixguide.net/network/socketfaq/2.16.shtml
    property TCPNoDelay: Integer index TCP_NODELAY write SetInt32OptionByIndex;
    /// set the SO_SNDTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking send calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property SendTimeout: Integer index SO_SNDTIMEO write SetInt32OptionByIndex;
    /// set the SO_RCVTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking receive calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property ReceiveTimeout: Integer index SO_RCVTIMEO write SetInt32OptionByIndex;
    /// set the SO_KEEPALIVE option for the connection
    // - 1 (true) will enable keep-alive packets for the connection
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ee470551
    property KeepAlive: Integer index SO_KEEPALIVE write SetInt32OptionByIndex;
    /// set the SO_LINGER option for the connection, to control its shutdown
    // - by default (or Linger<0), Close will return immediately to the caller,
    // and any pending data will be delivered if possible
    // - Linger > 0  represents the time in seconds for the timeout period
    // to be applied at Close; under Linux, will also set SO_REUSEADDR; under
    // Darwin, set SO_NOSIGPIPE
    // - Linger = 0 causes the connection to be aborted and any pending data
    // is immediately discarded at Close
    property Linger: Integer index SO_LINGER write SetInt32OptionByIndex;
    /// after CreateSockIn, use Readln(SockIn^,s) to read a line from the opened socket
    property SockIn: PTextFile read fSockIn;
    /// after CreateSockOut, use Writeln(SockOut^,s) to send a line to the opened socket
    property SockOut: PTextFile read fSockOut;
  published
    /// low-level socket handle, initialized after Open() with socket
    property Sock: TSocket read fSock write fSock;
    /// low-level socket type, initialized after Open() with socket
    property SocketLayer: TCrtSocketLayer read fSocketLayer;
    /// IP address, initialized after Open() with Server name
    property Server: SockString read fServer;
    /// IP port, initialized after Open() with port number
    property Port: SockString read fPort;
    /// if higher than 0, read loop will wait for incoming data till
    // TimeOut milliseconds (default value is 10000) - used also in SockSend()
    property TimeOut: cardinal read fTimeOut;
    /// total bytes received
    property BytesIn: Int64 read fBytesIn;
    /// total bytes sent
    property BytesOut: Int64 read fBytesOut;
  end;
  {$M-}

  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlzo' or 'synlz')
  // - the data is compressed (if Compress=TRUE) or uncompressed (if
  // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
  // - to be used with THttpSocket.RegisterCompress method
  // - DataRawByteStringtype should be a generic AnsiString/RawByteString, which
  // should be in practice a SockString or a RawByteString
  THttpSocketCompress = function(var DataRawByteString; Compress: boolean): AnsiString;

  /// used to maintain a list of known compression algorithms
  THttpSocketCompressRec = record
    /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
    Name: SockString;
    /// the function handling compression and decompression
    Func: THttpSocketCompress;
    /// the size in bytes after which compress will take place
    // - will be 1024 e.g. for 'zip' or 'deflate'
    // - could be 0 e.g. when encrypting the content, meaning "always compress"
    CompressMinSize: integer;
  end;

  /// list of known compression algorithms
  THttpSocketCompressRecDynArray = array of THttpSocketCompressRec;

  /// identify some items in a list of known compression algorithms
  THttpSocketCompressSet = set of 0..31;

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing HTTP/1.1 using the Socket API
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlzo/synlz) protocols
  THttpSocket = class(TCrtSocket)
  protected
    /// true if the TRANSFER-ENCODING: CHUNKED was set in headers
    Chunked: boolean;
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    /// GetHeader set index of protocol in fCompress[], from ACCEPT-ENCODING:
    fCompressHeader: THttpSocketCompressSet;
    /// same as HeaderValue('Content-Encoding'), but retrieved during Request
    // and mapped into the fCompress[] array
    fContentCompress: integer;
    /// compress the data, adding corresponding headers via SockSend()
    // - always add a 'Content-Length: ' header entry (even if length=0)
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Data is not '', will add 'Content-Type: ' header
    procedure CompressDataAndWriteHeaders(const OutContentType: SockString;
      var OutContent: SockString);
  public
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    TCPPrefix: SockString;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: SockString;
    /// will contain the header lines after a Request - use HeaderValue() to get one
    Headers: array of SockString;
    /// will contain the data retrieved from the server, after the Request
    Content: SockString;
    /// same as HeaderValue('Content-Length'), but retrieved during Request
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: integer;
    /// same as HeaderValue('Content-Type'), but retrieved during Request
    ContentType: SockString;
    /// same as HeaderValue('Connection')='Close', but retrieved during Request
    ConnectionClose: boolean;
    /// same as HeaderValue('Connection')='Upgrade', but retrieved during Request
    ConnectionUpgrade: boolean;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    procedure GetHeader;
    /// retrieve the HTTP body (after uncompression if necessary) into Content
    procedure GetBody;
    /// add an header entry, returning the just entered entry index in Headers[]
    function HeaderAdd(const aValue: SockString): integer;
    /// set all Header values at once, from CRLF delimited text
    procedure HeaderSetText(const aText: SockString;
      const aForcedContentType: SockString='');
    /// get all Header values at once, as CRLF delimited text
    function HeaderGetText: SockString; virtual;
    /// HeaderValue('Content-Type')='text/html', e.g.
    function HeaderValue(aName: SockString): SockString;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024): boolean;
  end;

  THttpServer = class;

  /// Socket API based HTTP/1.1 server class used by THttpServer Threads
  THttpServerSocket = class(THttpSocket)
  protected
    fMethod: SockString;
    fURL: SockString;
    fKeepAliveClient: boolean;
    fRemoteIP: SockString;
    fHeaderText: SockString;
    fServer: THttpServer;
  public
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    constructor Create(aServer: THttpServer); reintroduce;
    /// main object function called after aClientSock := Accept + Create:
    // - get initialize the socket with the supplied accepted socket
    // - caller will then use the GetRequest method below to
    // get the request
    procedure InitRequest(aClientSock: TSocket);
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if ContentLength<>0)
    // - return false if the socket was not connected any more, or if
    // any exception occured during the process
    function GetRequest(withBody: boolean=true): boolean;
    /// get all Header values at once, as CRLF delimited text
    // - this overridden version will add the 'RemoteIP: 1.2.3.4' header
    function HeaderGetText: SockString; override;
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    property Method: SockString read fMethod;
    /// contains the URL ('/' e.g.) after GetRequest()
    property URL: SockString read fURL;
    /// true if the client is HTTP/1.1 and 'Connection: Close' is not set
    // - default HTTP/1.1 behavior is "keep alive", unless 'Connection: Close'
    // is specified, cf. RFC 2068 page 108: "HTTP/1.1 applications that do not
    // support persistent connections MUST include the "close" connection option
    // in every message"
    property KeepAliveClient: boolean read fKeepAliveClient write fKeepAliveClient;
    /// the recognized client IP, after a call to InitRequest()
    property RemoteIP: SockString read fRemoteIP;
  end;

  /// Socket API based REST and HTTP/1.1 compatible client class
  // - this component is HTTP/1.1 compatible, according to RFC 2068 document
  // - the REST commands (GET/POST/PUT/DELETE) are directly available
  // - open connection with the server with inherited Open(server,port) function
  // - if KeepAlive>0, the connection is not broken: a further request (within
  // KeepAlive milliseconds) will use the existing connection if available,
  // or recreate a new one if the former is outdated or reset by server
  // (will retry only once); this is faster, uses less resources (especialy
  // under Windows), and is the recommended way to implement a HTTP/1.1 server
  // - on any error (timeout, connection closed) will retry once to get the value
  // - don't forget to use Free procedure when you are finished
  THttpClientSocket = class(THttpSocket)
  protected
    fUserAgent: SockString;
    fProcessName: SockString;
    procedure RequestSendHeader(const url, method: SockString); virtual;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    // - you can customize the default client timeouts by setting appropriate
    // aTimeout parameters (in ms) if you left the 0 default parameters,
    // it would use global HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(aTimeOut: cardinal=0); override;
    /// low-level HTTP/1.1 request
    // - called by all Get/Head/Post/Put/Delete REST methods
    // - after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    // - retry is false by caller, and will be recursively called with true to retry once
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const header, Data, DataType: SockString; retry: boolean): integer; virtual;

    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    function Get(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    // - if AuthToken<>'', will add an header with 'Authorization: Bearer '+AuthToken
    function GetAuth(const url, AuthToken: SockString; KeepAlive: cardinal=0): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise - only
    // header is read from server: Content is always '', but Headers are set
    function Head(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Post(const url, Data, DataType: SockString; KeepAlive: cardinal=0;
      const header: SockString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Put(const url, Data, DataType: SockString; KeepAlive: cardinal=0;
      const header: SockString=''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    function Delete(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;

    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    property UserAgent: SockString read fUserAgent write fUserAgent;
    /// the associated process name
    property ProcessName: SockString read fProcessName write fProcessName;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // SynBidirSock unit)
  THttpClientSocketClass = class of THttpClientSocket;

  {$ifndef LVCL}
  /// event prototype used e.g. by THttpServerGeneric.OnHttpThreadStart
  TNotifyThreadEvent = procedure(Sender: TThread) of object;
  {$endif}

  {$M+}
  TSynThreadPool = class;

  /// a simple TThread with a "Terminate" event run in the thread context
  // - the TThread.OnTerminate event is run within Synchronize() so did not
  // match our expectations to be able to release the resources in the thread
  // context which created them (e.g. for COM objects, or some DB drivers)
  // - used internally by THttpServerGeneric.NotifyThreadStart() - you should
  // not have to use the protected fOnTerminate event handler
  // - also define a Start method for compatibility with older versions of Delphi
  TSynThread = class(TThread)
  protected
    // ensure fOnTerminate is called only if NotifyThreadStart has been done
    fStartNotified: TObject;
    {$ifndef LVCL} // already available in LVCL
    // we re-defined an fOnTerminate event which would be run in the terminated
    // thread context (whereas TThread.OnTerminate is called in the main thread)
    // -> see THttpServerGeneric.OnHttpThreadTerminate event property
    fOnTerminate: TNotifyThreadEvent;
    procedure DoTerminate; override;
    {$endif}
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: boolean); reintroduce; virtual;
    {$ifndef HASTTHREADSTART}
    /// method to be called when the thread was created as suspended
    // - Resume is deprecated in the newest RTL, since some OS - e.g. Linux -
    // do not implement this pause/resume feature
    // - we define here this method for older versions of Delphi
    procedure Start;
    {$endif}
    /// safe version of Sleep() which won't break the thread process
    // - returns TRUE if the thread was Terminated
    // - returns FALSE if successfully waited up to MS milliseconds
    function SleepOrTerminated(MS: cardinal): boolean;
    /// defined as public since may be used to terminate the processing methods
    property Terminated;
  end;
  {$M-}

  /// HTTP response Thread as used by THttpServer Socket API based class
  // - Execute procedure get the request and calculate the answer
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TSynThread)
  protected
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    fClientSock: TSocket;
    fConnectionID: integer;
    /// main thread loop: read request from socket, send back answer
    procedure Execute; override;
  public
    /// initialize the response thread for the corresponding incoming socket
    // - this version will get the request directly from an incoming socket
    constructor Create(aSock: TSocket; aServer: THttpServer); reintroduce; overload;
    /// initialize the response thread for the corresponding incoming socket
    // - this version will handle KeepAlive, for such an incoming request
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer);
      reintroduce; overload; virtual;
    /// the associated socket to communicate with the client
    property ServerSock: THttpServerSocket read fServerSock;
    /// the associated main HTTP server instance
    property Server: THttpServer read fServer;
    /// the unique identifier of this connection
    property ConnectionID: integer read fConnectionID;
  end;

  /// metaclass of HTTP response Thread
  THttpServerRespClass = class of THttpServerResp;

  {$ifdef MSWINDOWS}
    // I/O completion ports API is the best option under Windows
    // under Linux/POSIX, we fallback to a classical event-driven pool
    {$define USE_WINIOCP}
  {$endif}

  /// defines the sub-threads used by TSynThreadPool
  TSynThreadPoolSubThread = class(TSynThread)
  protected
    fOwner: TSynThreadPool;
    fProcessingContext: pointer;
    fProcessingContextCS: TRTLCriticalSection;
    {$ifndef USE_WINIOCP}
    fEvent: TEvent;
    function AssignProcess(aContext: pointer): boolean;
    {$endif}
    procedure NotifyThreadStart(Sender: TSynThread);
  public
    /// initialize the thread
    constructor Create(Owner: TSynThreadPool); reintroduce;
    /// finalize the thread
    destructor Destroy; override;
    /// will loop for any pending IOCP commands, and execute fOwner.Task()
    procedure Execute; override;
  end;

  /// a simple Thread Pool, used e.g. for fast handling HTTP requests
  // - implemented over I/O Completion Ports under Windows, or a classical
  // Event-driven approach under Linux/POSIX
  TSynThreadPool = class
  protected
    {$ifdef USE_WINIOCP}
    fRequestQueue: THandle;
    {$endif}
    fSubThread: TObjectList; // holds TSynThreadPoolSubThread
    fRunningThreads: integer;
    fExceptionsCount: integer;
    fOnTerminate: TNotifyThreadEvent;
    fTerminated: boolean;
    fOnThreadStart: TNotifyThreadEvent;
    /// end thread on IO error
    function NeedStopOnIOError: boolean; virtual;
    /// process to be executed after notification
    procedure Task(aCaller: TSynThread; aContext: Pointer); virtual; abstract;
  public
    /// initialize a thread pool with the supplied number of threads
    // - abstract Task() virtual method will be called by one of the threads
    // - up to 256 threads can be associated to a Thread Pool
    // - can optionaly accept aOverlapHandle - a handle previously
    // opened for overlapped I/O (IOCP)
    constructor Create(NumberOfThreads: Integer=32
      {$ifdef USE_WINIOCP}; aOverlapHandle: THandle=INVALID_HANDLE_VALUE{$endif});
    /// shut down the Thread pool, releasing all associated threads
    destructor Destroy; override;
    /// let a task be processed by the Thread Pool
    // - returns false if there is no idle thread available in the pool (caller
    // should retry later)
    // - matches TOnThreadPoolSocketPush event handler signature
    function Push(aContext: pointer): boolean;
  published
    /// how many threads are currently running in this thread pool
    property RunningThreads: integer read fRunningThreads;
  end;

  /// a simple Thread Pool, used for fast handling HTTP requests of a THttpServer
  // - will handle multi-connection with less overhead than creating a thread
  // for each incoming request
  // - will create a THttpServerResp response thread, if the incoming request is
  // identified as HTTP/1.1 keep alive, or HTTP body length is bigger than 1 MB
  TSynThreadPoolTHttpServer = class(TSynThreadPool)
  protected
    fServer: THttpServer;
    fHeaderErrors: integer;
    fHeaderProcessed: integer;
    fBodyProcessed: integer;
    fBodyOwnThreads: integer;
    // here aContext is a pointer(TSocket=THandle) value
    procedure Task(aCaller: TSynThread; aContext: Pointer); override;
  public
    /// initialize a thread pool with the supplied number of threads
    // - Task() overridden method processs the HTTP request set by Push()
    // - up to 256 threads can be associated to a Thread Pool
    constructor Create(Server: THttpServer; NumberOfThreads: Integer=32); reintroduce;
  published
    /// how many invalid HTTP headers have been rejected by this thread pool
    property HeaderErrors: integer read fHeaderErrors write fHeaderErrors;
    /// how many HTTP headers have been processed by this thread pool
    property HeaderProcessed: integer read fHeaderProcessed write fHeaderProcessed;
    /// how many HTTP bodies have been processed by this thread pool
    property BodyProcessed: integer read fBodyProcessed write fBodyProcessed;
    /// how many HTTP bodies have been processed by a dedicated THttpServerResp thread
    property BodyOwnThreads: integer read fBodyOwnThreads write fBodyOwnThreads;
  end;

  {$M+} // to have existing RTTI for published properties
  THttpServerGeneric = class;
  {$M-}

  /// the server-side available authentication schemes
  // - as used by THttpServerRequest.AuthenticationStatus
  // - hraNone..hraKerberos will match low-level HTTP_REQUEST_AUTH_TYPE enum as
  // defined in HTTP 2.0 API and
  THttpServerRequestAuthentication = (
    hraNone, hraFailed, hraBasic, hraDigest, hraNtlm, hraNegotiate, hraKerberos);

  /// a generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  THttpServerRequest = class
  protected
    fURL, fMethod, fInHeaders, fInContent, fInContentType: SockString;
    fOutContent, fOutContentType, fOutCustomHeaders: SockString;
    fServer: THttpServerGeneric;
    fConnectionID: Int64;
    fConnectionThread: TSynThread;
    fUseSSL: boolean;
    fAuthenticationStatus: THttpServerRequestAuthentication;
    fAuthenticatedUser: SockString;
    {$ifdef MSWINDOWS}
    fHttpApiRequest: Pointer;
    {$endif}
  public
    /// initialize the context, associated to a HTTP server instance
    constructor Create(aServer: THttpServerGeneric; aConnectionID: Int64;
      aConnectionThread: TSynThread); virtual;
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aURL,aMethod,aInHeaders,aInContent,aInContentType,
      aRemoteIP: SockString);
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(additionalHeader: SockString);
    /// input parameter containing the caller URI
    property URL: SockString read fURL;
    /// input parameter containing the caller method (GET/POST...)
    property Method: SockString read fMethod;
    /// input parameter containing the caller message headers
    property InHeaders: SockString read fInHeaders;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    property InContent: SockString read fInContent;
    // input parameter defining the caller message body content type
    property InContentType: SockString read fInContentType;
    /// output parameter to be set to the response message body
    property OutContent: SockString read fOutContent write fOutContent ;
    /// output parameter to define the reponse message body content type
    // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE', defined
    // as STATICFILE_CONTENT_TYPE in mORMot.pas), then OutContent is the UTF-8
    // file name of a file which must be sent to the client via http.sys (much
    // faster than manual buffering/sending)
    // - if OutContentType is HTTP_RESP_NORESPONSE (i.e. '!NORESPONSE', defined
    // as NORESPONSE_CONTENT_TYPE in mORMot.pas), then the actual transmission
    // protocol may not wait for any answer - used e.g. for WebSockets
    property OutContentType: SockString read fOutContentType write fOutContentType;
    /// output parameter to be sent back as the response message header
    // - e.g. to set Content-Type/Location
    property OutCustomHeaders: SockString read fOutCustomHeaders write fOutCustomHeaders;
    /// the associated server instance
    // - may be a THttpServer or a THttpApiServer class
    property Server: THttpServerGeneric read fServer;
    /// the ID of the connection which called this execution context
    // - e.g. SynCrtSock's TWebSocketProcess.NotifyCallback method would use
    // this property to specify the client connection to be notified
    // - is set as an Int64 to match http.sys ID type, but will be an
    // increasing integer sequence for (web)socket-based servers
    property ConnectionID: Int64 read fConnectionID;
    /// the thread which owns the connection of this execution context
    // - depending on the HTTP server used, may not follow ConnectionID
    property ConnectionThread: TSynThread read fConnectionThread;
    /// is TRUE if the caller is connected via HTTPS
    // - only set for THttpApiServer class yet
    property UseSSL: boolean read fUseSSL;
    /// contains the THttpServer-side authentication status
    // - e.g. when using http.sys authentication with HTTP API 2.0
    property AuthenticationStatus: THttpServerRequestAuthentication
      read fAuthenticationStatus;
    /// contains the THttpServer-side authenticated user name, UTF-8 encoded
    // - e.g. when using http.sys authentication with HTTP API 2.0, the
    // domain user name is retrieved from the supplied AccessToken
    // - could also be set by the THttpServerGeneric.Request() method, after
    // proper authentication, so that it would be logged as expected
    property AuthenticatedUser: SockString read fAuthenticatedUser;
    {$ifdef MSWINDOWS}
    /// for THttpApiServer, points to a PHTTP_REQUEST structure
    // - not used by now for other servers 
    property HttpApiRequest: Pointer read fHttpApiRequest;
    {$endif}
  end;

  /// event handler used by THttpServerGeneric.OnRequest property
  // - Ctxt defines both input and output parameters
  // - result of the function is the HTTP error code (200 if OK, e.g.)
  // - OutCustomHeader will handle Content-Type/Location
  // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE' aka
  // STATICFILE_CONTENT_TYPE in mORMot.pas), then OutContent is the UTF-8 file
  // name of a file which must be sent to the client via http.sys (much faster
  // than manual buffering/sending) and  the OutCustomHeader should
  // contain the proper 'Content-type: ....'
  TOnHttpServerRequest = function(Ctxt: THttpServerRequest): cardinal of object;

  /// event handler used by THttpServerGeneric.OnBeforeBody property
  // - if defined, is called just before the body is retrieved from the client
  // - supplied parameters reflect the current input state
  // - should return STATUS_SUCCESS=200 to continue the process, or an HTTP
  // error code (e.g. STATUS_FORBIDDEN or STATUS_PAYLOADTOOLARGE) to reject
  // the request
  TOnHttpServerBeforeBody = function(const aURL,aMethod,aInHeaders,
    aInContentType,aRemoteIP: SockString; aContentLength: integer;
    aUseSSL: boolean): cardinal of object;

  {$M+}
  /// abstract class to implement a server thread
  // - do not use this class, but rather the THttpServer, THttpApiServer
  // or TAsynchFrameServer (as defined in SynBidirSock)
  TServerGeneric = class(TSynThread)
  protected
    fProcessName: SockString;
    fOnHttpThreadStart: TNotifyThreadEvent;
    procedure SetOnTerminate(const Event: TNotifyThreadEvent); virtual;
    procedure NotifyThreadStart(Sender: TSynThread);
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: boolean; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString); reintroduce; virtual;
  end;

  /// abstract class to implement a HTTP server
  // - do not use this class, but rather the THttpServer or THttpApiServer
  THttpServerGeneric = class(TServerGeneric)
  protected
    fShutdownInProgress: boolean;
    /// optional event handler for the virtual Request method
    fOnRequest: TOnHttpServerRequest;
    fOnBeforeBody: TOnHttpServerBeforeBody;
    fOnAfterResponseSent: TOnHttpServerRequest;
    fMaximumAllowedContentLength: cardinal;
    /// list of all registered compression algorithms
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    fServerName: SockString;
    fCurrentConnectionID: integer;
    fCanNotifyCallback: boolean;
    fRemoteIPHeader, fRemoteIPHeaderUpper: SockString;
    function GetAPIVersion: string; virtual; abstract;
    procedure SetServerName(const aName: SockString); virtual;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); virtual;
    procedure SetMaximumAllowedContentLength(aMax: cardinal); virtual;
    procedure SetRemoteIPHeader(const aHeader: SockString);
    function NextConnectionID: integer;
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: boolean; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString); override;
    /// override this function to customize your http server
    // - InURL/InMethod/InContent properties are input parameters
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - result of the function is the HTTP error code (200 if OK, e.g.),
    // - OutCustomHeader is available to handle Content-Type/Location
    // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE' or
    // STATICFILE_CONTENT_TYPE defined in mORMot.pas), then OutContent is the
    // UTF-8 file name of a file which must be sent to the client via http.sys
    // (much faster than manual buffering/sending) and  the OutCustomHeader should
    // contain the proper 'Content-type: ....'
    // - default implementation is to call the OnRequest event (if existing),
    // and will return STATUS_NOTFOUND if OnRequest was not set
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously, but with a given Ctxt instance for each)
    function Request(Ctxt: THttpServerRequest): cardinal; virtual;
    /// server can send a request back to the client, when the connection has
    // been upgraded e.g. to WebSockets
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return STATUS_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    // - warning: this void implementation will raise an ECrtSocket exception -
    // inherited classes should override it, e.g. as in TWebSocketServerRest
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; virtual;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024); virtual;
    /// you can call this method to prepare the HTTP server for shutting down
    procedure Shutdown;
    /// event handler called by the default implementation of the
    // virtual Request method
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    property OnRequest: TOnHttpServerRequest read fOnRequest write fOnRequest;
    /// event handler called just before the body is retrieved from the client
    // - should return STATUS_SUCCESS=200 to continue the process, or an HTTP
    // error code to reject the request immediatly, and close the connection
    property OnBeforeBody: TOnHttpServerBeforeBody read fOnBeforeBody write SetOnBeforeBody;
    /// event handler called after each working Thread is just initiated
    // - called in the thread context at first place in THttpServerGeneric.Execute
    property OnHttpThreadStart: TNotifyThreadEvent
      read fOnHttpThreadStart write fOnHttpThreadStart;
    /// event handler called when a working Thread is terminating
    // - called in the corresponding thread context
    // - the TThread.OnTerminate event will be called within a Synchronize()
    // wrapper, so it won't fit our purpose
    // - to be used e.g. to call CoUnInitialize from thread in which CoInitialize
    // was made, for instance via a method defined as such:
    // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
    // ! begin // TSQLDBConnectionPropertiesThreadSafe
    // !   fMyConnectionProps.EndCurrentThread;
    // ! end;
    // - is used e.g. by TSQLRest.EndCurrentThread for proper multi-threading
    property OnHttpThreadTerminate: TNotifyThreadEvent read fOnTerminate write SetOnTerminate;
    /// reject any incoming request with a body size bigger than this value
    // - default to 0, meaning any input size is allowed
    // - returns STATUS_PAYLOADTOOLARGE = 413 error if "Content-Length" incoming
    // header overflow the supplied number of bytes
    property MaximumAllowedContentLength: cardinal read fMaximumAllowedContentLength
      write SetMaximumAllowedContentLength;
    /// TRUE if the inherited class is able to handle callbacks
    // - only TWebSocketServer has this ability by now
    property CanNotifyCallback: boolean read fCanNotifyCallback;
    /// the value of a custom HTTP header containing the real client IP
    // - by default, the RemoteIP information will be retrieved from the socket
    // layer - but if the server runs behind some proxy service, you should
    // define here the HTTP header name which indicates the true remote client
    // IP value, mostly as 'X-Real-IP' or 'X-Forwarded-For'
    property RemoteIPHeader: SockString read fRemoteIPHeader write SetRemoteIPHeader;
  published
    /// returns the API version used by the inherited implementation
    property APIVersion: string read GetAPIVersion;
    /// the Server name, UTF-8 encoded, e.g. 'mORMot/1.18 (Linux)'
    // - will be served as "Server: ..." HTTP header
    // - for THttpApiServer, when called from the main instance, will propagate
    // the change to all cloned instances, and included in any HTTP API 2.0 log
    property ServerName: SockString read fServerName write SetServerName;
    /// the associated process name
    property ProcessName: SockString read fProcessName write fProcessName;
  end;

  {$ifndef UNICODE}
  ULONGLONG = Int64;
  {$endif}

  {$ifdef MSWINDOWS}

  HTTP_OPAQUE_ID = ULONGLONG;
  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
  HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
  HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;

  /// http.sys API 2.0 logging file supported layouts
  // - match low-level HTTP_LOGGING_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingType = (
    hltW3C, hltIIS, hltNCSA, hltRaw);

  /// http.sys API 2.0 logging file rollover types
  // - match low-level HTTP_LOGGING_ROLLOVER_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingRollOver = (
    hlrSize, hlrDaily, hlrWeekly, hlrMonthly, hlrHourly);

  /// http.sys API 2.0 logging option flags
  // - used to alter the default logging behavior
  // - hlfLocalTimeRollover would force the log file rollovers by local time,
  // instead of the default GMT time
  // - hlfUseUTF8Conversion will use UTF-8 instead of default local code page
  // - only one of hlfLogErrorsOnly and hlfLogSuccessOnly flag could be set
  // at a time: if neither of them are present, both errors and success will
  // be logged, otherwise mutually exclusive flags could be set to force only
  // errors or success logging
  // - match low-level HTTP_LOGGING_FLAG_* constants as defined in HTTP 2.0 API
  THttpApiLoggingFlags = set of (
    hlfLocalTimeRollover, hlfUseUTF8Conversion,
    hlfLogErrorsOnly, hlfLogSuccessOnly);

  /// http.sys API 2.0 fields used for W3C logging
  // - match low-level HTTP_LOG_FIELD_* constants as defined in HTTP 2.0 API
  THttpApiLogFields = set of (
    hlfDate, hlfTime, hlfClientIP, hlfUserName, hlfSiteName, hlfComputerName,
    hlfServerIP, hlfMethod, hlfURIStem, hlfURIQuery, hlfStatus, hlfWIN32Status,
    hlfBytesSent, hlfBytesRecv, hlfTimeTaken, hlfServerPort, hlfUserAgent,
    hlfCookie, hlfReferer, hlfVersion, hlfHost, hlfSubStatus);

  /// http.sys API 2.0 fields used for server-side authentication
  // - as used by THttpApiServer.SetAuthenticationSchemes/AuthenticationSchemes
  // - match low-level HTTP_AUTH_ENABLE_* constants as defined in HTTP 2.0 API
  THttpApiRequestAuthentications = set of (
    haBasic, haDigest, haNtlm, haNegotiate, haKerberos);

  /// HTTP server using fast http.sys kernel-mode server
  // - The HTTP Server API enables applications to communicate over HTTP without
  // using Microsoft Internet Information Server (IIS). Applications can register
  // to receive HTTP requests for particular URLs, receive HTTP requests, and send
  // HTTP responses. The HTTP Server API includes SSL support so that applications
  // can exchange data over secure HTTP connections without IIS. It is also
  // designed to work with I/O completion ports.
  // - The HTTP Server API is supported on Windows Server 2003 operating systems
  // and on Windows XP with Service Pack 2 (SP2). Be aware that Microsoft IIS 5
  // running on Windows XP with SP2 is not able to share port 80 with other HTTP
  // applications running simultaneously.
  THttpApiServer = class(THttpServerGeneric)
  protected
    /// the internal request queue
    fReqQueue: THandle;
    /// contain list of THttpApiServer cloned instances
    fClones: TObjectList;
    // if fClones=nil, fOwner contains the main THttpApiServer instance
    fOwner: THttpApiServer;
    /// list of all registered URL
    fRegisteredUnicodeUrl: array of SynUnicode;
    fServerSessionID: HTTP_SERVER_SESSION_ID;
    fUrlGroupID: HTTP_URL_GROUP_ID;
    fExecuteFinished: boolean;
    fLogData: pointer;
    fLogDataStorage: array of byte;
    fLoggingServiceName: SockString;
    fAuthenticationSchemes: THttpApiRequestAuthentications;
    fReceiveBufferSize: cardinal;
    procedure SetReceiveBufferSize(Value: cardinal);
    function GetRegisteredUrl: SynUnicode;
    function GetCloned: boolean;
    function GetHTTPQueueLength: Cardinal;
    procedure SetHTTPQueueLength(aValue: Cardinal);
    function GetMaxBandwidth: Cardinal;
    procedure SetMaxBandwidth(aValue: Cardinal);
    function GetMaxConnections: Cardinal;
    procedure SetMaxConnections(aValue: Cardinal);
    procedure SetOnTerminate(const Event: TNotifyThreadEvent); override;
    function GetAPIVersion: string; override;
    function GetLogging: boolean;
    procedure SetServerName(const aName: SockString); override;
    procedure SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody); override;
    procedure SetMaximumAllowedContentLength(aMax: cardinal); override;
    procedure SetLoggingServiceName(const aName: SockString);
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrive the content
    procedure Execute; override;
    /// retrieve flags for SendHttpResponse 
   // - if response content type is not HTTP_RESP_STATICFILE
    function GetSendResponseFlags(Ctxt: THttpServerRequest): integer; virtual;
    /// executed after successfully SendHttpResponse() API call
    // - if response content type is not HTTP_RESP_STATICFILE
    procedure DoAfterSentResponse(Ctxt: THttpServerRequest); virtual;
    /// create a clone
    constructor CreateClone(From: THttpApiServer); virtual;
    /// free resources (for not cloned server)
    procedure DestroyMainThread; virtual;
  public
    /// initialize the HTTP Service
    // - will raise an exception if http.sys is not available (e.g. before
    // Windows XP SP2) or if the request queue creation failed
    // - if you override this contructor, put the AddUrl() methods within,
    // and you can set CreateSuspended to FALSE
    // - if you will call AddUrl() methods later, set CreateSuspended to TRUE,
    // then call explicitely the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(CreateSuspended: boolean; QueueName: SynUnicode='';
      OnStart: TNotifyThreadEvent=nil; OnStop: TNotifyThreadEvent=nil;
      const ProcessName: SockString=''); reintroduce;
    /// release all associated memory and handles
    destructor Destroy; override;
    /// will clone this thread into multiple other threads
    // - could speed up the process on multi-core CPU
    // - will work only if the OnProcess property was set (this is the case
    // e.g. in TSQLHttpServer.Create() constructor)
    // - maximum value is 256 - higher should not be worth it
    procedure Clone(ChildThreadCount: integer);
    /// register the URLs to Listen On
    // - e.g. AddUrl('root','888')
    // - aDomainName could be either a fully qualified case-insensitive domain
    // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port)
    // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
    // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
    // running with enough rights (by default, UAC requires administrator rights
    // for adding an URL to http.sys registration list) - solution is to call
    // the THttpApiServer.AddUrlAuthorize class method during program setup
    // - if this method is not used within an overridden constructor, default
    // Create must have be called with CreateSuspended = TRUE and then call the
    // Resume method after all Url have been added
    // - if aRegisterURI is TRUE, the URI will be registered (need adminitrator
    // rights) - default is FALSE, as defined by Windows security policy
    function AddUrl(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'; aRegisterURI: boolean=false;
      aContext: Int64=0): integer;
    /// un-register the URLs to Listen On
    // - this method expect the same parameters as specified to AddUrl()
    // - return 0 (NO_ERROR) on success, an error code if failed (e.g.
    // -1 if the corresponding parameters do not match any previous AddUrl)
    function RemoveUrl(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'): integer;
    /// will authorize a specified URL prefix
    // - will allow to call AddUrl() later for any user on the computer
    // - if aRoot is left '', it will authorize any root for this port
    // - must be called with Administrator rights: this class function is to be
    // used in a Setup program for instance, especially under Vista or Seven,
    // to reserve the Url for the server
    // - add a new record to the http.sys URL reservation store
    // - return '' on success, an error message otherwise
    // - will first delete any matching rule for this URL prefix
    // - if OnlyDelete is true, will delete but won't add the new authorization;
    // in this case, any error message at deletion will be returned
    class function AddUrlAuthorize(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'; OnlyDelete: boolean=false): string;
    /// will register a compression algorithm
    // - overridden method which will handle any cloned instances
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024); override;
    /// access to the internal THttpApiServer list cloned by this main instance
    // - as created by Clone() method
    property Clones: TObjectList read fClones;
  public { HTTP API 2.0 methods and properties }
    /// can be used to check if the HTTP API 2.0 is available
    function HasAPI2: boolean;
    /// enable HTTP API 2.0 advanced timeout settings
    // - all those settings are set for the current URL group
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   SetTimeOutLimits(....);
    // - aEntityBody is the time, in seconds, allowed for the request entity
    // body to arrive - default value is 2 minutes
    // - aDrainEntityBody is the time, in seconds, allowed for the HTTP Server
    // API to drain the entity body on a Keep-Alive connection - default value
    // is 2 minutes
    // - aRequestQueue is the time, in seconds, allowed for the request to
    // remain in the request queue before the application picks it up - default
    // value is 2 minutes
    // - aIdleConnection is the time, in seconds, allowed for an idle connection;
    // is similar to THttpServer.ServerKeepAliveTimeOut - default value is
    // 2 minutes
    // - aHeaderWait is the time, in seconds, allowed for the HTTP Server API
    // to parse the request header - default value is 2 minutes
    // - aMinSendRate is the minimum send rate, in bytes-per-second, for the
    // response - default value is 150 bytes-per-second
    // - any value set to 0 will set the HTTP Server API default value
    procedure SetTimeOutLimits(aEntityBody, aDrainEntityBody,
      aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
    /// enable HTTP API 2.0 logging
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   LogStart(....);
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
    // - you can select the output folder and the expected logging layout
    // - aSoftwareName will set the optional W3C-only software name string
    // - aRolloverSize will be used only when aRolloverType is hlrSize
    procedure LogStart(const aLogFolder: TFileName;
      aType: THttpApiLoggingType=hltW3C;
      const aSoftwareName: TFileName='';
      aRolloverType: THttpApiLoggingRollOver=hlrDaily;
      aRolloverSize: cardinal=0;
      aLogFields: THttpApiLogFields=[hlfDate..hlfSubStatus];
      aFlags: THttpApiLoggingFlags=[hlfUseUTF8Conversion]);
    /// disable HTTP API 2.0 logging
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
    procedure LogStop;
    /// enable HTTP API 2.0 server-side authentication
    // - once enabled, the client sends an unauthenticated request: it is up to
    // the server application to generate the initial 401 challenge with proper
    // WWW-Authenticate headers; any further authentication steps will be
    // perform in kernel mode, until the authentication handshake is finalized;
    // later on, the application can check the AuthenticationStatus property
    // of THttpServerRequest and its associated AuthenticatedUser value
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   SetAuthenticationSchemes(....);
    // - this method will work on the current group, for all instances
    // - see HTTPAPI_AUTH_ENABLE_ALL constant to set all available schemes
    // - optional Realm parameters can be used when haBasic scheme is defined
    // - optional DomainName and Realm parameters can be used for haDigest
    procedure SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
      const DomainName: SynUnicode=''; const Realm: SynUnicode='');
    /// read-only access to HTTP API 2.0 server-side enabled authentication schemes
    property AuthenticationSchemes: THttpApiRequestAuthentications
      read fAuthenticationSchemes;
    /// read-only access to check if the HTTP API 2.0 logging is enabled
    // - use LogStart/LogStop methods to change this property value
    property Logging: boolean read GetLogging;
    /// the current HTTP API 2.0 logging Service name
    // - should be UTF-8 encoded, if LogStart(aFlags=[hlfUseUTF8Conversion])
    // - this value is dedicated to one instance, so the main instance won't
    // propagate the change to all cloned instances
    property LoggingServiceName: SockString
      read fLoggingServiceName write SetLoggingServiceName;
    /// read-only access to the low-level HTTP API 2.0 Session ID
    property ServerSessionID: HTTP_SERVER_SESSION_ID read fServerSessionID;
    /// read-only access to the low-level HTTP API 2.0 URI Group ID
    property UrlGroupID: HTTP_URL_GROUP_ID read fUrlGroupID;
    /// how many bytes are retrieved in a single call to ReceiveRequestEntityBody
    // - set by default to 1048576, i.e. 1 MB - practical limit is around 20 MB
    // - you may customize this value if you encounter HTTP error 406 from client,
    // corresponding to an ERROR_NO_SYSTEM_RESOURCES (1450) exception on server
    // side, when uploading huge data content
    property ReceiveBufferSize: cardinal read fReceiveBufferSize write SetReceiveBufferSize;
  published
    /// TRUE if this instance is in fact a cloned instance for the thread pool
    property Cloned: boolean read GetCloned;
    /// return the list of registered URL on this server instance
    property RegisteredUrl: SynUnicode read GetRegisteredUrl;
    /// HTTP.sys request/response queue length (via HTTP API 2.0)
    // - default value if 1000, which sounds fine for most use cases
    // - increase this value in case of many 503 HTTP answers or if many
    // "QueueFull" messages appear in HTTP.sys log files (normaly in
    // C:\Windows\System32\LogFiles\HTTPERR\httperr*.log) - may appear with
    // thousands of concurrent clients accessing at once the same server
  	// - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa364501
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    // - this method will also handle any cloned instances, so you can write e.g.
    // ! if aSQLHttpServer.HttpServer.InheritsFrom(THttpApiServer) then
    // !   THttpApiServer(aSQLHttpServer.HttpServer).HTTPQueueLength := 5000;
    property HTTPQueueLength: Cardinal read GetHTTPQueueLength write SetHTTPQueueLength;
    /// the maximum allowed bandwidth rate in bytes per second (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited bandwidth
    // - by default Windows not limit bandwidth (actually limited to 4 Gbit/sec).
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxBandwidth: Cardinal read GetMaxBandwidth write SetMaxBandwidth;
    /// the maximum number of HTTP connections allowed (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited number of connections
    // - by default Windows does not limit number of allowed connections
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxConnections: Cardinal read GetMaxConnections write SetMaxConnections;
  end;

  /// low-level API reference to a WebSocket session
  WEB_SOCKET_HANDLE = Pointer;
  /// WebSocket close status as defined by http://tools.ietf.org/html/rfc6455#section-7.4
  WEB_SOCKET_CLOSE_STATUS = Word;
  /// the bit values used to construct the WebSocket frame header for httpapi.dll
  // - not equals to WINHTTP_WEB_SOCKET_BUFFER_TYPE from winhttp.dll
  WEB_SOCKET_BUFFER_TYPE = ULONG;

  TSynThreadPoolHttpApiWebSocketServer = class;
  TSynWebSocketGuard = class;
  THttpApiWebSocketServer = class;
  THttpApiWebSocketServerProtocol = class;

  /// current state of a THttpApiWebSocketConnection
  TWebSocketState = (wsConnecting, wsOpen,
    wsClosing, wsClosedByClient, wsClosedByServer, wsClosedByGuard, wsClosedByShutdown);

  /// structure representing a single WebSocket connection
  {$ifdef UNICODE}
  THttpApiWebSocketConnection = record
  {$else}
  THttpApiWebSocketConnection = object
  {$endif}
  private
    fOverlapped: TOverlapped;
    fState: TWebSocketState;
    fProtocol: THttpApiWebSocketServerProtocol;
    fOpaqueHTTPRequestId: HTTP_REQUEST_ID;
    fWSHandle: WEB_SOCKET_HANDLE;
    fLastActionContext: Pointer;
    fLastReceiveTickCount: Cardinal;
    fPrivateData: pointer;
    fBuffer: SockString;
    fCloseStatus: WEB_SOCKET_CLOSE_STATUS;
    fIndex: integer;
    function ProcessActions(ActionQueue: Cardinal): boolean;
    procedure ReadData(const WebsocketBufferData);
    procedure WriteData(const WebsocketBufferData);
    procedure BeforeRead;
    procedure DoOnMessage(aBufferType: WEB_SOCKET_BUFFER_TYPE;
      aBuffer: Pointer; aBufferSize: ULONG);
    procedure DoOnConnect;
    procedure DoOnDisconnect();
    procedure InternalSend(aBufferType: WEB_SOCKET_BUFFER_TYPE; WebsocketBufferData: pointer);
    procedure Ping;
    procedure Disconnect;
    procedure CheckIsActive;
    // call onAccept Method of protocol, and if protocol not accept connection or 
    // can not be accepted from other reasons return false else return true
    function TryAcceptConnection(aProtocol: THttpApiWebSocketServerProtocol; Ctxt: THttpServerRequest; aNeedHeader: boolean): boolean;
  public
    /// Index of connection in protocol's connection list
    property Index: integer read fIndex;
    /// Protocol of connection
    property Protocol: THttpApiWebSocketServerProtocol read fProtocol;
    /// Custom user data
    property PrivateData: pointer read fPrivateData write fPrivateData;
    /// Access to the current state of this connection
    property State: TWebSocketState read fState;
    /// Send data to client
    procedure Send(aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);
    /// Close connection
    procedure Close(aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG);
  end;

  PHttpApiWebSocketConnection = ^THttpApiWebSocketConnection;

  THttpApiWebSocketConnectionVector = array[0..MaxInt div SizeOf(PHttpApiWebSocketConnection) - 1] of PHttpApiWebSocketConnection;

  PHttpApiWebSocketConnectionVector = ^THttpApiWebSocketConnectionVector;

  /// Event handlers for WebSocket
  THttpApiWebSocketServerOnAcceptEvent = function(Ctxt: THttpServerRequest;
    var Conn: THttpApiWebSocketConnection): Boolean of object;
  THttpApiWebSocketServerOnMessageEvent = procedure(const Conn: THttpApiWebSocketConnection;
    aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG) of object;
  THttpApiWebSocketServerOnConnectEvent = procedure(const Conn: THttpApiWebSocketConnection) of object;
  THttpApiWebSocketServerOnDisconnectEvent = procedure(const Conn: THttpApiWebSocketConnection;
    aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG) of object;

  /// Protocol Handler of websocket endpoints events
  // - maintains a list of all WebSockets clients for a given protocol
  THttpApiWebSocketServerProtocol = class
  private
    fName: SockString;
    fManualFragmentManagement: Boolean;
    fOnAccept: THttpApiWebSocketServerOnAcceptEvent;
    fOnMessage: THttpApiWebSocketServerOnMessageEvent;
    fOnFragment: THttpApiWebSocketServerOnMessageEvent;
    fOnConnect: THttpApiWebSocketServerOnConnectEvent;
    fOnDisconnect: THttpApiWebSocketServerOnDisconnectEvent;
    fConnections: PHttpApiWebSocketConnectionVector;
    fConnectionsCapacity: Integer;
    //Count of used connections. Some of them can be nil(if not used more)
    fConnectionsCount: Integer;
    fFirstEmptyConnectionIndex: Integer;
    fServer: THttpApiWebSocketServer;
    fSafe: TRTLCriticalSection;
    fPendingForClose: TList;
    fIndex: integer;
    function AddConnection(aConn: PHttpApiWebSocketConnection): Integer;
    procedure RemoveConnection(index: integer);
    procedure doShutdown;
  public
    /// initialize the WebSockets process
    // - if aManualFragmentManagement is true, onMessage will appear only for whole
    // received messages, otherwise OnFragment handler must be passed (for video
    // broadcast, for example)
    constructor Create(const aName: SockString; aManualFragmentManagement: Boolean;
      aServer: THttpApiWebSocketServer;
      aOnAccept: THttpApiWebSocketServerOnAcceptEvent;
      aOnMessage: THttpApiWebSocketServerOnMessageEvent;
      aOnConnect: THttpApiWebSocketServerOnConnectEvent;
      aOnDisconnect: THttpApiWebSocketServerOnDisconnectEvent;
      aOnFragment: THttpApiWebSocketServerOnMessageEvent=nil);
    /// finalize the process
    destructor Destroy; override;
    /// text identifier
    property Name: SockString read fName;
    /// identify the endpoint instance
    property Index: integer read fIndex;
    /// OnFragment event will be called for each fragment
    property ManualFragmentManagement: Boolean read fManualFragmentManagement;
    /// event triggerred when a WebSockets client is initiated
    property OnAccept: THttpApiWebSocketServerOnAcceptEvent read fOnAccept;
    /// event triggerred when a WebSockets message is received
    property OnMessage: THttpApiWebSocketServerOnMessageEvent read fOnMessage;
    /// event triggerred when a WebSockets client is connected
    property OnConnect: THttpApiWebSocketServerOnConnectEvent read fOnConnect;
    /// event triggerred when a WebSockets client is gracefully disconnected
    property OnDisconnect: THttpApiWebSocketServerOnDisconnectEvent read fOnDisconnect;
    /// event triggerred when a non complete frame is received
    // - required if ManualFragmentManagement is true
    property OnFragment: THttpApiWebSocketServerOnMessageEvent read fOnFragment;

    /// Send message to the WebSocket connection identified by its index
    function Send(index: Integer; aBufferType: ULONG; aBuffer: Pointer; aBufferSize: ULONG): boolean;
    /// Send message to all connections of this protocol
    function Broadcast(aBufferType: ULONG; aBuffer: Pointer; aBufferSize: ULONG): boolean;
    /// Close WebSocket connection identified by its index
    function Close(index: Integer; aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG): boolean;
  end;
  THttpApiWebSocketServerProtocolDynArray = array of THttpApiWebSocketServerProtocol;
  PHttpApiWebSocketServerProtocolDynArray = ^THttpApiWebSocketServerProtocolDynArray;

  /// HTTP & WebSocket server using fast http.sys kernel-mode server
  // - can be used like simple THttpApiServer
  // - when AddUrlWebSocket is called WebSocket support are added
  // in this case WebSocket will receiving the frames in asynchronous
  THttpApiWebSocketServer = class(THttpApiServer)
  private
    fThreadPoolServer: TSynThreadPoolHttpApiWebSocketServer;
    fGuard: TSynWebSocketGuard;
    fLastConnection: PHttpApiWebSocketConnection;
    fPingTimeout: integer;
    fRegisteredProtocols: PHttpApiWebSocketServerProtocolDynArray;
    fOnWSThreadStart: TNotifyThreadEvent;
    fOnWSThreadTerminate: TNotifyThreadEvent;
    fSendOverlaped: TOverlapped;
    fServiceOverlaped: TOverlapped;
    fOnServiceMessage: TThreadMethod;
    procedure SetOnWSThreadTerminate(const Value: TNotifyThreadEvent);
    function GetProtocol(index: integer): THttpApiWebSocketServerProtocol;
    function getProtocolsCount: Integer;
    procedure SetOnWSThreadStart(const Value: TNotifyThreadEvent);
  protected
    function UpgradeToWebSocket(Ctxt: THttpServerRequest): cardinal;
    procedure DoAfterSentResponse(Ctxt: THttpServerRequest); override;
    function GetSendResponseFlags(Ctxt: THttpServerRequest): Integer; override;
    constructor CreateClone(From: THttpApiServer); override;
    procedure DestroyMainThread; override;
  public
    /// initialize the HTTPAPI based Server with WebSocket support
    // - will raise an exception if http.sys or websocket.dll is not available
    // (e.g. before Windows 8) or if the request queue creation failed
    // - for aPingTimeout explanation see PingTimeout property documentation
    constructor Create(CreateSuspended: Boolean; aSocketThreadsCount: integer=1;
      aPingTimeout: integer=0; QueueName: SynUnicode='';
      aOnWSThreadStart: TNotifyThreadEvent=nil;
      aOnWSThreadTerminate: TNotifyThreadEvent=nil); reintroduce;
    /// prepare the process for a given THttpApiWebSocketServerProtocol
    procedure RegisterProtocol(const aName: SockString; aManualFragmentManagement: Boolean;
      aOnAccept: THttpApiWebSocketServerOnAcceptEvent;
      aOnMessage: THttpApiWebSocketServerOnMessageEvent;
      aOnConnect: THttpApiWebSocketServerOnConnectEvent;
      aOnDisconnect: THttpApiWebSocketServerOnDisconnectEvent;
      aOnFragment: THttpApiWebSocketServerOnMessageEvent=nil);
    /// register the URLs to Listen on using WebSocket
    // - aProtocols is an array of a recond with callbacks, server call during
    // WebSocket activity
    function AddUrlWebSocket(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'; aRegisterURI: boolean=false): integer;
    function Request(Ctxt: THttpServerRequest): cardinal; override;
    /// Ping timeout in seconds. 0 mean no ping.
    // - if connection not receive messages longer than this timeout
    // TSynWebSocketGuard will send ping frame
    // - if connection not receive any messages longer than double of
    // this timeout it will be closed
    property PingTimeout: integer read fPingTimeout;
    /// access to the associated endpoints
    property Protocols[index: integer]: THttpApiWebSocketServerProtocol read GetProtocol;
    /// access to the associated endpoints count
    property ProtocolsCount: Integer read getProtocolsCount;
    /// event called when the processing thread starts
    property OnWSThreadStart: TNotifyThreadEvent read FOnWSThreadStart
      write SetOnWSThreadStart;
    /// event called when the processing thread termintes
    property OnWSThreadTerminate: TNotifyThreadEvent read FOnWSThreadTerminate
      write SetOnWSThreadTerminate;
    /// can be called from any thread
    // - will send a "service" message to a WebSocketServer to wake up a WebSocket thread
    // - When a webSocket thread receives such a message it will call onServiceMessage in the thread context
    procedure SendServiceMessage;
    /// event called when a service message is raised
    property OnServiceMessage: TThreadMethod read fOnServiceMessage write fOnServiceMessage;
  end;

  /// a Thread Pool, used for fast handling WebSocket requests
  TSynThreadPoolHttpApiWebSocketServer = class(TSynThreadPool)
  protected
    fServer: THttpApiWebSocketServer;
    procedure onThreadStart(Sender: TThread);
    procedure onThreadTerminate(Sender: TThread);
    function NeedStopOnIOError: Boolean; override;
    procedure Task(aCaller: TSynThread; aContext: Pointer); override;
  public
    /// initialize the thread pool
    constructor Create(Server: THttpApiWebSocketServer; NumberOfThreads: Integer=1); reintroduce;
  end;

  /// Thread for closing WebSocket connections which not response more than PingTimeout interval
  TSynWebSocketGuard = class(TThread)
  protected
    fServer: THttpApiWebSocketServer;
    fSmallWait, fWaitCount: integer;
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(Server: THttpApiWebSocketServer); reintroduce;
  end;
  {$endif MSWINDOWS}

  /// an event handler for implementing a socked-based Thread Pool
  // - matches TSynThreadPool.Push method signature
  // - uses pointer instead of TSocket=THandle to be implementation neutral
  // - should return false if there is no thread available in the pool
  TOnThreadPoolSocketPush = function(aClientSock: pointer): boolean of object;

  /// main HTTP server Thread using the standard Sockets API (e.g. WinSock)
  // - bind to a port and listen to incoming requests
  // - assign this requests to THttpServerResp threads
  // - it implements a HTTP/1.1 compatible server, according to RFC 2068 specifications
  // - if the client is also HTTP/1.1 compatible, KeepAlive connection is handled:
  //  multiple requests will use the existing connection and thread;
  //  this is faster and uses less resources, especialy under Windows
  // - a Thread Pool is used internaly to speed up HTTP/1.0 connections - a
  // typical use, under Linux, is to run this class behind a NGINX frontend,
  // configured as https reverse proxy, leaving default "proxy_http_version 1.0"
  // and "proxy_request_buffering on" options for best performance
  // - it will trigger the Windows firewall popup UAC window at first run
  // - don't forget to use Free procedure when you are finished
  THttpServer = class(THttpServerGeneric)
  protected
    /// used to protect Process() call
    fProcessCS: TRTLCriticalSection;
    fThreadPoolPush: TOnThreadPoolSocketPush;
    fThreadPoolContentionCount: cardinal;
    fThreadPoolContentionAbortCount: cardinal;
    fThreadPool: TSynThreadPoolTHttpServer;
    fInternalHttpServerRespList: TList;
    fServerConnectionCount: cardinal;
    fServerKeepAliveTimeOut: cardinal;
    fTCPPrefix: SockString;
    fSock: TCrtSocket;
    fThreadRespClass: THttpServerRespClass;
    // this overridden version will return e.g. 'Winsock 2.514'
    function GetAPIVersion: string; override;
    /// server main loop - don't change directly
    procedure Execute; override;
    /// this method is called on every new client connection, i.e. every time
    // a THttpServerResp thread is created with a new incoming socket
    procedure OnConnect; virtual;
    /// this method is called on every client disconnection to update stats
    procedure OnDisconnect; virtual;
    /// override this function in order to low-level process the request;
    // default process is to get headers, and call public function Request
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: integer; ConnectionThread: TSynThread); virtual;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - you can specify a number of threads to be initialized to handle
    // incoming connections (default is 32, which may be sufficient for most
    // cases, maximum is 256) - if you set 0, the thread pool will be disabled
    // and one thread will be created for any incoming connection
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString; ServerThreadPoolCount: integer=32); reintroduce; virtual;
    /// release all memory and handlers
    destructor Destroy; override;
    /// access to the main server low-level Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - THttpServerSocket are created on the fly for every request, then
    // a THttpServerResp thread is created for handling this THttpServerSocket
    property Sock: TCrtSocket read fSock;
  published
    /// will contain the total number of connection to the server
    // - it's the global count since the server started
    property ServerConnectionCount: cardinal
      read fServerConnectionCount write fServerConnectionCount;
    /// time, in milliseconds, for the HTTP.1/1 connections to be kept alive;
    // default is 3000 ms
    // - see THttpApiServer.SetTimeOutLimits(aIdleConnection) parameter
    property ServerKeepAliveTimeOut: cardinal
      read fServerKeepAliveTimeOut write fServerKeepAliveTimeOut;
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    property TCPPrefix: SockString read fTCPPrefix write fTCPPrefix;
    /// the associated thread pool
    // - may be nil if ServerThreadPoolCount was 0 on constructor
    property ThreadPool: TSynThreadPoolTHttpServer read fThreadPool;
    /// number of times there was no availibility in the internal thread pool
    // to handle an incoming request
    // - this won't make any error, but just delay for 20 ms and try again
    property ThreadPoolContentionCount: cardinal read fThreadPoolContentionCount;
    /// number of times there an incoming request is rejected due to overload
    // - this is an error after 30 seconds of not any process availability
    property ThreadPoolContentionAbortCount: cardinal read fThreadPoolContentionAbortCount;
  end;
  {$M-}

  /// structure used to parse an URI into its components
  // - ready to be supplied e.g. to a THttpRequest sub-class
  // - used e.g. by class function THttpRequest.Get()
  TURI = {$ifdef UNICODE}record{$else}object{$endif}
  public
    /// if the server is accessible via https:// and not plain http://
    Https: boolean;
    /// if the server is accessible via something else than http:// or https://
    // - e.g. 'ws' or 'wss' for ws:// or wss://
    Scheme: SockString;
    /// the server name
    // - e.g. 'www.somewebsite.com'
    Server: SockString;
    /// the server port
    // - e.g. '80'
    Port: SockString;
    /// the resource address
    // - e.g. '/category/name/10?param=1'
    Address: SockString;
    /// fill the members from a supplied URI
    function From(aURI: SockString; const DefaultPort: SockString=''): boolean;
    /// compute the whole normalized URI
    function URI: SockString;
    /// the server port, as integer value
    function PortInt: integer;
    /// compute the root resource Address, without any URI-encoded parameter
    // - e.g. '/category/name/10'
    function Root: SockString;
    /// reset all stored information
    procedure Clear;
  end;

  /// the supported authentication schemes which may be used by HTTP clients
  // - supported only by TWinHTTP class yet
  THttpRequestAuthentication = (wraNone,wraBasic,wraDigest,wraNegotiate);

  /// a record to set some extended options for HTTP clients
  // - allow easy propagation e.g. from a TSQLHttpClient* wrapper class to
  // the actual SynCrtSock's THttpRequest implementation class
  THttpRequestExtendedOptions = record
    /// let HTTPS be less paranoid about SSL certificates
    // - IgnoreSSLCertificateErrors is handled by TWinHttp and TCurlHTTP
    IgnoreSSLCertificateErrors: boolean;
    /// allow HTTP authentication to take place at connection
    // - Auth.Scheme and UserName/Password properties are handled
    // by the TWinHttp class only by now
    Auth: record
      UserName: SynUnicode;
      Password: SynUnicode;
      Scheme: THttpRequestAuthentication;
    end;
  end;

  {$M+} // to have existing RTTI for published properties
  /// abstract class to handle HTTP/1.1 request
  // - never instantiate this class, but inherited TWinHTTP, TWinINet or TCurlHTTP
  THttpRequest = class
  protected
    fServer: SockString;
    fProxyName: SockString;
    fProxyByPass: SockString;
    fPort: cardinal;
    fHttps: boolean;
    fKeepAlive: cardinal;
    fUserAgent: SockString;
    fExtendedOptions: THttpRequestExtendedOptions;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressHeader: THttpSocketCompressSet;
    fTag: PtrInt;
    class function InternalREST(const url,method,data,header: SockString;
      aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString=nil): SockString;
    // inherited class should override those abstract methods
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); virtual; abstract;
    procedure InternalCreateRequest(const method, aURL: SockString); virtual; abstract;
    procedure InternalSendRequest(const aData: SockString); virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
      Data: SockString): integer; virtual; abstract;
    procedure InternalCloseRequest; virtual; abstract;
    procedure InternalAddHeader(const hdr: SockString); virtual; abstract;
  public
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy:
    // aProxyName/aProxyByPass will be recognized by TWinHTTP and TWinINet,
    // and aProxyName will set the CURLOPT_PROXY option to TCurlHttp
    // - you can customize the default client timeouts by setting appropriate
    // SendTimeout and ReceiveTimeout parameters (in ms) - note that after
    // creation of this instance, the connection is tied to the initial
    // parameters, so we won't publish any properties to change those
    // initial values once created - if you left the 0 default parameters, it
    // would use global HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT
    // and HTTP_DEFAULT_RECEIVETIMEOUT variable values
    // - *TimeOut parameters are currently ignored by TCurlHttp
    constructor Create(const aServer, aPort: SockString; aHttps: boolean;
      const aProxyName: SockString=''; const aProxyByPass: SockString='';
      ConnectionTimeOut: DWORD=0; SendTimeout: DWORD=0; ReceiveTimeout: DWORD=0); overload; virtual;
    /// connect to the supplied URI
    // - is just a wrapper around TURI and the overloaded Create() constructor
    constructor Create(const aURI: SockString;
      const aProxyName: SockString=''; const aProxyByPass: SockString='';
      ConnectionTimeOut: DWORD=0; SendTimeout: DWORD=0; ReceiveTimeout: DWORD=0;
      aIgnoreSSLCertificateErrors: boolean=false); overload;

    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwise
    // - KeepAlive is in milliseconds, 0 for "Connection: Close" HTTP/1.0 requests
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const InHeader, InData, InDataType: SockString;
      out OutHeader, OutData: SockString): integer; virtual;

    /// wrapper method to retrieve a resource via an HTTP GET
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Get() but either TWinHTTP.Get(), TWinINet.Get() or
    // TCurlHTTP.Get() methods
    class function Get(const aURI: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Post() but either TWinHTTP.Post(), TWinINet.Post() or
    // TCurlHTTP.Post() methods
    class function Post(const aURI, aData: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Put() but either TWinHTTP.Put(), TWinINet.Put() or
    // TCurlHTTP.Put() methods
    class function Put(const aURI, aData: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Delete() but either TWinHTTP.Delete(), TWinINet.Delete() or
    // TCurlHTTP.Delete() methods
    class function Delete(const aURI: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: boolean=true; outHeaders: PSockString=nil): SockString;

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024): boolean;

    /// allows to ignore untrusted SSL certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreSSLCertificateErrors: boolean
      read fExtendedOptions.IgnoreSSLCertificateErrors
      write fExtendedOptions.IgnoreSSLCertificateErrors;
    /// optional Authentication Scheme
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme write fExtendedOptions.Auth.Scheme;
    /// optional User Name for Authentication
    property AuthUserName: SynUnicode
      read fExtendedOptions.Auth.UserName write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SynUnicode
      read fExtendedOptions.Auth.Password write fExtendedOptions.Auth.Password;
    /// internal structure used to store extended options
    // - will be replicated by IgnoreSSLCertificateErrors and Auth* properties
    property ExtendedOptions: THttpRequestExtendedOptions
      read fExtendedOptions write fExtendedOptions;
    /// some internal field, which may be used by end-user code
    property Tag: PtrInt read fTag write fTag;
  published
    /// the remote server host name, as stated specified to the class constructor
    property Server: SockString read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: cardinal read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: SockString read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: SockString read fProxyByPass;
    /// the HTTP "User Agent:" header value
    // - you can customize this value from its default content
    // (only for TCurlHTTP class)
    property UserAgent: SockString read fUserAgent write fUserAgent;
  end;
  {$M-}

  /// store the actual class of a HTTP/1.1 client instance
  // - may be used to define at runtime which API to be used (e.g. WinHTTP,
  // WinINet or LibCurl), following the Liskov substitution principle
  THttpRequestClass = class of THttpRequest;

{$ifdef USEWININET}
  TWinHttpAPI = class;

  /// event callback to track download progress, e.g. in the UI
  // - used in TWinHttpAPI.OnProgress property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  TWinHttpProgress = procedure(Sender: TWinHttpAPI;
    CurrentSize, ContentLength: DWORD) of object;
  /// event callback to process the download by chunks, not in memory
  // - used in TWinHttpAPI.OnDownload property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  // - ChunkSize is the size of the latest downloaded chunk, available in
  // the untyped ChunkData memory buffer
  // - implementation should return TRUE to continue the download, or FALSE
  // to abort the download process
  TWinHttpDownload = function(Sender: TWinHttpAPI;
    CurrentSize, ContentLength, ChunkSize: DWORD; const ChunkData): boolean of object;

  /// a class to handle HTTP/1.1 request using either WinINet or WinHTTP API
  // - both APIs have a common logic, which is encapsulated by this parent class
  // - this abstract class defined some abstract methods which will be
  // implemented by TWinINet or TWinHttp with the proper API calls
  TWinHttpAPI = class(THttpRequest)
  protected
    fOnProgress: TWinHttpProgress;
    fOnDownload: TWinHttpDownload;
    fOnDownloadChunkSize: cardinal;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    function InternalGetInfo(Info: DWORD): SockString; virtual; abstract;
    function InternalGetInfo32(Info: DWORD): DWORD; virtual; abstract;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; virtual; abstract;
    function InternalRetrieveAnswer(
      var Header, Encoding, AcceptEncoding, Data: SockString): integer; override;
  public
    /// download would call this method to notify progress
    property OnProgress: TWinHttpProgress read fOnProgress write fOnProgress;
    /// download would call this method instead of filling Data: SockString value
    // - may be used e.g. when downloading huge content, and saving directly
    // the incoming data on disk or database
    // - if this property is set, raw TCP/IP incoming data would be supplied:
    // compression and encoding won't be handled by the class
    property OnDownload: TWinHttpDownload read fOnDownload write fOnDownload;
    /// how many bytes should be retrieved for each OnDownload event chunk
    // - if default 0 value is left, would use 65536, i.e. 64KB
    property OnDownloadChunkSize: cardinal
      read fOnDownloadChunkSize write fOnDownloadChunkSize;
  end;

  /// a class to handle HTTP/1.1 request using the WinINet API
  // - The Microsoft Windows Internet (WinINet) application programming interface
  // (API) enables applications to access standard Internet protocols, such as
  // FTP and HTTP/HTTPS, similar to what IE offers
  // - by design, the WinINet API should not be used from a service, since this
  // API may require end-user GUI interaction
  // - note: WinINet is MUCH slower than THttpClientSocket or TWinHttp: do not
  // use this, only if you find some configuration benefit on some old networks
  // (e.g. to diaplay the dialup popup window for a GUI client application)
  TWinINet = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EWinINet exception on error
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalGetInfo(Info: DWORD): SockString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinINet exception type
  EWinINet = class(ECrtSocket)
  public
    /// create a WinINet exception, with the error message as text
    constructor Create;
  end;

  /// a class to handle HTTP/1.1 request using the WinHTTP API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure https connection - so it seems to be the
  // class to use in your client programs
  // - WinHTTP does not share any proxy settings with Internet Explorer.
  // The WinHTTP proxy configuration is set by either
  // $ proxycfg.exe
  // on Windows XP and Windows Server 2003 or earlier, either
  // $ netsh.exe
  // on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run either:
  // $ proxycfg -u
  // $ netsh winhttp import proxy source=ie
  // to use the current user's proxy settings for Internet Explorer (under 64-bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - Microsoft Windows HTTP Services (WinHTTP) is targeted at middle-tier and
  // back-end server applications that require access to an HTTP client stack
  TWinHTTP = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EOSError exception on error
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalGetInfo(Info: DWORD): SockString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinHTTP exception type
  EWinHTTP = class(Exception);

  /// types of WebSocket buffers for winhttp.dll
  // it is the different thing than WEB_SOCKET_BUFFER_TYPE for httpapi.dll
  WINHTTP_WEB_SOCKET_BUFFER_TYPE = ULONG;

  /// A class to establish a client connection to a WebSocket server using Windows API
  // - used by TWinWebSocketClient class
  TWinHTTPUpgradeable = class(TWinHTTP)
  private
    fSocket: HINTERNET;
  protected
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
      Data: SockString): integer; override;
    procedure InternalSendRequest(const aData: SockString); override;
  public
    /// initialize the instance
    constructor Create(const aServer, aPort: SockString; aHttps: boolean;
      const aProxyName: SockString=''; const aProxyByPass: SockString='';
      ConnectionTimeOut: DWORD=0; SendTimeout: DWORD=0;
      ReceiveTimeout: DWORD=0); override;
  end;

  /// WebSocket client implementation
  TWinHTTPWebSocketClient = class
  protected
    fSocket: HINTERNET;
    function CheckSocket: Boolean;
  public
    /// initialize the instance
    // - all parameters do match TWinHTTP.Create except url: address of WebSocketServer
    // for sending upgrade request
    constructor Create(const aServer, aPort: SockString; aHttps: boolean;
      const url: SockString; const aSubProtocol: SockString = '';
      const aProxyName: SockString=''; const aProxyByPass: SockString='';
      ConnectionTimeOut: DWORD=0; SendTimeout: DWORD=0; ReceiveTimeout: DWORD=0);
    /// Send buffer
    function Send(aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; aBuffer: pointer;
      aBufferLength: DWORD): DWORD;
    /// Receive buffer
    function Receive(aBuffer: pointer; aBufferLength: DWORD;
      out aBytesRead: DWORD; out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): DWORD;
    /// Close current connection
    function CloseConnection(const aCloseReason: SockString): DWORD;
    destructor Destroy; override;
  end;

{$endif USEWININET}

{$ifdef USELIBCURL}
type
  /// a class to handle HTTP/1.1 request using the libcurl library
  // - libcurl is a free and easy-to-use cross-platform URL transfer library,
  // able to directly connect via HTTP or HTTPS on most Linux systems
  // - under a 32 bit Linux system, the libcurl library (and its dependencies,
  // like OpenSSL) may not be installed - you can add it via your package
  // manager, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3
  // - under a 64-bit Linux system, you should install the 32-bit flavor of
  // libcurl, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3:i386
  // - will use in fact libcurl.so, so either libcurl.so.3 or libcurl.so.4,
  // depending on the default version installation on the system
  TCurlHTTP = class(THttpRequest)
  protected
    fHandle: pointer;
    fRootURL: SockString;
    fIn: record
      Headers: pointer;
      Method: SockString;
      Data: SockString;
      DataOffset: integer;
    end;
    fOut: record
      Header, Encoding, AcceptEncoding, Data: SockString;
    end;
    fSSL: record
      CertFile: SockString;
      CACertFile: SockString;
      KeyName: SockString;
      PassPhrase: SockString;
    end;
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
      Data: SockString): integer; override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
  public
    /// release the connection
    destructor Destroy; override;
    /// set the client SSL certification details
    // - used e.g. as
    // ! UseClientCertificate('testcert.pem','cacert.pem','testkey.pem','pass');
    procedure UseClientCertificate(
      const aCertFile, aCACertFile, aKeyName, aPassPhrase: SockString);
  end;

{$endif USELIBCURL}

/// returns the best THttpRequest class, depending on the system it runs on
function MainHttpClass: THttpRequestClass;

/// create a TCrtSocket, returning nil on error
// (useful to easily catch socket error exception ECrtSocket)
function Open(const aServer, aPort: SockString; aTLS: boolean=false): TCrtSocket;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ECrtSocket
function OpenHttp(const aServer, aPort: SockString; aTLS: boolean=false): THttpClientSocket; overload;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ECrtSocket
function OpenHttp(const aURI: SockString; aAddress: PSockString=nil): THttpClientSocket; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket: if you want
// something able to use your computer proxy, take a look at TWinINet.Get()
function HttpGet(const server, port: SockString; const url: SockString;
  const inHeaders: SockString; outHeaders: PSockString=nil): SockString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHTTP/TCurlHTTP for any https URI
function HttpGet(const aURI: SockString; outHeaders: PSockString=nil): SockString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket for plain http URI,
// or TWinHTTP/TCurlHTTP for any https URI
function HttpGet(const aURI: SockString; const inHeaders: SockString;
  outHeaders: PSockString=nil): SockString; overload;

/// retrieve the content of a web page, using HTTP/1.1 GET method and a token
// - this method will use a low-level THttpClientSock socket and its GetAuth method
// - if AuthToken<>'', will add an header with 'Authorization: Bearer '+AuthToken
function HttpGetAuth(const aURI, aAuthToken: SockString; outHeaders: PSockString=nil): SockString;

/// send some data to a remote web server, using the HTTP/1.1 protocol and POST method
function HttpPost(const server, port: SockString; const url, Data, DataType: SockString;
  outData: PSockString=nil): boolean;

/// compute the 'Authorization: Bearer ####' HTTP header of a given token value
function AuthorizationBearer(const AuthToken: SockString): SockString;

const
  /// the layout of TSMTPConnection.FromText method
  SMTP_DEFAULT = 'user:password@smtpserver:port';

type
  /// may be used to store a connection to a SMTP server
  // - see SendEmail() overloaded function
  TSMTPConnection = {$ifdef UNICODE}record{$else}object{$endif}
  public
    /// the SMTP server IP or host name
    Host: SockString;
    /// the SMTP server port (25 by default)
    Port: SockString;
    /// the SMTP user login (if any)
    User: SockString;
    /// the SMTP user password (if any)
    Pass: SockString;
    /// fill the STMP server information from a single text field
    // - expects 'user:password@smtpserver:port' format
    // - if aText equals SMTP_DEFAULT ('user:password@smtpserver:port'),
    // does nothing
    function FromText(const aText: SockString): boolean;
  end;

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7 bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server, From, CSVDest, Subject, Text: SockString;
  const Headers: SockString=''; const User: SockString=''; const Pass: SockString='';
  const Port: SockString='25'; const TextCharSet: SockString = 'ISO-8859-1';
  aTLS: boolean=false): boolean; overload;

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7 bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server: TSMTPConnection;
  const From, CSVDest, Subject, Text: SockString; const Headers: SockString='';
  const TextCharSet: SockString = 'ISO-8859-1'; aTLS: boolean=false): boolean; overload;

/// convert a supplied subject text into an Unicode encoding
// - will convert the text into UTF-8 and append '=?UTF-8?B?'
// - for pre-Unicode versions of Delphi, Text is expected to be already UTF-8
// encoded - since Delphi 2010, it will be converted from UnicodeString
function SendEmailSubject(const Text: string): SockString;

const
  /// HTTP Status Code for "Success"
  STATUS_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  STATUS_CREATED = 201;
  /// HTTP Status Code for "No Content"
  STATUS_NOCONTENT = 204;
  /// HTTP Status Code for "Partial Content"
  STATUS_PARTIALCONTENT = 206;
  /// HTTP Status Code for "Not Modified"
  STATUS_NOTMODIFIED = 304;
  /// HTTP Status Code for "Bad Request"
  STATUS_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  STATUS_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  STATUS_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  STATUS_NOTFOUND = 404;
  /// HTTP Status Code for "Payload Too Large"
  STATUS_PAYLOADTOOLARGE = 413;
  /// HTTP Status Code for "Internal Server Error"
  STATUS_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  STATUS_NOTIMPLEMENTED = 501;

  {$ifdef MSWINDOWS}
  /// can be used with THttpApiServer.AuthenticationSchemes to enable all schemes
  HTTPAPI_AUTH_ENABLE_ALL = [hraBasic..hraKerberos];

  /// the buffer contains the last, and possibly only, part of a UTF8 message
  WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000000;
  /// the buffer contains part of a UTF8 message
  WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000001;
  /// the buffer contains the last, and possibly only, part of a binary message
  WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000002;
  /// the buffer contains part of a binary message
  WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000003;
  /// the buffer contains a close message
  WEB_SOCKET_CLOSE_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000004;
  /// the buffer contains a ping or pong message
  // - when sending, this value means 'ping'
  // - when processing received data, this value means 'pong'
  WEB_SOCKET_PING_PONG_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000005;
  /// the buffer contains an unsolicited pong message
  WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE: WEB_SOCKET_BUFFER_TYPE = $80000006;

  // https://msdn.microsoft.com/en-us/library/windows/desktop/hh449347
  WEB_SOCKET_MAX_CLOSE_REASON_LENGTH = 123;
  /// Close completed successfully
  WEB_SOCKET_SUCCESS_CLOSE_STATUS                : WEB_SOCKET_CLOSE_STATUS = 1000;
  /// The endpoint is going away and thus closing the connection
  WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS   : WEB_SOCKET_CLOSE_STATUS = 1001;
  /// Peer detected protocol error and it is closing the connection
  WEB_SOCKET_PROTOCOL_ERROR_CLOSE_STATUS         : WEB_SOCKET_CLOSE_STATUS = 1002;
  /// The endpoint cannot receive this type of data
  WEB_SOCKET_INVALID_DATA_TYPE_CLOSE_STATUS      : WEB_SOCKET_CLOSE_STATUS = 1003;
  /// No close status code was provided
  WEB_SOCKET_EMPTY_CLOSE_STATUS                  : WEB_SOCKET_CLOSE_STATUS = 1005;
  /// The connection was closed without sending or receiving a close frame
  WEB_SOCKET_ABORTED_CLOSE_STATUS                : WEB_SOCKET_CLOSE_STATUS = 1006;
  /// Data within a message is not consistent with the type of the message
  WEB_SOCKET_INVALID_PAYLOAD_CLOSE_STATUS        : WEB_SOCKET_CLOSE_STATUS = 1007;
  /// The message violates an endpoint's policy
  WEB_SOCKET_POLICY_VIOLATION_CLOSE_STATUS       : WEB_SOCKET_CLOSE_STATUS = 1008;
  /// The message sent was too large to process
  WEB_SOCKET_MESSAGE_TOO_BIG_CLOSE_STATUS        : WEB_SOCKET_CLOSE_STATUS = 1009;
  /// A client endpoint expected the server to negotiate one or more extensions,
  // but the server didn't return them in the response message of the WebSocket handshake
  WEB_SOCKET_UNSUPPORTED_EXTENSIONS_CLOSE_STATUS : WEB_SOCKET_CLOSE_STATUS = 1010;
  /// An unexpected condition prevented the server from fulfilling the request
  WEB_SOCKET_SERVER_ERROR_CLOSE_STATUS           : WEB_SOCKET_CLOSE_STATUS = 1011;
  /// The TLS handshake could not be completed
  WEB_SOCKET_SECURE_HANDSHAKE_ERROR_CLOSE_STATUS : WEB_SOCKET_CLOSE_STATUS = 1015;

  {$endif}

/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
// - see http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
// - mORMot.StatusCodeToErrorMsg() will call this function
function StatusCodeToReason(Code: cardinal): SockString;

/// retrieve the IP address from a computer name
function ResolveName(const Name: SockString;
  Family: Integer=AF_INET; SockProtocol: Integer=IPPROTO_TCP;
  SockType: integer=SOCK_STREAM): SockString;

/// Base64 encoding of a string
function Base64Encode(const s: SockString): SockString;

/// Base64 decoding of a string
function Base64Decode(const s: SockString): SockString;

/// escaping of HTML codes like < > & "
function HtmlEncode(const s: SockString): SockString;

{$ifdef MSWINDOWS}

/// remotly get the MAC address of a computer, from its IP Address
// - only works under Win2K and later
// - return the MAC address as a 12 hexa chars ('0050C204C80A' e.g.)
function GetRemoteMacAddress(const IP: SockString): SockString;

type
  TIPAddress = (tiaAny, tiaPublic, tiaPrivate);
  
/// enumerate all IP addresses of the current computer
// - may be used to enumerate all adapters
function GetIPAddresses(Kind: TIPAddress = tiaAny): TSockStringDynArray;

/// returns all IP addresses of the current computer as a single CSV text
// - may be used to enumerate all adapters
function GetIPAddressesText(const Sep: SockString = ' ';
  PublicOnly: boolean = false): SockString;

{$else}

/// returns how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit, false for the current process
// - returns -1 if the getrlimit() API call failed
function GetFileOpenLimit(hard: boolean=false): integer;

/// changes how many files could be opened at once on this POSIX system
// - hard=true is for the maximum allowed limit (requires root priviledges),
// false for the current process
// - returns the new value set (may not match the expected max value on error)
// - returns -1 if the getrlimit().setrlimit() API calls failed
// - for instance, to set the limit of the current process to its highest value:
// ! SetFileOpenLimit(GetFileOpenLimit(true));
function SetFileOpenLimit(max: integer; hard: boolean=false): integer;

{$endif MSWINDOWS}

/// low-level text description of  Socket error code
// - if Error is -1, will call WSAGetLastError to retrieve the last error code
function SocketErrorMessage(Error: integer=-1): string;

/// low-level direct creation of a TSocket handle for TCP, UDP or UNIX layers
// - doBind=true will call Bind() to create a server socket instance
// - doBind=false will call Connect() to create a client socket instance
function CallServer(const Server, Port: SockString; doBind: boolean;
  aLayer: TCrtSocketLayer; ConnectTimeout: DWORD): TSocket;

/// retrieve the text-converted remote IP address of a client socket
function GetRemoteIP(aClientSock: TSocket): SockString;

/// low-level direct shutdown of a given socket
procedure DirectShutdown(sock: TSocket);

/// low-level change of a socket to be in non-blocking mode
// - used e.g. by TPollAsynchSockets.Start
function AsynchSocket(sock: TSocket): boolean;

/// low-level direct write to the socket recv() function
// - by-pass overriden blocking recv() e.g. in SynFPCSock, so will work
// the socket is in non-blocking mode, as with AsynchSocket/TPollAsynchSockets
function AsynchRecv(sock: TSocket; buf: pointer; buflen: integer): integer;

/// low-level direct write to the socket send() function
// - by-pass overriden blocking send() e.g. in SynFPCSock, so will work if
// the socket is in non-blocking mode, as with AsynchSocket/TPollAsynchSockets
function AsynchSend(sock: TSocket; buf: pointer; buflen: integer): integer;


{ ************ socket polling optimized for multiple connections }

type
  /// the events monitored by TPollSocketAbstract classes
  // - we don't make any difference between urgent or normal read/write events
  TPollSocketEvent = (pseRead, pseWrite, pseError, pseClosed);
  /// set of events monitored by TPollSocketAbstract classes
  TPollSocketEvents = set of TPollSocketEvent;

  /// some opaque value (which may be a pointer) associated with a polling event
  TPollSocketTag = type PtrInt;

  /// modifications notified by TPollSocketAbstract.WaitForModified
  TPollSocketResult = record
    /// the events which are notified
    events: TPollSocketEvents;
    /// opaque value as defined by TPollSocketAbstract.Subscribe
    tag: TPollSocketTag;
  end;
  /// all modifications returned by TPollSocketAbstract.WaitForModified
  TPollSocketResults = array of TPollSocketResult;

  {$M+}
  /// abstract parent class for efficient socket polling
  // - works like Linux epoll API in level-triggered (LT) mode
  // - implements libevent-like cross-platform features
  // - use PollSockClass global function to retrieve the best class depending
  // on the running Operating System
  TPollSocketAbstract = class
  protected
    fCount: integer;
    fMaxSockets: integer;
  public
    /// class function factory, returning a socket polling instance matching
    // at best the current operating system
    // - returns a TPollSocketSelect/TPollSocketPoll instance under Windows,
    // a TPollSocketEpoll instance under Linux, or a TPollSocketPoll on BSD
    // - just a wrapper around PollSockClass.Create
    class function New: TPollSocketAbstract;
    /// initialize the polling
    constructor Create; virtual;
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    // - tag parameter will be returned as TPollSocketResult - you may set
    // here the socket file descriptor value, or a transtyped class instance
    // - similar to epoll's EPOLL_CTL_ADD control interface
    function Subscribe(socket: TSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; virtual; abstract;
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    // - on success, returns true and fill tag with the associated opaque value
    // - similar to epoll's EPOLL_CTL_DEL control interface
    function Unsubscribe(socket: TSocket): boolean; virtual; abstract;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns -1 on error (e.g. no TSocket currently registered), or
    // the number of modifications stored in results[] (may be 0 if none)
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; virtual; abstract;
  published
    /// how many TSocket instances could be tracked, at most
    // - depends on the API used
    property MaxSockets: integer read fMaxSockets;
    /// how many TSocket instances are currently tracked
    property Count: integer read fCount;
  end;
  {$M-}

  /// meta-class of TPollSocketAbstract socket polling classes
  // - since TPollSocketAbstract.Create is declared as virtual, could be used
  // to specify the proper polling class to add
  // - see PollSockClass function and TPollSocketAbstract.New method
  TPollSocketClass = class of TPollSocketAbstract;

/// returns the TPollSocketAbstract class best fitting with the current
// Operating System
// - as used by TPollSocketAbstract.New method
function PollSocketClass: TPollSocketClass;

type
  {$ifdef MSWINDOWS}
  /// socket polling via Windows' Select() API
  // - under Windows, Select() handles up to 64 TSocket, and is available
  // in Windows XP, whereas WSAPoll() is available only since Vista
  // - under Linux, select() is very limited, so poll/epoll APIs are to be used
  // - in practice, TPollSocketSelect is slighlty FASTER than TPollSocketPoll
  // when tracking a lot of connections (at least under Windows): WSAPoll()
  // seems to be just an emulation API - very disapointing :(
  TPollSocketSelect = class(TPollSocketAbstract)
  protected
    fHighestSocket: integer;
    fRead: TFDSet;
    fWrite: TFDSet;
    fTag: array[0..FD_SETSIZE-1] of record
      socket: TSocket;
      tag: TPollSocketTag;
    end;
  public
    /// initialize the polling via creating an epoll file descriptor
    constructor Create; override;
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    function Subscribe(socket: TSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    function Unsubscribe(socket: TSocket): boolean; override;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns -1 on error (e.g. no TSocket currently registered), or
    // the number of modifications stored in results[] (may be 0 if none)
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; override;
  end;
  {$endif MSWINDOWS}

  /// socket polling via poll/WSAPoll API
  // - direct call of the Linux/POSIX poll() API, or Windows WSAPoll() API
  TPollSocketPoll = class(TPollSocketAbstract)
  protected
    fFD: TPollFDDynArray; // fd=-1 for ignored fields
    fTags: array of TPollSocketTag;
    fFDCount: integer;
    procedure FDVacuum;
  public
    /// initialize the polling using poll/WSAPoll API
    constructor Create; override;
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    function Subscribe(socket: TSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    function Unsubscribe(socket: TSocket): boolean; override;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns -1 on error (e.g. no TSocket currently registered), or
    // the number of modifications stored in results[] (may be 0 if none)
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; override;
  end;

  {$ifdef LINUXNOTBSD}
  /// socket polling via Linux epoll optimized API
  // - not available under Windows or BSD/Darwin
  // - direct call of the epoll API in level-triggered (LT) mode
  // - only available on Linux - use TPollSocketPoll for using cross-plaform
  // poll/WSAPoll API
  TPollSocketEpoll = class(TPollSocketAbstract)
  protected
    fEPFD: integer;
    fResults: TEPollEventDynArray;
  public
    /// initialize the polling via creating an epoll file descriptor
    constructor Create; override;
    /// finalize the polling by closing the epoll file descriptor
    destructor Destroy; override;
    /// track status modifications on one specified TSocket
    // - you can specify which events are monitored - pseError and pseClosed
    // will always be notified
    // - directly calls epoll's EPOLL_CTL_ADD control interface
    function Subscribe(socket: TSocket; events: TPollSocketEvents;
      tag: TPollSocketTag): boolean; override;
    /// stop status modifications tracking on one specified TSocket
    // - the socket should have been monitored by a previous call to Subscribe()
    // - directly calls epoll's EPOLL_CTL_DEL control interface
    function Unsubscribe(socket: TSocket): boolean; override;
    /// waits for status modifications of all tracked TSocket
    // - will wait up to timeoutMS milliseconds, 0 meaning immediate return
    // and -1 for infinite blocking
    // - returns -1 on error (e.g. no TSocket currently registered), or
    // the number of modifications stored in results[] (may be 0 if none)
    // - directly calls epool_wait() function
    function WaitForModified(out results: TPollSocketResults;
      timeoutMS: integer): integer; override;
    /// read-only access to the low-level epoll_create file descriptor
    property EPFD: integer read fEPFD;
  end;
  {$endif LINUXNOTBSD}

type
  {$M+}
  /// implements efficient polling of multiple sockets
  // - will maintain a pool of TPollSocketAbstract instances, to monitor
  // incoming data or outgoing availability for a set of active connections
  // - call Subscribe/Unsubscribe to setup the monitored sockets
  // - call GetOne from any consumming threads to process new events
  TPollSockets = class
  protected
    fPollClass: TPollSocketClass;
    fPoll: array of TPollSocketAbstract;
    fPollIndex: integer;
    fPending: TPollSocketResults;
    fPendingIndex: integer;
    fTerminated: boolean;
    fCount: integer;
    fPollLock: TRTLCriticalSection;
    fPendingLock: TRTLCriticalSection;
  public
    /// initialize the sockets polling
    // - you can specify the TPollSocketAbsract class to be used, if the
    // default is not the one expected
    // - under Linux/POSIX, will set the open files maximum number for the
    // current process to match the system hard limit: if your system has a
    // low "ulimit -H -n" value, you may add the following line in your
    // /etc/limits.conf or /etc/security/limits.conf file:
    // $ * hard nofile 65535
    constructor Create(aPollClass: TPollSocketClass=nil);
    /// finalize the sockets polling, and release all used memory
    destructor Destroy; override;
    /// track modifications on one specified TSocket and tag
    // - the supplied tag value - maybe a PtrInt(aObject) - will be part of
    // GetOne method results
    // - will create as many TPollSocketAbstract instances as needed, depending
    // on the MaxSockets capability of the actual implementation class
    // - this method is thread-safe
    function Subscribe(socket: TSocket; tag: TPollSocketTag;
      events: TPollSocketEvents): boolean; virtual;
    /// stop status modifications tracking on one specified TSocket and tag
    // - the socket should have been monitored by a previous call to Subscribe()
    // - this method is thread-safe
    function Unsubscribe(socket: TSocket; tag: TPollSocketTag): boolean; virtual;
    /// retrieve the next pending notification, or let the poll wait for new
    // - if there is no pending notification, will poll and wait up to
    // timeoutMS milliseconds for pending data
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event was handled within the timeoutMS period
    // - this method is thread-safe, and could be called from several threads
    function GetOne(timeoutMS: integer; out notif: TPollSocketResult): boolean; virtual;
    /// retrieve the next pending notification
    // - returns true and set notif.events/tag with the corresponding notification
    // - returns false if no pending event is available
    // - this method is thread-safe, and could be called from several threads
    function GetOneWithinPending(out notif: TPollSocketResult): boolean;
    /// notify any GetOne waiting method to stop its polling loop
    procedure Terminate;
    /// the actual polling class used to track socket state changes 
    property PollClass: TPollSocketClass read fPollClass;
    /// set to true by the Terminate method
    property Terminated: boolean read fTerminated;
  published
    /// how many sockets are currently tracked
    property Count: integer read fCount;
  end;
  {$M-}

  /// store thread-safe information of one TPollAsynchSockets connection
  TPollSocketsSlot = {$ifdef UNICODE}record{$else}object{$endif}
    /// the associated TCP connection
    socket: TSocket;
    /// Lock/Unlock thread acquisition (lighter than a TRTLCriticalSection)
    lockcounter: integer;
    /// the current read data buffer of this slot
    readbuf: SockString;
    /// the current write data buffer of this slot
    writebuf: SockString;
    /// acquire an exclusive access to this connection
    // - returns true if slot has been acquired
    // - returns false if it is used by another thread
    // - warning: this method is not re-entrant
    function Lock: boolean;
    /// try to acquire an exclusive access to this connection
    // - returns true if slot has been acquired
    // - returns false if it is used by another thread, after the timeoutMS period
    // - warning: this method is not re-entrant
    function TryLock(timeoutMS: cardinal): boolean;
    /// release exclusive access to this connection
    procedure UnLock;
  end;
  /// points to thread-safe information of one TPollAsynchSockets connection
  PPollSocketsSlot = ^TPollSocketsSlot;

  /// possible options for TPollAsynchSockets process
  // - by default, TPollAsynchSockets.Write will first try to send the data
  // using Send() in non-blocking mode, unless paoWritePollOnly is defined,
  // and fWrite will be used to poll output state and send it asynchronously
  TPollAsynchSocketsOptions = set of (paoWritePollOnly);

  /// let TPollAsynchSockets.OnRead shutdown the socket if needed
  TPollAsynchSocketOnRead = (sorContinue, sorClose);

  {$M+}
  /// read/write buffer-oriented process of multiple non-blocking connections
  // - to be used e.g. for stream protocols (e.g. WebSockets or IoT communication)
  // - assigned sockets will be set in non-blocking mode, so that polling will
  // work as expected: you should then never use direclty the socket (e.g. via
  // blocking TCrtSocket), but rely on this class for asynchronous process:
  // OnRead() overriden method will receive all incoming data from input buffer,
  // and Write() should be called to add some data to asynchronous output buffer
  // - connections are identified as TObject instances, which should hold a
  // TPollSocketsSlot record as private values for the polling process
  // - ProcessRead/ProcessWrite methods are to be run for actual communication:
  // either you call those methods from multiple threads, or you run them in
  // loop from a single thread, then define a TSynThreadPool for running any
  // blocking process (e.g. computing requests answers) from OnRead callbacks
  // - inherited classes should override abstract OnRead, OnClose, OnError and
  // SlotFromConnection methods according to the actual connection class
  TPollAsynchSockets = class
  protected
    fRead: TPollSockets;
    fWrite: TPollSockets;
    fReadCount: integer;
    fWriteCount: integer;
    fReadBytes: Int64;
    fWriteBytes: Int64;
    fProcessing: integer;
    fOptions: TPollAsynchSocketsOptions;
    function GetCount: integer;
    // return low-level socket information from connection instance
    function SlotFromConnection(connection: TObject): PPollSocketsSlot; virtual; abstract;
    // extract frames from slot.readbuf, and handle them
    function OnRead(connection: TObject): TPollAsynchSocketOnRead; virtual; abstract;
    // called when slot.writebuf has been sent through the socket
    procedure AfterWrite(connection: TObject); virtual; abstract;
    // pseClosed: should do connection.free - Stop() has been called (socket=0)
    procedure OnClose(connection: TObject); virtual; abstract;
    // pseError: return false to close socket and connection (calling OnClose)
    function OnError(connection: TObject; events: TPollSocketEvents): boolean; virtual; abstract;
  public
    /// initialize the read/write sockets polling
    // - fRead and fWrite TPollSocketsBuffer instances will track pseRead or
    // pseWrite events, and maintain input and output data buffers
    constructor Create; virtual;
    /// finalize buffer-oriented sockets polling, and release all used memory
    destructor Destroy; override;
    /// assign a new connection to the internal poll
    // - the TSocket handle will be retrieved via SlotFromConnection, and
    // set in non-blocking mode from now on - it is not recommended to access
    // it directly any more, but use Write() and handle OnRead() callback
    // - fRead will poll incoming packets, then call OnRead to handle them,
    // or Unsubscribe and delete the socket when pseClosed is notified
    // - fWrite will poll for outgoing packets as specified by Write(), then
    // send any pending data once the socket is ready
    function Start(connection: TObject): boolean; virtual;
    /// remove a connection from the internal poll, and shutdown its socket
    // - most of the time, the connection is released by OnClose when the other
    // end shutdown the socket; but you can explicitely call this method when
    // the connection (and its socket) is to be shutdown
    // - this method won't call OnClose, since it is initiated by the class
    function Stop(connection: TObject): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    function Write(connection: TObject; const data; datalen: integer): boolean; virtual;
    /// add some data to the asynchronous output buffer of a given connection
    function WriteString(connection: TObject; const data: SockString): boolean;
    /// one or several threads should execute this method
    // - thread-safe handle of any incoming packets
    // - if this method is called from a single thread, you should use
    // a TSynThreadPool for any blocking process of OnRead events
    // - otherwise, this method is thread-safe, and incoming packets may be
    // consummed from a set of threads, and call OnRead with newly received data
    procedure ProcessRead(timeoutMS: integer);
    /// one or several threads should execute this method
    // - thread-safe handle of any outgoing packets
    procedure ProcessWrite(timeoutMS: integer);
    /// notify internal socket polls to stop their polling loop ASAP
    procedure Terminate(waitforMS: integer);
    /// low-level access to the polling class used for incoming data
    property PollRead: TPollSockets read fRead;
    /// low-level access to the polling class used for outgoind data
    property PollWrite: TPollSockets write fWrite;
    /// some processing options
    property Options: TPollAsynchSocketsOptions read fOptions write fOptions;
  published
    /// how many connections are currently managed by this instance
    property Count: integer read GetCount;
    /// how many times data has been received by this instance
    property ReadCount: integer read fReadCount;
    /// how many times data has been sent by this instance
    property WriteCount: integer read fWriteCount;
    /// how many data bytes have been received by this instance
    property ReadBytes: Int64 read fReadBytes;
    /// how many data bytes have been sent by this instance
    property WriteBytes: Int64 read fWriteBytes;
  end;
  {$M-}

  function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;

{$ifdef MSWINDOWS}
/// is HTTP.SYS web socket API available on the target system Windows 8 and UP
function WinHTTP_WebSocketEnabled: boolean;
{$endif}

implementation

{$ifdef FPC}
uses
  dynlibs;
{$endif}

{ ************ some shared helper functions and classes }

var
  ReasonCache: array[1..5,0..8] of SockString; // avoid memory allocation

function StatusCodeToReasonInternal(Code: cardinal): SockString;
begin
  case Code of
    100: result := 'Continue';
    101: result := 'Switching Protocols';
    200: result := 'OK';
    201: result := 'Created';
    202: result := 'Accepted';
    203: result := 'Non-Authoritative Information';
    204: result := 'No Content';
    205: result := 'Reset Content';
    206: result := 'Partial Content';
    207: result := 'Multi-Status';
    300: result := 'Multiple Choices';
    301: result := 'Moved Permanently';
    302: result := 'Found';
    303: result := 'See Other';
    304: result := 'Not Modified';
    305: result := 'Use Proxy';
    307: result := 'Temporary Redirect';
    308: result := 'Permanent Redirect';
    400: result := 'Bad Request';
    401: result := 'Unauthorized';
    403: result := 'Forbidden';
    404: result := 'Not Found';
    405: result := 'Method Not Allowed';
    406: result := 'Not Acceptable';
    407: result := 'Proxy Authentication Required';
    408: result := 'Request Timeout';
    409: result := 'Conflict';
    410: result := 'Gone';
    411: result := 'Length Required';
    412: result := 'Precondition Failed';
    413: result := 'Payload Too Large';
    414: result := 'URI Too Long';
    415: result := 'Unsupported Media Type';
    416: result := 'Requested Range Not Satisfiable';
    426: result := 'Upgrade Required';
    500: result := 'Internal Server Error';
    501: result := 'Not Implemented';
    502: result := 'Bad Gateway';
    503: result := 'Service Unavailable';
    504: result := 'Gateway Timeout';
    505: result := 'HTTP Version Not Supported';
    511: result := 'Network Authentication Required';
    else result := 'Invalid Request';
  end;
end;

function StatusCodeToReason(Code: cardinal): SockString;
var Hi,Lo: cardinal;
begin
  if Code=200 then begin
    Hi := 2;
    Lo := 0;
  end else begin
    Hi := Code div 100;
    Lo := Code-Hi*100;
    if not ((Hi in [1..5]) and (Lo in [0..8])) then begin
      result := StatusCodeToReasonInternal(Code);
      exit;
    end;
  end;
  result := ReasonCache[Hi,Lo];
  if result<>'' then
    exit;
  result := StatusCodeToReasonInternal(Code);
  ReasonCache[Hi,Lo] := result;
end;

function Hex2Dec(c: AnsiChar): byte;
begin
  case c of
  'A'..'Z': result := Ord(c) - (Ord('A') - 10);
  'a'..'z': result := Ord(c) - (Ord('a') - 10);
  '0'..'9': result := Ord(c) - Ord('0');
  else result := 255;
  end;
end;

// Base64 string encoding
function Base64Encode(const s: SockString): SockString;
procedure Encode(rp, sp: PAnsiChar; len: integer);
const
  b64: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i: integer;
    c: cardinal;
begin
  for i := 1 to len div 3 do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
  case len mod 3 of
    1: begin
      c := ord(sp[0]) shl 16;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := b64[(c shr 6) and $3f];
      rp[3] := '=';
    end;
  end;
end;
var len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result, ((len + 2) div 3) * 4);
  Encode(pointer(result),pointer(s),len);
end;

function Base64Decode(const s: SockString): SockString;
var i, j, len: integer;
    sp, rp: PAnsiChar;
    c, ch: integer;
begin
  result:= '';
  len := length(s);
  if (len = 0) or (len mod 4 <> 0) then
    exit;
  len := len shr 2;
  SetLength(result, len * 3);
  sp := pointer(s);
  rp := pointer(result);
  for i := 1 to len do begin
    c := 0;
    j := 0;
    while true do begin
      ch := ord(sp[j]);
      case chr(ch) of
        'A'..'Z': c := c or (ch - ord('A'));
        'a'..'z': c := c or (ch - (ord('a')-26));
        '0'..'9': c := c or (ch - (ord('0')-52));
        '+': c := c or 62;
        '/': c := c or 63;
        else
        if j=3 then begin
          rp[0] := AnsiChar(c shr 16);
          rp[1] := AnsiChar(c shr 8);
          SetLength(result, len*3-1);
          exit;
        end else begin
          rp[0] := AnsiChar(c shr 10);
          SetLength(result, len*3-2);
          exit;
        end;
      end;
      if j=3 then break;
      inc(j);
      c := c shl 6;
    end;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    inc(rp,3);
    inc(sp,4);
  end;
end;

function HtmlEncode(const s: SockString): SockString;
var i: integer;
begin // not very fast, but working
  result := '';
  for i := 1 to length(s) do
    case s[i] of
      '<': result := result+'&lt;';
      '>': result := result+'&gt;';
      '&': result := result+'&amp;';
      '"': result := result+'&quot;';
      else result := result+s[i];
    end;
end;

function HtmlEncodeString(const s: string): string;
var i: integer;
begin // not very fast, but working
  result := '';
  for i := 1 to length(s) do
    case s[i] of
      '<': result := result+'&lt;';
      '>': result := result+'&gt;';
      '&': result := result+'&amp;';
      '"': result := result+'&quot;';
      else result := result+s[i];
    end;
end;

const
  CRLF: array[0..1] of AnsiChar = (#13,#10);

function StrLen(S: PAnsiChar): integer;
begin
  result := 0;
  if S<>nil then
  while true do
    if S[0]<>#0 then
    if S[1]<>#0 then
    if S[2]<>#0 then
    if S[3]<>#0 then begin
      inc(S,4);
      inc(result,4);
    end else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;

function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var c: AnsiChar;
begin
  result := false;
  if p=nil then
    exit;
  if (up<>nil) and (up^<>#0) then
    repeat
      c := p^;
      if up^<>c then
        if c in ['a'..'z'] then begin
          dec(c,32);
          if up^<>c then
            exit;
        end else exit;
      inc(up);
      inc(p);
    until up^=#0;
  result := true;
end;

function IdemPCharArray(p: PAnsiChar; const upArray: array of PAnsiChar): integer;
var w: word;
begin
  if p<>nil then begin
    w := ord(p[0])+ord(p[1])shl 8;
    if p[0] in ['a'..'z'] then
      dec(w,32);
    if p[1] in ['a'..'z'] then
      dec(w,32 shl 8);
    for result := 0 to high(upArray) do
      if (PWord(upArray[result])^=w) and IdemPChar(p+2,upArray[result]+2) then
        exit;
  end;
  result := -1;
end;

procedure GetNextItem(var P: PAnsiChar; Sep: AnsiChar; var result: SockString);
// return next CSV string in P, nil if no more
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function SameText(const a,b: SockString): boolean;
var n,i: integer;
    c,d: AnsiChar;
begin
  result := false;
  n := length(a);
  if length(b)<>n then
    exit;
  for i := 1 to n do begin
    c := a[i];
    if c in ['a'..'z'] then 
      dec(c,32);
    d := b[i];
    if d in ['a'..'z'] then
      dec(d,32);
    if c<>d then
      exit;
  end;
  result := true;
end;

function GetNextItemUInt64(var P: PAnsiChar): ULONGLONG;
var c: PtrUInt;
begin
  result := 0;
  if P<>nil then
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+ULONGLONG(c);
      inc(P);
    until false;
end; // P^ will point to the first non digit char

function GetNextLine(var P: PAnsiChar): SockString;
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S^>=' ' do
      inc(S);
    SetString(result,P,S-P);
    while (S^<>#0) and (S^<' ') do inc(S); // ignore e.g. #13 or #10
    if S^<>#0 then
      P := S else
      P := nil;
  end;
end;

function GetHeaderValue(var headers: SockString; const upname: SockString;
  deleteInHeaders: boolean): SockString;
var i,j,k: integer;
begin
  result := '';
  if (headers='') or (upname='') then
    exit;
  i := 1;
  repeat
    k := length(headers)+1;
    for j := i to k-1 do
      if headers[j]<' ' then begin
        k := j;
        break;
      end;
    if IdemPChar(@headers[i],pointer(upname)) then begin
      j := i;
      inc(i,length(upname));
      while headers[i]=' ' do inc(i);
      result := copy(headers,i,k-i);
      if deleteInHeaders then begin
        while true do
          if (headers[k]=#0) or (headers[k]>=' ') then
            break else
            inc(k);
        delete(headers,j,k-j);
      end;
      exit;
    end;
    i := k;
    while headers[i]<' ' do
      if headers[i]=#0 then
        exit else
        inc(i);
  until false;
end;

function PosChar(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  result := Str;
  while result^<>Chr do begin
    if result^=#0 then begin
      result := nil;
      exit;
    end;
    Inc(result);
  end;
end;

{$ifdef HASCODEPAGE}
// rewrite some functions to avoid unattempted ansi<->unicode conversion

function Trim(const S: SockString): SockString;
{$ifdef FPC_OR_PUREPASCAL}
var I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I<=L) and (S[i]<=' ') do Inc(I);
  if I>L then
    Result := '' else
  if (I=1) and (S[L]>' ') then
    Result := S else begin
    while S[L]<=' ' do Dec(L);
    Result := Copy(S, I, L-I+1);
  end;
end;
{$else}
asm  // fast implementation by John O'Harrow
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@LStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@LStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@LStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
{$ifdef UNICODE}
  jbe  @@TrimRight
  push 65535 // SockString code page for Delphi 2009 and up
  call  System.@LStrFromPCharLen // we need a call, not a direct jmp
  ret
{$else}
  ja   System.@LStrFromPCharLen
{$endif}
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;
{$endif}

function UpperCase(const S: SockString): SockString;
procedure Upper(Source, Dest: PAnsiChar; L: cardinal);
var Ch: AnsiChar; // this sub-call is shorter and faster than 1 plain proc
begin
  repeat
    Ch := Source^;
    if (Ch>='a') and (Ch<='z') then
      dec(Ch,32);
    Dest^ := Ch;
    dec(L);
    inc(Source);
    inc(Dest);
  until L=0;
end;
var L: cardinal;
begin
  result := '';
  L := Length(S);
  if L=0 then
    exit;
  SetLength(result, L);
  Upper(pointer(S),pointer(result),L);
end;

{$endif HASCODEPAGE}

function GetCardinal(P: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^=' ' then repeat inc(P) until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

function GetCardinal(P,PEnd: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  result := 0;
  if (P=nil) or (P>=PEnd) then
    exit;
  if P^=' ' then repeat
    inc(P);
    if P=PEnd then exit;
  until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    exit;
  result := c;
  inc(P);
  while P<PEnd do begin
    c := byte(P^)-48;
    if c>9 then
      break else
      result := result*10+c;
    inc(P);
  end;
end;

function PCharToHex32(p: PAnsiChar): cardinal;
var v0,v1: byte;
begin
  result := 0;
  if p<>nil then begin
    while p^=' ' do inc(p);
    repeat
      v0 := Hex2Dec(p[0]);
      if v0=255 then break; // not in '0'..'9','a'..'f'
      v1 := Hex2Dec(p[1]);
      inc(p);
      if v1=255 then begin
        result := (result shl 4)+v0; // only one char left
        break;
      end;
      v0 := v0 shl 4;
      result := result shl 8;
      inc(v0,v1);
      inc(p);
      inc(result,v0);
    until false;
  end;
end;

{$ifdef DELPHI5OROLDER}
function Utf8ToAnsi(const UTF8: SockString): SockString;
begin
  result := UTF8; // no conversion
end;
{$endif}

const
  ENGLISH_LANGID = $0409;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770
  ERROR_WINHTTP_CANNOT_CONNECT = 12029;
  ERROR_WINHTTP_TIMEOUT = 12002;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;


function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;
{$ifdef MSWINDOWS}
var tmpLen: DWORD;
    err: PChar;
{$endif}
begin
  if Code=NO_ERROR then begin
    result := '';
    exit;
  end;
  {$ifdef MSWINDOWS}
  tmpLen := FormatMessage(
    FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    pointer(GetModuleHandle(ModuleName)),Code,ENGLISH_LANGID,@err,0,nil);
  try
    while (tmpLen>0) and (ord(err[tmpLen-1]) in [0..32,ord('.')]) do
      dec(tmpLen);
    SetString(result,err,tmpLen);
  finally
    LocalFree(HLOCAL(err));
  end;
  {$endif}
  if result='' then begin
    result := SysErrorMessage(Code);
    if result='' then
      if Code=ERROR_WINHTTP_CANNOT_CONNECT then
        result := 'cannot connect' else
      if Code=ERROR_WINHTTP_TIMEOUT then
        result := 'timeout' else
      if Code=ERROR_WINHTTP_INVALID_SERVER_RESPONSE then
        result := 'invalid server response' else
        result := IntToHex(Code,8);
  end;
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var LastError: Integer;
    Error: Exception;
begin
  LastError := GetLastError;
  if LastError<>NO_ERROR then
    Error := ModuleException.CreateFmt('%s error %d (%s)',
      [ModuleName,LastError,SysErrorMessagePerModule(LastError,ModuleName)]) else
    Error := ModuleException.CreateFmt('Undefined %s error',[ModuleName]);
  raise Error;
end;

const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';

procedure BinToHexDisplay(Bin: PByte; BinBytes: integer; var result: shortstring);
var j: cardinal;
begin
  result[0] := AnsiChar(BinBytes*2);
  for j := BinBytes-1 downto 0 do begin
    result[j*2+1] := HexChars[Bin^ shr 4];
    result[j*2+2] := HexChars[Bin^ and $F];
    inc(Bin);
  end;
end;

procedure BinToHexDisplayW(Bin: PByte; BinBytes: integer; var result: SynUnicode);
var j: cardinal;
    P: PWideChar;
begin
  SetString(Result,nil,BinBytes*2);
  P := pointer(Result);
  for j := BinBytes-1 downto 0 do begin
    P[j*2] := WideChar(HexChars[Bin^ shr 4]);
    P[j*2+1] := WideChar(HexChars[Bin^ and $F]);
    inc(Bin);
  end;
end;

function Ansi7ToUnicode(const Ansi: SockString): SockString;
var n, i: integer;
begin  // fast ANSI 7 bit conversion
  if Ansi='' then
    result := '' else begin
    n := length(Ansi);
    SetLength(result,n*2+1);
    for i := 0 to n do // to n = including last #0
      PWordArray(pointer(result))^[i] := PByteArray(pointer(Ansi))^[i];
  end;
end;

function DefaultUserAgent(Instance: TObject): SockString;
begin
  // note: some part of mORMot.pas would identify 'mORMot' pattern in the
  // agent header to enable advanced behavior e.g. about JSON transmission
  result := 'Mozilla/5.0 ('+XPOWEREDOS+'; '+XPOWEREDPROGRAM+' '+
    SockString(Instance.ClassName)+')';
end;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PAnsiChar): THttpSocketCompressSet;
var i: integer;
    aName: SockString;
    Beg: PAnsiChar;
begin
  integer(result) := 0;
  if P<>nil then
    repeat
      while P^ in [' ',','] do inc(P);
      Beg := P; // 'gzip;q=1.0, deflate' -> aName='gzip' then 'deflate'
      while not (P^ in [';',',',#0]) do inc(P);
      SetString(aName,Beg,P-Beg);
      for i := 0 to high(Compress) do
        if aName=Compress[i].Name then
          include(result,i);
      while not (P^ in [',',#0]) do inc(P);
    until P^=#0;
end;

function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: SockString;
  aCompressMinSize: integer): SockString;
var i, n: integer;
    dummy, aName: SockString;
begin
  result := '';
  if @aFunction=nil then
    exit;
  n := length(Compress);
  aName := aFunction(dummy,true);
  for i := 0 to n-1 do
    with Compress[i] do
      if Name=aName then begin // already set
        if @Func=@aFunction then // update min. compress size value
          CompressMinSize := aCompressMinSize;
        exit;
      end;
  if n=sizeof(integer)*8 then
    exit; // fCompressHeader is 0..31 (casted as integer)
  SetLength(Compress,n+1);
  with Compress[n] do begin
    Name := aName;
    @Func := @aFunction;
    CompressMinSize := aCompressMinSize;
  end;
  if aAcceptEncoding='' then
    aAcceptEncoding := 'Accept-Encoding: '+aName else
    aAcceptEncoding := aAcceptEncoding+','+aName;
  result := aName;
end;

function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
  const Handled: THttpSocketCompressRecDynArray; const OutContentType: SockString;
  var OutContent: SockString): SockString;
var i, OutContentLen: integer;
    compressible: boolean;
    OutContentTypeP: PAnsiChar absolute OutContentType;
begin
  if (integer(Accepted)<>0) and (OutContentType<>'') and (Handled<>nil) then begin
    OutContentLen := length(OutContent);
    case IdemPCharArray(OutContentTypeP,['TEXT/','IMAGE/','APPLICATION/']) of
    0: compressible := true;
    1: compressible := IdemPCharArray(OutContentTypeP+6,['SVG','X-ICO'])>=0;
    2: compressible := IdemPCharArray(OutContentTypeP+12,['JSON','XML','JAVASCRIPT'])>=0;
    else compressible := false;
    end;
    for i := 0 to high(Handled) do
    if i in Accepted then
    with Handled[i] do
    if (CompressMinSize=0) or // 0 here means "always" (e.g. for encryption)
       (compressible and (OutContentLen>=CompressMinSize)) then begin
      // compression of the OutContent + update header
      result := Func(OutContent,true);
      exit; // first in fCompress[] is prefered
    end;
  end;
  result := '';
end;

procedure IP4Text(const ip4addr; var result: SockString);
var b: array[0..3] of byte absolute ip4addr;
begin
  if cardinal(ip4addr)=0 then
    result := '' else
  if cardinal(ip4addr)=$0100007f then
    result := '127.0.0.1' else
    result := SockString(Format('%d.%d.%d.%d',[b[0],b[1],b[2],b[3]]))
end;

{$ifdef MSWINDOWS}

function SendARP(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer;
  PhyAddrLen: Pointer): DWORD; stdcall; external 'iphlpapi.dll';

function GetRemoteMacAddress(const IP: SockString): SockString;
// implements http://msdn.microsoft.com/en-us/library/aa366358
var dwRemoteIP: DWORD;
    PhyAddrLen: Longword;
    pMacAddr: array [0..7] of byte;
    I: integer;
    P: PAnsiChar;
begin
  result := '';
  dwremoteIP := inet_addr(pointer(IP));
  if dwremoteIP<>0 then begin
    PhyAddrLen := 8;
    if SendARP(dwremoteIP, 0, @pMacAddr, @PhyAddrLen)=NO_ERROR then begin
      if PhyAddrLen=6 then begin
        SetLength(result,12);
        P := pointer(result);
        for i := 0 to 5 do begin
          P[0] := HexChars[pMacAddr[i] shr 4];
          P[1] := HexChars[pMacAddr[i] and $F];
          inc(P,2);
        end;
      end;
    end;
  end;
end;

type
  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    ip: array[0..200] of record
      dwAddr: DWORD;
      dwIndex: DWORD;
      dwMask: DWORD;
      dwBCastAddr: DWORD;
      dwReasmSize: DWORD;
      unused1: Word;
      wType: Word;
    end;
  end;

function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE;
  var pdwSize: DWORD; bOrder: BOOL): DWORD; stdcall; external 'iphlpapi.dll';

function IsPublicIP(ip4: cardinal): boolean;
begin
  result := false;
  case ip4 and 255 of // ignore IANA private IP4 address spaces
    10: exit;
    172: if ((ip4 shr 8) and 255) in [16..31] then exit;
    192: if (ip4 shr 8) and 255=168 then exit;
  end;
  result := true;
end;

function GetIPAddresses(Kind: TIPAddress): TSockStringDynArray;
var Table: MIB_IPADDRTABLE;
    Size: DWORD;
    i: integer;
    n: cardinal;
begin
  result := nil;
  Size := SizeOf(Table);
  if GetIpAddrTable(@Table,Size,false)<>NO_ERROR then
    exit;
  SetLength(result,Table.dwNumEntries);
  n := 0;
  for i := 0 to Table.dwNumEntries-1 do
    with Table.ip[i] do
    if (dwAddr<>$0100007f) and (dwAddr<>0) then begin
      case Kind of
        tiaPublic: if not IsPublicIP(dwAddr) then continue;
        tiaPrivate: if IsPublicIP(dwAddr) then continue;
      end;
      IP4Text(dwAddr,result[n]);
      inc(n);
    end;
  if n<>Table.dwNumEntries then
    SetLength(result,n);
end;

var
  // should not change during process livetime
  IPAddressesText: array[boolean] of SockString;

function GetIPAddressesText(const Sep: SockString; PublicOnly: boolean): SockString;
var ip: TSockStringDynArray;
    i: integer;
begin
  if Sep=' ' then
    result := IPAddressesText[PublicOnly] else
    result := '';
  if result<>'' then
    exit;
  if PublicOnly then
    ip := GetIPAddresses(tiaPublic) else
    ip := GetIPAddresses(tiaAny);
  if ip=nil then
    exit;
  result := ip[0];
  for i := 1 to high(ip) do
    result := result+Sep+ip[i];
  if Sep=' ' then
    IPAddressesText[PublicOnly] := result;
end;

{$else}

function GetFileOpenLimit(hard: boolean=false): integer;
var limit: TRLIMIT;
begin
  {$ifdef FPC}
  if fpgetrlimit(RLIMIT_NOFILE,@limit)=0 then
  {$else}
  if getrlimit(RLIMIT_NOFILE,limit)=0 then
  {$endif}
    if hard then
      result := limit.rlim_max else
      result := limit.rlim_cur else
    result := -1;
end;

function SetFileOpenLimit(max: integer; hard: boolean=false): integer;
var limit: TRLIMIT;
begin
  result := -1;
  {$ifdef FPC}
  if fpgetrlimit(RLIMIT_NOFILE,@limit)<>0 then
  {$else}
  if getrlimit(RLIMIT_NOFILE,limit)<>0 then
  {$endif}
    exit;
  if (hard and (integer(limit.rlim_max)=max)) or
     (not hard and (integer(limit.rlim_cur)=max)) then begin
    result := max; // already to the expected value
    exit;
  end;
  if hard then
    limit.rlim_max := max else
    limit.rlim_cur := max;
  {$ifdef FPC}
  if fpsetrlimit(RLIMIT_NOFILE,@limit)=0 then
  {$else}
  if setrlimit(RLIMIT_NOFILE,limit)=0 then
  {$endif}
    result := GetFileOpenLimit(hard);
end;

{$endif MSWINDOWS}

{$ifndef NOXPOWEREDNAME}
const
  XPOWEREDNAME = 'X-Powered-By';
  XPOWEREDVALUE = XPOWEREDPROGRAM + ' synopse.info';
{$endif}

{ TURI }

const
  DEFAULT_PORT: array[boolean] of SockString = ('80','443');

procedure TURI.Clear;
begin
  Https := false;
  Finalize(self);
end;

function TURI.From(aURI: SockString; const DefaultPort: SockString): boolean;
var P,S: PAnsiChar;
begin
  Clear;
  result := false;
  aURI := Trim(aURI);
  if aURI='' then
    exit;
  P := pointer(aURI);
  S := P;
  while S^ in ['a'..'z','A'..'Z','+','-','.','0'..'9'] do inc(S);
  if PInteger(S)^ and $ffffff=ord(':')+ord('/')shl 8+ord('/')shl 16 then begin
    SetString(Scheme,P,S-P);
    if IdemPChar(P,'HTTPS') then
      Https := true;
    P := S+3;
  end;
  S := P;
  while not (S^ in [#0,':','/']) do inc(S);
  SetString(Server,P,S-P);
  if S^=':' then begin
    inc(S);
    P := S;
    while not (S^ in [#0,'/']) do inc(S);
    SetString(Port,P,S-P);
  end else
    if DefaultPort<>'' then
      Port := DefaultPort else
      Port := DEFAULT_PORT[Https];
  if S^<>#0 then // ':' or '/'
    inc(S);
  Address := S;
  if Server<>'' then
    result := true;
end;

function TURI.URI: SockString;
const Prefix: array[boolean] of SockString = ('http://','https://');
begin
  if (Port='') or (Port='0') or (Port=DEFAULT_PORT[Https]) then
    result := Prefix[Https]+Server+'/'+Address else
    result := Prefix[Https]+Server+':'+Port+'/'+Address;
end;

function TURI.PortInt: integer;
var err: integer;
begin
  Val(string(Port),result,err);
  if err<>0 then
    result := 0;
end;

function TURI.Root: SockString;
var i: integer;
begin
  i := Pos({$ifdef HASCODEPAGE}SockString{$endif}('?'),Address);
  if i=0 then
    Root := Address else
    Root := copy(Address,1,i-1);
end;


{ ************ Socket API access - TCrtSocket and THttp*Socket }

var
  WsaDataOnce: TWSADATA;
  SO_TRUE: integer = ord(true);

function ResolveName(const Name: SockString;
  Family, SockProtocol, SockType: integer): SockString;
var l: TStringList;
begin
  l := TStringList.Create;
  try
    ResolveNameToIP(Name, Family, SockProtocol, SockType, l);
    if l.Count=0 then
      result := Name else
      result := SockString(l[0]);
  finally
    l.Free;
  end;
end;

procedure SetInt32Option(Sock: TSocket; OptName, OptVal: integer);
var li: TLinger;
    {$ifndef MSWINDOWS}
    timeval: TTimeval;
    {$endif}
begin
  if Sock<=0 then
    raise ECrtSocket.CreateFmt('Unexpected SetOption(%d,%d)',[OptName,OptVal]);
  case OptName of
  SO_SNDTIMEO, SO_RCVTIMEO: begin
    {$ifndef MSWINDOWS} // POSIX expects a timeval parameter for time out values
    timeval.tv_sec := OptVal div 1000;
    timeval.tv_usec := (OptVal mod 1000)*1000;
    if SetSockOpt(Sock,SOL_SOCKET,OptName,@timeval,sizeof(timeval))=0 then
    {$else}             // WinAPI expects the time out directly as ms integer
    if SetSockOpt(Sock,SOL_SOCKET,OptName,pointer(@OptVal),sizeof(OptVal))=0 then
    {$endif}
      exit;
  end;
  SO_KEEPALIVE: // boolean (0/1) value
    if SetSockOpt(Sock,SOL_SOCKET,OptName,pointer(@OptVal),sizeof(OptVal))=0 then
      exit;
  SO_LINGER: begin // not available on UDP
    if OptVal<0 then
      li.l_onoff := Ord(false) else begin
      li.l_onoff := Ord(true);
      li.l_linger := OptVal;
    end;
    SetSockOpt(Sock,SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
    if OptVal>0 then begin
      {$ifdef LINUX}
      SetSockOpt(Sock,SOL_SOCKET, SO_REUSEADDR,@SO_TRUE,SizeOf(SO_TRUE));
      {$endif}
      {$ifdef BSD}
      SetSockOpt(Sock,SOL_SOCKET,SO_NOSIGPIPE,@SO_TRUE,SizeOf(SO_TRUE));
      {$endif}
    end;
    exit;
  end;
  TCP_NODELAY: // boolean (0/1) value
    if SetSockOpt(Sock,IPPROTO_TCP,OptName,@OptVal,sizeof(OptVal))=0 then
      exit;
  end;
  raise ECrtSocket.CreateFmt('SetOption(%d,%d)',[OptName,OptVal],-1);
end;

function CallServer(const Server, Port: SockString; doBind: boolean;
   aLayer: TCrtSocketLayer; ConnectTimeout: DWORD): TSocket;
var sin: TVarSin;
    IP: SockString;
    socktype, ipproto: integer;
    {$ifndef MSWINDOWS}
    serveraddr: sockaddr;
    {$endif}
begin
  result := -1;
  case aLayer of
    cslTCP: begin
      socktype := SOCK_STREAM;
      ipproto := IPPROTO_TCP;
    end;
    cslUDP: begin
      socktype := SOCK_DGRAM;
      ipproto := IPPROTO_UDP;
    end;
    cslUNIX: begin
      {$ifdef MSWINDOWS}
      exit; // not handled under Win32
      {$else} // special version for UNIX sockets
      socktype := SOCK_STREAM;
      ipproto := 0;
      result := socket(AF_UNIX,socktype,ipproto);
      if result<0 then
        exit;
      if doBind then begin
        fillchar(serveraddr,sizeof(serveraddr),0);
        //http://publib.boulder.ibm.com/infocenter/iseries/v5r3/index.jsp?topic=/rzab6/rzab6uafunix.htm
        {$ifdef KYLIX3}
        if (Libc.bind(result,serveraddr,sizeof(serveraddr))<0) or
           (Libc.listen(result,SOMAXCONN)<0) then begin
        {$else}
        if (fpbind(result,@serveraddr,sizeof(serveraddr))<0) or
           (fplisten(result,SOMAXCONN)<0) then begin
        {$endif}
          result := -1;
        end;
      end;
      exit;
      {$endif}
    end;
    else exit;
  end;

  if SameText(Server,'localhost')
    {$ifndef MSWINDOWS}or ((Server='') and not doBind){$endif} then
    IP := cLocalHost else
    IP := ResolveName(Server,AF_INET,ipproto,socktype);
  // use AF_INET instead of AF_UNSPEC: IP6 is buggy!
  if SetVarSin(sin,IP,Port,AF_INET,ipproto,socktype,false)<>0 then
    exit;
  result := Socket(integer(sin.AddressFamily),socktype,ipproto);
  if result=-1 then
    exit;
  if doBind then begin
    // Socket should remain open for 5 seconds after a closesocket() call
    SetInt32Option(result,SO_LINGER,5);
    // bind and listen to this port
    if (Bind(result,sin)<>0) or
       ((aLayer<>cslUDP) and (Listen(result,SOMAXCONN)<>0)) then begin
      CloseSocket(result);
      result := -1;
    end;
  end else begin
    if ConnectTimeout>0 then begin
      SetInt32Option(result,SO_RCVTIMEO,ConnectTimeout);
      SetInt32Option(result,SO_SNDTIMEO,ConnectTimeout);
    end;
    if Connect(result,sin)<>0 then begin
       CloseSocket(result);
       result := -1;
    end;
  end;
end;

type
  PCrtSocket = ^TCrtSocket;

function OutputSock(var F: TTextRec): integer;
begin
  if F.BufPos=0 then
    result := 0 else
    if PCrtSocket(@F.UserData)^.TrySndLow(F.BufPtr,F.BufPos) then begin
      F.BufPos := 0;
      result := 0;
    end else
      result := -1; // on socket error -> raise ioresult error
end;

function InputSock(var F: TTextRec): Integer;
// SockIn pseudo text file fill its internal buffer only with available data
// -> no unwanted wait time is added
// -> very optimized use for readln() in HTTP stream
var Size: integer;
    Sock: TCRTSocket;
    {$ifdef MSWINDOWS}
    iSize: integer;
    {$else}
    sin: TVarSin;
    {$endif}
begin
  F.BufEnd := 0;
  F.BufPos := 0;
  result := -1; // on socket error -> raise ioresult error
  Sock := PCrtSocket(@F.UserData)^;
  if (Sock=nil) or (Sock.Sock<=0) then
    exit; // file closed = no socket -> error
  if Sock.TimeOut<>0 then begin // will wait for pending data
    if IOCtlSocket(Sock.Sock, FIONREAD, Size)<>0 then // get exact count
      exit else
      if (Size<=0) or (Size>integer(F.BufSize)) then
        Size := F.BufSize;
  end else
    Size := F.BufSize;
  case Sock.SocketLayer of
  cslTCP, cslUNIX: begin
    if not Sock.TrySockRecv(F.BufPtr, Size, true) then
      Size := -1; // fatal socket error
  end else begin
    {$ifdef MSWINDOWS}
    iSize := SizeOf(TSockAddr);
    Size := RecvFrom(Sock.Sock, F.BufPtr, Size, 0, @Sock.fPeerAddr, @iSize);
    {$else}
    Size := RecvFrom(Sock.Sock, F.BufPtr, Size, 0, sin);
    Sock.fPeerAddr.sin_port := sin.sin_port;
    Sock.fPeerAddr.sin_addr := sin.sin_addr;
    {$endif}
  end;
  end;
  // Recv() may return Size=0 if no data is pending, but no TCP/IP error
  if Size>=0 then begin
    F.BufEnd := Size;
    inc(Sock.fBytesIn,Size);
    result := 0; // no error
  end else begin
    Sock.fSockInEof := true; // error -> mark end of SockIn
    result := -WSAGetLastError();
    // result <0 will update ioresult and raise an exception if {$I+}
  end;
end;

function CloseSock(var F: TTextRec): integer;
begin
  if PCrtSocket(@F.UserData)^<>nil then
    PCrtSocket(@F.UserData)^.Close;
  PCrtSocket(@F.UserData)^ := nil;
  Result := 0;
end;

function OpenSock(var F: TTextRec): integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  if F.Mode=fmInput then begin // ReadLn
    F.InOutFunc := @InputSock;
    F.FlushFunc := nil;
  end else begin               // WriteLn
    F.Mode := fmOutput;
    F.InOutFunc := @OutputSock;
    F.FlushFunc := @OutputSock;
  end;
  F.CloseFunc := @CloseSock;
  Result := 0;
end;


{ TCrtSocket }

function Split(const Text: SockString; Sep: AnsiChar; var Before,After: SockString): boolean;
var i: integer;
begin
  for i := length(Text)-1 downto 2 do
    if Text[i]=Sep then begin
      Before := trim(copy(Text,1,i-1));
      After := trim(copy(Text,i+1,maxInt));
      result := true;
      exit;
    end;
  result := false;
end;

constructor TCrtSocket.Bind(const aPort: SockString; aLayer: TCrtSocketLayer=cslTCP);
var s,p: SockString;
begin
  // on Linux, Accept() blocks even after Shutdown() -> use 0.5 second timeout
  Create({$ifdef LINUX}500{$else}5000{$endif});
  if not Split(aPort,':',s,p) then begin
    s := '0.0.0.0';
    p := aPort;
  end;
  OpenBind(s,p,true,-1,aLayer); // raise an ECrtSocket exception on error
end;

constructor TCrtSocket.Open(const aServer, aPort: SockString; aLayer: TCrtSocketLayer;
  aTimeOut: cardinal; aTLS: boolean);
begin
  Create(aTimeOut); // default read timeout is 10 seconds
  OpenBind(aServer,aPort,false,-1,aLayer,aTLS); // raise an ECrtSocket exception on error
end;

type
  PTextRec = ^TTextRec;

procedure TCrtSocket.Close;
begin
  fSndBufLen := 0; // always reset (e.g. in case of further Open)
  if (SockIn<>nil) or (SockOut<>nil) then begin
    ioresult; // reset ioresult value if SockIn/SockOut were used
    if SockIn<>nil then begin
      PTextRec(SockIn)^.BufPos := 0;  // reset input buffer
      PTextRec(SockIn)^.BufEnd := 0;
    end;
    if SockOut<>nil then begin
      PTextRec(SockOut)^.BufPos := 0; // reset output buffer
      PTextRec(SockOut)^.BufEnd := 0;
    end;
  end;
  if fSock<=0 then
    exit; // no opened connection, or Close already executed
  {$ifdef MSWINDOWS}
  if fSecure.Initialized then
    fSecure.BeforeDisconnection(fSock);
  {$endif MSWINDOWS}
  DirectShutdown(fSock);
  fSock := -1; // don't change Server or Port, since may try to reconnect
end;

constructor TCrtSocket.Create(aTimeOut: cardinal);
begin
  fTimeOut := aTimeOut;
end;

procedure TCrtSocket.SetInt32OptionByIndex(OptName, OptVal: integer);
begin
  SetInt32Option(Sock,OptName,OptVal);
end;

procedure TCrtSocket.AcceptRequest(aClientSock: TSocket; aRemoteIP: PSockString);
begin
  CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  OpenBind('','',false,aClientSock); // set the ACCEPTed aClientSock
  Linger := 5; // should remain open for 5 seconds after a closesocket() call
  if aRemoteIP<>nil then
    aRemoteIP^ := GetRemoteIP(aClientSock);
end;

procedure TCrtSocket.OpenBind(const aServer, aPort: SockString;
  doBind: boolean; aSock: integer; aLayer: TCrtSocketLayer; aTLS: boolean);
const BINDTXT: array[boolean] of string = ('open','bind');
begin
  fSocketLayer := aLayer;
  if aSock<0 then begin
    if aPort='' then
      fPort := DEFAULT_PORT[aTLS] else // default port is 80/443 (HTTP/S)
      fPort := aPort;
    fServer := aServer;
    fSock := CallServer(aServer,Port,doBind,aLayer,Timeout); // OPEN or BIND
    if fSock<0 then
      raise ECrtSocket.CreateFmt('OpenBind(%s:%s,%s): CallServer=%d',
        [aServer,Port,BINDTXT[doBind],fSock],-1);
  end else
    fSock := aSock; // ACCEPT mode -> socket is already created by caller
  if TimeOut>0 then begin // set timout values for both directions
    ReceiveTimeout := TimeOut;
    SendTimeout := TimeOut;
  end;
  if aLayer = cslTCP then begin
    TCPNoDelay := 1; // disable Nagle algorithm since we use our own buffers
    KeepAlive := 1; // enable TCP keepalive (even if we rely on transport layer)
    if aTLS and (aSock<0) and not doBind then
    try
      {$ifdef MSWINDOWS}
      fSecure.AfterConnection(fSock,pointer(aServer));
      {$else}
      raise ECrtSocket.Create('Unsupported');
      {$endif MSWINDOWS}
      fTLS := true;
    except
      on E: Exception do
        raise ECrtSocket.CreateFmt('OpenBind(%s:%s): aTLS failed [%s %s]',
          [aServer,Port,E.ClassName,E.Message],-1);
    end;
  end;
end;

procedure TCrtSocket.SockSend(const Values: array of const);
var i: integer;
    tmp: shortstring;
begin
  for i := 0 to high(Values) do
  with Values[i] do
  case VType of
    vtString:
      SockSend(@VString^[1],pByte(VString)^);
    vtAnsiString:
      SockSend(VAnsiString,length(SockString(VAnsiString)));
    {$ifdef HASVARUSTRING}
    vtUnicodeString: begin
      tmp := ShortString(UnicodeString(VUnicodeString)); // convert into ansi
      SockSend(@tmp[1],length(tmp));
    end;
    {$endif}
    vtPChar:
      SockSend(VPChar,StrLen(VPChar));
    vtChar:
      SockSend(@VChar,1);
    vtWideChar:
      SockSend(@VWideChar,1); // only ansi part of the character
    vtInteger: begin
      Str(VInteger,tmp);
      SockSend(@tmp[1],length(tmp));
    end;
    vtInt64{$ifdef FPC},vtQWord{$endif}: begin
      Str(VInt64^,tmp);
      SockSend(@tmp[1],length(tmp));
    end;
  end;
  SockSend(@CRLF,2);
end;

procedure TCrtSocket.SockSend(const Line: SockString);
begin
  if Line<>'' then
    SockSend(pointer(Line),length(Line));
  SockSend(@CRLF,2);
end;

procedure TCrtSocket.SockSendFlush;
begin
  if fSndBufLen=0 then
    exit;
  SndLow(pointer(fSndBuf),fSndBufLen);
  fSndBufLen := 0;
end;

procedure TCrtSocket.SndLow(P: pointer; Len: integer);
begin
  if not TrySndLow(P,Len) then
    raise ECrtSocket.Create('SndLow');
end;

function TCrtSocket.TrySndLow(P: pointer; Len: integer): boolean;
var sent, err: integer;
    starttix,endtix,tix: cardinal;
begin
  result := Len=0;
  if (self=nil) or (fSock<=0) or (Len<=0) or (P=nil) then
    exit;
  starttix := GetTickCount;
  endtix := starttix+TimeOut;
  repeat
    {$ifdef MSWINDOWS}
    if fSecure.Initialized then
      sent := fSecure.Send(fSock, P, Len) else
    {$endif MSWINDOWS}
      sent := AsynchSend(fSock, P, Len);
    if sent>0 then begin
      inc(fBytesOut,sent);
      dec(Len,sent);
      if Len<=0 then
        break;
      inc(PByte(P),sent);
    end else begin
      err := WSAGetLastError;
      if (err<>WSATRY_AGAIN) and (err<>WSAEINTR) then
        exit; // fatal socket error
    end;
    tix := GetTickCount;
    if (tix>endtix) or (tix<starttix) then
      exit; // identify read timeout as error
    sleep(1);
  until false;
  result := true;
end;

procedure TCrtSocket.Write(const Data: SockString);
begin
  SndLow(pointer(Data),length(Data));
end;

function TCrtSocket.AcceptIncoming(RemoteIP: PSockString;
  ResultClass: TCrtSocketClass): TCrtSocket;
var client: TSocket;
    sin: TVarSin;
begin
  result := nil;
  if (self=nil) or (fSock<=0) then
    exit;
  client := Accept(fSock,sin);
  if client<=0 then
    exit;
  if ResultClass=nil then
    ResultClass := TCrtSocket;
  result := ResultClass.Create;
  result.AcceptRequest(client,RemoteIP);
end;

function TCrtSocket.SockInRead(Content: PAnsiChar; Length: integer;
  UseOnlySockIn: boolean): integer;
var len,res: integer;
// read Length bytes from SockIn^ buffer + Sock if necessary
begin
  // get data from SockIn buffer, if any (faster than ReadChar)
  result := 0;
  if Length=0 then
    exit;
  if SockIn<>nil then
    with PTextRec(SockIn)^ do
    repeat
      len := BufEnd-BufPos;
      if len>0 then begin
        if len>Length then
          len := Length;
        move(BufPtr[BufPos],Content^,len);
        inc(BufPos,len);
        inc(Content,len);
        dec(Length,len);
        inc(result,len);
      end;
      if Length=0 then
        exit; // we got everything we wanted
      if not UseOnlySockIn then
        break;
      res := InputSock(PTextRec(SockIn)^);
      if res<0 then
        raise ECrtSocket.CreateFmt('SockInRead InputSock=%d',[res],-1);
    until Timeout=0;
  // direct receiving of the remaining bytes from socket
  if Length>0 then begin
    SockRecv(Content,Length);
    inc(result,Length);
  end;
end;

function TCrtSocket.SockInPending(aTimeOutMS: integer; aSocketForceCheck: boolean): integer;
var backup: cardinal;
    insocket: integer;
begin
  if SockIn=nil then
    raise ECrtSocket.Create('SockInPending without SockIn');
  if aTimeOutMS<0 then
    raise ECrtSocket.Create('SockInPending(aTimeOutMS<0)');
  with PTextRec(SockIn)^ do begin
    result := BufEnd-BufPos;
    if result=0 then
      // no data in SockIn^.Buffer, but some at socket level -> retrieve now
      case SockReceivePending(aTimeOutMS) of
      cspDataAvailable: begin
        backup := TimeOut;
        fTimeOut := 0; // not blocking call to fill full buffer
        try
          // call InputSock() to actually retrieve any pending data
          if InputSock(PTextRec(SockIn)^)=NO_ERROR then
            result := BufEnd-BufPos else
            result := -1; // indicates broken socket
        finally
          fTimeOut := backup;
        end;
      end;
      cspSocketError:
        result := -1; // indicates broken socket
      end; // cspNoData will leave result=0
  end;
  if aSocketForceCheck then
    if (IOCtlSocket(Sock, FIONREAD, insocket)=0) and (insocket>0) then
      inc(result,insocket);
end;

destructor TCrtSocket.Destroy;
begin
  Close;
  CloseSockIn;
  CloseSockOut;
  inherited;
end;

procedure TCrtSocket.SockSend(P: pointer; Len: integer);
var cap: integer;
begin
  if Len<=0 then
    exit;
  cap := Length(fSndBuf);
  if Len+fSndBufLen>cap then
    SetLength(fSndBuf,len+cap+cap shr 3+2048);
  move(P^,PAnsiChar(pointer(fSndBuf))[fSndBufLen],Len);
  inc(fSndBufLen,Len);
end;

const
  SOCKMINBUFSIZE = 1024; // big enough for headers (content will be read directly)

{$ifdef FPC}
procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  case Style Of
    tlbsCR:   TextRec(T).LineEnd := #13;
    tlbsLF:   TextRec(T).LineEnd := #10;
    tlbsCRLF: TextRec(T).LineEnd := #13#10;
  end;
end;
{$endif FPC}

procedure TCrtSocket.CreateSockIn(LineBreak: TTextLineBreakStyle;
  InputBufferSize: Integer);
begin
  if (Self=nil) or (SockIn<>nil) then
    exit; // initialization already occured
  if InputBufferSize<SOCKMINBUFSIZE then
    InputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockIn,sizeof(TTextRec)+InputBufferSize);
  fillchar(SockIn^,sizeof(TTextRec),0);
  with TTextRec(SockIn^) do begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    BufSize := InputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn)+sizeof(TTextRec)); // ignore Buffer[] (Delphi 2009+)
    OpenFunc := @OpenSock;
    Handle := -1;
  end;
  {$ifndef DELPHI5OROLDER}
  SetLineBreakStyle(SockIn^,LineBreak); // http does break lines with #13#10
  {$endif}
  Reset(SockIn^);
end;

procedure TCrtSocket.CreateSockOut(OutputBufferSize: Integer);
begin
  if SockOut<>nil then
    exit; // initialization already occured
  if OutputBufferSize<SOCKMINBUFSIZE then
    OutputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockOut,sizeof(TTextRec)+OutputBufferSize);
  fillchar(SockOut^,sizeof(TTextRec),0);
  with TTextRec(SockOut^) do begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    BufSize := OutputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn)+sizeof(TTextRec)); // ignore Buffer[] (Delphi 2009+)
    OpenFunc := @OpenSock;
    Handle := -1;
  end;
  {$ifndef DELPHI5OROLDER}
  SetLineBreakStyle(SockOut^,tlbsCRLF); // force e.g. for Linux platforms
  {$endif}
  Rewrite(SockOut^);
end;

procedure TCrtSocket.CloseSockIn;
begin
  if (self<>nil) and (fSockIn<>nil) then begin
    Freemem(fSockIn);
    fSockIn := nil;
  end;
end;

procedure TCrtSocket.CloseSockOut;
begin
  if (self<>nil) and (fSockOut<>nil) then begin
    Freemem(fSockOut);
    fSockOut := nil;
  end;
end;

procedure TCrtSocket.SockRecv(Buffer: pointer; Length: integer);
begin
  if not TrySockRecv(Buffer,Length) then
    raise ECrtSocket.CreateFmt('SockRecv(%d)',[Length]);
end;

function TCrtSocket.TrySockRecv(Buffer: pointer; var Length: integer;
  StopBeforeLength: boolean): boolean;
var expected,read,err: PtrInt;
    starttix,endtix,tix: cardinal;
begin
  result := false;
  if (self=nil) or (fSock<0) then
    exit;
  if (Buffer<>nil) and (Length>0) then begin
    expected := Length;
    Length := 0;
    starttix := GetTickCount;
    endtix := starttix+TimeOut;
    repeat
      read := expected-Length;
      {$ifdef MSWINDOWS}
      if fSecure.Initialized then
        read := fSecure.Receive(fSock,Buffer,read) else
      {$endif MSWINDOWS}
        read := AsynchRecv(fSock,Buffer,read);
      if read=0 then begin // socket closed gracefully
        if not StopBeforeLength then
          Close;
        exit;
      end else
      if read>0 then begin
        inc(fBytesIn,read);
        inc(Length,read);
        if StopBeforeLength or (Length=expected) then
          break; // good enough for now
        inc(PByte(Buffer),read);
      end else begin
        err := WSAGetLastError;
        if (err<>WSATRY_AGAIN) and (err<>WSAEINTR) then
          exit; // fatal socket error
      end;
      tix := GetTickCount;
      if (tix>endtix) or (tix<starttix) then
        exit; // identify read timeout as error
      sleep(1);
    until false;
  end;
  result := true;
end;

function TCrtSocket.SockReceivePending(TimeOutMS: cardinal): TCrtSocketPending;
var {$ifdef MSWINDOWS}
    tv: TTimeVal;
    fdset: TFDSet;
    {$else}
    p: TPollFD; // TFDSet limited to 1024 total sockets in POSIX -> use poll()
    {$endif}
    res: integer;
begin
  if (self=nil) or (fSock<=0) then begin
    result := cspSocketError;
    exit;
  end;
  {$ifdef MSWINDOWS}
  fdset.fd_array[0] := fSock;
  fdset.fd_count := 1;
  tv.tv_usec := TimeOutMS*1000;
  tv.tv_sec := 0;
  res := Select(fSock+1,@fdset,nil,nil,@tv);
  {$else}
  // https://moythreads.com/wordpress/2009/12/22/select-system-call-limitation
  p.fd := fSock;
  p.events := POLLIN;
  p.revents := 0;
  res := poll(@p,1,TimeOutMS);
  {$endif}
  if res=0 then
    result := cspNoData else
  if res>0 then
    result := cspDataAvailable else
    {$ifdef LINUX}
    if (WSAGetLastError=WSATRY_AGAIN) or (WSAGetLastError=WSAEWOULDBLOCK) then
      result := cspNoData else
    {$endif}
      result := cspSocketError;
end;

function TCrtSocket.LastLowSocketError: Integer;
begin
  result := WSAGetLastError; // retrieved directly from Sockets API
end;

procedure TCrtSocket.SockRecvLn(out Line: SockString; CROnly: boolean=false);
procedure RecvLn(var Line: SockString);
var P: PAnsiChar;
    LP, L: PtrInt;
    tmp: array[0..1023] of AnsiChar; // avoid ReallocMem() every char
begin
  P := @tmp;
  Line := '';
  repeat
    SockRecv(P,1); // this is very slow under Windows -> use SockIn^ instead
    if P^<>#13 then // at least NCSA 1.3 does send a #10 only -> ignore #13
      if P^=#10 then begin
        if Line='' then // get line
          SetString(Line,tmp,P-tmp) else begin
          LP := P-tmp; // append to already read chars
          L := length(Line);
          Setlength(Line,L+LP);
          move(tmp,(PAnsiChar(pointer(Line))+L)^,LP);
        end;
        exit;
      end else
      if P=@tmp[1023] then begin // tmp[] buffer full?
        L := length(Line); // -> append to already read chars
        Setlength(Line,L+1024);
        move(tmp,(PAnsiChar(pointer(Line))+L)^,1024);
        P := tmp;
      end else
        inc(P);
  until false;
end;
var c: AnsiChar;
   Error: integer;
begin
  if CROnly then begin // slow but accurate version expecting #13 as line end
    // SockIn^ expect either #10, either #13#10 -> a dedicated version is needed
    repeat
      SockRecv(@c,1); // this is slow but works
      if c in [#0,#13] then
        exit; // end of line
      Line := Line+c; // will do the work anyway
    until false;
  end else
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^,Line); // example: HTTP/1.0 200 OK
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    RecvLn(Line); // slow under Windows -> use SockIn^ instead
end;

procedure TCrtSocket.SockRecvLn;
var c: AnsiChar;
    Error: integer;
begin
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^);
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    repeat
      SockRecv(@c,1);
    until c=#10;
end;

function TCrtSocket.SockConnected: boolean;
var sin: TVarSin;
begin
  result := (self<>nil) and (fSock>0) and (GetPeerName(fSock,sin)=0);
end;

function TCrtSocket.PeerAddress: SockString;
begin
  IP4Text(fPeerAddr.sin_addr,result);
end;

function TCrtSocket.PeerPort: integer;
begin
  result := fPeerAddr.sin_port;
end;

{$ifdef MSWINDOWS}
procedure SleepHiRes(ms: cardinal);
begin
  {$ifndef FPC} // function SwitchToThread oddly not defined in fpc\rtl\win
  if (ms<>0) or not SwitchToThread then
  {$endif}
    Windows.Sleep(ms);
end;
{$endif}

function TCrtSocket.SockReceiveString: SockString;
var available, resultlen, read: integer;
begin
  result := '';
  if (self=nil) or (fSock<=0) then
    exit;
  resultlen := 0;
  repeat
    SleepHiRes(0);
    if IOCtlSocket(fSock, FIONREAD, available)<>0 then // get exact count
      exit; // raw socket error
    if available=0 then // no data in the allowed timeout
      if result='' then begin // wait till something
        SleepHiRes(10); // 10 ms delay in infinite loop
        continue;
      end else
        break; // return what we have
    SetLength(result,resultlen+available); // append to result
    read := available;
    if not TrySockRecv(@PByteArray(result)[resultlen],read,true) then begin
      Close;
      SetLength(result,resultlen);
      exit;
    end;
    inc(resultlen,read);
    if read<available then
      SetLength(result,resultlen); // e.g. Read=0 may happen
  until false;
end;


{ THttpClientSocket }

constructor THttpClientSocket.Create(aTimeOut: cardinal);
begin
  if aTimeOut=0 then
    aTimeOut := HTTP_DEFAULT_RECEIVETIMEOUT;
  inherited Create(aTimeOut);
  UserAgent := DefaultUserAgent(self);
end;

function THttpClientSocket.Delete(const url: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'DELETE',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Get(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
begin
  result := Request(url,'GET',KeepAlive,header,'','',false);
end;

function AuthorizationBearer(const AuthToken: SockString): SockString;
begin
  if AuthToken='' then
    result := '' else
    result := 'Authorization: Bearer '+AuthToken;
end;

function THttpClientSocket.GetAuth(const url, AuthToken: SockString; KeepAlive: cardinal=0): integer;
begin
  result := Get(url,KeepAlive,AuthorizationBearer(AuthToken));
end;

function THttpClientSocket.Head(const url: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'HEAD',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Post(const url, Data, DataType: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'POST',KeepAlive,header,Data,DataType,false);
end;

function THttpClientSocket.Put(const url, Data, DataType: SockString;
  KeepAlive: cardinal; const header: SockString): integer;
begin
  result := Request(url,'PUT',KeepAlive,header,Data,DataType,false);
end;

procedure THttpClientSocket.RequestSendHeader(const url, method: SockString);
var aURL: SockString;
begin
  if fSock<=0 then
    exit;
  if SockIn=nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  if TCPPrefix<>'' then
    SockSend(TCPPrefix);
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  SockSend([method,' ',aURL,' HTTP/1.1']);
  if Port=DEFAULT_PORT[fTLS] then
    SockSend(['Host: ',Server]) else
    SockSend(['Host: ',Server,':',Port]);
  SockSend(['Accept: */*'#13#10'User-Agent: ',UserAgent]);
end;

function THttpClientSocket.Request(const url, method: SockString;
  KeepAlive: cardinal; const Header, Data, DataType: SockString; retry: boolean): integer;
procedure DoRetry(Error: integer);
begin
  if retry then // retry once -> return error only if failed after retrial
    result := Error else begin
    Close; // close this connection
    try
      OpenBind(Server,Port,false); // then retry this request with a new socket
      result := Request(url,method,KeepAlive,Header,Data,DataType,true);
    except
      on Exception do
        result := Error;
    end;
  end;
end;
var P: PAnsiChar;
    aData: SockString;
begin
  if SockIn=nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  Content := '';
  if fSock<=0 then
    DoRetry(STATUS_NOTFOUND) else // socket closed (e.g. KeepAlive=0) -> reconnect
  try
  try
    // send request - we use SockSend because writeln() is calling flush()
    // -> all header will be sent at once
    RequestSendHeader(url,method);
    if KeepAlive>0 then
      SockSend(['Keep-Alive: ',KeepAlive,#13#10'Connection: Keep-Alive']) else
      SockSend('Connection: Close');
    aData := Data; // need var for Data to be eventually compressed
    CompressDataAndWriteHeaders(DataType,aData);
    if header<>'' then
      SockSend(header);
    if fCompressAcceptEncoding<>'' then
      SockSend(fCompressAcceptEncoding);
    SockSend; // send CRLF
    SockSendFlush; // flush all pending data (i.e. headers) to network
    if aData<>'' then // for POST and PUT methods: content to be sent
      SndLow(pointer(aData),length(aData)); // no CRLF at the end of data
    // get headers
    SockRecvLn(Command); // will raise ECrtSocket on any error
    if TCPPrefix<>'' then
      if Command<>TCPPrefix then begin
        result :=  505;
        exit;
      end else
      SockRecvLn(Command);
    P := pointer(Command);
    if IdemPChar(P,'HTTP/1.') then begin
      result := GetCardinal(P+9); // get http numeric status code
      if result=0 then begin
        result :=  505;
        exit;
      end;
      while result=100 do begin
        repeat // 100 CONTINUE will just be ignored client side
          SockRecvLn(Command);
          P := pointer(Command);
        until IdemPChar(P,'HTTP/1.');  // ignore up to next command
        result := GetCardinal(P+9);
      end;
      if P[7]='0' then
        KeepAlive := 0; // HTTP/1.0 -> force connection close
    end else begin // error on reading answer
      DoRetry(505); // 505=wrong format
      exit;
    end;
    GetHeader; // read all other headers
    if not IdemPChar(pointer(method),'HEAD') then
      GetBody; // get content if necessary (not HEAD method)
  except
    on Exception do
      DoRetry(STATUS_NOTFOUND);
  end;
  finally
    if KeepAlive=0 then
      Close;
  end;
end;

function Open(const aServer, aPort: SockString; aTLS: boolean): TCrtSocket;
begin
  try
    result := TCrtSocket.Open(aServer,aPort,cslTCP,10000,aTLS);
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function OpenHttp(const aServer, aPort: SockString; aTLS: boolean): THttpClientSocket;
begin
  try
    result := THttpClientSocket.Open(aServer,aPort,cslTCP,0,aTLS); // HTTP_DEFAULT_RECEIVETIMEOUT
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function OpenHttp(const aURI: SockString; aAddress: PSockString): THttpClientSocket;
var URI: TURI;
begin
  result := nil;
  if URI.From(aURI) then begin
    result := OpenHttp(URI.Server,URI.Port,URI.Https);
    if aAddress <> nil then
      aAddress^ := URI.Address;
  end;
end;

function HttpGet(const server, port: SockString; const url: SockString;
  const inHeaders: SockString; outHeaders: PSockString): SockString;
var Http: THttpClientSocket;
begin
  result := '';
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    if Http.Get(url,0,inHeaders) in [STATUS_SUCCESS..STATUS_PARTIALCONTENT] then begin
      result := Http.Content;
      if outHeaders<>nil then
        outHeaders^ := Http.HeaderGetText;
    end;
  finally
    Http.Free;
  end;
end;

function HttpGet(const aURI: SockString; outHeaders: PSockString): SockString;
begin
  result := HttpGet(aURI,'',outHeaders);
end;

function HttpGet(const aURI: SockString; const inHeaders: SockString; outHeaders: PSockString): SockString;
var URI: TURI;
begin
  if URI.From(aURI) then
    if URI.Https then
      {$ifdef USEWININET}
      result := TWinHTTP.Get(aURI,inHeaders,true,outHeaders) else
      {$else}
      {$ifdef USELIBCURL}
      result := TCurlHTTP.Get(aURI,inHeaders,true,outHeaders) else
      {$else}
      raise ECrtSocket.CreateFmt('https is not supported by HttpGet(%s)',[aURI]) else
      {$endif}
      {$endif USEWININET}
      result := HttpGet(URI.Server,URI.Port,URI.Address,inHeaders,outHeaders) else
    result := '';
  {$ifdef LINUX}
  if result='' then
    writeln('HttpGet returned VOID for ',URI.server,':',URI.Port,' ',URI.Address);
  {$endif}
end;

function HttpGetAuth(const aURI, aAuthToken: SockString; outHeaders: PSockString=nil): SockString;
var url: SockString;
    http: THttpClientSocket;
begin
  result := '';
  http := OpenHttp(aURI,@url);
  if http<>nil then
    try
      if http.GetAuth(url,aAuthToken) in [STATUS_SUCCESS..STATUS_PARTIALCONTENT] then
        result := http.Content;
    finally
      http.Free;
    end;
end;

function HttpPost(const server, port: SockString; const url, Data, DataType: SockString;
  outData: PSockString): boolean;
var Http: THttpClientSocket;
begin
  result := false;
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    result := Http.Post(url,Data,DataType) in
      [STATUS_SUCCESS,STATUS_CREATED,STATUS_NOCONTENT];
    if outdata<>nil then
      outdata^ := Http.Content;
  finally
    Http.Free;
  end;
end;

function TSMTPConnection.FromText(const aText: SockString): boolean;
var u,h: SockString;
begin
  if aText=SMTP_DEFAULT then begin
    result := false;
    exit;
  end;
  if Split(aText,'@',u,h) then begin
    if not Split(u,':',User,Pass) then
      User := u;
  end else
    h := aText;
  if not Split(h,':',Host,Port) then begin
    Host := h;
    Port := '25';
  end;
  if (Host<>'') and (Host[1]='?') then
    Host := '';
  result := Host<>'';
end;

function SendEmail(const Server: TSMTPConnection; const From, CSVDest, Subject,
  Text, Headers, TextCharSet: SockString; aTLS: boolean): boolean;
begin
  result := SendEmail(Server.Host, From, CSVDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet,
    (Server.Port = '465') or (Server.Port = '587'));
end;

function SendEmail(const Server, From, CSVDest, Subject, Text, Headers,
  User, Pass, Port, TextCharSet: SockString; aTLS: boolean): boolean;
var TCP: TCrtSocket;
procedure Expect(const Answer: SockString);
var Res: SockString;
begin
  repeat
    readln(TCP.SockIn^,Res);
  until (Length(Res)<4)or(Res[4]<>'-');
  if not IdemPChar(pointer(Res),pointer(Answer)) then
    raise ECrtSocket.Create(string(Res));
end;
procedure Exec(const Command, Answer: SockString);
begin
  writeln(TCP.SockOut^,Command);
  Expect(Answer)
end;
var P: PAnsiChar;
    rec, ToList, head: SockString;
begin
  result := false;
  P := pointer(CSVDest);
  if P=nil then exit;
  TCP := Open(Server,Port,aTLS);
  if TCP<>nil then
  try
    TCP.CreateSockIn; // we use SockIn and SockOut here
    TCP.CreateSockOut;
    Expect('220');
    if (User<>'') and (Pass<>'') then begin
      Exec('EHLO '+Server,'25');
      Exec('AUTH LOGIN','334');
      Exec(Base64Encode(User),'334');
      Exec(Base64Encode(Pass),'235');
    end else
      Exec('HELO '+Server,'25');
    writeln(TCP.SockOut^,'MAIL FROM:<',From,'>'); Expect('250');
    repeat
      GetNextItem(P,',',rec);
      rec := Trim(rec);
      if rec='' then continue;
      if pos({$ifdef HASCODEPAGE}SockString{$endif}('<'),rec)=0 then
        rec := '<'+rec+'>';
      Exec('RCPT TO:'+rec,'25');
      if ToList='' then
        ToList := #13#10'To: '+rec else
        ToList := ToList+', '+rec;
    until P=nil;
    Exec('DATA','354');
    head := trim(Headers);
    if head<>'' then
      head := head+#13#10;
    writeln(TCP.SockOut^,'Subject: ',Subject,#13#10'From: ',From,
      ToList,#13#10'Content-Type: text/plain; charset=',TextCharSet,
      #13#10'Content-Transfer-Encoding: 8bit'#13#10,head,#13#10,Text);
    Exec('.','25');
    writeln(TCP.SockOut^,'QUIT');
    result := true;
  finally
    TCP.Free;
  end;
end;

function IsAnsi7(const s: string): boolean;
var i: integer;
begin
  result := false;
  for i := 1 to length(s) do
    if ord(s[i])>126 then
      exit;
  result := true;
end;

function SendEmailSubject(const Text: string): SockString;
var utf8: UTF8String;
begin
  if IsAnsi7(Text) then
    result := SockString(Text) else begin
    utf8 := UTF8String(Text);
    result := '=?UTF-8?B?'+Base64Encode(utf8);
  end;
end;


{ THttpServerRequest }

constructor THttpServerRequest.Create(aServer: THttpServerGeneric;
  aConnectionID: Int64; aConnectionThread: TSynThread);
begin
  inherited Create;
  fServer := aServer;
  fConnectionID := aConnectionID;
  fConnectionThread := aConnectionThread;
end;

procedure THttpServerRequest.Prepare(const aURL, aMethod,
  aInHeaders, aInContent, aInContentType, aRemoteIP: SockString);
begin
  fURL := aURL;
  fMethod := aMethod;
  if aRemoteIP<>'' then
    if aInHeaders='' then
      fInHeaders := 'RemoteIP: '+aRemoteIP else
      fInHeaders := aInHeaders+#13#10'RemoteIP: '+aRemoteIP else
    fInHeaders := aInHeaders;
  fInContent := aInContent;
  fInContentType := aInContentType;
  fOutContent := '';
  fOutContentType := '';
  fOutCustomHeaders := '';
  fUseSSL := false;
end;

procedure THttpServerRequest.AddInHeader(additionalHeader: SockString);
begin
  additionalHeader := Trim(additionalHeader);
  if additionalHeader<>'' then
    if fInHeaders='' then
      fInHeaders := additionalHeader else
      fInHeaders := fInHeaders+#13#10+additionalHeader;
end;


{ TServerGeneric }

constructor TServerGeneric.Create(CreateSuspended: boolean;
  OnStart,OnStop: TNotifyThreadEvent; const ProcessName: SockString);
begin
  fProcessName := ProcessName;
  fOnHttpThreadStart := OnStart;
  SetOnTerminate(OnStop);
  inherited Create(CreateSuspended);
end;

procedure TServerGeneric.NotifyThreadStart(Sender: TSynThread);
begin
  if Sender=nil then
    raise ECrtSocket.Create('NotifyThreadStart(nil)');
  if Assigned(fOnHttpThreadStart) and not Assigned(Sender.fStartNotified) then begin
    fOnHttpThreadStart(Sender);
    Sender.fStartNotified := self;
  end;
end;

procedure TServerGeneric.SetOnTerminate(const Event: TNotifyThreadEvent);
begin
  fOnTerminate := Event;
end;


{ THttpServerGeneric }

constructor THttpServerGeneric.Create(CreateSuspended: boolean;
  OnStart,OnStop: TNotifyThreadEvent; const ProcessName: SockString);
begin
  SetServerName('mORMot ('+XPOWEREDOS+')');
  inherited Create(CreateSuspended,OnStart,OnStop,ProcessName);
end;

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer=1024);
begin
  RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize);
end;

procedure THttpServerGeneric.Shutdown;
begin
  if self<>nil then
    fShutdownInProgress := true;
end;

function THttpServerGeneric.Request(Ctxt: THttpServerRequest): cardinal;
begin
  if (self=nil) or fShutdownInProgress then
    result := STATUS_NOTFOUND else begin
    NotifyThreadStart(Ctxt.ConnectionThread);
    if Assigned(OnRequest) then
      result := OnRequest(Ctxt) else
      result := STATUS_NOTFOUND;
  end;
end;

function THttpServerGeneric.Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal;
begin
  raise ECrtSocket.CreateFmt('%s.Callback is not implemented: try to use '+
    'another communication protocol, e.g. WebSockets',[ClassName]);
end;

procedure THttpServerGeneric.SetServerName(const aName: SockString);
begin
  fServerName := aName;
end;

procedure THttpServerGeneric.SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody);
begin
  fOnBeforeBody := aEvent;
end;

procedure THttpServerGeneric.SetMaximumAllowedContentLength(aMax: cardinal);
begin
  fMaximumAllowedContentLength := aMax;
end;

procedure THttpServerGeneric.SetRemoteIPHeader(const aHeader: SockString);
begin
  fRemoteIPHeader := aHeader;
  fRemoteIPHeaderUpper := UpperCase(aHeader);
end;

function THttpServerGeneric.NextConnectionID: integer;
begin
  result := InterlockedIncrement(fCurrentConnectionID);
end;


{ THttpServer }

constructor THttpServer.Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
  const ProcessName: SockString; ServerThreadPoolCount: integer);
begin
  InitializeCriticalSection(fProcessCS);
  fSock := TCrtSocket.Bind(aPort); // BIND + LISTEN
  ServerKeepAliveTimeOut := 3000; // HTTP.1/1 KeepAlive is 3 seconds by default
  fInternalHttpServerRespList := TList.Create;
  // event handlers set before inherited Create to be visible in childs
  fOnHttpThreadStart := OnStart;
  SetOnTerminate(OnStop);
  if fThreadRespClass=nil then
    fThreadRespClass := THttpServerResp;
  if ServerThreadPoolCount>0 then begin
    fThreadPool := TSynThreadPoolTHttpServer.Create(self,ServerThreadPoolCount);
    fThreadPoolPush := fThreadPool.Push;
  end;
  inherited Create(false,OnStart,OnStop,ProcessName);
end;

function THttpServer.GetAPIVersion: string;
begin
  result := Format('%s.%d',[WsaDataOnce.szDescription,WsaDataOnce.wVersion]);
end;

destructor THttpServer.Destroy;
var starttix,endtix: Cardinal;
    i: integer;
begin
  Terminate; // set Terminated := true for THttpServerResp.Execute
  starttix := GetTickCount;
  endtix := starttix+20000;
  EnterCriticalSection(fProcessCS);
  if fInternalHttpServerRespList<>nil then begin
    for i := 0 to fInternalHttpServerRespList.Count-1 do
      THttpServerResp(fInternalHttpServerRespList.List[i]).Terminate;
    repeat // wait for all THttpServerResp.Execute to be finished
      if fInternalHttpServerRespList.Count=0 then
        break;
      LeaveCriticalSection(fProcessCS);
      SleepHiRes(100);
      EnterCriticalSection(fProcessCS);
    until (GetTickCount>endtix) or (GetTickCount<starttix);
    FreeAndNil(fInternalHttpServerRespList);
  end;
  LeaveCriticalSection(fProcessCS);
  fThreadPoolPush := nil;
  FreeAndNil(fThreadPool); // release all associated threads and I/O completion
  {$ifdef LINUX}
  {$ifdef FPC}
  KillThread(ThreadID);  // manualy do it here
  {$endif}
  {$endif}
  FreeAndNil(fSock);
  inherited Destroy;     // direct Thread abort, no wait till ended
  DeleteCriticalSection(fProcessCS);
end;

{.$define MONOTHREAD}
// define this not to create a thread at every connection (not recommended)

procedure THttpServer.Execute;
var ClientSock: TSocket;
    ClientSin: TVarSin;
    {$ifdef MONOTHREAD}
    ClientCrtSock: THttpServerSocket;
    {$endif}
    i: integer;
begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  NotifyThreadStart(self);
  // main server process loop
  if Sock.Sock>0 then
  try
    while not Terminated do begin
      ClientSock := Accept(Sock.Sock,ClientSin);
      if ClientSock<0 then
        if Terminated then
          break else begin
          SleepHiRes(0);
          continue;
        end;
      if Terminated or (Sock=nil) then begin
        DirectShutdown(ClientSock);
        break; // don't accept input if server is down
      end;
      OnConnect;
      {$ifdef MONOTHREAD}
      ClientCrtSock := THttpServerSocket.Create(self);
      try
        ClientCrtSock.InitRequest(ClientSock);
        if ClientCrtSock.GetRequest then
          Process(ClientCrtSock,self);
        OnDisconnect;
        DirectShutdown(ClientSock);
      finally
        ClientCrtSock.Free;
      end;
      {$else}
      if Assigned(fThreadPoolPush) then begin
        // use thread pool to process the request header, and probably its body
        if not fThreadPoolPush(pointer(ClientSock)) then begin
          // returned false if there is no idle thread in the pool
          for i := 1 to 1500 do begin
            inc(fThreadPoolContentionCount);
            SleepHiRes(20); // wait a little until a thread is available
            if Terminated then
              break;
            if fThreadPoolPush(pointer(ClientSock)) then
              exit; // the thread pool acquired the client sock
          end;
          inc(fThreadPoolContentionAbortCount);
          DirectShutdown(ClientSock); // 1500*20 = 30 seconds timeout
        end;
      end else
        // default implementation creates one thread for each incoming socket
        fThreadRespClass.Create(ClientSock, self);
      {$endif MONOTHREAD}
      end;
  except
    on Exception do
      ; // any exception would break and release the thread
  end;
end;

procedure THttpServer.OnConnect;
begin
  inc(fServerConnectionCount);
end;

procedure THttpServer.OnDisconnect;
begin
  // nothing to do by default
end;

procedure THttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: integer; ConnectionThread: TSynThread);
var Context: THttpServerRequest;
    P: PAnsiChar;
    Code: cardinal;
    s, reason: SockString;
    staticfn, ErrorMsg: string;
    FileToSend: TFileStream;
begin
  if (ClientSock=nil) or (ClientSock.Headers=nil) then
    // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  if Terminated then
    exit;
  Context := THttpServerRequest.Create(self,ConnectionID,ConnectionThread);
  try
    // calc answer
    with ClientSock do
      Context.Prepare(URL,Method,HeaderGetText,Content,ContentType,'');
    try
      Code := Request(Context);
    except
      on E: Exception do begin
        ErrorMsg := E.ClassName+': '+E.Message;
        Code := STATUS_SERVERERROR;
      end;
    end;
    if Terminated then
      exit;
    // handle case of direct sending of static file (as with http.sys)
    if (Context.OutContent<>'') and (Context.OutContentType=HTTP_RESP_STATICFILE) then
      try
        staticfn := {$ifdef UNICODE}UTF8ToUnicodeString{$else}Utf8ToAnsi{$endif}(Context.OutContent);
        FileToSend := TFileStream.Create(staticfn,fmOpenRead or fmShareDenyNone);
        try
          SetString(Context.fOutContent,nil,FileToSend.Size);
          FileToSend.Read(Pointer(Context.fOutContent)^,length(Context.fOutContent));
          Context.OutContentType := GetHeaderValue(Context.fOutCustomHeaders,'CONTENT-TYPE:',true);
       finally
          FileToSend.Free;
        end;
      except
        on E: Exception do begin // error reading or sending file
         ErrorMsg := E.ClassName+': '+E.Message;
         Code := STATUS_NOTFOUND;
        end;
      end;
    if Context.OutContentType=HTTP_RESP_NORESPONSE then
      Context.OutContentType := ''; // true HTTP always expects a response
    // send response (multi-thread OK) at once
    if (Code<STATUS_SUCCESS) or (ClientSock.Headers=nil) then
      Code := STATUS_NOTFOUND;
    reason := StatusCodeToReason(Code);
    if ErrorMsg<>'' then begin
      Context.OutCustomHeaders := '';
      Context.OutContentType := 'text/html; charset=utf-8'; // create message to display
      Context.OutContent := {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(
        format('<body style="font-family:verdana">'#13+
        '<h1>%s Server Error %d</h1><hr><p>HTTP %d %s<p>%s<p><small>%s',
        [ClassName,Code,Code,reason,HtmlEncodeString(ErrorMsg),fServerName]));
    end;
    // 1. send HTTP status command
    if ClientSock.TCPPrefix<>'' then
      ClientSock.SockSend(ClientSock.TCPPrefix);
    if ClientSock.KeepAliveClient then
      ClientSock.SockSend(['HTTP/1.1 ',Code,' ',reason]) else
      ClientSock.SockSend(['HTTP/1.0 ',Code,' ',reason]);
    // 2. send headers
    // 2.1. custom headers from Request() method
    P := pointer(Context.fOutCustomHeaders);
    while P<>nil do begin
      s := GetNextLine(P);
      if s<>'' then begin // no void line (means headers ending)
        ClientSock.SockSend(s);
        if IdemPChar(pointer(s),'CONTENT-ENCODING:') then
          integer(ClientSock.fCompressHeader) := 0; // custom encoding: don't compress
      end;
    end;
    // 2.2. generic headers
    ClientSock.SockSend([
      {$ifndef NOXPOWEREDNAME}XPOWEREDNAME+': '+XPOWEREDVALUE+#13#10+{$endif}
      'Server: ',fServerName]);
    ClientSock.CompressDataAndWriteHeaders(Context.OutContentType,Context.fOutContent);
    if ClientSock.KeepAliveClient then begin
      if ClientSock.fCompressAcceptEncoding<>'' then
        ClientSock.SockSend(ClientSock.fCompressAcceptEncoding);
      ClientSock.SockSend('Connection: Keep-Alive'#13#10); // #13#10 -> end headers
    end else
      ClientSock.SockSend; // headers must end with a void line
    ClientSock.SockSendFlush; // flush all pending data (i.e. headers) to network
    // 3. sent HTTP body content (if any)
    if Context.OutContent<>'' then
      // direct send to socket (no CRLF at the end of data)
      ClientSock.SndLow(pointer(Context.OutContent),length(Context.OutContent));
  finally
    if Sock<>nil then begin // add transfert stats to main socket
      EnterCriticalSection(fProcessCS);
      inc(Sock.fBytesIn,ClientSock.BytesIn);
      inc(Sock.fBytesOut,ClientSock.BytesOut);
      LeaveCriticalSection(fProcessCS);
      ClientSock.fBytesIn := 0;
      ClientSock.fBytesOut := 0;
    end;
    Context.Free;
  end;
end;


{ TSynThread }

constructor TSynThread.Create(CreateSuspended: boolean);
begin
  {$ifdef FPC}
  inherited Create(CreateSuspended,512*1024); // DefaultSizeStack=512KB
  {$else}
  inherited Create(CreateSuspended);
  {$endif}
end;

function TSynThread.SleepOrTerminated(MS: cardinal): boolean;
var starttix,endtix: cardinal;
begin
  result := true; // notify Terminated
  if Terminated then
    exit;
  if MS<32 then begin // smaller than GetTickCount resolution (under Windows)
    sleep(MS);
    if Terminated then
      exit;
  end else begin
    starttix := GetTickCount;
    endtix := starttix+MS;
    repeat
      sleep(10);
      if Terminated then
        exit;
    until (GetTickCount>endtix) or (GetTickCount<starttix);
  end;
  result := false; // normal delay expiration
end;

{$ifndef LVCL}
procedure TSynThread.DoTerminate;
begin
  if Assigned(fStartNotified) and Assigned(fOnTerminate) then begin
    fOnTerminate(self);
    fStartNotified := nil;
  end;
  inherited DoTerminate;
end;
{$endif}

{$ifndef HASTTHREADSTART}
procedure TSynThread.Start;
begin
  Resume;
end;
{$endif}


{ THttpServerResp }

constructor THttpServerResp.Create(aSock: TSocket; aServer: THttpServer);
begin
  fClientSock := aSock; // ensure it is set ASAP: on Linux, Execute raises immediately
  Create(THttpServerSocket.Create(aServer),aServer);
end;

constructor THttpServerResp.Create(aServerSock: THttpServerSocket; aServer: THttpServer);
begin
  fServer := aServer;
  fServerSock := aServerSock;
  fOnTerminate := fServer.fOnTerminate;
  EnterCriticalSection(fServer.fProcessCS);
  try
    fServer.fInternalHttpServerRespList.Add(self);
  finally
    LeaveCriticalSection(fServer.fProcessCS);
  end;
  fConnectionID := fServer.NextConnectionID;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure THttpServerResp.Execute;

  procedure HandleRequestsProcess;
  var starttix,endtix,tix: cardinal;
      pending: TCrtSocketPending;
  begin
    try
      repeat
        starttix := GetTickCount;
        endtix := starttix+fServer.ServerKeepAliveTimeOut;
        repeat // within this loop, break=wait for next command, exit=quit
          if (fServer=nil) or fServer.Terminated or (fServerSock=nil) then
            exit; // server is down -> close connection
          pending := fServerSock.SockReceivePending(50); // 50 ms timeout
          if (fServer=nil) or fServer.Terminated then
            exit; // server is down -> disconnect the client
          case pending of
          cspSocketError:
            exit; // socket error -> disconnect the client
          cspNoData: begin
            tix := GetTickCount;  // wait for keep alive timeout
            if tix<starttix then // time wrap after continuous run for 49.7 days
              break; // reset Ticks count + retry
            if tix>=endtix then
              exit; // reached time out -> close connection
          end;
          cspDataAvailable: begin
            // get request and headers
            if not fServerSock.GetRequest(True) then
              // fServerSock connection was down or headers are not correct
              exit;
            // calc answer and send response
            fServer.Process(fServerSock,ConnectionID,self);
            // keep connection only if necessary
            if fServerSock.KeepAliveClient then
              break else
              exit;
          end;
          end;
         until false;
      until false;
    except
      on E: Exception do
        ; // any exception will silently disconnect the client
    end;
  end;

var aSock: TSocket;
    i: integer;
begin
  fServer.NotifyThreadStart(self);
  try
    try
      if fClientSock<>0 then begin
        // direct call from incoming socket
        aSock := fClientSock;
        fClientSock := 0; // mark no need to Shutdown and close fClientSock
        fServerSock.InitRequest(aSock); // now fClientSock is in fServerSock
        if fServer<>nil then
          HandleRequestsProcess;
      end else begin
        // call from TSynThreadPoolTHttpServer -> handle first request
        if not fServerSock.fBodyRetrieved then
          fServerSock.GetBody;
        fServer.Process(fServerSock,ConnectionID,self);
        if (fServer<>nil) and fServerSock.KeepAliveClient then
          HandleRequestsProcess; // process further kept alive requests
      end;
    finally
      try
        if fServer<>nil then
        try
          fServer.OnDisconnect;
        finally
          EnterCriticalSection(fServer.fProcessCS);
          try
            if (fServer.fInternalHttpServerRespList<>nil) then begin
              i := fServer.fInternalHttpServerRespList.IndexOf(self);
              if i>=0 then
                fServer.fInternalHttpServerRespList.Delete(i);
            end;
          finally
            LeaveCriticalSection(fServer.fProcessCS);
            fServer := nil;
          end;
        end;
      finally
        FreeAndNil(fServerSock);
        // if Destroy happens before fServerSock.GetRequest() in Execute below
        DirectShutdown(fClientSock);
      end;
    end;
  except
    on Exception do
      ; // just ignore unexpected exceptions here, especially during clean-up
  end;
end;


{ THttpSocket }

procedure THttpSocket.GetBody;
var Line: SockString; // 32 bits chunk length in hexa
    LinePChar: array[0..31] of AnsiChar;
    Len, LContent, Error: integer;
begin
  fBodyRetrieved := true;
  Content := '';
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if Chunked then begin // we ignore the Length
    LContent := 0; // current read position in Content
    repeat
      if SockIn<>nil then begin
        readln(SockIn^,LinePChar);      // use of a static PChar is faster
        Error := ioresult;
        if Error<>0 then
          raise ECrtSocket.Create('GetBody1',Error);
        Len := PCharToHex32(LinePChar); // get chunk length in hexa
      end else begin
        SockRecvLn(Line);
        Len := PCharToHex32(pointer(Line)); // get chunk length in hexa
      end;
      if Len=0 then begin // ignore next line (normaly void)
        SockRecvLn;
        break;
      end;
      SetLength(Content,LContent+Len); // reserve memory space for this chunk
      SockInRead(pointer(PAnsiChar(pointer(Content))+LContent),Len) ; // append chunk data
      inc(LContent,Len);
      SockRecvLn; // ignore next #13#10
    until false;
  end else
  if ContentLength>0 then begin
    SetLength(Content,ContentLength); // not chuncked: direct read
    SockInRead(pointer(Content),ContentLength); // works with SockIn=nil or not
  end else
  if ContentLength<0 then begin // ContentLength=-1 if no Content-Length
    // no Content-Length nor Chunked header -> read until eof()
    if SockIn<>nil then
      while not eof(SockIn^) do begin
        readln(SockIn^,Line);
        if Content='' then
          Content := Line else
          Content := Content+#13#10+Line;
      end;
    ContentLength := length(Content); // update Content-Length
    exit;
  end;
  // optionaly uncompress content
  if cardinal(fContentCompress)<cardinal(length(fCompress)) then
    if fCompress[fContentCompress].Func(Content,false)='' then
      // invalid content
      raise ECrtSocket.CreateFmt('%s uncompress',[fCompress[fContentCompress].Name]);
  ContentLength := length(Content); // update Content-Length
  if SockIn<>nil then begin
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('GetBody2',Error);
  end;
  {$I+}
end;

procedure THttpSocket.GetHeader;
var s: SockString;
    i, n: integer;
    P: PAnsiChar;
begin
  fBodyRetrieved := false;
  ContentType := '';
  ContentLength := -1;
  fContentCompress := -1;
  ConnectionClose := false;
  ConnectionUpgrade := false;
  Chunked := false;
  n := 0;
  repeat
    SockRecvLn(s);
    if s='' then
      break; // headers end with a void line
    if length(Headers)<=n then
      SetLength(Headers,n+10);
    Headers[n] := s;
    inc(n);
    P := pointer(s);
    case IdemPCharArray(P,['CONTENT-','TRANSFER-ENCODING: CHUNKED','CONNECTION: ',
      'ACCEPT-ENCODING:']) of
    0: case IdemPCharArray(P+8,['LENGTH:','TYPE:','ENCODING:']) of
       0: ContentLength := GetCardinal(pointer(PAnsiChar(pointer(s))+16));
       1: ContentType := trim(copy(s,14,128));
       2: if fCompress<>nil then begin
            i := 18;
            while s[i+1]=' ' do inc(i);
            delete(s,1,i);
            for i := 0 to high(fCompress) do
              if fCompress[i].Name=s then begin
                fContentCompress := i;
                break;
              end;
          end;
       end;
    1: Chunked := true;
    2: case IdemPCharArray(P+12,['CLOSE','UPGRADE','KEEP-ALIVE, UPGRADE']) of
       0:   ConnectionClose := true;
       1,2: ConnectionUpgrade := true;
       end;
    3: if fCompress<>nil then
         fCompressHeader := ComputeContentEncoding(fCompress,P+16);
    end;
  until false;
  SetLength(Headers,n);
end;

function THttpSocket.HeaderAdd(const aValue: SockString): integer;
begin
  result := length(Headers);
  SetLength(Headers,result+1);
  Headers[result] := aValue;
end;

procedure THttpSocket.HeaderSetText(const aText, aForcedContentType: SockString);
var P, PDeb: PAnsiChar;
    n: integer;
begin
  P := pointer(aText);
  n := 0;
  if P<>nil then
    repeat
      PDeb := P;
      while P^>#13 do inc(P);
      if PDeb<>P then begin // add any not void line
        if length(Headers)<=n then
          SetLength(Headers,n+n shr 3+8);
        SetString(Headers[n],PDeb,P-PDeb);
        inc(n);
      end;
      while (P^=#13) or (P^=#10) do inc(P);
    until P^=#0;
  SetLength(Headers,n);
  if (aForcedContentType='') or (HeaderValue('Content-Type')<>'') then
    exit;
  SetLength(Headers,n+1);
  Headers[n] := 'Content-Type: '+aForcedContentType;
end;

function THttpSocket.HeaderGetText: SockString;
var i,L,n: integer;
    P: PAnsiChar;
begin // faster than for i := 0 to Count-1 do result := result+Headers[i]+#13#10
  result := '';
  n := length(Headers);
  if n=0 then
    exit;
  L := n*2; // #13#10 size
  dec(n);
  for i := 0 to n do
    inc(L,length(Headers[i]));
  SetLength(result,L);
  P := pointer(result);
  for i := 0 to n do begin
    L := length(Headers[i]);
    if L>0 then begin
      move(pointer(Headers[i])^,P^,L);
      inc(P,L);
    end;
    PWord(P)^ := 13+10 shl 8;
    inc(P,2);
  end;
end;

function THttpSocket.HeaderValue(aName: SockString): SockString;
var i: integer;
begin
  if Headers<>nil then begin
    aName := UpperCase(aName)+':';
    for i := 0 to high(Headers) do
      if IdemPChar(pointer(Headers[i]),pointer(aName)) then begin
        result := trim(copy(Headers[i],length(aName)+1,maxInt));
        exit;
      end;
  end;
  result := '';
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize)<>'';
end;

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: SockString;
  var OutContent: SockString);
var OutContentEncoding: SockString;
begin
  if integer(fCompressHeader)<>0 then begin
    OutContentEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
      OutContentType,OutContent);
    if OutContentEncoding<>'' then
        SockSend(['Content-Encoding: ',OutContentEncoding]);
  end;
  SockSend(['Content-Length: ',length(OutContent)]); // needed even 0
  if (OutContent<>'') and (OutContentType<>'') then
    SockSend(['Content-Type: ',OutContentType]);
end;

procedure GetSinIPFromCache(const sin: TVarSin; var result: SockString);
begin
  if sin.sin_family=AF_INET then
    IP4Text(sin.sin_addr,result) else begin
    result := GetSinIP(sin); // AF_INET6 may be optimized in a future revision
    if result='::1' then
      result := '127.0.0.1'; // IP6 localhost loopback benefits of matching IP4
  end;
end;


{ THttpServerSocket }

constructor THttpServerSocket.Create(aServer: THttpServer);
begin
  inherited Create(5000);
  if aServer<>nil then begin
    fServer := aServer;
    fCompress := aServer.fCompress;
    fCompressAcceptEncoding := aServer.fCompressAcceptEncoding;
    TCPPrefix := aServer.TCPPrefix;
  end;
end;

procedure THttpServerSocket.InitRequest(aClientSock: TSocket);
begin
  AcceptRequest(aClientSock, @fRemoteIP);
end;

function THttpServerSocket.HeaderGetText: SockString;
begin
  if fHeaderText='' then begin
    fHeaderText := inherited HeaderGetText;
    if fRemoteIP<>'' then
      fHeaderText := fHeaderText+'RemoteIP: '+fRemoteIP+#13#10;
  end;
  result := fHeaderText;
end;

function THttpServerSocket.GetRequest(withBody: boolean=true): boolean;
var P: PAnsiChar;
    i, L: integer;
    H: ^PAnsiChar;
    maxtix, status: cardinal;
begin
  result := false;
  try
    maxtix := GetTickCount+10000; // allow 10 sec for header -> DOS/TCPSYN Flood
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Command);
    if TCPPrefix<>'' then
      if TCPPrefix<>Command then
        exit else
        SockRecvLn(Command);
    P := pointer(Command);
    GetNextItem(P,' ',fMethod); // 'GET'
    GetNextItem(P,' ',fURL);    // '/path'
    fKeepAliveClient := IdemPChar(P,'HTTP/1.1');
    Content := '';
    // get headers and content
    GetHeader;
    if fServer<>nil then begin // e.g. =nil from TRTSPOverHTTPServer
      L := length(fServer.fRemoteIPHeaderUpper);
      if L<>0 then  begin
        H := pointer(Headers);
        for i := 1 to length(Headers) do
        if IdemPChar(H^,pointer(fServer.fRemoteIPHeaderUpper)) and (H^[L]=':') then begin
          repeat inc(L) until H^[L]<>' ';
          if H^<>#0 then
            fRemoteIP := H^;
          break;
        end else
          inc(H);
      end;
    end;
    if ConnectionClose then
      fKeepAliveClient := false;
    if (ContentLength<0) and (KeepAliveClient or (fMethod = 'GET')) then
      ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    if GetTickCount>maxtix then // time wrap after 49.7 days is just ignored
      exit;
    if fServer<>nil then begin
      if (ContentLength>0) and (fServer.MaximumAllowedContentLength>0) and
         (cardinal(ContentLength)>fServer.MaximumAllowedContentLength) then begin
        SockSend('HTTP/1.0 413 Payload Too Large'#13#10#13#10'Rejected');
        SockSendFlush;
        exit;
      end;
      if Assigned(fServer.OnBeforeBody) then begin
        status := fServer.OnBeforeBody(
          fURL,fMethod,HeaderGetText,ContentType,RemoteIP,ContentLength,false);
        if status<>STATUS_SUCCESS then begin
          SockSend(['HTTP/1.0 ',status,' ',StatusCodeToReason(status),
            #13#10#13#10,'Rejected']);
          SockSendFlush;
          exit;
        end;
      end;
    end;
    if withBody and not ConnectionUpgrade then
      GetBody;
    result := true;
  except
    on E: Exception do
      result := false; // mark error
  end;
end;

procedure DirectShutdown(sock: TSocket);
begin
  if sock<=0 then
    exit;
  Shutdown(sock,SHUT_WR);
  CloseSocket(sock); // SO_LINGER usually set to 5 or 10 seconds
end;

function AsynchSocket(sock: TSocket): boolean;
var nonblocking: integer;
begin
  nonblocking := 1; // for both Windows and POSIX
  if sock<=0 then
    result := false else
    result := IoctlSocket(sock, FIONBIO, nonblocking)=0;
end;

function AsynchRecv(sock: TSocket; buf: pointer; buflen: integer): integer;
begin
  {$ifdef MSWINDOWS}
  result := Recv(sock,buf,buflen,0);
  {$else}
  {$ifdef KYLIX3}
  result := LibC.Recv(sock,buf^,buflen,0);
  {$else}
  result := fpRecv(sock,buf,buflen,0);
  {$endif}
  {$endif}
end;

function AsynchSend(sock: TSocket; buf: pointer; buflen: integer): integer;
begin
  {$ifdef MSWINDOWS}
  result := Send(sock,buf,buflen,MSG_NOSIGNAL);
  {$else}
  {$ifdef KYLIX3}
  result := LibC.Send(sock,buf^,buflen,MSG_NOSIGNAL);
  {$else}
  result := fpSend(sock,buf,buflen,MSG_NOSIGNAL);
  {$endif}
  {$endif}
end;


{ ECrtSocket }

function GetRemoteIP(aClientSock: TSocket): SockString;
var Name: TVarSin;
begin
  if GetPeerName(aClientSock,Name)=0 then
    GetSinIPFromCache(Name,result) else
    result := '';
end;

function SocketErrorMessage(Error: integer): string;
begin
  if Error=-1 then
   Error := WSAGetLastError;
  case Error of
  WSAETIMEDOUT:    result := 'WSAETIMEDOUT';
  WSAENETDOWN:     result := 'WSAENETDOWN';
  WSAEWOULDBLOCK:  result := 'WSAEWOULDBLOCK';
  WSAECONNABORTED: result := 'WSAECONNABORTED';
  WSAECONNRESET:   result := 'WSAECONNRESET';
  WSAEMFILE:       result := 'WSAEMFILE';
  else result := '';
  end;
  result := Format('%d %s [%s]',[Error,result,SysErrorMessage(Error)]);
end;
constructor ECrtSocket.Create(const Msg: string);
begin
  Create(Msg,WSAGetLastError);
end;

constructor ECrtSocket.Create(const Msg: string; Error: integer);
begin
  if Error=0 then
    fLastError := WSAEWOULDBLOCK else // if unknown, probably a timeout
    fLastError := abs(Error);
  inherited Create(Msg+' '+SocketErrorMessage(fLastError));
end;

constructor ECrtSocket.CreateFmt(const Msg: string; const Args: array of const; Error: integer);
begin
  if Error<0 then
    Error := WSAGetLastError;
  Create(Format(Msg,Args),Error);
end;


{ TSynThreadPool }

const
  // up to 256 * 2MB = 512MB of RAM for the TSynThreadPoolSubThread stack
  THREADPOOL_MAXSUBTHREADS = 256;

  // kept-alive or big HTTP requests will create a dedicated THttpServerResp
  // - each thread reserves 2 MB of memory so it may break the server
  // - keep the value to a decent number, to let resources be constrained up to 1GB
  THREADPOOL_MAXWORKTHREADS = 512;

  // if HTTP body length is bigger than 1 MB, creates a dedicated THttpServerResp
  THREADPOOL_BIGBODYSIZE = 1024*1024;

constructor TSynThreadPool.Create(NumberOfThreads: Integer
  {$ifdef USE_WINIOCP}; aOverlapHandle: THandle{$endif});
var i: integer;
begin
  if NumberOfThreads=0 then
    NumberOfThreads := 1 else
    if cardinal(NumberOfThreads)>THREADPOOL_MAXSUBTHREADS then
      NumberOfThreads := THREADPOOL_MAXSUBTHREADS;
  // create IO completion port to queue the HTTP requests
  {$ifdef USE_WINIOCP}
  fRequestQueue := CreateIoCompletionPort(aOverlapHandle, 0, 0, NumberOfThreads);
  if fRequestQueue=INVALID_HANDLE_VALUE then begin
    fRequestQueue := 0;
    exit;
  end;
  {$endif}
  // now create the worker threads
  fSubThread := TObjectList.Create(true);
  for i := 1 to NumberOfThreads do
    fSubThread.Add(TSynThreadPoolSubThread.Create(Self));
end;

destructor TSynThreadPool.Destroy;
var i: integer;
    endtix: cardinal;
begin
  fTerminated := true;
  try
    // notify the threads we are shutting down
    for i := 0 to fSubThread.Count-1 do
      {$ifdef USE_WINIOCP}
      PostQueuedCompletionStatus(fRequestQueue,0,0,nil);
      {$else}
      TSynThreadPoolSubThread(fSubThread.Items[i]).fEvent.SetEvent;
      {$endif}
    // wait for threads to finish, with 30 seconds TimeOut
    endtix := GetTickCount+30000;
    while (fRunningThreads>0) and (GetTickCount<endtix) do
      Sleep(5);
    fSubThread.Free;
  finally
    {$ifdef USE_WINIOCP}
    CloseHandle(fRequestQueue);
    {$endif}
  end;
  inherited Destroy;
end;

function TSynThreadPool.Push(aContext: pointer): boolean;
{$ifndef USE_WINIOCP}
var i: integer;
    thread: ^TSynThreadPoolSubThread;
{$endif}
begin
  result := false;
  if (self=nil) or fTerminated then
    exit;
  {$ifdef USE_WINIOCP}
  result := PostQueuedCompletionStatus(fRequestQueue,0,0,aContext);
  {$else}
  thread := pointer(fSubThread.List);
  for i := 1 to fSubThread.Count do
    if (thread^.fProcessingContext=nil) and thread^.AssignProcess(aContext) then begin
      result := true;
      exit;
    end else
      inc(thread);
  {$endif}
end;

function TSynThreadPool.NeedStopOnIOError: boolean;
begin
  result := True;
end;

{ TSynThreadPoolSubThread }

constructor TSynThreadPoolSubThread.Create(Owner: TSynThreadPool);
begin
  fOwner := Owner; // ensure it is set ASAP: on Linux, Execute raises immediately
  fOnTerminate := Owner.fOnTerminate;
  InitializeCriticalSection(fProcessingContextCS);
  {$ifndef USE_WINIOCP}
  fEvent := TEvent.Create(nil,false,false,'');
  {$endif}
  inherited Create(false);
end;

destructor TSynThreadPoolSubThread.Destroy;
begin
  inherited Destroy;
  DeleteCriticalSection(fProcessingContextCS);
  {$ifndef USE_WINIOCP}
  fEvent.Free;
  {$endif}
end;

{$ifdef USE_WINIOCP}
function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: PtrUInt;
  var lpOverlapped: pointer; dwMilliseconds: DWORD): BOOL; stdcall;
  external kernel32; // redefine with an unique signature for all Delphi/FPC
{$else}
function TSynThreadPoolSubThread.AssignProcess(aContext: pointer): boolean;
begin
  result := false;
  EnterCriticalSection(fProcessingContextCS);
  try
    if fProcessingContext=nil then begin
      fProcessingContext := aContext;
      fEvent.SetEvent;
      result := true; // successfully sent to an idle thread
    end;
  finally
    LeaveCriticalSection(fProcessingContextCS);
  end;
end;
{$endif}

procedure TSynThreadPoolSubThread.Execute;
var Context: pointer;
    {$ifdef USE_WINIOCP}
    Dummy1: DWORD;
    Dummy2: PtrUInt;
    {$endif}
begin
  if fOwner<>nil then
  try
    InterlockedIncrement(fOwner.fRunningThreads);
    NotifyThreadStart(self);
    repeat
      {$ifdef USE_WINIOCP}
      if not GetQueuedCompletionStatus(fOwner.fRequestQueue,Dummy1,Dummy2,Context,INFINITE) and
         fOwner.NeedStopOnIOError then
        break;
      {$else}
      fEvent.WaitFor(INFINITE);
      if fOwner.fTerminated then
        break;
      EnterCriticalSection(fProcessingContextCS);
      Context := fProcessingContext;
      LeaveCriticalSection(fProcessingContextCS);
      {$endif}
      if Context<>nil then
        try
          try
            fOwner.Task(Self,Context);
          finally
            EnterCriticalSection(fProcessingContextCS);
            fProcessingContext := nil;
            LeaveCriticalSection(fProcessingContextCS);
          end;
        except
          on Exception do  // we should handle all exceptions in this loop
            inc(fOwner.fExceptionsCount);
        end;
    until fOwner.fTerminated;
  finally
    InterlockedDecrement(fOwner.fRunningThreads);
  end;
end;

procedure TSynThreadPoolSubThread.NotifyThreadStart(Sender: TSynThread);
begin
  if Sender=nil then
    raise ECrtSocket.Create('NotifyThreadStart(nil)');
  if Assigned(fOwner.fOnThreadStart) and not Assigned(Sender.fStartNotified) then begin
    fOwner.fOnThreadStart(Sender);
    Sender.fStartNotified := self;
  end;
end;


{ TSynThreadPoolTHttpServer }

constructor TSynThreadPoolTHttpServer.Create(Server: THttpServer; NumberOfThreads: Integer=32);
begin
  fServer := Server;
  fOnTerminate := fServer.fOnTerminate;
  inherited Create(NumberOfThreads);
end;

procedure TSynThreadPoolTHttpServer.Task(aCaller: TSynThread; aContext: Pointer);
var ServerSock: THttpServerSocket;
begin
  if fServer.Terminated then
    exit;
  ServerSock := THttpServerSocket.Create(fServer);
  try
    ServerSock.InitRequest(TSocket(aContext));
    // get Header of incoming request in the thread pool
    if ServerSock.GetRequest(false) then begin
      InterlockedIncrement(fHeaderProcessed);
      // connection and header seem valid -> process request further
      if (fServer.fInternalHttpServerRespList.Count<THREADPOOL_MAXWORKTHREADS) and
         (ServerSock.KeepAliveClient or
          (ServerSock.ContentLength>THREADPOOL_BIGBODYSIZE)) then begin
        // HTTP/1.1 Keep Alive (including WebSockets) or posted data > 1 MB
        // -> process in dedicated background thread
        fServer.fThreadRespClass.Create(ServerSock,fServer);
        ServerSock := nil; // THttpServerResp will do ServerSock.Free
        InterlockedIncrement(fBodyOwnThreads);
      end else begin
        // no Keep Alive = multi-connection -> process in the Thread Pool
        ServerSock.GetBody; // we need to get it now
        // multi-connection -> ID=0
        fServer.Process(ServerSock,0,aCaller);
        fServer.OnDisconnect;
        // no Shutdown here: will be done client-side
        InterlockedIncrement(fBodyProcessed);
      end;
    end else
      InterlockedIncrement(fHeaderErrors);
  finally
    FreeAndNil(ServerSock);
  end;
end;


{$ifdef MSWINDOWS}

{ ************  http.sys / HTTP API low-level direct access }

{$MINENUMSIZE 4}
{$A+}

{$ifdef FPC}
{$PACKRECORDS C}
{$endif}

type
  // HTTP version used
  HTTP_VERSION = packed record
    MajorVersion: word;
    MinorVersion: word;
  end;

  // the req* values identify Request Headers, and resp* Response Headers
  THttpHeader = (
    reqCacheControl,
    reqConnection,
    reqDate,
    reqKeepAlive,
    reqPragma,
    reqTrailer,
    reqTransferEncoding,
    reqUpgrade,
    reqVia,
    reqWarning,
    reqAllow,
    reqContentLength,
    reqContentType,
    reqContentEncoding,
    reqContentLanguage,
    reqContentLocation,
    reqContentMd5,
    reqContentRange,
    reqExpires,
    reqLastModified,
    reqAccept,
    reqAcceptCharset,
    reqAcceptEncoding,
    reqAcceptLanguage,
    reqAuthorization,
    reqCookie,
    reqExpect,
    reqFrom,
    reqHost,
    reqIfMatch,
    reqIfModifiedSince,
    reqIfNoneMatch,
    reqIfRange,
    reqIfUnmodifiedSince,
    reqMaxForwards,
    reqProxyAuthorization,
    reqReferrer,
    reqRange,
    reqTe,
    reqTranslate,
    reqUserAgent
{$ifdef DELPHI5OROLDER}
   );
const // Delphi 5 does not support values overlapping for enums
  respAcceptRanges = THttpHeader(20);
  respAge = THttpHeader(21);
  respEtag = THttpHeader(22);
  respLocation = THttpHeader(23);
  respProxyAuthenticate = THttpHeader(24);
  respRetryAfter = THttpHeader(25);
  respServer = THttpHeader(26);
  respSetCookie = THttpHeader(27);
  respVary = THttpHeader(28);
  respWwwAuthenticate = THttpHeader(29);
type
{$else}  ,
    respAcceptRanges = 20,
    respAge,
    respEtag,
    respLocation,
    respProxyAuthenticate,
    respRetryAfter,
    respServer,
    respSetCookie,
    respVary,
    respWwwAuthenticate);
{$endif}

  THttpVerb = (
    hvUnparsed,
    hvUnknown,
    hvInvalid,
    hvOPTIONS,
    hvGET,
    hvHEAD,
    hvPOST,
    hvPUT,
    hvDELETE,
    hvTRACE,
    hvCONNECT,
    hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
    hvMOVE,
    hvCOPY,
    hvPROPFIND,
    hvPROPPATCH,
    hvMKCOL,
    hvLOCK,
    hvUNLOCK,
    hvSEARCH,
    hvMaximum );

  THttpChunkType = (
    hctFromMemory,
    hctFromFileHandle,
    hctFromFragmentCache);

  THttpServiceConfigID = (
    hscIPListenList,
    hscSSLCertInfo,
    hscUrlAclInfo,
    hscMax);
  THttpServiceConfigQueryType = (
    hscQueryExact,
    hscQueryNext,
    hscQueryMax);

  HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;
  HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
  HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;

  // Pointers overlap and point into pFullUrl. nil if not present.
  HTTP_COOKED_URL = record
    FullUrlLength: word;     // in bytes not including the #0
    HostLength: word;        // in bytes not including the #0
    AbsPathLength: word;     // in bytes not including the #0
    QueryStringLength: word; // in bytes not including the #0
    pFullUrl: PWideChar;     // points to "http://hostname:port/abs/.../path?query"
    pHost: PWideChar;        // points to the first char in the hostname
    pAbsPath: PWideChar;     // Points to the 3rd '/' char
    pQueryString: PWideChar; // Points to the 1st '?' char or #0
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: PSOCKADDR;
    pLocalAddress: PSOCKADDR;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: word;          // in bytes not including the #0
    RawValueLength: word;      // in bytes not including the n#0
    pName: PAnsiChar;          // The header name (minus the ':' character)
    pRawValue: PAnsiChar;      // The header value
  end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;
  HTTP_UNKNOWN_HEADERs = array of HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    RawValueLength: word;     // in bytes not including the #0
    pRawValue: PAnsiChar;
  end;
  PHTTP_KNOWN_HEADER = ^HTTP_KNOWN_HEADER;

  HTTP_RESPONSE_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: pointer;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..respWwwAuthenticate] of HTTP_KNOWN_HEADER;
  end;

  HTTP_REQUEST_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..reqUserAgent] of HTTP_KNOWN_HEADER;
  end;

  HTTP_BYTE_RANGE = record
    StartingOffset: ULARGE_INTEGER;
    Length: ULARGE_INTEGER;
  end;

  // we use 3 distinct HTTP_DATA_CHUNK_* records since variable records
  // alignment is buggy/non compatible under Delphi XE3
  HTTP_DATA_CHUNK_INMEMORY = record
    DataChunkType: THttpChunkType; // always hctFromMemory
    Reserved1: ULONG;
    pBuffer: pointer;
    BufferLength: ULONG;
    Reserved2: ULONG;
    Reserved3: ULONG;
  end;
  PHTTP_DATA_CHUNK_INMEMORY = ^HTTP_DATA_CHUNK_INMEMORY;
  HTTP_DATA_CHUNK_FILEHANDLE = record
    DataChunkType: THttpChunkType; // always hctFromFileHandle
    ByteRange: HTTP_BYTE_RANGE;
    FileHandle: THandle;
  end;
  HTTP_DATA_CHUNK_FRAGMENTCACHE = record
    DataChunkType: THttpChunkType; // always hctFromFragmentCache
    FragmentNameLength: word;      // in bytes not including the #0
    pFragmentName: PWideChar;
  end;

  HTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags: ULONG;
    CertEncodedSize: ULONG;
    pCertEncoded: PUCHAR;
    Token: THandle;
    CertDeniedByMapper: boolean;
  end;
  PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

  HTTP_SSL_INFO = record
    ServerCertKeySize: word;
    ConnectionKeySize: word;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_URLACL_PARAM;
  end;
  HTTP_SERVICE_CONFIG_URLACL_QUERY = record
    QueryDesc: THttpServiceConfigQueryType;
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    dwToken: DWORD;
  end;

  HTTP_REQUEST_INFO_TYPE = (
    HttpRequestInfoTypeAuth
    );

  // about Authentication in HTTP Version 2.0
  // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452

  HTTP_AUTH_STATUS = (
    HttpAuthStatusSuccess,
    HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure
    );

  HTTP_REQUEST_AUTH_TYPE = (
    HttpRequestAuthTypeNone,
    HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest,
    HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate,
    HttpRequestAuthTypeKerberos
    );

  SECURITY_STATUS = ULONG;

  HTTP_REQUEST_AUTH_INFO = record
    AuthStatus: HTTP_AUTH_STATUS;
    SecStatus: SECURITY_STATUS;
    Flags: ULONG;
    AuthType: HTTP_REQUEST_AUTH_TYPE;
    AccessToken: THandle;
    ContextAttributes: ULONG;
    PackedContextLength: ULONG;
    PackedContextType: ULONG;
    PackedContext: pointer;
    MutualAuthDataLength: ULONG;
    pMutualAuthData: PCHAR;
  end;
  PHTTP_REQUEST_AUTH_INFO = ^HTTP_REQUEST_AUTH_INFO;

  HTTP_REQUEST_INFO = record
    InfoType: HTTP_REQUEST_INFO_TYPE;
    InfoLength: ULONG;
    pInfo: pointer;
  end;
  HTTP_REQUEST_INFOS = array[0..1000] of HTTP_REQUEST_INFO;
  PHTTP_REQUEST_INFOS = ^HTTP_REQUEST_INFOS;

  /// structure used to handle data associated with a specific request
  HTTP_REQUEST = record
    // either 0 (Only Header), either HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY
    Flags: cardinal;
    // An identifier for the connection on which the request was received
    ConnectionId: HTTP_CONNECTION_ID;
    // A value used to identify the request when calling
    // HttpReceiveRequestEntityBody, HttpSendHttpResponse, and/or
    // HttpSendResponseEntityBody
    RequestId: HTTP_REQUEST_ID;
    // The context associated with the URL prefix
    UrlContext: HTTP_URL_CONTEXT;
    // The HTTP version number
    Version: HTTP_VERSION;
    // An HTTP verb associated with this request
    Verb: THttpVerb;
    // The length of the verb string if the Verb field is hvUnknown
    // (in bytes not including the last #0)
    UnknownVerbLength: word;
    // The length of the raw (uncooked) URL (in bytes not including the last #0)
    RawUrlLength: word;
     // Pointer to the verb string if the Verb field is hvUnknown
    pUnknownVerb: PAnsiChar;
    // Pointer to the raw (uncooked) URL
    pRawUrl: PAnsiChar;
    // The canonicalized Unicode URL
    CookedUrl: HTTP_COOKED_URL;
    // Local and remote transport addresses for the connection
    Address: HTTP_TRANSPORT_ADDRESS;
    // The request headers.
    Headers: HTTP_REQUEST_HEADERS;
    // The total number of bytes received from network for this request
    BytesReceived: ULONGLONG;
    EntityChunkCount: word;
    pEntityChunks: pointer;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    // SSL connection information
    pSslInfo: PHTTP_SSL_INFO;
    // beginning of HTTP_REQUEST_V2 structure
    xxxPadding: DWORD;
    RequestInfoCount: word;
    pRequestInfo: PHTTP_REQUEST_INFOS;
  end;
  PHTTP_REQUEST = ^HTTP_REQUEST;

  HTTP_RESPONSE_INFO_TYPE = (
    HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty,
    HttpResponseInfoTypeQosProperty,
    HttpResponseInfoTypeChannelBind
    );

  HTTP_RESPONSE_INFO = record
    Typ: HTTP_RESPONSE_INFO_TYPE;
    Length: ULONG;
    pInfo: Pointer;
  end;
  PHTTP_RESPONSE_INFO = ^HTTP_RESPONSE_INFO;

  /// structure as expected by HttpSendHttpResponse() API
  HTTP_RESPONSE = object
  public
    Flags: cardinal;
    // The raw HTTP protocol version number
    Version: HTTP_VERSION;
    // The HTTP status code (e.g., 200)
    StatusCode: word;
    // in bytes not including the '\0'
    ReasonLength: word;
    // The HTTP reason (e.g., "OK"). This MUST not contain non-ASCII characters
    // (i.e., all chars must be in range 0x20-0x7E).
    pReason: PAnsiChar;
    // The response headers
    Headers: HTTP_RESPONSE_HEADERS;
    // number of elements in pEntityChunks[] array
    EntityChunkCount: word;
    // pEntityChunks points to an array of EntityChunkCount HTTP_DATA_CHUNK_*
    pEntityChunks: pointer;
    // contains the number of HTTP API 2.0 extended information
    ResponseInfoCount: word;
    // map the HTTP API 2.0 extended information
    pResponseInfo: PHTTP_RESPONSE_INFO;
    // will set both StatusCode and Reason
    // - OutStatus is a temporary variable which will be field with the
    // corresponding text
    procedure SetStatus(code: integer; var OutStatus: SockString);
    // will set the content of the reponse, and ContentType header
    procedure SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
      const Content: SockString; const ContentType: SockString='text/html');
    /// will set all header values from lines
    // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
    // - all other headers will be set in temp UnknownHeaders[]
    procedure SetHeaders(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs);
    /// add one header value to the internal headers
    // - SetHeaders() method should have been called before to initialize the
    // internal UnknownHeaders[] array
    function AddCustomHeader(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs;
      ForceCustomHeader: boolean): PAnsiChar;
  end;
  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_PROPERTY_FLAGS = ULONG;

  HTTP_ENABLED_STATE = (
    HttpEnabledStateActive,
    HttpEnabledStateInactive
    );
  PHTTP_ENABLED_STATE = ^HTTP_ENABLED_STATE;

  HTTP_STATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    State: HTTP_ENABLED_STATE;
  end;
  PHTTP_STATE_INFO = ^HTTP_STATE_INFO;

  THTTP_503_RESPONSE_VERBOSITY = (
    Http503ResponseVerbosityBasic,
    Http503ResponseVerbosityLimited,
    Http503ResponseVerbosityFull
    );
  PHTTP_503_RESPONSE_VERBOSITY = ^ THTTP_503_RESPONSE_VERBOSITY;

  HTTP_QOS_SETTING_TYPE = (
    HttpQosSettingTypeBandwidth,
    HttpQosSettingTypeConnectionLimit,
    HttpQosSettingTypeFlowRate // Windows Server 2008 R2 and Windows 7 only.
    );
  PHTTP_QOS_SETTING_TYPE = ^HTTP_QOS_SETTING_TYPE;

  HTTP_QOS_SETTING_INFO = record
    QosType: HTTP_QOS_SETTING_TYPE;
    QosSetting: Pointer;
  end;
  PHTTP_QOS_SETTING_INFO = ^HTTP_QOS_SETTING_INFO;

  HTTP_CONNECTION_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxConnections: ULONG;
  end;
  PHTTP_CONNECTION_LIMIT_INFO = ^HTTP_CONNECTION_LIMIT_INFO;

  HTTP_BANDWIDTH_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
  end;
  PHTTP_BANDWIDTH_LIMIT_INFO = ^HTTP_BANDWIDTH_LIMIT_INFO;

  HTTP_FLOWRATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
    MaxPeakBandwidth: ULONG;
    BurstSize: ULONG;
  end;
  PHTTP_FLOWRATE_INFO = ^HTTP_FLOWRATE_INFO;

const
   HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG} = 1024;
   HTTP_LIMIT_INFINITE {:ULONG} = ULONG(-1);

type
  HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (
    IdleConnectionTimeout,
    HeaderWaitTimeout
    );
  PHTTP_SERVICE_CONFIG_TIMEOUT_KEY = ^HTTP_SERVICE_CONFIG_TIMEOUT_KEY;

  HTTP_SERVICE_CONFIG_TIMEOUT_PARAM = word;
  PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM = ^HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

  HTTP_SERVICE_CONFIG_TIMEOUT_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
  end;
  PHTTP_SERVICE_CONFIG_TIMEOUT_SET = ^HTTP_SERVICE_CONFIG_TIMEOUT_SET;

  HTTP_TIMEOUT_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EntityBody: word;
    DrainEntityBody: word;
    RequestQueue: word;
    IdleConnection: word;
    HeaderWait: word;
    MinSendRate: cardinal;
  end;
  PHTTP_TIMEOUT_LIMIT_INFO = ^HTTP_TIMEOUT_LIMIT_INFO;

  HTTP_LISTEN_ENDPOINT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EnableSharing: boolean;
  end;
  PHTTP_LISTEN_ENDPOINT_INFO = ^HTTP_LISTEN_ENDPOINT_INFO;

  HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
    DomainNameLength: word;
    DomainName: PWideChar;
    RealmLength: word;
    Realm: PWideChar;
  end;
  PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = ^HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

  HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
    RealmLength: word;
    Realm: PWideChar;
  end;
  PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = ^HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

const
  HTTP_AUTH_ENABLE_BASIC        = $00000001;
  HTTP_AUTH_ENABLE_DIGEST       = $00000002;
  HTTP_AUTH_ENABLE_NTLM         = $00000004;
  HTTP_AUTH_ENABLE_NEGOTIATE    = $00000008;
  HTTP_AUTH_ENABLE_KERBEROS     = $00000010;
  HTTP_AUTH_ENABLE_ALL          = $0000001F;

  HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING  = $01;
  HTTP_AUTH_EX_FLAG_CAPTURE_CREDENTIAL                  = $02;

type
  HTTP_SERVER_AUTHENTICATION_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    AuthSchemes: ULONG;
    ReceiveMutualAuth: BYTEBOOL;
    ReceiveContextHandle: BYTEBOOL;
    DisableNTLMCredentialCaching: BYTEBOOL;
    ExFlags: BYTE;
    DigestParams: HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
    BasicParams: HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
  end;
  PHTTP_SERVER_AUTHENTICATION_INFO = ^HTTP_SERVER_AUTHENTICATION_INFO;


  HTTP_SERVICE_BINDING_TYPE=(
    HttpServiceBindingTypeNone,
    HttpServiceBindingTypeW,
    HttpServiceBindingTypeA
    );

  HTTP_SERVICE_BINDING_BASE = record
    BindingType: HTTP_SERVICE_BINDING_TYPE;
  end;
  PHTTP_SERVICE_BINDING_BASE = ^HTTP_SERVICE_BINDING_BASE;

  HTTP_SERVICE_BINDING_A = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PAnsiChar;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_A = HTTP_SERVICE_BINDING_A;

  HTTP_SERVICE_BINDING_W = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PWCHAR;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_W = ^HTTP_SERVICE_BINDING_W;

  HTTP_AUTHENTICATION_HARDENING_LEVELS = (
    HttpAuthenticationHardeningLegacy,
    HttpAuthenticationHardeningMedium,
    HttpAuthenticationHardeningStrict
  );

const
  HTTP_CHANNEL_BIND_PROXY = $1;
  HTTP_CHANNEL_BIND_PROXY_COHOSTING = $20;

  HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK = $2;
  HTTP_CHANNEL_BIND_DOTLESS_SERVICE = $4;
  HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN = $8;
  HTTP_CHANNEL_BIND_CLIENT_SERVICE = $10;

type
  HTTP_CHANNEL_BIND_INFO = record
    Hardening: HTTP_AUTHENTICATION_HARDENING_LEVELS;
    Flags: ULONG;
    ServiceNames: PHTTP_SERVICE_BINDING_BASE;
    NumberOfServiceNames: ULONG;
  end;
  PHTTP_CHANNEL_BIND_INFO = ^HTTP_CHANNEL_BIND_INFO;

  HTTP_REQUEST_CHANNEL_BIND_STATUS = record
    ServiceName: PHTTP_SERVICE_BINDING_BASE;
    ChannelToken: PUCHAR;
    ChannelTokenSize: ULONG;
    Flags: ULONG;
  end;
  PHTTP_REQUEST_CHANNEL_BIND_STATUS = ^HTTP_REQUEST_CHANNEL_BIND_STATUS;

const
   // Logging option flags. When used in the logging configuration alters
   // some default logging behaviour.

   // HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER - This flag is used to change
   //      the log file rollover to happen by local time based. By default
   //      log file rollovers happen by GMT time.
   HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER = 1;

   // HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION - When set the unicode fields
   //      will be converted to UTF8 multibytes when writting to the log
   //      files. When this flag is not present, the local code page
   //      conversion happens.
   HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION = 2;

   // HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY -
   // HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY - These two flags are used to
   //      to do selective logging. If neither of them are present both
   //      types of requests will be logged. Only one these flags can be
   //      set at a time. They are mutually exclusive.
   HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY = 4;
   HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY = 8;

   // The known log fields recognized/supported by HTTPAPI. Following fields
   // are used for W3C logging. Subset of them are also used for error logging
   HTTP_LOG_FIELD_DATE              = $00000001;
   HTTP_LOG_FIELD_TIME              = $00000002;
   HTTP_LOG_FIELD_CLIENT_IP         = $00000004;
   HTTP_LOG_FIELD_USER_NAME         = $00000008;
   HTTP_LOG_FIELD_SITE_NAME         = $00000010;
   HTTP_LOG_FIELD_COMPUTER_NAME     = $00000020;
   HTTP_LOG_FIELD_SERVER_IP         = $00000040;
   HTTP_LOG_FIELD_METHOD            = $00000080;
   HTTP_LOG_FIELD_URI_STEM          = $00000100;
   HTTP_LOG_FIELD_URI_QUERY         = $00000200;
   HTTP_LOG_FIELD_STATUS            = $00000400;
   HTTP_LOG_FIELD_WIN32_STATUS      = $00000800;
   HTTP_LOG_FIELD_BYTES_SENT        = $00001000;
   HTTP_LOG_FIELD_BYTES_RECV        = $00002000;
   HTTP_LOG_FIELD_TIME_TAKEN        = $00004000;
   HTTP_LOG_FIELD_SERVER_PORT       = $00008000;
   HTTP_LOG_FIELD_USER_AGENT        = $00010000;
   HTTP_LOG_FIELD_COOKIE            = $00020000;
   HTTP_LOG_FIELD_REFERER           = $00040000;
   HTTP_LOG_FIELD_VERSION           = $00080000;
   HTTP_LOG_FIELD_HOST              = $00100000;
   HTTP_LOG_FIELD_SUB_STATUS        = $00200000;

   HTTP_ALL_NON_ERROR_LOG_FIELDS = HTTP_LOG_FIELD_SUB_STATUS*2-1;

   // Fields that are used only for error logging
   HTTP_LOG_FIELD_CLIENT_PORT    = $00400000;
   HTTP_LOG_FIELD_URI            = $00800000;
   HTTP_LOG_FIELD_SITE_ID        = $01000000;
   HTTP_LOG_FIELD_REASON         = $02000000;
   HTTP_LOG_FIELD_QUEUE_NAME     = $04000000;

type
  HTTP_LOGGING_TYPE = (
    HttpLoggingTypeW3C,
    HttpLoggingTypeIIS,
    HttpLoggingTypeNCSA,
    HttpLoggingTypeRaw
    );

  HTTP_LOGGING_ROLLOVER_TYPE = (
    HttpLoggingRolloverSize,
    HttpLoggingRolloverDaily,
    HttpLoggingRolloverWeekly,
    HttpLoggingRolloverMonthly,
    HttpLoggingRolloverHourly
    );

  HTTP_LOGGING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    LoggingFlags: ULONG;
    SoftwareName: PWideChar;
    SoftwareNameLength: word;
    DirectoryNameLength: word;
    DirectoryName: PWideChar;
    Format: HTTP_LOGGING_TYPE;
    Fields: ULONG;
    pExtFields: pointer;
    NumOfExtFields: word;
    MaxRecordSize: word;
    RolloverType: HTTP_LOGGING_ROLLOVER_TYPE;
    RolloverSize: ULONG;
    pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  end;
  PHTTP_LOGGING_INFO = ^HTTP_LOGGING_INFO;

  HTTP_LOG_DATA_TYPE = (
    HttpLogDataTypeFields
    );

  HTTP_LOG_DATA = record
    Typ: HTTP_LOG_DATA_TYPE
  end;
  PHTTP_LOG_DATA = ^HTTP_LOG_DATA;

  HTTP_LOG_FIELDS_DATA = record
    Base: HTTP_LOG_DATA;
    UserNameLength: word;
    UriStemLength: word;
    ClientIpLength: word;
    ServerNameLength: word;
    ServiceNameLength: word;
    ServerIpLength: word;
    MethodLength: word;
    UriQueryLength: word;
    HostLength: word;
    UserAgentLength: word;
    CookieLength: word;
    ReferrerLength: word;
    UserName: PWideChar;
    UriStem: PWideChar;
    ClientIp: PAnsiChar;
    ServerName: PAnsiChar;
    ServiceName: PAnsiChar;
    ServerIp: PAnsiChar;
    Method: PAnsiChar;
    UriQuery: PAnsiChar;
    Host: PAnsiChar;
    UserAgent: PAnsiChar;
    Cookie: PAnsiChar;
    Referrer: PAnsiChar;
    ServerPort: word;
    ProtocolStatus: word;
    Win32Status: ULONG;
    MethodNum: THttpVerb;
    SubStatus: word;
  end;
  PHTTP_LOG_FIELDS_DATA = ^HTTP_LOG_FIELDS_DATA;

  HTTP_BINDING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    RequestQueueHandle: THandle;
  end;

  HTTP_PROTECTION_LEVEL_TYPE=(
    HttpProtectionLevelUnrestricted,
    HttpProtectionLevelEdgeRestricted,
    HttpProtectionLevelRestricted
    );

  HTTP_PROTECTION_LEVEL_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    Level: HTTP_PROTECTION_LEVEL_TYPE;
  end;
  PHTTP_PROTECTION_LEVEL_INFO = ^HTTP_PROTECTION_LEVEL_INFO;

const
  HTTP_VERSION_UNKNOWN: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 0);
  HTTP_VERSION_0_9: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 9);
  HTTP_VERSION_1_0: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 0);
  HTTP_VERSION_1_1: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 1);
  /// error raised by HTTP API when the client disconnected (e.g. after timeout)
  HTTPAPI_ERROR_NONEXISTENTCONNECTION = 1229;
  // if set, available entity body is copied along with the request headers
  // into pEntityChunks
  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  // there is more entity body to be read for this request
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  // initialization for applications that use the HTTP Server API
  HTTP_INITIALIZE_SERVER = 1;
  // initialization for applications that use the HTTP configuration functions
  HTTP_INITIALIZE_CONFIG = 2;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364496
  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364499
  HTTP_SEND_RESPONSE_FLAG_DISCONNECT = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA = $00000002;
  HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA = $00000004;
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = $00000020;
  HTTP_SEND_RESPONSE_FLAG_OPAQUE = $00000040;
  // flag which can be used by HttpRemoveUrlFromUrlGroup()
  HTTP_URL_FLAG_REMOVE_ALL = 1;

function RetrieveHeaders(const Request: HTTP_REQUEST;
  const RemoteIPHeadUp: SockString; out RemoteIP: SockString): SockString;
const
  KNOWNHEADERS: array[reqCacheControl..reqUserAgent] of string[19] = (
    'Cache-Control','Connection','Date','Keep-Alive','Pragma','Trailer',
    'Transfer-Encoding','Upgrade','Via','Warning','Allow','Content-Length',
    'Content-Type','Content-Encoding','Content-Language','Content-Location',
    'Content-MD5','Content-Range','Expires','Last-Modified','Accept',
    'Accept-Charset','Accept-Encoding','Accept-Language','Authorization',
    'Cookie','Expect','From','Host','If-Match','If-Modified-Since',
    'If-None-Match','If-Range','If-Unmodified-Since','Max-Forwards',
    'Proxy-Authorization','Referer','Range','TE','Translate','User-Agent');
  REMOTEIP_HEADERLEN = 10;
  REMOTEIP_HEADER: string[REMOTEIP_HEADERLEN] = 'RemoteIP: ';
var i, L: integer;
    H: THttpHeader;
    P: PHTTP_UNKNOWN_HEADER;
    D: PAnsiChar;
begin
  assert(low(KNOWNHEADERS)=low(Request.Headers.KnownHeaders));
  assert(high(KNOWNHEADERS)=high(Request.Headers.KnownHeaders));
  // compute remote IP
  L := length(RemoteIPHeadUp);
  if L<>0 then begin
    P := Request.Headers.pUnknownHeaders;
    if P<>nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do
      if (P^.NameLength=L) and IdemPChar(P^.pName,Pointer(RemoteIPHeadUp)) then begin
        SetString(RemoteIP,p^.pRawValue,p^.RawValueLength);
        break;
      end else
      inc(P);
  end;
  if (RemoteIP='') and (Request.Address.pRemoteAddress<>nil) then
    GetSinIPFromCache(PVarSin(Request.Address.pRemoteAddress)^,RemoteIP);
  // compute headers length
  if RemoteIP<>'' then
    L := (REMOTEIP_HEADERLEN+2)+length(RemoteIP) else
    L := 0;
  for H := low(KNOWNHEADERS) to high(KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[h].RawValueLength<>0 then
      inc(L,Request.Headers.KnownHeaders[h].RawValueLength+ord(KNOWNHEADERS[h][0])+4);
  P := Request.Headers.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do begin
      inc(L,P^.NameLength+P^.RawValueLength+4); // +4 for each ': '+#13#10
      inc(P);
    end;
  // set headers content
  SetString(result,nil,L);
  D := pointer(result);
  for H := low(KNOWNHEADERS) to high(KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[h].RawValueLength<>0 then begin
      move(KNOWNHEADERS[h][1],D^,ord(KNOWNHEADERS[h][0]));
      inc(D,ord(KNOWNHEADERS[h][0]));
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(Request.Headers.KnownHeaders[h].pRawValue^,D^,
        Request.Headers.KnownHeaders[h].RawValueLength);
      inc(D,Request.Headers.KnownHeaders[h].RawValueLength);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  P := Request.Headers.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do begin
      move(P^.pName^,D^,P^.NameLength);
      inc(D,P^.NameLength);
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(P^.pRawValue^,D^,P^.RawValueLength);
      inc(D,P^.RawValueLength);
      inc(P);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  if RemoteIP<>'' then begin
    move(REMOTEIP_HEADER[1],D^,REMOTEIP_HEADERLEN);
    inc(D,REMOTEIP_HEADERLEN);
    move(pointer(RemoteIP)^,D^,length(RemoteIP));
    inc(D,length(RemoteIP));
    PWord(D)^ := 13+10 shl 8;
  {$ifopt C+}
    inc(D,2);
  end;
  assert(D-pointer(result)=L);
  {$else}
  end;
  {$endif}
end;

type
  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    HttpServerChannelBindProperty,
    HttpServerProtectionLevelProperty
    );

  /// direct late-binding access to the HTTP API server 1.0 or 2.0
  THttpAPI = packed record
    /// access to the httpapi.dll loaded library
    Module: THandle;
    /// will be either 1.0 or 2.0, depending on the published .dll functions
    Version: HTTP_VERSION;
    /// The HttpInitialize function initializes the HTTP Server API driver, starts it,
    // if it has not already been started, and allocates data structures for the
    // calling application to support response-queue creation and other operations.
    // Call this function before calling any other functions in the HTTP Server API.
    Initialize: function(Version: HTTP_VERSION; Flags: cardinal;
      pReserved: pointer=nil): HRESULT; stdcall;
    /// The HttpTerminate function cleans up resources used by the HTTP Server API
    // to process calls by an application. An application should call HttpTerminate
    // once for every time it called HttpInitialize, with matching flag settings.
    Terminate: function(Flags: cardinal;
      Reserved: integer=0): HRESULT; stdcall;
    /// The HttpCreateHttpHandle function creates an HTTP request queue for the
    // calling application and returns a handle to it.
    CreateHttpHandle: function(var ReqQueueHandle: THandle;
      Reserved: integer=0): HRESULT; stdcall;
    /// The HttpAddUrl function registers a given URL so that requests that match
    // it are routed to a specified HTTP Server API request queue. An application
    // can register multiple URLs to a single request queue using repeated calls to
    // HttpAddUrl
    // - a typical url prefix is 'http://+:80/vroot/', 'https://+:80/vroot/' or
    // 'https://adatum.com:443/secure/database/' - here the '+' is called a
    // Strong wildcard, i.e. will match every IP or server name
    AddUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar;
      Reserved: integer=0): HRESULT; stdcall;
    /// Unregisters a specified URL, so that requests for it are no longer
    // routed to a specified queue.
    RemoveUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar): HRESULT; stdcall;
    /// retrieves the next available HTTP request from the specified request queue
    ReceiveHttpRequest: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: cardinal; var pRequestBuffer: HTTP_REQUEST; RequestBufferLength: ULONG;
      var pBytesReceived: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// sent the response to a specified HTTP request
    // - pLogData optional parameter is handled since HTTP API 2.0
    SendHttpResponse: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: integer; var pHttpResponse: HTTP_RESPONSE; pReserved1: pointer;
      var pBytesSent: cardinal; pReserved2: pointer=nil; Reserved3: ULONG=0;
      pOverlapped: pointer=nil; pLogData: PHTTP_LOG_DATA=nil): HRESULT; stdcall;
    /// receives additional entity body data for a specified HTTP request
    ReceiveRequestEntityBody: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: ULONG; pBuffer: pointer; BufferLength: cardinal; var pBytesReceived: cardinal;
      pOverlapped: pointer=nil): HRESULT; stdcall;
    /// sends entity-body data associated with an HTTP response.
    SendResponseEntityBody: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: integer; EntityChunkCount: word; pEntityChunks: pointer; var pBytesSent: Cardinal;
      pReserved1: Pointer=nil; pReserved2: Pointer=nil; pOverlapped: POverlapped=nil;
      pLogData: PHTTP_LOG_DATA=nil): HRESULT; stdcall;
    /// set specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    SetServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// deletes specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    DeleteServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// removes from the HTTP Server API cache associated with a given request
    // queue all response fragments that have a name whose site portion matches
    // a specified UrlPrefix
    FlushResponseCache: function(ReqQueueHandle: THandle; pUrlPrefix: PWideChar; Flags: ULONG;
      pOverlapped: POverlapped): ULONG; stdcall;
    /// cancels a specified request
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CancelHttpRequest: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      pOverlapped: pointer = nil): HRESULT; stdcall;
    /// creates a server session for the specified HTTP API version
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateServerSession: function(Version: HTTP_VERSION;
      var ServerSessionId: HTTP_SERVER_SESSION_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// deletes the server session identified by the server session ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseServerSession: function(ServerSessionId: HTTP_SERVER_SESSION_ID): HRESULT; stdcall;
    ///  creates a new request queue or opens an existing request queue
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - replaces the HTTP version 1.0 CreateHttpHandle() function
    CreateRequestQueue: function(Version: HTTP_VERSION;
      pName: PWideChar; pSecurityAttributes: Pointer;
      Flags: ULONG; var ReqQueueHandle: THandle): HRESULT; stdcall;
    /// sets a new server session property or modifies an existing property
    // on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a server property on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// creates a URL Group under the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateUrlGroup: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      var UrlGroupId: HTTP_URL_GROUP_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// closes the URL Group identified by the URL Group ID
    // - this call also removes all of the URLs that are associated with
    // the URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID): HRESULT; stdcall;
    /// adds the specified URL to the URL Group identified by the URL Group ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - this function replaces the HTTP version 1.0 AddUrl() function
    AddUrlToUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; UrlContext: HTTP_URL_CONTEXT = 0;
      Reserved: ULONG = 0): HRESULT; stdcall;
    /// removes the specified URL from the group identified by the URL Group ID
    // - this function removes one, or all, of the URLs from the group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - it replaces the HTTP version 1.0 RemoveUrl() function
    RemoveUrlFromUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; Flags: ULONG): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the specified
    // URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a property on the specified URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the request
    // queue identified by the specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetRequestQueueProperty: function(ReqQueueHandle: THandle;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReserved: Pointer): HRESULT; stdcall;
    ///  queries a property of the request queue identified by the
    // specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryRequestQueueProperty: function(ReqQueueHandle: THandle;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReturnLength: PULONG; pReserved: Pointer): HRESULT; stdcall;
  end;

var
  Http: THttpAPI;

type
  THttpAPIs = (hInitialize,hTerminate,hCreateHttpHandle,
    hAddUrl, hRemoveUrl, hReceiveHttpRequest,
    hSendHttpResponse, hReceiveRequestEntityBody,
    hResponseEntityBody,
    hSetServiceConfiguration, hDeleteServiceConfiguration, hFlushResponseCache,
    hCancelHttpRequest,
    hCreateServerSession, hCloseServerSession,
    hCreateRequestQueue,
    hSetServerSessionProperty, hQueryServerSessionProperty,
    hCreateUrlGroup, hCloseUrlGroup,
    hAddUrlToUrlGroup, hRemoveUrlFromUrlGroup,
    hSetUrlGroupProperty, hQueryUrlGroupProperty,
    hSetRequestQueueProperty, hQueryRequestQueueProperty
    );
const
  hHttpApi2First = hCancelHttpRequest;

  HttpNames: array[THttpAPIs] of PChar = (
    'HttpInitialize','HttpTerminate','HttpCreateHttpHandle',
    'HttpAddUrl', 'HttpRemoveUrl', 'HttpReceiveHttpRequest',
    'HttpSendHttpResponse', 'HttpReceiveRequestEntityBody',
    'HttpSendResponseEntityBody',
    'HttpSetServiceConfiguration', 'HttpDeleteServiceConfiguration',
    'HttpFlushResponseCache',
    'HttpCancelHttpRequest',
    'HttpCreateServerSession', 'HttpCloseServerSession',
    'HttpCreateRequestQueue',
    'HttpSetServerSessionProperty', 'HttpQueryServerSessionProperty',
    'HttpCreateUrlGroup', 'HttpCloseUrlGroup',
    'HttpAddUrlToUrlGroup', 'HttpRemoveUrlFromUrlGroup',
    'HttpSetUrlGroupProperty', 'HttpQueryUrlGroupProperty',
    'HttpSetRequestQueueProperty', 'HttpQueryRequestQueueProperty'
    );

function RegURL(aRoot, aPort: SockString; Https: boolean;
  aDomainName: SockString): SynUnicode;
const Prefix: array[boolean] of SockString = ('http://','https://');
begin
  if aPort='' then
    aPort := DEFAULT_PORT[Https];
  aRoot := trim(aRoot);
  aDomainName := trim(aDomainName);
  if aDomainName='' then begin
    result := '';
    exit;
  end;
  if aRoot<>'' then begin
    if aRoot[1]<>'/' then
      insert('/',aRoot,1);
    if aRoot[length(aRoot)]<>'/' then
      aRoot := aRoot+'/';
  end else
    aRoot := '/'; // allow for instance 'http://*:2869/'
  aRoot := Prefix[Https]+aDomainName+':'+aPort+aRoot;
  result := SynUnicode(aRoot);
end;

const
  HTTPAPI_DLL = 'httpapi.dll';

procedure HttpApiInitialize;
var api: THttpAPIs;
    P: PPointer;
begin
  if Http.Module<>0 then
    exit; // already loaded
  try
    Http.Module := LoadLibrary(HTTPAPI_DLL);
    Http.Version.MajorVersion := 2; // API 2.0 if all functions are available
    if Http.Module<=255 then
      raise ECrtSocket.CreateFmt('Unable to find %s',[HTTPAPI_DLL]);
    P := @@Http.Initialize;
    for api := low(api) to high(api) do begin
      P^ := GetProcAddress(Http.Module,HttpNames[api]);
      if P^=nil then
        if api<hHttpApi2First then
          raise ECrtSocket.CreateFmt('Unable to find %s() in %s',[HttpNames[api],HTTPAPI_DLL]) else
          Http.Version.MajorVersion := 1; // e.g. Windows XP or Server 2003
      inc(P);
    end;
  except
    on E: Exception do begin
      if Http.Module>255 then begin
        FreeLibrary(Http.Module);
        Http.Module := 0;
      end;
      raise;
    end;
  end;
end;


{ EHttpApiServer }

type
  EHttpApiServer = class(ECrtSocket)
  protected
    fLastApi: THttpAPIs;
  public
    class procedure RaiseOnError(api: THttpAPIs; Error: integer);
    constructor Create(api: THttpAPIs; Error: integer); reintroduce;
  published
    property LastApi: THttpAPIs read fLastApi;
  end;

class procedure EHttpApiServer.RaiseOnError(api: THttpAPIs; Error: integer);
begin
  if Error<>NO_ERROR then
    raise self.Create(api,Error);
end;

constructor EHttpApiServer.Create(api: THttpAPIs; Error: integer);
begin
  fLastError := Error;
  fLastApi := api;
  inherited CreateFmt('%s failed: %s (%d)',
    [HttpNames[api],SysErrorMessagePerModule(Error,HTTPAPI_DLL),Error])
end;


{ THttpApiServer }

function THttpApiServer.AddUrl(const aRoot, aPort: SockString; Https: boolean;
  const aDomainName: SockString; aRegisterURI: boolean; aContext: Int64): integer;
var uri: SynUnicode;
    n: integer;
begin
  result := -1;
  if (Self=nil) or (fReqQueue=0) or (Http.Module=0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri='' then
    exit; // invalid parameters
  if aRegisterURI then
    AddUrlAuthorize(aRoot,aPort,Https,aDomainName);
  if Http.Version.MajorVersion>1 then
    result := Http.AddUrlToUrlGroup(fUrlGroupID,pointer(uri),aContext) else
    result := Http.AddUrl(fReqQueue,pointer(uri));
  if result=NO_ERROR then begin
    n := length(fRegisteredUnicodeUrl);
    SetLength(fRegisteredUnicodeUrl,n+1);
    fRegisteredUnicodeUrl[n] := uri;
  end;
end;

function THttpApiServer.RemoveUrl(const aRoot, aPort: SockString; Https: boolean;
  const aDomainName: SockString): integer;
var uri: SynUnicode;
    i,j,n: integer;
begin
  result := -1;
  if (Self=nil) or (fReqQueue=0) or (Http.Module=0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri='' then
    exit; // invalid parameters
  n := High(fRegisteredUnicodeUrl);
  for i := 0 to n do
    if fRegisteredUnicodeUrl[i]=uri then begin
      if Http.Version.MajorVersion>1 then
        result := Http.RemoveUrlFromUrlGroup(fUrlGroupID,pointer(uri),0) else
        result := Http.RemoveUrl(fReqQueue,pointer(uri));
      if result<>0 then
        exit; // shall be handled by caller
      for j := i to n-1 do
        fRegisteredUnicodeUrl[j] := fRegisteredUnicodeUrl[j+1];
      SetLength(fRegisteredUnicodeUrl,n);
      exit;
    end;
end;

class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: SockString;
  Https: boolean; const aDomainName: SockString; OnlyDelete: boolean): string;
const
  /// will allow AddUrl() registration to everyone
  // - 'GA' (GENERIC_ALL) to grant all access
  // - 'S-1-1-0'	defines a group that includes all users
  HTTPADDURLSECDESC: PWideChar = 'D:(A;;GA;;;S-1-1-0)';
var prefix: SynUnicode;
    Error: HRESULT;
    Config: HTTP_SERVICE_CONFIG_URLACL_SET;
begin
  try
    HttpApiInitialize;
    prefix := RegURL(aRoot, aPort, Https, aDomainName);
    if prefix='' then
      result := 'Invalid parameters' else begin
      EHttpApiServer.RaiseOnError(hInitialize,Http.Initialize(
        Http.Version,HTTP_INITIALIZE_CONFIG));
      try
        fillchar(Config,sizeof(Config),0);
        Config.KeyDesc.pUrlPrefix := pointer(prefix);
        // first delete any existing information
        Error := Http.DeleteServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        // then add authorization rule
        if not OnlyDelete then begin
          Config.KeyDesc.pUrlPrefix := pointer(prefix);
          Config.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
          Error := Http.SetServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        end;
        if (Error<>NO_ERROR) and (Error<>ERROR_ALREADY_EXISTS) then
          raise EHttpApiServer.Create(hSetServiceConfiguration,Error);
        result := ''; // success
      finally
        Http.Terminate(HTTP_INITIALIZE_CONFIG);
      end;
    end;
  except
    on E: Exception do
      result := E.Message;
  end;
end;

type
  THttpApiServerClass = class of THttpApiServer;

procedure THttpApiServer.Clone(ChildThreadCount: integer);
var i: integer;
begin
  if (fReqQueue=0) or not Assigned(OnRequest) or (ChildThreadCount<=0) then
    exit; // nothing to clone (need a queue and a process event)
  if ChildThreadCount>256 then
    ChildThreadCount := 256; // not worth adding
  for i := 1 to ChildThreadCount do
    fClones.Add(THttpApiServerClass(Self.ClassType).CreateClone(self));
end;

function THttpApiServer.GetAPIVersion: string;
begin
  result := Format('HTTP API %d.%d',[Http.Version.MajorVersion,Http.Version.MinorVersion]);
end;

procedure THttpApiServer.SetOnTerminate(const Event: TNotifyThreadEvent);
var i: integer;
begin
  inherited SetOnTerminate(Event);
  if (Clones<>nil) and (fOwner=nil) then
    for i := 0 to Clones.Count-1 do
      THttpApiServer(Clones[i]).OnHttpThreadTerminate := Event;
end;

constructor THttpApiServer.Create(CreateSuspended: boolean; QueueName: SynUnicode;
  OnStart,OnStop: TNotifyThreadEvent; const ProcessName: SockString);
var bindInfo: HTTP_BINDING_INFO;
begin
  SetLength(fLogDataStorage,sizeof(HTTP_LOG_FIELDS_DATA)); // should be done 1st
  inherited Create(true,OnStart,OnStop,ProcessName);
  HttpApiInitialize; // will raise an exception in case of failure
  EHttpApiServer.RaiseOnError(hInitialize,
    Http.Initialize(Http.Version,HTTP_INITIALIZE_SERVER));
  if Http.Version.MajorVersion>1 then begin
    EHttpApiServer.RaiseOnError(hCreateServerSession,Http.CreateServerSession(
      Http.Version,fServerSessionID));
    EHttpApiServer.RaiseOnError(hCreateUrlGroup,Http.CreateUrlGroup(
      fServerSessionID,fUrlGroupID));
    if QueueName='' then
      BinToHexDisplayW(@fServerSessionID,SizeOf(fServerSessionID),QueueName);
    EHttpApiServer.RaiseOnError(hCreateRequestQueue,Http.CreateRequestQueue(
      Http.Version,pointer(QueueName),nil,0,fReqQueue));
    bindInfo.Flags := 1;
    bindInfo.RequestQueueHandle := FReqQueue;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,Http.SetUrlGroupProperty(
      fUrlGroupID,HttpServerBindingProperty,@bindInfo,SizeOf(bindInfo)));
  end else
    EHttpApiServer.RaiseOnError(hCreateHttpHandle,Http.CreateHttpHandle(fReqQueue));
  fClones := TObjectList.Create;
  fReceiveBufferSize := 1048576; // i.e. 1 MB
  if not CreateSuspended then
    Suspended := False;
end;

constructor THttpApiServer.CreateClone(From: THttpApiServer);
begin
  SetLength(fLogDataStorage,sizeof(HTTP_LOG_FIELDS_DATA));
  inherited Create(false,From.fOnHttpThreadStart,From.fOnTerminate,From.ProcessName);
  fOwner := From;
  fReqQueue := From.fReqQueue;
  fOnRequest := From.fOnRequest;
  fOnBeforeBody := From.fOnBeforeBody;
  fCanNotifyCallback := From.fCanNotifyCallback;
  fCompress := From.fCompress;
  fCompressAcceptEncoding := From.fCompressAcceptEncoding;
  fReceiveBufferSize := From.fReceiveBufferSize;
  if From.fLogData<>nil then
    fLogData := pointer(fLogDataStorage);
  SetServerName(From.fServerName);
  fLoggingServiceName := From.fLoggingServiceName;
end;

procedure THttpApiServer.DestroyMainThread;
var i: integer;
begin
  if fReqQueue<>0 then begin
    if Http.Version.MajorVersion>1 then begin
     if fUrlGroupID<>0 then begin
       Http.RemoveUrlFromUrlGroup(fUrlGroupID,nil,HTTP_URL_FLAG_REMOVE_ALL);
       Http.CloseUrlGroup(fUrlGroupID);
       fUrlGroupID := 0;
     end;
     CloseHandle(FReqQueue);
     if fServerSessionID<>0 then begin
       Http.CloseServerSession(fServerSessionID);
       fServerSessionID := 0;
     end;
    end else begin
      for i := 0 to high(fRegisteredUnicodeUrl) do
        Http.RemoveUrl(fReqQueue,pointer(fRegisteredUnicodeUrl[i]));
      CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
    end;
    fReqQueue := 0;
    FreeAndNil(fClones);
    Http.Terminate(HTTP_INITIALIZE_SERVER);
  end;
end;

destructor THttpApiServer.Destroy;
{$ifdef LVCL}
var i: integer;
{$endif}
begin
  {$ifdef LVCL}
  Terminate; // for Execute to be notified about end of process
  {$endif}
  try
    if (fClones<>nil) and (Http.Module<>0) then // fClones=nil for clone threads
      DestroyMainThread;
    {$ifdef LVCL}
    for i := 1 to 500 do
      if fExecuteFinished then
        break else
        SleepHiRes(10);
    {$endif}
  finally
    inherited Destroy;
  end;
end;

procedure GetDomainUserNameFromToken(UserToken: THandle; var result: SockString);
var Buffer: array[0..511] of byte;
    BufferSize, UserSize, DomainSize: DWORD;
    UserInfo: PSIDAndAttributes;
    NameUse: {$ifdef FPC}SID_NAME_USE{$else}Cardinal{$endif};
    tmp: SynUnicode;
    P: PWideChar;
begin
   if not GetTokenInformation(UserToken,TokenUser,@Buffer,SizeOf(Buffer),BufferSize) then
     exit;
   UserInfo := @Buffer;
   UserSize := 0;
   DomainSize := 0;
   LookupAccountSidW(nil,UserInfo^.Sid,nil,UserSize,nil,DomainSize,NameUse);
   if (UserSize=0) or (DomainSize=0) then
     exit;
   SetLength(tmp,UserSize+DomainSize-1);
   P := pointer(tmp);
   if not LookupAccountSidW(nil,UserInfo^.Sid,P+DomainSize,UserSize,P,DomainSize,NameUse) then
     exit;
   P[DomainSize] := '\';
   result := {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(tmp);
end;

procedure THttpApiServer.DoAfterSentResponse(Ctxt: THttpServerRequest);
begin
  if Assigned(fOnAfterResponseSent) then
    fOnAfterResponseSent(Ctxt);
end;

function THttpApiServer.GetSendResponseFlags(Ctxt: THttpServerRequest): Integer;
begin
  result := 0;
end;

procedure AppendI64(value: Int64; var dest: shortstring);
var temp: shortstring;
begin
  str(value,temp);
  dest := dest+temp;
end;

procedure AppendChar(chr: AnsiChar; var dest: shortstring);
begin
  inc(dest[0]);
  dest[ord(dest[0])] := chr;
end;

procedure THttpApiServer.Execute;
type
  TVerbText = array[hvOPTIONS..pred(hvMaximum)] of SockString;
const
  VERB_TEXT: TVerbText = (
    'OPTIONS','GET','HEAD','POST','PUT','DELETE','TRACE','CONNECT','TRACK',
    'MOVE','COPY','PROPFIND','PROPPATCH','MKCOL','LOCK','UNLOCK','SEARCH');
var Req: PHTTP_REQUEST;
    ReqID: HTTP_REQUEST_ID;
    ReqBuf, RespBuf, RemoteIP: SockString;
    ContentRange: shortstring;
    i: integer;
    flags, bytesRead, bytesSent: cardinal;
    Err: HRESULT;
    InCompressAccept: THttpSocketCompressSet;
    InContentLength, InContentLengthChunk, InContentLengthRead: cardinal;
    InContentEncoding, InAcceptEncoding, Range: SockString;
    OutContentEncoding, OutStatus: SockString;
    Context: THttpServerRequest;
    FileHandle: THandle;
    Resp: PHTTP_RESPONSE;
    BufRead, R: PAnsiChar;
    Heads: HTTP_UNKNOWN_HEADERs;
    RangeStart, RangeLength: ULONGLONG;
    OutContentLength: ULARGE_INTEGER;
    DataChunkInMemory: HTTP_DATA_CHUNK_INMEMORY;
    DataChunkFile: HTTP_DATA_CHUNK_FILEHANDLE;
    CurrentLog: PHTTP_LOG_FIELDS_DATA;
    Verbs: TVerbText; // to avoid memory allocation

  procedure SendError(StatusCode: cardinal; const ErrorMsg: string; E: Exception=nil);
  var Msg: string;
  begin
    try
      Resp^.SetStatus(StatusCode,OutStatus);
      CurrentLog^.ProtocolStatus := StatusCode;
      Msg := format(
        '<html><body style="font-family:verdana;"><h1>Server Error %d: %s</h1><p>',
        [StatusCode,OutStatus]);
      if E<>nil then
        Msg := Msg+string(E.ClassName)+' Exception raised:<br>';
      Resp^.SetContent(DataChunkInMemory,UTF8String(Msg)+HtmlEncode(
        {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(ErrorMsg))
        {$ifndef NOXPOWEREDNAME}+'</p><p><small>'+XPOWEREDVALUE{$endif},
        'text/html; charset=utf-8');
      Http.SendHttpResponse(fReqQueue,
        Req^.RequestId,0,Resp^,nil,bytesSent,nil,0,nil,fLogData);
    except
      on Exception do
        ; // ignore any HttpApi level errors here (client may crashed)
    end;
  end;

begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  NotifyThreadStart(self);
  // reserve working buffers
  SetLength(Heads,64);
  SetLength(RespBuf,sizeof(Resp^));
  Resp := pointer(RespBuf);
  SetLength(ReqBuf,16384+sizeof(HTTP_REQUEST)); // space for Req^ + 16 KB of headers
  Req := pointer(ReqBuf);
  CurrentLog := pointer(fLogDataStorage);
  Verbs := VERB_TEXT;
  Context := THttpServerRequest.Create(self,0,self);
  try
    // main loop
    ReqID := 0;
    Context.fServer := self;
    repeat
      Context.fInContent := ''; // release input/output body buffers ASAP
      Context.fOutContent := '';
      // retrieve next pending request, and read its headers
      fillchar(Req^,sizeof(HTTP_REQUEST),0);
      Err := Http.ReceiveHttpRequest(fReqQueue,ReqID,0,Req^,length(ReqBuf),bytesRead);
      if Terminated then
        break;
      case Err of
      NO_ERROR:
      try
        // parse method and headers
        Context.fConnectionID := Req^.ConnectionId;
        Context.fHttpApiRequest := Req;
        Context.fURL := Req^.pRawUrl;
        if Req^.Verb in [low(Verbs)..high(Verbs)] then
          Context.fMethod := Verbs[Req^.Verb] else
          SetString(Context.fMethod,Req^.pUnknownVerb,Req^.UnknownVerbLength);
        with Req^.Headers.KnownHeaders[reqContentType] do
          SetString(Context.fInContentType,pRawValue,RawValueLength);
        with Req^.Headers.KnownHeaders[reqAcceptEncoding] do
          SetString(InAcceptEncoding,pRawValue,RawValueLength);
        InCompressAccept := ComputeContentEncoding(fCompress,pointer(InAcceptEncoding));
        Context.fUseSSL := Req^.pSslInfo<>nil;
        Context.fInHeaders := RetrieveHeaders(Req^,fRemoteIPHeaderUpper,RemoteIP);
        // retrieve any SetAuthenticationSchemes() information
        if byte(fAuthenticationSchemes)<>0 then // set only with HTTP API 2.0
          for i := 0 to Req^.RequestInfoCount-1 do
          if Req^.pRequestInfo^[i].InfoType=HttpRequestInfoTypeAuth then
            with PHTTP_REQUEST_AUTH_INFO(Req^.pRequestInfo^[i].pInfo)^ do
            case AuthStatus of
            HttpAuthStatusSuccess:
            if AuthType>HttpRequestAuthTypeNone then begin
              byte(Context.fAuthenticationStatus) := ord(AuthType)+1;
              if AccessToken<>0 then
                GetDomainUserNameFromToken(AccessToken,Context.fAuthenticatedUser);
            end;
            HttpAuthStatusFailure:
              Context.fAuthenticationStatus := hraFailed;
            end;
        // retrieve body
        if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and Req^.Flags<>0 then begin
          with Req^.Headers.KnownHeaders[reqContentLength] do
            InContentLength := GetCardinal(pRawValue,pRawValue+RawValueLength);
          with Req^.Headers.KnownHeaders[reqContentEncoding] do
            SetString(InContentEncoding,pRawValue,RawValueLength);
          if (InContentLength>0) and (MaximumAllowedContentLength>0) and
             (InContentLength>MaximumAllowedContentLength) then begin
            SendError(STATUS_PAYLOADTOOLARGE,'Rejected');
            continue;
          end;
          if Assigned(OnBeforeBody) then begin
            with Context do
              Err := OnBeforeBody(URL,Method,InHeaders,InContentType,RemoteIP,InContentLength,fUseSSL);
            if Err<>STATUS_SUCCESS then begin
              SendError(Err,'Rejected');
              continue;
            end;
          end;
          if InContentLength<>0 then begin
            SetLength(Context.fInContent,InContentLength);
            BufRead := pointer(Context.InContent);
            InContentLengthRead := 0;
            repeat
              BytesRead := 0;
              if Http.Version.MajorVersion>1 then // speed optimization for Vista+
                flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER else
                flags := 0;
              InContentLengthChunk := InContentLength-InContentLengthRead;
              if (fReceiveBufferSize>=1024) and (InContentLengthChunk>fReceiveBufferSize) then
                InContentLengthChunk := fReceiveBufferSize;
              Err := Http.ReceiveRequestEntityBody(fReqQueue,Req^.RequestId,flags,
                BufRead,InContentLengthChunk,BytesRead);
              if Terminated then
                exit;
              inc(InContentLengthRead,BytesRead);
              if Err=ERROR_HANDLE_EOF then begin
                if InContentLengthRead<InContentLength then
                  SetLength(Context.fInContent,InContentLengthRead);
                Err := NO_ERROR;
                break; // should loop until returns ERROR_HANDLE_EOF
              end;
              if Err<>NO_ERROR then
                break;
              inc(BufRead,BytesRead);
            until InContentLengthRead=InContentLength;
            if Err<>NO_ERROR then begin
              SendError(406,SysErrorMessagePerModule(Err,HTTPAPI_DLL));
              continue;
            end;
            if InContentEncoding<>'' then
              for i := 0 to high(fCompress) do
                if fCompress[i].Name=InContentEncoding then begin
                  fCompress[i].Func(Context.fInContent,false); // uncompress
                  break;
                end;
          end;
        end;
        try
          // compute response
          Context.OutContent := '';
          Context.OutContentType := '';
          Context.OutCustomHeaders := '';
          fillchar(Resp^,sizeof(Resp^),0);
          Resp^.SetStatus(Request(Context),OutStatus);
          if Terminated then
            exit;
          // update log information
          if Http.Version.MajorVersion>=2 then
            with Req^,CurrentLog^ do begin
              MethodNum := Verb;
              UriStemLength := CookedUrl.AbsPathLength;
              UriStem := CookedUrl.pAbsPath;
              with Headers.KnownHeaders[reqUserAgent] do begin
                UserAgentLength := RawValueLength;
                UserAgent := pRawValue;
              end;
              with Headers.KnownHeaders[reqHost] do begin
                HostLength := RawValueLength;
                Host := pRawValue;
              end;
              with Headers.KnownHeaders[reqReferrer] do begin
                ReferrerLength := RawValueLength;
                Referrer := pRawValue;
              end;
              ProtocolStatus := Resp^.StatusCode;
              ClientIp := pointer(RemoteIP);
              ClientIpLength := length(RemoteIP);
              Method := pointer(Context.fMethod);
              MethodLength := length(Context.fMethod);
              UserName := pointer(Context.fAuthenticatedUser);
              UserNameLength := Length(Context.fAuthenticatedUser);
            end;
          // send response
          Resp^.Version := Req^.Version;
          Resp^.SetHeaders(pointer(Context.OutCustomHeaders),Heads);
          if fCompressAcceptEncoding<>'' then
            Resp^.AddCustomHeader(pointer(fCompressAcceptEncoding),Heads,false);
          with Resp^.Headers.KnownHeaders[respServer] do begin
            pRawValue := pointer(fServerName);
            RawValueLength := length(fServerName);
          end;
          if Context.OutContentType=HTTP_RESP_STATICFILE then begin
            // response is file -> OutContent is UTF-8 file name to be served
            FileHandle := FileOpen(
              {$ifdef UNICODE}UTF8ToUnicodeString{$else}Utf8ToAnsi{$endif}(Context.OutContent),
              fmOpenRead or fmShareDenyNone);
            if PtrInt(FileHandle)<0 then begin
              SendError(STATUS_NOTFOUND,SysErrorMessage(GetLastError));
              continue;
            end;
            try // http.sys will serve then close the file from kernel
              DataChunkFile.DataChunkType := hctFromFileHandle;
              DataChunkFile.FileHandle := FileHandle;
              flags := 0;
              DataChunkFile.ByteRange.StartingOffset.QuadPart := 0;
              Int64(DataChunkFile.ByteRange.Length.QuadPart) := -1; // to eof
              with Req^.Headers.KnownHeaders[reqRange] do begin
                if (RawValueLength>6) and IdemPChar(pRawValue,'BYTES=') and
                   (pRawValue[6] in ['0'..'9']) then begin
                  SetString(Range,pRawValue+6,RawValueLength-6); // need #0 end
                  R := pointer(Range);
                  RangeStart := GetNextItemUInt64(R);
                  if R^='-' then begin
                    OutContentLength.LowPart := GetFileSize(FileHandle,@OutContentLength.HighPart);
                    DataChunkFile.ByteRange.Length.QuadPart := OutContentLength.QuadPart-RangeStart;
                    inc(R);
                    flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
                    DataChunkFile.ByteRange.StartingOffset.QuadPart := RangeStart;
                    if R^ in ['0'..'9'] then begin
                      RangeLength := GetNextItemUInt64(R)-RangeStart+1;
                      if RangeLength<DataChunkFile.ByteRange.Length.QuadPart then
                        // "bytes=0-499" -> start=0, len=500
                        DataChunkFile.ByteRange.Length.QuadPart := RangeLength;
                    end; // "bytes=1000-" -> start=1000, to eof)
                    ContentRange := 'Content-Range: bytes ';
                    AppendI64(RangeStart,ContentRange);
                    AppendChar('-',ContentRange);
                    AppendI64(RangeStart+DataChunkFile.ByteRange.Length.QuadPart-1,ContentRange);
                    AppendChar('/',ContentRange);
                    AppendI64(OutContentLength.QuadPart,ContentRange);
                    AppendChar(#0,ContentRange);
                    Resp^.AddCustomHeader(@ContentRange[1],Heads,false);
                    Resp^.SetStatus(STATUS_PARTIALCONTENT,OutStatus);
                  end;
                end;
                with Resp^.Headers.KnownHeaders[respAcceptRanges] do begin
                   pRawValue := 'bytes';
                   RawValueLength := 5;
                end;
              end;
              Resp^.EntityChunkCount := 1;
              Resp^.pEntityChunks := @DataChunkFile;
              Http.SendHttpResponse(fReqQueue,
                Req^.RequestId,flags,Resp^,nil,bytesSent,nil,0,nil,fLogData);
            finally
              FileClose(FileHandle);
            end;
          end else begin
            // response is in OutContent -> send it from memory
            if Context.OutContentType=HTTP_RESP_NORESPONSE then
              Context.OutContentType := ''; // true HTTP always expects a response
            if fCompress<>nil then begin
              with Resp^.Headers.KnownHeaders[reqContentEncoding] do
              if RawValueLength=0 then begin
                // no previous encoding -> try if any compression
                OutContentEncoding := CompressDataAndGetHeaders(InCompressAccept,
                  fCompress,Context.OutContentType,Context.fOutContent);
                pRawValue := pointer(OutContentEncoding);
                RawValueLength := length(OutContentEncoding);
              end;
            end;
            Resp^.SetContent(DataChunkInMemory,Context.OutContent,Context.OutContentType);
            EHttpApiServer.RaiseOnError(hSendHttpResponse,Http.SendHttpResponse(
              fReqQueue,Req^.RequestId,getSendResponseFlags(Context),
              Resp^,nil,bytesSent,nil,0,nil,fLogData));
          end;
          DoAfterSentResponse(Context);
        except
          on E: Exception do
            // handle any exception raised during process: show must go on!
            if not E.InheritsFrom(EHttpApiServer) or // ensure still connected
               (EHttpApiServer(E).LastError<>HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
              SendError(STATUS_SERVERERROR,E.Message,E);
        end;
      finally
        ReqId := 0; // reset Request ID to handle the next pending request
      end;
      ERROR_MORE_DATA: begin
        // input buffer was too small to hold the request headers
        // -> increase buffer size and call the API again
        ReqID := Req^.RequestId;
        SetLength(ReqBuf,bytesRead);
        Req := pointer(ReqBuf);
      end;
      ERROR_CONNECTION_INVALID:
        if ReqID=0 then
          break else
          // TCP connection was corrupted by the peer -> ignore + next request
          ReqID := 0;
      else break; // unhandled Err value
      end;
    until Terminated;
  finally
    Context.Free;
    fExecuteFinished := true;
  end;
end;

procedure THttpApiServer.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer=1024);
var i: integer;
begin
  inherited;
  if fClones<>nil then
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).
        RegisterCompress(aFunction,aCompressMinSize);
end;

function THttpApiServer.GetHTTPQueueLength: Cardinal;
var returnLength: ULONG;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then
    result := 0 else begin
    if fOwner<>nil then
      self := fOwner;
    if fReqQueue=0 then
      result := 0 else
      EHttpApiServer.RaiseOnError(hQueryRequestQueueProperty,
        Http.QueryRequestQueueProperty(fReqQueue,HttpServerQueueLengthProperty,
          @Result, sizeof(Result), 0, @returnLength, nil));
  end;
end;

procedure THttpApiServer.SetHTTPQueueLength(aValue: Cardinal);
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetRequestQueueProperty, ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fReqQueue<>0) then
    EHttpApiServer.RaiseOnError(hSetRequestQueueProperty,
      Http.SetRequestQueueProperty(fReqQueue,HttpServerQueueLengthProperty,
        @aValue, sizeof(aValue), 0, nil));
end;

function THttpApiServer.GetRegisteredUrl: SynUnicode;
var i: integer;
begin
  if fRegisteredUnicodeUrl=nil then
    result := '' else
    result := fRegisteredUnicodeUrl[0];
  for i := 1 to high(fRegisteredUnicodeUrl) do
    result := result+','+fRegisteredUnicodeUrl[i];
end;

function THttpApiServer.GetCloned: boolean;
begin
  result := (fOwner<>nil);
end;

procedure THttpApiServer.SetMaxBandwidth(aValue: Cardinal);
var qosInfo: HTTP_QOS_SETTING_INFO;
    limitInfo: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fUrlGroupID<>0) then begin
    if AValue=0 then
      limitInfo.MaxBandwidth := HTTP_LIMIT_INFINITE else
    if AValue<HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
      limitInfo.MaxBandwidth := HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE else
      limitInfo.MaxBandwidth := aValue;
    limitInfo.Flags := 1;
    qosInfo.QosType := HttpQosSettingTypeBandwidth;
    qosInfo.QosSetting := @limitInfo;
    EHttpApiServer.RaiseOnError(hSetServerSessionProperty,
      Http.SetServerSessionProperty(fServerSessionID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
  end;
end;

function THttpApiServer.GetMaxBandwidth: Cardinal;
var qosInfoGet: record
      qosInfo: HTTP_QOS_SETTING_INFO;
      limitInfo: HTTP_BANDWIDTH_LIMIT_INFO;
    end;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then begin
    result := 0;
    exit;
  end;
  if fOwner<>nil then
    self := fOwner;
  if fUrlGroupID=0 then begin
    result := 0;
    exit;
  end;
  qosInfoGet.qosInfo.QosType := HttpQosSettingTypeBandwidth;
  qosInfoGet.qosInfo.QosSetting := @qosInfoGet.limitInfo;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @qosInfoGet, SizeOf(qosInfoGet)));
  Result := qosInfoGet.limitInfo.MaxBandwidth;
end;

function THttpApiServer.GetMaxConnections: Cardinal;
var qosInfoGet: record
      qosInfo: HTTP_QOS_SETTING_INFO;
      limitInfo: HTTP_CONNECTION_LIMIT_INFO;
    end;
    returnLength: ULONG;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then begin
    result := 0;
    exit;
  end;
  if fOwner<>nil then
    self := fOwner;
  if fUrlGroupID=0 then begin
    result := 0;
    exit;
  end;
  qosInfoGet.qosInfo.QosType := HttpQosSettingTypeConnectionLimit;
  qosInfoGet.qosInfo.QosSetting := @qosInfoGet.limitInfo;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @qosInfoGet, SizeOf(qosInfoGet), @returnLength));
  Result := qosInfoGet.limitInfo.MaxConnections;
end;

procedure THttpApiServer.SetMaxConnections(aValue: Cardinal);
var qosInfo: HTTP_QOS_SETTING_INFO;
    limitInfo: HTTP_CONNECTION_LIMIT_INFO;
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fUrlGroupID<>0) then begin
    if AValue = 0 then
      limitInfo.MaxConnections := HTTP_LIMIT_INFINITE else
      limitInfo.MaxConnections := aValue;
    limitInfo.Flags := 1;
    qosInfo.QosType := HttpQosSettingTypeConnectionLimit;
    qosInfo.QosSetting := @limitInfo;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
  end;
end;

function THttpApiServer.HasAPI2: boolean;
begin
  result := Http.Version.MajorVersion>=2;
end;

function THttpApiServer.GetLogging: boolean;
begin
  result := (fLogData<>nil);
end;

procedure THttpApiServer.LogStart(const aLogFolder: TFileName;
  aType: THttpApiLoggingType; const aSoftwareName: TFileName;
  aRolloverType: THttpApiLoggingRollOver; aRolloverSize: cardinal;
  aLogFields: THttpApiLogFields; aFlags: THttpApiLoggingFlags);
var logInfo : HTTP_LOGGING_INFO;
    folder,software: SynUnicode;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  fLogData := nil; // disable any previous logging
  fillchar(logInfo,SizeOf(logInfo),0);
  logInfo.Flags := 1;
  logInfo.LoggingFlags := byte(aFlags);
  if aLogFolder='' then
    raise EHttpApiServer.CreateFmt('LogStart(aLogFolder="")',[]);
  if length(aLogFolder)>212 then
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532
    raise EHttpApiServer.CreateFmt('aLogFolder is too long for LogStart(%s)',[aLogFolder]);
  folder := SynUnicode(aLogFolder);
  software := SynUnicode(aSoftwareName);
  logInfo.SoftwareNameLength := length(software)*2;
  logInfo.SoftwareName := pointer(software);
  logInfo.DirectoryNameLength := length(folder)*2;
  logInfo.DirectoryName := pointer(folder);
  logInfo.Format := HTTP_LOGGING_TYPE(aType);
  if aType=hltNCSA then
    aLogFields := [hlfDate..hlfSubStatus];
  logInfo.Fields := integer(aLogFields);
  logInfo.RolloverType := HTTP_LOGGING_ROLLOVER_TYPE(aRolloverType);
  if aRolloverType=hlrSize then
    logInfo.RolloverSize := aRolloverSize;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerLoggingProperty,
      @logInfo, SizeOf(logInfo)));
  // on success, update the actual log memory structure
  fLogData := pointer(fLogDataStorage);
end;

procedure THttpApiServer.LogStop;
var i: integer;
begin
  if (self=nil) or (fClones=nil) or (fLogData=nil) then
    exit;
  fLogData := nil;
  for i := 0 to fClones.Count-1 do
    THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).fLogData := nil;
end;

procedure THttpApiServer.SetReceiveBufferSize(Value: cardinal);
var i: integer;
begin
  fReceiveBufferSize := Value;
  if fClones<>nil then // parameter shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).fReceiveBufferSize := Value;
end;

procedure THttpApiServer.SetServerName(const aName: SockString);
var i: integer;
begin
  inherited SetServerName(aName);
  with PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^ do begin
    ServerName := pointer(aName);
    ServerNameLength := Length(aName);
  end;
  if fClones<>nil then // server name is shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).SetServerName(aName);
end;

procedure THttpApiServer.SetOnBeforeBody(const aEvent: TOnHttpServerBeforeBody);
var i: integer;
begin
  inherited SetOnBeforeBody(aEvent);
  if fClones<>nil then // event is shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).SetOnBeforeBody(aEvent);
end;

procedure THttpApiServer.SetMaximumAllowedContentLength(aMax: cardinal);
var i: integer;
begin
  inherited SetMaximumAllowedContentLength(aMax);
  if fClones<>nil then // event is shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).SetMaximumAllowedContentLength(aMax);
end;

procedure THttpApiServer.SetLoggingServiceName(const aName: SockString);
begin
  if self=nil then
    exit;
  fLoggingServiceName := aName;
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServiceNameLength := Length(fLoggingServiceName);
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServiceName := pointer(fLoggingServiceName);
end;

procedure THttpApiServer.SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
  const DomainName, Realm: SynUnicode);
var authInfo: HTTP_SERVER_AUTHENTICATION_INFO;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  fAuthenticationSchemes := schemes;
  FillChar(authInfo,SizeOf(authInfo),0);
  authInfo.Flags := 1;
  authInfo.AuthSchemes := byte(schemes);
  authInfo.ReceiveMutualAuth := true;
  if haBasic in schemes then
    with authInfo.BasicParams do begin
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  if haDigest in schemes then
    with authInfo.DigestParams do begin
      DomainNameLength := Length(DomainName);
      DomainName := pointer(DomainName);
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerAuthenticationProperty,
      @authInfo, SizeOf(authInfo)));
end;

procedure THttpApiServer.SetTimeOutLimits(aEntityBody, aDrainEntityBody,
  aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
var timeoutInfo: HTTP_TIMEOUT_LIMIT_INFO;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  FillChar(timeOutInfo,SizeOf(timeOutInfo),0);
  timeoutInfo.Flags := 1;
  timeoutInfo.EntityBody := aEntityBody;
  timeoutInfo.DrainEntityBody := aDrainEntityBody;
  timeoutInfo.RequestQueue := aRequestQueue;
  timeoutInfo.IdleConnection := aIdleConnection;
  timeoutInfo.HeaderWait := aHeaderWait;
  timeoutInfo.MinSendRate := aMinSendRate;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerTimeoutsProperty,
      @timeoutInfo, SizeOf(timeoutInfo)));
end;


type
  WEB_SOCKET_PROPERTY_TYPE = (
    WEB_SOCKET_RECEIVE_BUFFER_SIZE_PROPERTY_TYPE, //0
    WEB_SOCKET_SEND_BUFFER_SIZE_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_MASKING_PROPERTY_TYPE,
    WEB_SOCKET_ALLOCATED_BUFFER_PROPERTY_TYPE,
    WEB_SOCKET_DISABLE_UTF8_VERIFICATION_PROPERTY_TYPE,
    WEB_SOCKET_KEEPALIVE_INTERVAL_PROPERTY_TYPE,
    WEB_SOCKET_SUPPORTED_VERSIONS_PROPERTY_TYPE
  );
  WEB_SOCKET_ACTION_QUEUE = Cardinal;

  WEB_SOCKET_ACTION = (
    WEB_SOCKET_NO_ACTION, //0
    WEB_SOCKET_SEND_TO_NETWORK_ACTION,
    WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION,
    WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION,
    WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION
  );
  PWEB_SOCKET_ACTION = ^WEB_SOCKET_ACTION;

  WEB_SOCKET_PROPERTY = record
    PropType: WEB_SOCKET_PROPERTY_TYPE;
    pvValue: Pointer;
    ulValueSize: ULONG;
  end;
  PWEB_SOCKET_PROPERTY = ^WEB_SOCKET_PROPERTY;

  WEB_SOCKET_HTTP_HEADER = record
    pcName: PAnsiChar;
    ulNameLength: ULONG;
    pcValue: PAnsiChar;
    ulValueLength: ULONG;
  end;
  PWEB_SOCKET_HTTP_HEADER = ^WEB_SOCKET_HTTP_HEADER;
  WEB_SOCKET_HTTP_HEADER_ARR = array of WEB_SOCKET_HTTP_HEADER;

  PWEB_SOCKET_BUFFER_DATA = ^WEB_SOCKET_BUFFER_DATA;
  WEB_SOCKET_BUFFER_DATA = record
    pbBuffer: PBYTE;
    ulBufferLength: ULONG;
    Reserved1: Word;
  end;
  WEB_SOCKET_BUFFER_CLOSE_STATUS = record
    pbReason: PBYTE;
    ulReasonLength: ULONG;
    usStatus: WEB_SOCKET_CLOSE_STATUS;
  end;

  /// direct late-binding access to the WebSocket Protocol Component API functions
  TWebSocketAPI = packed record
    /// acces to the loaded library handle
    LibraryHandle: THandle;
    /// depends on Windows version
    WebSocketEnabled: Boolean;
    /// aborts a WebSocket session handle created by WebSocketCreateClientHandle
    // or WebSocketCreateServerHandle
    AbortHandle: procedure (hWebSocket: WEB_SOCKET_HANDLE); stdcall;
    /// begins the client-side handshake
    BeginClientHandshake: function (hWebSocket: WEB_SOCKET_HANDLE; pszSubprotocols: PAnsiChar;
      ulSubprotocolCount: ULONG; pszExtensions: PAnsiChar; ulExtensionCount: ULONG;
      const pInitialHeaders: PWEB_SOCKET_HTTP_HEADER; ulInitialHeaderCount: ULONG;
      out pAdditionalHeaders: PWEB_SOCKET_HTTP_HEADER; out pulAdditionalHeaderCount: ULONG): HRESULT; stdcall;
    /// begins the server-side handshake
    BeginServerHandshake: function (hWebSocket: WEB_SOCKET_HANDLE; pszSubprotocolSelected: PAnsiChar;
      pszExtensionSelected: PAnsiChar; ulExtensionSelectedCount: ULONG;
      const pRequestHeaders: PWEB_SOCKET_HTTP_HEADER;
      ulRequestHeaderCount: ULONG; out pResponseHeaders: PWEB_SOCKET_HTTP_HEADER;
      out pulResponseHeaderCount: ULONG): HRESULT; stdcall;
    /// completes an action started by WebSocketGetAction
    CompleteAction: function (hWebSocket: WEB_SOCKET_HANDLE;
      pvActionContext: Pointer; ulBytesTransferred: ULONG): HRESULT; stdcall;
    /// creates a client-side WebSocket session handle
    CreateClientHandle: function (const pProperties: PWEB_SOCKET_PROPERTY; ulPropertyCount: ULONG;
      out phWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// creates a server-side WebSocket session handle
    CreateServerHandle: function (const pProperties: PWEB_SOCKET_PROPERTY; ulPropertyCount: ULONG;
      out phWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// deletes a WebSocket session handle created by WebSocketCreateClientHandle
    // or WebSocketCreateServerHandle
    DeleteHandle: procedure (hWebSocket: WEB_SOCKET_HANDLE); stdcall;
    /// completes the client-side handshake
    EndClientHandshake: function (hWebSocket: WEB_SOCKET_HANDLE;
      const pResponseHeaders: PWEB_SOCKET_HTTP_HEADER;
      ulReponseHeaderCount: ULONG; var pulSelectedExtensions: ULONG;
      var pulSelectedExtensionCount: ULONG;
      var pulSelectedSubprotocol: ULONG): HRESULT; stdcall;
    /// completes the server-side handshake
    EndServerHandshake: function (hWebSocket: WEB_SOCKET_HANDLE): HRESULT; stdcall;
    /// returns an action from a call to WebSocketSend, WebSocketReceive or WebSocketCompleteAction
    GetAction: function (hWebSocket: WEB_SOCKET_HANDLE; eActionQueue: WEB_SOCKET_ACTION_QUEUE;
      pDataBuffers: Pointer {WEB_SOCKET_BUFFER_DATA}; var pulDataBufferCount: ULONG;
      var pAction: WEB_SOCKET_ACTION;
      var pBufferType: WEB_SOCKET_BUFFER_TYPE; var pvApplicationContext: Pointer;
      var pvActionContext: Pointer): HRESULT; stdcall;
    /// gets a single WebSocket property
    GetGlobalProperty: function (eType: WEB_SOCKET_PROPERTY_TYPE;
      pvValue: Pointer; var ulSize: ULONG): HRESULT ; stdcall;
    /// adds a receive operation to the protocol component operation queue
    Receive: function (hWebSocket: WEB_SOCKET_HANDLE; pBuffer: Pointer {PWEB_SOCKET_BUFFER_*};
      pvContext: Pointer): HRESULT; stdcall;
    /// adds a send operation to the protocol component operation queue
    Send: function (hWebSocket: WEB_SOCKET_HANDLE; BufferType: WEB_SOCKET_BUFFER_TYPE;
      pBuffer: Pointer {PWEB_SOCKET_BUFFER_*}; Context: Pointer): HRESULT; stdcall;
  end;

  /// identify each TWebSocketAPI late-binding API function
  TWebSocketAPIs = (hAbortHandle, hBeginClientHandshake, hBeginServerHandshake,
    hCompleteAction, hCreateClientHandle, hCreateServerHandle, hDeleteHandle,
    hEndClientHandshake, hEndServerHandshake, hGetAction, hGetGlobalProperty,
    hReceive, hSend
  );

const sProtocolHeader: SockString = 'SEC-WEBSOCKET-PROTOCOL';

function HttpSys2ToWebSocketHeaders(const aHttpHeaders: HTTP_REQUEST_HEADERS): WEB_SOCKET_HTTP_HEADER_ARR;
var headerCnt: Integer;
    i, idx: Integer;
    h : THttpHeader;
    p : PHTTP_UNKNOWN_HEADER;
const
   C_KNOWNHEADERS_NAME : array [reqCacheControl..reqUserAgent] of PAnsiChar = (
      'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma', 'Trailer',
      'Transfer-Encoding', 'Upgrade', 'Via', 'Warning', 'Allow', 'Content-Length',
      'Content-Type', 'Content-Encoding', 'Content-Language', 'Content-Location',
      'Content-MD5', 'Content-Range', 'Expires', 'Last-Modified', 'Accept',
      'Accept-Charset', 'Accept-Encoding', 'Accept-Language', 'Authorization',
      'Cookie', 'Expect', 'From', 'Host', 'If-Match', 'If-Modified-Since',
      'If-None-Match', 'If-Range', 'If-Unmodified-Since', 'Max-Forwards',
      'Proxy-Authorization', 'Referer', 'Range', 'TE', 'Translate', 'User-Agent');
begin
//  Assert(Low(C_KNOWNHEADERS_NAME) = Low(aHttpHeaders.KnownHeaders));
//  Assert(High(C_KNOWNHEADERS_NAME) = High(aHttpHeaders.KnownHeaders));
  headerCnt := 0;
  for h := Low(C_KNOWNHEADERS_NAME) to High(C_KNOWNHEADERS_NAME) do
    if aHttpHeaders.KnownHeaders[h].RawValueLength <> 0 then
      Inc(headerCnt);
  p := aHttpHeaders.pUnknownHeaders;
  if p<>nil then
    Inc(headerCnt, aHttpHeaders.UnknownHeaderCount);
  SetLength(Result, headerCnt);
  idx := 0;
  for h := Low(C_KNOWNHEADERS_NAME) to High(C_KNOWNHEADERS_NAME) do
    if aHttpHeaders.KnownHeaders[h].RawValueLength<>0 then begin
      Result[idx].pcName := C_KNOWNHEADERS_NAME[h];
      Result[idx].ulNameLength := Length(C_KNOWNHEADERS_NAME[h]);
      Result[idx].pcValue := aHttpHeaders.KnownHeaders[h].pRawValue;
      Result[idx].ulValueLength := aHttpHeaders.KnownHeaders[h].RawValueLength;
      Inc(idx);
    end;
  p := aHttpHeaders.pUnknownHeaders;
  if p<>nil then
    for i := 1 to aHttpHeaders.UnknownHeaderCount do begin
      Result[idx].pcName := p^.pName;
      Result[idx].ulNameLength := p^.NameLength;
      Result[idx].pcValue := p^.pRawValue;
      Result[idx].ulValueLength := p^.RawValueLength;
      Inc(idx);
      Inc(p);
    end;
end;

function WebSocketHeadersToSockString(const aHeaders: PWEB_SOCKET_HTTP_HEADER;
  const aHeadersCount: Integer): SockString;
var i: Integer;
    h: PWEB_SOCKET_HTTP_HEADER;
    len: Integer;
    d : PAnsiChar;
begin
  len := 0;
  h := aHeaders;
  for i := 1 to aHeadersCount do begin
    if h^.ulValueLength<>0 then
      Inc(len, h^.ulNameLength + h^.ulValueLength + 4);
    Inc(h);
  end;
  SetString(Result, nil, len);
  d := Pointer(Result);
  h := aHeaders;
  for i := 1 to aHeadersCount do begin
    if h^.ulValueLength<>0 then begin
      Move(h^.pcName^, d^, h^.ulNameLength);
      Inc(d, h^.ulNameLength);
      PWord(d)^ := Ord(':') + Ord(' ') shl 8;
      Inc(d, 2);
      Move(h^.pcValue^, d^, h^.ulValueLength);
      Inc(d, h^.ulValueLength);
      PWord(d)^ := 13 + 10 shl 8;
      Inc(d, 2);
    end;
    Inc(h);
  end;
  Assert(d - Pointer(Result) = len);
end;

const
  WEBSOCKET_DLL = 'websocket.dll';

  WebSocketNames: array [TWebSocketAPIs] of PChar = (
    'WebSocketAbortHandle',
    'WebSocketBeginClientHandshake',
    'WebSocketBeginServerHandshake',
    'WebSocketCompleteAction',
    'WebSocketCreateClientHandle',
    'WebSocketCreateServerHandle',
    'WebSocketDeleteHandle',
    'WebSocketEndClientHandshake',
    'WebSocketEndServerHandshake',
    'WebSocketGetAction',
    'WebSocketGetGlobalProperty',
    'WebSocketReceive',
    'WebSocketSend'
  );

  WEB_SOCKET_SEND_ACTION_QUEUE     = $1;
  WEB_SOCKET_RECEIVE_ACTION_QUEUE  = $2;
  WEB_SOCKET_ALL_ACTION_QUEUE      = WEB_SOCKET_SEND_ACTION_QUEUE or WEB_SOCKET_RECEIVE_ACTION_QUEUE;

  ///Context ID of WebSocket URI group
  WEB_SOCKET_URL_CONTEXT = 1;

var
  WebSocketAPI: TWebSocketAPI;

procedure WebSocketApiInitialize;
var api: TWebSocketAPIs;
    P: PPointer;
begin
  if WebSocketAPI.LibraryHandle<>0 then
    exit; // already loaded
  WebSocketAPI.WebSocketEnabled := false;
  WebSocketAPI.LibraryHandle := SafeLoadLibrary(WEBSOCKET_DLL);
  if WebSocketAPI.LibraryHandle=0 then
    exit;
  P := @@WebSocketAPI.AbortHandle;
  for api := low(api) to high(api) do begin
    P^ := GetProcAddress(WebSocketAPI.LibraryHandle,WebSocketNames[api]);
    if P^ = nil then begin
      FreeLibrary(WebSocketAPI.LibraryHandle);
      WebSocketAPI.LibraryHandle := 0;
      exit;
    end;
    inc(P);
  end;
  WebSocketAPI.WebSocketEnabled := true;
end;

function WinHTTP_WebSocketEnabled: boolean;
begin
  Result := WebSocketAPI.WebSocketEnabled;
end;


{ EWebSocketApi }

type
  EWebSocketApi = class(ECrtSocket)
  protected
    fLastApi: TWebSocketAPIs;
  public
    class procedure RaiseOnError(api: TWebSocketAPIs; Error: integer);
    constructor Create(api: TWebSocketAPIs; Error: integer); reintroduce;
  published
    property LastApi: TWebSocketAPIs read fLastApi;
  end;

class procedure EWebSocketApi.RaiseOnError(api: TWebSocketAPIs; Error: integer);
begin
  if Error<>NO_ERROR then
    raise self.Create(api,Error);
end;

constructor EWebSocketApi.Create(api: TWebSocketAPIs; Error: integer);
begin
  fLastError := Error;
  fLastApi := api;
  inherited CreateFmt('%s failed: %s (%d)',
    [WebSocketNames[api],SysErrorMessagePerModule(Error,WEBSOCKET_DLL),Error])
end;


{ THttpApiWebSocketServerProtocol }

const WebSocketConnectionCapacity = 1000;

function THttpApiWebSocketServerProtocol.AddConnection(aConn: PHttpApiWebSocketConnection): Integer;
var i: integer;
begin
  if fFirstEmptyConnectionIndex >= fConnectionsCapacity - 1 then begin
    Inc(fConnectionsCapacity, WebSocketConnectionCapacity);
    ReallocMem(fConnections, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
    Fillchar(fConnections^[fConnectionsCapacity - WebSocketConnectionCapacity], WebSocketConnectionCapacity * SizeOf(PHttpApiWebSocketConnection), 0);
  end;
  if fFirstEmptyConnectionIndex >= fConnectionsCount then
    fConnectionsCount := fFirstEmptyConnectionIndex + 1;
  fConnections[fFirstEmptyConnectionIndex] := aConn;
  Result := fFirstEmptyConnectionIndex;
  for i := fFirstEmptyConnectionIndex + 1 to fConnectionsCount do begin
    if fConnections[i] = nil then begin
      fFirstEmptyConnectionIndex := i;
      Break;
    end;
  end;
end;

function THttpApiWebSocketServerProtocol.Broadcast(
  aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer;
  aBufferSize: ULONG): boolean;
var i:  integer;
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fConnectionsCount - 1 do
    if Assigned(fConnections[i]) then
      fConnections[i].Send(aBufferType, aBuffer, aBufferSize);
  finally
    LeaveCriticalSection(fSafe);
  end;
  result := True;
end;

function THttpApiWebSocketServerProtocol.Close(index: Integer;
  aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG): boolean;
var conn: PHttpApiWebSocketConnection;
begin
  Result := false;
  if (index>=0) and (index<fConnectionsCount) then begin
    conn := fConnections^[index];
    if (conn<>nil) and (conn.fState = wsOpen) then begin
      conn.Close(aStatus, aBuffer, aBufferSize);
      result := True;
    end;
  end;
end;

constructor THttpApiWebSocketServerProtocol.Create(const aName: SockString;
  aManualFragmentManagement: Boolean;
  aServer: THttpApiWebSocketServer;
  aOnAccept: THttpApiWebSocketServerOnAcceptEvent;
  aOnMessage: THttpApiWebSocketServerOnMessageEvent;
  aOnConnect: THttpApiWebSocketServerOnConnectEvent;
  aOnDisconnect: THttpApiWebSocketServerOnDisconnectEvent;
  aOnFragment: THttpApiWebSocketServerOnMessageEvent);
begin
  if aManualFragmentManagement and not Assigned(aOnFragment) then
    raise EWebSocketApi.CreateFmt('Error register WebSocket protocol. Protocol %s does not use buffer, ' + 'but OnFragment handler is not assigned', [aName]);
  {$ifdef FPC}
    InitCriticalSection(fSafe);
  {$else}
   InitializeCriticalSection(fSafe);
  {$endif}
  fPendingForClose := TList.Create;
  fName := aName;
  fManualFragmentManagement := aManualFragmentManagement;
  fServer := aServer;
  fOnAccept := aOnAccept;
  fOnMessage := aOnMessage;
  fOnConnect := aOnConnect;
  fOnDisconnect := aOnDisconnect;
  fOnFragment := aOnFragment;
  fConnectionsCapacity := WebSocketConnectionCapacity;
  fConnectionsCount := 0;
  fFirstEmptyConnectionIndex := 0;
  GetMem(fConnections, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
  Fillchar(fConnections^, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection), 0);
end;

destructor THttpApiWebSocketServerProtocol.Destroy;
var i: integer;
    conn: PHttpApiWebSocketConnection;
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fPendingForClose.Count-1 do begin
      conn := fPendingForClose[i];
      if Assigned(conn) then begin
        conn.DoOnDisconnect();
        conn.Disconnect();
        Dispose(conn);
      end;
    end;
    fPendingForClose.Free;
  finally
    LeaveCriticalSection(fSafe);
  end;
  {$IFDEF FPC}
  DoneCriticalsection(fSafe);
  {$ELSE}
  DeleteCriticalSection(fSafe);
  {$ENDIF}
  FreeMem(fConnections, fConnectionsCapacity * SizeOf(PHttpApiWebSocketConnection));
  inherited;
end;

procedure THttpApiWebSocketServerProtocol.doShutdown;
var i: Integer;
    conn: PHttpApiWebSocketConnection;
const sReason = 'Server shutdown';
begin
  EnterCriticalSection(fSafe);
  try
    for i := 0 to fConnectionsCount - 1 do begin
      conn := fConnections[i];
      if Assigned(conn) then begin
        RemoveConnection(i);
        conn.fState := wsClosedByShutdown;
        conn.fBuffer := sReason;
        conn.fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
        conn.Close(WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS, Pointer(conn.fBuffer), Length(conn.fBuffer));
//        PostQueuedCompletionStatus(fServer.fThreadPoolServer.FRequestQueue, 0, 0, @conn.fOverlapped);
      end;
    end;
  finally
    LeaveCriticalSection(fSafe);
  end;
end;

procedure THttpApiWebSocketServerProtocol.RemoveConnection(index: integer);
begin
  fPendingForClose.Add(fConnections[index]);
  fConnections[index] := nil;
  if (fFirstEmptyConnectionIndex > index) then
    fFirstEmptyConnectionIndex := index;
end;

function THttpApiWebSocketServerProtocol.Send(index: Integer; aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG): boolean;
var conn: PHttpApiWebSocketConnection;
begin
  result := false;
  if (index>=0) and (index<fConnectionsCount) then begin
    conn := fConnections^[index];
    if (conn<>nil) and (conn.fState=wsOpen) then begin
      conn.Send(aBufferType, aBuffer, aBufferSize);
      result := True;
    end;
  end;
end;


 { THttpApiWebSocketConnection }

function THttpApiWebSocketConnection.TryAcceptConnection(aProtocol: THttpApiWebSocketServerProtocol;
  Ctxt: THttpServerRequest; aNeedHeader: boolean): boolean;
var req: PHTTP_REQUEST;
    wsRequestHeaders: WEB_SOCKET_HTTP_HEADER_ARR;
    wsServerHeaders: PWEB_SOCKET_HTTP_HEADER;
    wsServerHeadersCount: ULONG;
begin
  fState := wsConnecting;
  fBuffer := '';
  fWSHandle := nil;
  fLastActionContext := nil;
  Fillchar(fOverlapped, SizeOf(fOverlapped), 0);
  fProtocol := aProtocol;
  req := PHTTP_REQUEST(Ctxt.HttpApiRequest);
  fIndex := fProtocol.fFirstEmptyConnectionIndex;
  fOpaqueHTTPRequestId := req^.RequestId;
  if (fProtocol=nil) or (Assigned(fProtocol.OnAccept) and not fProtocol.OnAccept(Ctxt, Self)) then begin
    result := False;
    exit;
  end;
  EWebSocketApi.RaiseOnError(hCreateServerHandle, WebSocketAPI.CreateServerHandle(nil, 0, fWSHandle));
  wsRequestHeaders := HttpSys2ToWebSocketHeaders(req^.Headers);
  if aNeedHeader then
    result := WebSocketAPI.BeginServerHandshake(fWSHandle, Pointer(fProtocol.name), nil, 0,
      @wsRequestHeaders[0], Length(wsRequestHeaders), wsServerHeaders, wsServerHeadersCount) = S_OK else
    result := WebSocketAPI.BeginServerHandshake(fWSHandle, nil, nil, 0,
      @wsRequestHeaders[0], Length(wsRequestHeaders), wsServerHeaders, wsServerHeadersCount) = S_OK;
  if result then 
    try
      Ctxt.OutCustomHeaders := WebSocketHeadersToSockString(wsServerHeaders, wsServerHeadersCount);
    finally
      result := WebSocketAPI.EndServerHandshake(fWSHandle) = S_OK;
    end;
  if not Result then
    Disconnect else
    fLastReceiveTickCount := 0;
end;

procedure THttpApiWebSocketConnection.DoOnMessage(aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);
  procedure PushFragmentIntoBuffer;
  var
    l: Integer;
  begin
    l := Length(fBuffer);
    SetLength(fBuffer, l + Integer(aBufferSize));
    Move(aBuffer^, fBuffer[l + 1], aBufferSize);
  end;
begin
  if (fProtocol = nil) then
    exit;
  if (aBufferType=WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE) or
     (aBufferType=WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) then begin // Fragment
    if not fProtocol.ManualFragmentManagement then
      PushFragmentIntoBuffer;
    if Assigned(fProtocol.OnFragment) then
      fProtocol.OnFragment(self,aBufferType,aBuffer,aBufferSize);
  end else begin // last Fragment
    if Assigned(fProtocol.OnMessage) then begin
      if fProtocol.ManualFragmentManagement then 
        fProtocol.OnMessage(self,aBufferType,aBuffer,aBufferSize) else begin
        PushFragmentIntoBuffer;
        fProtocol.OnMessage(self,aBufferType,Pointer(fBuffer),Length(fBuffer));
        fBuffer := '';
      end;
    end;
  end;
end;

procedure THttpApiWebSocketConnection.DoOnConnect;
begin
  if (fProtocol<>nil) and Assigned(fProtocol.OnConnect) then
    fProtocol.OnConnect(self);
end;

procedure THttpApiWebSocketConnection.DoOnDisconnect;
begin
  if (fProtocol<>nil) and Assigned(fProtocol.OnDisconnect) then
    fProtocol.OnDisconnect(self,fCloseStatus,Pointer(fBuffer),length(fBuffer));
end;

procedure THttpApiWebSocketConnection.ReadData(const WebsocketBufferData);
var Err: HRESULT;
    fBytesRead: cardinal;
    aBuf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  if fWSHandle = nil then
    exit;
  Err := Http.ReceiveRequestEntityBody(fProtocol.fServer.FReqQueue, fOpaqueHTTPRequestId, 0,
    aBuf.pbBuffer, aBuf.ulBufferLength, fBytesRead, @self.fOverlapped);
  case Err of
    ERROR_HANDLE_EOF: Disconnect;
    ERROR_IO_PENDING: ; //
    NO_ERROR: ;//
  else
    // todo: close connection
  end;
end;

procedure THttpApiWebSocketConnection.WriteData(const WebsocketBufferData);
var Err: HRESULT;
    httpSendEntity: HTTP_DATA_CHUNK_INMEMORY;
    bytesWrite: Cardinal;
    aBuf: WEB_SOCKET_BUFFER_DATA absolute WebsocketBufferData;
begin
  if fWSHandle = nil then
    exit;
  bytesWrite := 0;
  httpSendEntity.DataChunkType := hctFromMemory;
  httpSendEntity.pBuffer := aBuf.pbBuffer;
  httpSendEntity.BufferLength := aBuf.ulBufferLength;
  Err := Http.SendResponseEntityBody(fProtocol.fServer.FReqQueue,fOpaqueHTTPRequestId,
    HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA or HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
    1, @httpSendEntity, bytesWrite, nil, nil, @fProtocol.fServer.fSendOverlaped);
  case Err of
    ERROR_HANDLE_EOF: Disconnect;
    ERROR_IO_PENDING: ; //
    NO_ERROR: ;//
  else
    // todo: close connection
  end;
end;

procedure THttpApiWebSocketConnection.CheckIsActive;
var elapsed: integer;
const sCloseReason = 'Closed by ellapsed ping timeout';
begin
  if (fLastReceiveTickCount>0) and (fProtocol.fServer.fPingTimeout>0) then begin
    elapsed := integer(GetTickCount-fLastReceiveTickCount);
    if elapsed>2*fProtocol.fServer.PingTimeout*1000 then begin
      fProtocol.RemoveConnection(fIndex);
      fState := wsClosedByGuard;
      fCloseStatus := WEB_SOCKET_ENDPOINT_UNAVAILABLE_CLOSE_STATUS;
      fBuffer := sCloseReason;
      PostQueuedCompletionStatus(fProtocol.fServer.fThreadPoolServer.FRequestQueue, 0, 0, @fOverlapped);
    end else if elapsed>=fProtocol.fServer.PingTimeout * 1000 then
      Ping;
  end;
end;

procedure THttpApiWebSocketConnection.Disconnect;
var //Err: HRESULT; //todo: handle error
    httpSendEntity: HTTP_DATA_CHUNK_INMEMORY;
    bytesWrite: Cardinal;
begin
  WebSocketAPI.AbortHandle(fWSHandle);
  WebSocketAPI.DeleteHandle(fWSHandle);
  fWSHandle := nil;
  httpSendEntity.DataChunkType := hctFromMemory;
  httpSendEntity.pBuffer := nil;
  httpSendEntity.BufferLength := 0;
  {Err :=} Http.SendResponseEntityBody(fProtocol.fServer.fReqQueue, fOpaqueHTTPRequestId,
    HTTP_SEND_RESPONSE_FLAG_DISCONNECT, 1, @httpSendEntity, bytesWrite, nil, nil, nil);
end;

procedure THttpApiWebSocketConnection.BeforeRead;
begin
  // if reading is in progress then try read messages else try receive new messages
  if fState in [wsOpen, wsClosing] then begin
    if Assigned(fLastActionContext) then begin
      EWebSocketApi.RaiseOnError(hCompleteAction, WebSocketAPI.CompleteAction(fWSHandle, fLastActionContext, fOverlapped.InternalHigh));
      fLastActionContext := nil;
    end else
      EWebSocketApi.RaiseOnError(hReceive, WebSocketAPI.Receive(fWSHandle, nil, nil));
  end else
    raise EWebSocketApi.CreateFmt('THttpApiWebSocketConnection.BeforeRead state is not wsOpen', []);
end;

const
  C_WEB_SOCKET_BUFFER_SIZE = 2;

type
  TWebSocketBufferDataArr = array [0 .. C_WEB_SOCKET_BUFFER_SIZE - 1] of WEB_SOCKET_BUFFER_DATA;

function THttpApiWebSocketConnection.ProcessActions(ActionQueue: WEB_SOCKET_ACTION_QUEUE): boolean;
var ulDataBufferCount: ULONG;
    Action: WEB_SOCKET_ACTION;
    BufferType: WEB_SOCKET_BUFFER_TYPE;
    ApplicationContext: Pointer;
    ActionContext: Pointer;
    i: integer;
    Err: HRESULT;
    Buffer: TWebSocketBufferDataArr;
begin
  result := true;
  repeat
    ulDataBufferCount := Length(Buffer);
    EWebSocketApi.RaiseOnError(hGetAction,
      WebSocketAPI.GetAction(fWSHandle, ActionQueue, @Buffer[0], ulDataBufferCount,
          Action, BufferType, ApplicationContext, ActionContext));
    case Action of
      WEB_SOCKET_NO_ACTION: ;
      WEB_SOCKET_SEND_TO_NETWORK_ACTION: begin
        for i := 0 to ulDataBufferCount - 1 do
          WriteData(Buffer[i]);
        if fWSHandle <> nil then begin
          Err := WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0);
          EWebSocketApi.RaiseOnError(hCompleteAction, Err);
        end;
        result := False;
        exit;
      end;
      WEB_SOCKET_INDICATE_SEND_COMPLETE_ACTION: ;
      WEB_SOCKET_RECEIVE_FROM_NETWORK_ACTION: begin
        for i := 0 to ulDataBufferCount - 1 do
          ReadData(Buffer[i]);
        fLastActionContext := ActionContext;
        result := False;
        exit;
      end;
      WEB_SOCKET_INDICATE_RECEIVE_COMPLETE_ACTION: begin
        fLastReceiveTickCount := GetTickCount;
        if BufferType = WEB_SOCKET_CLOSE_BUFFER_TYPE then begin
          EnterCriticalSection(fProtocol.fSafe);
          try
            fProtocol.RemoveConnection(fIndex);
          finally
            LeaveCriticalSection(fProtocol.fSafe);
          end;
          if fState = wsOpen then
            fState := wsClosedByClient else
            fState := wsClosedByServer;
          SetString(fBuffer, PChar(Buffer[0].pbBuffer), Buffer[0].ulBufferLength);
          fCloseStatus := Buffer[0].Reserved1;
          EWebSocketApi.RaiseOnError(hCompleteAction, WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0));
          result := False;
          exit;
        end else if BufferType = WEB_SOCKET_PING_PONG_BUFFER_TYPE then begin
          //todo: may be ansver on client's ping
          EWebSocketApi.RaiseOnError(hCompleteAction, WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0));
          exit;
        end else if BufferType = WEB_SOCKET_UNSOLICITED_PONG_BUFFER_TYPE then begin
          //todo: may be handle this situation
          EWebSocketApi.RaiseOnError(hCompleteAction, WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0));
          exit;
        end else begin
          DoOnMessage(BufferType, Buffer[0].pbBuffer, Buffer[0].ulBufferLength);
          EWebSocketApi.RaiseOnError(hCompleteAction, WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0));
          exit;
        end;
    end else
      raise EWebSocketApi.CreateFmt('Invalid WebSocket action %d', [byte(Action)]);
    end;
    Err := WebSocketAPI.CompleteAction(fWSHandle, ActionContext, 0);
    if ActionContext <> nil then
      EWebSocketApi.RaiseOnError(hCompleteAction, Err);
  until (Action = WEB_SOCKET_NO_ACTION);
end;

procedure THttpApiWebSocketConnection.InternalSend(aBufferType: WEB_SOCKET_BUFFER_TYPE;
  WebsocketBufferData: pointer);
begin
  EWebSocketApi.RaiseOnError(hSend, WebSocketAPI.Send(fWSHandle, aBufferType, WebsocketBufferData, nil));
  ProcessActions(WEB_SOCKET_SEND_ACTION_QUEUE);
end;

procedure THttpApiWebSocketConnection.Send(aBufferType: WEB_SOCKET_BUFFER_TYPE; aBuffer: Pointer; aBufferSize: ULONG);
var wsSendBuf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState<>wsOpen then
    exit;
  wsSendBuf.pbBuffer := aBuffer;
  wsSendBuf.ulBufferLength := aBufferSize;
  InternalSend(aBufferType, @wsSendBuf);
end;

procedure THttpApiWebSocketConnection.Close(aStatus: WEB_SOCKET_CLOSE_STATUS; aBuffer: Pointer; aBufferSize: ULONG);
var wsSendBuf: WEB_SOCKET_BUFFER_DATA;
begin
  if fState=wsOpen then
    fState := wsClosing;
  wsSendBuf.pbBuffer := aBuffer;
  wsSendBuf.ulBufferLength := aBufferSize;
  wsSendBuf.Reserved1 := aStatus;
  InternalSend(WEB_SOCKET_CLOSE_BUFFER_TYPE, @wsSendBuf);
end;

procedure THttpApiWebSocketConnection.Ping;
begin
  InternalSend(WEB_SOCKET_PING_PONG_BUFFER_TYPE, nil);
end;


{ THttpApiWebSocketServer }

constructor THttpApiWebSocketServer.Create(CreateSuspended: Boolean;
  aSocketThreadsCount: integer = 1; aPingTimeout: integer = 0;
  QueueName: SynUnicode = ''; aOnWSThreadStart: TNotifyThreadEvent = nil;
  aOnWSThreadTerminate: TNotifyThreadEvent = nil);
begin
  inherited Create(CreateSuspended, QueueName);
  if not (WebSocketAPI.WebSocketEnabled) then
    raise ECrtSocket.Create('WebSocket is not supported');
  fPingTimeout := aPingTimeout;
  if fPingTimeout>0 then
    fGuard := TSynWebSocketGuard.Create(Self);
  New(fRegisteredProtocols);
  SetLength(fRegisteredProtocols^, 0);
  FOnWSThreadStart := aOnWSThreadStart;
  FOnWSThreadTerminate := aOnWSThreadTerminate;
  fThreadPoolServer := TSynThreadPoolHttpApiWebSocketServer.Create(Self, aSocketThreadsCount);
end;

constructor THttpApiWebSocketServer.CreateClone(From: THttpApiServer);
var wsServer: THttpApiWebSocketServer absolute From;
begin
  inherited CreateClone(From);
  fThreadPoolServer := wsServer.fThreadPoolServer;
  fPingTimeout := wsServer.fPingTimeout;
  fRegisteredProtocols := wsServer.fRegisteredProtocols
end;

procedure THttpApiWebSocketServer.DestroyMainThread;
var i: integer;
begin
  fGuard.Free;
  for i := 0 to Length(fRegisteredProtocols^) - 1 do
    fRegisteredProtocols^[i].doShutdown;
  FreeAndNil(fThreadPoolServer);
  for i := 0 to Length(fRegisteredProtocols^) - 1 do
    fRegisteredProtocols^[i].Free;
  fRegisteredProtocols^ := nil;
  Dispose(fRegisteredProtocols);
  inherited;
end;

procedure THttpApiWebSocketServer.DoAfterSentResponse(Ctxt: THttpServerRequest);
begin
  if Assigned(fLastConnection) then
    PostQueuedCompletionStatus(fThreadPoolServer.FRequestQueue, 0, 0,
      @fLastConnection.fOverlapped);
  inherited DoAfterSentResponse(Ctxt);
end;

function THttpApiWebSocketServer.GetProtocol(index: integer): THttpApiWebSocketServerProtocol;
begin
  if (index>=0) and (index<=Length(fRegisteredProtocols^)) then
    result := fRegisteredProtocols^[index] else
    result := nil;
end;

function THttpApiWebSocketServer.getProtocolsCount: Integer;
begin
  if self=nil then
    result := 0 else
    result := Length(fRegisteredProtocols^);
end;

function THttpApiWebSocketServer.getSendResponseFlags(Ctxt: THttpServerRequest): Integer;
begin
  if (PHTTP_REQUEST(Ctxt.HttpApiRequest)^.UrlContext=WEB_SOCKET_URL_CONTEXT) and
     (fLastConnection<>nil) then
    result := HTTP_SEND_RESPONSE_FLAG_OPAQUE or HTTP_SEND_RESPONSE_FLAG_MORE_DATA
      or HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA else
    result := inherited getSendResponseFlags(Ctxt);
end;

function THttpApiWebSocketServer.UpgradeToWebSocket(Ctxt: THttpServerRequest): cardinal;
var Protocol: THttpApiWebSocketServerProtocol;
    i, j: Integer;
    p: PHTTP_UNKNOWN_HEADER;
    ch, chB: PAnsiChar;
    aName: SockString;
    ProtocolHeaderFound: Boolean;
label protocolFound;
begin
  result := 404;
  Protocol := nil;
  ProtocolHeaderFound := false;
  p := PHTTP_REQUEST(Ctxt.HttpApiRequest)^.Headers.pUnknownHeaders;
  for j := 1 to PHTTP_REQUEST(Ctxt.HttpApiRequest)^.Headers.UnknownHeaderCount do begin
    if (p.NameLength=Length(sProtocolHeader)) and IdemPChar(p.pName,Pointer(sProtocolHeader)) then begin
      ProtocolHeaderFound := True;
      for i := 0 to Length(fRegisteredProtocols^) - 1 do begin
        ch := p.pRawValue;
        while (ch-p.pRawValue)<P.RawValueLength do begin
          while ((ch-p.pRawValue)<P.RawValueLength) and (ch^ in [',', ' ']) do Inc(ch);
          chB := ch;
          while ((ch-p.pRawValue)<P.RawValueLength) and not (ch^ in [',']) do Inc(ch);
          SetString(aName, chB, ch - chB);
          if aName = fRegisteredProtocols^[i].name then begin
            Protocol := fRegisteredProtocols^[i];
            goto protocolFound;
          end;
        end;
      end;
    end;
    Inc(p);
  end;
  if not ProtocolHeaderFound and (Protocol=nil) and (Length(fRegisteredProtocols^)=1) then
    Protocol := fRegisteredProtocols^[0];
protocolFound:
  if Protocol <> nil then begin
    EnterCriticalSection(Protocol.fSafe);
    try
      New(fLastConnection);
      if fLastConnection.TryAcceptConnection(Protocol,Ctxt,ProtocolHeaderFound) then begin
        Protocol.AddConnection(fLastConnection);
        result := 101
      end else begin
        Dispose(fLastConnection);
        fLastConnection := nil;
        result := 405;
      end;
    finally
      LeaveCriticalSection(Protocol.fSafe);
    end;
  end;
end;

function THttpApiWebSocketServer.AddUrlWebSocket(const aRoot, aPort: SockString;
  Https: boolean; const aDomainName: SockString; aRegisterURI: boolean): integer;
begin
  result := AddUrl(aRoot, aPort, Https, aDomainName, aRegisterURI, WEB_SOCKET_URL_CONTEXT);
end;

procedure THttpApiWebSocketServer.RegisterProtocol(const aName: SockString; 
  aManualFragmentManagement: Boolean;
  aOnAccept: THttpApiWebSocketServerOnAcceptEvent;
  aOnMessage: THttpApiWebSocketServerOnMessageEvent;
  aOnConnect: THttpApiWebSocketServerOnConnectEvent;
  aOnDisconnect: THttpApiWebSocketServerOnDisconnectEvent;
  aOnFragment: THttpApiWebSocketServerOnMessageEvent);
var protocol: THttpApiWebSocketServerProtocol;
begin
  if self=nil then exit;
  protocol := THttpApiWebSocketServerProtocol.Create(aName, aManualFragmentManagement,
    Self, aOnAccept, aOnMessage, aOnConnect, aOnDisconnect, aOnFragment);
  protocol.fIndex := length(fRegisteredProtocols^);
  SetLength(fRegisteredProtocols^, protocol.fIndex + 1);
  fRegisteredProtocols^[protocol.fIndex] := protocol;
end;

function THttpApiWebSocketServer.Request(Ctxt: THttpServerRequest): cardinal;
begin
  if PHTTP_REQUEST(Ctxt.HttpApiRequest).UrlContext=WEB_SOCKET_URL_CONTEXT then
    result := UpgradeToWebSocket(Ctxt)
  else begin
    result := inherited Request(Ctxt);
    fLastConnection := nil;
  end;
end;

procedure THttpApiWebSocketServer.SendServiceMessage;
begin
  PostQueuedCompletionStatus(fThreadPoolServer.FRequestQueue, 0, 0, @fServiceOverlaped);
end;

procedure THttpApiWebSocketServer.SetOnWSThreadStart(
  const Value: TNotifyThreadEvent);
begin
  FOnWSThreadStart := Value;
end;

procedure THttpApiWebSocketServer.SetOnWSThreadTerminate(
  const Value: TNotifyThreadEvent);
begin
  FOnWSThreadTerminate := Value;
end;


{ TSynThreadPoolHttpApiWebSocketServer }

function TSynThreadPoolHttpApiWebSocketServer.NeedStopOnIOError: Boolean;
begin
  // If connection closed by guard than ERROR_HANDLE_EOF or ERROR_OPERATION_ABORTED 
  // can be returned - Other connections must work normally
  result := False;
end;

procedure TSynThreadPoolHttpApiWebSocketServer.onThreadStart(Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadStart) then
    fServer.OnWSThreadStart(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.onThreadTerminate(
  Sender: TThread);
begin
  if Assigned(fServer.OnWSThreadTerminate) then
    fServer.OnWSThreadTerminate(Sender);
end;

procedure TSynThreadPoolHttpApiWebSocketServer.Task(aCaller: TSynThread; aContext: Pointer);
var conn: PHttpApiWebSocketConnection;
begin
  if aContext=@fServer.fSendOverlaped then
    exit;
  if (aContext=@fServer.fServiceOverlaped) then begin
    if Assigned(fServer.onServiceMessage) then
      fServer.onServiceMessage;
    exit;
  end;
  conn := PHttpApiWebSocketConnection(aContext);
  if conn.fState=wsConnecting then begin
    conn.fState := wsOpen;
    conn.fLastReceiveTickCount := GetTickCount;
    conn.DoOnConnect();
  end;
  if conn.fState in [wsOpen, wsClosing] then
    repeat
      conn.BeforeRead;
    until not conn.ProcessActions(WEB_SOCKET_RECEIVE_ACTION_QUEUE);
  if conn.fState in [wsClosedByGuard] then
    EWebSocketApi.RaiseOnError(hCompleteAction,
      WebSocketAPI.CompleteAction(conn.fWSHandle, conn.fLastActionContext, 0));
  if conn.fState in [wsClosedByClient,wsClosedByServer,wsClosedByGuard,wsClosedByShutdown] then begin
    conn.DoOnDisconnect;
    if conn.fState = wsClosedByClient then
      conn.Close(conn.fCloseStatus, Pointer(conn.fBuffer), length(conn.fBuffer));
    conn.Disconnect;
    EnterCriticalSection(conn.Protocol.fSafe);
    try
      conn.Protocol.fPendingForClose.Remove(conn);
    finally
      LeaveCriticalSection(conn.Protocol.fSafe);
    end;
    Dispose(conn);
  end;
end;

constructor TSynThreadPoolHttpApiWebSocketServer.Create(Server: THttpApiWebSocketServer;
  NumberOfThreads: Integer);
begin
  fServer := Server;
  fOnThreadStart := onThreadStart;
  fOnTerminate := onThreadTerminate;
  inherited Create(NumberOfThreads, Server.fReqQueue);
end;


{ TSynWebSocketGuard }

procedure TSynWebSocketGuard.Execute;
var i, j: Integer;
begin
  if fServer.fPingTimeout>0 then
    while not Terminated do begin
      if fServer<>nil then
        for i := 0 to Length(fServer.fRegisteredProtocols^)-1 do begin
          EnterCriticalSection(fServer.fRegisteredProtocols^[i].fSafe);
          try
            for j := 0 to fServer.fRegisteredProtocols^[i].fConnectionsCount - 1 do
              if Assigned(fServer.fRegisteredProtocols^[i].fConnections[j]) then
                fServer.fRegisteredProtocols^[i].fConnections[j].CheckIsActive;
          finally
            LeaveCriticalSection(fServer.fRegisteredProtocols^[i].fSafe);
          end;
        end;
      i := 0;
      while not Terminated and (i<fServer.fPingTimeout) do begin
        Sleep(1000);
        inc(i);
      end;
    end
  else
    Terminate;
end;

constructor TSynWebSocketGuard.Create(Server: THttpApiWebSocketServer);
begin
  fServer := Server;
  inherited Create(false);
end;


{ HTTP_RESPONSE }

procedure HTTP_RESPONSE.SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
  const Content, ContentType: SockString);
begin
  fillchar(DataChunk,sizeof(DataChunk),0);
  if Content='' then
    exit;
  DataChunk.DataChunkType := hctFromMemory;
  DataChunk.pBuffer := pointer(Content);
  DataChunk.BufferLength := length(Content);
  EntityChunkCount := 1;
  pEntityChunks := @DataChunk;
  Headers.KnownHeaders[reqContentType].RawValueLength := length(ContentType);
  Headers.KnownHeaders[reqContentType].pRawValue := pointer(ContentType);
end;

function HTTP_RESPONSE.AddCustomHeader(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs;
  ForceCustomHeader: boolean): PAnsiChar;
const KNOWNHEADERS: array[reqCacheControl..respWwwAuthenticate] of PAnsiChar = (
    'CACHE-CONTROL:','CONNECTION:','DATE:','KEEP-ALIVE:','PRAGMA:','TRAILER:',
    'TRANSFER-ENCODING:','UPGRADE:','VIA:','WARNING:','ALLOW:','CONTENT-LENGTH:',
    'CONTENT-TYPE:','CONTENT-ENCODING:','CONTENT-LANGUAGE:','CONTENT-LOCATION:',
    'CONTENT-MD5:','CONTENT-RANGE:','EXPIRES:','LAST-MODIFIED:',
    'ACCEPT-RANGES:','AGE:','ETAG:','LOCATION:','PROXY-AUTHENTICATE:',
    'RETRY-AFTER:','SERVER:','SET-COOKIE:','VARY:','WWW-AUTHENTICATE:');
var UnknownName: PAnsiChar;
    i: integer;
begin
  if ForceCustomHeader then
    i := -1 else
    i := IdemPCharArray(P,KNOWNHEADERS);
  // WebSockets need CONNECTION as unknown header
  if (i>=0) and (THttpHeader(i)<>reqConnection) then
  with Headers.KnownHeaders[THttpHeader(i)] do begin
    while P^<>':' do inc(P);
    inc(P); // jump ':'
    while P^=' ' do inc(P);
    pRawValue := P;
    while P^>=' ' do inc(P);
    RawValueLength := P-pRawValue;
  end else begin
    UnknownName := P;
    while (P^>=' ') and (P^<>':') do inc(P);
    if P^=':' then
      with UnknownHeaders[Headers.UnknownHeaderCount] do begin
        pName := UnknownName;
        NameLength := P-pName;
        repeat inc(P) until P^<>' ';
        pRawValue := P;
        while P^>=' ' do inc(P);
        RawValueLength := P-pRawValue;
        if Headers.UnknownHeaderCount=high(UnknownHeaders) then begin
          SetLength(UnknownHeaders,Headers.UnknownHeaderCount+32);
          Headers.pUnknownHeaders := pointer(UnknownHeaders);
        end;
        inc(Headers.UnknownHeaderCount);
      end else
      while P^>=' ' do inc(P);
  end;
  result := P;
end;

procedure HTTP_RESPONSE.SetHeaders(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs);
{$ifndef NOXPOWEREDNAME}
const XPN: PAnsiChar = XPOWEREDNAME;
      XPV: PAnsiChar = XPOWEREDVALUE;
{$endif}
begin
  Headers.pUnknownHeaders := pointer(UnknownHeaders);
  {$ifdef NOXPOWEREDNAME}
  Headers.UnknownHeaderCount := 0;
  {$else}
  with UnknownHeaders[0] do begin
    pName := XPN;
    NameLength := length(XPOWEREDNAME);
    pRawValue := XPV;
    RawValueLength := length(XPOWEREDVALUE);
  end;
  Headers.UnknownHeaderCount := 1;
  {$endif}
  if P<>nil then
  repeat
    while P^ in [#13,#10] do inc(P);
    if P^=#0 then
      break;
    P := AddCustomHeader(P,UnknownHeaders,false);
  until false;
end;

procedure HTTP_RESPONSE.SetStatus(code: integer; var OutStatus: SockString);
begin
  StatusCode := code;
  OutStatus := StatusCodeToReason(code);
  ReasonLength := length(OutStatus);
  pReason := pointer(OutStatus);
end;

const
  HTTP_LOG_FIELD_TEST_SUB_STATUS: THttpApiLogFields = [hlfSubStatus];

{$endif MSWINDOWS} // encapsulate whole http.sys / HTTP API process


{ THttpRequest }

function THttpRequest.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize)<>'';
end;

constructor THttpRequest.Create(const aServer, aPort: SockString;
  aHttps: boolean; const aProxyName,aProxyByPass: SockString;
  ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
begin
  fPort := GetCardinal(pointer(aPort));
  if fPort=0 then
    if aHttps then
      fPort := 443 else
      fPort := 80;
  fServer := aServer;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fUserAgent := DefaultUserAgent(self);
  if ConnectionTimeOut=0 then
    ConnectionTimeOut := HTTP_DEFAULT_CONNECTTIMEOUT;
  if SendTimeout=0 then
    SendTimeout := HTTP_DEFAULT_SENDTIMEOUT;
  if ReceiveTimeout=0 then
    ReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT;
  InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout); // raise an exception on error
end;

constructor THttpRequest.Create(const aURI, aProxyName,aProxyByPass: SockString;
  ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD; aIgnoreSSLCertificateErrors: boolean);
var URI: TURI;
begin
  if not URI.From(aURI) then
    raise ECrtSocket.CreateFmt('%.Create: invalid aURI=%', [ClassName, aURI]);
  Create(URI.Server,URI.Port,URI.Https,aProxyName,aProxyByPass,
    ConnectionTimeOut,SendTimeout,ReceiveTimeout);
  IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
end;

class function THttpRequest.InternalREST(const url,method,data,header: SockString;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString): SockString;
var URI: TURI;
    oh: SockString;
begin
  result := '';
  with URI do
  if From(url) then
  try
    with self.Create(Server,Port,Https,'','') do
    try
      IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
      Request(Address,method,0,header,data,'',oh,result);
      if outHeaders<>nil then
        outHeaders^ := oh;
    finally
      Free;
    end;
  except
    result := '';
  end;
end;

class function THttpRequest.Get(const aURI,aHeader: SockString;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'GET','',aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Post(const aURI, aData, aHeader: SockString;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'POST',aData,aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Put(const aURI, aData, aHeader: SockString;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'PUT',aData,aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Delete(const aURI, aHeader: SockString;
  aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'DELETE','',aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

function THttpRequest.Request(const url, method: SockString;
  KeepAlive: cardinal; const InHeader, InData, InDataType: SockString;
  out OutHeader, OutData: SockString): integer;
var aData, aDataEncoding, aAcceptEncoding, aURL: SockString;
    i: integer;
begin
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  fKeepAlive := KeepAlive;
  InternalCreateRequest(method,aURL); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType<>'' then
      InternalAddHeader(SockString('Content-Type: ')+InDataType);
    // handle custom compression
    aData := InData;
    if integer(fCompressHeader)<>0 then begin
      aDataEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
        InDataType,aData);
      if aDataEncoding<>'' then
        InternalAddHeader(SockString('Content-Encoding: ')+aDataEncoding);
    end;
    if fCompressAcceptEncoding<>'' then
      InternalAddHeader(fCompressAcceptEncoding);
    // send request to remote server
    InternalSendRequest(aData);
    // retrieve status and headers
    result := InternalRetrieveAnswer(OutHeader,aDataEncoding,aAcceptEncoding,OutData);
    // handle incoming answer compression
    if OutData<>'' then begin
      if aDataEncoding<>'' then
        for i := 0 to high(fCompress) do
          with fCompress[i] do
          if Name=aDataEncoding then
            if Func(OutData,false)='' then
              raise ECrtSocket.CreateFmt('%s uncompress',[Name]) else
              break; // successfully uncompressed content
      if aAcceptEncoding<>'' then
        fCompressHeader := ComputeContentEncoding(fCompress,pointer(aAcceptEncoding));
    end;
  finally
    InternalCloseRequest;
  end;
end;


{$ifdef USEWININET}

{ ************ WinHttp / WinINet HTTP clients }

{ TWinHttpAPI }

const
  // while reading an HTTP response, read it in blocks of this size. 8K for now
  HTTP_RESP_BLOCK_SIZE = 8*1024;

function TWinHttpAPI.InternalRetrieveAnswer(
  var Header, Encoding, AcceptEncoding, Data: SockString): integer;
var Bytes, ContentLength, Read: DWORD;
    tmp: SockString;
begin // HTTP_QUERY* and WINHTTP_QUERY* do match -> common to TWinINet + TWinHTTP
  result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
  Header := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
  Encoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
  AcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
  // retrieve received content (if any)
  Read := 0;
  ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
  if Assigned(fOnDownload) then begin
    // download per-chunk using calback event
    Bytes := fOnDownloadChunkSize;
    if Bytes<=0 then
      Bytes := 65536; // 64KB seems fair enough by default
    SetLength(tmp,Bytes);
    repeat
      Bytes := InternalReadData(tmp,0);
      if Bytes=0 then
        break;
      inc(Read,Bytes);
      if not fOnDownload(self,Read,ContentLength,Bytes,pointer(tmp)^) then
        break; // returned false = aborted
      if Assigned(fOnProgress) then
        fOnProgress(self,Read,ContentLength);
    until false;
  end else
  if ContentLength<>0 then begin
    // optimized version reading "Content-Length: xxx" bytes
    SetLength(Data,ContentLength);
    repeat
      Bytes := InternalReadData(Data,Read);
      if Bytes=0 then begin
        SetLength(Data,Read); // truncated content
        break;
      end;
      inc(Read,Bytes);
      if Assigned(fOnProgress) then
        fOnProgress(self,Read,ContentLength);
    until Read=ContentLength;
  end else begin
    // Content-Length not set: read response in blocks of HTTP_RESP_BLOCK_SIZE
    repeat
      SetLength(Data,Read+HTTP_RESP_BLOCK_SIZE);
      Bytes := InternalReadData(Data,Read);
      if Bytes=0 then
        break;
      inc(Read,Bytes);
      if Assigned(fOnProgress) then
        fOnProgress(self,Read,ContentLength);
    until false;
    SetLength(Data,Read);
  end;
end;


{ EWinINet }

constructor EWinINet.Create;
var dwError, tmpLen: DWORD;
    msg, tmp: string;
begin // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  fLastError := GetLastError;
  msg := SysErrorMessagePerModule(fLastError,'wininet.dll');
  if fLastError=ERROR_INTERNET_EXTENDED_ERROR then begin
    InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError,nil,tmpLen);
    if tmpLen > 0 then begin
      SetLength(tmp,tmpLen);
      InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError,PChar(tmp),tmpLen);
      msg := msg+' ['+tmp+']';
    end;
  end;
  inherited CreateFmt('%s (%d)',[msg,fLastError]);
end;


{ TWinINet }

destructor TWinINet.Destroy;
begin
  if fConnection<>nil then
    InternetCloseHandle(FConnection);
  if fSession<>nil then
    InternetCloseHandle(FSession);
  inherited;
end;

procedure TWinINet.InternalAddHeader(const hdr: SockString);
begin
  if (hdr<>'') and not HttpAddRequestHeadersA(fRequest,
     Pointer(hdr), length(hdr), HTTP_ADDREQ_FLAG_COALESCE) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;

procedure TWinINet.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
var OpenType: integer;
begin
  if fProxyName='' then
   OpenType := INTERNET_OPEN_TYPE_PRECONFIG else
   OpenType := INTERNET_OPEN_TYPE_PROXY;
  fSession := InternetOpenA(Pointer(fUserAgent), OpenType,
    pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession=nil then
    raise EWinINet.Create;
  InternetSetOption(fConnection,INTERNET_OPTION_CONNECT_TIMEOUT,
    @ConnectionTimeOut,SizeOf(ConnectionTimeOut));
  InternetSetOption(fConnection,INTERNET_OPTION_SEND_TIMEOUT,
    @SendTimeout,SizeOf(SendTimeout));
  InternetSetOption(fConnection,INTERNET_OPTION_RECEIVE_TIMEOUT,
    @ReceiveTimeout,SizeOf(ReceiveTimeout));
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort, nil, nil,
    INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection=nil then
    raise EWinINet.Create;
end;

function TWinINet.InternalGetInfo(Info: DWORD): SockString;
var dwSize, dwIndex: DWORD;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not HttpQueryInfoA(fRequest, Info, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(result,dwSize-1);
    if not HttpQueryInfoA(fRequest, Info, pointer(result), dwSize, dwIndex) then
      result := '';
  end;
end;

function TWinINet.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalReadData(var Data: SockString; Read: integer): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCreateRequest(const method, aURL: SockString);
const ALL_ACCEPT: array[0..1] of PAnsiChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := INTERNET_FLAG_HYPERLINK or INTERNET_FLAG_PRAGMA_NOCACHE or
    INTERNET_FLAG_RESYNCHRONIZE; // options for a true RESTful request
  if fKeepAlive<>0 then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
  if fHttps then
    Flags := Flags or INTERNET_FLAG_SECURE;
  FRequest := HttpOpenRequestA(FConnection, Pointer(method), Pointer(aURL), nil,
    nil, @ALL_ACCEPT, Flags,0);
  if FRequest=nil then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalSendRequest(const aData: SockString);
begin
  if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    raise EWinINet.Create;
end;


{ TWinHTTP }

const
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NO_PROXY = 1;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4; // Windows 8.1 and newer
  WINHTTP_FLAG_BYPASS_PROXY_CACHE = $00000100; // add "pragma: no-cache" request header
  WINHTTP_FLAG_REFRESH = WINHTTP_FLAG_BYPASS_PROXY_CACHE;
  WINHTTP_FLAG_SECURE = $00800000; // use SSL if applicable (HTTPS)
  WINHTTP_ADDREQ_FLAG_COALESCE = $40000000;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;

  // taken from http://www.tek-tips.com/faqs.cfm?fid=7493
  // status manifests for WinHttp status callback
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = $00000001;
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = $00000002;
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = $00000004;
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = $00000008;
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = $00000010;
    WINHTTP_CALLBACK_STATUS_REQUEST_SENT = $00000020;
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = $00000040;
    WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = $00000080;
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = $00000100;
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = $00000200;
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = $00000400;
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = $00000800;
    WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = $00001000;
    WINHTTP_CALLBACK_STATUS_REDIRECT = $00004000;
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = $00008000;
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = $00010000;
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = $00020000;
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = $00040000;
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE = $00080000;
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = $00100000;
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = $00200000;
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = $00400000;

    WINHTTP_CALLBACK_FLAG_RESOLVE_NAME =
     (WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED);
    WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER =
     (WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or
      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER);
    WINHTTP_CALLBACK_FLAG_SEND_REQUEST =
     (WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or
      WINHTTP_CALLBACK_STATUS_REQUEST_SENT);
    WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE =
     (WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or
      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED);
    WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION =
     (WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or
      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED);
    WINHTTP_CALLBACK_FLAG_HANDLES =
     (WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or
      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING);
    WINHTTP_CALLBACK_FLAG_DETECTING_PROXY = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
    WINHTTP_CALLBACK_FLAG_REDIRECT = WINHTTP_CALLBACK_STATUS_REDIRECT;
    WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE =  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
    WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
    WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
    WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_READ_COMPLETE = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
    WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
    WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;

    WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS =
        (WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
       or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
       or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
       or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR);
    WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = $ffffffff;

    WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
    WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
    // tls 1.1 & 1.2 const from here:
    // https://github.com/nihon-tc/Rtest/blob/master/header/Microsoft%20SDKs/Windows/v7.0A/Include/winhttp.h
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;

   // Sets an unsigned long integer value that specifies which secure protocols are acceptable.
   // By default only SSL3 and TLS1 are enabled in Windows 7 and Windows 8.
   // By default only SSL3, TLS1.0, TLS1.1, and TLS1.2 are enabled in Windows 8.1 and Windows 10.
   WINHTTP_OPTION_SECURE_PROTOCOLS = 84;
   // Instructs the stack to start a WebSocket handshake process with WinHttpSendRequest.
   // This option takes no parameters.
   WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET = 114;

   // if the following value is returned by WinHttpSetStatusCallback, then
   // probably an invalid (non-code) address was supplied for the callback
   WINHTTP_INVALID_STATUS_CALLBACK = -1;

   WINHTTP_OPTION_DISABLE_FEATURE = 63;
   // values for WINHTTP_OPTION_DISABLE_FEATURE
   WINHTTP_DISABLE_COOKIES = $00000001;
   WINHTTP_DISABLE_REDIRECTS = $00000002;
   WINHTTP_DISABLE_AUTHENTICATION = $00000004;
   WINHTTP_DISABLE_KEEP_ALIVE = $00000008;

   WINHTTP_OPTION_ENABLE_FEATURE = 79;
   // values for WINHTTP_OPTION_ENABLE_FEATURE
   WINHTTP_ENABLE_SSL_REVOCATION = $00000001;
   WINHTTP_ENABLE_SSL_REVERT_IMPERSONATION = $00000002;

type
  WINHTTP_STATUS_CALLBACK = procedure(hInternet: HINTERNET; dwContext: PDWORD;
    dwInternetStatus: DWORD; lpvStatusInformation: pointer; dwStatusInformationLength: DWORD); stdcall;
  PWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;

  /// direct late-binding access to the WinHTTP API
  // - note: WebSocket* API calls require Windows 8 and later
  TWinHTTPBinding = packed record
    /// access to the winhttp.dll loaded library
    LibraryHandle: THandle;
    /// depends on the published .dll functions
    WebSocketEnabled: Boolean;
    /// Initializes an application's use of the WinHTTP functions.
    Open: function(pwszUserAgent: PWideChar; dwAccessType: DWORD;
      pwszProxyName, pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall;
    /// Sets up a callback function that WinHTTP can call as progress is made during an operation.
    SetStatusCallback: function(hSession: HINTERNET; lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
      dwNotificationFlags: DWORD; dwReserved: PDWORD): WINHTTP_STATUS_CALLBACK; stdcall;
    /// Specifies the initial target server of an HTTP request.
    Connect: function(hSession: HINTERNET; pswzServerName: PWideChar;
      nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET; stdcall;
    /// Creates an HTTP request handle.
    OpenRequest: function(hConnect: HINTERNET; pwszVerb: PWideChar;
      pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
      ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall;
    /// Closes a single HINTERNET handle.
    CloseHandle: function(hInternet: HINTERNET): BOOL; stdcall;
    /// Adds one or more HTTP request headers to the HTTP request handle.
    AddRequestHeaders: function(hRequest: HINTERNET; pwszHeaders: PWideChar; dwHeadersLength: DWORD;
      dwModifiers: DWORD): BOOL; stdcall;
    /// Sends the specified request to the HTTP server.
    SendRequest: function(hRequest: HINTERNET; pwszHeaders: PWideChar;
      dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD; dwTotalLength: DWORD;
      dwContext: DWORD): BOOL; stdcall;
    /// Ends an HTTP request that is initiated by WinHttpSendRequest.
    ReceiveResponse: function(hRequest: HINTERNET;
      lpReserved: Pointer): BOOL; stdcall;
    /// Retrieves header information associated with an HTTP request.
    QueryHeaders: function(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: PWideChar;
      lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall;
    /// Reads data from a handle opened by the WinHttpOpenRequest function.
    ReadData: function(hRequest: HINTERNET; lpBuffer: Pointer;
      dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
    /// Sets the various time-outs that are involved with HTTP transactions.
    SetTimeouts: function(hInternet: HINTERNET; dwResolveTimeout: DWORD;
      dwConnectTimeout: DWORD; dwSendTimeout: DWORD; dwReceiveTimeout: DWORD): BOOL; stdcall;
    /// Sets an Internet option.
    SetOption: function(hInternet: HINTERNET; dwOption: DWORD;
      lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall;
    /// Passes the required authorization credentials to the server.
    SetCredentials: function(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD;
      pwszUserName: PWideChar; pwszPassword: PWideChar; pAuthParams: Pointer) : BOOL; stdcall;
    /// Completes a WebSocket handshake started by WinHttpSendRequest.
    WebSocketCompleteUpgrade: function(hRequest: HINTERNET;
      lpReserved: Pointer): HINTERNET; stdcall;
    /// Closes a WebSocket connection.
    WebSocketClose: function(hWebSocket: HINTERNET; usStatus: Word;
      pvReason: Pointer; dwReasonLength: DWORD): DWORD; stdcall;
    /// Retrieves the close status sent by a server
    WebSocketQueryCloseStatus: function(hWebSocket: HINTERNET; out usStatus: Word;
      pvReason: Pointer; dwReasonLength: DWORD; out dwReasonLengthConsumed: DWORD): DWORD; stdcall;
    /// Sends data over a WebSocket connection.
    WebSocketSend: function(hWebSocket: HINTERNET; eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
      pvBuffer: Pointer; dwBufferLength: DWORD): DWORD; stdcall;
    /// Receives data from a WebSocket connection.
    WebSocketReceive: function(hWebSocket: HINTERNET; pvBuffer: Pointer; dwBufferLength: DWORD;
      out dwBytesRead: DWORD; out eBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): DWORD; stdcall;
  end;

var
  WinHttpAPI: TWinHTTPBinding;

type
  TWinHttpAPIs = (hOpen, hSetStatusCallback, hConnect,
    hOpenRequest, hCloseHandle, hAddRequestHeaders,
    hSendRequest, hReceiveResponse, hQueryHeaders,
    hReadData, hSetTimeouts, hSetOption, hSetCredentials,
    hWebSocketCompleteUpgrade, hWebSocketClose, hWebSocketQueryCloseStatus,
    hWebSocketSend, hWebSocketReceive);
const
  hWebSocketApiFirst = hWebSocketCompleteUpgrade;

const
  WinHttpNames: array[TWinHttpAPIs] of PChar = (
    'WinHttpOpen', 'WinHttpSetStatusCallback', 'WinHttpConnect',
    'WinHttpOpenRequest', 'WinHttpCloseHandle', 'WinHttpAddRequestHeaders',
    'WinHttpSendRequest', 'WinHttpReceiveResponse', 'WinHttpQueryHeaders',
    'WinHttpReadData', 'WinHttpSetTimeouts', 'WinHttpSetOption', 'WinHttpSetCredentials',
    'WinHttpWebSocketCompleteUpgrade', 'WinHttpWebSocketClose', 'WinHttpWebSocketQueryCloseStatus',
    'WinHttpWebSocketSend', 'WinHttpWebSocketReceive');

procedure WinHttpAPIInitialize;
var api: TWinHttpAPIs;
    P: PPointer;
begin
  if WinHttpAPI.LibraryHandle<>0 then
    exit; // already loaded
  WinHttpAPI.LibraryHandle := SafeLoadLibrary(winhttpdll);
  WinHttpAPI.WebSocketEnabled := true; // WebSocketEnabled if all functions are available
  if WinHttpAPI.LibraryHandle=0 then
    raise ECrtSocket.CreateFmt('Unable to load library %s',[winhttpdll]);
  P := @@WinHttpAPI.Open;
  for api := low(api) to high(api) do begin
    P^ := GetProcAddress(WinHttpAPI.LibraryHandle,WinHttpNames[api]);
    if P^=nil then
      if api<hWebSocketApiFirst then begin
        FreeLibrary(WinHttpAPI.LibraryHandle);
        WinHttpAPI.LibraryHandle := 0;
        raise ECrtSocket.CreateFmt('Unable to find %s() in %s',[WinHttpNames[api], winhttpdll]);
      end else
        WinHttpAPI.WebSocketEnabled := false; // e.g. version is lower than Windows 8
    inc(P);
  end;
  if WinHttpAPI.WebSocketEnabled then
    WebSocketApiInitialize else
    WebSocketAPI.WebSocketEnabled := false;
end;

destructor TWinHTTP.Destroy;
begin
  if fConnection<>nil then
    WinHttpAPI.CloseHandle(fConnection);
  if fSession<>nil then
    WinHttpAPI.CloseHandle(fSession);
  inherited;
end;

procedure TWinHTTP.InternalAddHeader(const hdr: SockString);
begin
  if (hdr<>'') and
    not WinHttpAPI.AddRequestHeaders(FRequest, Pointer(Ansi7ToUnicode(hdr)), length(hdr),
      WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

procedure TWinHTTP.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    WinHttpAPI.CloseHandle(fRequest);
    FRequest := nil;
  end;
end;

procedure WinHTTPSecurityErrorCallback(hInternet: HINTERNET; dwContext: PDWORD;
  dwInternetStatus: DWORD; lpvStatusInformation: pointer; dwStatusInformationLength: DWORD); stdcall;
begin
  // in case lpvStatusInformation^=-2147483648 this is attempt to connect to
  // non-https socket wrong port - perhaps must be 443?
  raise EWinHTTP.CreateFmt('WinHTTP security error. Status %d, statusInfo: %d',
    [dwInternetStatus, pdword(lpvStatusInformation)^]);
end;

{$ifndef UNICODE}
type
  /// not defined in older Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';
{$endif}

var // raw OS call, to avoid dependency to SynCommons.pas unit
  OSVersionInfo: TOSVersionInfoEx;

procedure TWinHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
var OpenType: integer;
    Callback: WINHTTP_STATUS_CALLBACK;
    CallbackRes: PtrInt absolute Callback; // for FPC compatibility
    // MPV - don't know why, but if I pass WINHTTP_FLAG_SECURE_PROTOCOL_SSL2
    // flag also, TLS1.2 do not work
    protocols: DWORD;
begin
  if OSVersionInfo.dwOSVersionInfoSize=0 then begin // API call once
    OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
    GetVersionEx(OSVersionInfo);
  end;
  if fProxyName='' then
    if (OSVersionInfo.dwMajorVersion>6) or
       ((OSVersionInfo.dwMajorVersion=6) and (OSVersionInfo.dwMinorVersion>=3)) then
      OpenType := WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY else // Windows 8.1 and newer
      OpenType := WINHTTP_ACCESS_TYPE_NO_PROXY else
    OpenType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  fSession := WinHttpAPI.Open(pointer(Ansi7ToUnicode(fUserAgent)), OpenType,
    pointer(Ansi7ToUnicode(fProxyName)), pointer(Ansi7ToUnicode(fProxyByPass)), 0);
  if fSession=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpAPI.SetTimeouts(fSession,HTTP_DEFAULT_RESOLVETIMEOUT,
     ConnectionTimeOut,SendTimeout,ReceiveTimeout) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
  if fHTTPS then begin
     protocols := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3
       or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
     // Windows 7 and newer support TLS 1.1 & 1.2
     if (OSVersionInfo.dwMajorVersion>6) or
       ((OSVersionInfo.dwMajorVersion=6) and (OSVersionInfo.dwMinorVersion>=1)) then
       protocols := protocols or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1
         or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
    if not WinHttpAPI.SetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
       @protocols, SizeOf(protocols)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
    Callback := WinHttpAPI.SetStatusCallback(fSession, WinHTTPSecurityErrorCallback,
       WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes=WINHTTP_INVALID_STATUS_CALLBACK then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;
  fConnection := WinHttpAPI.Connect(fSession, pointer(Ansi7ToUnicode(FServer)), fPort, 0);
  if fConnection=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

function TWinHTTP.InternalGetInfo(Info: DWORD): SockString;
var dwSize, dwIndex: DWORD;
    tmp: SockString;
    i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpAPI.QueryHeaders(fRequest, Info, nil, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(tmp,dwSize);
    if WinHttpAPI.QueryHeaders(fRequest, Info, nil, pointer(tmp), dwSize, dwIndex) then begin
      dwSize := dwSize shr 1;
      SetLength(result,dwSize);
      for i := 0 to dwSize-1 do // fast ANSI 7 bit conversion
        PByteArray(result)^[i] := PWordArray(tmp)^[i];
    end;
  end;
end;

function TWinHTTP.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpAPI.QueryHeaders(fRequest, Info, nil, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinHTTP.InternalReadData(var Data: SockString; Read: integer): cardinal;
begin
  if not WinHttpAPI.ReadData(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

procedure TWinHTTP.InternalCreateRequest(const method, aURL: SockString);
const ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
  if fHttps then
    Flags := Flags or WINHTTP_FLAG_SECURE;
  fRequest := WinHttpAPI.OpenRequest(fConnection, pointer(Ansi7ToUnicode(method)),
    pointer(Ansi7ToUnicode(aURL)), nil, nil, @ALL_ACCEPT, Flags);
  if fRequest=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
  if fKeepAlive = 0 then begin
    Flags := WINHTTP_DISABLE_KEEP_ALIVE;
    if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @Flags, sizeOf(Flags)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;
end;

const
  // from http://www.tek-tips.com/faqs.cfm?fid=7493
  WINHTTP_OPTION_SECURITY_FLAGS = 31;
  WINHTTP_OPTION_CLIENT_CERT_CONTEXT = $0000002F;
  WINHTTP_NO_CLIENT_CERT_CONTEXT = $00000000;
  ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED = $00002F0C;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = $00002000; // expired X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID = $00001000; // bad common name in X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = $00000200;
  SECURITY_FLAT_IGNORE_CERTIFICATES: DWORD =
    SECURITY_FLAG_IGNORE_UNKNOWN_CA or
    SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
    SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
    SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;

  WINHTTP_AUTH_TARGET_SERVER = 0;
  WINHTTP_AUTH_TARGET_PROXY = 1;
  WINHTTP_AUTH_SCHEME_BASIC = $00000001;
  WINHTTP_AUTH_SCHEME_NTLM = $00000002;
  WINHTTP_AUTH_SCHEME_PASSPORT = $00000004;
  WINHTTP_AUTH_SCHEME_DIGEST = $00000008;
  WINHTTP_AUTH_SCHEME_NEGOTIATE = $00000010;

procedure TWinHTTP.InternalSendRequest(const aData: SockString);
var L: integer;
    winAuth: DWORD;
begin
  with fExtendedOptions do
  if AuthScheme<>wraNone then begin
    case AuthScheme of
    wraBasic: winAuth := WINHTTP_AUTH_SCHEME_BASIC;
    wraDigest: winAuth := WINHTTP_AUTH_SCHEME_DIGEST;
    wraNegotiate: winAuth := WINHTTP_AUTH_SCHEME_NEGOTIATE;
    else raise EWinHTTP.CreateFmt('Unsupported AuthScheme=%d',[ord(AuthScheme)]);
    end;
    if not WinHttpAPI.SetCredentials(fRequest,WINHTTP_AUTH_TARGET_SERVER,
       winAuth,pointer(AuthUserName),pointer(AuthPassword),nil) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;
  if fHTTPS and IgnoreSSLCertificateErrors then
    if not WinHttpAPI.SetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  L := length(aData);
  if not WinHttpAPI.SendRequest(fRequest,nil,0,pointer(aData),L,L,0) or
     not WinHttpAPI.ReceiveResponse(fRequest,nil) then begin
    if not fHTTPS then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
    if (GetLastError=ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED) and
       IgnoreSSLCertificateErrors then begin
      if not WinHttpAPI.SetOption(fRequest,WINHTTP_OPTION_SECURITY_FLAGS,
         @SECURITY_FLAT_IGNORE_CERTIFICATES,SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
        RaiseLastModuleError(winhttpdll,EWinHTTP);
      if not WinHttpAPI.SetOption(fRequest,WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
         pointer(WINHTTP_NO_CLIENT_CERT_CONTEXT),0) then
        RaiseLastModuleError(winhttpdll,EWinHTTP);
      if not WinHttpAPI.SendRequest(fRequest,nil,0,pointer(aData),L,L,0) or
         not WinHttpAPI.ReceiveResponse(fRequest,nil) then
        RaiseLastModuleError(winhttpdll,EWinHTTP);
    end;
  end;
end;


{ TWinHTTPUpgradeable }

constructor TWinHTTPUpgradeable.Create(const aServer, aPort: SockString;
  aHttps: boolean; const aProxyName, aProxyByPass: SockString;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: DWORD);
begin
  inherited;
  fSocket := nil;
end;

function TWinHTTPUpgradeable.InternalRetrieveAnswer(var Header, Encoding,
  AcceptEncoding, Data: SockString): integer;
begin
  result := inherited InternalRetrieveAnswer(Header, Encoding, AcceptEncoding, Data);
  fSocket := WinHttpAPI.WebSocketCompleteUpgrade(fRequest, nil);
  if fSocket=nil then
    raise EWinHTTP.Create('Error upgrading socket');
end;

procedure TWinHTTPUpgradeable.InternalSendRequest(const aData: SockString);
begin
  if not WinHttpAPI.SetOption(fRequest,WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET,nil,0) then
    raise EWinHTTP.Create('Error upgrading socket');
  inherited;
end;


{ TWinHTTPWinSocketClient }

function TWinHTTPWebSocketClient.CheckSocket: Boolean;
begin
  result := fSocket <> nil;
end;

function TWinHTTPWebSocketClient.CloseConnection(const aCloseReason: SockString): DWORD;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE else
    result := WinHttpAPI.WebSocketClose(fSocket, WEB_SOCKET_SUCCESS_CLOSE_STATUS, Pointer(aCloseReason), Length(aCloseReason));
  if (Result = NO_ERROR) then
    fSocket := nil;
end;

constructor TWinHTTPWebSocketClient.Create(const aServer, aPort: SockString;
  aHttps: boolean; const url, aSubProtocol, aProxyName, aProxyByPass: SockString;
  ConnectionTimeOut, SendTimeout, ReceiveTimeout: DWORD);
var _http: TWinHTTPUpgradeable;
    inH, outH, outD: SockString;
begin
  fSocket := nil;
  _http := TWinHTTPUpgradeable.Create(aServer, aPort, aHttps, aProxyName, aProxyByPass,
              ConnectionTimeOut, SendTimeout, ReceiveTimeout);
  try
    // WebSocketAPI.BeginClientHandshake()
    if aSubProtocol <> '' then
      inH := sProtocolHeader + ': '+aSubProtocol else
      inH := '';
    if _http.Request(url, 'GET', 0, inH, '', '', outH, outD) = 101 then
      fSocket := _http.fSocket else
      raise ECrtSocket.Create('WebSocketClient creation fail');
  finally
    _http.Free;
  end;
end;

destructor TWinHTTPWebSocketClient.Destroy;
const CloseReason: SockString = 'object is destroyed';
var status: Word;
    reason: SockString;
    reasonLength: DWORD;
begin
  if CheckSocket then begin // todo: check result
    WinHttpAPI.WebSocketClose(fSocket, WEB_SOCKET_ABORTED_CLOSE_STATUS, Pointer(CloseReason), Length(CloseReason));
    SetLength(reason, WEB_SOCKET_MAX_CLOSE_REASON_LENGTH);
    WinHttpAPI.WebSocketQueryCloseStatus(fSocket, status, Pointer(reason),
      WEB_SOCKET_MAX_CLOSE_REASON_LENGTH, reasonLength);
    WinHttpAPI.CloseHandle(fSocket);
  end;
  inherited;
end;

function TWinHTTPWebSocketClient.Receive(aBuffer: pointer; aBufferLength: DWORD; out aBytesRead: DWORD; out aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE): DWORD;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE else
    result := WinHttpAPI.WebSocketReceive(fSocket, aBuffer, aBufferLength, aBytesRead, aBufferType);
end;

function TWinHTTPWebSocketClient.Send(aBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
  aBuffer: pointer; aBufferLength: DWORD): DWORD;
begin
  if not CheckSocket then
    result := ERROR_INVALID_HANDLE else
    result := WinHttpAPI.WebSocketSend(fSocket, aBufferType, aBuffer, aBufferLength);
end;
{$endif USEWININET}

{$ifdef USELIBCURL}

{ ************ libcurl implementation }

const
  LIBCURL_DLL = {$IFDEF Darwin} 'libcurl.dylib' {$ELSE}
    {$IFDEF LINUX} 'libcurl.so' {$ELSE} 'libcurl.dll' {$ENDIF}{$ENDIF};

type
  TCurlOption = (
    coPort                 = 3,
    coTimeout              = 13,
    coInFileSize           = 14,
    coLowSpeedLimit        = 19,
    coLowSpeedTime         = 20,
    coResumeFrom           = 21,
    coCRLF                 = 27,
    coSSLVersion           = 32,
    coTimeCondition        = 33,
    coTimeValue            = 34,
    coVerbose              = 41,
    coHeader               = 42,
    coNoProgress           = 43,
    coNoBody               = 44,
    coFailOnError          = 45,
    coUpload               = 46,
    coPost                 = 47,
    coFTPListOnly          = 48,
    coFTPAppend            = 50,
    coNetRC                = 51,
    coFollowLocation       = 52,
    coTransferText         = 53,
    coPut                  = 54,
    coAutoReferer          = 58,
    coProxyPort            = 59,
    coPostFieldSize        = 60,
    coHTTPProxyTunnel      = 61,
    coSSLVerifyPeer        = 64,
    coMaxRedirs            = 68,
    coFileTime             = 69,
    coMaxConnects          = 71,
    coClosePolicy          = 72,
    coFreshConnect         = 74,
    coForbidResue          = 75,
    coConnectTimeout       = 78,
    coHTTPGet              = 80,
    coSSLVerifyHost        = 81,
    coHTTPVersion          = 84,
    coFTPUseEPSV           = 85,
    coSSLEngineDefault     = 90,
    coDNSUseGlobalCache    = 91,
    coDNSCacheTimeout      = 92,
    coCookieSession        = 96,
    coBufferSize           = 98,
    coNoSignal             = 99,
    coProxyType            = 101,
    coUnrestrictedAuth     = 105,
    coFTPUseEPRT           = 106,
    coHTTPAuth             = 107,
    coFTPCreateMissingDirs = 110,
    coProxyAuth            = 111,
    coFTPResponseTimeout   = 112,
    coIPResolve            = 113,
    coMaxFileSize          = 114,
    coFTPSSL               = 119,
    coTCPNoDelay           = 121,
    coFTPSSLAuth           = 129,
    coIgnoreContentLength  = 136,
    coFTPSkipPasvIp        = 137,
    coFile                 = 10001,
    coURL                  = 10002,
    coProxy                = 10004,
    coUserPwd              = 10005,
    coProxyUserPwd         = 10006,
    coRange                = 10007,
    coInFile               = 10009,
    coErrorBuffer          = 10010,
    coPostFields           = 10015,
    coReferer              = 10016,
    coFTPPort              = 10017,
    coUserAgent            = 10018,
    coCookie               = 10022,
    coHTTPHeader           = 10023,
    coHTTPPost             = 10024,
    coSSLCert              = 10025,
    coSSLCertPasswd        = 10026,
    coQuote                = 10028,
    coWriteHeader          = 10029,
    coCookieFile           = 10031,
    coCustomRequest        = 10036,
    coStdErr               = 10037,
    coPostQuote            = 10039,
    coWriteInfo            = 10040,
    coProgressData         = 10057,
    coInterface            = 10062,
    coKRB4Level            = 10063,
    coCAInfo               = 10065,
    coTelnetOptions        = 10070,
    coRandomFile           = 10076,
    coEGDSocket            = 10077,
    coCookieJar            = 10082,
    coSSLCipherList        = 10083,
    coSSLCertType          = 10086,
    coSSLKey               = 10087,
    coSSLKeyType           = 10088,
    coSSLEngine            = 10089,
    coPreQuote             = 10093,
    coDebugData            = 10095,
    coCAPath               = 10097,
    coShare                = 10100,
    coEncoding             = 10102,
    coPrivate              = 10103,
    coHTTP200Aliases       = 10104,
    coSSLCtxData           = 10109,
    coNetRCFile            = 10118,
    coSourceUserPwd        = 10123,
    coSourcePreQuote       = 10127,
    coSourcePostQuote      = 10128,
    coIOCTLData            = 10131,
    coSourceURL            = 10132,
    coSourceQuote          = 10133,
    coFTPAccount           = 10134,
    coCookieList           = 10135,
    coWriteFunction        = 20011,
    coReadFunction         = 20012,
    coProgressFunction     = 20056,
    coHeaderFunction       = 20079,
    coDebugFunction        = 20094,
    coSSLCtxtFunction      = 20108,
    coIOCTLFunction        = 20130,
    coInFileSizeLarge      = 30115,
    coResumeFromLarge      = 30116,
    coMaxFileSizeLarge     = 30117,
    coPostFieldSizeLarge   = 30120
  );
  TCurlResult = (
    crOK, crUnsupportedProtocol, crFailedInit, crURLMalformat, crURLMalformatUser,
    crCouldntResolveProxy, crCouldntResolveHost, crCouldntConnect,
    crFTPWeirdServerReply, crFTPAccessDenied, crFTPUserPasswordIncorrect,
    crFTPWeirdPassReply, crFTPWeirdUserReply, crFTPWeirdPASVReply,
    crFTPWeird227Format, crFTPCantGetHost, crFTPCantReconnect, crFTPCouldntSetBINARY,
    crPartialFile, crFTPCouldntRetrFile, crFTPWriteError, crFTPQuoteError,
    crHTTPReturnedError, crWriteError, crMalFormatUser, crFTPCouldntStorFile,
    crReadError, crOutOfMemory, crOperationTimeouted,
    crFTPCouldntSetASCII, crFTPPortFailed, crFTPCouldntUseREST, crFTPCouldntGetSize,
    crHTTPRangeError, crHTTPPostError, crSSLConnectError, crBadDownloadResume,
    crFileCouldntReadFile, crLDAPCannotBind, crLDAPSearchFailed,
    crLibraryNotFound, crFunctionNotFound, crAbortedByCallback,
    crBadFunctionArgument, crBadCallingOrder, crInterfaceFailed,
    crBadPasswordEntered, crTooManyRedirects, crUnknownTelnetOption,
    crTelnetOptionSyntax, crObsolete, crSSLPeerCertificate, crGotNothing,
    crSSLEngineNotFound, crSSLEngineSetFailed, crSendError, crRecvError,
    crShareInUse, crSSLCertProblem, crSSLCipher, crSSLCACert, crBadContentEncoding,
    crLDAPInvalidURL, crFileSizeExceeded, crFTPSSLFailed, crSendFailRewind,
    crSSLEngineInitFailed, crLoginDenied, crTFTPNotFound, crTFTPPerm,
    crTFTPDiskFull, crTFTPIllegal, crTFTPUnknownID, crTFTPExists, crTFTPNoSuchUser
  );
  TCurlInfo = (
    ciNone,
    ciLastOne               = 28,
    ciEffectiveURL          = 1048577,
    ciContentType           = 1048594,
    ciPrivate               = 1048597,
    ciResponseCode          = 2097154,
    ciHeaderSize            = 2097163,
    ciRequestSize           = 2097164,
    ciSSLVerifyResult       = 2097165,
    ciFileTime              = 2097166,
    ciRedirectCount         = 2097172,
    ciHTTPConnectCode       = 2097174,
    ciHTTPAuthAvail         = 2097175,
    ciProxyAuthAvail        = 2097176,
    ciOS_Errno              = 2097177,
    ciNumConnects           = 2097178,
    ciTotalTime             = 3145731,
    ciNameLookupTime        = 3145732,
    ciConnectTime           = 3145733,
    ciPreTRansferTime       = 3145734,
    ciSizeUpload            = 3145735,
    ciSizeDownload          = 3145736,
    ciSpeedDownload         = 3145737,
    ciSpeedUpload           = 3145738,
    ciContentLengthDownload = 3145743,
    ciContentLengthUpload   = 3145744,
    ciStartTransferTime     = 3145745,
    ciRedirectTime          = 3145747,
    ciSSLEngines            = 4194331,
    ciCookieList            = 4194332
  );

  TCurlVersion = (cvFirst,cvSecond,cvThird,cvFour);
  TCurlGlobalInit = set of (giSSL,giWin32);
  PAnsiCharArray = array[0..1023] of PAnsiChar;

  TCurlVersionInfo = record
    age: TCurlVersion;
    version: PAnsiChar;
    version_num: cardinal;
    host: PAnsiChar;
    features: longint;
    ssl_version: PAnsiChar;
    ssl_version_num: PAnsiChar;
    libz_version: PAnsiChar;
    protocols: ^PAnsiCharArray;
    ares: PAnsiChar;
    ares_num: longint;
    libidn: PAnsiChar;
  end;
  PCurlVersionInfo = ^TCurlVersionInfo;

  TCurl = type pointer;
  TCurlList = type pointer;

  curl_write_callback = function (buffer: PAnsiChar; size,nitems: integer;
    outstream: pointer): integer; cdecl;
  curl_read_callback = function (buffer: PAnsiChar; size,nitems: integer;
    instream: pointer): integer; cdecl;

var
  curl: packed record
    {$ifdef FPC}
    Module: TLibHandle;
    {$else}
    Module: THandle;
    {$endif}
    global_init: function(flags: TCurlGlobalInit): TCurlResult; cdecl;
    global_cleanup: procedure; cdecl;
    version_info: function(age: TCurlVersion): PCurlVersionInfo; cdecl;
    easy_init: function: pointer; cdecl;
    easy_setopt: function(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs;
    easy_perform: function(curl: TCurl): TCurlResult; cdecl;
    easy_cleanup: procedure(curl: TCurl); cdecl;
    easy_getinfo: function(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl;
    easy_duphandle: function(curl: TCurl): pointer; cdecl;
    easy_reset: procedure(curl: TCurl); cdecl;
    easy_strerror: function(code: TCurlResult): PAnsiChar; cdecl;
    slist_append: function(list: TCurlList; s: PAnsiChar): TCurlList; cdecl;
    slist_free_all: procedure(list: TCurlList); cdecl;
    info: TCurlVersionInfo;
    infoText: string;
  end;

procedure LibCurlInitialize;
var P: PPointer;
    api: integer;
const NAMES: array[0..12] of string = (
  'global_init','global_cleanup','version_info',
  'easy_init','easy_setopt','easy_perform','easy_cleanup','easy_getinfo',
  'easy_duphandle','easy_reset','easy_strerror',
  'slist_append','slist_free_all');
begin
  if curl.Module=0 then
  try
    curl.Module := LoadLibrary(LIBCURL_DLL);
    {$ifdef Darwin}
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl.3.dylib');
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl.4.dylib');
    {$else}
    {$ifdef LINUX}
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl.so.3');
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl.so.4');
    // for latest Linux Mint and other similar distros
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl-gnutls.so.3');
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl-gnutls.so.4');
    {$endif}
    {$endif}
    if curl.Module=0 then
      raise ECrtSocket.CreateFmt('Unable to find %s'{$ifdef LINUX}
        +': try e.g. sudo apt-get install libcurl3:i386'{$endif},[LIBCURL_DLL]);
    P := @@curl.global_init;
    for api := low(NAMES) to high(NAMES) do begin
      P^ := GetProcAddress(curl.Module,{$ifndef FPC}PChar{$endif}('curl_'+NAMES[api]));
      if P^=nil then
        raise ECrtSocket.CreateFmt('Unable to find %s() in %s',[NAMES[api],LIBCURL_DLL]);
      inc(P);
    end;
    curl.global_init([giSSL]);
    curl.info := curl.version_info(cvFour)^;
    curl.infoText := format('%s version %s',[LIBCURL_DLL,curl.info.version]);
    if curl.info.ssl_version<>nil then
      curl.infoText := format('%s using %s',[curl.infoText,curl.info.ssl_version]);
//   api := 0; with curl.info do while protocols[api]<>nil do begin
//     write(protocols[api], ' '); inc(api); end; writeln(#13#10,curl.infoText);
  except
    on E: Exception do begin
      if curl.Module<>0 then begin
        FreeLibrary(curl.Module);
        curl.Module := 0;
      end;
      {$ifdef LINUX}
      writeln(E.Message);
      {$else}
      raise;
      {$endif}
    end;
  end;
end;

function CurlWriteRawByteString(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;
var storage: ^SockString absolute opaque;
    n: integer;
begin
  if storage=nil then
    result := 0 else begin
    n := length(storage^);
    result := size*nitems;
    SetLength(storage^,n+result);
    Move(buffer^,PPAnsiChar(opaque)^[n],result);
  end;
end;

function CurlReadData(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;
var instance: TCurlHTTP absolute opaque;
    dataLen: integer;
begin
  dataLen := length(instance.fIn.Data)-instance.fIn.DataOffset;
  result := size*nitems;
  if result>dataLen then
    result := dataLen;
  move(PAnsiChar(pointer(instance.fIn.Data))[instance.fIn.DataOffset],buffer^,result);
  inc(instance.fIn.DataOffset,result);
end;


{ TCurlHTTP }

procedure TCurlHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
const HTTPS: array[boolean] of string = ('','s');
begin
  inherited;
  if curl.Module=0 then
    LibCurlInitialize;     
  fHandle := curl.easy_init;
  fRootURL := AnsiString(Format('http%s://%s:%d',[HTTPS[fHttps],fServer,fPort]));
end;

destructor TCurlHTTP.Destroy;
begin
  if fHandle<>nil then
    curl.easy_cleanup(fHandle);
  inherited;
end;

procedure TCurlHTTP.UseClientCertificate(
  const aCertFile, aCACertFile, aKeyName, aPassPhrase: SockString);
begin
  fSSL.CertFile := aCertFile;
  fSSL.CACertFile := aCACertFile;
  fSSL.KeyName := aKeyName;
  fSSL.PassPhrase := aPassPhrase;
end;

procedure TCurlHTTP.InternalCreateRequest(const method, aURL: SockString);
const CERT_PEM: SockString = 'PEM';
var url: SockString;
begin
  url := fRootURL+aURL;
  curl.easy_setopt(fHandle,coURL,pointer(url));
  if fProxyName<>'' then
    curl.easy_setopt(fHandle,coProxy,pointer(fProxyName));
  if fHttps then
    if IgnoreSSLCertificateErrors then begin
      curl.easy_setopt(fHandle,coSSLVerifyPeer,0);
      curl.easy_setopt(fHandle,coSSLVerifyHost,0);
    end else begin
      // see https://curl.haxx.se/libcurl/c/simplessl.html
      if fSSL.CertFile<>'' then begin
        curl.easy_setopt(fHandle,coSSLCertType,pointer(CERT_PEM));
        curl.easy_setopt(fHandle,coSSLCert,pointer(fSSL.CertFile));
        if fSSL.PassPhrase<>'' then
          curl.easy_setopt(fHandle,coSSLCertPasswd,pointer(fSSL.PassPhrase));
        curl.easy_setopt(fHandle,coSSLKeyType,nil);
        curl.easy_setopt(fHandle,coSSLKey,pointer(fSSL.KeyName));
        curl.easy_setopt(fHandle,coCAInfo,pointer(fSSL.CACertFile));
        curl.easy_setopt(fHandle,coSSLVerifyPeer,1);
      end;
    end;
  curl.easy_setopt(fHandle,coUserAgent,pointer(fUserAgent));
  fIn.Method := UpperCase(method);
  fIn.Headers := nil;
  Finalize(fOut);
end;

procedure TCurlHTTP.InternalAddHeader(const hdr: SockString);
var P,H: PAnsiChar;
begin
  P := pointer(hdr);
  while P<>nil do begin
    H := pointer(GetNextLine(P));
    if H<>nil then // nil would reset the whole list
      fIn.Headers := curl.slist_append(fIn.Headers,H);
  end;
end;

procedure TCurlHTTP.InternalSendRequest(const aData: SockString);
begin // see http://curl.haxx.se/libcurl/c/CURLOPT_CUSTOMREQUEST.html
  // libcurl has dedicated options for GET,HEAD verbs
  if (fIn.Method='') or (fIn.Method='GET') then begin
    curl.easy_setopt(fHandle,coHTTPGet,1);
    if (aData<>'') and (fIn.Method='GET') then begin // e.g. GET with body
      curl.easy_setopt(fHandle,coCustomRequest,pointer(fIn.Method));
      curl.easy_setopt(fHandle,coNoBody,0);
      curl.easy_setopt(fHandle,coUpload,1);
      fIn.Data := aData;
      fIn.DataOffset := 0;
      curl.easy_setopt(fHandle,coInFile,pointer(self));
      curl.easy_setopt(fHandle,coReadFunction,@CurlReadData);
      curl.easy_setopt(fHandle,coInFileSize,length(aData));
    end;
  end else
  if fIn.Method='HEAD' then
    curl.easy_setopt(fHandle,coNoBody,1) else begin
    // handle other HTTP verbs
    curl.easy_setopt(fHandle,coCustomRequest,pointer(fIn.Method));
    if aData='' then begin // e.g. DELETE or LOCK
      curl.easy_setopt(fHandle,coNoBody,1);
      curl.easy_setopt(fHandle,coUpload,0);
    end else begin // e.g. POST or PUT
      curl.easy_setopt(fHandle,coNoBody,0);
      curl.easy_setopt(fHandle,coUpload,1);
      fIn.Data := aData;
      fIn.DataOffset := 0;
      curl.easy_setopt(fHandle,coInFile,pointer(self));
      curl.easy_setopt(fHandle,coReadFunction,@CurlReadData);
      curl.easy_setopt(fHandle,coInFileSize,length(aData));
    end;
    InternalAddHeader('Expect:'); // disable 'Expect: 100 Continue'
  end;
  curl.easy_setopt(fHandle,coWriteFunction,@CurlWriteRawByteString);
  curl.easy_setopt(fHandle,coFile,@fOut.Data);
  curl.easy_setopt(fHandle,coHeaderFunction,@CurlWriteRawByteString);
  curl.easy_setopt(fHandle,coWriteHeader,@fOut.Header);
end;

function TCurlHTTP.InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
  Data: SockString): integer;
var res: TCurlResult;
    P: PAnsiChar;
    s: SockString;
    i: integer;
    rc: longint; // needed on Linux x86-64
begin
  curl.easy_setopt(fHandle,coHTTPHeader,fIn.Headers);
  res := curl.easy_perform(fHandle);
  if res<>crOK then begin
    result := STATUS_NOTFOUND;
    Data := SockString(format('libcurl easy_perform=%d', [ord(res)]));
  end else begin
    curl.easy_getinfo(fHandle,ciResponseCode,rc);
    result := rc;
    Header := Trim(fOut.Header);
    if IdemPChar(pointer(Header),'HTTP/') then begin
      i := 6;
      while Header[i]>=' ' do inc(i);
      while Header[i] in [#13,#10] do inc(i);
      system.Delete(Header,1,i-1); // trim leading 'HTTP/1.1 200 OK'#$D#$A
    end;
    P := pointer(Header);
    while P<>nil do begin
      s := GetNextLine(P);
      if IdemPChar(pointer(s),'ACCEPT-ENCODING:') then
        AcceptEncoding := trim(copy(s,17,100)) else
      if IdemPChar(pointer(s),'CONTENT-ENCODING:') then
        Encoding := trim(copy(s,19,100))
    end;
    Data := fOut.Data;
  end;
end;

procedure TCurlHTTP.InternalCloseRequest;
begin
  if fIn.Headers<>nil then begin
    curl.slist_free_all(fIn.Headers);
    fIn.Headers := nil;
  end;
  Finalize(fIn);
  fIn.DataOffset := 0;
  Finalize(fOut);
end;

{$endif USELIBCURL}


{ ************ socket polling for optimized multiple connections }

{ TPollSocketAbstract }

{.$define USEWSAPOLL}
// you may try it - but seems slightly SLOWER under Windows 7

function PollSocketClass: TPollSocketClass;
begin
{$ifdef LINUXNOTBSD}
  result := TPollSocketEpoll; // the preferred way for our purpose
{$else}
  {$ifdef MSWINDOWS}
  {$ifdef USEWSAPOLL}
  if Win32MajorVersion>=6 then // WSAPoll() not available before Vista
    result := TPollSocketPoll else
  {$endif USEWSAPOLL}
    result := TPollSocketSelect; // Select() is FASTER than WSAPoll() :(
  {$else}
  result := TPollSocketPoll; // available on all POSIX systems
  {$endif MSWINDOWS}
{$endif LINUXNOTBSD}
end;

constructor TPollSocketAbstract.Create;
begin
  inherited Create;
end;

class function TPollSocketAbstract.New: TPollSocketAbstract;
begin
  result := PollSocketClass.Create;
end;


{$ifdef MSWINDOWS}

{ TPollSocketSelect }

constructor TPollSocketSelect.Create;
begin
  inherited Create;
  fMaxSockets := FD_SETSIZE; // 64
end;

function TPollSocketSelect.Subscribe(socket: TSocket;
  events: TPollSocketEvents; tag: TPollSocketTag): boolean;
begin
  result := false;
  if (self=nil) or (socket=0) or (byte(events)=0) or (fCount=fMaxSockets) then
    exit;
  if pseRead in events then
    FD_SET(socket, fRead);
  if pseWrite in events then
    FD_SET(socket, fWrite);
  fTag[fCount].socket := socket;
  fTag[fCount].tag := tag;
  inc(fCount);
  if socket>fHighestSocket then
    fHighestSocket := socket;
  result := true;
end;

function TPollSocketSelect.Unsubscribe(socket: TSocket): boolean;
var i: integer;
begin
  result := false;
  if (self<>nil) and (socket<>0) then
    for i := 0 to fCount-1 do
      if fTag[i].socket=socket then begin
        FD_CLR(socket,fRead);
        FD_CLR(socket,fWrite);
        dec(fCount);
        if i<fCount then
          move(fTag[i+1],fTag[i],(fCount-i)*sizeof(fTag[i]));
        if fCount=0 then
          fHighestSocket := 0;
        result := true;
        exit;
      end;
end;

function TPollSocketSelect.WaitForModified(out results: TPollSocketResults;
  timeoutMS: integer): integer;
var tv: TTimeVal;
    rd,wr: TFDSet;
    rdp,wrp: PFDSet;
    ev: TPollSocketEvents;
    i: integer;
    tmp: array[0..FD_SETSIZE-1] of TPollSocketResult;
begin
  result := -1; // error
  if (self=nil) or (fCount=0) then
    exit;
  if fRead.fd_count>0 then begin
    rd := fRead;
    rdp := @rd;
  end else
    rdp := nil;
  if fWrite.fd_count>0 then begin
    wr := fWrite;
    wrp := @wr;
  end else
    wrp := nil;
  tv.tv_usec := timeoutMS*1000;
  tv.tv_sec := 0;
  result := Select(fHighestSocket+1,rdp,wrp,nil,@tv);
  if result<=0 then
    exit;
  result := 0;
  for i := 0 to fCount-1 do 
    with fTag[i] do begin
      byte(ev) := 0;
      if (rdp<>nil) and FD_ISSET(socket,rd) then
        include(ev,pseRead);
      if (wrp<>nil) and FD_ISSET(socket,wr) then
        include(ev,pseWrite);
      if byte(ev)<>0 then begin
        tmp[result].events := ev;
        tmp[result].tag := tag;
        inc(result);
      end;
    end;
  SetLength(results,result);
  move(tmp,results[0],result*sizeof(tmp[0]));
end;

{$endif MSWINDOWS}


{ TPollSocketPoll }

constructor TPollSocketPoll.Create;
begin
  inherited Create;
  {$ifdef MSWINDOWS} // some practical values
  fMaxSockets := 1024;
  {$else}
  fMaxSockets := 20000;
  {$endif}
end;

function TPollSocketPoll.Subscribe(socket: TSocket;
  events: TPollSocketEvents; tag: TPollSocketTag): boolean;
var i, n, e, fd: integer;
begin
  result := false;
  if (self=nil) or (socket=0) or (byte(events)=0) or (fCount=fMaxSockets) then
    exit;
  if pseRead in events then
    e := POLLIN else
    e := 0;
  if pseWrite in events then
    e := e or POLLOUT;
  if fFDCount=fCount then begin // no void entry
    for i := 0 to fFDCount-1 do
      if fFD[i].fd=socket then  // already subscribed
        exit;
  end else
  for i := 0 to fFDCount-1 do begin
    fd := fFD[i].fd;
    if fd=socket then  // already subscribed
      exit else
    if fd<0 then begin // found void entry
      with fFD[i] do begin
        fd := socket;
        events := e;
        revents := 0;
      end;
      inc(fCount);
      result := true;
      exit;
    end;
  end;
  if fFDCount=length(fFD) then begin // add new entry to the array
    n := fFDCount+128+fFDCount shr 3;
    if n>fMaxSockets then
      n := fMaxSockets;
    SetLength(fFD,n);
    SetLength(fTags,n);
  end;
  with fFD[fFDCount] do begin
    fd := socket;
    events := e;
    revents := 0;
  end;
  fTags[fFDCount] := tag;
  inc(fFDCount);
  inc(fCount);
  result := true;
end;

procedure TPollSocketPoll.FDVacuum;
var n, i: integer;
begin
  n := 0;
  for i := 0 to fFDCount-1 do
    if fFD[i].fd>0 then begin
      if i<>n then begin
        fFD[n] := fFD[i];
        fTags[n] := fTags[i];
      end;
      inc(n);
    end;
  fFDCount := n;
end;

function TPollSocketPoll.Unsubscribe(socket: TSocket): boolean;
var i: integer;
begin
  for i := 0 to fFDCount-1 do
    if fFD[i].fd=socket then  begin
      fFD[i].fd := -1; // mark entry as void
      dec(fCount);
      if fCount<=fFDCount shr 1 then
        FDVacuum; // avoid too many void entries
      result := true;
      exit;
    end;
  result := false;
end;

function TPollSocketPoll.WaitForModified(out results: TPollSocketResults;
  timeoutMS: integer): integer;
var e: TPollSocketEvents;
    i, ev, d: integer;
begin
  result := -1; // error
  if (self=nil) or (fCount=0) then
    exit;
  result := poll(pointer(fFD),fFDCount,timeoutMS);
  if result<=0 then
    exit;
  SetLength(results,result);
  d := 0;
  for i := 0 to fFDCount-1 do
  if fFD[i].fd>0 then begin
    ev := fFD[i].revents;
    if ev<>0 then begin
      byte(e) := 0;
      if ev and POLLIN<>0 then
        include(e,pseRead);
      if ev and POLLOUT<>0 then
        include(e,pseWrite);
      if ev and POLLERR<>0 then
        include(e,pseError);
      if ev and POLLHUP<>0 then
        include(e,pseClosed);
      results[d].events := e;
      results[d].tag := fTags[i];
      inc(d);
      fFD[i].revents := 0; // reset result flags for reuse
    end;
  end;
  if d<>result then
    raise ECrtSocket.CreateFmt('TPollSocketPoll: result=%d d=%d',[result,d]);
end;


{$ifdef LINUXNOTBSD}

{ TPollSocketEpoll }

constructor TPollSocketEpoll.Create;
begin
  inherited Create;
  fEPFD := epoll_create($cafe);
  fMaxSockets := 20000;
  SetLength(fResults,fMaxSockets);
end;

destructor TPollSocketEpoll.Destroy;
begin
  epoll_close(fEPFD);
  inherited;
end;

function TPollSocketEpoll.Subscribe(socket: TSocket; events: TPollSocketEvents;
  tag: TPollSocketTag): boolean;
var e: TEPollEvent;
begin
  result := false;
  if (self=nil) or (socket=0) or (socket=fEPFD) or
     (byte(events)=0) or (fCount=fMaxSockets) then
    exit;
  e.data.u64 := tag;
  if pseRead in events then
    e.events := EPOLLIN else
    e.events := 0;
  if pseWrite in events then
    e.events := e.events or EPOLLOUT;
  // EPOLLERR and EPOLLHUP are always implicitly defined
  result := epoll_ctl(fEPFD,EPOLL_CTL_ADD,socket,@e)=0;
  if result then
    inc(fCount);
end;

function TPollSocketEpoll.Unsubscribe(socket: TSocket): boolean;
var e: TEPollEvent; // should be there even if not used
begin
  if (self=nil) or (socket=0) or (socket=fEPFD) then
    result := false else begin
    result := epoll_ctl(fEPFD,EPOLL_CTL_DEL,socket,@e)=0;
    if result then
      dec(fCount);
  end;
end;

function TPollSocketEpoll.WaitForModified(out results: TPollSocketResults;
  timeoutMS: integer): integer;
var e: TPollSocketEvents;
    i, ev: integer;
begin
  result := -1; // error
  if (self=nil) or (fCount=0) then
    exit;
  result := epoll_wait(fEPFD,pointer(fResults),fMaxSockets,timeoutMS);
  if result<=0 then
    exit;
  SetLength(results,result);
  for i := 0 to result-1 do begin
    ev := fResults[i].events;
    byte(e) := 0;
    if ev and EPOLLIN<>0 then
      include(e,pseRead);
    if ev and EPOLLOUT<>0 then
      include(e,pseWrite);
    if ev and EPOLLERR<>0 then
      include(e,pseError);
    if ev and EPOLLHUP<>0 then
      include(e,pseClosed);
    results[i].events := e;
    results[i].tag := TPollSocketTag(fResults[i].data.ptr);
  end;
end;

{$endif LINUXNOTBSD}


{ TPollSockets }

constructor TPollSockets.Create(aPollClass: TPollSocketClass=nil);
begin
  inherited Create;
  InitializeCriticalSection(fPendingLock);
  InitializeCriticalSection(fPollLock);
  if aPollClass=nil then
    fPollClass := PollSocketClass else
    fPollClass := aPollClass;
  {$ifndef MSWINDOWS}
  SetFileOpenLimit(GetFileOpenLimit(true)); // set soft limit to hard value
  {$endif MSWINDOWS}
end;

destructor TPollSockets.Destroy;
var p: integer;
begin
  for p := 0 to high(fPoll) do
    fPoll[p].Free;
  DeleteCriticalSection(fPendingLock);
  DeleteCriticalSection(fPollLock);
  inherited Destroy;
end;

function TPollSockets.Subscribe(socket: TSocket; tag: TPollSocketTag;
  events: TPollSocketEvents): boolean;
var p,n: integer;
    poll: TPollSocketAbstract;
begin
  result := false;
  if (self=nil) or (socket=0) or (events=[]) then
    exit;
  EnterCriticalSection(fPollLock);
  try
    poll := nil;
    n := length(fPoll);
    for p := 0 to n-1 do
      if fPoll[p].Count<fPoll[p].MaxSockets then begin
        poll := fPoll[p]; // stil some place in this poll instance
        break;
      end;
    if poll=nil then begin
      poll := fPollClass.Create;
      SetLength(fPoll,n+1);
      fPoll[n] := poll;
    end;
    result := poll.Subscribe(socket,events,tag);
    if result then
      inc(fCount);
  finally
    LeaveCriticalSection(fPollLock);
  end;
end;

function TPollSockets.Unsubscribe(socket: TSocket; tag: TPollSocketTag): boolean;
var p: integer;
begin
  result := false;
  EnterCriticalSection(fPollLock);
  try
    EnterCriticalSection(fPendingLock);
    try
      for p := fPendingIndex to high(fPending) do
        if fPending[p].tag=tag then
          byte(fPending[p].events) := 0; // tag to be ignored in future GetOne
    finally
      LeaveCriticalSection(fPendingLock);
    end;
    for p := 0 to high(fPoll) do
      if fPoll[p].Unsubscribe(socket) then begin
        dec(fCount);
        result := true;
        exit;
      end;
  finally
    LeaveCriticalSection(fPollLock);
  end;
end;

function TPollSockets.GetOneWithinPending(out notif: TPollSocketResult): boolean;
var last,index: integer;
begin
  result := false;
  if fTerminated then
    exit;
  EnterCriticalSection(fPendingLock);
  try
    index := fPendingIndex;
    last := high(fPending);
    while index<=last do begin
      notif := fPending[index]; // return notified events
      if index<last then begin
        inc(index);
        fPendingIndex := index;
      end else begin
        fPending := nil;
        fPendingIndex := 0;
      end;
      if byte(notif.events)<>0 then begin // void e.g. after Unsubscribe()
        result := true;
        exit;
      end;
      if fPending=nil then
        break; // end of list
    end;
  finally
    LeaveCriticalSection(fPendingLock);
  end;
end;

function TPollSockets.GetOne(timeoutMS: integer; out notif: TPollSocketResult): boolean;
  function PollAndSearchWithinPending(p: integer): boolean;
  begin
    if not fTerminated and (fPoll[p].WaitForModified(fPending,0)>0) then begin
      result := GetOneWithinPending(notif);
      if result then
        fPollIndex := p; // continue getting data from fPoll[fPendingPoll]
    end else
      result := false;
  end;
var p,n: integer;
    tix,start,endtix: cardinal;
begin
  result := false;
  byte(notif.events) := 0;
  if (timeoutMS<0) or fTerminated then
    exit;
  start := GetTickCount;
  endtix := start+cardinal(timeoutMS);
  repeat
    // non-blocking search within fPollLock
    EnterCriticalSection(fPollLock);
    try
      if GetOneWithinPending(notif) then
        exit; // found some in fPending[] from fPoll[fPendingPoll]
      n := length(fPoll);
      if n>0 then begin
        for p := fPollIndex+1 to n-1 do
          if PollAndSearchWithinPending(p) then
            exit;
        for p := 0 to fPollIndex do // finally retry on fPendingPoll
          if PollAndSearchWithinPending(p) then
            exit;
      end;
    finally
      LeaveCriticalSection(fPollLock);
      result := byte(notif.events)<>0;
    end;
    // wait a little for something to happen
    if fTerminated or (timeoutMS=0) then
      exit;
    tix := GetTickCount; // allow multi-threaded process
    if (tix<start) or (tix>=endtix) then
      exit;
    dec(tix,start);
    if tix>300 then
      sleep(100) else
    if tix>50 then
      sleep(10) else
      sleep(1);
  until fTerminated;
end;

procedure TPollSockets.Terminate;
begin
  if self<>nil then
    fTerminated := true;
end;


{ TPollSocketsSlot }

function TPollSocketsSlot.Lock: boolean;
begin
  result := InterlockedIncrement(lockcounter)=1;
  if not result then
    InterlockedDecrement(lockcounter);
end;

procedure TPollSocketsSlot.Unlock;
begin
  if @self<>nil then
    InterlockedDecrement(lockcounter);
end;

function TPollSocketsSlot.TryLock(timeoutMS: cardinal): boolean;
var tix,starttix,endtix: cardinal;
begin
  starttix := GetTickCount;
  endtix := starttix+timeoutMS; // never wait forever
  repeat
    result := Lock;
    if result then
      exit; // we acquired the slot
    sleep(1);
    tix := GetTickCount;
  until (tix>=endtix) or (tix<starttix);
end;


{ TPollAsynchSockets }

constructor TPollAsynchSockets.Create;
var c: TPollSocketClass;
begin
  inherited Create;
  c := PollSocketClass;
  fRead := TPollSockets.Create(c);
  {$ifdef LINUXNOTBSD}
  c := TPollSocketPoll; // epoll is overkill for short-living writes
  {$endif}
  fWrite := TPollSockets.Create(c);
end;

destructor TPollAsynchSockets.Destroy;
begin
  if not fRead.Terminated then
    Terminate(5000);
  inherited Destroy;
  fRead.Free;
  fWrite.Free;
end;

function TPollAsynchSockets.Start(connection: TObject): boolean;
var slot: PPollSocketsSlot;
begin
  result := false;
  if (fRead.Terminated) or (connection=nil) then
    exit;
  InterlockedIncrement(fProcessing);
  try
    slot := SlotFromConnection(connection);
    if (slot=nil) or (slot.socket=0) then
      exit;
    if not AsynchSocket(slot.socket) then
      exit; // we expect non-blocking mode on a real working socket
    result := fRead.Subscribe(slot.socket,TPollSocketTag(connection),[pseRead]);
    // now, ProcessRead will handle pseRead + pseError/pseClosed on this socket
  finally
    InterlockedDecrement(fProcessing);
  end;
end;

function TPollAsynchSockets.Stop(connection: TObject): boolean;
var slot: PPollSocketsSlot;
begin
  result := false;
  if (fRead.Terminated) or (connection=nil) then
    exit;
  InterlockedIncrement(fProcessing);
  try
    slot := SlotFromConnection(connection);
    if (slot<>nil) and (slot.socket<>0) then
      try
        fRead.Unsubscribe(slot.socket,TPollSocketTag(connection));
        fWrite.Unsubscribe(slot.socket,TPollSocketTag(connection));
        result := true;
      finally
        DirectShutdown(slot.socket);
        slot.socket := 0;
      end;
  finally
    InterlockedDecrement(fProcessing);
  end;
end;

function TPollAsynchSockets.GetCount: integer;
begin
  if self=nil then
    result := 0 else
    result := fRead.Count;
end;

procedure TPollAsynchSockets.Terminate(waitforMS: integer);
var starttix,endtix: cardinal;
begin
  fRead.Terminate;
  fWrite.Terminate;
  if waitforMS<=0 then
    exit;
  starttix := GetTickCount;
  endtix := starttix+cardinal(waitforMS);
  repeat
    sleep(1);
    if fProcessing=0 then
      break;
  until (GetTickCount>endtix) or (GetTickCount<starttix);
end;

function TPollAsynchSockets.WriteString(connection: TObject;
  const data: SockString): boolean;
begin
  if self=nil then
    result := false else
    result := Write(connection,pointer(data)^,length(data));
end;

procedure AppendData(var buf: SockString; const data; datalen: integer);
var buflen: integer;
begin
  if datalen>0 then begin
    buflen := length(buf);
    SetLength(buf,buflen+datalen);
    move(data,PByteArray(buf)^[buflen],datalen);
  end;
end;

function TPollAsynchSockets.Write(connection: TObject; const data; datalen: integer): boolean;
var tag: TPollSocketTag;
    slot: PPollSocketsSlot;
    P: PByte;
    res,previous: integer;
begin
  result := false;
  if (datalen<=0) or (connection=nil) or fWrite.Terminated then
    exit;
  InterlockedIncrement(fProcessing);
  try
    tag := TPollSocketTag(connection);
    slot := SlotFromConnection(connection);
    if (slot=nil) or (slot.socket=0) then
      exit;
    if slot.TryLock(5000) then // try for 5 seconds for ProcessRead/Write to finish
      try
        P := @data;
        previous := length(slot.writebuf);
        if (previous=0) and not (paoWritePollOnly in fOptions) then
          repeat
            if fWrite.Terminated then
              exit;
            // try to send now in non-blocking mode (works most of the time)
            res := AsynchSend(slot.socket,P,datalen);
            if res<=0 then
              break;
            inc(fWriteCount);
            inc(fWriteBytes,res);
            dec(datalen,res);
            if datalen=0 then begin
              try // notify everything written
                AfterWrite(connection);
                result := true;
                exit;
              except
                result := false;
              end;
              exit;
            end;
            inc(P,res);
          until false;
        // use fWrite output polling for the remaining data
        AppendData(slot.writebuf,P^,datalen);
        if previous>0 then // already subscribed
          result := true else
          if fWrite.Subscribe(slot.socket,tag,[pseWrite]) then
            result := true else
            slot.writebuf := ''; // subscription error -> abort
      finally
        slot.UnLock;
      end;
  finally
    InterlockedDecrement(fProcessing);
  end;
end;

procedure TPollAsynchSockets.ProcessRead(timeoutMS: integer);
var notif: TPollSocketResult;
    connection: TObject;
    slot: PPollSocketsSlot;
    res,added: integer;
    temp: array[0..$7fff] of byte; // read up to 32KB chunks
  procedure CloseConnection;
  begin
    if connection=nil then
      exit;
    Stop(connection); // will shutdown the socket
    try
      OnClose(connection); // do connection.Free
    except
      connection := nil;
    end;
    slot := nil; // ignore pseClosed
  end;
begin
  if (self=nil) or fRead.Terminated then
    exit;
  InterlockedIncrement(fProcessing);
  try
    if not fRead.GetOne(timeoutMS,notif) then
      exit;
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot=nil) or (slot.socket=0) then
      exit;
    if pseError in notif.events then
      if not OnError(connection,notif.events) then begin // false = shutdown
        CloseConnection;
        exit;
      end;
    if pseRead in notif.events then begin
      if slot.Lock then // ensure read slot not already processed in another thread
        try
          added := 0;
          repeat
            if fRead.Terminated then
              exit;
            res := AsynchRecv(slot.socket,@temp,sizeof(temp));
            if res<0 then       // error - probably "may block"
              break;
            if res=0 then begin // socket closed -> abort
              CloseConnection;
              exit;
            end;
            AppendData(slot.readbuf,temp,res);
            inc(added,res);
          until false;
          if added>0 then
            try
              inc(fReadCount);
              inc(fReadBytes,added);
              if OnRead(connection)=sorClose then
                CloseConnection;
            except
              CloseConnection; // any exception will force socket shutdown
            end;
        finally
          slot.UnLock;
        end;
    end;
    if (slot<>nil) and (pseClosed in notif.events) then begin
      CloseConnection;
      exit;
    end;
  finally
    InterlockedDecrement(fProcessing);
  end;
end;

procedure TPollAsynchSockets.ProcessWrite(timeoutMS: integer);
var notif: TPollSocketResult;
    connection: TObject;
    slot: PPollSocketsSlot;
    buf: PByte;
    buflen,res,sent: integer;
begin
  if (self=nil) or fWrite.Terminated then
    exit;
  InterlockedIncrement(fProcessing);
  try
    if not fWrite.GetOne(timeoutMS,notif) then
      exit;
    if notif.events<>[pseWrite] then
      exit; // only try if we are sure the socket is writable and safe
    connection := TObject(notif.tag);
    slot := SlotFromConnection(connection);
    if (slot=nil) or (slot.socket=0) then
      exit;
    if slot.Lock then // ensure write slot not already processed in another thread
      try
        if slot.writebuf<>'' then begin
          buflen := length(slot.writebuf);
          buf := pointer(slot.writebuf);
          inc(buf,buflen);
          sent := 0;
          repeat
            if fWrite.Terminated then
              exit;
            res := AsynchSend(slot.socket,buf,buflen);
            if res<=0 then
              break;
            inc(fWriteCount);
            inc(sent,res);
            inc(buf,res);
            dec(buflen,res);
          until buflen=0;
          inc(fWriteBytes,sent);
          delete(slot.writebuf,1,sent);
        end;
        if slot.writebuf='' then begin // no data any more to be sent
          fWrite.Unsubscribe(slot.socket,notif.tag);
          try
            AfterWrite(connection);
          except
          end;
        end;
      finally
        slot.UnLock;
      end;
  finally
    InterlockedDecrement(fProcessing);
  end;
end;


var
  _MainHttpClass: THttpRequestClass;

function MainHttpClass: THttpRequestClass;
begin
  if _MainHttpClass = nil then begin
    {$ifdef USEWININET}
    _MainHttpClass := TWinHTTP;
    {$else}
    {$ifdef USELIBCURL}
    _MainHttpClass := TCurlHTTP
    {$else}
    raise ECrtSocket.Create('No THttpRequest class known!');
    {$endif}
    {$endif}
  end;
  result := _MainHttpClass;
end;

initialization
  {$ifdef MSWINDOWS}
  Assert(
    {$ifdef CPU64}
    (sizeof(HTTP_REQUEST)=864) and
    (sizeof(HTTP_SSL_INFO)=48) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY)=32) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE)=32) and
    (sizeof(HTTP_REQUEST_HEADERS)=688) and
    (sizeof(HTTP_RESPONSE_HEADERS)=512) and
    (sizeof(HTTP_COOKED_URL)=40) and
    (sizeof(HTTP_RESPONSE)=568) and
    {$else}
    (sizeof(HTTP_REQUEST)=472) and
    (sizeof(HTTP_SSL_INFO)=28) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY)=24) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE)=32) and
    (sizeof(HTTP_RESPONSE)=288) and
    (sizeof(HTTP_REQUEST_HEADERS)=344) and
    (sizeof(HTTP_RESPONSE_HEADERS)=256) and
    (sizeof(HTTP_COOKED_URL)=24) and
    {$endif CPU64}
    (ord(reqUserAgent)=40) and
    (ord(respLocation)=23) and (sizeof(THttpHeader)=4) and
    (integer(HTTP_LOG_FIELD_TEST_SUB_STATUS)=HTTP_LOG_FIELD_SUB_STATUS));
  FillChar(WinHttpAPI, SizeOf(WinHttpAPI), 0);
  WinHttpAPIInitialize;
  {$endif MSWINDOWS}
  if InitSocketInterface then
    WSAStartup(WinsockLevel, WsaDataOnce) else
    fillchar(WsaDataOnce,sizeof(WsaDataOnce),0);
finalization
  {$ifdef USELIBCURL}
  if curl.Module<>0 then begin
    curl.global_cleanup;
    FreeLibrary(curl.Module);
  end;
  {$endif USELIBCURL}
  if WsaDataOnce.wVersion<>0 then
  try
    {$ifdef MSWINDOWS}
    if Assigned(WSACleanup) then
      WSACleanup;
    {$endif}
  finally
    fillchar(WsaDataOnce,sizeof(WsaDataOnce),0);
  end;
  {$ifdef MSWINDOWS}
  if Http.Module<>0 then begin
    FreeLibrary(Http.Module);
    Http.Module := 0;
  end;
  {$endif}
  DestroySocketInterface;
end.
