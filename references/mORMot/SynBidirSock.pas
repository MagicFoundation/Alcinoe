/// implements bidirectional client and server protocol, e.g. WebSockets
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynBidirSock;

{
    This file is part of the Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alfred (alf)
  - AntoineGS
  - f-vicente
  - nortg


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

   TODO: broadcast a message to several aCallingThread: THttpServerResp values
     (add TWebSocketClientRequest? or asynch notifications may be enough, and
      the "broadcast" feature should be implementation at application level,
      using a list of callbacks)

   TODO: enhance TWebSocketServer process to use events, and not threads
   - current implementation has its threads spending most time waiting in loops
   - eventually also at SynCrtSock's THttpServer class level also

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$else}
  SynFPCSock, // shared with Kylix
  {$ifdef KYLIX3}
  LibC,
  Types,
  SynKylix,
  {$endif}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  Contnrs,
  SyncObjs,
  SynLZ,
  SynCommons,
  SynTable,
  SynLog,
  SynCrtSock,
  SynCrypto, // for SHA and AES
  SynEcc;    // for TECDHEProtocol


{ -------------- high-level SynCrtSock classes depending on SynCommons }

type
  /// in-memory storage of one THttpRequestCached entry
  THttpRequestCache = record
    Tag: RawUTF8;
    Content: RawByteString;
  end;
  /// in-memory storage of all THttpRequestCached entries
  THttpRequestCacheDynArray = array of THttpRequestCache;

  /// handles cached HTTP connection to a remote server
  // - use in-memory cached content when HTTP_NOTMODIFIED (304) is returned
  // for an already known ETAG header value
  THttpRequestCached = class(TSynPersistent)
  protected
    fURI: TURI;
    fHttp: THttpRequest; // either fHttp or fSocket is used
    fSocket: THttpClientSocket;
    fKeepAlive: integer;
    fTokenHeader: RawUTF8;
    fCache: TSynDictionary;
  public
    /// initialize the cache for a given server
    // - once set, you can change the request URI using the Address property
    // - aKeepAliveSeconds = 0 will force "Connection: Close" HTTP/1.0 requests
    // - an internal cache will be maintained, and entries will be flushed after
    // aTimeoutSeconds - i.e. 15 minutes per default - setting 0 will disable
    // the client-side cache content
    // - aToken is an optional token which will be transmitted as HTTP header:
    // $ Authorization: Bearer <aToken>
    // - TWinHttp will be used by default under Windows, unless you specify
    // another class
    constructor Create(const aURI: RawUTF8; aKeepAliveSeconds: integer=30;
      aTimeoutSeconds: integer=15*60; const aToken: RawUTF8='';
      aHttpClass: THttpRequestClass=nil); reintroduce;
    /// finalize the current connnection and flush its in-memory cache
    // - you may use LoadFromURI() to connect to a new server
    procedure Clear;
    /// connect to a new server
    // - aToken is an optional token which will be transmitted as HTTP header:
    // $ Authorization: Bearer <aToken>
    // - TWinHttp will be used by default under Windows, unless you specify
    // another class
    function LoadFromURI(const aURI: RawUTF8; const aToken: RawUTF8='';
      aHttpClass: THttpRequestClass=nil): boolean;
    /// finalize the cache
    destructor Destroy; override;
    /// retrieve a resource from the server, or internal cache
    // - aModified^ = true if server returned a HTTP_SUCCESS (200) with some new
    // content, or aModified^ = false if HTTP_NOTMODIFIED (304) was returned
    function Get(const aAddress: SockString; aModified: PBoolean=nil;
      aStatus: PInteger=nil): SockString;
    /// erase one resource from internal cache
    function Flush(const aAddress: SockString): boolean;
    /// read-only access to the connected server
    property URI: TURI read fURI;
  end;


/// will remove most usual HTTP headers which are to be recomputed on sending
function PurgeHeaders(P: PUTF8Char): RawUTF8;


{ ------------ client or server asynchronous process of multiple connections }

type
  /// exception associated with TAsynchConnection / TAsynchConnections process
  EAsynchConnections = class(ESynException);

  /// 32-bit integer value used to identify an asynchronous connection
  // - will start from 1, and increase during the TAsynchConnections live-time
  TAsynchConnectionHandle = type integer;

  TAsynchConnections = class;

  /// abstract class to store one TAsynchConnections connection
  // - may implement e.g. WebSockets frames, or IoT binary protocol
  // - each connection will be identified by a TAsynchConnectionHandle integer
  // - idea is to minimize the resources used per connection, and allow full
  // customization of the process by overriding the OnRead virtual method (and,
  // if needed, AfterCreate/AfterWrite/BeforeDestroy/OnLastOperationIdle)
  TAsynchConnection = class(TSynPersistent)
  protected
    fSlot: TPollSocketsSlot;
    fHandle: TAsynchConnectionHandle;
    fLastOperation: cardinal;
    fRemoteIP: RawUTF8;
    /// this method is called when the instance is connected to a poll
    // - default implementation will set fLastOperation content
    procedure AfterCreate(Sender: TAsynchConnections); virtual;
    /// this method is called when the some input data is pending on the socket
    // - should extract frames or requests from fSlot.readbuf, and handle them
    // - this is where the input should be parsed and extracted according to
    // the implemented procotol; fSlot.readbuf could be kept as temporary
    // buffer during the parsing, and voided by the caller once processed
    // - Sender.Write() could be used for asynchronous answer sending
    // - Sender.LogVerbose() allows logging of escaped data
    // - could return sorClose to shutdown the socket, e.g. on parsing error
    function OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead; virtual; abstract;
    /// this method is called when some data has been written to the socket
    // - default implementation will do nothing
    procedure AfterWrite(Sender: TAsynchConnections); virtual;
    /// this method is called when the instance is about to be deleted from a poll
    // - default implementation will reset fHandle to 0
    procedure BeforeDestroy(Sender: TAsynchConnections); virtual;
    // called after TAsynchConnections.LastOperationIdleSeconds of no activity
    // - reset fLastOperation by default - overriden code should be fast
    // - Sender.Write() could be used to send e.g. a hearbeat frame
    procedure OnLastOperationIdle(Sender: TAsynchConnections); virtual;
  public
    /// initialize this instance
    constructor Create(const aRemoteIP: RawUTF8); reintroduce; virtual;
  published
    /// the associated remote IP4/IP6, as text
    property RemoteIP: RawUTF8 read fRemoteIP;
    /// read-only access to the handle number associated with this connection
    property Handle: TAsynchConnectionHandle read fHandle;
    /// read-only access to the socket number associated with this connection
    property Socket: TSocket read fSlot.socket;
  end;

  /// meta-class of one TAsynchConnections connection
  TAsynchConnectionClass = class of TAsynchConnection;
  /// used to store a dynamic array of TAsynchConnection
  TAsynchConnectionObjArray = array of TAsynchConnection;

  /// handle multiple non-blocking connections using TAsynchConnection instances
  // - OnRead will redirect to TAsynchConnection.OnRead virtual method
  // - OnClose will remove the instance from TAsynchConnections.fConnections[]
  // - OnError will return false to shutdown the connection (unless
  // acoOnErrorContinue is defined in TAsynchConnections.Options)
  TAsynchConnectionsSockets = class(TPollAsynchSockets)
  protected
    fOwner: TAsynchConnections;
    function SlotFromConnection(connection: TObject): PPollSocketsSlot; override;
    function OnRead(connection: TObject): TPollAsynchSocketOnRead; override;
    procedure AfterWrite(connection: TObject); override;
    procedure OnClose(connection: TObject); override;
    function OnError(connection: TObject; events: TPollSocketEvents): boolean; override;
    function GetTotal: integer;
  public
    /// add some data to the asynchronous output buffer of a given connection
    // - this overriden method will refresh TAsynchConnection.LastOperation
    // - can be executed from an TAsynchConnection.OnRead method
    function Write(connection: TObject; const data; datalen: integer;
      timeout: integer=5000): boolean; override;
  published
    /// how many clients have been handled by the poll, from the beginning
    property Total: integer read GetTotal;
  end;

  /// used to implement a thread poll to process TAsynchConnection instances
  TAsynchConnectionsThread = class(TSynThread)
  protected
    fOwner: TAsynchConnections;
    fProcess: TPollSocketEvent; // pseRead or pseWrite
    procedure Execute; override;
  public
    /// initialize the thread
    constructor Create(aOwner: TAsynchConnections; aProcess: TPollSocketEvent); reintroduce;
  end;

  /// low-level options for TAsynchConnections processing
  // - TAsynchConnectionsSockets.OnError will shutdown the connection on any error,
  // unless acoOnErrorContinue is defined
  // - acoOnAcceptFailureStop will let failed Accept() finalize the process
  // - acoNoLogRead and acoNoLogWrite could reduce the log verbosity
  // - acoVerboseLog will log transmitted frames content, for debugging purposes
  // - acoLastOperationNoRead and acoLastOperationNoWrite could be used to
  // avoid TAsynchConnection.fLastOperation reset at read or write
  TAsynchConnectionsOptions = set of (acoOnErrorContinue,
    acoOnAcceptFailureStop, acoNoLogRead, acoNoLogWrite, acoVerboseLog,
    acoLastOperationNoRead, acoLastOperationNoWrite);

  /// implements an abstract thread-pooled high-performance TCP clients or server
  // - internal TAsynchConnectionsSockets will handle high-performance process
  // of a high number of long-living simultaneous connections
  // - will use a TAsynchConnection inherited class to maintain connection state
  // - don't use this abstract class but either TAsynchServer or TAsynchClients
  // - under Linux/POSIX, check your "ulimit -H -n" value: one socket consumes
  // two file descriptors: you may better add the following line to your
  // /etc/limits.conf or /etc/security/limits.conf system file:
  // $ * hard nofile 65535
  TAsynchConnections = class(TServerGeneric)
  protected
    fStreamClass: TAsynchConnectionClass;
    fConnection: TAsynchConnectionObjArray;
    fConnectionCount: integer;
    fConnections: TDynArray; // fConnection[] sorted by TAsynchConnection.Handle
    fClients: TAsynchConnectionsSockets;
    fThreads: array of TAsynchConnectionsThread;
    fLastHandle: integer;
    fLog: TSynLogClass;
    fTempConnectionForSearchPerHandle: TAsynchConnection;
    fOptions: TAsynchConnectionsOptions;
    fLastOperationIdleSeconds: cardinal;
    fThreadClients: record // used by TAsynchClient
      Count, Timeout: integer;
      Address, Port: SockString;
    end;
    fConnectionLock: TSynLocker;
    procedure IdleEverySecond;
    function ConnectionCreate(aSocket: TSocket; const aRemoteIp: RawUTF8;
      out aConnection: TAsynchConnection): boolean; virtual;
    function ConnectionAdd(aSocket: TSocket; aConnection: TAsynchConnection): boolean; virtual;
    function ConnectionDelete(aHandle: TAsynchConnectionHandle): boolean; overload; virtual;
    function ConnectionDelete(aConnection: TAsynchConnection; aIndex: integer): boolean; overload;
    procedure ThreadClientsConnect; // from fThreadClients
  public
    /// initialize the multiple connections
    // - warning: currently reliable only with aThreadPoolCount=1
    constructor Create(OnStart,OnStop: TNotifyThreadEvent;
      aStreamClass: TAsynchConnectionClass; const ProcessName: SockString;
      aLog: TSynLogClass; aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer); reintroduce; virtual;
    /// shut down the instance, releasing all associated threads and sockets
    destructor Destroy; override;
    /// high-level access to a connection instance, from its handle
    // - could be executed e.g. from a TAsynchConnection.OnRead method
    // - returns nil if the handle was not found
    // - returns the maching instance, and caller should release the lock as:
    // ! try ... finally UnLock; end;
    function ConnectionFindLocked(aHandle: TAsynchConnectionHandle;
      aIndex: PInteger=nil): TAsynchConnection;
    /// just a wrapper around fConnectionLock.Lock
    procedure Lock;
    /// just a wrapper around fConnectionLock.UnLock
    procedure Unlock;
    /// remove an handle from the internal list, and close its connection
    // - could be executed e.g. from a TAsynchConnection.OnRead method
    function ConnectionRemove(aHandle: TAsynchConnectionHandle): boolean;
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsynchConnection.OnRead method
    function Write(connection: TAsynchConnection; const data; datalen: integer): boolean; overload;
    /// add some data to the asynchronous output buffer of a given connection
    // - could be executed e.g. from a TAsynchConnection.OnRead method
    function Write(connection: TAsynchConnection; const data: SockString): boolean; overload;
    /// log some binary data with proper escape
    // - can be executed from an TAsynchConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(self,...);
    procedure LogVerbose(connection: TAsynchConnection; const ident: RawUTF8;
      frame: pointer; framelen: integer); overload;
    /// log some binary data with proper escape
    // - can be executed from an TAsynchConnection.OnRead method to track content:
    // $ if acoVerboseLog in Sender.Options then Sender.LogVerbose(...);
    procedure LogVerbose(connection: TAsynchConnection; const ident: RawUTF8;
      const frame: RawByteString); overload;
    /// will execute TAsynchConnection.OnLastOperationIdle after an idle period
    // - could be used to send heartbeats after read/write inactivity
    // - equals 0 (i.e. disabled) by default
    property LastOperationIdleSeconds: cardinal read fLastOperationIdleSeconds
      write fLastOperationIdleSeconds;
    /// allow to customize low-level options for processing
    property Options: TAsynchConnectionsOptions read fOptions write fOptions;
    /// access to the associated log class
    property Log: TSynLogClass read fLog;
    /// low-level unsafe direct access to the connection instances
    // - ensure this property is used in a thread-safe manner, i.e. via
    // ! Lock; try ... finally UnLock; end;
    property Connection: TAsynchConnectionObjArray read fConnection;
    /// low-level unsafe direct access to the connection count
    // - ensure this property is used in a thread-safe manner, i.e. via
    // ! Lock; try ... finally UnLock; end;
    property ConnectionCount: integer read fConnectionCount;
  published
    /// access to the TCP client sockets poll
    // - TAsynchConnection.OnRead should rather use Write() and LogVerbose()
    // methods of this TAsynchConnections class instead of using Clients
    property Clients: TAsynchConnectionsSockets read fClients;
  end;

  /// implements a thread-pooled high-performance TCP server
  // - will use a TAsynchConnection inherited class to maintain connection state
  // for server process
  TAsynchServer = class(TAsynchConnections)
  protected
    fServer: TCrtSocket;
    fExecuteFinished: boolean;
    procedure Execute; override;
  public
    /// run the TCP server, listening on a supplied IP port
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      aStreamClass: TAsynchConnectionClass; const ProcessName: SockString;
      aLog: TSynLogClass; aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer=1); reintroduce; virtual;
    /// shut down the server, releasing all associated threads and sockets
    destructor Destroy; override;
  published
    /// access to the TCP server socket
    property Server: TCrtSocket read fServer;
  end;

  /// implements thread-pooled high-performance TCP multiple clients
  // - e.g. to run some load stress tests with optimized resource use
  // - will use a TAsynchConnection inherited class to maintain connection state
  // of each connected client
  TAsynchClient = class(TAsynchConnections)
  protected
    procedure Execute; override;
  public
    /// start the TCP client connections, connecting to the supplied IP server
    constructor Create(const aServer,aPort: SockString;
      aClientsCount,aClientsTimeoutSecs: integer; OnStart,OnStop: TNotifyThreadEvent;
      aStreamClass: TAsynchConnectionClass; const ProcessName: SockString;
      aLog: TSynLogClass; aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer=1); reintroduce; virtual;
  published
    /// server IP address
    property Server: SockString read fThreadClients.Address;
    /// server IP port
    property Port: SockString read fThreadClients.Port;
  end;


{ -------------- WebSockets shared classes for bidirectional remote access }

type
  /// Exception raised when processing WebSockets
  EWebSockets = class(ESynException);

  /// defines the interpretation of the WebSockets frame data
  // - match order expected by the WebSockets RFC
  TWebSocketFrameOpCode = (
    focContinuation, focText, focBinary,
    focReserved3, focReserved4, focReserved5, focReserved6, focReserved7,
    focConnectionClose, focPing, focPong,
    focReservedB, focReservedC, focReservedD, focReservedE, focReservedF);

  /// set of WebSockets frame interpretation
  TWebSocketFrameOpCodes = set of TWebSocketFrameOpCode;

  /// define one attribute of a WebSockets frame data
  TWebSocketFramePayload = (
    fopAlreadyCompressed);
  /// define the attributes of a WebSockets frame data
  TWebSocketFramePayloads = set of TWebSocketFramePayload;

  /// stores a WebSockets frame
  // - see @http://tools.ietf.org/html/rfc6455 for reference
  TWebSocketFrame = record
    /// the interpretation of the frame data
    opcode: TWebSocketFrameOpCode;
    /// what is stored in the frame data, i.e. in payload field
    content: TWebSocketFramePayloads;
    /// equals GetTickCount64 shr 10, as used for TWebSocketFrameList timeout
    tix: cardinal;
    /// the frame data itself
    // - is plain UTF-8 for focText kind of frame
    // - is raw binary for focBinary or any other frames
    payload: RawByteString;
  end;

  /// points to a WebSockets frame
  PWebSocketFrame = ^TWebSocketFrame;

  /// a dynamic list of WebSockets frames
  TWebSocketFrameDynArray = array of TWebSocketFrame;

  {$M+}
  TWebSocketProcess = class;
  {$M-}

  /// callback event triggered by TWebSocketProtocol for any incoming message
  // - called before TWebSocketProtocol.ProcessIncomingFrame for incoming
  // focText/focBinary frames
  // - should return true if the frame has been handled, or false if the
  // regular processing should take place
  TOnWebSocketProtocolIncomingFrame =
    function(Sender: TWebSocketProcess; var Frame: TWebSocketFrame): boolean of object;

  /// one instance implementing application-level WebSockets protocol
  // - shared by TWebSocketServer and TWebSocketClient classes
  // - once upgraded to WebSockets, a HTTP link could be used e.g. to transmit our
  // proprietary 'synopsejson' or 'synopsebin' application content, as stated
  // by this typical handshake:
  // $ GET /myservice HTTP/1.1
  // $ Host: server.example.com
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
  // $ Sec-WebSocket-Protocol: synopsejson
  // $ Sec-WebSocket-Version: 13
  // $ Origin: http://example.com
  // $
  // $ HTTP/1.1 101 Switching Protocols
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk=
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolJSON inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolBinary inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsebin
  TWebSocketProtocol = class(TSynPersistent)
  protected
    fName: RawUTF8;
    fURI: RawUTF8;
    fFramesInCount: integer;
    fFramesOutCount: integer;
    fFramesInBytes: QWord;
    fFramesOutBytes: QWord;
    fOnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame;
    fRemoteLocalhost: boolean;
    fRemoteIP: SockString;
    fUpgradeURI: RawUTF8;
    fLastError: string;
    fEncryption: IProtocol;
    // calls fEncryption.ProcessHandshake, if defined
    function ProcessHandshake(const ExtIn: TRawUTF8DynArray; out ExtOut: RawUTF8;
      ErrorMsg: PRawUTF8): boolean; virtual;
    function ProcessURI(const aClientURI: RawUTF8): boolean; virtual; // e.g. for authentication
    // focText/focBinary or focContinuation/focConnectionClose from ProcessStart/ProcessStop
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); virtual; abstract;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean; virtual;
    procedure AfterGetFrame(var frame: TWebSocketFrame); virtual;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); virtual;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUTF8;
      HeadFound: PRawUTF8=nil): pointer; virtual;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; virtual;
    function GetRemoteIP: SockString;
    function GetEncrypted: boolean;
    // fName by default, but e.g. 'synopsebin, synopsebinary'
    function GetSubprotocols: RawUTF8; virtual;
    function SetSubprotocol(const aProtocolName: RawUTF8): boolean; virtual;
  public
    /// abstract constructor to initialize the protocol
    // - the protocol should be named, so that the client may be able to request
    // for a given protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aName,aURI: RawUTF8); reintroduce;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; virtual; abstract;
    /// set the fEncryption: IProtocol according to the supplied key
    // - any asymetric algorithm need to know which side (client/server) to work on
    // - try TECDHEProtocol.FromKey(aKey) and fallback to TProtocolAES.Create(TAESCFB)
    // using SHA256Weak(aKey)
    procedure SetEncryptKey(aServer: boolean; const aKey: RawUTF8);
    /// set the fEncryption: IProtocol as TProtocolAES.Create(TAESCFB)
    procedure SetEncryptKeyAES(const aKey; aKeySize: cardinal);
    /// allow low-level interception before ProcessIncomingFrame is done
    property OnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame
      read fOnBeforeIncomingFrame write fOnBeforeIncomingFrame;
    /// access low-level frame encryption
    property Encryption: IProtocol read fEncryption;
  published
    /// the Sec-WebSocket-Protocol application name currently involved
    // - e.g. 'synopsejson', 'synopsebin' or 'synopsebinary'
    property Name: RawUTF8 read fName;
    /// the optional URI on which this protocol would be enabled
    // - leave to '' if any URI should match
    property URI: RawUTF8 read fURI;
    /// the associated 'Remote-IP' HTTP header value
    // - returns '' if self=nil or RemoteLocalhost=true
    property RemoteIP: SockString read GetRemoteIP;
    /// the URI on which this protocol has been upgraded
    property UpgradeURI: RawUTF8 read fUpgradeURI;
    /// the last error message, during frame processing
    property LastError: string read fLastError;
    /// returns TRUE if encryption is enabled during the transmission
    // - is currently only available for TWebSocketProtocolBinary
    property Encrypted: boolean read GetEncrypted;
    /// how many frames have been received by this instance
    property FramesInCount: integer read fFramesInCount;
    /// how many frames have been sent by this instance
    property FramesOutCount: integer read fFramesOutCount;
    /// how many (uncompressed) bytes have been received by this instance
    property FramesInBytes: QWord read fFramesInBytes;
    /// how many (uncompressed) bytes have been sent by this instance
    property FramesOutBytes: QWord read fFramesOutBytes;
  end;

  /// callback event triggered by TWebSocketProtocolChat for any incoming message
  // - a first call with frame.opcode=focContinuation will take place when
  // the connection will be upgraded to WebSockets
  // - then any incoming focText/focBinary events will trigger this callback
  // - eventually, a focConnectionClose will notify the connection ending
  TOnWebSocketProtocolChatIncomingFrame =
    procedure(Sender: THttpServerResp; const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. with AJAX applications
  // - see TWebSocketProtocolRest for bi-directional events expecting answers
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// initialize the chat protocol with an incoming frame callback
    constructor Create(const aName,aURI: RawUTF8;
      const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame); overload;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
    /// on the server side, allows to send a message over the wire to a
    // specified client connection
    // - a temporary copy of the Frame content will be made for safety
    function SendFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame): boolean;
    /// on the server side, allows to send a JSON message over the wire to a
    // specified client connection
    // - the supplied JSON content is supplied as "var", since it may be
    // modified during execution, e.g. XORed for frame masking
    function SendFrameJson(Sender: THttpServerResp; var JSON: RawUTF8): boolean;
    /// you can assign an event to this property to be notified of incoming messages
    property OnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame
      read fOnIncomingFrame write fOnIncomingFrame;
  end;


  /// handle a REST application-level bi-directional WebSockets protocol
  // - will emulate a bi-directional REST process, using THttpServerRequest to
  // store and handle the request parameters: clients would be able to send
  // regular REST requests to the server, but the server could use the same
  // communication channel to push REST requests to the client
  // - a local THttpServerRequest will be used on both client and server sides,
  // to store REST parameters and compute the corresponding WebSockets frames
  TWebSocketProtocolRest = class(TWebSocketProtocol)
  protected
    fSequencing: boolean;
    fSequence: integer;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); virtual; abstract;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; virtual; abstract;
    /// convert the input information of REST request to a WebSocket frame
    procedure InputToFrame(Ctxt: THttpServerRequest; aNoAnswer: boolean;
      out request: TWebSocketFrame; out head: RawUTF8); virtual;
    /// convert a WebSocket frame to the input information of a REST request
    function FrameToInput(var request: TWebSocketFrame; out aNoAnswer: boolean;
      Ctxt: THttpServerRequest): boolean; virtual;
    /// convert a WebSocket frame to the output information of a REST request
    function FrameToOutput(var answer: TWebSocketFrame; Ctxt: THttpServerRequest): cardinal; virtual;
    /// convert the output information of REST request to a WebSocket frame
    procedure OutputToFrame(Ctxt: THttpServerRequest; Status: Cardinal;
      var outhead: RawUTF8; out answer: TWebSocketFrame); virtual;
  end;

  /// used to store the class of a TWebSocketProtocol type
  TWebSocketProtocolClass = class of TWebSocketProtocol;

  /// handle a REST application-level WebSockets protocol using JSON for transmission
  // - could be used e.g. for AJAX or non Delphi remote access
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsejson
  TWebSocketProtocolJSON = class(TWebSocketProtocolRest)
  protected
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUTF8;
      HeadFound: PRawUTF8=nil): pointer; override;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; override;
  public
    /// initialize the WebSockets JSON protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aURI: RawUTF8); reintroduce;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  end;

  /// handle a REST application-level WebSockets protocol using compressed and
  // optionally AES-CFB encrypted binary
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsebin
  // or fallback to the previous subprotocol
  // $ Sec-WebSocket-Protocol: synopsebinary
  // - 'synopsebin' will expect requests sequenced as 'r000001','r000002',...
  // headers matching 'a000001','a000002',... instead of 'request'/'answer'
  TWebSocketProtocolBinary = class(TWebSocketProtocolRest)
  protected
    fCompressed: boolean;
    fFramesInBytesSocket: QWord;
    fFramesOutBytesSocket: QWord;
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    procedure AfterGetFrame(var frame: TWebSocketFrame); override;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); override;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUTF8;
      HeadFound: PRawUTF8=nil): pointer; override;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; override;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean; override;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
    function GetFramesInCompression: integer;
    function GetFramesOutCompression: integer;
    function GetSubprotocols: RawUTF8; override;
    function SetSubprotocol(const aProtocolName: RawUTF8): boolean; override;
  public
    /// initialize the WebSockets binary protocol with no encryption
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; aCompressed: boolean=true); reintroduce; overload; virtual;
    /// initialize the WebSockets binary protocol with a symmetric AES key
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - if aKeySize if 128, 192 or 256, TProtocolAES (i.e. AES-CFB encryption)
    //  will be used to secure the transmission
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; const aKey; aKeySize: cardinal;
      aCompressed: boolean=true); reintroduce; overload;
    /// initialize the WebSockets binary protocol from a textual key
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - will create a TProtocolAES or TECDHEProtocol instance, corresponding to
    // the supplied aKey and aServer values, to secure the transmission using
    // a symmetric or assymetric algorithm
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; aServer: boolean; const aKey: RawUTF8;
      aCompressed: boolean=true); reintroduce; overload;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  published
    /// defines if SynLZ compression is enabled during the transmission
    // - is set to TRUE by default
    property Compressed: boolean read fCompressed write fCompressed;
    /// how many bytes have been received by this instance from the wire
    property FramesInBytesSocket: QWord read fFramesInBytesSocket;
    /// how many bytes have been sent by this instance to the wire
    property FramesOutBytesSocket: QWord read fFramesOutBytesSocket;
    /// compression ratio of frames received by this instance
    property FramesInCompression: integer read GetFramesInCompression;
    /// compression ratio of frames Sent by this instance
    property FramesOutCompression: integer read GetFramesOutCompression;
  end;

  /// used to maintain a list of websocket protocols (for the server side)
  TWebSocketProtocolList = class(TSynPersistentLock)
  protected
    fProtocols: array of TWebSocketProtocol;
    // caller should make fSafe.Lock/UnLock
    function FindIndex(const aName,aURI: RawUTF8): integer;
  public
    /// add a protocol to the internal list
    // - returns TRUE on success
    // - if this protocol is already existing for this given name and URI,
    // returns FALSE: it is up to the caller to release aProtocol if needed
    function Add(aProtocol: TWebSocketProtocol): boolean;
    /// add once a protocol to the internal list
    // - if this protocol is already existing for this given name and URI, any
    // previous one will be released - so it may be confusing on a running server
    // - returns TRUE if the protocol was added for the first time, or FALSE
    // if the protocol has been replaced or is invalid (e.g. aProtocol=nil)
    function AddOnce(aProtocol: TWebSocketProtocol): boolean;
    /// erase a protocol from the internal list, specified by its name
    function Remove(const aProtocolName,aURI: RawUTF8): boolean;
    /// finalize the list storage
    destructor Destroy; override;
    /// create a new protocol instance, from the internal list
    function CloneByName(const aProtocolName, aClientURI: RawUTF8): TWebSocketProtocol;
    /// create a new protocol instance, from the internal list
    function CloneByURI(const aClientURI: RawUTF8): TWebSocketProtocol;
    /// how many protocols are stored
    function Count: integer;
  end;

  /// indicates which kind of process did occur in the main WebSockets loop
  TWebSocketProcessOne = (
    wspNone, wspPing, wspDone, wspAnswer, wspError, wspClosed);

  /// indicates how TWebSocketProcess.NotifyCallback() will work
  TWebSocketProcessNotifyCallback = (
    wscBlockWithAnswer, wscBlockWithoutAnswer, wscNonBlockWithoutAnswer);

  /// used to manage a thread-safe list of WebSockets frames
  TWebSocketFrameList = class(TSynPersistentLock)
  protected
    fTimeoutSec: PtrInt;
    procedure Delete(i: integer);
  public
    /// low-level access to the WebSocket frames list
    List: TWebSocketFrameDynArray;
    /// current number of WebSocket frames in the list
    Count: integer;
    /// initialize the list
    constructor Create(timeoutsec: integer); reintroduce;
    /// add a WebSocket frame in the list
    // - this method is thread-safe
    procedure Push(const frame: TWebSocketFrame);
    /// add a void WebSocket frame in the list
    // - this method is thread-safe
    procedure PushVoidFrame(opcode: TWebSocketFrameOpCode);
    /// retrieve a WebSocket frame from the list, oldest first
    // - you should specify a frame type to search for, according to the
    // specified WebSockets protocl
    // - this method is thread-safe
    function Pop(protocol: TWebSocketProtocol;
      const head: RawUTF8; out frame: TWebSocketFrame): boolean;
    /// how many 'answer' frames are to be ignored
    // - this method is thread-safe
    function AnswerToIgnore(incr: integer=0): integer;
  end;

  /// parameters to be used for WebSockets process
  {$ifdef USERECORDWITHMETHODS}TWebSocketProcessSettings = record
    {$else}TWebSocketProcessSettings = object{$endif}
  public
    /// time in milli seconds between each focPing commands sent to the other end
    // - default is 0, i.e. no automatic ping sending on client side, and
    // 20000, i.e. 20 seconds, on server side
    HeartbeatDelay: cardinal;
    /// maximum period time in milli seconds when ProcessLoop thread will stay
    // idle before checking for the next pending requests
    // - default is 500 ms, but you may put a lower value, if you expects e.g.
    // REST commands or NotifyCallback(wscNonBlockWithoutAnswer) to be processed
    // with a lower delay
    LoopDelay: cardinal;
    /// ms between sending - allow to gather output frames
    // - GetTickCount resolution is around 16ms under Windows, so default 10ms
    // seems fine for a cross-platform similar behavior
    SendDelay: cardinal;
    /// will close the connection after a given number of invalid Heartbeat sent
    // - when a Hearbeat is failed to be transmitted, the class will start
    // counting how many ping/pong did fail: when this property value is
    // reached, it will release and close the connection
    // - default value is 5
    DisconnectAfterInvalidHeartbeatCount: cardinal;
    /// how many milliseconds the callback notification should wait acquiring
    // the connection before failing
    // - defaut is 5000, i.e. 5 seconds
    CallbackAcquireTimeOutMS: cardinal;
    /// how many milliseconds the callback notification should wait for the
    // client to return its answer
    // - defaut is 30000, i.e. 30 seconds
    CallbackAnswerTimeOutMS: cardinal;
    /// callback run when a WebSockets client is just connected
    // - triggerred by TWebSocketProcess.ProcessStart
    OnClientConnected: TNotifyEvent;
    /// callback run when a WebSockets client is just disconnected
    // - triggerred by TWebSocketProcess.ProcessStop
    OnClientDisconnected: TNotifyEvent;
    /// by default, contains [] to minimize the logged information
    // - set logHeartbeat if you want the ping/pong frames to be logged
    // - set logTextFrameContent if you want the text frame content to be logged
    // - set logBinaryFrameContent if you want the binary frame content to be logged
    // - used only if WebSocketLog global variable is set to a TSynLog class
    LogDetails: set of (logHeartbeat,logTextFrameContent,logBinaryFrameContent);
    /// will set the default values
    procedure SetDefaults;
    /// will set LogDetails to its highest level of verbosity
    // - used only if WebSocketLog global variable is set
    procedure SetFullLog;
  end;

  /// points to parameters to be used for WebSockets process
  // - using a pointer/reference type will allow in-place modification of
  // any TWebSocketProcess.Settings, TWebSocketServer.Settings or
  // THttpClientWebSockets.Settings property
  PWebSocketProcessSettings = ^TWebSocketProcessSettings;

  /// the current state of the WebSockets process
  TWebSocketProcessState = (wpsCreate,wpsRun,wpsClose,wpsDestroy);

  /// abstract WebSockets process, used on both client or server sides
  // - CanGetFrame/ReceiveBytes/SendBytes abstract methods should be overriden with
  // actual communication, and fState and ProcessStart/ProcessStop should be
  // updated from the actual processing thread (e.g. as in TWebCrtSocketProcess)
  TWebSocketProcess = class(TSynPersistent)
  protected
    fProcessName: RawUTF8;
    fIncoming: TWebSocketFrameList;
    fOutgoing: TWebSocketFrameList;
    fOwnerThread: TSynThread;
    fOwnerConnection: THttpServerConnectionID;
    fState: TWebSocketProcessState;
    fProtocol: TWebSocketProtocol;
    fMaskSentFrames: byte;
    fProcessEnded: boolean;
    fNoConnectionCloseAtDestroy: boolean;
    fProcessCount: integer;
    fSettings: TWebSocketProcessSettings;
    fSafeIn, fSafeOut: PSynLocker;
    fInvalidPingSendCount: cardinal;
    fSafePing: PSynLocker;
    fLastSocketTicks: Int64;
    function LastPingDelay: Int64;
    procedure SetLastPingTicks(invalidPing: boolean=false);
    /// callback methods run by ProcessLoop
    procedure ProcessStart; virtual;
    procedure ProcessStop; virtual;
    // called by ProcessLoop - TRUE=continue, FALSE=ended
    // - caller may have checked that some data is pending to read
    function ProcessLoopStepReceive: boolean;
    // called by ProcessLoop - TRUE=continue, FALSE=ended
    // - caller may check that LastPingDelay>fSettings.SendDelay and Socket is writable
    function ProcessLoopStepSend: boolean;
    // blocking process, for one thread handling all WebSocket connection process
    procedure ProcessLoop;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; virtual; abstract;
    procedure HiResDelay(const start: Int64);
    procedure Log(const frame: TWebSocketFrame; const aMethodName: RawUTF8;
      aEvent: TSynLogInfo=sllTrace; DisableRemoteLog: Boolean=false); virtual;
    function SendPendingOutgoingFrames: boolean;
  public
    /// initialize the WebSockets process on a given connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aProtocol: TWebSocketProtocol; aOwnerConnection: THttpServerConnectionID;
      aOwnerThread: TSynThread; const aSettings: TWebSocketProcessSettings;
      const aProcessName: RawUTF8); reintroduce;
    /// finalize the context
    // - if needed, will notify the other end with a focConnectionClose frame
    // - will release the TWebSocketProtocol associated instance
    destructor Destroy; override;
    /// abstract low-level method to retrieve pending input data
    // - should return the number of bytes (<=count) received and written to P
    // - is defined separated to allow multi-thread pooling
    function ReceiveBytes(P: PAnsiChar; count: integer): integer;
      virtual; abstract;
    /// abstract low-level method to send pending output data
    // - returns false on any error, try on success
    // - is defined separated to allow multi-thread pooling
    function SendBytes(P: pointer; Len: integer): boolean;
      virtual; abstract;
    /// abstract low-level method to check if there is some pending input data
    // in the input Socket ready for GetFrame/ReceiveBytes
    // - is defined separated to allow multi-thread pooling
    function CanGetFrame(TimeOut: cardinal; ErrorWithoutException: PInteger): boolean;
      virtual; abstract;
    /// blocking process incoming WebSockets framing protocol
    // - CanGetFrame should have been called and returned true before
    // - will call overriden ReceiveBytes() for the actual communication
    function GetFrame(out Frame: TWebSocketFrame; ErrorWithoutException: PInteger): boolean;
    /// process outgoing WebSockets framing protocol -> to be overriden
    // - will call overriden SendBytes() for the actual communication
    // - use Outgoing.Push() to send frames asynchronously
    function SendFrame(var Frame: TWebSocketFrame): boolean;
    /// will push a request or notification to the other end of the connection
    // - caller should set the aRequest with the outgoing parameters, and
    // optionally receive a response from the other end
    // - the request may be sent in blocking or non blocking mode
    // - returns the HTTP Status code (e.g. HTTP_SUCCESS=200 for success)
    function NotifyCallback(aRequest: THttpServerRequest;
      aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the settings currently used during the WebSockets process
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
    /// returns the current state of the underlying connection
    function State: TWebSocketProcessState;
    /// the associated 'Remote-IP' HTTP header value
    // - returns '' if Protocol=nil or Protocol.RemoteLocalhost=true
    function RemoteIP: SockString;
    /// direct access to the low-level incoming frame stack
    property Incoming: TWebSocketFrameList read fIncoming;
    /// direct access to the low-level outgoing frame stack
    // - call Outgoing.Push() to send frames asynchronously, with optional
    // jumboframe gathering (if supported by the protocol)
    property Outgoing: TWebSocketFrameList read fOutgoing;
    /// the associated low-level processing thread
    property OwnerThread: TSynThread read fOwnerThread;
    /// the associated low-level WebSocket connection opaque identifier
    property OwnerConnection: THttpServerConnectionID read fOwnerConnection;
    /// how many frames are currently processed by this connection
    property ProcessCount: integer read fProcessCount;
    /// may be set to TRUE before Destroy to force raw socket disconnection
    property NoConnectionCloseAtDestroy: boolean
      read fNoConnectionCloseAtDestroy write fNoConnectionCloseAtDestroy;
  published
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    property Protocol: TWebSocketProtocol read fProtocol;
    /// the associated process name
    property ProcessName: RawUTF8 read fProcessName write fProcessName;
    /// how many invalid heartbeat frames have been sent
    // - a non 0 value indicates a connection problem
    property InvalidPingSendCount: cardinal read fInvalidPingSendCount;
  end;

  /// TCrtSocket-based WebSockets process, used on both client or server sides
  // - will use the socket in blocking mode, so expects its own processing thread
  TWebCrtSocketProcess = class(TWebSocketProcess)
  protected
    fSocket: TCrtSocket;
  public
    /// initialize the WebSockets process on a given TCrtSocket connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aSocket: TCrtSocket; aProtocol: TWebSocketProtocol;
      aOwnerConnection: THttpServerConnectionID; aOwnerThread: TSynThread;
      const aSettings: TWebSocketProcessSettings;
      const aProcessName: RawUTF8); reintroduce; virtual;
    /// first step of the low level incoming WebSockets framing protocol over TCrtSocket
    // - in practice, just call fSocket.SockInPending to check for pending data
    function CanGetFrame(TimeOut: cardinal; ErrorWithoutException: PInteger): boolean; override;
    /// low level receive incoming WebSockets frame data over TCrtSocket
    // - in practice, just call fSocket.SockInRead to check for pending data
    function ReceiveBytes(P: PAnsiChar; count: integer): integer; override;
    /// low level receive incoming WebSockets frame data over TCrtSocket
    // - in practice, just call fSocket.TrySndLow to send pending data
    function SendBytes(P: pointer; Len: integer): boolean; override;
    /// the associated communication socket
    // - on the server side, is a THttpServerSocket
    // - access to this instance is protected by Safe.Lock/Unlock
    property Socket: TCrtSocket read fSocket;
  end;


{ -------------- WebSockets Server classes for bidirectional remote access }

type
  TWebSocketServerSocket = class(THttpServerSocket)
  public
    /// overriden to detect upgrade: websocket header and return grOwned
    function GetRequest(withBody: boolean;
      headerMaxTix: Int64): THttpServerSocketGetRequestResult; override;
  end;

type
  {$M+}
  TWebSocketServerResp = class;
  {$M-}

  /// implements WebSockets process as used on server side
  TWebSocketProcessServer = class(TWebCrtSocketProcess)
  protected
    fServerResp: TWebSocketServerResp;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; override;
  end;

  /// an enhanced input/output structure used for HTTP and WebSockets requests
  // - this class will contain additional parameters used to maintain the
  // WebSockets execution context in overriden TWebSocketServer.Process method
  TWebSocketServerResp = class(THttpServerResp)
  protected
    fProcess: TWebSocketProcessServer;
  public
    /// initialize the context, associated to a HTTP/WebSockets server instance
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer); override;
    /// push a notification to the client
    function NotifyCallback(Ctxt: THttpServerRequest; aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    function WebSocketProtocol: TWebSocketProtocol;
    /// low-level WebSocket protocol processing instance
    property WebSocketProcess: TWebSocketProcessServer read fProcess;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fWebSocketConnections: TSynObjectListLocked;
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
    /// validate the WebSockets handshake, then call Context.fProcess.ProcessLoop()
    function WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
      Context: TWebSocketServerResp): integer; virtual;
    /// overriden method which will recognize the WebSocket protocol handshake,
    // then run the whole bidirectional communication in its calling thread
    // - here ConnectionThread is a THttpServerResp, and ClientSock.Headers
    // and ConnectionUpgrade properties should be checked for the handshake
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread); override;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - due to the way how WebSockets works, one thread will be created
    // for any incoming connection
    // - note that this constructor will not register any protocol, so is
    // useless until you execute Protocols.Add()
    // - in the current implementation, the ServerThreadPoolCount parameter will
    // use two threads by default to handle shortliving HTTP/1.0 "connection: close"
    // requests, and one thread will be maintained per keep-alive/websockets client
    // - by design, the KeepAliveTimeOut value is ignored with this server
    // once it has been upgraded to WebSockets
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString; ServerThreadPoolCount: integer=2;
      KeepAliveTimeOut: integer=30000; HeadersNotFiltered: boolean=false;
      CreateSuspended: boolean = false); override;
    /// close the server
    destructor Destroy; override;
    /// will send a given frame to all connected clients
    // - expect aFrame.opcode to be either focText or focBinary
    // - will call TWebSocketProcess.Outgoing.Push for asynchronous sending
    procedure WebSocketBroadcast(const aFrame: TWebSocketFrame); overload;
    /// will send a given frame to clients matching the supplied connection IDs
    // - expect aFrame.opcode to be either focText or focBinary
    // - will call TWebSocketProcess.Outgoing.Push for asynchronous sending
    procedure WebSocketBroadcast(const aFrame: TWebSocketFrame;
      const aClientsConnectionID: THttpServerConnectionIDDynArray); overload;
    /// give access to the underlying connection from its ID
    // - also identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocket(ConnectionID: THttpServerConnectionID): TWebSocketServerResp;
    /// give access to the underlying connection from its connection thread
    // - also identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocketThread(ConnectionThread: TSynThread): TWebSocketServerResp;
    /// the settings to be used for WebSockets process
    // - note that those parameters won't be propagated to existing connections
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
    /// how many WebSockets connections are currently maintained
    function WebSocketConnections: integer;
    /// access to the protocol list handled by this server
    property WebSocketProtocols: TWebSocketProtocolList read fProtocols;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // our proprietary Sec-WebSocket-Protocol: 'synopsejson' or 'synopsebin'
  // application content, managing regular REST client-side requests and
  // also server-side push notifications
  // - once in 'synopse*' mode, the Request() method will be trigerred from
  // any incoming REST request from the client, and the OnCallback event
  // will be available to push a request from the server to the client
  TWebSocketServerRest = class(TWebSocketServer)
  public
    /// create a Server Thread, binded and listening on a port, with our
    // 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    // - warning: WaitStarted should be called after Create() to check for
    // for actual port binding in the background thread
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const aProcessName, aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false); reintroduce; overload;
    /// defines the WebSockets protocols to be used for this Server
    // - i.e. 'synopsebin' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    procedure WebSocketsEnable(const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompressed: boolean=true);
    /// server can send a request back to the client, when the connection has
    // been upgraded to WebSocket
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return STATUS_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; override;
  end;


/// used to return the text corresponding to a specified WebSockets frame data
function ToText(opcode: TWebSocketFrameOpCode): PShortString; overload;

/// used to return the text corresponding to a specified WebSockets sending mode
function ToText(mode: TWebSocketProcessNotifyCallback): PShortString; overload;

/// low-level intitialization of a TWebSocketFrame for proper REST content
procedure FrameInit(opcode: TWebSocketFrameOpCode; const Content, ContentType: RawByteString;
  out frame: TWebSocketFrame);


{ -------------- WebSockets Client classes for bidirectional remote access }

type
  {$M+}
  THttpClientWebSockets = class;
  TWebSocketProcessClientThread = class;
  {$M-}

  /// implements WebSockets process as used on client side
  TWebSocketProcessClient = class(TWebCrtSocketProcess)
  protected
    fClientThread: TWebSocketProcessClientThread;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; override;
  public
    /// initialize the client process for a given THttpClientWebSockets
    constructor Create(aSender: THttpClientWebSockets; aProtocol: TWebSocketProtocol;
      const aProcessName: RawUTF8); reintroduce; virtual;
    /// finalize the process
    destructor Destroy; override;
  end;

  /// the current state of the client side processing thread
  TWebSocketProcessClientThreadState = (sCreate, sRun, sFinished, sClosed);

  /// WebSockets processing thread used on client side
  // - will handle any incoming callback
  TWebSocketProcessClientThread = class(TSynThread)
  protected
    fThreadState: TWebSocketProcessClientThreadState;
    fProcess: TWebSocketProcessClient;
    procedure Execute; override;
  public
    constructor Create(aProcess: TWebSocketProcessClient); reintroduce;
  end;

  /// Socket API based REST and HTTP/1.1 client, able to upgrade to WebSockets
  // - will implement regular HTTP/1.1 until WebSocketsUpgrade() is called
  THttpClientWebSockets = class(THttpClientSocket)
  protected
    fProcess: TWebSocketProcessClient;
    fSettings: TWebSocketProcessSettings;
    fOnCallbackRequestProcess: TOnHttpServerRequest;
    fOnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame;
    fOnWebSocketsClosed: TNotifyEvent;
    procedure SetInt32OptionByIndex(OptName, OptVal: integer); override;
  public
    /// low-level initialization of a client WebSockets connection
    // - calls Open() then WebSocketsUpgrade() for a given protocol
    // - with proper error interception and optional logging, returning nil
    class function WebSocketsConnect(const aHost, aPort: SockString;
      aProtocol: TWebSocketProtocol; aLog: TSynLogClass=nil;
      const aLogContext: RawUTF8=''; const aURI: RawUTF8='';
      const aCustomHeaders: RawUTF8=''): THttpClientWebSockets;
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    constructor Create(aTimeOut: PtrInt=10000); override;
    /// finalize the connection
    destructor Destroy; override;
    /// process low-level REST request, either on HTTP/1.1 or via WebSockets
    // - after WebSocketsUpgrade() call, will use WebSockets for the communication
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const header, Data, DataType: SockString; retry: boolean): integer; override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - i.e. 'synopsebin' and optionally 'synopsejson' modes
    // - you may specify an URI to as expected by the server for upgrade
    // - if aWebSocketsAJAX equals default FALSE, it will register the
    // TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAJAX is TRUE, it will register the slower and less secure
    // TWebSocketProtocolJSON (to be used for AJAX debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - alternatively, you can specify your own custom TWebSocketProtocol instance
    // (owned by this method and immediately released on error)
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompression: boolean=true;
      aProtocol: TWebSocketProtocol=nil; const aCustomHeaders: RawUTF8=''): RawUTF8;
    /// the settings to be used for WebSockets process
    // - note that those parameters won't be propagated to existing connections
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
    /// this event handler will be executed for any incoming push notification
    property OnCallbackRequestProcess: TOnHttpServerRequest
      read fOnCallbackRequestProcess write fOnCallbackRequestProcess;
    /// event handler trigerred when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
    /// allow low-level interception before
    // TWebSocketProcessClient.ProcessIncomingFrame is executed
    property OnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame
      read fOnBeforeIncomingFrame write fOnBeforeIncomingFrame;
  published
    /// the current WebSockets processing class
    // - equals nil for plain HTTP/1.1 mode
    // - points to the current WebSockets process instance, after a successful
    // WebSocketsUpgrade() call, so that you could use e.g. WebSockets.Protocol
    // to retrieve the protocol currently used
    property WebSockets: TWebSocketProcessClient read fProcess;
  end;


var
  /// if set, will log all WebSockets raw information
  // - see also TWebSocketProcessSettings.LogDetails and
  // TWebSocketProcessSettings.SetFullLog to setup even more verbose information,
  // e.g. by setting HttpServerFullWebSocketsLog and HttpClientFullWebSocketsLog
  // global variables to true (as defined in mORMotHttpServer/mORMotHttpClient)
  WebSocketLog: TSynLogClass;

  /// number of bytes above which SynLZ compression may be done
  // - when working with TWebSocketProtocolBinary
  // - it is useless to compress smaller frames, which fits in network MTU
  WebSocketsBinarySynLzThreshold: integer = 450;

  /// how replay attacks will be handled in TWebSocketProtocolBinary encryption
  // - you may set this global value to repCheckedIfAvailable if you are
  // really paranoid (but resulting security may be lower, since the IV is
  // somewhat more predictable than plain random)
  WebSocketsIVReplayAttackCheck: TAESIVReplayAttackCheck = repNoCheck;

  /// the allowed maximum size, in MB, of a WebSockets frame
  WebSocketsMaxFrameMB: cardinal = 256;



implementation

{ -------------- high-level SynCrtSock classes depending on SynCommons }

{ THttpRequestCached }

constructor THttpRequestCached.Create(const aURI: RawUTF8;
  aKeepAliveSeconds, aTimeoutSeconds: integer; const aToken: RawUTF8;
  aHttpClass: THttpRequestClass);
begin
  inherited Create;
  fKeepAlive := aKeepAliveSeconds*1000;
  if aTimeoutSeconds > 0 then // 0 means no cache
    fCache := TSynDictionary.Create(TypeInfo(TRawUTF8DynArray),
      TypeInfo(THttpRequestCacheDynArray),true,aTimeoutSeconds);
  if not LoadFromURI(aURI,aToken,aHttpClass) then
    raise ESynException.CreateUTF8('%.Create: invalid aURI=%',[self,aURI]);
end;

procedure THttpRequestCached.Clear;
begin
  FreeAndNil(fHttp); // either fHttp or fSocket is used
  FreeAndNil(fSocket);
  if fCache <> nil then
    fCache.DeleteAll;
  fURI.Clear;
  fTokenHeader := '';
end;

destructor THttpRequestCached.Destroy;
begin
  fCache.Free;
  fHttp.Free;
  fSocket.Free;
  inherited Destroy;
end;

function THttpRequestCached.Get(const aAddress: SockString;
  aModified: PBoolean; aStatus: PInteger): SockString;
var cache: THttpRequestCache;
    headin, headout: SockString;
    status: integer;
    modified: boolean;
begin
  result := '';
  if (fHttp=nil) and (fSocket=nil) then // either fHttp or fSocket is used
    exit;
  if (fCache <> nil) and fCache.FindAndCopy(aAddress,cache) then
    FormatUTF8('If-None-Match: %',[cache.Tag],RawUTF8(headin));
  if fTokenHeader<>'' then begin
    if headin<>'' then
      headin := headin+#13#10;
    headin := headin+fTokenHeader;
  end;
  if fSocket<>nil then begin
    if connectionClose in fSocket.HeaderFlags then begin
      // server may close after a few requests (e.g. nginx keepalive_requests)
      FreeAndNil(fSocket);
      fSocket := THttpClientSocket.Open(fURI.Server,fURI.Port)
    end;
    status := fSocket.Get(aAddress,fKeepAlive,headin);
    result := fSocket.Content;
  end else
    status := fHttp.Request(aAddress,'GET',fKeepAlive,headin,'','',headout,result);
  modified := true;
  case status of
  STATUS_SUCCESS:
    if fCache <> nil then begin
      if fHttp <> nil then
        FindNameValue(headout,'ETAG:',cache.Tag) else
        cache.Tag := fSocket.HeaderGetValue('ETAG');
      if cache.Tag <> '' then begin
        cache.Content := result;
        fCache.AddOrUpdate(aAddress,cache);
      end;
    end;
  STATUS_NOTMODIFIED: begin
    result := cache.Content;
    modified := false;
  end;
  end;
  if aModified<>nil then
    aModified^ := modified;
  if aStatus<>nil then
    aStatus^ := status;
end;

function THttpRequestCached.LoadFromURI(const aURI, aToken: RawUTF8;
  aHttpClass: THttpRequestClass): boolean;
begin
  result := false;
  if (self=nil) or (fHttp<>nil) or (fSocket<>nil) or not fURI.From(aURI) then
    exit;
  fTokenHeader := AuthorizationBearer(aToken);
  if aHttpClass=nil then begin
    {$ifdef USEWININET}
    aHttpClass := TWinHTTP;
    {$else}
    {$ifdef USELIBCURL}
    if fURI.Https then
      aHttpClass := TCurlHTTP;
    {$endif}
    {$endif}
  end;
  if aHttpClass=nil then
    fSocket := THttpClientSocket.Open(fURI.Server,fURI.Port) else
    fHttp := aHttpClass.Create(fURI.Server,fURI.Port,fURI.Https);
  result := true;
end;

function THttpRequestCached.Flush(const aAddress: SockString): boolean;
begin
  if fCache <> nil then
    result := fCache.Delete(aAddress)>=0 else
    result := true;
end;


function PurgeHeaders(P: PUTF8Char): RawUTF8;
var
  tmp: TTextWriterStackBuffer;
  next: PUTF8Char;
  W: TTextWriter;
begin
  result := '';
  W := nil;
  try
    while P <> nil do begin
      next := GotoNextLine(P);
      if IdemPCharArray(P, ['CONTENT-', 'CONNECTION:', 'KEEP-ALIVE:', 'TRANSFER-',
         'X-POWERED', 'USER-AGENT', 'REMOTEIP:', 'HOST:', 'ACCEPT:']) < 0 then begin
        if W = nil then
          W := TTextWriter.CreateOwnedStream(tmp);
        if next=nil then
          W.AddNoJSONEscape(P) else
          W.AddNoJSONEscape(P, next - P);
      end;
      P := next;
    end;
    if W <> nil then
      W.SetText(result);
  finally
    W.Free;
  end;
end;


{ -------------- WebSockets shared classes for bidirectional remote access }

var
  _TWebSocketFrameOpCode: array[TWebSocketFrameOpCode] of PShortString;
  _TWebSocketProcessNotifyCallback: array[TWebSocketProcessNotifyCallback] of PShortString;

function ToText(opcode: TWebSocketFrameOpCode): PShortString;
begin
  result := _TWebSocketFrameOpCode[opcode];
end;

function ToText(mode: TWebSocketProcessNotifyCallback): PShortString;
begin
  result := _TWebSocketProcessNotifyCallback[mode];
end;

function ToText(st: TWebSocketProcessClientThreadState): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessClientThreadState),ord(st));
end;

function ToText(ev: TPollSocketEvent): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TPollSocketEvent),ord(ev));
end;

function ToText(st: TWebSocketProcessState): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessState),ord(st));
end;


const
  STATUS_WEBSOCKETCLOSED = 0;

procedure ComputeChallenge(const Base64: RawByteString; out Digest: TSHA1Digest);
const SALT: string[36] = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
var SHA: TSHA1;
begin
  SHA.Init;
  SHA.Update(pointer(Base64),length(Base64));
  SHA.Update(@SALT[1],36);
  SHA.Final(Digest);
end;


{ TWebSocketProcessSettings }

procedure TWebSocketProcessSettings.SetDefaults;
begin
  HeartbeatDelay := 0;
  LoopDelay := 500;
  SendDelay := 10;
  DisconnectAfterInvalidHeartbeatCount := 5;
  CallbackAcquireTimeOutMS := 5000;
  CallbackAnswerTimeOutMS := 5000;
  LogDetails := [];
  OnClientConnected := nil;
  OnClientDisconnected := nil;
end;

procedure TWebSocketProcessSettings.SetFullLog;
begin
  LogDetails := [logHeartbeat,logTextFrameContent,logBinaryFrameContent];
end;


{ TWebSocketProtocol }

constructor TWebSocketProtocol.Create(const aName,aURI: RawUTF8);
begin
  fName := aName;
  fURI := aURI;
end;

procedure TWebSocketProtocol.SetEncryptKey(aServer: boolean; const aKey: RawUTF8);
var key: TSHA256Digest;
begin
  if aKey='' then
    fEncryption := nil else begin
    fEncryption := TECDHEProtocol.FromKey(aKey,aServer);
    if fEncryption=nil then begin
      SHA256Weak(aKey,key); // fallback to TProtocolAES/TAESCFB
      SetEncryptKeyAES(key,256);
    end;
  end;
end;

procedure TWebSocketProtocol.SetEncryptKeyAES(const aKey; aKeySize: cardinal);
begin
  if aKeySize>=128 then
    fEncryption := TProtocolAES.Create(
      TAESCFB,aKey,aKeySize,WebSocketsIVReplayAttackCheck);
end;

procedure TWebSocketProtocol.AfterGetFrame(var frame: TWebSocketFrame);
begin
  inc(fFramesInCount);
  inc(fFramesInBytes,length(frame.payload)+2);
end;

procedure TWebSocketProtocol.BeforeSendFrame(var frame: TWebSocketFrame);
begin
  inc(fFramesOutCount);
  inc(fFramesOutBytes,length(frame.payload)+2);
end;

function TWebSocketProtocol.FrameData(const frame: TWebSocketFrame;
  const Head: RawUTF8; HeadFound: PRawUTF8): pointer;
begin
  result := nil; // no frame type by default
end;

function TWebSocketProtocol.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
begin
  result := '*'; // no frame URI by default
end;

function TWebSocketProtocol.ProcessHandshake(const ExtIn: TRawUTF8DynArray;
  out ExtOut: RawUTF8; ErrorMsg: PRawUTF8): boolean;
var res: TProtocolResult;
    msgin,msgout: RawUTF8;
    synhk: boolean;
    i: integer;
begin
  result := fEncryption=nil;
  if result then
    exit; // nothing to handshake for -> try to continue
  synhk := false;
  if ExtIn<>nil then begin
    for i := 0 to length(ExtIn)-1 do
      if IdemPropNameU(ExtIn[i],'synhk') then
        synhk := true else
      if synhk and IdemPChar(pointer(ExtIn[i]),'HK=') then begin
        msgin := copy(ExtIn[i],4,maxInt);
        break;
      end;
    if (msgin='') or not synhk then
      exit;
  end;
  res := fEncryption.ProcessHandshake(msgin,msgout);
  case res of
  sprSuccess: begin
    AddToCSV('synhk; hk='+msgout,ExtOut,'; ');
    result := true;
    exit;
  end;
  sprUnsupported:
    if not synhk then begin
      result := true; // try to continue execution
      exit;
    end;
  end;
  WebSocketLog.Add.Log(sllWarning,'ProcessHandshake=% In=[%]',[ToText(res)^,msgin],self);
  if ErrorMsg<>nil then
    ErrorMsg^ := FormatUTF8('%: %',[ErrorMsg^,
      GetCaptionFromEnum(TypeInfo(TProtocolResult),ord(res))]);
end;

function TWebSocketProtocol.ProcessURI(const aClientURI: RawUTF8): boolean;
begin
  result := true; // override and return false to return STATUS_UNAUTHORIZED
end;

function TWebSocketProtocol.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
var i,n: integer;
begin // this default implementation will send all frames one by one
  n := FramesCount;
  if (n>0) and (Owner<>nil) then begin
    result := false;
    FramesCount := 0;
    for i := 0 to n-1 do
      if Owner.SendFrame(Frames[i]) then
        Frames[i].payload := '' else
        exit;
  end;
  result := true;
end;

function TWebSocketProtocol.GetEncrypted: boolean;
begin
  result := (self<>nil) and (fEncryption<>nil);
end;

function TWebSocketProtocol.GetSubprotocols: RawUTF8;
begin
  result := fName;
end;

function TWebSocketProtocol.SetSubprotocol(const aProtocolName: RawUTF8): boolean;
begin
  result := IdemPropNameU(aProtocolName,fName);
end;

function TWebSocketProtocol.GetRemoteIP: SockString;
begin
  if (self = nil) or fRemoteLocalhost then
    result := '' else
    result := fRemoteIP;
end;


{ TWebSocketFrameList }

constructor TWebSocketFrameList.Create(timeoutsec: integer);
begin
  inherited Create;
  fTimeoutSec := timeoutsec;
end;

function TWebSocketFrameList.AnswerToIgnore(incr: integer): integer;
begin
  Safe^.Lock;
  if incr<>0 then
    inc(Safe^.Padding[0].VInteger,incr);
  result := Safe^.Padding[0].VInteger;
  Safe^.UnLock;
end;

function TWebSocketFrameList.Pop(protocol: TWebSocketProtocol; const head: RawUTF8;
  out frame: TWebSocketFrame): boolean;
var i: integer;
    tix: cardinal;
    item: PWebSocketFrame;
begin
  result := false;
  if (self=nil) or (Count=0) or (head='') or (protocol=nil) then
    exit;
  if fTimeoutSec=0 then
    tix := 0 else
    tix := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64 shr 10;
  Safe.Lock;
  try
    for i := Count-1 downto 0 do begin
      item := @List[i];
      if protocol.FrameData(item^,head)<>nil then begin
        result := true;
        frame := item^;
        Delete(i);
        exit;
      end else
      if (tix>0) and (tix>item^.tix) then
        Delete(i);
    end;
  finally
    Safe.UnLock;
  end;
end;

procedure TWebSocketFrameList.Push(const frame: TWebSocketFrame);
begin
  if self=nil then
    exit;
  Safe.Lock;
  try
    if Count>=length(List) then
      SetLength(List,Count+Count shr 3+8);
    List[Count] := frame;
    if fTimeoutSec>0 then
      List[Count].tix := fTimeoutSec+
        ({$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64 shr 10);
    inc(Count);
  finally
    Safe.UnLock;
  end;
end;

procedure TWebSocketFrameList.PushVoidFrame(opcode: TWebSocketFrameOpCode);
var frame: TWebSocketFrame;
begin
  frame.opcode := opcode;
  frame.content := [];
  Push(frame);
end;

procedure TWebSocketFrameList.Delete(i: integer);
begin // faster than a TDynArray which will release the memory
  List[i].payload := '';
  dec(Count);
  if i<Count then begin
    MoveFast(List[i+1],List[i],(Count-i)*sizeof(List[i]));
    pointer(List[Count].payload) := nil;
  end;
end;


{ TWebSocketProtocolChat }

constructor TWebSocketProtocolChat.Create(const aName, aURI: RawUTF8;
  const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame);
begin
  inherited Create(aName,aURI);
  fOnIncomingFrame := aOnIncomingFrame;
end;

function TWebSocketProtocolChat.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName,fURI);
  if fEncryption<>nil then
    result.fEncryption := fEncryption.Clone;
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

procedure TWebSocketProtocolChat.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
begin
  if Assigned(OnInComingFrame) then
    try
      if Sender.InheritsFrom(TWebSocketProcessServer) then
        OnIncomingFrame(TWebSocketProcessServer(Sender).fServerResp,request) else
        OnIncomingFrame(nil,request);
    except
      // ignore any exception in the callback
    end;
end;

function TWebSocketProtocolChat.SendFrame(Sender: THttpServerResp;
  const frame: TWebSocketFrame): boolean;
var tmp: TWebSocketFrame; // SendFrame() may change frame content (e.g. mask)
begin
  result := false;
  if (self=nil) or (Sender=nil) or Sender.Terminated or
     not (Frame.opcode in [focText,focBinary]) or
     ((Sender.Server as TWebSocketServer).IsActiveWebSocketThread(Sender)<>Sender) then
    exit;
  tmp.opcode := frame.opcode;
  tmp.content := frame.content;
  SetString(tmp.payload,PAnsiChar(Pointer(frame.payload)),length(frame.payload));
  result := (Sender as TWebSocketServerResp).fProcess.SendFrame(tmp)
end;

function TWebSocketProtocolChat.SendFrameJson(Sender: THttpServerResp;
  var JSON: RawUTF8): boolean;
var frame: TWebSocketFrame;
begin
  result := false;
  if (self=nil) or (Sender=nil) or Sender.Terminated or
     ((Sender.Server as TWebSocketServer).IsActiveWebSocketThread(Sender)<>Sender) then
    exit;
  frame.opcode := focText;
  frame.content := [];
  frame.payload := JSON;
  result := (Sender as TWebSocketServerResp).fProcess.SendFrame(frame)
end;


{ TWebSocketProtocolRest }

procedure TWebSocketProtocolRest.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
var Ctxt: THttpServerRequest;
    onRequest: TOnHttpServerRequest;
    status: cardinal;
    noAnswer: boolean;
    answer: TWebSocketFrame;
    head: RawUTF8;
begin
  if not (request.opcode in [focText,focBinary]) then
    exit; // ignore e.g. from TWebSocketServerResp.ProcessStart/ProcessStop
  if FrameData(request,'r',@head)<>nil then
  try
    Ctxt := Sender.ComputeContext(onRequest);
    try
      if (Ctxt=nil) or not Assigned(onRequest) then
        raise EWebSockets.CreateUTF8('%.ProcessOne: onRequest=nil',[self]);
      if (head='') or not FrameToInput(request,noAnswer,Ctxt) then
        raise EWebSockets.CreateUTF8('%.ProcessOne: invalid frame',[self]);
      request.payload := ''; // release memory ASAP
      if info<>'' then
        Ctxt.AddInHeader(info);
      status := onRequest(Ctxt); // blocking call to compute the answer
      if (Ctxt.OutContentType=HTTP_RESP_NORESPONSE) or noAnswer then
        exit;
      OutputToFrame(Ctxt,status,head,answer);
      if not Sender.SendFrame(answer) then
        fLastError := UTF8ToString(FormatUTF8('SendFrame error %',[Sender]));
    finally
      Ctxt.Free;
    end;
  except
    on E: Exception do
      FormatString('% [%]',[E.ClassName,E.Message],fLastError);
  end else
  if (Sender.fIncoming.AnswerToIgnore>0) and (FrameData(request,'answer')<>nil) then begin
    Sender.fIncoming.AnswerToIgnore(-1);
    Sender.Log(request,'Ignored answer after NotifyCallback TIMEOUT',sllWarning);
  end else
    Sender.fIncoming.Push(request); // e.g. asynch 'answer'
end;

// by convention, defaults are POST and JSON, to reduce frame size for SOA calls

procedure TWebSocketProtocolRest.InputToFrame(Ctxt: THttpServerRequest;
  aNoAnswer: boolean; out request: TWebSocketFrame; out head: RawUTF8);
var Method,InContentType: RawByteString;
    seq: integer;
begin
  if not IdemPropNameU(Ctxt.Method,'POST') then
    Method := Ctxt.Method;
  if (Ctxt.InContent<>'') and (Ctxt.InContentType<>'') and
     not IdemPropNameU(Ctxt.InContentType,JSON_CONTENT_TYPE) then
    InContentType := Ctxt.InContentType;
  if fSequencing then begin
    seq := InterlockedIncrement(fSequence);
    SetLength(head,7); // safe overlap after 16,777,216 frames
    PAnsiChar(pointer(head))^ := 'r';
    BinToHexDisplay(@seq,PAnsiChar(pointer(head))+1,3);
  end else
    head := 'request';
  FrameCompress(head,[Method,Ctxt.URL,Ctxt.InHeaders,ord(aNoAnswer)],
    Ctxt.InContent,InContentType,request);
  if fSequencing then
    head[1] := 'a' else
    head := 'answer';
end;

function TWebSocketProtocolRest.FrameToInput(var request: TWebSocketFrame;
  out aNoAnswer: boolean; Ctxt: THttpServerRequest): boolean;
var URL,Method,InHeaders,NoAnswer,InContentType,InContent: RawByteString;
begin
  result := FrameDecompress(request,'r',[@Method,@URL,@InHeaders,@NoAnswer],
    InContentType,InContent);
  if result then begin
    if (InContentType='') and (InContent<>'') then
      InContentType := JSON_CONTENT_TYPE_VAR;
    if Method='' then
      Method := 'POST';
    Ctxt.Prepare(URL,Method,InHeaders,InContent,InContentType,fRemoteIP,Ctxt.UseSSL);
    aNoAnswer := NoAnswer='1';
  end;
end;

procedure TWebSocketProtocolRest.OutputToFrame(Ctxt: THttpServerRequest;
  Status: Cardinal; var outhead: RawUTF8; out answer: TWebSocketFrame);
var OutContentType: RawByteString;
begin
  if (Ctxt.OutContent<>'') and
     not IdemPropNameU(Ctxt.OutContentType,JSON_CONTENT_TYPE) then
    OutContentType := Ctxt.OutContentType;
  if NormToUpperAnsi7[outhead[3]]='Q' then
    outhead := 'answer' else // 'request' -> 'answer'
    outhead[1] := 'a';       // 'r000001' -> 'a000001'
  FrameCompress(outhead,[Status,Ctxt.OutCustomHeaders],
    Ctxt.OutContent,OutContentType,answer);
end;

function TWebSocketProtocolRest.FrameToOutput(
  var answer: TWebSocketFrame; Ctxt: THttpServerRequest): cardinal;
var status,outHeaders,outContentType,outContent: RawByteString;
begin
  result := STATUS_NOTFOUND;
  if not FrameDecompress(answer,'a',[@status,@outHeaders],outContentType,outContent) then
    exit;
  result := GetInteger(pointer(status));
  Ctxt.OutCustomHeaders := outHeaders;
  if (outContentType='') and (outContent<>'') then
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR else
    Ctxt.OutContentType := outContentType;
  Ctxt.OutContent := outContent;
end;


{ TWebSocketProtocolJSON }

constructor TWebSocketProtocolJSON.Create(const aURI: RawUTF8);
begin
  inherited Create('synopsejson',aURI);
end;

function TWebSocketProtocolJSON.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolJSON.Create(fURI);
end;

procedure TWebSocketProtocolJSON.FrameCompress(const Head: RawUTF8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var WR: TTextWriter;
    tmp: TTextWriterStackBuffer;
    i: integer;
begin
  frame.opcode := focText;
  frame.content := [];
  WR := TTextWriter.CreateOwnedStream(tmp);
  try
    WR.Add('{');
    WR.AddFieldName(Head);
    WR.Add('[');
    for i := 0 to High(Values) do begin
      WR.AddJSONEscape(Values[i]);
      WR.Add(',');
    end;
    WR.Add('"');
    WR.AddString(ContentType);
    WR.Add('"',',');
    if Content='' then
      WR.Add('"','"') else
    if (ContentType='') or
       IdemPropNameU(ContentType,JSON_CONTENT_TYPE) then
      WR.AddNoJSONEscape(pointer(Content),length(Content)) else
    if IdemPChar(pointer(ContentType),'TEXT/') then
      WR.AddCSVUTF8([Content]) else
      WR.WrBase64(pointer(Content),length(Content),true);
    WR.Add(']','}');
    WR.SetText(RawUTF8(frame.payload));
  finally
    WR.Free;
  end;
end;

function TWebSocketProtocolJSON.FrameData(const frame: TWebSocketFrame;
  const Head: RawUTF8; HeadFound: PRawUTF8): pointer;
var P,txt: PUTF8Char;
    len: integer;
begin
  result := nil;
  if (length(frame.payload)<10) or (frame.opcode<>focText) then
    exit;
  P := pointer(frame.payload);
  if not NextNotSpaceCharIs(P,'{') then
    exit;
  while P^<>'"' do begin
    inc(P);
    if P^=#0 then exit;
  end;
  txt := P+1;
  P := GotoEndOfJSONString(P); // here P^ should be '"'
  len := length(Head);
  if (P^<>#0) and (P-txt>=len) and CompareMem(pointer(Head),txt,len) then begin
    result := P+1;
    if HeadFound<>nil then
      SetString(HeadFound^,txt,P-txt);
  end;
end;

function TWebSocketProtocolJSON.FrameDecompress(
  const frame: TWebSocketFrame; const Head: RawUTF8;
  const values: array of PRawByteString;
  var contentType,content: RawByteString): Boolean;
var i: Integer;
    P: PUTF8Char;
    b64: PUTF8Char;
    b64len: integer;
  procedure GetNext(var content: RawByteString);
  var txt: PUTF8Char;
      txtlen: integer;
  begin
    txt := GetJSONField(P,P,nil,nil,@txtlen);
    SetString(content,txt,txtlen);
  end;
begin
  result := false;
  P := FrameData(frame,Head);
  if P=nil then
    exit;
  if not NextNotSpaceCharIs(P,':') or
     not NextNotSpaceCharIs(P,'[') then
    exit;
  for i := 0 to high(values) do
    GetNext(values[i]^);
  GetNext(contentType);
  if P=nil then
    exit;
  if (contentType='') or IdemPropNameU(contentType,JSON_CONTENT_TYPE) then
    GetJSONItemAsRawJSON(P,RawJSON(content)) else
    if IdemPChar(pointer(contentType),'TEXT/') then
      GetNext(content) else begin
      b64 := GetJSONField(P,P,nil,nil,@b64len);
      if not Base64MagicCheckAndDecode(b64,b64len,content) then
        exit;
    end;
  result := true;
end;

function TWebSocketProtocolJSON.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
var P,txt: PUTF8Char;
begin
  result := '*';
  if (length(frame.payload)<10) or (frame.opcode<>focText) then
    exit;
  P := pointer(frame.payload);
  if not NextNotSpaceCharIs(P,'{') or
     not NextNotSpaceCharIs(P,'"') then
    exit;
  txt := P;
  P := GotoEndOfJSONString(P);
  SetString(result,txt,P-Txt);
end;


{ TWebSocketProtocolBinary }

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  aCompressed: boolean);
begin
  inherited Create('synopsebin',aURI);
  fCompressed := aCompressed;
end;

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  const aKey; aKeySize: cardinal; aCompressed: boolean);
begin
  Create(aURI,aCompressed);
  SetEncryptKeyAES(aKey, aKeySize);
end;

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  aServer: boolean; const aKey: RawUTF8; aCompressed: boolean);
begin
  Create(aURI,aCompressed);
  SetEncryptKey(aServer,aKey);
end;

function TWebSocketProtocolBinary.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolBinary.Create(fURI,{dummykey=}self,0,fCompressed);
  TWebSocketProtocolBinary(result).fSequencing := fSequencing;
  if fEncryption<>nil then
    result.fEncryption := fEncryption.Clone;
end;

const
  FRAME_HEAD_SEP = #1;

procedure FrameInit(opcode: TWebSocketFrameOpCode; const Content, ContentType: RawByteString;
  out frame: TWebSocketFrame);
begin
  frame.opcode := opcode;
  if (ContentType<>'') and (Content<>'') and
     not IdemPChar(pointer(ContentType),'TEXT/') and
     IsContentCompressed(pointer(Content),length(Content)) then
    frame.content := [fopAlreadyCompressed] else
    frame.content := [];
end;

procedure TWebSocketProtocolBinary.FrameCompress(const Head: RawUTF8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var item: RawUTF8;
    i: integer;
    W: TFileBufferWriter;
begin
  FrameInit(focBinary,Content,ContentType,frame);
  W := TFileBufferWriter.Create(TRawByteStringStream);
  try
    W.WriteBinary(Head);
    W.Write1(byte(FRAME_HEAD_SEP));
    for i := 0 to high(Values) do
    with Values[i] do begin
      VarRecToUTF8(Values[i],item);
      W.Write(item);
    end;
    W.Write(ContentType);
    W.WriteBinary(Content);
    W.Flush;
    frame.payload := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

function TWebSocketProtocolBinary.FrameData(const frame: TWebSocketFrame;
  const Head: RawUTF8; HeadFound: PRawUTF8): pointer;
var len: integer;
    P: PAnsiChar;
begin
  P := pointer(frame.payload);
  len := length(Head);
  if (frame.opcode=focBinary) and (length(frame.payload)>=len+6) and
     CompareMemSmall(pointer(Head),P,len) then begin
    result := PosChar(PUTF8Char(P)+len,FRAME_HEAD_SEP);
    if result<>nil then begin
      if HeadFound<>nil then
        SetString(HeadFound^,P,PAnsiChar(result)-P);
      inc(PByte(result));
    end;
  end else
    result := nil;
end;

function TWebSocketProtocolBinary.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
var i: integer;
begin
  if (length(frame.payload)<10) or (frame.opcode<>focBinary) then
    i := 0 else
    i := PosExChar(FRAME_HEAD_SEP,frame.payload);
  if i=0 then
    result := '*' else
    result := copy(frame.payload,1,i-1);
end;

procedure TWebSocketProtocolBinary.BeforeSendFrame(var frame: TWebSocketFrame);
var value: RawByteString;
    threshold: integer;
begin
  inherited BeforeSendFrame(frame);
  if frame.opcode=focBinary then begin
    if fCompressed then begin
      if fRemoteLocalhost or (fopAlreadyCompressed in frame.content) then
        threshold := maxInt else // localhost or compressed -> no SynLZ
        threshold := WebSocketsBinarySynLzThreshold;
      SynLZCompress(pointer(frame.payload),length(frame.payload),value,threshold);
    end else
      value := frame.payload;
    if fEncryption<>nil then
      fEncryption.Encrypt(value,frame.payload) else
      frame.payload := value;
  end;
  inc(fFramesOutBytesSocket,length(frame.payload)+2);
end;

procedure TWebSocketProtocolBinary.AfterGetFrame(var frame: TWebSocketFrame);
var value: RawByteString;
    res: TProtocolResult;
begin
  inc(fFramesInBytesSocket,length(frame.payload)+2);
  if frame.opcode=focBinary then begin
    if fEncryption<>nil then begin
      res := fEncryption.Decrypt(frame.payload,value);
      if res<>sprSuccess then
        raise EWebSockets.CreateUTF8('%.AfterGetFrame: encryption error %',
          [self,ToText(res)^]);
     end else
      value := frame.payload;
    if fCompressed then
      SynLZDecompress(pointer(value),length(value),frame.payload) else
      frame.payload := value;
  end;
  inherited AfterGetFrame(frame);
end;

function TWebSocketProtocolBinary.FrameDecompress(const frame: TWebSocketFrame;
  const Head: RawUTF8; const values: array of PRawByteString;
  var contentType,content: RawByteString): Boolean;
var i: integer;
    P: PByte;
begin
  result := false;
  P := FrameData(frame,Head);
  if P=nil then
    exit;
  for i := 0 to high(values) do
    values[i]^ := FromVarString(P);
  contentType := FromVarString(P);
  i := length(frame.payload)-(PAnsiChar(P)-pointer(frame.payload));
  if i<0 then
    exit;
  SetString(content,PAnsiChar(P),i);
  result := true;
end;

function TWebSocketProtocolBinary.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
const JUMBO_HEADER: array[0..6] of AnsiChar = 'frames'+FRAME_HEAD_SEP;
var jumboFrame: TWebSocketFrame;
    i, len: integer;
    P: PByte;
begin
  if (FramesCount=0) or (Owner=nil) then begin
    result := true;
    exit;
  end;
  dec(FramesCount);
  if FramesCount=0 then begin
    result := Owner.SendFrame(Frames[0]);
    exit;
  end;
  jumboFrame.opcode := focBinary;
  jumboFrame.content := [];
  len := sizeof(JUMBO_HEADER)+ToVarUInt32Length(FramesCount);
  for i := 0 to FramesCount do
    if Frames[i].opcode=focBinary then
      inc(len,ToVarUInt32LengthWithData(length(Frames[i].payload))) else
      raise EWebSockets.CreateUTF8('%.SendFrames[%]: Unexpected opcode=%',
        [self,i,ord(Frames[i].opcode)]);
  SetString(jumboframe.payload,nil,len);
  P := pointer(jumboframe.payload);
  MoveFast(JUMBO_HEADER,P^,SizeOf(JUMBO_HEADER));
  inc(P,SizeOf(JUMBO_HEADER));
  P := ToVarUInt32(FramesCount,P);
  for i := 0 to FramesCount do begin
    len := length(Frames[i].payload);
    P := ToVarUInt32(len,P);
    MoveFast(pointer(Frames[i].payload)^,P^,len);
    inc(P,len);
  end;
  FramesCount := 0;
  Frames := nil;
  result := Owner.SendFrame(jumboFrame); // send all frames at once
end;

procedure TWebSocketProtocolBinary.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
var jumboInfo: RawByteString;
    n,i: integer;
    frame: TWebSocketFrame;
    P: PByte;
begin
  P := FrameData(request,'frames');
  if P<>nil then begin
    n := FromVarUInt32(P);
    for i := 0 to n do begin
      if i=0 then
        jumboInfo:= 'Sec-WebSocket-Frame: [0]' else
      if i=n then
        jumboInfo := 'Sec-WebSocket-Frame: [1]' else
        jumboInfo := '';
      frame.opcode := focBinary;
      frame.content := [];
      frame.payload := FromVarString(P);
      Sender.Log(frame,FormatUTF8('GetSubFrame(%/%)',[i+1,n+1]));
      inherited ProcessIncomingFrame(Sender,frame,jumboInfo);
    end;
  end else
    inherited ProcessIncomingFrame(Sender,request,info);
end;

function TWebSocketProtocolBinary.GetFramesInCompression: integer;
begin
  if (self=nil) or (fFramesInBytes=0) then
    result := 100 else
    if not fCompressed or (fFramesInBytesSocket<fFramesInBytes) then
      result := 0 else
      result := 100-(fFramesInBytesSocket*100) div fFramesInBytes;
end;

function TWebSocketProtocolBinary.GetFramesOutCompression: integer;
begin
  if (self=nil) or (fFramesOutBytes=0) then
    result := 100 else
    if not fCompressed or (fFramesOutBytesSocket<=fFramesOutBytes) then
      result := 0 else
      result := 100-(fFramesOutBytesSocket*100) div fFramesOutBytes;
end;

function TWebSocketProtocolBinary.GetSubprotocols: RawUTF8;
begin
  result := 'synopsebin, synopsebinary';
end;

function TWebSocketProtocolBinary.SetSubprotocol(
  const aProtocolName: RawUTF8): boolean;
begin
  case FindPropName(['synopsebin', 'synopsebinary'], aProtocolName) of
  0: fSequencing := true;
  1: fSequencing := false;
  else begin
    result := false;
    exit;
  end;
  end;
  result := true;
end;


{ TWebSocketProtocolList }

function TWebSocketProtocolList.CloneByName(
  const aProtocolName, aClientURI: RawUTF8): TWebSocketProtocol;
var i: Integer;
begin
  result := nil;
  if self=nil then
    exit;
  fSafe.Lock;
  try
    for i := 0 to length(fProtocols)-1 do
      with fProtocols[i] do
      if ((fURI='') or IdemPropNameU(fURI,aClientURI)) and
         SetSubprotocol(aProtocolName) then begin
        result := fProtocols[i].Clone(aClientURI);
        result.fName := aProtocolName;
        exit;
      end;
  finally
    fSafe.UnLock;
  end;
end;

function TWebSocketProtocolList.CloneByURI(const aClientURI: RawUTF8): TWebSocketProtocol;
var i: integer;
begin
  result := nil;
  if (self=nil) or (aClientURI='') then
    exit;
  fSafe.Lock;
  try
    for i := 0 to length(fProtocols)-1 do
      if IdemPropNameU(fProtocols[i].fURI,aClientURI) then begin
        result := fProtocols[i].Clone(aClientURI);
        exit;
      end;
  finally
    fSafe.UnLock;
  end;
end;

function TWebSocketProtocolList.Count: integer;
begin
  if self=nil then
    result := 0 else
    result := length(fProtocols);
end;

destructor TWebSocketProtocolList.Destroy;
begin
  ObjArrayClear(fProtocols);
  inherited;
end;

function TWebSocketProtocolList.FindIndex(const aName,aURI: RawUTF8): integer;
begin
  if aName<>'' then
    for result := 0 to high(fProtocols) do
      with fProtocols[result] do
      if IdemPropNameU(fName,aName) and
         ((fURI='') or IdemPropNameU(fURI,aURI)) then
        exit;
  result := -1;
end;

function TWebSocketProtocolList.Add(aProtocol: TWebSocketProtocol): boolean;
var i: Integer;
begin
  result := false;
  if aProtocol=nil then
    exit;
  fSafe.Lock;
  try
    i := FindIndex(aProtocol.Name,aProtocol.URI);
    if i<0 then begin
      ObjArrayAdd(fProtocols,aProtocol);
      result := true;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TWebSocketProtocolList.AddOnce(aProtocol: TWebSocketProtocol): boolean;
var i: Integer;
begin
  result := false;
  if aProtocol=nil then
    exit;
  fSafe.Lock;
  try
    i := FindIndex(aProtocol.Name,aProtocol.URI);
    if i<0 then begin
      ObjArrayAdd(fProtocols,aProtocol);
      result := true;
    end else begin
      fProtocols[i].Free;
      fProtocols[i] := aProtocol;
    end;
  finally
    fSafe.UnLock;
  end;
end;

function TWebSocketProtocolList.Remove(const aProtocolName,aURI: RawUTF8): Boolean;
var i: Integer;
begin
  fSafe.Lock;
  try
    i := FindIndex(aProtocolName,aURI);
    if i>=0 then begin
      ObjArrayDelete(fProtocols,i);
      result := true;
    end else
      result := false;
  finally
    fSafe.UnLock;
  end;
end;


{ TWebSocketProcess }

constructor TWebSocketProcess.Create(aProtocol: TWebSocketProtocol;
  aOwnerConnection: THttpServerConnectionID; aOwnerThread: TSynThread;
  const aSettings: TWebSocketProcessSettings; const aProcessName: RawUTF8);
begin
  inherited Create;
  fProcessName := aProcessName;
  fProtocol := aProtocol;
  fOwnerConnection := aOwnerConnection;
  fOwnerThread := aOwnerThread;
  fSettings := aSettings;
  fIncoming := TWebSocketFrameList.Create(30*60);
  fOutgoing := TWebSocketFrameList.Create(0);
  fSafeIn := NewSynLocker;
  fSafeOut := NewSynLocker;
  fSafePing := NewSynLocker;
end;

destructor TWebSocketProcess.Destroy;
var frame: TWebSocketFrame;
    timeout: Int64;
    log: ISynLog;
    dummyerror: integer;
begin
  log := WebSocketLog.Enter('Destroy %',[ToText(fState)^],self);
  if fState = wpsCreate then
    fProcessEnded := true else
    if not fNoConnectionCloseAtDestroy then begin
      if log<>nil then
        log.Log(sllTrace,'Destroy: notify focConnectionClose',self);
      InterlockedIncrement(fProcessCount);
      try
        fState := wpsDestroy;
        if fOutgoing.Count>0 then
          SendPendingOutgoingFrames;
        frame.opcode := focConnectionClose;
        dummyerror := 0;
        if not SendFrame(frame) or not CanGetFrame(1000,@dummyerror) or
           not GetFrame(frame,@dummyerror) then
            if log<>nil then // expects an answer from peer
              log.Log(sllWarning,'Destroy: no focConnectionClose ACK %',[dummyerror],self);
      finally
        InterlockedDecrement(fProcessCount);
      end;
    end;
  fState := wpsDestroy;
  if (fProcessCount>0) or not fProcessEnded then begin
    if log<>nil then
      log.Log(sllDebug,'Destroy: wait for fProcessCount=%',[fProcessCount],self);
    timeOut := GetTickCount64+5000;
    repeat
      SleepHiRes(2);
    until ((fProcessCount=0) and fProcessEnded) or (GetTickCount64>timeOut);
    if log<>nil then
      log.Log(sllDebug,'Destroy: waited fProcessCount=%',[fProcessCount],self);
  end;
  fProtocol.Free;
  fOutgoing.Free;
  fIncoming.Free;
  fSafeIn.DoneAndFreeMem;
  fSafeOut.DoneAndFreeMem;
  fSafePing.DoneAndFreeMem; // to be done lately to avoid GPF in above Destroy
  inherited Destroy;
end;

procedure TWebSocketProcess.ProcessStart;
var frame: TWebSocketFrame; // notify e.g. TOnWebSocketProtocolChatIncomingFrame
begin
  if Assigned(fSettings.OnClientConnected) then
    try
      WebSocketLog.Add.Log(sllTrace,'ProcessStart: OnClientConnected',self);
      fSettings.OnClientConnected(Self);
    except
    end;
  WebSocketLog.Add.Log(sllTrace,'ProcessStart: callbacks',self);
  frame.opcode := focContinuation;
  if not Assigned(fProtocol.fOnBeforeIncomingFrame) or
     not fProtocol.fOnBeforeIncomingFrame(self,frame) then
    fProtocol.ProcessIncomingFrame(self,frame,''); // any exception would abort
  WebSocketLog.Add.Log(sllDebug,'ProcessStart %', [fProtocol],self);
end;

procedure TWebSocketProcess.ProcessStop;
var frame: TWebSocketFrame; // notify e.g. TOnWebSocketProtocolChatIncomingFrame
begin
  try
    WebSocketLog.Add.Log(sllTrace,'ProcessStop: callbacks',self);
    frame.opcode := focConnectionClose;
    if not Assigned(fProtocol.fOnBeforeIncomingFrame) or
       not fProtocol.fOnBeforeIncomingFrame(self,frame) then
      fProtocol.ProcessIncomingFrame(self,frame,'');
    if Assigned(fSettings.OnClientDisconnected) then begin
      WebSocketLog.Add.Log(sllTrace,'ProcessStop: OnClientDisconnected',self);
      fSettings.OnClientDisconnected(Self);
    end;
  except // exceptions are just ignored at shutdown
  end;
  fProcessEnded := true;
  WebSocketLog.Add.Log(sllDebug,'ProcessStop %', [fProtocol], self);
end;

procedure TWebSocketProcess.SetLastPingTicks(invalidPing: boolean);
var tix: Int64;
begin
  tix := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64;
  fSafePing.Lock;
  try
    fLastSocketTicks := tix;
    if invalidPing then begin
      inc(fInvalidPingSendCount);
      fNoConnectionCloseAtDestroy := true;
    end else
      fInvalidPingSendCount := 0;
  finally
    fSafePing.UnLock;
  end;
end;

function TWebSocketProcess.LastPingDelay: Int64;
begin
  result := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64;
  fSafePing.Lock;
  try
    dec(result,fLastSocketTicks);
  finally
    fSafePing.UnLock;
  end;
end;

function TWebSocketProcess.ProcessLoopStepReceive: boolean;
var request: TWebSocketFrame;
    sockerror: integer;
begin
  if fState=wpsRun then begin
    InterlockedIncrement(fProcessCount); // flag currently processing
    try
      if CanGetFrame({timeout=}1,@sockerror) and GetFrame(request,@sockerror) then begin
        case request.opcode of
        focPing: begin
          request.opcode := focPong;
          SendFrame(request);
        end;
        focPong:
          ; // nothing to do
        focText,focBinary:
          if not Assigned(fProtocol.fOnBeforeIncomingFrame) or
             not fProtocol.fOnBeforeIncomingFrame(self,request) then
            fProtocol.ProcessIncomingFrame(self,request,'');
        focConnectionClose: begin
          if fState=wpsRun then begin
            fState := wpsClose; // will close the connection
            SendFrame(request); // send back the frame as ACK
          end;
        end;
        end;
      end else
      if (fOwnerThread<>nil) and fOwnerThread.Terminated then
        fState := wpsClose else
      if sockerror<>0 then begin
        WebSocketLog.Add.Log(sllInfo,'GetFrame SockInPending error % on %',
          [sockerror,fProtocol],self);
        fState := wpsClose;
      end;
    finally
      InterlockedDecrement(fProcessCount); // release flag
    end;
  end;
  result := (fState=wpsRun);
end;

function TWebSocketProcess.ProcessLoopStepSend: boolean;
var request: TWebSocketFrame;
    elapsed: cardinal;
begin
  if fState=wpsRun then begin
    InterlockedIncrement(fProcessCount); // flag currently processing
    try
      elapsed := LastPingDelay;
      if elapsed>fSettings.SendDelay then
        if (fOutgoing.Count>0) and not SendPendingOutgoingFrames then
          fState := wpsClose
        else
        if (fSettings.HeartbeatDelay<>0) and
           (elapsed>fSettings.HeartbeatDelay) then begin
          request.opcode := focPing;
          if not SendFrame(request) then
            if (fSettings.DisconnectAfterInvalidHeartbeatCount<>0) and
               (fInvalidPingSendCount>=fSettings.DisconnectAfterInvalidHeartbeatCount) then
              fState := wpsClose
            else
              SetLastPingTicks(true); // mark invalid, and avoid immediate retry
        end;
    finally
      InterlockedDecrement(fProcessCount); // release flag
    end;
  end;
  result := (fState=wpsRun);
end;

procedure TWebSocketProcess.ProcessLoop;
begin
  if fProtocol=nil then
    exit;
  try
    ProcessStart; // any exception will close the socket
    try
      SetLastPingTicks;
      fState := wpsRun;
      while (fOwnerThread=nil) or not fOwnerThread.Terminated do
        if ProcessLoopStepReceive and ProcessLoopStepSend then
          HiResDelay(fLastSocketTicks)
        else
          break; // connection ended
    finally
      ProcessStop;
    end;
  except // don't be optimistic: abort and close connection
    fState := wpsClose;
  end;
end;

procedure TWebSocketProcess.HiResDelay(const start: Int64);
var delay: cardinal;
begin
  case {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64-start of
  0..50:      delay := 0; // 10 microsecs on POSIX
  51..200:    delay := 1;
  201..500:   delay := 5;
  501..2000:  delay := 50;
  2001..5000: delay := 100;
  else        delay := 500;
  end;
  if (fSettings.LoopDelay<>0) and (delay>fSettings.LoopDelay) then
    delay := fSettings.LoopDelay;
  SleepHiRes(delay);
end;

function TWebSocketProcess.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketProcess.State: TWebSocketProcessState;
begin
  if self=nil then
    result := wpsCreate else
    result := fState;
end;

function TWebSocketProcess.RemoteIP: SockString;
begin
  if (self=nil) or (fProtocol=nil) or fProtocol.fRemoteLocalhost then
    result := '' else
    result := fProtocol.fRemoteIP;
end;

function TWebSocketProcess.NotifyCallback(aRequest: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
var request,answer: TWebSocketFrame;
    i: integer;
    start,max: Int64;
    head: RawUTF8;
begin
  result := STATUS_NOTFOUND;
  if (fProtocol=nil) or (aRequest=nil) or
     not fProtocol.InheritsFrom(TWebSocketProtocolRest) then
    exit;
  if WebSocketLog<>nil then
    WebSocketLog.Add.Log(sllTrace,'NotifyCallback(%,%)',
      [aRequest.URL,_TWebSocketProcessNotifyCallback[aMode]^],self);
  TWebSocketProtocolRest(fProtocol).InputToFrame(
    aRequest,aMode in [wscBlockWithoutAnswer,wscNonBlockWithoutAnswer],request,head);
  case aMode of
  wscNonBlockWithoutAnswer: begin
    // add to the internal sending list for asynchronous sending
    fOutgoing.Push(request);
    result := STATUS_SUCCESS;
    exit;
  end;
  wscBlockWithAnswer:
    if fIncoming.AnswerToIgnore>0 then begin
      WebSocketLog.Add.Log(sllDebug,'NotifyCallback: Waiting for AnswerToIgnore=%',
        [fIncoming.AnswerToIgnore],self);
      start := GetTickCount64;
      max := start+30000;
      repeat
        HiResDelay(start);
        if fState in [wpsDestroy,wpsClose] then begin
          result := STATUS_WEBSOCKETCLOSED;
          exit;
        end;
        if fIncoming.AnswerToIgnore=0 then
          break; // it is now safe to send a new 'request'
        if GetTickCount64<max then
          continue;
        self.Log(request,'NotifyCallback AnswerToIgnore TIMEOUT -> abort connection',sllInfo);
        result := STATUS_NOTIMPLEMENTED; // 501 will force recreate connection
        exit;
      until false;
    end;
  end;
  i := InterlockedIncrement(fProcessCount);
  try
    if (i>2) and (WebSocketLog<>nil) then
      WebSocketLog.Add.Log(sllWarning,'NotifyCallback with fProcessCount=%',[i],self);
    if not SendFrame(request) then
      exit;
    if aMode=wscBlockWithoutAnswer then begin
      result := STATUS_SUCCESS;
      exit;
    end;
    start := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64;
    if fSettings.CallbackAnswerTimeOutMS=0 then
      max := start+30000 else // never wait for ever
    if fSettings.CallbackAnswerTimeOutMS<2000 then
      max := start+2000 else // 2 seconds minimal wait
      max := start+fSettings.CallbackAnswerTimeOutMS;
    while not fIncoming.Pop(fProtocol,head,answer) do
      if fState in [wpsDestroy,wpsClose] then begin
        result := STATUS_WEBSOCKETCLOSED;
        exit;
      end else
      if {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64>max then begin
        WebSocketLog.Add.Log(sllWarning,'NotifyCallback TIMEOUT %',[head],self);
        if head='answer' then
          fIncoming.AnswerToIgnore(1); // ignore next 'answer'
        exit; // returns STATUS_NOTFOUND
      end else
        HiResDelay(start);
  finally
    InterlockedDecrement(fProcessCount);
  end;
  result := TWebSocketProtocolRest(fProtocol).FrameToOutput(answer,aRequest);
end;

function TWebSocketProcess.SendPendingOutgoingFrames: boolean;
begin
  result := false;
  fOutgoing.Safe.Lock;
  try
    if fProtocol.SendFrames(self,fOutgoing.List,fOutgoing.Count) then
      result := true else
      WebSocketLog.Add.Log(sllInfo,'SendPendingOutgoingFrames: SendFrames failed',self);
  finally
    fOutgoing.Safe.UnLock;
  end;
end;

procedure TWebSocketProcess.Log(const frame: TWebSocketFrame;
  const aMethodName: RawUTF8; aEvent: TSynLogInfo; DisableRemoteLog: Boolean);
var tmp: TLogEscape;
    log: TSynLog;
    len: integer;
begin
  if WebSocketLog<>nil then
  with WebSocketLog.Family do
  if aEvent in Level then
  if (logHeartbeat in fSettings.LogDetails) or
     not (frame.opcode in [focPing,focPong]) then begin
    log := SynLog;
    log.DisableRemoteLog(DisableRemoteLog);
    try
      if (frame.opcode=focText) and
         (logTextFrameContent in fSettings.LogDetails) then
        log.Log(aEvent,'% % % focText %',[aMethodName,fProtocol.GetRemoteIP,
          Protocol.FrameType(frame),frame.PayLoad],self) else begin
        len := length(frame.PayLoad);
        log.Log(aEvent,'% % % % len=%%',[aMethodName,fProtocol.GetRemoteIP,
          Protocol.FrameType(frame),_TWebSocketFrameOpCode[frame.opcode]^,len,
          LogEscape(pointer(frame.PayLoad),len,tmp,
            logBinaryFrameContent in fSettings.LogDetails)],self);
      end;
    finally
      log.DisableRemoteLog(false);
    end;
  end;
end;

const
  FRAME_OPCODE_FIN=128;
  FRAME_LEN_MASK=128;
  FRAME_LEN_2BYTES=126;
  FRAME_LEN_8BYTES=127;

type
 TFrameHeader = packed record
   first: byte;
   len8: byte;
   len32: cardinal;
   len64: cardinal;
   mask: cardinal; // 0 indicates no payload masking
 end;

procedure ProcessMask(data: pointer; mask: cardinal; len: PtrInt);
var i,maskCount: PtrInt;
begin
  maskCount := len shr 2;
  for i := 0 to maskCount-1 do
    PCardinalArray(data)^[i] := PCardinalArray(data)^[i] xor mask;
  maskCount := maskCount shl 2;
  for i := maskCount to maskCount+(len and 3)-1 do begin
    PByteArray(data)^[i] := PByteArray(data)^[i] xor mask;
    mask := mask shr 8;
  end;
end;

type
  // asynchronous state machine to process incoming frames
  TWebProcessInFrameState = (
    pfsHeader1, pfsData1, pfsHeaderN, pfsDataN, pfsDone, pfsError);
  TWebProcessInFrame = object
    hdr: TFrameHeader;
    opcode: TWebSocketFrameOpCode;
    masked: boolean;
    st: TWebProcessInFrameState;
    process: TWebSocketProcess;
    outputframe: PWebSocketFrame;
    len: integer;
    data: RawByteString;
    procedure Init(owner: TWebSocketProcess; output: PWebSocketFrame);
    function GetBytes(P: PAnsiChar; count: integer): boolean;
    function GetHeader: boolean;
    function GetData: boolean;
    function Step(ErrorWithoutException: PInteger): TWebProcessInFrameState;
  end;

function TWebProcessInFrame.GetBytes(P: PAnsiChar; count: integer): boolean;
begin // SockInRead() below raise a ECrtSocket error on failure
  inc(len,process.ReceiveBytes(P+len,count-len));
  result := len=count;
end;

function TWebProcessInFrame.GetHeader: boolean;
begin
  result := false;
  if len<2 then begin
    data := '';
    FillCharFast(hdr,sizeof(hdr),0);
    if not GetBytes(@hdr,2) then // first+len8
      exit;
  end;
  opcode := TWebSocketFrameOpCode(hdr.first and 15);
  masked := hdr.len8 and FRAME_LEN_MASK<>0;
  if masked then
    hdr.len8 := hdr.len8 and 127;
  if hdr.len8<FRAME_LEN_2BYTES then
    hdr.len32 := hdr.len8 else
  if hdr.len8=FRAME_LEN_2BYTES then begin
    if not GetBytes(@hdr,4) then // first+len8+len32.low
      exit;
    hdr.len32 := swap(word(hdr.len32)); // FPC expects explicit word() cast
  end else
  if hdr.len8=FRAME_LEN_8BYTES then begin
    if not GetBytes(@hdr,10) then // first+len8+len32+len64.low
      exit;
    if hdr.len32<>0 then // size is more than 32 bits -> reject
      hdr.len32 := maxInt else
      hdr.len32 := bswap32(hdr.len64);
    if hdr.len32>WebSocketsMaxFrameMB shl 20 then
      raise EWebSockets.CreateUTF8('%.GetFrame: length should be < % MB',
        [process,WebSocketsMaxFrameMB]);
  end;
  if masked then begin
    len := 0; // not appended to hdr
    if not GetBytes(@hdr.mask,4) then
      raise EWebSockets.CreateUTF8('%.GetFrame: truncated mask',[process]);
  end;
  len := 0; // prepare upcoming GetData
  result := true;
end;

function TWebProcessInFrame.GetData: boolean;
begin
  if length(data)<>integer(hdr.len32) then
    SetString(data,nil,hdr.len32);
  result := GetBytes(pointer(data),hdr.len32);
  if result then begin
    if hdr.mask<>0 then
      ProcessMask(pointer(data),hdr.mask,hdr.len32);
    len := 0; // prepare upcoming GetHeader
  end;
end;

function TWebProcessInFrame.Step(ErrorWithoutException: PInteger): TWebProcessInFrameState;
begin
  while true do // process incoming data as much as possible
  case st of
  pfsHeader1:
    if GetHeader then begin
      outputframe.opcode := opcode;
      outputframe.content := [];
      st := pfsData1;
    end else
      break; // quit when not enough data is available from input
  pfsData1:
    if GetData then begin
      outputframe.payload := data;
      if hdr.first and FRAME_OPCODE_FIN=0 then
        st := pfsHeaderN else
        st := pfsDone;
    end else
      break;
  pfsHeaderN:
    if GetHeader then
      if (opcode<>focContinuation) and (opcode<>outputframe.opcode) then begin
        st := pfsError;
        if ErrorWithoutException<>nil then begin
          WebSocketLog.Add.Log(sllDebug, 'GetFrame: received %, expected %',
            [_TWebSocketFrameOpCode[opcode]^,_TWebSocketFrameOpCode[outputframe.opcode]^],process);
          ErrorWithoutException^ := maxInt;
        end else
          raise EWebSockets.CreateUTF8('%.GetFrame: received %, expected %',
            [process,_TWebSocketFrameOpCode[opcode]^,_TWebSocketFrameOpCode[outputframe.opcode]^]);
      end else
        st := pfsDataN else
      break;
  pfsDataN:
    if GetData then begin
      outputframe.payload := outputframe.payload+data;
      if hdr.first and FRAME_OPCODE_FIN=0 then
        st := pfsHeaderN else
        st := pfsDone;
    end else
      break;
  pfsDone: begin
    data := '';
    {$ifdef HASCODEPAGE}
    if opcode=focText then
      SetCodePage(outputframe.payload,CP_UTF8,false); // identify text value as UTF-8
    {$endif}
    if (process.fProtocol<>nil) and (outputframe.payload<>'') then
      process.fProtocol.AfterGetFrame(outputframe^);
    process.Log(outputframe^,'GetFrame');
    process.SetLastPingTicks;
    break;
  end;
  else // e.g. pfsError
    break;
  end;
  result := st;
end;

procedure TWebProcessInFrame.Init(owner: TWebSocketProcess; output: PWebSocketFrame);
begin
  process := owner;
  outputframe := output;
  st := pfsHeader1;
  len := 0;
end;


function TWebSocketProcess.GetFrame(out Frame: TWebSocketFrame;
  ErrorWithoutException: PInteger): boolean;
var f: TWebProcessInFrame;
begin
  f.Init(self,@Frame);
  fSafeIn.Lock;
  try
    repeat until f.Step(ErrorWithoutException) in [pfsDone,pfsError];
    result := f.st=pfsDone;
  finally
    fSafeIn.UnLock;
  end;
end;

function TWebSocketProcess.SendFrame(var Frame: TWebSocketFrame): boolean;
var hdr: TFrameHeader;
    hdrlen, len: cardinal;
    tmp: TSynTempBuffer;
begin
  fSafeOut.Lock;
  try
    Log(frame,'SendFrame',sllTrace,true);
    try
      result := true;
      if Frame.opcode=focConnectionClose then
        fNoConnectionCloseAtDestroy := true; // to be done once on each end
      if (fProtocol<>nil) and (Frame.payload<>'') then
        fProtocol.BeforeSendFrame(Frame);
      len := Length(Frame.payload);
      hdr.first := byte(Frame.opcode) or FRAME_OPCODE_FIN; // single frame
      if len<FRAME_LEN_2BYTES then begin
        hdr.len8 := len or fMaskSentFrames;
        hdrlen := 2; // opcode+len8
      end else
      if len<65536 then begin
        hdr.len8 := FRAME_LEN_2BYTES or fMaskSentFrames;
        hdr.len32 := swap(word(len)); // FPC expects explicit word() cast
        hdrlen := 4; // opcode+len8+len32.low
      end else begin
        hdr.len8 := FRAME_LEN_8BYTES or fMaskSentFrames;
        hdr.len64 := bswap32(len);
        hdr.len32 := 0;
        hdrlen := 10; // opcode+len8+len32+len64.low
      end;
      if fMaskSentFrames<>0 then begin
        hdr.mask := Random32gsl; // https://tools.ietf.org/html/rfc6455#section-10.3
        ProcessMask(pointer(Frame.payload),hdr.mask,len);
        inc(hdrlen,4);
      end;
      tmp.Init(hdrlen+len); // avoid most memory allocations
      try
        MoveSmall(@hdr,tmp.buf,hdrlen);
        if fMaskSentFrames<>0 then
          PInteger(PAnsiChar(tmp.buf)+hdrlen-4)^ := hdr.mask;
        MoveFast(pointer(Frame.payload)^,PAnsiChar(tmp.buf)[hdrlen],len);
        if not SendBytes(tmp.buf,hdrlen+len) then
          result := false;
      finally
        tmp.Done;
      end;
      SetLastPingTicks(not result);
    except
      result := false;
    end;
  finally
    fSafeOut.UnLock;
  end;
end;


{ TWebCrtSocketProcess }

constructor TWebCrtSocketProcess.Create(aSocket: TCrtSocket; aProtocol: TWebSocketProtocol;
  aOwnerConnection: THttpServerConnectionID; aOwnerThread: TSynThread;
  const aSettings: TWebSocketProcessSettings; const aProcessName: RawUTF8);
begin
  inherited Create(aProtocol,aOwnerConnection,aOwnerThread,aSettings,aProcessName);
  fSocket := aSocket;
end;

function TWebCrtSocketProcess.CanGetFrame(TimeOut: cardinal; ErrorWithoutException: PInteger): boolean;
var pending: integer;
begin
  if ErrorWithoutException<>nil then
    ErrorWithoutException^ := 0;
  pending := fSocket.SockInPending(TimeOut,{PendingAlsoInSocket=}true);
  if pending<0 then // socket error
    if ErrorWithoutException<>nil then begin
      ErrorWithoutException^ := fSocket.LastLowSocketError;
      result := false;
      exit;
    end else
      raise EWebSockets.CreateUTF8('SockInPending() Error % on %:% - from %',
        [fSocket.LastLowSocketError,fSocket.Server,fSocket.Port,fProtocol.fRemoteIP]);
  result := (pending>=2);
end;

function TWebCrtSocketProcess.ReceiveBytes(P: PAnsiChar; count: integer): integer;
begin
  result := fSocket.SockInRead(P,count,{useonlysockin=}false);
end;

function TWebCrtSocketProcess.SendBytes(P: pointer; Len: integer): boolean;
begin
  result := fSocket.TrySndLow(P,Len);
end;


{ -------------- WebSockets Server classes for bidirectional remote access }

function HttpServerWebSocketUpgrade(ClientSock: THttpServerSocket;
  Protocols: TWebSocketProtocolList; out Protocol: TWebSocketProtocol): integer;
var uri,version,prot,subprot,key,extin,extout,header: RawUTF8;
    extins: TRawUTF8DynArray;
    P: PUTF8Char;
    Digest: TSHA1Digest;
begin
  result := STATUS_BADREQUEST;
  try
    if not IdemPropNameU(ClientSock.Upgrade,'websocket') then
      exit;
    version := ClientSock.HeaderGetValue('SEC-WEBSOCKET-VERSION');
    if GetInteger(pointer(version))<13 then
      exit; // we expect WebSockets protocol version 13 at least
    uri := Trim(RawUTF8(ClientSock.URL));
    if (uri<>'') and (uri[1]='/') then
      Delete(uri,1,1);
    prot := ClientSock.HeaderGetValue('SEC-WEBSOCKET-PROTOCOL');
    P := pointer(prot);
    if P<>nil then begin
      repeat
        GetNextItemTrimed(P,',',subprot);
        Protocol := Protocols.CloneByName(subprot,uri);
      until (P=nil) or (Protocol<>nil);
      if (Protocol<>nil) and (Protocol.URI='') and not Protocol.ProcessURI(uri) then begin
        Protocol.Free;
        result := STATUS_UNAUTHORIZED;
        exit;
      end;
    end else
      // if no protocol is specified, try to match by URI
      Protocol := Protocols.CloneByURI(uri);
    if Protocol=nil then
      exit;
    Protocol.fUpgradeURI := uri;
    Protocol.fRemoteIP := ClientSock.HeaderGetValue('SEC-WEBSOCKET-REMOTEIP');
    if Protocol.fRemoteIP='' then
      Protocol.fRemoteIP := ClientSock.RemoteIP;
    Protocol.fRemoteLocalhost := Protocol.fRemoteIP='127.0.0.1';
    extin := ClientSock.HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
    if extin<>'' then begin
      CSVToRawUTF8DynArray(pointer(extin),extins,';',true);
      if not Protocol.ProcessHandshake(extins,extout,nil) then begin
        Protocol.Free;
        result := STATUS_UNAUTHORIZED;
        exit;
      end;
    end;
    key := ClientSock.HeaderGetValue('SEC-WEBSOCKET-KEY');
    if Base64ToBinLengthSafe(pointer(key),length(key))<>16 then begin
      Protocol.Free;
      exit; // this nonce must be a Base64-encoded value of 16 bytes
    end;
    ComputeChallenge(key,Digest);
    if extout<>'' then
      extout := 'Sec-WebSocket-Extensions: '+extout+#13#10;
    FormatUTF8('HTTP/1.1 101 Switching Protocols'#13#10+
      'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10+
      'Sec-WebSocket-Protocol: %'#13#10'%Sec-WebSocket-Accept: %'#13#10#13#10,
      [Protocol.Name,extout,BinToBase64Short(@Digest,sizeof(Digest))],header);
    if not ClientSock.TrySndLow(pointer(header),length(header)) then begin
      Protocol.Free;
      result := STATUS_WEBSOCKETCLOSED;
      exit;
    end;
    result := STATUS_SUCCESS; // connection upgraded: never back to HTTP/1.1
  finally
    if result<>STATUS_SUCCESS then begin // notify upgrade failure to client
      FormatUTF8('HTTP/1.0 % WebSocket Upgrade Error'#13#10+
        'Connection: Close'#13#10#13#10,[result],header);
      ClientSock.TrySndLow(pointer(header),length(header));
      ClientSock.KeepAliveClient := false;
    end;
  end;
end;


{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
  const ProcessName: SockString; ServerThreadPoolCount, KeepAliveTimeOut: integer;
  HeadersNotFiltered: boolean; CreateSuspended: boolean);
begin
  // override with custom processing classes
  fSocketClass := TWebSocketServerSocket;
  fThreadRespClass := TWebSocketServerResp;
  // initialize protocols and connections
  fWebSocketConnections := TSynObjectListLocked.Create({owned=}false);
  fProtocols := TWebSocketProtocolList.Create;
  fSettings.SetDefaults;
  fSettings.HeartbeatDelay := 20000;
  fCanNotifyCallback := true;
  // start the server
  inherited Create(aPort,OnStart,OnStop,ProcessName,ServerThreadPoolCount,
    KeepAliveTimeOut,HeadersNotFiltered,CreateSuspended);
end;

function TWebSocketServer.WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
  Context: TWebSocketServerResp): integer;
var protocol: TWebSocketProtocol;
begin
  result := HttpServerWebSocketUpgrade(ClientSock,fProtocols,protocol);
  if result<>STATUS_SUCCESS then
    exit;
  ClientSock.KeepAliveClient := false; // close connection with WebSockets
  Context.fProcess := TWebSocketProcessServer.Create(
    ClientSock,protocol,Context.ConnectionID,Context,fSettings,fProcessName);
  Context.fProcess.fServerResp := Context;
  fWebSocketConnections.Add(Context);
  try
    Context.fProcess.ProcessLoop;  // run main blocking loop
  finally
    FreeAndNil(Context.fProcess); // notify end of WebSockets
    fWebSocketConnections.Remove(Context);
  end;
end;

procedure TWebSocketServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: THttpServerConnectionID; ConnectionThread: TSynThread);
var err: integer;
begin
  if (connectionUpgrade in ClientSock.HeaderFlags) and ClientSock.KeepAliveClient and
     IdemPropNameU('GET',ClientSock.Method) and
     IdemPropNameU(ClientSock.Upgrade,'websocket') and
     ConnectionThread.InheritsFrom(TWebSocketServerResp) then begin
    err := WebSocketProcessUpgrade(ClientSock,TWebSocketServerResp(ConnectionThread));
    if err<>STATUS_SUCCESS then
      WebSocketLog.Add.Log(sllTrace,'Process: WebSocketProcessUpgrade failed as %',[err],self);
  end else
    inherited Process(ClientSock,ConnectionID,ConnectionThread);
end;

destructor TWebSocketServer.Destroy;
begin
  inherited Destroy; // close any pending connection
  fWebSocketConnections.Free;
  fProtocols.Free;
end;

function TWebSocketServer.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketServer.WebSocketConnections: integer;
begin
  result := fWebSocketConnections.Count;
end;

type
  PWebSocketServerResp = ^TWebSocketServerResp;

function TWebSocketServer.IsActiveWebSocketThread(ConnectionThread: TSynThread): TWebSocketServerResp;
var i: Integer;
    c: PWebSocketServerResp;
begin // no need to optimize (not called often)
  result := nil;
  if Terminated or (ConnectionThread=nil) or
     not ConnectionThread.InheritsFrom(TWebSocketServerResp) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    c := pointer(fWebSocketConnections.List);
    for i := 1 to fWebSocketConnections.Count do
      if c^=ConnectionThread then begin
        if c^.fProcess.State=wpsRun then
          result := c^;
        exit;
      end else
      inc(c);
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
end;

function FastFindConnection(c: PWebSocketServerResp; n: integer; id: THttpServerConnectionID): TWebSocketServerResp;
begin // speedup brute force check in case of high number of connections
  if n>0 then
    repeat
      result := c^;
      if result.ConnectionID=id then
        exit;
      inc(c);
      dec(n);
      if n=0 then
        break;
      result := c^;
      if result.ConnectionID=id then
        exit;
      inc(c);
      dec(n);
    until n=0;
  result := nil;
end;

function TWebSocketServer.IsActiveWebSocket(ConnectionID: THttpServerConnectionID): TWebSocketServerResp;
begin
  result := nil;
  if Terminated or (ConnectionID=0) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    result := FastFindConnection(pointer(fWebSocketConnections.List),
      fWebSocketConnections.Count, ConnectionID);
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame);
begin
  WebSocketBroadcast(aFrame,nil);
end;

procedure TWebSocketServer.WebSocketBroadcast(const aFrame: TWebSocketFrame;
  const aClientsConnectionID: THttpServerConnectionIDDynArray);
var i, len, ids: Integer;
    c: ^TWebSocketServerResp;
    temp: TWebSocketFrame; // local copy since SendFrame() modifies the payload
    sorted: TSynTempBuffer;
begin
  if Terminated or not(aFrame.opcode in [focText, focBinary]) then
    exit;
  ids := length(aClientsConnectionID);
  if ids>0 then begin
    sorted.Init(pointer(aClientsConnectionID),ids*8);
    QuickSortInt64(sorted.buf,0,ids-1); // faster O(log(n)) binary search
  end;
  dec(ids); // WebSocketBroadcast(nil) -> ids<0
  temp.opcode := aFrame.opcode;
  temp.content := aFrame.content;
  len := length(aFrame.payload);
  fWebSocketConnections.Safe.Lock;
  try
    c := pointer(fWebSocketConnections.List);
    for i := 1 to fWebSocketConnections.Count do begin
      if (c^.fProcess.State=wpsRun) and
         ((ids<0) or (FastFindInt64Sorted(sorted.buf,ids,c^.ConnectionID)>=0)) then begin
        SetString(temp.payload,PAnsiChar(pointer(aFrame.payload)),len);
        c^.fProcess.Outgoing.Push(temp); // non blocking asynchronous sending
      end;
      inc(c);
    end;
  finally
    fWebSocketConnections.Safe.UnLock;
    if ids>=0 then
      sorted.Done;
  end;
end;

{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: SockString;
  OnStart,OnStop: TNotifyThreadEvent; const aProcessName,
  aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean);
begin
  Create(aPort,OnStart,OnStop,aProcessName);
  WebSocketsEnable(aWebSocketsURI,aWebSocketsEncryptionKey,aWebSocketsAJAX);
end;

procedure TWebSocketServerRest.WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX,aWebSocketsCompressed: boolean);
begin
  if self=nil  then
    exit;
  fProtocols.AddOnce(TWebSocketProtocolBinary.Create(
    aWebSocketsURI,true,aWebSocketsEncryptionKey,aWebSocketsCompressed));
  if aWebSocketsAJAX then
    fProtocols.AddOnce(TWebSocketProtocolJSON.Create(aWebSocketsURI));
end;

function TWebSocketServerRest.Callback(
  Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal;
var connection: TWebSocketServerResp;
    mode: TWebSocketProcessNotifyCallback;
begin
  if Ctxt=nil then
    connection := nil else begin
    WebSocketLog.Add.Log(sllTrace,'Callback(%) on socket=%',
      [Ctxt.URL,Ctxt.ConnectionID],self);
    connection := IsActiveWebSocket(Ctxt.ConnectionID);
  end;
  if connection<>nil then begin
    //  this request is a websocket, on a non broken connection
    if aNonBlocking then // see TInterfacedObjectFakeServer.CallbackInvoke
      mode := wscNonBlockWithoutAnswer else
      mode := wscBlockWithAnswer;
    result := connection.NotifyCallback(Ctxt,mode);
  end else begin
    WebSocketLog.Add.Log(sllError,'Callback() on inactive socket',self);
    result := STATUS_NOTFOUND;
  end;
end;


{ TWebSocketServerResp }

constructor TWebSocketServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer);
begin
  if not aServer.InheritsFrom(TWebSocketServer) then
    raise EWebSockets.CreateUTF8('%.Create(%: TWebSocketServer?)',[self,aServer]);
  inherited Create(aServerSock,aServer);
end;

function TWebSocketServerResp.NotifyCallback(Ctxt: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
begin
  if fProcess=nil then
    result := STATUS_NOTFOUND else begin
    result := fProcess.NotifyCallback(Ctxt,aMode);
    if result=STATUS_WEBSOCKETCLOSED then begin
      WebSocketLog.Add.Log(sllError,'NotifyCallback on closed connection',self);
      ServerSock.KeepAliveClient := false; // force close the connection
      result := STATUS_NOTFOUND;
    end;
  end;
end;

function TWebSocketServerResp.WebSocketProtocol: TWebSocketProtocol;
begin
  if (Self=nil) or (fProcess=nil) then
    result := nil else
    result := fProcess.Protocol;
end;


{ TWebSocketProcessServer }

function TWebSocketProcessServer.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequest;
begin
  result := THttpServerRequest.Create(
    (fOwnerThread as TWebSocketServerResp).fServer,fOwnerConnection,fOwnerThread);
  RequestProcess := TWebSocketServerResp(fOwnerThread).fServer.Request;
end;


{ TWebSocketServerSocket }

function TWebSocketServerSocket.GetRequest(withBody: boolean;
  headerMaxTix: Int64): THttpServerSocketGetRequestResult;
begin
  result := inherited GetRequest(withBody, headerMaxTix);
  if (result=grHeaderReceived) and (connectionUpgrade in HeaderFlags) and
     KeepAliveClient and IdemPropNameU('GET',Method) and IdemPropNameU(Upgrade,'websocket') then
    //writeln('!!');
end;


{ -------------- WebSockets Client classes for bidirectional remote access }

{ THttpClientWebSockets }

constructor THttpClientWebSockets.Create(aTimeOut: PtrInt);
begin
  inherited;
  fSettings.SetDefaults;
  fSettings.CallbackAnswerTimeOutMS := aTimeOut;
end;

class function THttpClientWebSockets.WebSocketsConnect(const aHost,
  aPort: SockString; aProtocol: TWebSocketProtocol; aLog: TSynLogClass;
  const aLogContext, aURI, aCustomHeaders: RawUTF8): THttpClientWebSockets;
var error: RawUTF8;
begin
  result := nil;
  if (aProtocol = nil) or (aHost = '') then
    raise EWebSockets.CreateUTF8('%.WebSocketsConnect(nil)', [self]);
  try
    result := Open(aHost, aPort); // constructor
    error := result.WebSocketsUpgrade(aURI, '', false, false, aProtocol, aCustomHeaders);
    if error <> '' then
      FreeAndNil(result);
  except
    on E: Exception do begin
      aProtocol.Free; // as done in WebSocketsUpgrade()
      FreeAndNil(result);
      FormatUTF8('% %', [E, E.Message], error);
    end;
  end;
  if aLog <> nil then
    if result <> nil then
      aLog.Add.Log(sllDebug, '%: WebSocketsConnect %', [aLogContext, result])
    else
      aLog.Add.Log(sllWarning, '%: WebSocketsConnect %:% failed - %',
        [aLogContext, aHost, aPort, error]);
end;

destructor THttpClientWebSockets.Destroy;
begin
  FreeAndNil(fProcess);
  inherited;
end;

function THttpClientWebSockets.Request(const url, method: SockString;
  KeepAlive: cardinal; const header, Data, DataType: SockString;
  retry: boolean): integer;
var Ctxt: THttpServerRequest;
    block: TWebSocketProcessNotifyCallback;
    resthead: RawUTF8;
begin
  if fProcess<>nil then begin
    if fProcess.fClientThread.fThreadState=sCreate then
      sleep(10); // paranoid warmup of TWebSocketProcessClientThread.Execute
    if fProcess.fClientThread.fThreadState<>sRun then
      // WebSockets closed by server side
      result := STATUS_NOTIMPLEMENTED else begin
      // send the REST request over WebSockets
      Ctxt := THttpServerRequest.Create(
        nil,fProcess.fOwnerConnection,fProcess.fOwnerThread);
      try
        Ctxt.Prepare(url,method,header,data,dataType,'',fTLS);
        FindNameValue(header,'SEC-WEBSOCKET-REST:',resthead);
        if resthead='NonBlocking' then
          block := wscNonBlockWithoutAnswer else
          block := wscBlockWithAnswer;
        result := fProcess.NotifyCallback(Ctxt,block);
        if IdemPChar(pointer(Ctxt.OutContentType),JSON_CONTENT_TYPE_UPPER) then
          HeaderSetText(Ctxt.OutCustomHeaders) else
          HeaderSetText(Ctxt.OutCustomHeaders,Ctxt.OutContentType);
        Content := Ctxt.OutContent;
        ContentType := Ctxt.OutContentType;
        ContentLength := length(Ctxt.OutContent);
      finally
        Ctxt.Free;
      end;
    end;
  end else
    // standard HTTP/1.1 REST request (before WebSocketsUpgrade call)
    result := inherited Request(url,method,KeepAlive,header,Data,DataType,retry);
end;

procedure THttpClientWebSockets.SetInt32OptionByIndex(OptName, OptVal: integer);
begin
  inherited SetInt32OptionByIndex(OptName,OptVal);
  if OptName=SO_RCVTIMEO then
    fSettings.CallbackAnswerTimeOutMS := OptVal;
end;

function THttpClientWebSockets.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function THttpClientWebSockets.WebSocketsUpgrade(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean;
  aWebSocketsCompression: boolean; aProtocol: TWebSocketProtocol;
  const aCustomHeaders: RawUTF8): RawUTF8;
var key: TAESBlock;
    bin1,bin2: RawByteString;
    extin,extout,prot: RawUTF8;
    extins: TRawUTF8DynArray;
    cmd: SockString;
    digest1,digest2: TSHA1Digest;
begin
  try
    try
      if fProcess<>nil then begin
        result := 'Already upgraded to WebSockets';
        if IdemPropNameU(fProcess.Protocol.URI,aWebSocketsURI) then
          result := result+' on this URI' else
          result := FormatUTF8('% with URI=[%] but requested [%]',
            [result,fProcess.Protocol.URI,aWebSocketsURI]);
        exit;
      end;
      if aProtocol = nil then
        if aWebSocketsAJAX then
          aProtocol := TWebSocketProtocolJSON.Create(aWebSocketsURI) else
          aProtocol := TWebSocketProtocolBinary.Create(
            aWebSocketsURI,false,aWebSocketsEncryptionKey,aWebSocketsCompression);
      aProtocol.fOnBeforeIncomingFrame := fOnBeforeIncomingFrame;
      RequestSendHeader(aWebSocketsURI,'GET');
      TAESPRNG.Main.FillRandom(key);
      bin1 := BinToBase64(@key,sizeof(key));
      SockSend(['Content-Length: 0'#13#10'Connection: Upgrade'#13#10+
        'Upgrade: websocket'#13#10'Sec-WebSocket-Key: ',bin1,#13#10+
        'Sec-WebSocket-Protocol: ',aProtocol.GetSubprotocols,#13#10+
        'Sec-WebSocket-Version: 13']);
      if aProtocol.ProcessHandshake(nil,extout,nil) and (extout<>'') then
        SockSend(['Sec-WebSocket-Extensions: ',extout]);
      if aCustomHeaders<>'' then
        SockSend(aCustomHeaders);
      SockSend; // CRLF
      SockSendFlush('');
      SockRecvLn(cmd);
      GetHeader(false);
      prot := HeaderGetValue('SEC-WEBSOCKET-PROTOCOL');
      result := 'Invalid HTTP Upgrade Header';
      if not IdemPChar(pointer(cmd),'HTTP/1.1 101') or
         not (connectionUpgrade in HeaderFlags) or (ContentLength>0) or
         not IdemPropNameU(Upgrade,'websocket') or
         not aProtocol.SetSubprotocol(prot) then
        exit;
      aProtocol.fName := prot;
      result := 'Invalid HTTP Upgrade Accept Challenge';
      ComputeChallenge(bin1,digest1);
      bin2 := HeaderGetValue('SEC-WEBSOCKET-ACCEPT');
      if not Base64ToBin(pointer(bin2),@digest2,length(bin2),sizeof(digest2),false) or
         not IsEqual(digest1,digest2) then
        exit;
      if extout<>'' then begin
        result := 'Invalid HTTP Upgrade ProcessHandshake';
        extin := HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
        CSVToRawUTF8DynArray(pointer(extin),extins,';',true);
        if (extins=nil) or not aProtocol.ProcessHandshake(extins,extout,@result) then
          exit;
      end;
      // if we reached here, connection is successfully upgraded to WebSockets
      if (Server='localhost') or (Server='127.0.0.1') then begin
        aProtocol.fRemoteIP := '127.0.0.1';
        aProtocol.fRemoteLocalhost := true;
      end else
        aProtocol.fRemoteIP := Server;
      result := ''; // no error message = success
      fProcess := TWebSocketProcessClient.Create(self,aProtocol,fProcessName);
      aProtocol := nil; // protocol instance is owned by fProcess now
    finally
      aProtocol.Free;
    end;
  except
    on E: Exception do begin
      FreeAndNil(fProcess);
      FormatUTF8('%: %',[E,E.Message],result);
    end;
  end;
end;


{ TWebSocketProcessClient }

constructor TWebSocketProcessClient.Create(aSender: THttpClientWebSockets;
  aProtocol: TWebSocketProtocol; const aProcessName: RawUTF8);
var endtix: Int64;
begin
  fMaskSentFrames := FRAME_LEN_MASK; // https://tools.ietf.org/html/rfc6455#section-10.3
  inherited Create(aSender,aProtocol,0,nil,aSender.fSettings,aProcessName);
  // initialize the thread after everything is set (Execute may be instant)
  fClientThread := TWebSocketProcessClientThread.Create(self);
  endtix := GetTickCount64+5000;
  repeat // wait for TWebSocketProcess.ProcessLoop to initiate
    SleepHiRes(0);
  until fProcessEnded or (fState<>wpsCreate) or (GetTickCount64>endtix);
end;

destructor TWebSocketProcessClient.Destroy;
var tix: Int64;
begin
  with WebSocketLog.Enter('Destroy: ThreadState=%', [ToText(fClientThread.fThreadState)^], self) do
  try
    // focConnectionClose would be handled in this thread -> close client thread
    fClientThread.Terminate;
    tix := GetTickCount64+7000; // never wait forever
    while (fClientThread.fThreadState=sRun) and (GetTickCount64<tix) do
      SleepHiRes(1);
    fClientThread.fProcess := nil;
  finally
    // SendPendingOutgoingFrames + SendFrame/GetFrame(focConnectionClose)
    inherited Destroy;
    fClientThread.Free;
  end;
end;

function TWebSocketProcessClient.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequest;
begin
  RequestProcess := (fSocket as THttpClientWebSockets).fOnCallbackRequestProcess;
  if Assigned(RequestProcess) then
    result := THttpServerRequest.Create(nil,0,fOwnerThread) else
    result := nil;
end;


{ TWebSocketProcessClientThread }

constructor TWebSocketProcessClientThread.Create(aProcess: TWebSocketProcessClient);
begin
  fProcess := aProcess;
  fProcess.fOwnerThread := self;
  inherited Create({suspended=}false);
end;

procedure TWebSocketProcessClientThread.Execute;
begin
  try
    fThreadState := sRun;
    if fProcess<>nil then // may happen when debugging under FPC (alf)
      SetCurrentThreadName('% % %',[fProcess.fProcessName,self,fProcess.Protocol.Name]);
    WebSocketLog.Add.Log(sllDebug,'Execute: before ProcessLoop %', [fProcess], self);
    if not Terminated and (fProcess<>nil) then
      fProcess.ProcessLoop;
    WebSocketLog.Add.Log(sllDebug,'Execute: after ProcessLoop %',[fProcess],self);
    if (fProcess<>nil) and (fProcess.Socket<>nil) and
       fProcess.Socket.InheritsFrom(THttpClientWebSockets) then
      with THttpClientWebSockets(fProcess.Socket) do
        if Assigned(OnWebSocketsClosed) then
          OnWebSocketsClosed(self);
  except // ignore any exception in the thread
  end;
  fThreadState := sFinished; // safely set final state
  if (fProcess<>nil) and (fProcess.fState=wpsClose) then
    fThreadState := sClosed;
  WebSocketLog.Add.Log(sllDebug,'Execute: done (%)',[ToText(fThreadState)^],self);
end;


{ ------------ client or server asynchronous process of multiple connections }

{ TAsynchConnection }

constructor TAsynchConnection.Create(const aRemoteIP: RawUTF8);
begin
  inherited Create;
  fRemoteIP := aRemoteIP;
end;

procedure TAsynchConnection.AfterCreate(Sender: TAsynchConnections);
begin
  fLastOperation := UnixTimeUTC;
end;

procedure TAsynchConnection.OnLastOperationIdle(Sender: TAsynchConnections);
begin
  fLastOperation := UnixTimeUTC;
end;

procedure TAsynchConnection.AfterWrite(Sender: TAsynchConnections);
begin // do nothing
end;

procedure TAsynchConnection.BeforeDestroy(Sender: TAsynchConnections);
begin
  fHandle := 0; // to detect any dangling pointer
end;


{ TAsynchConnectionsSockets }

procedure TAsynchConnectionsSockets.OnClose(connection: TObject);
begin
  // caller did call Stop() before calling OnClose (socket=0)
  fOwner.fLog.Add.Log(sllTrace,'OnClose%',[connection],self);
  fOwner.ConnectionDelete((connection as TAsynchConnection).Handle); // do connection.Free
end;

function TAsynchConnectionsSockets.OnError(connection: TObject;
  events: TPollSocketEvents): boolean;
begin
  fOwner.fLog.Add.Log(sllDebug,'OnError% events=[%] -> free socket and instance',
    [connection,GetSetName(TypeInfo(TPollSocketEvents),events)],self);
  result := acoOnErrorContinue in fOwner.Options; // false=close by default
end;

function TAsynchConnectionsSockets.OnRead(connection: TObject): TPollAsynchSocketOnRead;
var ac: TAsynchConnection;
begin
  ac := connection as TAsynchConnection;
  if not(acoNoLogRead in fOwner.Options) then
    fOwner.fLog.Add.Log(sllTrace,'OnRead% len=%',[ac,length(ac.fSlot.readbuf)],self);
  result := ac.OnRead(fOwner);
  if not (acoLastOperationNoRead in fOwner.Options) then
    ac.fLastOperation := UnixTimeUTC;
end;

function TAsynchConnectionsSockets.SlotFromConnection(connection: TObject): PPollSocketsSlot;
begin
  try
    if (connection=nil) or not connection.InheritsFrom(TAsynchConnection) or
       (TAsynchConnection(connection).Handle=0) then begin
      fOwner.fLog.Add.Log(sllStackTrace,'SlotFromConnection() with dangling pointer %',
        [connection],self);
      result := nil;
    end else
      result := @TAsynchConnection(connection).fSlot;
  except
    fOwner.fLog.Add.Log(sllError,'SlotFromConnection() with dangling pointer %',
     [pointer(connection)],self);
    result := nil;
  end;
end;

function TAsynchConnectionsSockets.Write(connection: TObject;
  const data; datalen, timeout: integer): boolean;
var tmp: TLogEscape;
begin
  result := inherited Write(connection,data,datalen,timeout);
  if result and not (acoLastOperationNoWrite in fOwner.Options) then
    (connection as TAsynchConnection).fLastOperation := UnixTimeUTC;
  if (fOwner.fLog<>nil) and not(acoNoLogWrite in fOwner.Options) then
    fOwner.fLog.Add.Log(sllTrace,'Write%=% len=%%',
      [connection,BOOL_STR[result],datalen,LogEscape(@data,datalen,tmp,
       acoVerboseLog in fOwner.Options)],self);
end;

procedure TAsynchConnectionsSockets.AfterWrite(connection: TObject);
begin
  (connection as TAsynchConnection).AfterWrite(fOwner);
end;

function TAsynchConnectionsSockets.GetTotal: integer;
begin
  result := fOwner.fLastHandle; // by definition
end;


{ TAsynchConnectionsThread }

constructor TAsynchConnectionsThread.Create(aOwner: TAsynchConnections;
  aProcess: TPollSocketEvent);
begin
  fOwner := aOwner;
  fProcess := aProcess;
  fOnThreadTerminate := fOwner.fOnThreadTerminate;
  inherited Create(false);
end;

procedure TAsynchConnectionsThread.Execute;
var idletix: Int64;
begin
  SetCurrentThreadName('% % %',[fOwner.fProcessName,self,ToText(fProcess)^]);
  fOwner.NotifyThreadStart(self);
  try
    idletix := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64+1000;
    while not Terminated and (fOwner.fClients<>nil) do begin
      // implement parallel client connections for TAsynchClient
      if (fOwner.fThreadClients.Count>0) and
         (InterlockedDecrement(fOwner.fThreadClients.Count)>=0) then
        fOwner.ThreadClientsConnect else
      // generic TAsynchConnections read/write process
      case fProcess of
      pseRead:
        fOwner.fClients.ProcessRead(30000);
      pseWrite: begin
        fOwner.fClients.ProcessWrite(30000);
        if {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64>=idletix then begin
          fOwner.IdleEverySecond; // may take some time -> retrieve ticks again
          idletix := {$ifdef FPCLINUX}SynFPCLinux.{$endif}GetTickCount64+1000;
        end;
      end;
      else
        raise EAsynchConnections.CreateUTF8('%.Execute: unexpected fProcess=%',
          [self,ToText(fProcess)^]);
      end;
    end;
  except
    on E: Exception do
      fOwner.fLog.Add.Log(sllWarning,'Execute raised a % -> terminate % thread',
        [E.ClassType,fOwner.fStreamClass],self);
  end;
  fOwner.fLog.Add.Log(sllDebug,'Execute: done', self);
end;


{ TAsynchConnections }

function TAsynchConnectionCompareByHandle(const A,B): integer;
begin // for fast binary search from the connection handle
  result := TAsynchConnection(A).Handle-TAsynchConnection(B).Handle;
end;

constructor TAsynchConnections.Create(OnStart, OnStop: TNotifyThreadEvent;
  aStreamClass: TAsynchConnectionClass; const ProcessName: SockString;
  aLog: TSynLogClass; aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer);
var i: integer;
    log: ISynLog;
begin
  log := aLog.Enter('Create(%,%,%)',[aStreamClass,ProcessName,aThreadPoolCount],self);
  if (aStreamClass=TAsynchConnection) or (aStreamClass=nil) then
    raise EAsynchConnections.CreateUTF8('%.Create(%)',[self,aStreamClass]);
  if aThreadPoolCount<=0 then
    aThreadPoolCount := 1;
  fLog := aLog;
  fStreamClass := aStreamClass;
  fConnectionLock.Init;
  fConnections.Init(TypeInfo(TAsynchConnectionObjArray),fConnection,@fConnectionCount);
  fConnections.Compare := TAsynchConnectionCompareByHandle;
  fConnections.IsObjArray := false; // to call TAsynchConnection.BeforeDestroy
  fClients := TAsynchConnectionsSockets.Create;
  fClients.fOwner := self;
  fTempConnectionForSearchPerHandle := fStreamClass.Create('');
  fOptions := aOptions;
  inherited Create(false,OnStart,OnStop,ProcessName);
  SetLength(fThreads,aThreadPoolCount+1);
  fThreads[0] := TAsynchConnectionsThread.Create(self,pseWrite);
  for i := 1 to aThreadPoolCount do
    fThreads[i] := TAsynchConnectionsThread.Create(self,pseRead);
end;

destructor TAsynchConnections.Destroy;
var i: integer;
begin
  with fClients do
    fLog.Add.Log(sllDebug,'Destroy total=% reads=%/% writes=%/%',[Total,
      ReadCount,KB(ReadBytes),WriteCount,KB(WriteBytes)],self);
  Terminate;
  for i := 0 to high(fThreads) do
    fThreads[i].Terminate; // stop ProcessRead/ProcessWrite when polling stops
  FreeAndNil(fClients); // stop polling and refuse further Write/ConnectionRemove
  ObjArrayClear(fThreads);
  inherited Destroy;
  for i := 0 to fConnectionCount-1 do
    try
      fConnection[i].BeforeDestroy(self);
      fConnection[i].Free;
    except
    end;
  fConnectionLock.Done;
  fTempConnectionForSearchPerHandle.Free;
end;

procedure TAsynchConnections.ThreadClientsConnect;
var client: TSocket;
    connection: TAsynchConnection;
begin
  with fThreadClients do
    client := CallServer(Address,Port,false,cslTCP,Timeout);
  if client<0 then
    raise ECrtSocket.CreateFmt('%s: %s:%s connection failure', // ECrtSocket for error
      [ClassName,fThreadClients.Address,fThreadClients.Port],-1);
  connection := nil;
  if not ConnectionCreate(client,{ip=}'',connection) then
    DirectShutdown(client);
end;

function TAsynchConnections.ConnectionCreate(aSocket: TSocket; const aRemoteIp: RawUTF8;
  out aConnection: TAsynchConnection): boolean;
begin // you can override this class then call ConnectionAdd
  if Terminated then
    result := false else begin
    aConnection := fStreamClass.Create(aRemoteIP);
    result := ConnectionAdd(aSocket,aConnection);
  end;
end;

function TAsynchConnections.ConnectionAdd(aSocket: TSocket;
  aConnection: TAsynchConnection): boolean;
begin
  result := false; // caller should release aSocket
  if Terminated then
    exit;
  aConnection.fSlot.socket := aSocket;
  fConnectionLock.Lock;
  try
    inc(fLastHandle);
    aConnection.fHandle := fLastHandle;
    fConnections.Add(aConnection);
    fLog.Add.Log(sllTrace,'ConnectionAdd% count=%',[aConnection,fConnectionCount],self);
    fConnections.Sorted := true; // handles are increasing
  finally
    fConnectionLock.UnLock;
  end;
  aConnection.AfterCreate(self);
  result := true; // indicates aSocket owned by the pool
end;

function TAsynchConnections.ConnectionDelete(
  aConnection: TAsynchConnection; aIndex: integer): boolean;
var t: TClass;
    h: TAsynchConnectionHandle;
begin // caller should have done fConnectionLock.Lock
  try
    h := aConnection.Handle;
    t := aConnection.ClassType;
    aConnection.BeforeDestroy(self);
    aConnection.Free;
  finally
    fConnections.FastDeleteSorted(aIndex);
  end;
  fLog.Add.Log(sllTrace,'ConnectionDelete %.Handle=% count=%',[t,h,fConnectionCount],self);
  result := true;
end;

function TAsynchConnections.ConnectionDelete(aHandle: TAsynchConnectionHandle): boolean;
var i: integer;
    conn: TAsynchConnection;
begin // don't call fClients.Stop() here - see ConnectionRemove()
  result := false;
  if Terminated or (aHandle<=0) then
    exit;
  conn := ConnectionFindLocked(aHandle,@i);
  if conn<>nil then
  try
    result := ConnectionDelete(conn,i);
  finally
    fConnectionLock.UnLock;
  end;
  if not result then
    fLog.Add.Log(sllTrace,'ConnectionDelete(%)=false count=%',
      [aHandle,fConnectionCount],self);
end;

function TAsynchConnections.ConnectionFindLocked(aHandle: TAsynchConnectionHandle;
  aIndex: PInteger): TAsynchConnection;
var i: integer;
begin
  result := nil;
  if (self=nil) or Terminated or (aHandle<=0) then
    exit;
  fConnectionLock.Lock;
  try
    fTempConnectionForSearchPerHandle.fHandle := aHandle;
    i := fConnections.Find(fTempConnectionForSearchPerHandle); // fast O(log(n)) binary search
    if i>=0 then begin
      result := fConnection[i];
      if aIndex<>nil then
        aIndex^ := i;
    end;
    fLog.Add.Log(sllTrace,'ConnectionFindLocked(%)=%',[aHandle,result],self);
  finally
    if result=nil then
      fConnectionLock.UnLock;
  end;
end;

function TAsynchConnections.ConnectionRemove(aHandle: TAsynchConnectionHandle): boolean;
var i: integer;
    conn: TAsynchConnection;
begin
  result := false;
  if (self=nil) or Terminated or (aHandle<=0) then
    exit;
  conn := ConnectionFindLocked(aHandle,@i);
  if conn<>nil then
    try
      if not fClients.Stop(conn) then
        fLog.Add.Log(sllDebug,'ConnectionRemove: Stop=false for %',[conn],self);
      result := ConnectionDelete(conn,i);
    finally
      fConnectionLock.UnLock;
    end;
  if not result then
    fLog.Add.Log(sllTrace,'ConnectionRemove(%)=false',[aHandle],self);
end;

procedure TAsynchConnections.Lock;
begin
  fConnectionLock.Lock;
end;

procedure TAsynchConnections.Unlock;
begin
  fConnectionLock.UnLock;
end;

function TAsynchConnections.Write(connection: TAsynchConnection;
  const data; datalen: integer): boolean;
begin
  if Terminated then
    result := false else
    result := fClients.Write(connection,data,datalen);
end;

function TAsynchConnections.Write(connection: TAsynchConnection;
  const data: SockString): boolean;
begin
  if Terminated then
    result := false else
    result := fClients.WriteString(connection,data);
end;

procedure TAsynchConnections.LogVerbose(connection: TAsynchConnection;
  const ident: RawUTF8; frame: pointer; framelen: integer);
var tmp: TLogEscape;
begin
  if not(acoNoLogRead in Options) and (acoVerboseLog in Options) and (fLog<>nil) then
    fLog.Add.Log(sllTrace,'% len=%%',[ident,framelen,
      LogEscape(frame,framelen,tmp)],connection);
end;

procedure TAsynchConnections.LogVerbose(connection: TAsynchConnection;
  const ident: RawUTF8; const frame: RawByteString);
begin
  LogVerbose(connection,ident,pointer(frame),length(frame));
end;

procedure TAsynchConnections.IdleEverySecond;
var i,n: integer;
    allowed: cardinal;
    log: ISynLog;
begin
  if Terminated or (LastOperationIdleSeconds<=0) then
    exit;
  fConnectionLock.Lock;
  try
    n := 0;
    allowed := UnixTimeUTC-LastOperationIdleSeconds;
    for i := 0 to fConnectionCount-1 do
      if fConnection[i].fLastOperation<allowed then
      try
        if log=nil then
          log := fLog.Enter(self{$ifndef DELPHI5OROLDER},'IdleEverySecond'{$endif});
        fConnection[i].OnLastOperationIdle(self);
        inc(n);
      except
      end;
    if log<>nil then
      log.Log(sllTrace,'IdleEverySecond notified % %',[n,fStreamClass],self);
  finally
    fConnectionLock.UnLock;
  end;
end;


{ TAsynchServer }

constructor TAsynchServer.Create(const aPort: SockString; OnStart,
  OnStop: TNotifyThreadEvent; aStreamClass: TAsynchConnectionClass;
  const ProcessName: SockString; aLog: TSynLogClass;
  aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer);
begin
  fServer := TCrtSocket.Bind(aPort);
  inherited Create(OnStart,OnStop,aStreamClass,ProcessName,aLog,aOptions,aThreadPoolCount);
end;

destructor TAsynchServer.Destroy;
var endtix: Int64;
begin
  Terminate;
  fServer.Close; // shutdown the socket to unlock Accept() in Execute
  DirectShutdown(CallServer('127.0.0.1',fServer.Port,false,cslTCP,1)); // paranoid
  endtix := GetTickCount64+10000;
  inherited Destroy;
  while not fExecuteFinished and (GetTickCount64<endtix) do
    SleepHiRes(1); // wait for Execute to be finalized (unlikely)
  fServer.Free;
end;

procedure TAsynchServer.Execute;
var client: TSocket;
    connection: TAsynchConnection;
    sin: TVarSin;
    ip: SockString;
    err: string;
begin
  SetCurrentThreadName('% % Accept',[fProcessName,self]);
  NotifyThreadStart(self);
  if fServer.Sock<>0 then
  try
    while not Terminated do begin
      client := Accept(fServer.Sock,sin);
      if client<0 then
        if Terminated then
          break else begin
          err := SocketErrorMessage;
          fLog.Add.Log(sllWarning,'Execute: Accept()=%',[err],self);
          if acoOnAcceptFailureStop in fOptions then
            if WSAGetLastError=WSAEMFILE then
              raise EAsynchConnections.CreateUTF8('%.Execute: too many connections', [self]) else
              raise EAsynchConnections.CreateUTF8('%.Execute: Accept failed as %',[self,err]);
          SleepHiRes(1);
          continue;
        end;
      if Terminated then begin
        DirectShutdown(client);
        break;
      end;
      IPText(sin,ip);
      if ConnectionCreate(client,ip,connection) then
        if fClients.Start(connection) then
          fLog.Add.Log(sllTrace,'Execute: Accept()=%',[connection], self) else
          connection.Free else
        DirectShutdown(client);
    end;
  except
    on E: Exception do
      fLog.Add.Log(sllWarning,'Execute raised % -> terminate %',
        [E.ClassType,fProcessName],self);
  end;
  fLog.Add.Log(sllDebug, 'Execute: % done', [fProcessName], self);
  fExecuteFinished := true;
end;



{ TAsynchClient }

constructor TAsynchClient.Create(const aServer, aPort: SockString;
  aClientsCount,aClientsTimeoutSecs: integer; OnStart, OnStop: TNotifyThreadEvent;
  aStreamClass: TAsynchConnectionClass; const ProcessName: SockString;
  aLog: TSynLogClass; aOptions: TAsynchConnectionsOptions; aThreadPoolCount: integer);
begin
  fThreadClients.Count := aClientsCount;
  fThreadClients.Timeout := aClientsTimeoutSecs*1000;
  fThreadClients.Address := aServer;
  fThreadClients.Port := aPort;
  inherited Create(OnStart,OnStop,aStreamClass,ProcessName,aLog,aOptions,aThreadPoolCount);
end;

procedure TAsynchClient.Execute;
begin
  SetCurrentThreadName('% %',[fProcessName,self]);
  NotifyThreadStart(self);
  try
    while InterlockedDecrement(fThreadClients.Count)>=0 do
      ThreadClientsConnect; // will connect some clients in this main thread
  except
    on E: Exception do
      fLog.Add.Log(sllWarning,'Execute raised % -> terminate %',
        [E.ClassType,fProcessName],self);
  end;
  fLog.Add.Log(sllDebug, 'Execute: % done', [fProcessName], self);
end;


initialization
  GetEnumNames(TypeInfo(TWebSocketFrameOpCode),@_TWebSocketFrameOpCode);
  GetEnumNames(TypeInfo(TWebSocketProcessNotifyCallback),@_TWebSocketProcessNotifyCallback);
end.
