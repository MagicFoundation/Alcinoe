{*******************************************************************************
Delphi Client for memcached database.

What is Memcached?  Free & open source, high-performance,
distributed memory object caching system, generic in
nature, but intended for use in speeding up dynamic web
applications by alleviating database load.

Memcached is an in-memory key-value store for small chunks
of arbitrary data (strings, objects) from results of
database calls, API calls, or page rendering.

Memcached is simple yet powerful. Its simple design promotes
quick deployment, ease of development, and solves many
problems facing large data caches.
*******************************************************************************}

unit ALMemCachedClient;

interface

uses
  Winapi.WinSock2,
  System.Contnrs,
  System.SyncObjs,
  System.Diagnostics,
  ALCommon,
  ALString;

type

    {---------------------------------------}
    TAlMemCachedClient_responseType = (rpEND,
                                       rpOK,
                                       rpCRLF,
                                       rpSTORAGE,
                                       rpRETRIEVAL,
                                       rpRETRIEVALS,
                                       rpDELETE,
                                       rpINCRDECR,
                                       rpTOUCH,
                                       rpNONE);

    {------------------------------------}
    TAlMemCachedClient_StoredItem = record
      key: ansiString;
      flags: integer;
      cas_id: int64;
      data: ansiString;
    end;
    TAlMemCachedClient_StoredItems = array of TAlMemCachedClient_StoredItem;

    {-----------------------------------------------}
    EAlMemCachedClientException = class(EALException)
    private
      FCloseConnection: Boolean;
    public
      constructor Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
      constructor CreateFmt(const aMsg: ansistring;
                            const aArgs: array of const;
                            const aCloseConnection: Boolean = False);
      property CloseConnection: Boolean read FCloseConnection write FCloseConnection;
    end;

    {-------------------------------------}
    TAlBaseMemCachedClient = class(TObject)
    Private
      FSendTimeout: Integer;
      FReceiveTimeout: Integer;
      fKeepAlive: Boolean;
      fTCPNoDelay: Boolean;
    protected
      procedure DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer); virtual;
      procedure DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean); virtual;
      procedure SetSendTimeout(const Value: integer); virtual;
      procedure SetReceiveTimeout(const Value: integer); virtual;
      procedure SetKeepAlive(const Value: boolean); virtual;
      procedure SetTCPNoDelay(const Value: boolean); virtual;
      procedure CheckError(Error: Boolean);
      procedure CheckKey(Const Key: ansiString);
      procedure DoConnect(var aSocketDescriptor: TSocket;
                          const aHost: AnsiString;
                          const APort: integer;
                          const aSendTimeout: Integer;
                          const aReceiveTimeout: Integer;
                          const aKeepAlive: Boolean;
                          const aTCPNoDelay: Boolean);
      Procedure DoDisconnect(var aSocketDescriptor: TSocket); virtual;
      Function SocketWrite(aSocketDescriptor: TSocket; const Buf; len: Integer): Integer; Virtual;
      Function SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer; Virtual;
      Function SendCmd(aSocketDescriptor: TSocket;
                       const aCmd: AnsiString;
                       aResponseType: TAlMemCachedClient_responseType;
                       var aGETStoredItems: TAlMemCachedClient_StoredItems): AnsiString; overload; virtual;
      Function SendCmd(aSocketDescriptor: TSocket;
                       const aCmd: AnsiString;
                       aResponseType: TAlMemCachedClient_responseType): AnsiString; overload; virtual;
      Function GetResponse(aSocketDescriptor: TSocket;
                           aResponseType: TAlMemCachedClient_responseType;
                           var aGETStoredItems: TAlMemCachedClient_StoredItems): AnsiString;
      function DoGet(aSocketDescriptor: TSocket;
                     const key: ansiString;
                     var flags: integer;
                     var data: ansiString): boolean; overload; virtual;
      function DoGet(aSocketDescriptor: TSocket;
                     const key: ansiString): AnsiString; overload; virtual;
      function DoGet(aSocketDescriptor: TSocket;
                     const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      function DoGets(aSocketDescriptor: TSocket;
                      const key: ansiString;
                      var flags: integer;
                      var cas_id: int64;
                      var data: ansiString): boolean; overload; virtual;
      function DoGets(aSocketDescriptor: TSocket;
                      const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      Procedure DoSet(aSocketDescriptor: TSocket;
                      const key: ansiString;
                      const flags: integer;
                      const exptime:integer;
                      const data: ansiString); virtual;
      procedure DoAdd(aSocketDescriptor: TSocket;
                      const key: ansiString;
                      const flags: integer;
                      const exptime:integer;
                      const data: ansiString); virtual;
      procedure DoReplace(aSocketDescriptor: TSocket;
                          const key: ansiString;
                          const flags: integer;
                          const exptime:integer;
                          const data: ansiString); virtual;
      procedure DoAppend(aSocketDescriptor: TSocket;
                         const key: ansiString;
                         const data: ansiString); virtual;
      procedure DoPrepend(aSocketDescriptor: TSocket;
                          const key: ansiString;
                          const data: ansiString); virtual;
      function DoCas(aSocketDescriptor: TSocket;
                     const key: ansiString;
                     const flags: integer;
                     const exptime:integer;
                     const cas_id: int64;
                     const data: ansiString): boolean; virtual;
      function DoDelete(aSocketDescriptor: TSocket;
                        const key: ansiString): boolean; virtual;
      function DoIncr(aSocketDescriptor: TSocket;
                       const key: ansiString;
                       const Value: int64): int64; virtual;
      function DoDecr(aSocketDescriptor: TSocket;
                      const key: ansiString;
                      const Value: int64): int64; virtual;
      Procedure DoTouch(aSocketDescriptor: TSocket;
                        const key: ansiString;
                        const exptime:integer); virtual;
      Function DoStats(aSocketDescriptor: TSocket;
                       const args: AnsiString): AnsiString; virtual;
      procedure DoFlush_all(aSocketDescriptor: TSocket;
                            delay: integer); virtual;
      Function DoVersion(aSocketDescriptor: TSocket): AnsiString; virtual;
      procedure DoVerbosity(aSocketDescriptor: TSocket;
                            level: integer); virtual;
      procedure OnCmdDone(const aCmd: AnsiString;
                          TimeTaken: double); virtual;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
      property ReceiveTimeout: Integer read FReceiveTimeout write SetReceiveTimeout;
      property KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
      property TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
    end;

    {------------------------------------------------}
    TAlMemCachedClient = class(TAlBaseMemCachedClient)
    private
      Fconnected: Boolean;
      FSocketDescriptor: TSocket;
    protected
      procedure SetSendTimeout(const Value: integer); override;
      procedure SetReceiveTimeout(const Value: integer); override;
      procedure SetKeepAlive(const Value: boolean); override;
      procedure SetTCPNoDelay(const Value: boolean); override;
    public
      constructor Create; override;
      destructor Destroy; override;
      Procedure Connect(const aHost: AnsiString; const APort: integer); virtual;
      Procedure Disconnect; virtual;
      function Get(const key: ansiString;
                   var flags: integer;
                   var data: ansiString): boolean; overload; virtual;
      function Get(const key: ansiString): AnsiString; overload; virtual;
      function Get(const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      function Gets(const key: ansiString;
                    var flags: integer;
                    var cas_id: int64;
                    var data: ansiString): boolean; overload; virtual;
      function Gets(const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      Procedure _Set(const key: ansiString;
                    const flags: integer;
                    const exptime:integer;
                    const data: ansiString); virtual;
      procedure Add(const key: ansiString;
                    const flags: integer;
                    const exptime:integer;
                    const data: ansiString); virtual;
      procedure Replace(const key: ansiString;
                        const flags: integer;
                        const exptime:integer;
                        const data: ansiString); virtual;
      procedure Append(const key: ansiString;
                       const data: ansiString); virtual;
      procedure Prepend(const key: ansiString;
                        const data: ansiString); virtual;
      function Cas(const key: ansiString;
                   const flags: integer;
                   const exptime:integer;
                   const cas_id: int64;
                   const data: ansiString): boolean; virtual;
      function Delete(const key: ansiString): boolean; virtual;
      function Incr(const key: ansiString;
                     const Value: int64): int64; virtual;
      function Decr(const key: ansiString;
                    const Value: int64): int64; virtual;
      Procedure Touch(const key: ansiString;
                      const exptime:integer); virtual;
      Function Stats(const args: AnsiString): AnsiString; virtual;
      procedure Flush_all(delay: integer); virtual;
      Function Version: AnsiString; virtual;
      procedure Verbosity(level: integer); virtual;
      Procedure quit; virtual;
      property Connected: Boolean read FConnected;
    end;

    {------------------------------------------}
    TAlMemCachedConnectionPoolContainer = record
      SocketDescriptor: TSocket;
      LastAccessDate: int64;
    End;
    TAlMemCachedConnectionPool = array of TAlMemCachedConnectionPoolContainer;

    {--------------------------------------------------------------}
    TAlMemCachedConnectionPoolClient = class(TAlBaseMemCachedClient)
    private
      FConnectionPool: TAlMemCachedConnectionPool;
      FConnectionPoolCount: integer;
      FConnectionPoolCapacity: integer;
      FConnectionPoolCS: TCriticalSection;
      FWorkingConnectionCount: Integer;
      FReleasingAllconnections: Boolean;
      FLastConnectionGarbage: Int64;
      FConnectionMaxIdleTime: integer;
      fHost: AnsiString;
      fPort: integer;
    protected
      procedure SetSendTimeout(const Value: integer); override;
      procedure SetReceiveTimeout(const Value: integer); override;
      procedure SetKeepAlive(const Value: boolean); override;
      procedure SetTCPNoDelay(const Value: boolean); override;
      Function  AcquireConnection: TSocket; virtual;
      Procedure ReleaseConnection(var SocketDescriptor: TSocket;
                                  const CloseConnection: Boolean = False); virtual;
    public
      constructor Create(const aHost: AnsiString; const APort: integer); reintroduce;
      destructor Destroy; override;
      Procedure ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True); virtual;
      function Get(const key: ansiString;
                   var flags: integer;
                   var data: ansiString): boolean; overload; virtual;
      function Get(const key: ansiString): AnsiString; overload; virtual;
      function Get(const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      function Gets(const key: ansiString;
                    var flags: integer;
                    var cas_id: int64;
                    var data: ansiString): boolean; overload; virtual;
      function Gets(const keys: array of ansiString): TAlMemCachedClient_StoredItems; overload; virtual;
      Procedure _Set(const key: ansiString;
                    const flags: integer;
                    const exptime:integer;
                    const data: ansiString); virtual;
      procedure Add(const key: ansiString;
                    const flags: integer;
                    const exptime:integer;
                    const data: ansiString); virtual;
      procedure Replace(const key: ansiString;
                        const flags: integer;
                        const exptime:integer;
                        const data: ansiString); virtual;
      procedure Append(const key: ansiString;
                       const data: ansiString); virtual;
      procedure Prepend(const key: ansiString;
                        const data: ansiString); virtual;
      function Cas(const key: ansiString;
                   const flags: integer;
                   const exptime:integer;
                   const cas_id: int64;
                   const data: ansiString): boolean; virtual;
      function Delete(const key: ansiString): boolean; virtual;
      function Incr(const key: ansiString;
                     const Value: int64): int64; virtual;
      function Decr(const key: ansiString;
                    const Value: int64): int64; virtual;
      Procedure Touch(const key: ansiString;
                      const exptime:integer); virtual;
      Function Stats(const args: AnsiString): AnsiString; virtual;
      procedure Flush_all(delay: integer); virtual;
      Function Version: AnsiString; virtual;
      procedure Verbosity(level: integer); virtual;
      property Host: ansiString read fHost;
      property Port: integer read fPort;
    end;

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  ALStringList,
  AlWinsock,
  ALWindows;

{**************************************************************************************************************}
constructor EAlMemCachedClientException.Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
begin
  fCloseConnection := aCloseConnection;
  inherited Create(aMsg);
end;

{***********************************************************************}
constructor EAlMemCachedClientException.CreateFmt(const aMsg: ansistring;
                                                  const aArgs: array of const;
                                                  const aCloseConnection: Boolean = False);
begin
  fCloseConnection := aCloseConnection;
  inherited CreateFmt(aMsg, aArgs);
end;

{****************************************}
constructor TAlBaseMemCachedClient.Create;
var LWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), LWSAData) <> 0);
  FSendTimeout := 10000; // 10 seconds
  FReceiveTimeout := 10000; // 10 seconds
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{****************************************}
destructor TAlBaseMemCachedClient.Destroy;
begin
  WSACleanup;
  inherited;
end;

{**********************************************************}
procedure TAlBaseMemCachedClient.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{*********************************************************************}
// Data stored by memcached is identified with the help of a key. A key
// is a text string which should uniquely identify the data for clients
// that are interested in storing and retrieving it.  Currently the
// length limit of a key is set at 250 characters (of course, normally
// clients wouldn't need to use such long keys); the key must not include
// control characters or whitespace.
procedure TAlBaseMemCachedClient.CheckKey(const Key: AnsiString);
var I: integer;
begin
  if Key = '' then raise EAlMemCachedClientException.Create('Key can not be empty');
  if Length(Key) > 250 then raise EAlMemCachedClientException.CreateFmt('Length limit of a key is 250 characters, key: %s', [Key]);
  for I := 1 to length(Key) do
    if (Key[I] in [#0..#32]) then raise EAlMemCachedClientException.CreateFmt('The key must not include control characters or whitespace, key: %s', [Key]);
end;

{************************************************************************}
procedure TAlBaseMemCachedClient.DoConnect(var aSocketDescriptor: TSocket;
                                           const aHost: AnsiString;
                                           const APort: integer;
                                           const aSendTimeout: Integer;
                                           const aReceiveTimeout: Integer;
                                           const aKeepAlive: Boolean;
                                           const aTCPNoDelay: Boolean);

  {--------------------------------------------------------------}
  procedure _CallServer(const Server:AnsiString; const Port:word);
  var SockAddr:Sockaddr_in;
      IP: AnsiString;
  begin
    aSocketDescriptor:=Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    CheckError(aSocketDescriptor=INVALID_SOCKET);
    FillChar(SockAddr,SizeOf(SockAddr),0);
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=swap(Port);
    SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(Server));
    If SockAddr.sin_addr.S_addr = INADDR_NONE then begin
      checkError(not ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(IP));
    end;
    CheckError(WinApi.WinSock2.Connect(aSocketDescriptor,TSockAddr(SockAddr),SizeOf(SockAddr))=SOCKET_ERROR);
  end;

begin


  Try

    _CallServer(aHost,aPort);
    DoSetSendTimeout(aSocketDescriptor, aSendTimeout);
    DoSetReceiveTimeout(aSocketDescriptor, aReceiveTimeout);
    DoSetKeepAlive(aSocketDescriptor, aKeepAlive);
    DoSetTCPNoDelay(aSocketDescriptor, aTCPNoDelay);

  Except
    DoDisconnect(aSocketDescriptor);
    raise;
  end;

end;

{****************************************************************************}
procedure TAlBaseMemCachedClient.DoDisconnect(var aSocketDescriptor: TSocket);
begin
  if aSocketDescriptor <> INVALID_SOCKET then begin
    ShutDown(aSocketDescriptor,SD_BOTH);
    CloseSocket(aSocketDescriptor);
    aSocketDescriptor := INVALID_SOCKET;
  end;
end;

{*****************************************************************}
Function TAlBaseMemCachedClient.SendCmd(aSocketDescriptor: TSocket;
                                        const aCmd: AnsiString;
                                        aResponseType: TAlMemCachedClient_responseType;
                                        var aGETStoredItems: TAlMemCachedClient_StoredItems): AnsiString;
Var P: PAnsiChar;
    L: Integer;
    ByteSent: integer;
    LStopWatch: TStopWatch;
begin

  LStopWatch := TstopWatch.StartNew;

  p:=@aCmd[1]; // pchar
  l:=length(aCmd);
  while l>0 do begin
    ByteSent:=SocketWrite(aSocketDescriptor, p^,l);
    if ByteSent<=0 then raise EALException.Create('Connection close gracefully!');
    inc(p,ByteSent);
    dec(l,ByteSent);
  end;

  if aResponseType <> rpNone then Result := GetResponse(aSocketDescriptor,
                                                        aResponseType,
                                                        aGETStoredItems)
  else begin
    setlength(aGETStoredItems, 0);
    result := '';
  end;

  LStopWatch.Stop;
  OnCmdDone(aCmd, LStopWatch.Elapsed.TotalMilliseconds);

end;

{*****************************************************************}
function TAlBaseMemCachedClient.SendCmd(aSocketDescriptor: TSocket;
                                        const aCmd: AnsiString;
                                        aResponseType: TAlMemCachedClient_responseType): AnsiString;
Var LGETStoredItems: TAlMemCachedClient_StoredItems;
begin
  result := SendCmd(aSocketDescriptor,
                    aCmd,
                    aResponseType,
                    LGETStoredItems);
end;

{*******************************************************************}
// Each command sent by a client may be answered with an error string
// from the server. These error strings come in three types:
//
// - "ERROR\r\n"
//
//   means the client sent a nonexistent command name.
//
// - "CLIENT_ERROR <error>\r\n"
//
//   means some sort of client error in the input line, i.e. the input
//   doesn't conform to the protocol in some way. <error> is a
//   human-readable error string.
//
// - "SERVER_ERROR <error>\r\n"
//
//   means some sort of server error prevents the server from carrying
//   out the command. <error> is a human-readable error string. In cases
//   of severe server errors, which make it impossible to continue
//   serving the client (this shouldn't normally happen), the server will
//   close the connection after sending the error line. This is the only
//   case in which the server closes a connection to a client.
//
//
// In the descriptions of individual commands below, these error lines
// are not again specifically mentioned, but clients must allow for their
// possibility.
function TAlBaseMemCachedClient.GetResponse(aSocketDescriptor: TSocket;
                                            aResponseType: TAlMemCachedClient_responseType;
                                            var aGETStoredItems: TAlMemCachedClient_StoredItems): AnsiString;
Var LBytesReceived: Integer;
    LResultPos: Integer;
    LHeaderStartPos: Integer;
    LDataStartPos: Integer;
    LDataLength: Integer;
    LFlags: integer;
    LCasID: Int64;
    LKey: AnsiString;
    LLst: TalStringList;
    LResponseStatus: ansiString;
    LResponseStatusStartPos: integer;
const LBuffSize: integer = 4096;
begin

  //init local var
  LDataStartPos := -1;
  LDataLength := -1;
  LFlags := -1;
  LCasID := -1;
  LKey := '';
  setlength(aGETStoredItems, 0);
  Setlength(Result,LBuffSize);
  LResultPos := 0;

  //loop still we receive the full answer
  While True do begin

    //expnd the buffer
    if LResultPos = length(Result) then setlength(Result, length(Result) + LBuffSize);

    //read string from socket
    LBytesReceived := SocketRead(aSocketDescriptor, Result[LResultPos+1], length(Result) - LResultPos);
    If LBytesReceived <= 0 then raise EALException.Create('Connection close gracefully!');
    LResultPos := LResultPos + LBytesReceived;

    //all en response finish by #10 - so use #10 as a key
    If (Result[LResultPos] = #10) then begin

      // ...#13#10END#13#10
      //                ^aResultPos

      //init aResponseStatus
      if (aResponseType <> rpCRLF) then LResponseStatusStartPos := LResultPos - 2  // ...#13#10END#13#10
                                                                                   //            ^aResponseStatusStartPos
      else LResponseStatusStartPos := LResultPos - 1; // ...#13#10END#13#10
                                                      //             ^aResponseStatusStartPos
      while (LResponseStatusStartPos > 1) and (result[LResponseStatusStartPos] <> #13) do dec(LResponseStatusStartPos); // ...#13#10END#13#10
                                                                                                                        //    ^^^^^^^^^aResponseStatusStartPos
      LResponseStatus := AlCopyStr(Result, LResponseStatusStartPos, LResultPos-LResponseStatusStartPos+1); // #13#10END#13#10
      if (aResponseType <> rpCRLF) then LResponseStatus := ALTrim(LResponseStatus); // END


      // rpEND Response
      // example:
      //
      // stats\r\n
      // STAT pid 5048\r\n
      // STAT uptime 27\r\n
      // STAT evictions 0\r\n
      // STAT reclaimed 0\r\n
      // END\r\n
      //
      if (aResponseType = rpEND) and
         (LResponseStatus = 'END') then begin
        Setlength(Result,LResponseStatusStartPos-1); // STAT pid 5048\r\n
                                                     // STAT uptime 27\r\n
                                                     // STAT evictions 0\r\n
                                                     // STAT reclaimed 0
        Break;
      end

      // rpOK Response
      // example:
      //
      // flush_all\r\n
      // OK\r\n
      //
      else if (aResponseType = rpOK) and
              (LResponseStatusStartPos=1) and
              (LResponseStatus = 'OK') then begin
        Result := LResponseStatus;  // OK
        Break;
      end

      // rpSTORAGE Response
      // example:
      //
      // set test 0 900 6\r\n
      // azerty\r\n
      // STORED\r\n
      //
      else if (aResponseType = rpSTORAGE) and
              (LResponseStatusStartPos=1) and
              ((LResponseStatus = 'STORED') or
               (LResponseStatus = 'NOT_STORED') or
               (LResponseStatus = 'EXISTS') or
               (LResponseStatus = 'NOT_FOUND')) then begin
        Result := LResponseStatus; // STORED
        Break;
      end

      // rpDELETE Response
      // example:
      //
      // delete stephane\r\n
      // DELETED\r\n
      //
      else if (aResponseType = rpDELETE) and
              (LResponseStatusStartPos=1) and
              ((LResponseStatus = 'DELETED') or
               (LResponseStatus = 'NOT_FOUND')) then begin
        Result := LResponseStatus; // DELETED
        Break;
      end

      // rpINCRDECR Response
      // example:
      //
      // incr stephane 7\r\n
      // 130\r\n
      //
      else if (aResponseType = rpINCRDECR) and
              (LResponseStatusStartPos=1) and
              ((ALIsInt64(LResponseStatus)) or
               (LResponseStatus = 'NOT_FOUND')) then begin
        Result := LResponseStatus; // DELETED
        Break;
      end

      // rpTOUCH Response
      // example:
      //
      // actually seam to not work on my version of memcached
      //
      else if (aResponseType = rpINCRDECR) and
              (LResponseStatusStartPos=1) and
              ((LResponseStatus = 'TOUCHED') or
               (LResponseStatus = 'NOT_FOUND')) then begin
        Result := LResponseStatus; // TOUCHED
        Break;
      end

      // rpRETRIEVAL Response
      // example:
      //
      // get test\r\n
      // VALUE test 0 6\r\n
      // azerty\r\n
      // END\r\n
      //
      // rpRETRIEVALS Response
      // example:
      //
      // gets hihi hoho haha\r\n
      // VALUE hihi 0 4 12\r\n
      // AZER\r\n
      // VALUE hoho 0 10 13\r\n
      // AZDFUJNERT\r\n
      // VALUE haha 0 5 14\r\n
      // AZIUD\r\n
      // END\r\n
      //
      else if (aResponseType in [rpRETRIEVAL, rpRETRIEVALS]) and
              (LResponseStatus = 'END') then begin

        //no item founded
        if LResponseStatusStartPos = 1 then begin
          Result := '';
          Break;
        end;

        //init 1rt item
        if LDataStartPos < 0 then begin
          LDataStartPos := 1;
          while (LDataStartPos < LResultPos) and (Result[LDataStartPos] <> #13) do inc(LDataStartPos); // VALUE hihi 0 4 12\r\nAZER\r\nVALUE hoho 0 10 13\r\nAZDFUJNERT\r\nVALUE haha 0 5 14\r\nAZIUD\r\nEND\r\n
                                                                                                       // ^^^^^^^^^^^^^^^^^^aDataStartPos
          LLst := TalStringList.Create;
          try
            LLst.LineBreak := ' ';
            LLst.Text := ALCopyStr(Result,1, LDataStartPos-1); // VALUE
                                                               // hihi
                                                               // 0
                                                               // 4
                                                               // 12
            if aResponseType = rpRETRIEVALS then begin
              if (LLst.Count <> 5) or
                 (not alTryStrToInt(LLst[2], LFlags)) or
                 (not alTryStrToInt(LLst[3], LDataLength)) or
                 (not alTryStrToInt64(LLst[4], LCasID)) then raise EALException.CreateFmt('Memcache Error: Response does not conform to protocol - %s', [ALcopyStr(Result, 1, LResultPos)]);
            end
            else begin
              LCasID := 0;
              if (LLst.Count <> 4) or
                 (not alTryStrToInt(LLst[2], LFlags)) or
                 (not alTryStrToInt(LLst[3], LDataLength)) then raise EALException.CreateFmt('Memcache Error: Response does not conform to protocol - %s', [ALcopyStr(Result, 1, LResultPos)]);
            end;
            LKey := LLst[1];
            LDataStartPos := LDataStartPos + 2; // VALUE hihi 0 4 12\r\nAZER\r\nVALUE hoho 0 10 13\r\nAZDFUJNERT\r\nVALUE haha 0 5 14\r\nAZIUD\r\nEND\r\n
                                                //                      ^ aDataStartPos

          finally
            LLst.Free;
          end;
        end;

        //loop on all other items
        while LDataStartPos + LDataLength < LResponseStatusStartPos do begin
          setlength(aGETStoredItems, length(aGETStoredItems) + 1);
          aGETStoredItems[high(aGETStoredItems)].key := LKey;
          aGETStoredItems[high(aGETStoredItems)].flags := LFlags;
          aGETStoredItems[high(aGETStoredItems)].cas_id := LCasID;
          aGETStoredItems[high(aGETStoredItems)].data := AlcopyStr(Result, LDataStartPos, LDataLength);

          LHeaderStartPos := LDataStartPos + LDataLength + 2; // VALUE hihi 0 4 12\r\nAZER\r\nVALUE hoho 0 10 13\r\nAZDFUJNERT\r\nVALUE haha 0 5 14\r\nAZIUD\r\nEND\r\n
                                                              //                              ^aHeaderStartPos
          LDataStartPos := LHeaderStartPos;
          while (LDataStartPos < LResultPos) and (Result[LDataStartPos] <> #13) do inc(LDataStartPos); // VALUE hihi 0 4 12\r\nAZER\r\nVALUE hoho 0 10 13\r\nAZDFUJNERT\r\nVALUE haha 0 5 14\r\nAZIUD\r\nEND\r\n
                                                                                                       //                              ^^^^^^^^^^^^^^^^^^^aDataStartPos
          LLst := TalStringList.Create;
          try
            LLst.LineBreak := ' ';
            LLst.Text := ALCopyStr(Result,LHeaderStartPos, LDataStartPos - LHeaderStartPos); // VALUE
                                                                                             // hoho
                                                                                             // 0
                                                                                             // 10
                                                                                             // 13
            if aResponseType = rpRETRIEVALS then begin
              if (LLst.Count <> 5) or
                 (not alTryStrToInt(LLst[2], LFlags)) or
                 (not alTryStrToInt(LLst[3], LDataLength)) or
                 (not alTryStrToInt64(LLst[4], LCasID)) then raise EALException.Createfmt('Memcache Error: Response does not conform to protocol - %s', [ALcopyStr(Result, 1, LResultPos)]);
            end
            else begin
              LCasID := 0;
              if (LLst.Count <> 4) or
                 (not alTryStrToInt(LLst[2], LFlags)) or
                 (not alTryStrToInt(LLst[3], LDataLength)) then raise EALException.Createfmt('Memcache Error: Response does not conform to protocol - %s', [ALcopyStr(Result, 1, LResultPos)]);
            end;
            LKey := LLst[1];
            LDataStartPos := LDataStartPos + 2; // VALUE hihi 0 4 12\r\nAZER\r\nVALUE hoho 0 10 13\r\nAZDFUJNERT\r\nVALUE haha 0 5 14\r\nAZIUD\r\nEND\r\n
                                                //                                                    ^ aDataStartPos

          finally
            LLst.Free;
          end;

        end;

        //if we are at the end of the item
        if (LResponseStatusStartPos = LDataStartPos + LDataLength) then begin
          setlength(aGETStoredItems, length(aGETStoredItems) + 1);
          aGETStoredItems[high(aGETStoredItems)].key := LKey;
          aGETStoredItems[high(aGETStoredItems)].flags := LFlags;
          aGETStoredItems[high(aGETStoredItems)].cas_id := LCasID;
          aGETStoredItems[high(aGETStoredItems)].data := AlcopyStr(Result, LDataStartPos, LDataLength);
          Result := '';
          Break;
        end;

      end

      // rpCRLF Response
      // example:
      //
      // version\r\n
      // VERSION 1.4.5_4_gaa7839e\r\n
      //
      else if (aResponseType = rpCRLF) and
              (LResponseStatus = #13#10) then begin
        Setlength(Result,LResponseStatusStartPos-1); // VERSION 1.4.5_4_gaa7839e
        Break;
      end

      // ERROR Response
      else if (LResponseStatus = 'ERROR') and (LResponseStatusStartPos=1) then raise EALException.Create('Memcached Error - Nonexistent command')
      else if (ALpos('SERVER_ERROR', LResponseStatus) = 1) and (LResponseStatusStartPos=1) then raise EALException.Createfmt('Memcached Error - Server error - %s', [LResponseStatus])
      else if (ALpos('CLIENT_ERROR', LResponseStatus) = 1) and (LResponseStatusStartPos=1) then raise EAlMemCachedClientException.Create(ALformat('Memcached Error - Client error in the input line - %s', [LResponseStatus]));

    end;

  end;
end;

{************************************************************}
// The retrieval commands "get" and "gets" operates like this:
//
// get <key>*\r\n
// gets <key>*\r\n
//
// - <key>* means one or more key strings separated by whitespace.
//
// After this command, the client expects zero or more items, each of
// which is received as a text line followed by a data block. After all
// the items have been transmitted, the server sends the string
//
// "END\r\n"
//
// to indicate the end of response.
//
// Each item sent by the server looks like this:
//
// VALUE <key> <flags> <bytes> [<cas unique>]\r\n
// <data block>\r\n
//
// - <key> is the key for the item being sent
//
// - <flags> is the flags value set by the storage command
//
// - <bytes> is the length of the data block to follow, *not* including
//   its delimiting \r\n
//
// - <cas unique> is a unique 64-bit integer that uniquely identifies
//   this specific item.
//
// - <data block> is the data for this item.
//
// If some of the keys appearing in a retrieval request are not sent back
// by the server in the item list this means that the server does not
// hold items with such keys (because they were never stored, or stored
// but deleted to make space for more items, or expired, or explicitly
// deleted by a client).
function TAlBaseMemCachedClient.DoGet(aSocketDescriptor: TSocket;
                                      const key: ansiString;
                                      var flags: integer;
                                      var data: ansiString): boolean;
var LGETStoredItems: TAlMemCachedClient_StoredItems;
begin
  CheckKey(key);
  SendCmd(aSocketDescriptor, 'get ' + Key + #13#10, rpRetrieval, LGETStoredItems);
  if length(LGETStoredItems) <> 1 then begin
    result := False;
    flags := 0;
    data := '';
  end
  else begin
    result := True;
    flags := LGETStoredItems[0].flags;
    data := LGETStoredItems[0].data;
  end;
end;

{***************************************************************}
function TAlBaseMemCachedClient.DoGet(aSocketDescriptor: TSocket;
                                      const key: ansiString): AnsiString;
var LGETStoredItems: TAlMemCachedClient_StoredItems;
begin
  CheckKey(key);
  SendCmd(aSocketDescriptor, 'get ' + Key + #13#10, rpRetrieval, LGETStoredItems);
  if length(LGETStoredItems) <> 1 then raise EAlMemCachedClientException.Create('Not found');
  result := LGETStoredItems[0].data;
end;

{***************************************************************}
function TAlBaseMemCachedClient.DoGet(aSocketDescriptor: TSocket;
                                      const keys: array of ansiString): TAlMemCachedClient_StoredItems;
var LStrKeys: AnsiString;
    I: integer;
begin
  if length(keys) = 0 then raise EAlMemCachedClientException.Create('At least one key must be provided');
  LStrKeys := '';
  for I := low(keys) to High(keys) do begin
    CheckKey(keys[i]);
    LStrKeys := LStrKeys + keys[i] + ' ';
  end;
  SendCmd(aSocketDescriptor, 'get ' + AlTrim(LStrKeys) + #13#10, rpRetrieval, result);
end;

{****************************************************************}
function TAlBaseMemCachedClient.DoGets(aSocketDescriptor: TSocket;
                                       const key: ansiString;
                                       var flags: integer;
                                       var cas_id: int64;
                                       var data: ansiString): boolean;
var LGETStoredItems: TAlMemCachedClient_StoredItems;
begin
  CheckKey(key);
  SendCmd(aSocketDescriptor, 'gets ' + Key + #13#10, rpRetrievals, LGETStoredItems);
  if length(LGETStoredItems) <> 1 then begin
    result := False;
    flags := 0;
    cas_id := 0;
    data := '';
  end
  else begin
    result := True;
    flags := LGETStoredItems[0].flags;
    cas_id := LGETStoredItems[0].cas_id;
    data := LGETStoredItems[0].data;
  end;
end;

{****************************************************************}
function TAlBaseMemCachedClient.DoGets(aSocketDescriptor: TSocket;
                                       const keys: array of ansiString): TAlMemCachedClient_StoredItems;
var LStrKeys: AnsiString;
    I: integer;
begin
  if length(keys) = 0 then raise EAlMemCachedClientException.Create('At least one key must be provided');
  LStrKeys := '';
  for I := low(keys) to High(keys) do begin
    CheckKey(keys[i]);
    LStrKeys := LStrKeys + keys[i] + ' ';
  end;
  SendCmd(aSocketDescriptor, 'gets ' + AlTrim(LStrKeys) + #13#10, rpRetrievals, result);
end;

{**************************************************************}
// First, the client sends a command line which looks like this:
//
// <command name> <key> <flags> <exptime> <bytes> [noreply]\r\n
//
// - <command name> is "set", "add", "replace", "append" or "prepend"
//
// - <key> is the key under which the client asks to store the data
//
// - <flags> is an arbitrary 16-bit unsigned integer (written out in
//   decimal) that the server stores along with the data and sends back
//   when the item is retrieved. Clients may use this as a bit field to
//   store data-specific information; this field is opaque to the server.
//   Note that in memcached 1.2.1 and higher, flags may be 32-bits, instead
//   of 16, but you might want to restrict yourself to 16 bits for
//   compatibility with older versions.
//
// - <exptime> is expiration time. If it's 0, the item never expires
//   (although it may be deleted from the cache to make place for other
//   items). If it's non-zero (either Unix time or offset in seconds from
//   current time), it is guaranteed that clients will not be able to
//   retrieve this item after the expiration time arrives (measured by
//   server time).
//
// - <bytes> is the number of bytes in the data block to follow, *not*
//   including the delimiting \r\n. <bytes> may be zero (in which case
//   it's followed by an empty data block).
//
// - "noreply" optional parameter instructs the server to not send the
//   reply.  NOTE: if the request line is malformed, the server can't
//   parse "noreply" option reliably.  In this case it may send the error
//   to the client, and not reading it on the client side will break
//   things.  Client should construct only valid requests.
//
// After this line, the client sends the data block:
//
// <data block>\r\n
//
// - <data block> is a chunk of arbitrary 8-bit data of length <bytes>
//   from the previous line.
//
// After sending the command line and the data blockm the client awaits
// the reply, which may be:
//
// - "STORED\r\n", to indicate success.
//
// - "NOT_STORED\r\n" to indicate the data was not stored, but not
// because of an error. This normally means that the
// condition for an "add" or a "replace" command wasn't met.
//
// - "EXISTS\r\n" to indicate that the item you are trying to store with
// a "cas" command has been modified since you last fetched it.
//
// - "NOT_FOUND\r\n" to indicate that the item you are trying to store
// with a "cas" command did not exist.
//
// "set" means "store this data".
procedure TAlBaseMemCachedClient.DoSet(aSocketDescriptor: TSocket;
                                       const key: ansiString;
                                       const flags: integer;
                                       const exptime:integer;
                                       const data: ansiString);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  if flags < 0 then raise EAlMemCachedClientException.Create('flags must be upper or equal to 0');
  LResultCode := SendCmd(aSocketDescriptor,
                         'set '+Key+' '+
                                AlintToStr(flags)+' '+
                                ALInttostr(exptime) + ' ' +
                                ALIntToStr(length(data)) + #13#10 +
                                data + #13#10,
                         rpStorage);
  if LResultCode <> 'STORED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{************************************************************************}
// "add" means "store this data, but only if the server *doesn't* already
// hold data for this key".
procedure TAlBaseMemCachedClient.DoAdd(aSocketDescriptor: TSocket;
                                       const key: ansiString;
                                       const flags: integer;
                                       const exptime:integer;
                                       const data: ansiString);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  if flags < 0 then raise EAlMemCachedClientException.Create('flags must be upper or equal to 0');
  LResultCode := SendCmd(aSocketDescriptor,
                         'add '+Key+' '+
                                AlintToStr(flags)+' '+
                                ALInttostr(exptime) + ' ' +
                                ALIntToStr(length(data)) + #13#10 +
                                data + #13#10,
                         rpStorage);
  if LResultCode <> 'STORED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{****************************************************************}
// "replace" means "store this data, but only if the server *does*
// already hold data for this key".
procedure TAlBaseMemCachedClient.DoReplace(aSocketDescriptor: TSocket;
                                           const key: ansiString;
                                           const flags: integer;
                                           const exptime:integer;
                                           const data: ansiString);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  if flags < 0 then raise EAlMemCachedClientException.Create('flags must be upper or equal to 0');
  LResultCode := SendCmd(aSocketDescriptor,
                         'replace '+Key+' '+
                                    AlintToStr(flags)+' '+
                                    ALInttostr(exptime) + ' ' +
                                    ALIntToStr(length(data)) + #13#10 +
                                    data + #13#10,
                         rpStorage);
  if LResultCode <> 'STORED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{***********************************************************************}
// "append" means "add this data to an existing key after existing data".
// The append and prepend commands do not accept flags or exptime.
// They update existing data portions, and ignore new flag and exptime
// settings.
procedure TAlBaseMemCachedClient.DoAppend(aSocketDescriptor: TSocket;
                                          const key: ansiString;
                                          const data: ansiString);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor,
                         'append '+Key+' '+
                                   '0 '+
                                   '0 ' +
                                   ALIntToStr(length(data)) + #13#10 +
                                   data + #13#10,
                         rpStorage);
  if LResultCode <> 'STORED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{*************************************************************************}
// "prepend" means "add this data to an existing key before existing data".
// The append and prepend commands do not accept flags or exptime.
// They update existing data portions, and ignore new flag and exptime
// settings.
procedure TAlBaseMemCachedClient.DoPrepend(aSocketDescriptor: TSocket;
                                           const key: ansiString;
                                           const data: ansiString);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor,
                         'prepend '+Key+' '+
                                    '0 '+
                                    '0 ' +
                                    ALIntToStr(length(data)) + #13#10 +
                                    data + #13#10,
                         rpStorage);
  if LResultCode <> 'STORED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{*********************************************************************}
// "cas" is a check and set operation which means "store this data but
// only if no one else has updated since I last fetched it."
function TAlBaseMemCachedClient.DoCas(aSocketDescriptor: TSocket;
                                      const key: ansiString;
                                      const flags: integer;
                                      const exptime:integer;
                                      const cas_id: int64;
                                      const data: ansiString): boolean;
var LResultCode: AnsiString;
begin
  CheckKey(key);
  if flags < 0 then raise EAlMemCachedClientException.Create('flags must be upper or equal to 0');
  LResultCode := SendCmd(aSocketDescriptor,
                         'cas '+Key+' '+
                                AlintToStr(flags)+' '+
                                ALInttostr(exptime) + ' ' +
                                ALIntToStr(length(data)) + ' ' +
                                ALIntToStr(cas_id) + #13#10 +
                                data + #13#10,
                         rpStorage);
  if LResultCode = 'EXISTS' then result := False
  else if LResultCode = 'STORED' then result := True
  else raise EAlMemCachedClientException.Create(LResultCode);
end;

{************************************************************}
// The command "delete" allows for explicit deletion of items:
//
// delete <key> [noreply]\r\n
//
// - <key> is the key of the item the client wishes the server to delete
//
// - "noreply" optional parameter instructs the server to not send the
//   reply.  See the note in Storage commands regarding malformed
//   requests.
//
// The response line to this command can be one of:
//
// - "DELETED\r\n" to indicate success
//
// - "NOT_FOUND\r\n" to indicate that the item with this key was not
//   found.
//
// See the "flush_all" command below for immediate invalidation
// of all existing items.
function TAlBaseMemCachedClient.DoDelete(aSocketDescriptor: TSocket;
                                         const key: ansiString): boolean;
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor, 'delete ' + Key + #13#10, rpDelete);
  if LResultCode = 'NOT_FOUND' then result := False
  else if LResultCode = 'DELETED' then result := True
  else raise EAlMemCachedClientException.Create(LResultCode);
end;

{*****************************************************************}
// Commands "incr" and "decr" are used to change data for some item
// in-place, incrementing or decrementing it. The data for the item is
// treated as decimal representation of a 64-bit unsigned integer.  If
// the current data value does not conform to such a representation, the
// incr/decr commands return an error (memcached <= 1.2.6 treated the
// bogus value as if it were 0, leading to confusing). Also, the item
// must already exist for incr/decr to work; these commands won't pretend
// that a non-existent key exists with value 0; instead, they will fail.
//
// The client sends the command line:
//
// incr <key> <value> [noreply]\r\n
//
// or
//
// decr <key> <value> [noreply]\r\n
//
// - <key> is the key of the item the client wishes to change
//
// - <value> is the amount by which the client wants to increase/decrease
// the item. It is a decimal representation of a 64-bit unsigned integer.
//
// - "noreply" optional parameter instructs the server to not send the
//   reply.  See the note in Storage commands regarding malformed
//   requests.
//
// The response will be one of:
//
// - "NOT_FOUND\r\n" to indicate the item with this value was not found
//
// - <value>\r\n , where <value> is the new value of the item's data,
//   after the increment/decrement operation was carried out.
//
// Note that underflow in the "decr" command is caught: if a client tries
// to decrease the value below 0, the new value will be 0.  Overflow in
// the "incr" command will wrap around the 64 bit mark.
//
// Note also that decrementing a number such that it loses length isn't
// guaranteed to decrement its returned length.  The number MAY be
// space-padded at the end, but this is purely an implementation
// optimization, so you also shouldn't rely on that.
function TAlBaseMemCachedClient.DoIncr(aSocketDescriptor: TSocket;
                                       const key: ansiString;
                                       const Value: int64): int64;
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor, 'incr ' + Key + ' ' + aLintToStr(Value) + #13#10, rpINCRDECR);
  if not ALTryStrToInt64(LResultCode, Result) then raise EAlMemCachedClientException.Create(LResultCode);
end;

{****************************************************************}
function TAlBaseMemCachedClient.DoDecr(aSocketDescriptor: TSocket;
                                       const key: ansiString;
                                       const Value: int64): int64;
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor, 'decr ' + Key + ' ' + aLintToStr(Value) + #13#10, rpINCRDECR);
  if not ALTryStrToInt64(LResultCode, Result) then raise EAlMemCachedClientException.Create(LResultCode);
end;

{******************************************************************************}
// The "touch" command is used to update the expiration time of an existing item
// without fetching it.
//
// touch <key> <exptime> [noreply]\r\n
//
// - <key> is the key of the item the client wishes the server to delete
//
// - <exptime> is expiration time. Works the same as with the update commands
//   (set/add/etc). This replaces the existing expiration time. If an existing
//   item were to expire in 10 seconds, but then was touched with an
//   expiration time of "20", the item would then expire in 20 seconds.
//
// - "noreply" optional parameter instructs the server to not send the
//   reply.  See the note in Storage commands regarding malformed
//   requests.
//
// The response line to this command can be one of:
//
// - "TOUCHED\r\n" to indicate success
//
// - "NOT_FOUND\r\n" to indicate that the item with this key was not
//   found.
procedure TAlBaseMemCachedClient.DoTouch(aSocketDescriptor: TSocket;
                                         const key: ansiString;
                                         const exptime:integer);
var LResultCode: AnsiString;
begin
  CheckKey(key);
  LResultCode := SendCmd(aSocketDescriptor, 'touch ' + Key + ' ' + ALInttostr(exptime) + #13#10, rpTOUCH);
  if LResultCode <> 'TOUCHED' then raise EAlMemCachedClientException.Create(LResultCode);
end;

{**********************************************************************}
// "flush_all" is a command with an optional numeric argument. It always
// succeeds, and the server sends "OK\r\n" in response (unless "noreply"
// is given as the last parameter). Its effect is to invalidate all
// existing items immediately (by default) or after the expiration
// specified.  After invalidation none of the items will be returned in
// response to a retrieval command (unless it's stored again under the
// same key *after* flush_all has invalidated the items). flush_all
// doesn't actually free all the memory taken up by existing items; that
// will happen gradually as new items are stored. The most precise
// definition of what flush_all does is the following: it causes all
// items whose update time is earlier than the time at which flush_all
// was set to be executed to be ignored for retrieval purposes.
//
// The intent of flush_all with a delay, was that in a setting where you
// have a pool of memcached servers, and you need to flush all content,
// you have the option of not resetting all memcached servers at the
// same time (which could e.g. cause a spike in database load with all
// clients suddenly needing to recreate content that would otherwise
// have been found in the memcached daemon).
//
// The delay option allows you to have them reset in e.g. 10 second
// intervals (by passing 0 to the first, 10 to the second, 20 to the
// third, etc. etc.).
procedure TAlBaseMemCachedClient.DoFlush_all(aSocketDescriptor: TSocket;
                                             delay: integer);
begin
  if delay > 0 then SendCmd(aSocketDescriptor, 'flush_all ' + ALinttostr(delay) + #13#10, rpOK)
  else SendCmd(aSocketDescriptor, 'flush_all' + #13#10, rpOK);
end;

{***********}
// Statistics
// ----------
//
// The command "stats" is used to query the server about statistics it
// maintains and other internal data. It has two forms. Without
// arguments:
//
// stats\r\n
//
// it causes the server to output general-purpose statistics and
// settings, documented below.  In the other form it has some arguments:
//
// stats <args>\r\n
//
// Depending on <args>, various internal data is sent by the server. The
// kinds of arguments and the data sent are not documented in this version
// of the protocol, and are subject to change for the convenience of
// memcache developers.
//
//
// General-purpose statistics
// --------------------------
//
// Upon receiving the "stats" command without arguments, the server sents
// a number of lines which look like this:
//
// STAT <name> <value>\r\n
//
// The server terminates this list with the line
//
// END\r\n
//
// In each line of statistics, <name> is the name of this statistic, and
// <value> is the data.  The following is the list of all names sent in
// response to the "stats" command, together with the type of the value
// sent for this name, and the meaning of the value.
//
// In the type column below, "32u" means a 32-bit unsigned integer, "64u"
// means a 64-bit unsigned integer. '32u.32u' means two 32-bit unsigned
// integers separated by a colon (treat this as a floating point number).
//
// |-----------------------+---------+-------------------------------------------|
// | Name                  | Type    | Meaning                                   |
// |-----------------------+---------+-------------------------------------------|
// | pid                   | 32u     | Process id of this server process         |
// | uptime                | 32u     | Number of secs since the server started   |
// | time                  | 32u     | current UNIX time according to the server |
// | version               | string  | Version string of this server             |
// | pointer_size          | 32      | Default size of pointers on the host OS   |
// |                       |         | (generally 32 or 64)                      |
// | rusage_user           | 32u.32u | Accumulated user time for this process    |
// |                       |         | (seconds:microseconds)                    |
// | rusage_system         | 32u.32u | Accumulated system time for this process  |
// |                       |         | (seconds:microseconds)                    |
// | curr_items            | 32u     | Current number of items stored            |
// | total_items           | 32u     | Total number of items stored since        |
// |                       |         | the server started                        |
// | bytes                 | 64u     | Current number of bytes used              |
// |                       |         | to store items                            |
// | curr_connections      | 32u     | Number of open connections                |
// | total_connections     | 32u     | Total number of connections opened since  |
// |                       |         | the server started running                |
// | connection_structures | 32u     | Number of connection structures allocated |
// |                       |         | by the server                             |
// | reserved_fds          | 32u     | Number of misc fds used internally        |
// | cmd_get               | 64u     | Cumulative number of retrieval reqs       |
// | cmd_set               | 64u     | Cumulative number of storage reqs         |
// | cmd_flush             | 64u     | Cumulative number of flush reqs           |
// | cmd_touch             | 64u     | Cumulative number of touch reqs           |
// | get_hits              | 64u     | Number of keys that have been requested   |
// |                       |         | and found present                         |
// | get_misses            | 64u     | Number of items that have been requested  |
// |                       |         | and not found                             |
// | delete_misses         | 64u     | Number of deletions reqs for missing keys |
// | delete_hits           | 64u     | Number of deletion reqs resulting in      |
// |                       |         | an item being removed.                    |
// | incr_misses           | 64u     | Number of incr reqs against missing keys. |
// | incr_hits             | 64u     | Number of successful incr reqs.           |
// | decr_misses           | 64u     | Number of decr reqs against missing keys. |
// | decr_hits             | 64u     | Number of successful decr reqs.           |
// | cas_misses            | 64u     | Number of CAS reqs against missing keys.  |
// | cas_hits              | 64u     | Number of successful CAS reqs.            |
// | cas_badval            | 64u     | Number of CAS reqs for which a key was    |
// |                       |         | found, but the CAS value did not match.   |
// | touch_hits            | 64u     | Numer of keys that have been touched with |
// |                       |         | a new expiration time                     |
// | touch_misses          | 64u     | Numer of items that have been touched and |
// |                       |         | not found                                 |
// | auth_cmds             | 64u     | Number of authentication commands         |
// |                       |         | handled, success or failure.              |
// | auth_errors           | 64u     | Number of failed authentications.         |
// | evictions             | 64u     | Number of valid items removed from cache  |
// |                       |         | to free memory for new items              |
// | reclaimed             | 64u     | Number of times an entry was stored using |
// |                       |         | memory from an expired entry              |
// | bytes_read            | 64u     | Total number of bytes read by this server |
// |                       |         | from network                              |
// | bytes_written         | 64u     | Total number of bytes sent by this server |
// |                       |         | to network                                |
// | limit_maxbytes        | 32u     | Number of bytes this server is allowed to |
// |                       |         | use for storage.                          |
// | threads               | 32u     | Number of worker threads requested.       |
// |                       |         | (see doc/threads.txt)                     |
// | conn_yields           | 64u     | Number of times any connection yielded to |
// |                       |         | another due to hitting the -R limit.      |
// | hash_power_level      | 32u     | Current size multiplier for hash table    |
// | hash_bytes            | 64u     | Bytes currently used by hash tables       |
// | hash_is_expanding     | bool    | Indicates if the hash table is being      |
// |                       |         | grown to a new size                       |
// | expired_unfetched     | 64u     | Items pulled from LRU that were never     |
// |                       |         | touched by get/incr/append/etc before     |
// |                       |         | expiring                                  |
// | evicted_unfetched     | 64u     | Items evicted from LRU that were never    |
// |                       |         | touched by get/incr/append/etc.           |
// | slab_reassign_running | bool    | If a slab page is being moved             |
// | slabs_moved           | 64u     | Total slab pages moved                    |
// |-----------------------+---------+-------------------------------------------|
//
// Settings statistics
// -------------------
// CAVEAT: This section describes statistics which are subject to change in the
// future.
//
// The "stats" command with the argument of "settings" returns details of
// the settings of the running memcached.  This is primarily made up of
// the results of processing commandline options.
//
// Note that these are not guaranteed to return in any specific order and
// this list may not be exhaustive.  Otherwise, this returns like any
// other stats command.
//
// |-------------------+----------+----------------------------------------------|
// | Name              | Type     | Meaning                                      |
// |-------------------+----------+----------------------------------------------|
// | maxbytes          | size_t   | Maximum number of bytes allows in this cache |
// | maxconns          | 32       | Maximum number of clients allowed.           |
// | tcpport           | 32       | TCP listen port.                             |
// | udpport           | 32       | UDP listen port.                             |
// | inter             | string   | Listen interface.                            |
// | verbosity         | 32       | 0 = none, 1 = some, 2 = lots                 |
// | oldest            | 32u      | Age of the oldest honored object.            |
// | evictions         | on/off   | When off, LRU evictions are disabled.        |
// | domain_socket     | string   | Path to the domain socket (if any).          |
// | umask             | 32 (oct) | umask for the creation of the domain socket. |
// | growth_factor     | float    | Chunk size growth factor.                    |
// | chunk_size        | 32       | Minimum space allocated for key+value+flags. |
// | num_threads       | 32       | Number of threads (including dispatch).      |
// | stat_key_prefix   | char     | Stats prefix separator character.            |
// | detail_enabled    | bool     | If yes, stats detail is enabled.             |
// | reqs_per_event    | 32       | Max num IO ops processed within an event.    |
// | cas_enabled       | bool     | When no, CAS is not enabled for this server. |
// | tcp_backlog       | 32       | TCP listen backlog.                          |
// | auth_enabled_sasl | yes/no   | SASL auth requested and enabled.             |
// | item_size_max     | size_t   | maximum item size                            |
// | maxconns_fast     | bool     | If fast disconnects are enabled              |
// | hashpower_init    | 32       | Starting size multiplier for hash table      |
// | slab_reassign     | bool     | Whether slab page reassignment is allowed    |
// | slab_automove     | bool     | Whether slab page automover is enabled       |
// |-------------------+----------+----------------------------------------------|
//
//
// Item statistics
// ---------------
// CAVEAT: This section describes statistics which are subject to change in the
// future.
//
// The "stats" command with the argument of "items" returns information about
// item storage per slab class. The data is returned in the format:
//
// STAT items:<slabclass>:<stat> <value>\r\n
//
// The server terminates this list with the line
//
// END\r\n
//
// The slabclass aligns with class ids used by the "stats slabs" command. Where
// "stats slabs" describes size and memory usage, "stats items" shows higher
// level information.
//
// The following item values are defined as of writing.
//
// Name                   Meaning
// ------------------------------
// number                 Number of items presently stored in this class. Expired
//                        items are not automatically excluded.
// age                    Age of the oldest item in the LRU.
// evicted                Number of times an item had to be evicted from the LRU
//                        before it expired.
// evicted_nonzero        Number of times an item which had an explicit expire
//                        time set had to be evicted from the LRU before it
//                        expired.
// evicted_time           Seconds since the last access for the most recent item
//                        evicted from this class. Use this to judge how
//                        recently active your evicted data is.
// outofmemory            Number of times the underlying slab class was unable to
//                        store a new item. This means you are running with -M or
//                        an eviction failed.
// tailrepairs            Number of times we self-healed a slab with a refcount
//                        leak. If this counter is increasing a lot, please
//                        report your situation to the developers.
// reclaimed              Number of times an entry was stored using memory from
//                        an expired entry.
// expired_unfetched      Number of expired items reclaimed from the LRU which
//                        were never touched after being set.
// evicted_unfetched      Number of valid items evicted from the LRU which were
//                        never touched after being set.
//
// Note this will only display information about slabs which exist, so an empty
// cache will return an empty set.
//
//
// Item size statistics
// --------------------
// CAVEAT: This section describes statistics which are subject to change in the
// future.
//
// The "stats" command with the argument of "sizes" returns information about the
// general size and count of all items stored in the cache.
// WARNING: This command WILL lock up your cache! It iterates over *every item*
// and examines the size. While the operation is fast, if you have many items
// you could prevent memcached from serving requests for several seconds.
//
// The data is returned in the following format:
//
// <size> <count>\r\n
//
// The server terminates this list with the line
//
// END\r\n
//
// 'size' is an approximate size of the item, within 32 bytes.
// 'count' is the amount of items that exist within that 32-byte range.
//
// This is essentially a display of all of your items if there was a slab class
// for every 32 bytes. You can use this to determine if adjusting the slab growth
// factor would save memory overhead. For example: generating more classes in the
// lower range could allow items to fit more snugly into their slab classes, if
// most of your items are less than 200 bytes in size.
//
//
// Slab statistics
// ---------------
// CAVEAT: This section describes statistics which are subject to change in the
// future.
//
// The "stats" command with the argument of "slabs" returns information about
// each of the slabs created by memcached during runtime. This includes per-slab
// information along with some totals. The data is returned in the format:
//
// STAT <slabclass>:<stat> <value>\r\n
// STAT <stat> <value>\r\n
//
// The server terminates this list with the line
//
// END\r\n
//
// |-----------------+----------------------------------------------------------|
// | Name            | Meaning                                                  |
// |-----------------+----------------------------------------------------------|
// | chunk_size      | The amount of space each chunk uses. One item will use   |
// |                 | one chunk of the appropriate size.                       |
// | chunks_per_page | How many chunks exist within one page. A page by         |
// |                 | default is less than or equal to one megabyte in size.   |
// |                 | Slabs are allocated by page, then broken into chunks.    |
// | total_pages     | Total number of pages allocated to the slab class.       |
// | total_chunks    | Total number of chunks allocated to the slab class.      |
// | get_hits        | Total number of get requests serviced by this class.     |
// | cmd_set         | Total number of set requests storing data in this class. |
// | delete_hits     | Total number of successful deletes from this class.      |
// | incr_hits       | Total number of incrs modifying this class.              |
// | decr_hits       | Total number of decrs modifying this class.              |
// | cas_hits        | Total number of CAS commands modifying this class.       |
// | cas_badval      | Total number of CAS commands that failed to modify a     |
// |                 | value due to a bad CAS id.                               |
// | touch_hits      | Total number of touches serviced by this class.          |
// | used_chunks     | How many chunks have been allocated to items.            |
// | free_chunks     | Chunks not yet allocated to items, or freed via delete.  |
// | free_chunks_end | Number of free chunks at the end of the last allocated   |
// |                 | page.                                                    |
// | mem_requested   | Number of bytes requested to be stored in this slab[*].  |
// | active_slabs    | Total number of slab classes allocated.                  |
// | total_malloced  | Total amount of memory allocated to slab pages.          |
// |-----------------+----------------------------------------------------------|
//
// * Items are stored in a slab that is the same size or larger than the
//   item.  mem_requested shows the size of all items within a
//   slab. (total_chunks * chunk_size) - mem_requested shows memory
//   wasted in a slab class.  If you see a lot of waste, consider tuning
//   the slab factor.
Function TAlBaseMemCachedClient.DoStats(aSocketDescriptor: TSocket;
                                        const args: AnsiString): AnsiString;
begin
  if args <> '' then result := SendCmd(aSocketDescriptor, 'stats ' + args + #13#10, rpEND)
  else result := SendCmd(aSocketDescriptor, 'stats' + #13#10, rpEND);
end;

{******************************************}
// "version" is a command with no arguments:
//
// version\r\n
//
// In response, the server sends
//
// "VERSION <version>\r\n", where <version> is the version string for the
// server.
function TAlBaseMemCachedClient.DoVersion(aSocketDescriptor: TSocket): AnsiString;
begin
  Result := SendCmd(aSocketDescriptor, 'version' + #13#10, rpCRLF);
end;

// "verbosity" is a command with a numeric argument. It always succeeds,
// and the server sends "OK\r\n" in response (unless "noreply" is given
// as the last parameter). Its effect is to set the verbosity level of
// the logging output.
Procedure TAlBaseMemCachedClient.DoVerbosity(aSocketDescriptor: TSocket;
                                             level: integer);
begin
  SendCmd(aSocketDescriptor, 'verbosity ' + alinttostr(level) + #13#10, rpOK);
end;

{****************************************************************}
procedure TAlBaseMemCachedClient.OnCmdDone(const aCmd: AnsiString;
                                           TimeTaken: double);
begin
  // virtual
end;

{********************************************************************************************************}
Function TAlBaseMemCachedClient.SocketWrite(aSocketDescriptor: TSocket; const Buf; len: Integer): Integer;
begin
  Result := Send(aSocketDescriptor,Buf,len,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{*****************************************************************************************************}
function TAlBaseMemCachedClient.SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer;
begin
  Result := Recv(aSocketDescriptor,buf,len,0);
  CheckError(Result = SOCKET_ERROR);
end;

{**************************************************************************************************}
procedure TAlBaseMemCachedClient.DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{*****************************************************************************************************}
procedure TAlBaseMemCachedClient.DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{***********************************************************************************************************************}
// // http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlBaseMemCachedClient.DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean);
var LIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then LIntBool := 1
  else LIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlBaseMemCachedClient.DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean);
var LIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then LIntBool := 1
  else LIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@LIntBool),SizeOf(LIntBool))=SOCKET_ERROR);
end;

{********************************************************************}
procedure TAlBaseMemCachedClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
end;

{***********************************************************************}
procedure TAlBaseMemCachedClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
end;

{******************************************************************}
procedure TAlBaseMemCachedClient.SetKeepAlive(const Value: boolean);
begin
  FKeepAlive := Value;
end;

{*******************************************************************}
procedure TAlBaseMemCachedClient.SetTCPNoDelay(const Value: boolean);
begin
  fTCPNoDelay := Value;
end;

{****************************************************************}
procedure TAlMemCachedClient.SetSendTimeout(const Value: integer);
begin
  inherited SetSendTimeout(Value);
  if FConnected then DoSetSendTimeout(fSocketDescriptor, Value);
end;

{*******************************************************************}
procedure TAlMemCachedClient.SetReceiveTimeout(const Value: integer);
begin
  inherited SetReceiveTimeout(Value);
  if FConnected then DoSetReceiveTimeout(fSocketDescriptor, Value);
end;

{**************************************************************}
procedure TAlMemCachedClient.SetKeepAlive(const Value: boolean);
begin
  inherited SetKeepAlive(Value);
  if FConnected then DoSetKeepAlive(fSocketDescriptor, Value);
end;

{***************************************************************}
procedure TAlMemCachedClient.SetTCPNoDelay(const Value: boolean);
begin
  inherited SetTCPNoDelay(Value);
  if FConnected then DoSetTCPNoDelay(fSocketDescriptor, Value);
end;

{************************************}
constructor TAlMemCachedClient.Create;
begin
  inherited;
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
end;

{************************************}
destructor TAlMemCachedClient.Destroy;
begin
  If Fconnected then Disconnect;
  inherited;
end;

{**********************************************************************************}
procedure TAlMemCachedClient.Connect(const aHost: AnsiString; const APort: integer);
begin

  if FConnected then raise EALException.Create('MemCached component already connected');
  DoConnect(fSocketDescriptor,
            aHost,
            APort,
            fSendTimeout,
            fReceiveTimeout,
            fKeepAlive,
            fTCPNoDelay);
  fConnected := True;

end;

{**************************************}
procedure TAlMemCachedClient.Disconnect;
begin
  If Fconnected then begin
    doDisconnect(FSocketDescriptor);
    Fconnected := False;
  end;
end;

{****************************************************}
function TAlMemCachedClient.Get(const key: ansiString;
                                var flags: integer;
                                var data: ansiString): boolean;
begin
  result := DoGet(FSocketDescriptor,
                  key,
                  flags,
                  data);
end;

{*****************************************************************}
function TAlMemCachedClient.Get(const key: ansiString): AnsiString;
begin
  result := DoGet(FSocketDescriptor, key);
end;

{***********************************************************************************************}
function TAlMemCachedClient.Get(const keys: array of ansiString): TAlMemCachedClient_StoredItems;
begin
  result := DoGet(FSocketDescriptor, keys);
end;

{*****************************************************}
function TAlMemCachedClient.Gets(const key: ansiString;
                                 var flags: integer;
                                 var cas_id: int64;
                                 var data: ansiString): boolean;
begin
  result := DoGets(FSocketDescriptor,
                   key,
                   flags,
                   cas_id,
                   data);
end;

{************************************************************************************************}
function TAlMemCachedClient.Gets(const keys: array of ansiString): TAlMemCachedClient_StoredItems;
begin
  result := DoGets(FSocketDescriptor, keys);
end;

{******************************************************}
Procedure TAlMemCachedClient._Set(const key: ansiString;
                                  const flags: integer;
                                  const exptime:integer;
                                  const data: ansiString);
begin
  DoSet(FSocketDescriptor,
        key,
        flags,
        exptime,
        data);
end;

{*****************************************************}
procedure TAlMemCachedClient.Add(const key: ansiString;
                                 const flags: integer;
                                 const exptime:integer;
                                 const data: ansiString);
begin
  DoAdd(FSocketDescriptor,
        key,
        flags,
        exptime,
        data);
end;

{*********************************************************}
procedure TAlMemCachedClient.Replace(const key: ansiString;
                                     const flags: integer;
                                     const exptime:integer;
                                     const data: ansiString);
begin
  DoReplace(FSocketDescriptor,
            key,
            flags,
            exptime,
            data);
end;

{********************************************************}
procedure TAlMemCachedClient.Append(const key: ansiString;
                                    const data: ansiString);
begin
  DoAppend(FSocketDescriptor,
           key,
           data);
end;

{*********************************************************}
procedure TAlMemCachedClient.Prepend(const key: ansiString;
                                     const data: ansiString);
begin
  DoPrepend(FSocketDescriptor,
            key,
            data);
end;

{****************************************************}
function TAlMemCachedClient.Cas(const key: ansiString;
                                const flags: integer;
                                const exptime:integer;
                                const cas_id: int64;
                                const data: ansiString): boolean;
begin
  result := DoCas(FSocketDescriptor,
                  key,
                  flags,
                  exptime,
                  Cas_ID,
                  data);
end;

{*****************************************************************}
function TAlMemCachedClient.Delete(const key: ansiString): boolean;
begin
  result := DoDelete(FSocketDescriptor, key);
end;

{*****************************************************}
function TAlMemCachedClient.Incr(const key: ansiString;
                                 const Value: int64): int64;
begin
  result := DoIncr(FSocketDescriptor, key, Value);
end;

{*****************************************************}
function TAlMemCachedClient.Decr(const key: ansiString;
                                 const Value: int64): int64;
begin
  result := DoDecr(FSocketDescriptor, key, Value);
end;

{*******************************************************}
procedure TAlMemCachedClient.Touch(const key: ansiString;
                                   const exptime:integer);
begin
  DoTouch(FSocketDescriptor,
          key,
          exptime);
end;

{********************************************************************}
function TAlMemCachedClient.Stats(const args: AnsiString): AnsiString;
begin
  result := DoStats(FSocketDescriptor, args);
end;

{*****************************************************}
procedure TAlMemCachedClient.Flush_all(delay: integer);
begin
  DoFlush_all(FSocketDescriptor,delay);
end;

{**********************************************}
function TAlMemCachedClient.Version: AnsiString;
begin
  result := DoVersion(FSocketDescriptor);
end;

{*****************************************************}
procedure TAlMemCachedClient.Verbosity(level: integer);
begin
  DoVerbosity(FSocketDescriptor, level);
end;

{***************************************}
// "quit" is a command with no arguments:
//
// quit\r\n
//
// Upon receiving this command, the server closes the
// connection. However, the client may also simply close the connection
// when it no longer needs it, without issuing this command.
Procedure TAlMemCachedClient.Quit;
begin
  SendCmd(fSocketDescriptor, 'quit' + #13#10, rpNone);
  Disconnect;
end;

{******************************************************************************}
procedure TAlMemCachedConnectionPoolClient.SetSendTimeout(const Value: integer);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetSendTimeout(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{*********************************************************************************}
procedure TAlMemCachedConnectionPoolClient.SetReceiveTimeout(const Value: integer);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetReceiveTimeout(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{****************************************************************************}
procedure TAlMemCachedConnectionPoolClient.SetKeepAlive(const Value: boolean);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetKeepAlive(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{*****************************************************************************}
procedure TAlMemCachedConnectionPoolClient.SetTCPNoDelay(const Value: boolean);
begin
  FConnectionPoolCS.Acquire;
  Try
    inherited SetTCPNoDelay(Value);
  finally
    FConnectionPoolCS.Release;
  end;
  ReleaseAllConnections;
end;

{*******************************************************************}
function TAlMemCachedConnectionPoolClient.AcquireConnection: TSocket;
Var LTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    LTickCount := GetTickCount64;
    if LTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
      while FConnectionPoolCount > 0 do begin
        if LTickCount - FConnectionPool[0].Lastaccessdate > FConnectionMaxIdleTime then begin

          Try
            DoDisconnect(FConnectionPool[0].SocketDescriptor);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;

          Dec(FConnectionPoolCount);
          if  FConnectionPoolCount > 0 then
          begin
            System.Move(FConnectionPool[1], FConnectionPool[0],
              (FConnectionPoolCount) * SizeOf(TAlMemCachedConnectionPoolContainer));
          end;

        end
        else break;
      end;
      FLastConnectionGarbage := LTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPoolCount > 0 then begin
      Result := FConnectionPool[FConnectionPoolCount - 1].SocketDescriptor;
      Dec(FConnectionPoolCount);
    end

    //ask to create a new connection
    else begin
      result := INVALID_SOCKET;
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

  //create a new connection if pool was empty
  if result = INVALID_SOCKET then begin
    try
      Doconnect(result,
                fHost,//aHost,
                fPort,//APort,
                fSendTimeout,
                fReceiveTimeout,
                fKeepAlive,
                fTCPNoDelay);
    except
      FConnectionPoolCS.Acquire;
      dec(FWorkingConnectionCount);
      FConnectionPoolCS.Release;
      raise;
    end;
  end;

End;

{*****************************************************************************************}
procedure TAlMemCachedConnectionPoolClient.ReleaseConnection(var SocketDescriptor: TSocket;
                                                             const CloseConnection: Boolean = False);
begin

  //security check
  if SocketDescriptor = INVALID_SOCKET then raise exception.Create('Connection handle can not be INVALID_SOCKET');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      if FConnectionPoolCount = FConnectionPoolCapacity then begin
        if FConnectionPoolCapacity > 64 then FConnectionPoolCapacity := FConnectionPoolCapacity + (FConnectionPoolCapacity div 4) else
          if FConnectionPoolCapacity > 8 then FConnectionPoolCapacity := FConnectionPoolCapacity + 16 else
            FConnectionPoolCapacity := FConnectionPoolCapacity + 4;
        SetLength(FConnectionPool, FConnectionPoolCapacity);
      end;
      FConnectionPool[FConnectionPoolCount].SocketDescriptor := SocketDescriptor;
      FConnectionPool[FConnectionPoolCount].LastAccessDate := GetTickCount64;
      Inc(FConnectionPoolCount);
    end

    //close the connection
    else begin
      try
        doDisconnect(SocketDescriptor);
      Except
        //Disconnect must be a "safe" procedure because it's mostly called in
        //finalization part of the code that it is not protected
      end;
    end;

    //set the connectionhandle to nil
    SocketDescriptor := INVALID_SOCKET;

    //dec the WorkingConnectionCount
    Dec(FWorkingConnectionCount);

  finally
    FConnectionPoolCS.Release;
  end;

end;

{*************************************************************************************************}
constructor TAlMemCachedConnectionPoolClient.Create(const aHost: AnsiString; const APort: integer);
begin
  inherited create;
  fHost:= aHost;
  fPort:= APort;
  setlength(FConnectionPool,0);
  FConnectionPoolCount := 0;
  FConnectionPoolCapacity := 0;
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := GettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
end;

{**************************************************}
destructor TAlMemCachedConnectionPoolClient.Destroy;
begin
  ReleaseAllConnections;
  FConnectionPoolCS.free;
  inherited;
end;

{*************************************************************************************************************}
procedure TAlMemCachedConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
begin

  {we do this to forbid any new thread to create a new transaction}
  FReleasingAllconnections := True;
  Try

    //wait that all transaction are finished
    if WaitWorkingConnections then
      while true do begin
        FConnectionPoolCS.Acquire;
        Try
          if FWorkingConnectionCount <= 0 then break;
        finally
          FConnectionPoolCS.Release;
        end;
        sleep(1);
      end;

    {free all database}
    FConnectionPoolCS.Acquire;
    Try
      while FConnectionPoolCount > 0 do begin
        Try
          DoDisconnect(FConnectionPool[FConnectionPoolcount - 1].SocketDescriptor);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        Dec(FConnectionPoolCount);
      end;
      FLastConnectionGarbage := GetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

{******************************************************************}
function TAlMemCachedConnectionPoolClient.Get(const key: ansiString;
                                              var flags: integer;
                                              var data: ansiString): boolean;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoGet(LSocketDescriptor,
                    key,
                    flags,
                    data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************************}
function TAlMemCachedConnectionPoolClient.Get(const key: ansiString): AnsiString;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoGet(LSocketDescriptor, key);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*************************************************************************************************************}
function TAlMemCachedConnectionPoolClient.Get(const keys: array of ansiString): TAlMemCachedClient_StoredItems;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoGet(LSocketDescriptor, keys);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
function TAlMemCachedConnectionPoolClient.Gets(const key: ansiString;
                                               var flags: integer;
                                               var cas_id: int64;
                                               var data: ansiString): boolean;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoGets(LSocketDescriptor,
                     key,
                     flags,
                     cas_id,
                     data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{**************************************************************************************************************}
function TAlMemCachedConnectionPoolClient.Gets(const keys: array of ansiString): TAlMemCachedClient_StoredItems;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoGets(LSocketDescriptor, keys);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{********************************************************************}
Procedure TAlMemCachedConnectionPoolClient._Set(const key: ansiString;
                                                const flags: integer;
                                                const exptime:integer;
                                                const data: ansiString);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoSet(LSocketDescriptor,
          key,
          flags,
          exptime,
          data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
procedure TAlMemCachedConnectionPoolClient.Add(const key: ansiString;
                                               const flags: integer;
                                               const exptime:integer;
                                               const data: ansiString);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoAdd(LSocketDescriptor,
          key,
          flags,
          exptime,
          data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{***********************************************************************}
procedure TAlMemCachedConnectionPoolClient.Replace(const key: ansiString;
                                                   const flags: integer;
                                                   const exptime:integer;
                                                   const data: ansiString);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoReplace(LSocketDescriptor,
              key,
              flags,
              exptime,
              data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{**********************************************************************}
procedure TAlMemCachedConnectionPoolClient.Append(const key: ansiString;
                                                  const data: ansiString);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoAppend(LSocketDescriptor,
             key,
             data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{***********************************************************************}
procedure TAlMemCachedConnectionPoolClient.Prepend(const key: ansiString;
                                                   const data: ansiString);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoPrepend(LSocketDescriptor,
              key,
              data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{******************************************************************}
function TAlMemCachedConnectionPoolClient.Cas(const key: ansiString;
                                              const flags: integer;
                                              const exptime:integer;
                                              const cas_id: int64;
                                              const data: ansiString): boolean;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoCas(LSocketDescriptor,
                    key,
                    flags,
                    exptime,
                    Cas_ID,
                    data);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************************}
function TAlMemCachedConnectionPoolClient.Delete(const key: ansiString): boolean;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoDelete(LSocketDescriptor, key);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
function TAlMemCachedConnectionPoolClient.Incr(const key: ansiString;
                                               const Value: int64): int64;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoIncr(LSocketDescriptor, key, Value);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
function TAlMemCachedConnectionPoolClient.Decr(const key: ansiString;
                                               const Value: int64): int64;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoDecr(LSocketDescriptor, key, Value);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*********************************************************************}
procedure TAlMemCachedConnectionPoolClient.Touch(const key: ansiString;
                                                 const exptime:integer);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoTouch(LSocketDescriptor,
            key,
            exptime);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{**********************************************************************************}
function TAlMemCachedConnectionPoolClient.Stats(const args: AnsiString): AnsiString;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoStats(LSocketDescriptor, args);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
procedure TAlMemCachedConnectionPoolClient.Flush_all(delay: integer);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoFlush_all(LSocketDescriptor,delay);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{************************************************************}
function TAlMemCachedConnectionPoolClient.Version: AnsiString;
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    result := DoVersion(LSocketDescriptor);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

{*******************************************************************}
procedure TAlMemCachedConnectionPoolClient.Verbosity(level: integer);
var LSocketDescriptor: TSocket;
begin
  LSocketDescriptor := AcquireConnection;
  try
    DoVerbosity(LSocketDescriptor, level);
    ReleaseConnection(LSocketDescriptor);
  except
    On E: Exception do begin
      ReleaseConnection(LSocketDescriptor,
                        (not (E Is EAlMemCachedClientException)) or
                        (E as EAlMemCachedClientException).CloseConnection);
      raise;
    end;
  end;
end;

end.
