unit ALMongoDBClient;

interface

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     WinSock2,
     {$ELSE}
     WinSock,
     {$IFEND}
     Contnrs,
     SyncObjs,
     ALString;

const MONGO_OP_REPLY = 1; //Reply to a client request. responseTo is set
      MONGO_OP_MSG = 1000; //generic msg command followed by a string
      MONGO_OP_UPDATE = 2001; //update document
      MONGO_OP_INSERT = 2002; //insert new document
      MONGO_RESERVED = 2003; //formerly used for OP_GET_BY_OID
      MONGO_OP_QUERY = 2004; //query a collection
      MONGO_OP_GET_MORE = 2005; //Get more data from a query. See Cursors
      MONGO_OP_DELETE = 2006; //Delete documents
      MONGO_OP_KILL_CURSORS = 2007; //Tell database client is done with a cursor

type

    {---------------------------------------------}
    EAlMongoDBClientException = class(EALException)
    private
      FCloseConnection: Boolean;
    public
      constructor Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
      property CloseConnection: Boolean read FCloseConnection write FCloseConnection;
    end;

    {-----------------------------------}
    TAlBaseMongoDBClient = class(TObject)
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
      procedure DoConnect(var aSocketDescriptor: TSocket;
                          const aHost: AnsiString;
                          const APort: integer;
                          const aSendTimeout: Integer;
                          const aReceiveTimeout: Integer;
                          const aKeepAlive: Boolean;
                          const aTCPNoDelay: Boolean);
      Procedure DoDisconnect(var aSocketDescriptor: TSocket); virtual;
      Function SocketWrite(aSocketDescriptor: TSocket; {$IF CompilerVersion >= 23}const{$ELSE}var{$IFEND} Buf; len: Integer): Integer; Virtual;
      Function SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer; Virtual;
      Function SendCmd(aSocketDescriptor: TSocket; const aCmd: AnsiString): AnsiString; virtual;
      Function GetResponse(aSocketDescriptor: TSocket): AnsiString;
      Function BuildOPQUERYRequestMessages(const requestID: integer;                           // identifier for this message
                                           const responseTo: integer;                          // requestID from the original request (used in reponses from db)
                                           const opCode: integer;                              // request type - see table below
                                           const flags: integer;                               // bit vector of query options.  See below for details.
                                           const fullCollectionName: ansiString;               // "dbname.collectionname"
                                           const numberToSkip: integer;                        // number of documents to skip
                                           const numberToReturn: integer;                      // number of documents to return in the first OP_REPLY batch
                                           const query: Ansistring;                            // query object.  See below for details.
                                           const returnFieldSelector: ansiString): AnsiString; // Optional. Selector indicating the fields to return.  See below for details
    public
      function DoRunDBCommand(aSocketDescriptor: TSocket; const aDB: AnsiString; Const aDBCmd: AnsiString): AnsiString;



      constructor Create; virtual;
      destructor Destroy; override;
      property SendTimeout: Integer read FSendTimeout write SetSendTimeout;
      property ReceiveTimeout: Integer read FReceiveTimeout write SetReceiveTimeout;
      property KeepAlive: Boolean read fKeepAlive write SetKeepAlive;
      property TcpNoDelay: Boolean read fTCPNoDelay write fTCPNoDelay;
    end;

    {--------------------------------------------}
    TAlMongoDBClient = class(TAlBaseMongoDBClient)
    private
      Fconnected: Boolean;
    protected
      procedure SetSendTimeout(const Value: integer); override;
      procedure SetReceiveTimeout(const Value: integer); override;
      procedure SetKeepAlive(const Value: boolean); override;
      procedure SetTCPNoDelay(const Value: boolean); override;
    public
      FSocketDescriptor: TSocket;


      constructor Create; override;
      destructor Destroy; override;
      Procedure Connect(const aHost: AnsiString; const APort: integer); virtual;
      Procedure Disconnect; virtual;
      property Connected: Boolean read FConnected;
    end;

    {------------------------------------------------}
    TAlMongoDBConnectionPoolContainer = Class(TObject)
      SocketDescriptor: TSocket;
      LastAccessDate: int64;
    End;

    {----------------------------------------------------------}
    TAlMongoDBConnectionPoolClient = class(TAlBaseMongoDBClient)
    private
      FConnectionPool: TObjectList;
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
      property Host: ansiString read fHost;
      property Port: integer read fPort;
    end;

implementation

Uses Windows,
     SysUtils,
     ALStringList,
     AlWinsock,
     ALWindows;

{**************************************************************************************************************}
constructor EAlMongoDBClientException.Create(const aMsg: AnsiString; const aCloseConnection: Boolean = False);
begin
  fCloseConnection := aCloseConnection;
  inherited create(aMsg);
end;

{****************************************}
constructor TAlBaseMongoDBClient.Create;
var aWSAData: TWSAData;
begin
  CheckError(WSAStartup(MAKEWORD(2,2), aWSAData) <> 0);
  FSendTimeout := 10000; // 10 seconds
  FReceiveTimeout := 10000; // 10 seconds
  FKeepAlive := True;
  fTCPNoDelay := True;
end;

{****************************************}
destructor TAlBaseMongoDBClient.Destroy;
begin
  WSACleanup;
  inherited;
end;

{**********************************************************}
procedure TAlBaseMongoDBClient.CheckError(Error: Boolean);
begin
  if Error then RaiseLastOSError;
end;

{************************************************************************}
procedure TAlBaseMongoDBClient.DoConnect(var aSocketDescriptor: TSocket;
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
    {$IF CompilerVersion >= 23} {Delphi XE2}
    If SockAddr.sin_addr.S_addr = INADDR_NONE then begin
    {$ELSE}
    If SockAddr.sin_addr.S_addr = integer(INADDR_NONE) then begin
    {$IFEND}
      checkError(not ALHostToIP(Server, IP));
      SockAddr.sin_addr.S_addr:=inet_addr(PAnsiChar(IP));
    end;
    {$IF CompilerVersion >= 23} {Delphi XE2}
    CheckError(WinSock2.Connect(aSocketDescriptor,TSockAddr(SockAddr),SizeOf(SockAddr))=SOCKET_ERROR);
    {$ELSE}
    CheckError(WinSock.Connect(aSocketDescriptor,SockAddr,SizeOf(SockAddr))=SOCKET_ERROR);
    {$IFEND}

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
procedure TAlBaseMongoDBClient.DoDisconnect(var aSocketDescriptor: TSocket);
begin
  if aSocketDescriptor <> INVALID_SOCKET then begin
    ShutDown(aSocketDescriptor,SD_BOTH);
    CloseSocket(aSocketDescriptor);
    aSocketDescriptor := INVALID_SOCKET;
  end;
end;

{****************************************************************************************************}
Function TAlBaseMongoDBClient.SendCmd(aSocketDescriptor: TSocket; const aCmd: AnsiString): AnsiString;
Var P: PAnsiChar;
    L: Integer;
    ByteSent: integer;
begin
  p:=@aCmd[1]; // pchar
  l:=length(aCmd);
  while l>0 do begin
    ByteSent:=SocketWrite(aSocketDescriptor, p^,l);
    if ByteSent<=0 then raise EALException.Create('Connection close gracefully!');
    inc(p,ByteSent);
    dec(l,ByteSent);
  end;
  Result := GetResponse(aSocketDescriptor);
end;

{********************************************************************************}
function TAlBaseMongoDBClient.GetResponse(aSocketDescriptor: TSocket): AnsiString;
Var aBytesReceived: Integer;
    aResultPos: Integer;
const aBuffSize: integer = 4096;
begin

  //init local var
  Setlength(Result,aBuffSize);
  aResultPos := 0;

  //loop still we receive the full answer
  While True do begin

    //expnd the buffer
    if aResultPos = length(Result) then
      setlength(Result, length(Result) + aBuffSize);

    //read string from socket
    aBytesReceived := SocketRead(aSocketDescriptor, Result[aResultPos+1], length(Result) - aResultPos);
    If aBytesReceived <= 0 then raise EALException.Create('Connection close gracefully!');
    aResultPos := aResultPos + aBytesReceived;

  end;
end;

{*********************************************************************************}
Function TAlBaseMongoDBClient.BuildOPQUERYRequestMessages(const requestID: integer;                           // identifier for this message
                                                          const responseTo: integer;                          // requestID from the original request (used in reponses from db)
                                                          const opCode: integer;                              // request type - see table below
                                                          const flags: integer;                               // bit vector of query options.  See below for details.
                                                          const fullCollectionName: ansiString;               // "dbname.collectionname"
                                                          const numberToSkip: integer;                        // number of documents to skip
                                                          const numberToReturn: integer;                      // number of documents to return in the first OP_REPLY batch
                                                          const query: Ansistring;                            // query object.  See below for details.
                                                          const returnFieldSelector: ansiString): AnsiString; // Optional. Selector indicating the fields to return.  See below for details
var aCurrPos: integer;
    amessageLength: Integer;
begin
  amessageLength := sizeof(aMessageLength) +
                    sizeof(requestID) +
                    sizeof(responseTo) +
                    sizeof(opCode) +
                    sizeof(flags) +
                    length(fullCollectionName) + 1 +
                    sizeof(numberToSkip) +
                    sizeof(numberToReturn) +
                    //length(query) + 1 +
                    5 +
                    //length(returnFieldSelector) + 1;
                    0;
  setlength(result, amessageLength);


  //
  //  messageLength     requestID       responseTo        opCode                 flags
  //  [0][1][2][3]     [4][5][6][7]    [8][9][10][11]   [12][13][14][15]    [16][17][18][19]
  //
  //  fullCollectionName       numberToSkip       numberToReturn          query     returnFieldSelector
  //  [20]...[23]            [24][25][26][27]     [28][29][30][31]    [32]...[50]    [51]...[70]
  //

  //messageLength
  aCurrPos := 1;
  System.Move(amessageLength, result[aCurrPos], sizeof(amessageLength));
  inc(aCurrPos,sizeof(amessageLength));

  //requestID
  System.Move(requestID, result[aCurrPos], sizeof(requestID));
  inc(aCurrPos,sizeof(requestID));

  //responseTo
  System.Move(responseTo, result[aCurrPos], sizeof(responseTo));
  inc(aCurrPos,sizeof(responseTo));

  //opCode
  System.Move(opCode, result[aCurrPos], sizeof(opCode));
  inc(aCurrPos,sizeof(opCode));

  //flags
  System.Move(flags, result[aCurrPos], sizeof(flags));
  inc(aCurrPos,sizeof(flags));

  //fullCollectionName
  if length(fullCollectionName) > 0 then begin
    System.Move(fullCollectionName[1], result[aCurrPos], length(fullCollectionName));
    inc(aCurrPos,length(fullCollectionName));
  end;
  result[aCurrPos] := #0;
  inc(aCurrPos);

  //numberToSkip
  System.Move(numberToSkip, result[aCurrPos], sizeof(numberToSkip));
  inc(aCurrPos,sizeof(numberToSkip));

  //numberToReturn
  System.Move(numberToReturn, result[aCurrPos], sizeof(numberToReturn));
  inc(aCurrPos,sizeof(numberToReturn));


  //query
{  if length(query) > 0 then begin
    System.Move(query[1], result[aCurrPos], length(query));
    inc(aCurrPos,length(query));
  end;
  result[aCurrPos] := #0;
  inc(aCurrPos);}
  amessageLength := 5;
  System.Move(amessageLength, result[aCurrPos], sizeof(amessageLength));
  inc(aCurrPos,sizeof(amessageLength));
  result[aCurrPos] := #0;
  inc(aCurrPos);



  //returnFieldSelector
  if length(returnFieldSelector) > 0 then begin
    System.Move(returnFieldSelector[1], result[aCurrPos], length(returnFieldSelector));
    inc(aCurrPos,length(returnFieldSelector));
    result[aCurrPos] := #0;
  end;

  alSaveStringToFile(result,'c:\_______xxx.txt');
end;

{*****************************************************************************************************************************************************}
Function TAlBaseMongoDBClient.SocketWrite(aSocketDescriptor: TSocket; {$IF CompilerVersion >= 23}const{$ELSE}var{$IFEND} Buf; len: Integer): Integer;
begin
  Result := Send(aSocketDescriptor,Buf,len,0);
  CheckError(Result =  SOCKET_ERROR);
end;

{*****************************************************************************************************}
function TAlBaseMongoDBClient.SocketRead(aSocketDescriptor: TSocket; var buf; len: Integer): Integer;
begin
  Result := Recv(aSocketDescriptor,buf,len,0);
  CheckError(Result = SOCKET_ERROR);
end;

{**************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetSendTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_SNDTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{*****************************************************************************************************}
procedure TAlBaseMongoDBClient.DoSetReceiveTimeout(aSocketDescriptor: TSocket; const Value: integer);
begin
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_RCVTIMEO,PAnsiChar(@Value),SizeOf(Value))=SOCKET_ERROR);
end;

{***********************************************************************************************************************}
// // http://blogs.technet.com/b/nettracer/archive/2010/06/03/things-that-you-may-want-to-know-about-tcp-keepalives.aspx
procedure TAlBaseMongoDBClient.DoSetKeepAlive(aSocketDescriptor: TSocket; const Value: boolean);
var aIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then aIntBool := 1
  else aIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,SO_KEEPALIVE,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
end;

{***************************************************************************************************************************************************************************************************************}
// https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_MRG/1.1/html/Realtime_Tuning_Guide/sect-Realtime_Tuning_Guide-Application_Tuning_and_Deployment-TCP_NODELAY_and_Small_Buffer_Writes.html
procedure TAlBaseMongoDBClient.DoSetTCPNoDelay(aSocketDescriptor: TSocket; const Value: boolean);
var aIntBool: integer;
begin
  // warning the winsock seam buggy because the getSockOpt return optlen = 1 (byte) intead of 4 (dword)
  // so the getSockOpt work only if aIntBool = byte ! (i see this on windows vista)
  // but this is only for getSockOpt, for setsockopt it's seam to work OK so i leave it like this
  if Value then aIntBool := 1
  else aIntBool := 0;
  CheckError(setsockopt(aSocketDescriptor,SOL_SOCKET,TCP_NODELAY,PAnsiChar(@aIntBool),SizeOf(aIntBool))=SOCKET_ERROR);
end;

{********************************************************************}
procedure TAlBaseMongoDBClient.SetSendTimeout(const Value: integer);
begin
  FSendTimeout := Value;
end;

{***********************************************************************}
procedure TAlBaseMongoDBClient.SetReceiveTimeout(const Value: integer);
begin
  FReceiveTimeout := Value;
end;

{******************************************************************}
procedure TAlBaseMongoDBClient.SetKeepAlive(const Value: boolean);
begin
  FKeepAlive := Value;
end;

{*******************************************************************}
procedure TAlBaseMongoDBClient.SetTCPNoDelay(const Value: boolean);
begin
  fTCPNoDelay := Value;
end;

{************************************************************************************************************************************}
function TAlBaseMongoDBClient.DoRunDBCommand(aSocketDescriptor: TSocket; const aDB: AnsiString; Const aDBCmd: AnsiString): AnsiString;
var aMessage: AnsiString;
begin
  aMessage := BuildOPQUERYRequestMessages(1, //const requestID: integer;
                                          0, //const responseTo: integer;
                                          MONGO_OP_QUERY, //const opCode: integer;
                                          0, //const flags: integer;
                                          aDB, //const fullCollectionName: ansiString;
                                          0, //const numberToSkip: integer;
                                          0, //const numberToReturn: integer;
                                          '', //const query: Ansistring;
                                          ''); //const returnFieldSelector: ansiString);
  result := SendCmd(aSocketDescriptor,  aMessage);
end;

{****************************************************************}
procedure TAlMongoDBClient.SetSendTimeout(const Value: integer);
begin
  inherited SetSendTimeout(Value);
  if FConnected then DoSetSendTimeout(fSocketDescriptor, Value);
end;

{*******************************************************************}
procedure TAlMongoDBClient.SetReceiveTimeout(const Value: integer);
begin
  inherited SetReceiveTimeout(Value);
  if FConnected then DoSetReceiveTimeout(fSocketDescriptor, Value);
end;

{**************************************************************}
procedure TAlMongoDBClient.SetKeepAlive(const Value: boolean);
begin
  inherited SetKeepAlive(Value);
  if FConnected then DoSetKeepAlive(fSocketDescriptor, Value);
end;

{***************************************************************}
procedure TAlMongoDBClient.SetTCPNoDelay(const Value: boolean);
begin
  inherited SetTCPNoDelay(Value);
  if FConnected then DoSetTCPNoDelay(fSocketDescriptor, Value);
end;

{************************************}
constructor TAlMongoDBClient.Create;
begin
  inherited;
  Fconnected:= False;
  FSocketDescriptor:= INVALID_SOCKET;
end;

{************************************}
destructor TAlMongoDBClient.Destroy;
begin
  If Fconnected then Disconnect;
  inherited;
end;

{**********************************************************************************}
procedure TAlMongoDBClient.Connect(const aHost: AnsiString; const APort: integer);
begin

  if FConnected then raise EALException.Create('MongoDB component already connected');
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
procedure TAlMongoDBClient.Disconnect;
begin
  If Fconnected then begin
    doDisconnect(FSocketDescriptor);
    Fconnected := False;
  end;
end;

{******************************************************************************}
procedure TAlMongoDBConnectionPoolClient.SetSendTimeout(const Value: integer);
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
procedure TAlMongoDBConnectionPoolClient.SetReceiveTimeout(const Value: integer);
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
procedure TAlMongoDBConnectionPoolClient.SetKeepAlive(const Value: boolean);
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
procedure TAlMongoDBConnectionPoolClient.SetTCPNoDelay(const Value: boolean);
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
function TAlMongoDBConnectionPoolClient.AcquireConnection: TSocket;
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
    aTickCount: int64;
Begin

  //synchronize the code
  FConnectionPoolCS.Acquire;
  Try

    //raise an exception if currently realeasing all connection
    if FReleasingAllconnections then raise exception.Create('Can not acquire connection: currently releasing all connections');

    //delete the old unused connection
    aTickCount := ALGetTickCount64;
    if aTickCount - fLastConnectionGarbage > (60000 {every minutes})  then begin
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[0]);
        if aTickCount - aConnectionPoolContainer.Lastaccessdate > FConnectionMaxIdleTime then begin
          Try
            DoDisconnect(aConnectionPoolContainer.SocketDescriptor);
          Except
            //Disconnect must be a "safe" procedure because it's mostly called in
            //finalization part of the code that it is not protected
          End;
          FConnectionPool.Delete(0); // must be delete here because FConnectionPool free the object also
        end
        else break;
      end;
      FLastConnectionGarbage := aTickCount;
    end;

    //acquire the new connection from the pool
    If FConnectionPool.Count > 0 then begin
      aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
      Result := aConnectionPoolContainer.SocketDescriptor;
      FConnectionPool.Delete(FConnectionPool.count - 1);
    end

    //create a new connection
    else begin
      Doconnect(result,
                fHost,//aHost,
                fPort,//APort,
                fSendTimeout,
                fReceiveTimeout,
                fKeepAlive,
                fTCPNoDelay);
    end;

    //increase the connection count
    inc(FWorkingConnectionCount);

  //get out of the synchronization
  finally
    FConnectionPoolCS.Release;
  end;

End;

{*****************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseConnection(var SocketDescriptor: TSocket;
                                                             const CloseConnection: Boolean = False);
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
begin

  //security check
  if SocketDescriptor = INVALID_SOCKET then raise exception.Create('Connection handle can not be INVALID_SOCKET');

  //release the connection
  FConnectionPoolCS.Acquire;
  Try

    //add the connection to the pool
    If (not CloseConnection) and (not FReleasingAllconnections) then begin
      aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer.Create;
      aConnectionPoolContainer.SocketDescriptor := SocketDescriptor;
      aConnectionPoolContainer.LastAccessDate := ALGetTickCount64;
      FConnectionPool.add(aConnectionPoolContainer);
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
constructor TAlMongoDBConnectionPoolClient.Create(const aHost: AnsiString; const APort: integer);
begin
  inherited create;
  fHost:= aHost;
  fPort:= APort;
  FConnectionPool:= TObjectList.Create(True);
  FConnectionPoolCS:= TCriticalSection.create;
  FWorkingConnectionCount:= 0;
  FReleasingAllconnections := False;
  FLastConnectionGarbage := ALGettickCount64;
  FConnectionMaxIdleTime := 1200000; // 1000 * 60 * 20 = 20 min
end;

{**************************************************}
destructor TAlMongoDBConnectionPoolClient.Destroy;
begin
  ReleaseAllConnections;
  FConnectionPool.free;
  FConnectionPoolCS.free;
  inherited;
end;

{*************************************************************************************************************}
procedure TAlMongoDBConnectionPoolClient.ReleaseAllConnections(Const WaitWorkingConnections: Boolean = True);
Var aConnectionPoolContainer: TAlMongoDBConnectionPoolContainer;
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
      while FConnectionPool.Count > 0 do begin
        aConnectionPoolContainer := TAlMongoDBConnectionPoolContainer(FConnectionPool[FConnectionPool.count - 1]);
        Try
          DoDisconnect(aConnectionPoolContainer.SocketDescriptor);
        Except
          //Disconnect must be a "safe" procedure because it's mostly called in
          //finalization part of the code that it is not protected
        End;
        FConnectionPool.Delete(FConnectionPool.count - 1); // must be delete here because FConnectionPool free the object also
      end;
      FLastConnectionGarbage := ALGetTickCount64;
    finally
      FConnectionPoolCS.Release;
    end;

  finally
    //Do not forbid anymore new thread to create a new transaction
    FReleasingAllconnections := False;
  End;

end;

end.
