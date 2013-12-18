(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@progdigy.com>.
*)

unit PDGSocketStub;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}
{$I PDGAppServer.inc}

interface
uses
  {$IFNDEF FPC}Windows,{$ENDIF}
  {$IFDEF FPC}sockets,{$ELSE}Winsock,{$ENDIF}
  {$IFDEF UNIX}baseunix,{$ENDIF}
  PDGUtils;

{$IFDEF FPC}
const
  SOL_SOCKET    = $ffff;
  SO_REUSEADDR  = $0004;
  SO_RCVTIMEO   = $1006; 
  IPPROTO_TCP   = 6;
  TCP_NODELAY   = $0001;
{$ENDIF}
  
type
  // forward declarations
  TPDGThread = class;
  TSocketStub = class;
  TSocketServer = class;

  TSocketStubClass = class of TSocketStub;
  TSocketServerClass = class of TSocketServer;
  TPDGThreadClass = class of TPDGThread;

  PThreadList = ^TThreadList;
  TThreadList = array[0..(Maxint div 16) - 1] of TPDGThread;

  TPDGThread = class(TInterfacedObject)
  private
    FThreadId: TThreadID;
    FThreadHandle: TThreadId;
    FPaused: boolean;
    FCriticalSection: TRtlCriticalSection;
    FOwner: TPDGThread;
    FChildList: PThreadList;
    FChildCount: Integer;
    FChildCapacity: Integer;
    FThreadRefCount: Integer;
    FStopped: Longint;
    function ChildGet(Index: Integer): TPDGThread;
    procedure ChildSetCapacity(NewCapacity: Integer);
    function ChildAdd(Item: TPDGThread): Integer;
    procedure ChildDelete(Index: Integer);
    function ChildIndexOf(Item: TPDGThread): Integer;
    function ChildRemove(Item: TPDGThread): Integer;
    function GetChildCount: Integer;
    function GetStopped: boolean;
  protected
    function Run: Cardinal; virtual;
    procedure Stop; virtual;
  public
    class function ThreadCount: integer;
    procedure ChildClear; virtual;
    procedure Pause;
    procedure Resume;
    procedure Start;
    procedure Lock;
    procedure UnLock;
    constructor Create(AOwner: TPDGThread); virtual;
    destructor Destroy; override;
    property ChildCount: Integer read GetChildCount;
    property ChildItems[Index: Integer]: TPDGThread read ChildGet; default;
    property Stopped: boolean read GetStopped;
  end;

  TSocketServer = class(TPDGThread)
  private
    FAddress: TSockAddr;
    FSocketHandle: longint;
    FPort: Word;
    FBind: Longint;
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
    function doOnCreateStub(Socket: longint; Address: TSockAddr): TSocketStub; virtual; abstract;
  public
    property Address: TSockAddr read FAddress;
    constructor CreateServer(AOwner: TPDGThread; Port: Word; const Bind: LongInt = INADDR_ANY); virtual;
  end;

  TSocketStub = class(TPDGThread)
  private
    FAddress: TSockAddr;
    FSocketHandle: longint;
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
  public
    property SocketHandle: longint read FSocketHandle;
    property Address: TSockAddr read FAddress;
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); virtual;
  end;

implementation
uses
  SysUtils;

var
  AThreadCount: Integer = 0;

{$IFDEF FPC}
const
  INVALID_HANDLE_VALUE = TThreadId(-1);
  INVALID_SOCKET = longint(-1);
{$ENDIF}

function ThreadRun(Thread: Pointer): PtrInt; {$IFNDEF FPC}stdcall;{$ENDIF}
var
  t: TPDGThread;
begin
  t := TPDGThread(Thread);
  InterlockedIncrement(t.FThreadRefCount);
  try
    result := t.Run;
  finally
    if InterlockedDecrement(t.FThreadRefCount) = 0 then
      t.Free else
      if t.FOwner <> nil then
        t.FOwner.ChildRemove(t);
  end;
end;

{ TPDGThread }

constructor TPDGThread.Create(AOwner: TPDGThread);
begin
  inherited Create;
  InterlockedIncrement(AThreadCount);
{$IFDEF FPC}
  InitCriticalSection(FCriticalSection);
{$ELSE}
  InitializeCriticalSection(FCriticalSection);
{$ENDIF}
  FPaused := False;
  InterlockedExchange(FStopped, 0);
  FOwner := AOwner;
  FThreadHandle := INVALID_HANDLE_VALUE;
  FThreadId := INVALID_HANDLE_VALUE;
  if (FOwner <> nil) then
    FOwner.ChildAdd(Self);
end;

destructor TPDGThread.Destroy;
begin
  InterlockedDecrement(AThreadCount);
  Stop;
  ChildClear;
  //TerminateThread(FThreadHandle, 0);
{$IFDEF FPC}
  DoneCriticalSection(FCriticalSection);
{$ELSE}
  DeleteCriticalSection(FCriticalSection);
  if FThreadHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FThreadHandle);
{$ENDIF}
  inherited;
end;

procedure TPDGThread.Pause;
var i: integer;
begin
  if not FPaused then
  begin
    Lock;
    try
      if FThreadHandle <> INVALID_HANDLE_VALUE then
        SuspendThread(FThreadHandle);
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Pause;
      FPaused := True;
    finally
      UnLock;
    end;
  end;
end;

procedure TPDGThread.Resume;
var i: integer;
begin
  if FPaused then
  begin
    lock;
    try
      if FThreadHandle <> INVALID_HANDLE_VALUE then
        ResumeThread(FThreadHandle);
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Resume;
      FPaused := False;
    finally
      UnLock;
    end;
  end;
end;

function TPDGThread.Run: Cardinal;
begin
  Result := 0;
//  raise Exception.Create('not implemented');
end;

// Childs ...

function TPDGThread.ChildAdd(Item: TPDGThread): Integer;
var
  Delta: Integer;
begin
  Lock;
  try
    Result := FChildCount;
    if Result = FChildCapacity then
    begin
      if FChildCapacity > 64 then
        Delta := FChildCapacity div 4
      else
        if FChildCapacity > 8 then
          Delta := 16
        else
          Delta := 4;
      ChildSetCapacity(FChildCapacity + Delta);
    end;
    FChildList^[Result] := Item;
    InterlockedIncrement(TPDGThread(Item).FThreadRefCount);
    Inc(FChildCount);
  finally
    UnLock;
  end;
end;

procedure TPDGThread.ChildClear;
begin
  Lock;
  try
    while FChildCount > 0 do
      ChildRemove(ChildGet(0));
    ChildSetCapacity(0);
  finally
    UnLock;
  end;
end;

procedure TPDGThread.ChildDelete(Index: Integer);
begin
  if (Index < 0) or (Index >= FChildCount) then exit;

  with ChildGet(Index) do
    if InterlockedDecrement(FThreadRefCount) = 0 then
      Destroy else
      Stop;

  Dec(FChildCount);
  if Index < FChildCount then
    System.Move(FChildList^[Index + 1], FChildList^[Index],
      (FChildCount - Index) * SizeOf(Pointer));
end;

function TPDGThread.ChildGet(Index: Integer): TPDGThread;
begin
  Result := nil;
  Lock;
  try
    if (Index < 0) or (Index >= FChildCount) then
      raise Exception.CreateFmt('List index out of bounds (%d)', [Index]);
    Result := FChildList^[Index];
  finally
    UnLock;
  end;
end;

function TPDGThread.ChildIndexOf(Item: TPDGThread): Integer;
begin
  Result := 0;
  while (Result < FChildCount) and (FChildList^[Result] <> Item) do
    Inc(Result);
  if Result = FChildCount then
    Result := -1;
end;

function TPDGThread.ChildRemove(Item: TPDGThread): Integer;
begin
  Lock;
  try
    Result := ChildIndexOf(Item);
    if Result >= 0 then
      ChildDelete(Result);
  finally
    UnLock;
  end;
end;

procedure TPDGThread.ChildSetCapacity(NewCapacity: Integer);
begin
  Lock;
  try
    if (NewCapacity < FChildCount) or (NewCapacity > (Maxint div 16)) then
      raise Exception.CreateFmt('List capacity out of bounds (%d)', [NewCapacity]);
    if NewCapacity <> FChildCapacity then
    begin
      ReallocMem(FChildList, NewCapacity * SizeOf(Pointer));
      FChildCapacity := NewCapacity;
    end;
  finally
    UnLock;
  end;
end;

procedure TPDGThread.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TPDGThread.UnLock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TPDGThread.GetChildCount: Integer;
begin
  Lock;
  try
    Result := FChildCount;
  finally
    UnLock;
  end;
end;

procedure TPDGThread.Start;
var
  i: Integer;
begin
  Lock;
  try
    if ClassType <> TPDGThread then
    {$IFDEF FPC}
      FThreadHandle := BeginThread(@ThreadRun, Self, FThreadId);
    {$ELSE}
      FThreadHandle := CreateThread(nil, 0, @ThreadRun, Self, 0, FThreadId);
    {$ENDIF}
    for i := 0 to ChildCount - 1 do
      ChildItems[i].Start;
  finally
    UnLock;
  end;
end;

procedure TPDGThread.Stop;
begin
  InterlockedExchange(FStopped, 1);
end;

function TPDGThread.GetStopped: boolean;
begin
  Result := FStopped <> 0;
end;

class function TPDGThread.ThreadCount: integer;
begin
  Result := AThreadCount;
end;

{ TSocketServer }

function TSocketServer.Run: Cardinal;
var
  InputSocket: longint;
  InputAddress: TSockAddr;
  InputLen: Integer;
  Stub: TSocketStub;
  SO_True: Integer;
begin
  SO_True := -1;
  Result := 0;
{$IFDEF FPC}
  FSocketHandle := fpsocket(AF_INET, SOCK_STREAM, 0);
{$ELSE}
  FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
{$ENDIF}
  FAddress.sin_addr.s_addr := FBind;
  FAddress.sin_family := AF_INET;
  FAddress.sin_port := htons(FPort);

{$IFDEF FPC}
  fpsetsockopt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_True), SizeOf(SO_True));
  fpsetsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PChar(@SO_True), SizeOf(SO_True));
{$ELSE}
  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_True), SizeOf(SO_True));
{$ENDIF}

{$IFDEF FPC}
  if fpbind(FSocketHandle, @FAddress, SizeOf(FAddress)) <> 0 then
{$ELSE}
  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
{$ENDIF}
  begin
    Stop;
    raise Exception.Create('can''t bind.');
  end;
{$IFDEF FPC}
  if (fplisten(FSocketHandle, 15) <> 0) then
{$ELSE}
  if (listen(FSocketHandle, 15) <> 0) then
{$ENDIF}
  begin
    Stop;
    raise Exception.Create('can''t listen.');
  end;

  InputLen := SizeOf(InputAddress);
  while not Stopped do
  try
{$IFDEF FPC}
    InputSocket := fpaccept(FSocketHandle, @InputAddress, @InputLen);
{$ELSE}
    InputSocket := accept(FSocketHandle, @InputAddress, @InputLen);
{$ENDIF}
    if (InputSocket <> INVALID_SOCKET) then
    begin
      Stub := doOnCreateStub(InputSocket, InputAddress);
      if Stub <> nil then
        Stub.Start else
        closesocket(InputSocket);
    end;
  except
    // the server must continue to listen !
  end;
end;

constructor TSocketServer.CreateServer(AOwner: TPDGThread; Port: Word; const Bind: LongInt);
begin
  inherited Create(AOwner);
  FSocketHandle := INVALID_SOCKET;
  FPort := Port;
  FBind := Bind;
end;

procedure TSocketServer.Stop;
{$IFDEF UNIX}
var
  addr: TSockAddr;
  ASocket: longint;
{$ENDIF}
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    closesocket(FSocketHandle);
  {$IFDEF UNIX}
    // connect to itself to stop waiting
    ASocket := fpsocket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    addr.sin_family := AF_INET;
    addr.sin_port := htons(FPort);
    addr.sin_addr.S_addr := $0100007F; // 127.0.0.1
    fpconnect(ASocket, @addr, sizeof(addr));
    closesocket(ASocket);
  {$ENDIF}
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
  end;
end;

{ TSocketStub }

constructor TSocketStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited Create(AOwner);
  FSocketHandle := ASocket;
  FAddress := AAddress;
end;

function TSocketStub.Run: Cardinal;
begin
  // you must implement this method
  Result := 0;
end;

procedure TSocketStub.Stop;
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    CloseSocket(FSocketHandle);
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
  end;
end;

{$IFNDEF FPC}
initialization
  IsMultiThread := true;
{$ENDIF}

end.
