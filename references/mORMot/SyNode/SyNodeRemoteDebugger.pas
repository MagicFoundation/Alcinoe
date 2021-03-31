/// SyNodeRemoteDebugger - Remote debugger protocol for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeRemoteDebugger;
{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    SyNode for mORMot Copyright (C) 2021 Pavel Mashlyakovsky & Vadim Orel
      pavel.mash at gmail.com

    Some ideas taken from
       http://code.google.com/p/delphi-javascript
       http://delphi.mozdev.org/javascript_bridge/

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is
  Pavel Mashlyakovsky.
  Portions created by the Initial Developer are Copyright (C) 2014
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel
  - Pavel Mashlyakovsky
  - win2014
  - George

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


  Version 1.18
  - initial release. Use SpiderMonkey 45

}

interface
{$I Synopse.inc} // define BRANCH_WIN_WEB_SOCKET

uses
  Classes, SynCrtSock, SynTable {for TJSONWriter},
  SynCommons, SyNode, SpiderMonkey;

type
/// Thread for mozilla Remote Debugging Protocol
// https://wiki.mozilla.org/Remote_Debugging_Protocol
// https://wiki.mozilla.org/Remote_Debugging_Protocol_Stream_Transport
  TSMRemoteDebuggerThread = class(TThread)
  private
    fThreadInWork: Integer;
    fDebuggers: TSynObjectListLocked;
    fCommunicationThreads: TSynObjectListLocked;
    fCurThreadIndex: integer;
    fPort: SockString;
    fManager: TSMEngineManager;
    FNeedPauseOnFirstStep: boolean;
  protected
    procedure Execute; override;
  public
    procedure SetTerminated;
    /// Create thread and start listening on custom port
    // - expects the port to be specified as Ansi string, e.g. '1234'
    // - you can optionally specify a server address to bind to, e.g.
    // '1.2.3.4:1234'
    constructor Create(aManager: TSMEngineManager; const aPort: SockString = '6000');
    destructor Destroy; override;
    procedure startDebugCurrentThread(aEng: TSMEngine);
    procedure stopDebugCurrentThread(aEng: TSMEngine);
    /// Write log to current thread engine
    procedure doLog(const Text: RawUTF8);

    property NeedPauseOnFirstStep: boolean read FNeedPauseOnFirstStep write FNeedPauseOnFirstStep;
  end;

  function SyNodeBindingProc_debugger(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;

implementation
uses
  {$ifdef MSWINDOWS}
    Windows,
    SynWinSock,
  {$else}
    Types,
    BaseUnix,
    Sockets,
    SynFPCSock,
    SynFPCLinux,
  {$endif}
  SysUtils;

type
  TSMDebugger = class;

  TSMRemoteDebuggerCommunicationThread = class(TThread)
  private
    fParent: TSMRemoteDebuggerThread;
    fNeedClose: boolean;
    fDebugger: TSMDebugger;
    fCommunicationSock: TCrtSocket;
    // read a packages in format package-length:JSON
    function sockRead(out packet: RawUTF8): boolean;
    procedure sockWrite(const packet: RawUTF8);
    procedure HandleMessage(const request: Variant);
  protected
    procedure Execute; override;
    procedure SetTerminated;
  public
    constructor Create(aParent: TSMRemoteDebuggerThread);
    destructor Destroy; override;

    procedure Send(const packet: RawUTF8);
    procedure startListening(socket: TCrtSocket);
  end;

  TSMDebugger = class
  private
    fIndex: Integer;
    fIsPaused: boolean;
    fMessagesQueue: TRawUTF8ListLocked;
    fLogQueue: TRawUTF8ListLocked;
    {$IFNDEF SM52}
    fOldInterruptCallback: JSInterruptCallback;
    {$ENDIF}
    fSmThreadID: TThreadID;
    fNameForDebug: RawUTF8;
    fCommunicationThread: TSMRemoteDebuggerCommunicationThread;
    fIsJustInited: boolean;
    fDebuggerName: RawUTF8;
    fWebAppRootPath: RawUTF8;
    /// Debugger create his own compartmnet (his own global object & scripting context)
    // Here we initialize a new compartment
    procedure InitializeDebuggerCompartment(aEng: TSMEngine; aNeedPauseOnFirstStep: boolean);
  protected
    // writer for serialize outgiong JSON's
    fJsonWriter: TJSONWriter;
  public
    constructor Create(aParent: TSMRemoteDebuggerThread; aEng: TSMEngine);
    destructor Destroy; override;
    procedure Send(const packet: RawUTF8);

    procedure attach(aThread: TSMRemoteDebuggerCommunicationThread);
  end;

{ TSMRemoteDebuggerThread }

constructor TSMRemoteDebuggerThread.Create(aManager: TSMEngineManager; const aPort: SockString);
begin
  fDebuggers := TSynObjectListLocked.Create(true);
  fCommunicationThreads := TSynObjectListLocked.Create(false);
  FNeedPauseOnFirstStep := false;
  fCurThreadIndex := 0;
  fThreadInWork := 0;
  fPort := aPort;
  fManager := aManager;
  FreeOnTerminate := true;
  inherited Create(False);
end;

destructor TSMRemoteDebuggerThread.Destroy;
var
  i: Integer;
begin
  fCommunicationThreads.Safe.Lock;
  try
    i := fCommunicationThreads.Count;
    while i > 0 do begin
      Dec(i);
      TSMRemoteDebuggerCommunicationThread(fCommunicationThreads.List[i]).Terminate;
      fCommunicationThreads.Delete(i);
    end;
  finally
    fCommunicationThreads.Safe.UnLock;
  end;
  fCommunicationThreads.Free;
  fCommunicationThreads := nil;

  fDebuggers.Safe.Lock;
  try
    while fDebuggers.Count>0 do
      fDebuggers.Delete(fDebuggers.Count-1);
  finally
    fDebuggers.Safe.UnLock;
  end;
  fDebuggers.Free;
  fDebuggers := nil;
  inherited;
end;

procedure TSMRemoteDebuggerThread.doLog(const Text: RawUTF8);
var
  Debugger: TSMDebugger;
  eng: TSMEngine;
  curThreadID: TThreadID;
begin
  curThreadID := GetCurrentThreadId;
  eng := fManager.EngineForThread(curThreadID);
  if eng<>nil then begin
    Debugger := eng.PrivateDataForDebugger;
    Debugger.fLogQueue.SafePush(Text);

    if eng.cx.IsRunning then
{$IFDEF SM52}
      eng.cx.RequestInterruptCallback
{$ELSE}
      eng.rt.RequestInterruptCallback
{$ENDIF}
    else
{$IFDEF SM52}
    begin
      eng.cx.RequestInterruptCallback;
      eng.cx.CheckForInterrupt;
    end;
{$ELSE}
      eng.rt.InterruptCallback(eng.cx);
{$ENDIF}
  end;
end;

procedure TSMRemoteDebuggerThread.Execute;
var
  ServerSock: TCrtSocket;
  AcceptedSocket: TCrtSocket;
  thread: TSMRemoteDebuggerCommunicationThread;
  threadsCnt: integer;
begin
  AcceptedSocket := nil;
  ServerSock := TCrtSocket.Bind(fPort);
  try
    repeat
      AcceptedSocket := ServerSock.AcceptIncoming();
      if (AcceptedSocket <> nil) then begin
        if Terminated then begin
          fCommunicationThreads.Safe.Lock;
          try
            while fCommunicationThreads.count > 0 do begin
              threadsCnt := fCommunicationThreads.Count;
              thread := TSMRemoteDebuggerCommunicationThread(fCommunicationThreads.List[threadsCnt - 1]);
              fCommunicationThreads.Delete(threadsCnt - 1);
              thread.SetTerminated;
            end;
          finally
            fCommunicationThreads.Safe.UnLock;
          end;

          while fThreadInWork>0 do
            SleepHiRes(10);
          exit;
        end;

        fCommunicationThreads.Safe.Lock;
        try
          threadsCnt := fCommunicationThreads.Count;
          if threadsCnt = 0 then begin //no free threads;
            AcceptedSocket.Close;
          end else begin
            thread := TSMRemoteDebuggerCommunicationThread(fCommunicationThreads[threadsCnt - 1]);
            fCommunicationThreads.Delete(threadsCnt - 1);
            thread.startListening(AcceptedSocket);
          end;
        finally
          fCommunicationThreads.Safe.UnLock;
        end;
      end;
    until Terminated;
  finally
    AcceptedSocket.Free;
    ServerSock.Free;
  end;
end;

procedure TSMRemoteDebuggerThread.startDebugCurrentThread(aEng: TSMEngine);
var
  i: integer;
  curThreadID: TThreadID;
begin
  curThreadID := GetCurrentThreadId;
  if not Terminated and (fDebuggers <> nil) then begin
    fDebuggers.Safe.Lock;
    try
      if aEng <> nil then begin
        for I := 0 to fDebuggers.Count - 1 do
          if TSMDebugger(fDebuggers.List[i]).fNameForDebug = aEng.nameForDebug then begin
            // todo
            TSMDebugger(fDebuggers.List[i]).fSmThreadID := curThreadID;
            TSMDebugger(fDebuggers.List[i]).InitializeDebuggerCompartment(aEng, FNeedPauseOnFirstStep);
            exit;
          end;
        fDebuggers.Add(TSMDebugger.Create(self, aEng));
      end else
        raise ESMException.Create('Can''t start debugger for non-existed engine');
    finally
      fDebuggers.Safe.UnLock;
    end;
  end;
end;

procedure TSMRemoteDebuggerThread.stopDebugCurrentThread(aEng: TSMEngine);
var
  i: Integer;
  cx: PJSContext;
  cmpDbg: PJSCompartment;

  curThreadID: TThreadID;
  dbgObject: PJSRootedObject;
begin
  curThreadID := GetCurrentThreadId;
  if not Terminated and (fDebuggers <> nil) then begin
    fDebuggers.Safe.Lock;
    try
      for I := 0 to fDebuggers.Count - 1 do
        if TSMDebugger(fDebuggers.List[i]).fSmThreadID = curThreadID then begin
          if aEng<>nil then begin
            cx := aEng.cx;
            cmpDbg := cx.EnterCompartment(aEng.GlobalObjectDbg.ptr);
            try
              dbgObject := cx.NewRootedObject(aEng.GlobalObjectDbg.ptr.GetPropValue(cx, 'process').asObject.GetPropValue(cx, 'dbg').asObject);
              try
                if dbgObject.ptr.HasProperty(cx, 'uninit') then
                  aEng.CallObjectFunction(dbgObject, 'uninit', []);
              finally
                cx.FreeRootedObject(dbgObject);
              end;
            finally
              cx.LeaveCompartment(cmpDbg);
            end;
            aEng.CancelExecution;
          end else
            raise ESMException.Create('internal error: no engine');
          TSMDebugger(fDebuggers.List[i]).fSmThreadID := 0;
          exit;
        end;
    finally
      fDebuggers.Safe.UnLock;
    end;
  end;
end;

procedure TSMRemoteDebuggerThread.SetTerminated;
var
  socket: TCrtSocket;
begin
  if not Terminated then begin
    Terminate;
    socket := Open('127.0.0.1', fPort);
    if socket<>nil then
      socket.Free;
    while fThreadInWork>0 do
      SleepHiRes(10);
  end;
end;

{ TSMRemoteDebuggerCommunicationThread }

constructor TSMRemoteDebuggerCommunicationThread.Create(aParent: TSMRemoteDebuggerThread);
begin
  inherited Create(true);
  fParent := aParent;
  InterlockedIncrement(fParent.fThreadInWork);
  FreeOnTerminate := true;
end;

destructor TSMRemoteDebuggerCommunicationThread.Destroy;
begin
  InterlockedDecrement(fParent.fThreadInWork);
  inherited;
end;

procedure TSMRemoteDebuggerCommunicationThread.Execute;
const
  timeForSelectThreadInSeconds = 300;
var
  packet: RawUTF8;
  request: Variant;
  tickCountsForSelectEngine: Int64;
begin
  inherited;
  repeat
    Send('{"from":"root","applicationType":"synode","traits" : {"debuggerSourceActors":true, "conditionalBreakpoints": true}}');
    tickCountsForSelectEngine := GetTickCount64 + timeForSelectThreadInSeconds * 1000;
    fNeedClose := false;
    repeat
      if sockRead(packet) then
      begin
        request := _JsonFast(packet);
        SynSMLog.Add.Log(sllCustom4, packet);
        HandleMessage(request);
      end;
      if (fDebugger = nil) and (GetTickCount64 > tickCountsForSelectEngine) then begin
        fNeedClose := true
      end;
      if fParent.Terminated then
        SetTerminated;
    until fNeedClose or (fCommunicationSock.Sock = -1) or Terminated;

    fCommunicationSock.Free;
    fCommunicationSock := nil;

    if not Terminated then begin
      fParent.fCommunicationThreads.Safe.Lock;
      try
        if fDebugger <> nil then begin
          fDebugger.fCommunicationThread := nil;
          fDebugger := nil;
        end;
        fParent.fCommunicationThreads.Add(Self);
      finally
        fParent.fCommunicationThreads.Safe.UnLock;
      end;
      Suspended := true;
    end;
  until Terminated;
end;

procedure TSMRemoteDebuggerCommunicationThread.HandleMessage(const request: Variant);
var
  data: RawUTF8;
  i: integer;
  debuggerIndex: integer;
  debugger: TSMDebugger;
  Writer: TTextWriter;
  engine: TSMEngine;
begin
  if {$IFDEF FPC}request.&to{$ELSE}request.to{$ENDIF} = 'root' then begin
    Writer := TTextWriter.CreateOwnedStream;
    try
      if {$IFDEF FPC}request.&type{$ELSE}request.type{$ENDIF} = 'listAddons' then begin
        Writer.AddShort('{"from":"root","addons":[');
        fParent.fDebuggers.Safe.Lock;
        try
          for I := 0 to fParent.fDebuggers.Count - 1 do begin
            debugger := TSMDebugger(fParent.fDebuggers.List[i]);
            engine := fParent.fManager.EngineForThread(debugger.fSmThreadID);
            if engine <> nil then begin
              // Actor represent debug thread here, setting proper name with coxtext thread id
              // Writer.AddShort('{"actor":"server1.conn1.addon');
              // Writer.Add(TSMDebugger(fParent.fDebuggers[i]).fIndex);
              Writer.AddShort('{"actor":"');
              Writer.AddShort(debugger.fDebuggerName);
              Writer.AddShort('.conn1.thread_');
              { TODO : check that in multithread mode this field equal thread id with js context that we debug, otherwire replace with proper assigment }
              Writer.Add(debugger.fSmThreadID);
              // id should be addon id, value from DoOnGetEngineName event
              // Writer.AddShort('","id":"server1.conn1.addon');
              // Writer.Add(TSMDebugger(fParent.fDebuggers[i]).fIndex);
              Writer.AddShort('","id":"');
              Writer.AddString(debugger.fNameForDebug);
              Writer.AddShort('","name":"');
              Writer.AddString(debugger.fNameForDebug);
              // url most likly should be addon folder in format: file:///drive:/path/
              // Writer.AddShort('","url":"server1.conn1.addon');
              // Writer.Add(TSMDebugger(fParent.fDebuggers[i]).fIndex);
              { TODO : replace with path generation, should be context home dir in format file:///drive:/path/ }
              Writer.AddShort('","url":"file:///' + StringReplaceAll(debugger.fWebAppRootPath, '\', '/'));
              Writer.AddShort('","debuggable":');
              Writer.Add(debugger.fCommunicationThread = nil);
              Writer.AddShort(',"consoleActor":"console');
              Writer.Add(debugger.fIndex);
              Writer.AddShort('"},');
            end;
          end;
        finally
          fParent.fDebuggers.Safe.UnLock;
        end;
        Writer.CancelLastComma;
        Writer.AddShort(']}');
      end else if {$IFDEF FPC}request.&type{$ELSE}request.type{$ENDIF} = 'listTabs' then begin
        // VSCode FireFox Debug extension https://github.com/hbenl/vscode-firefox-debug
        // require at last one tab
        Writer.AddShort('{"from":"root","tabs":[{}],"selected":0}');
      end else
        exit;
      Send(Writer.Text);
    finally
      Writer.Free;
    end;
  end else begin
    if fDebugger = nil then begin
      data := VariantToUTF8({$IFDEF FPC}request.&to{$ELSE}request.to{$ENDIF});
      debuggerIndex := GetInteger(@data[8]);
      fParent.fDebuggers.Safe.Lock;
      try
        for I := 0 to fParent.fDebuggers.Count-1 do
          if TSMDebugger(fParent.fDebuggers.List[i]).fIndex = debuggerIndex then begin
            fDebugger := TSMDebugger(fParent.fDebuggers.List[i]);
            break;
          end;
        if (fDebugger = nil) or (fDebugger.fCommunicationThread <> nil) then begin
          fDebugger := nil;
          fNeedClose := true;
          exit;
        end;
      finally
        fParent.fDebuggers.Safe.UnLock;
      end;
      fDebugger.attach(Self);
    end;

    engine := fParent.fManager.EngineForThread(fDebugger.fSmThreadID);
    if (engine <> nil) then begin
      fDebugger.fMessagesQueue.SafePush(VariantToUTF8(request));
      if not fDebugger.fIsPaused then begin
        if (not engine.cx.IsRunning) then begin
          if not Assigned(engine.doInteruptInOwnThread) then
            raise ESMException.Create('not Assigned(engine.doInteruptInOwnThread)');
          engine.doInteruptInOwnThread;
        end;
        {$IFDEF SM52}
        engine.cx.RequestInterruptCallback;
        {$ELSE}
        engine.rt.RequestInterruptCallback;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TSMRemoteDebuggerCommunicationThread.Send(const packet: RawUTF8);
begin
  sockWrite(packet);
  SynSMLog.Add.Log(sllCustom4, packet);
end;

function TSMRemoteDebuggerCommunicationThread.sockRead(out packet: RawUTF8): boolean;
const
  bufSize = 8;
var
  buf: array [0..bufSize] of Byte;
  ch: PUTF8Char;
  len, head, bytesToRead: integer;
begin
  bytesToRead := bufSize;
  FillChar(buf, Length(buf), #0);
  Result := (fCommunicationSock <> nil) and fCommunicationSock.TrySockRecv(@buf[1], bytesToRead);
  if not Result then
    exit;
  ch := @buf[1];
  len := GetNextItemCardinal(ch, ':');
  SetLength(packet, len);
  head := bufSize - (ch - @buf[1]);
  Move(ch^, packet[1], head);
  bytesToRead := len - head;
  Result := fCommunicationSock.TrySockRecv(@packet[head + 1], bytesToRead);
end;

procedure TSMRemoteDebuggerCommunicationThread.sockWrite(const packet: RawUTF8);
var
  tmp: shortstring;
const
  sep: shortstring = ':';
begin
  if fCommunicationSock = nil then
    exit;
  Str(Length(packet), tmp);
  fCommunicationSock.SockSend(@tmp[1], length(tmp));
  fCommunicationSock.SockSend(@sep[1], length(sep));
  fCommunicationSock.SockSend(@packet[1], length(packet));
  fCommunicationSock.SockSendFlush('');
end;

procedure TSMRemoteDebuggerCommunicationThread.startListening(socket: TCrtSocket);
begin
  fCommunicationSock := socket;
  SynSMLog.Add.Log(sllCustom4, 'Accepted');
  Suspended := false;
end;

procedure TSMRemoteDebuggerCommunicationThread.SetTerminated;
begin
  Terminate;
  Suspended := false;
end;

{ TSMDebugger }
procedure TSMDebugger.attach(aThread: TSMRemoteDebuggerCommunicationThread);
begin
  fCommunicationThread := aThread;
  fMessagesQueue.SafeClear;
  fLogQueue.SafeClear;
end;

constructor TSMDebugger.Create(aParent: TSMRemoteDebuggerThread; aEng: TSMEngine);
begin
  fIsPaused := false;
  aParent.fCommunicationThreads.Safe.Lock;
  try
    aParent.fCommunicationThreads.Add(TSMRemoteDebuggerCommunicationThread.Create(aParent));
  finally
    aParent.fCommunicationThreads.Safe.UnLock;
  end;
  fIndex := aParent.fCurThreadIndex;
  inc(aParent.fCurThreadIndex);

  fSmThreadID := GetCurrentThreadId;

  fMessagesQueue := TRawUTF8ListLocked.Create();
  fLogQueue := TRawUTF8ListLocked.Create();
  fNameForDebug := aEng.nameForDebug;
  fDebuggerName := 'synode_debPort_' + aParent.fPort;
  fWebAppRootPath := aEng.webAppRootDir;
  fJsonWriter := TJSONWriter.CreateOwnedStream(1024*50);

  InitializeDebuggerCompartment(aEng, aParent.FNeedPauseOnFirstStep);
end;

destructor TSMDebugger.Destroy;
begin
  if fCommunicationThread <> nil then
    fCommunicationThread.SetTerminated;

  fMessagesQueue.Free;
  fMessagesQueue := nil;
  fLogQueue.Free;
  fLogQueue := nil;
  fJsonWriter.Free;
  inherited;
end;

function doInterupt(cx: PJSContext): Boolean; cdecl;
var
  cmpDbg: PJSCompartment;
  debugger: TSMDebugger;
  engine: TSMEngine;
  dbgObject: PJSRootedObject;
begin
  engine := TSMEngine(cx.PrivateData);
  debugger := engine.PrivateDataForDebugger;
  try
    if (debugger.fMessagesQueue <> nil) and not debugger.fIsPaused and (debugger.fCommunicationThread <> nil) then begin
      cmpDbg := cx.EnterCompartment(engine.GlobalObjectDbg.ptr);
      try
        dbgObject := cx.NewRootedObject(engine.GlobalObjectDbg.ptr.GetPropValue(cx, 'process').asObject.GetPropValue(cx, 'dbg').asObject);
        try
          engine.CallObjectFunction(dbgObject, 'doInterupt', []);
        finally
          cx.FreeRootedObject(dbgObject);
        end;
      finally
        cx.LeaveCompartment(cmpDbg);
      end;
    end;
  finally
  {$IFDEF SM52}
    result := True;
  {$ELSE}
    result := debugger.fOldInterruptCallback(cx);
  {$ENDIF}
  end;
end;

procedure TSMDebugger.InitializeDebuggerCompartment(aEng: TSMEngine; aNeedPauseOnFirstStep: boolean);
var
  cx: PJSContext;
  cmpDbg: PJSCompartment;
  rval: jsval;
  dbgObject: PJSRootedObject;
  res: Boolean;
begin
  fMessagesQueue.SafeClear;
  fLogQueue.SafeClear;

  cx := aEng.cx;
  cmpDbg := cx.EnterCompartment(aEng.GlobalObjectDbg.ptr);
  try
    if not aEng.GlobalObjectDbg.ptr.GetProperty(cx, 'Debugger', rval) or rval.isVoid then begin
      aEng.PrivateDataForDebugger := self;
      res := cx.InitStandardClasses(aEng.GlobalObjectDbg.ptr); Assert(res);
      res := cx.DefineDebuggerObject(aEng.GlobalObjectDbg.ptr); Assert(res);
      res := cx.InitModuleClasses(aEng.GlobalObjectDbg.ptr); Assert(res);
      aEng.DefineProcessBinding;
      aEng.DefineModuleLoader;
      aEng.EvaluateModule('DevTools/Debugger.js');
      dbgObject := cx.NewRootedObject(aEng.GlobalObjectDbg.ptr.GetPropValue(cx, 'process').asObject.GetPropValue(cx, 'dbg').asObject);
      try
        aEng.CallObjectFunction(dbgObject, 'init', [
          SimpleVariantToJSval(cx, fIndex),
          SimpleVariantToJSval(cx, aNeedPauseOnFirstStep)
          ]);
      finally
        cx.FreeRootedObject(dbgObject);
      end;

      if Assigned(aEng.Manager.OnDebuggerInit) then
        aEng.Manager.OnDebuggerInit(aEng);

{$IFDEF SM52}
      aEng.cx.AddInterruptCallback(doInterupt);
{$ELSE}
      foldInterruptCallback := aEng.rt.InterruptCallback;
      aEng.rt.InterruptCallback := doInterupt;
{$ENDIF}
    end;
  finally
    cx.LeaveCompartment(cmpDbg);
  end;
  fIsJustInited := true;
end;

procedure TSMDebugger.Send(const packet: RawUTF8);
begin
  if fCommunicationThread <> nil then
    fCommunicationThread.Send(packet);
end;

function debugger_send(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  val: jsval;
  msg: RawUTF8;
  debugger: TSMDebugger;
begin
  result := true;
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  if debugger.fCommunicationThread = nil then
    exit;
  val := vp.argv[0];
  if val.isString then
    msg := val.asJSString.ToUTF8(cx)
  else begin
    debugger.fJsonWriter.CancelAll;
    val.AddJSON(cx,debugger.fJsonWriter);
    debugger.fJsonWriter.SetText(msg);
  end;
  debugger.Send(msg);
end;

function debugger_err(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  val: jsval;
  msg: RawUTF8;
begin
  val := vp.argv[0];
  if val.isString then
    msg := val.asJSString.ToUTF8(cx)
  else
    msg := val.asJson[cx];
  SynSMLog.Add.Log(sllError, msg);
  result := true;
end;

function debugger_read(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
  msg: RawUTF8;
  Queue: TRawUTF8ListLocked;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  if (argc = 0) or vp.argv[0].asBoolean then
    Queue := debugger.fMessagesQueue
  else
    Queue := debugger.fLogQueue;
  msg := '';
  while ((Queue <> nil) and (debugger.fCommunicationThread <> nil) and
    (not Queue.SafePop(msg))) and (argc = 0) do
    SleepHiRes(10);
  result := true;
  if (Queue <> nil) and (debugger.fCommunicationThread <> nil) then
    vp.rval := SimpleVariantToJSval(cx, msg)
  else // debugger.js will detach current debugee if msg === null
    vp.rval := JSVAL_NULL;
end;

function debugger_listen(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  vp.rval := SimpleVariantToJSval(cx, Assigned(debugger.fCommunicationThread));
  result := true;
end;

function debugger_setPaused(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  debugger.fIsPaused := vp.argv[0].asBoolean;
  if debugger.fIsJustInited and not debugger.fIsPaused then begin
    debugger.fIsJustInited := false;
    if (debugger.fCommunicationThread <> nil) and Assigned(debugger.fCommunicationThread.fParent.fManager.OnDebuggerConnected) then
      debugger.fCommunicationThread.fParent.fManager.OnDebuggerConnected(TSMEngine(cx.PrivateData));
  end;
  result := true;
end;

function debugger_isPaused(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  vp.rval := SimpleVariantToJSval(cx, debugger.fIsPaused);
  result := true;
end;

function debugger_debuggerName(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  vp.rval := SimpleVariantToJSval(cx, debugger.fDebuggerName);
  result := true;
end;

function debugger_nameForDebug(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  vp.rval := SimpleVariantToJSval(cx, debugger.fNameForDebug);
  result := true;
end;

function debugger_threadId(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  { TODO : check that in multithread mode this field equal thread id with js context that we debug, otherwire replace with proper assigment }  
  vp.rval := SimpleVariantToJSval(cx, ToUTF8(debugger.fSmThreadID));
  // TSMDebugger(fParent.fDebuggers[i]).fSmThreadID
  result := true;
end;

function debugger_webAppRootPath(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  debugger: TSMDebugger;
begin
  debugger := TSMEngine(cx.PrivateData).PrivateDataForDebugger;
  vp.rval := SimpleVariantToJSval(cx, debugger.fWebAppRootPath);
  result := true;
end;

function SyNodeBindingProc_debugger(const Engine: TSMEngine;
  const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
  res: Boolean;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    res := cx.WrapObject(Engine.GlobalObject.ptr); Assert(res);

    obj.ptr.DefineFunction(cx, 'send', debugger_send, 1);
    obj.ptr.DefineFunction(cx, 'logError', debugger_err, 1);
    obj.ptr.DefineFunction(cx, 'read', debugger_read, 0);
    obj.ptr.DefineProperty(cx, 'listen', JSVAL_NULL, 0, debugger_listen);
    obj.ptr.DefineProperty(cx, 'paused', JSVAL_NULL, 0, debugger_isPaused, debugger_setPaused);
    obj.ptr.DefineProperty(cx, 'debuggerName', JSVAL_NULL, 0, debugger_debuggerName);
    obj.ptr.DefineProperty(cx, 'addonID', JSVAL_NULL, 0, debugger_nameForDebug);
    obj.ptr.DefineProperty(cx, 'threadId', JSVAL_NULL, 0, debugger_threadId);
    obj.ptr.DefineProperty(cx, 'webAppRootPath', JSVAL_NULL, 0, debugger_webAppRootPath);

    obj.ptr.DefineProperty(cx, 'global', Engine.GlobalObject.ptr.ToJSValue);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('debugger', SyNodeBindingProc_debugger);

end.

