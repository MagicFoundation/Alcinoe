/// SyNode - server-side JavaScript execution using the SpiderMonkey 45/52
// library with nodeJS modules support
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNode;
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
  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez
  - Vadim Orel
  - Pavel Mashlyakovsky
  - win2014
  - Geogre
  - ssoftpro

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
  - x64 support added
  - move a Worker classes from UnityBase to SyNode
    Allow to create and communicate with a threads from JavaScript
  - implement nodeJS Buffer
  - improved compartibility with nodeJS utils
  - add a `process.version`
  - implement a setTimeout/setInverval/setImmediate etc.
  - SpiderMonkey 52 (x32 & x64) support ( SM52 condition should be defined )
  - VSCode debugging support via [vscode-firefox-debug](https://github.com/hbenl/vscode-firefox-debug) adapter
    Thanks to George for patch
  - JS `process` now instance of EventEmitter
  - JS Engine will emit `exit` event on destroy as in node

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER
{$I SyNode.inc}   // define SM_DEBUG CONSIDER_TIME_IN_Z
{$IFDEF CORE_MODULES_IN_RES}
  {$R 'core_modules.res' 'core_modules.rc'}
{$ENDIF}

interface

uses
  {$ifndef FPC}
  Windows,
  ShLwApi, // for older Delphi versions download this file from JEDI library
  {$else}
  LazFileUtils, dynlibs,
  {$endif}
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  {$ifndef LVCL}
  Contnrs,
  {$endif}
  {$ifdef ISDELPHIXE2}
  System.Rtti,
  {$endif}
  Variants,
  SynCommons,
  SynLog,
  SpiderMonkey,
  NSPRAPI,
  SyNodeProto,
  mORMot,
  SynCrtSock,
  SyNodeBinding_worker;

const
  /// default stack growning size
  STACK_CHUNK_SIZE: Cardinal = 8192;

type

  {$M+}
  TSMEngineManager = class;
  {$M-}

  TSMEngine = class;

  TSMEngineClass = class of TSMEngine;

  /// implements a ThreadSafe JavaScript engine
  // - use TSMEngineManager.ThreadSafeEngine to retrieve the Engine instance
  // corresponding to the current thread, in multithread application
  // - contains JSRuntime + JSContext (to be ready for new SpiderMonkey version where
  // context and runtime is the same)
  // - contains also one "global" JavaScript object. From script it is
  // accessible via "global." (in browser, this is the "window." object)
  // - set SpiderMonkey error reporter and store last SpiderMonkey error in
  // LastError property
  TSMEngine = class
  private
    fPrivateDataForDebugger: Pointer;
    fNameForDebug: RawUTF8;
    fDoInteruptInOwnThread: TThreadMethod;
    {$IFNDEF SM52}
    fRt: PJSRuntime;
    {$ENDIF}
    fcomp: PJSCompartment;
    fStringFinalizer: JSStringFinalizer;

    fManager: TSMEngineManager;

    fGlobalObject: PJSRootedObject;
    // gloal._timerLoop function used to emulate setTimeout/setInterval
    // event loop. Implemented in WindowsTimer.js
    FGlobalTimerLoopFunc: PJSRootedValue;
    fGlobalObjectDbg: PJSRootedObject;

    fEngineContentVersion: Cardinal;
    fThreadID: TThreadID;
    fLastErrorMsg: string;
    fLastErrorNum: integer;
    fLastErrorFileName: RawUTF8;
    fLastErrorLine: integer;
    fLastErrorStackTrace: SynUnicode;
    fErrorExist: boolean;
    fThreadData: pointer;
    fDllModulesUnInitProcList: TList;
    fNeverExpire: boolean;
    fWebAppRootDir: RawUTF8;
    /// called from SpiderMonkey callback. Do not raise exception here
    // instead use CheckJSError metod after JSAPI compile/evaluate call
{$IFDEF SM52}
    procedure DoProcessJSError(report: PJSErrorReport); virtual;
{$ELSE}
    procedure DoProcessJSError(errMsg: PCChar; report: PJSErrorReport); virtual;
{$ENDIF}
    /// called from SpiderMonkey callback. It used for interrupt execution of script
    //  when it executes too long
    function DoProcessOperationCallback: Boolean; virtual;
    procedure SetPrivateDataForDebugger(const Value: Pointer);
  protected
    fCx: PJSContext;
    // used by Watchdog thread state. See js.cpp
    fTimeOutAborted: Boolean;
    fTimedOut: Boolean;
    fWatchdogLock: PRLock;
    fWatchdogWakeup: PRCondVar;
    fWatchdogThread: PRThread;
    fWatchdogHasTimeout: Boolean;
    fWatchdogTimeout: Int64;
    fSleepWakeup: PRCondVar;
    fTimeoutValue: integer;
    fCreatedAtTick: Int64;

    function ScheduleWatchdog(t: integer): Boolean;
    procedure KillWatchdog;
    function InitWatchdog: boolean;
    procedure SetTimeoutValue(const Value: Integer);
  public
    /// create one threadsafe JavaScript Engine instance
    // - initialize internal JSRuntime, JSContext, and global objects and
    // standard JavaScript classes
    // - do not create Engine directly via this constructor, but instead call
    // TSMEngineManager.ThreadSafeEngine
    constructor Create(aManager: TSMEngineManager); virtual;
    /// finalize the JavaScript engine instance
    destructor Destroy; override;
    /// reference to TSMEngineManager own this engine
    property Manager: TSMEngineManager read FManager;

    /// thread specific data - pointer to any structure, passed into ThreadSafeEngine call
    //  used to access a thread-details in the native functions
    //  as TsmEngive(cx.PrivateData).ThreadData
    property ThreadData: pointer read FThreadData;
    procedure SetThreadData(pThreadData: pointer);

    /// check if last call to JSAPI compile/eval fucntion was successful
    // - raise ESMException if any error occurred
    // - put error description to SynSMLog
    procedure CheckJSError(res: Boolean);
    /// clear last JavaScript error
    // - called before every evaluate() function call
    procedure ClearLastError;

    /// trigger Garbage Collection
    // - all unrooted things (JSString, JSObject, VSVal) will be released
    procedure GarbageCollect;
    /// Offer the JavaScript engine an opportunity to perform garbage collection if needed
    // - Tries to determine whether garbage collection in would free up enough
    // memory to be worth the amount of time it would take. If so, it performs
    // some garbage collection
    // - Frequent calls are safe and will not cause the application to spend a
    // lot of time doing redundant garbage collection work
    procedure MaybeGarbageCollect;


    /// evaluate a JavaScript script in the global scope
    // - if exception raised in script - raise Delphi ESMException
    // - on success returns last executed expression statement processed
    // in the script as a variant
    // - JavaScript equivalent to
    // ! eval(script)
    procedure Evaluate(const script: SynUnicode; const scriptName: RawUTF8;
      lineNo: Cardinal; out result: jsval); overload;

    procedure Evaluate(const script: SynUnicode; const scriptName: RawUTF8;
      lineNo: Cardinal); overload;

    /// evaluate script embadedd into application resources
    procedure EvaluateRes(const ResName: string; out result: jsval);

    /// evaluate a JavaScript script as Module
    // - if exception raised in script - raise Delphi ESMException
    // - JavaScript equivalent to import of ES6
    function EvaluateModule(const scriptName: RawUTF8): jsval;

    /// run method of object
    // - if exception raised in script - raise Delphi ESMException
    // - on success returns function result
    // - JavaScript equivalent to
    // ! obj.funcName(args[0], args[1]...)
    function CallObjectFunction(obj: PJSRootedObject; funcName: PCChar; const args: array of jsval): jsval;
    /// The same as CallObjectFunction but accept funcVal insteadof fuction name
    // a little bit faster when CallObjectFunction
    function CallObjectFunctionVal(obj: PJSRootedObject; const funcVal: PJSRootedValue; const args: array of jsval): jsval;

    /// access to the associated global object as low-level PJSRootedObject
    property GlobalObject: PJSRootedObject read FGlobalObject;
    /// access to the associated global object for debugging as low-level PJSRootedObject
    property GlobalObjectDbg: PJSRootedObject read FGlobalObjectDbg;
    /// access to the associated execution context
    property cx: PJSContext read fCx;
    {$IFNDEF SM52}
    /// access to the associated execution runtime
    property rt: PJSRuntime read frt;
    {$ENDIF}
    /// access to the associated execution compartment
    property comp: PJSCompartment read fcomp;

    /// internal version number of engine scripts
    // - used in TSMEngine.ThreadSafeEngine to determine if context is up to
    // date, in order to trigger on-the-fly reload of scripts without the need
    // if restarting the application
    // - caller must change this parameter value e.g. in case of changes in
    // the scripts folder in an HTTP server
    property EngineContentVersion: Cardinal read FEngineContentVersion;

    /// last error message triggered during JavaScript execution
    property LastErrorMsg: string read FLastErrorMsg;
    /// last error source code line number triggered during JavaScript execution
    property LastErrorLine: integer read FLastErrorLine;
    /// last error file name triggered during JavaScript execution
    property LastErrorFileName: RawUTF8 read FLastErrorFileName;
    /// TRUE if an error was triggered during JavaScript execution
    property ErrorExist: boolean read FErrorExist;
    /// notifies a WatchDog timeout
    property TimeOutAborted: boolean read FTimeOutAborted;
    /// define a WatchDog timeout interval (in seconds)
    // - is set to -1 by default, i.e. meaning no execution timeout
    property TimeOutValue: Integer read fTimeoutValue write SetTimeoutValue default -1;
    /// If `true` engine will never expire
    property NeverExpire: boolean read FNeverExpire write FNeverExpire;

    /// Define a Delphi class as JS class (prototype), so it can be created via new:
    //    engine.defineClass(TmyObject, TSMNewRTTIProtoObject);
    //  and in JS
    //    var myObj = new TmyObject();
    // - Class will be exposed in the aParent javaScript object, if aParent is omitted - in the global object
    // - AProto parameter can be a TSMCustomProtoObject descendant (TSMSimpleRTTIProtoObject, TSMNewRTTIProtoObject)
    // WARNING - possible prototypes count is limited to 255 - JSCLASS_GLOBAL_SLOT_COUNT = 76 for SM45,
    //   so it is better to use a bindings, add where a native functions and wrap it in class into the JavaScript
    //   see `fs.js` for sample.
    function defineClass(AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject = nil): TSMCustomProtoObject;
    /// Add a set of classes with the same delphi binding implementation:
    //    engine.defineClasses([TubEntity, TubDomain, TubEntityAttribute], TSMNewRTTIProtoObject);
    procedure defineClasses(const AClasses: array of TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject = nil);
    /// define JS object for enumeratin type
    //  for TMyEnum = (etA, etB); will create in JS aObj.TMyEnum = {etA: 0, etB: 1);
    procedure defineEnum(ti: PTypeInfo; aObj: PJSRootedObject);

    /// Cancel current execution JavaScript.
    // This procedure can be called from other script
    // if AWithException paramener is thue, will raise a
    // ESMException with message LONG_SCRIPT_EXECUTION
    procedure CancelExecution(AWithException: boolean = true);

    procedure DefineProcessBinding;
    procedure DefineNodeProcess;
    procedure DefineModuleLoader;

    /// Used by debugger
    property PrivateDataForDebugger: Pointer read FPrivateDataForDebugger write SetPrivateDataForDebugger;
    /// Name of this engine. Will be shown in debugger for indentify this engine
    property nameForDebug: RawUTF8 read fnameForDebug;
    /// root path for current web app engine context
    property webAppRootDir: RawUTF8 read fWebAppRootDir;
    /// Handler hor debugger. Called from debugger thread when debugger need
    // call rt.InterruptCallback(cx) in engine''s thread
    // this method must wake up the engine's thread and thread must
    // execute rt.InterruptCallback(cx) for this engine
    property doInteruptInOwnThread: TThreadMethod read fDoInteruptInOwnThread write fDoInteruptInOwnThread;
  end;


  /// prototype of SpideMonkey notification callback method
  TEngineEvent = procedure(const Engine: TSMEngine) of object;

  /// event of getting engine name or web app root path
  TEngineNameEvent = function(const Engine: TSMEngine): RawUTF8 of object;

  /// event for loading dll module
  TDllModuleEvent = procedure(Handle: HMODULE) of object;

  /// event for adding values to process.binding
  TSMProcessBindingHandler = function(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;

  TDllModuleInitProc = function(cx: PJSContext; exports_: PJSRootedObject; require: PJSRootedObject; module: PJSRootedObject; __filename: PWideChar; __dirname: PWideChar): boolean; cdecl;
  TDllModuleUnInitProc = function: boolean; cdecl;
  TDllModuleRec = record
    init: TDllModuleInitProc;
    unInit: TDllModuleUnInitProc;
  end;
  PDllModuleRec = ^TDllModuleRec;

  /// main access point to the SpiderMonkey per-thread scripting engines
  // - allow thread-safe access to an internal per-thread TSMEngine instance list
  // - contains runtime-level properties shared between thread-safe engines
  // - you can create several TSMEngineManager instances, if you need several
  // separate scripting instances
  // - set OnNewEngine callback to initialize each TSMEngine, when a new thread
  // is accessed, and tune per-engine memory allocation via MaxPerEngineMemory
  // and MaxRecursionDepth
  // - get the current per-thread TSMEngine instance via ThreadSafeEngine method
  TSMEngineManager = class
  private
    FMaxPerEngineMemory: Cardinal;
    FMaxNurseryBytes: Cardinal;
    FMaxRecursionDepth: Cardinal;
    FEnginePool: TSynObjectListLocked;
    FRemoteDebuggerThread: TThread;
    FContentVersion: Cardinal;
    FOnNewEngine: TEngineEvent;
    FOnDebuggerInit: TEngineEvent;
    FOnGetName: TEngineNameEvent;
    FOnGetWebAppRootPath: TEngineNameEvent;
    FOnDebuggerConnected: TEngineEvent;
    FOnDllModuleLoaded: TDllModuleEvent;
    {$ifdef ISDELPHIXE2}
    FRttiCx: TRttiContext;
    {$endif}
    FEngineClass: TSMEngineClass;

    /// List of loaded dll modules
    FDllModules: TRawUTF8ListHashedLocked;
    /// Path to core modules
    FCoreModulesPath: RawUTF8;
    FEngineExpireTimeOutTicks: Int64;
    FWorkersManager: TJSWorkersManager;

    function GetEngineExpireTimeOutMinutes: cardinal;
    procedure SetEngineExpireTimeOutMinutes(const minutes: cardinal);
    procedure SetMaxPerEngineMemory(AMaxMem: Cardinal);
    procedure SetMaxNurseryBytes(AMaxNurseryBytes: Cardinal);
  protected
    grandParent: TSMEngine;
    /// Release a javaScript engine for specified thread
    procedure ReleaseEngineForThread(aThreadID: TThreadID);
    /// returns -1 if none was defined yet
    // - this method is not protected via the global FEngineCS mutex/lock
    function ThreadEngineIndex(ThreadID: TThreadID): Integer;
    /// returns nil if none was defined yet
    function CurrentThreadEngine: TSMEngine;
    /// called when a new Engine is created
    // - this default implementation will run the OnNewEngine callback (if any)
    procedure DoOnNewEngine(const Engine: TSMEngine); virtual;

    function getPauseDebuggerOnFirstStep: boolean;
    procedure setPauseDebuggerOnFirstStep(const Value: boolean);

    function evalDllModule(cx: PJSContext; module: PJSRootedObject; const filename: RawUTF8): TDllModuleUnInitProc;
    /// Get handler of process.binding
    function GetBinding(const Name: RawUTF8): TSMProcessBindingHandler;
  public
    /// initialize the SpiderMonkey scripting engine
    // aCoreModulesPath can be empty in case core modules are embadded
    // as resources (CORE_MODULES_IN_RES is defined)
    constructor Create(const aCoreModulesPath: RawUTF8; aEngineClass: TSMEngineClass = nil); virtual;
    /// finalize the SpiderMonkey scripting engine
    destructor Destroy; override;
    /// Add a binding namespace - a set of native functions exposed to the JavaScript engine
    // - we recommend not to use it directly in the businnes logic,
    // but create a JavaScript module and wrap a binding function by the JavaScript
    // see how `fs` binding from SyNodeBinding_fs unit is wrapped by `SyNode\core_modules\node_modules\fs.js`
    class procedure RegisterBinding(const Name: RawUTF8; const handler: TSMProcessBindingHandler);
    /// get or create one Engine associated with current running thread
    // pThreadData is a pointer to any structure relative to this thread
    // accessible via TsmEngine.ThreadData
    function ThreadSafeEngine(pThreadData: pointer): TSMEngine;
    /// method to be called when a thread is about to be finished
    // - you can call this method just before a thread is finished to ensure
    // that the associated scripting Engine will be released
    // - could be used e.g. in a try...finally block inside a TThread.Execute
    // overriden method
    procedure ReleaseCurrentThreadEngine;
    /// returns nil if none was defined yet
    function EngineForThread(ThreadID: TTHreadID): TSMEngine;
    /// internal version of the script files
    // - used in TSMEngine.ThreadSafeEngine to determine if context is up to
    // date, in order to trigger on-the-fly reload of scripts without the need
    // if restarting the application
    property ContentVersion: Cardinal read FContentVersion write FContentVersion;
    /// lock/mutex used for thread-safe access to the TSMEngine list
    procedure Lock;
    procedure UnLock;
    {$ifdef ISDELPHIXE2}
    /// for defining JS object based on New RTTI we need a single TRttiContext
    // in other case we got AV if used from DLL
    property RttiCx: TRttiContext read FRttiCx;
    {$endif}
    /// Start/stop debugger listening on selected port
    // - expects the port to be specified as Ansi string, e.g. '1234'
    // - you can optionally specify a server address to bind to, e.g.
    // '1.2.3.4:1234'
    // - debugger create a dedicated thread where it listen to a requests
    // from a remote debug UI
    procedure startDebugger(const port: SockString = '6000');
    procedure stopDebugger;
    /// Write text as console log to current thread Engine's debugger (if exists)
    procedure debuggerLog(const Text: RawUTF8);
    /// when debugger connected to new engine this Engine must pause on first step
    property pauseDebuggerOnFirstStep: boolean read getPauseDebuggerOnFirstStep write setPauseDebuggerOnFirstStep;
    /// Workers manager
    property WorkersManager: TJSWorkersManager read FWorkersManager;
    /// Delete all started worker threads
    procedure ClearWorkers;
  published
    /// max amount of memory (in bytes) for a single SpiderMonkey instance
    // - this parameter will be set only at Engine start, i.e. it  must be set
    // BEFORE any call to ThreadSafeEngine
    // - default is 32 MB
    property MaxPerEngineMemory: Cardinal read FMaxPerEngineMemory write SetMaxPerEngineMemory
      default 32*1024*1024;
    // Nursery size in bytes
    // - default is 16 MB
    property MaxNurseryBytes: Cardinal read FMaxNurseryBytes write SetMaxNurseryBytes
      default 16*1024*1024;
    /// maximum expected recursion depth for JavaScript functions
    // - to avoid out of memory situation in functions like
    // ! function f(){ f() };
    // - default is 32, but you can specify some higher value
    property MaxRecursionDepth: Cardinal read FMaxRecursionDepth write FMaxRecursionDepth
      default 32;
    /// specify a maximum lifetime period after which JavaScript Engine will
    // be recreated, to avoid potential JavaScript memory leak (variables in global,
    // closures circle, etc.)
    // - 0 by default - mean no expire timeout
    // In case engine must never expire - set the NeverExpire property of engine manually
   property EngineExpireTimeOutMinutes: cardinal
      read GetEngineExpireTimeOutMinutes write SetEngineExpireTimeOutMinutes default 0;
    /// Path to synode core modules.
    // Not used in case core modules are embadded as resources (CORE_MODULES_IN_RES is defined)
    property CoreModulesPath: RawUTF8 read FCoreModulesPath;
    /// event triggered every time a new Engine is created
    // event trigered before OnDebuggerInit and OnNewEngine events
    // Result og this method is Engine's name for debug
    property OnGetName: TEngineNameEvent read FOnGetName write FOnGetName;
    /// event triggered every time a new Engine is created
    // event trigered before OnDebuggerInit and OnNewEngine events
    // Result og this method is Engine's web app root path
    property OnGetWebAppRootPath: TEngineNameEvent read FOnGetWebAppRootPath write FOnGetWebAppRootPath;

    /// event triggered every time a internal debugger process connected to Engine
    // - event trigered in debugger's compartment
    // - here your code can change the initial state of the debugger
    property OnDebuggerInit: TEngineEvent read FOnDebuggerInit write FOnDebuggerInit;

    /// event triggered every time a new Engine is created
    // - here your code can change the initial state of the Engine
    property OnNewEngine: TEngineEvent read FOnNewEngine write FOnNewEngine;

    /// event triggered every time a new remote debugger(Firefox) connect to Engine
    // - Warning!!! event trigered in debugger's communication thread(not thread of Engine)
    // so you cannot use Engine's Javascript in this method, but only Engine's properties
    property OnDebuggerConnected: TEngineEvent read FOnDebuggerConnected write FOnDebuggerConnected;

    /// event triggered every time a new dll module is loaded
    // - here you code can customize dll module or call some functions from dll
    property OnDllModuleLoaded: TDllModuleEvent read FOnDllModuleLoaded write FOnDllModuleLoaded;
  end;
  {$M-}
  function SyNodeBinding_modules(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;

var
  /// define the TSynLog class used for logging for all our SynSM related units
  // - you may override it with TSQLLog, if available from mORMot.pas
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SynSMLog: TSynLogClass=TSynLog;

  LONG_SCRIPT_EXECUTION: string = 'Script runs for too long. Terminating';

  /// check is AFileName is relative path, and if true - transform it to absolute from ABaseDir
  //  if ACheckResultInsideBase = true the perform check of result path is under ABaseDir. If not - return '';
  //  In any case will concatenates any relative path elements and replace '/' by '\' under Windows
  function RelToAbs(const ABaseDir, AFileName: TFileName; ACheckResultInsideBase: boolean = false): TFileName;

implementation

uses
  {$IFNDEF MSWINDOWS}
  Errors, SynFPCLinux,
  {$ENDIF}
  SyNodeRemoteDebugger,
  SyNodeBinding_fs {used by other core modules},
  SyNodeBinding_const,
  SyNodeBinding_buffer,
  SyNodeBinding_util,
  SyNodeBinding_uv,
  synodebinding_os;

const
  jsglobal_class: JSClass = (name: 'global';
    flags:
      JSCLASS_GLOBAL_FLAGS or
      JSCLASS_HAS_PRIVATE or
      (255 shl JSCLASS_RESERVED_SLOTS_SHIFT);
    );

var
  GlobalSyNodeBindingHandlers: TRawUTF8ListHashedLocked;

/// handle errors from JavaScript. Just call DoProcessJSError of corresponding TSMEngine
// to set TSMEngine error properties
{$IFDEF SM52}
procedure WarningReporter(cx: PJSContext; report: PJSErrorReport); cdecl;
begin
  TSMEngine(cx.PrivateData).DoProcessJSError(report)
end;
{$ELSE}
procedure ErrorReporter(cx: PJSContext; pErrMsg: PCChar; report: PJSErrorReport); cdecl;
begin
  TSMEngine(cx.PrivateData).DoProcessJSError(pErrMsg, report)
end;
{$ENDIF}

function OperationCallback(cx: PJSContext): Boolean; cdecl;
begin
  Result := TSMEngine(cx.PrivateData).DoProcessOperationCallback;
end;

{ TSMEngine }

// do nothing here
procedure ExternalStringFinalizer(fin: PJSStringFinalizer; chars: PCChar16); cdecl;
begin
  {}
end;

function process_binding(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage: binding(scope: String)';
var
  Eng: TSMEngine;
  in_argv: PjsvalVector;
  bindingNS: SynUnicode;
  res: jsval;
  handler: TSMProcessBindingHandler;
  FBindingObject: PJSRootedObject;
begin
  Eng := TObject(cx.PrivateData) as TSMEngine;
  try
    in_argv := vp.argv;
    if (argc <> 1) or not (in_argv[0].isString) then
      raise ESMException.Create(USAGE);
    bindingNS := in_argv[0].asJSString.ToSynUnicode(cx);
    FBindingObject := cx.NewRootedObject(vp.calleObject);
    try
      if FBindingObject.ptr <> nil then begin
        res := FBindingObject.ptr.GetPropValue(cx, bindingNS);
        if res.isVoid then begin
          handler := Eng.Manager.GetBinding(SynUnicodeToUtf8(bindingNS));
          if Assigned(handler) then begin
            res := handler(eng, bindingNS);
            if not res.isVoid then begin
              FBindingObject.ptr.DefineUCProperty(cx, bindingNS, res);
            end;
          end else
            raise ESMException.CreateFmt('Invalid binding namespace "%s"',[bindingNS]);
        end;
      end else
        res.SetNull;
    finally
      cx.FreeRootedObject(FBindingObject);
    end;

    vp.rval := res;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

constructor TSMEngine.Create(aManager: TSMEngineManager);
const
  gMaxStackSize = 128 * sizeof(size_t) * 1024;
var
{$IFDEF SM52}
  cOpts: PJSContextOptions;
{$ELSE}
  rOpts: PJSRuntimeOptions;
{$ENDIF}
  fpu: IUnknown;
begin
  fpu := TSynFPUException.ForLibraryCode;
  if aManager = nil then
    raise ESMException.Create('No manager provided');
  FCreatedAtTick := GetTickCount64;
  FManager := aManager;
  FEngineContentVersion := FManager.ContentVersion;
{$IFDEF SM52}
// TODO - solve problem of destroying parent engine before slave and uncomment a code below
// this will save up to 20% of RAM - some internal structures of SM Engines will be reused 
// between threads
//  if aManager.grandParent <> nil then
//    fCx := PJSContext(nil).CreateNew(FManager.MaxPerEngineMemory, $01000000, aManager.grandParent.cx)
//  else
    fCx := PJSContext(nil).CreateNew(FManager.MaxPerEngineMemory, $01000000, nil);
  if fCx = nil then
    raise ESMException.Create('Create context: out of memory');
{$IFNDEF CPUX64} // This check does not always work correctly under 64-bit configurations. Need more investigation to understand the problem
  fCx.SetNativeStackQuota(gMaxStackSize);
{$ENDIF}
  fCx.GCParameter[JSGC_MAX_BYTES] := FManager.MaxPerEngineMemory;
// MPV as Mozilla recommend in https://bugzilla.mozilla.org/show_bug.cgi?id=950044
//TODO - USE JS_SetGCParametersBasedOnAvailableMemory for SM32 and override JSGC_MAX_MALLOC_BYTES
  if (FManager.MaxPerEngineMemory >= 512 * 1024 * 1024) then begin
    fCx.GCParameter[JSGC_MAX_MALLOC_BYTES] :=  96 * 1024 * 1024;
    fCx.GCParameter[JSGC_SLICE_TIME_BUDGET] :=  30;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1000;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_HIGH_LIMIT] := 500;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_LOW_LIMIT] := 100;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MAX] :=  300;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MIN] :=  150;
    fCx.GCParameter[JSGC_LOW_FREQUENCY_HEAP_GROWTH] := 150;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fCx.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fCx.GCParameter[JSGC_ALLOCATION_THRESHOLD] := 30;
//    fCx.GCParameter[JSGC_DECOMMIT_THRESHOLD] := 32;
//    fCx.GCParameter[JSGC_MODE] := uint32(JSGC_MODE_COMPARTMENT);
    fCx.GCParameter[JSGC_MODE] := uint32(JSGC_MODE_INCREMENTAL);
  end else begin
    fCx.GCParameter[JSGC_MAX_MALLOC_BYTES] := 6 * 1024 * 1024; //MPV in PREV realisation - FManager.MaxPerEngineMemory div 2; we got a memory overuse
    fCx.GCParameter[JSGC_MODE] := uint32(JSGC_MODE_INCREMENTAL);
  end;
{$ELSE}
  frt := PJSRuntime(nil).New(FManager.MaxPerEngineMemory, $01000000, nil);
  if frt = nil then
    raise ESMException.Create('Create runtime: out of memory');
  fRt.SetNativeStackQuota(gMaxStackSize);
  fRt.GCParameter[JSGC_MAX_BYTES] := FManager.MaxPerEngineMemory;
// MPV as Mozilla recommend in https://bugzilla.mozilla.org/show_bug.cgi?id=950044
//TODO - USE JS_SetGCParametersBasedOnAvailableMemory for SM32 and override JSGC_MAX_MALLOC_BYTES
  if (FManager.MaxPerEngineMemory >= 512 * 1024 * 1024) then begin
    fRt.GCParameter[JSGC_MAX_MALLOC_BYTES] :=  96 * 1024 * 1024;
    fRt.GCParameter[JSGC_SLICE_TIME_BUDGET] :=  30;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1000;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_HIGH_LIMIT] := 500;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_LOW_LIMIT] := 100;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MAX] :=  300;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_HEAP_GROWTH_MIN] :=  150;
    fRt.GCParameter[JSGC_LOW_FREQUENCY_HEAP_GROWTH] := 150;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fRt.GCParameter[JSGC_HIGH_FREQUENCY_TIME_LIMIT] := 1500;
    fRt.GCParameter[JSGC_ALLOCATION_THRESHOLD] := 30;
    fRt.GCParameter[JSGC_DECOMMIT_THRESHOLD] := 32;
    fRt.GCParameter[JSGC_MODE] := uint32(JSGC_MODE_COMPARTMENT);
  end else begin
    fRt.GCParameter[JSGC_MAX_MALLOC_BYTES] := 6 * 1024 * 1024; //MPV in PREV realisation - FManager.MaxPerEngineMemory div 2; we got a memory overuse
    fRt.GCParameter[JSGC_MODE] := uint32(JSGC_MODE_INCREMENTAL);
  end;

  fCx := rt.NewContext(STACK_CHUNK_SIZE);
  if fCx = nil then
    raise ESMException.Create('JSContext create');

{$ENDIF}

{$IFDEF SM52}
  cOpts := cx.Options;
  cOpts.Baseline := True;
  cOpts.Ion := True;
  cOpts.AsmJS := True;
{$ELSE}
  // You must set jsoBaseLine,jsoTypeInference,jsoIon for the enabling ION
  // ION is disabled without these options
  rOpts := rt.Options;
  rOpts.Baseline := True;
  rOpts.Ion := True;
  rOpts.AsmJS := True;
{$ENDIF}

  fStringFinalizer.finalize := ExternalStringFinalizer;
  cx.PrivateData := self;
{$IFDEF SM52}
  // not working for errors
  // TODO: implement log warnings if needed
  // fCx.WarningReporter := WarningReporter;
{$ELSE}
  fRt.ErrorReporter := ErrorReporter;
{$ENDIF}

  FGlobalObject := cx.NewRootedObject(cx.NewGlobalObject(@jsglobal_class));
  if FGlobalObject.ptr = nil then
    raise ESMException.Create('Create global object');
  fcomp := cx.EnterCompartment(FGlobalObject.ptr);
  if not cx.InitStandardClasses(FGlobalObject.ptr) then
    raise ESMException.Create('InitStandardClasses failure');
  if not cx.InitCTypesClass(FGlobalObject.ptr) then
    raise ESMException.Create('InitCTypesClass failure');
//  if not cx.InitReflectParse(FGlobalObject.ptr) then
//    raise ESMException.Create('InitReflectParse failure');
  if not cx.InitModuleClasses(FGlobalObject.ptr) then
    raise ESMException.Create('InitModuleClasses failure');

  FGlobalObject.ptr.DefineProperty(cx, 'global', FGlobalObject.ptr.ToJSValue,
    JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY, nil, nil);

  fTimeoutValue := -1;
  if not InitWatchdog then
    raise ESMException.Create('InitWatchDog failure');

{$IFDEF SM52}
  fcx.AddInterruptCallback(OperationCallback);
{$ELSE}
  fRt.InterruptCallback := OperationCallback;
{$ENDIF}
  FDllModulesUnInitProcList := TList.Create;
  DefineProcessBinding;
  DefineNodeProcess;
  DefineModuleLoader;

  EvaluateModule('synode.js');
  FGlobalTimerLoopFunc := cx.NewRootedValue(FGlobalObject.ptr.GetPropValue(cx, '_timerLoop'));

  FGlobalObjectDbg := cx.NewRootedObject(cx.NewGlobalObject(@jsglobal_class));
end;

procedure TSMEngine.defineEnum(ti: PTypeInfo; aObj: PJSRootedObject);
begin
  SyNodeProto.defineEnum(cx, ti, aObj);
end;

procedure TSMEngine.DefineModuleLoader;
var ModuleLoaderPath: TFileName;
    script: SynUnicode;
    rval: jsval;
begin
  {$IFDEF CORE_MODULES_IN_RES}
  EvaluateRes('MODULELOADER.JS', rval);
  {$ELSE}
  ModuleLoaderPath := RelToAbs(UTF8ToString(FManager.coreModulesPath) , 'ModuleLoader.js');
  script := AnyTextFileToSynUnicode(ModuleLoaderPath);
  if script = '' then
    raise ESMException.Create('File not found ' + ModuleLoaderPath);
  Evaluate(script, 'ModuleLoader.js', 1, rval);
  {$ENDIF}
end;

procedure TSMEngine.DefineNodeProcess;
var process: PJSRootedObject;
    env: PJSRootedObject;
    FStartupPath: TFileName;
    {$IFDEF FPC}
    I, Cnt: Integer;
    EnvStr: AnsiString;
    Parts: array of AnsiString;
    {$ELSE}
    L: PtrInt;
    EnvBlock, P, pEq: PChar;
    strName, strVal: SynUnicode;
    {$ENDIF}
begin
  process := cx.NewRootedObject(FGlobalObject.ptr.GetPropValue(cx, 'process').asObject);
  env := cx.NewRootedObject(cx.NewObject(nil));
  try
    process.ptr.defineProperty(cx, 'binPath', cx.NewJSString(ExeVersion.ProgramFilePath).ToJSVal,
      JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY);

    process.ptr.defineProperty(cx, 'execPath', cx.NewJSString(ExeVersion.ProgramFileName).ToJSVal,
      JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY);

    FStartupPath := GetCurrentDir + {$IFDEF FPC}DirectorySeparator{$ELSE}'\'{$ENDIF};
    if FStartupPath = '' then
      FStartupPath := ExeVersion.ProgramFilePath;

    process.ptr.defineProperty(cx, 'startupPath', cx.NewJSString(StringToSynUnicode(FStartupPath)).ToJSVal,
      JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY);
    {$IFDEF FPC}
    Cnt := GetEnvironmentVariableCount;
    for I := 1 to Cnt do begin
      EnvStr := GetEnvironmentString(I);
      Parts := EnvStr.Split('=', TStringSplitOptions.ExcludeEmpty);
      if (Length(Parts) = 2) and (Trim(Parts[0]) <> '') then begin
        env.ptr.DefineUCProperty(cx, StringToSynUnicode(Trim(Parts[0])), cx.NewJSString(Trim(Parts[1])).ToJSVal,
          JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY, nil, nil);
      end
    end;
    {$ELSE}
    EnvBlock := GetEnvironmentStrings;
    try
      P := EnvBlock;
      while P^<>#0 do begin
        {$ifdef UNICODE}
        L := StrLenW(P);
        {$else}
        L := StrLen(P);
        {$endif}
        if (L>0) and (P^<>'=') then begin
          pEq := StrScan(P, '=');
          if pEq <> nil then begin
            SetString(strName, p, pEq-P);
            SetString(strVal, pEq+1, {$ifdef UNICODE}StrLenW{$else}StrLen{$endif}(pEq+1));
            env.ptr.DefineUCProperty(cx, Pointer(strName), Length(strName), cx.NewJSString(strVal).ToJSVal,
              JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY, nil, nil);
          end;
        end;
        inc(P,L+1);
      end;
    finally
      FreeEnvironmentStrings(EnvBlock);
    end;
    {$ENDIF}
    process.ptr.defineProperty(cx, 'env', env.ptr.ToJSValue,
      JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY);
    {$IFDEF MSWINDOWS}
    process.ptr.defineProperty(cx, 'platform', cx.NewJSString('win32').ToJSVal);
    {$ELSE}
    process.ptr.defineProperty(cx, 'platform', cx.NewJSString('nix').ToJSVal);
    {$ENDIF}
    {$IFDEF CPU64}
      process.ptr.defineProperty(cx, 'arch', cx.NewJSString('x64').ToJSVal);
    {$ELSE}
      process.ptr.defineProperty(cx, 'arch', cx.NewJSString('x32').ToJSVal);
    {$ENDIF}
    with ExeVersion.Version do
      process.ptr.DefineProperty(cx, 'version',
        cx.NewJSString('v' + IntToString(Major)+'.'+IntToString(Minor) + '.' + IntToString(Release)).ToJSVal);

  finally
    cx.FreeRootedObject(env);
    cx.FreeRootedObject(process);
  end;
end;

procedure TSMEngine.DefineProcessBinding;
var
  process: PJSRootedObject;
  global: PJSRootedObject;
begin
  process := cx.NewRootedObject(cx.NewObject(nil));
  global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
  try
    process.ptr.DefineFunction(cx, 'binding', process_binding, 1);
    global.ptr.DefineProperty(cx, 'process', process.ptr.ToJSValue,
      JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY, nil, nil);
  finally
    cx.FreeRootedObject(global);
    cx.FreeRootedObject(process);
  end;
end;

destructor TSMEngine.Destroy;
var
  unInitProc: TDllModuleUnInitProc;
  process: PJSRootedObject;
  procExitCodeVal: jsval;
begin
  try
    process := cx.NewRootedObject(GlobalObject.ptr.GetPropValue(cx,'process').asObject);
    try
      process.ptr.SetProperty(cx, '_exiting', SimpleVariantToJSval(cx, true));
      if process.ptr.HasProperty(cx, 'emit') then
        CallObjectFunction(process, 'emit', [cx.NewJSString('exit').ToJSVal])
      else
        raise Exception.Create('`process` initialized incorrectly (dont have `emit` method)');
      procExitCodeVal := process.ptr.GetPropValue(cx, 'exitCode');
      if procExitCodeVal.isInteger then
        ExitCode := procExitCodeVal.asInteger;
    finally
      cx.FreeRootedObject(process);
    end;
  except
    on E: Exception do begin
      ExitCode := 1;
    end;
  end;

  if Manager.FRemoteDebuggerThread <> nil then begin
    TSMRemoteDebuggerThread(Manager.FRemoteDebuggerThread).stopDebugCurrentThread(Self);
  end;
  while FDllModulesUnInitProcList.Count > 0 do begin
    unInitProc := FDllModulesUnInitProcList[FDllModulesUnInitProcList.Count - 1];
    FDllModulesUnInitProcList.Delete(FDllModulesUnInitProcList.Count - 1);
    unInitProc();
  end;
  FDllModulesUnInitProcList.Free;
  inherited Destroy;

  if FGlobalTimerLoopFunc <> nil then cx.FreeRootedValue(FGlobalTimerLoopFunc);
  if FGlobalObjectDbg <> nil then cx.FreeRootedObject(FGlobalObjectDbg);
  if FGlobalObject <> nil then cx.FreeRootedObject(FGlobalObject);
  with TSynFPUException.ForLibraryCode do begin
    cx.LeaveCompartment(comp);
    if FThreadID=GetCurrentThreadId then
      cx^.Destroy; // SM 45 expects the context to be released in the same thread
    KillWatchdog;
  {$IFNDEF SM52}
    if FThreadID=GetCurrentThreadId then
      rt^.Destroy;
  {$ENDIF}
  end;
end;

{$IFDEF SM52}
procedure TSMEngine.DoProcessJSError(report: PJSErrorReport);
{$ELSE}
procedure TSMEngine.DoProcessJSError(errMsg: PCChar; report: PJSErrorReport);
{$ENDIF}
var
  exc: jsval;
  exObj: PJSRootedObject;
begin
  if {$IFDEF SM52}(report = nil) or {$ENDIF}(report.flags = JSREPORT_WARNING) then
    Exit;

  FErrorExist := True;
  if report^.filename = nil then
    FLastErrorFileName := '<>' else
    FLastErrorFileName := CurrentAnsiConvert.AnsiBufferToRawUTF8(
      report^.filename,StrLen(pointer(report^.filename)));
  FLastErrorLine := report^.lineno;
  FLastErrorNum := report^.errorNumber;
  {$IFDEF SM52}
  FLastErrorMsg := Utf8ToString(FormatUTF8('%', [report^.message_]));
  {$ELSE}
  if report^.ucmessage=nil then
    FLastErrorMsg := Utf8ToString(FormatUTF8('%', [errMsg])) else
    SetString(FLastErrorMsg, PWideChar(report^.ucmessage), StrLenW(PWideChar(report^.ucmessage)));
  {$ENDIF}
  FLastErrorStackTrace := '';
  if ( cx.GetPendingException(exc)) then begin
    if exc.isObject then begin
      exObj := cx.NewRootedObject(exc.asObject);
      exObj.ptr.GetProperty(cx, 'stack', exc);
      if (not exc.isVoid) and (exc.isString) then
        FLastErrorStackTrace := exc.asJSString.ToSynUnicode(cx);
      cx.FreeRootedObject(exObj);
    end;
  end;

  (*
  // This situation is possible when application are run from the IDE
  // and stop on the breakpoint.
  // When we evaluate some js script with errors(like call JS_Stringify
  // for global object) this function will be called.
  // If breakpoint is set between ClearLastError and CheckJSError we get
  // FErrorExist value is equivalent true, but script have no error
  if DebugHook=0 then try
    CheckJSError(JS_FALSE);
  finally
    FErrorExist := false;
  end;
  *)
end;

procedure TSMEngine.CheckJSError(res: Boolean);
var exc: jsval;
    {$IFDEF SM52}
    excObj: PJSRootedObject;
    {$ENDIF}
    rep: PJSErrorReport;
    R: RawUTF8;
begin
{$IFDEF SM52}
  if JS_IsExceptionPending(fCx) and JS_GetPendingException(cx, exc) then begin
    if exc.isObject then begin
      excObj := cx.NewRootedObject(exc.asObject);
      try
        rep := JS_ErrorFromException(cx, excObj.ptr);
        if (rep <> nil) and (rep.flags = JSREPORT_WARNING) then
          exit;
        // Error inheritance (assert.AssertionError for example)
        // will return an nil as a rep, so we need to examine a exc object
        FErrorExist := True;

        excObj.ptr.GetProperty(cx, 'fileName', exc);
        if (not exc.isVoid) and (exc.isString) then
          FLastErrorFileName := exc.asJSString.ToUTF8(fCx);

        excObj.ptr.GetProperty(cx, 'lineNumber', exc);
        if (not exc.isVoid) then begin
          if (exc.isString) then begin // error line is in format line:char (123:16)
            R := exc.asJSString.ToUTF8(fCx);
            FLastErrorLine := GetInteger(pointer(R));
          end else if (exc.isInteger) then
            FLastErrorLine := exc.asInteger
        end;

        excObj.ptr.GetProperty(cx, 'message', exc);
        if (not exc.isVoid) and (exc.isString) then
          FLastErrorMsg := exc.asJSString.ToString(fCx);

        excObj.ptr.GetProperty(cx, 'stack', exc);
        if (not exc.isVoid) and (exc.isString) then
          FLastErrorStackTrace := exc.asJSString.ToSynUnicode(fCx);

        if (rep <> nil) then
          FLastErrorNum := rep^.errorNumber
        else begin
          excObj.ptr.GetProperty(cx, 'errorNumber', exc);
          if (not exc.isVoid) and (exc.isInteger) then
            FLastErrorNum := exc.asInteger
          else
            FLastErrorNum := 0;
        end;
      finally
        cx.FreeRootedObject(excObj);
      end;
    end else if exc.isString then begin
      FErrorExist := True;
      FLastErrorFileName := '<betterToThrowErrorInsteadOfPlainValue>';
      FLastErrorMsg := exc.asJSString.ToString(fCx);
      FLastErrorStackTrace := '';
      FLastErrorNum := 0;
    end else begin
      FErrorExist := True;
      FLastErrorFileName := '<betterToThrowError>';
      FLastErrorMsg := 'code throw a plain value instead of Error (or error like) object';
      FLastErrorStackTrace := '';
      FLastErrorNum := 0;
    end;
    raise ESMException.CreateWithTrace(FLastErrorFileName, FLastErrorNum, FLastErrorLine, FLastErrorMsg, FLastErrorStackTrace);
  end;
{$ENDIF}
  if (FTimeOutAborted and (FLastErrorMsg <> '')) or FErrorExist then begin
    raise ESMException.CreateWithTrace(FLastErrorFileName, FLastErrorNum, FLastErrorLine, FLastErrorMsg, FLastErrorStackTrace);
  end;
  if not res and not FTimeOutAborted then begin
    raise ESMException.CreateWithTrace(FLastErrorFileName, 0, FLastErrorLine, 'Error compiling script', '');
  end;
end;

procedure TSMEngine.ClearLastError;
begin
  fcx.ClearPendingException;
  FErrorExist := False;
  FTimeOutAborted := False;
  fTimedOut := False;
end;

procedure TSMEngine.GarbageCollect;
begin
{$IFDEF SM52}
  cx.GC;
{$ELSE}
  rt.GC;
{$ENDIF}
end;

procedure TSMEngine.MaybeGarbageCollect;
begin
  cx.MaybeGC;
end;

{ TSMEngineManager }

class procedure TSMEngineManager.RegisterBinding(const Name: RawUTF8;
  const handler: TSMProcessBindingHandler);
begin
  if GlobalSyNodeBindingHandlers = nil then
    GlobalSyNodeBindingHandlers := TRawUTF8ListHashedLocked.Create(false);
  GlobalSyNodeBindingHandlers.AddObject(Name, TObject(@handler));
end;

function TSMEngineManager.GetBinding(const Name: RawUTF8): TSMProcessBindingHandler;
var
  obj: TObject;
  handler: TSMProcessBindingHandler absolute obj;
begin
  obj := GlobalSyNodeBindingHandlers.GetObjectByName(Name);
  result := handler;
end;


function TSMEngineManager.GetEngineExpireTimeOutMinutes: cardinal;
begin
  result := fEngineExpireTimeOutTicks div 60000;
end;

procedure TSMEngineManager.ClearWorkers;
begin
  FWorkersManager.Free;
  FWorkersManager := TJSWorkersManager.Create;
end;

constructor TSMEngineManager.Create(const aCoreModulesPath: RawUTF8; aEngineClass: TSMEngineClass = nil);
begin
  FMaxPerEngineMemory := 32*1024*1024;
  FMaxRecursionDepth := 32;
  FEnginePool := TSynObjectListLocked.Create(true);
  if aEngineClass <> nil then
    FEngineClass := aEngineClass
  else
    FEngineClass := TSMEngine;
  {$ifdef ISDELPHIXE2}
  FRttiCx := TRttiContext.Create();
  {$endif}
  FDllModules := TRawUTF8ListHashedLocked.Create();
  FCoreModulesPath := aCoreModulesPath;
  FWorkersManager := TJSWorkersManager.Create;
end;

procedure TSMEngineManager.SetMaxPerEngineMemory(AMaxMem: Cardinal);
begin
  if aMaxMem<STACK_CHUNK_SIZE*MaxRecursionDepth then
    raise ESMException.CreateFmt(
      'Per engine memory must be >= STACK_CHUNK_SIZE*%d, i.e. %d',
      [MaxRecursionDepth,STACK_CHUNK_SIZE*MaxRecursionDepth]);
  FMaxPerEngineMemory := AMaxMem;
end;

procedure TSMEngineManager.SetEngineExpireTimeOutMinutes(const minutes: cardinal);
begin
  fEngineExpireTimeOutTicks := minutes * 60000;
end;

procedure TSMEngineManager.SetMaxNurseryBytes(AMaxNurseryBytes: Cardinal);
begin
  FMaxNurseryBytes := AMaxNurseryBytes;
end;

function TSMEngineManager.ThreadEngineIndex(ThreadID: TThreadID): Integer;
begin
  if self <> nil then
    for result := 0 to FEnginePool.Count-1 do
      if TSMEngine(FEnginePool.List[result]).fThreadID=ThreadID then
        exit;
  result := -1;
end;

destructor TSMEngineManager.Destroy;
var
  dllModule: PDllModuleRec;
  i: Integer;
begin
  FWorkersManager.Free;
  FWorkersManager := nil;
  stopDebugger;
  if FEnginePool.Count>0 then
    raise ESMException.Create('There are unreleased engines');
  FEnginePool.Free;
  for I := 0 to FDllModules.Count - 1 do begin
    dllModule := PDllModuleRec(FDllModules.Objects[i]);
    Dispose(dllModule);
  end;
  FDllModules.Free;
  inherited;
  {$ifdef ISDELPHIXE2}
  FRttiCx.Free;
  {$endif}
end;

procedure TSMEngineManager.DoOnNewEngine(const Engine: TSMEngine);
begin
  if Assigned(FOnNewEngine) then begin
    Engine.cx.BeginRequest();
    try
      FOnNewEngine(Engine);
    finally
      Engine.cx.EndRequest;
    end;
  end;
end;

function TSMEngineManager.EngineForThread(ThreadID: TThreadID): TSMEngine;
var
  i: integer;
begin
  FEnginePool.Safe.Lock;
  try
    i := ThreadEngineIndex(ThreadID);
    if i < 0 then
      result := nil else
      result := FEnginePool.List[i];
  finally
    FEnginePool.Safe.UnLock;
  end;
end;

function TSMEngineManager.evalDllModule(cx: PJSContext; module: PJSRootedObject;
  const filename: RawUTF8): TDllModuleUnInitProc;
var
  dirname: TFileName;
  exports_V: jsval;
  exports_: PJSRootedObject;
  require_V: jsval;
  require: PJSRootedObject;
  __filename: PWideChar;
  __dirname: PWideChar;

  fHandle: HMODULE;
  ModuleRec: PDllModuleRec;

const
  EXPORTS_NAME: AnsiString = 'exports';
  REQUIRE_NAME: AnsiString = 'require';
  INIT_PROC_NAME = 'InitPlugin';
  UNINIT_PROC_NAME = 'UnInitPlugin';

begin
  Lock;
  try
    cx.BeginRequest;
    try
      dirname := ExtractFilePath(UTF8ToString(filename)) ;
      ModuleRec := PDllModuleRec(FDllModules.GetObjectByName(filename));
      if ModuleRec = nil then begin
        fHandle := {$IFDEF FPC}dynlibs.{$ENDIF}SafeLoadLibrary(UTF8ToString(filename));
        if fHandle=0 then
          raise ESMException.CreateFmt('Unable to load %s (%s)',
            [filename, {$ifdef FPC}GetLoadErrorStr{$else}SysErrorMessage(GetLastError){$endif}]);
        new(ModuleRec);
        ModuleRec.init := GetProcAddress(fHandle, INIT_PROC_NAME);
        if not Assigned(ModuleRec.init) then begin
          FreeLibrary(fHandle);
          Dispose(ModuleRec);
          raise ESMException.CreateFmt('Invalid %s: missing %s',[filename, INIT_PROC_NAME]);
        end;
        if Assigned(OnDllModuleLoaded) then
          OnDllModuleLoaded(fHandle);

        ModuleRec.unInit := GetProcAddress(fHandle, UNINIT_PROC_NAME);
        FDllModules.AddObject(filename, TObject(ModuleRec));
      end;

      __filename := Pointer(filename);
      __dirname := Pointer(dirname);

      if module.ptr.GetProperty(cx, pointer(EXPORTS_NAME), exports_V) then
        exports_ := cx.NewRootedObject(exports_V.asObject)
      else
        exports_ := cx.NewRootedObject(nil);
      try

        if module.ptr.GetProperty(cx, pointer(REQUIRE_NAME), require_V) then
          require :=  cx.NewRootedObject(require_V.asObject)
        else
          require := cx.NewRootedObject(nil);
        try
          if not ModuleRec.init(cx, exports_, require, module, __filename, __dirname) then begin
            try
              RaiseLastOSError;
            except
              on E: Exception do begin
                E.Message := format('require %s failed:',[ ExtractFileName(UTF8ToString(filename))])+E.Message;
                raise;
              end;
            end;
          end;
        finally
          cx.FreeRootedObject(require);
        end;
      finally
        cx.FreeRootedObject(exports_);
      end;
        //TODO: Store all evaled libs and call UnInit on destroy engine
    finally
      cx.EndRequest;
    end;
  finally
    UnLock;
  end;

  Result := ModuleRec.unInit;
end;

function TSMEngineManager.getPauseDebuggerOnFirstStep: boolean;
begin
  if FRemoteDebuggerThread<> nil then begin
    result := TSMRemoteDebuggerThread(FRemoteDebuggerThread).NeedPauseOnFirstStep;
  end else
    result := false;
end;

function TSMEngineManager.ThreadSafeEngine(pThreadData: pointer): TSMEngine;
var i: integer;
    ThreadID: TThreadID;
begin
  FEnginePool.Safe.Lock;
  try
    ThreadID := GetCurrentThreadId;
    i := ThreadEngineIndex(ThreadID); // inlined CurrentThreadEngine
    if i<0 then
      result := nil else begin
      result := FEnginePool.List[i];
      if (pThreadData) <> nil then
        result.SetThreadData(pThreadData);
    end;
    if result<>nil then begin
      // memorize a thread data for possible engine recreation
      if pThreadData = nil then
        pThreadData := result.ThreadData;
      if result.EngineContentVersion=Self.ContentVersion then begin
        if Result.NeverExpire or (FEngineExpireTimeOutTicks = 0) or (GetTickCount64 - Result.fCreatedAtTick < FEngineExpireTimeOutTicks ) then
        // return existing Engine corresponding to the current thread
        exit else begin
          // content expired -> force recreate thread Engine
          {$ifdef SM_DEBUG}
          SynSMLog.Add.Log(sllDebug,
            'Drop SpiderMonkey Engine for thread % - timeout expired', ThreadID);
          {$endif}
          ReleaseEngineForThread(ThreadID);
        end;
      end else begin
        // content version changed -> force recreate thread Engine
        {$ifdef SM_DEBUG}
        SynSMLog.Add.Log(sllDebug,
          'Drop SpiderMonkey Engine for thread % - modification found', ThreadID);
        {$endif}
        ReleaseEngineForThread(ThreadID);
      end;
    end;
    // here result=nil or to be ignored (just dropped)
    {$ifdef SM_DEBUG}
    SynSMLog.Add.Log(sllDebug, 'Create new JavaScript Engine for thread %', ThreadID);
    {$endif}

    Result := FEngineClass.Create(Self);
    if grandParent = nil then
      grandParent := Result;

    if (pThreadData <> nil) then
      Result.SetThreadData(pThreadData);

    if WorkersManager.curThreadIsWorker then
      Result.fnameForDebug := WorkersManager.getCurrentWorkerThreadName
    else if Assigned(OnGetName) then
      Result.fnameForDebug := OnGetName(Result);

    if Assigned(OnGetWebAppRootPath) then
      Result.fWebAppRootDir := OnGetWebAppRootPath(Result)
    else
      Result.fWebAppRootDir := StringToUTF8(ExeVersion.ProgramFilePath);

    result.fThreadID := ThreadID;
    FEnginePool.Add(result);
  finally
    FEnginePool.Safe.UnLock;
  end;

  if FRemoteDebuggerThread <> nil then
    TSMRemoteDebuggerThread(FRemoteDebuggerThread).startDebugCurrentThread(result);
  if WorkersManager.curThreadIsWorker then
    Result.doInteruptInOwnThread := WorkersManager.DoInteruptInOwnThreadhandlerForCurThread;

  DoOnNewEngine(Result);
end;

procedure TSMEngineManager.Lock;
begin
  FEnginePool.Safe.Lock;
end;

procedure TSMEngineManager.UnLock;
begin
  FEnginePool.Safe.UnLock;
end;

procedure TSMEngineManager.ReleaseEngineForThread(aThreadID: TThreadID);
var
  i: integer;
begin
  FEnginePool.Safe.Lock;
  try
    i := ThreadEngineIndex(aThreadID);
    if i>=0 then begin
      (FEnginePool[i] as TSMEngine).GarbageCollect;
      FEnginePool.Delete(i);
    end;
  finally
    FEnginePool.Safe.UnLock;
  end;
end;

procedure TSMEngineManager.ReleaseCurrentThreadEngine;
begin
  ReleaseEngineForThread(GetCurrentThreadId);
end;

function TSMEngineManager.CurrentThreadEngine: TSMEngine;
var
  i: integer;
begin
  FEnginePool.Safe.Lock;
  try
    i := ThreadEngineIndex(GetCurrentThreadId);
    if i < 0 then
      result := nil else
      result := FEnginePool.List[i];
  finally
    FEnginePool.Safe.UnLock;
  end;
end;

procedure TSMEngineManager.debuggerLog(const Text: RawUTF8);
begin
  if FRemoteDebuggerThread<> nil then begin
    TSMRemoteDebuggerThread(FRemoteDebuggerThread).doLog(Text);
  end;
end;

procedure TSMEngineManager.setPauseDebuggerOnFirstStep(const Value: boolean);
begin
  if FRemoteDebuggerThread<> nil then begin
    TSMRemoteDebuggerThread(FRemoteDebuggerThread).NeedPauseOnFirstStep := Value;
  end
end;

procedure TSMEngineManager.startDebugger(const port: SockString = '6000');
begin
  FRemoteDebuggerThread := TSMRemoteDebuggerThread.Create(self, port);
  inc(FContentVersion);
end;

procedure TSMEngineManager.stopDebugger;
begin
  if FRemoteDebuggerThread <> nil then begin
    TSMRemoteDebuggerThread(FRemoteDebuggerThread).SetTerminated;
    FRemoteDebuggerThread := nil;
    end;
  end;

function StringReplaceChars(const Source: String; OldChar, NewChar: Char): String;
var i,j,n: integer;
begin
  if (OldChar<>NewChar) and (Source<>'') then begin
    n := length(Source);
    for i := 0 to n-1 do
      if PChar(pointer(Source))[i]=OldChar then begin
        SetString(result,PChar(pointer(Source)),n);
        for j := i to n-1 do
          if PChar(pointer(result))[j]=OldChar then
            PChar(pointer(result))[j] := NewChar;
        exit;
      end;
  end;
  result := Source;
end;

function RelToAbs(const ABaseDir, AFileName: TFileName; ACheckResultInsideBase: boolean = false): TFileName;
var
{$IFNDEF FPC}
  aBase, aTail: PChar;
  localBase, localTail: TFileName;
{$ELSE}
  aBase: TFileName;
{$ENDIF}
begin
{$IFNDEF FPC}
  if AFileName <> '' then begin
    if PathIsRelative(PChar(AFileName)) then begin
      localTail := AFileName;
      localBase := ABaseDir;
    end else begin
      localTail := '';
      localBase := AFileName;
    end;
  end else begin
    localTail := '';
    localBase := ABaseDir;
  end;
  localBase := StringReplaceChars(localBase, '/', '\');;
  localTail := StringReplaceChars(localTail, '/', '\');
  aBase := PChar(LocalBase);
  aTail := PChar(localTail);

  SetLength(Result, MAX_PATH);
  // PathCombine do not understand '/', so we raplace '/' -> '\' above
  if PathCombine(@Result[1], aBase, aTail) = nil then
    Result := ''
  else
    SetLength(Result, {$ifdef UNICODE}StrLenW{$else}StrLen{$endif}(@Result[1]));

  if ACheckResultInsideBase and
     ((length(Result) < length(ABaseDir)) or (length(ABaseDir)=0) or
      (StrLIComp(PChar(@Result[1]), @localBase[1], length(localBase)) <> 0)) then
    Result := ''
{$ELSE}
{
  aBase := GetForcedPathDelims(ABaseDir);
  if AFileName <> '' then begin
    aTail := GetForcedPathDelims(AFileName);
    if FilenameIsAbsolute(aTail) then begin
      localTail := '';
      localBase := aBase;
    end else begin
      localTail := aTail;
      localBase := aBase;
    end;
  end else begin
    localTail := '';
    localBase := aBase;
  end;
  Result := CreateAbsolutePath(localTail, localBase);
}
  aBase := ExpandFileName(ABaseDir);
  Result := ExpandFileNameUTF8(AFileName, aBase);

  if ACheckResultInsideBase and
     ((Length(Result) < Length(aBase)) or (Length(aBase)=0) or
      (StrCompL(PUtf8Char(Result), PUtf8Char(aBase), length(aBase), 0) <> 0)) then
      //{ not Result.StartsWith(aBase, false))} not compiled in fpc 3.1.1
    Result := ''
{$ENDIF}
end;

function TSMEngine.DoProcessOperationCallback: Boolean;
begin
  Result := not fTimedOut;
end;

// Remove #13 characters from script(change it to #32)
// Spidermonkey debugger crashes when `...`(new ES6 strings) contains #13#10
procedure remChar13FromScript(const Script: SynUnicode); {$IFDEF HASINLINE}inline;{$ENDIF}
var
    c: PWideChar;
    i: Integer;
begin
  c := pointer(script);
  for I := 1 to Length(script) do begin
    if (c^ = #13) then
        c^ := ' ';
    Inc(c);
  end;
end;

// get a pointer to a file embadded as a UNICODE resource and it length in chars
function getResCharsAndLength(const ResName: string; out pRes: pointer;
  out resLength: LongWord): boolean;
var HResInfo: THandle;
    HGlobal: THandle;
    Instance: THandle;
begin
  Instance := HInstance;
  HResInfo := FindResource(Instance,PChar(ResName),PChar(10)); //RC_DATA
  if HResInfo=0 then
    exit(false);
  HGlobal := LoadResource(Instance,HResInfo);
  if HGlobal=0 then
    exit(false);
  pRes := LockResource(HGlobal);
  resLength := SizeofResource(Instance,HResInfo) div 2;
  Result := resLength > 0;
end;

procedure TSMEngine.EvaluateRes(const ResName: string; out result: jsval);
var r: Boolean;
    opts: PJSCompileOptions;
    isFirst: Boolean;
    pScript: pointer;
    scriptLength: LongWord;
    rval: jsval;
begin
  with TSynFPUException.ForLibraryCode do begin
    ClearLastError;
    ScheduleWatchdog(fTimeoutValue);
    isFirst := not cx.IsRunning;
    opts := cx.NewCompileOptions;
    opts.filename := Pointer(ResName);

    if not getResCharsAndLength(ResName, pScript, scriptLength) then
      raise ESMException.CreateUTF8('Resource "%" not found', [ResName]);

    r := cx.EvaluateUCScript(opts, pScript, scriptLength, result);
    cx.FreeCompileOptions(opts);
    if r and isFirst and GlobalObject.ptr.HasProperty(cx, '_timerLoop') then
      r := GlobalObject.ptr.CallFunctionName(cx, '_timerLoop', 0, nil, rval);
    if not r then
      r := false;
    ScheduleWatchdog(-1);
    CheckJSError(r);
  end;
end;

procedure TSMEngine.Evaluate(const script: SynUnicode;
  const scriptName: RawUTF8; lineNo: Cardinal; out result: jsval);
var r: Boolean;
    opts: PJSCompileOptions;
    isFirst: Boolean;
    rval: jsval;
begin
  with TSynFPUException.ForLibraryCode do begin
    ClearLastError;
    ScheduleWatchdog(fTimeoutValue);
    isFirst := not cx.IsRunning;
    opts := cx.NewCompileOptions;
    opts.filename := Pointer(scriptName);

    remChar13FromScript(script);
    r := cx.EvaluateUCScript(
        opts, pointer(script), length(script), result);
    cx.FreeCompileOptions(opts);
    if r and isFirst and GlobalObject.ptr.HasProperty(cx, '_timerLoop') then
      r := GlobalObject.ptr.CallFunctionName(cx, '_timerLoop', 0, nil, rval);
    if not r then
      r := false;
    ScheduleWatchdog(-1);
    CheckJSError(r);
  end;
end;

procedure TSMEngine.Evaluate(const script: SynUnicode; const scriptName: RawUTF8; lineNo: Cardinal);
var
  jsvalue: jsval;
begin
  Evaluate(script, scriptName, lineNo, jsvalue);
end;

function TSMEngine.EvaluateModule(const scriptName: RawUTF8): jsval;
var
  global: PJSRootedObject;
  reflect: jsval;
  moduleLoader: PJSRootedObject;
begin
  global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
  reflect := global.ptr.GetPropValue(cx, 'Reflect');
  moduleLoader := cx.NewRootedObject(reflect.asObject.GetPropValue(cx, 'Loader').asObject);
  try
    result := CallObjectFunction(moduleLoader, 'import', [cx.NewJSString(scriptName).ToJSVal])
  finally
    cx.FreeRootedObject(moduleLoader);
    cx.FreeRootedObject(global);
  end;
end;

function TSMEngine.CallObjectFunctionVal(obj: PJSRootedObject; const funcVal: PJSRootedValue; const args: array of jsval): jsval;
var r: Boolean;
    isFirst: Boolean;
    rval: jsval;
    global: PJSRootedObject;
begin
  with TSynFPUException.ForLibraryCode do begin
    ClearLastError;
    ScheduleWatchdog(fTimeoutValue);
    isFirst := not cx.IsRunning;
    r := obj.ptr.CallFunctionValue(cx, funcVal.ptr, high(args) + 1, @args[0], Result);
    if r and isFirst then begin
      global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
      try
        if FGlobalTimerLoopFunc <> nil then
          r := global.ptr.CallFunctionValue(cx, FGlobalTimerLoopFunc.ptr, 0, nil, rval);
      finally
        cx.FreeRootedObject(global);
      end;
    end;
    ScheduleWatchdog(-1);
    CheckJSError(r);
  end;
end;

function TSMEngine.CallObjectFunction(obj: PJSRootedObject; funcName: PCChar;
  const args: array of jsval): jsval;
var r: Boolean;
    isFirst: Boolean;
    rval: jsval;
    global: PJSRootedObject;
begin
  with TSynFPUException.ForLibraryCode do begin
    ClearLastError;
    ScheduleWatchdog(fTimeoutValue);
    isFirst := not cx.IsRunning;
    r := obj.ptr.CallFunctionName(cx, funcName, high(args) + 1, @args[0], Result);
    if r and isFirst then begin
      global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
      try
        if FGlobalTimerLoopFunc <> nil then
          r := global.ptr.CallFunctionValue(cx, FGlobalTimerLoopFunc.ptr, 0, nil, rval);
      finally
        cx.FreeRootedObject(global);
      end;
    end;
    ScheduleWatchdog(-1);
    CheckJSError(r);
  end;
end;

procedure TSMEngine.CancelExecution(AWithException: boolean = true);
begin
  fTimedOut := True;
  FTimeOutAborted := True;
  if AWithException then begin
    FErrorExist := True;
    FLastErrorFileName := '<>';
    FLastErrorLine := 0;
    FLastErrorNum := 0;
    FLastErrorMsg := LONG_SCRIPT_EXECUTION;
  end;
{$IFDEF SM52}
  cx.RequestInterruptCallback;
{$ELSE}
  rt.RequestInterruptCallback;
{$ENDIF}
end;

function TSMEngine.InitWatchdog: boolean;
begin
  Assert(not Assigned(fWatchdogThread));
  fWatchdogLock := PR_NewLock;
  if Assigned(fWatchdogLock) then begin
    fWatchdogWakeup := PR_NewCondVar(fWatchdogLock);
    if Assigned(fWatchdogWakeup) then begin
      fSleepWakeup := PR_NewCondVar(fWatchdogLock);
      if Assigned(fSleepWakeup) then begin
        result := True;
        exit;
      end;
      PR_DestroyCondVar(fWatchdogWakeup);
    end;
  end;
  result := False;
end;

procedure TSMEngine.KillWatchdog;
var thread: PRThread;
begin
  PR_Lock(fWatchdogLock);
  thread := fWatchdogThread;
  if Assigned(thread) then begin
    // The watchdog thread is running, tell it to terminate waking it up
    // if necessary.
    fWatchdogThread := nil;
    PR_NotifyCondVar(fWatchdogWakeup);
  end;
  PR_Unlock(fWatchdogLock);
  if Assigned(thread) then
    PR_JoinThread(thread);
  PR_DestroyCondVar(fSleepWakeup);
  PR_DestroyCondVar(fWatchdogWakeup);
  PR_DestroyLock(fWatchdogLock);
end;

function IsBefore( t1, t2: int64): Boolean;
begin
  Result := int32(t1 - t2) < 0;
end;

procedure WatchdogMain(arg: pointer); cdecl;
var eng: TSMEngine;
{$IFDEF SM52}
    cx: PJSContext;
{$ELSE}
    rt: PJSRuntime;
{$ENDIF}
    now_: int64;
    sleepDuration: PRIntervalTime;
    status: PRStatus;
begin
  PR_SetCurrentThreadName('JS Watchdog');
  eng := TSMEngine(arg);
{$IFDEF SM52}
  cx := eng.cx;
{$ELSE}
  rt := eng.rt;
{$ENDIF}
  PR_Lock(eng.fWatchdogLock);
  while Assigned(eng.fWatchdogThread) do begin
{$IFDEF SM52}
    now_ := cx.NowMs;
{$ELSE}
    now_ := rt.NowMs;
{$ENDIF}
    if (eng.fWatchdogHasTimeout and not IsBefore(now_, eng.fWatchdogTimeout)) then begin
      // The timeout has just expired. Trigger the operation callback outside the lock
      eng.fWatchdogHasTimeout := false;
      PR_Unlock(eng.fWatchdogLock);
      eng.CancelExecution;
      PR_Lock(eng.fWatchdogLock);
      // Wake up any threads doing sleep
      PR_NotifyAllCondVar(eng.fSleepWakeup);
    end else begin
      if (eng.fWatchdogHasTimeout) then begin
        // Time hasn't expired yet. Simulate an operation callback
        // which doesn't abort execution.
{$IFDEF SM52}
        cx.RequestInterruptCallback;
{$ELSE}
        rt.RequestInterruptCallback;
{$ENDIF}
      end;
      sleepDuration := PR_INTERVAL_NO_TIMEOUT;
      if (eng.fWatchdogHasTimeout) then
        sleepDuration := PR_TicksPerSecond() div 10;
      status := PR_WaitCondVar(eng.fWatchdogWakeup, sleepDuration);
      Assert(status = PR_SUCCESS);
    end
  end;
  PR_Unlock(eng.fWatchdogLock);
end;

function TSMEngine.ScheduleWatchdog(t: integer): Boolean;
var interval: Int64;
    timeout: Int64;
begin
  if (t <= 0) then begin
    PR_Lock(fWatchdogLock);
    fWatchdogHasTimeout := false;
    PR_Unlock(fWatchdogLock);
    result := true;
    exit;
  end;
  interval := int64(t * PRMJ_USEC_PER_SEC);
{$IFDEF SM52}
  timeout := cx.NowMs + interval;
{$ELSE}
  timeout := rt.NowMs + interval;
{$ENDIF}
  PR_Lock(fWatchdogLock);
  if not Assigned(fWatchdogThread) then begin
    Assert(not fWatchdogHasTimeout);
    fWatchdogThread := PR_CreateThread(PR_USER_THREAD,
                                       @WatchdogMain,
                                       Self,
                                       PR_PRIORITY_NORMAL,
                                       PR_LOCAL_THREAD,
                                       PR_JOINABLE_THREAD,
                                       0);
    if not Assigned(fWatchdogThread) then begin
      PR_Unlock(fWatchdogLock);
      Result := false;
      Exit;
   end
  end else if (not fWatchdogHasTimeout or IsBefore(timeout, fWatchdogTimeout)) then
    PR_NotifyCondVar(fWatchdogWakeup);

  fWatchdogHasTimeout := true;
  fWatchdogTimeout := timeout;
  PR_Unlock(fWatchdogLock);
  Result := true;
end;

procedure TSMEngine.SetPrivateDataForDebugger(const Value: Pointer);
begin
  FPrivateDataForDebugger := Value;
end;

function TSMEngine.defineClass(AForClass: TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject=nil): TSMCustomProtoObject;
begin
  if aParent = nil then
    aParent := GlobalObject;
  result := SyNodeProto.defineClass(cx, AForClass, AProto, aParent);
end;

procedure TSMEngine.defineClasses(const AClasses: array of TClass; AProto: TSMCustomProtoObjectClass; aParent: PJSRootedObject=nil);
var i: Integer;
begin
  for i := 0 to high(AClasses) do
    defineClass(AClasses[i], AProto);
end;

procedure TSMEngine.SetThreadData(pThreadData: pointer);
begin
  FThreadData := pThreadData;
end;

procedure TSMEngine.SetTimeoutValue(const Value: Integer);
begin
  if fTimeoutValue = Value then
    exit;
  fTimeoutValue := Value;
  ScheduleWatchdog(fTimeoutValue);
end;

// Bindings
// synode
function synode_parseModule(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  Script: SynUnicode;
  FileName: RawUTF8;
  global: PJSRootedObject;
  options: PJSCompileOptions;
  res: PJSRootedObject;
const
  USAGE = 'usage parseModule(source, [path]: String): ModuleObject';
begin
  try
    in_argv := vp.argv;
    if (argc=0) or not in_argv[0].IsString or ((argc > 1) and not in_argv[1].IsString) then
      raise ESMException.Create(USAGE);

    Script := in_argv[0].asJSString.ToSynUnicode(cx);

    FileName := '';
    if argc > 1 then
      FileName := in_argv[1].asJSString.ToUTF8(cx);
    if FileName = '' then
      FileName := 'no file';

    global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
    try
      options := cx.NewCompileOptions;
      options.filename := Pointer(FileName);
      res := cx.NewRootedObject(cx.CompileModule(global.ptr, options, Pointer(Script), Length(Script)));
      result := res <> nil;
      if result then
        vp.rval := res.ptr.ToJSValue;
      cx.FreeRootedObject(res);
      cx.FreeCompileOptions(options);
    finally
      cx.FreeRootedObject(global);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

{$IFDEF CORE_MODULES_IN_RES}
/// Parse and evaluate module stored in resources
function synode_parseModuleRes(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  ResName: string;
  FileName: RawUTF8;
  global: PJSRootedObject;
  options: PJSCompileOptions;
  res: PJSRootedObject;
  pScript: pointer;
  scriptLength: LongWord;
const
  USAGE = 'usage parseModuleRes(resourceName, [path]: String): ModuleObject';
begin
  try
    in_argv := vp.argv;
    if (argc=0) or not in_argv[0].IsString or ((argc > 1) and not in_argv[1].IsString) then
      raise ESMException.Create(USAGE);

    ResName := in_argv[0].asJSString.ToString(cx);
    if not getResCharsAndLength(ResName, pScript, scriptLength) then
      raise ESMException.CreateUTF8('Resource "%" not found', [ResName]);

    FileName := '';
    if argc > 1 then
      FileName := in_argv[1].asJSString.ToUTF8(cx);
    if FileName = '' then
      FileName := ResName;

    global := cx.NewRootedObject(cx.CurrentGlobalOrNull);
    try
      options := cx.NewCompileOptions;
      options.filename := Pointer(FileName);
      res := cx.NewRootedObject(cx.CompileModule(global.ptr, options, pScript, scriptLength));
      result := res <> nil;
      if result then
        vp.rval := res.ptr.ToJSValue;
      cx.FreeRootedObject(res);
      cx.FreeCompileOptions(options);
    finally
      cx.FreeRootedObject(global);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;
{$endif}

function synode_setModuleResolveHook(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  ro: PJSRootedObject;
begin
  try
    in_argv := vp.argv;

    result := (argc > 0) and (in_argv[0].isObject);
    if Result then begin
      ro := cx.NewRootedObject(in_argv[0].asObject);
      try
        Result := ro.ptr.isFunction(cx);
        if Result then
          cx.SetModuleResolveHook(ro.ptr);
      finally
        cx.FreeRootedObject(ro);
      end;
    end;

  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function synode_runInThisContext(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage: runInThisContext(script, [fileName]: string)';
var
  in_argv: PjsvalVector;
  res: jsval;
  Script: SynUnicode;
  FileName: RawUTF8;
  opts: PJSCompileOptions;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString or ((argc >1) and not in_argv[1].isString) then
      raise ESMException.Create(USAGE);
    Script := in_argv[0].asJSString.ToSynUnicode(cx);
    FileName := '';
    if argc > 1 then
      FileName := in_argv[1].asJSString.ToUTF8(cx);
    if FileName = '' then
      FileName := 'no file';
    cx.BeginRequest;
    try
      opts := cx.NewCompileOptions;
      opts.filename := Pointer(FileName);

      remChar13FromScript(Script);
      Result := cx.EvaluateUCScript(opts, Pointer(Script), Length(Script), res);
      cx.FreeCompileOptions(opts);
      if Result then
        vp.rval := res;
    finally
      cx.EndRequest;
    end;
    vp.rval := res;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

{$IFDEF CORE_MODULES_IN_RES}
/// Compile and execute a script from named resource
function synode_runInThisContextRes(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage: runResInThisContextRes(resourceName, [fileName]: string)';
var
  in_argv: PjsvalVector;
  res: jsval;
  ResName: string;
  pScript: pointer;
  scriptLength: LongWord;
  FileName: RawUTF8;
  opts: PJSCompileOptions;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString or ((argc >1) and not in_argv[1].isString) then
      raise ESMException.Create(USAGE);
    ResName := in_argv[0].asJSString.ToString(cx);
    if not getResCharsAndLength(ResName, pScript, scriptLength) then
      raise ESMException.CreateUTF8('Resource "%" not found', [ResName]);

    FileName := '';
    if argc > 1 then
      FileName := in_argv[1].asJSString.ToUTF8(cx);
    if FileName = '' then
      FileName := ResName;
    cx.BeginRequest;
    try
      opts := cx.NewCompileOptions;
      opts.filename := Pointer(FileName);
      Result := cx.EvaluateUCScript(opts, pScript, scriptLength, res);
      cx.FreeCompileOptions(opts);
      if Result then
        vp.rval := res;
    finally
      cx.EndRequest;
    end;
    vp.rval := res;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;
{$ENDIF}

function synode_loadDll(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: loadDll(module:Object , filename: String)';
var
  Eng: TSMEngine;
  in_argv: PjsvalVector;
  res: jsval;
  filename: RawUTF8;
  module: PJSRootedObject;
  uninitProc: TDllModuleUnInitProc;
begin
  Eng := TObject(cx.PrivateData) as TSMEngine;
  try
    in_argv := vp.argv;
    if (argc < 2) or not (in_argv[0].isObject) or not (in_argv[1].isString) then
      raise ESMException.Create(USAGE);
    filename := in_argv[1].asJSString.ToUTF8(cx);
    module := cx.NewRootedObject(in_argv[0].asObject);
    try
      uninitProc := Eng.FManager.evalDllModule(cx, module, filename);
      if Assigned(uninitProc) then
        Eng.FDllModulesUnInitProcList.Add(@uninitProc);
    finally
      cx.FreeRootedObject(module);
    end;

    res := JSVAL_NULL;
    vp.rval := res;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;

end;

function SyNodeBinding_modules(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
  {$IFDEF CORE_MODULES_IN_RES}
  val: jsval;
  {$endif}
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'parseModule', synode_parseModule, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'setModuleResolveHook', synode_setModuleResolveHook, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'runInThisContext', synode_runInThisContext, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'loadDll', synode_loadDll, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    {$IFDEF CORE_MODULES_IN_RES}
    val.asBoolean := true;
    obj.ptr.DefineProperty(cx, '_coreModulesInRes', val, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'parseModuleRes', synode_parseModuleRes, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'runInThisContextRes', synode_runInThisContextRes, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    {$ENDIF}
    obj.ptr.DefineProperty(cx, 'coreModulesPath', cx.NewJSString(Engine.Manager.CoreModulesPath).ToJSVal, JSPROP_READONLY or JSPROP_PERMANENT);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

/// sleep X ms
function synode_sleep(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  interval: int64;
  engine: TSMEngine;
const
  USAGE = 'usage: sleep(module: Number)';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isNumber then
      raise ESMException.Create(USAGE);
    engine := TSMEngine(cx.PrivateData);
    if in_argv[0].isInteger then
      interval := in_argv[0].asInteger
    else
      interval := Trunc(in_argv[0].asDouble);
    while not engine.TimeOutAborted and (interval > 0) do
    begin
      Sleep(100);
      interval := interval - 100;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function SyNodeBinding_syNode(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'sleep', synode_sleep, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('modules', SyNodeBinding_modules);
  TSMEngineManager.RegisterBinding('syNode', SyNodeBinding_syNode);
finalization
  FreeAndNil(GlobalSyNodeBindingHandlers);

end.

