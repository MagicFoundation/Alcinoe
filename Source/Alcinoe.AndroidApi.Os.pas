unit Alcinoe.AndroidApi.Os;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes;

type

  {******************}
  JStatFs = interface;
  JAsyncTask_Status = interface;
  JAsyncTask = interface;
  JDebug = interface;


  {************************************}
  JStatFsClass = interface(JObjectClass)
    ['{E5587205-C324-4FAF-A101-E31BCD83BD4D}']
    {class} function init(path: JString): JStatFs; cdecl; // public StatFs(String path)
  end;
  [JavaSignature('android/os/StatFs')]
  JStatFs = interface(JObject)
    ['{121A2CDF-6B8A-4C8F-BE9A-B2DEDF861CFB}']
    procedure restat(path: JString); cdecl;
    function getBlockSize: integer; cdecl;
    function getBlockSizeLong: int64; cdecl;
    function getBlockCount: integer; cdecl;
    function getBlockCountLong: int64; cdecl;
    function getFreeBlocks: integer; cdecl;
    function getFreeBlocksLong: int64; cdecl;
    function getFreeBytes: int64; cdecl;
    function getAvailableBlocks: integer; cdecl;
    function getAvailableBlocksLong: int64; cdecl;
    function getAvailableBytes: int64; cdecl;
    function getTotalBytes: int64; cdecl;
  end;
  TJStatFs = class(TJavaGenericImport<JStatFsClass, JStatFs>) end;

  {********************************************}
  JAsyncTask_StatusClass = interface(JEnumClass)
    ['{16452E24-44D5-4E84-990E-3C1916FB372B}']
    {class} function _GetFINISHED: JAsyncTask_Status; cdecl;
    {class} function _GetPENDING: JAsyncTask_Status; cdecl;
    {class} function _GetRUNNING: JAsyncTask_Status; cdecl;
    {class} function valueOf(name: JString): JAsyncTask_Status; cdecl;
    {class} function values: TJavaObjectArray<JAsyncTask_Status>; cdecl;
    {class} property FINISHED: JAsyncTask_Status read _GetFINISHED;
    {class} property PENDING: JAsyncTask_Status read _GetPENDING;
    {class} property RUNNING: JAsyncTask_Status read _GetRUNNING;
  end;
  [JavaSignature('android/os/AsyncTask$Status')]
  JAsyncTask_Status = interface(JEnum)
    ['{96B0BCE7-1312-49B9-9F33-43541680B0E7}']
  end;
  TJAsyncTask_Status = class(TJavaGenericImport<JAsyncTask_StatusClass, JAsyncTask_Status>) end;

  {***************************************}
  JAsyncTaskClass = interface(JObjectClass)
    ['{73C141D6-F8D7-4FE4-BFA3-3441B6367189}']
    {class} function _GetSERIAL_EXECUTOR: JExecutor; cdecl;
    {class} function _GetTHREAD_POOL_EXECUTOR: JExecutor; cdecl;
    {class} function init: JAsyncTask; cdecl;
    {class} property SERIAL_EXECUTOR: JExecutor read _GetSERIAL_EXECUTOR;
    {class} property THREAD_POOL_EXECUTOR: JExecutor read _GetTHREAD_POOL_EXECUTOR;
  end;
  [JavaSignature('android/os/AsyncTask')]
  JAsyncTask = interface(JObject)
    ['{8BC49850-F199-4620-BCFF-ACDA1D69417A}']
    function cancel(mayInterruptIfRunning: Boolean): Boolean; cdecl;
    procedure execute(runnable: JRunnable); cdecl; overload;
    function getStatus: JAsyncTask_Status; cdecl;
    function isCancelled: Boolean; cdecl;
    function get: JObject; cdecl; overload;
    function get(timeout: Int64; &unit: JTimeUnit): JObject; cdecl; overload;
  end;
  TJAsyncTask = class(TJavaGenericImport<JAsyncTaskClass, JAsyncTask>) end;

  {***********************************}
  JDebugClass = interface(JObjectClass)
    ['{1DF44C41-5188-4659-9FCC-DD525AF42B89}']
    //{class} function _GetSHOW_CLASSLOADER: Integer; cdecl;
    //{class} function _GetSHOW_FULL_DETAIL: Integer; cdecl;
    //{class} function _GetSHOW_INITIALIZED: Integer; cdecl;
    //{class} function _GetTRACE_COUNT_ALLOCS: Integer; cdecl;
    //{class} procedure changeDebugPort(port: Integer); cdecl;
    //{class} procedure dumpHprofData(fileName: JString); cdecl;
    //{class} function dumpService(name: JString; fd: JFileDescriptor; args: TJavaObjectArray<JString>): Boolean; cdecl;
    //{class} procedure enableEmulatorTraceOutput; cdecl;
    //{class} function getBinderDeathObjectCount: Integer; cdecl;
    //{class} function getBinderLocalObjectCount: Integer; cdecl;
    //{class} function getBinderProxyObjectCount: Integer; cdecl;
    //{class} function getBinderReceivedTransactions: Integer; cdecl;
    //{class} function getBinderSentTransactions: Integer; cdecl;
    //{class} function getGlobalAllocCount: Integer; cdecl;
    //{class} function getGlobalAllocSize: Integer; cdecl;
    //{class} function getGlobalClassInitCount: Integer; cdecl;
    //{class} function getGlobalClassInitTime: Integer; cdecl;
    //{class} function getGlobalExternalAllocCount: Integer; cdecl;
    //{class} function getGlobalExternalAllocSize: Integer; cdecl;
    //{class} function getGlobalExternalFreedCount: Integer; cdecl;
    //{class} function getGlobalExternalFreedSize: Integer; cdecl;
    //{class} function getGlobalFreedCount: Integer; cdecl;
    //{class} function getGlobalFreedSize: Integer; cdecl;
    //{class} function getGlobalGcInvocationCount: Integer; cdecl;
    //{class} function getLoadedClassCount: Integer; cdecl;
    //{class} procedure getMemoryInfo(memoryInfo: JDebug_MemoryInfo); cdecl;
    {class} function getNativeHeapAllocatedSize: Int64; cdecl;
    {class} function getNativeHeapFreeSize: Int64; cdecl;
    {class} function getNativeHeapSize: Int64; cdecl;
    //{class} function getPss: Int64; cdecl;
    //{class} function getRuntimeStat(statName: JString): JString; cdecl;
    //{class} function getRuntimeStats: JMap; cdecl;
    //{class} function getThreadAllocCount: Integer; cdecl;
    //{class} function getThreadAllocSize: Integer; cdecl;
    //{class} function getThreadExternalAllocCount: Integer; cdecl;
    //{class} function getThreadExternalAllocSize: Integer; cdecl;
    //{class} function getThreadGcInvocationCount: Integer; cdecl;
    //{class} function isDebuggerConnected: Boolean; cdecl;
    //{class} procedure printLoadedClasses(flags: Integer); cdecl;
    //{class} procedure resetAllCounts; cdecl;
    //{class} procedure resetGlobalAllocCount; cdecl;
    //{class} procedure resetGlobalAllocSize; cdecl;
    //{class} procedure resetGlobalClassInitCount; cdecl;
    //{class} procedure resetGlobalClassInitTime; cdecl;
    //{class} procedure resetGlobalExternalAllocCount; cdecl;
    //{class} procedure resetGlobalExternalAllocSize; cdecl;
    //{class} procedure resetGlobalExternalFreedCount; cdecl;
    //{class} procedure resetGlobalExternalFreedSize; cdecl;
    //{class} procedure resetGlobalFreedCount; cdecl;
    //{class} procedure resetGlobalFreedSize; cdecl;
    //{class} procedure resetGlobalGcInvocationCount; cdecl;
    //{class} procedure resetThreadAllocCount; cdecl;
    //{class} procedure resetThreadAllocSize; cdecl;
    //{class} procedure resetThreadExternalAllocCount; cdecl;
    //{class} procedure resetThreadExternalAllocSize; cdecl;
    //{class} procedure resetThreadGcInvocationCount; cdecl;
    //{class} function setAllocationLimit(limit: Integer): Integer; cdecl;
    //{class} function setGlobalAllocationLimit(limit: Integer): Integer; cdecl;
    //{class} procedure startAllocCounting; cdecl;
    //{class} procedure startMethodTracing; cdecl; overload;
    //{class} procedure startMethodTracing(traceName: JString); cdecl; overload;
    //{class} procedure startMethodTracing(traceName: JString; bufferSize: Integer); cdecl; overload;
    //{class} procedure startMethodTracing(traceName: JString; bufferSize: Integer; flags: Integer); cdecl; overload;
    //{class} procedure startMethodTracingSampling(traceName: JString; bufferSize: Integer; intervalUs: Integer); cdecl;
    //{class} procedure startNativeTracing; cdecl;
    //{class} procedure stopAllocCounting; cdecl;
    //{class} procedure stopMethodTracing; cdecl;
    //{class} procedure stopNativeTracing; cdecl;
    //{class} function threadCpuTimeNanos: Int64; cdecl;
    //{class} procedure waitForDebugger; cdecl;
    //{class} function waitingForDebugger: Boolean; cdecl;
    //{class} property SHOW_CLASSLOADER: Integer read _GetSHOW_CLASSLOADER;
    //{class} property SHOW_FULL_DETAIL: Integer read _GetSHOW_FULL_DETAIL;
    //{class} property SHOW_INITIALIZED: Integer read _GetSHOW_INITIALIZED;
    //{class} property TRACE_COUNT_ALLOCS: Integer read _GetTRACE_COUNT_ALLOCS;
  end;
  [JavaSignature('android/os/Debug')]
  JDebug = interface(JObject)
    ['{624F38B4-6EAE-4326-B01D-6E8498D5C321}']
  end;
  TJDebug = class(TJavaGenericImport<JDebugClass, JDebug>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Os.JStatFs', TypeInfo(Alcinoe.AndroidApi.Os.JStatFs));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Os.JAsyncTask_Status', TypeInfo(Alcinoe.AndroidApi.Os.JAsyncTask_Status));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Os.JAsyncTask', TypeInfo(Alcinoe.AndroidApi.Os.JAsyncTask));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Os.JDebug', TypeInfo(Alcinoe.AndroidApi.Os.JDebug));
end;

initialization
  {$IF defined(UiModeManager)}
  ALLog('Alcinoe.AndroidApi.Os','initialization');
  {$ENDIF}
  RegisterTypes;

end.
