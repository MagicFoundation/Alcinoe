unit ALAndroidApi;

interface

//  Java Type    Delphi Type
//  boolean      Boolean
//  byte         ShortInt
//  char         WideChar
//  double       Double
//  float        Single
//  int          Integer
//  long         Int64
//  short        SmallInt
//  void         N/A

uses Androidapi.JNI.Widget,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.Util,
     Androidapi.JNIBridge,
     Androidapi.JNI.Net,
     Androidapi.JNI.JavaTypes;

type

  {**********************}
  JALFileUtil = interface;
  JALDatePickerDialogListener = interface;
  JALDatePickerDialog = interface;
  JALKeyPreImeListener = interface;
  JALEditText = interface;
  JLog = interface;
  JStatFs = interface;
  JLocalBroadcastManager = interface;
  JPreferenceManager = interface;
  JAsyncTask_Status = interface;
  JAsyncTask = interface;
  JChoreographer = interface;
  JChoreographer_FrameCallback = interface;
  JALBroadcastReceiverListener = interface;
  JALBroadcastReceiver = interface;
  JRuntime = interface;
  JRenderScript = interface;
  JBaseObj = interface;
  JAllocation = interface;
  JAllocation_MipmapControl = interface;
  JType = interface;
  JScript = interface;
  JScriptIntrinsic = interface;
  JScriptIntrinsicBlur = interface;
  JElement = interface;
  JDebug = interface;

  {****************************************}
  JALFileUtilClass = interface(JObjectClass)
    ['{3448C3D0-1FAD-489B-88F5-329E09FE2CCF}']
    {class} function createFileFromURI(context: JContext; uri: Jnet_Uri; fileName: Jstring): boolean; cdecl;
  end;

  {********************************************}
  [JavaSignature('com/alcinoe/util/ALFileUtil')]
  JALFileUtil = interface(JObject)
    ['{5F130085-03D3-4436-87BB-84A3BB9F22E0}']
  end;
  TJALFileUtil = class(TJavaGenericImport<JALFileUtilClass, JALFileUtil>) end;

  {******************************************************}
  JALDatePickerDialogListenerClass = interface(IJavaClass)
    ['{3EDC638B-74FD-40D8-A09C-B92919C9D85B}']
  end;

  {******************************************************************}
  [JavaSignature('com/alcinoe/datepicker/ALDatePickerDialogListener')]
  JALDatePickerDialogListener = interface(IJavaInstance)
    ['{9A145783-462B-4E51-AAFC-48F68C79C3EA}']
    procedure onBtnClick(which: integer; year: integer; month: integer; dayOfMonth: integer); cdecl;
  end;
  TJALDatePickerDialogListener = class(TJavaGenericImport<JALDatePickerDialogListenerClass, JALDatePickerDialogListener>) end;

  {************************************************}
  JALDatePickerDialogClass = interface(JObjectClass)
    ['{5CBB555C-9128-492E-BFE9-B0B6AE42F26B}']
    {class} function init(context: JContext;
                          button_positive_text: JCharSequence;
	                        button_negative_text: JCharSequence;
                          button_neutral_text: JCharSequence;
                          title: JCharSequence): JALDatePickerDialog; cdecl;
  end;

  {**********************************************************}
  [JavaSignature('com/alcinoe/datepicker/ALDatePickerDialog')]
  JALDatePickerDialog = interface(JObject)
    ['{DF4E7117-15AA-4063-9150-EEEC2356FCD7}']
    procedure show(year: integer;
                   month: integer;
                   dayOfMonth: integer); cdecl;
    procedure setListener(listener: JALDatePickerDialogListener); cdecl;
  end;
  TJALDatePickerDialog = class(TJavaGenericImport<JALDatePickerDialogClass, JALDatePickerDialog>) end;

  {***********************************************}
  JALKeyPreImeListenerClass = interface(IJavaClass)
    ['{E01C70E2-4BBF-47CB-8713-5A73344E9EA9}']
  end;

  {*********************************************************}
  [JavaSignature('com/alcinoe/edittext/ALKeyPreImeListener')]
  JALKeyPreImeListener = interface(IJavaInstance)
    ['{343578E2-962A-461E-ADD7-47A1E4BAA1D9}']
    function onKeyPreIme(keyCode: Integer; event: JKeyEvent): Boolean; cdecl;
  end;
  TJALKeyPreImeListener = class(TJavaGenericImport<JALKeyPreImeListenerClass, JALKeyPreImeListener>) end;

  {******************************************}
  JALEditTextClass = interface(JEditTextClass)
    ['{1969D8DA-0870-47A7-8F4D-E556BC10BB41}']
    {class} function init(context: JContext): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JALEditText; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JALEditText; cdecl; overload;
  end;

  {************************************************}
  [JavaSignature('com/alcinoe/edittext/ALEditText')]
  JALEditText = interface(JEditText)
    ['{A3E765A1-44EB-45C0-9AA5-19A38C029CE5}']
    procedure setKeyPreImeListener(listener: JALKeyPreImeListener); cdecl;
    procedure setMaxLength(value: integer); cdecl;
  end;
  TJALEditText = class(TJavaGenericImport<JALEditTextClass, JALEditText>) end;

  {*********************************}
  JLogClass = interface(JObjectClass)
    ['{E4B8D3E7-409F-41E4-A7A6-9E011CD9B87E}']
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetASSERT: Integer; cdecl;
    {class} function init: JLog; cdecl;
    {class} function v(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: integer): boolean; cdecl;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function println(priority: integer; tag: JString; msg: JString): integer; cdecl;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property INFO: Integer read _GetINFO;
    {class} property WARN: Integer read _GetWARN;
    {class} property ERROR: Integer read _GetERROR;
    {class} property ASSERT: Integer read _GetASSERT;
  end;

  {*********************************}
  [JavaSignature('android/util/Log')]
  JLog = interface(JObject)
    ['{AED82B19-8B1E-4F35-85D9-851D6F1B4F54}']
  end;
  TJLog = class(TJavaGenericImport<JLogClass, JLog>) end;

  {************************************}
  JStatFsClass = interface(JObjectClass)
    ['{E5587205-C324-4FAF-A101-E31BCD83BD4D}']
    {class} function init(path: JString): JStatFs; cdecl; // public StatFs(String path)
  end;

  {**********************************}
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

  {***************************************************}
  JLocalBroadcastManagerClass = interface(JObjectClass)
    ['{03179F7E-C439-4369-93CC-AA2BADC54398}']
    {class} function getInstance(context: JContext): JLocalBroadcastManager; cdecl;
  end;

  {*****************************************************************}
  [JavaSignature('android/support/v4/content/LocalBroadcastManager')]
  JLocalBroadcastManager = interface(JObject)
    ['{6C255CD6-D94E-40BC-A758-EC4965A40725}']
    procedure registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter); cdecl;
    function sendBroadcast(intent: JIntent): Boolean; cdecl;
    procedure sendBroadcastSync(intent: JIntent); cdecl;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
  end;
  TJLocalBroadcastManager = class(TJavaGenericImport<JLocalBroadcastManagerClass, JLocalBroadcastManager>) end;

  {***********************************************}
  JPreferenceManagerClass = interface(JObjectClass)
    ['{2BCBB8F6-B5EE-441E-B01B-5F7E37A783B5}']
    {class} function getDefaultSharedPreferences(context: JContext): JSharedPreferences; cdecl;
  end;

  {*****************************************************}
  [JavaSignature('android/preference/PreferenceManager')]
  JPreferenceManager = interface(JObject)
    ['{62FC9030-B469-461B-98AD-C5E3F9AAACBA}']
  end;
  TJPreferenceManager = class(TJavaGenericImport<JPreferenceManagerClass, JPreferenceManager>) end;

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

  {********************************************}
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

  {*************************************}
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

  {*******************************************}
  JChoreographerClass = interface(JObjectClass)
    ['{EEA5A7FB-9148-4A16-A835-50622172A47D}']
    {class} function getInstance: JChoreographer; cdecl;
  end;

  {*******************************************}
  [JavaSignature('android/view/Choreographer')]
  JChoreographer = interface(JObject)
    ['{C5D38D1C-AA30-49D7-A98F-E1A4E2AFED4D}']
    procedure removeFrameCallback(callback: JChoreographer_FrameCallback); cdecl;
    procedure postFrameCallback(callback: JChoreographer_FrameCallback); cdecl;
    procedure postFrameCallbackDelayed(callback: JChoreographer_FrameCallback; delayMillis: Int64); cdecl;
  end;
  TJChoreographer = class(TJavaGenericImport<JChoreographerClass, JChoreographer>) end;

  {*******************************************************}
  JChoreographer_FrameCallbackClass = interface(IJavaClass)
    ['{5E2EB024-1023-47DA-A6D2-606B779E70C2}']
    procedure doFrame(frameTimeNanos: Int64); cdecl;
  end;

  {*********************************************************}
  [JavaSignature('android/view/Choreographer$FrameCallback')]
  JChoreographer_FrameCallback = interface(IJavaInstance)
    ['{305C5D2F-8277-401F-A56A-784912F8CF6E}']
  end;
  TJChoreographer_FrameCallback = class(TJavaGenericImport<JChoreographer_FrameCallbackClass, JChoreographer_FrameCallback>) end;

  {*******************************************************}
  JALBroadcastReceiverListenerClass = interface(IJavaClass)
    ['{64D38904-11AB-4B0E-B9C1-D5038273BC0D}']
  end;

  {****************************************************************}
  [JavaSignature('com/alcinoe/content/ALBroadcastReceiverListener')]
  JALBroadcastReceiverListener = interface(IJavaInstance)
    ['{8AD95D78-A7FC-4613-8D94-55CC19EB1565}']
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  end;
  TJALBroadcastReceiverListener = class(TJavaGenericImport<JALBroadcastReceiverListenerClass, JALBroadcastReceiverListener>) end;

  {************************************************************}
  JALBroadcastReceiverClass = interface(JBroadcastReceiverClass)
    ['{23BF821F-A433-4564-AD56-7704A58D66C9}']
    {class} function init: JALBroadcastReceiver; cdecl;
  end;

  {********************************************************}
  [JavaSignature('com/alcinoe/content/ALBroadcastReceiver')]
  JALBroadcastReceiver = interface(JBroadcastReceiver)
    ['{42427B26-C270-4832-8645-F788FCB549CB}']
    procedure setListener(listener: JALBroadcastReceiverListener); cdecl;
  end;
  TJALBroadcastReceiver = class(TJavaGenericImport<JALBroadcastReceiverClass, JALBroadcastReceiver>) end;

  {*************************************}
  JRuntimeClass = interface(JObjectClass)
    ['{B466FF95-339F-4060-965B-44350FDC2686}']
    {class} function getRuntime: JRuntime; cdecl;
  end;

  {**********************************}
  [JavaSignature('java/lang/Runtime')]
  JRuntime = interface(JObject)
    ['{AB57684A-3434-4C09-ACD4-5EC5F220D02D}']
    //function exec(progArray: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>; directory: JFile): Jlang_Process; cdecl; overload;
    //function exec(prog: JString): Jlang_Process; cdecl; overload;
    //function exec(prog: JString; envp: TJavaObjectArray<JString>): Jlang_Process; cdecl; overload;
    //function exec(prog: JString; envp: TJavaObjectArray<JString>; directory: JFile): Jlang_Process; cdecl; overload;
    procedure exit(code: Integer); cdecl;
    procedure gc; cdecl;
    procedure load(absolutePath: JString); cdecl;
    procedure loadLibrary(nickname: JString); cdecl;
    procedure runFinalization; cdecl;
    procedure runFinalizersOnExit(run: Boolean); cdecl; deprecated;
    procedure traceInstructions(enable: Boolean); cdecl;
    procedure traceMethodCalls(enable: Boolean); cdecl;
    function getLocalizedInputStream(stream: JInputStream): JInputStream; cdecl; deprecated;
    function getLocalizedOutputStream(stream: JOutputStream): JOutputStream; cdecl; deprecated;
    procedure addShutdownHook(hook: JThread); cdecl;
    function removeShutdownHook(hook: JThread): Boolean; cdecl;
    procedure halt(code: Integer); cdecl;
    function availableProcessors: Integer; cdecl;
    function freeMemory: Int64; cdecl;
    function totalMemory: Int64; cdecl;
    function maxMemory: Int64; cdecl;
  end;
  TJRuntime = class(TJavaGenericImport<JRuntimeClass, JRuntime>) end;

  {******************************************}
  JRenderScriptClass = interface(JObjectClass)
    ['{ACD2A56F-451D-4D2C-9EF0-339EE08C663F}']
    {class} function _GetCREATE_FLAG_LOW_LATENCY: Integer; cdecl;
    {class} function _GetCREATE_FLAG_LOW_POWER: Integer; cdecl;
    {class} function _GetCREATE_FLAG_NONE: Integer; cdecl;
    {class} function create(ctx: JContext): JRenderScript; cdecl; overload;
    {class} //function create(ctx: JContext; ct: JRenderScript_ContextType): JRenderScript; cdecl; overload;
    {class} //function create(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer): JRenderScript; cdecl; overload;
    {class} //function createMultiContext(ctx: JContext; ct: JRenderScript_ContextType; flags: Integer; API_number: Integer): JRenderScript; cdecl;
    {class} function getMinorVersion: Int64; cdecl;
    {class} procedure releaseAllContexts; cdecl;
    {class} property CREATE_FLAG_LOW_LATENCY: Integer read _GetCREATE_FLAG_LOW_LATENCY;
    {class} property CREATE_FLAG_LOW_POWER: Integer read _GetCREATE_FLAG_LOW_POWER;
    {class} property CREATE_FLAG_NONE: Integer read _GetCREATE_FLAG_NONE;
  end;

  {**************************************************}
  [JavaSignature('android/renderscript/RenderScript')]
  JRenderScript = interface(JObject)
    ['{1375A3CE-12EA-4DB9-B20F-128B944A172B}']
    //function getErrorHandler: JRenderScript_RSErrorHandler; cdecl;
    //function getMessageHandler: JRenderScript_RSMessageHandler; cdecl;
    //procedure setMessageHandler(msg: JRenderScript_RSMessageHandler); cdecl;
    //procedure setPriority(p: JRenderScript_Priority); cdecl;
    procedure contextDump; cdecl;
    procedure destroy; cdecl;
    procedure finish; cdecl;
    function getApplicationContext: JContext; cdecl;
    procedure sendMessage(id: Integer; data: TJavaArray<Integer>); cdecl;
    //procedure setErrorHandler(msg: JRenderScript_RSErrorHandler); cdecl;
  end;
  TJRenderScript = class(TJavaGenericImport<JRenderScriptClass, JRenderScript>) end;

  {*************************************}
  JBaseObjClass = interface(JObjectClass)
    ['{F1A3D5D0-1151-47D2-A690-B3B562F350CC}']
  end;

  {*********************************************}
  [JavaSignature('android/renderscript/BaseObj')]
  JBaseObj = interface(JObject)
    ['{20615312-245C-4906-B481-80FDCA85B4B7}']
    procedure destroy; cdecl;
    procedure setName(name: JString); cdecl;
    function getName: JString; cdecl;
  end;
  TJBaseObj = class(TJavaGenericImport<JBaseObjClass, JBaseObj>) end;

  {*****************************************}
  JAllocationClass = interface(JBaseObjClass)
    ['{B01FABA3-B1B6-4F4F-AAB7-FCDDCEECB161}']
    {class} function _GetUSAGE_GRAPHICS_CONSTANTS: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_RENDER_TARGET: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_TEXTURE: Integer; cdecl;
    {class} function _GetUSAGE_GRAPHICS_VERTEX: Integer; cdecl;
    {class} function _GetUSAGE_IO_INPUT: Integer; cdecl;
    {class} function _GetUSAGE_IO_OUTPUT: Integer; cdecl;
    {class} function _GetUSAGE_SCRIPT: Integer; cdecl;
    {class} function _GetUSAGE_SHARED: Integer; cdecl;
    {class} //function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} //function createCubemapFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} //function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createCubemapFromCubeFaces(rs: JRenderScript; xpos: JBitmap; xneg: JBitmap; ypos: JBitmap; yneg: JBitmap; zpos: JBitmap; zneg: JBitmap): JAllocation; cdecl; overload;
    {class} //function createFromBitmap(rs: JRenderScript; b: JBitmap; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmap(rs: JRenderScript; b: JBitmap): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createFromBitmapResource(rs: JRenderScript; res: JResources; id: Integer): JAllocation; cdecl; overload;
    {class} function createFromString(rs: JRenderScript; str: JString; usage: Integer): JAllocation; cdecl;
    {class} //function createSized(rs: JRenderScript; e: JElement; count: Integer; usage: Integer): JAllocation; cdecl; overload;
    {class} //function createSized(rs: JRenderScript; e: JElement; count: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; mips: JAllocation_MipmapControl; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType; usage: Integer): JAllocation; cdecl; overload;
    {class} function createTyped(rs: JRenderScript; type_: JType): JAllocation; cdecl; overload;
    {class} property USAGE_GRAPHICS_CONSTANTS: Integer read _GetUSAGE_GRAPHICS_CONSTANTS;
    {class} property USAGE_GRAPHICS_RENDER_TARGET: Integer read _GetUSAGE_GRAPHICS_RENDER_TARGET;
    {class} property USAGE_GRAPHICS_TEXTURE: Integer read _GetUSAGE_GRAPHICS_TEXTURE;
    {class} property USAGE_GRAPHICS_VERTEX: Integer read _GetUSAGE_GRAPHICS_VERTEX;
    {class} property USAGE_IO_INPUT: Integer read _GetUSAGE_IO_INPUT;
    {class} property USAGE_IO_OUTPUT: Integer read _GetUSAGE_IO_OUTPUT;
    {class} property USAGE_SCRIPT: Integer read _GetUSAGE_SCRIPT;
    {class} property USAGE_SHARED: Integer read _GetUSAGE_SHARED;
  end;

  {************************************************}
  [JavaSignature('android/renderscript/Allocation')]
  JAllocation = interface(JBaseObj)
    ['{1A259581-F9C8-4808-99C1-63859107FE15}']
    procedure copy1DRangeFrom(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; data: JAllocation; dataOff: Integer); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Integer>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer; dataZoff: Integer); cdecl; overload;
    procedure copy3DRangeTo(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl;
    procedure copyFrom(d: TJavaObjectArray<JBaseObj>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Single>); cdecl; overload;
    procedure copyFrom(b: JBitmap); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Byte>); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Single>); cdecl; overload;
    procedure copyTo(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyTo(d: TJavaArray<Single>); cdecl; overload;
    procedure destroy; cdecl;
    procedure generateMipmaps; cdecl;
    function getBytesSize: Integer; cdecl;
    function getUsage: Integer; cdecl;
    procedure ioReceive; cdecl;
    procedure ioSend; cdecl;
    //procedure setFromFieldPacker(xoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    //procedure setFromFieldPacker(xoff: Integer; yoff: Integer; zoff: Integer; component_number: Integer; fp: JFieldPacker); cdecl; overload;
    //procedure setOnBufferAvailableListener(callback: JAllocation_OnBufferAvailableListener); cdecl;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFrom(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeFromUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Byte>); cdecl; overload;
    procedure copy1DRangeTo(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; array_: JObject); cdecl; overload;
    procedure copy1DRangeToUnchecked(off: Integer; count: Integer; d: TJavaArray<Single>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Byte>); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: JAllocation; dataXoff: Integer; dataYoff: Integer); cdecl; overload;
    procedure copy2DRangeFrom(xoff: Integer; yoff: Integer; data: JBitmap); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; array_: JObject); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Integer>); cdecl; overload;
    procedure copy2DRangeTo(xoff: Integer; yoff: Integer; w: Integer; h: Integer; data: TJavaArray<Single>); cdecl; overload;
    procedure copy3DRangeFrom(xoff: Integer; yoff: Integer; zoff: Integer; w: Integer; h: Integer; d: Integer; array_: JObject); cdecl; overload;
    procedure copyFrom(array_: JObject); cdecl; overload;
    procedure copyFrom(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyFrom(d: TJavaArray<SmallInt>); cdecl; overload;
    procedure copyFrom(a: JAllocation); cdecl; overload;
    procedure copyFromUnchecked(array_: JObject); cdecl; overload;
    procedure copyFromUnchecked(d: TJavaArray<Integer>); cdecl; overload;
    procedure copyTo(b: JBitmap); cdecl; overload;
    procedure copyTo(array_: JObject); cdecl; overload;
    procedure copyTo(d: TJavaArray<Byte>); cdecl; overload;
    //function getElement: JElement; cdecl;
    function getSurface: JSurface; cdecl;
    function getType: JType; cdecl;
    procedure resize(dimX: Integer); cdecl;
    procedure setAutoPadding(useAutoPadding: Boolean); cdecl;
    //procedure setFromFieldPacker(xoff: Integer; fp: JFieldPacker); cdecl; overload;
    procedure setSurface(sur: JSurface); cdecl;
    procedure syncAll(srcLocation: Integer); cdecl;
  end;
  TJAllocation = class(TJavaGenericImport<JAllocationClass, JAllocation>) end;

  {****************************************************}
  JAllocation_MipmapControlClass = interface(JEnumClass)
    ['{87E40B07-4700-4177-9E49-3FED1F0886F7}']
    {class} function _GetMIPMAP_FULL: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_NONE: JAllocation_MipmapControl; cdecl;
    {class} function _GetMIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl; cdecl;
    {class} function valueOf(name: JString): JAllocation_MipmapControl; cdecl;
    {class} function values: TJavaObjectArray<JAllocation_MipmapControl>; cdecl;
    {class} property MIPMAP_FULL: JAllocation_MipmapControl read _GetMIPMAP_FULL;
    {class} property MIPMAP_NONE: JAllocation_MipmapControl read _GetMIPMAP_NONE;
    {class} property MIPMAP_ON_SYNC_TO_TEXTURE: JAllocation_MipmapControl read _GetMIPMAP_ON_SYNC_TO_TEXTURE;
  end;

  {**************************************************************}
  [JavaSignature('android/renderscript/Allocation$MipmapControl')]
  JAllocation_MipmapControl = interface(JEnum)
    ['{2381D33F-176C-4B5F-A896-F00D69E2B8A5}']
  end;
  TJAllocation_MipmapControl = class(TJavaGenericImport<JAllocation_MipmapControlClass, JAllocation_MipmapControl>) end;

  {***********************************}
  JTypeClass = interface(JBaseObjClass)
    ['{EA772550-90A2-4831-A62F-063EC75F6402}']
    {class} //function createX(rs: JRenderScript; e: JElement; dimX: Integer): JType; cdecl;
    {class} //function createXY(rs: JRenderScript; e: JElement; dimX: Integer; dimY: Integer): JType; cdecl;
    {class} //function createXYZ(rs: JRenderScript; e: JElement; dimX: Integer; dimY: Integer; dimZ: Integer): JType; cdecl;
  end;

  {******************************************}
  [JavaSignature('android/renderscript/Type')]
  JType = interface(JBaseObj)
    ['{1EC7E148-3497-4D42-AF8C-67FF257EECFD}']
    function getCount: Integer; cdecl;
    //function getElement: JElement; cdecl;
    function hasFaces: Boolean; cdecl;
    function hasMipmaps: Boolean; cdecl;
    function getX: Integer; cdecl;
    function getY: Integer; cdecl;
    function getZ: Integer; cdecl;
    function getYuv: Integer; cdecl;
  end;
  TJType = class(TJavaGenericImport<JTypeClass, JType>) end;

  {*************************************}
  JScriptClass = interface(JBaseObjClass)
    ['{3C4B94DB-AA9D-4EAE-BE4C-22FB4E0E6EAE}']
  end;

  {********************************************}
  [JavaSignature('android/renderscript/Script')]
  JScript = interface(JBaseObj)
    ['{25CE110C-9510-469A-A5C5-957A802D881A}']
    function getVarB(index: Integer): Boolean; cdecl;
    function getVarD(index: Integer): Double; cdecl;
    function getVarF(index: Integer): Single; cdecl;
    procedure setTimeZone(timeZone: JString); cdecl;
    procedure setVar(index: Integer; v: Single); cdecl; overload;
    procedure setVar(index: Integer; v: Double); cdecl; overload;
    procedure setVar(index: Integer; o: JBaseObj); cdecl; overload;
    //procedure setVar(index: Integer; v: JFieldPacker); cdecl; overload;
    //procedure setVar(index: Integer; v: JFieldPacker; e: JElement; dims: TJavaArray<Integer>); cdecl; overload;
    procedure bindAllocation(va: JAllocation; slot: Integer); cdecl;
    function getVarI(index: Integer): Integer; cdecl;
    function getVarJ(index: Integer): Int64; cdecl;
    //procedure getVarV(index: Integer; v: JFieldPacker); cdecl;
    procedure setVar(index: Integer; v: Integer); cdecl; overload;
    procedure setVar(index: Integer; v: Int64); cdecl; overload;
    procedure setVar(index: Integer; v: Boolean); cdecl; overload;
  end;
  TJScript = class(TJavaGenericImport<JScriptClass, JScript>) end;

  {*********************************************}
  JScriptIntrinsicClass = interface(JScriptClass)
    ['{EFFAD6DF-A3D4-461C-B594-851D221474ED}']
  end;

  {*****************************************************}
  [JavaSignature('android/renderscript/ScriptIntrinsic')]
  JScriptIntrinsic = interface(JScript)
    ['{584A2682-B1D5-4DD4-A5DD-CBCF599E52E8}']
  end;
  TJScriptIntrinsic = class(TJavaGenericImport<JScriptIntrinsicClass, JScriptIntrinsic>) end;

  {**********************************************************}
  JScriptIntrinsicBlurClass = interface(JScriptIntrinsicClass)
    ['{38491E8D-407C-4BA3-86D4-FE5A6419C4CB}']
    {class} function create(rs: JRenderScript; e: JElement): JScriptIntrinsicBlur; cdecl;
  end;

  {*********************************************************}
  [JavaSignature('android/renderscript/ScriptIntrinsicBlur')]
  JScriptIntrinsicBlur = interface(JScriptIntrinsic)
    ['{63EE931E-7DE6-468A-B5A8-6ACC301BCAF7}']
    procedure forEach(aout: JAllocation); cdecl; overload;
    //procedure forEach(aout: JAllocation; opt: JScript_LaunchOptions); cdecl; overload;
    //function getFieldID_Input: JScript_FieldID; cdecl;
    //function getKernelID: JScript_KernelID; cdecl;
    procedure setInput(ain: JAllocation); cdecl;
    procedure setRadius(radius: Single); cdecl;
  end;
  TJScriptIntrinsicBlur = class(TJavaGenericImport<JScriptIntrinsicBlurClass, JScriptIntrinsicBlur>) end;

  {***************************************************}
  JElementClass = interface(JBaseObjClass)
    ['{19D75D2F-CF5B-4EFE-9A01-A69F51A1685D}']
    {class} function ALLOCATION(rs: JRenderScript): JElement; cdecl;
    {class} function A_8(rs: JRenderScript): JElement; cdecl;
    {class} function BOOLEAN(rs: JRenderScript): JElement; cdecl;
    {class} function ELEMENT(rs: JRenderScript): JElement; cdecl;
    {class} function F16(rs: JRenderScript): JElement; cdecl;
    {class} function F16_2(rs: JRenderScript): JElement; cdecl;
    {class} function F16_3(rs: JRenderScript): JElement; cdecl;
    {class} function F16_4(rs: JRenderScript): JElement; cdecl;
    {class} function F32(rs: JRenderScript): JElement; cdecl;
    {class} function F32_2(rs: JRenderScript): JElement; cdecl;
    {class} function F32_3(rs: JRenderScript): JElement; cdecl;
    {class} function F32_4(rs: JRenderScript): JElement; cdecl;
    {class} function F64(rs: JRenderScript): JElement; cdecl;
    {class} function F64_2(rs: JRenderScript): JElement; cdecl;
    {class} function F64_3(rs: JRenderScript): JElement; cdecl;
    {class} function F64_4(rs: JRenderScript): JElement; cdecl;
    {class} function FONT(rs: JRenderScript): JElement; cdecl;
    {class} function I16(rs: JRenderScript): JElement; cdecl;
    {class} function I16_2(rs: JRenderScript): JElement; cdecl;
    {class} function I16_3(rs: JRenderScript): JElement; cdecl;
    {class} function I16_4(rs: JRenderScript): JElement; cdecl;
    {class} function I32(rs: JRenderScript): JElement; cdecl;
    {class} function I32_2(rs: JRenderScript): JElement; cdecl;
    {class} function I32_3(rs: JRenderScript): JElement; cdecl;
    {class} function I32_4(rs: JRenderScript): JElement; cdecl;
    {class} function I64(rs: JRenderScript): JElement; cdecl;
    {class} function I64_2(rs: JRenderScript): JElement; cdecl;
    {class} function I64_3(rs: JRenderScript): JElement; cdecl;
    {class} function I64_4(rs: JRenderScript): JElement; cdecl;
    {class} function I8(rs: JRenderScript): JElement; cdecl;
    {class} function I8_2(rs: JRenderScript): JElement; cdecl;
    {class} function I8_3(rs: JRenderScript): JElement; cdecl;
    {class} function I8_4(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX4X4(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_2X2(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_3X3(rs: JRenderScript): JElement; cdecl;
    {class} function MATRIX_4X4(rs: JRenderScript): JElement; cdecl;
    {class} function MESH(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_FRAGMENT(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_RASTER(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_STORE(rs: JRenderScript): JElement; cdecl;
    {class} function PROGRAM_VERTEX(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_4444(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_5551(rs: JRenderScript): JElement; cdecl;
    {class} function RGBA_8888(rs: JRenderScript): JElement; cdecl;
    {class} function RGB_565(rs: JRenderScript): JElement; cdecl;
    {class} function RGB_888(rs: JRenderScript): JElement; cdecl;
    {class} function SAMPLER(rs: JRenderScript): JElement; cdecl;
    {class} function SCRIPT(rs: JRenderScript): JElement; cdecl;
    {class} function &TYPE(rs: JRenderScript): JElement; cdecl;
    {class} function U16(rs: JRenderScript): JElement; cdecl;
    {class} function U16_2(rs: JRenderScript): JElement; cdecl;
    {class} function U16_3(rs: JRenderScript): JElement; cdecl;
    {class} function U16_4(rs: JRenderScript): JElement; cdecl;
    {class} function U32(rs: JRenderScript): JElement; cdecl;
    {class} function U32_2(rs: JRenderScript): JElement; cdecl;
    {class} function U32_3(rs: JRenderScript): JElement; cdecl;
    {class} function U32_4(rs: JRenderScript): JElement; cdecl;
    {class} function U64(rs: JRenderScript): JElement; cdecl;
    {class} function U64_2(rs: JRenderScript): JElement; cdecl;
    {class} function U64_3(rs: JRenderScript): JElement; cdecl;
    {class} function U64_4(rs: JRenderScript): JElement; cdecl;
    {class} function U8(rs: JRenderScript): JElement; cdecl;
    {class} function U8_2(rs: JRenderScript): JElement; cdecl;
    {class} function U8_3(rs: JRenderScript): JElement; cdecl;
    {class} function U8_4(rs: JRenderScript): JElement; cdecl;
    {class} function YUV(rs: JRenderScript): JElement; cdecl;
    {class} //function createPixel(rs: JRenderScript; dt: JElement_DataType; dk: JElement_DataKind): JElement; cdecl;
    {class} //function createVector(rs: JRenderScript; dt: JElement_DataType; size: Integer): JElement; cdecl;
  end;

  {*********************************************}
  [JavaSignature('android/renderscript/Element')]
  JElement = interface(JBaseObj)
    ['{47E2A1DE-1101-43E3-BF42-9F8128EDD03E}']
    function getBytesSize: Integer; cdecl;
    function getSubElementArraySize(index: Integer): Integer; cdecl;
    function getSubElementCount: Integer; cdecl;
    function isCompatible(e: JElement): Boolean; cdecl;
    function isComplex: Boolean; cdecl;
    //function getDataKind: JElement_DataKind; cdecl;
    //function getDataType: JElement_DataType; cdecl;
    function getSubElement(index: Integer): JElement; cdecl;
    function getSubElementName(index: Integer): JString; cdecl;
    function getSubElementOffsetBytes(index: Integer): Integer; cdecl;
    function getVectorSize: Integer; cdecl;
  end;
  TJElement = class(TJavaGenericImport<JElementClass, JElement>) end;

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

  {*********************************}
  [JavaSignature('android/os/Debug')]
  JDebug = interface(JObject)
    ['{365EDB47-3CE7-45FB-A2CD-9AF6DD7B2A49}']
  end;
  TJDebug = class(TJavaGenericImport<JDebugClass, JDebug>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidApi.JALFileUtil', TypeInfo(ALAndroidApi.JALFileUtil));
  TRegTypes.RegisterType('ALAndroidApi.JALDatePickerDialogListener', TypeInfo(ALAndroidApi.JALDatePickerDialogListener));
  TRegTypes.RegisterType('ALAndroidApi.JALDatePickerDialog', TypeInfo(ALAndroidApi.JALDatePickerDialog));
  TRegTypes.RegisterType('ALAndroidApi.JALKeyPreImeListener', TypeInfo(ALAndroidApi.JALKeyPreImeListener));
  TRegTypes.RegisterType('ALAndroidApi.JALEditText', TypeInfo(ALAndroidApi.JALEditText));
  TRegTypes.RegisterType('ALAndroidApi.JLog', TypeInfo(ALAndroidApi.JLog));
  TRegTypes.RegisterType('ALAndroidApi.JStatFs', TypeInfo(ALAndroidApi.JStatFs));
  TRegTypes.RegisterType('ALAndroidApi.JLocalBroadcastManager', TypeInfo(ALAndroidApi.JLocalBroadcastManager));
  TRegTypes.RegisterType('ALAndroidApi.JPreferenceManager', TypeInfo(ALAndroidApi.JPreferenceManager));
  TRegTypes.RegisterType('ALAndroidApi.JAsyncTask_Status', TypeInfo(ALAndroidApi.JAsyncTask_Status));
  TRegTypes.RegisterType('ALAndroidApi.JAsyncTask', TypeInfo(ALAndroidApi.JAsyncTask));
  TRegTypes.RegisterType('ALAndroidApi.JChoreographer', TypeInfo(ALAndroidApi.JChoreographer));
  TRegTypes.RegisterType('ALAndroidApi.JChoreographer_FrameCallback', TypeInfo(ALAndroidApi.JChoreographer_FrameCallback));
  TRegTypes.RegisterType('ALAndroidApi.JALBroadcastReceiverListener', TypeInfo(ALAndroidApi.JALBroadcastReceiverListener));
  TRegTypes.RegisterType('ALAndroidApi.JALBroadcastReceiver', TypeInfo(ALAndroidApi.JALBroadcastReceiver));
  TRegTypes.RegisterType('ALAndroidApi.JRuntime', TypeInfo(ALAndroidApi.JRuntime));
  TRegTypes.RegisterType('ALAndroidApi.JRenderScript', TypeInfo(ALAndroidApi.JRenderScript));
  TRegTypes.RegisterType('ALAndroidApi.JBaseObj', TypeInfo(ALAndroidApi.JBaseObj));
  TRegTypes.RegisterType('ALAndroidApi.JAllocation', TypeInfo(ALAndroidApi.JAllocation));
  TRegTypes.RegisterType('ALAndroidApi.JAllocation_MipmapControl', TypeInfo(ALAndroidApi.JAllocation_MipmapControl));
  TRegTypes.RegisterType('ALAndroidApi.JType', TypeInfo(ALAndroidApi.JType));
  TRegTypes.RegisterType('ALAndroidApi.JScript', TypeInfo(ALAndroidApi.JScript));
  TRegTypes.RegisterType('ALAndroidApi.JScriptIntrinsic', TypeInfo(ALAndroidApi.JScriptIntrinsic));
  TRegTypes.RegisterType('ALAndroidApi.JScriptIntrinsicBlur', TypeInfo(ALAndroidApi.JScriptIntrinsicBlur));
  TRegTypes.RegisterType('ALAndroidApi.JElement', TypeInfo(ALAndroidApi.JElement));
  TRegTypes.RegisterType('ALAndroidApi.JDebug', TypeInfo(ALAndroidApi.JDebug));
end;

initialization
  RegisterTypes;

end.
