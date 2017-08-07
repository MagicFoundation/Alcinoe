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
     Androidapi.JNI.JavaTypes;

type

  {**************************************}
  JALDatePickerDialogListener = interface;
  JALDatePickerDialog = interface;
  JALSoftInputListener = interface;
  JALKeyPreImeListener = interface;
  JALEditText = interface;
  JALControlHostLayout = interface;
  JLog = interface;
  JStatFs = interface;
  JLocalBroadcastManager = interface;
  JPreferenceManager = interface;
  JAsyncTask_Status = interface;
  JAsyncTask = interface;

  {******************************************************}
  JALDatePickerDialogListenerClass = interface(IJavaClass)
    ['{3EDC638B-74FD-40D8-A09C-B92919C9D85B}']
  end;

  {***********************************************************}
  [JavaSignature('com/alcinoe/app/ALDatePickerDialogListener')]
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
                          button_neutral_text: JCharSequence): JALDatePickerDialog; cdecl;
  end;

  {***************************************************}
  [JavaSignature('com/alcinoe/app/ALDatePickerDialog')]
  JALDatePickerDialog = interface(JObject)
    ['{DF4E7117-15AA-4063-9150-EEEC2356FCD7}']
    procedure show(year: integer;
                   month: integer;
                   dayOfMonth: integer); cdecl;
    procedure setListener(listener: JALDatePickerDialogListener); cdecl;
  end;
  TJALDatePickerDialog = class(TJavaGenericImport<JALDatePickerDialogClass, JALDatePickerDialog>) end;

  {***********************************************}
  JALSoftInputListenerClass = interface(IJavaClass)
    ['{30390DEB-3807-4FD3-B0A9-5D53A3CC000A}']
  end;

  {*****************************************************************}
  [JavaSignature('com/alcinoe/view/inputmethod/ALSoftInputListener')]
  JALSoftInputListener = interface(IJavaInstance)
    ['{674AD1A1-0A33-4519-978D-171D00176930}']
    procedure onSoftInputShown; cdecl;
    procedure onSoftInputHidden; cdecl;
  end;
  TJALSoftInputListener = class(TJavaGenericImport<JALSoftInputListenerClass, JALSoftInputListener>) end;

  {***********************************************}
  JALKeyPreImeListenerClass = interface(IJavaClass)
    ['{E01C70E2-4BBF-47CB-8713-5A73344E9EA9}']
  end;

  {************************************************************}
  [JavaSignature('com/alcinoe/text/method/ALKeyPreImeListener')]
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

  {**********************************************}
  [JavaSignature('com/alcinoe/widget/ALEditText')]
  JALEditText = interface(JEditText)
    ['{A3E765A1-44EB-45C0-9AA5-19A38C029CE5}']
    procedure showSoftInput; cdecl;
    procedure hideSoftInput; cdecl;
    procedure setSoftInputListener(listener: JALSoftInputListener); cdecl;
    procedure setKeyPreImeListener(listener: JALKeyPreImeListener); cdecl;
    procedure setMaxLength(value: integer); cdecl;
  end;
  TJALEditText = class(TJavaGenericImport<JALEditTextClass, JALEditText>) end;

  {*******************************************************}
  JALControlHostLayoutClass = interface(JLinearLayoutClass)
    ['{4BB0539E-F89F-4C09-BE60-9AD04F4BBA57}']
    {class} function init(context: JContext): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JALControlHostLayout; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JALControlHostLayout; cdecl; overload;
  end;

  {*******************************************************}
  [JavaSignature('com/alcinoe/widget/ALControlHostLayout')]
  JALControlHostLayout = interface(JLinearLayout)
    ['{601855E2-9BCF-4FB6-8438-FD26D55FFD8D}']
    function disableMoveAnimations: boolean; cdecl;
  end;
  TJALControlHostLayout = class(TJavaGenericImport<JALControlHostLayoutClass, JALControlHostLayout>) end;

  {***********************************}
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

  {***************************************************}
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
    {class} function valueOf(name: JString): JAsyncTask_Status; cdecl;//Deprecated
    {class} function values: TJavaObjectArray<JAsyncTask_Status>; cdecl;//Deprecated
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


implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidApi.JALDatePickerDialogListener', TypeInfo(ALAndroidApi.JALDatePickerDialogListener));
  TRegTypes.RegisterType('ALAndroidApi.JALDatePickerDialog', TypeInfo(ALAndroidApi.JALDatePickerDialog));
  TRegTypes.RegisterType('ALAndroidApi.JALSoftInputListener', TypeInfo(ALAndroidApi.JALSoftInputListener));
  TRegTypes.RegisterType('ALAndroidApi.JALKeyPreImeListener', TypeInfo(ALAndroidApi.JALKeyPreImeListener));
  TRegTypes.RegisterType('ALAndroidApi.JALEditText', TypeInfo(ALAndroidApi.JALEditText));
  TRegTypes.RegisterType('ALAndroidApi.JALControlHostLayout', TypeInfo(ALAndroidApi.JALControlHostLayout));
  TRegTypes.RegisterType('ALAndroidApi.JLog', TypeInfo(ALAndroidApi.JLog));
  TRegTypes.RegisterType('ALAndroidApi.JStatFs', TypeInfo(ALAndroidApi.JStatFs));
  TRegTypes.RegisterType('ALAndroidApi.JLocalBroadcastManager', TypeInfo(ALAndroidApi.JLocalBroadcastManager));
  TRegTypes.RegisterType('ALAndroidApi.JPreferenceManager', TypeInfo(ALAndroidApi.JPreferenceManager));
  TRegTypes.RegisterType('ALAndroidApi.JAsyncTask_Status', TypeInfo(ALAndroidApi.JAsyncTask_Status));
  TRegTypes.RegisterType('ALAndroidApi.JAsyncTask', TypeInfo(ALAndroidApi.JAsyncTask));
end;

initialization
  RegisterTypes;

end.
