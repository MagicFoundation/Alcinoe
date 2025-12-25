unit Alcinoe.AndroidApi.AndroidX;

interface

{$I Alcinoe.inc}

uses
  Androidapi.JNI.Widget,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Util,
  Androidapi.JNIBridge,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes;

type

  {************************}
  JALHttpWorker = interface;
  JLocalBroadcastManager = interface;
  JLifecycleOwner = interface;
  Jlifecycle_Observer = interface;
  JLiveData = interface;
  JMutableLiveData = interface;
  JPreferenceManager = interface;

  {************************************}
  JWorkerClass = interface(JObjectClass)
    ['{CDDDECF7-16C2-4C4B-8792-978E3BEDFE1B}']
  end;
  [JavaSignature('androidx/work/Worker')]
  JWorker = interface(JObject)
    ['{45387655-3B07-476D-AA6E-6DFE96BBE83E}']
  end;
  TJWorker = class(TJavaGenericImport<JWorkerClass, JWorker>) end;

  {******************************************}
  JALHttpWorkerClass = interface(JWorkerClass)
    ['{E793143E-6277-4C10-AA0A-B79330756F93}']
    {class} function _GetACTION_HTTP_COMPLETED: JString; cdecl;
    {class} function _GetEXTRA_HTTP_SUCCESS: JString; cdecl;
    {class} function _GetEXTRA_HTTP_CANCELED: JString; cdecl;
    {class} function _GetEXTRA_HTTP_REQUEST_ID: JString; cdecl;
    {class} function _GetEXTRA_HTTP_RESPONSE_STATUS_CODE: JString; cdecl;
    {class} function _GetEXTRA_HTTP_RESPONSE_HEADERS: JString; cdecl;
    {class} function _GetEXTRA_HTTP_RESPONSE_BODY_FILE_PATH: JString; cdecl;
    {class} property ACTION_HTTP_COMPLETED: JString read _GetACTION_HTTP_COMPLETED;
    {class} property EXTRA_HTTP_SUCCESS: JString read _GetEXTRA_HTTP_SUCCESS;
    {class} property EXTRA_HTTP_CANCELED: JString read _GetEXTRA_HTTP_CANCELED;
    {class} property EXTRA_HTTP_REQUEST_ID: JString read _GetEXTRA_HTTP_REQUEST_ID;
    {class} property EXTRA_HTTP_RESPONSE_STATUS_CODE: JString read _GetEXTRA_HTTP_RESPONSE_STATUS_CODE;
    {class} property EXTRA_HTTP_RESPONSE_HEADERS: JString read _GetEXTRA_HTTP_RESPONSE_HEADERS;
    {class} property EXTRA_HTTP_RESPONSE_BODY_FILE_PATH: JString read _GetEXTRA_HTTP_RESPONSE_BODY_FILE_PATH;
    {class} function enqueue(context: JContext; url: JString; method: JString; bodyFilePath: JString; deleteBodyFile: Boolean; headers: JString): JUUID; overload; cdecl;
    {class} function enqueue(context: JContext; url: JString; method: JString; bodyString: JString; headers: JString): JUUID; overload; cdecl;
    {class} procedure cancel(context: JContext; requestId: JUUID); overload; cdecl;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/http/ALHttpWorker')]
  JALHttpWorker = interface(JWorker)
    ['{ED46E1D7-D8B5-4B78-8EF2-0E8A5BE2F64F}']
  end;
  TJALHttpWorker = class(TJavaGenericImport<JALHttpWorkerClass, JALHttpWorker>) end;

  {*************************************************************************}
  //https://developer.android.com/reference/androidx/lifecycle/LifecycleOwner
  JLifecycleOwnerClass = interface(IJavaClass)
    ['{4A72E007-665C-4E9A-BF6A-3997CC4C55FB}']
  end;
  [JavaSignature('androidx/lifecycle/LifecycleOwner')]
  JLifecycleOwner = interface(IJavaInstance)
    ['{4ECDBBFC-2F20-471F-912E-BE96CA23E96E}']
  end;
  TJLifecycleOwner = class(TJavaGenericImport<JLifecycleOwnerClass, JLifecycleOwner>) end;

  {****************************************************************************************************}
  //https://developer.android.com/reference/androidx/localbroadcastmanager/content/LocalBroadcastManager
  JLocalBroadcastManagerClass = interface(JObjectClass)
    ['{03179F7E-C439-4369-93CC-AA2BADC54398}']
    {class} function getInstance(context: JContext): JLocalBroadcastManager; cdecl; deprecated;
  end;
  [JavaSignature('androidx/localbroadcastmanager/content/LocalBroadcastManager')]
  JLocalBroadcastManager = interface(JObject)
    ['{6C255CD6-D94E-40BC-A758-EC4965A40725}']
    procedure registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter); cdecl; deprecated;
    function sendBroadcast(intent: JIntent): Boolean; cdecl; deprecated;
    procedure sendBroadcastSync(intent: JIntent); cdecl; deprecated;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl; deprecated;
  end;
  TJLocalBroadcastManager = class(TJavaGenericImport<JLocalBroadcastManagerClass, JLocalBroadcastManager>) end;

  {*******************************************************************}
  //https://developer.android.com/reference/androidx/lifecycle/Observer
  Jlifecycle_ObserverClass = interface(IJavaClass)
    ['{4AF7ACEE-58A0-4FED-BE07-21162B9AA120}']
  end;
  [JavaSignature('androidx/lifecycle/Observer')]
  Jlifecycle_Observer = interface(IJavaInstance)
    ['{A78847B9-5FDD-4E82-86A0-4D026F433099}']
    procedure onChanged(t: JObject); cdecl;
  end;
  TJlifecycle_Observer = class(TJavaGenericImport<Jlifecycle_ObserverClass, Jlifecycle_Observer>) end;

  {*******************************************************************}
  //https://developer.android.com/reference/androidx/lifecycle/LiveData
  JLiveDataClass = interface(JObjectClass)
    ['{57738F7E-F789-4911-90D1-9C8E14720CC0}']
  end;
  [JavaSignature('androidx/lifecycle/LiveData')]
  JLiveData = interface(JObject)
    ['{520539FF-2147-4B43-8287-23E12551B616}']
    function getValue: JObject; cdecl;
    function hasActiveObservers: Boolean; cdecl;
    function hasObservers: Boolean; cdecl;
    procedure observe(owner: JLifecycleOwner; observer: Jlifecycle_Observer); cdecl;
    procedure observeForever(observer: Jlifecycle_Observer); cdecl;
    procedure postValue(value: JObject); cdecl;
    procedure removeObserver(observer: Jlifecycle_Observer); cdecl;
    procedure removeObservers(owner: JLifecycleOwner); cdecl;
    procedure setValue(value: JObject); cdecl;
  end;
  TJLiveData = class(TJavaGenericImport<JLiveDataClass, JLiveData>) end;

  {**************************************************************************}
  //https://developer.android.com/reference/androidx/lifecycle/MutableLiveData
  JMutableLiveDataClass = interface(JLiveDataClass)
    ['{F28C102C-51C2-4048-8FCC-9AB9EA939938}']
  end;
  [JavaSignature('androidx/lifecycle/MutableLiveData')]
  JMutableLiveData = interface(JLiveData)
    ['{597E7685-0F91-4384-A153-818DD5436B5D}']
  end;
  TJMutableLiveData = class(TJavaGenericImport<JMutableLiveDataClass, JMutableLiveData>) end;

  {*****************************************************************************}
  //https://developer.android.com/reference/androidx/preference/PreferenceManager
  JPreferenceManagerClass = interface(JObjectClass)
    ['{2BCBB8F6-B5EE-441E-B01B-5F7E37A783B5}']
    {class} function getDefaultSharedPreferences(context: JContext): JSharedPreferences; cdecl;
  end;
  [JavaSignature('androidx/preference/PreferenceManager')]
  JPreferenceManager = interface(JObject)
    ['{62FC9030-B469-461B-98AD-C5E3F9AAACBA}']
  end;
  TJPreferenceManager = class(TJavaGenericImport<JPreferenceManagerClass, JPreferenceManager>) end;


implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JALHttpWorker', TypeInfo(Alcinoe.AndroidApi.AndroidX.JALHttpWorker));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JLocalBroadcastManager', TypeInfo(Alcinoe.AndroidApi.AndroidX.JLocalBroadcastManager));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JLifecycleOwner', TypeInfo(Alcinoe.AndroidApi.AndroidX.JLifecycleOwner));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.Jlifecycle_Observer', TypeInfo(Alcinoe.AndroidApi.AndroidX.Jlifecycle_Observer));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JLiveData', TypeInfo(Alcinoe.AndroidApi.AndroidX.JLiveData));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JMutableLiveData', TypeInfo(Alcinoe.AndroidApi.AndroidX.JMutableLiveData));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.AndroidX.JPreferenceManager', TypeInfo(Alcinoe.AndroidApi.AndroidX.JPreferenceManager));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.AndroidX','initialization');
  {$ENDIF}
  RegisterTypes;

end.