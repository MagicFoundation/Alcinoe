unit ALAndroidFirebaseApi;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.PlayServices.Tasks,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App,
  Androidapi.JNI.Os,
  ALAndroidXApi;

type

  {***************************}
  JFirebaseOptions = interface;
  JFirebaseOptions_Builder = interface;
  JFirebaseApp = interface;
  JFirebaseInstallationsApi = interface;
  JFirebaseInstallations = interface;
  JFirebaseMessaging = interface;
  JALFirebaseMessagingService = interface;
  JFirebaseAnalytics = interface;

  {**************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/FirebaseOptions
  JFirebaseOptionsClass = interface(JObjectClass)
    ['{45A7F992-FFBD-4CE9-B35F-7C00A55B1A8E}']
    {class} function fromResource(context: JContext): JFirebaseOptions; cdecl;
  end;
  [JavaSignature('com/google/firebase/FirebaseOptions')]
  JFirebaseOptions = interface(JObject)
    ['{E037ADE0-0511-43CB-B2F6-EAC84EBFFD3A}']
    function equals(o: JObject): Boolean; cdecl;
    function getApiKey: JString; cdecl;
    function getApplicationId: JString; cdecl;
    function getDatabaseUrl: JString; cdecl;
    function getGcmSenderId: JString; cdecl;
    function getStorageBucket: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseOptions = class(TJavaGenericImport<JFirebaseOptionsClass, JFirebaseOptions>) end;

  {**********************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/FirebaseOptions.Builder
  JFirebaseOptions_BuilderClass = interface(JObjectClass)
    ['{A9C44F80-53FA-44E8-A7D0-4D61911172EA}']
    {class} function init: JFirebaseOptions_Builder; cdecl; overload;
    {class} function init(options: JFirebaseOptions): JFirebaseOptions_Builder; cdecl; overload;
  end;
  [JavaSignature('com/google/firebase/FirebaseOptions$Builder')]
  JFirebaseOptions_Builder = interface(JObject)
    ['{D658912E-D615-4648-AC53-562635CACC40}']
    function build: JFirebaseOptions; cdecl;
    function setApiKey(apiKey: JString): JFirebaseOptions_Builder; cdecl;
    function setApplicationId(applicationId: JString): JFirebaseOptions_Builder; cdecl;
    function setDatabaseUrl(databaseUrl: JString): JFirebaseOptions_Builder; cdecl;
    function setGcmSenderId(gcmSenderId: JString): JFirebaseOptions_Builder; cdecl;
    function setStorageBucket(storageBucket: JString): JFirebaseOptions_Builder; cdecl;
  end;
  TJFirebaseOptions_Builder = class(TJavaGenericImport<JFirebaseOptions_BuilderClass, JFirebaseOptions_Builder>) end;

  {**********************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/FirebaseApp
  JFirebaseAppClass = interface(JObjectClass)
    ['{6F49665E-6CC5-4208-AFF9-368372B2337F}']
    {class} function _GetDEFAULT_APP_NAME: JString; cdecl;
    {class} function getApps(context: JContext): JList; cdecl;
    {class} function getInstance: JFirebaseApp; cdecl; overload;
    {class} function getInstance(name: JString): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions): JFirebaseApp; cdecl; overload;
    {class} function initializeApp(context: JContext; options: JFirebaseOptions; name: JString): JFirebaseApp; cdecl; overload;
    {class} property DEFAULT_APP_NAME: JString read _GetDEFAULT_APP_NAME;
  end;
  [JavaSignature('com/google/firebase/FirebaseApp')]
  JFirebaseApp = interface(JObject)
    ['{FDAAA04D-2BD8-4C9E-8D1A-30600C405625}']
    function equals(o: JObject): Boolean; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getName: JString; cdecl;
    function getOptions: JFirebaseOptions; cdecl;
    function hashCode: Integer; cdecl;
    procedure setAutomaticResourceManagementEnabled(enabled: Boolean); cdecl;
    function toString: JString; cdecl;
  end;
  TJFirebaseApp = class(TJavaGenericImport<JFirebaseAppClass, JFirebaseApp>) end;

  {*************************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/installations/FirebaseInstallationsApi
  JFirebaseInstallationsApiClass = interface(IJavaClass)
    ['{1EA29A3C-1F12-494E-A51A-3950B4402160}']
  end;
  [JavaSignature('com/google/firebase/installations/FirebaseInstallationsApi')]
  JFirebaseInstallationsApi = interface(IJavaInstance)
    ['{3B6C38B7-B924-4DB4-9E9A-0BDE8752D21C}']
    function delete: JTask; cdecl;
    function getId: JTask; cdecl;
    function getToken(forceRefresh: Boolean): JTask; cdecl;
  end;
  TJFirebaseInstallationsApi = class(TJavaGenericImport<JFirebaseInstallationsApiClass, JFirebaseInstallationsApi>) end;

  {**********************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/installations/FirebaseInstallations
  JFirebaseInstallationsClass = interface(JFirebaseInstallationsApiClass)
    ['{A9EE0398-F392-4D27-8726-D57BEAA431CE}']
    {class} function getInstance: JFirebaseInstallations; cdecl; overload;
    {class} function getInstance(app: JFirebaseApp): JFirebaseInstallations; cdecl; overload;
  end;
  [JavaSignature('com/google/firebase/installations/FirebaseInstallations')]
  JFirebaseInstallations = interface(JFirebaseInstallationsApi)
    ['{87894305-D70D-4A20-B343-D3465DE65061}']
    function delete: JTask; cdecl;
    function getId: JTask; cdecl;
    function getToken(forceRefresh: Boolean): JTask; cdecl;
  end;
  TJFirebaseInstallations = class(TJavaGenericImport<JFirebaseInstallationsClass, JFirebaseInstallations>) end;

  {**************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/messaging/FirebaseMessaging
  JFirebaseMessagingClass = interface(JObjectClass)
    ['{5E14657A-AF87-404A-8BF2-9B14D1730E90}']
    {class} function getInstance: JFirebaseMessaging; cdecl; overload;
  end;
  [JavaSignature('com/google/firebase/messaging/FirebaseMessaging')]
  JFirebaseMessaging = interface(JObject)
    ['{EE5EEAAC-6BB6-4401-A1A0-69E348580BD0}']
    function deleteToken: JTask; cdecl;
    function getToken: JTask; cdecl;
    function isAutoInitEnabled: Boolean; cdecl;
    procedure setAutoInitEnabled(enable: Boolean); cdecl;
    function subscribeToTopic(topic: JString): JTask; cdecl;
    function unsubscribeFromTopic(topic: JString): JTask; cdecl;
  end;
  TJFirebaseMessaging = class(TJavaGenericImport<JFirebaseMessagingClass, JFirebaseMessaging>) end;

  {******************************************************}
  JALFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{0A2D87AC-C8A8-4565-8EC9-598592836070}']
    {class} function _GetnewTokenDispatcher: JMutableLiveData; cdecl;
    {class} function _GetnewMessageDispatcher: JMutableLiveData; cdecl;
    {class} property newTokenDispatcher: JMutableLiveData read _GetnewTokenDispatcher;
    {class} property newMessageDispatcher: JMutableLiveData read _GetnewMessageDispatcher;
  end;
  [JavaSignature('com/alcinoe/firebase/messaging/ALFirebaseMessagingService')]
  JALFirebaseMessagingService = interface(JObject)
    ['{9F3FF329-E17E-41B4-9C4A-214AF6A1FC05}']
  end;
  TJALFirebaseMessagingService = class(TJavaGenericImport<JALFirebaseMessagingServiceClass, JALFirebaseMessagingService>) end;

  {**************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/analytics/FirebaseAnalytics
  JFirebaseAnalyticsClass = interface(JObjectClass)
    ['{19AA41AC-06A8-4D39-A37C-CA44DCA4EDAB}']
    {class} function getInstance(context: JContext): JFirebaseAnalytics; cdecl;
  end;
  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics')]
  JFirebaseAnalytics = interface(JObject)
    ['{536A10DE-A85D-4260-98AC-0311F43DC370}']
    procedure logEvent(name: JString; params: JBundle); cdecl;
    procedure setUserId(id: JString); cdecl;
  end;
  TJFirebaseAnalytics = class(TJavaGenericImport<JFirebaseAnalyticsClass, JFirebaseAnalytics>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseOptions', TypeInfo(ALAndroidFirebaseApi.JFirebaseOptions));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseOptions_Builder', TypeInfo(ALAndroidFirebaseApi.JFirebaseOptions_Builder));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseApp', TypeInfo(ALAndroidFirebaseApi.JFirebaseApp));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseInstallationsApi', TypeInfo(ALAndroidFirebaseApi.JFirebaseInstallationsApi));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseInstallations', TypeInfo(ALAndroidFirebaseApi.JFirebaseInstallations));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseMessaging', TypeInfo(ALAndroidFirebaseApi.JFirebaseMessaging));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseMessagingService', TypeInfo(ALAndroidFirebaseApi.JALFirebaseMessagingService));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseAnalytics', TypeInfo(ALAndroidFirebaseApi.JFirebaseAnalytics));
end;

initialization
  RegisterTypes;

end.
