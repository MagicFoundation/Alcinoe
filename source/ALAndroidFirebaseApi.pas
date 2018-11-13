unit ALAndroidFirebaseApi;

interface

uses Androidapi.JNIBridge,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.App,
     Androidapi.JNI.Os;

type

  {***************************}
  JFirebaseOptions = interface;
  JFirebaseOptions_Builder = interface;
  JFirebaseApp = interface;
  JFirebaseInstanceId = interface;
  JALFirebaseMessagingService = interface;
  JALFirebaseInstanceIdService = interface;
  JFirebaseAnalytics = interface;

  {*********************************************}
  JFirebaseOptionsClass = interface(JObjectClass)
    ['{45A7F992-FFBD-4CE9-B35F-7C00A55B1A8E}']
    {class} function fromResource(context: JContext): JFirebaseOptions; cdecl;
  end;

  {****************************************************}
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

  {*****************************************************}
  JFirebaseOptions_BuilderClass = interface(JObjectClass)
    ['{A9C44F80-53FA-44E8-A7D0-4D61911172EA}']
    {class} function init: JFirebaseOptions_Builder; cdecl; overload;
    {class} function init(options: JFirebaseOptions): JFirebaseOptions_Builder; cdecl; overload;
  end;

  {************************************************************}
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

  {*****************************************}
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

  {************************************************}
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

  {************************************************}
  JFirebaseInstanceIdClass = interface(JObjectClass)
    ['{A048F024-040F-4E5B-A4EB-120737474F13}']
    {class} function getInstance: JFirebaseInstanceId; cdecl; overload;
    {class} function getInstance(app: JFirebaseApp): JFirebaseInstanceId; cdecl; overload;
  end;

  {***********************************************************}
  [JavaSignature('com/google/firebase/iid/FirebaseInstanceId')]
  JFirebaseInstanceId = interface(JObject)
    ['{95893835-0298-4AFA-8C01-5C4ABB1E007A}']
    procedure deleteInstanceId; cdecl;
    procedure deleteToken(authorizedEntity: JString; scope: JString); cdecl;
    function getCreationTime: Int64; cdecl;
    function getId: JString; cdecl;
    function getToken: JString; cdecl; overload;
    function getToken(authorizedEntity: JString; scope: JString): JString; cdecl; overload;
  end;
  TJFirebaseInstanceId = class(TJavaGenericImport<JFirebaseInstanceIdClass, JFirebaseInstanceId>) end;

  {******************************************************}
  JALFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{0A2D87AC-C8A8-4565-8EC9-598592836070}']
    {class} function _GetACTION_MESSAGERECEIVED: JString; cdecl;
    {class} property ACTION_MESSAGERECEIVED: JString read _GetACTION_MESSAGERECEIVED;
    {class} function getPendingDataMessages: JString; cdecl;
  end;

  {**************************************************************************}
  [JavaSignature('com/alcinoe/firebase/messaging/ALFirebaseMessagingService')]
  JALFirebaseMessagingService = interface(JObject)
    ['{9F3FF329-E17E-41B4-9C4A-214AF6A1FC05}']
  end;
  TJALFirebaseMessagingService = class(TJavaGenericImport<JALFirebaseMessagingServiceClass, JALFirebaseMessagingService>) end;

  {******************************************************}
  JALFirebaseInstanceIdServiceClass = interface(JObjectClass)
    ['{2D47591A-F7F7-4FB4-9032-6FAD8AB3519A}']
    {class} function _GetACTION_TOKENREFRESHED: JString; cdecl;
    {class} property ACTION_TOKENREFRESHED: JString read _GetACTION_TOKENREFRESHED;
  end;

  {*********************************************************************}
  [JavaSignature('com/alcinoe/firebase/iid/ALFirebaseInstanceIdService')]
  JALFirebaseInstanceIdService = interface(JObject)
    ['{B2962A80-D526-436C-B352-66601DB9201D}']
  end;
  TJALFirebaseInstanceIdService = class(TJavaGenericImport<JALFirebaseInstanceIdServiceClass, JALFirebaseInstanceIdService>) end;

  {***********************************************}
  JFirebaseAnalyticsClass = interface(JObjectClass)
    ['{19AA41AC-06A8-4D39-A37C-CA44DCA4EDAB}']
    {class} function getInstance(context: JContext): JFirebaseAnalytics; cdecl;
  end;

  {****************************************************************}
  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics')]
  JFirebaseAnalytics = interface(JObject)
    ['{536A10DE-A85D-4260-98AC-0311F43DC370}']
    procedure logEvent(name: JString; params: JBundle); cdecl;
    procedure setUserId(id: JString); cdecl;
  end;
  TJFirebaseAnalytics = class(TJavaGenericImport<JFirebaseAnalyticsClass, JFirebaseAnalytics>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseOptions', TypeInfo(ALAndroidFirebaseApi.JFirebaseOptions));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseOptions_Builder', TypeInfo(ALAndroidFirebaseApi.JFirebaseOptions_Builder));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseApp', TypeInfo(ALAndroidFirebaseApi.JFirebaseApp));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseInstanceId', TypeInfo(ALAndroidFirebaseApi.JFirebaseInstanceId));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseMessagingService', TypeInfo(ALAndroidFirebaseApi.JALFirebaseMessagingService));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseInstanceIdService', TypeInfo(ALAndroidFirebaseApi.JALFirebaseInstanceIdService));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseAnalytics', TypeInfo(ALAndroidFirebaseApi.JFirebaseAnalytics));
end;

initialization
  RegisterTypes;

end.
