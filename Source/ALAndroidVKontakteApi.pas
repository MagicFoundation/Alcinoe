unit ALAndroidVKontakteApi;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.App;

type

  {*******************}
  JVKScope = interface;
  JVKAccessToken = interface;
  JVKAuthCallback = interface;
  JVK = interface;
  JVKUtils = interface;

  {***********************************}
  JVKScopeClass = interface(JEnumClass)
    ['{6D299E22-9E66-4DD9-8556-17C7830134F6}']
    {class} function _GetADS: JVKScope; cdecl;
    {class} function _GetAUDIO: JVKScope; cdecl;
    {class} function _GetDOCS: JVKScope; cdecl;
    {class} function _GetEMAIL: JVKScope; cdecl;
    {class} function _GetFRIENDS: JVKScope; cdecl;
    {class} function _GetGROUPS: JVKScope; cdecl;
    {class} function _GetMARKET: JVKScope; cdecl;
    {class} function _GetMESSAGES: JVKScope; cdecl;
    {class} function _GetNOTES: JVKScope; cdecl;
    {class} function _GetNOTIFICATIONS: JVKScope; cdecl;
    {class} function _GetNOTIFY: JVKScope; cdecl;
    {class} function _GetOFFLINE: JVKScope; cdecl;
    {class} function _GetPAGES: JVKScope; cdecl;
    {class} function _GetPHONE: JVKScope; cdecl;
    {class} function _GetPHOTOS: JVKScope; cdecl;
    {class} function _GetSTATS: JVKScope; cdecl;
    {class} function _GetSTATUS: JVKScope; cdecl;
    {class} function _GetSTORIES: JVKScope; cdecl;
    {class} function _GetVIDEO: JVKScope; cdecl;
    {class} function _GetWALL: JVKScope; cdecl;
    {class} function valueOf(name: JString): JVKScope; cdecl;
    {class} function values: TJavaObjectArray<JVKScope>; cdecl;
    {class} property ADS: JVKScope read _GetADS;
    {class} property AUDIO: JVKScope read _GetAUDIO;
    {class} property DOCS: JVKScope read _GetDOCS;
    {class} property EMAIL: JVKScope read _GetEMAIL;
    {class} property FRIENDS: JVKScope read _GetFRIENDS;
    {class} property GROUPS: JVKScope read _GetGROUPS;
    {class} property MARKET: JVKScope read _GetMARKET;
    {class} property MESSAGES: JVKScope read _GetMESSAGES;
    {class} property NOTES: JVKScope read _GetNOTES;
    {class} property NOTIFICATIONS: JVKScope read _GetNOTIFICATIONS;
    {class} property NOTIFY: JVKScope read _GetNOTIFY;
    {class} property OFFLINE: JVKScope read _GetOFFLINE;
    {class} property PAGES: JVKScope read _GetPAGES;
    {class} property PHONE: JVKScope read _GetPHONE;
    {class} property PHOTOS: JVKScope read _GetPHOTOS;
    {class} property STATS: JVKScope read _GetSTATS;
    {class} property STATUS: JVKScope read _GetSTATUS;
    {class} property STORIES: JVKScope read _GetSTORIES;
    {class} property VIDEO: JVKScope read _GetVIDEO;
    {class} property WALL: JVKScope read _GetWALL;
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKScope')]
  JVKScope = interface(JEnum)
    ['{2596E59E-D2E5-457F-B3EE-CA7ADD5FD1C2}']
  end;
  TJVKScope = class(TJavaGenericImport<JVKScopeClass, JVKScope>) end;

  {*******************************************}
  JVKAccessTokenClass = interface(JObjectClass)
    ['{4AA5D139-70EF-4BED-AFCE-6AD11B109F57}']
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKAccessToken')]
  JVKAccessToken = interface(JObject)
    ['{B7E0A898-D133-4A4B-A74E-F192D64E28DA}']
    function getAccessToken: JString; cdecl;
    function getCreated: Int64; cdecl;
    function getEmail: JString; cdecl;
    function getPhone: JString; cdecl;
    function getPhoneAccessKey: JString; cdecl;
    function getSecret: JString; cdecl;
    function getUserId: Integer; cdecl;
    function isValid: Boolean; cdecl;
  end;
  TJVKAccessToken = class(TJavaGenericImport<JVKAccessTokenClass, JVKAccessToken>) end;

  {******************************************}
  JVKAuthCallbackClass = interface(IJavaClass)
    ['{85991DF5-2CD9-46BD-B3BC-41373EA0850F}']
    {class} function _GetAUTH_CANCELED: Integer; cdecl;
    {class} function _GetUNKNOWN_ERROR: Integer; cdecl;
    {class} property AUTH_CANCELED: Integer read _GetAUTH_CANCELED;
    {class} property UNKNOWN_ERROR: Integer read _GetUNKNOWN_ERROR;
  end;
  [JavaSignature('com/vk/api/sdk/auth/VKAuthCallback')]
  JVKAuthCallback = interface(IJavaInstance)
    ['{682834EF-3C5A-40DA-AD7D-30DE4015C2B5}']
    procedure onLogin(token: JVKAccessToken); cdecl;
    procedure onLoginFailed(errorCode: Integer); cdecl;
  end;
  TJVKAuthCallback = class(TJavaGenericImport<JVKAuthCallbackClass, JVKAuthCallback>) end;

  {********************************}
  JVKClass = interface(JObjectClass)
    ['{0450DCD7-E946-43D6-801E-9DAC255B6052}']
    //{class} function _GetINSTANCE: JVK; cdecl;
    //{class} function _GetapiManager: JVKApiManager; cdecl;
    //{class} procedure addTokenExpiredHandler(handler: JVKTokenExpiredHandler); cdecl;
    {class} procedure clearAccessToken(context: JContext); cdecl;
    //{class} procedure execute(request: JApiCommand; callback: JVKApiCallback); cdecl;
    //{class} function executeSync(cmd: JApiCommand): JObject; cdecl;
    {class} function getApiVersion: JString; cdecl;
    {class} function getAppId(context: JContext): Integer; cdecl;
    {class} function getUserId: Integer; cdecl;
    {class} procedure initialize(context: JContext); cdecl;
    {class} function isLoggedIn: Boolean; cdecl;
    {class} procedure login(activity: JActivity); cdecl; overload;
    {class} procedure login(activity: JActivity; scopes: JCollection); cdecl; overload;
    {class} procedure logout; cdecl;
    {class} function onActivityResult(requestCode: Integer; resultCode: Integer; data: JIntent; callback: JVKAuthCallback): Boolean; cdecl;
    //{class} procedure removeTokenExpiredHandler(handler: JVKTokenExpiredHandler); cdecl;
    //{class} procedure saveAccessToken(context: JContext; userId: Integer; accessToken: JString; secret: JString); cdecl;
    //{class} procedure setConfig(config: JVKApiConfig); cdecl;
    //{class} procedure setCredentials(context: JContext; userId: Integer; accessToken: JString; secret: JString; saveAccessTokenToStorage: Boolean); cdecl;
  end;
  [JavaSignature('com/vk/api/sdk/VK')]
  JVK = interface(JObject)
    ['{CBA5C3FA-9F3B-467C-A3D5-0F21093F2CFB}']
  end;
  TJVK = class(TJavaGenericImport<JVKClass, JVK>) end;

  {*************************************}
  JVKUtilsClass = interface(JObjectClass)
    ['{8D1AE62F-18C2-414B-8048-F837494A60B1}']
    {class} function explodeQueryString(queryString: JString): JMap; cdecl;
    {class} function getCertificateFingerprint(Context: JContext; packageName: JString): TJavaObjectArray<JString>; cdecl;
    {class} function isAppInstalled(context: JContext; packageName: JString): Boolean; cdecl;
    {class} function isIntentAvailable(context: JContext; action: JString): Boolean; cdecl;
  end;
  [JavaSignature('com/vk/api/sdk/utils/VKUtils')]
  JVKUtils = interface(JObject)
    ['{4830DA3E-0FC5-4B07-9284-4E9B702DDEF7}']
    procedure clearAllCookies(context: JContext); cdecl;
    function density: Single; cdecl;
    function dp(dp: Integer): Integer; cdecl;
    //function getDisplayMetrics: JDisplayMetrics; cdecl;
    function height(context: JContext): Integer; cdecl;
    function width(context: JContext): Integer; cdecl;
  end;
  TJVKUtils = class(TJavaGenericImport<JVKUtilsClass, JVKUtils>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidVKontakteApi.JVKScope', TypeInfo(ALAndroidVKontakteApi.JVKScope));
  TRegTypes.RegisterType('ALAndroidVKontakteApi.JVKAccessToken', TypeInfo(ALAndroidVKontakteApi.JVKAccessToken));
  TRegTypes.RegisterType('ALAndroidVKontakteApi.JVKAuthCallback', TypeInfo(ALAndroidVKontakteApi.JVKAuthCallback));
  TRegTypes.RegisterType('ALAndroidVKontakteApi.JVK', TypeInfo(ALAndroidVKontakteApi.JVK));
  TRegTypes.RegisterType('ALAndroidVKontakteApi.JVKUtils', TypeInfo(ALAndroidVKontakteApi.JVKUtils));
end;

initialization
  RegisterTypes;

end.
