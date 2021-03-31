unit ALAndroidFacebookApi;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  ALAndroidApi;

type

  {*************************************}
  JALFacebookShareLinkDialog = interface;
  JFacebookException = interface;
  JFacebookCallback = interface;
  JCallbackManager = interface;
  JCallbackManager_Factory = interface;
  JLoginManager = interface;
  JAccessToken = interface;
  JLoginResult = interface;
  JHttpMethod = interface;
  JGraphRequestAsyncTask = interface;
  JFacebookRequestError = interface;
  JFacebookSdk = interface;
  JGraphResponse = interface;
  JGraphRequest_Callback = interface;
  JGraphRequest_GraphJSONArrayCallback = interface;
  JGraphRequest_GraphJSONObjectCallback = interface;
  JGraphRequest = interface;

  {*******************************************************}
  JALFacebookShareLinkDialogClass = interface(JObjectClass)
    ['{3FEB53AB-882F-4D64-8797-C7873A5FF84B}']
    {class} function canShow: boolean; cdecl;
    {class} procedure show(activity: JActivity; contentUrl: Jnet_Uri; quote: JString); cdecl;
  end;
  [JavaSignature('com/alcinoe/facebook/ALFacebookShareLinkDialog')]
  JALFacebookShareLinkDialog = interface(JObject)
    ['{CB1C33F4-A26F-4543-8899-63D25EF5C6E0}']
  end;
  TJALFacebookShareLinkDialog = class(TJavaGenericImport<JALFacebookShareLinkDialogClass, JALFacebookShareLinkDialog>) end;

  {*********************************************************}
  JFacebookExceptionClass = interface(JRuntimeExceptionClass)
    ['{BE0539B7-F533-4942-8B5E-C21399C1F8A7}']
    {class} function init: JFacebookException; cdecl; overload;
    {class} function init(throwable: JThrowable): JFacebookException; cdecl; overload;
    {class} function init(message: JString): JFacebookException; cdecl; overload;
    {class} function init(format: JString; throwable: JThrowable): JFacebookException; cdecl; overload;
  end;
  [JavaSignature('com/facebook/FacebookException')]
  JFacebookException = interface(JRuntimeException)
    ['{050DED2E-2BC2-4EA6-8165-9EEFA922E48F}']
  end;
  TJFacebookException = class(TJavaGenericImport<JFacebookExceptionClass, JFacebookException>) end;

  {********************************************}
  JFacebookCallbackClass = interface(IJavaClass)
    ['{CF9AADAB-E4A4-4DFA-B199-AD66604217E0}']
  end;
  [JavaSignature('com/facebook/FacebookCallback')]
  JFacebookCallback = interface(IJavaInstance)
    ['{E04C00BC-BA3C-4088-94CA-F4BDFF4F0C7B}']
    procedure onCancel; cdecl;
    procedure onError(error: JFacebookException); cdecl;
    procedure onSuccess(result: JObject); cdecl;
  end;
  TJFacebookCallback = class(TJavaGenericImport<JFacebookCallbackClass, JFacebookCallback>) end;

  {*****************************************************}
  JCallbackManager_FactoryClass = interface(JObjectClass)
    ['{A93E1F7B-1AFC-4A9A-95B9-C26F24DFFDA5}']
    {class} function create: JCallbackManager; cdecl;
    {class} function init: JCallbackManager_Factory; cdecl;
  end;
  [JavaSignature('com/facebook/CallbackManager$Factory')]
  JCallbackManager_Factory = interface(JObject)
    ['{BF95CD77-4899-48DD-9B76-ACB75E44A7C4}']
  end;
  TJCallbackManager_Factory = class(TJavaGenericImport<JCallbackManager_FactoryClass, JCallbackManager_Factory>) end;

  {*******************************************}
  JCallbackManagerClass = interface(IJavaClass)
    ['{FA96881F-CE8B-4339-88A6-A03A2A46406B}']
  end;
  [JavaSignature('com/facebook/CallbackManager')]
  JCallbackManager = interface(IJavaInstance)
    ['{57EBE17B-4A7C-4A08-A69E-782618D704DB}']
    function onActivityResult(requestCode: Integer; resultCode: Integer; data: JIntent): Boolean; cdecl;
  end;
  TJCallbackManager = class(TJavaGenericImport<JCallbackManagerClass, JCallbackManager>) end;

  {******************************************}
  JLoginManagerClass = interface(JObjectClass)
    ['{1D084EE4-CF75-4D4D-B2A8-C0A1AAE9369A}']
    {class} function getInstance: JLoginManager; cdecl;
  end;
  [JavaSignature('com/facebook/login/LoginManager')]
  JLoginManager = interface(JObject)
    ['{33506ADC-2297-4A95-B617-43C27D79BA12}']
    procedure logInWithPublishPermissions(activity: JActivity; permissions: JCollection); cdecl; overload;
    procedure logInWithPublishPermissions(fragment: JFragment; permissions: JCollection); cdecl; overload;
    procedure logInWithReadPermissions(fragment: JFragment; permissions: JCollection); cdecl; overload;
    procedure logInWithReadPermissions(activity: JActivity; permissions: JCollection); cdecl; overload;
    procedure logOut; cdecl;
    procedure registerCallback(callbackManager: JCallbackManager; callback: JFacebookCallback); cdecl;
  end;
  TJLoginManager = class(TJavaGenericImport<JLoginManagerClass, JLoginManager>) end;

  {*********************************************}
  JAccessTokenClass = interface(JParcelableClass)
    ['{D45F97E4-A070-459B-B7FF-63DB58993826}']
    {class} function _GetACCESS_TOKEN_KEY: JString; cdecl;
    {class} function _GetEXPIRES_IN_KEY: JString; cdecl;
    {class} function _GetUSER_ID_KEY: JString; cdecl;
    {class} function getCurrentAccessToken: JAccessToken; cdecl;
    {class} procedure setCurrentAccessToken(accessToken: JAccessToken); cdecl;
    {class} property ACCESS_TOKEN_KEY: JString read _GetACCESS_TOKEN_KEY;
    {class} property EXPIRES_IN_KEY: JString read _GetEXPIRES_IN_KEY;
    {class} property USER_ID_KEY: JString read _GetUSER_ID_KEY;
  end;
  [JavaSignature('com/facebook/AccessToken')]
  JAccessToken = interface(JParcelable)
    ['{8056F0A5-7707-44BA-9991-BAF2B464474F}']
    function describeContents: Integer; cdecl;
    function getApplicationId: JString; cdecl;
    function getDeclinedPermissions: JSet; cdecl;
    function getExpires: JDate; cdecl;
    function getLastRefresh: JDate; cdecl;
    function getPermissions: JSet; cdecl;
    function getToken: JString; cdecl;
    function getUserId: JString; cdecl;
    function isExpired: Boolean; cdecl;
  end;
  TJAccessToken = class(TJavaGenericImport<JAccessTokenClass, JAccessToken>) end;

  {*****************************************}
  JLoginResultClass = interface(JObjectClass)
    ['{E238A5E3-641C-4991-B576-C87A68797ADC}']
  end;
  [JavaSignature('com/facebook/login/LoginResult')]
  JLoginResult = interface(JObject)
    ['{35E02E47-4492-41D1-AF13-5485EE1A15F8}']
    function getAccessToken: JAccessToken; cdecl;
    function getRecentlyDeniedPermissions: JSet; cdecl;
    function getRecentlyGrantedPermissions: JSet; cdecl;
  end;
  TJLoginResult = class(TJavaGenericImport<JLoginResultClass, JLoginResult>) end;

  {**************************************}
  JHttpMethodClass = interface(JEnumClass)
    ['{169D72DB-480A-439B-B2E5-00F540752436}']
    {class} function _GetDELETE: JHttpMethod; cdecl;
    {class} function _GetGET: JHttpMethod; cdecl;
    {class} function _GetPOST: JHttpMethod; cdecl;
    {class} function valueOf(name: JString): JHttpMethod; cdecl;
    {class} function values: TJavaObjectArray<JHttpMethod>; cdecl;
    {class} property DELETE: JHttpMethod read _GetDELETE;
    {class} property GET: JHttpMethod read _GetGET;
    {class} property POST: JHttpMethod read _GetPOST;
  end;
  [JavaSignature('com/facebook/HttpMethod')]
  JHttpMethod = interface(JEnum)
    ['{3B28BFE6-FA42-4BD5-9E58-DE0B0AA25588}']
  end;
  TJHttpMethod = class(TJavaGenericImport<JHttpMethodClass, JHttpMethod>) end;

  {******************************************************}
  JGraphRequestAsyncTaskClass = interface(JAsyncTaskClass)
    ['{BA53014D-EC8F-4DB7-A4F0-3542262946CE}']
  end;
  [JavaSignature('com/facebook/GraphRequestAsyncTask')]
  JGraphRequestAsyncTask = interface(JAsyncTask)
    ['{5FF63611-D483-40BF-9C29-F86E63A76768}']
  end;
  TJGraphRequestAsyncTask = class(TJavaGenericImport<JGraphRequestAsyncTaskClass, JGraphRequestAsyncTask>) end;

  {******************************************************}
  JFacebookRequestErrorClass = interface(JParcelableClass)
    ['{692CE587-75C4-4D60-9A3E-310D061F8693}']
    {class} function _GetINVALID_ERROR_CODE: Integer; cdecl;
    {class} function _GetINVALID_HTTP_STATUS_CODE: Integer; cdecl;
    {class} property INVALID_ERROR_CODE: Integer read _GetINVALID_ERROR_CODE;
    {class} property INVALID_HTTP_STATUS_CODE: Integer read _GetINVALID_HTTP_STATUS_CODE;
  end;
  [JavaSignature('com/facebook/FacebookRequestError')]
  JFacebookRequestError = interface(JParcelable)
    ['{A2A8F82F-F83C-4032-8FAF-3483ECC3B14C}']
    function describeContents: Integer; cdecl;
    function getBatchRequestResult: JObject; cdecl;
    function getErrorCode: Integer; cdecl;
    function getErrorMessage: JString; cdecl;
    function getErrorRecoveryMessage: JString; cdecl;
    function getErrorType: JString; cdecl;
    function getErrorUserMessage: JString; cdecl;
    function getErrorUserTitle: JString; cdecl;
    function getException: JFacebookException; cdecl;
    function getRequestResult: JJSONObject; cdecl;
    function getRequestResultBody: JJSONObject; cdecl;
    function getRequestStatusCode: Integer; cdecl;
    function getSubErrorCode: Integer; cdecl;
  end;
  TJFacebookRequestError = class(TJavaGenericImport<JFacebookRequestErrorClass, JFacebookRequestError>) end;

  {*****************************************}
  JFacebookSdkClass = interface(JObjectClass)
    ['{0B655652-B6FA-45AD-BDF3-A357B5DDA839}']
    {class} function _GetAPPLICATION_ID_PROPERTY: JString; cdecl;
    {class} function _GetAPPLICATION_NAME_PROPERTY: JString; cdecl;
    {class} function _GetCLIENT_TOKEN_PROPERTY: JString; cdecl;
    {class} function _GetWEB_DIALOG_THEME: JString; cdecl;
    //{class} procedure addLoggingBehavior(behavior: JLoggingBehavior); cdecl;
    //{class} procedure clearLoggingBehaviors; cdecl;
    {class} function getApplicationContext: JContext; cdecl;
    {class} function getApplicationId: JString; cdecl;
    {class} function getApplicationName: JString; cdecl;
    {class} function getApplicationSignature(context: JContext): JString; cdecl;
    {class} function getCacheDir: JFile; cdecl;
    {class} function getCallbackRequestCodeOffset: Integer; cdecl;
    {class} function getClientToken: JString; cdecl;
    //{class} function getExecutor: JExecutor; cdecl;
    {class} function getFacebookDomain: JString; cdecl;
    {class} function getGraphApiVersion: JString; cdecl;
    //{class} function getLimitEventAndDataUsage(context: JContext): Boolean; cdecl;
    //{class} function getLoggingBehaviors: JSet; cdecl;
    {class} function getOnProgressThreshold: Int64; cdecl;
    {class} function getSdkVersion: JString; cdecl;
    {class} function isDebugEnabled: Boolean; cdecl;
    {class} function isFacebookRequestCode(requestCode: Integer): Boolean; cdecl;
    {class} function isInitialized: Boolean; cdecl;
    {class} function isLegacyTokenUpgradeSupported: Boolean; cdecl;
    //{class} function isLoggingBehaviorEnabled(behavior: JLoggingBehavior): Boolean; cdecl;
    //{class} procedure publishInstallAsync(context: JContext; applicationId: JString); cdecl;
    //{class} procedure removeLoggingBehavior(behavior: JLoggingBehavior); cdecl;
    {class} procedure sdkInitialize(applicationContext: JContext); cdecl; overload;
    //{class} procedure sdkInitialize(applicationContext: JContext; callback: JFacebookSdk_InitializeCallback); cdecl; overload;
    {class} procedure sdkInitialize(applicationContext: JContext; callbackRequestCodeOffset: Integer); cdecl; overload;
    //{class} procedure sdkInitialize(applicationContext: JContext; callbackRequestCodeOffset: Integer; callback: JFacebookSdk_InitializeCallback); cdecl; overload;
    {class} procedure setApplicationId(applicationId: JString); cdecl;
    {class} procedure setApplicationName(applicationName: JString); cdecl;
    {class} procedure setCacheDir(cacheDir: JFile); cdecl;
    {class} procedure setClientToken(clientToken: JString); cdecl;
    {class} procedure setExecutor(executor: JExecutor); cdecl;
    {class} procedure setFacebookDomain(facebookDomain: JString); cdecl;
    {class} procedure setGraphApiVersion(graphApiVersion: JString); cdecl;
    {class} procedure setIsDebugEnabled(enabled: Boolean); cdecl;
    {class} procedure setLegacyTokenUpgradeSupported(supported: Boolean); cdecl;
    {class} procedure setLimitEventAndDataUsage(context: JContext; limitEventUsage: Boolean); cdecl;
    {class} procedure setOnProgressThreshold(threshold: Int64); cdecl;
    {class} property APPLICATION_ID_PROPERTY: JString read _GetAPPLICATION_ID_PROPERTY;
    {class} property APPLICATION_NAME_PROPERTY: JString read _GetAPPLICATION_NAME_PROPERTY;
    {class} property CLIENT_TOKEN_PROPERTY: JString read _GetCLIENT_TOKEN_PROPERTY;
    {class} property WEB_DIALOG_THEME: JString read _GetWEB_DIALOG_THEME;
  end;
  [JavaSignature('com/facebook/FacebookSdk')]
  JFacebookSdk = interface(JObject)
    ['{F138CBE5-D161-4740-93F3-817FC2F2F4C5}']
  end;
  TJFacebookSdk = class(TJavaGenericImport<JFacebookSdkClass, JFacebookSdk>) end;

  {*******************************************}
  JGraphResponseClass = interface(JObjectClass)
    ['{2CF25531-06CA-4423-A804-2DAD41F6DEF4}']
    {class} function _GetNON_JSON_RESPONSE_PROPERTY: JString; cdecl;
    {class} function _GetSUCCESS_KEY: JString; cdecl;
    {class} property NON_JSON_RESPONSE_PROPERTY: JString read _GetNON_JSON_RESPONSE_PROPERTY;
    {class} property SUCCESS_KEY: JString read _GetSUCCESS_KEY;
  end;
  [JavaSignature('com/facebook/GraphResponse')]
  JGraphResponse = interface(JObject)
    ['{678C170B-168A-42DA-9CE6-E7BFB8325A19}']
    function getError: JFacebookRequestError; cdecl;
    function getJSONArray: JJSONArray; cdecl;
    function getJSONObject: JJSONObject; cdecl;
    function getRawResponse: JString; cdecl;
    function getRequest: JGraphRequest; cdecl;
  end;
  TJGraphResponse = class(TJavaGenericImport<JGraphResponseClass, JGraphResponse>) end;

  {*************************************************}
  JGraphRequest_CallbackClass = interface(IJavaClass)
    ['{5A6F68A1-95DF-43A5-8445-D1AE041ADAE2}']
  end;
  [JavaSignature('com/facebook/GraphRequest$Callback')]
  JGraphRequest_Callback = interface(IJavaInstance)
    ['{BE1CE1CB-5322-48F6-B162-5D1AC36177BF}']
    procedure onCompleted(response: JGraphResponse); cdecl;
  end;
  TJGraphRequest_Callback = class(TJavaGenericImport<JGraphRequest_CallbackClass, JGraphRequest_Callback>) end;

  {***************************************************************}
  JGraphRequest_GraphJSONArrayCallbackClass = interface(IJavaClass)
    ['{CF9973A9-8B24-47A4-8059-782813324F77}']
  end;
  [JavaSignature('com/facebook/GraphRequest$GraphJSONArrayCallback')]
  JGraphRequest_GraphJSONArrayCallback = interface(IJavaInstance)
    ['{3FFD5147-3AB8-475E-AA40-03C4B36D28D7}']
    procedure onCompleted(objects: JJSONArray; response: JGraphResponse); cdecl;
  end;
  TJGraphRequest_GraphJSONArrayCallback = class(TJavaGenericImport<JGraphRequest_GraphJSONArrayCallbackClass, JGraphRequest_GraphJSONArrayCallback>) end;

  {****************************************************************}
  JGraphRequest_GraphJSONObjectCallbackClass = interface(IJavaClass)
    ['{AD9164C8-F6AF-410A-92B4-AF5D65373228}']
  end;
  [JavaSignature('com/facebook/GraphRequest$GraphJSONObjectCallback')]
  JGraphRequest_GraphJSONObjectCallback = interface(IJavaInstance)
    ['{50212943-589E-44A6-87CB-96801BCD6D1A}']
    procedure onCompleted(&object: JJSONObject; response: JGraphResponse); cdecl;
  end;
  TJGraphRequest_GraphJSONObjectCallback = class(TJavaGenericImport<JGraphRequest_GraphJSONObjectCallbackClass, JGraphRequest_GraphJSONObjectCallback>) end;

  {******************************************}
  JGraphRequestClass = interface(JObjectClass)
    ['{375A8EB4-BF56-4DC7-9F56-BF05A7E7F230}']
    {class} function init: JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod; callback: JGraphRequest_Callback; version: JString): JGraphRequest; cdecl; overload;
  end;
  [JavaSignature('com/facebook/GraphRequest')]
  JGraphRequest = interface(JObject)
    ['{5EEAEEF3-71DD-43D9-ACDC-A26212714DB7}']
    function executeAsync: JGraphRequestAsyncTask; cdecl;
  end;
  TJGraphRequest = class(TJavaGenericImport<JGraphRequestClass, JGraphRequest>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALFacebookShareLinkDialog', TypeInfo(ALAndroidFacebookApi.JALFacebookShareLinkDialog));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JFacebookException', TypeInfo(ALAndroidFacebookApi.JFacebookException));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JFacebookCallback', TypeInfo(ALAndroidFacebookApi.JFacebookCallback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JCallbackManager', TypeInfo(ALAndroidFacebookApi.JCallbackManager));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JCallbackManager_Factory', TypeInfo(ALAndroidFacebookApi.JCallbackManager_Factory));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JLoginManager', TypeInfo(ALAndroidFacebookApi.JLoginManager));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JAccessToken', TypeInfo(ALAndroidFacebookApi.JAccessToken));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JLoginResult', TypeInfo(ALAndroidFacebookApi.JLoginResult));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JHttpMethod', TypeInfo(ALAndroidFacebookApi.JHttpMethod));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequestAsyncTask', TypeInfo(ALAndroidFacebookApi.JGraphRequestAsyncTask));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JFacebookRequestError', TypeInfo(ALAndroidFacebookApi.JFacebookRequestError));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JFacebookSdk', TypeInfo(ALAndroidFacebookApi.JFacebookSdk));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphResponse', TypeInfo(ALAndroidFacebookApi.JGraphResponse));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_Callback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_Callback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_GraphJSONArrayCallback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_GraphJSONArrayCallback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_GraphJSONObjectCallback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_GraphJSONObjectCallback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest', TypeInfo(ALAndroidFacebookApi.JGraphRequest));
end;

initialization
  RegisterTypes;

end.
