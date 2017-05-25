unit ALAndroidFacebookApi;

interface

uses Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.Os,
     Androidapi.JNI.Net,
     Androidapi.JNI.App,
     ALAndroidApi;

type

  {*******************************************************}
  JALFacebookDeferredAppLinkDataResultListener = interface;
  JALFacebookDeferredAppLinkDataResult = interface;
  JALFacebookAppInviteDialog = interface;
  JALFacebookShareLinkDialog = interface;
  JAppEventsLogger = interface;
  JAppEventsLogger_FlushBehavior = interface;
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
  JGraphResponse = interface;
  JGraphRequest_Callback = interface;
  JGraphRequest_GraphJSONArrayCallback = interface;
  JGraphRequest_GraphJSONObjectCallback = interface;
  JGraphRequest = interface;


  {***********************************************************************}
  JALFacebookDeferredAppLinkDataResultListenerClass = interface(IJavaClass)
    ['{8D59B4C6-72BB-4662-A3D9-64B5B56292A7}']
  end;

  {*********************************************************************************}
  [JavaSignature('com/alcinoe/facebook/ALFacebookDeferredAppLinkDataResultListener')]
  JALFacebookDeferredAppLinkDataResultListener = interface(IJavaInstance)
    ['{FA5B9F3F-4630-4BB2-B0D2-97F82B617B13}']
    procedure onError(code: integer); cdecl;
    procedure onSuccess(targetUri: JString; promotionCode: JString); cdecl;
  end;
  TJALFacebookDeferredAppLinkDataResultListener = class(TJavaGenericImport<JALFacebookDeferredAppLinkDataResultListenerClass, JALFacebookDeferredAppLinkDataResultListener>) end;

  {*****************************************************************}
  JALFacebookDeferredAppLinkDataResultClass = interface(JObjectClass)
    ['{5F4F6814-A3B6-407F-9C48-C16593B3AC44}']
    {class} function init(activity: JActivity): JALFacebookDeferredAppLinkDataResult; cdecl;
  end;

  {*************************************************************************}
  [JavaSignature('com/alcinoe/facebook/ALFacebookDeferredAppLinkDataResult')]
  JALFacebookDeferredAppLinkDataResult = interface(JObject)
    ['{277D9137-4EB4-46EC-BDBE-F94AAB6BB76C}']
    procedure retrieve; cdecl;
    procedure setListener(listener: JALFacebookDeferredAppLinkDataResultListener); cdecl;
  end;
  TJALFacebookDeferredAppLinkDataResult = class(TJavaGenericImport<JALFacebookDeferredAppLinkDataResultClass, JALFacebookDeferredAppLinkDataResult>) end;

  {*******************************************************}
  JALFacebookAppInviteDialogClass = interface(JObjectClass)
    ['{2C98A58F-8F6D-4E16-A163-9173424DFBE1}']
    {class} function canShow: boolean; cdecl;
    {class} procedure show(activity: JActivity; applinkUrl: JString; promotionText: JString; promotionCode: JString; previewImageUrl: JString); cdecl;
  end;

  {***************************************************************}
  [JavaSignature('com/alcinoe/facebook/ALFacebookAppInviteDialog')]
  JALFacebookAppInviteDialog = interface(JObject)
    ['{CB1C33F4-A26F-4543-8899-63D25EF5C6E0}']
  end;
  TJALFacebookAppInviteDialog = class(TJavaGenericImport<JALFacebookAppInviteDialogClass, JALFacebookAppInviteDialog>) end;

  {*******************************************************}
  JALFacebookShareLinkDialogClass = interface(JObjectClass)
    ['{3FEB53AB-882F-4D64-8797-C7873A5FF84B}']
    {class} function canShow: boolean; cdecl;
    {class} procedure show(activity: JActivity; contentUrl: Jnet_Uri; contentTitle: JString; contentDescription: JString; imageUrl: Jnet_Uri; quote: JString); cdecl;
  end;

  {***************************************************************}
  [JavaSignature('com/alcinoe/facebook/ALFacebookShareLinkDialog')]
  JALFacebookShareLinkDialog = interface(JObject)
    ['{CB1C33F4-A26F-4543-8899-63D25EF5C6E0}']
  end;
  TJALFacebookShareLinkDialog = class(TJavaGenericImport<JALFacebookShareLinkDialogClass, JALFacebookShareLinkDialog>) end;

  {*********************************************************}
  JAppEventsLogger_FlushBehaviorClass = interface(JEnumClass)
    ['{DB2B7FEC-CEFC-471F-9972-CC960064EC40}']
    {class} function _GetAUTO: JAppEventsLogger_FlushBehavior; cdecl;
    {class} function _GetEXPLICIT_ONLY: JAppEventsLogger_FlushBehavior; cdecl;
    {class} function valueOf(P1: JString): JAppEventsLogger_FlushBehavior; cdecl;
    {class} function values: TJavaObjectArray<JAppEventsLogger_FlushBehavior>; cdecl;
    {class} property AUTO: JAppEventsLogger_FlushBehavior read _GetAUTO;
    {class} property EXPLICIT_ONLY: JAppEventsLogger_FlushBehavior read _GetEXPLICIT_ONLY;
  end;

  {*********************************************************************}
  [JavaSignature('com/facebook/appevents/AppEventsLogger$FlushBehavior')]
  JAppEventsLogger_FlushBehavior = interface(JEnum)
    ['{3795CBAA-446A-40CA-890E-02EA4DEDC55A}']
  end;
  TJAppEventsLogger_FlushBehavior = class(TJavaGenericImport<JAppEventsLogger_FlushBehaviorClass, JAppEventsLogger_FlushBehavior>) end;

  {*********************************************}
  JAppEventsLoggerClass = interface(JObjectClass)
    ['{5553EE80-7B53-4614-85D7-108049A51490}']
    {class} function _GetAPP_EVENT_PREFERENCES: JString; cdecl;
    {class} function _GetACTION_APP_EVENTS_FLUSHED: JString; cdecl;
    {class} function _GetAPP_EVENTS_EXTRA_FLUSH_RESULT: JString; cdecl;
    {class} function _GetAPP_EVENTS_EXTRA_NUM_EVENTS_FLUSHED: JString; cdecl;
    {class} procedure activateApp(application: JApplication); cdecl; overload;
    {class} procedure activateApp(application: JApplication; applicationId: JString); cdecl; overload;
    {class} procedure activateApp(context: JContext); cdecl; overload; //Deprecated
    {class} procedure activateApp(context: JContext; applicationId: JString); cdecl; overload; //Deprecated
    {class} procedure deactivateApp(context: JContext); cdecl; overload; //Deprecated
    {class} procedure deactivateApp(context: JContext; applicationId: JString); cdecl; overload; //Deprecated
    {class} function newLogger(context: JContext): JAppEventsLogger; cdecl; overload;
    {class} //function newLogger(context: JContext; accessToken: JAccessToken): JAppEventsLogger; cdecl; overload;
    {class} //function newLogger(context: JContext; applicationId: JString; accessToken: JAccessToken): JAppEventsLogger; cdecl; overload;
    {class} function newLogger(context: JContext; applicationId: JString): JAppEventsLogger; cdecl; overload;
    {class} function getFlushBehavior: JAppEventsLogger_FlushBehavior; cdecl;
    {class} procedure setFlushBehavior(flushBehavior: JAppEventsLogger_FlushBehavior); cdecl;
    {class} procedure onContextStop; cdecl;
    {class} procedure setPushNotificationsRegistrationId(registrationId: JString); cdecl;
    {class} function getPushNotificationsRegistrationId: JString; cdecl;
    {class} procedure setUserID(userID: JString); cdecl;
    {class} function getUserID: JString; cdecl;
    {class} procedure clearUserID; cdecl;
    {class} //procedure updateUserProperties(parameters: JBundle; callback: JGraphRequest_Callback); cdecl; overload;
    {class} //procedure updateUserProperties(parameters: JBundle; applicationID: JString; callback: JGraphRequest_Callback); cdecl; overload;
    {class} procedure eagerFlush; cdecl;
    {class} procedure setSourceApplication(applicationPackage: JString; openByAppLink: Boolean); cdecl;
    {class} function getSourceApplication: JString; cdecl;
    {class} procedure resetSourceApplication; cdecl;
    {class} function getAnalyticsExecutor: JExecutor; cdecl;
    {class} function getAnonymousAppDeviceGUID(context: JContext): JString; cdecl;
    {class} property APP_EVENT_PREFERENCES: JString read _GetAPP_EVENT_PREFERENCES;
    {class} property ACTION_APP_EVENTS_FLUSHED: JString read _GetACTION_APP_EVENTS_FLUSHED;
    {class} property APP_EVENTS_EXTRA_FLUSH_RESULT: JString read _GetAPP_EVENTS_EXTRA_FLUSH_RESULT;
    {class} property APP_EVENTS_EXTRA_NUM_EVENTS_FLUSHED: JString read _GetAPP_EVENTS_EXTRA_NUM_EVENTS_FLUSHED;
  end;

  {*******************************************************}
  [JavaSignature('com/facebook/appevents/AppEventsLogger')]
  JAppEventsLogger = interface(JObject)
    ['{D4657D58-85BE-4DB5-8D8D-CEE27798E98E}']
    procedure logEvent(eventName: JString); cdecl; overload;
    procedure logEvent(eventName: JString; valueToSum: Double); cdecl; overload;
    procedure logEvent(eventName: JString; parameters: JBundle); cdecl; overload;
    procedure logEvent(eventName: JString; valueToSum: Double; parameters: JBundle); cdecl; overload;
    //procedure logPurchase(purchaseAmount: JBigDecimal; currency: JCurrency); cdecl; overload;
    //procedure logPurchase(purchaseAmount: JBigDecimal; currency: JCurrency; parameters: JBundle); cdecl; overload;
    procedure logPushNotificationOpen(payload: JBundle); cdecl; overload;
    procedure logPushNotificationOpen(payload: JBundle; action: JString); cdecl; overload;
    procedure flush; cdecl;
    //function isValidForAccessToken(accessToken: JAccessToken): Boolean; cdecl;
    procedure logSdkEvent(eventName: JString; valueToSum: JDouble; parameters: JBundle); cdecl;
    function getApplicationId: JString; cdecl;
  end;
  TJAppEventsLogger = class(TJavaGenericImport<JAppEventsLoggerClass, JAppEventsLogger>) end;

  {*********************************************************}
  JFacebookExceptionClass = interface(JRuntimeExceptionClass)
    ['{BE0539B7-F533-4942-8B5E-C21399C1F8A7}']
    {class} function init: JFacebookException; cdecl; overload;
    {class} function init(message: JString): JFacebookException; cdecl; overload;
    {class} function init(format: JString; throwable: JThrowable): JFacebookException; cdecl; overload;
    {class} function init(throwable: JThrowable): JFacebookException; cdecl; overload;
    {class} function toString: JString; cdecl;
  end;

  {***********************************************}
  [JavaSignature('com/facebook/FacebookException')]
  JFacebookException = interface(JRuntimeException)
    ['{050DED2E-2BC2-4EA6-8165-9EEFA922E48F}']
  end;
  TJFacebookException = class(TJavaGenericImport<JFacebookExceptionClass, JFacebookException>) end;

  {********************************************}
  JFacebookCallbackClass = interface(IJavaClass)
    ['{CF9AADAB-E4A4-4DFA-B199-AD66604217E0}']
  end;

  {**********************************************}
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

  {*****************************************************}
  [JavaSignature('com/facebook/CallbackManager$Factory')]
  JCallbackManager_Factory = interface(JObject)
    ['{BF95CD77-4899-48DD-9B76-ACB75E44A7C4}']
  end;
  TJCallbackManager_Factory = class(TJavaGenericImport<JCallbackManager_FactoryClass, JCallbackManager_Factory>) end;

  {*******************************************}
  JCallbackManagerClass = interface(IJavaClass)
    ['{FA96881F-CE8B-4339-88A6-A03A2A46406B}']
  end;

  {*********************************************}
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

  {************************************************}
  [JavaSignature('com/facebook/login/LoginManager')]
  JLoginManager = interface(JObject)
    ['{33506ADC-2297-4A95-B617-43C27D79BA12}']
    //procedure resolveError(activity: JActivity; response: JGraphResponse); cdecl; overload;
    //procedure resolveError(fragment: JFragment; response: JGraphResponse); cdecl; overload;
    //procedure resolveError(fragment: Japp_Fragment; response: JGraphResponse); cdecl; overload;
    procedure registerCallback(callbackManager: JCallbackManager; callback: JFacebookCallback); cdecl;
    //function getLoginBehavior: JLoginBehavior; cdecl;
    //function setLoginBehavior(loginBehavior: JLoginBehavior): JLoginManager; cdecl;
    //function getDefaultAudience: JDefaultAudience; cdecl;
    //function setDefaultAudience(defaultAudience: JDefaultAudience): JLoginManager; cdecl;
    procedure logOut; cdecl;
    procedure logInWithReadPermissions(fragment: JFragment; permissions: JCollection); cdecl; overload;
    //procedure logInWithReadPermissions(fragment: Japp_Fragment; permissions: JCollection); cdecl; overload;
    procedure logInWithReadPermissions(activity: JActivity; permissions: JCollection); cdecl; overload;
    procedure logInWithPublishPermissions(fragment: JFragment; permissions: JCollection); cdecl; overload;
    //procedure logInWithPublishPermissions(fragment: Japp_Fragment; permissions: JCollection); cdecl; overload;
    procedure logInWithPublishPermissions(activity: JActivity; permissions: JCollection); cdecl; overload;
  end;
  TJLoginManager = class(TJavaGenericImport<JLoginManagerClass, JLoginManager>) end;

  {*********************************************}
  JAccessTokenClass = interface(JParcelableClass)
    ['{D45F97E4-A070-459B-B7FF-63DB58993826}']
    {class} function _GetACCESS_TOKEN_KEY: JString; cdecl;
    {class} function _GetEXPIRES_IN_KEY: JString; cdecl;
    {class} function _GetUSER_ID_KEY: JString; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} //function init(accessToken: JString;
            //              applicationId: JString;
            //              userId: JString;
            //              permissions: JCollection;
            //              declinedPermissions: JCollection;
            //              accessTokenSource: JAccessTokenSource;
            //              expirationTime: JDate;
            //              lastRefreshTime: JDate): JAccessToken; cdecl;
    {class} function getCurrentAccessToken: JAccessToken; cdecl;
    {class} procedure setCurrentAccessToken(accessToken: JAccessToken); cdecl;
    {class} procedure refreshCurrentAccessTokenAsync; cdecl; overload;
    {class} //procedure refreshCurrentAccessTokenAsync(callback: JAccessToken_AccessTokenRefreshCallback); cdecl; overload;
    {class} //procedure createFromNativeLinkingIntent(intent: JIntent; applicationId: JString; accessTokenCallback: JAccessToken_AccessTokenCreationCallback); cdecl;
    {class} property ACCESS_TOKEN_KEY: JString read _GetACCESS_TOKEN_KEY;
    {class} property EXPIRES_IN_KEY: JString read _GetEXPIRES_IN_KEY;
    {class} property USER_ID_KEY: JString read _GetUSER_ID_KEY;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  {*****************************************}
  [JavaSignature('com/facebook/AccessToken')]
  JAccessToken = interface(JParcelable)
    ['{8056F0A5-7707-44BA-9991-BAF2B464474F}']
    function getToken: JString; cdecl;
    function getExpires: JDate; cdecl;
    function getPermissions: JSet; cdecl;
    function getDeclinedPermissions: JSet; cdecl;
    //function getSource: JAccessTokenSource; cdecl;
    function getLastRefresh: JDate; cdecl;
    function getApplicationId: JString; cdecl;
    function getUserId: JString; cdecl;
    function isExpired: Boolean; cdecl;
    function describeContents: Integer; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJAccessToken = class(TJavaGenericImport<JAccessTokenClass, JAccessToken>) end;

  {*****************************************}
  JLoginResultClass = interface(JObjectClass)
    ['{E238A5E3-641C-4991-B576-C87A68797ADC}']
    {class} function init(accessToken: JAccessToken; recentlyGrantedPermissions: JSet; recentlyDeniedPermissions: JSet): JLoginResult; cdecl;
  end;

  {***********************************************}
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

  {****************************************}
  [JavaSignature('com/facebook/HttpMethod')]
  JHttpMethod = interface(JEnum)
    ['{3B28BFE6-FA42-4BD5-9E58-DE0B0AA25588}']
  end;
  TJHttpMethod = class(TJavaGenericImport<JHttpMethodClass, JHttpMethod>) end;

  {******************************************************}
  JGraphRequestAsyncTaskClass = interface(JAsyncTaskClass)
    ['{BA53014D-EC8F-4DB7-A4F0-3542262946CE}']
            //public GraphRequestAsyncTask(GraphRequest requests)
    {class} //function init(requests: JCollection): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function init(requests: JGraphRequestBatch): JGraphRequestAsyncTask; cdecl; overload;
            //public GraphRequestAsyncTask(HttpURLConnection connection, GraphRequest requests)
    {class} //function init(connection: JHttpURLConnection; requests: JCollection): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function init(connection: JHttpURLConnection; requests: JGraphRequestBatch): JGraphRequestAsyncTask; cdecl; overload;
  end;

  {***************************************************}
  [JavaSignature('com/facebook/GraphRequestAsyncTask')]
  JGraphRequestAsyncTask = interface(JAsyncTask)
    ['{5FF63611-D483-40BF-9C29-F86E63A76768}']
  end;
  TJGraphRequestAsyncTask = class(TJavaGenericImport<JGraphRequestAsyncTaskClass, JGraphRequestAsyncTask>) end;

  {******************************************************}
  JFacebookRequestErrorClass = interface(JParcelableClass)
    ['{692CE587-75C4-4D60-9A3E-310D061F8693}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetINVALID_ERROR_CODE: Integer; cdecl;
    {class} function _GetINVALID_HTTP_STATUS_CODE: Integer; cdecl;
    {class} function init(errorCode: Integer; errorType: JString; errorMessage: JString): JFacebookRequestError; cdecl; overload;
    {class} property INVALID_ERROR_CODE: Integer read _GetINVALID_ERROR_CODE;
    {class} property INVALID_HTTP_STATUS_CODE: Integer read _GetINVALID_HTTP_STATUS_CODE;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  {**************************************************}
  [JavaSignature('com/facebook/FacebookRequestError')]
  JFacebookRequestError = interface(JParcelable)
    ['{A2A8F82F-F83C-4032-8FAF-3483ECC3B14C}']
    //function getCategory: JFacebookRequestError_Category; cdecl;
    function getRequestStatusCode: Integer; cdecl;
    function getErrorCode: Integer; cdecl;
    function getSubErrorCode: Integer; cdecl;
    function getErrorType: JString; cdecl;
    function getErrorMessage: JString; cdecl;
    function getErrorRecoveryMessage: JString; cdecl;
    function getErrorUserMessage: JString; cdecl;
    function getErrorUserTitle: JString; cdecl;
    function getRequestResultBody: JJSONObject; cdecl;
    function getRequestResult: JJSONObject; cdecl;
    function getBatchRequestResult: JObject; cdecl;
    //function getConnection: JHttpURLConnection; cdecl;
    function getException: JFacebookException; cdecl;
    //public void writeToParcel(Parcel out, int flags)
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    function describeContents: Integer; cdecl;
  end;
  TJFacebookRequestError = class(TJavaGenericImport<JFacebookRequestErrorClass, JFacebookRequestError>) end;

  {*******************************************}
  JGraphResponseClass = interface(JObjectClass)
    ['{2CF25531-06CA-4423-A804-2DAD41F6DEF4}']
    {class} function _GetNON_JSON_RESPONSE_PROPERTY: JString; cdecl;
    {class} function _GetSUCCESS_KEY: JString; cdecl;
    {class} property NON_JSON_RESPONSE_PROPERTY: JString read _GetNON_JSON_RESPONSE_PROPERTY;
    {class} property SUCCESS_KEY: JString read _GetSUCCESS_KEY;
  end;

  {*******************************************}
  [JavaSignature('com/facebook/GraphResponse')]
  JGraphResponse = interface(JObject)
    ['{678C170B-168A-42DA-9CE6-E7BFB8325A19}']
    function getError: JFacebookRequestError; cdecl;
    function getJSONObject: JJSONObject; cdecl;
    function getJSONArray: JJSONArray; cdecl;
    //function getConnection: JHttpURLConnection; cdecl;
    function getRequest: JGraphRequest; cdecl;
    function getRawResponse: JString; cdecl;
    //function getRequestForPagedResults(direction: JGraphResponse_PagingDirection): JGraphRequest; cdecl;
  end;
  TJGraphResponse = class(TJavaGenericImport<JGraphResponseClass, JGraphResponse>) end;

  {*************************************************}
  JGraphRequest_CallbackClass = interface(IJavaClass)
    ['{5A6F68A1-95DF-43A5-8445-D1AE041ADAE2}']
  end;

  {***************************************************}
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

  {*****************************************************************}
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

  {******************************************************************}
  [JavaSignature('com/facebook/GraphRequest$GraphJSONObjectCallback')]
  JGraphRequest_GraphJSONObjectCallback = interface(IJavaInstance)
    ['{50212943-589E-44A6-87CB-96801BCD6D1A}']
    procedure onCompleted(&object: JJSONObject; response: JGraphResponse); cdecl;
  end;
  TJGraphRequest_GraphJSONObjectCallback = class(TJavaGenericImport<JGraphRequest_GraphJSONObjectCallbackClass, JGraphRequest_GraphJSONObjectCallback>) end;

  {******************************************}
  JGraphRequestClass = interface(JObjectClass)
    ['{375A8EB4-BF56-4DC7-9F56-BF05A7E7F230}']
    {class} function _GetACCESS_TOKEN_PARAM: JString; cdecl;
    {class} function _GetFIELDS_PARAM: JString; cdecl;
    {class} function _GetMAXIMUM_BATCH_SIZE: Integer; cdecl;
    {class} function _GetTAG: JString; cdecl;
    {class} function init: JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function init(accessToken: JAccessToken; graphPath: JString; parameters: JBundle; httpMethod: JHttpMethod; callback: JGraphRequest_Callback; version: JString): JGraphRequest; cdecl; overload;
    {class} function newDeleteObjectRequest(accessToken: JAccessToken; id: JString; callback: JGraphRequest_Callback): JGraphRequest; cdecl;
    {class} function newMeRequest(accessToken: JAccessToken; callback: JGraphRequest_GraphJSONObjectCallback): JGraphRequest; cdecl;
    {class} function newPostRequest(accessToken: JAccessToken; graphPath: JString; graphObject: JJSONObject; callback: JGraphRequest_Callback): JGraphRequest; cdecl;
    {class} function newMyFriendsRequest(accessToken: JAccessToken; callback: JGraphRequest_GraphJSONArrayCallback): JGraphRequest; cdecl;
    {class} function newGraphPathRequest(accessToken: JAccessToken; graphPath: JString; callback: JGraphRequest_Callback): JGraphRequest; cdecl;
    {class} //function newPlacesSearchRequest(accessToken: JAccessToken; location: JLocation; radiusInMeters: Integer; resultsLimit: Integer; searchText: JString; callback: JGraphRequest_GraphJSONArrayCallback): JGraphRequest; cdecl;
    {class} function newUploadPhotoRequest(accessToken: JAccessToken; graphPath: JString; image: JBitmap; caption: JString; params: JBundle; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function newUploadPhotoRequest(accessToken: JAccessToken; graphPath: JString; &file: JFile; caption: JString; params: JBundle; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function newUploadPhotoRequest(accessToken: JAccessToken; graphPath: JString; photoUri: Jnet_Uri; caption: JString; params: JBundle; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function newCustomAudienceThirdPartyIdRequest(accessToken: JAccessToken; context: JContext; applicationId: JString; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function newCustomAudienceThirdPartyIdRequest(accessToken: JAccessToken; context: JContext; callback: JGraphRequest_Callback): JGraphRequest; cdecl; overload;
    {class} function getDefaultBatchApplicationId: JString; cdecl;
    {class} procedure setDefaultBatchApplicationId(applicationId: JString); cdecl;
            //public static HttpURLConnection toHttpConnection(GraphRequest requests)
    {class} //function toHttpConnection(requests: JGraphRequestBatch): JHttpURLConnection; cdecl; overload;
    {class} //function toHttpConnection(requests: JCollection): JHttpURLConnection; cdecl; overload;
    {class} function executeAndWait(request: JGraphRequest): JGraphResponse; cdecl; overload;
            //public static List executeBatchAndWait(GraphRequest requests)
    {class} //function executeBatchAndWait(requests: JGraphRequestBatch): JList; cdecl; overload;
    {class} function executeBatchAndWait(requests: JCollection): JList; cdecl; overload;
            //public static GraphRequestAsyncTask executeBatchAsync(GraphRequest requests)
    {class} //function executeBatchAsync(requests: JGraphRequestBatch): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function executeBatchAsync(requests: JCollection): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function executeConnectionAndWait(connection: JHttpURLConnection; requests: JCollection): JList; cdecl; overload;
    {class} //function executeConnectionAndWait(connection: JHttpURLConnection; requests: JGraphRequestBatch): JList; cdecl; overload;
    {class} //function executeConnectionAsync(connection: JHttpURLConnection; requests: JGraphRequestBatch): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function executeConnectionAsync(callbackHandler: JHandler; connection: JHttpURLConnection; requests: JGraphRequestBatch): JGraphRequestAsyncTask; cdecl; overload;
    {class} //function createOpenGraphObject(openGraphObject: JShareOpenGraphObject): JGraphRequest; cdecl;
    {class} property ACCESS_TOKEN_PARAM: JString read _GetACCESS_TOKEN_PARAM;
    {class} property FIELDS_PARAM: JString read _GetFIELDS_PARAM;
    {class} property MAXIMUM_BATCH_SIZE: Integer read _GetMAXIMUM_BATCH_SIZE;
    {class} property TAG: JString read _GetTAG;
  end;

  {******************************************}
  [JavaSignature('com/facebook/GraphRequest')]
  JGraphRequest = interface(JObject)
    ['{5EEAEEF3-71DD-43D9-ACDC-A26212714DB7}']
    function getGraphObject: JJSONObject; cdecl;
    procedure setGraphObject(graphObject: JJSONObject); cdecl;
    function getGraphPath: JString; cdecl;
    procedure setGraphPath(graphPath: JString); cdecl;
    function getHttpMethod: JHttpMethod; cdecl;
    procedure setHttpMethod(httpMethod: JHttpMethod); cdecl;
    function getVersion: JString; cdecl;
    procedure setVersion(version: JString); cdecl;
    procedure setSkipClientToken(skipClientToken: Boolean); cdecl;
    function getParameters: JBundle; cdecl;
    procedure setParameters(parameters: JBundle); cdecl;
    function getAccessToken: JAccessToken; cdecl;
    procedure setAccessToken(accessToken: JAccessToken); cdecl;
    function getBatchEntryName: JString; cdecl;
    procedure setBatchEntryName(batchEntryName: JString); cdecl;
    function getBatchEntryDependsOn: JString; cdecl;
    procedure setBatchEntryDependsOn(batchEntryDependsOn: JString); cdecl;
    function getBatchEntryOmitResultOnSuccess: Boolean; cdecl;
    procedure setBatchEntryOmitResultOnSuccess(P1: Boolean); cdecl;
    function getCallback: JGraphRequest_Callback; cdecl;
    procedure setCallback(callback: JGraphRequest_Callback); cdecl;
    procedure setTag(tag: JObject); cdecl;
    function getTag: JObject; cdecl;
    function executeAndWait: JGraphResponse; cdecl; overload;
    function executeAsync: JGraphRequestAsyncTask; cdecl;
  end;
  TJGraphRequest = class(TJavaGenericImport<JGraphRequestClass, JGraphRequest>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALAppInviteInvitationResultListener', TypeInfo(ALAndroidFacebookApi.JALFacebookDeferredAppLinkDataResultListener));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALAppInviteInvitationResult', TypeInfo(ALAndroidFacebookApi.JALFacebookDeferredAppLinkDataResult));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALFacebookAppInviteDialog', TypeInfo(ALAndroidFacebookApi.JALFacebookAppInviteDialog));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALFacebookShareLinkDialog', TypeInfo(ALAndroidFacebookApi.JALFacebookShareLinkDialog));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JAppEventsLogger', TypeInfo(ALAndroidFacebookApi.JAppEventsLogger));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JAppEventsLogger_FlushBehavior', TypeInfo(ALAndroidFacebookApi.JAppEventsLogger_FlushBehavior));
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
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphResponse', TypeInfo(ALAndroidFacebookApi.JGraphResponse));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_Callback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_Callback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_GraphJSONArrayCallback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_GraphJSONArrayCallback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest_GraphJSONObjectCallback', TypeInfo(ALAndroidFacebookApi.JGraphRequest_GraphJSONObjectCallback));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JGraphRequest', TypeInfo(ALAndroidFacebookApi.JGraphRequest));
end;

initialization
  RegisterTypes;

end.
