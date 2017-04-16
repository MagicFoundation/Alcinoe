unit ALAndroidFacebookApi;

interface

uses Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.Os,
     Androidapi.JNI.Net,
     Androidapi.JNI.App;

type

  {*******************************************************}
  JALFacebookDeferredAppLinkDataResultListener = interface;
  JALFacebookDeferredAppLinkDataResult = interface;
  JALFacebookAppInviteDialog = interface;
  JALFacebookShareLinkDialog = interface;
  JAppEventsLogger = interface;
  JAppEventsLogger_FlushBehavior = interface;

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

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALAppInviteInvitationResultListener', TypeInfo(ALAndroidFacebookApi.JALFacebookDeferredAppLinkDataResultListener));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALAppInviteInvitationResult', TypeInfo(ALAndroidFacebookApi.JALFacebookDeferredAppLinkDataResult));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALFacebookAppInviteDialog', TypeInfo(ALAndroidFacebookApi.JALFacebookAppInviteDialog));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JALFacebookShareLinkDialog', TypeInfo(ALAndroidFacebookApi.JALFacebookShareLinkDialog));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JAppEventsLogger', TypeInfo(ALAndroidFacebookApi.JAppEventsLogger));
  TRegTypes.RegisterType('ALAndroidFacebookApi.JAppEventsLogger_FlushBehavior', TypeInfo(ALAndroidFacebookApi.JAppEventsLogger_FlushBehavior));
end;

initialization
  RegisterTypes;

end.
