unit ALAndroidapi;

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
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.Accounts,
     Androidapi.JNI.Os,
     Androidapi.JNI.Net,
     Androidapi.JNI.App;

type

  {*******************************}
  JALSoftInputListener = interface;
  JALKeyPreImeListener = interface;
  JALEditText = interface;
  JALControlHostLayout = interface;
  JALLog = interface;
  JALStatFs = interface;
  JALAppInviteInvitation = interface;
  JALAppInviteInvitation_IntentBuilder = interface;
  JALFaceBookAppInvite = interface;
  JALFacebookAppEventsLogger = interface;
  JALFacebookAppEventsLogger_FlushBehavior = interface;

  {**********************************************}
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
  JALLogClass = interface(JObjectClass)
    ['{E4B8D3E7-409F-41E4-A7A6-9E011CD9B87E}']
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetASSERT: Integer; cdecl;
    {class} function init: JALLog; cdecl;
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
  JALLog = interface(JObject)
    ['{AED82B19-8B1E-4F35-85D9-851D6F1B4F54}']
  end;
  TJALLog = class(TJavaGenericImport<JALLogClass, JALLog>) end;

  {***********************************}
  JALStatFsClass = interface(JObjectClass)
    ['{E5587205-C324-4FAF-A101-E31BCD83BD4D}']
    {class} function init(path: JString): JALStatFs; cdecl; // public StatFs(String path)
  end;

  {**********************************}
  [JavaSignature('android/os/StatFs')]
  JALStatFs = interface(JObject)
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
  TJALStatFs = class(TJavaGenericImport<JALStatFsClass, JALStatFs>) end;

  {***************************************************}
  JALAppInviteInvitationClass = interface(JObjectClass)
    ['{34D1F50D-8EF5-4374-9B5F-384E93E3BF92}']
    {class} function init: JALAppInviteInvitation; cdecl;
  end;

  {*********************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteInvitation')]
  JALAppInviteInvitation = interface(JObject)
    ['{5E6EAC6E-85F3-45F6-8E46-3627ED77587E}']
  end;
  TJALAppInviteInvitation = class(TJavaGenericImport<JALAppInviteInvitationClass, JALAppInviteInvitation>) end;

  {***************************************************************}
  JALAppInviteInvitation_IntentBuilderClass = interface(JObjectClass)
    ['{E131213D-5067-4E24-AB74-F9ECBD5611A8}']
    {class} function _GetMAX_CALL_TO_ACTION_TEXT_LENGTH: Integer; cdecl;
    {class} function _GetMAX_EMAIL_HTML_CONTENT: Integer; cdecl;
    {class} function _GetMAX_MESSAGE_LENGTH: Integer; cdecl;
    {class} function _GetMIN_CALL_TO_ACTION_TEXT_LENGTH: Integer; cdecl;
    {class} function init(title: JCharSequence): JALAppInviteInvitation_IntentBuilder; cdecl;
    {class} property MAX_CALL_TO_ACTION_TEXT_LENGTH: Integer read _GetMAX_CALL_TO_ACTION_TEXT_LENGTH;
    {class} property MAX_EMAIL_HTML_CONTENT: Integer read _GetMAX_EMAIL_HTML_CONTENT;
    {class} property MAX_MESSAGE_LENGTH: Integer read _GetMAX_MESSAGE_LENGTH;
    {class} property MIN_CALL_TO_ACTION_TEXT_LENGTH: Integer read _GetMIN_CALL_TO_ACTION_TEXT_LENGTH;
  end;

  {***********************************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteInvitation$IntentBuilder')]
  JALAppInviteInvitation_IntentBuilder = interface(JObject)
    ['{838C0D39-D6D5-42A4-BF9A-A23DFDB9D062}']
    function build: JIntent; cdecl;
    function setAccount(account: JAccount): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setAdditionalReferralParameters(params: JMap): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setAndroidMinimumVersionCode(versionCode: Integer): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setCallToActionText(callToActionText: JCharSequence): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setCustomImage(imageUri: Jnet_Uri): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setDeepLink(deepLink: Jnet_Uri): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setEmailHtmlContent(htmlContent: JString): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setEmailSubject(subject: JString): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setGoogleAnalyticsTrackingId(trackingId: JString): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setMessage(msg: JCharSequence): JALAppInviteInvitation_IntentBuilder; cdecl;
    function setOtherPlatformsTargetApplication(targetPlatform: Integer; clientId: JString): JALAppInviteInvitation_IntentBuilder; cdecl;
  end;
  TJALAppInviteInvitation_IntentBuilder = class(TJavaGenericImport<JALAppInviteInvitation_IntentBuilderClass, JALAppInviteInvitation_IntentBuilder>) end;

  {*************************************************}
  JALFaceBookAppInviteClass = interface(JObjectClass)
    ['{3FEB53AB-882F-4D64-8797-C7873A5FF84B}']
    {class} function canShow: boolean; cdecl;
    {class} procedure show(activity: JActivity; applinkUrl: JString; promotionText: JString; promotionCode: JString; previewImageUrl: JString); cdecl;
  end;

  {*********************************************************}
  [JavaSignature('com/alcinoe/facebook/ALFaceBookAppInvite')]
  JALFaceBookAppInvite = interface(JObject)
    ['{CB1C33F4-A26F-4543-8899-63D25EF5C6E0}']
  end;
  TJALFaceBookAppInvite = class(TJavaGenericImport<JALFaceBookAppInviteClass, JALFaceBookAppInvite>) end;

  {*******************************************************************}
  JALFacebookAppEventsLogger_FlushBehaviorClass = interface(JEnumClass)
    ['{DB2B7FEC-CEFC-471F-9972-CC960064EC40}']
    {class} function _GetAUTO: JALFacebookAppEventsLogger_FlushBehavior; cdecl;
    {class} function _GetEXPLICIT_ONLY: JALFacebookAppEventsLogger_FlushBehavior; cdecl;
    {class} function valueOf(P1: JString): JALFacebookAppEventsLogger_FlushBehavior; cdecl;
    {class} function values: TJavaObjectArray<JALFacebookAppEventsLogger_FlushBehavior>; cdecl;
    {class} property AUTO: JALFacebookAppEventsLogger_FlushBehavior read _GetAUTO;
    {class} property EXPLICIT_ONLY: JALFacebookAppEventsLogger_FlushBehavior read _GetEXPLICIT_ONLY;
  end;

  {*********************************************************************}
  [JavaSignature('com/facebook/appevents/AppEventsLogger$FlushBehavior')]
  JALFacebookAppEventsLogger_FlushBehavior = interface(JEnum)
    ['{3795CBAA-446A-40CA-890E-02EA4DEDC55A}']
  end;
  TJALFacebookAppEventsLogger_FlushBehavior = class(TJavaGenericImport<JALFacebookAppEventsLogger_FlushBehaviorClass, JALFacebookAppEventsLogger_FlushBehavior>) end;

  {*******************************************************}
  JALFacebookAppEventsLoggerClass = interface(JObjectClass)
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
    {class} function newLogger(context: JContext): JALFacebookAppEventsLogger; cdecl; overload;
    {class} //function newLogger(context: JContext; accessToken: JAccessToken): JALFacebookAppEventsLogger; cdecl; overload;
    {class} //function newLogger(context: JContext; applicationId: JString; accessToken: JAccessToken): JALFacebookAppEventsLogger; cdecl; overload;
    {class} function newLogger(context: JContext; applicationId: JString): JALFacebookAppEventsLogger; cdecl; overload;
    {class} function getFlushBehavior: JALFacebookAppEventsLogger_FlushBehavior; cdecl;
    {class} procedure setFlushBehavior(flushBehavior: JALFacebookAppEventsLogger_FlushBehavior); cdecl;
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
  JALFacebookAppEventsLogger = interface(JObject)
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
  TJALFacebookAppEventsLogger = class(TJavaGenericImport<JALFacebookAppEventsLoggerClass, JALFacebookAppEventsLogger>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidapi.JALSoftInputListener', TypeInfo(ALAndroidapi.JALSoftInputListener));
  TRegTypes.RegisterType('ALAndroidapi.JALKeyPreImeListener', TypeInfo(ALAndroidapi.JALKeyPreImeListener));
  TRegTypes.RegisterType('ALAndroidapi.JALEditText', TypeInfo(ALAndroidapi.JALEditText));
  TRegTypes.RegisterType('ALAndroidapi.JALControlHostLayout', TypeInfo(ALAndroidapi.JALControlHostLayout));
  TRegTypes.RegisterType('ALAndroidapi.JALLog', TypeInfo(ALAndroidapi.JALLog));
  TRegTypes.RegisterType('ALAndroidapi.JALStatFs', TypeInfo(ALAndroidapi.JALStatFs));
  TRegTypes.RegisterType('ALAndroidapi.JALAppInviteInvitation', TypeInfo(ALAndroidapi.JALAppInviteInvitation));
  TRegTypes.RegisterType('ALAndroidapi.JALAppInviteInvitation_IntentBuilder', TypeInfo(ALAndroidapi.JALAppInviteInvitation_IntentBuilder));
  TRegTypes.RegisterType('ALAndroidapi.JALFaceBookAppInvite', TypeInfo(ALAndroidapi.JALFaceBookAppInvite));
  TRegTypes.RegisterType('ALAndroidapi.JALFacebookAppEventsLogger', TypeInfo(ALAndroidapi.JALFacebookAppEventsLogger));
  TRegTypes.RegisterType('ALAndroidapi.JALFacebookAppEventsLogger_FlushBehavior', TypeInfo(ALAndroidapi.JALFacebookAppEventsLogger_FlushBehavior));
end;

initialization
  RegisterTypes;

end.
