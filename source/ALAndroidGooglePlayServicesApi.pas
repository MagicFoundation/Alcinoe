unit ALAndroidGooglePlayServicesApi;

interface

uses Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNIBridge,
     Androidapi.JNI.JavaTypes,
     Androidapi.JNI.Accounts,
     Androidapi.JNI.Net,
     Androidapi.JNI.App,
     Androidapi.JNI.Location;

type

  {*******************************}
  JAppInviteInvitation = interface;
  JIntentBuilder_PlatformMode = interface;
  JAppInviteInvitation_IntentBuilder = interface;
  JAppInviteReferral = interface;
  JALAppInviteInvitationResultListener = interface;
  JALAppInviteInvitationResult = interface;
  JALLocationServicesListener = interface;
  JALLocationServices = interface;


  {*************************************************}
  JAppInviteInvitationClass = interface(JObjectClass)
    ['{34D1F50D-8EF5-4374-9B5F-384E93E3BF92}']
    {class} function init: JAppInviteInvitation; cdecl;
  end;

  {*********************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteInvitation')]
  JAppInviteInvitation = interface(JObject)
    ['{5E6EAC6E-85F3-45F6-8E46-3627ED77587E}']
  end;
  TJAppInviteInvitation = class(TJavaGenericImport<JAppInviteInvitationClass, JAppInviteInvitation>) end;

  {************************************************************}
  JIntentBuilder_PlatformModeClass = interface(JAnnotationClass)
    ['{419B2BDC-2EDC-4F0D-B6AD-F9DA715F6F4D}']
    {class} function _GetPROJECT_PLATFORM_ANDROID: Integer; cdecl;
    {class} function _GetPROJECT_PLATFORM_IOS: Integer; cdecl;
    {class} property PROJECT_PLATFORM_ANDROID: Integer read _GetPROJECT_PLATFORM_ANDROID;
    {class} property PROJECT_PLATFORM_IOS: Integer read _GetPROJECT_PLATFORM_IOS;
  end;

  {************************************************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteInvitation$IntentBuilder$PlatformMode')]
  JIntentBuilder_PlatformMode = interface(JAnnotation)
    ['{D05F92FA-07BE-4D0E-8C29-20D126B713C5}']
  end;
  TJIntentBuilder_PlatformMode = class(TJavaGenericImport<JIntentBuilder_PlatformModeClass, JIntentBuilder_PlatformMode>) end;

  {***************************************************************}
  JAppInviteInvitation_IntentBuilderClass = interface(JObjectClass)
    ['{E131213D-5067-4E24-AB74-F9ECBD5611A8}']
    {class} function _GetMAX_CALL_TO_ACTION_TEXT_LENGTH: Integer; cdecl;
    {class} function _GetMAX_EMAIL_HTML_CONTENT: Integer; cdecl;
    {class} function _GetMAX_MESSAGE_LENGTH: Integer; cdecl;
    {class} function _GetMIN_CALL_TO_ACTION_TEXT_LENGTH: Integer; cdecl;
    {class} function init(title: JCharSequence): JAppInviteInvitation_IntentBuilder; cdecl;
    {class} property MAX_CALL_TO_ACTION_TEXT_LENGTH: Integer read _GetMAX_CALL_TO_ACTION_TEXT_LENGTH;
    {class} property MAX_EMAIL_HTML_CONTENT: Integer read _GetMAX_EMAIL_HTML_CONTENT;
    {class} property MAX_MESSAGE_LENGTH: Integer read _GetMAX_MESSAGE_LENGTH;
    {class} property MIN_CALL_TO_ACTION_TEXT_LENGTH: Integer read _GetMIN_CALL_TO_ACTION_TEXT_LENGTH;
  end;

  {***********************************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteInvitation$IntentBuilder')]
  JAppInviteInvitation_IntentBuilder = interface(JObject)
    ['{838C0D39-D6D5-42A4-BF9A-A23DFDB9D062}']
    function build: JIntent; cdecl;
    function setAccount(account: JAccount): JAppInviteInvitation_IntentBuilder; cdecl;
    function setAdditionalReferralParameters(params: JMap): JAppInviteInvitation_IntentBuilder; cdecl;
    function setAndroidMinimumVersionCode(versionCode: Integer): JAppInviteInvitation_IntentBuilder; cdecl;
    function setCallToActionText(callToActionText: JCharSequence): JAppInviteInvitation_IntentBuilder; cdecl;
    function setCustomImage(imageUri: Jnet_Uri): JAppInviteInvitation_IntentBuilder; cdecl;
    function setDeepLink(deepLink: Jnet_Uri): JAppInviteInvitation_IntentBuilder; cdecl;
    function setEmailHtmlContent(htmlContent: JString): JAppInviteInvitation_IntentBuilder; cdecl;
    function setEmailSubject(subject: JString): JAppInviteInvitation_IntentBuilder; cdecl;
    function setGoogleAnalyticsTrackingId(trackingId: JString): JAppInviteInvitation_IntentBuilder; cdecl;
    function setMessage(msg: JCharSequence): JAppInviteInvitation_IntentBuilder; cdecl;
    function setOtherPlatformsTargetApplication(targetPlatform: Integer; clientId: JString): JAppInviteInvitation_IntentBuilder; cdecl;
  end;
  TJAppInviteInvitation_IntentBuilder = class(TJavaGenericImport<JAppInviteInvitation_IntentBuilderClass, JAppInviteInvitation_IntentBuilder>) end;

  {***********************************************}
  JAppInviteReferralClass = interface(JObjectClass)
    ['{D999EBBE-74DC-426B-A6A5-2C63A8EF335A}']
    {class} function addPlayStoreReferrerToIntent(playStoreReferrerIntent: JIntent; referralIntent: JIntent): JIntent; cdecl; //deprecated
    {class} function addReferralDataToIntent(invitationId: JString; deepLink: JString; referralIntent: JIntent): JIntent; cdecl; //deprecated
    {class} function getDeepLink(referralIntent: JIntent): JString; cdecl;
    {class} function getInvitationId(referralIntent: JIntent): JString; cdecl;
    {class} function hasReferral(referralIntent: JIntent): Boolean; cdecl;
    {class} function isOpenedFromPlayStore(referralIntent: JIntent): Boolean; cdecl;
  end;

  {*******************************************************************}
  [JavaSignature('com/google/android/gms/appinvite/AppInviteReferral')]
  JAppInviteReferral = interface(JObject)
    ['{0BB52E8A-6921-4821-89DE-79EDD4BE3297}']
  end;
  TJAppInviteReferral = class(TJavaGenericImport<JAppInviteReferralClass, JAppInviteReferral>) end;

  {***************************************************************}
  JALAppInviteInvitationResultListenerClass = interface(IJavaClass)
    ['{D3768F2A-D5BB-43A5-BE1B-A8133445FB6D}']
  end;

  {***********************************************************************************}
  [JavaSignature('com/alcinoe/googleplayservices/ALAppInviteInvitationResultListener')]
  JALAppInviteInvitationResultListener = interface(IJavaInstance)
    ['{6FAC6B04-8B86-40A5-935C-476087E55082}']
    procedure onError(level: integer; code: integer); cdecl;
    procedure onSuccess(deepLink: JString; invitationId: JString); cdecl;
  end;
  TJALAppInviteInvitationResultListener = class(TJavaGenericImport<JALAppInviteInvitationResultListenerClass, JALAppInviteInvitationResultListener>) end;

  {*********************************************************}
  JALAppInviteInvitationResultClass = interface(JObjectClass)
    ['{43B2EDB9-2F4C-49EC-8145-C3B3DF7FF178}']
    {class} function init(activity: JActivity): JALAppInviteInvitationResult; cdecl;
  end;

  {***************************************************************************}
  [JavaSignature('com/alcinoe/googleplayservices/ALAppInviteInvitationResult')]
  JALAppInviteInvitationResult = interface(JObject)
    ['{6299DA17-EFB1-4954-AA48-760C2B732597}']
    procedure retrieve(autoLaunchDeepLink: Boolean); cdecl;
    procedure setListener(listener: JALAppInviteInvitationResultListener); cdecl;
  end;
  TJALAppInviteInvitationResult = class(TJavaGenericImport<JALAppInviteInvitationResultClass, JALAppInviteInvitationResult>) end;

  {******************************************************}
  JALLocationServicesListenerClass = interface(IJavaClass)
    ['{0C115FE4-04ED-47EE-948A-EC67D83CD9A3}']
  end;

  {****************************************************************}
  [JavaSignature('com/alcinoe/location/ALLocationServicesListener')]
  JALLocationServicesListener = interface(IJavaInstance)
    ['{613CEAD9-104B-4F6A-B105-200F34DDB5DD}']
    procedure onLocationChanged(location: JLocation); cdecl;
  end;
  TJALLocationServicesListener = class(TJavaGenericImport<JALLocationServicesListenerClass, JALLocationServicesListener>) end;

  {************************************************}
  JALLocationServicesClass = interface(JObjectClass)
    ['{253118F7-7CF0-4EBB-8F94-3FD6C560B708}']
    {class} function init(context: JContext): JALLocationServices; cdecl;
  end;

  {********************************************************}
  [JavaSignature('com/alcinoe/location/ALLocationServices')]
  JALLocationServices = interface(JObject)
    ['{F0547773-B7C9-4AE4-8838-99CB941CA0CB}']
    procedure setListener(listener: JALLocationServicesListener); cdecl;
    procedure startLocationUpdates(startWithLastKnownLocation: boolean;
                                   interval: Int64;
                                   fastestInterval: Int64;
                                   maxWaitTime: Int64;
                                   priority: integer;
                                   smallestDisplacement: Single); cdecl;
    procedure stopLocationUpdates; cdecl;
  end;
  TJALLocationServices = class(TJavaGenericImport<JALLocationServicesClass, JALLocationServices>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JAppInviteInvitation', TypeInfo(ALAndroidGooglePlayServicesApi.JAppInviteInvitation));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JIntentBuilder_PlatformMode', TypeInfo(ALAndroidGooglePlayServicesApi.JIntentBuilder_PlatformMode));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JAppInviteInvitation_IntentBuilder', TypeInfo(ALAndroidGooglePlayServicesApi.JAppInviteInvitation_IntentBuilder));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JAppInviteReferral', TypeInfo(ALAndroidGooglePlayServicesApi.JAppInviteReferral));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALAppInviteInvitationResultListener', TypeInfo(ALAndroidGooglePlayServicesApi.JALAppInviteInvitationResultListener));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALAppInviteInvitationResult', TypeInfo(ALAndroidGooglePlayServicesApi.JALAppInviteInvitationResult));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALLocationServicesListener', TypeInfo(ALAndroidGooglePlayServicesApi.JALLocationServicesListener));
  TRegTypes.RegisterType('ALAndroidGooglePlayServicesApi.JALLocationServices', TypeInfo(ALAndroidGooglePlayServicesApi.JALLocationServices));
end;

initialization
  RegisterTypes;

end.
