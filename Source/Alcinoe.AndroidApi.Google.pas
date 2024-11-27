unit Alcinoe.AndroidApi.Google;

interface

uses
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Androidapi.JNI.Location,
  Alcinoe.AndroidApi.Common;

type

  {****************}
  JTask = interface;
  JGoogleApi = interface;
  JGoogleSignInAccount = interface;
  JGoogleSignInClient = interface;
  JGoogleSignInOptions = interface;
  JGoogleSignInOptions_Builder = interface;
  JGoogleSignIn = interface;
  JAdvertisingIdClient_Info = interface;
  JAdvertisingIdClient = interface;
  JPriority = interface;
  JLocationServices = interface;
  JLocationListener = interface;
  JLocationRequest = interface;
  JLocationRequest_Builder = interface;
  JFusedLocationProviderClient = interface;

  {**********************************}
  JTaskClass = interface(JObjectClass)
    ['{B0BAFE22-5B84-4F88-A112-EC55C13DD262}']
  end;
  [JavaSignature('com/google/android/gms/tasks/Task')]
  JTask = interface(JObject)
    ['{92D8A8B1-462F-468A-A1E4-C7C32C396EA0}']
    function getException: JException; cdecl;
    function getResult: JObject; cdecl;
    function isSuccessful: Boolean; cdecl;
    function isComplete: Boolean; cdecl;
  end;
  TJTask = class(TJavaGenericImport<JTaskClass, JTask>) end;

  {***************************************}
  JGoogleApiClass = interface(JObjectClass)
    ['{8309C03E-C042-4E3C-8EFD-DC8489744651}']
  end;
  [JavaSignature('com/google/android/gms/common/api/GoogleApi')]
  JGoogleApi = interface(JObject)
    ['{A0E6168F-5367-47EF-B483-FB330351D41E}']
  end;
  TJGoogleApi = class(TJavaGenericImport<JGoogleApiClass, JGoogleApi>) end;

  {*************************************************}
  JGoogleSignInAccountClass = interface(JObjectClass)
    ['{3DE5F05F-B748-4A49-A3DD-E92A3F895AA4}']
  end;
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInAccount')]
  JGoogleSignInAccount = interface(JObject)
    ['{9572FF07-A9D9-428E-932F-CFB8029D9111}']
    function getDisplayName: JString; cdecl;
    function getEmail: JString; cdecl;
    function getFamilyName: JString; cdecl;
    function getGivenName: JString; cdecl;
    function getGrantedScopes: JSet; cdecl;
    function getId: JString; cdecl;
    function getIdToken: JString; cdecl;
    function getPhotoUrl: Jnet_Uri; cdecl;
    function getServerAuthCode: JString; cdecl;
  end;
  TJGoogleSignInAccount = class(TJavaGenericImport<JGoogleSignInAccountClass, JGoogleSignInAccount>) end;

  {***************************************************}
  JGoogleSignInClientClass = interface(JGoogleApiClass)
    ['{472DC2F9-E83F-4893-96F9-1963EE4369A8}']
  end;
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInClient')]
  JGoogleSignInClient = interface(JGoogleApi)
    ['{3E5CC3AA-0076-4665-BCD3-858EA64DAFC9}']
    function getSignInIntent: JIntent; cdecl;
    //function revokeAccess: JTask; cdecl;
    //function signOut: JTask; cdecl;
    //function silentSignIn: JTask; cdecl;
  end;
  TJGoogleSignInClient = class(TJavaGenericImport<JGoogleSignInClientClass, JGoogleSignInClient>) end;

  {*************************************************}
  JGoogleSignInOptionsClass = interface(JObjectClass)
    ['{13E258D6-50CD-4712-9B07-FD6FA4D34096}']
    {class} function _GetDEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} function _GetDEFAULT_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} property DEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_GAMES_SIGN_IN;
    {class} property DEFAULT_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_SIGN_IN;
  end;
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInOptions')]
  JGoogleSignInOptions = interface(JObject)
    ['{BBAB310D-4286-4384-B622-F54E50ED6169}']
  end;
  TJGoogleSignInOptions = class(TJavaGenericImport<JGoogleSignInOptionsClass, JGoogleSignInOptions>) end;

  {*********************************************************}
  JGoogleSignInOptions_BuilderClass = interface(JObjectClass)
    ['{3EE36B68-1725-43CD-B64F-8552E43F7BF7}']
    {class} function init: JGoogleSignInOptions_Builder; cdecl; overload;
    {class} function init(googleSignInOptions: JGoogleSignInOptions): JGoogleSignInOptions_Builder; cdecl; overload;
  end;
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInOptions$Builder')]
  JGoogleSignInOptions_Builder = interface(JObject)
    ['{F2D1D975-4808-49BB-A1AA-F443D86C9C82}']
    //function addExtension(extension: JGoogleSignInOptionsExtension): JGoogleSignInOptions_Builder; cdecl;
    function build: JGoogleSignInOptions; cdecl;
    function requestEmail: JGoogleSignInOptions_Builder; cdecl;
    function requestId: JGoogleSignInOptions_Builder; cdecl;
    function requestIdToken(serverClientId: JString): JGoogleSignInOptions_Builder; cdecl;
    function requestProfile: JGoogleSignInOptions_Builder; cdecl;
    function requestServerAuthCode(serverClientId: JString): JGoogleSignInOptions_Builder; cdecl; overload;
    function requestServerAuthCode(serverClientId: JString; forceCodeForRefreshToken: Boolean): JGoogleSignInOptions_Builder; cdecl; overload;
    function setAccountName(accountName: JString): JGoogleSignInOptions_Builder; cdecl;
    function setHostedDomain(hostedDomain: JString): JGoogleSignInOptions_Builder; cdecl;
  end;
  TJGoogleSignInOptions_Builder = class(TJavaGenericImport<JGoogleSignInOptions_BuilderClass, JGoogleSignInOptions_Builder>) end;

  {******************************************}
  JGoogleSignInClass = interface(JObjectClass)
    ['{8FAB6882-38A4-4327-8895-FF94A4ED5E52}']
    {class} function getClient(context: JContext; options: JGoogleSignInOptions): JGoogleSignInClient; cdecl; overload;
    {class} function getClient(activity: JActivity; options: JGoogleSignInOptions): JGoogleSignInClient; cdecl; overload;
    {class} function getSignedInAccountFromIntent(data: JIntent): JTask; cdecl;
  end;
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignIn')]
  JGoogleSignIn = interface(JObject)
    ['{72958192-27A6-454D-91AE-75D339BB4F4C}']
  end;
  TJGoogleSignIn = class(TJavaGenericImport<JGoogleSignInClass, JGoogleSignIn>) end;

  {*******************************************************************************}
  [JavaSignature('com/google/android/gms/ads/identifier/AdvertisingIdClient$Info')]
  JAdvertisingIdClient_Info = interface(JObject)
    ['{B54B273F-529D-45AA-989C-4C8197BC6563}']
    function getId: JString; cdecl;
    function isLimitAdTrackingEnabled: boolean; cdecl;
    //function toString: JString; cdecl;
  end;
  TJAdvertisingIdClient_Info = class(TJavaGenericImport<JAdvertisingIdClient_InfoClass, JAdvertisingIdClient_Info>) end;

  {*************************************************}
  JAdvertisingIdClientClass = interface(JObjectClass)
    ['{1FEB6C2A-8188-4C87-B180-426DA87F942B}']
    {class} function getAdvertisingIdInfo(context: JContext) : JAdvertisingIdClient_Info; cdecl;
  end;
  [JavaSignature('com/google/android/gms/ads/identifier/AdvertisingIdClient')]
  JAdvertisingIdClient = interface(JObject)
    ['{66ACFF60-5E5C-4937-986D-3174BA4FF1ED}']
  end;
  TJAdvertisingIdClient = class(TJavaGenericImport<JAdvertisingIdClientClass, JAdvertisingIdClient>) end;

  {****************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/Priority
  JPriorityClass = interface(JObjectClass)
    ['{53B6F77D-4F76-49DC-B787-0136B0DE6FB4}']
    {class} function _GetPRIORITY_BALANCED_POWER_ACCURACY: Integer; cdecl;
    {class} function _GetPRIORITY_HIGH_ACCURACY: Integer; cdecl;
    {class} function _GetPRIORITY_LOW_POWER: Integer; cdecl;
    {class} function _GetPRIORITY_PASSIVE: Integer; cdecl;
    {class} property PRIORITY_BALANCED_POWER_ACCURACY: Integer read _GetPRIORITY_BALANCED_POWER_ACCURACY;
    {class} property PRIORITY_HIGH_ACCURACY: Integer read _GetPRIORITY_HIGH_ACCURACY;
    {class} property PRIORITY_LOW_POWER: Integer read _GetPRIORITY_LOW_POWER;
    {class} property PRIORITY_PASSIVE: Integer read _GetPRIORITY_PASSIVE;
  end;
  [JavaSignature('com/google/android/gms/location/Priority')]
  JPriority = interface(JObject)
    ['{73C0971B-D53F-40DE-9DBB-F1C02A3C8A6B}']
  end;
  TJPriority = class(TJavaGenericImport<JPriorityClass, JPriority>) end;

  {************************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/LocationServices
  JLocationServicesClass = interface(JObjectClass)
    ['{56C234C3-A364-4F96-82BA-27A4C683D5AA}']
    {class} function getFusedLocationProviderClient(context: JContext): JFusedLocationProviderClient; cdecl; overload;
    {class} function getFusedLocationProviderClient(activity: JActivity): JFusedLocationProviderClient; cdecl; overload;
  end;
  [JavaSignature('com/google/android/gms/location/LocationServices')]
  JLocationServices = interface(JObject)
    ['{F98983C1-A8CF-4116-B7C1-00B7FE18FBA6}']
  end;
  TJLocationServices = class(TJavaGenericImport<JLocationServicesClass, JLocationServices>) end;

  {************************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/LocationListener
  JLocationListenerClass = interface(IJavaClass)
    ['{FCF58DA8-B892-497A-A676-40D6D32A4DE9}']
  end;
  [JavaSignature('com/google/android/gms/location/LocationListener')]
  JLocationListener = interface(IJavaInstance)
    ['{01DB4B21-C92D-4BBC-8D21-D8270F4C0370}']
    procedure onLocationChanged(location: JLocation); cdecl;
  end;
  TJLocationListener = class(TJavaGenericImport<JLocationListenerClass, JLocationListener>) end;

  {***********************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/LocationRequest
  JLocationRequestClass = interface(JObjectClass)
    ['{77B7A133-5FE4-4892-8526-7E49ED77318E}']
  end;
  [JavaSignature('com/google/android/gms/location/LocationRequest')]
  JLocationRequest = interface(JObject)
    ['{A176F476-6BF5-47AB-89E8-663409B224B7}']
  end;
  TJLocationRequest = class(TJavaGenericImport<JLocationRequestClass, JLocationRequest>) end;

  {*******************************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/LocationRequest.Builder
  JLocationRequest_BuilderClass = interface(JObjectClass)
    ['{0D6005B8-A673-40CD-8365-D4F3EE8B6C2F}']
    {class} function _GetIMPLICIT_MAX_UPDATE_AGE: Int64; cdecl;
    {class} function _GetIMPLICIT_MIN_UPDATE_INTERVAL: Int64; cdecl;
    {class} function init(intervalMillis: Int64): JLocationRequest_Builder; cdecl; overload;
    {class} function init(priority: Integer; intervalMillis: Int64): JLocationRequest_Builder; cdecl; overload;
    {class} property IMPLICIT_MAX_UPDATE_AGE: Int64 read _GetIMPLICIT_MAX_UPDATE_AGE;
    {class} property IMPLICIT_MIN_UPDATE_INTERVAL: Int64 read _GetIMPLICIT_MIN_UPDATE_INTERVAL;
  end;
  [JavaSignature('com/google/android/gms/location/LocationRequest$Builder')]
  JLocationRequest_Builder = interface(JObject)
    ['{8F74C046-2CC4-45C7-8F8B-5526DEE4C54E}']
    function build: JLocationRequest; cdecl;
    function setDurationMillis(durationMillis: Int64): JLocationRequest_Builder; cdecl;
    function setGranularity(granularity: Integer): JLocationRequest_Builder; cdecl;
    function setIntervalMillis(intervalMillis: Int64): JLocationRequest_Builder; cdecl;
    function setMaxUpdateAgeMillis(maxUpdateAgeMillis: Int64): JLocationRequest_Builder; cdecl;
    function setMaxUpdateDelayMillis(maxUpdateDelayMillis: Int64): JLocationRequest_Builder; cdecl;
    function setMaxUpdates(maxUpdates: Integer): JLocationRequest_Builder; cdecl;
    function setMinUpdateDistanceMeters(minUpdateDistanceMeters: Single): JLocationRequest_Builder; cdecl;
    function setMinUpdateIntervalMillis(minUpdateIntervalMillis: Int64): JLocationRequest_Builder; cdecl;
    function setPriority(priority: Integer): JLocationRequest_Builder; cdecl;
    function setWaitForAccurateLocation(waitForAccurateLocation: Boolean): JLocationRequest_Builder; cdecl;
  end;
  TJLocationRequest_Builder = class(TJavaGenericImport<JLocationRequest_BuilderClass, JLocationRequest_Builder>) end;

  {***********************************************************************************************************}
  //https://developers.google.com/android/reference/com/google/android/gms/location/FusedLocationProviderClient
  JFusedLocationProviderClientClass = interface(JObjectClass)
    ['{04407D7B-A09F-4559-BD11-1D57BAC61F46}']
  end;
  [JavaSignature('com/google/android/gms/location/FusedLocationProviderClient')]
  JFusedLocationProviderClient = interface(JObject)
    ['{9D8088CC-B29A-4173-B9A7-87A7B6714F5E}']
    function getLastLocation: JTask; cdecl;
    function removeLocationUpdates(listener: JLocationListener): JTask; cdecl;
    function requestLocationUpdates(request: JLocationRequest; listener: JLocationListener; looper: JLooper): JTask; cdecl;
  end;
  TJFusedLocationProviderClient = class(TJavaGenericImport<JFusedLocationProviderClientClass, JFusedLocationProviderClient>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JTask', TypeInfo(Alcinoe.AndroidApi.Google.JTask));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleSignInAccount', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleSignInAccount));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleApi', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleApi));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleSignInClient', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleSignInClient));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleSignInOptions', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleSignInOptions));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleSignInOptions_Builder', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleSignInOptions_Builder));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JGoogleSignIn', TypeInfo(Alcinoe.AndroidApi.Google.JGoogleSignIn));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JAdvertisingIdClient_Info', TypeInfo(Alcinoe.AndroidApi.Google.JAdvertisingIdClient_Info));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JAdvertisingIdClient', TypeInfo(Alcinoe.AndroidApi.Google.JAdvertisingIdClient));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JPriority', TypeInfo(Alcinoe.AndroidApi.Google.JPriority));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JLocationServices', TypeInfo(Alcinoe.AndroidApi.Google.JLocationServices));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JLocationListener', TypeInfo(Alcinoe.AndroidApi.Google.JLocationListener));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JLocationRequest', TypeInfo(Alcinoe.AndroidApi.Google.JLocationRequest));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JLocationRequest_Builder', TypeInfo(Alcinoe.AndroidApi.Google.JLocationRequest_Builder));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Google.JFusedLocationProviderClient', TypeInfo(Alcinoe.AndroidApi.Google.JFusedLocationProviderClient));
end;

initialization
  RegisterTypes;

end.
