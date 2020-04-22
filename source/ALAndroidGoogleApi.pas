unit ALAndroidGoogleApi;

interface

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  ALAndroidApi;

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

  {**********************************}
  JTaskClass = interface(JObjectClass)
    ['{B0BAFE22-5B84-4F88-A112-EC55C13DD262}']
  end;

  {**************************************************}
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

  {************************************************************}
  [JavaSignature('com/google/android/gms/common/api/GoogleApi')]
  JGoogleApi = interface(JObject)
    ['{A0E6168F-5367-47EF-B483-FB330351D41E}']
  end;
  TJGoogleApi = class(TJavaGenericImport<JGoogleApiClass, JGoogleApi>) end;

  {************************************************}
  JGoogleSignInAccountClass = interface(JObjectClass)
    ['{3DE5F05F-B748-4A49-A3DD-E92A3F895AA4}']
  end;

  {***************************************************************************}
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

  {**************************************************************************}
  [JavaSignature('com/google/android/gms/auth/api/signin/GoogleSignInClient')]
  JGoogleSignInClient = interface(JGoogleApi)
    ['{3E5CC3AA-0076-4665-BCD3-858EA64DAFC9}']
    function getSignInIntent: JIntent; cdecl;
    //function revokeAccess: JTask; cdecl;
    //function signOut: JTask; cdecl;
    //function silentSignIn: JTask; cdecl;
  end;
  TJGoogleSignInClient = class(TJavaGenericImport<JGoogleSignInClientClass, JGoogleSignInClient>) end;

  {************************************************}
  JGoogleSignInOptionsClass = interface(JObjectClass)
    ['{13E258D6-50CD-4712-9B07-FD6FA4D34096}']
    {class} function _GetDEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} function _GetDEFAULT_SIGN_IN: JGoogleSignInOptions; cdecl;
    {class} property DEFAULT_GAMES_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_GAMES_SIGN_IN;
    {class} property DEFAULT_SIGN_IN: JGoogleSignInOptions read _GetDEFAULT_SIGN_IN;
  end;

  {***************************************************************************}
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

  {***********************************************************************************}
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

  {********************************************************************}
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

  {**************************************************************************}
  [JavaSignature('com/google/android/gms/ads/identifier/AdvertisingIdClient')]
  JAdvertisingIdClient = interface(JObject)
    ['{66ACFF60-5E5C-4937-986D-3174BA4FF1ED}']
  end;
  TJAdvertisingIdClient = class(TJavaGenericImport<JAdvertisingIdClientClass, JAdvertisingIdClient>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidGoogleApi.JTask', TypeInfo(ALAndroidGoogleApi.JTask));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleSignInAccount', TypeInfo(ALAndroidGoogleApi.JGoogleSignInAccount));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleApi', TypeInfo(ALAndroidGoogleApi.JGoogleApi));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleSignInClient', TypeInfo(ALAndroidGoogleApi.JGoogleSignInClient));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleSignInOptions', TypeInfo(ALAndroidGoogleApi.JGoogleSignInOptions));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleSignInOptions_Builder', TypeInfo(ALAndroidGoogleApi.JGoogleSignInOptions_Builder));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JGoogleSignIn', TypeInfo(ALAndroidGoogleApi.JGoogleSignIn));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JAdvertisingIdClient_Info', TypeInfo(ALAndroidGoogleApi.JAdvertisingIdClient_Info));
  TRegTypes.RegisterType('ALAndroidGoogleApi.JAdvertisingIdClient', TypeInfo(ALAndroidGoogleApi.JAdvertisingIdClient));
end;

initialization
  RegisterTypes;

end.
