//
// Made from Facebook SDK version 15.2.0
//
unit Alcinoe.AndroidApi.Facebook;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.facebook.android:facebook-android-sdk:xx.xx.xx where xx.xx.xx
  //is the last version of the facebook-android-sdk (You can find this version at
  //https://github.com/facebook/facebook-android-sdk/releases) and gave also the path to
  //<Alcinoe>\Source\Alcinoe.AndroidApi.Facebook.pas to build the compare source file. Then make a diff
  //compare between the new generated Alcinoe.AndroidApi.Facebook.pas and this one to see if the api
  //signature is still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of facebook sdk (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Net,
  Androidapi.JNI.App,
  Alcinoe.AndroidApi.Common;

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
  JGraphRequest = interface;

  {*******************************************************}
  JALFacebookShareLinkDialogClass = interface(JObjectClass)
    ['{3FEB53AB-882F-4D64-8797-C7873A5FF84B}']
    {class} function canShow: boolean; cdecl;
    {class} procedure show(activity: JActivity; contentUrl: Jnet_Uri; quote: JString); cdecl;
  end;
  [JavaSignature('io/magicfoundation/alcinoe/facebook/share/ALFacebookShareLinkDialog')]
  JALFacebookShareLinkDialog = interface(JObject)
    ['{CB1C33F4-A26F-4543-8899-63D25EF5C6E0}']
  end;
  TJALFacebookShareLinkDialog = class(TJavaGenericImport<JALFacebookShareLinkDialogClass, JALFacebookShareLinkDialog>) end;

  {*********************************************************}
  JFacebookExceptionClass = interface(JRuntimeExceptionClass)
    ['{BE0539B7-F533-4942-8B5E-C21399C1F8A7}']
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
    procedure logInWithReadPermissions(activity: JActivity; permissions: JCollection); cdecl; overload;
    procedure logOut; cdecl;
    procedure registerCallback(callbackManager: JCallbackManager; callback: JFacebookCallback); cdecl;
  end;
  TJLoginManager = class(TJavaGenericImport<JLoginManagerClass, JLoginManager>) end;

  {*********************************************}
  JAccessTokenClass = interface(JParcelableClass)
    ['{D45F97E4-A070-459B-B7FF-63DB58993826}']
    {class} function getCurrentAccessToken: JAccessToken; cdecl;
    {class} procedure setCurrentAccessToken(accessToken: JAccessToken); cdecl;
  end;
  [JavaSignature('com/facebook/AccessToken')]
  JAccessToken = interface(JParcelable)
    ['{8056F0A5-7707-44BA-9991-BAF2B464474F}']
    function getDeclinedPermissions: JSet; cdecl;
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
  end;
  TJLoginResult = class(TJavaGenericImport<JLoginResultClass, JLoginResult>) end;

  {**************************************}
  JHttpMethodClass = interface(JEnumClass)
    ['{169D72DB-480A-439B-B2E5-00F540752436}']
    {class} function _GetGET: JHttpMethod; cdecl;
    {class} function _GetPOST: JHttpMethod; cdecl;
    {class} property &GET: JHttpMethod read _GetGET;
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
  end;
  [JavaSignature('com/facebook/FacebookRequestError')]
  JFacebookRequestError = interface(JParcelable)
    ['{A2A8F82F-F83C-4032-8FAF-3483ECC3B14C}']
    function getErrorCode: Integer; cdecl;
    function getErrorMessage: JString; cdecl;
  end;
  TJFacebookRequestError = class(TJavaGenericImport<JFacebookRequestErrorClass, JFacebookRequestError>) end;

  {*****************************************}
  JFacebookSdkClass = interface(JObjectClass)
    ['{0B655652-B6FA-45AD-BDF3-A357B5DDA839}']
    {class} function isInitialized: Boolean; cdecl;
    {class} procedure sdkInitialize(applicationContext: JContext); cdecl;
  end;
  [JavaSignature('com/facebook/FacebookSdk')]
  JFacebookSdk = interface(JObject)
    ['{F138CBE5-D161-4740-93F3-817FC2F2F4C5}']
  end;
  TJFacebookSdk = class(TJavaGenericImport<JFacebookSdkClass, JFacebookSdk>) end;

  {*******************************************}
  JGraphResponseClass = interface(JObjectClass)
    ['{2CF25531-06CA-4423-A804-2DAD41F6DEF4}']
  end;
  [JavaSignature('com/facebook/GraphResponse')]
  JGraphResponse = interface(JObject)
    ['{678C170B-168A-42DA-9CE6-E7BFB8325A19}']
    function getError: JFacebookRequestError; cdecl;
    function getRawResponse: JString; cdecl;
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

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JALFacebookShareLinkDialog', TypeInfo(Alcinoe.AndroidApi.Facebook.JALFacebookShareLinkDialog));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JFacebookException', TypeInfo(Alcinoe.AndroidApi.Facebook.JFacebookException));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JFacebookCallback', TypeInfo(Alcinoe.AndroidApi.Facebook.JFacebookCallback));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JCallbackManager', TypeInfo(Alcinoe.AndroidApi.Facebook.JCallbackManager));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JCallbackManager_Factory', TypeInfo(Alcinoe.AndroidApi.Facebook.JCallbackManager_Factory));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JLoginManager', TypeInfo(Alcinoe.AndroidApi.Facebook.JLoginManager));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JAccessToken', TypeInfo(Alcinoe.AndroidApi.Facebook.JAccessToken));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JLoginResult', TypeInfo(Alcinoe.AndroidApi.Facebook.JLoginResult));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JHttpMethod', TypeInfo(Alcinoe.AndroidApi.Facebook.JHttpMethod));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JGraphRequestAsyncTask', TypeInfo(Alcinoe.AndroidApi.Facebook.JGraphRequestAsyncTask));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JFacebookRequestError', TypeInfo(Alcinoe.AndroidApi.Facebook.JFacebookRequestError));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JFacebookSdk', TypeInfo(Alcinoe.AndroidApi.Facebook.JFacebookSdk));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JGraphResponse', TypeInfo(Alcinoe.AndroidApi.Facebook.JGraphResponse));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JGraphRequest_Callback', TypeInfo(Alcinoe.AndroidApi.Facebook.JGraphRequest_Callback));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Facebook.JGraphRequest', TypeInfo(Alcinoe.AndroidApi.Facebook.JGraphRequest));
end;

initialization
  RegisterTypes;

end.
