//
// Made from firebase-messaging 23.1.0
//
unit ALAndroidFirebaseApi;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.google.firebase:firebase-messaging:xx.xx.xx where xx.xx.xx
  //is the last version of the firebase-messaging and gave also the path to
  //<Alcinoe>\Sources\ALAndroidFirebaseApi.pas to build the compare source file. Then make a diff
  //compare between the new generated ALAndroidFirebaseApi.pas and this one to see if the api
  //signature is still the same. repeat the operation below with the library
  //com.google.firebase:firebase-analytics:xx.xx.xx
  {$MESSAGE WARN 'Check if the api signature of the last version of firebase sdk (android) is still the same'}
{$IFEND}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.PlayServices.Tasks,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  ALAndroidXApi;

type

  {*****************************}
  JFirebaseMessaging = interface;
  JALFirebaseMessagingService = interface;
  JFirebaseAnalytics = interface;

  {**************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/messaging/FirebaseMessaging
  JFirebaseMessagingClass = interface(JObjectClass)
    ['{5E14657A-AF87-404A-8BF2-9B14D1730E90}']
    {class} function getInstance: JFirebaseMessaging; cdecl; overload;
  end;
  [JavaSignature('com/google/firebase/messaging/FirebaseMessaging')]
  JFirebaseMessaging = interface(JObject)
    ['{EE5EEAAC-6BB6-4401-A1A0-69E348580BD0}']
    function deleteToken: JTask; cdecl;
    function getToken: JTask; cdecl;
  end;
  TJFirebaseMessaging = class(TJavaGenericImport<JFirebaseMessagingClass, JFirebaseMessaging>) end;

  {********************************************************}
  JALFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{0A2D87AC-C8A8-4565-8EC9-598592836070}']
    {class} function _GetnewTokenDispatcher: JMutableLiveData; cdecl;
    {class} function _GetnewMessageDispatcher: JMutableLiveData; cdecl;
    {class} property newTokenDispatcher: JMutableLiveData read _GetnewTokenDispatcher;
    {class} property newMessageDispatcher: JMutableLiveData read _GetnewMessageDispatcher;
  end;
  [JavaSignature('com/alcinoe/firebase/messaging/ALFirebaseMessagingService')]
  JALFirebaseMessagingService = interface(JObject)
    ['{9F3FF329-E17E-41B4-9C4A-214AF6A1FC05}']
  end;
  TJALFirebaseMessagingService = class(TJavaGenericImport<JALFirebaseMessagingServiceClass, JALFirebaseMessagingService>) end;

  {**************************************************************************************************}
  //https://firebase.google.com/docs/reference/android/com/google/firebase/analytics/FirebaseAnalytics
  JFirebaseAnalyticsClass = interface(JObjectClass)
    ['{19AA41AC-06A8-4D39-A37C-CA44DCA4EDAB}']
    {class} function getInstance(context: JContext): JFirebaseAnalytics; cdecl;
  end;
  [JavaSignature('com/google/firebase/analytics/FirebaseAnalytics')]
  JFirebaseAnalytics = interface(JObject)
    ['{536A10DE-A85D-4260-98AC-0311F43DC370}']
    procedure logEvent(name: JString; params: JBundle); cdecl;
    procedure setUserId(id: JString); cdecl;
  end;
  TJFirebaseAnalytics = class(TJavaGenericImport<JFirebaseAnalyticsClass, JFirebaseAnalytics>) end;

implementation

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseMessaging', TypeInfo(ALAndroidFirebaseApi.JFirebaseMessaging));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JALFirebaseMessagingService', TypeInfo(ALAndroidFirebaseApi.JALFirebaseMessagingService));
  TRegTypes.RegisterType('ALAndroidFirebaseApi.JFirebaseAnalytics', TypeInfo(ALAndroidFirebaseApi.JFirebaseAnalytics));
end;

initialization
  RegisterTypes;

end.
