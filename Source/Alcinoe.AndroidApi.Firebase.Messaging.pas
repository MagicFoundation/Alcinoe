//
// Made from firebase-messaging 23.2.0
//
unit Alcinoe.AndroidApi.Firebase.Messaging;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.google.firebase:firebase-messaging:xx.xx.xx where xx.xx.xx
  //is the last version of the firebase-messaging (You can find this version at
  //https://maven.google.com/web/index.html#com.google.firebase:firebase-messaging) and gave also the path to
  //<Alcinoe>\Source\Alcinoe.AndroidApi.Firebase.Messaging.pas to build the compare source file. Then make a diff
  //compare between the new generated Alcinoe.AndroidApi.Firebase.Messaging.pas and this one to see if the api
  //signature is still the same.
  {$MESSAGE WARN 'Check if the api signature of the last version of firebase sdk (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.PlayServices.Tasks,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.AndroidX;

type

  {*****************************}
  JFirebaseMessaging = interface;
  JALFirebaseMessagingService = interface;

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
  [JavaSignature('io/magicfoundation/alcinoe/firebase/messaging/ALFirebaseMessagingService')]
  JALFirebaseMessagingService = interface(JObject)
    ['{9F3FF329-E17E-41B4-9C4A-214AF6A1FC05}']
  end;
  TJALFirebaseMessagingService = class(TJavaGenericImport<JALFirebaseMessagingServiceClass, JALFirebaseMessagingService>) end;

implementation

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Firebase.Messaging.JFirebaseMessaging', TypeInfo(Alcinoe.AndroidApi.Firebase.Messaging.JFirebaseMessaging));
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Firebase.Messaging.JALFirebaseMessagingService', TypeInfo(Alcinoe.AndroidApi.Firebase.Messaging.JALFirebaseMessagingService));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.Firebase.Messaging','initialization');
  {$ENDIF}
  RegisterTypes;

end.
