//
// Made from firebase-analytics 23.2.0
//
unit Alcinoe.AndroidApi.Firebase.Analytics;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Please run <Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorAndroid.bat
  //with the library identifiers com.google.firebase:firebase-analytics:xx.xx.xx where xx.xx.xx
  //is the last version of the firebase-analytics (You can find this version at
  //https://maven.google.com/web/index.html#com.google.firebase:firebase-analytics) and gave also the path to
  //<Alcinoe>\Source\Alcinoe.AndroidApi.Firebase.Analytics.pas to build the compare source file. Then make a diff
  //compare between the new generated Alcinoe.AndroidApi.Firebase.Analytics.pas and this one to see if the api
  //signature is still the same.
  {$MESSAGE WARN 'Check if the api signature of the last version of firebase sdk (android) is still the same'}
{$ENDIF}

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os;

type

  {*****************************}
  JFirebaseAnalytics = interface;

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

uses
  Alcinoe.Common;

{**********************}
procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Alcinoe.AndroidApi.Firebase.Analytics.JFirebaseAnalytics', TypeInfo(Alcinoe.AndroidApi.Firebase.Analytics.JFirebaseAnalytics));
end;

initialization
  {$IF defined(DEBUG)}
  ALLog('Alcinoe.AndroidApi.Firebase.Analytics','initialization');
  {$ENDIF}
  RegisterTypes;

end.