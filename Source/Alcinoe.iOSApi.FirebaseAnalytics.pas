//
// Made from firebase-messaging 10.12.0
//
unit Alcinoe.iOSApi.FirebaseAnalytics;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported132}
  //Pleast update <Alcinoe>\Libraries\ios\Firebase\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.FirebaseAnalytics.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.FirebaseAnalytics.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of Firebase sdk (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation;

{$M+}

type

  {***********************}
  FIRAnalytics = interface;

  {***************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebaseanalytics/api/reference/Classes/FIRAnalytics
  FIRAnalyticsClass = interface(NSObjectClass)
  ['{26742CFD-49FB-4403-BF2A-B78A6A1AE8C9}']
    {class} procedure logEventWithName(name: NSString; parameters: NSDictionary); cdecl;
    {class} procedure setUserPropertyString(value: NSString; forName: NSString); cdecl;
    {class} procedure setUserID(userID: NSString); cdecl;
    {class} function appInstanceID : NSString; cdecl;
  end;
  FIRAnalytics = interface(NSObject)
  ['{0ABE1BC8-6B8E-44E3-939F-3F08626E14CC}']
  end;
  TFIRAnalytics = class(TOCGenericImport<FIRAnalyticsClass, FIRAnalytics>)  end;

implementation

uses
  Alcinoe.iOSApi.FirebaseCore; // [MANDATORY] Because we need it's initialization/finalization section

{*******************************************************************************}
procedure FirebaseAnalyticsLoader; cdecl; external framework 'FirebaseAnalytics';

end.