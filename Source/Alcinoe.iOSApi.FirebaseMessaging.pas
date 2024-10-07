//
// Made from firebase-messaging 10.12.0
//
unit Alcinoe.iOSApi.FirebaseMessaging;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  //Pleast update <Alcinoe>\Libraries\ios\firebase\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.FirebaseMessaging.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.FirebaseMessaging.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of Firebase Messaging (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation;

{$M+}

type

  {***********************}
  FIRMessaging = interface;

  {******************************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebasemessaging/api/reference/Protocols/FIRMessagingDelegate.html
  FIRMessagingDelegate = interface(IObjectiveC)
  ['{9784786A-515F-41F0-84C3-8F298623275E}']
    procedure messaging(messaging: FIRMessaging; fcmToken: NSString); cdecl;
  end;

  {*********************************************************************************************}
  TFIRMessagingTokenWithCompletionHandler = procedure(token: NSString; error: NSError) of object;
  TFIRMessagingDeleteTokenWithCompletionHandler = procedure(error: NSError) of object;

  {***************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebasemessaging/api/reference/Classes/FIRMessaging
  FIRMessagingClass = interface(NSObjectClass)
  ['{FC9DDBCE-4C91-4DE4-B2FC-80289562D9F5}']
    {class} function messaging : Pointer{instancetype}; cdecl;
  end;
  FIRMessaging = interface(NSObject)
  ['{FCF96F2C-513B-409C-87D7-3FFE504EA79D}']
    procedure setDelegate(delegate: FIRMessagingDelegate); cdecl;
    function delegate : FIRMessagingDelegate; cdecl;
    procedure tokenWithCompletion(completion: TFIRMessagingTokenWithCompletionHandler); cdecl;
    procedure deleteTokenWithCompletion(completion: TFIRMessagingDeleteTokenWithCompletionHandler); cdecl;
  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;

implementation

uses
  Alcinoe.iOSApi.FirebaseCore; // [MANDATORY] Because we need it's initialization/finalization section

{*******************************************************************************}
procedure FirebaseMessagingLoader; cdecl; external framework 'FirebaseMessaging';

end.
