//
// Made from firebase-messaging 10.12.0
//
unit Alcinoe.iOSApi.FirebaseMessaging;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported131}
  //Pleast update <Alcinoe>\Libraries\ios\Firebase\ to the last one and then run
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

  {************************************}
  FIRMessagingAPNSTokenType = NSInteger;

const

  {***********************************}
  FIRMessagingAPNSTokenTypeUnknown = 0;
  FIRMessagingAPNSTokenTypeSandbox = 1;
  FIRMessagingAPNSTokenTypeProd = 2;

type

  {***********************}
  FIRMessaging = interface;

  {******************************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebasemessaging/api/reference/Protocols/FIRMessagingDelegate.html
  FIRMessagingDelegate = interface(IObjectiveC)
  ['{9784786A-515F-41F0-84C3-8F298623275E}']
    procedure messaging(messaging: FIRMessaging; didReceiveRegistrationToken: NSString); cdecl;
  end;

  {*******************************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebasemessaging/api/reference/Classes/FIRMessagingMessageInfo.html
  FIRMessagingMessageInfoClass = interface(NSObjectClass)
  ['{163EC455-1240-4524-9C63-EBF04E7FE67F}']
  end;
  FIRMessagingMessageInfo = interface(NSObject)
  ['{5172B797-F014-4A90-8690-4A255D493B7B}']
  end;

  {*********************************************************************************************}
  TFIRMessagingTokenWithCompletionHandler = procedure(token: NSString; error: NSError) of object;
  TFIRMessagingDeleteTokenWithCompletionHandler = procedure(error: NSError) of object;

  {***************************************************************************************************}
  //https://firebase.google.com/docs/reference/ios/firebasemessaging/api/reference/Classes/FIRMessaging
  FIRMessagingClass = interface(NSObjectClass)
  ['{FC9DDBCE-4C91-4DE4-B2FC-80289562D9F5}']
    {class} function messaging: FIRMessaging; cdecl;
  end;
  FIRMessaging = interface(NSObject)
  ['{FCF96F2C-513B-409C-87D7-3FFE504EA79D}']
    procedure setDelegate(delegate: FIRMessagingDelegate); cdecl;
    function delegate: FIRMessagingDelegate; cdecl;
    procedure setAPNSToken(APNSToken: NSData); cdecl; overload;
    procedure setAPNSToken(apnsToken: NSData; &type: FIRMessagingAPNSTokenType); cdecl; overload;
    function APNSToken: NSData; cdecl;
    function FCMToken: NSString; cdecl;
    procedure tokenWithCompletion(completion: TFIRMessagingTokenWithCompletionHandler); cdecl;
    procedure deleteTokenWithCompletion(completion: TFIRMessagingDeleteTokenWithCompletionHandler); cdecl;
    function appDidReceiveMessage(message: NSDictionary): FIRMessagingMessageInfo; cdecl;
  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;

implementation

uses
  Alcinoe.iOSApi.FirebaseCore; // [MANDATORY] Because we need it's initialization/finalization section

{*******************************************************************************}
procedure FirebaseMessagingLoader; cdecl; external framework 'FirebaseMessaging';

end.