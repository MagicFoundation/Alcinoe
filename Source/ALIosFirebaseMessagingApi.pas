unit ALIosFirebaseMessagingApi;

interface

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
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure messagingDidReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
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
  ALiOSFirebaseCommonAPI;

procedure FirebaseMessagingLoader; cdecl; external framework 'FirebaseMessaging';

end.
