
//
//  This unit was (originaly) inspired by
//  DelphiWorlds PushClient project - https://github.com/DelphiWorlds/PushClient
//  http://www.delphiworlds.com
//  http://delphiworlds.com/2017/04/firebase-cloud-messaging-android-ios/
//  ------------------------------------------
//  A cross-platform method of using Firebase Cloud Messaging (FCM) to receive push notifications
//

//
// -----
//
// When i send a notification message, under android i can not set the largeIcon, so i must
// use data message and construct myself the notification inside the notification service
// who listen incoming data message.
//
// However, on IOS, their is no such "notification service" running in background and the
// app will receive the data message only when it's become foreground (as far as i understand).
// so it's seam on ios the only choice i have is to send notification message with are incompatible with android
//
// all of this mean that on the server we need to keep the token + the platform (ios/android) to know
// how to format the message.
//
// -----
//
// On android we build ourself the notification, so don't use any firebase "notification": {} payload
// but instead move it in the "data": { } section like the exemple below:
//
// {
//  "to": "zerz...REZ",
//  "data": {
//
//    "notification": "1",
//    "notification.tag": "myTag",
//    "notification.title": "TOTO",
//    "notification.smallicon": "notification_icon",
//    "notification.largeicon": "https://scontent.xx.fbcdn.net/v/t31.0-8/10296216_10152247072841144_868918792214465059_o.jpg?oh=b3d2aea191053493a1fede2fe2fba1fb&oe=5999CD99",
//    "notification.text": "I m toto",
//    "notification.vibrate": "1",
//    "notification.color":"152",
//    "notification.onlyalertonce":"1",
//    "notification.badgecount":"5",
//    "notification.number": "1",
//    "notification.ticker":"I m toto ticker",
//
//    "my_key" : { "my_other_value": "xxx" },
//    "my_custom_key" : "my_custom_value",
//    "other_key" : true
//
//  }
// }
//
// actually for the notification alert i support these params, but nothing forbid to extends them
//
// notification - Must be equal to 1 to activate showing of custom notification when no receiver
// notification.tag - A string identifier for this notification.
// notification.color - The accent color to use
// notification.text - Set the second line of text in the platform notification template.
// notification.title - Set the first line of text in the platform notification template.
// notification.largeicon - url of the large icon to use - Add a large icon to the notification content view
// notification.number - must equal to "auto" to increase the number of items this notification represents.
// notification.onlyalertonce - Set this flag if you would only like the sound, vibrate and ticker to be played if the notification is not already showing.
// notification.smallicon - The name of the desired resource. - Set the small icon resource, which will be used to represent the notification in the status bar.
// notification.ticker - Set the "ticker" text which is sent to accessibility services (The pop-up Text in Status Bar when the Notification is Called)
// notification.vibrate - must equal to 1 to activate the default vibration pattern (0, 1200)
// notification.visibility - Specify the value of visibility - One of VISIBILITY_PRIVATE (the default), VISIBILITY_SECRET, or VISIBILITY_PUBLIC.
// notification.priority - Relative priority for this notification
// notification.sound - Set the sound to play
// notification.badgecount - update the shortcut badge count with this number
// notification.present - only for IOS 10+, it's equal to 1 then even if the app is in foreground the notification will be presented to the end user
//
// ALFirebaseMessaging will append theses params :
//
// notification.presented - added by the ALFirebaseMessaging Framework to each notification presented to end user (so it's mean user tapped on it)
//
// -----
//
// On android you can choose to replace a notification alert with a new version (with the notification.tag in the data payload)
// but this is not possible under IOS
//
// -----
//
// iOS doesn't sum the badge numbers you send to the app. It just displays the latest badge number
// sent from your server. For example You server should send a push notification with badge number of 10 if
// that's the badge number you want to display.
//
// -----
//
// IOS 9: data message                 + app in FOREGROUND              : NO ALERT - NO BADGE - we receive the data message                                      via TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification
// IOS 9: data & notification message  + app FOREGROUND                 : NO ALERT - NO BADGE - we receive the data message                                      via TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification
// IOS 9: data message                 + app in BACKGROUND / NO RUNNING : NO ALERT - NO BADGE - WHEN the app will BECAME FOREGROUND: we receive the data message via TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification
// IOS 9: data & notification message  + app BACKGROUND / NO RUNNING    : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message  via TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification / TALFirebaseMessagingClient.applicationDidFinishLaunchingRemoteNotificationKey
//
// IOS 10: data message                + app in FOREGROUND              : NO ALERT - NO BADGE - we receive the data message                                      via TALFirebaseMessagingClient.TFIRMessagingDelegate.applicationReceivedRemoteMessage
// IOS 10: data & notification message + app FOREGROUND                 : NO ALERT - NO BADGE - we receive the data message                                      via TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterWillPresentNotificationWithCompletionHandler
// IOS 10: data message                + app in BACKGROUND / NO RUNNING : NO ALERT - NO BADGE - WHEN the app will BECAME FOREGROUND: we receive the data message via TALFirebaseMessagingClient.TFIRMessagingDelegate.applicationReceivedRemoteMessage
// IOS 10: data & notification message + app BACKGROUND / NO RUNNING    : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message  via TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler
//
// -----
//
// ANDROID: data message                        + app in FOREGROUND              : NO ALERT - NO BADGE - we receive the data message
// ANDROID: data & custom notification message  + app FOREGROUND                 : NO ALERT - NO BADGE - we receive the data message
// ANDROID: data message                        + app in BACKGROUND / NO RUNNING : NO ALERT - NO BADGE - WHEN the app will BECAME FOREGROUND: we receive the data message
// ANDROID: data & custom notification message  + app BACKGROUND / NO RUNNING    : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message
//
// -----

unit ALFirebase;

interface

uses system.Classes,
     system.Messaging,
     {$IF defined(android)}
     Androidapi.JNI.Embarcadero,
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.Helpers,
     Androidapi.JNIBridge,
     ALAndroidApi,
     ALAndroidFirebaseApi,
     {$ELSEIF defined(IOS)}
     System.TypInfo,
     iOSapi.Foundation,
     Macapi.ObjectiveC,
     ALIosUserNotificationsApi,
     ALIosFirebaseApi,
     {$ENDIF}
     ALStringList;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseMessagingClient = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseInstanceIdClientTokenRefreshEvent = procedure(const aToken: String) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseInstanceIdClient = class(TObject)
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TBroadcastReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
      private
        [Weak] fFirebaseInstanceIdClient: TALFirebaseInstanceIdClient;
      public
        constructor Create(const aFirebaseInstanceIdClient: TALFirebaseInstanceIdClient);
        procedure onReceive(context: JContext; intent: JIntent); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      ITokenRefreshNotification = interface(NSObject)
      ['{1AFBA293-4D77-41FD-8917-59E6C2E04002}']
        procedure onTokenRefresh(notification: Pointer); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TTokenRefreshNotificationListener = class(TOCLocal)
      private
        [Weak] fFirebaseInstanceIdClient: TALFirebaseInstanceIdClient;
      protected
        function GetObjectiveCClass: PTypeInfo; override;
      public
        constructor Create(const aFirebaseInstanceIdClient: TALFirebaseInstanceIdClient);
        procedure onTokenRefresh(notification: Pointer); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    fOnTokenRefresh: TALFirebaseInstanceIdClientTokenRefreshEvent;
    [weak] fFirebaseMessagingClient: TALFirebaseMessagingClient;

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    FBroadcastReceiverListener: TBroadcastReceiverListener;
    fBroadcastReceiver: JFMXBroadcastReceiver;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    fTokenRefreshNotificationListener: TTokenRefreshNotificationListener;
    procedure FIRInstanceIDdeleteIDHandler(error: NSError);
    {$ENDIF}
    {$ENDREGION}

  public
    constructor Create; virtual;
    destructor Destroy; override;
    property onTokenRefresh: TALFirebaseInstanceIdClientTokenRefreshEvent read fOnTokenRefresh write fOnTokenRefresh;
    property FirebaseMessagingClient: TALFirebaseMessagingClient read fFirebaseMessagingClient write fFirebaseMessagingClient;
    function getToken: String; virtual;
    procedure deleteInstanceId; virtual; // Resets Instance ID and revokes all tokens.
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseMessagingClientMessageReceivedEvent = procedure(const aPayload: TAlStringListU) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseMessagingClient = class(TObject)
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TBroadcastReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
      private
        [Weak] fFirebaseMessagingClient: TALFirebaseMessagingClient;
      public
        constructor Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
        procedure onReceive(context: JContext; intent: JIntent); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TUserNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
      strict private
        [Weak] fFirebaseMessagingClient: TALFirebaseMessagingClient;
      public
        constructor Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
        [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
        procedure userNotificationCenterWillPresentNotificationWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                     willPresentNotification: UNNotification;
                                                                                     withCompletionHandler: TUserNotificationCenterWillPresentNotificationCompletionHandler); cdecl;
        [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
        procedure userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                            didReceiveNotificationResponse: UNNotificationResponse;
                                                                                            withCompletionHandler: TUserNotificationCenterDidReceiveNotificationResponseCompletionHandler); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
      strict private
        [Weak] fFirebaseMessagingClient: TALFirebaseMessagingClient;
      public
        constructor Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
        procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    fOnMessageReceived: TALFirebaseMessagingClientMessageReceivedEvent;
    fOnAuthorizationRefused: TNotifyEvent;
    [weak] fFirebaseInstanceIdClient: TALFirebaseInstanceIdClient;
    fConnected: Boolean;

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    fIsPhysicallyConnected: Boolean;
    FBroadcastReceiverListener: TBroadcastReceiverListener;
    fBroadcastReceiver: JFMXBroadcastReceiver;
    FStartupIndentProcessed: Boolean;
    procedure applicationEvent(const Sender: TObject; const M: TMessage);
    procedure notificationEvent(const Sender: TObject; const M: TMessage);
    procedure HandleNotificationIntent(const Intent: JIntent);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    fUserNotificationCenterDelegate: TUserNotificationCenterDelegate;
    fFIRMessagingDelegate: TFIRMessagingDelegate;
    procedure UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
    procedure FIRMessagingConnectCompletionHandler(error: NSError);
    class procedure applicationDidFinishLaunchingRemoteNotificationKey(const Sender: TObject; const M: TMessage);
    procedure applicationDidReceiveRemoteNotification(const Sender: TObject; const M: TMessage);
    procedure applicationdidFailToRegisterForRemoteNotificationsWithError(const Sender: TObject; const M: TMessage);
    procedure applicationEvent(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    class var StartupNotificationMessage: String;
    {$ENDIF}
    {$ENDREGION}

  public
    constructor Create(const aFirebaseInstanceIdClient: TalFirebaseInstanceIdClient); virtual;
    destructor Destroy; override;
    procedure connect; virtual;
    procedure disconnect; virtual;
    procedure setBadgeCount(const aNewValue: integer; const extData: pointer = nil); virtual;
    property OnMessageReceived: TALFirebaseMessagingClientMessageReceivedEvent read fOnMessageReceived write fOnMessageReceived;
    property OnAuthorizationRefused: TNotifyEvent read fOnAuthorizationRefused write fOnAuthorizationRefused;
    property connected: boolean read fConnected;
  end;

implementation

uses system.SysUtils,
     fmx.platform,
     {$IF defined(android)}
     androidapi.JNI.JavaTypes,
     Androidapi.JNI.Os,
     FMX.Helpers.Android,
     FMX.platform.Android,
     ALAndroidShortcutBadgerApi,
     {$ELSEIF defined(IOS)}
     Macapi.Helpers,
     iOSapi.Helpers,
     iOSapi.CocoaTypes,
     IOSapi.UIKit,
     Macapi.ObjCRuntime,
     FMX.Helpers.iOS,
     ALMacapiObjCRuntime,
     {$ENDIF}
     AlString,
     alJsonDoc,
     alcommon;

/////////////////////////////////
// TALFirebaseInstanceIdClient //
/////////////////////////////////

{*********************************************}
constructor TALFirebaseInstanceIdClient.Create;
begin

  inherited Create;
  fOnTokenRefresh := nil;
  fFirebaseMessagingClient := Nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    // we listen for the broadcast message ACTION_TOKENREFRESHED to retrieve from
    // the new token
    FBroadcastReceiverListener := TBroadcastReceiverListener.Create(Self);
    fBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FBroadcastReceiverListener);
    TUIThreadCaller.Call<JFMXBroadcastReceiver>(
      procedure (aBroadcastReceiver: JFMXBroadcastReceiver)
      var aIntentFilter: JIntentFilter;
      begin
        aIntentFilter := TJIntentFilter.JavaClass.init(TJALFirebaseInstanceIdService.JavaClass.ACTION_TOKENREFRESHED);
        TJLocalBroadcastManager.javaclass.getInstance(TAndroidHelper.Context).registerReceiver(aBroadcastReceiver, aIntentFilter); // registerReceiver do synchronized (mReceivers) { } no need to callinUIThread
      end, fBroadcastReceiver);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    // Initialize Firebase in your app
    // Configure a FIRApp shared instance, typically in your application's
    // application:didFinishLaunchingWithOptions: method:
    // but in some way we create the TALFirebaseInstanceIdClient not in initialization section
    // but in formcreate section, and at this step didFinishLaunchingWithOptions is already ON
    // this is the normal flow:
    // 1) Unit initialization
    // 2) didFinishLaunchingWithOptions
    // 3) main form create
    // 4) BecameActive event received
    TFIRApp.OCClass.configure;

    // Monitor token generation
    // You can access the token's updated value by adding an observer that listens to kFIRInstanceIDTokenRefreshNotification
    // then retrieve the token from the observer's selector.
    // The registration token may change when:
    // * The app deletes Instance ID
    // * The app is restored on a new device
    // * The user uninstalls/reinstall the app
    // * The user clears app data.
    fTokenRefreshNotificationListener := TTokenRefreshNotificationListener.Create(Self);
    TiOSHelper.DefaultNotificationCenter.addObserver(fTokenRefreshNotificationListener.GetObjectID, // notificationObserver: Pointer;
                                                     sel_getUid('onTokenRefresh:'), // selector: SEL
                                                     (kFIRInstanceIDTokenRefreshNotification as ILocalObject).GetObjectID, // name: Pointer
                                                     nil); // &object: Pointer

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************}
destructor TALFirebaseInstanceIdClient.Destroy;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    FBroadcastReceiverListener.fFirebaseInstanceIdClient := nil;
    TUIThreadCaller.Call<JFMXBroadcastReceiver>(
      procedure (aBroadcastReceiver: JFMXBroadcastReceiver)
      begin
        TJLocalBroadcastManager.javaclass.getInstance(TAndroidHelper.Context).unregisterReceiver(aBroadcastReceiver); // unregisterReceiver do synchronized (mReceivers) { } no need to callinUIThread
      end, fBroadcastReceiver);
    FBroadcastReceiverListener := nil; // << we can not free it because it's maybe used by the previous instruction
                                       // << anyway it's not too much matter because with ARC it's will be free automatiquelly
    fBroadcastReceiver := Nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    TiOSHelper.DefaultNotificationCenter.removeObserver(fTokenRefreshNotificationListener.GetObjectID);
    AlFreeAndNil(fTokenRefreshNotificationListener);

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{****************************************************}
function TALFirebaseInstanceIdClient.getToken: String;
begin
  {$IF defined(android)}
  result := JstringToString(TJFirebaseInstanceId.javaclass.getInstance().getToken());
  {$ELSEIF defined(IOS)}
  result := NsStrToStr(TFIRInstanceID.Wrap(TFIRInstanceID.OCClass.instanceID).token)
  {$ENDIF}
end;

{*****************************************************}
procedure TALFirebaseInstanceIdClient.deleteInstanceId; // Resets Instance ID and revokes all tokens.
begin
  {$IF defined(android)}
  TJFirebaseInstanceId.javaclass.getInstance().deleteInstanceId;
  {$ELSEIF defined(IOS)}
  TFIRInstanceID.Wrap(TFIRInstanceID.OCClass.instanceID).deleteIDWithHandler(FIRInstanceIDdeleteIDHandler);
  {$ENDIF}
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{**************************************************************************************************************************************}
constructor TALFirebaseInstanceIdClient.TBroadcastReceiverListener.Create(const aFirebaseInstanceIdClient: TALFirebaseInstanceIdClient);
begin
  inherited Create;
  fFirebaseInstanceIdClient := aFirebaseInstanceIdClient;
end;

{*************************************************************************************************************}
procedure TALFirebaseInstanceIdClient.TBroadcastReceiverListener.onReceive(context: JContext; intent: JIntent);
var aToken: String;
begin

  aToken := JstringToString(intent.getStringExtra(StringToJstring('token')));
  {$IFDEF DEBUG}
  allog('TALFirebaseInstanceIdClient.TBroadcastReceiverListener.onReceive','Token: ' + aToken +
                                                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  {$IF CompilerVersion > 31} // berlin
    {$MESSAGE WARN 'remove TThread.synchronize because maybe not anymore needed in tokyo (look if now TThread.Current.ThreadID=MainThreadID)'}
  {$ENDIF}
  TThread.queue(nil,
    procedure
    begin
      if assigned(fFirebaseInstanceIdClient) and
         assigned(fFirebaseInstanceIdClient.fOnTokenRefresh) then fFirebaseInstanceIdClient.fOnTokenRefresh(aToken);
    end);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{*********************************************************************************************************************************************}
constructor TALFirebaseInstanceIdClient.TTokenRefreshNotificationListener.Create(const aFirebaseInstanceIdClient: TALFirebaseInstanceIdClient);
begin
  inherited Create;
  fFirebaseInstanceIdClient := aFirebaseInstanceIdClient;
end;

{************************************************************************************************************}
procedure TALFirebaseInstanceIdClient.TTokenRefreshNotificationListener.onTokenRefresh(notification: Pointer);
var aToken: String;
begin

  // Note that this callback will be fired everytime a new token is generated, including the first
  // time. So if you need to retrieve the token as soon as it is available this is where that
  // should be done.

  aToken := fFirebaseInstanceIdClient.getToken;
  {$IFDEF DEBUG}
  allog('TALFirebaseInstanceIdClient.TTokenRefreshNotificationListener.onTokenRefresh','Token: ' + aToken +
                                                                                       ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  // Connect again to FCM since connection may have failed when attempted before having a token.
  if assigned(fFirebaseInstanceIdClient.fFirebaseMessagingClient) and
     fFirebaseInstanceIdClient.fFirebaseMessagingClient.connected then fFirebaseInstanceIdClient.fFirebaseMessagingClient.connect;

  //execute fOnTokenRefresh
  if assigned(fFirebaseInstanceIdClient.fOnTokenRefresh) then
    fFirebaseInstanceIdClient.fOnTokenRefresh(aToken);

end;

{***************************************************************************************************}
function TALFirebaseInstanceIdClient.TTokenRefreshNotificationListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(ITokenRefreshNotification);
end;

{*********************************************************************************}
procedure TALFirebaseInstanceIdClient.FIRInstanceIDdeleteIDHandler(error: NSError);
begin
  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALFirebaseInstanceIdClient.FIRInstanceIDdeleteIDHandler', 'Unable to delete the tokens associated with the app identity - ' + NSStrToStr(error.localizedDescription) +
                                                                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else allog('TALFirebaseInstanceIdClient.FIRInstanceIDdeleteIDHandler', 'All the tokens associated with the app identity are deleted' +
                                                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;

{$ENDIF}
{$ENDREGION}




////////////////////////////////
// TALFirebaseMessagingClient //
////////////////////////////////

{**********************************************************************************************************}
constructor TALFirebaseMessagingClient.Create(const aFirebaseInstanceIdClient: TalFirebaseInstanceIdClient);

{$REGION ' IOS'}
{$IF defined(IOS)}
var aTypes: NSUInteger;
    aOptions: UNAuthorizationOptions;
    aSettings: UIUserNotificationSettings;
{$ENDIF}
{$ENDREGION}

begin

  inherited Create;
  fconnected := False;
  fOnMessageReceived := nil;
  fOnAuthorizationRefused := nil;
  fFirebaseInstanceIdClient := aFirebaseInstanceIdClient;
  fFirebaseInstanceIdClient.FirebaseMessagingClient := Self;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    //init fIsReallyConnected
    fIsPhysicallyConnected := False;
    FStartupIndentProcessed := False;

    //register message handler
    TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, applicationEvent);
    MainActivity.registerIntentAction(TJALFirebaseMessagingService.JavaClass.ACTION_MESSAGERECEIVED);
    TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, notificationEvent);

    //create the fBroadcastReceiver but no connect it
    FBroadcastReceiverListener := TBroadcastReceiverListener.Create(Self);
    fBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FBroadcastReceiverListener);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    //register message handler
    TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, applicationDidReceiveRemoteNotification);
    TMessageManager.DefaultManager.SubscribeToMessage(TPushFailToRegisterMessage, applicationdidFailToRegisterForRemoteNotificationsWithError);
    TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, applicationEvent);

    //fUserNotificationCenterDelegate
    fUserNotificationCenterDelegate := nil;
    fFIRMessagingDelegate := nil;

    // Register for remote notifications. This shows a permission dialog on first run, to
    // show the dialog at a more appropriate time move this registration accordingly.
    if TOSVersion.Check(10) then begin // iOS 10 or later

      // For iOS 10 display notification (sent via APNS)
      fUserNotificationCenterDelegate := TUserNotificationCenterDelegate.Create(self);
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.setdelegate(fUserNotificationCenterDelegate.GetObjectID);
      aOptions := UNAuthorizationOptionSound or
                  UNAuthorizationOptionAlert or
                  UNAuthorizationOptionBadge;
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.requestAuthorizationWithOptions(aOptions{options}, UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler{completionHandler});

      // For iOS 10 data message (sent via FCM)
      fFIRMessagingDelegate := TFIRMessagingDelegate.Create(self);
      TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).setRemoteMessageDelegate(fFIRMessagingDelegate.GetObjectID);

      // registerForRemoteNotifications
      SharedApplication.registerForRemoteNotifications;

    end
    else if TOSVersion.Check(8) then begin // iOS 8 or later

      aTypes := UIUserNotificationTypeSound or
                UIUserNotificationTypeAlert or
                UIUserNotificationTypeBadge;
      aSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(aTypes{types}, nil{categories}));
      sharedApplication.registerUserNotificationSettings(aSettings);
      SharedApplication.registerForRemoteNotifications;

    end
    else begin // iOS 7.1 or earlier.

      aTypes := UIRemoteNotificationTypeSound or
                UIRemoteNotificationTypeAlert or
                UIRemoteNotificationTypeBadge;
      SharedApplication.registerForRemoteNotificationTypes(Addr(aTypes));

    end;

  {$ENDIF}
  {$ENDREGION}

end;

{********************************************}
destructor TALFirebaseMessagingClient.Destroy;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    disconnect;

    TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, applicationEvent);
    TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, notificationEvent);

    FBroadcastReceiverListener.fFirebaseMessagingClient := nil;
    FBroadcastReceiverListener := nil; // << we can not free it because it's maybe used by the previous instruction
                                       // << anyway it's not too much matter because with ARC it's will be free automatiquelly
    fBroadcastReceiver := Nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    disconnect;

    TMessageManager.DefaultManager.Unsubscribe(TPushRemoteNotificationMessage, applicationDidReceiveRemoteNotification);
    TMessageManager.DefaultManager.Unsubscribe(TPushFailToRegisterMessage, applicationdidFailToRegisterForRemoteNotificationsWithError);
    TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, applicationEvent);

    if fUserNotificationCenterDelegate <> nil then begin
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.setdelegate(nil);
      alfreeAndNil(fUserNotificationCenterDelegate);
    end;

    if fFIRMessagingDelegate <> nil then begin
      TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).setRemoteMessageDelegate(nil);
      alFreeAndNil(fFIRMessagingDelegate);
    end;

  {$ENDIF}
  {$ENDREGION}

  fFirebaseInstanceIdClient.FirebaseMessagingClient := nil;
  inherited Destroy;

end;

{*******************************************}
procedure TALFirebaseMessagingClient.connect;
begin

  // set connected
  fconnected := true;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    if not fIsPhysicallyConnected then begin

      fIsPhysicallyConnected := True;
      TUIThreadCaller.Call<JFMXBroadcastReceiver>(
        procedure (aBroadcastReceiver: JFMXBroadcastReceiver)
        var aIntentFilter: JIntentFilter;
        begin
          aIntentFilter := TJIntentFilter.JavaClass.init(TJALFirebaseMessagingService.JavaClass.ACTION_MESSAGERECEIVED);
          TJLocalBroadcastManager.javaclass.getInstance(TAndroidHelper.Context).registerReceiver(aBroadcastReceiver, aIntentFilter); // registerReceiver do synchronized (mReceivers) { } no need to callinUIThread
        end, fBroadcastReceiver);

      {$IFDEF DEBUG}
      allog('TALFirebaseMessagingClient.connect', 'Physically connected', TalLogType.verbose);
      {$ENDIF}

    end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    // Won't connect since there is no token
    //the connection will be done automatiquelly in onTokenRefresh
    if fFirebaseInstanceIdClient.getToken = '' then exit;

    // Disconnect previous FCM connection if it exists.
    TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).disconnect;

    // connect to the FCM connection
    TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).connectWithCompletion(FIRMessagingConnectCompletionHandler);

  {$ENDIF}
  {$ENDREGION}

end;

{**********************************************}
procedure TALFirebaseMessagingClient.disconnect;
begin

  // set connected
  fconnected := False;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    if fIsPhysicallyConnected then begin

      fIsPhysicallyConnected := False;
      TUIThreadCaller.Call<JFMXBroadcastReceiver>(
        procedure (aBroadcastReceiver: JFMXBroadcastReceiver)
        begin
          TJLocalBroadcastManager.javaclass.getInstance(TAndroidHelper.Context).unregisterReceiver(aBroadcastReceiver);  // unregisterReceiver do synchronized (mReceivers) { } no need to callinUIThread
        end, fBroadcastReceiver);

      {$IFDEF DEBUG}
      allog('TALFirebaseMessagingClient.connect', 'Physically disconnect', TalLogType.verbose);
      {$ENDIF}

    end;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    // Disconnect previous FCM connection if it exists.
    TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).disconnect;

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************************************************************************}
procedure TALFirebaseMessagingClient.setBadgeCount(const aNewValue: integer; const extData: pointer = nil);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  If TJShortcutBadger.JavaClass.isBadgeCounterSupported(TAndroidHelper.Context) then
    TJShortcutBadger.JavaClass.applyCount(TAndroidHelper.Context, aNewValue);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  SharedApplication.setApplicationIconBadgeNumber(aNewValue);
  {$ENDIF}
  {$ENDREGION}

  // you must override this method to also reset the badgeCount on the Server side (because bullsheet ios don't support to increase
  // the badge count) - if you need extra data to do this, then you can pass it via extData

end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{***********************************************************************************************************************************}
constructor TALFirebaseMessagingClient.TBroadcastReceiverListener.Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
begin
  inherited Create;
  fFirebaseMessagingClient := aFirebaseMessagingClient;
end;

{************************************************************************************************************}
procedure TALFirebaseMessagingClient.TBroadcastReceiverListener.onReceive(context: JContext; intent: JIntent);
begin

  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.TBroadcastReceiverListener.onReceive','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  {$IF CompilerVersion > 31} // berlin
    {$MESSAGE WARN 'remove TThread.synchronize because maybe not anymore needed in tokyo (look if now TThread.Current.ThreadID=MainThreadID)'}
  {$ENDIF}
  TThread.queue(nil,
    procedure
    begin
      if assigned(fFirebaseMessagingClient) then
        fFirebaseMessagingClient.HandleNotificationIntent(intent);
    end);

end;

{***********************************************************************************************}
procedure TALFirebaseMessagingClient.notificationEvent(const Sender: TObject; const M: TMessage);
begin

  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.notificationEvent','ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if (M is TMessageReceivedNotification) then
    HandleNotificationIntent(TMessageReceivedNotification(M).Value);

end;

{**********************************************************************************************}
procedure TALFirebaseMessagingClient.applicationEvent(const Sender: TObject; const M: TMessage);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _handlePendingDataMessage;
  var aJsonDoc: TalJsonDocumentU;
      aMessagesNode: TalJsonNodeU;
      aPayload: TalStringListU;
      i: integer;
  begin
    Try
      aJsonDoc := TalJsonDocumentU.create;
      try
        aJsonDoc.LoadFromJSONString(JstringToString(TJALFirebaseMessagingService.JavaClass.getPendingDataMessages));
        aMessagesNode := aJsonDoc.ChildNodes.FindNode('messages');
        if (aMessagesNode <> nil) and assigned(fOnMessageReceived) then begin
          aPayload := TalStringListU.Create;
          try
            for I := 0 to aMessagesNode.ChildNodes.Count - 1 do begin
              aPayload.Clear;
              ALJSONToTStringsU(aMessagesNode.ChildNodes[i], aPayload);
              fOnMessageReceived(aPayload);
            end;
          finally
            alFreeAndNil(aPayload);
          end;
        end;
      finally
        ALFreeAndNil(aJsonDoc, false{adelayed}, false{aRefCountWarn});
      end;
    except
      {$IFDEF DEBUG}
      on E: Exception do
        allog('TALFirebaseMessagingClient.applicationEvent._handlePendingDataMessage', E.Message, TalLogType.ERROR);
      {$ENDIF}
    end;
  end;

var aWasConnected: Boolean;
begin
  if M is TApplicationEventMessage then begin
    case (M as TApplicationEventMessage).Value.Event of
      TApplicationEvent.WillBecomeForeground: begin
                                                if connected then connect;
                                                _handlePendingDataMessage;
                                              end;
      TApplicationEvent.BecameActive: begin
                                        if connected then connect;
                                        _handlePendingDataMessage;
                                        if not FStartupIndentProcessed then begin
                                          FStartupIndentProcessed := True;
                                          HandleNotificationIntent(MainActivity.getIntent); // it's seam that BecameActive will be fire after the formCreate so everything is OK if
                                        end;                                                // we create the TALFirebaseMessagingClient in the formCreate
                                      end;
      TApplicationEvent.EnteredBackground: begin
                                              aWasConnected := fconnected;
                                              disconnect;
                                              fconnected := aWasConnected;
                                            end;
      TApplicationEvent.WillBecomeInactive:; // << don't do anything here, because this event is fired when for exemple we open the keyboard
    end;
  end;
end;

{***********************************************************************************}
procedure TALFirebaseMessagingClient.HandleNotificationIntent(const Intent: JIntent);
var aPayload: TalStringListU;
    ABundle: JBundle;
    aIterator: JIterator;
    aKeyObj: JObject;
    aKeyStr: Jstring;
    aValueObj: JObject;
    aValueStr: String;
begin

  if (Intent = nil) or
     (Intent.getAction = nil) or
     (Intent.getAction.compareTo(TJALFirebaseMessagingService.JavaClass.ACTION_MESSAGERECEIVED) <> 0) then exit;

  aPayload := TalStringListU.Create;
  try

    ABundle := intent.getExtras;
    if ABundle <> nil then begin
      aIterator := ABundle.keySet.iterator;
      while aIterator.hasNext do begin
        //-----
        aKeyObj := aIterator.next;
        if aKeyObj = nil then continue;
        aKeyStr := aKeyObj.toString;
        //-----
        aValueObj := ABundle.&get(aKeyStr);
        if aValueObj = nil then aValueStr := ''
        else aValueStr := JStringToString(aValueObj.toString);
        //-----
        aPayload.Add(JStringToString(aKeyStr) + aPayload.NameValueSeparator + aValueStr);
        //-----
      end;
    end;

    {$IFDEF DEBUG}
    allog('TALFirebaseMessagingClient.HandleNotificationIntent','Payload: ' + aPayload.Text +
                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
    {$ENDIF}

    if assigned(fOnMessageReceived) then
      fOnMessageReceived(aPayload);

  finally
    ALFreeAndNil(aPayload);
  end;

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{****************************************************************************************************************************************}
constructor TALFirebaseMessagingClient.TUserNotificationCenterDelegate.Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
begin
  inherited Create;
  fFirebaseMessagingClient := aFirebaseMessagingClient;
end;

{********************************************************************}
function _NSDictionaryToJSON(const ADictionary: NSDictionary): string;
var LData: NSData;
    LString: NSString;
    LError: NSError;
begin
  LData := TNSJSONSerialization.OCClass.dataWithJSONObject((ADictionary as ILocalObject).GetObjectID, 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else Result := string.Empty;
end;

{******************************************************************************}
// IOS 10: Handle incoming notification messages while app is in the foreground.
procedure TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterWillPresentNotificationWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                                                                        willPresentNotification: UNNotification;
                                                                                                                                        withCompletionHandler: TUserNotificationCenterWillPresentNotificationCompletionHandler);
var aImp: procedure(aOptions: UNNotificationPresentationOptions); cdecl;
    aPresent: boolean;
    aOptions: UNNotificationPresentationOptions;
    aMessage: TPushRemoteNotificationMessage;
    aJsonDoc: TalJsonDocumentU;
    aJsonStr: String;
begin

  {$IF CompilerVersion > 31} // berlin
    {$MESSAGE WARN 'check if this is not already implemented in FMX.Platform.iOS'}
  {$ENDIF}

  // if 'notification.present' is in the data payload, then it's mean it's
  // an alert we want to show
  aJsonDoc := TalJsonDocumentU.create;
  try
    aJsonStr := _NSDictionaryToJSON(willPresentNotification.request.content.userInfo);
    if aJsonStr <> '' then aJsonDoc.LoadFromJSONString(aJsonStr);
    aOptions := UNNotificationPresentationOptionNone;
    aPresent := ALStrToBoolU(aJsonDoc.Node.GetChildNodeValueText('notification.present', '0'));
    if aPresent then begin
      aOptions := aOptions or UNNotificationPresentationOptionAlert;
      if aJsonDoc.Node.ChildNodes.FindNode('notification.badgecount') <> nil then aOptions := aOptions or UNNotificationPresentationOptionBadge;
      if aJsonDoc.Node.ChildNodes.FindNode('notification.sound') <> nil then aOptions := aOptions or UNNotificationPresentationOptionSound;
    end;
  finally
    ALFreeAndNil(aJsonDoc);
  end;

  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterWillPresentNotificationWithCompletionHandler', aJsonStr +
                                                                                                                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
  if not aPresent then begin
    aMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(aJsonStr));
    TMessageManager.DefaultManager.SendMessage(nil, aMessage);
  end;

  @aImp := imp_implementationWithBlock(withCompletionHandler);
  aImp(aOptions);
  imp_removeBlock(@aImp);

end;

{***************************************************************************************}
// IOS 10: Handle notification messages after display notification is tapped by the user.
procedure TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                                                                               didReceiveNotificationResponse: UNNotificationResponse;
                                                                                                                                               withCompletionHandler: TUserNotificationCenterDidReceiveNotificationResponseCompletionHandler); cdecl;
var aImp: procedure(); cdecl;
    aMessage: TPushRemoteNotificationMessage;
    aJsonDoc: TalJsonDocumentU;
    aJsonStr: String;
begin

  {$IF CompilerVersion > 31} // berlin
    {$MESSAGE WARN 'check if this is not already implemented in FMX.Platform.iOS'}
  {$ENDIF}

  aJsonDoc := TalJsonDocumentU.create;
  try
    aJsonStr := _NSDictionaryToJSON(didReceiveNotificationResponse.notification.request.content.userInfo);
    if aJsonStr <> '' then aJsonDoc.LoadFromJSONString(aJsonStr);
    if aJsonDoc.Node.ChildNodes.FindNode('notification.presented') = nil then
      aJsonDoc.Node.AddChild('notification.presented').Text := '1';
    aMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(aJsonDoc.JSON));
  finally
    ALFreeAndNil(aJsonDoc);
  end;
  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.TUserNotificationCenterDelegate.userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler', aMessage.Value.Notification +
                                                                                                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
  TMessageManager.DefaultManager.SendMessage(nil, aMessage);

  @aImp := imp_implementationWithBlock(withCompletionHandler);
  aImp();
  imp_removeBlock(@aImp);

end;

{******************************************************************************************************************************}
constructor TALFirebaseMessagingClient.TFIRMessagingDelegate.Create(const aFirebaseMessagingClient: TALFirebaseMessagingClient);
begin
  inherited Create;
  fFirebaseMessagingClient := aFirebaseMessagingClient;
end;

{**************************************************************}
// iOS 10: Receive data message  while app is in the foreground.
procedure TALFirebaseMessagingClient.TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
var aMessage: TPushRemoteNotificationMessage;
begin

  aMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(_NSDictionaryToJSON(remoteMessage.appData)));
  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.TFIRMessagingDelegate.applicationReceivedRemoteMessage', aMessage.Value.Notification +
                                                                                             ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
  TMessageManager.DefaultManager.SendMessage(nil, aMessage);

end;

{*********************************************************************************************************************}
procedure TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification(const Sender: TObject; const M: TMessage);
var aPayload: TalStringListU;
    aJsonDoc: TalJsonDocumentU;
    aJsonStr: String;
begin

  aPayload := TalStringListU.Create;
  try

    if (M is TPushRemoteNotificationMessage) then begin

      //ios 9-
      if not TOSVersion.Check(10) then begin

        // Just received notification (Foreground)
        if sharedApplication.applicationState = UIApplicationStateActive then begin
          aJsonStr := (M as TPushRemoteNotificationMessage).Value.Notification
        end

        // launched by taping notification
        else if sharedApplication.applicationState = UIApplicationStateInactive then begin
          aJsonDoc := TalJsonDocumentU.create;
          try
            aJsonStr := (M as TPushRemoteNotificationMessage).Value.Notification;
            if aJsonStr <> '' then aJsonDoc.LoadFromJSONString(aJsonStr);
            if ((ALStrToBoolU(aJsonDoc.Node.GetChildNodeValueText('notification', '0'))) or
                (aJsonDoc.Node.childnodes.findnode('aps') <> nil)) and
               (aJsonDoc.Node.ChildNodes.FindNode('notification.presented') = nil) then
              aJsonDoc.Node.AddChild('notification.presented').Text := '1';
            aJsonStr := aJsonDoc.JSON;
          finally
            ALFreeAndNil(aJsonDoc);
          end;
        end

        // Just received notification (Background)
        else if sharedApplication.applicationState = UIApplicationStateBackground then begin
          aJsonStr := (M as TPushRemoteNotificationMessage).Value.Notification
        end;
        
      end

      //ios 10+
      else aJsonStr := (M as TPushRemoteNotificationMessage).Value.Notification

    end
    
    else if (M is TPushStartupNotificationMessage) then begin
      aJsonDoc := TalJsonDocumentU.create;
      try
        aJsonStr := (M as TPushStartupNotificationMessage).Value.Notification;
        if aJsonStr <> '' then aJsonDoc.LoadFromJSONString(aJsonStr);
        if aJsonDoc.Node.ChildNodes.FindNode('notification.presented') = nil then
          aJsonDoc.Node.AddChild('notification.presented').Text := '1';
        aJsonStr := aJsonDoc.JSON;
      finally
        ALFreeAndNil(aJsonDoc);
      end;
    end
    
    else aJsonStr := '';

    {$IFDEF DEBUG}
    allog('TALFirebaseMessagingClient.applicationDidReceiveRemoteNotification', aJsonStr +
                                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
    {$ENDIF}

    if aJsonStr <> '' then ALJSONToTStringsU(aJsonStr, ALDefaultFormatSettingsU, aPayload);
        
    if assigned(fOnMessageReceived) then
      fOnMessageReceived(aPayload);

  finally
    ALFreeAndNil(aPayload);
  end;

end;

{**************************************************************************************************************************************}
class procedure TALFirebaseMessagingClient.applicationDidFinishLaunchingRemoteNotificationKey(const Sender: TObject; const M: TMessage);
begin
  if (M is TPushStartupNotificationMessage) then TALFirebaseMessagingClient.StartupNotificationMessage := (M as TPushStartupNotificationMessage).Value.Notification;
end;

{********************************************************************************************************************************************}
procedure TALFirebaseMessagingClient.UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin

 // If the local or remote notifications of your app or app extension interact
 // with the user in any way, you must call this method to request authorization
 // for those interactions. The first time your app ever calls the method, the
 // system prompts the user to authorize the requested options. The user may
 // respond by granting or denying authorization, and the system stores the user�s
 // response so that subsequent calls to this method do not prompt the user again.
 // The user may change the allowed interactions at any time. Use the
 // getNotificationSettingsWithCompletionHandler: method to determine what your
 // app is allowed to do.

  {$IFDEF DEBUG}
  allog('TALFirebaseMessagingClient.UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler', 'granted: ' + ALBoolToStrU(granted) +
                                                                                                             ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

 if (not granted) and assigned(fOnAuthorizationRefused) then begin
  TThread.Synchronize(nil, // << Strangely it's seam this function is not called from the mainThread
    procedure
    begin
      fOnAuthorizationRefused(self);
    end);
 end;

end;

{****************************************************************************************}
procedure TALFirebaseMessagingClient.FIRMessagingConnectCompletionHandler(error: NSError);
var aPushStartupNotificationMessage: TPushStartupNotificationMessage;
begin

  // The handler to be invoked once the connection is established.
  // If the connection fails we invoke the handler with an
  // appropriate error code letting you know why it failed. At
  // the same time, FIRMessaging performs exponential backoff to retry
  // establishing a connection and invoke the handler when successful.
  //
  // NOTE: i see FIRMessaging performs exponential backoff to retry
  // establishing a connection but this event is not called again

  {$IFDEF DEBUG}
  if (error <> nil) then allog('TALFirebaseMessagingClient.FIRMessagingConnectCompletionHandler', 'Unable to connect to FCM - ' + NSStrToStr(error.localizedDescription) +
                                                                                                  ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else allog('TALFirebaseMessagingClient.FIRMessagingConnectCompletionHandler', 'Connected to FCM' +
                                                                                ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  // handle the TALFirebaseMessagingClient.StartupNotificationMessage
  // i do it here because i don't want this event to be executed during
  // the oncreate or the connect procedure, but in annother distinct synch loop
  if TALFirebaseMessagingClient.StartupNotificationMessage <> '' then begin
    aPushStartupNotificationMessage := TPushStartupNotificationMessage.Create(TPushNotificationData.Create(TALFirebaseMessagingClient.StartupNotificationMessage));
    TALFirebaseMessagingClient.StartupNotificationMessage := '';
    applicationDidReceiveRemoteNotification(Self, aPushStartupNotificationMessage);
  end;

end;

{*****************************************************************************************************************************************}
procedure TALFirebaseMessagingClient.applicationdidFailToRegisterForRemoteNotificationsWithError(const Sender: TObject; const M: TMessage);
begin

  // After you call the registerForRemoteNotifications method of the UIApplication object, the app calls
  // this method when there is an error in the registration process.
  //
  // application:didFailToRegisterForRemoteNotificationsWithError: gets called when the app is signed
  // with incorrect provisioning profile.
  //
  // NOTE: don't know what else to do here except loging the error

  {$IFDEF DEBUG}
  if (M is TPushFailToRegisterMessage) then allog('TALFirebaseMessagingClient.applicationdidFailToRegisterForRemoteNotificationsWithError', 'Unable to register for remote notifications'+
                                                                                                                                            ' - ' + (M as TPushFailToRegisterMessage).Value.ErrorMessage +
                                                                                                                                            ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else allog('TALFirebaseMessagingClient.applicationdidFailToRegisterForRemoteNotificationsWithError', 'Unable to register for remote notifications' +
                                                                                                       ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

end;

{**********************************************************************************************}
procedure TALFirebaseMessagingClient.applicationEvent(const Sender: TObject; const M: TMessage);
begin
  if M is TApplicationEventMessage then begin
    case (M as TApplicationEventMessage).Value.Event of
      TApplicationEvent.WillBecomeForeground,
      TApplicationEvent.BecameActive: begin
                                        if connected then connect;
                                      end;
      TApplicationEvent.EnteredBackground: begin
                                              // Call this before `teardown` when your app is going to the background.
                                              // Since the FIRMessaging connection won't be allowed to live when in background it is
                                              // prudent to close the connection.
                                              TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).disconnect;
                                              {$IFDEF DEBUG}
                                              allog('TALFirebaseMessagingClient.applicationEvent','Disconnected from FCM', TalLogType.VERBOSE);
                                              {$ENDIF}
                                            end;
      TApplicationEvent.WillBecomeInactive:; // << don't do anything here, because this event is fired when for exemple we open notification center
                                             // << NOTE: i notice that even calling disconnect from here don't really disconnect the FCM as we still
                                             // <<       receive the notification in the app
    end;
  end;
end;

{$ENDIF}
{$ENDREGION}


initialization

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TALFirebaseMessagingClient.StartupNotificationMessage := '';
  if not TOSVersion.Check(10) then
    TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, TALFirebaseMessagingClient.applicationDidFinishLaunchingRemoteNotificationKey);
  {$ENDIF}
  {$ENDREGION}

finalization

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  if not TOSVersion.Check(10) then
    TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, TALFirebaseMessagingClient.applicationDidFinishLaunchingRemoteNotificationKey);
  {$ENDIF}
  {$ENDREGION}

end.

