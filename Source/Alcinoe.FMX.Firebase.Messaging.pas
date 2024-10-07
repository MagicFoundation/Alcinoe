(*******************************************************************************
 A cross-platform method of using Firebase Cloud Messaging (FCM) to receive
 push notifications

Setup (ANDROID)
---------------

On android you just need to include the library io.magicfoundation.alcinoe:alcinoe-firebase-messaging:1.0.0
in the project. You also need to include google-services.json. You can do all
of this with the help of AndroidMerger. you can see an exemple in
<Alcinoe>\Demos\ALNotificationService\_source\android\MergeLibraries.bat


Setup (IOS)
-----------

1) In the Project > Option > Building > Delphi Compiler > Linking > Options
   passed to the LD linker add -ObjC (like asked in
   https://firebase.google.com/docs/ios/setup#integrate-without-swift-pm)

2) In project > option > Building > Delphi Compiler > FRAMEWORK search path
   you need to add the following path:
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\FirebaseInstallations.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\FirebaseCore.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\FirebaseCoreInternal.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseMessaging\FirebaseMessaging.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseAnalytics\nanopb.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\firebase\FirebaseMessaging\GoogleDataTransport.xcframework\ios-arm64

3) Under Tools, Options, SDK Manager you will need to add the following frameworks

   for ios64:
   ----------

   Path on remote machine                                                                    |  File mask |  Path Type     |  Include subdirectories
   $(SDKROOT)/usr/lib/swift                                                                  |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/iphoneos      |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift-5.0/iphoneos  |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift-5.5/iphoneos  |  *         |  Library path  |  no

   https://quality.embarcadero.com/browse/RSP-38700
   You will need to manually copy the content of (assuming c:\SDKs is your BDSPLATFORMSDKSDIR)
   c:\SDKs\iPhoneOSXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift
   c:\SDKs\iPhoneOSXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift-5.0  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift-5.0
   c:\SDKs\iPhoneOSXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift-5.5  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift-5.5

   for Ios64 simulator:
   --------------------

   $(SDKROOT)/usr/lib/swift                                                                         |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift/iphonesimulator      |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift-5.0/iphonesimulator  |  *         |  Library path  |  no
   $(SDKROOT)/../../../../../Toolchains/XcodeDefault.xctoolchain/usr/lib/swift-5.5/iphonesimulator  |  *         |  Library path  |  no

   https://quality.embarcadero.com/browse/RSP-38700
   You will need to manually copy the content of (assuming c:\SDKs is your BDSPLATFORMSDKSDIR)
   c:\SDKs\iPhoneSimulatorXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift
   c:\SDKs\iPhoneSimulatorXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift-5.0  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift-5.0
   c:\SDKs\iPhoneSimulatorXXX.sdk\Applications\Xcode.app\Contents\Developer\Toolchains\XcodeDefault.xctoolchain\usr\lib\swift-5.5  to  c:\SDKs\iPhoneOSXXX.sdk\usr\lib\swift-5.5

4) In the Project > Option > Building > Delphi Compiler > Linking >
   Options passed to the LD linker add -rpath /usr/lib/swift
   this because of this bug: https://quality.embarcadero.com/browse/RSP-38700

5) You must deploy the GoogleService-Info.plist to the root of the app. You
   can use DeployMan for this. you can see an exemple in
   <Alcinoe>\Demos\ALNotificationService\_source\ios\DeployMan.bat.
   All info regarding generating the GoogleService-Info.plist can be found at
   https://firebase.google.com/docs/ios/setup

6) If you want the iOS app to be able to receive data notification in
   background you must add in info.plist.TemplateiOS.xml

     <key>UIBackgroundModes</key>
     <array>
       <string>remote-notification</string>
     </array>

7) If you want to show an image in an ios alert, you must add a service
   extension to the project.

   Their is actually 1 difficulties with this way: The bundle identifier of the
   app extension must start with the bundle identifier of the delphi project.
   That mean we can not create a reusable app extension for different projects.
   https://stackoverflow.com/questions/74854659/how-to-change-from-command-line-the-bundle-identifier-of-an-app-extension

   You can use the template of an app extension:
   * Replace in <alcinoe>\References\iOSNotification\iOSNotification.xcodeproj\project.pbxproj
     all occurences of io.magicfoundation.alcinoe.alnotificationservicedemo by the bundle identifiers
     of your delphi app
   * copy the content of <alcinoe>\References\iOSNotification\ somewhere in
     the macos. ex: /Users/<username>/Documents/iOSNotification
   * in the macos run
     xcodebuild -project /Users/<username>/Documents/iOSNotification/iOSNotification.xcodeproj -scheme iOSNotification -configuration Release -sdk iphoneos CONFIGURATION_BUILD_DIR=/Users/<username>/Documents/Compiled
   * copy the content of /Users/<username>/Documents/Compiled/iOSNotification.app/PlugIns/
     in a local folder to your project and with DeployMan instruct the dproj
     to deploy those file with your app. see an exemple of the DeployMan
     command in <Alcinoe>\Demos\ALNotificationService\_source\ios\DeployMan.bat

   Or You can create the app extension in the following way:
   * Launch Xcode and select "create a new xcode project"
   * Select ios app as a template for the new project
     * Enter the productname you want. Ex: iOSNotification
     * Select your team (you need one just to be able to compile the
       project)
     * Enter the organization identifier of your project. Ex:
       io.magicfoundation.alcinoe.alnotificationservicedemo, doesn't matter if not exact you will
       change it later
     * Select SwiftUI for the Interface
     * Select Swift for the language
     * Uncheck "use core data"
     * Uncheck "Include tests"
     * Click "Next" and Save it anywhere you want. ex:
       /Users/<username>/Documents and do not forget in the finder dialog
       to uncheck "create git repository on my mac"
   * In Xcode, with your app project open, navigate to File > Add Packages.
   * When prompted, add the Firebase Apple platforms SDK repository:
     https://github.com/firebase/firebase-ios-sdk
     * Dependancy rule: Exact version - 10.2.0 (or the version of firebase
       you use)
     * click "Add Package"
     * Choose only FirebaseMessaging library and click again to
       "Add Package"
   * In Xcode, select File > New > Target to add a new target to your
     project.
   * In the iOS > Application Extension section, select the Notification
     Service Extension target.
     * Enter the productname you want. Ex NotificationService
     * Select your team (you need one just to be able to compile the
       project)
     * Language: Objective-c
     * Project: The main project
     * Embed in application: The main project
     * click "finish"
     * in the popup dialog Activate "notificationservice" scheme: click
       cancel
   * in the left panel of xcode select your project. Ex ALiOSNotification
   * In target select the app. Ex ALiOSNotification
     * Select the General tab
       * Minimum Deployments: iOS 15
     * Select the signing & capabilities tab
       * Select "ALL" capabillity
       * Bundle Identifier: the bundle identifier of your delphi app
         (without the team id) Ex: io.magicfoundation.alcinoe.alnotificationservicedemo
   * In target select the service extension. Ex NotificationService
     * Select the General tab
       * Deployment info: iOS 11
       * Under "Framework and Libraries" click on the "+"
         * Select FirebaseMessaging
       * then select the signing & capabilities tab
         * Bundle Identifier: the bundle identifier of your delphi app
           (without the team id) + .notificationservice. Ex:
           io.magicfoundation.alcinoe.alnotificationservicedemo.notificationservice
   * in the left panel of xcode select the app extension.
     Ex NotificationService
   * Select the file NotificationService.m
     * add below the line #import "NotificationService.h":
       #import "FirebaseMessaging/FirebaseMessaging.h"
     * remove the line:
       // Modify the notification content here as you wish
       self.bestAttemptContent.title = [NSString stringWithFormat:@"%@ [modified]", self.bestAttemptContent.title];
     * replace self.contentHandler(self.bestAttemptContent); by:
       [[FIRMessaging extensionHelper] populateNotificationContent:self.bestAttemptContent withContentHandler:contentHandler];
   * in xcode, do File > Save
   * close xcode
   * from a command line build the project with a command line like for exemple :
     xcodebuild -project /Users/<username>/Documents/iOSNotification/iOSNotification.xcodeproj -scheme iOSNotification -configuration Release -sdk iphoneos CONFIGURATION_BUILD_DIR=/Users/<username>/Documents/Compiled
   * copy the content of /Users/<username>/Documents/Compiled/iOSNotification.app/PlugIns/
     in a local folder to your project and with DeployMan instruct the dproj
     to deploy those file with your app. see an exemple of the DeployMan
     command in <Alcinoe>\Demos\ALNotificationService\_source\ios\DeployMan.bat


Regarding Badge
---------------

iOS doesn't sum the badge numbers you send to the app. It just displays the
latest badge number sent from your server. For example You server should send
a push notification with badge number of 10 if that's the badge number you
want to display.

  const payload = {
    ...
    "aps":{
      "alert":"test alert",
      "badge":5,
      "sound":"default"
    }
  }

 https://developer.android.com/develop/ui/views/notifications/badges
 on android, By default, each notification increments a number displayed on
 the long- press menu (visible in figure 1), but you can override this number
 for your app


Note
----

To know when a notification was "tapped" by an user you can look in the data
payload for the presence of alcinoe.notification_clicked=1 It's added by the
ALFirebaseMessaging Framework to each notification presented to end user so
it's mean user tapped on it. This is the behavior of the notification in iOS/android:

IOS: data notification          + app in FOREGROUND     : NO ALERT - NO BADGE - we receive the data message         - via TApplicationDelegate.applicationDidReceiveRemoteNotificationWithFetchCompletionHandler                            > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: alert notification         + app in FOREGROUND     : NO ALERT - NO BADGE - we receive the alert message        - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:notification:completionHandler > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data + alert notification  + app in FOREGROUND     : NO ALERT - NO BADGE - we receive the alert + data message - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:notification:completionHandler > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
---
IOS: data notification          + app in BACKGROUND (1) : NO ALERT - NO BADGE - we receive the data message                                                                             - via TApplicationDelegate.applicationDidReceiveRemoteNotificationWithFetchCompletionHandler                        > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data notification          + app in BACKGROUND (2) : NO ALERT - NO BADGE - WHEN the app will BECAME FOREGROUND: we receive only the LAST SENT data message                         - via TApplicationDelegate.applicationDidReceiveRemoteNotificationWithFetchCompletionHandler                        > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data notification          + app in BACKGROUND (3) : NO ALERT - NO BADGE - we NEVER receive the data message
IOS: alert notification         + app in BACKGROUND     : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the alert message (with alcinoe.notification_clicked=1)        - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:response:completionHandler > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data + alert notification  + app in BACKGROUND     : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the alert + data message (with alcinoe.notification_clicked=1) - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:response:completionHandler > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
---
IOS: data notification          + app NOT RUNNING (1)   : NO ALERT - NO BADGE - we receive the data message                                                                                                         - via TApplicationDelegate.applicationDidFinishLaunchingWithOptions > TALFirebaseMessaging.StartupNotificationMessageHandler > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.applicationDidReceiveRemoteNotification  | and also via TApplicationDelegate.applicationDidReceiveRemoteNotificationWithFetchCompletionHandler > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data notification          + app NOT RUNNING (2)   : NO ALERT - NO BADGE - WHEN the app will BECAME FOREGROUND: we receive only the LAST SENT data message                                                     - via TApplicationDelegate.applicationDidReceiveRemoteNotificationWithFetchCompletionHandler                                                                                           > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: alert notification         + app NOT RUNNING       : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the notification message (with alcinoe.notification_clicked=1)                       - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:response:completionHandler                                                                    > TALFirebaseMessaging.applicationDidReceiveRemoteNotification  | and also via TApplicationDelegate.applicationDidFinishLaunchingWithOptions > TALFirebaseMessaging.StartupNotificationMessageHandler > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.applicationDidReceiveRemoteNotification
IOS: data + alert notification  + app NOT RUNNING       : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message and the notification message (with alcinoe.notification_clicked=1)  - via TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:response:completionHandler                                                                    > TALFirebaseMessaging.applicationDidReceiveRemoteNotification  | and also via TApplicationDelegate.applicationDidFinishLaunchingWithOptions > TALFirebaseMessaging.StartupNotificationMessageHandler > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.applicationDidReceiveRemoteNotification

  (1) We are lucky (for exemple We didn't launch any other app from the time the app goes in background and in the info.plist we have <key>UIBackgroundModes</key><array><string>remote-notification</string></array> and we follow https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/pushing_background_updates_to_your_app?language=objc)
  (2) We was unlucky in (1) (For exemple we launch another app from the time the app goes in background)
  (3) We receive the data message when the app was in background and then the user (or the system) kill the app

  NOTE: All of this is just a total bullsheet from apple/ios as it's mean you will never know if you will fall in (1), (2),
  (3) or even in another case I didn't observe here.
  That mean on ios data notifications are totaly useless in most of case! in firebase legacy HTTP it's was corrected
  by the firebase messaging framework but in firebase http V1 they deprecated this essential feature:
  https://stackoverflow.com/questions/74789738/on-ios-only-last-sent-data-message-is-retrieved-when-the-app-back-online

-----

ANDROID: data notification          + app in FOREGROUND : NO ALERT - NO BADGE - we receive the data message                        - via ALFirebaseMessagingService.onMessageReceived > TALFirebaseMessaging.TNewMessageObserver.onChanged
ANDROID: alert notification         + app in FOREGROUND : NO ALERT - NO BADGE - we receive the notification message                - via ALFirebaseMessagingService.onMessageReceived > TALFirebaseMessaging.TNewMessageObserver.onChanged
ANDROID: data + alert notification  + app in FOREGROUND : NO ALERT - NO BADGE - we receive the motification message + data message - via ALFirebaseMessagingService.onMessageReceived > TALFirebaseMessaging.TNewMessageObserver.onChanged
-------
ANDROID: data notification          + app in BACKGROUND : NO ALERT - NO BADGE - we receive the data message                                                                                                        - via ALFirebaseMessagingService.onMessageReceived > TALFirebaseMessaging.TNewMessageObserver.onChanged
ANDROID: alert notification         + app in BACKGROUND : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the notification message (with alcinoe.notification_clicked=1)                      - via TALFirebaseMessaging.StartupNotificationMessageHandler (TMessageReceivedNotification) > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.ReceiveStartupNotificationMessage
ANDROID: data + alert notification  + app in BACKGROUND : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message and the notification message (with alcinoe.notification_clicked=1) - via TALFirebaseMessaging.StartupNotificationMessageHandler (TMessageReceivedNotification) > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.ReceiveStartupNotificationMessage
-------
ANDROID: data notification          + app NOT RUNNING   : NO ALERT - NO BADGE - we NEVER receive the data message (actually we receive it in the ALFirebaseMessagingService only)
ANDROID: alert notification         + app NOT RUNNING   : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the notification message (with alcinoe.notification_clicked=1)                      - via TALFirebaseMessaging.StartupNotificationMessageHandler (TApplicationEventMessage.FinishedLaunching) > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.ReceiveStartupNotificationMessage
ANDROID: data + alert notification  + app NOT RUNNING   : ALERT    - BADGE    - WHEN the user will CLICK THE ALERT: we receive the data message and the notification message (with alcinoe.notification_clicked=1) - via TALFirebaseMessaging.StartupNotificationMessageHandler (TApplicationEventMessage.FinishedLaunching) > TALFirebaseMessaging.DeliverStartupNotificationMessages > TALFirebaseMessaging.ReceiveStartupNotificationMessage
*******************************************************************************)
unit Alcinoe.FMX.Firebase.Messaging;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-38700 is corrected and if yes update the previous documentation regarding "Setup (IOS)"'}
{$ENDIF}

uses
  system.Classes,
  system.Messaging,
  system.Generics.Collections,
  {$IF defined(android)}
  Androidapi.JNI.PlayServices.Tasks,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Alcinoe.AndroidApi.AndroidX,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UserNotifications,
  Alcinoe.iOSApi.FirebaseMessaging,
  {$ENDIF}
  Alcinoe.FMX.Firebase.Core, // [MANDATORY] Because we need it's initialization/finalization section
  Alcinoe.JSONDoc,
  Alcinoe.StringList;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFirebaseMessaging = class(TObject)

    {$REGION ' ANDROID'}
    {$IF defined(android)}

    //FNewMessageObserver
    private
      Type
        TNewMessageObserver = class(TJavaLocal, Jlifecycle_Observer)
        private
          FFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const AFirebaseMessaging: TALFirebaseMessaging);
          procedure onChanged(t: JObject); cdecl;
        end;
    private
      FNewMessageObserver: TNewMessageObserver;

    //FNewTokenObserver
    private
      Type
        TNewTokenObserver = class(TJavaLocal, Jlifecycle_Observer)
        private
          FFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const AFirebaseMessaging: TALFirebaseMessaging);
          procedure onChanged(t: JObject); cdecl;
        end;
    private
      FNewTokenObserver: TNewTokenObserver;

    //FGetTokenTaskCompleteListener
    private
      Type
        TGetTokenTaskCompleteListener = class(TJavaLocal, JOnCompleteListener)
        private
          FFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const AFirebaseMessaging: TALFirebaseMessaging);
          procedure onComplete(task: JTask); cdecl;
        end;
    private
      FGetTokenTaskCompleteListener: TGetTokenTaskCompleteListener;

    //FDeleteTokenTaskCompleteListener
    private
      Type
        TDeleteTokenTaskCompleteListener = class(TJavaLocal, JOnCompleteListener)
        private
          FFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const AFirebaseMessaging: TALFirebaseMessaging);
          procedure onComplete(task: JTask); cdecl;
        end;
    private
      FDeleteTokenTaskCompleteListener: TDeleteTokenTaskCompleteListener;

    //Startup Notification
    private
      procedure ReceiveStartupNotificationMessage(const Sender: TObject; const M: TMessage);

    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}

    //FUserNotificationCenterDelegate
    private
      type
        TUserNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
        strict private
          fFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const aFirebaseMessaging: TALFirebaseMessaging);
          [MethodName('userNotificationCenter:openSettingsForNotification:')]
          procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
          [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
          procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
          [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
          procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
        end;
    private
      fUserNotificationCenterDelegate: TUserNotificationCenterDelegate;

    //FFIRMessagingDelegate
    private
      type
        TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
        strict private
          FFirebaseMessaging: TALFirebaseMessaging;
        public
          constructor Create(const AFirebaseMessaging: TALFirebaseMessaging);
          procedure messaging(messaging: FIRMessaging; fcmToken: NSString); cdecl;
        end;
    private
      fFIRMessagingDelegate: TFIRMessagingDelegate;
      procedure FIRMessagingTokenWithCompletionHandler(token: NSString; error: NSError);
      procedure FIRMessagingDeleteTokenWithCompletionHandler(error: NSError);
    //Message handler
    private
      procedure applicationDidReceiveRemoteNotification(const Sender: TObject; const M: TMessage);
      procedure applicationDidFailToRegisterForRemoteNotificationsWithError(const Sender: TObject; const M: TMessage);

    {$ENDIF}
    {$ENDREGION}

  public
    Type
      TGetTokenEvent = procedure(const AToken: String; const AErrorMessage: String) of object;
      TDeleteTokenEvent = procedure(const AIsSuccessful: Boolean; const AErrorMessage: String) of object;
      TTokenRefreshEvent = procedure(const aToken: String) of object;
      TMessageReceivedEvent = procedure(const aPayload: TALStringListW) of object;
  private
    FDeliveredMessageIDs: TDictionary<String,boolean>;
    FGetTokenTaskIsRunning: Boolean;
    FDeleteTokenTaskIsRunning: Boolean;
    fOnGetToken: TGetTokenEvent;
    fOnDeleteToken: TDeleteTokenEvent;
    fOnTokenRefresh: TTokenRefreshEvent;
    fOnMessageReceived: TMessageReceivedEvent;
  protected
    class var StartupNotificationMessages: Tarray<String>;
    class var CanDeliverStartupNotificationMessages: Boolean;
    class procedure StartupNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    class procedure DeliverStartupNotificationMessages;
    procedure doTokenRefresh(const aToken: String);
    procedure doMessageReceived(const aPayload: TALStringListW); overload;
    procedure doMessageReceived(const aPayload: TALJSONNodeW); overload;
    procedure doMessageReceived(const aPayload: String); overload;
  public
    const MessageIdKeys: Array[0..2] of String = ('google.message_id','gcm.message_id','fcm_options.gcm.message_id');
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetToken;
    procedure DeleteToken;
    property OnGetToken: TGetTokenEvent read fOnGetToken write fOnGetToken;
    property OnDeleteToken: TDeleteTokenEvent read fOnDeleteToken write fOnDeleteToken;
    property OnTokenRefresh: TTokenRefreshEvent read fOnTokenRefresh write fOnTokenRefresh;
    property OnMessageReceived: TMessageReceivedEvent read fOnMessageReceived write fOnMessageReceived;
  end;

implementation

uses
  system.SysUtils,
  system.Rtti,
  fmx.platform,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.JNI.Os,
  FMX.platform.Android,
  Alcinoe.AndroidApi.Firebase,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  iOSapi.CocoaTypes,
  FMX.Helpers.iOS,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**************************************}
constructor TALFirebaseMessaging.Create;
begin

  {$IFDEF DEBUG}
  allog('TALFirebaseMessaging.Create', 'begin', TalLogType.verbose);
  {$ENDIF}

  inherited Create;
  FDeliveredMessageIDs := TDictionary<String,boolean>.create;
  FGetTokenTaskIsRunning := False;
  FDeleteTokenTaskIsRunning := False;
  fOnGetToken := nil;
  fOnDeleteToken := nil;
  fOnTokenRefresh := nil;
  fOnMessageReceived := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  FNewMessageObserver := TNewMessageObserver.Create(self);
  FNewTokenObserver := TNewTokenObserver.Create(self);
  FGetTokenTaskCompleteListener := nil;
  FDeleteTokenTaskCompleteListener := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, ReceiveStartupNotificationMessage);
  //as fOnMessageReceived and fOnTokenRefresh = nil right now then calling observeForever
  //will do nothing even if FNewMessageObserver/FNewTokenObserver contain new data
  TJALFirebaseMessagingService.JavaClass.newMessageDispatcher.observeForever(FNewMessageObserver);
  TJALFirebaseMessagingService.JavaClass.newTokenDispatcher.observeForever(FNewTokenObserver);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  fUserNotificationCenterDelegate := TUserNotificationCenterDelegate.Create(self);
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.setdelegate(fUserNotificationCenterDelegate);
  //--
  fFIRMessagingDelegate := TFIRMessagingDelegate.Create(self);
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).setDelegate(fFIRMessagingDelegate);
  //--
  TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, applicationDidReceiveRemoteNotification);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushFailToRegisterMessage, applicationdidFailToRegisterForRemoteNotificationsWithError);
  {$ENDIF}
  {$ENDREGION}

  //Deliver the TALFirebaseMessaging.StartupNotificationMessage
  //I do it in ForceQueue because i don't want this event to be executed during
  //the oncreate procedure, but in annother distinct synch loop. also because
  //right now fOnMessageReceived=nil and fOnTokenRefresh=nil. not a big deal
  //to use Forcequeue because DeliverStartupNotificationMessages is a CLASS procedure
  TALFirebaseMessaging.CanDeliverStartupNotificationMessages := True;
  TThread.ForceQueue(nil, DeliverStartupNotificationMessages);

  {$IFDEF DEBUG}
  allog('TALFirebaseMessaging.Create', 'end', TalLogType.verbose);
  {$ENDIF}

end;

{**************************************}
destructor TALFirebaseMessaging.Destroy;
begin

  {$IFDEF DEBUG}
  allog('TALFirebaseMessaging.Destroy', TalLogType.verbose);
  {$ENDIF}

  if FGetTokenTaskIsRunning then
    raise Exception.Create('You cannot Destroy a TALFirebaseMessaging Instance when a GetToken task is running');
  if FDeleteTokenTaskIsRunning then
    raise Exception.Create('You cannot Destroy a TALFirebaseMessaging Instance when a DeleteToken task is running');

  TALFirebaseMessaging.CanDeliverStartupNotificationMessages := False;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TJALFirebaseMessagingService.JavaClass.newMessageDispatcher.removeObserver(FNewMessageObserver);
  TJALFirebaseMessagingService.JavaClass.newTokenDispatcher.removeObserver(FNewTokenObserver);
  ALFreeAndNil(FNewMessageObserver);
  ALFreeAndNil(FNewTokenObserver);
  ALFreeAndNil(FGetTokenTaskCompleteListener);
  ALFreeAndNil(FDeleteTokenTaskCompleteListener);
  TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, ReceiveStartupNotificationMessage);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.setdelegate(nil);
  alfreeAndNil(fUserNotificationCenterDelegate);
  //---
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).setDelegate(nil);
  alFreeAndNil(fFIRMessagingDelegate);
  //---
  TMessageManager.DefaultManager.Unsubscribe(TPushRemoteNotificationMessage, applicationDidReceiveRemoteNotification);
  TMessageManager.DefaultManager.Unsubscribe(TPushFailToRegisterMessage, applicationDidFailToRegisterForRemoteNotificationsWithError);
  {$ENDIF}
  {$ENDREGION}

  ALFreeAndNil(FDeliveredMessageIDs);
  inherited Destroy;

end;

{******************************************************************}
//Returns the FCM registration token for this Firebase project. This
//creates a Firebase Installations ID, if one does not exist, and sends
//information about the application and the device where it's running to the
//Firebase backend. See deleteToken for information on deleting the token
//and the Firebase Installations ID.
procedure TALFirebaseMessaging.GetToken;
begin

  if FGetTokenTaskIsRunning then Exit;
  FGetTokenTaskIsRunning := true;
  if FDeleteTokenTaskIsRunning then exit;

  {$REGION ' android'}
  {$IF defined(android)}
  ALFreeAndNil(FGetTokenTaskCompleteListener);
  FGetTokenTaskCompleteListener := TGetTokenTaskCompleteListener.Create(self);
  TJFirebaseMessaging.javaclass.getInstance().GetToken.addOnCompleteListener(FGetTokenTaskCompleteListener);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).tokenWithCompletion(FIRMessagingTokenWithCompletionHandler);
  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************************}
//Deletes the FCM registration token for this Firebase project.
//Note that this does not delete the Firebase Installations ID
//that may have been created when generating the token. See
//FirebaseInstallations.delete() for deleting that.
procedure TALFirebaseMessaging.DeleteToken;
begin

  if FDeleteTokenTaskIsRunning then Exit;
  FDeleteTokenTaskIsRunning := true;
  if FGetTokenTaskIsRunning then exit;

  {$REGION ' android'}
  {$IF defined(android)}
  ALFreeAndNil(FDeleteTokenTaskCompleteListener);
  FDeleteTokenTaskCompleteListener := TDeleteTokenTaskCompleteListener.Create(self);
  TJFirebaseMessaging.javaclass.getInstance().DeleteToken.addOnCompleteListener(FDeleteTokenTaskCompleteListener);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).deleteTokenWithCompletion(FIRMessagingDeleteTokenWithCompletionHandler);
  {$ENDIF}
  {$ENDREGION}

end;

{******************************************************************}
procedure TALFirebaseMessaging.doTokenRefresh(const aToken: String);
begin
  if assigned(fOnTokenRefresh) then
    fOnTokenRefresh(aToken);
end;

{*******************************************************************************}
procedure TALFirebaseMessaging.doMessageReceived(const aPayload: TALStringListW);
begin
  if assigned(fOnMessageReceived) then begin
    //under ios, when the app is not running and the user click on an alert notification
    //then the app at the launch time will fire TPushStartupNotificationMessage and
    //also sometime (dependly when we create the TALFirebaseMessaging instance) also
    //userNotificationCenter:center::response:completionHandler. so this is because
    //we must deduplicate notification throught FDeliveredMessageIDs
    var LMessageId: String := '';
    for var LMessageIdKey in MessageIdKeys do begin
      LMessageId := aPayload.Values[LMessageIdKey];
      if LMessageId <> '' then break;
    end;
    {$IF defined(debug)}
    if LMessageId = '' then
      raise Exception.Create('google.message_id in notification message cannot be null');
    {$endif}
    if (LMessageId = '') then exit;
    if (FDeliveredMessageIDs.TryAdd(LMessageId,true)) then begin
      {$IF defined(debug)}
      allog('TALFirebaseMessaging.doMessageReceived', aPayload.Text, TalLogType.VERBOSE);
      {$endif}
      fOnMessageReceived(aPayload)
    end
    else begin
      {$IF defined(debug)}
      allog('TALFirebaseMessaging.doMessageReceived', 'Duplicate message skipped (google.message_id:'+LMessageId+')', TalLogType.VERBOSE);
      {$endif}
    end;
  end;
end;

{*****************************************************************************}
procedure TALFirebaseMessaging.doMessageReceived(const aPayload: TALJSONNodeW);
begin
  var LPayload := TALStringListW.Create;
  try
    ALJSONToTStringsW(aPayload, LPayload);
    doMessageReceived(LPayload);
  finally
    alFreeAndNil(LPayload);
  end;
end;
{***********************************************************************}
procedure TALFirebaseMessaging.doMessageReceived(const aPayload: String);
begin
  var LJsonDoc := TALJSONDocumentW.create;
  try
    LJsonDoc.LoadFromJSONString(aPayload);
    doMessageReceived(LJsonDoc);
  finally
    ALFreeAndNil(LJsonDoc);
  end;
end;

{**********************************}
//On android this handler is called:
//  * On any ApplicationEvent with TApplicationEventMessage message and we care only about TApplicationEvent.FinishedLaunching
//  * On Activity.onNewIntent with TMessageReceivedNotification message.
//On iOS this handler is called:
//  * On applicationDidFinishLaunchingWithOptions with TPushStartupNotificationMessage message
class procedure TALFirebaseMessaging.StartupNotificationMessageHandler(const Sender: TObject; const M: TMessage);
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var LBundle: JBundle := nil;
  if (M is TApplicationEventMessage) then begin
    case TApplicationEventMessage(M).Value.Event of
      TApplicationEvent.FinishedLaunching: begin
        var LIntent := MainActivity.getIntent;
        if LIntent <> nil then begin
          LBundle := LIntent.getExtras;
          if LBundle <> nil then begin
            var LContainsKey: Boolean := False;
            for var LMessageIdKey in MessageIdKeys do begin
              if (LBundle.containsKey(StringToJString(LMessageIdKey))) then begin
                LContainsKey := True;
                Break;
              end;
            end;
            if not LContainsKey then LBundle := nil;
          end;
        end;
      end;
    end;
  end
  else if (M is TMessageReceivedNotification) then begin
    var LIntent := TMessageReceivedNotification(M).Value;
    if LIntent <> nil then begin
      //don't ask me why but the FMXNativeActivity put all the infos
      //inside "fcm" when it's detect that the intent comme from
      //firebase cloud messaging
      LBundle := LIntent.getBundleExtra(StringToJString('fcm'));
      if LBundle = nil then LBundle := LIntent.getExtras;
    end;
  end;
  if LBundle = nil then exit;
  var LJsonDoc := TALJSONDocumentW.Create;
  try
    var LIterator := LBundle.keySet.iterator;
    while LIterator.hasNext do begin
      var LKeyObj := LIterator.next;
      if LKeyObj = nil then continue;
      var LKeyStr := LKeyObj.toString;
      //-----
      var LValueObj := LBundle.&get(LKeyStr);
      var LValueStr: String;
      if LValueObj = nil then LValueStr := ''
      else LValueStr := JStringToString(LValueObj.toString);
      //-----
      LJsonDoc.SetChildNodeValueText(JStringToString(LKeyStr), LValueStr);
    end;
    Setlength(StartupNotificationMessages, length(StartupNotificationMessages) + 1);
    StartupNotificationMessages[high(StartupNotificationMessages)] := LJsonDoc.JSON;
  finally
    ALFreeAndNil(LJsonDoc);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  if (M is TPushStartupNotificationMessage) then begin
    Setlength(StartupNotificationMessages, length(StartupNotificationMessages) + 1);
    StartupNotificationMessages[high(StartupNotificationMessages)] := TPushStartupNotificationMessage(M).Value.Notification;
  end
  else exit;
  {$ENDIF}
  {$ENDREGION}

  {$IFDEF DEBUG}
  allog(
    'TALFirebaseMessaging.StartupNotificationMessageHandler',
    'M.Classname: ' + M.ClassName + ' | ' +
    'Notification: ' + StartupNotificationMessages[high(StartupNotificationMessages)],
    TalLogType.verbose);
  {$ENDIF}

  DeliverStartupNotificationMessages;

end;

{**********************************************************************}
class procedure TALFirebaseMessaging.DeliverStartupNotificationMessages;
begin
  if not TALFirebaseMessaging.CanDeliverStartupNotificationMessages then exit;
  Var LtmpStartupNotificationMessages := StartupNotificationMessages;
  setlength(StartupNotificationMessages, 0);
  if length(LtmpStartupNotificationMessages) > 0 then begin
    for var LStartupNotificationMessage in LtmpStartupNotificationMessages do begin
      if LStartupNotificationMessage = '' then continue;
      var LJsonDoc := TALJSONDocumentW.create;
      try

        LJsonDoc.LoadFromJSONString(LStartupNotificationMessage);

        {$IFDEF DEBUG}
        allog('TALFirebaseMessaging.DeliverStartupNotificationMessages', LJsonDoc.JSON, TalLogType.verbose);
        {$ENDIF}

        {$REGION ' ANDROID'}
        {$IF defined(ANDROID)}
        LJsonDoc.SetChildNodeValueText('alcinoe.notification_clicked', '1');
        var LPushStartupNotificationMessage := TPushStartupNotificationMessage.Create(TPushNotificationData.Create(LJsonDoc.JSON));
        TMessageManager.DefaultManager.SendMessage(nil, LPushStartupNotificationMessage);
        {$ENDIF}
        {$ENDREGION}

        {$REGION ' IOS'}
        {$IF defined(IOS)}
        //under ios data message can (if you are lucky) wake up the app and fire this method
        //so we must detect if it's an alert or a pure data message. I use content-available that I
        //think will be only present in data message
        if LJsonDoc.GetChildNodeValueFloat(['aps', 'content-available'], 0) = 0 then
          LJsonDoc.SetChildNodeValueText('alcinoe.notification_clicked', '1');
        var LPushRemoteNotificationMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJsonDoc.JSON));
        TMessageManager.DefaultManager.SendMessage(nil, LPushRemoteNotificationMessage);
        {$ENDIF}
        {$ENDREGION}

      finally
        ALFreeAndNil(LJsonDoc);
      end;
    end;
  end;
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{**********************************************************************************************************}
constructor TALFirebaseMessaging.TNewMessageObserver.Create(const AFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

{***********************************************************************}
procedure TALFirebaseMessaging.TNewMessageObserver.onChanged(t: JObject);
begin
  var LMessage := JStringToString(TJString.Wrap(t));
  {$IF defined(debug)}
  allog('TALFirebaseMessaging.TNewMessageObserver.onChanged', 'Message: ' + LMessage, TalLogType.VERBOSE);
  {$ENDIF}
  FFirebaseMessaging.doMessageReceived(LMessage);
end;

{********************************************************************************************************}
constructor TALFirebaseMessaging.TNewTokenObserver.Create(const AFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

{*********************************************************************}
procedure TALFirebaseMessaging.TNewTokenObserver.onChanged(t: JObject);
begin
  var LNewToken := JStringToString(TJString.Wrap(t));
  {$IF defined(debug)}
  allog('TALFirebaseMessaging.TNewTokenObserver.onChanged', 'NewToken: ' + LNewToken, TalLogType.VERBOSE);
  {$ENDIF}
  FFirebaseMessaging.doTokenRefresh(LNewToken);
end;

{********************************************************************************************************************}
constructor TALFirebaseMessaging.TGetTokenTaskCompleteListener.Create(const AFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

{***********************************************************************************}
procedure TALFirebaseMessaging.TGetTokenTaskCompleteListener.onComplete(task: JTask);
begin
  var LIsSuccessful := task.isSuccessful;
  var LToken: String;
  var LErrorMessage: String;
  if LIsSuccessful then begin
    LToken := JStringToString(TJString.Wrap(task.getResult));
    LErrorMessage := '';
  end
  else begin
    LToken := '';
    LErrorMessage := JStringToString(task.getException.getMessage);
  end;
  {$IF defined(debug)}
  var LLogType: TalLogType;
  if LIsSuccessful then LLogType := TalLogType.Verbose
  else LLogType := TalLogType.ERROR;
  allog(
    'TALFirebaseMessaging.TGetTokenTaskCompleteListener.onComplete',
    'Token: ' + LToken + ' | ' +
    'Error: ' + LErrorMessage,
    LLogType);
  {$ENDIF}
  FFirebaseMessaging.FGetTokenTaskIsRunning := False;
  var LDoDeleteToken := FFirebaseMessaging.FDeleteTokenTaskIsRunning;
  FFirebaseMessaging.FDeleteTokenTaskIsRunning := False;
  if assigned(FFirebaseMessaging.fOnGetToken) then
    FFirebaseMessaging.fOnGetToken(LToken, LErrorMessage);
  if LIsSuccessful then FFirebaseMessaging.doTokenRefresh(LToken);
  if LDoDeleteToken then FFirebaseMessaging.DeleteToken;
end;

{***********************************************************************************************************************}
constructor TALFirebaseMessaging.TDeleteTokenTaskCompleteListener.Create(const AFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

{**************************************************************************************}
procedure TALFirebaseMessaging.TDeleteTokenTaskCompleteListener.onComplete(task: JTask);
begin
  var LIsSuccessful := task.isSuccessful;
  var LErrorMessage: String;
  if LIsSuccessful then LErrorMessage := ''
  else LErrorMessage := JStringToString(task.getException.getMessage);
  {$IF defined(debug)}
  var LLogType: TalLogType;
  if LIsSuccessful then LLogType := TalLogType.Verbose
  else LLogType := TalLogType.ERROR;
  allog(
    'TALFirebaseMessaging.TDeleteTokenTaskCompleteListener.onComplete',
    'IsSuccessful: ' + ALBoolToStrW(LIsSuccessful) + ' | ' +
    'Error: ' + LErrorMessage,
    LLogType);
  {$ENDIF}
  FFirebaseMessaging.FDeleteTokenTaskIsRunning := False;
  var LDoGetToken := FFirebaseMessaging.FGetTokenTaskIsRunning;
  FFirebaseMessaging.FGetTokenTaskIsRunning := False;
  if assigned(FFirebaseMessaging.fOnDeleteToken) then
    FFirebaseMessaging.fOnDeleteToken(LIsSuccessful, LErrorMessage);
  if LIsSuccessful then FFirebaseMessaging.doTokenRefresh('');
  if LDoGetToken then FFirebaseMessaging.GetToken;
end;

{*********************************************************************************************************}
procedure TALFirebaseMessaging.ReceiveStartupNotificationMessage(const Sender: TObject; const M: TMessage);
begin
  if M is TPushStartupNotificationMessage then begin
    var LMessage := TPushStartupNotificationMessage(M).Value.Notification;
    {$IFDEF DEBUG}
    allog(
      'TALFirebaseMessaging.ReceiveStartupNotificationMessage',
      'Message: ' + LMessage,
      TalLogType.verbose);
    {$ENDIF}
    doMessageReceived(LMessage);
  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{**********************************************************************************************************************}
constructor TALFirebaseMessaging.TUserNotificationCenterDelegate.Create(const aFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  fFirebaseMessaging := aFirebaseMessaging;
end;

{********************************************************************}
function _NSDictionaryToJSON(const ADictionary: NSDictionary): string;
begin
  var LError: NSError;
  var LData := TNSJSONSerialization.OCClass.dataWithJSONObject(NSObjectToID(ADictionary), 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then begin
    var LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else Result := string.Empty;
end;

{***************************************************************}
// Asks the delegate to display the in-app notification settings.
procedure TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification);
begin
  {$IFDEF DEBUG}
  allog('TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:notification', TalLogType.verbose);
  {$ENDIF}
end;

{*********************************************************************************************************}
// Asks the delegate how to handle a notification that arrived while the app was running in the foreground.
procedure TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer);
var LImp: procedure(aOptions: UNNotificationPresentationOptions); cdecl;
begin
  var LJsonDoc := TALJSONDocumentW.create;
  try

    var LJsonStr := _NSDictionaryToJSON(notification.request.content.userInfo);
    if LJsonStr <> '' then LJsonDoc.LoadFromJSONString(LJsonStr);

    {$IFDEF DEBUG}
    allog('TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:notification:completionHandler', LJsonStr, TalLogType.verbose);
    {$ENDIF}

    if ALStrToBool(LJsonDoc.GetChildNodeValuetext('alcinoe.present_notification', '0')) then begin

      @LImp := imp_implementationWithBlock(completionHandler);
      if TOSVersion.Check(14) then
        LImp(
          UNNotificationPresentationOptionBadge or
          UNNotificationPresentationOptionSound or
          UNNotificationPresentationOptionList or
          UNNotificationPresentationOptionBanner)
      else
        LImp(
          UNNotificationPresentationOptionBadge or
          UNNotificationPresentationOptionSound or
          UNNotificationPresentationOptionAlert);
      imp_removeBlock(@LImp);

    end
    else begin

      var LMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJsonStr));
      TMessageManager.DefaultManager.SendMessage(nil, LMessage);

      @LImp := imp_implementationWithBlock(completionHandler);
      LImp(UNNotificationPresentationOptionNone);
      imp_removeBlock(@LImp);

    end;

  finally
    ALFreeAndNil(LJsonDoc);
  end;
end;

{******************************************************************************}
// Asks the delegate to process the user's response to a delivered notification.
procedure TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); cdecl;
var LImp: procedure(); cdecl;
begin
  var LJsonDoc := TALJSONDocumentW.create;
  try
    var LJsonStr := _NSDictionaryToJSON(response.notification.request.content.userInfo);
    if LJsonStr <> '' then LJsonDoc.LoadFromJSONString(LJsonStr);

    {$IFDEF DEBUG}
    allog('TALFirebaseMessaging.TUserNotificationCenterDelegate.userNotificationCenter:center:response:completionHandler', 'Message: ' + LJsonDoc.JSON, TalLogType.verbose);
    {$ENDIF}

    LJsonDoc.SetChildNodeValueText('alcinoe.notification_clicked', '1');
    var LMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(LJsonDoc.JSON));
    TMessageManager.DefaultManager.SendMessage(nil, LMessage);

    @LImp := imp_implementationWithBlock(completionHandler);
    LImp();
    imp_removeBlock(@LImp);

  finally
    ALFreeAndNil(LJsonDoc);
  end;
end;

{*****************************************************************************************************}
procedure TALFirebaseMessaging.FIRMessagingTokenWithCompletionHandler(token: NSString; error: NSError);
begin
  var LIsSuccessful := error = nil;
  var LToken := NSStrToStr(Token);
  var LErrorMessage: String;
  if LIsSuccessful then LErrorMessage := ''
  else LErrorMessage := NSStrToStr(error.localizedDescription);
  {$IF defined(debug)}
  var LLogType: TalLogType;
  if LIsSuccessful then LLogType := TalLogType.Verbose
  else LLogType := TalLogType.ERROR;
  allog(
    'TALFirebaseMessaging.FIRMessagingTokenWithCompletionHandler',
    'Token: ' + LToken + ' | ' +
    'Error: ' + LErrorMessage,
    LLogType);
  {$ENDIF}
  FGetTokenTaskIsRunning := False;
  var LDoDeleteToken := FDeleteTokenTaskIsRunning;
  FDeleteTokenTaskIsRunning := False;
  if assigned(fOnGetToken) then
    fOnGetToken(LToken, LErrorMessage);
  if LIsSuccessful then doTokenRefresh(LToken);
  if LDoDeleteToken then DeleteToken;
end;

{******************************************************************************************}
procedure TALFirebaseMessaging.FIRMessagingDeleteTokenWithCompletionHandler(error: NSError);
begin
  var LIsSuccessful := error = nil;
  var LErrorMessage: String;
  if LIsSuccessful then LErrorMessage := ''
  else LErrorMessage := NSStrToStr(error.localizedDescription);
  {$IF defined(debug)}
  var LLogType: TalLogType;
  if LIsSuccessful then LLogType := TalLogType.Verbose
  else LLogType := TalLogType.ERROR;
  allog(
    'TALFirebaseMessaging.FIRMessagingDeleteTokenWithCompletionHandler',
    'IsSuccessful: ' + ALBoolToStrW(LIsSuccessful) + ' | ' +
    'Error: ' + LErrorMessage,
    LLogType);
  {$ENDIF}
  FDeleteTokenTaskIsRunning := False;
  var LDoGetToken := FGetTokenTaskIsRunning;
  FGetTokenTaskIsRunning := False;
  if assigned(fOnDeleteToken) then
    fOnDeleteToken(LIsSuccessful, LErrorMessage);
  if LIsSuccessful then doTokenRefresh('');
  if LDoGetToken then GetToken;
end;

{************************************************************************************************************}
constructor TALFirebaseMessaging.TFIRMessagingDelegate.Create(const AFirebaseMessaging: TALFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

{****************************************************************************************************************************}
//This method will be called once a token is available, or has been refreshed. Typically it will be called once per app start,
//but may be called more often, if token is invalidated or updated. In this method, you should perform operations such as:
// * Uploading the FCM token to your application server, so targeted notifications can be sent.
// * Subscribing to any topics.
procedure TALFirebaseMessaging.TFIRMessagingDelegate.messaging(messaging: FIRMessaging; fcmToken: NSString);
begin
  {$IFDEF DEBUG}
  allog('TALFirebaseMessaging.TFIRMessagingDelegate.messaging', 'Token: ' + NSStrToStr(fcmToken), TalLogType.VERBOSE);
  {$ENDIF}
  FFirebaseMessaging.doTokenRefresh(NSStrToStr(fcmToken));
end;

{***************************************************************************************************************}
procedure TALFirebaseMessaging.applicationDidReceiveRemoteNotification(const Sender: TObject; const M: TMessage);
begin
  if (M is TPushRemoteNotificationMessage) then begin
    var LMessage := TPushRemoteNotificationMessage(M).Value.Notification;
    {$IFDEF DEBUG}
    allog('TALFirebaseMessaging.applicationDidReceiveRemoteNotification', 'Message: ' + LMessage, TalLogType.verbose);
    {$ENDIF}
    doMessageReceived(LMessage);
  end;
end;

{***********************************************************************************************************************************}
procedure TALFirebaseMessaging.applicationDidFailToRegisterForRemoteNotificationsWithError(const Sender: TObject; const M: TMessage);
begin

  // After you call the registerForRemoteNotifications method of the UIApplication object, the app calls
  // this method when there is an error in the registration process.
  //
  // application:didFailToRegisterForRemoteNotificationsWithError: gets called when the app is signed
  // with incorrect provisioning profile.
  //
  // NOTE: don't know what else to do here except loging the error

  {$IFDEF DEBUG}
  if (M is TPushFailToRegisterMessage) then
    allog(
      'TALFirebaseMessaging.applicationdidFailToRegisterForRemoteNotificationsWithError',
      'Unable to register for remote notifications | ' +
      TPushFailToRegisterMessage(M).Value.ErrorMessage,
      TalLogType.error)
  else
    allog(
      'TALFirebaseMessaging.applicationdidFailToRegisterForRemoteNotificationsWithError',
      'Unable to register for remote notifications',
      TalLogType.VERBOSE);
  {$ENDIF}

end;

{$ENDIF}
{$ENDREGION}

initialization

  setlength(TALFirebaseMessaging.StartupNotificationMessages, 0);
  TALFirebaseMessaging.CanDeliverStartupNotificationMessages := False;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, TALFirebaseMessaging.StartupNotificationMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, TALFirebaseMessaging.StartupNotificationMessageHandler);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, TALFirebaseMessaging.StartupNotificationMessageHandler);
  {$ENDIF}
  {$ENDREGION}

finalization

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, TALFirebaseMessaging.StartupNotificationMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, TALFirebaseMessaging.StartupNotificationMessageHandler);
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, TALFirebaseMessaging.StartupNotificationMessageHandler);
  {$ENDIF}
  {$ENDREGION}

end.
