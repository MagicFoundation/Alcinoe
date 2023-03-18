(*******************************************************************************
When people log into your app with Facebook, they can grant permissions to your
app so you can retrieve information or perform actions on Facebook on their
behalf.

Setup (ANDROID)
---------------

https://developers.facebook.com/docs/android/getting-started

1) https://developers.facebook.com/docs/android/getting-started
   https://developers.facebook.com/docs/facebook-login/android/
   Open your /app/res/values/strings.xml file.
   Add string elements with the names facebook_app_id, fb_login_protocol_scheme
   and facebook_client_token, and set the values to your App ID and Client
   Token. For example, if your app ID is 1234 and your client token is 56789
   your code looks like the following:

    <string name="facebook_app_id">1234</string>
    <string name="fb_login_protocol_scheme">fb1234</string>
    <string name="facebook_client_token">56789</string>

   You can merge this strings.xml with the help of AndroidMerger. You can see
   an exemple in <Alcinoe>\Demos\ALFacebookLogin\_source\android\MergeLibraries.bat

2) https://developers.facebook.com/docs/android/getting-started
   Open the /app/manifest/AndroidManifest.xml file.
   Add meta-data elements to the application element for your app ID and client
   token:

    <application>
        ...
        <meta-data android:name="com.facebook.sdk.ApplicationId" android:value="@string/facebook_app_id"/>
        <meta-data android:name="com.facebook.sdk.ClientToken" android:value="@string/facebook_client_token"/>
        ...
    </application>

3) https://developers.facebook.com/docs/app-events/getting-started-app-events-android
   To disable automatically logged events add the following to your
   AndroidManifest.xml file:

    <application>
      ...
      <meta-data android:name="com.facebook.sdk.AutoLogAppEventsEnabled"
                 android:value="false"/>
      ...
    </application>

4) https://developers.facebook.com/docs/app-events/getting-started-app-events-android
   To disable collection of advertiser-id, add the following to your
   AndroidManifest.xml file:

    <application>
      ...
      <meta-data android:name="com.facebook.sdk.AdvertiserIDCollectionEnabled"
               android:value="false"/>
      ...
    </application>

5) To disable automatic SDK initialization (the alcinoe framework will do it for
   you when you will use any facebook functions below), you can replace the
   following in your AndroidManifest.xml file:

    <provider android:name="com.facebook.internal.FacebookInitProvider"
              android:authorities="%package%.FacebookInitProvider"
              android:exported="false"/>

    by

    <!-- !! <provider android:name="com.facebook.internal.FacebookInitProvider"
              android:authorities="%package%.FacebookInitProvider"
              android:exported="false"/> !! -->


Setup (IOS)
-----------

https://developers.facebook.com/docs/ios/getting-started

1) follow the step bescribed described for FIREBASE in Alcinoe.FMX.FirebaseMessaging.pas
     1) : LD linker add -ObjC
     3) : Add the libswift frameworks for ios64 and Ios64 simulator
     4) : LD linker add -rpath /usr/lib/swift

2) in LD linker add add -w to remove the warning
   "was built for newer iOS version (12.0) than being linked (11.0)"

2) In project > option > Building > Delphi Compiler > FRAMEWORK search path
   you need to add the following path:
   <Alcinoe>\Libraries\ios\facebook\FBSDKLoginKit.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\facebook\FBSDKShareKit.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\facebook\FBSDKCoreKit_Basics.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\facebook\FBSDKCoreKit.xcframework\ios-arm64
   <Alcinoe>\Libraries\ios\facebook\FBAEMKit.xcframework\ios-arm64

3) https://developers.facebook.com/docs/ios/getting-started
   Configure the Info.plist file with an XML snippet that contains data about
   your app.

    <key>CFBundleURLTypes</key>
    <array>
      <dict>
      <key>CFBundleURLSchemes</key>
      <array>
        <string>fbAPP-ID</string>
      </array>
      </dict>
    </array>
    <key>FacebookAppID</key>
    <string>APP-ID</string>
    <key>FacebookClientToken</key>
    <string>CLIENT-TOKEN</string>
    <key>FacebookDisplayName</key>
    <string>APP-NAME</string>

4) https://developers.facebook.com/docs/ios/getting-started
   To use any of the Facebook dialogs (e.g., Login, Share, App Invites, etc.)
   that can perform an app switch to Facebook apps, your application's
   Info.plist also needs to include the following:

    <key>LSApplicationQueriesSchemes</key>
    <array>
      <string>fbapi</string>
      <string>fb-messenger-share-api</string>
    </array>

5) https://developers.facebook.com/docs/app-events/getting-started-app-events-ios/
   Disable Automatically Logged Events
   To disable automatic event logging, open the application's Info.plist as code
   in Xcode and add the following XML to the property dictionary:

    <key>FacebookAutoLogAppEventsEnabled</key>
    <false/>

6) https://developers.facebook.com/docs/app-events/getting-started-app-events-ios/
   Disable Collection of Advertiser IDs
   To disable collection of advertiser-id, open the application's .plist as
   code in Xcode and add the following XML to the property dictionary:

     <key>FacebookAdvertiserIDCollectionEnabled</key>
     <false/>

7) To disable automatic SDK initialization (the alcinoe framework will do it for
   you when you will use any facebook functions below), you can set
   ALInitFacebookSDKAtStartup := false
   in the begin .. end section of your dpr
*******************************************************************************)
unit Alcinoe.FMX.FacebookCore;

interface

{$I Alcinoe.inc}

{***********************}
procedure ALInitFacebook;

{*}
var
  ALInitFacebookSDKAtStartup: Boolean = true;

implementation

uses
  system.Messaging,
  {$IF defined(android)}
  Alcinoe.AndroidApi.Facebook,
  Androidapi.Helpers,
  {$ELSEIF defined(IOS)}
  iOSapi.Foundation,
  Macapi.Helpers,
  iOSapi.Helpers,
  FMX.Platform,
  FMX.Platform.iOS,
  Alcinoe.iOSApi.FacebookCoreKit,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.Common;

{*}
var
  _ALFacebookInitialised: Boolean;

{***********************}
procedure ALInitFacebook;
begin
  if not _ALFacebookInitialised then begin

    {$REGION 'ANDROID'}
    {$IF defined(android)}

    {$IFDEF DEBUG}
    allog('ALInitFacebook', 'sdkInitialize | isInitialized: '+ALBoolToStrW(TJFacebookSdk.JavaClass.isInitialized), TalLogType.VERBOSE);
    {$ENDIF}

    TJFacebookSdk.JavaClass.sdkInitialize(TAndroidHelper.Context);

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'IOS'}
    {$IF defined(ios)}

    {$IFDEF DEBUG}
    allog('ALInitFacebook', 'initializeSDK', TalLogType.VERBOSE);
    {$ENDIF}

    //https://github.com/facebook/facebook-ios-sdk/issues/1731
    //those monkey facebook developpers made 2 initialization procedures
    //  *TFBSDKApplicationDelegate.OCClass.sharedInstance.initializeSDK
    //  *TFBSDKApplicationDelegate.OCClass.sharedInstance.applicationDidFinishLaunchingWithOptions
    //but their is a notable difference between thoses 2 procedure. the first
    //(initializeSDK) will not persist the token on restart (token will be empty
    //and you must call again the login procedure to retrieve it again)
    //where the 2nd (applicationDidFinishLaunchingWithOptions) will persist the
    //token between app restart
    TFBSDKApplicationDelegate.OCClass.sharedInstance.initializeSDK;

    {$ENDIF}
    {$ENDREGION}

    _ALFacebookInitialised := True;

  end;
end;

{$REGION ' IOS'}
{$IF defined(IOS)}

{*******************************************************************************************}
procedure ALFmxFacebookCoreApplicationEventHandler(const Sender: TObject; const M: TMessage);
begin
  if M is TApplicationEventMessage then begin
    var LValue := (M as TApplicationEventMessage).value;
    if LValue.Event = TApplicationEvent.OpenURL then begin
      var Lcontext := TiOSOpenApplicationContext(LValue.Context);
      {$IFDEF DEBUG}
      ALLog(
        'ALFmxFacebookCoreApplicationEventHandler',
        'Event: OpenURL | '+
        'ALFacebookInitialised: '+ALBoolToStrW(_ALFacebookInitialised)+' | '+
        'Url: ' + Lcontext.URL,
        TalLogType.VERBOSE);
      {$ENDIF}
      if not _ALFacebookInitialised then exit;
      {$IFNDEF ALCompilerVersionSupported}
        //Ios doc say iOS 13 moved opening URL functionality to the SceneDelegate. If you are
        //using iOS 13, add the following method to your SceneDelegate so that operations like
        //logging in or sharing function as intended (https://developers.facebook.com/docs/ios/getting-started)
        //but actually their is no implementation in delphi for the SceneDelegate and also it's
        //seam that the event is still called in ios 13+
        //(https://stackoverflow.com/questions/75062913/applicationopenurloptions-vs-sceneopenurlcontexts)
        //so for now I simply skip it and I add this warn to verify if one day
        //SceneDelegate will be implemented in delphi
        {$MESSAGE WARN 'Check if SceneDelegate is implemented in Delphi source code'}
      {$ENDIF}
      TFBSDKApplicationDelegate.OCClass.sharedInstance.applicationOpenURLOptions(
        TiOSHelper.SharedApplication, // application: UIApplication
        StrToNSUrl(Lcontext.Url),  // openURL: NSURL;
        TNSDictionary.Wrap(Lcontext.Context)); // options: NSDictionary
    end
    else if LValue.Event = TApplicationEvent.FinishedLaunching then begin
      if not ALInitFacebookSDKAtStartup then exit;
      if _ALFacebookInitialised then exit;
      {$IFDEF DEBUG}
      ALLog(
        'ALFmxFacebookCoreApplicationEventHandler',
        'Event: FinishedLaunching',
        TalLogType.VERBOSE);
      {$ENDIF}
      {$IFNDEF ALCompilerVersionSupported}
        {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-40351 is implemented like expected (With TiOSOpenApplicationContext)'}
      {$ENDIF}
      var LdidFinishLaunchingWithOptions: NSDictionary;
      if LValue.Context <> nil then LdidFinishLaunchingWithOptions := TNSDictionary.Wrap(TiOSOpenApplicationContext(LValue.Context).Context)
      else LdidFinishLaunchingWithOptions := nil;
      //see note in ALInitFacebook regarding initializeSDK in ALInitFacebook
      TFBSDKApplicationDelegate.OCClass.sharedInstance.applicationDidFinishLaunchingWithOptions(
        TiOSHelper.SharedApplication, // application: UIApplication
        LdidFinishLaunchingWithOptions); // options: NSDictionary
      _ALFacebookInitialised := True;
    end;
  end;
end;

{$ENDIF}
{$ENDREGION}

initialization

  _ALFacebookInitialised := False;

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ALFmxFacebookCoreApplicationEventHandler);
  {$ENDIF}
  {$ENDREGION}

finalization

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ALFmxFacebookCoreApplicationEventHandler);
  {$ENDIF}
  {$ENDREGION}

end.
