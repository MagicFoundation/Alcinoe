//
// This component is designed as a versatile container, serving as a central
// hub for managing push notifications in a mobile application. Initially
// integrated with TALFirebaseMessaging, it empowers developers to handle
// Firebase Cloud Messaging seamlessly.
//
// However, the true strength of this container lies in its future-proof
// design. It has been architected with the foresight to accommodate other push
// notification services like Huawei Push Kit and Apple APNS. By leveraging
// this container, developers can efficiently manage push notifications across
// various platforms without the need to modify their core application logic
// each time a new service is integrated.
//

unit Alcinoe.FMX.NotificationService;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Messaging,
  {$IF defined(IOS)}
  iOSapi.Foundation,
  {$ENDIF}
  Alcinoe.StringList,
  Alcinoe.FMX.Firebase.Messaging;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNotificationService = class(TObject)

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    private
      const
        RequestPermissionsCode: Integer = 32770;
    private
      procedure PermissionsRequestResultHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    private
      procedure UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
    {$ENDIF}
    {$ENDREGION}

  public
    type
      TTokenRefreshEvent = procedure(const aToken: String) of object;
      TNotificationReceivedEvent = procedure(const aPayload: TALStringListW) of object;
  private
    FFirebaseMessaging: TALFirebaseMessaging;
    fOnAuthorizationRefused: TNotifyEvent;
    fOnAuthorizationGranted: TNotifyEvent;
    fOnTokenRefresh: TTokenRefreshEvent;
    fOnNotificationReceived: TNotificationReceivedEvent;
    procedure onFCMTokenRefresh(const aToken: String);
    procedure onFCMMessageReceived(const aPayload: TALStringListW);
  public
    type
      //https://developer.android.com/reference/android/app/Notification#VISIBILITY_PRIVATE
      //VISIBILITY_PRIVATE=Show this notification on all lockscreens, but conceal sensitive or private information on secure lockscreens
      //VISIBILITY_PUBLIC=Show this notification in its entirety on all lockscreens
      //VISIBILITY_SECRET=Do not reveal any part of this notification on a secure lockscreen
      TNotificationChannelLockscreenVisibility = (Public, Private, Secret);
      //https://developer.android.com/reference/android/app/NotificationManager#IMPORTANCE_DEFAULT
      //IMPORTANCE_DEFAULT=shows everywhere, makes noise, but does not visually intrude
      //IMPORTANCE_HIGH=shows everywhere, makes noise and peeks. May use full screen intents
      //IMPORTANCE_LOW=Shows in the shade, and potentially in the status bar but is not audibly intrusive
      //IMPORTANCE_MIN=only shows in the shade, below the fold
      //IMPORTANCE_NONE=does not show in the shade
      TNotificationChannelImportance = (None, Default, Min, Low, High);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RequestNotificationPermission;
    class procedure CreateNotificationChannel(
                      const AID: String;
                      const AName: String;
                      const AImportance: TNotificationChannelImportance = TNotificationChannelImportance.Default; // Equivalent to message.android.notification.notification_priority of the http V1 message
                      const ALockscreenVisibility: TNotificationChannelLockscreenVisibility = TNotificationChannelLockscreenVisibility.Private; // Equivalent to message.android.notification.visibility of the http V1 message
                      const AEnableVibration: boolean = true;
                      const AEnableLights: Boolean = true;
                      const ALightColor: integer = 0;
                      const ABypassDnd: Boolean = false;
                      const AShowBadge: Boolean = true;
                      const ASoundUri: String = 'content://settings/system/notification_sound'); // Equivalent to message.android.notification.sound of the http V1 message
    class procedure setBadgeCount(const aNewValue: integer);
    procedure GetToken;
    procedure removeAllDeliveredNotifications;
    property OnAuthorizationRefused: TNotifyEvent read fOnAuthorizationRefused write fOnAuthorizationRefused;
    property OnAuthorizationGranted: TNotifyEvent read fOnAuthorizationGranted write fOnAuthorizationGranted;
    property OnTokenRefresh: TTokenRefreshEvent read fOnTokenRefresh write fOnTokenRefresh;
    property OnNotificationReceived: TNotificationReceivedEvent read fOnNotificationReceived write fOnNotificationReceived;
  end;

implementation

uses
  System.SysUtils,
  {$IF defined(android)}
  Androidapi.Helpers,
  AndroidApi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNIBridge,
  Androidapi.JNI.Provider,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  FMX.Platform.Android,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.UserNotifications,
  FMX.Helpers.iOS,
  {$ENDIF}
  Alcinoe.stringUtils,
  Alcinoe.Common;

{****************************************}
constructor TALNotificationService.Create;
begin
  inherited Create;

  FFirebaseMessaging := TALFirebaseMessaging.create;
  FFirebaseMessaging.OnTokenRefresh := onFCMTokenRefresh;
  FFirebaseMessaging.OnMessageReceived := onFCMMessageReceived;
  fOnAuthorizationRefused := nil;
  fOnAuthorizationGranted := nil;
  fOnTokenRefresh := nil;
  fOnNotificationReceived := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TMessageManager.DefaultManager.SubscribeToMessage(TPermissionsRequestResultMessage, PermissionsRequestResultHandler);
  {$ENDIF}
  {$ENDREGION}

end;

{****************************************}
destructor TALNotificationService.Destroy;
begin

  AlFreeAndNil(FFirebaseMessaging);

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  TMessageManager.DefaultManager.Unsubscribe(TPermissionsRequestResultMessage, PermissionsRequestResultHandler);
  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{*************************************************************}
procedure TALNotificationService.RequestNotificationPermission;
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  // This is only necessary for API level >= 33 (TIRAMISU)
  if (TOSVersion.Check(13, 0)) and
     (MainActivity.checkSelfPermission(StringToJString('android.permission.POST_NOTIFICATIONS')) <> TJPackageManager.JavaClass.PERMISSION_GRANTED) then begin
    var LPermissions := TJavaObjectArray<JString>.create(1);
    try
      LPermissions.Items[0] := StringToJString('android.permission.POST_NOTIFICATIONS');
      MainActivity.requestPermissions(LPermissions, RequestPermissionsCode);
    finally
      ALFreeAndNil(LPermissions);
    end;
  end
  else begin
    if assigned(fOnAuthorizationGranted) then
      fOnAuthorizationGranted(self);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  var LOptions := UNAuthorizationOptionSound or
                  UNAuthorizationOptionAlert or
                  UNAuthorizationOptionBadge;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.requestAuthorizationWithOptions(LOptions{options}, UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler{completionHandler});
  SharedApplication.registerForRemoteNotifications;
  {$ENDIF}
  {$ENDREGION}

end;

{***************************************************************}
class procedure TALNotificationService.CreateNotificationChannel(
                  const AID: String;
                  const AName: String;
                  const AImportance: TNotificationChannelImportance = TNotificationChannelImportance.Default;
                  const ALockscreenVisibility: TNotificationChannelLockscreenVisibility = TNotificationChannelLockscreenVisibility.Private;
                  const AEnableVibration: boolean = true;
                  const AEnableLights: Boolean = true;
                  const ALightColor: integer = 0;
                  const ABypassDnd: Boolean = false;
                  const AShowBadge: Boolean = true;
                  const ASoundUri: String = 'content://settings/system/notification_sound');
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  var LImportanceNative: integer;
  case AImportance of
    TNotificationChannelImportance.None: LImportanceNative := TJNotificationManager.JavaClass.IMPORTANCE_NONE;
    TNotificationChannelImportance.Default: LImportanceNative := TJNotificationManager.JavaClass.IMPORTANCE_DEFAULT;
    TNotificationChannelImportance.Min: LImportanceNative := TJNotificationManager.JavaClass.IMPORTANCE_MIN;
    TNotificationChannelImportance.Low: LImportanceNative := TJNotificationManager.JavaClass.IMPORTANCE_LOW;
    TNotificationChannelImportance.High: LImportanceNative := TJNotificationManager.JavaClass.IMPORTANCE_HIGH;
    else raise Exception.Create('Error D55BED63-B524-4422-B075-71F40CDF5327');
  end;
  var LNotificationChannel := TJNotificationChannel.JavaClass.init(
                                StringToJString(aID),
                                StrToJCharSequence(aName),
                                LImportanceNative);
  //--
  var LLockscreenVisibilityNative: integer;
  case ALockscreenVisibility of
    TNotificationChannelLockscreenVisibility.Public: LLockscreenVisibilityNative := TJNotification.JavaClass.VISIBILITY_PUBLIC;
    TNotificationChannelLockscreenVisibility.Private: LLockscreenVisibilityNative := TJNotification.JavaClass.VISIBILITY_PRIVATE;
    TNotificationChannelLockscreenVisibility.Secret: LLockscreenVisibilityNative := TJNotification.JavaClass.VISIBILITY_SECRET;
    else raise Exception.Create('Error 276AB7CE-3809-46A5-B4B7-65A9B0630A3C');
  end;
  LNotificationChannel.setLockscreenVisibility(LLockscreenVisibilityNative);
  //--
  LNotificationChannel.enableVibration(aEnableVibration);
  if aEnableVibration then begin
    var LPattern := TJavaArray<Int64>.create(2);
    try
      LPattern[0] := 0;
      LPattern[1] := 1200;
      LNotificationChannel.setVibrationPattern(LPattern);
    finally
      AlFreeAndNil(LPattern);
    end;
  end;
  //--
  LNotificationChannel.enableLights(true);
  if ALightColor <> 0 then LNotificationChannel.setLightColor(ALightColor);
  //--
  LNotificationChannel.setBypassDnd(ABypassDnd);
  //--
  LNotificationChannel.setShowBadge(AShowBadge);
  //--
  if ASoundUri = 'content://settings/system/notification_sound' then
    LNotificationChannel.setSound(
      TJSettings_System.JavaClass.DEFAULT_NOTIFICATION_URI,
      TJNotification.JavaClass.AUDIO_ATTRIBUTES_DEFAULT)
  else if ASoundUri <> '' then
    LNotificationChannel.setSound(
      StrToJURI(ASoundUri),
      TJNotification.JavaClass.AUDIO_ATTRIBUTES_DEFAULT)
  else
    LNotificationChannel.setSound(nil, nil);
  //--
  var LNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  var LNotificationManager := TJNotificationManager.Wrap((LNotificationServiceNative as ILocalObject).GetObjectID);
  LNotificationManager.createNotificationChannel(LNotificationChannel);
  {$ENDIF}
  {$ENDREGION}

end;

{*****************************************************************************}
class procedure TALNotificationService.setBadgeCount(const aNewValue: integer);
begin

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  SharedApplication.setApplicationIconBadgeNumber(aNewValue);
  {$ENDIF}
  {$ENDREGION}

end;

{****************************************}
procedure TALNotificationService.GetToken;
begin
  FFirebaseMessaging.GetToken;
end;

{***************************************************************}
procedure TALNotificationService.removeAllDeliveredNotifications;
begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}

  var LNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  var LNotificationManager := TJNotificationManager.Wrap((LNotificationServiceNative as ILocalObject).GetObjectID);
  LNotificationManager.cancelAll;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  // under ios 9- no way to remove delivered notifications
  if not TOSVersion.Check(10) then exit;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removeAllDeliveredNotifications;

  {$ENDIF}
  {$ENDREGION}

end;

{***********************************************************************}
procedure TALNotificationService.onFCMTokenRefresh(const aToken: String);
begin
  if assigned(FOnTokenRefresh) then
    FOnTokenRefresh(aToken);
end;

{************************************************************************************}
procedure TALNotificationService.onFCMMessageReceived(const aPayload: TALStringListW);
begin
  if assigned(FOnNotificationReceived) then
    FOnNotificationReceived(aPayload);
end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*********************************************************************************************************}
procedure TALNotificationService.PermissionsRequestResultHandler(const Sender: TObject; const M: TMessage);
begin
  if (M is TPermissionsRequestResultMessage) then begin

    var LMsg := TPermissionsRequestResultMessage(M);
    if LMsg.Value.RequestCode <> RequestPermissionsCode then exit;

    If MainActivity.checkSelfPermission(StringToJString('android.permission.POST_NOTIFICATIONS')) <> TJPackageManager.JavaClass.PERMISSION_GRANTED then begin
      {$IFDEF DEBUG}
      allog('TALNotificationService.PermissionsRequestResultHandler', 'granted: ' + ALBoolToStrW(False), TalLogType.verbose);
      {$ENDIF}
      if assigned(fOnAuthorizationRefused) then
        fOnAuthorizationRefused(self);
    end
    else begin
      {$IFDEF DEBUG}
      allog('TALNotificationService.PermissionsRequestResultHandler', 'granted: ' + ALBoolToStrW(True), TalLogType.verbose);
      {$ENDIF}
      if assigned(fOnAuthorizationGranted) then
        fOnAuthorizationGranted(self);
    end;

  end;
end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{****************************************************************************************************************************************}
procedure TALNotificationService.UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin

  // If the local or remote notifications of your app or app extension interact
  // with the user in any way, you must call this method to request authorization
  // for those interactions. The first time your app ever calls the method, the
  // system prompts the user to authorize the requested options. The user may
  // respond by granting or denying authorization, and the system stores the user’s
  // response so that subsequent calls to this method do not prompt the user again.
  // The user may change the allowed interactions at any time. Use the
  // getNotificationSettingsWithCompletionHandler: method to determine what your
  // app is allowed to do.

  {$IFDEF DEBUG}
  allog('TALNotificationService.UserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler', 'granted: ' + ALBoolToStrW(granted), TalLogType.verbose);
  {$ENDIF}

 if (not granted) then begin
   TThread.Synchronize(nil, // << Strangely it's seam this function is not called from the mainThread
     procedure
     begin
       if assigned(fOnAuthorizationRefused) then
         fOnAuthorizationRefused(self);
     end);
  end
  else begin
   TThread.Synchronize(nil, // << Strangely it's seam this function is not called from the mainThread
     procedure
     begin
       if assigned(fOnAuthorizationGranted) then
         fOnAuthorizationGranted(self);
     end);
  end;

end;

{$ENDIF}
{$ENDREGION}

end.
