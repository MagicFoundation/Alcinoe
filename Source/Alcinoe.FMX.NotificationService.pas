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
  System.UITypes,
  System.Generics.Collections,
  {$IF defined(IOS)}
  iOSapi.Foundation,
  {$ENDIF}
  Alcinoe.StringList,
  Alcinoe.FMX.Firebase.Messaging;

type

  //https://developer.android.com/reference/android/app/Notification#VISIBILITY_PRIVATE
  //VISIBILITY_PRIVATE=Show this notification on all lockscreens, but conceal sensitive or private information on secure lockscreens
  //VISIBILITY_PUBLIC=Show this notification in its entirety on all lockscreens
  //VISIBILITY_SECRET=Do not reveal any part of this notification on a secure lockscreen
  TALNotificationVisibility = (Public, Private, Secret);
  //https://developer.android.com/reference/android/app/NotificationManager#IMPORTANCE_DEFAULT
  //IMPORTANCE_DEFAULT=shows everywhere, makes noise, but does not visually intrude
  //IMPORTANCE_NONE=does not show in the shade
  //IMPORTANCE_MIN=only shows in the shade, below the fold
  //IMPORTANCE_LOW=Shows in the shade, and potentially in the status bar but is not audibly intrusive
  //IMPORTANCE_HIGH=shows everywhere, makes noise and peeks. May use full screen intents
  //IMPORTANCE_MAX=Unused
  TALNotificationImportance = (None, Default, Min, Low, High, Max);

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALNotification = record
  private
    FTag: String;
    FChannelId: String;
    FTitle: String;
    FText: String;
    FTicker: String;
    FLargeIconStream: TMemoryStream;
    FLargeIconUrl: String;
    FSmallIconResName: String;
    FPayload: TArray<TPair<String, String>>;
    FNumber: integer;
    FSound: boolean;
    FVibrate: boolean;
    FLights: boolean;
    FImportance: TALNotificationImportance;
    FVisibility: TALNotificationVisibility;
  public
    constructor Create(const ATag: String);
    function SetChannelId(const aChannelId: String): TALNotification; //Specifies the channel the notification should be delivered on. No-op on versions prior to O.
    function SetTitle(const aTitle: String): TALNotification; //Set the title (first row) of the notification, in a standard notification.
    function SetText(const aText: String): TALNotification; //Set the text (second row) of the notification, in a standard notification.
    function setTicker(const aTicker: String): TALNotification; //Sets the "ticker" text which is sent to accessibility services.
    function SetLargeIconStream(const aLargeIconStream: TMemoryStream): TALNotification; //Sets the large icon that is shown in the notification.
    function SetLargeIconUrl(const aLargeIconUrl: String): TALNotification; //Sets the large icon that is shown in the notification.
    function setSmallIconResName(const aSmallIconResName: String): TALNotification; //Set the small icon to use in the notification layouts.
    function SetPayload(const aPayload: TALStringListW): TALNotification;
    function AddPayload(const aName, aValue: String): TALNotification;
    function setNumber(const aNumber: integer): TALNotification; //Sets the number of items this notification represents. On the latest platforms, this may be displayed as a badge count for Launchers that support badging.
    function setSound(const aSound: boolean): TALNotification;
    function setVibrate(const aVibrate: boolean): TALNotification;
    function setLights(const aLights: boolean): TALNotification;
    function setImportance(const aImportance: TALNotificationImportance): TALNotification;
    function setVisibility(const aVisibility: TALNotificationVisibility): TALNotification;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALNotificationChannel = record
  private
    FID: String;
    FName: String;
    FImportance: TALNotificationImportance;
    FLockscreenVisibility: TALNotificationVisibility;
    FEnableVibration: Boolean;
    FEnableLights: Boolean;
    FEnableSound: Boolean;
    FShowBadge: Boolean;
  public
    constructor Create(const AID: String); // The id of the channel. Must be unique per package
    function SetName(const AName: String): TALNotificationChannel; // The user visible name of the channel.
    function SetImportance(const AImportance: TALNotificationImportance): TALNotificationChannel; // Sets the level of interruption of this notification channel.
    function SetLockscreenVisibility(const ALockscreenVisibility: TALNotificationVisibility): TALNotificationChannel; // Sets whether notifications posted to this channel appear on the lockscreen or not, and if so, whether they appear in a redacted form.
    function SetEnableVibration(const AEnableVibration: Boolean): TALNotificationChannel; // Sets whether notification posted to this channel should vibrate.
    //function SetVibrationPattern(const AVibrationPattern: TArray<Int64>): TALNotificationChannel;
    function SetEnableLights(const AEnableLights: Boolean): TALNotificationChannel; // Sets whether notifications posted to this channel should display notification lights
    //function SetLightColor(const ALightColor: TAlphaColor): TALNotificationChannel;
    function SetEnableSound(const AEnableSound: Boolean): TALNotificationChannel;
    //function SetSoundUri(const ASoundUri: String): TALNotificationChannel;
    function SetShowBadge(const AShowBadge: Boolean): TALNotificationChannel; // Sets whether notifications posted to this channel can appear as application icon badges in a Launcher.
  End;

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

  private
    class function CreateInstance: TALNotificationService;
    class var FInstance: TALNotificationService;
    class function GetInstance: TALNotificationService; static;
  public
    type
      TCreateInstanceFunct = function: TALNotificationService;
    class var CreateInstanceFunct: TCreateInstanceFunct;
    class property Instance: TALNotificationService read GetInstance;
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
    FLastGeneratedUniqueID: integer;
    procedure onFCMTokenRefresh(const aToken: String);
    procedure onFCMMessageReceived(const aPayload: TALStringListW);
    {$HINTS OFF}
    function GenerateUniqueID: integer;
    {$HINTS ON}
  public
    class var TmpPath: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure RequestNotificationPermission;
    procedure CreateNotificationChannel(const ANotificationChannel: TALNotificationChannel); virtual;
    procedure SetBadgeCount(const aNewValue: integer); virtual;
    procedure ShowNotification(const ANotification: TALNotification); virtual;
    procedure GetToken;
    procedure removeAllDeliveredNotifications;
    property OnAuthorizationRefused: TNotifyEvent read fOnAuthorizationRefused write fOnAuthorizationRefused;
    property OnAuthorizationGranted: TNotifyEvent read fOnAuthorizationGranted write fOnAuthorizationGranted;
    property OnTokenRefresh: TTokenRefreshEvent read fOnTokenRefresh write fOnTokenRefresh;
    property OnNotificationReceived: TNotificationReceivedEvent read fOnNotificationReceived write fOnNotificationReceived;
  end;

{$REGION ' ANDROID'}
{$IF defined(android)}
function ALNotificationVisibilityToNative(const AVisibility: TALNotificationVisibility): integer;
function ALNotificationImportanceToNative(const AImportance: TALNotificationImportance): integer;
{$ENDIF}
{$ENDREGION}

implementation

uses
  System.Types,
  System.SysUtils,
  System.DateUtils,
  System.Net.HttpClient,
  System.IOUtils,
  System.Math,
  {$IF defined(android)}
  Androidapi.Helpers,
  AndroidApi.Jni,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNIBridge,
  Androidapi.JNI.Provider,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Support,
  FMX.Platform.Android,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  Macapi.ObjectiveC,
  iOSapi.UserNotifications,
  FMX.Helpers.iOS,
  {$ENDIF}
  ALcinoe.FMX.Graphics,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.stringUtils,
  Alcinoe.Common;

{*****************************************************}
constructor TALNotification.Create(const ATag: String);
begin
  FTag := ATag;
  FChannelId := '';
  FTitle := '';
  FText := '';
  FTicker := '';
  FLargeIconStream := nil;
  FLargeIconUrl := '';
  FSmallIconResName := '';
  FPayload := [];
  FNumber := -1;
  FSound := True;
  FVibrate := True;
  FLights := True;
  FImportance := TALNotificationImportance.Default;
  FVisibility := TALNotificationVisibility.Private;
end;

{*******************************************************************************}
function TALNotification.SetChannelId(const aChannelId: String): TALNotification;
begin
  FChannelId := aChannelId;
  result := Self;
end;

{***********************************************************************}
function TALNotification.SetTitle(const aTitle: String): TALNotification;
begin
  FTitle := aTitle;
  result := Self;
end;

{*********************************************************************}
function TALNotification.SetText(const aText: String): TALNotification;
begin
  FText := aText;
  result := Self;
end;

{*************************************************************************}
function TALNotification.setTicker(const aTicker: String): TALNotification;
begin
  FTicker := aTicker;
  result := Self;
end;

{**************************************************************************************************}
function TALNotification.SetLargeIconStream(const aLargeIconStream: TMemoryStream): TALNotification;
begin
  FLargeIconStream := aLargeIconStream;
  result := Self;
end;

{*************************************************************************************}
function TALNotification.SetLargeIconUrl(const aLargeIconUrl: String): TALNotification;
begin
  FLargeIconUrl := aLargeIconUrl;
  result := Self;
end;

{*********************************************************************************************}
function TALNotification.setSmallIconResName(const aSmallIconResName: String): TALNotification;
begin
  FSmallIconResName := aSmallIconResName;
  result := Self;
end;

{***********************************************************************************}
function TALNotification.SetPayload(const aPayload: TALStringListW): TALNotification;
begin
  setlength(FPayload, aPayload.Count);
  for var I := 0 to aPayload.Count - 1 do
    FPayload[I] := TPair<String, String>.create(aPayload.Names[i], aPayload.ValueFromIndex[i]);
  result := Self;
end;

{********************************************************************************}
function TALNotification.AddPayload(const aName, aValue: String): TALNotification;
begin
  setlength(FPayload, length(FPayload) + 1);
  FPayload[high(FPayload)] := TPair<String, String>.create(aName, aValue);
  result := Self;
end;

{**************************************************************************}
function TALNotification.setNumber(const aNumber: integer): TALNotification;
begin
  FNumber := aNumber;
  result := Self;
end;

{************************************************************************}
function TALNotification.setSound(const aSound: boolean): TALNotification;
begin
  FSound := aSound;
  result := Self;
end;

{****************************************************************************}
function TALNotification.setVibrate(const aVibrate: boolean): TALNotification;
begin
  FVibrate := aVibrate;
  result := Self;
end;

{**************************************************************************}
function TALNotification.setLights(const aLights: boolean): TALNotification;
begin
  FLights := aLights;
  result := Self;
end;

{****************************************************************************************************}
function TALNotification.setImportance(const aImportance: TALNotificationImportance): TALNotification;
begin
  FImportance := aImportance;
  result := Self;
end;

{****************************************************************************************************}
function TALNotification.setVisibility(const aVisibility: TALNotificationVisibility): TALNotification;
begin
  FVisibility := aVisibility;
  result := Self;
end;

{***********************************************************}
constructor TALNotificationChannel.Create(const AID: String);
begin
  FID := AID;
  FName := AID;
  FImportance := TALNotificationImportance.Default;
  FLockscreenVisibility := TALNotificationVisibility.Private;
  FEnableVibration := true;
  FEnableLights := true;
  FEnableSound := true;
  FShowBadge := true;
end;

{***********************************************************************************}
function TALNotificationChannel.SetName(const AName: String): TALNotificationChannel;
begin
  FName := AName;
  result := Self;
end;

{******************************************************************************************************************}
function TALNotificationChannel.SetImportance(const AImportance: TALNotificationImportance): TALNotificationChannel;
begin
  FImportance := AImportance;
  result := Self;
end;

{**************************************************************************************************************************************}
function TALNotificationChannel.SetLockscreenVisibility(const ALockscreenVisibility: TALNotificationVisibility): TALNotificationChannel;
begin
  FLockscreenVisibility := ALockscreenVisibility;
  result := Self;
end;

{**********************************************************************************************************}
function TALNotificationChannel.SetEnableVibration(const AEnableVibration: Boolean): TALNotificationChannel;
begin
  FEnableVibration := AEnableVibration;
  result := Self;
end;

{****************************************************************************************************}
function TALNotificationChannel.SetEnableLights(const AEnableLights: Boolean): TALNotificationChannel;
begin
  FEnableLights := AEnableLights;
  result := Self;
end;

{**************************************************************************************************}
function TALNotificationChannel.SetEnableSound(const AEnableSound: Boolean): TALNotificationChannel;
begin
  FEnableSound := AEnableSound;
  result := Self;
end;

{**********************************************************************************************}
function TALNotificationChannel.SetShowBadge(const AShowBadge: Boolean): TALNotificationChannel;
begin
  FShowBadge := AShowBadge;
  result := Self;
end;

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
  FLastGeneratedUniqueID := integer(ALDateTimeToUnixms(ALUTCNow) mod Maxint);

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

{***************************************************************************}
class function TALNotificationService.CreateInstance: TALNotificationService;
begin
  result := TALNotificationService.Create;
end;

{************************************************************************}
class function TALNotificationService.GetInstance: TALNotificationService;
begin
  if FInstance = nil then begin
    var LInstance := CreateInstanceFunct;
    if AtomicCmpExchange(Pointer(FInstance), Pointer(LInstance), nil) <> nil then ALFreeAndNil(LInstance)
  end;
  Result := FInstance;
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

{*************************************************************************************************************}
procedure TALNotificationService.CreateNotificationChannel(const ANotificationChannel: TALNotificationChannel);
begin

  {$REGION ' ANDROID'}
  {$IF defined(ANDROID)}
  // Create the NotificationChannel, but only on API 26+ (Android 8.0 oreo)
  if TOSVersion.Check(8) then begin
    var LNotificationChannel := TJNotificationChannel.JavaClass.init(
                                  StringToJString(ANotificationChannel.FID),
                                  StrToJCharSequence(ANotificationChannel.FName),
                                  ALNotificationImportanceToNative(ANotificationChannel.FImportance));
    LNotificationChannel.setLockscreenVisibility(ALNotificationVisibilityToNative(ANotificationChannel.FLockscreenVisibility));
    LNotificationChannel.enableVibration(ANotificationChannel.FEnableVibration);
    LNotificationChannel.enableLights(ANotificationChannel.FEnableLights);
    LNotificationChannel.setShowBadge(ANotificationChannel.FShowBadge);
    if ANotificationChannel.FEnableSound then
      LNotificationChannel.setSound(
        TJSettings_System.JavaClass.DEFAULT_NOTIFICATION_URI, // 'content://settings/system/notification_sound'
        TJNotification.JavaClass.AUDIO_ATTRIBUTES_DEFAULT)
    else
      LNotificationChannel.setSound(nil, nil);
    var LNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
    var LNotificationManager := TJNotificationManager.Wrap((LNotificationServiceNative as ILocalObject).GetObjectID);
    LNotificationManager.createNotificationChannel(LNotificationChannel);
  end;
  {$ENDIF}
  {$ENDREGION}

end;

{***********************************************************************}
procedure TALNotificationService.SetBadgeCount(const aNewValue: integer);
begin

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  SharedApplication.setApplicationIconBadgeNumber(aNewValue);
  {$ENDIF}
  {$ENDREGION}

end;

{**************************************************************************************}
procedure TALNotificationService.ShowNotification(const ANotification: TALNotification);
begin

  if (ANotification.FLargeIconUrl <> '') and (ANotification.FLargeIconStream = nil) then begin

    var LNotification := ANotification;
    TALNetHttpClientPool.Instance.Get(
      // const AUrl: String;
      LNotification.FLargeIconUrl,
      //
      // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
      procedure (const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject)
      begin
        var LContentStream := AContentStream;
        TThread.Synchronize(nil,
          procedure
          begin
            LNotification.FLargeIconStream := LContentStream;
            TALNotificationService.Instance.ShowNotification(LNotification);
          end);
      end,
      //
      // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
      procedure (const AErrMessage: string; var AExtData: Tobject)
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            LNotification.FLargeIconStream := TMemoryStream.create;
            try
              TALNotificationService.Instance.ShowNotification(LNotification);
            finally
              ALFreeAndNil(LNotification.FLargeIconStream);
            end;
          end);
      end);

    exit;

  end;

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var LLargeIconBitmap: Jbitmap := nil;
  try

    //download the LLargeIconBitmap
    if (ANotification.FLargeIconStream <> nil) and (ANotification.FLargeIconStream.Size > 0)  then begin
      try

        LLargeIconBitmap := ALFitIntoAndCropImageV2(
                              ANotification.FLargeIconStream,
                              function(const aOriginalSize: TPointF): TpointF
                              begin
                                if aOriginalSize.x > aOriginalSize.y then result := TpointF.create(aOriginalSize.Y, aOriginalSize.Y)
                                else result := TpointF.create(aOriginalSize.x, aOriginalSize.x);
                              end,
                              TpointF.create(-50, -50));

      except
        on E: Exception do begin
          ALLog('TALNotification.Show', E.Message, TalLogType.Error);
          if assigned(LLargeIconBitmap) then LLargeIconBitmap.recycle;
          LLargeIconBitmap := nil;
        end;
      end;
    end;

    //-----
    var LIntent := TAndroidHelper.Context.getPackageManager.getLaunchIntentForPackage(TAndroidHelper.Context.getPackageName);
    LIntent.setAction(TJNotificationInfo.JavaClass.ACTION_NOTIFICATION); // need by the FMX framework to detect it's a notification and sent the TMessageReceivedNotification message
    LIntent.putExtra(StringToJstring('google.message_id'), StringToJstring('alcinoe:'+ALLowerCase(ALNewGUIDStringW(true{WithoutBracket}, true{WithoutHyphen})))); // Need to unduplicate received messages in TALFirebaseMessaging
    for var I := low(ANotification.FPayload) to high(ANotification.FPayload) do
      LIntent.putExtra(StringToJstring(ANotification.FPayload[i].Key), StringToJstring(ANotification.FPayload[i].Value));
    LIntent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP or TJIntent.javaclass.FLAG_ACTIVITY_CLEAR_TOP);
    var LPendingIntent := TJPendingIntent.javaclass.getActivity(
                            TAndroidHelper.Context, // context	Context: The Context in which this PendingIntent should start the activity.
                            GenerateUniqueID, // requestCode	int: Private request code for the sender
                            LIntent, // intents	Intent: Array of Intents of the activities to be launched.
                            TJPendingIntent.javaclass.FLAG_IMMUTABLE); // flags	int: May be FLAG_ONE_SHOT, - Flag indicating that this PendingIntent can be used only once.
                                                                       //                   FLAG_NO_CREATE, - Flag indicating that if the described PendingIntent does not already exist, then simply return null instead of creating it.
                                                                       //                   FLAG_CANCEL_CURRENT, - Flag indicating that if the described PendingIntent already exists, the current one should be canceled before generating a new one.
                                                                       //                   FLAG_UPDATE_CURRENT, - Flag indicating that if the described PendingIntent already exists, then keep it but replace its extra data with what is in this new Intent.
                                                                       //                   FLAG_IMMUTABLE - Flag indicating that the created PendingIntent should be immutable.
                                                                       // or any of the flags as supported by Intent.fillIn() to
                                                                       // control which unspecified parts of the intent that can
                                                                       // be supplied when the actual send happens. */

    //-----
    var LNotificationBuilder := TJApp_NotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, StringToJstring(ANotification.FChannelId));
    if ANotification.FTitle <> '' then LNotificationBuilder := LNotificationBuilder.setContentTitle(StrToJCharSequence(ANotification.Ftitle));
    if ANotification.Ftext <> '' then LNotificationBuilder := LNotificationBuilder.setContentText(StrToJCharSequence(ANotification.Ftext));
    if ANotification.FTicker <> '' then LNotificationBuilder := LNotificationBuilder.SetTicker(StrToJCharSequence(ANotification.FTicker));
    if LLargeIconBitmap <> nil then LNotificationBuilder := LNotificationBuilder.setLargeIcon(LLargeIconBitmap);
    if ANotification.FSmallIconResName <> '' then
      LNotificationBuilder := LNotificationBuilder.setSmallIcon(
                                TAndroidHelper.Context.getResources().getIdentifier(
                                  StringToJstring(ANotification.FSmallIconResName), // name	String: The name of the desired resource.
                                  StringToJstring('drawable'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
                                  TAndroidHelper.Context.getPackageName())); // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
    if ANotification.FNumber >= 0 then LNotificationBuilder.setNumber(ANotification.FNumber);
    var LDefaults: integer := 0;
    if ANotification.FSound then LDefaults := LDefaults or TJNotification.javaclass.DEFAULT_SOUND;
    if ANotification.FVibrate then LDefaults := LDefaults or TJNotification.javaclass.DEFAULT_Vibrate;
    if ANotification.FLights then LDefaults := LDefaults or TJNotification.javaclass.DEFAULT_Lights;
    LNotificationBuilder := LNotificationBuilder.setDefaults(LDefaults);
    if (not ANotification.FSound) and (not ANotification.FVibrate) then LNotificationBuilder.setSilent(true);
    LNotificationBuilder := LNotificationBuilder.setVisibility(ALNotificationVisibilityToNative(ANotification.FVisibility));
    if ANotification.FImportance <> TALNotificationImportance.None then begin
      var LPriority: integer;
      case ANotification.FImportance of
        TALNotificationImportance.Default: LPriority := TJNotification.JavaClass.PRIORITY_DEFAULT;
        TALNotificationImportance.Min: LPriority := TJNotification.JavaClass.PRIORITY_MIN;
        TALNotificationImportance.Low: LPriority := TJNotification.JavaClass.PRIORITY_LOW;
        TALNotificationImportance.High: LPriority := TJNotification.JavaClass.PRIORITY_HIGH;
        TALNotificationImportance.Max: LPriority := TJNotification.JavaClass.PRIORITY_MAX;
        else raise Exception.Create('Error F1BEB799-B181-491B-AA83-47A0F27CE1C1');
      end;
      LNotificationBuilder := LNotificationBuilder.setPriority(LPriority);
    end;
    LNotificationBuilder := LNotificationBuilder.setContentIntent(LPendingIntent);

    //-----
    var LNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
    var LNotificationManager := TJNotificationManager.Wrap((LNotificationServiceNative as ILocalObject).GetObjectID);
    if ANotification.FTag <> '' then
      LNotificationManager.notify(
        StringToJstring(ANotification.FTag), // tag	String: A string identifier for this notification. May be null.
        0, // id	int: An identifier for this notification. The pair (tag, id) must be unique within your application.
        LNotificationBuilder.build()) // notification	Notification: A Notification object describing what to show the user. Must not be null.
    else
      LNotificationManager.notify(
        nil, // tag	String: A string identifier for this notification. May be null.
        GenerateUniqueID, // id	int: An identifier for this notification. The pair (tag, id) must be unique within your application.
        LNotificationBuilder.build()); // notification	Notification: A Notification object describing what to show the user. Must not be null.

  finally
    if assigned(LLargeIconBitmap) then LLargeIconBitmap.recycle;
    LLargeIconBitmap := nil;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  // under ios 9- no way to show notification when the app is in foreground
  if not TOSVersion.Check(10) then exit;

  //show the notification in ios 10+
  var LNotificationContent := TUNMutableNotificationContent.Create;
  try
    var LUserInfo := TNSMutableDictionary.Create;
    try
      LUserInfo.setValue(StringToID('alcinoe:'+ALLowerCase(ALNewGUIDStringW(true{WithoutBracket}, true{WithoutHyphen}))), StrToNsStr('google.message_id')); // Need to unduplicate received messages in TALFirebaseMessaging
      for var I := low(ANotification.FPayload) to high(ANotification.FPayload) do
        LUserInfo.setValue(StringToID(ANotification.FPayload[i].Value), StrToNsStr(ANotification.FPayload[i].Key));
      LUserInfo.setValue(StringToID('1'), StrToNsStr('alcinoe.presentnotification'));
      LNotificationContent.setUserInfo(LUserInfo);
    finally
      LUserInfo.release;
    end;
    if ANotification.FTitle <> '' then LNotificationContent.setTitle(StrToNSStr(ANotification.FTitle));
    if ANotification.Ftext <> '' then LNotificationContent.setbody(StrToNSStr(ANotification.Ftext));
    if ANotification.FNumber >= 0 then LNotificationContent.setBadge(TNSNumber.Wrap(TNSNumber.OCClass.numberWithInt(ANotification.FNumber)));
    if ANotification.Fsound then LNotificationContent.setSound(TUNNotificationSound.OCClass.defaultSound);
    if ANotification.FLargeIconStream <> nil then begin
      var LFileExt := AlDetectImageExtension(ANotification.FLargeIconStream);
      Var LTmpPath := TALNotificationService.TmpPath;
      if LTmpPath = '' then LTmpPath := Tpath.GetTempPath;
      var LFileName := Tpath.Combine(
                         LTmpPath,
                         'alcinoe_notification_'+ALNewGUIDStringW(true{WithoutBracket}, true{WithoutHyphen}).tolower + ALIfThenW(LFileExt <> '', '.') + LFileExt);
      ANotification.FLargeIconStream.SaveToFile(LFileName);
      var LErrorPtr: PNSError := nil;
      var LNotificationAttachment := TUNNotificationAttachment.OCClass.attachmentWithIdentifier(
                                       nil, // identifier: NSString;
                                       StrToNSUrl(LFileName), // URL: NSURL;
                                       nil, // options: NSDictionary;
                                       @LErrorPtr); // error: PNSError
      if LErrorPtr <> nil then
        ALLog(
          'TALNotification.Show',
          NSStrToStr(TNSError.Wrap(LErrorPtr).localizedDescription),
          TalLogType.Error)
      else if LNotificationAttachment <> nil then
        LNotificationContent.SetAttachments(
          TNSArray.Wrap(
            TNSArray.OCClass.arrayWithObject(
              NSObjectToID(
                LNotificationAttachment))));
    end;
    //-----
    var LNotificationRequest := TUNNotificationRequest.OCClass.requestWithIdentifier(
                                  StrToNsStr(ANotification.FTag), // identifier: NSString; ;
                                  LNotificationContent, // content: UNNotificationContent
                                  nil); // trigger: UNNotificationTrigger
    try
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.addNotificationRequest(
        LNotificationRequest, // request: UNNotificationRequest;
        nil); //withCompletionHandler: TUserNotificationsWithCompletionHandler
    finally
      //exception if we release it here
      //LNotificationRequest.release;
    end;
  finally
    LNotificationContent.release;
  end;
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

{********************************************************}
function TALNotificationService.GenerateUniqueID: integer;
begin
  inc(FLastGeneratedUniqueID);
  result := FLastGeneratedUniqueID;
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

{***********************************************************************************************}
function ALNotificationVisibilityToNative(const AVisibility: TALNotificationVisibility): integer;
begin
  case AVisibility of
    TALNotificationVisibility.Public: Result := TJNotification.JavaClass.VISIBILITY_PUBLIC;
    TALNotificationVisibility.Private: Result := TJNotification.JavaClass.VISIBILITY_PRIVATE;
    TALNotificationVisibility.Secret: Result := TJNotification.JavaClass.VISIBILITY_SECRET;
    else raise Exception.Create('Error 276AB7CE-3809-46A5-B4B7-65A9B0630A3C');
  end;
end;

{***********************************************************************************************}
function ALNotificationImportanceToNative(const AImportance: TALNotificationImportance): integer;
begin
  case AImportance of
    TALNotificationImportance.None: Result := TJNotificationManager.JavaClass.IMPORTANCE_NONE;
    TALNotificationImportance.Default: Result := TJNotificationManager.JavaClass.IMPORTANCE_DEFAULT;
    TALNotificationImportance.Min: Result := TJNotificationManager.JavaClass.IMPORTANCE_MIN;
    TALNotificationImportance.Low: Result := TJNotificationManager.JavaClass.IMPORTANCE_LOW;
    TALNotificationImportance.High: Result := TJNotificationManager.JavaClass.IMPORTANCE_HIGH;
    TALNotificationImportance.Max: Result := TJNotificationManager.JavaClass.IMPORTANCE_MAX;
    else raise Exception.Create('Error D55BED63-B524-4422-B075-71F40CDF5327');
  end;
end;

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

initialization
  TALNotificationService.TmpPath := '';
  TALNotificationService.FInstance := nil;
  TALNotificationService.CreateInstanceFunct := @TALNotificationService.CreateInstance;

finalization
  ALFreeAndNil(TALNotificationService.FInstance);

end.
