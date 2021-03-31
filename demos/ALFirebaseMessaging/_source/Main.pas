unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  {$IF Defined(IOS) or Defined(ANDROID)}
  Grijjy.ErrorReporting,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Helpers.Android,
  FMX.platform.android,
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.Helpers,
  Androidapi.JNI.Support,
  Androidapi.JNI.Os,
  Androidapi.JNI.Media,
  Androidapi.JNI.JavaTypes,
  ALAndroidFirebaseApi,
  ALAndroidApi,
  ALGraphics,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.Foundation,
  iOSapi.UIKit,
  Macapi.Helpers,
  iOSapi.Helpers,
  ALIosUserNotificationsApi,
  ALIosFirebaseApi,
  {$ENDIF}
  system.net.httpClient,
  system.Messaging,
  alString,
  alStringList,
  alFirebase,
  alcommon,
  FMX.Memo.Types,
  ALFmxObjects;

type
  TForm1 = class(TForm)
    ButtonShowSampleNotification: TButton;
    Memo1: TMemo;
    ButtonGetToken: TButton;
    TextIntro: TALText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure ButtonShowSampleNotificationClick(Sender: TObject);
  private
    { Private declarations }
    fFirebaseInstanceIdClient: TALFirebaseInstanceIdClient;
    fFirebaseMessagingClient: TALFirebaseMessagingClient;
    procedure onFCMTokenRefresh(const aToken: String);
    procedure onFCMMessageReceived(const aPayload: TAlStringListU);
    {$IF defined(IOS)}
    procedure UserNotificationCenteraddNotificationRequestWithCompletionHandler(error: NSError);
    {$ENDIF}
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF Defined(MSWINDOWS) or Defined(_MACOS)}
    procedure ApplicationExceptionHandler(Sender: TObject; E: Exception);
    {$ENDIF}
    procedure ShowNotification(const aTitle: String;
                               const aText: String;
                               const aLargeicon: String;
                               const aTicker: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{********************}
{$IF Defined(ANDROID)}
procedure _createNotificationChannel(const aChannel_ID: String; const aChannel_Name: String; const aImportance: integer; const aEnableVibration: boolean);
var aNotificationChannel: JNotificationChannel;
    aNotificationServiceNative: JObject;
    aNotificationManager: JNotificationManager;
    aPattern: TJavaArray<Int64>;
begin
  aNotificationChannel := TJNotificationChannel.JavaClass.init(StringToJString(aChannel_ID), StrToJCharSequence(aChannel_Name), aImportance);
  if aEnableVibration then begin
    aNotificationChannel.enableVibration(aEnableVibration);
    aPattern := TJavaArray<Int64>.create(2);
    try
      aPattern[0] := 0;
      aPattern[1] := 1200;
      aNotificationChannel.setVibrationPattern(aPattern);
    finally
      AlFreeAndNil(aPattern);
    end;
  end;
  aNotificationChannel.setShowBadge(true);
  aNotificationChannel.enableLights(true);
  aNotificationChannel.setSound(nil, nil);
  aNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  aNotificationManager := TJNotificationManager.Wrap((aNotificationServiceNative as ILocalObject).GetObjectID);
  aNotificationManager.createNotificationChannel(aNotificationChannel);
end;
{$ENDIF}

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ELSE}
  Application.OnException := ApplicationExceptionHandler;
  {$ENDIF}

  {$IF defined(ANDROID)}
  // Create the NotificationChannel, but only on API 26+ because
  // the NotificationChannel class is new and not in the support library
  if TJBuild_VERSION.JavaClass.SDK_INT >= 26 {oreo} then begin
    _createNotificationChannel('demo', 'demo',    1, true);
  end;
  {$ENDIF}

  fFirebaseInstanceIdClient := TALFirebaseInstanceIdClient.create;
  fFirebaseInstanceIdClient.onTokenRefresh := onFCMTokenRefresh;
  FFirebaseMessagingClient := TalFirebaseMessagingClient.create(fFirebaseInstanceIdClient);
  FFirebaseMessagingClient.OnMessageReceived := OnFCMMessageReceived;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FFirebaseMessagingClient.disconnect;
  AlFreeAndNil(FFirebaseMessagingClient);
  AlFreeAndNil(fFirebaseInstanceIdClient);
  {$IF Defined(IOS) or Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}
end;

{************************************}
{$IF Defined(IOS) or Defined(ANDROID)}
procedure TForm1.ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
var aReport: IgoExceptionReport;
begin
  aReport := TgoExceptionReportMessage(M).Report;
  allog('ERROR', aReport.Report, TalLogType.error);
  memo1.lines.clear;
  memo1.lines.Add(aReport.Report);

  {$IF Defined(IOS)}
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil,
      procedure
      begin
        Halt(1); // << This is the only way i found to crash the app :(
      end);
    end).Start;
  {$ELSE}
  Application.Terminate;
  {$ENDIF}

end;
{$ENDIF}

{*****************************************}
{$IF Defined(MSWINDOWS) or Defined(_MACOS)}
procedure TForm1.ApplicationExceptionHandler(Sender: TObject; E: Exception);
begin
  Application.Terminate;
end;
{$ENDIF}

{*******************************************************}
procedure TForm1.onFCMTokenRefresh(const aToken: String);
begin
  ALLog('onFCMTokenRefresh', 'Token: ' + aToken);
  memo1.lines.clear;
  memo1.lines.Add('onFCMTokenRefresh: ' + aToken);
end;

{********************************************************************}
procedure TForm1.onFCMMessageReceived(const aPayload: TAlStringListU);
begin
  ALLog('onFCMMessageReceived', aPayload.text);
  memo1.lines.clear;
  memo1.lines.Add('onFCMMessageReceived');
  memo1.lines.Add(aPayload.Text);
end;

{******************************************************************}
procedure TForm1.ButtonShowSampleNotificationClick(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      ShowNotification('Notification Title',
                       'Notification Text',
                       'https://i.stack.imgur.com/EwPfY.jpg?s=64&g=1',
                       'Notification Ticker');
    end).Start;
end;

{****************************************************}
procedure TForm1.ButtonGetTokenClick(Sender: TObject);
begin
  ALLog('Token', fFirebaseInstanceIdClient.getToken);
  memo1.lines.clear;
  memo1.lines.Add('Token: ' + fFirebaseInstanceIdClient.getToken);
end;

{*****************************}
//this procedure is multithread
//NOTE: this procedure must be called from a background thread because it's download the
//      Largeicon in sync
procedure TForm1.ShowNotification(const aTitle: String;
                                  const aText: String;
                                  const aLargeicon: String;
                                  const aTicker: String);

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  var aLargeIconBitmap: Jbitmap;
      aStream: TMemoryStream;
      aHttpClient: THttpClient;
      aHTTPResponse: IHTTPResponse;
      aNotificationBuilder: JNotificationCompat_Builder;
      aNotificationManager: JnotificationManager;
      aNotificationServiceNative: JObject;
      aintent: Jintent;
      aPendingIntent: JPendingIntent;
      aPattern: TJavaArray<Int64>;
      aDefaults: integer;
      aTag: String;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION 'ANDROID'}
  {$IF defined(ANDROID)}
  aLargeIconBitmap := nil;
  try

    //download the aLargeiconBitmap
    if aLargeicon <> '' then begin
      try

        aHttpClient := THTTPClient.Create;
        aStream := TMemoryStream.Create;
        try

            aHTTPResponse := aHttpClient.Get(aLargeicon, // AURL
                                             aStream); // AResponseContent

            if (aHTTPResponse.StatusCode = 200) and
               (aStream.size > 0) then begin
              aLargeIconBitmap := ALFitIntoAndCropImageV2(
                                    aStream,
                                    function(const aOriginalSize: TPointF): TpointF
                                    begin
                                      if aOriginalSize.x > aOriginalSize.y then result := TpointF.create(aOriginalSize.Y, aOriginalSize.Y)
                                      else result := TpointF.create(aOriginalSize.x, aOriginalSize.x);
                                    end,
                                    TpointF.create(-50, -50));
            end;

        finally
          aHTTPResponse := nil;
          AlFreeAndNil(aStream);
          AlFreeAndNil(aHttpClient);
        end;

      except
        on E: Exception do begin
          ALLog('ShowNotification (ERROR)', E.message, TalLogType.error);
          memo1.lines.clear;
          memo1.lines.Add(E.message);
          if assigned(aLargeIconBitmap) then aLargeIconBitmap.recycle;
          aLargeIconBitmap := nil;
        end;
      end;

    end;

    //init aDefaults
    aDefaults := TJNotification.javaclass.DEFAULT_LIGHTS;

    //create aPendingIntent
    aIntent := TJIntent.javaclass.init(TJALFirebaseMessagingService.javaclass.ACTION_MESSAGERECEIVED);
    //----
    aIntent.putExtra(StringToJstring('notification'), 1);
    aIntent.putExtra(StringToJstring('notification.presented'), '1');
    aIntent.putExtra(StringToJstring('extraData'), StringToJstring('sample of extra data'));
    //----
    aIntent.setClass(TAndroidHelper.Context, MainActivity.getclass);
    aIntent.setFlags(TJIntent.javaclass.FLAG_ACTIVITY_CLEAR_TOP);
    aPendingIntent := TJPendingIntent.javaclass.getActivity(TAndroidHelper.Context, // context	Context: The Context in which this PendingIntent should start the activity.
                                                            random(maxint), // requestCode	int: Private request code for the sender
                                                            aIntent, // intents	Intent: Array of Intents of the activities to be launched.
                                                            TJPendingIntent.javaclass.FLAG_UPDATE_CURRENT); // flags	int: May be FLAG_ONE_SHOT, - Flag indicating that this PendingIntent can be used only once.
                                                                                                            //                    FLAG_NO_CREATE, - Flag indicating that if the described PendingIntent does not already exist, then simply return null instead of creating it.
                                                                                                            //                    FLAG_CANCEL_CURRENT, - Flag indicating that if the described PendingIntent already exists, the current one should be canceled before generating a new one.
                                                                                                            //                    FLAG_UPDATE_CURRENT, - Flag indicating that if the described PendingIntent already exists, then keep it but replace its extra data with what is in this new Intent.
                                                                                                            //                    FLAG_IMMUTABLE - Flag indicating that the created PendingIntent should be immutable.
                                                                                                            // or any of the flags as supported by Intent.fillIn() to
                                                                                                            // control which unspecified parts of the intent that can
                                                                                                            // be supplied when the actual send happens. */

    //init a Tag
    aTag := ALInttostrU(random(maxint));

    //create the NotificationBuilder
    aNotificationBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, StringToJstring('demo'));
    //aNotificationBuilder := aNotificationBuilder.setColor(WinDT_getNotificationIconBGColor(aType));
    if (atext <> '') then aNotificationBuilder := aNotificationBuilder.setContentText(StrToJCharSequence(atext));
    if (aTitle <> '') then aNotificationBuilder := aNotificationBuilder.setContentTitle(StrToJCharSequence(atitle));
    if (aLargeIconBitmap <> nil) then aNotificationBuilder := aNotificationBuilder.setLargeIcon(aLargeIconBitmap);
    //-----
    //
    // finally i prefere to not show the Number, because it's mean we must also need to reset it and this number
    // is often ignored by the user, so don't complicate my life with it
    //
    //aSharedPreferences := TJPreferenceManager.javaclass.getDefaultSharedPreferences(TAndroidHelper.Context.getApplicationContext);
    //aCurrentCount := aSharedPreferences.getInt(StringToJstring('notification.count_' + aTag), 0);
    //aPreferenceEditor := aSharedPreferences.edit;
    //aPreferenceEditor.putInt(StringToJstring('notification.count_' + aTag), aCurrentCount + 1);
    //aPreferenceEditor.commit;
    //if (acurrentCount > 0) then aNotificationBuilder := aNotificationBuilder.setNumber(acurrentCount + 1);
    //-----
    aNotificationBuilder := aNotificationBuilder.setOnlyAlertOnce(true);
    aNotificationBuilder := aNotificationBuilder.setSmallIcon(
      TAndroidHelper.Context.getResources().getIdentifier(
        StringToJstring('notification_icon'), // name	String: The name of the desired resource.
        StringToJstring('drawable'), // String: Optional default resource type to find, if "type/" is not included in the name. Can be null to require an explicit type.
        TAndroidHelper.Context.getPackageName())); // String: Optional default package to find, if "package:" is not included in the name. Can be null to require an explicit package.
    if (aTicker <> '') then aNotificationBuilder := aNotificationBuilder.setTicker(StrToJCharSequence(aTicker));

    aDefaults := aDefaults or TJNotification.javaclass.DEFAULT_VIBRATE;
    aPattern := TJavaArray<Int64>.create(2);
    try
      aPattern[0] := 0;
      aPattern[1] := 1200;
      aNotificationBuilder := aNotificationBuilder.setVibrate(aPattern);
    finally
      AlFreeAndNil(aPattern);
    end;

    aDefaults := aDefaults or TJNotification.javaclass.DEFAULT_SOUND;
    aNotificationBuilder := aNotificationBuilder.setsound(TJRingtoneManager.JavaClass.getDefaultUri(TJRingtoneManager.JavaClass.TYPE_NOTIFICATION));

    aNotificationBuilder := aNotificationBuilder.setPriority(1{high});
    aNotificationBuilder := aNotificationBuilder.setDefaults(aDefaults);
    aNotificationBuilder := aNotificationBuilder.setWhen(TJDate.Create.getTime);
    //aNotificationBuilder := aNotificationBuilder.setShowWhen(true);
    aNotificationBuilder := aNotificationBuilder.setAutoCancel(true);
    aNotificationBuilder := aNotificationBuilder.setContentIntent(aPendingIntent);

    aNotificationServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
    aNotificationManager := TJNotificationManager.Wrap((aNotificationServiceNative as ILocalObject).GetObjectID);
    aNotificationManager.Notify(StringToJstring(aTag), // tag	String: A string identifier for this notification. May be null.
                                0, // id	int: An identifier for this notification. The pair (tag, id) must be unique within your application.
                                anotificationBuilder.build()); // notification	Notification: A Notification object describing what to show the user. Must not be null.

  finally
    if assigned(aLargeIconBitmap) then aLargeIconBitmap.recycle;
    aLargeIconBitmap := nil;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  // under ios 9- no way to show notification when the app is in foreground
  if not TOSVersion.Check(10) then exit;

  //show the notification in ios 10+
  TThread.Synchronize(nil,
    procedure
    var aUserInfo: NSMutableDictionary;
        aNotificationRequest: UNNotificationRequest;
        aNotificationContent: UNMutableNotificationContent;
        aTag: String;
    begin

      aNotificationContent := TUNMutableNotificationContent.Create;
      //----
      aUserInfo := TNSMutableDictionary.Create;
      aUserInfo.setValue(StringToID('1'), StrToNsStr('notification'));
      aUserInfo.setValue(StringToID('1'), StrToNsStr('notification.present'));
      aUserInfo.setValue(StringToID('extraData'), StrToNsStr('sample of extra data'));
      aNotificationContent.setUserInfo(aUserInfo);
      //-----
      if (atext <> '') then aNotificationContent.setbody(StrToNSStr(atext));
      //-----
      aNotificationContent.setSound(TUNNotificationSound.Wrap(TUNNotificationSound.OCClass.defaultSound));
      //-----
      aTag := ALInttostrU(random(maxint));
      aNotificationContent.setThreadIdentifier(StrToNSStr(aTag));
      //-----
      if (aTitle <> '') then aNotificationContent.setTitle(StrToNSStr(aTitle));
      //-----
      aNotificationRequest := TUNNotificationRequest.Wrap(TUNNotificationRequest.OCClass.requestWithIdentifier(StrToNsStr(ALinttostrU(random(maxint))), // identifier: NSString; ;
                                                                                                               aNotificationContent, // content: UNNotificationContent
                                                                                                               nil)); // trigger: UNNotificationTrigger
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.addNotificationRequest(aNotificationRequest, // request: UNNotificationRequest;
                                                                                         UserNotificationCenteraddNotificationRequestWithCompletionHandler); //withCompletionHandler: TUserNotificationsWithCompletionHandler

    end);

  {$ENDIF}
  {$ENDREGION}

end;

{**********}
{$IFDEF IOS}
procedure TForm1.UserNotificationCenteraddNotificationRequestWithCompletionHandler(error: NSError);
begin
  {$IFDEF DEBUG}
  if (error <> nil) then allog('TForm1.UserNotificationCenteraddNotificationRequestWithCompletionHandler', 'ERROR - ' + NSStrToStr(error.localizedDescription) +
                                                                                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else allog('TForm1.UserNotificationCenteraddNotificationRequestWithCompletionHandler', 'SUCCESS' +
                                                                                         ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}
end;
{$ENDIF}

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
