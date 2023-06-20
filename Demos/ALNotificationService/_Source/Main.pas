unit Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.DialogService,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  FMX.Edit,
  {$IF Defined(MSWindows)}
  Alcinoe.Cipher,
  {$ENDIF}
  {$IF Defined(IOS) or Defined(ANDROID)}
  Grijjy.ErrorReporting,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.Provider,
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
  Alcinoe.AndroidApi.Common,
  Alcinoe.FMX.Graphics,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.UserNotifications,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Macapi.Helpers,
  iOSapi.Helpers,
  {$ENDIF}
  system.Messaging,
  Alcinoe.StringUtils,
  Alcinoe.StringList,
  Alcinoe.FMX.NotificationService,
  Alcinoe.Common,
  Alcinoe.FMX.Objects;

type
  TForm1 = class(TForm)
    MemoLog: TMemo;
    ButtonGetToken: TButton;
    TextIntro: TALText;
    EditToken: TEdit;
    ButtonSendAlertNotificationViaHttpV1: TButton;
    ButtonSendDataNotificationViaHttpV1: TButton;
    ButtonSendAlertDataNotificationViaHttpV1: TButton;
    ButtonShowNotification: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSendAlertNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendDataNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendAlertDataNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonShowNotificationClick(Sender: TObject);
  private
    { Private declarations }
    FBadge: integer;
    procedure onTokenRefresh(const aToken: String);
    procedure OnNotificationReceived(const aPayload: TALStringListW);
    procedure OnAuthorizationRefused(Sender: TObject);
    procedure OnAuthorizationGranted(Sender: TObject);
    procedure ShowLog(const aLog: String);
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const

  //The info are taken from an account (wonderful.life.photos@gmail.com) that I
  //created specifically for this demo. I retrieve it like this:
  // * go to Firebase console project settings (ALNotificationServiceDemo)
  //   https://console.firebase.google.com/project/alnotificationservicedemo/settings/general/android:com.alcinoe.alnotificationservicedemo
  // * Then click on clound messaging and click on Manage Service Accounts under Firebase Cloud Messaging API (V1)
  // * Then download the json key associated with the Service accounts for project "ALNotificationServiceDemo"
  // * find all the info below inside the json key you just downloaded
  FirebaseMessagingHttpV1ProjectID: AnsiString = 'alnotificationservicedemo';
  FirebaseMessagingHttpV1ServiceAccountEmail: AnsiString = 'firebase-adminsdk-nwe4e@alnotificationservicedemo.iam.gserviceaccount.com';
  FirebaseMessagingHttpV1ServiceAccountPrivateKey: AnsiString = '-----BEGIN PRIVATE KEY-----'+
                                                                'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDMzhD2+LmYJrxD'+
                                                                'RtUqQkHPwQRDjRC0AyD5JBJjuvtpgZoBEfYAd8CYuS2ah2dY7jGgzjqrQ7ovyv8H'+
                                                                'gs1gKLXZXC8VHxKn/VtGqpqBWm65M3CvlFN3/aFCw2PV8YC/dq4NuVr3/bowJbha'+
                                                                'hL+LUhFzXRqKQAhGajO/wZJQkjaO48F9zNbXuuyRf60Sj6aPjqOJjHJeFHEq9Kij'+
                                                                'Dam4R/IJVMMeoOXhyX26xPrqRBoPxuJwlSZg6lZBzGCXxXxMUTCHvO5Ki15TeJef'+
                                                                'Fo4EpXkZrw3VfsLF2+ucbSzByRctjqyU3m7/kOp0hxiZ5wRolSyIwX9N9NIbw4lO'+
                                                                'FJu5/jeRAgMBAAECggEABnckWZkMhFGqi9ASRTSGQ1fUAnMBxrXAZOi34nIartFw'+
                                                                'ZeZutX4plFc6Bvakx6rWEMmx47jCuB8qEHIXYg0yl7hnpNP2HHLMu8XS/WIJpSgu'+
                                                                'BTz0k24PO+zeyibG3qGRaROvRMSNOoYQgADcGPTv7Yw44s+t8zJkjI21GgiX+iVM'+
                                                                'xW7bvii0sC7v+QSmfKpdmHtjc0QcdGz02dQ2m0uaNyGSYQzqn21D+vS7vbCUpr5X'+
                                                                'QdQ+TEp4lDsa3Wf2b4avDEVnMFrag1bPO9mjo8M+9NkpMQbA/SvFv78He4DZpljJ'+
                                                                'Rp6I+cJ6xWyxdNR4PPVV89Dsk34XQS/TytMlSZipKQKBgQDo3iuOAKshh/m8uiwm'+
                                                                'uT2NuvKuvqbuDgwTgKd7Nr9tKTbJcfHfBaiPRdrgYDu30YOHsxFfL80HUIv15CDe'+
                                                                'DNeufp8UVEqXl6EzOAgcXm3BWFqOjkgFWpcgQeBevYlJYhdzLUF6WeO6gBpYhr2F'+
                                                                'mnIxHtsn+y7jRSDyoLBmPF8tEwKBgQDhJkGjBP3B+R+wxcjphoZZgobraFFm+DKk'+
                                                                '5eJKu9f5O3BJfIk57pFnGzfhrjEKKKe3Ub7i0f1SV7jkCROSxRtUXB8zWi6adQkK'+
                                                                'fPN3CGCkU//yQz0PUcwX7XPzS/PvMMkvQ690LAgxHTBwEKEHHxF0zZcw6dRUNkA+'+
                                                                'NCA61+NRSwKBgHqZvcSdhXu54zoBqo0YqdecvXhS2AAtVR5Pdd3hDRejwzx7ySPq'+
                                                                'DryfwoSvJcG5hM5E8Lh0qjVXqthiqws9J7Cu6YICfuMg6bXVoi+NZ6uLoOG89x4a'+
                                                                'e8Z36HD4yPbhtgblpLuN59+g4j4Jcm6MyeIipK+AB0eQdbBH+ZdZ4aIRAoGAQbeB'+
                                                                '5wxY6RpHFrjUSN1cL5o6uVGMmQqO9bSn1Xp4sqWXw8tW3pL02+yE5hmK9NUjBw4U'+
                                                                'Mm5qi3NRYzYHAYsIzBnLfZiwC6NnjSklgzPtyWk/Rr47f1I3yTAk7PnZbJKH1oTi'+
                                                                'HH2Rsow7jCo+Zi66UKaFn+BQengPTli2o5ZKInsCgYEAzuEA7iz6mxj4Tl14/EHl'+
                                                                'lXPOUUZ8a1T+hkiV4IHg5sacRoyIF8LtlQdx4LL8tNiCj0p1Y6vVzmFDDrOdGww2'+
                                                                'TENN4jneIB6Y2Gzh7KLfiD543aLOd5RJc6hNGbBHJ5jd5E8VxnswUd0L6v0LrG9o'+
                                                                'RVNesy22JS69CCX39m8T2MU='+
                                                                '-----END PRIVATE KEY-----';

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}
  //----
  FBadge := 0;
  TALNotificationService.Instance.OnTokenRefresh := onTokenRefresh;
  TALNotificationService.Instance.OnNotificationReceived := OnNotificationReceived;
  TALNotificationService.Instance.OnAuthorizationRefused := OnAuthorizationRefused;
  TALNotificationService.Instance.OnAuthorizationGranted := OnAuthorizationGranted;
  TALNotificationService.Instance.CreateNotificationChannel(
    TALNotificationChannel.create('demo'{AID})
      .SetImportance(TALNotificationImportance.High));
  TALNotificationService.Instance.setBadgeCount(0);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}
end;

{*****************************************}
procedure TForm1.FormShow(Sender: TObject);
begin
  TALNotificationService.Instance.RequestNotificationPermission;
end;

{************************************}
{$IF Defined(IOS) or Defined(ANDROID)}
procedure TForm1.ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
begin
  var LReport := TgoExceptionReportMessage(M).Report;
  allog('ERROR', LReport.Report, TalLogType.error);
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

{****************************************************}
procedure TForm1.onTokenRefresh(const aToken: String);
begin
  ALLog('onTokenRefresh', 'Token: ' + aToken);
  EditToken.text := aToken;
  ShowLog('onTokenRefresh: ' + aToken);
end;

{**********************************************************************}
procedure TForm1.OnNotificationReceived(const aPayload: TALStringListW);
begin
  ALLog('OnNotificationReceived', aPayload.Text);
  ShowLog('OnNotificationReceived'#13#10+aPayload.Text);
end;

{*******************************************************}
procedure TForm1.OnAuthorizationRefused(Sender: TObject);
begin
  ALLog('OnAuthorizationRefused');
  ShowLog('OnAuthorizationRefused');
end;

{*******************************************************}
procedure TForm1.OnAuthorizationGranted(Sender: TObject);
begin
  ALLog('OnAuthorizationGranted');
  ShowLog('OnAuthorizationGranted');
end;

{*******************************************}
procedure TForm1.ShowLog(const aLog: String);
begin
  MemoLog.text := ALTrim(aLog) + #13#10 +
                  #13#10 +
                  '------------------------'#13#10 +
                  #13#10 +
                  MemoLog.text;
end;

{*************************************************************************}
procedure TForm1.ButtonSendDataNotificationViaHttpV1Click(Sender: TObject);
begin
  //https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
  {$IF Defined(MSWindows)}
  var LHTTP := TNetHTTPClient.Create(nil);
  try
    var LStream := TStringStream.Create(
                     '{'+
                       '"message":{'+
                         '"token":"'+EditToken.Text+'",'+
                         '"data":{'+
                           '"one_sample_key": "one_sample_value"'+
                         '}'+
                       '}'+
                     '}',
                     TEncoding.UTF8);
    try
      var LContentType := TNameValuePair.create('Content-Type', 'application/json');
      var LAccessToken := ALGenerateGoogleOAuth2AccessToken(
                            FirebaseMessagingHttpV1ServiceAccountEmail, // const aServiceAccountEmail: ansiString;
                            'https://www.googleapis.com/auth/firebase.messaging', // const aScope: ansiString;
                            FirebaseMessagingHttpV1ServiceAccountPrivateKey); // const aPrivateKey: ansiString);
      ShowLog(
        LHTTP.Post(
          'https://fcm.googleapis.com/v1/projects/'+string(FirebaseMessagingHttpV1ProjectID)+'/messages:send',
          LStream,
          nil,
          [TNameValuePair.Create('Authorization','Bearer '+String(LaccessToken)),
           TNameValuePair.create('Content-Type', 'application/json')]).ContentAsString);
    finally
      ALFreeAndNil(LStream);
    end;
  finally
    ALFreeAndNil(LHTTP);
  end;
  {$ELSE}
  TDialogService.ShowMessage('You can send messages using HTTP V1 protocol only from Microsoft Windows operating system');
  {$ENDIF}
end;

{**************************************************************************}
procedure TForm1.ButtonSendAlertNotificationViaHttpV1Click(Sender: TObject);
begin
  //https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
  {$IF Defined(MSWindows)}
  var LHTTP := TNetHTTPClient.Create(nil);
  try
    inc(FBadge);
    var LStream := TStringStream.Create(
                     '{'+
                       '"message":{'+
                         '"token":"'+EditToken.Text+'",'+
                         '"notification":{'+
                           '"body":"This is an FCM notification message!",'+
                           '"title":"FCM Message",'+
                           '"image":"https://i.stack.imgur.com/EwPfY.jpg?s=64&g=1"'+
                         '},'+
                         '"android":{'+
                           '"notification":{'+
                             '"channel_id":"demo",'+
                             '"notification_priority":"PRIORITY_HIGH",' +
                             '"visibility":"PRIVATE",' +
                             '"sound":"default"'+
                           '}'+
                         '},'+
                         '"apns":{'+
                           '"payload":{'+
                             '"aps":{'+
                               '"badge": '+ALIntToStrW(FBadge)+
                             '}'+
                           '}'+
                         '}'+
                       '}'+
                     '}',
                     TEncoding.UTF8);
    try
      var LContentType := TNameValuePair.create('Content-Type', 'application/json');
      var LAccessToken := ALGenerateGoogleOAuth2AccessToken(
                            FirebaseMessagingHttpV1ServiceAccountEmail, // const aServiceAccountEmail: ansiString;
                            'https://www.googleapis.com/auth/firebase.messaging', // const aScope: ansiString;
                            FirebaseMessagingHttpV1ServiceAccountPrivateKey); // const aPrivateKey: ansiString);
      ShowLog(
        LHTTP.Post(
          'https://fcm.googleapis.com/v1/projects/'+string(FirebaseMessagingHttpV1ProjectID)+'/messages:send',
          LStream,
          nil,
          [TNameValuePair.Create('Authorization','Bearer '+String(LaccessToken)),
           TNameValuePair.create('Content-Type', 'application/json')]).ContentAsString);
    finally
      ALFreeAndNil(LStream);
    end;
  finally
    ALFreeAndNil(LHTTP);
  end;
  {$ELSE}
  TDialogService.ShowMessage('You can send messages using HTTP V1 protocol only from Microsoft Windows operating system');
  {$ENDIF}
end;

{******************************************************************************}
procedure TForm1.ButtonSendAlertDataNotificationViaHttpV1Click(Sender: TObject);
begin
  //https://firebase.google.com/docs/reference/fcm/rest/v1/projects.messages
  {$IF Defined(MSWindows)}
  var LHTTP := TNetHTTPClient.Create(nil);
  try
    inc(FBadge);
    var LStream := TStringStream.Create(
                     '{'+
                       '"message":{'+
                         '"token":"'+EditToken.Text+'",'+
                         '"notification":{'+
                           '"body":"This is an FCM notification message!",'+
                           '"title":"FCM Message",'+
                           '"image":"https://i.stack.imgur.com/EwPfY.jpg?s=64&g=1"'+
                         '},'+
                         '"android":{'+
                           '"notification":{'+
                             '"channel_id":"demo",'+
                             '"notification_priority":"PRIORITY_HIGH",' +
                             '"visibility":"PRIVATE",' +
                             '"sound":"default"'+
                           '}'+
                         '},'+
                         '"apns":{'+
                           '"payload":{'+
                             '"aps":{'+
                               '"badge": '+ALIntToStrW(FBadge)+
                             '}'+
                           '}'+
                         '},'+
                         '"data":{'+
                           '"one_sample_key": "one_sample_value"'+
                         '}'+
                       '}'+
                     '}',
                     TEncoding.UTF8);
    try
      var LContentType := TNameValuePair.create('Content-Type', 'application/json');
      var LAccessToken := ALGenerateGoogleOAuth2AccessToken(
                            FirebaseMessagingHttpV1ServiceAccountEmail, // const aServiceAccountEmail: ansiString;
                            'https://www.googleapis.com/auth/firebase.messaging', // const aScope: ansiString;
                            FirebaseMessagingHttpV1ServiceAccountPrivateKey); // const aPrivateKey: ansiString);
      ShowLog(
        LHTTP.Post(
          'https://fcm.googleapis.com/v1/projects/'+string(FirebaseMessagingHttpV1ProjectID)+'/messages:send',
          LStream,
          nil,
          [TNameValuePair.Create('Authorization','Bearer '+String(LaccessToken)),
           TNameValuePair.create('Content-Type', 'application/json')]).ContentAsString);
    finally
      ALFreeAndNil(LStream);
    end;
  finally
    ALFreeAndNil(LHTTP);
  end;
  {$ELSE}
  TDialogService.ShowMessage('You can send messages using HTTP V1 protocol only from Microsoft Windows operating system');
  {$ENDIF}
end;

{************************************************************}
procedure TForm1.ButtonShowNotificationClick(Sender: TObject);
begin
  TALNotificationService.Instance.ShowNotification(
    TALNotification.Create(ALRandomStrW(25){ATag})
      .SetChannelId('demo')
      .SetTitle('Notification Title')
      .SetText('This is a sample notification alert!')
      .setTicker('Sample ticker')
      .SetLargeIconUrl('https://i.stack.imgur.com/EwPfY.jpg?s=64&g=1')
      .setSmallIconResName('notification_icon')
      .AddPayload('one_sample_key'{aName}, 'one_sample_value'{aValue})
      .AddPayload('another_sample_key'{aName}, 'another_sample_value'{aValue}));
end;

{****************************************************}
procedure TForm1.ButtonGetTokenClick(Sender: TObject);
begin
  TALNotificationService.Instance.getToken;
end;

initialization

  {$IF defined(IOS)}
  {$IFDEF DEBUG}
  //https://stackoverflow.com/questions/74777909/nslog-dont-write-all-desired-lines-when-the-app-is-starting
  //when I click on a notification when the app is not running, then i miss a lot of log in the console.app.
  //I do not know why but adding a sleep of 2s here seam to correct this problem
  sleep(2000);
  {$ENDIF}
  {$ENDIF}

  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
