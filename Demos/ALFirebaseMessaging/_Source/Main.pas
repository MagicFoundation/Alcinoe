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
  Alcinoe.FMX.FirebaseMessaging,
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure ButtonShowSampleNotificationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSendAlertNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendDataNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendAlertDataNotificationViaHttpV1Click(Sender: TObject);
  private
    { Private declarations }
    FBadge: integer;
    FFirebaseMessaging: TALFirebaseMessaging;
    procedure onFCMTokenRefresh(const aToken: String);
    procedure onFCMMessageReceived(const aPayload: TALStringListW);
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
  // * go to Firebase console project settings (ALFirebaseMessagingApp)
  //   https://console.firebase.google.com/project/alfirebasemessagingapp/settings/cloudmessaging/android:com.ALFirebaseMessaging.app
  // * Then click on clound messaging and click on Manage Service Accounts under Firebase Cloud Messaging API (V1)
  // * Then download the json key associated with the Service accounts for project "ALFirebaseMessagingApp"
  // * find all the info below inside the json key you just downloaded
  FirebaseMessagingHttpV1ProjectID: AnsiString = 'alfirebasemessagingapp';
  FirebaseMessagingHttpV1ServiceAccountEmail: AnsiString = 'firebase-adminsdk-s54m2@alfirebasemessagingapp.iam.gserviceaccount.com';
  FirebaseMessagingHttpV1ServiceAccountPrivateKey: AnsiString = '-----BEGIN PRIVATE KEY-----'+
                                                                'MIIEvAIBADANBgkqhkiG9w0BAQEFAASCBKYwggSiAgEAAoIBAQDfUXJ03IVm/Vhs'+
                                                                'fdnuJdjvyXH9EvOwots8HQR+kaeHhD8h+1Gu3DNncMfkshKbRqiYjWDjqbiw6jPD'+
                                                                'UNVKDzhK20HxqiLFbqkV8G7PgxANSDw7+iuu4vRMTeKaco6FDs+OCuODFd48J2sd'+
                                                                'fd7HaAILbedgmbOHBEoAzeNl6ceeOkdk8+ap4ne/JK6gC4ENZ30oV5T5Ob1VpCBP'+
                                                                'K/M4+4hsS5r+Uf+ZU3jpjHQxlMbSo/rUGKnz+WuQoBCcfrYhXDIE89kMEjEqQapz'+
                                                                '+wTmn5EK7CVg5SBjwheriAy4F0HBW4kB91Ef01aV/f0OB2SVXoIoiziNayx/nHZ4'+
                                                                'QO8xMSWrAgMBAAECgf8NJX4SV17CXU7Aym0C/ruGNKFJ2xooD3eE30Pd1TVduvOs'+
                                                                '3t3Y4Al+KtknCVVmNQe15FhMR3bdw3WFzc5ivHlnpmkKja1Vkk87pHfaA/R0NTs1'+
                                                                'dH/FjPvExtkUVyjZu5vhu8l7ke1YyUGXsX0yzexGsoHTfW/kdBWZGbhZRmSL3O5B'+
                                                                'vayCZASX9Iy26H6f/UvP6fnH/TDDUGz8HWSOH8Ne7Xvaa3kukG5Uod9lrykhX3ak'+
                                                                'Xx6cXbcohE0+rhoNjR7ks1V736fjuRxOnj0btF709qvsKVthaR4lxrmoGswMCSS8'+
                                                                'h78ZJOa4Fvle3hPxL1eaE3SPKSZdqAzxwlKerQkCgYEA+xfaQtgjh3/65h3OMiZ6'+
                                                                '/VP+oupuNHcHq+O2Sf+uMh2peuk/Addf8lpFhR5AOK368BiAOljuIcMqVIJV2kJU'+
                                                                'vWbAuakxsxSkq/Rz89SjR9YmbKeiEj+ZYUnMCl2lSU8LDuz4J5hEQ0Vb/BEf/3iZ'+
                                                                'fU6KYqc35sGlqVWkjhyxHi0CgYEA466k3jf/q+dNfyhmdmtUWMcNsLlJWslNVuas'+
                                                                'nub5CGJYPOHwZZS4xW16sQIs3tL3WN1cD3XJIz+uW5847BwBZHav7o1FEsfpc41W'+
                                                                'fLlH3i0F5bTY/OFg1rJkmp40qVWBG1msfZn4OIR5Mbs6C4NRO71bUcb76XV77B0G'+
                                                                'lLZmkjcCgYEA2+QSpzFDRBmm7rkxZyfd01YojCHDKz0GQdjkPb+knIzvbA0xuXoU'+
                                                                's1esxBwu37Q1Kug5+18ABB83RdTyPHaUYV3H74+lT5AHefNVTDZuW63F7qeLPnHl'+
                                                                '75ZCEt3Zru6C36pU08/8D/GA0alpnT/PIzaR6D1KrlHtsvKmbjHgRm0CgYBdqk43'+
                                                                'ARCUVq6h/ivQ0ay8bP8r0b3ktGW1t2YSZPDUSykDuutbzsgIqFZOFZgB/wY0r7Qc'+
                                                                'xBcAAkWneaRANfE4tD8CQ2nEJSvcFqwa2VpAg4MmkbSmq81b5b3PggAmHX/kkYqN'+
                                                                'jVb5YT4+gEiLzfUQP4Ee7l5aF7PoWbSIX1VpFwKBgQDwjfI8sX7LwxvKQuNu5qcx'+
                                                                'mMfLnDfRgssLE3tMfjUX9sntCLpczRtjk+x7ImM8vy2C3S1ukvJhp77ZYn6uiHVq'+
                                                                'vH1tQxnf7TvB1u39SY8EK0dKOQbBVJi8Zk8AGCP9rt3BLsM+BIr9qv9i9iteguDw'+
                                                                'onhj6y2OHcmbEvc3pbSMdQ=='+
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
  FFirebaseMessaging := TALFirebaseMessaging.create;
  FFirebaseMessaging.OnTokenRefresh := onFCMTokenRefresh;
  FFirebaseMessaging.OnMessageReceived := OnFCMMessageReceived;
  FFirebaseMessaging.OnAuthorizationRefused := OnAuthorizationRefused;
  FFirebaseMessaging.OnAuthorizationGranted := OnAuthorizationGranted;
  FFirebaseMessaging.CreateNotificationChannel(
    'demo', // const AID: String;
    'demo', // const AName: String;
    TALFirebaseMessaging.TNotificationChannelImportance.High); //const AImportance: TNotificationChannelImportance = TNotificationChannelImportance.Default;
  FFirebaseMessaging.setBadgeCount(0);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  AlFreeAndNil(FFirebaseMessaging);
  {$IF Defined(IOS) or Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}
end;

{*****************************************}
procedure TForm1.FormShow(Sender: TObject);
begin
  FFirebaseMessaging.RequestNotificationPermission;
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

{*******************************************************}
procedure TForm1.onFCMTokenRefresh(const aToken: String);
begin
  ALLog('onFCMTokenRefresh', 'Token: ' + aToken);
  EditToken.text := aToken;
  ShowLog('onFCMTokenRefresh: ' + aToken);
end;

{********************************************************************}
procedure TForm1.onFCMMessageReceived(const aPayload: TALStringListW);
begin
  ALLog('onFCMMessageReceived', aPayload.Text);
  ShowLog('onFCMMessageReceived'#13#10+aPayload.Text);
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

{******************************************************************}
procedure TForm1.ButtonShowSampleNotificationClick(Sender: TObject);
begin
//toto
end;

{****************************************************}
procedure TForm1.ButtonGetTokenClick(Sender: TObject);
begin
  FFirebaseMessaging.getToken;
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
