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
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.Controls;

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
    ButtonDeleteToken: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonGetTokenClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSendAlertNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendDataNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonSendAlertDataNotificationViaHttpV1Click(Sender: TObject);
    procedure ButtonShowNotificationClick(Sender: TObject);
    procedure ButtonDeleteTokenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBadge: integer;
    procedure NotificationReceivedMessageHandler(const Sender: TObject; const AMessage: TMessage);
    procedure GetTokenMessageHandler(const Sender: TObject; const AMessage: TMessage);
    procedure DeleteTokenMessageHandler(const Sender: TObject; const AMessage: TMessage);
    procedure TokenRefreshMessageHandler(const Sender: TObject; const AMessage: TMessage);
    procedure NotificationPermissionResultMessageHandler(const Sender: TObject; const AMessage: TMessage);
    procedure ShowLog(const aLog: String);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const

  // The info are taken from an account (st...e@magicfoundation.io) that I
  // created specifically for this demo. I retrieve it like this:
  //  * go to Firebase console project settings (ALFmxNotificationServiceDemo)
  //    https://console.firebase.google.com/project/alfmxnotificationservicedemo/settings/general/android:io.magicfoundation.alcinoe.alfmxnotificationservicedemo
  //  * Then click on clound messaging and click on Manage Service Accounts under Firebase Cloud Messaging API (V1)
  //  * Then download the json key associated with the Service accounts for project "ALFmxNotificationServiceDemo"
  //  * find all the info below inside the json key you just downloaded
  FirebaseMessagingHttpV1ProjectID: AnsiString = 'alfmxnotificationservicedemo';
  FirebaseMessagingHttpV1ServiceAccountEmail: AnsiString = 'firebase-adminsdk-fbsvc@alfmxnotificationservicedemo.iam.gserviceaccount.com';
  FirebaseMessagingHttpV1ServiceAccountPrivateKey: AnsiString =
    '-----BEGIN PRIVATE KEY-----'+
    'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCx3yog0e8Te7T4'+
    'cYip3yggOnfsKJCZsEb1mhHoZF/rtCogqFSCRbFNQ/QkpY3GrPf8gB/Ge0rBGiBN'+
    'hqKof8mPSa3h/CmmfAyVK/wothnnBU99KFcPW06ythe6m0pIYFGhBLmdBrJ1Zy0E'+
    'hPs8UikFHkylClzilD3tEtPrOvcBXgaTFVPwmHGYGtRVNRF5YdElbHQYH2XocIPo'+
    'Z2Do83KwzsJ4aXI/E0MYOa1wYgWRJUk3sUEzjtEGv89ARvaoWuQN6qzODabbFXnJ'+
    'AsaaBvdtnpMurscDRN9OnymCqyyfM5WcXED4C89Y/mlYDJ67zmUuL6KOtCOsd5YF'+
    '9UhnR39RAgMBAAECggEAULraExVVbkFabNySy47WVnJTFaN/pdGlEzm/YPuVsZQO'+
    '/s2HtazOASSTfcKKK5872qWIRnyU/DnQaX9u+g0tvVQSkJvzh3WL/HR/OTcVYI4/'+
    'eq3Bfd3SRThyYqayu1DEG9HaMNPmUXTnsMJuiP09Uu5imRGwAMKJrL074+raSwrP'+
    'X378Xvk1bLxc/PzLytKBzXRgRdEB7Utz20t5SgAMq1RPsmTKcVMGf8SQu5HwBZmg'+
    'TBZj7Vn4wB/PDamearB9/bU53N6Nmg8bd1rRlLyzPj3b8s2j05tYAbvmnfO1gR9g'+
    'gbzVVhS/SsLLno4GlOJnnoq/wvV05iaKTPZJTjkZdQKBgQD1Ds/tC/PBwpuJQH1O'+
    'yeTJ98bLb7dR3zN41wH2CLUKTQv0aPkgKK6s49M0+8Oio9Mt9IVebg281M3GrLgm'+
    'NEDvA/Dam/hvRN3j1tK30XkcUiMt9u+af1K13ihFgdaZVHSMm0aqhAHAvuak1Yt6'+
    'NumGNXL42IsGnYHT9hLef11GEwKBgQC50F3Yco8vNeIUuCX1uarTjpJkKDpLQLIF'+
    'dW0xVmY7m1vqBEfR5sTpfdAlMfkPLmfxKXMG5SjlW5ZhqHl1/PICNIZD9JPMHL9m'+
    'c5NBtjp+XUTlv/20Bb9epNpYHVl21yLkDjtr03nVvAcIiE7sML+eJeRcr54nhMmj'+
    'Jt4H2z4hiwKBgA4PO36uIN4M/D9zo7zReH7d6FnjnvDjjWBxdXtYJIriwLzVCPX/'+
    'X4YrzAJOL3s1Svhn/v7b8Y3T57puTmJivRb5cugX6rj3ioN3378MFZa6X6pTcIu8'+
    'olIs3MKgIF/LqKQohHVj/XXBfhoW5lmsrNk2V9JPGirW/ovaolqBlChhAoGBAJV4'+
    'BWWNkZxabchY8UAe4ElZkGy851eNbSZy9SkQ5R/hsygl1VgFmDRynjKtZjlODRKz'+
    'UvIi+Ki26dsY2MuOc2ZOtgqH/TPdlCFnqvbbDWaExF1D+WMMlIofdfwXb9Xm7Qwi'+
    'Xq436PODUu0MKOBZvmoK9Q2Rv1juufxh0YzkUbBNAoGAO/HLbDQDqzaRltO/P6bq'+
    'aFwqF/1WkMWk3TooCIW28Cbu54CFvm2wQ/Vd3UUoSTebyqW8MNjyvzni+u20wtb5'+
    'bBcafbSpanrof4a6NdbOdT/Ee7F8Xmm0U4JuKW8XRrl3grP2huKkaOXyuMtuhQ2w'+
    'Wy+2QvL7BOj5OGCS4mODqRg='+
    '-----END PRIVATE KEY-----';

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  TALErrorReporting.Instance;
  FBadge := 0;
  TMessageManager.DefaultManager.SubscribeToMessage(TALNotificationService.TNotificationReceivedMessage, NotificationReceivedMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TALNotificationService.TGetTokenMessage, GetTokenMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TALNotificationService.TDeleteTokenMessage, DeleteTokenMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TALNotificationService.TTokenRefreshMessage, TokenRefreshMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TALNotificationService.TNotificationPermissionResultMessage, NotificationPermissionResultMessageHandler);
  TALNotificationService.Instance.CreateNotificationChannel(
    TALNotificationChannel.create('demo'{AID})
      .SetImportance(TALNotificationImportance.High));
  TALNotificationService.Instance.setBadgeCount(0);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TALNotificationService.TNotificationReceivedMessage, NotificationReceivedMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TALNotificationService.TGetTokenMessage, GetTokenMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TALNotificationService.TDeleteTokenMessage, DeleteTokenMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TALNotificationService.TTokenRefreshMessage, TokenRefreshMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TALNotificationService.TNotificationPermissionResultMessage, NotificationPermissionResultMessageHandler);
end;

{*****************************************}
procedure TForm1.FormShow(Sender: TObject);
begin
  TALNotificationService.Instance.RequestNotificationPermission;
end;

{***************************************************************************************}
procedure TForm1.GetTokenMessageHandler(const Sender: TObject; const AMessage: TMessage);
begin
  var LGetTokenMessage := TALNotificationService.TGetTokenMessage(AMessage);
  ALLog('GetToken', 'Token: ' + LGetTokenMessage.Token + ' | ErrorMessage: ' + LGetTokenMessage.ErrorMessage);
  if LGetTokenMessage.Token = '' then ShowLog('GetToken: ' + LGetTokenMessage.ErrorMessage)
  else ShowLog('GetToken: Success');
end;

{******************************************************************************************}
procedure TForm1.DeleteTokenMessageHandler(const Sender: TObject; const AMessage: TMessage);
begin
  var LDeleteTokenMessage := TALNotificationService.TDeleteTokenMessage(AMessage);
  ALLog('DeleteToken', 'IsSuccessful: ' + ALBoolToStrW(LDeleteTokenMessage.IsSuccessful) + ' | ErrorMessage: ' + LDeleteTokenMessage.ErrorMessage);
  if not LDeleteTokenMessage.IsSuccessful then ShowLog('DeleteToken: ' + LDeleteTokenMessage.ErrorMessage)
  else ShowLog('DeleteToken: Success');
end;

{*******************************************************************************************}
procedure TForm1.TokenRefreshMessageHandler(const Sender: TObject; const AMessage: TMessage);
begin
  var LTokenRefreshMessage := TALNotificationService.TTokenRefreshMessage(AMessage);
  ALLog('TokenRefresh', 'Token: ' + LTokenRefreshMessage.Token);
  EditToken.text := LTokenRefreshMessage.Token;
  ShowLog('TokenRefresh: ' + LTokenRefreshMessage.Token);
end;

{***************************************************************************************************}
procedure TForm1.NotificationReceivedMessageHandler(const Sender: TObject; const AMessage: TMessage);
begin
  var LNotificationReceivedMessage := TALNotificationService.TNotificationReceivedMessage(AMessage);
  ALLog('NotificationReceived', LNotificationReceivedMessage.Payload.Text);
  ShowLog('NotificationReceived'#13#10+LNotificationReceivedMessage.Payload.Text);
end;

{***********************************************************************************************************}
procedure TForm1.NotificationPermissionResultMessageHandler(const Sender: TObject; const AMessage: TMessage);
begin
  var LNotificationPermissionResultMessage := TALNotificationService.TNotificationPermissionResultMessage(AMessage);
  if not LNotificationPermissionResultMessage.Granted then begin
    ALLog('AuthorizationRefused');
    ShowLog('AuthorizationRefused');
  end
  else begin
    ALLog('AuthorizationGranted');
    ShowLog('AuthorizationGranted');
  end;
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
                           '"sample_key": "sample_value"'+
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
                           '"sample_key": "sample_value"'+
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
      .AddPayload('sample_key'{aName}, 'sample_value'{aValue})
      .AddPayload('another_sample_key'{aName}, 'another_sample_value'{aValue}));
end;

{****************************************************}
procedure TForm1.ButtonGetTokenClick(Sender: TObject);
begin
  TALNotificationService.Instance.getToken;
end;

{*******************************************************}
procedure TForm1.ButtonDeleteTokenClick(Sender: TObject);
begin
  TALNotificationService.Instance.DeleteToken;
end;

initialization

  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.