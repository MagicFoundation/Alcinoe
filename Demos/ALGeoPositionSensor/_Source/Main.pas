unit Main;

interface

uses
  System.SysUtils,
  system.AnsiStrings,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  system.Messaging,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.DialogService,
  FMX.Dialogs,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Memo.Types,
  Alcinoe.FMX.ErrorReporting,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.GeoPosition.Sensor,
  Alcinoe.FMX.Objects,
  Alcinoe.Common,
  Alcinoe.StringUtils,
  Alcinoe.StringList;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FGeoPositionSensor: TALGeoPositionSensor;
    procedure OnGeoPositionSensorGeoPositionUpdate(
                const Sender: TObject;
                const ALatitude: Double;
                const ALongitude: Double;
                const AAltitude: Double;
                const AAccuracy: Double;
                Const ADateTime: TdateTime);
    procedure OnGeoPositionSensorActivateGpsAndGrantGeoPositionAccessResult(Sender: TObject);
    Procedure OnGeoPositionSensorShowRequestPermissionRationale(
                const Sender: TObject;
                const AToActivateGPS: Boolean;
                const AToRequestCoarseGeoPositionPermission: Boolean;
                const AToRequestPreciseGeoPositionPermission: Boolean;
                const AToRequestAlwaysAuthorization: Boolean;
                const AIsForced: Boolean; // when true it's mean that the user denied the previous permission request with checking "Never ask again option"
                const ACanRequestPermissionProc: TProc; // the procedure to launch when the user response positivelly to the rationale
                const ACanNotRequestPermissionProc: TProc); // the procedure to launch when the user response negativelly to the rationale
  public
  end;

var
  Form1: TForm1;

implementation

{$IF defined(IOS)}
uses
 FMX.Helpers.iOS,
 iOSapi.UserNotifications;
{$ENDIF}


{$R *.fmx}

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin

  TALErrorReporting.Instance;

  FGeoPositionSensor := TALGeoPositionSensor.Create(true{AUseGooglePlayServicesIfAvailable});
  FGeoPositionSensor.OnGeoPositionUpdate := OnGeoPositionSensorGeoPositionUpdate;
  FGeoPositionSensor.OnAuthorizationStatus := OnGeoPositionSensorActivateGpsAndGrantGeoPositionAccessResult;
  FGeoPositionSensor.OnShowRequestPermissionRationale := OnGeoPositionSensorShowRequestPermissionRationale;

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  //stupidely on IOS to see the app in the settings app i must do stuff like this :(
  //https://stackoverflow.com/questions/75964728/my-app-is-not-appearing-in-the-settings-app-after-i-deploy-it-in-my-iphone
  var LOptions := UNAuthorizationOptionSound or
                  UNAuthorizationOptionAlert or
                  UNAuthorizationOptionBadge;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.requestAuthorizationWithOptions(LOptions{options}, nil{completionHandler});
  SharedApplication.registerForRemoteNotifications;
  {$ENDIF}
  {$ENDREGION}

end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FGeoPositionSensor);
end;

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
begin
  FGeoPositionSensor.ActivateGpsAndGrantGeoPositionAccess(
    true, // const ACoarseGeoPosition: boolean = True;
    false, // const APreciseGeoPosition: boolean = True;
    false); // const AAlwaysAuthorization: boolean = False)
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
begin
  FGeoPositionSensor.StopGeoPositionUpdates;
end;

{*********************************************}
procedure TForm1.Button4Click(Sender: TObject);
begin
  FGeoPositionSensor.ActivateGpsAndGrantGeoPositionAccess(
    true, // const ACoarseGeoPosition: boolean = True;
    true, // const APreciseGeoPosition: boolean = True;
    true); // const AAlwaysAuthorization: boolean = False)
end;

{*********************************************}
procedure TForm1.Button5Click(Sender: TObject);
begin
  FGeoPositionSensor.ActivateGpsAndGrantGeoPositionAccess(
    false, // const ACoarseGeoPosition: boolean = True;
    true, // const APreciseGeoPosition: boolean = True;
    false); // const AAlwaysAuthorization: boolean = False)
end;

{*********************************************}
procedure TForm1.Button6Click(Sender: TObject);
begin
  FGeoPositionSensor.StartGeoPositionUpdates(1{aMinDistance});
end;

{*********************************************}
procedure TForm1.Button7Click(Sender: TObject);
begin
  var LRestricted: boolean;
  var LCoarseGeoPositionGranted: Boolean;
  var LPreciseGeoPositionGranted: boolean;
  var LAuthorizedAlways: Boolean;
  FGeoPositionSensor.GetPermissionsGranted(
    LRestricted,
    LCoarseGeoPositionGranted,
    LPreciseGeoPositionGranted,
    LAuthorizedAlways);
  Memo1.Lines.add('PermissionsGranted');
  Memo1.Lines.add('  GpsEnabled: ' + ALBoolToStrW(FGeoPositionSensor.IsGpsEnabled));
  Memo1.Lines.add('  Restricted: ' + ALBoolToStrW(LRestricted));
  Memo1.Lines.add('  CoarseGeoPositionGranted: ' + ALBoolToStrW(LCoarseGeoPositionGranted));
  Memo1.Lines.add('  PreciseGeoPositionGranted: ' + ALBoolToStrW(LPreciseGeoPositionGranted));
  Memo1.Lines.add('  AuthorizedAlways: ' + ALBoolToStrW(LAuthorizedAlways));
  Memo1.Lines.add('');
  Memo1.Lines.add('************');
  Memo1.Lines.add('');
  Memo1.SelStart := Length(Memo1.Text);
  Memo1.SelLength := 0;
end;

{**********************************************************************************************}
procedure TForm1.OnGeoPositionSensorActivateGpsAndGrantGeoPositionAccessResult(Sender: TObject);
begin
  var LRestricted: boolean;
  var LCoarseGeoPositionGranted: Boolean;
  var LPreciseGeoPositionGranted: boolean;
  var LAuthorizedAlways: Boolean;
  FGeoPositionSensor.GetPermissionsGranted(
    LRestricted,
    LCoarseGeoPositionGranted,
    LPreciseGeoPositionGranted,
    LAuthorizedAlways);
  Memo1.Lines.add('OnAuthorizationStatus');
  Memo1.Lines.add('  GpsEnabled: ' + ALBoolToStrW(FGeoPositionSensor.IsGpsEnabled));
  Memo1.Lines.add('  Restricted: ' + ALBoolToStrW(LRestricted));
  Memo1.Lines.add('  CoarseGeoPositionGranted: ' + ALBoolToStrW(LCoarseGeoPositionGranted));
  Memo1.Lines.add('  PreciseGeoPositionGranted: ' + ALBoolToStrW(LPreciseGeoPositionGranted));
  Memo1.Lines.add('  AuthorizedAlways: ' + ALBoolToStrW(LAuthorizedAlways));
  Memo1.Lines.add('');
  Memo1.Lines.add('************');
  Memo1.Lines.add('');
  Memo1.SelStart := Length(Memo1.Text);
  Memo1.SelLength := 0;
end;

{****************************************************}
procedure TForm1.OnGeoPositionSensorGeoPositionUpdate(
            const Sender: TObject;
            const ALatitude: Double;
            const ALongitude: Double;
            const AAltitude: Double;
            const AAccuracy: Double;
            Const ADateTime: TdateTime);
begin
  Memo1.Lines.add('OnGeoPositionUpdate');
  Memo1.Lines.add('  Latitude: ' + ALFloatToStrW(ALatitude, ALDefaultFormatSettingsW));
  Memo1.Lines.add('  Longitude: ' + ALFloatToStrW(ALongitude, ALDefaultFormatSettingsW));
  Memo1.Lines.add('  Altitude: ' + ALFloatToStrW(AAltitude, ALDefaultFormatSettingsW));
  Memo1.Lines.add('  Accuracy: ' + ALFloatToStrW(AAccuracy, ALDefaultFormatSettingsW));
  Memo1.Lines.add('  DateTime: ' + ALDateTimeToStrW(ADateTime, ALDefaultFormatSettingsW));
  Memo1.Lines.add('');
  Memo1.Lines.add('************');
  Memo1.Lines.add('');
  Memo1.SelStart := Length(Memo1.Text);
  Memo1.SelLength := 0;
end;

{*****************************************************************}
procedure TForm1.OnGeoPositionSensorShowRequestPermissionRationale(
            const Sender: TObject;
            const AToActivateGPS: Boolean;
            const AToRequestCoarseGeoPositionPermission: Boolean;
            const AToRequestPreciseGeoPositionPermission: Boolean;
            const AToRequestAlwaysAuthorization: Boolean;
            const AIsForced: Boolean; // when true it's mean that the user denied the previous permission request with checking "Never ask again option"
            const ACanRequestPermissionProc: TProc; // the procedure to launch when the user response positivelly to the rationale
            const ACanNotRequestPermissionProc: TProc); // the procedure to launch when the user response negativelly to the rationale
begin
  var LMessage: String := '';
  if AToActivateGPS then begin
    LMessage :=
      'Hi there! Our app uses GPS to provide location-based services. To use '+
      'these features, we need your permission to access your device''s GPS '+
      'sensor. Please make sure that your GPS sensor is enabled in your '+
      'device''s settings. Don''t worry, we respect your privacy and will '+
      'only use your location information for the intended purpose. Once '+
      'you''ve granted us access, you''ll be able to enjoy all the benefits '+
      'of our app''s location-based features. Thank you!'
  end
  else if AToRequestCoarseGeoPositionPermission then begin
    if AIsForced then LMessage :=
      'Hi there! Our app uses your location to provide you with personalized '+
      'content and services. To enable this feature, please go to the '+
      'settings of our app and grant us access to your device''s location. '+
      'Don''t worry, we respect your privacy and will only use your location '+
      'information for the intended purpose. Once you''ve granted us access, '+
      'you''ll be able to enjoy all the benefits of our app''s '+
      'location-based features. Thank you!'
    else LMessage :=
      'Hi there! Our app uses your location to provide you with personalized '+
      'content and services. To enable this feature, we need your permission '+
      'to access your device''s location. Don''t worry, we respect your '+
      'privacy and will only use your location information for the intended '+
      'purpose. Would you like to authorize our app to access your location?';
  end
  else if AToRequestPreciseGeoPositionPermission then begin
    if AIsForced then LMessage :=
      'Hello! Our app relies on your precise location to provide you with '+
      'personalized content and services. To enable this feature, please go '+
      'to our app''s settings and grant us access to your device''s precise '+
      'location. We understand the importance of privacy and assure you that '+
      'we will only use your location information for its intended purpose. '+
      'Once you''ve granted us access, you''ll be able to fully experience '+
      'and benefit from our app''s location-based features. Thank you for '+
      'choosing our app!'
    else LMessage :=
      'Hello! Our app requires your device''s precise location to provide you '+
      'with personalized content and services. In order to use this feature, '+
      'we kindly ask for your permission to access your device''s precise '+
      'location. We understand that privacy is important, and we want to '+
      'assure you that we will only use your location information for its '+
      'intended purpose. Would you like to authorize our app to access your '+
      'precise location? Once you grant us permission, you''ll be able to '+
      'fully experience and benefit from our app''s location-based features. '+
      'Thank you for considering our request!';

  end
  else if AToRequestAlwaysAuthorization then begin
    if AIsForced then LMessage :=
      'Hello! Our app requires access to your device''s location in the '+
      'background to provide you with personalized content and services. To '+
      'enable this feature, please go to the settings of our app and grant '+
      'us permission to access your device''s location in the background. '+
      'We want to assure you that we respect your privacy and will only use '+
      'your location information for its intended purpose. Once you grant us '+
      'permission, you''ll be able to fully enjoy all the benefits of our '+
      'app''s location-based features. Thank you for considering our request!'
    else LMessage :=
      'Hello! Our app requires access to your device''s location in the '+
      'background to provide you with personalized content and services. In '+
      'order to use this feature, we kindly ask for your permission to access '+
      'your device''s background location. We respect your privacy and will '+
      'only use your location information for its intended purpose. Would '+
      'you like to authorize our app to access your device''s background '+
      'location? Once you grant us permission, you''ll be able to fully '+
      'enjoy all the benefits of our app''s location-based features. '+
      'Thank you for considering our request!';
  end
  else
    raise Exception.Create('Error 13359DD4-3BB0-4DF7-ACC7-F5D501151B58');

  TDialogService.MessageDialog(
    LMessage, // const AMessage: string;
    TMsgDlgType.mtConfirmation, // const ADialogType: TMsgDlgType;
    [TMsgDlgBtn.mbCancel, TMsgDlgBtn.mbOK], // const AButtons: TMsgDlgButtons;
    TMsgDlgBtn.mbCancel, // const ADefaultButton: TMsgDlgBtn;
    0, // const AHelpContext: THelpContext;
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOk then ACanRequestPermissionProc
      else ACanNotRequestPermissionProc;
    end); // const ACloseDialogProc: TInputCloseDialogProc);

end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
