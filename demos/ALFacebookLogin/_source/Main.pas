unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  {$IF Defined(IOS) or Defined(ANDROID)}
  Grijjy.ErrorReporting,
  {$ENDIF}
  system.Messaging,
  alString,
  alStringList,
  alfmxCommon,
  alFaceBook,
  alcommon, FMX.Memo.Types, ALFmxObjects;

type
  TForm1 = class(TForm)
    ButtonFacebookLogin: TButton;
    Memo1: TMemo;
    ButtonLogoutFromFacebook: TButton;
    ButtonGetCurrentTokenInfos: TButton;
    ButtonGetCurrentUserInfos: TButton;
    TextIntro: TALText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonFacebookLoginClick(Sender: TObject);
    procedure ButtonGetCurrentTokenInfosClick(Sender: TObject);
    procedure ButtonLogoutFromFacebookClick(Sender: TObject);
    procedure ButtonGetCurrentUserInfosClick(Sender: TObject);
  private
    fFaceBookLogin: TalFacebookLogin;
    fFacebookGraphRequest: TALFacebookGraphRequest;
    procedure onFaceBookLoginCancel;
    procedure onFaceBookLoginError(const aMsg: String);
    procedure onFaceBookLoginSuccess(const aUserID: String; const aToken: String; const AGrantedPermissions: TArray<String>; const ADeniedPermissions: TArray<String>);
    procedure onFaceBookGraphRequestCompleted(const aResponse: string; Const aErrorCode: integer; Const aErrorMsg: String);
  private
    {$IF Defined(IOS) or Defined(ANDROID)}
    procedure ApplicationExceptionHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$IF Defined(MSWINDOWS) or Defined(_MACOS)}
    procedure ApplicationExceptionHandler(Sender: TObject; E: Exception);
    {$ENDIF}
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin

  {$IF Defined(IOS) or Defined(ANDROID)}
  Application.OnException := TgoExceptionReporter.ExceptionHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ELSE}
  Application.OnException := ApplicationExceptionHandler;
  {$ENDIF}

  fFaceBookLogin := TalFacebookLogin.Create;
  fFaceBookLogin.onCancel := onFaceBookLoginCancel;
  fFaceBookLogin.onError := onFaceBookLoginError;
  fFaceBookLogin.onSuccess := onFaceBookLoginSuccess;

  fFacebookGraphRequest := TALFacebookGraphRequest.Create;
  fFacebookGraphRequest.onCompleted := onFaceBookGraphRequestCompleted;

end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin

  {$IF Defined(IOS) or Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TgoExceptionReportMessage, ApplicationExceptionHandler);
  {$ENDIF}

  alFreeAndNil(fFaceBookLogin);
  alFreeAndNil(fFacebookGraphRequest);

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

{*************************************}
procedure TForm1.onFaceBookLoginCancel;
begin
  allog('FaceBookLogin:onCancel', 'FaceBookLogin:onCancel', TalLogType.warn);
  memo1.lines.clear;
  memo1.lines.Add('FacebookLogin:onCancel');
end;

{********************************************************}
procedure TForm1.onFaceBookLoginError(const aMsg: String);
begin
  allog('FaceBookLogin:onError', 'FaceBookLogin:onError', TalLogType.error);
  memo1.lines.clear;
  memo1.lines.Add(aMsg);
end;

{************************************************************************************************************************************************************************}
procedure TForm1.onFaceBookLoginSuccess(const aUserID: String; const aToken: String; const AGrantedPermissions: TArray<String>; const ADeniedPermissions: TArray<String>);
var S: string;
    i: integer;
begin
  allog('FaceBookLogin:onSuccess', 'FaceBookLogin:onSuccess', TalLogType.info);
  memo1.lines.clear;

  memo1.lines.Add('UserId: ' + aUserID);
  memo1.lines.Add('-----');
  memo1.lines.Add('Token: ' + aToken);
  memo1.lines.Add('-----');
  S := '';
  for I := Low(AGrantedPermissions) to High(AGrantedPermissions) do
    s := s + AGrantedPermissions[i] + ';';
  memo1.lines.Add('Granted Permissions: ' + S);
  memo1.lines.Add('-----');
  S := '';
  for I := Low(ADeniedPermissions) to High(ADeniedPermissions) do
    s := s + ADeniedPermissions[i] + ';';
  memo1.lines.Add('Denied Permissions: ' + S);
end;

{****************************************************************************************************************************}
procedure TForm1.onFaceBookGraphRequestCompleted(const aResponse: string; Const aErrorCode: integer; Const aErrorMsg: String);
begin
  allog('FaceBookGraphRequest:onCompleted', 'FaceBookGraphRequest:onCompleted', TalLogType.info);
  memo1.lines.clear;
  memo1.lines.Add('ErrorCode: ' + inttoStr(aErrorCode));
  memo1.lines.Add('ErrorMsg: ' + aErrorMsg);
  memo1.lines.Add('-----');
  memo1.lines.Add(aResponse);
end;

{*********************************************************}
procedure TForm1.ButtonFacebookLoginClick(Sender: TObject);
begin
  fFaceBookLogin.logInWithReadPermissions(['public_profile', 'user_friends', 'email']);
end;

{****************************************************************}
procedure TForm1.ButtonGetCurrentTokenInfosClick(Sender: TObject);
var S: string;
    i: integer;
begin
  memo1.lines.clear;
  memo1.lines.Add('UserId: ' + fFaceBookLogin.CurrentUserId);
  memo1.lines.Add('-----');
  memo1.lines.Add('Token: ' + fFaceBookLogin.CurrentToken);
  memo1.lines.Add('-----');
  S := '';
  for I := Low(fFaceBookLogin.CurrentGrantedPermissions) to High(fFaceBookLogin.CurrentGrantedPermissions) do
    s := s + fFaceBookLogin.CurrentGrantedPermissions[i] + ';';
  memo1.lines.Add('Granted Permissions: ' + S);
  memo1.lines.Add('-----');
  S := '';
  for I := Low(fFaceBookLogin.CurrentDeniedPermissions) to High(fFaceBookLogin.CurrentDeniedPermissions) do
    s := s + fFaceBookLogin.CurrentDeniedPermissions[i] + ';';
  memo1.lines.Add('Denied Permissions: ' + S);
end;

{***************************************************************}
procedure TForm1.ButtonGetCurrentUserInfosClick(Sender: TObject);
begin
  fFacebookGraphRequest.Request('me', ['fields=picture.type(large),id,birthday,cover,email,first_name,gender,languages,last_name,locale,location,name']);
end;

{**************************************************************}
procedure TForm1.ButtonLogoutFromFacebookClick(Sender: TObject);
begin
  fFaceBookLogin.logout;
  memo1.lines.clear;
  memo1.lines.Add('Logged out');
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
