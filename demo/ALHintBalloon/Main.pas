unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, ALHintBalloon, ExtCtrls, ShellApi;

type
  TfrmMain = class(TForm)
    BtnExit: TButton;
    XPManifest: TXPManifest;
    LblInfo: TLabel;
    Hint: TALHintBalloonControl;
    GbStandart: TGroupBox;
    BtnError: TButton;
    BtnWarn: TButton;
    BtnInfo: TButton;
    GbPicture: TGroupBox;
    BtnPict1: TButton;
    BtnPict2: TButton;
    BtnPict3: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    DlgOpen: TOpenDialog;
    LblAuthor: TLabel;
    LblAuthorName: TLabel;
    LblDemo: TLabel;
    LblMe: TLabel;
    GbArrowPos: TRadioGroup;
    GbAnimType: TRadioGroup;
    GbProps: TGroupBox;
    Label2: TLabel;
    Label7: TLabel;
    EdAnimSpeed: TEdit;
    EdDuration: TEdit;
    Panel1: TPanel;
    Label5: TLabel;
    procedure BtnExitClick(Sender: TObject);
    procedure BtnErrorClick(Sender: TObject);
    procedure BtnWarnClick(Sender: TObject);
    procedure BtnInfoClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure BtnPict1Click(Sender: TObject);
    procedure BtnPict2Click(Sender: TObject);
    procedure BtnPict3Click(Sender: TObject);
    procedure EdAnimSpeedChange(Sender: TObject);
    procedure GbAnimTypeClick(Sender: TObject);
    procedure LblAuthorNameClick(Sender: TObject);
    procedure LblMeClick(Sender: TObject);
  private
    function ArrowPos: TALHintBalloonArrowPosition;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// Private declarations ///////////////////////////
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
function TfrmMain.ArrowPos: TALHintBalloonArrowPosition;
begin
  Result := TALHintBalloonArrowPosition(GbArrowPos.ItemIndex);
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// Private declarations ///////////////////////////
////////////////////////////////////////////////////////////////////////////////
//------------------------------------------------------------------------------
procedure TfrmMain.BtnExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnErrorClick(Sender: TObject);
begin
  Hint.ShowTextHintBalloon(bmtError, 'How is the test?',
      'WHOA! Am i looking frightnng for you? :)',
      300, 10, 10, BtnError, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnWarnClick(Sender: TObject);
begin
  Hint.ShowTextHintBalloon(bmtWarning, 'How is the test?',
      'Oh my god! I am clicked :)', 300, 10, 10, BtnWarn, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnInfoClick(Sender: TObject);
begin
  Hint.ShowTextHintBalloon(bmtInfo, 'How is the test?',
    'A am very nice hint! :)', 300, 50, 50, btnInfo, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.FormClick(Sender: TObject);
begin
  Hint.ShowTextHintBalloon(bmtInfo, 'How is the test?',
      'A am very nice hint! :)', 300, 100, 100, self, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnPict1Click(Sender: TObject);
begin
  Hint.ShowPictureHintBalloon(ExtractFilePath(ParamStr(0)) + '1.ico',
    100, 100, BtnPict1, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnPict2Click(Sender: TObject);
begin
  Hint.ShowPictureHintBalloon(ExtractFilePath(ParamStr(0)) + '2.bmp',
    100, 100, BtnPict2, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.BtnPict3Click(Sender: TObject);
begin
  if DlgOpen.Execute then
    Hint.ShowPictureHintBalloon(dlgOpen.FileName,
      100, 100, BtnPict3, ArrowPos);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.EdAnimSpeedChange(Sender: TObject);
begin
  if (Sender as TEdit).Text > '' then
    Hint.AnimationSpeed := StrToInt((Sender as TEdit).Text);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.GbAnimTypeClick(Sender: TObject);
begin
  Hint.AnimationType := TALHintBalloonAnimationType(GbAnimType.ItemIndex);
end;

procedure TfrmMain.LblAuthorNameClick(Sender: TObject);
begin
  ShellApi.ShellExecute(Handle, 'open', 'mailto:SVanderClock@arkadia.com', nil, nil, 0);
end;

procedure TfrmMain.LblMeClick(Sender: TObject);
begin
  ShellApi.ShellExecute(Handle, 'open', 'mailto:quadr02005@yahoo.com', nil, nil, 0);
end;

end.
