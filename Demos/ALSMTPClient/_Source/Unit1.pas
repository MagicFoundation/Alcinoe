unit Unit1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  shellapi,
  ExtCtrls,
  ComCtrls,
  SyncObjs,
  Alcinoe.SMTP.Client;

type
  TForm1 = class(TForm)
    MsgMemo: TMemo;
    DisplayMemo: TMemo;
    ToolsPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Subject: TLabel;
    Label4: TLabel;
    HostEdit: TEdit;
    FromEdit: TEdit;
    ToEdit: TEdit;
    SubjectEdit: TEdit;
    PortEdit: TEdit;
    Label5: TLabel;
    AttachPanel: TPanel;
    Label6: TLabel;
    FileAttachMemo: TMemo;
    InfoPanel: TPanel;
    Label7: TLabel;
    ConnectPlainButton: TButton;
    HeloButton: TButton;
    MailFromButton: TButton;
    RcptToButton: TButton;
    DataButton: TButton;
    QuitButton: TButton;
    Label9: TLabel;
    Label10: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    AuthComboBox: TComboBox;
    PriorityComboBox: TComboBox;
    Label11: TLabel;
    EhloButton: TButton;
    AuthButton: TButton;
    Label12: TLabel;
    CcEdit: TEdit;
    Label13: TLabel;
    BccEdit: TEdit;
    ConfirmCheckBox: TCheckBox;
    ConnectTslButton: TButton;
    StartTLSButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ConnectPlainButtonClick(Sender: TObject);
    procedure HeloButtonClick(Sender: TObject);
    procedure MailFromButtonClick(Sender: TObject);
    procedure RcptToButtonClick(Sender: TObject);
    procedure DataButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure EhloButtonClick(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectTslButtonClick(Sender: TObject);
    procedure StartTLSButtonClick(Sender: TObject);
  private
    FSMTPCLient: TALSMTPCLient;
  end;

var
  Form1: TForm1;

implementation

Uses
  System.AnsiStrings,
  Alcinoe.Net,
  Alcinoe.StringUtils,
  Alcinoe.StringList,
  Alcinoe.InternetMessages,
  Alcinoe.Mime.MultiPart,
  Alcinoe.Common;

{$R *.DFM}

//
// To work with gmail you must sign in with app passwords:
// https://support.google.com/mail/answer/185833?hl=en
//

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FSmtpClient := TAlSMTPClient.Create;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  ALFreeAndNil(FSMTPCLient);
end;

{********************************************************}
procedure TForm1.ConnectPlainButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(
      FSmtpClient.Connect(
        AnsiString(HostEdit.Text),
        StrToInt(PortEdit.Text),
        False{AStartTLS})));
end;

{******************************************************}
procedure TForm1.ConnectTslButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(
      FSmtpClient.Connect(
        AnsiString(HostEdit.Text),
        StrToInt(PortEdit.Text),
        True{AStartTLS})));
end;

{************************************************}
procedure TForm1.HeloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(FSMTPCLient.Helo));
end;

{************************************************}
procedure TForm1.EhloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(FSMTPCLient.eHlo));
end;

{************************************************}
procedure TForm1.AuthButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(
      FSmtpClient.Auth(
        AnsiString(UsernameEdit.Text),
        AnsiString(PasswordEdit.Text),
        TALSmtpClient.TAuthType(AuthComboBox.ItemIndex))));
end;

{****************************************************}
procedure TForm1.MailFromButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(FSmtpClient.MailFrom(AnsiString(FromEdit.Text))));
end;

{**************************************************}
procedure TForm1.RcptToButtonClick(Sender: TObject);
begin
  var LRcptNames: TArray<AnsiString> := [];

  if ALTrim(ToEdit.Text) <> '' then begin
    setlength(LRcptNames, length(LRcptNames) + 1);
    LRcptNames[high(LRcptNames)] := AnsiString(AlTrim(ToEdit.Text));
  end;

  if ALTrim(CcEdit.Text) <> '' then begin
    setlength(LRcptNames, length(LRcptNames) + 1);
    LRcptNames[high(LRcptNames)] := AnsiString(AlTrim(CcEdit.Text));
  end;

  if ALTrim(BccEdit.Text) <> '' then begin
    setlength(LRcptNames, length(LRcptNames) + 1);
    LRcptNames[high(LRcptNames)] := AnsiString(AlTrim(BccEdit.Text));
  end;

  DisplayMemo.Lines.Add(
    String(FSmtpClient.RcptTo(LRcptNames)));

end;

{****************************************************}
procedure TForm1.StartTLSButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(FSmtpClient.StartTLS));
end;

{************************************************}
procedure TForm1.DataButtonClick(Sender: TObject);
begin
  var LMailHeaders := TALMailHeadersA.Create;
  Try

    LMailHeaders.From := AnsiString(FromEdit.Text);
    LMailHeaders.&To := AnsiString(ToEdit.Text);
    LMailHeaders.cc := AnsiString(ccEdit.Text);
    LMailHeaders.Subject := AnsiString(SubjectEdit.Text);
    If ConfirmCheckBox.Checked then LMailHeaders.DispositionNotificationTo := AnsiString(FromEdit.Text);
    LMailHeaders.Importance := AnsiString(PriorityComboBox.Text);
    var LMultipartAlternativeEncoder := TALMultipartAlternativeEncoderA.Create;
    Try

      LMultipartAlternativeEncoder.AddText(
        AnsiString(MsgMemo.Lines.Text), // const AText: AnsiString;
        'text/plain; charset=utf-8'); // const AContentType: AnsiString;
      LMultipartAlternativeEncoder.AddText(
        '<html><body><p>'+AnsiString(MsgMemo.Lines.Text)+'</p></body></html>', // const AText: AnsiString;
        'text/html; charset=utf-8'); // const AContentType: AnsiString;

      If FileAttachMemo.Lines.Count > 0 then begin
        var LMultipartMixedEncoder := TALMultipartMixedEncoderA.Create(true);
        Try

          LMultipartMixedEncoder.AddText(
            LMultipartAlternativeEncoder.PayloadString, // const AText: AnsiString;
            LMultipartAlternativeEncoder.MimeTypeHeader, // const AContentType: AnsiString;
            [TALNameValuePairA.Create('Content-Transfer-Encoding', '')]);// const AHeaders: TALNameValueArrayA = nil

          For var I := 0 to FileAttachMemo.Lines.Count - 1 do
            If FileAttachMemo.Lines[I] <> '' then
              LMultipartMixedEncoder.AddFile(
                FileAttachMemo.Lines[I]); // const AFilePath: String;

          LMailHeaders.ContentType := LMultipartMixedEncoder.MimeTypeHeader;
          DisplayMemo.Lines.Add(
            String(
              FSmtpClient.Data(
                AnsiString(LMailHeaders.RawHeaderText), // AHeader
                AnsiString(LMultipartMixedEncoder.PayloadString)))); // ABody

        finally
          ALFreeAndNil(LMultipartMixedEncoder);
        end;
      end
      else begin
        LMailHeaders.ContentType := LMultipartAlternativeEncoder.MimeTypeHeader;
        DisplayMemo.Lines.Add(
          String(
            FSmtpClient.Data(
              AnsiString(LMailHeaders.RawHeaderText), // AHeader
              AnsiString(LMultipartAlternativeEncoder.PayloadString)))); // ABody
      end;

    Finally
      AlfreeAndNil(LMultipartAlternativeEncoder);
    End;

  finally
    ALFreeAndNil(LMailHeaders);
  end;
end;

{************************************************}
procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(
    String(FSmtpClient.quit));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.