unit Unit1;

interface

uses Windows,
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
     AlSMTPClient,
     ActiveX,
     OleCtrls,
     SHDocVw,
     ComObj;

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
    ClearDisplayButton: TButton;
    ConnectButton: TButton;
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
    AllInOneButton: TButton;
    ConfirmCheckBox: TCheckBox;
    Panel2: TPanel;
    Label15: TLabel;
    Label16: TLabel;
    Panel3: TPanel;
    PanelWebBrowser: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ClearDisplayButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure HeloButtonClick(Sender: TObject);
    procedure MailFromButtonClick(Sender: TObject);
    procedure RcptToButtonClick(Sender: TObject);
    procedure DataButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EhloButtonClick(Sender: TObject);
    procedure AuthButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AllInOneButtonClick(Sender: TObject);
  private
    FSMTPCLient: TALSMTPCLient;
  end;

var
  Form1: TForm1;

implementation

Uses ALString,
     ALStringList,
     ALInternetMessages,
     ALMultiPartParser;

{$R *.DFM}

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSMTPCLient.Free;
end;

{*****************************************}
procedure TForm1.FormShow(Sender: TObject);
begin

end;

{********************************************************}
procedure TForm1.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;

{***************************************************}
procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.Connect(AnsiString(HostEdit.Text), StrToInt(PortEdit.Text)))));
end;

{************************************************}
procedure TForm1.HeloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSMTPCLient.Helo)));
end;

{************************************************}
procedure TForm1.EhloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSMTPCLient.eHlo)));
end;

{************************************************}
procedure TForm1.AuthButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.Auth(AnsiString(UsernameEdit.Text), AnsiString(PasswordEdit.Text), TAlSmtpClientAuthType(AuthComboBox.ItemIndex)))));
end;

{****************************************************}
procedure TForm1.MailFromButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.MailFrom(AnsiString(FromEdit.Text)))));
end;

{**************************************************}
procedure TForm1.RcptToButtonClick(Sender: TObject);
Var ALst: TALStringList;
    Str: AnsiString;
    i: integer;
begin
  aLst := TALStringList.Create;
  Try

    Str := AnsiString(ToEdit.Text) + #13#10 + AnsiString(CcEdit.Text) + #13#10 + AnsiString(BccEdit.text);
    Str := AlStringReplace(Str,',',#13#10,[RfReplaceall]);
    Str := AlStringReplace(Str,';',#13#10,[RfReplaceall]);
    aLst.Text := ALTrim(Str);
    i := 0;
    While i <= aLst.Count - 1 do begin
      aLst[i] := ALTrim(aLst[i]);
      If aLst[i] = '' then aLst.Delete(i)
      else inc(i);
    end;

    DisplayMemo.Lines.Add(String(ALTrim(AnsiString(FSmtpClient.RcptTo(Alst)))));

  finally
    aLst.free;
  end;
end;

{************************************************}
procedure TForm1.DataButtonClick(Sender: TObject);
Var aEmailHeader: TALEmailHeader;
    AMultiPartMixedAttachments : TALMultiPartMixedContents;
    i : integer;
    Str: AnsiString;
begin
  aEmailHeader := TALEmailHeader.Create;
  Try

    aEmailHeader.From := AnsiString(FromEdit.Text);
    aEmailHeader.SendTo := AnsiString(ToEdit.Text);
    aEmailHeader.cc := AnsiString(ccEdit.Text);
    aEmailHeader.Subject := AnsiString(SubjectEdit.Text);
    If ConfirmCheckBox.Checked then aEmailHeader.DispositionNotificationTo := AnsiString(FromEdit.Text);
    aEmailHeader.Priority := AnsiString(PriorityComboBox.Text);
    If PriorityComboBox.ItemIndex = 1 then str := 'High'
    else If PriorityComboBox.ItemIndex = 2 then str := 'Normal'
    else str := 'Low';
    aEmailHeader.CustomHeaders.Add('X-MSMail-Priority: ' + str);
    If Trim(FileAttachMemo.Lines.text) <> '' then begin
      AMultiPartMixedAttachments := TALMultiPartMixedContents.Create(true);
      Try
        For i := 0 to FileAttachMemo.Lines.Count - 1 do
          If FileAttachMemo.Lines[i] <> '' then
            AMultiPartMixedAttachments.Add.LoadDataFromFileAsAttachmentBase64Encode(ALTrim(AnsiString(FileAttachMemo.Lines[i])));
          DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.DataMultipartMixed(aEmailHeader, AnsiString(MsgMemo.Lines.Text), 'text/plain', AMultiPartMixedAttachments))));
      finally
        AMultiPartMixedAttachments.Free;
      end;
    end
    else DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.Data(AnsiString(aEmailHeader.RawHeaderText), AnsiString(MsgMemo.Lines.Text)))));


  finally
    aEmailHeader.Free;
  end;
end;

{************************************************}
procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(String(ALTrim(FSmtpClient.quit)));
end;

{****************************************************}
procedure TForm1.AllInOneButtonClick(Sender: TObject);
Var aEmailHeader: TALEmailHeader;
    AMultiPartMixedAttachments : TALMultiPartMixedContents;
    aLst: TALStringList;
    Str: AnsiString;
    i : integer;
begin

  aLst := TALStringList.Create;
  aEmailHeader := TALEmailHeader.Create;
  Try

    Str := AnsiString(ToEdit.Text) + #13#10 + AnsiString(CcEdit.Text) + #13#10 + AnsiString(BccEdit.text);
    Str := AlStringReplace(Str,',',#13#10,[RfReplaceall]);
    Str := AlStringReplace(Str,';',#13#10,[RfReplaceall]);
    aLst.Text := ALTrim(Str);
    i := 0;
    While i <= aLst.Count - 1 do begin
      aLst[i] := ALTrim(aLst[i]);
      If aLst[i] = '' then aLst.Delete(i)
      else inc(i);
    end;

    aEmailHeader.From := AnsiString(FromEdit.Text);
    aEmailHeader.SendTo := AnsiString(ToEdit.Text);
    aEmailHeader.cc := AnsiString(ccEdit.Text);
    aEmailHeader.Subject := AnsiString(SubjectEdit.Text);
    If ConfirmCheckBox.Checked then aEmailHeader.DispositionNotificationTo := AnsiString(FromEdit.Text);
    aEmailHeader.Priority := AnsiString(PriorityComboBox.Text);
    If PriorityComboBox.ItemIndex = 1 then str := 'High'
    else If PriorityComboBox.ItemIndex = 2 then str := 'Normal'
    else str := 'Low';
    aEmailHeader.CustomHeaders.Add('X-MSMail-Priority: ' + str);
    If Trim(FileAttachMemo.Lines.text) <> '' then begin
      AMultiPartMixedAttachments := TALMultiPartMixedContents.Create(true);
      Try
        For i := 0 to FileAttachMemo.Lines.Count - 1 do
          If FileAttachMemo.Lines[i] <> '' then
            AMultiPartMixedAttachments.Add.LoadDataFromFileAsAttachmentBase64Encode(ALTrim(AnsiString(FileAttachMemo.Lines[i])));

          FSmtpClient.SendMailMultipartMixed(AnsiString(HostEdit.Text),
                                             StrToInt(PortEdit.Text),
                                             AnsiString(FromEdit.Text),
                                             aLst,
                                             AnsiString(UsernameEdit.Text),
                                             AnsiString(PasswordEdit.Text),
                                             TAlSmtpClientAuthType(AuthComboBox.ItemIndex),
                                             aEmailHeader,
                                             AnsiString(MsgMemo.Lines.Text),
                                             'text/plain',
                                             AMultiPartMixedAttachments);

      finally
        AMultiPartMixedAttachments.Free;
      end;
    end
    else FSmtpClient.SendMail(AnsiString(HostEdit.Text),
                              StrToInt(PortEdit.Text),
                              AnsiString(FromEdit.Text),
                              aLst,
                              AnsiString(UsernameEdit.Text),
                              AnsiString(PasswordEdit.Text),
                              TAlSmtpClientAuthType(AuthComboBox.ItemIndex),
                              aEmailHeader.RawHeaderText,
                              AnsiString(MsgMemo.Lines.Text));

    DisplayMemo.Lines.Add('Success');

  finally
    aEmailHeader.Free;
    aLst.free;
  end;
end;




{-------------------}
var ie: IWebBrowser2;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
var Url, Flags, TargetFrameName, PostData, Headers: OleVariant;
begin
  DisplayMemo.Clear;
  FSmtpClient := TAlSMTPClient.Create;
  CoInitialize(nil);

  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  ie.Width := 100;
  ie.Height := 300;
  Url := 'http://static.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    ie.quit;
  except
  end;
  sleep(500);
  CoUninitialize;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.

