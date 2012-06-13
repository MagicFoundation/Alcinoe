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
     inifiles,
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
    FIniFileName  : AnsiString;
    FInitialized  : Boolean;
    FSMTPCLient: TALSMTPCLient;
  end;

var
  Form1: TForm1;

implementation

Uses alFcnString,
     ALInternetMessageCommon,
     ALMultiPartMixedParser;

{$R *.DFM}

const
  SectionData       = 'Data';
  KeyHost           = 'HostName';
  KeyPort           = 'Port';
  KeyFrom           = 'From';
  KeyTo             = 'To';
  KeyCc             = 'Cc';
  KeyBcc            = 'Bcc';
  KeySubject        = 'Subject';
  KeyUser           = 'UserName';
  KeyPass           = 'Password';
  KeyAuth           = 'Authentification';
  KeyPriority       = 'Priority';
  KeyConfirm        = 'Confirm';
  SectionWindow     = 'Window';
  KeyTop            = 'Top';
  KeyLeft           = 'Left';
  KeyWidth          = 'Width';
  KeyHeight         = 'Height';
  SectionFileAttach = 'Files';
  KeyFileAttach     = 'File';
  SectionMsgMemo    = 'Message';
  KeyMsgMemo        = 'Msg';

{********************************************************}
procedure SaveStringsToIniFile(const IniFileName : AnsiString;
                               const IniSection  : AnsiString;
                               const IniKey      : AnsiString;
                               Strings           : TStrings);
var IniFile : TIniFile;
    nItem   : Integer;
begin
  if (IniFileName = '') or
     (IniSection = '') or
     (IniKey = '') or
     (not Assigned(Strings)) then Exit;
  IniFile := TIniFile.Create(IniFileName);
  IniFile.EraseSection(IniSection);
  if Strings.Count <= 0 then IniFile.WriteString(IniSection, IniKey + 'EmptyFlag', 'Empty')
  else
    for nItem := 0 to Strings.Count - 1 do
        IniFile.WriteString(
                            IniSection,
                            IniKey + IntToStr(nItem),
                            Strings.Strings[nItem]
                           );
  IniFile.Free;
end;

{*********************************************************}
function LoadStringsFromIniFile(const IniFileName : AnsiString;
                                const IniSection  : AnsiString;
                                const IniKey      : AnsiString;
                                Strings           : TStrings) : Boolean;
var IniFile : TIniFile;
    nItem   : Integer;
    I       : Integer;
    Buf     : AnsiString;
begin
  Result := TRUE;
  if (IniFileName = '') or
     (IniSection = '') or
     (IniKey = '') or
     (not Assigned(Strings)) then Exit;
  Strings.Clear;
  IniFile := TIniFile.Create(IniFileName);
  try
    if IniFile.ReadString(IniSection, IniKey + 'EmptyFlag', '') <> '' then Exit;
    IniFile.ReadSectionValues(IniSection, Strings);
  finally
    IniFile.Free;
  end;
  nItem := Strings.Count - 1;
  while nItem >= 0 do begin
    Buf := Strings.Strings[nItem];
    if CompareText(IniKey, Copy(Buf, 1, Length(IniKey))) <> 0 then Strings.Delete(nItem)
    else begin
      if not (Buf[Length(IniKey) + 1] in ['0'..'9']) then Strings.Delete(nItem)
      else begin
        I := Pos('=', Buf);
        Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
      end;
    end;
    Dec(nItem);
  end;
  Result := (Strings.Count <> 0);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSMTPCLient.Free;
end;

{*****************************************}
procedure TForm1.FormShow(Sender: TObject);
var IniFile : TIniFile;
begin
  if not FInitialized then begin
    FInitialized := TRUE;
    IniFile := TIniFile.Create(FIniFileName);
    HostEdit.Text    := IniFile.ReadString(SectionData, KeyHost,'localhost');
    PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort, '25');
    FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom, 'first.last@company.com');
    ToEdit.Text      := IniFile.ReadString(SectionData, KeyTo, 'john.doe@acme');
    CcEdit.Text      := IniFile.ReadString(SectionData, KeyCc, '');
    BccEdit.Text     := IniFile.ReadString(SectionData, KeyBcc, 'francois.piette@swing.be');
    SubjectEdit.Text := IniFile.ReadString(SectionData, KeySubject, 'This is the message subject');
    UsernameEdit.Text :=  IniFile.ReadString(SectionData, KeyUser, 'account name');
    PasswordEdit.Text      :=  IniFile.ReadString(SectionData, KeyPass, 'account password');
    AuthComboBox.ItemIndex     := IniFile.ReadInteger(SectionData, KeyAuth, 0);
    PriorityComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyPriority, 2);
    ConfirmCheckBox.Checked    := Boolean(IniFile.ReadInteger(SectionData, KeyConfirm, 0));
    if not LoadStringsFromIniFile(
                                  FIniFileName,
                                  SectionFileAttach,
                                  KeyFileAttach,
                                  FileAttachMemo.Lines
                                 ) then
      FileAttachMemo.Text := '';
    if not LoadStringsFromIniFile(
                                  FIniFileName,
                                  SectionMsgMemo,
                                  KeyMsgMemo,
                                  MsgMemo.Lines
                                 ) then
      MsgMemo.Text := 'This is the first line' + #13#10 +
                      'Then the second one' + #13#10 +
                      'The next one is empty' + #13#10 +
                      '' + #13#10 +
                      'The next one has only a single dot' + #13#10 +
                      '.' + #13#10 +
                      'Finally the last one' + #13#10;
    Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
    Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
    Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
    Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
    IniFile.Free;
  end;
end;

{********************************************************}
procedure TForm1.ClearDisplayButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;

{***************************************************}
procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSmtpClient.Connect(HostEdit.Text, strtoint(PortEdit.Text))));
end;

{************************************************}
procedure TForm1.HeloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSMTPCLient.Helo));
end;

{************************************************}
procedure TForm1.EhloButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSMTPCLient.eHlo));
end;

{************************************************}
procedure TForm1.AuthButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSmtpClient.Auth(UsernameEdit.Text, PasswordEdit.Text, TAlSmtpClientAuthType(AuthComboBox.ItemIndex))));
end;

{****************************************************}
procedure TForm1.MailFromButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSmtpClient.MailFrom(FromEdit.Text)));
end;

{**************************************************}
procedure TForm1.RcptToButtonClick(Sender: TObject);
Var ALst: TstringList;
    Str: AnsiString;
    i: integer;
begin
  aLst := TstringList.Create;
  Try

    Str := ToEdit.Text + #13#10 + CcEdit.Text + #13#10 + BccEdit.text;
    Str := AlStringReplace(Str,',',#13#10,[RfReplaceall]);
    Str := AlStringReplace(Str,';',#13#10,[RfReplaceall]);
    aLst.Text := Trim(Str);
    i := 0;
    While i <= aLst.Count - 1 do begin
      aLst[i] := trim(aLst[i]);
      If aLst[i] = '' then aLst.Delete(i)
      else inc(i);
    end;

    DisplayMemo.Lines.Add(trim(FSmtpClient.RcptTo(Alst)));

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

    aEmailHeader.From := FromEdit.Text;
    aEmailHeader.SendTo := ToEdit.Text;;
    aEmailHeader.cc := ccEdit.Text;;
    aEmailHeader.Subject := SubjectEdit.Text;
    If ConfirmCheckBox.Checked then aEmailHeader.DispositionNotificationTo := FromEdit.Text;
    aEmailHeader.Priority := PriorityComboBox.Text;
    If PriorityComboBox.ItemIndex = 1 then str := 'High'
    else If PriorityComboBox.ItemIndex = 2 then str := 'Normal'
    else str := 'Low';
    aEmailHeader.CustomHeaders.Add('X-MSMail-Priority: ' + str);
    If trim(FileAttachMemo.Lines.text) <> '' then begin
      AMultiPartMixedAttachments := TALMultiPartMixedContents.Create(true);
      Try
        For i := 0 to FileAttachMemo.Lines.Count - 1 do
          If FileAttachMemo.Lines[i] <> '' then
            AMultiPartMixedAttachments.Add.LoadDataFromFileAsAttachmentBase64Encode(trim(FileAttachMemo.Lines[i]));
          DisplayMemo.Lines.Add(trim(FSmtpClient.DataMultipartMixed(aEmailHeader, MsgMemo.Lines.Text, 'text/plain', AMultiPartMixedAttachments)));
      finally
        AMultiPartMixedAttachments.Free;
      end;
    end
    else DisplayMemo.Lines.Add(trim(FSmtpClient.Data(aEmailHeader, MsgMemo.Lines.Text)));


  finally
    aEmailHeader.Free;
  end;
end;

{************************************************}
procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  DisplayMemo.Lines.Add(trim(FSmtpClient.quit));
end;

{****************************************************}
procedure TForm1.AllInOneButtonClick(Sender: TObject);
Var aEmailHeader: TALEmailHeader;
    AMultiPartMixedAttachments : TALMultiPartMixedContents;
    aLst: TstringList;
    Str: AnsiString;
    i : integer;
begin

  aLst := TstringList.Create;
  aEmailHeader := TALEmailHeader.Create;
  Try

    Str := ToEdit.Text + #13#10 + CcEdit.Text + #13#10 + BccEdit.text;
    Str := AlStringReplace(Str,',',#13#10,[RfReplaceall]);
    Str := AlStringReplace(Str,';',#13#10,[RfReplaceall]);
    aLst.Text := Trim(Str);
    i := 0;
    While i <= aLst.Count - 1 do begin
      aLst[i] := trim(aLst[i]);
      If aLst[i] = '' then aLst.Delete(i)
      else inc(i);
    end;

    aEmailHeader.From := FromEdit.Text;
    aEmailHeader.SendTo := ToEdit.Text;;
    aEmailHeader.cc := ccEdit.Text;;
    aEmailHeader.Subject := SubjectEdit.Text;
    If ConfirmCheckBox.Checked then aEmailHeader.DispositionNotificationTo := FromEdit.Text;
    aEmailHeader.Priority := PriorityComboBox.Text;
    If PriorityComboBox.ItemIndex = 1 then str := 'High'
    else If PriorityComboBox.ItemIndex = 2 then str := 'Normal'
    else str := 'Low';
    aEmailHeader.CustomHeaders.Add('X-MSMail-Priority: ' + str);
    If trim(FileAttachMemo.Lines.text) <> '' then begin
      AMultiPartMixedAttachments := TALMultiPartMixedContents.Create(true);
      Try
        For i := 0 to FileAttachMemo.Lines.Count - 1 do
          If FileAttachMemo.Lines[i] <> '' then
            AMultiPartMixedAttachments.Add.LoadDataFromFileAsAttachmentBase64Encode(trim(FileAttachMemo.Lines[i]));

          FSmtpClient.SendMailMultipartMixed(
                                             HostEdit.Text,
                                             strtoint(PortEdit.Text),
                                             FromEdit.Text,
                                             aLst,
                                             UsernameEdit.Text,
                                             PasswordEdit.Text,
                                             TAlSmtpClientAuthType(AuthComboBox.ItemIndex),
                                             aEmailHeader,
                                             MsgMemo.Lines.Text,
                                             'text/plain',
                                             AMultiPartMixedAttachments
                                            );

      finally
        AMultiPartMixedAttachments.Free;
      end;
    end
    else FSmtpClient.SendMail(
                              HostEdit.Text,
                              strtoint(PortEdit.Text),
                              FromEdit.Text,
                              aLst,
                              UsernameEdit.Text,
                              PasswordEdit.Text,
                              TAlSmtpClientAuthType(AuthComboBox.ItemIndex),
                              aEmailHeader.RawHeaderText,
                              MsgMemo.Lines.Text
                              );

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
  FIniFileName := LowerCase(Application.ExeName);
  FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
  FSmtpClient := TAlSMTPClient.Create;
  CoInitialize(nil);

  ie := CreateOleObject('InternetExplorer.Application') as IWebBrowser2;
  SetWindowLong(ie.hwnd, GWL_STYLE, GetWindowLong(ie.hwnd, GWL_STYLE) and not WS_BORDER and not WS_SIZEBOX and not WS_DLGFRAME );
  SetWindowPos(ie.hwnd, HWND_TOP, Left, Top, Width, Height, SWP_FRAMECHANGED);
  windows.setparent(ie.hwnd, PanelWebBrowser.handle);
  ie.Left := maxint; // don't understand why it's look impossible to setup the position
  ie.Top  := maxint; // don't understand why it's look impossible to setup the position
  ie.Width := 100;
  ie.Height := 300;
  ie.MenuBar := false;
  ie.AddressBar := false;
  ie.Resizable := false;
  ie.StatusBar := false;
  ie.ToolBar := 0;
  Url := 'http://www.arkadia.com/html/alcinoe_like.html';
  ie.Navigate2(Url,Flags,TargetFrameName,PostData,Headers);
  ie.Visible := true;
end;

{********************************************************************}
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(FIniFileName);
  IniFile.WriteString(SectionData, KeyHost, HostEdit.Text);
  IniFile.WriteString(SectionData, KeyPort, PortEdit.Text);
  IniFile.WriteString(SectionData, KeyFrom, FromEdit.Text);
  IniFile.WriteString(SectionData, KeyTo, ToEdit.Text);
  IniFile.WriteString(SectionData, KeyCc, CcEdit.Text);
  IniFile.WriteString(SectionData, KeyBcc, BccEdit.Text);
  IniFile.WriteString(SectionData, KeySubject, SubjectEdit.Text);
  IniFile.WriteString(SectionData, KeyUser, UsernameEdit.Text);
  IniFile.WriteString(SectionData, KeyPass, PasswordEdit.Text);
  IniFile.WriteInteger(SectionData, KeyAuth, AuthComboBox.ItemIndex);
  IniFile.WriteInteger(SectionData, KeyPriority, PriorityComboBox.ItemIndex);
  IniFile.WriteInteger(SectionData, KeyConfirm, Ord(ConfirmCheckBox.Checked));
  SaveStringsToIniFile(FIniFileName, SectionFileAttach, KeyFileAttach, FileAttachMemo.Lines);
  SaveStringsToIniFile(FIniFileName, SectionMsgMemo, KeyMsgMemo, MsgMemo.Lines);
  IniFile.WriteInteger(SectionWindow, KeyTop, Top);
  IniFile.WriteInteger(SectionWindow, KeyLeft, Left);
  IniFile.WriteInteger(SectionWindow, KeyWidth, Width);
  IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
  IniFile.Free;

  try
    ie.quit;
  except
  end;
  sleep(500);
  CoUninitialize;
end;

{$IFDEF DEBUG}
initialization
  ReportMemoryleaksOnSHutdown := True;
{$ENDIF}

end.

