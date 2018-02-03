unit MapiDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, JclMapi;

type
  TMainForm = class(TForm)
    ClientTypeGroupBox: TGroupBox;
    AutomaticRadioBtn: TRadioButton;
    MapiRadioBtn: TRadioButton;
    DirectRadioBtn: TRadioButton;
    ClientsListView: TListView;
    ToNameEdit: TEdit;
    Label1: TLabel;
    SubjectEdit: TEdit;
    Label2: TLabel;
    BodyEdit: TRichEdit;
    SendBtn: TButton;
    Label3: TLabel;
    ClientLabel: TLabel;
    Bevel1: TBevel;
    AttachmentBtn: TButton;
    Label4: TLabel;
    ToAddressEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    OpenDialog1: TOpenDialog;
    DialogCheckBox: TCheckBox;
    AttachmentPaintBox: TPaintBox;
    ProfilesListView: TListView;
    HtmlCheckBox: TCheckBox;
    SaveBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClientsListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure AutomaticRadioBtnClick(Sender: TObject);
    procedure ClientsListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SendBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AttachmentBtnClick(Sender: TObject);
    procedure AttachmentPaintBoxPaint(Sender: TObject);
    procedure ProfilesListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure SaveBtnClick(Sender: TObject);
  private
    procedure BuildClientList;
    procedure BuildProfilesList;
    procedure UpdateClientName;
  public
    SimpleMapiMail: TJclEmail;
  end;

var
  MainForm: TMainForm;

implementation

uses
  JclFileUtils, JclSysUtils;

{$R *.DFM}

procedure TMainForm.BuildClientList;
var
  I: Integer;
begin
  // Create list of registered mail clients
  ClientsListView.Items.BeginUpdate;
  try
    ClientsListView.Items.Clear;
    with SimpleMapiMail do
    begin
      for I := 0 to ClientCount - 1 do
        with ClientsListView.Items.Add do
        begin
          Caption := Clients[I].RegKeyName;
          Data := Pointer(Clients[I].Valid);
          SubItems.Add(Clients[I].ClientName);
          SubItems.Add(Clients[I].ClientPath);
        end;
      ClientsListView.Items[SelectedClientIndex].Selected := True;
      AutomaticRadioBtn.Enabled := AnyClientInstalled;
      MapiRadioBtn.Enabled := SimpleMapiInstalled;
      DirectRadioBtn.Enabled := ClientCount > 0;
    end;
  finally
    ClientsListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.BuildProfilesList;
var
  I: Integer;
begin
  ProfilesListView.Items.BeginUpdate;
  try
    ProfilesListView.Items.Clear;
    with SimpleMapiMail do
      for I := 0 to ProfileCount - 1 do
        with ProfilesListView.Items.Add do
        begin
          Caption := string(Profiles[I]);
          Data := Pointer(Caption = string(DefaultProfileName));
        end;  
  finally
    ProfilesListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SimpleMapiMail := TJclEmail.Create;
  BuildClientList;
  BuildProfilesList;
  UpdateClientName;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SimpleMapiMail);
end;

procedure TMainForm.ClientsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
  begin
    SimpleMapiMail.SelectedClientIndex := Item.Index;
    UpdateClientName;
  end;
end;

procedure TMainForm.UpdateClientName;
begin
  ClientLabel.Caption := SimpleMapiMail.CurrentClientName;
end;

procedure TMainForm.AutomaticRadioBtnClick(Sender: TObject);
begin
  with SimpleMapiMail do
  begin
    if AutomaticRadioBtn.Checked then
      ClientConnectKind := ctAutomatic;
    if MapiRadioBtn.Checked then
      ClientConnectKind := ctMapi;
    if DirectRadioBtn.Checked then
      ClientConnectKind := ctDirect;
  end;
  UpdateClientName;
end;

procedure TMainForm.ClientsListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Boolean(Item.Data) then
    Sender.Canvas.Font.Color := clInactiveCaption;
end;

procedure TMainForm.ProfilesListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Boolean(Item.Data) then
    Sender.Canvas.Font.Style := [fsBold];
end;

procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
  if not DialogCheckBox.Checked then
    Application.MessageBox('The message will be inserted to Draft folder.',
      PChar(Caption), MB_OK or MB_ICONWARNING);

{ // Simple message creating, using TJclEmail.SimpleSendMail class method
  JclSimpleSendMail(ToAddressEdit.Text, ToNameEdit.Text, SubjectEdit.Text,
    BodyEdit.Text, OpenDialog1.FileName, DialogCheckBox.Checked);}

  // Creating message using TJclEmail object, it is more flexible, but you have
  // to create an instance (SimpleMapiMail variable in this example) of the class
  SimpleMapiMail.Clear;
  SimpleMapiMail.Recipients.Add(AnsiString(ToAddressEdit.Text), AnsiString(ToNameEdit.Text));
  SimpleMapiMail.Subject := AnsiString(SubjectEdit.Text);
  SimpleMapiMail.Body := AnsiString(BodyEdit.Text);
  SimpleMapiMail.HtmlBody := HtmlCheckBox.Checked;
  if OpenDialog1.FileName <> '' then
    SimpleMapiMail.Attachments.Add(AnsiString(OpenDialog1.FileName));
  SimpleMapiMail.Save;
end;

procedure TMainForm.SendBtnClick(Sender: TObject);
begin
  if not DialogCheckBox.Checked then
    Application.MessageBox('The message will be inserted to Outgoing folder.',
      PChar(Caption), MB_OK or MB_ICONWARNING);

{ // Simple message creating, using TJclEmail.SimpleSendMail class method
  JclSimpleSendMail(ToAddressEdit.Text, ToNameEdit.Text, SubjectEdit.Text,
    BodyEdit.Text, OpenDialog1.FileName, DialogCheckBox.Checked);}

  // Creating message using TJclEmail object, it is more flexible, but you have
  // to create an instance (SimpleMapiMail variable in this example) of the class
  SimpleMapiMail.Clear;
  SimpleMapiMail.Recipients.Add(AnsiString(ToAddressEdit.Text), AnsiString(ToNameEdit.Text));
  SimpleMapiMail.Subject := AnsiString(SubjectEdit.Text);
  SimpleMapiMail.Body := AnsiString(BodyEdit.Text);
  SimpleMapiMail.HtmlBody := HtmlCheckBox.Checked;
  if OpenDialog1.FileName <> '' then
    SimpleMapiMail.Attachments.Add(AnsiString(OpenDialog1.FileName));
  SimpleMapiMail.Send(DialogCheckBox.Checked);
end;

procedure TMainForm.AttachmentBtnClick(Sender: TObject);
begin
  with OpenDialog1 do
  begin
    FileName := '';
    Execute;
    AttachmentPaintBox.Invalidate;
  end;
end;

procedure TMainForm.AttachmentPaintBoxPaint(Sender: TObject);
begin
  with TPaintBox(Sender) do
    Canvas.TextRect(ClientRect, 0, 0,
      PathCompactPath(Canvas.Handle, OpenDialog1.FileName, Width, cpCenter));
end;

end.
