unit LanManDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtUserName: TEdit;
    Label2: TLabel;
    edtPassword: TEdit;
    Label3: TLabel;
    edtComment: TEdit;
    Label4: TLabel;
    edtScript: TEdit;
    Label5: TLabel;
    edtServer: TEdit;
    Label6: TLabel;
    edtHomedir: TEdit;
    GroupBox2: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    edtSIDName: TEdit;
    cboSID: TComboBox;
    GroupBox3: TGroupBox;
    rbLocal: TRadioButton;
    rbRemote: TRadioButton;
    edtSystemName: TEdit;
    GroupBox4: TGroupBox;
    btnAddUser: TButton;
    btnDeleteUser: TButton;
    Label9: TLabel;
    edtGroupName: TEdit;
    btnAddGroup: TButton;
    btnDeleteGroup: TButton;
    Label10: TLabel;
    edtGroupComment: TEdit;
    Label11: TLabel;
    edtFullName: TEdit;
    procedure btnAddUserClick(Sender: TObject);
    procedure cboSIDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses JclLANMan, JclSysInfo;

{$R *.DFM}

procedure TForm1.btnAddUserClick(Sender: TObject);
begin
  if CreateLocalAccount(edtUsername.Text,
                        edtFullName.Text,
                        edtPassword.Text,
                        edtComment.Text,
                        edtHomeDir.Text,
                        edtScript.Text) then
  begin
    ShowMessage('Success')
  end
  else
    ShowMessage('Failure');

end;

procedure TForm1.cboSIDChange(Sender: TObject);
var
  SystemName: string;
begin
  if rbLocal.Checked then
    SystemName := ''
  else
    SystemName := edtSystemName.Text;

  case cboSID.ItemIndex of
    0: edtSIDName.Text := LookupGroupname(SystemName, wkrAdmins);
    1: edtSIDName.Text := LookupGroupname(SystemName, wkrUsers);
    2: edtSIDName.Text := LookupGroupname(SystemName, wkrGuests);
    3: edtSIDName.Text := LookupGroupname(SystemName, wkrPowerUsers);
    4: edtSIDName.Text := LookupGroupname(SystemName, wkrBackupOPs);
    5: edtSIDName.Text := LookupGroupname(SystemName, wkrReplicator);
    6: edtSIDName.Text := LookupGroupname(SystemName, wkrEveryone);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edtSystemName.Text := GetLocalComputerName;
end;

procedure TForm1.btnDeleteUserClick(Sender: TObject);
begin
  DeleteLocalAccount(edtUserName.Text);
end;

procedure TForm1.btnAddGroupClick(Sender: TObject);
begin
  if CreateLocalGroup('', edtGroupName.Text, edtGroupComment.Text) then
    ShowMessage('success')
  else
    SHowMessage('failure');
end;

procedure TForm1.btnDeleteGroupClick(Sender: TObject);
begin
  DeleteLocalGroup('', edtGroupName.Text);
end;

end.
