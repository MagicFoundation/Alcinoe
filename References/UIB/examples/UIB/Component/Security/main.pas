unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QGrids, QDialogs, 
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes, uib;


type
  TMainForm = class(TForm)
    Add: TButton;
    Security: TUIBSecurity;
    Grid: TStringGrid;
    Edit: TButton;
    Delete: TButton;
    List: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CheckUserSelected;
  end;

var
  MainForm: TMainForm;

implementation

uses user;

{$R *.dfm}

const
  colUserName = 0;
  colFirstName = 1;
  colMiddleName = 2;
  colLastName = 3;
  colUserID = 4;
  colGroupID = 5;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  with Grid do
  begin
    ColCount := colGroupID + 1;
    Cells[colUserName, 0] := 'User Name';
    Cells[colFirstName, 0] := 'First Name';
    Cells[colMiddleName, 0] := 'Middle Name';
    Cells[colLastName, 0] := 'Last Name';
    Cells[colUserID, 0] := 'User ID';
    Cells[colGroupID, 0] := 'Group ID';
  end;
  ListClick(Self);
end;

procedure TMainForm.ListClick(Sender: TObject);
var
  I : Integer;
begin
  Security.DisplayUsers;
  with Grid do
  begin
    RowCount := Security.UserInfoCount + 1;
    for I := 1 to RowCount - 1 do
      with Security.UserInfo[I - 1] do
      begin
        Cells[colUserName, I] := UserName;
        Cells[colFirstName, I] := FirstName;
        Cells[colMiddleName, I] := MiddleName;
        Cells[colLastName, I] := LastName;
        Cells[colUserID, I] := InttoStr(UserID);
        Cells[colGroupID, I] := InttoStr(GroupID);
      end;
  end;
end;

procedure TMainForm.AddClick(Sender: TObject);
var
  Temp : Integer;
begin
  with UserForm do
  begin
    Caption := 'Add New User';
    UserName.Text := '';
    UserName.Enabled := True;
    FirstName.Text := '';
    MiddleName.Text := '';
    LastName.Text := '';
    UserID.Text := '0';
    GroupID.Text := '0';
    Password.Text := '';
    Confirm.Text := '';
    if ShowModal = mrOK then
    begin
      Security.User := UserName.Text;
      Security.FirstName := FirstName.Text;
      Security.MiddleName := MiddleName.Text;
      Security.LastName := LastName.Text;
      if TryStrToInt(UserID.Text, Temp) then
        Security.UserID := Temp;
      if TryStrToInt(GroupID.Text, Temp) then
        Security.GroupID := Temp;
      Security.Pass := Password.Text;
      Security.AddUser;
      ListClick(Self);
    end;
  end;
end;

procedure TMainForm.CheckUserSelected;
begin
  if (Grid.Row < 1) or (Grid.Row >= Grid.RowCount) or
     (Grid.Cells[colUserName, Grid.Row] = '') then
    raise Exception.Create('Please select a user to edit/delete');
end;

procedure TMainForm.EditClick(Sender: TObject);
var
  Row, Temp : Integer;
const
  DummyPassword = 'dummypassword';
begin
  CheckUserSelected;
  with UserForm do
  begin
    Caption := 'Edit User';
    Row := Grid.Row;
    UserName.Text := Grid.Cells[colUserName, Row];
    UserName.Enabled := False;
    FirstName.Text := Grid.Cells[colFirstName, Row];
    MiddleName.Text := Grid.Cells[colMiddleName, Row];
    LastName.Text := Grid.Cells[colLastName, Row];
    UserID.Text := Grid.Cells[colUserID, Row];;
    GroupID.Text := Grid.Cells[colGroupID, Row];;
    Password.Text := DummyPassword;
    Confirm.Text := DummyPassword;

    { this program won't allow you to change SYSDBA password for security reasons! }
    { remove lines below only if you are aware of what you do! }
    Password.Enabled := not SameText(UserName.Text, 'SYSDBA');
    Confirm.Enabled := Password.Enabled;

    if ShowModal = mrOK then
    begin
      Security.User := UserName.Text;
      if FirstName.Modified then
        Security.FirstName := FirstName.Text;
      if LastName.Modified then
        Security.LastName := LastName.Text;
      if MiddleName.Modified then
        Security.MiddleName := MiddleName.Text;
      if UserID.Modified and TryStrToInt(UserID.Text, Temp) then
        Security.UserID := Temp;
      if GroupID.Modified and TryStrToInt(GroupID.Text, Temp) then
        Security.GroupID := Temp;
      if Password.Modified and (Password.Text = Confirm.Text) and
         (Password.Text <> DummyPassword) then
        Security.Pass := Password.Text;
      Security.ModifyUser;
      ListClick(Self);
    end;
  end;

end;

procedure TMainForm.DeleteClick(Sender: TObject);
const
  Confirm = 'Are you sure that you want to delete %s?';
begin
  CheckUserSelected;
  Security.User := Grid.Cells[colUserName, Grid.Row];

  { SYSDBA can't be deleted with this program for security reasons }
  if not SameText(Security.User, 'SYSDBA') then
  begin
    if MessageDlg(Format(Confirm, [Security.User]),
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Security.DeleteUser;
      ListClick(Self);
    end;
  end;
end;

end.
