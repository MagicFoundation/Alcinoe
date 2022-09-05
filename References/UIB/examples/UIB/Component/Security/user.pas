unit user;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QGrids, QDialogs, 
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes;

type
  TUserForm = class(TForm)
    Label1: TLabel;
    UserName: TEdit;
    Label2: TLabel;
    FirstName: TEdit;
    Label3: TLabel;
    MiddleName: TEdit;
    Label4: TLabel;
    LastName: TEdit;
    Label5: TLabel;
    UserID: TEdit;
    Label6: TLabel;
    GroupID: TEdit;
    Label7: TLabel;
    Password: TEdit;
    Label8: TLabel;
    Confirm: TEdit;
    Ok: TButton;
    Cancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UserForm: TUserForm;

implementation

{$R *.dfm}

procedure TUserForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    if Password.Text <> Confirm.Text then
      raise Exception.Create('Passwords don''t match!');
  end;
end;

end.
