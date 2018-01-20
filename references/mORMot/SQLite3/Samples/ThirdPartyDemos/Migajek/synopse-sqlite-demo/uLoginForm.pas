unit uLoginForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TLoginForm = class(TForm)
    Panel1: TPanel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure LabeledEdit2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LoginOk: boolean;
  end;

var
  LoginForm: TLoginForm;

implementation

uses SynCommons, uCustomer;

{$R *.dfm}

procedure TLoginForm.Button1Click(Sender: TObject);
begin
if (TSQLUser.SignIn(StringToUTF8(LabeledEdit1.Text), StringToUTF8(LabeledEdit2.Text)) > 0) then
  begin
    LoginOk:= true;
    Close();
  end
else
  MessageBox(0, 'Invalid login or password provided, or the account has expired', 'Login error', MB_ICONEXCLAMATION);
end;

procedure TLoginForm.LabeledEdit2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if Key = VK_RETURN then
  Button1Click(nil);
end;

procedure TLoginForm.FormCreate(Sender: TObject);
begin
LoginOk:= false;
end;

end.
