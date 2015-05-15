unit ALLibPhoneNumberDemo_Main;

interface

uses
  ALLibPhoneNumber, ALString, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Button1: TButton;
    GroupBox2: TGroupBox;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    GroupBox3: TGroupBox;
    Edit4: TEdit;
    Label4: TLabel;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit2.Text := string(ALIntToStr(ALStrPhoneNumberToInt64(Edit1.Text, Edit3.Text)));
  Edit4.Text := Edit2.Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit2.Text := string(ALInt64PhoneNumberToStr(ALStrToInt64(Edit4.Text)));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  case ALGetPhoneNumberType(ALStrToInt64(Edit4.Text)) of
    0: Edit2.Text := 'Landing line';
    1: Edit2.Text := 'Mobile';
    2: Edit2.Text := 'Mobile or Landing (mostly for US)';
    3: Edit2.Text := 'TOLL-FREE';
    4: Edit2.Text := 'Premium rate';
    5: Edit2.Text := 'Shared cost';
    6: Edit2.Text := 'Voice over IP';
    7: Edit2.Text := 'Personal Number';
    8: Edit2.Text := 'Pager';
    9: Edit2.Text := 'UAN (Universal Access Numbers)';
    10: Edit2.Text := 'Voice Mail';
    11: Edit2.Text := 'Unknown';
  end;
end;

end.
