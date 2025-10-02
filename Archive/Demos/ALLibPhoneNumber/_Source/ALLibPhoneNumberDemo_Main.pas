unit ALLibPhoneNumberDemo_Main;

interface

uses
  Alcinoe.LibPhoneNumber, Alcinoe.StringUtils, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
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

{*********************************************}
procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit2.Text := string(ALIntToStrA(ALStrPhoneNumberToInt64(ansiString(Edit1.Text), ansiString(Edit3.Text))));
  Edit4.Text := Edit2.Text;
end;

{*********************************************}
procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit2.Text := string(ALInt64PhoneNumberToStr(ALStrToInt64(ansiString(Edit4.Text))));
end;

{*********************************************}
procedure TForm1.Button3Click(Sender: TObject);
begin
  case ALGetPhoneNumberType(ALStrToInt64(ansiString(Edit4.Text))) of
    cALFixedLine: Edit2.Text := 'Landing line';
    cALMobile: Edit2.Text := 'Mobile';
    cALFixedLineOrMobil: Edit2.Text := 'Mobile or Landing (mostly for US)';
    cALTollFree: Edit2.Text := 'TOLL-FREE';
    cALPremiumRate: Edit2.Text := 'Premium rate';
    cALSharedCost: Edit2.Text := 'Shared cost';
    cALVoIP: Edit2.Text := 'Voice over IP';
    cALPersonalNumber: Edit2.Text := 'Personal Number';
    cALPager: Edit2.Text := 'Pager';
    cALUAN: Edit2.Text := 'UAN (Universal Access Numbers)';
    cALVoiceMail: Edit2.Text := 'Voice Mail';
    cALUnknown: Edit2.Text := 'Unknown';
  end;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
