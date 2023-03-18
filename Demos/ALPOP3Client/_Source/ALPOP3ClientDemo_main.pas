unit ALPOP3ClientDemo_main;

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
     ComCtrls,
     Alcinoe.POP3.Client;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    MemoDebug: TMemo;
    Label1: TLabel;
    UsernameEdit: TEdit;
    PasswordEdit: TEdit;
    Label2: TLabel;
    LoginButton: TButton;
    Label3: TLabel;
    HostEdit: TEdit;
    Label4: TLabel;
    PortEdit: TEdit;
    ConnectButton: TButton;
    StatButton: TButton;
    ListButton: TButton;
    QuitButton: TButton;
    RsetButton: TButton;
    NumberEdit: TEdit;
    Label5: TLabel;
    RetrButton: TButton;
    DeleButton: TButton;
    UIDLButton: TButton;
    procedure LoginButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure StatButtonClick(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure RsetButtonClick(Sender: TObject);
    procedure RetrButtonClick(Sender: TObject);
    procedure DeleButtonClick(Sender: TObject);
    procedure UIDLButtonClick(Sender: TObject);
  private
    { Private declarations }
    FPOP3Client: TALPOP3Client;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Alcinoe.StringUtils;

{$R *.dfm}

{***************************************************}
procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  if HostEdit.Text = '' then raise Exception.Create('Host cannot be empty')
  else begin
    if PortEdit.Text = '' then raise Exception.Create('Port cannot be empty')
    else MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Connect(AnsiString(HostEdit.Text), StrToInt(PortEdit.Text)))));
  end;
end;

{************************************************}
procedure TForm1.DeleButtonClick(Sender: TObject);
var P1: integer;
begin
  if NumberEdit.Text = '' then raise Exception.Create('Number field must be filled');
  if TryStrToInt(NumberEdit.Text, P1) then MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Dele(P1))))
  else raise Exception.Create('Number must be integer');
end;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  FPOP3Client := TALPOP3Client.Create;
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPOP3Client.Free;
end;

{************************************************}
procedure TForm1.ListButtonClick(Sender: TObject);
begin
  MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.List)));
end;

{*************************************************}
procedure TForm1.LoginButtonClick(Sender: TObject);
begin
  if UsernameEdit.Text = '' then raise Exception.Create('Username cannot be empty')
  else begin
    if PasswordEdit.Text = '' then raise Exception.Create('Password cannot be empty')
    else begin
      MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.User(AnsiString(UserNameEdit.Text)))));
      MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Pass(AnsiString(PasswordEdit.Text)))));
    end;
  end;
end;

{************************************************}
procedure TForm1.QuitButtonClick(Sender: TObject);
begin
  MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Quit)));
end;

{************************************************}
procedure TForm1.RetrButtonClick(Sender: TObject);
var P1: integer;
begin
  if NumberEdit.Text = '' then raise Exception.Create('Number field must be filled');
  if TryStrToInt(NumberEdit.Text, P1) then MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Retr(P1))))
  else raise Exception.Create('Number must be integer');
end;

{************************************************}
procedure TForm1.RsetButtonClick(Sender: TObject);
begin
  MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Rset)));
end;

{************************************************}
procedure TForm1.StatButtonClick(Sender: TObject);
begin
  MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.Stat)));
end;

{************************************************}
procedure TForm1.UIDLButtonClick(Sender: TObject);
begin
  MemoDebug.Lines.Add(String(ALTrim(FPOP3Client.UIDL)));
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
