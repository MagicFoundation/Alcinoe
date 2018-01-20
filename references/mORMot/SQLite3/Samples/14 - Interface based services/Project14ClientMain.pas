unit Project14ClientMain;

{

 By definition, you need the proper server to run:
 - Project14Server.dpr for Named Pipes
 - Project14ServerHttp.dpr for HTTP
 - Project14ServerHttpWeak.dpr for HTTP/weak

}

interface

uses
  {$IFDEF WINDOWS}  Windows, Messages, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, mORMot, mORMotHttpClient,
  Project14Interface;

type
  TForm1 = class(TForm)
    edtA: TEdit;
    edtB: TEdit;
    lblA: TLabel;
    lblB: TLabel;
    btnCall: TButton;
    btnCancel: TButton;
    lblResult: TLabel;
    ComboProtocol: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCallClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboProtocolChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Model: TSQLModel;
    Client: TSQLRestClientURI;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$ifndef FPC}
{$R Vista.res}
{$endif}

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnCallClick(Sender: TObject);
var a,b: integer;
    err: integer;
    I: ICalculator;
begin
  val(edtA.Text,a,err);
  if err<>0 then begin
    edtA.SetFocus;
    exit;
  end;
  val(edtB.Text,b,err);
  if err<>0 then begin
    edtB.SetFocus;
    exit;
  end;
  if Client=nil then begin
    if Model=nil then
      Model := TSQLModel.Create([],ROOT_NAME);
    case ComboProtocol.ItemIndex of
    0,2: Client := TSQLHttpClient.Create('localhost', PORT_NAME,Model);
    {$IFDEF WINDOWS}
    1: Client := TSQLRestClientURINamedPipe.Create(Model,APPLICATION_NAME);
    {$ENDIF}
    else exit;
    end;
    if not Client.ServerTimeStampSynchronize then begin
      ShowMessage(UTF8ToString(Client.LastErrorMessage));
      exit;
    end;
    case ComboProtocol.ItemIndex of
    2:   TSQLRestServerAuthenticationNone.ClientSetUser(Client,'User','');
    else Client.SetUser('User','synopse');
    end;
    Client.ServiceDefine([ICalculator],sicShared);
  end;
  if Client.Services['Calculator'].Get(I) then
    lblResult.Caption := IntToStr(I.Add(a,b));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;
end;

procedure TForm1.ComboProtocolChange(Sender: TObject);
begin
  FreeAndNil(Client);
end;

end.
