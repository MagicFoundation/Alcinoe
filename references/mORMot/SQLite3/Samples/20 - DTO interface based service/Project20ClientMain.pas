unit Project20ClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCommons, mORMot, mORMotHttpClient,
  Project20Interface;

type
  TForm1 = class(TForm)
    edtID: TEdit;
    lblA: TLabel;
    btnCall: TButton;
    btnCancel: TButton;
    mmoResult: TMemo;
    edtNumberOfCalls: TEdit;
    lbl1: TLabel;
    lblTiming: TLabel;
    edtServerName: TEdit;
    lbl2: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnCallClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
var id, numcalls, err, n: integer;
    Timer: TPrecisionTimer;
    I: IAirportService;
    Airport: TDTOAirportDefinition;
begin
  val(edtID.Text,id,err);
  if err<>0 then begin
    edtID.SetFocus;
    exit;
  end;
  val(edtNumberOfCalls.Text,numcalls,err);
  if (err<>0) or (numcalls<=0) then begin
    edtNumberOfCalls.SetFocus;
    exit;
  end;
  if Client=nil then
  try
    if Model=nil then
      Model := TSQLModel.Create([],ROOT_NAME);
    Client := TSQLHttpClient.Create(AnsiString(edtServerName.Text),'888',Model);
    if not Client.ServerTimeStampSynchronize then begin
      ShowMessage(UTF8ToString(Client.LastErrorMessage));
      FreeAndNil(Client);
      exit;
    end;
    Client.ServiceRegister([TypeInfo(IAirportService)],sicShared);
  except
    on Exception do begin
      FreeAndNil(Client);
      exit;
    end;
  end;
  if not Client.Services['AirportService'].Get(I) then
    exit;
  Airport := TDTOAirportDefinition.Create;
  try
    Timer.Start;
    for n := 1 to numcalls do
      I.GetAirportDefinition(id,Airport);
    lblTiming.Caption := Format('Total time: %s'#13'Average time: %s',
      [Timer.Stop,Timer.ByCount(numcalls)]);
    mmoResult.Text := UTF8ToString(ObjectToJSON(Airport,[woHumanReadable]));
  finally
    Airport.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Client.Free;
  Model.Free;
end;

end.
