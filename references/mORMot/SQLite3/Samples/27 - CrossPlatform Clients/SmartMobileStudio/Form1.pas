unit Form1;

interface

uses 
  SmartCL.System, SmartCL.Graphics, SmartCL.Components, SmartCL.Forms, 
  SmartCL.Fonts, SmartCL.Borders, SmartCL.Application, SmartCL.Controls.Panel,
  SmartCL.Controls.Label, SmartCL.Controls.EditBox, SmartCL.Controls.Button,
  SynCrossPlatformREST, mORMotClient;

type
  TForm1 = class(TW3Form)
    procedure BtnComputeSynchClick(Sender: TObject);
    procedure BtnComputeAsynchClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
  private
    {$I 'Form1:intf'}
  protected
    Client: TSQLRestClientURI;
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

{ TForm1 }

procedure TForm1.BtnConnectClick(Sender: TObject);
begin
  if Client=nil then
    GetClient('127.0.0.1','User','synopse',
      lambda (aClient: TSQLRestClientURI)
        PanelCompute.Visible := true;
        W3Label1.Visible := true;
        W3Label2.Visible := true;
        LabelConnect.Caption := '';
        BtnConnect.Caption := 'Disconnect';
        LabelResult.Caption := '';
        Client := aClient;
      end,
      lambda
        ShowMessage('Impossible to connect to the server!');
      end)
  else begin
    PanelCompute.Visible := false;
    BtnConnect.Caption := 'Server Connect';
    Client.Free;
    Client := nil;
  end;
end;

procedure TForm1.BtnComputeAsynchClick(Sender: TObject);
begin
  TServiceCalculator.Create(Client).Add(
    StrToInt(EditA.Text),StrToInt(EditB.Text),
    lambda (res: integer)
      LabelResult.Caption := format('Result = %d',[res]);
    end,
    lambda
      ShowMessage('Error calling the method!');
    end);
end;

procedure TForm1.BtnComputeSynchClick(Sender: TObject);
begin
  LabelResult.Caption := format('Result = %d',
    [TServiceCalculator.Create(Client)._Add(
      StrToInt(EditA.Text),StrToInt(EditB.Text))]);
end;

procedure TForm1.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
  EditA.InputType := itNumber;
  EditB.InputType := itNumber;
end;

procedure TForm1.InitializeObject;
begin
  inherited;
  {$I 'Form1:impl'}
end;
 
procedure TForm1.Resize;
begin
  inherited;
end;
 
initialization
  Forms.RegisterForm({$I %FILE%}, TForm1);
end.