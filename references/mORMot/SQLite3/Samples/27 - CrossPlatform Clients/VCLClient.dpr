program VCLClient;

uses
  Forms,
  VCLMain in 'VCLMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
