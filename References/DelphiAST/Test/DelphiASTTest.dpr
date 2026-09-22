program DelphiASTTest;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {Form2};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
