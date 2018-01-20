program ORMCDS_Test;

uses
  FastMM4,
  Vcl.Forms,
  MAinFormU in 'MAinFormU.pas' {MainForm},
  ORMCDS in 'ORMCDS.pas',
  SampleForm1U in 'SampleForm1U.pas' {SampleForm1},
  SampleForm2U in 'SampleForm2U.pas' {SampleForm2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
