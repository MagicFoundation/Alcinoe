program JclDFMTest;

uses
  QForms,
  JclDFMTestMain in 'JclDFMTestMain.pas' {fmJclDFMTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmJclDFMTest, fmJclDFMTest);
  Application.Run;
end.
