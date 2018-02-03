program JclDFMTest;

uses
  Forms,
  JclDFMTestMain in 'JclDFMTestMain.pas' {fmJclDFMTest};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmJclDFMTest, fmJclDFMTest);
  Application.Run;
end.
