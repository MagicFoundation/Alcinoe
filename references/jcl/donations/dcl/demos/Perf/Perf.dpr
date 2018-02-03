program Perf;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmPerf};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPerf, frmPerf);
  Application.Run;
end.
