program CodeProfiler;

uses
  Alcinoe.CodeProfiler,
  Forms,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  ALCodeProfilerStop;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.