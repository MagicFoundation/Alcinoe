program mORMotDebugVisualizerTestAppdproj;

uses
  Vcl.Forms,
  mORMotVisualizerTesterMainForm in 'mORMotVisualizerTesterMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
