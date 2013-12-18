program UIBMonitor;

uses
  Forms,
  main in 'main.pas' {MainForm},
  ThreadAppLoader in 'ThreadAppLoader.pas',
  SharedHook in 'SharedHook.pas',
  avl in 'avl.pas',
  TrayIcon in 'TrayIcon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'UIB SQL Monitor';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
