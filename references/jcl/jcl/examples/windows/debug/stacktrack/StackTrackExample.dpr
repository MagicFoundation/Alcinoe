program StackTrackExample;

{$I jcl.inc}

uses
  Forms,
  StackTrackDemoMain in 'StackTrackDemoMain.pas' {MainForm};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
