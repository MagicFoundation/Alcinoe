program MapiExample;

{$I jcl.inc}

uses
  Forms,
  MapiDemoMain in 'MapiDemoMain.pas' {MainForm};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
