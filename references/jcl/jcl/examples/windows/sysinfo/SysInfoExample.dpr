program SysInfoExample;

{$I jcl.inc}

uses
  Forms,
  SysInfoDemoMain in 'SysInfoDemoMain.pas' {MainForm};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
