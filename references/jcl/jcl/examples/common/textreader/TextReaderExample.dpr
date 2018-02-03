program TextReaderExample;

{$I jcl.inc}

uses
  Forms,
  TextReaderDemoMain in 'TextReaderDemoMain.pas' {MainForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
