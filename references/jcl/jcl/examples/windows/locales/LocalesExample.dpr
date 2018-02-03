program LocalesExample;

{$I jcl.inc}

uses
  Forms,
  LocalesDemoMain in 'LocalesDemoMain.pas' {MainForm};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
