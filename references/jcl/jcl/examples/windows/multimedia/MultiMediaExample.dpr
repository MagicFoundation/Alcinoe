program MultiMediaExample;

{$I jcl.inc}

uses
  Forms,
  MultimediaDemoMain in 'MultimediaDemoMain.pas' {MainForm};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
