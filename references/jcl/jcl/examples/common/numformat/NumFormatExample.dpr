program NumFormatExample;

{$I jcl.inc}

uses
  Forms,
  NumFormatExampleMain in 'NumFormatExampleMain.pas' {MainForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
