program ResFix;

{$I jcl.inc}

uses
  Forms,
  ResFixMain in 'ResFixMain.pas' {MainForm},
  About in '..\Common\About.pas' {AboutBox},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  ExceptDlg in '..\..\..\..\experts\repository\ExceptionDialog\StandardDialogs\ExceptDlg.pas' {ExceptionDialog};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'ResFix';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
