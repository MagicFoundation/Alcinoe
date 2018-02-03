program ScreenJPG;

{$I jcl.inc}

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  About in '..\Common\About.pas' {AboutBox},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  ExceptDlg in '..\..\..\..\experts\repository\ExceptionDialog\StandardDialogs\ExceptDlg.pas' {ExceptionDialog};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'ScreenJPG';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
