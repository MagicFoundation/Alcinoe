program DependView;

{$I jcl.inc}

uses
  Forms,
  SysUtils,
  D6MdiMsgFix in '..\Common\D6MdiMsgFix.pas',
  DependViewMain in 'DependViewMain.pas' {MainForm},
  FileViewer in 'FileViewer.pas' {FileViewerChild},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  About in '..\Common\About.pas' {AboutBox},
  FindDlg in '..\Common\FindDlg.pas' {FindTextForm},
  ExceptDlg in '..\..\..\..\experts\repository\ExceptionDialog\StandardDialogs\ExceptDlg.pas' {ExceptionDialog};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'Dependency Viewer';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
