program ToolHelpViewer;

{$I jcl.inc}

uses
  Forms,
  SysUtils,
  JclAppInst,
  Main in 'Main.pas' {MainForm},
  ChangePriority in 'ChangePriority.pas' {ChangePriorityDlg},
  HeapDump in 'HeapDump.pas' {HeapDumpForm},
  MemoryDump in 'MemoryDump.pas' {MemoryDumpForm},
  Global in 'Global.pas' {GlobalModule: TDataModule},
  ViewTemplate in 'ViewTemplate.pas' {ViewForm},
  ModulesDump in 'ModulesDump.pas' {ModulesDumpForm},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  About in '..\Common\About.pas' {AboutBox},
  FindDlg in '..\Common\FindDlg.pas' {FindForm},
  ExceptDlg in '..\..\..\..\experts\repository\ExceptionDialog\StandardDialogs\ExceptDlg.pas' {ExceptionDialog};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  try
    JclAppInstances.CheckSingleInstance;
    Application.Initialize;
    Application.Title := 'ToolHelp Viewer';
    Application.CreateForm(TGlobalModule, GlobalModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  except // fix for Delphi 5's RTL bug 
    SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end.
