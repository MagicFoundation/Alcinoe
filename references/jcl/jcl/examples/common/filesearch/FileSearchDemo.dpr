program FileSearchDemo;

{$I jcl.inc}

uses
  Forms,
  FileSearchDemoMain in 'FileSearchDemoMain.pas' {FileSearchForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TFileSearchForm, FileSearchForm);
  Application.Run;
end.
