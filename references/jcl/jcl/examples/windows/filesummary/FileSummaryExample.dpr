program FileSummaryExample;

{$I jcl.inc}

uses
  Forms,
  FileSummaryDemoMain in 'FileSummaryDemoMain.pas' {FormMain};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
