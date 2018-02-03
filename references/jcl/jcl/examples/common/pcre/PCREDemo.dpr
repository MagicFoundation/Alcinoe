program PCREDemo;

{$I jcl.inc}

uses
  Forms,
  PCREDemoMain in 'PCREDemoMain.pas' {frmMain};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'JclPCRE Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
