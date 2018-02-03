program VerInfoExample;

{$I jcl.inc}

uses
  Forms,
  VerInfoDemoMain in 'VerInfoDemoMain.pas' {Form1};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
