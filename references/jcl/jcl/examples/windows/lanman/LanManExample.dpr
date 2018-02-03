program LanManExample;

{$I jcl.inc}

uses
  Forms,
  LanManDemoMain in 'LanManDemoMain.pas' {Form1};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAdmin.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
