program PeFuncExample;

{$I jcl.inc}

uses
  Forms,
  PeFuncDemoMain in 'PeFuncDemoMain.pas' {Form1};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
