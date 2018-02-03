program SingleInstExample;

{$I jcl.inc}

uses
  JclAppInst, // Added JclAppInst unit
  Forms,
  SingleInstDemoMain in 'SingleInstDemoMain.pas' {Form1};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  JclAppInstances.CheckSingleInstance; // Added instance checking
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
