program SourceLocExample;

{$I jcl.inc}

uses
  Forms,
  SourceLocDemoMain in 'SourceLocDemoMain.pas' {Form1};

{$R *.RES}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
