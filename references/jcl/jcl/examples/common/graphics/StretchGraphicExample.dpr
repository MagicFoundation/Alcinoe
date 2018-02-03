program StretchGraphicExample;

{$I jcl.inc}

uses
  Forms,
  StretchGraphicDemoMain in 'StretchGraphicDemoMain.pas' {StretchDemoForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TStretchDemoForm, StretchDemoForm);
  Application.Run;
end.
