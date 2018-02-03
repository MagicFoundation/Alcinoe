program AlgorithmsExample;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
		{$IFDEF SUPPORTS_NAMESPACES}
  Vcl.Forms,
		{$ELSE}
  Forms,
		{$ENDIF SUPPORTS_NAMESPACES}
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  AlgorithmsExampleMain in 'AlgorithmsExampleMain.pas' {MainForm};

{$R *.res}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
