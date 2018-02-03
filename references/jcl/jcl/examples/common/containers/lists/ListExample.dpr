program ListExample;

{$I jcl.inc}

uses
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Vcl.Forms,
  {$ENDIF MSWINDOWS}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  ListExampleMain in 'ListExampleMain.pas' {MainForm},
  MyObjectList in 'MyObjectList.pas';

{$R *.res}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
