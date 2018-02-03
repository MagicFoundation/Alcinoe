program ExprEvalExample;

{$I jcl.inc}

uses
  Forms,
  ExprEvalExampleMain in 'ExprEvalExampleMain.pas' {ExprEvalForm},
  JclExprEval in '..\..\..\source\common\JclExprEval.pas',
  JclStrHashMap in '..\..\..\source\common\JclStrHashMap.pas',
  ExprEvalExampleLogic in 'ExprEvalExampleLogic.pas';

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TExprEvalForm, ExprEvalForm);
  Application.Run;
end.
