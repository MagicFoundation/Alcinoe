program ConditionParser;

uses
  Forms,
  ConditionParserMain in 'ConditionParserMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
