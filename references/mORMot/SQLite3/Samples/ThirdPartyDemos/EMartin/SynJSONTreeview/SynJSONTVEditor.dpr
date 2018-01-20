program SynJSONTVEditor;

uses
  Forms,
  fMain in 'fMain.pas' {frmJSONEditor},
  SynJSONTreeView in 'SynJSONTreeView.pas',
  fLevel in 'fLevel.pas' {frmLevel};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmJSONEditor, frmJSONEditor);
  Application.Run;
end.
