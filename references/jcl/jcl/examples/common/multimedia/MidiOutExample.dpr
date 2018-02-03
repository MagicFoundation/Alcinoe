program MidiOutExample;

uses
  Forms,
  MidiOutExampleMain in 'MidiOutExampleMain.pas' {Keyboard},
  MidiOutExampleTuningDlg in 'MidiOutExampleTuningDlg.pas' {TuningDialog};

{$R *.RES}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TKeyboard, Keyboard);
  Application.CreateForm(TTuningDialog, TuningDialog);
  Application.Run;
end.
