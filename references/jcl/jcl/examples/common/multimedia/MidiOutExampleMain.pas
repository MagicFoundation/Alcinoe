//
// by Robert Rossmair, June 5 2002
//
unit MidiOutExampleMain;

interface

uses
  SysUtils, Classes, Controls, Forms, Menus, StdCtrls, ComCtrls, Buttons, Spin,
  JclMIDI;

type
  TKeyboard = class(TForm)
    Key48: TSpeedButton;
    Key49: TSpeedButton;
    Key51: TSpeedButton;
    Key50: TSpeedButton;
    Key55: TSpeedButton;
    Key54: TSpeedButton;
    Key53: TSpeedButton;
    Key52: TSpeedButton;
    Key58: TSpeedButton;
    Key56: TSpeedButton;
    Key59: TSpeedButton;
    Key57: TSpeedButton;
    MidiProgramNum: TSpinEdit;
    Label1: TLabel;
    KeyMenu: TPopupMenu;
    TuningItem: TMenuItem;
    Key72: TSpeedButton;
    Key74: TSpeedButton;
    Key76: TSpeedButton;
    Key77: TSpeedButton;
    Key73: TSpeedButton;
    Key75: TSpeedButton;
    Key79: TSpeedButton;
    Key81: TSpeedButton;
    Key83: TSpeedButton;
    Key78: TSpeedButton;
    Key80: TSpeedButton;
    Key82: TSpeedButton;
    Key60: TSpeedButton;
    Key62: TSpeedButton;
    Key64: TSpeedButton;
    Key65: TSpeedButton;
    Key61: TSpeedButton;
    Key63: TSpeedButton;
    Key67: TSpeedButton;
    Key69: TSpeedButton;
    Key71: TSpeedButton;
    Key66: TSpeedButton;
    Key68: TSpeedButton;
    Key70: TSpeedButton;
    PitchBender: TTrackBar;
    btnAllNotesOff: TButton;
    cbMidiOutSelect: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ModWheel: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure KeyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure KeyMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MidiProgramNumChange(Sender: TObject);
    procedure TuningItemClick(Sender: TObject);
    procedure PitchBenderChange(Sender: TObject);
    procedure KeyClick(Sender: TObject);
    procedure btnAllNotesOffClick(Sender: TObject);
    procedure cbMidiOutSelectChange(Sender: TObject);
    procedure ModWheelChange(Sender: TObject);
  private
    FMidiOut: IJclMidiOut;
    FChannel: TMidiChannel;
    Keys: array[TMidiNote] of TSpeedButton;
    procedure InitKeyboard;
    procedure AllNotesOff;
  end;

var
  Keyboard: TKeyboard;

implementation

uses MidiOutExampleTuningDlg;

{$R *.dfm}

procedure TKeyboard.FormCreate(Sender: TObject);
begin
  FChannel := 1;
  GetMidiOutputs(cbMidiOutSelect.Items);
  cbMidiOutSelect.ItemIndex := 0;
  cbMidiOutSelectChange(Self);
  InitKeyboard;
end;

procedure TKeyboard.InitKeyboard;
var
  Note: TMidiNote;
begin
  for Note := Low(Keys) to High(Keys) do
  begin
    Keys[Note] := FindComponent(Format('Key%d', [Note])) as TSpeedButton;
    if Keys[Note] <> nil then
      with Keys[Note] do
      begin
        PopupMenu := KeyMenu;
        Hint := Format('MIDI Note #%d'#13#10'%s', [Tag, MidiNoteToStr(Tag)]);
      end;
  end;
end;

procedure TKeyboard.AllNotesOff;
var
  Note: TMidiNote;
begin
  if Assigned(FMidiOut) then
    FMidiOut.SwitchAllNotesOff(FChannel);
  for Note := Low(Note) to High(Note) do
    if Assigned(Keys[Note]) then
      Keys[Note].Down := False;
end;

procedure TKeyboard.KeyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    if (Sender as TSpeedButton).Down then
      FMidiOut.SendNoteOff(FChannel, TComponent(Sender).Tag, 127)
    else
      FMidiOut.SendNoteOn(FChannel, TComponent(Sender).Tag, 127);
end;

procedure TKeyboard.KeyMouseUp(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FMidiOut.SendNoteOff(FChannel, TComponent(Sender).Tag, 127);
end;

procedure TKeyboard.MidiProgramNumChange(Sender: TObject);
begin
  FMidiOut.SendProgramChange(FChannel, MidiProgramNum.Value);
end;

procedure TKeyboard.TuningItemClick(Sender: TObject);
begin
  with TuningDialog do
  begin
    MIDIKey.Value := KeyMenu.PopupComponent.Tag;
    if ShowModal = mrOK then
      FMidiOut.SendSingleNoteTuningChange(0, 0, [MidiSingleNoteTuningData(MIDIKey.Value, MIDIFrequency)]);
  end;
end;

procedure TKeyboard.PitchBenderChange(Sender: TObject);
begin
  FMidiOut.SendPitchWheelChange(FChannel, PitchBender.Position + MidiPitchWheelCenter);
end;

procedure TKeyboard.ModWheelChange(Sender: TObject);
begin
  FMidiOut.SendModulationWheelChangeHR(FChannel, ModWheel.Position);
end;

procedure TKeyboard.KeyClick(Sender: TObject);
begin
  with Sender as TSpeedButton do
  begin
    if Down then
      FMidiOut.SendNoteOn(FChannel, TComponent(Sender).Tag, 127)
    else
      FMidiOut.SendNoteOff(FChannel, TComponent(Sender).Tag, 127);
  end;
end;

procedure TKeyboard.btnAllNotesOffClick(Sender: TObject);
begin
  AllNotesOff;
end;

procedure TKeyboard.cbMidiOutSelectChange(Sender: TObject);
begin
  AllNotesOff;
  FMidiOut := MidiOut(cbMidiOutSelect.ItemIndex);
  FMidiOut.SendProgramChange(FChannel, MidiProgramNum.Value);
end;

end.

