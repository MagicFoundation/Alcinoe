//
//   Robert Rossmair, 2002
//
unit MidiOutExampleTuningDlg;

{$I jcl.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Spin,
  JclMath, JclMidi;

type
  TTuningDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    MIDIFreq: TEdit;
    FreqHertz: TEdit;
    MIDIFreqLabel: TLabel;
    FreqLabel: TLabel;
    MIDIKey: TSpinEdit;
    MIDIKeyLabel: TLabel;
    NoteLabel: TLabel;
    procedure MIDIKeyChange(Sender: TObject);
    procedure MIDIFreqChange(Sender: TObject);
    procedure FreqHertzChange(Sender: TObject);
    procedure MIDIFreqExit(Sender: TObject);
    procedure FreqHertzExit(Sender: TObject);
  private
    FInMIDIFreqChange: Boolean;
    FInFreqHertzChange: Boolean;
    FChangingFrequency: Boolean;
    FChangingMidiFrequency: Boolean;
    FFrequency: Single;
    FMidiFrequency: Single;
    procedure SetFrequency(Value: Single);
    procedure SetMidiFrequency(Value: Single);
  public
    property Frequency: Single read FFrequency write SetFrequency; // Hertz
    property MidiFrequency: Single read FMidiFrequency write SetMidiFrequency;
  end;

var
  TuningDialog: TTuningDialog;

implementation

{$R *.dfm}

const
  HalftonesPerOctave = 12;
  MiddleA            = 440.0; // Hertz
  MidiMiddleA        = 69;    // A3 = 440 Hertz
  Digits = 6;
  MIDIFreqMax = 127.99993896;
  FreqHertzMin = 8.17579892;
  FreqHertzMax = 13289.70346552;

function Hertz(MIDINote: Extended): Extended;
begin
  Hertz := TwoToY((MIDINote - MidiMiddleA) / HalftonesPerOctave) * MiddleA;
end;

function MIDINote(Hertz: Extended): Extended;
begin
  if Hertz < 1.0 then
    MIDINote := Low(Integer)
  else
    MIDINote := LogBase2(Hertz / MiddleA) * HalftonesPerOctave + MidiMiddleA;
end;

procedure TTuningDialog.MIDIKeyChange(Sender: TObject);
begin
  MIDIFrequency := MIDIKey.Value;
  NoteLabel.Caption := MidiNoteToStr(MIDIKey.Value);
end;

procedure TTuningDialog.MIDIFreqChange(Sender: TObject);
var
  F: Extended;
begin
  if FInFreqHertzChange or (MIDIFreq.Text = '') then
    Exit;
  FInMIDIFreqChange := True;
  try
    {$IFDEF COMPILER6_UP}
    if TryStrToFloat(MidiFreq.Text, F) then
    {$ELSE}
    if TextToFloat(PChar(MidiFreq.Text), F, fvExtended) then
    {$ENDIF COMPILER6_UP}
      MidiFrequency := F;
  finally
    FInMIDIFreqChange := False;
  end;
end;

procedure TTuningDialog.FreqHertzChange(Sender: TObject);
var
  F: Extended;
begin
  if FInMIDIFreqChange or (FreqHertz.Text = '') then
    Exit;
  FInFreqHertzChange := True;
  try
    {$IFDEF COMPILER6_UP}
    if TryStrToFloat(FreqHertz.Text, F) then
    {$ELSE}
    if TextToFloat(PChar(FreqHertz.Text), F, fvExtended) then
    {$ENDIF COMPILER6_UP}
      Frequency := F;
  finally
    FInFreqHertzChange := False;
  end;
end;

procedure TTuningDialog.SetFrequency(Value: Single);
begin
  if FChangingFrequency or (Value = Frequency) then
    Exit;
  FChangingFrequency := True;
  try
    if Value < FreqHertzMin then
      Value := FreqHertzMin
    else
    if Value > FreqHertzMax then
      Value := FreqHertzMax;
    FFrequency := Value;
    if not FInFreqHertzChange then
      FreqHertz.Text := FloatToStrF(Value, ffFixed, 9, Digits);
    MidiFrequency := MIDINote(Value);
  finally
    FChangingFrequency := False;
  end;
end;

procedure TTuningDialog.SetMidiFrequency(Value: Single);
begin
  if FChangingMidiFrequency then
  // or (Value = MidiFrequency) then
    Exit;
  if Value < 0 then
    Value := 0
  else
  if Value > MidiFreqMax then
    Value := MidiFreqMax;
  FChangingMidiFrequency := True;
  try
    FMidiFrequency := Value;
    if not FInMidiFreqChange then
      MIDIFreq.Text := FloatToStrF(Value, ffFixed, 9, Digits);
    Frequency := Hertz(Value);
  finally
    FChangingMidiFrequency := False;
  end;
end;

procedure TTuningDialog.MIDIFreqExit(Sender: TObject);
begin
  MIDIFreq.Text := FloatToStrF(MidiFrequency, ffFixed, 9, Digits);
end;

procedure TTuningDialog.FreqHertzExit(Sender: TObject);
begin
  FreqHertz.Text := FloatToStrF(Frequency, ffFixed, 9, Digits);
end;

end.
