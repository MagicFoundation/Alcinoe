object TuningDialog: TTuningDialog
  Left = 245
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Tuning'
  ClientHeight = 177
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 297
    Height = 117
    Shape = bsFrame
  end
  object MIDIFreqLabel: TLabel
    Left = 36
    Top = 56
    Width = 110
    Height = 13
    Caption = '&MIDI relative frequency'
  end
  object FreqLabel: TLabel
    Left = 36
    Top = 84
    Width = 84
    Height = 13
    Caption = 'Frequency [Hertz]'
  end
  object MIDIKeyLabel: TLabel
    Left = 36
    Top = 28
    Width = 81
    Height = 13
    Caption = 'MIDI key number'
  end
  object NoteLabel: TLabel
    Left = 224
    Top = 28
    Width = 49
    Height = 13
    Caption = 'NoteLabel'
  end
  object OKBtn: TButton
    Left = 79
    Top = 140
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 159
    Top = 140
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object MIDIFreq: TEdit
    Left = 156
    Top = 52
    Width = 121
    Height = 21
    TabOrder = 2
    OnChange = MIDIFreqChange
    OnExit = MIDIFreqExit
  end
  object FreqHertz: TEdit
    Left = 156
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = FreqHertzChange
    OnExit = FreqHertzExit
  end
  object MIDIKey: TSpinEdit
    Left = 156
    Top = 24
    Width = 53
    Height = 22
    MaxValue = 127
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = MIDIKeyChange
  end
end
