object Form1: TForm1
  Left = 438
  Top = 259
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 173
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonAnsiToAnsi: TButton
    Left = 16
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Ansi --> Ansi'
    Enabled = False
    TabOrder = 0
  end
  object ButtonAnsiToUTF8: TButton
    Left = 136
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Ansi --> UTF8'
    TabOrder = 1
    OnClick = ButtonAnsiToUTF8Click
  end
  object ButtonAnsiToUTF16: TButton
    Left = 256
    Top = 16
    Width = 105
    Height = 25
    Caption = 'Ansi --> UTF16'
    TabOrder = 2
    OnClick = ButtonAnsiToUTF16Click
  end
  object ButtonUTF8ToAnsi: TButton
    Left = 16
    Top = 56
    Width = 105
    Height = 25
    Caption = 'UTF8 --> Ansi'
    TabOrder = 3
    OnClick = ButtonUTF8ToAnsiClick
  end
  object ButtonUTF8ToUTF8: TButton
    Left = 136
    Top = 56
    Width = 105
    Height = 25
    Caption = 'UTF8 --> UTF8'
    Enabled = False
    TabOrder = 4
  end
  object ButtonUTF8ToUTF16: TButton
    Left = 256
    Top = 56
    Width = 105
    Height = 25
    Caption = 'UTF8 --> UTF16'
    TabOrder = 5
    OnClick = ButtonUTF8ToUTF16Click
  end
  object ButtonUTF16ToAnsi: TButton
    Left = 16
    Top = 96
    Width = 105
    Height = 25
    Caption = 'UTF16 --> Ansi'
    TabOrder = 6
    OnClick = ButtonUTF16ToAnsiClick
  end
  object ButtonUTF16ToUTF8: TButton
    Left = 136
    Top = 96
    Width = 105
    Height = 25
    Caption = 'UTF16 --> UTF8'
    TabOrder = 7
    OnClick = ButtonUTF16ToUTF8Click
  end
  object ButtonUTF16ToUTF16: TButton
    Left = 256
    Top = 96
    Width = 105
    Height = 25
    Caption = 'UTF16 --> UTF16'
    Enabled = False
    TabOrder = 8
  end
  object ButtonAutoToAnsi: TButton
    Left = 16
    Top = 136
    Width = 105
    Height = 25
    Caption = 'Auto --> Ansi'
    TabOrder = 9
    OnClick = ButtonAutoToAnsiClick
  end
  object ButtonAutoToUTF8: TButton
    Left = 136
    Top = 136
    Width = 105
    Height = 25
    Caption = 'Auto --> UTF8'
    TabOrder = 10
    OnClick = ButtonAutoToUTF8Click
  end
  object ButtonAutoToUTF16: TButton
    Left = 256
    Top = 136
    Width = 105
    Height = 25
    Caption = 'Auto --> UTF16'
    TabOrder = 11
    OnClick = ButtonAutoToUTF16Click
  end
  object OpenDialogTxt: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Text file to convert...'
    Left = 88
    Top = 48
  end
  object SaveDialogTxt: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save result as...'
    Left = 128
    Top = 48
  end
end
