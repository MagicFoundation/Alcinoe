object OptionsForm: TOptionsForm
  Left = 543
  Top = 374
  Width = 251
  Height = 204
  Caption = 'Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Library'
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 51
    Height = 13
    Caption = 'User name'
  end
  object Label4: TLabel
    Left = 8
    Top = 88
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object edLibrary: TEdit
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    TabOrder = 0
    OnKeyPress = FormKeyPress
  end
  object edUSER: TEdit
    Left = 8
    Top = 64
    Width = 145
    Height = 21
    TabOrder = 1
    OnKeyPress = FormKeyPress
  end
  object edPASS: TEdit
    Left = 8
    Top = 104
    Width = 145
    Height = 21
    TabOrder = 2
    OnKeyPress = FormKeyPress
  end
  object btSave: TButton
    Left = 8
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Save'
    TabOrder = 3
    OnClick = btSaveClick
    OnKeyPress = FormKeyPress
  end
  object btCancel: TButton
    Left = 160
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 4
    OnClick = btCancelClick
    OnKeyPress = FormKeyPress
  end
end
