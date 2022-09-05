object ShutDownForm: TShutDownForm
  Left = 379
  Top = 327
  Width = 325
  Height = 244
  Caption = 'Shutdown database'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 128
    Width = 48
    Height = 13
    Caption = 'Wait (sec)'
  end
  object edWait: TEdit
    Left = 120
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object GroupBox1: TGroupBox
    Left = 48
    Top = 16
    Width = 225
    Height = 105
    Caption = 'Options  '
    TabOrder = 1
    object rbForced: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Forced'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbDenyTransaction: TRadioButton
      Left = 16
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Deny Transaction'
      TabOrder = 1
    end
    object rbDenyAttachment: TRadioButton
      Left = 16
      Top = 72
      Width = 113
      Height = 17
      Caption = 'Deny Attachment'
      TabOrder = 2
    end
  end
  object btShutdown: TButton
    Left = 120
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Shutdown'
    TabOrder = 2
    OnClick = btShutdownClick
  end
  object Config: TUIBConfig
    LibraryName = 'gds32.dll'
    Left = 8
    Top = 8
  end
end
