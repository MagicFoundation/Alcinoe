object FrameJclOptions: TFrameJclOptions
  Left = 0
  Top = 0
  Width = 404
  Height = 103
  TabOrder = 0
  TabStop = True
  Width = 404
  Height = 103
  object LabelIniFile: TLabel
    Left = 16
    Top = 18
    Width = 116
    Height = 13
    Caption = 'RsUsesConfigurationFile'
  end
  object CheckBoxWizardActive: TCheckBox
    Left = 16
    Top = 49
    Width = 201
    Height = 17
    Caption = 'RsUsesActive'
    TabOrder = 2
  end
  object CheckBoxWizardConfirm: TCheckBox
    Left = 16
    Top = 72
    Width = 201
    Height = 17
    Caption = 'RsUsesConfirm'
    TabOrder = 3
  end
  object EditIniFile: TEdit
    Left = 136
    Top = 15
    Width = 228
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ButtonIniFile: TButton
    Left = 370
    Top = 15
    Width = 18
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = ButtonIniFileClick
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'ini'
    Filter = 'RsUsesOpenFilters'
    FilterIndex = 0
    Title = 'RsUsesOpenTitle'
    Left = 280
    Top = 56
  end
end
