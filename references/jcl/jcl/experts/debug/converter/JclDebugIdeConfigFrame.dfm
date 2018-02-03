object JclDebugIdeConfigFrame: TJclDebugIdeConfigFrame
  Left = 0
  Top = 0
  Width = 369
  Height = 436
  AutoScroll = True
  TabOrder = 0
  TabStop = True
  object RadioGroupGenerateJdbg: TRadioGroup
    Left = 3
    Top = 3
    Width = 347
    Height = 129
    Caption = 'RsDebugGenerateJdbg'
    Items.Strings = (
      'RsAlwaysDisabled'
      'RsDefaultDisabled'
      'RsDefaultEnabled'
      'RsAlwaysEnabled')
    TabOrder = 0
  end
  object RadioGroupInsertJdbg: TRadioGroup
    Left = 3
    Top = 138
    Width = 347
    Height = 129
    Caption = 'RsDebugInsertJdbg'
    Items.Strings = (
      'RsAlwaysDisabled'
      'RsDefaultDisabled'
      'RsDefaultEnabled'
      'RsAlwaysEnabled')
    TabOrder = 1
  end
  object RadioGroupDeleteMapFile: TRadioGroup
    Left = 3
    Top = 273
    Width = 347
    Height = 129
    Caption = 'RsDeleteMapFile'
    Items.Strings = (
      'RsDataAlwaysDisabled'
      'RsDataDefaultDisabled'
      'RsDataDefaultEnabled'
      'RsDataAlwaysEnabled')
    TabOrder = 2
  end
  object CheckBoxQuiet: TCheckBox
    Left = 3
    Top = 408
    Width = 347
    Height = 17
    Caption = 'RsQuiet'
    TabOrder = 3
  end
end
