inherited JclOtaExcDlgIgnorePage: TJclOtaExcDlgIgnorePage
  object LabelIgnoredExceptions: TLabel
    Left = 120
    Top = 80
    Width = 102
    Height = 13
    Caption = 'RsIgnoredExceptions'
    FocusControl = MemoIgnoredExceptions
  end
  object CheckBoxTraceAllExceptions: TCheckBox
    Left = 96
    Top = 16
    Width = 393
    Height = 17
    Caption = 'RsTraceAllExceptions'
    TabOrder = 0
    OnClick = CheckBoxTraceAllExceptionsClick
  end
  object CheckBoxTraceEAbort: TCheckBox
    Left = 120
    Top = 48
    Width = 369
    Height = 17
    Caption = 'RsTraceEAbort'
    TabOrder = 1
  end
  object MemoIgnoredExceptions: TMemo
    Left = 120
    Top = 99
    Width = 369
    Height = 177
    TabOrder = 2
  end
end
