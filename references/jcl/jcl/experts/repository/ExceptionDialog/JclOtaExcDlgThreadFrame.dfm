inherited JclOtaExcDlgThreadPage: TJclOtaExcDlgThreadPage
  object LabelPreview: TLabel
    Left = 303
    Top = 7
    Width = 50
    Height = 13
    Caption = 'RsPreview'
  end
  object MemoStack: TMemo
    Left = 303
    Top = 26
    Width = 313
    Height = 263
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object CheckBoxStackList: TCheckBox
    Left = 32
    Top = 26
    Width = 257
    Height = 17
    Caption = 'RsStackList'
    TabOrder = 0
    OnClick = CheckBoxStackListClick
  end
  object RadioButtonAllThreads: TRadioButton
    Left = 56
    Top = 57
    Width = 233
    Height = 17
    Caption = 'RsAllThreads'
    TabOrder = 2
    OnClick = RadioButtonClick
  end
  object RadioButtonAllRegisteredThreads: TRadioButton
    Left = 56
    Top = 87
    Width = 233
    Height = 17
    Caption = 'RsAllRegisteredThreads'
    TabOrder = 3
    OnClick = RadioButtonClick
  end
  object RadioButtonMainExceptionThreads: TRadioButton
    Left = 56
    Top = 117
    Width = 233
    Height = 17
    Caption = 'RsMainExceptionThreads'
    TabOrder = 4
    OnClick = RadioButtonClick
  end
  object RadioButtonExceptionThread: TRadioButton
    Left = 56
    Top = 147
    Width = 233
    Height = 17
    Caption = 'RsExceptionThread'
    TabOrder = 5
    OnClick = RadioButtonClick
  end
  object RadioButtonMainThread: TRadioButton
    Left = 56
    Top = 177
    Width = 233
    Height = 17
    Caption = 'RsMainThread'
    TabOrder = 6
    OnClick = RadioButtonClick
  end
end
