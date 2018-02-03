inherited JclOtaExcDlgSystemPage: TJclOtaExcDlgSystemPage
  object CheckBoxDelayed: TCheckBox
    Left = 120
    Top = 18
    Width = 265
    Height = 17
    Caption = 'RsDelayedStackTrace'
    TabOrder = 0
  end
  object CheckBoxHookDll: TCheckBox
    Left = 120
    Top = 50
    Width = 265
    Height = 17
    Caption = 'RsHookDll'
    TabOrder = 1
  end
  object CheckBoxModuleList: TCheckBox
    Left = 120
    Top = 114
    Width = 265
    Height = 17
    Caption = 'RsModuleList'
    TabOrder = 3
    OnClick = CheckBoxModuleListClick
  end
  object CheckBoxOSInfo: TCheckBox
    Left = 120
    Top = 178
    Width = 265
    Height = 17
    Caption = 'RsOSInfo'
    TabOrder = 5
  end
  object CheckBoxActiveControls: TCheckBox
    Left = 120
    Top = 210
    Width = 265
    Height = 17
    Caption = 'RsActiveControls'
    TabOrder = 6
  end
  object CheckBoxCatchMainThread: TCheckBox
    Left = 120
    Top = 82
    Width = 265
    Height = 17
    Caption = 'RsCatchMainThread'
    TabOrder = 2
  end
  object CheckBoxUnitVersioning: TCheckBox
    Left = 152
    Top = 146
    Width = 233
    Height = 17
    Caption = 'RsUnitVersioning'
    TabOrder = 4
  end
  object CheckBoxDisableIfDebuggerAttached: TCheckBox
    Left = 120
    Top = 242
    Width = 265
    Height = 17
    Caption = 'RsDisableIfDebuggerAttached'
    TabOrder = 7
  end
end
