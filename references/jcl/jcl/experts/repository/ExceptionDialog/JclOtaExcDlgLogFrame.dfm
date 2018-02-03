inherited JclOtaExcDlgLogPage: TJclOtaExcDlgLogPage
  object LabelLogFileName: TLabel
    Left = 170
    Top = 59
    Width = 55
    Height = 13
    Caption = 'RsFileName'
    FocusControl = EditLogFileName
  end
  object CheckBoxLogFile: TCheckBox
    Left = 120
    Top = 25
    Width = 361
    Height = 17
    Caption = 'RsLogFile'
    TabOrder = 0
    OnClick = CheckBoxLogFileClick
  end
  object EditLogFileName: TEdit
    Left = 240
    Top = 56
    Width = 241
    Height = 21
    TabOrder = 1
  end
  object CheckBoxLogInWorkingDirectory: TCheckBox
    Left = 144
    Top = 96
    Width = 337
    Height = 17
    Caption = 'RsLogInWorkingDirectory'
    TabOrder = 2
  end
  object CheckBoxLogInApplicationDirectory: TCheckBox
    Left = 144
    Top = 128
    Width = 337
    Height = 17
    Caption = 'RsLogInApplicationDirectory'
    TabOrder = 3
  end
  object CheckBoxLogInDesktopDirectory: TCheckBox
    Left = 144
    Top = 160
    Width = 337
    Height = 17
    Caption = 'RsLogInDesktopDirectory'
    TabOrder = 4
  end
  object CheckBoxSaveDialog: TCheckBox
    Left = 144
    Top = 192
    Width = 337
    Height = 17
    Caption = 'RsLogSaveDialog'
    TabOrder = 5
  end
end
