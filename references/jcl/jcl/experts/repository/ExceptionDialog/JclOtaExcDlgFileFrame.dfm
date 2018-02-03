inherited JclOtaExcDlgFilePage: TJclOtaExcDlgFilePage
  object LabelLanguage: TLabel
    Left = 23
    Top = 27
    Width = 59
    Height = 13
    Caption = 'RsLanguage'
    FocusControl = ComboBoxLanguage
  end
  object LabelFormName: TLabel
    Left = 23
    Top = 182
    Width = 63
    Height = 13
    Caption = 'RsFormName'
    FocusControl = EditFormName
  end
  object LabelFileName: TLabel
    Left = 23
    Top = 75
    Width = 55
    Height = 13
    Caption = 'RsFileName'
    FocusControl = EditFileName
  end
  object LabelFormAncestor: TLabel
    Left = 23
    Top = 222
    Width = 79
    Height = 13
    Caption = 'RsFormAncestor'
    FocusControl = EditFormAncestor
  end
  object ComboBoxLanguage: TComboBox
    Left = 136
    Top = 24
    Width = 249
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 0
    OnClick = ComboBoxLanguageClick
  end
  object EditFormName: TEdit
    Left = 136
    Top = 179
    Width = 249
    Height = 21
    TabOrder = 3
  end
  object EditFileName: TEdit
    Left = 136
    Top = 72
    Width = 249
    Height = 21
    TabOrder = 1
  end
  object ButtonFileBrowse: TButton
    Left = 391
    Top = 72
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = ButtonFileBrowseClick
  end
  object EditFormAncestor: TEdit
    Left = 136
    Top = 219
    Width = 249
    Height = 21
    TabOrder = 4
  end
  object SaveDialogFileName: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 260
    Top = 96
  end
end
