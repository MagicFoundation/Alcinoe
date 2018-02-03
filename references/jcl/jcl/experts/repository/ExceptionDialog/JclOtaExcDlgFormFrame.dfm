inherited JclOtaExcDlgFormPage: TJclOtaExcDlgFormPage
  object LabelEMailAddress: TLabel
    Left = 97
    Top = 186
    Width = 36
    Height = 13
    Caption = 'RsEmail'
    FocusControl = EditEMail
  end
  object LabelSubject: TLabel
    Left = 97
    Top = 226
    Width = 48
    Height = 13
    Caption = 'RsSubject'
    FocusControl = EditSubject
  end
  object CheckBoxMail: TCheckBox
    Left = 72
    Top = 144
    Width = 233
    Height = 17
    Caption = 'RsDialogWithMailButton'
    TabOrder = 3
    OnClick = CheckBoxMailClick
  end
  object EditEMail: TEdit
    Left = 160
    Top = 183
    Width = 193
    Height = 21
    TabOrder = 4
  end
  object CheckBoxModalDialog: TCheckBox
    Left = 72
    Top = 24
    Width = 233
    Height = 17
    Caption = 'RsModalDialog'
    TabOrder = 0
  end
  object CheckBoxSizeable: TCheckBox
    Left = 72
    Top = 64
    Width = 233
    Height = 17
    Caption = 'RsSizeable'
    TabOrder = 1
  end
  object EditSubject: TEdit
    Left = 160
    Top = 223
    Width = 193
    Height = 21
    TabOrder = 5
  end
  object CheckBoxAutoScrollBars: TCheckBox
    Left = 72
    Top = 104
    Width = 233
    Height = 17
    Caption = 'RsAutoScrollBars'
    TabOrder = 2
  end
end
