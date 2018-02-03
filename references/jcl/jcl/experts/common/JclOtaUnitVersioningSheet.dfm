object JclOtaUnitVersioningFrame: TJclOtaUnitVersioningFrame
  Left = 0
  Top = 0
  Width = 369
  Height = 375
  Anchors = [akLeft, akTop, akRight, akBottom]
  TabOrder = 0
  TabStop = True
  object MemoUnitVersioning: TMemo
    Left = 8
    Top = 8
    Width = 353
    Height = 321
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object ButtonCopyToClipboard: TButton
    Left = 8
    Top = 335
    Width = 137
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'RsCopyClipboard'
    TabOrder = 1
    OnClick = ButtonCopyToClipboardClick
  end
  object ButtonSaveAsText: TButton
    Left = 151
    Top = 335
    Width = 137
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'RsSaveAsText'
    TabOrder = 2
    OnClick = ButtonSaveAsTextClick
  end
  object SaveDialogText: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 120
    Top = 168
  end
end
