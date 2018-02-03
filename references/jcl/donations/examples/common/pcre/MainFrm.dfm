object frmMain: TfrmMain
  Left = 300
  Top = 115
  Width = 470
  Height = 370
  Caption = 'JclPCRE Demo'
  Color = clBtnFace
  Constraints.MinHeight = 370
  Constraints.MinWidth = 470
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 10
    Width = 96
    Height = 13
    Caption = 'Reg&ular Expression:'
    FocusControl = edRegExpr
  end
  object edRegExpr: TEdit
    Left = 12
    Top = 24
    Width = 271
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = edRegExprChange
  end
  object btnFind: TButton
    Left = 292
    Top = 24
    Width = 75
    Height = 25
    Action = acFind
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object btnFindNext: TButton
    Left = 370
    Top = 24
    Width = 75
    Height = 25
    Action = acFindNext
    Anchors = [akTop, akRight]
    TabOrder = 2
  end
  object reFile: TMemo
    Left = 12
    Top = 54
    Width = 437
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    HideSelection = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 3
    WantReturns = False
    WordWrap = False
  end
  object btnOpen: TButton
    Left = 364
    Top = 294
    Width = 75
    Height = 25
    Action = acOpen
    Anchors = [akRight, akBottom]
    TabOrder = 4
  end
  object chkIgnoreCase: TCheckBox
    Left = 18
    Top = 244
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Ignore Case'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chkMultiLine: TCheckBox
    Left = 18
    Top = 262
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Multi Line'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chkDotAll: TCheckBox
    Left = 18
    Top = 280
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Dot All'
    TabOrder = 7
  end
  object chkExtended: TCheckBox
    Left = 18
    Top = 298
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Extended'
    TabOrder = 8
  end
  object chkAnchored: TCheckBox
    Left = 132
    Top = 244
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Anchored'
    TabOrder = 9
  end
  object chkDollarEndOnly: TCheckBox
    Left = 132
    Top = 262
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Dollar End Onl&y'
    TabOrder = 10
  end
  object chkExtra: TCheckBox
    Left = 132
    Top = 280
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Ex&tra'
    TabOrder = 11
  end
  object chkNotBOL: TCheckBox
    Left = 132
    Top = 298
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Not &BOL'
    TabOrder = 12
  end
  object chkNotEOL: TCheckBox
    Left = 246
    Top = 244
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Not EO&L'
    TabOrder = 13
  end
  object chkUnGreedy: TCheckBox
    Left = 246
    Top = 262
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Ungreedy'
    Checked = True
    State = cbChecked
    TabOrder = 14
  end
  object chkNotEmpty: TCheckBox
    Left = 246
    Top = 280
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Not Em&pty'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
  object chkUTF8: TCheckBox
    Left = 246
    Top = 298
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'UTF&8'
    TabOrder = 16
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 324
    Width = 462
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object alMain: TActionList
    Left = 144
    Top = 102
    object acFind: TAction
      Caption = '&Find'
      ShortCut = 16454
      OnExecute = acFindExecute
    end
    object acFindNext: TAction
      Caption = 'Find &Next'
      ShortCut = 114
      OnExecute = acFindNextExecute
    end
    object acOpen: TAction
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = acOpenExecute
    end
  end
  object odOpen: TOpenDialog
    Options = [ofEnableSizing, ofDontAddToRecent, ofForceShowHidden]
    Left = 240
    Top = 120
  end
end
