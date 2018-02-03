object frmMain: TfrmMain
  Left = 300
  Top = 115
  Caption = 'JclPCRE Demo'
  ClientHeight = 517
  ClientWidth = 462
  Color = clBtnFace
  Constraints.MinHeight = 361
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
    Height = 203
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
    Left = 373
    Top = 263
    Width = 75
    Height = 25
    Action = acOpen
    Anchors = [akLeft, akBottom]
    TabOrder = 4
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 498
    Width = 462
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object GroupBoxMatchOptions: TGroupBox
    Left = 8
    Top = 365
    Width = 359
    Height = 127
    Anchors = [akLeft, akBottom]
    Caption = 'Match options:'
    TabOrder = 6
    object chkIgnoreCase: TCheckBox
      Left = 26
      Top = 29
      Width = 97
      Height = 17
      Caption = '&Ignore Case'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkMultiLine: TCheckBox
      Left = 26
      Top = 52
      Width = 97
      Height = 17
      Caption = '&Multi Line'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkDotAll: TCheckBox
      Left = 26
      Top = 75
      Width = 97
      Height = 17
      Caption = '&Dot All'
      TabOrder = 2
    end
    object chkExtended: TCheckBox
      Left = 26
      Top = 98
      Width = 97
      Height = 17
      Caption = '&Extended'
      TabOrder = 3
    end
    object chkAnchored: TCheckBox
      Left = 140
      Top = 29
      Width = 97
      Height = 17
      Caption = '&Anchored'
      TabOrder = 4
    end
    object chkDollarEndOnly: TCheckBox
      Left = 140
      Top = 52
      Width = 97
      Height = 17
      Caption = 'Dollar End Onl&y'
      TabOrder = 5
    end
    object chkExtra: TCheckBox
      Left = 140
      Top = 75
      Width = 97
      Height = 17
      Caption = 'Ex&tra'
      TabOrder = 6
    end
    object chkNotBOL: TCheckBox
      Left = 140
      Top = 98
      Width = 97
      Height = 17
      Caption = 'Not &BOL'
      TabOrder = 7
    end
    object chkNotEOL: TCheckBox
      Left = 256
      Top = 29
      Width = 97
      Height = 17
      Caption = 'Not EO&L'
      TabOrder = 8
    end
    object chkUnGreedy: TCheckBox
      Left = 256
      Top = 52
      Width = 97
      Height = 17
      Caption = '&Ungreedy'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object chkNotEmpty: TCheckBox
      Left = 256
      Top = 75
      Width = 97
      Height = 17
      Caption = 'Not Em&pty'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
  end
  object GroupBoxCompileOptions: TGroupBox
    Left = 8
    Top = 309
    Width = 356
    Height = 50
    Anchors = [akLeft, akBottom]
    Caption = 'Compile options:'
    TabOrder = 7
    object chkStudy: TCheckBox
      Left = 26
      Top = 24
      Width = 97
      Height = 17
      Caption = '&Study'
      TabOrder = 0
    end
    object chkUserLocale: TCheckBox
      Left = 140
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Use&r locale'
      TabOrder = 1
    end
    object chkJITCompile: TCheckBox
      Left = 256
      Top = 24
      Width = 97
      Height = 17
      Caption = '&JIT Compile'
      TabOrder = 2
    end
  end
  object GroupBoxSystemOptions: TGroupBox
    Left = 8
    Top = 263
    Width = 359
    Height = 40
    Caption = 'System options:'
    TabOrder = 8
    object RadioButtonDefault: TRadioButton
      Left = 11
      Top = 20
      Width = 65
      Height = 16
      Caption = 'Default'
      TabOrder = 0
    end
    object RadioButtonANSI: TRadioButton
      Left = 79
      Top = 20
      Width = 65
      Height = 16
      Caption = 'ANSI'
      TabOrder = 4
    end
    object RadioButtonUTF8: TRadioButton
      Left = 140
      Top = 19
      Width = 73
      Height = 17
      Caption = 'UTF-8'
      TabOrder = 1
    end
    object RadioButtonUCS2: TRadioButton
      Left = 220
      Top = 19
      Width = 73
      Height = 17
      Caption = 'UCS-2'
      TabOrder = 2
    end
    object RadioButtonUTF16: TRadioButton
      Left = 296
      Top = 19
      Width = 73
      Height = 17
      Caption = 'UTF-16'
      TabOrder = 3
    end
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
    Left = 240
    Top = 120
  end
end
