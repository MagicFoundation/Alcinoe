object MainForm: TMainForm
  Left = 506
  Top = 235
  BorderStyle = bsDialog
  Caption = ' mORMot Source Code Repository Synch'
  ClientHeight = 507
  ClientWidth = 601
  Color = clBtnFace
  Constraints.MinHeight = 422
  Constraints.MinWidth = 617
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    601
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 24
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Pending Files:'
  end
  object lbl2: TLabel
    Left = 24
    Top = 224
    Width = 95
    Height = 13
    Caption = 'Commit Description:'
  end
  object mmoStatus: TMemo
    Left = 16
    Top = 24
    Width = 569
    Height = 193
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object mmoDescription: TMemo
    Left = 16
    Top = 240
    Width = 569
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnFossilSynch: TButton
    Left = 168
    Top = 427
    Width = 113
    Height = 32
    Anchors = [akLeft, akBottom]
    Caption = 'Fossil Synch'
    TabOrder = 3
    OnClick = btnFossilSynchClick
  end
  object btnFullSynch: TButton
    Left = 472
    Top = 427
    Width = 113
    Height = 57
    Anchors = [akLeft, akBottom]
    Caption = 'Fossil and Git Synch'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    WordWrap = True
    OnClick = btnFullSynchClick
  end
  object btnGitSynch: TButton
    Left = 304
    Top = 427
    Width = 81
    Height = 41
    Anchors = [akLeft, akBottom]
    Caption = 'Git Synch'
    TabOrder = 4
    OnClick = btnGitSynchClick
  end
  object btnRefreshStatus: TButton
    Left = 520
    Top = 5
    Width = 75
    Height = 18
    Anchors = [akTop, akRight]
    Caption = 'Refresh'
    TabOrder = 5
    OnClick = btnRefreshStatusClick
  end
  object btnGitShell: TButton
    Left = 232
    Top = 478
    Width = 49
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Git Shell'
    TabOrder = 6
    OnClick = btnGitShellClick
  end
  object btnFossilShell: TButton
    Left = 168
    Top = 478
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Fossil Shell'
    TabOrder = 7
    OnClick = btnFossilShellClick
  end
  object btnTests: TButton
    Left = 16
    Top = 427
    Width = 113
    Height = 60
    Anchors = [akLeft, akBottom]
    Caption = 'Regression Tests'
    TabOrder = 8
    WordWrap = True
    OnClick = btnTestsClick
  end
  object btnCopyLink: TButton
    Left = 512
    Top = 222
    Width = 75
    Height = 18
    Caption = 'Copy Link'
    TabOrder = 9
    OnClick = btnCopyLinkClick
  end
  object btnGitAll: TButton
    Left = 304
    Top = 474
    Width = 41
    Height = 25
    Hint = 
      'Git Commit mORMot + SynPDF + SynMustache + LVCL + SynProject rep' +
      'ositories'
    Anchors = [akLeft, akBottom]
    Caption = 'Git ALL'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = btnGitSynchClick
  end
  object btnSynProject: TButton
    Left = 392
    Top = 426
    Width = 65
    Height = 25
    Hint = 'Git Commit SynProject Repository'
    Anchors = [akLeft, akBottom]
    Caption = 'SynProject'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    OnClick = btnGitSynchClick
  end
  object btnSynPdf: TButton
    Left = 392
    Top = 450
    Width = 65
    Height = 25
    Hint = 'Git Commit SynPdf Repository'
    Anchors = [akLeft, akBottom]
    Caption = 'SynPdf'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = btnGitSynchClick
  end
  object btnDMustache: TButton
    Left = 392
    Top = 474
    Width = 65
    Height = 25
    Hint = 'Git Commit dmustache Repository'
    Anchors = [akLeft, akBottom]
    Caption = 'dmustache'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = btnGitSynchClick
  end
  object btnLVCL: TButton
    Left = 344
    Top = 474
    Width = 41
    Height = 25
    Hint = 'Git Commit LVCL Repository'
    Anchors = [akLeft, akBottom]
    Caption = 'LVCL'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    OnClick = btnGitSynchClick
  end
  object chkCopyLink: TCheckBox
    Left = 496
    Top = 486
    Width = 89
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'and copy link'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
  object chkFossilPush: TCheckBox
    Left = 168
    Top = 458
    Width = 65
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'and push'
    TabOrder = 16
  end
  object chkFossilPull: TCheckBox
    Left = 230
    Top = 458
    Width = 59
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'and pull'
    Checked = True
    State = cbChecked
    TabOrder = 17
  end
end
