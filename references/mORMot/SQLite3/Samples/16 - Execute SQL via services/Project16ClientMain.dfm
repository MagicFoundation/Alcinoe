object MainForm: TMainForm
  Left = 599
  Top = 350
  Width = 810
  Height = 602
  Caption = ' Service-based remote SQL access'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object spl1: TSplitter
    Left = 0
    Top = 89
    Width = 794
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object mmoQuery: TMemo
    Left = 0
    Top = 0
    Width = 794
    Height = 89
    Align = alTop
    TabOrder = 0
    Visible = False
  end
  object pnlLogin: TPanel
    Left = 0
    Top = 92
    Width = 794
    Height = 69
    Align = alTop
    TabOrder = 1
    object lbl1: TLabel
      Left = 22
      Top = 10
      Width = 38
      Height = 16
      Caption = 'Engine'
    end
    object lbledtServer: TLabeledEdit
      Left = 264
      Top = 8
      Width = 121
      Height = 24
      EditLabel.Width = 38
      EditLabel.Height = 16
      EditLabel.Caption = 'Server'
      LabelPosition = lpLeft
      TabOrder = 2
    end
    object lbledtDatabase: TLabeledEdit
      Left = 264
      Top = 32
      Width = 121
      Height = 24
      EditLabel.Width = 53
      EditLabel.Height = 16
      EditLabel.Caption = 'Database'
      LabelPosition = lpLeft
      TabOrder = 3
    end
    object lbledtUser: TLabeledEdit
      Left = 456
      Top = 8
      Width = 121
      Height = 24
      EditLabel.Width = 26
      EditLabel.Height = 16
      EditLabel.Caption = 'User'
      LabelPosition = lpLeft
      TabOrder = 4
    end
    object lbledtPassword: TLabeledEdit
      Left = 456
      Top = 32
      Width = 121
      Height = 24
      EditLabel.Width = 55
      EditLabel.Height = 16
      EditLabel.Caption = 'Password'
      LabelPosition = lpLeft
      PasswordChar = '*'
      TabOrder = 5
    end
    object cbbEngine: TComboBox
      Left = 22
      Top = 28
      Width = 169
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 1
    end
    object btnOpen: TButton
      Left = 592
      Top = 16
      Width = 97
      Height = 33
      Caption = 'Open'
      TabOrder = 0
      OnClick = btnOpenClick
    end
  end
  object drwgrdData: TDrawGrid
    Left = 0
    Top = 202
    Width = 794
    Height = 362
    Align = alClient
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 2
    Visible = False
  end
  object pnlCommand: TPanel
    Left = 0
    Top = 161
    Width = 794
    Height = 41
    Align = alTop
    TabOrder = 3
    Visible = False
    DesignSize = (
      794
      41)
    object lblSelectTable: TLabel
      Left = 288
      Top = 11
      Width = 82
      Height = 16
      Alignment = taRightJustify
      Caption = 'Select * from '
    end
    object btnExecute: TButton
      Left = 668
      Top = 4
      Width = 84
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btnExecuteClick
    end
    object cbbTableNames: TComboBox
      Left = 376
      Top = 8
      Width = 201
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 1
      OnChange = cbbTableNamesChange
    end
  end
end
