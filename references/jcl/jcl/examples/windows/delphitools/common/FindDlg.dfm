object FindTextForm: TFindTextForm
  Left = 305
  Top = 226
  ActiveControl = SearchTextEdit
  BorderStyle = bsDialog
  Caption = 'Find text'
  ClientHeight = 110
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object FindBtn: TButton
    Left = 264
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Find'
    Default = True
    TabOrder = 0
    OnClick = FindBtnClick
  end
  object CancelBtn: TButton
    Left = 264
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 96
    Width = 329
    Height = 9
    Min = 0
    Max = 100
    Step = 20
    TabOrder = 2
    Visible = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 249
    Height = 89
    TabOrder = 3
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 56
      Height = 13
      Caption = '&Text to find:'
      FocusControl = SearchTextEdit
    end
    object Label2: TLabel
      Left = 8
      Top = 44
      Width = 38
      Height = 13
      Caption = 'C&olumn:'
      FocusControl = ColumnComboBox
    end
    object SearchTextEdit: TEdit
      Left = 72
      Top = 16
      Width = 169
      Height = 21
      TabOrder = 0
    end
    object ColumnComboBox: TComboBox
      Left = 72
      Top = 40
      Width = 169
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object CaseCheckBox: TCheckBox
      Left = 72
      Top = 64
      Width = 81
      Height = 17
      Caption = '&Ignore case'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object ExactCheckBox: TCheckBox
      Left = 160
      Top = 64
      Width = 81
      Height = 17
      Caption = '&Exact match'
      TabOrder = 3
    end
  end
end
