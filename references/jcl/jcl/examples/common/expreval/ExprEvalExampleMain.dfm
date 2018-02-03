object ExprEvalForm: TExprEvalForm
  Left = 222
  Top = 107
  Caption = 'JclExprEval Example'
  ClientHeight = 321
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 37
    Width = 54
    Height = 13
    Caption = 'E&xpression:'
    FocusControl = ExpressionInput
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 49
    Height = 13
    Caption = 'Functions:'
  end
  object Label3: TLabel
    Left = 8
    Top = 11
    Width = 46
    Height = 13
    Caption = 'Variables:'
  end
  object ExpressionInput: TEdit
    Left = 80
    Top = 34
    Width = 305
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object Memo1: TMemo
    Left = 0
    Top = 88
    Width = 479
    Height = 242
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 6
  end
  object EnterButton: TButton
    Left = 396
    Top = 34
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Evaluate'
    Default = True
    TabOrder = 5
    OnClick = EnterButtonClick
  end
  object FuncList: TComboBox
    Left = 80
    Top = 61
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 4
    OnClick = FuncListClick
  end
  object ValueEdit: TEdit
    Left = 271
    Top = 8
    Width = 114
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = ValueEditChange
  end
  object VarComboBox: TComboBox
    Left = 80
    Top = 8
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'x'
    OnChange = VarComboBoxChange
    Items.Strings = (
      'x'
      'y'
      'z')
  end
  object AssignButton: TButton
    Left = 231
    Top = 8
    Width = 34
    Height = 21
    Caption = ':='
    TabOrder = 1
    OnClick = AssignButtonClick
  end
end
