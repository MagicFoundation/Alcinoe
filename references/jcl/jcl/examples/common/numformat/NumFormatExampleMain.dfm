object MainForm: TMainForm
  Left = 234
  Top = 223
  Width = 800
  Height = 581
  ActiveControl = ValueEdit
  Caption = 'TJclNumericFormat Example'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 78
    Height = 15
    Caption = 'Decimal value'
  end
  object Label2: TLabel
    Left = 320
    Top = 12
    Width = 52
    Height = 15
    Caption = 'Precision'
  end
  object ValueEdit: TEdit
    Left = 100
    Top = 8
    Width = 149
    Height = 23
    TabOrder = 0
    Text = '123456789'
    OnChange = ValueEditChange
  end
  object EvalBtn: TButton
    Left = 704
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Evaluate'
    TabOrder = 8
    OnClick = EvalBtnClick
  end
  object RandBtn: TButton
    Left = 704
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Random'
    TabOrder = 9
    OnClick = RandBtnClick
  end
  object PrecisionEdit: TSpinEdit
    Left = 380
    Top = 8
    Width = 81
    Height = 23
    MaxValue = 64
    MinValue = 1
    TabOrder = 1
    Value = 9
    OnChange = PrecisionEditChange
  end
  object Output: TMemo
    Left = 0
    Top = 120
    Width = 800
    Height = 461
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Color = clBlack
    Font.Height = 16
    Font.Name = 'Courier'
    ParentFont = False
    ScrollBars = ssAutoBoth
    TabOrder = 12
    WordWrap = False
  end
  object BlockSeparatorSelector: TComboBox
    Left = 632
    Top = 36
    Width = 57
    Height = 23
    Style = csDropDownList
    ItemHeight = 17
    Items.Strings = (
      ','
      ' '
      '|')
    ItemIndex = 0
    TabOrder = 13
    Text = ','
    OnChange = BlockSeparatorSelectorChange
  end
  object Label3: TLabel
    Left = 504
    Top = 40
    Width = 109
    Height = 15
    Caption = 'DigitBlockSeparator'
  end
  object Label4: TLabel
    Left = 504
    Top = 68
    Width = 78
    Height = 15
    Caption = 'DigitBlockSize'
  end
  object BlockSizeEdit: TSpinEdit
    Left = 632
    Top = 64
    Width = 57
    Height = 23
    TabOrder = 7
    Value = 3
    OnChange = BlockSizeEditChange
  end
  object cbShowPlusSign: TCheckBox
    Left = 100
    Top = 32
    Width = 149
    Height = 31
    Caption = 'Show plus sign'
    TabOrder = 10
    OnClick = cbShowPlusSignClick
  end
  object Label5: TLabel
    Left = 276
    Top = 68
    Width = 95
    Height = 15
    Caption = 'ExponentDivision'
  end
  object ExpDivisionEdit: TSpinEdit
    Left = 380
    Top = 64
    Width = 81
    Height = 23
    MaxValue = 12
    MinValue = 1
    TabOrder = 3
    Value = 3
    OnChange = ExpDivisionEditChange
  end
  object WidthEdit: TSpinEdit
    Left = 380
    Top = 92
    Width = 81
    Height = 23
    TabOrder = 4
    Value = 4
    OnChange = WidthEditChange
  end
  object Label6: TLabel
    Left = 340
    Top = 96
    Width = 31
    Height = 15
    Caption = 'Width'
  end
  object cbZeroPadding: TCheckBox
    Left = 100
    Top = 64
    Width = 149
    Height = 30
    Caption = 'Zero padding'
    TabOrder = 11
    OnClick = cbZeroPaddingClick
  end
  object Label7: TLabel
    Left = 504
    Top = 12
    Width = 49
    Height = 15
    Caption = 'Multiplier'
  end
  object MultiplierSelector: TComboBox
    Left = 632
    Top = 8
    Width = 57
    Height = 23
    Style = csDropDownList
    ItemHeight = 17
    Items.Strings = (
      #215
      '*')
    ItemIndex = 0
    TabOrder = 18
    Text = #215
    OnChange = MultiplierSelectorChange
  end
  object Label8: TLabel
    Left = 260
    Top = 40
    Width = 111
    Height = 15
    Caption = 'Fractional part digits'
  end
  object FractionDigitsEdit: TSpinEdit
    Left = 380
    Top = 36
    Width = 81
    Height = 23
    MaxValue = 64
    TabOrder = 2
    Value = 6
    OnChange = FractionDigitsEditChange
  end
end
