object Form1: TForm1
  Left = 317
  Top = 279
  Width = 490
  Height = 244
  Caption = ' mORMot Client using VCL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 16
    Top = 16
    Width = 87
    Height = 13
    Caption = 'Enter some value:'
  end
  object lbl2: TLabel
    Left = 16
    Top = 88
    Width = 82
    Height = 13
    Caption = 'Computed JSON:'
  end
  object edtValue: TEdit
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = edtValueChange
  end
  object mmoJSON: TMemo
    Left = 16
    Top = 104
    Width = 425
    Height = 81
    TabOrder = 1
  end
  object grpTable: TGroupBox
    Left = 184
    Top = 8
    Width = 89
    Height = 81
    Caption = ' TJSONTable '
    TabOrder = 2
    object btnTableRewind: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'First'
      TabOrder = 0
      OnClick = btnTableNextClick
    end
    object btnTableNext: TButton
      Left = 6
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 1
      OnClick = btnTableNextClick
    end
  end
  object grpORM: TGroupBox
    Left = 296
    Top = 8
    Width = 89
    Height = 81
    Caption = ' ORM '
    TabOrder = 3
    object btnORMFirst: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'First'
      TabOrder = 0
      OnClick = ORMClick
    end
    object btnORMNext: TButton
      Left = 6
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Next'
      TabOrder = 1
      OnClick = ORMClick
    end
  end
end
