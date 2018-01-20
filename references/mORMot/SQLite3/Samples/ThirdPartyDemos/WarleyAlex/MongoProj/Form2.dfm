object Items: TItems
  Left = 0
  Top = 0
  Width = 286
  Height = 361
  ActiveControl = StringGrid
  Caption = 'Items Details'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    278
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 196
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'SKU:'
  end
  object Label2: TLabel
    Left = 8
    Top = 222
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Quantity:'
  end
  object Label3: TLabel
    Left = 8
    Top = 248
    Width = 27
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Price:'
  end
  object StringGrid: TStringGrid
    Left = 8
    Top = 8
    Width = 265
    Height = 173
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultColWidth = 50
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 7
  end
  object edtName: TEdit
    Left = 63
    Top = 193
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 0
  end
  object btnAdd: TButton
    Left = 193
    Top = 191
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add'
    Default = True
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnDelete: TButton
    Left = 193
    Top = 218
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = btnDeleteClick
  end
  object Button1: TButton
    Left = 193
    Top = 248
    Width = 76
    Height = 25
    Caption = 'Save'
    TabOrder = 5
    OnClick = Button1Click
  end
  object edtCity: TMaskEdit
    Left = 63
    Top = 219
    Width = 95
    Height = 21
    AutoSelect = False
    AutoSize = False
    EditMask = '999;1;_'
    MaxLength = 3
    TabOrder = 1
    Text = '   '
  end
  object edtEmail: TMaskEdit
    Left = 61
    Top = 246
    Width = 86
    Height = 21
    TabOrder = 2
  end
  object Button2: TButton
    Left = 104
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 6
    OnClick = Button2Click
  end
end
