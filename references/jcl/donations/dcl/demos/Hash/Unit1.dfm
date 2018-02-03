object frmHash: TfrmHash
  Left = 281
  Top = 201
  Width = 497
  Height = 279
  HorzScrollBar.Range = 476
  VertScrollBar.Range = 209
  ActiveControl = btnIntfIntfHashMap
  AutoScroll = False
  Caption = 'Hash'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  DesignSize = (
    489
    252)
  PixelsPerInch = 96
  TextHeight = 13
  object btnIntfIntfHashMap: TButton
    Left = 16
    Top = 24
    Width = 89
    Height = 25
    Caption = 'IntfIntfHashMap'
    TabOrder = 0
    OnClick = btnIntfIntfHashMapClick
  end
  object btnIntfHashSet: TButton
    Left = 120
    Top = 24
    Width = 75
    Height = 25
    Caption = 'IntfHashSet'
    TabOrder = 5
    OnClick = btnIntfHashSetClick
  end
  object btnHashMap: TButton
    Left = 16
    Top = 184
    Width = 89
    Height = 25
    Caption = 'HashMap'
    TabOrder = 4
    OnClick = btnHashMapClick
  end
  object btnHashSet: TButton
    Left = 120
    Top = 184
    Width = 75
    Height = 25
    Caption = 'HashSet'
    TabOrder = 7
    OnClick = btnHashSetClick
  end
  object btnStrIntfHashMap: TButton
    Left = 16
    Top = 64
    Width = 89
    Height = 25
    Caption = 'StrIntfHashMap'
    TabOrder = 1
    OnClick = btnStrIntfHashMapClick
  end
  object btnIntfArraySet: TButton
    Left = 216
    Top = 24
    Width = 75
    Height = 25
    Caption = 'IntfArraySet'
    TabOrder = 8
    OnClick = btnIntfArraySetClick
  end
  object btnArraySet: TButton
    Left = 216
    Top = 184
    Width = 75
    Height = 25
    Caption = 'ArraySet'
    TabOrder = 10
    OnClick = btnArraySetClick
  end
  object btnStrStrHashMap: TButton
    Left = 16
    Top = 104
    Width = 89
    Height = 25
    Caption = 'StrStrHashMap'
    TabOrder = 2
    OnClick = btnStrStrHashMapClick
  end
  object btnStrHashMap: TButton
    Left = 16
    Top = 144
    Width = 89
    Height = 25
    Caption = 'StrHashMap'
    TabOrder = 3
    OnClick = btnStrHashMapClick
  end
  object btnStrHashSet: TButton
    Left = 120
    Top = 104
    Width = 73
    Height = 25
    Caption = 'StrHashSet'
    TabOrder = 6
    OnClick = btnStrHashSetClick
  end
  object btnStrArraySet: TButton
    Left = 216
    Top = 104
    Width = 73
    Height = 25
    Caption = 'StrArraySet'
    TabOrder = 9
    OnClick = btnStrArraySetClick
  end
  object memResult: TListBox
    Left = 304
    Top = 0
    Width = 185
    Height = 248
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 11
  end
end
