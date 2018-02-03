object MainForm: TMainForm
  Left = 281
  Top = 201
  Width = 513
  Height = 280
  HorzScrollBar.Range = 476
  VertScrollBar.Range = 209
  ActiveControl = btnIntfIntfHashMap
  Caption = 'Hashing Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object btnIntfIntfHashMap: TButton
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'IntfIntfHashMap'
    TabOrder = 0
    OnClick = btnIntfIntfHashMapClick
  end
  object btnIntfHashSet: TButton
    Left = 103
    Top = 8
    Width = 89
    Height = 25
    Caption = 'IntfHashSet'
    TabOrder = 5
    OnClick = btnIntfHashSetClick
  end
  object btnHashMap: TButton
    Left = 8
    Top = 220
    Width = 89
    Height = 25
    Caption = 'HashMap'
    TabOrder = 4
    OnClick = btnHashMapClick
  end
  object btnHashSet: TButton
    Left = 103
    Top = 220
    Width = 89
    Height = 25
    Caption = 'HashSet'
    TabOrder = 7
    OnClick = btnHashSetClick
  end
  object btnAnsiStrIntfHashMap: TButton
    Left = 8
    Top = 47
    Width = 138
    Height = 25
    Caption = 'AnsiStrIntfHashMap'
    TabOrder = 1
    OnClick = btnAnsiStrIntfHashMapClick
  end
  object btnIntfArraySet: TButton
    Left = 198
    Top = 8
    Width = 89
    Height = 25
    Caption = 'IntfArraySet'
    TabOrder = 8
    OnClick = btnIntfArraySetClick
  end
  object btnArraySet: TButton
    Left = 198
    Top = 220
    Width = 89
    Height = 25
    Caption = 'ArraySet'
    TabOrder = 10
    OnClick = btnArraySetClick
  end
  object btnAnsiStrAnsiStrHashMap: TButton
    Left = 8
    Top = 78
    Width = 138
    Height = 25
    Caption = 'AnsiStrAnsiStrHashMap'
    TabOrder = 2
    OnClick = btnAnsiStrAnsiStrHashMapClick
  end
  object btnAnsiStrHashMap: TButton
    Left = 8
    Top = 171
    Width = 138
    Height = 25
    Caption = 'AnsiStrHashMap'
    TabOrder = 3
    OnClick = btnAnsiStrHashMapClick
  end
  object btnAnsiStrHashSet: TButton
    Left = 8
    Top = 109
    Width = 138
    Height = 25
    Caption = 'AnsiStrHashSet'
    TabOrder = 6
    OnClick = btnAnsiStrHashSetClick
  end
  object btnAnsiStrArraySet: TButton
    Left = 8
    Top = 140
    Width = 138
    Height = 25
    Caption = 'AnsiStrArraySet'
    TabOrder = 9
    OnClick = btnAnsiStrArraySetClick
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
  object btnWideStrIntfHashMap: TButton
    Left = 152
    Top = 47
    Width = 135
    Height = 25
    Caption = 'WideStrIntfHashMap'
    TabOrder = 12
    OnClick = btnWideStrIntfHashMapClick
  end
  object btnWideStrWideStrHashMap: TButton
    Left = 152
    Top = 78
    Width = 135
    Height = 25
    Caption = 'WideStrWideStrHashMap'
    TabOrder = 13
    OnClick = btnWideStrWideStrHashMapClick
  end
  object btnWideStrHashSet: TButton
    Left = 152
    Top = 109
    Width = 135
    Height = 25
    Caption = 'WideStrHashSet'
    TabOrder = 14
    OnClick = btnWideStrHashSetClick
  end
  object btnWideStrArraySet: TButton
    Left = 152
    Top = 140
    Width = 135
    Height = 25
    Caption = 'WideStrArraySet'
    TabOrder = 15
    OnClick = btnWideStrArraySetClick
  end
  object btnWideStrHashMap: TButton
    Left = 152
    Top = 171
    Width = 135
    Height = 25
    Caption = 'AnsiStrHashMap'
    TabOrder = 16
    OnClick = btnWideStrHashMapClick
  end
end
