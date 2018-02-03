object MainForm: TMainForm
  Left = 276
  Top = 195
  Width = 564
  Height = 277
  HorzScrollBar.Range = 508
  VertScrollBar.Range = 217
  ActiveControl = btnIntfArrayList
  Caption = 'List Example'
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
  object btnIntfArrayList: TButton
    Left = 24
    Top = 24
    Width = 89
    Height = 25
    Caption = 'IntfArrayList'
    TabOrder = 0
    OnClick = btnIntfArrayListClick
  end
  object btnIntfLinkedList: TButton
    Left = 152
    Top = 24
    Width = 89
    Height = 25
    Caption = 'IntfLinkedList'
    TabOrder = 3
    OnClick = btnIntfLinkedListClick
  end
  object btnIntfVector: TButton
    Left = 272
    Top = 24
    Width = 89
    Height = 25
    Caption = 'IntfVector'
    TabOrder = 6
    OnClick = btnIntfVectorClick
  end
  object btnArrayList: TButton
    Left = 24
    Top = 168
    Width = 89
    Height = 25
    Caption = 'ArrayList'
    TabOrder = 2
    OnClick = btnArrayListClick
  end
  object btnLinkedList: TButton
    Left = 152
    Top = 168
    Width = 89
    Height = 25
    Caption = 'LinkedList'
    TabOrder = 5
    OnClick = btnLinkedListClick
  end
  object btnVector: TButton
    Left = 272
    Top = 168
    Width = 89
    Height = 25
    Caption = 'Vector'
    TabOrder = 8
    OnClick = btnVectorClick
  end
  object memResult: TMemo
    Left = 395
    Top = 0
    Width = 161
    Height = 243
    Align = alRight
    TabOrder = 10
  end
  object btnMyObjectList: TButton
    Left = 152
    Top = 216
    Width = 89
    Height = 25
    Caption = 'MyObjectList'
    TabOrder = 9
    OnClick = btnMyObjectListClick
  end
  object btnAnsiStrArrayList: TButton
    Left = 24
    Top = 72
    Width = 89
    Height = 25
    Caption = 'AnsiStrArrayList'
    TabOrder = 1
    OnClick = btnAnsiStrArrayListClick
  end
  object btnAnsiStrLinkedList: TButton
    Left = 152
    Top = 72
    Width = 89
    Height = 25
    Caption = 'AnsiStrLinkedList'
    TabOrder = 4
    OnClick = btnAnsiStrLinkedListClick
  end
  object btnAnsiStrVector: TButton
    Left = 272
    Top = 72
    Width = 89
    Height = 25
    Caption = 'AnsiStrVector'
    TabOrder = 7
    OnClick = btnAnsiStrVectorClick
  end
  object btnWideStrArrayList: TButton
    Left = 24
    Top = 120
    Width = 89
    Height = 25
    Caption = 'WideStrArrayList'
    TabOrder = 11
    OnClick = btnWideStrArrayListClick
  end
  object btnWideStrLinkedList: TButton
    Left = 152
    Top = 120
    Width = 89
    Height = 25
    Caption = 'WideStrLinkedList'
    TabOrder = 12
    OnClick = btnWideStrLinkedListClick
  end
  object btnWideStrVector: TButton
    Left = 272
    Top = 120
    Width = 89
    Height = 25
    Caption = 'WideStrVector'
    TabOrder = 13
    OnClick = btnWideStrVectorClick
  end
end
