object frmList: TfrmList
  Left = 276
  Top = 195
  Width = 548
  Height = 276
  HorzScrollBar.Range = 508
  VertScrollBar.Range = 217
  ActiveControl = btnIntfArrayList
  AutoScroll = False
  Caption = 'List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnIntfArrayList: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'IntfArrayList'
    TabOrder = 0
    OnClick = btnIntfArrayListClick
  end
  object btnIntfLinkedList: TButton
    Left = 152
    Top = 24
    Width = 75
    Height = 25
    Caption = 'IntfLinkedList'
    TabOrder = 3
    OnClick = btnIntfLinkedListClick
  end
  object btnIntfVector: TButton
    Left = 272
    Top = 24
    Width = 75
    Height = 25
    Caption = 'IntfVector'
    TabOrder = 6
    OnClick = btnIntfVectorClick
  end
  object btnArrayList: TButton
    Left = 24
    Top = 120
    Width = 75
    Height = 25
    Caption = 'ArrayList'
    TabOrder = 2
    OnClick = btnArrayListClick
  end
  object btnLinkedList: TButton
    Left = 152
    Top = 120
    Width = 75
    Height = 25
    Caption = 'LinkedList'
    TabOrder = 5
    OnClick = btnLinkedListClick
  end
  object btnVector: TButton
    Left = 272
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Vector'
    TabOrder = 8
    OnClick = btnVectorClick
  end
  object memResult: TMemo
    Left = 379
    Top = 0
    Width = 161
    Height = 249
    Align = alRight
    TabOrder = 10
  end
  object btnMyObjectList: TButton
    Left = 152
    Top = 192
    Width = 75
    Height = 25
    Caption = 'MyObjectList'
    TabOrder = 9
    OnClick = btnMyObjectListClick
  end
  object btnStrArrayList: TButton
    Left = 24
    Top = 72
    Width = 75
    Height = 25
    Caption = 'StrArrayList'
    TabOrder = 1
    OnClick = btnStrArrayListClick
  end
  object btnStrLinkedList: TButton
    Left = 152
    Top = 72
    Width = 75
    Height = 25
    Caption = 'StrLinkedList'
    TabOrder = 4
    OnClick = btnStrLinkedListClick
  end
  object btnStrVector: TButton
    Left = 272
    Top = 72
    Width = 75
    Height = 25
    Caption = 'StrVector'
    TabOrder = 7
    OnClick = btnStrVectorClick
  end
end
