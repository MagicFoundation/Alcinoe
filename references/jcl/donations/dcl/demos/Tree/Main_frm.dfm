object Form1: TForm1
  Left = 328
  Top = 237
  Width = 470
  Height = 295
  Caption = 'Binary Tree'
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
  object btnIntfArrayTree: TButton
    Left = 72
    Top = 24
    Width = 81
    Height = 25
    Caption = 'IntfBinaryTree'
    TabOrder = 1
    OnClick = btnIntfArrayTreeClick
  end
  object memoResult: TMemo
    Left = 230
    Top = 0
    Width = 232
    Height = 268
    Align = alRight
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnArrayTree: TButton
    Left = 72
    Top = 152
    Width = 81
    Height = 25
    Caption = 'BinaryTree'
    TabOrder = 0
    OnClick = btnArrayTreeClick
  end
  object btnStrBinaryTree: TButton
    Left = 72
    Top = 88
    Width = 81
    Height = 25
    Caption = 'StrBinaryTree'
    TabOrder = 3
    OnClick = btnStrBinaryTreeClick
  end
end
