object MainForm: TMainForm
  Left = 328
  Top = 237
  Caption = 'Binary Tree'
  ClientHeight = 259
  ClientWidth = 462
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
    Left = 64
    Top = 24
    Width = 97
    Height = 25
    Caption = 'IntfBinaryTree'
    TabOrder = 1
    OnClick = btnIntfArrayTreeClick
  end
  object memoResult: TMemo
    Left = 230
    Top = 0
    Width = 232
    Height = 259
    Align = alRight
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnArrayTree: TButton
    Left = 64
    Top = 192
    Width = 97
    Height = 25
    Caption = 'BinaryTree'
    TabOrder = 0
    OnClick = btnArrayTreeClick
  end
  object btnAnsiStrBinaryTree: TButton
    Left = 64
    Top = 80
    Width = 97
    Height = 25
    Caption = 'AnsiStrBinaryTree'
    TabOrder = 3
    OnClick = btnAnsiStrBinaryTreeClick
  end
  object btnWideStrBinaryTree: TButton
    Left = 64
    Top = 136
    Width = 97
    Height = 25
    Caption = 'WideStrBinaryTree'
    TabOrder = 4
    OnClick = btnWideStrBinaryTreeClick
  end
end
