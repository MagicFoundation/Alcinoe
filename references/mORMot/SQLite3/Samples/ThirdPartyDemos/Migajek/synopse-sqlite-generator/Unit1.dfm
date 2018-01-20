object Form1: TForm1
  Left = 192
  Top = 107
  Width = 696
  Height = 380
  Caption = 'Synopse SQLite Record tool'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    680
    342)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 632
    Top = 16
    Width = 27
    Height = 16
    Caption = 'pas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 344
    Top = 16
    Width = 35
    Height = 16
    Caption = 'meta'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 369
    Height = 265
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Name:str'
      'Surname:str'
      'Phone:str;r;w'
      'Age:int;r')
    TabOrder = 0
  end
  object generate: TButton
    Left = 8
    Top = 312
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'meta2pas'
    TabOrder = 1
    OnClick = generateClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'Person'
  end
  object Memo2: TMemo
    Left = 400
    Top = 40
    Width = 257
    Height = 270
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
  object parse: TButton
    Left = 88
    Top = 312
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'pas2meta'
    TabOrder = 4
    OnClick = parseClick
  end
  object Button1: TButton
    Left = 584
    Top = 317
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'info'
    TabOrder = 5
    OnClick = Button1Click
  end
end
