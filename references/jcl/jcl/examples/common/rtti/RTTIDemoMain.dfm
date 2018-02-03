object Form1: TForm1
  Left = 98
  Top = 153
  ClientWidth = 967
  ClientHeight = 440
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmResult: TMemo
    Left = 0
    Top = 0
    Width = 967
    Height = 411
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 0
    Top = 414
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Type info'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 80
    Top = 414
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Conversions'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 160
    Top = 414
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Declarations'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 240
    Top = 414
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Custom types'
    TabOrder = 4
    OnClick = Button4Click
  end
end
