object Form1: TForm1
  Left = 192
  Top = 136
  ClientWidth = 782
  ClientHeight = 474
  Caption = 'Exception frame tracking example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 172
    Top = 32
    Width = 610
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 4
    Top = 32
    Width = 165
    Height = 25
    Caption = 'Assign to PChar(nil)'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 4
    Top = 60
    Width = 165
    Height = 25
    Caption = 'try ... except'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 4
    Top = 88
    Width = 165
    Height = 25
    Caption = 'try except on.... else'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 4
    Top = 116
    Width = 165
    Height = 25
    Caption = 'try ... finally'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 4
    Top = 144
    Width = 165
    Height = 25
    Caption = 'try try ... finally except'
    TabOrder = 5
    OnClick = Button5Click
  end
  object chkShowAllFrames: TCheckBox
    Left = 180
    Top = 8
    Width = 145
    Height = 17
    Caption = 'Show all exception frames'
    TabOrder = 6
  end
  object Button6: TButton
    Left = 704
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 7
    OnClick = Button6Click
  end
end
