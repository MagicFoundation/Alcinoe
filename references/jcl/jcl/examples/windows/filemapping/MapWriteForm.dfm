object WriteForm: TWriteForm
  Left = 206
  Top = 115
  Caption = 'Write text to the file map view'
  ClientHeight = 453
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 423
    Width = 688
    Height = 30
    Align = alBottom
    TabOrder = 0
    object cmdWrite: TButton
      Left = 608
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Write'
      Default = True
      TabOrder = 0
      OnClick = cmdWriteClick
    end
  end
  object mmWrite: TMemo
    Left = 0
    Top = 0
    Width = 688
    Height = 423
    Align = alClient
    TabOrder = 1
  end
end
