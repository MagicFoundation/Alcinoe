object Form1: TForm1
  Left = 194
  Top = 107
  ClientWidth = 270
  ClientHeight = 145
  Caption = 'Single application instance only'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DialogBtn: TButton
    Left = 72
    Top = 48
    Width = 113
    Height = 25
    Caption = 'Show modal dialog'
    TabOrder = 0
    OnClick = DialogBtnClick
  end
end
