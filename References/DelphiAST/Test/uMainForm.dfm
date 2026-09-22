object Form2: TForm2
  Left = -3
  Top = 81
  Caption = 'DelphiAST Test Application'
  ClientHeight = 231
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    687
    231)
  PixelsPerInch = 96
  TextHeight = 13
  object memLog: TMemo
    Left = 0
    Top = 0
    Width = 687
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnRun: TButton
    Left = 604
    Top = 198
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Run'
    TabOrder = 1
    OnClick = btnRunClick
  end
end
