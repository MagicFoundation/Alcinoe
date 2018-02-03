object MTTestForm: TMTTestForm
  Left = 399
  Top = 375
  Width = 325
  Height = 159
  Caption = 'JclDebug MT Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 225
    Height = 25
    Caption = 'Thread Exception Test (requires 2009)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 8
    Top = 40
    Width = 225
    Height = 25
    Caption = 'Show Thread Snapshot'
    TabOrder = 1
    OnClick = Button3Click
  end
end
