object Form1: TForm1
  Left = 669
  Top = 147
  Width = 453
  Height = 608
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 32
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 120
    Top = 272
    Width = 265
    Height = 169
    DataSource = DataSource
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button3: TButton
    Left = 32
    Top = 16
    Width = 75
    Height = 25
    Caption = 'GetString'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Edit1: TEdit
    Left = 112
    Top = 16
    Width = 249
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
  object DBGrid2: TDBGrid
    Left = 120
    Top = 80
    Width = 265
    Height = 169
    DataSource = DataSource1
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    Left = 16
    Top = 272
  end
  object DataSource1: TDataSource
    Left = 16
    Top = 88
  end
end
