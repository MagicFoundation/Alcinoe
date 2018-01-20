object MainForm: TMainForm
  Left = 236
  Top = 358
  BorderStyle = bsDialog
  Caption = ' REST BLOB Client'
  ClientHeight = 238
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewID: TLabel
    Left = 144
    Top = 80
    Width = 3
    Height = 13
  end
  object btnConnect: TButton
    Left = 40
    Top = 16
    Width = 137
    Height = 33
    Caption = 'Connect'
    TabOrder = 0
    OnClick = btnConnectClick
  end
  object btnGet: TButton
    Left = 192
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Get BLOB'
    Enabled = False
    TabOrder = 1
    OnClick = btnGetClick
  end
  object btnNew: TButton
    Left = 48
    Top = 72
    Width = 75
    Height = 25
    Caption = 'New'
    Enabled = False
    TabOrder = 2
    OnClick = btnNewClick
  end
  object btnSet: TButton
    Left = 48
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Set BLOB'
    Enabled = False
    TabOrder = 3
    OnClick = btnSetClick
  end
  object mmoSet: TMemo
    Left = 48
    Top = 136
    Width = 137
    Height = 81
    Enabled = False
    Lines.Strings = (
      '')
    TabOrder = 4
  end
  object mmoGet: TMemo
    Left = 192
    Top = 136
    Width = 137
    Height = 81
    Enabled = False
    Lines.Strings = (
      '')
    TabOrder = 5
  end
  object edtGetID: TEdit
    Left = 280
    Top = 108
    Width = 49
    Height = 21
    Enabled = False
    TabOrder = 6
  end
end
