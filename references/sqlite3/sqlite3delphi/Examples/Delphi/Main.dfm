object Form_Main: TForm_Main
  Left = 421
  Top = 198
  Width = 673
  Height = 434
  Caption = 'Delphi SQLite Example'
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
  object Panel_Actions: TPanel
    Left = 0
    Top = 0
    Width = 154
    Height = 396
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Button_DB_Read: TButton
      Left = 8
      Top = 48
      Width = 136
      Height = 33
      Caption = 'Read Database'
      TabOrder = 1
      OnClick = Button_DB_ReadClick
    end
    object Button_DB_Create: TButton
      Left = 8
      Top = 8
      Width = 136
      Height = 33
      Caption = 'Create Database'
      TabOrder = 0
      OnClick = Button_DB_CreateClick
    end
  end
  object Memo_Result: TMemo
    Left = 154
    Top = 0
    Width = 503
    Height = 396
    Align = alClient
    BorderStyle = bsNone
    Color = 4207920
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 16773344
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
