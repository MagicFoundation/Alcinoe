object Form1: TForm1
  Left = 192
  Top = 124
  Width = 754
  Height = 419
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = chkFromSQLClick
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrdData: TDBGrid
    Left = 0
    Top = 41
    Width = 738
    Height = 340
    Align = alClient
    DataSource = ds1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 738
    Height = 41
    Align = alTop
    TabOrder = 1
    object lblTiming: TLabel
      Left = 456
      Top = 13
      Width = 241
      Height = 13
      AutoSize = False
    end
    object lblFrom: TLabel
      Left = 40
      Top = 12
      Width = 28
      Height = 13
      Caption = 'From:'
    end
    object chkViaTClientDataSet: TCheckBox
      Left = 216
      Top = 12
      Width = 137
      Height = 17
      Caption = 'Via TClientDataSet'
      TabOrder = 0
      OnClick = chkFromSQLClick
    end
    object cbbDataSource: TComboBox
      Left = 72
      Top = 10
      Width = 129
      Height = 21
      Style = csDropDownList
      DropDownCount = 10
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'JSON direct'
      OnChange = chkFromSQLClick
      Items.Strings = (
        'JSON direct'
        'JSON TDocVariant'
        'SQLite3 direct'
        'SQLite3 proxy direct'
        'SQLite3 proxy compressed'
        'SQLite3 HTTP WinHTTP'
        'SQLite3 HTTP WinINet'
        'SQLite3 HTTP Sockets'
        'SQLite3 SQL TDataSet')
    end
    object btnRefresh: TButton
      Left = 360
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = chkFromSQLClick
    end
    object btnApply: TButton
      Left = 648
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Apply Updates'
      TabOrder = 3
      OnClick = btnApplyClick
    end
  end
  object ds1: TDataSource
    Left = 96
    Top = 72
  end
end
