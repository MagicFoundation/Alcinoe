object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 455
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label6: TLabel
    Left = 372
    Top = 16
    Width = 74
    Height = 13
    Caption = 'Result (XML)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label24: TLabel
    Left = 16
    Top = 19
    Width = 57
    Height = 13
    Caption = 'Sqlite3.dll'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label25: TLabel
    Left = 16
    Top = 239
    Width = 25
    Height = 13
    Caption = 'SQL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label19: TLabel
    Left = 16
    Top = 49
    Width = 55
    Height = 13
    Caption = 'Database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label21: TLabel
    Left = 112
    Top = 191
    Width = 65
    Height = 13
    Caption = 'cache_size'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label22: TLabel
    Left = 244
    Top = 191
    Width = 58
    Height = 13
    Caption = 'page_size'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ALMemoResult: TMemo
    Left = 372
    Top = 35
    Width = 337
    Height = 400
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object ALEditSqlite3Lib: TEdit
    Left = 79
    Top = 16
    Width = 250
    Height = 21
    Cursor = crArrow
    TabOrder = 1
    Text = 'Sqlite3.dll'
  end
  object ALMemoSqlite3Query: TMemo
    Left = 16
    Top = 258
    Width = 338
    Height = 115
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object ALButtonSqlLite3Select: TButton
    Left = 16
    Top = 379
    Width = 161
    Height = 25
    Caption = 'Execute SELECT via SqlLite3'
    TabOrder = 3
    OnClick = ALButtonSqlLite3SelectClick
  end
  object ALEditSqlite3Database: TEdit
    Left = 79
    Top = 45
    Width = 250
    Height = 21
    Cursor = crArrow
    TabOrder = 4
  end
  object ALButtonSqlite3Update: TButton
    Left = 193
    Top = 379
    Width = 161
    Height = 25
    Caption = 'Execute UPDATE via SqlLite3'
    TabOrder = 5
    OnClick = ALButtonSqlite3UpdateClick
  end
  object RadioGroupSqlite3Journal_Mode: TRadioGroup
    Left = 16
    Top = 77
    Width = 118
    Height = 105
    Caption = 'journal_mode'
    ItemIndex = 0
    Items.Strings = (
      'DELETE'
      'TRUNCATE '
      'PERSIST '
      'MEMORY '
      'WAL '
      'OFF')
    TabOrder = 6
  end
  object RadioGroupSQLite3Temp_Store: TRadioGroup
    Left = 140
    Top = 77
    Width = 104
    Height = 105
    Caption = 'temp_store'
    ItemIndex = 0
    Items.Strings = (
      'DEFAULT '
      'FILE '
      'MEMORY')
    TabOrder = 7
  end
  object RadioGroupSqlite3Synhcronous: TRadioGroup
    Left = 250
    Top = 77
    Width = 104
    Height = 105
    Caption = 'synchronous '
    ItemIndex = 2
    Items.Strings = (
      'OFF '
      'NORMAL '
      'FULL')
    TabOrder = 8
  end
  object ALEditSqlite3Cache_Size: TEdit
    Left = 183
    Top = 188
    Width = 43
    Height = 21
    TabOrder = 9
    Text = '2000'
  end
  object ALEditSqlite3Page_Size: TEdit
    Left = 308
    Top = 188
    Width = 46
    Height = 21
    TabOrder = 10
    Text = '1024'
  end
  object ALCheckBoxSqlite3SharedCache: TCheckBox
    Left = 112
    Top = 211
    Width = 97
    Height = 19
    Caption = 'shared cache'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 11
  end
  object ALCheckBoxSqlite3ReadUncommited: TCheckBox
    Left = 229
    Top = 211
    Width = 124
    Height = 19
    Caption = 'read uncommitted'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 12
  end
  object ALButtonSqlLite3Vacuum: TButton
    Left = 16
    Top = 410
    Width = 161
    Height = 25
    Caption = 'VACUUM the database'
    TabOrder = 13
    OnClick = ALButtonSqlLite3VacuumClick
  end
  object Button1: TButton
    Left = 329
    Top = 45
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 14
    OnClick = ALEditSqlite3DatabaseButtonClick
  end
  object Button2: TButton
    Left = 329
    Top = 16
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 15
    OnClick = ALEditSqlite3LibButtonClick
  end
  object OpenDialog1: TOpenDialog
    Left = 488
    Top = 186
  end
  object OpenDialog2: TOpenDialog
    Left = 616
    Top = 186
  end
end
