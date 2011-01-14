object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 693
  ClientWidth = 371
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 16
    Top = 411
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
    Left = 37
    Top = 12
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
    Top = 199
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
    Left = 44
    Top = 37
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
    Top = 173
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
    Top = 173
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
  object Panel1: TPanel
    Left = 16
    Top = 625
    Width = 337
    Height = 58
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 0
    object Label5: TLabel
      Left = 5
      Top = 8
      Width = 300
      Height = 39
      Caption = 
        'Please add in your website a link to http://www.arkadia.com or s' +
        'end me an email to svanderclock@arkadia.com if you like this com' +
        'ponent!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object ALMemoResult: TALMemo
    Left = 16
    Top = 427
    Width = 337
    Height = 192
    OnPaint = ALMemoPaint
    OnPaintScrollBar = ALMemoPaintScrollBar
    TabOrder = 1
    DesignSize = (
      337
      192)
  end
  object ALEditSqlite3Lib: TALEdit
    Left = 105
    Top = 9
    Width = 249
    Height = 19
    OnButtonClick = ALEditSqlite3LibButtonClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 2
    Text = 'Sqlite3.dll'
  end
  object ALMemoSqlite3Query: TALMemo
    Left = 16
    Top = 218
    Width = 338
    Height = 115
    OnPaint = ALMemoPaint
    OnPaintScrollBar = ALMemoPaintScrollBar
    TabOrder = 3
    WordWrap = False
    DesignSize = (
      338
      115)
  end
  object ALButtonSqlLite3Select: TALButton
    Left = 16
    Top = 339
    Width = 161
    Height = 25
    Caption = 'Execute SELECT via SqlLite3'
    TabOrder = 4
    OnClick = ALButtonSqlLite3SelectClick
    OnPaint = ALButtonPaint
  end
  object ALEditSqlite3Database: TALEdit
    Left = 105
    Top = 34
    Width = 249
    Height = 19
    OnButtonClick = ALEditSqlite3DatabaseButtonClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 5
  end
  object ALButtonSqlite3Update: TALButton
    Left = 193
    Top = 339
    Width = 161
    Height = 25
    Caption = 'Execute UPDATE via SqlLite3'
    TabOrder = 6
    OnClick = ALButtonSqlite3UpdateClick
    OnPaint = ALButtonPaint
  end
  object RadioGroupSqlite3Journal_Mode: TRadioGroup
    Left = 30
    Top = 59
    Width = 104
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
    TabOrder = 7
  end
  object RadioGroupSQLite3Temp_Store: TRadioGroup
    Left = 140
    Top = 59
    Width = 104
    Height = 105
    Caption = 'temp_store'
    ItemIndex = 0
    Items.Strings = (
      'DEFAULT '
      'FILE '
      'MEMORY')
    TabOrder = 8
  end
  object RadioGroupSqlite3Synhcronous: TRadioGroup
    Left = 250
    Top = 59
    Width = 104
    Height = 105
    Caption = 'synchronous '
    ItemIndex = 2
    Items.Strings = (
      'OFF '
      'NORMAL '
      'FULL')
    TabOrder = 9
  end
  object ALEditSqlite3Cache_Size: TALEdit
    Left = 183
    Top = 170
    Width = 43
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 10
    Text = '2000'
  end
  object ALEditSqlite3Page_Size: TALEdit
    Left = 308
    Top = 170
    Width = 46
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 11
    Text = '1024'
  end
  object ALCheckBoxSqlite3SharedCache: TALCheckBox
    Left = 112
    Top = 193
    Width = 97
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Caption = 'shared cache'
    TabOrder = 12
  end
  object ALCheckBoxSqlite3ReadUncommited: TALCheckBox
    Left = 229
    Top = 193
    Width = 124
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Caption = 'read uncommitted'
    TabOrder = 13
  end
  object ALButtonSqlLite3Vacuum: TALButton
    Left = 16
    Top = 370
    Width = 161
    Height = 25
    Caption = 'VACUUM the database'
    TabOrder = 14
    OnClick = ALButtonSqlLite3VacuumClick
    OnPaint = ALButtonPaint
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 2
  end
  object OpenDialog2: TOpenDialog
    Left = 32
    Top = 26
  end
end
