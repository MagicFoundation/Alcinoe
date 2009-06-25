object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 669
  ClientWidth = 733
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 404
    Top = 67
    Width = 63
    Height = 13
    Caption = 'Host Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 435
    Top = 141
    Width = 32
    Height = 13
    Caption = 'Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 411
    Top = 166
    Width = 55
    Height = 13
    Caption = 'Password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 396
    Top = 92
    Width = 71
    Height = 13
    Caption = 'Port Number'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 423
    Top = 191
    Width = 44
    Height = 13
    Caption = 'Charset'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label11: TLabel
    Left = 405
    Top = 42
    Width = 62
    Height = 13
    Caption = 'libmysql.dll'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 384
    Top = 229
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
  object Bevel2: TBevel
    Left = 361
    Top = 32
    Width = 5
    Height = 361
  end
  object Label12: TLabel
    Left = 412
    Top = 117
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
  object Label13: TLabel
    Left = 8
    Top = 397
    Width = 37
    Height = 13
    Caption = 'Result'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 59
    Top = 92
    Width = 32
    Height = 13
    Caption = 'Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 35
    Top = 117
    Width = 55
    Height = 13
    Caption = 'Password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label15: TLabel
    Left = 47
    Top = 142
    Width = 44
    Height = 13
    Caption = 'Charset'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label16: TLabel
    Left = 16
    Top = 42
    Width = 75
    Height = 13
    Caption = 'FBClient DLL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label17: TLabel
    Left = 8
    Top = 229
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
  object Label18: TLabel
    Left = 36
    Top = 68
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
  object Label1: TLabel
    Left = 97
    Top = 8
    Width = 77
    Height = 19
    Caption = 'FIREBIRD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label14: TLabel
    Left = 473
    Top = 8
    Width = 56
    Height = 19
    Caption = 'MYSQL'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 266
    Top = 592
    Width = 200
    Height = 70
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 17
    object Label5: TLabel
      Left = 5
      Top = 8
      Width = 189
      Height = 52
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
  object ALButtonFirebird: TALButton
    Left = 8
    Top = 352
    Width = 161
    Height = 25
    Caption = 'Execute SELECT via FireBird'
    TabOrder = 6
    OnClick = ALButtonFirebirdClick
    OnPaint = ALButtonPaint
  end
  object ALEditMySqlHostName: TALEdit
    Left = 473
    Top = 64
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 8
    Text = 'localhost'
  end
  object ALEditMySqlLogin: TALEdit
    Left = 473
    Top = 138
    Width = 249
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
    Text = 'root'
  end
  object ALEditMySqlPassword: TALEdit
    Left = 473
    Top = 163
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 12
  end
  object ALEditMySqlPortNumber: TALEdit
    Left = 473
    Top = 89
    Width = 88
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 9
    Text = '3306'
  end
  object ALEditMySqlCharset: TALEdit
    Left = 473
    Top = 188
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 13
    Text = 'binary'
  end
  object ALEditMysqlLybMsqldll: TALEdit
    Left = 473
    Top = 39
    Width = 249
    Height = 19
    OnButtonClick = ALEditButtonFindFileClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 7
    Text = 'libmysql.dll'
  end
  object ALMemoMySqlQuery: TALMemo
    Left = 384
    Top = 248
    Width = 338
    Height = 84
    OnPaint = ALMemoPaint
    TabOrder = 14
    DesignSize = (
      338
      84)
  end
  object ALButtonMySQLExecute: TALButton
    Left = 384
    Top = 352
    Width = 161
    Height = 25
    Caption = 'Execute SELECT via MySql'
    TabOrder = 15
    OnClick = ALButtonMySqlClick
    OnPaint = ALButtonPaint
  end
  object ALEditMySqlDatabase: TALEdit
    Left = 473
    Top = 114
    Width = 249
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
  end
  object ALMemoResult: TALMemo
    Left = 8
    Top = 416
    Width = 714
    Height = 162
    OnPaint = ALMemoPaint
    TabOrder = 16
    DesignSize = (
      714
      162)
  end
  object ALEditFirebirdLogin: TALEdit
    Left = 97
    Top = 89
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 2
    Text = 'SYSDBA'
  end
  object ALEditFirebirdPassword: TALEdit
    Left = 97
    Top = 114
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 3
  end
  object ALEditFirebirdCharset: TALEdit
    Left = 97
    Top = 139
    Width = 249
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 4
    Text = 'NONE'
  end
  object ALEditFirebirdFBClientDLL: TALEdit
    Left = 97
    Top = 39
    Width = 249
    Height = 19
    OnButtonClick = ALEditButtonFindFileClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 0
    Text = 'FBClient.dll'
  end
  object ALMemoFireBirdQuery: TALMemo
    Left = 8
    Top = 248
    Width = 338
    Height = 84
    OnPaint = ALMemoPaint
    TabOrder = 5
    DesignSize = (
      338
      84)
  end
  object ALEditFirebirdDatabase: TALEdit
    Left = 97
    Top = 65
    Width = 249
    Height = 19
    OnButtonClick = ALEditButtonFindFileClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 560
    Top = 368
  end
end
