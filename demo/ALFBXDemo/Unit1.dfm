object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 537
  ClientWidth = 705
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 176
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
  object Label4: TLabel
    Left = 16
    Top = 52
    Width = 91
    Height = 13
    Caption = 'Database Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 80
    Width = 100
    Height = 13
    Caption = 'Database Params'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 20
    Width = 89
    Height = 13
    Caption = 'Firebird Version'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 16
    Top = 336
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
  object ALButton1: TALButton
    Left = 16
    Top = 296
    Width = 121
    Height = 25
    Caption = 'Execute SELECT SQL'
    TabOrder = 4
    OnClick = ALButton1Click
    OnPaint = ALButton1Paint
  end
  object ALButton2: TALButton
    Left = 144
    Top = 296
    Width = 225
    Height = 25
    Caption = 'Execute INSERT/UPDATE/DELETE SQL'
    TabOrder = 5
    OnClick = ALButton2Click
    OnPaint = ALButton1Paint
  end
  object Memo_SQL: TALMemo
    Left = 16
    Top = 192
    Width = 353
    Height = 84
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    TabOrder = 3
    DesignSize = (
      353
      84)
  end
  object edt_DataBaseName: TALEdit
    Left = 120
    Top = 49
    Width = 249
    Height = 19
    OnButtonClick = edt_DataBaseNameButtonClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEdit1Paint
    ReadOnly = True
    TabOrder = 1
  end
  object Memo_DatabaseParams: TALMemo
    Left = 16
    Top = 96
    Width = 353
    Height = 65
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    Lines.Strings = (
      'user_name='
      'password='
      'lc_ctype=')
    TabOrder = 2
    DesignSize = (
      353
      65)
  end
  object ComboBox_apiVer: TALComboBox
    Left = 120
    Top = 16
    Width = 145
    Height = 21
    OnPaint = ComboBox_apiVerPaint
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      'FB102'
      'FB103'
      'FB15'
      'FB20'
      'FB25')
  end
  object Memo_SelectResult: TALMemo
    Left = 16
    Top = 352
    Width = 353
    Height = 169
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    TabOrder = 6
    DesignSize = (
      353
      169)
  end
  object Panel1: TPanel
    Left = 395
    Top = 8
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 7
    object Label8: TLabel
      Left = 5
      Top = 8
      Width = 132
      Height = 45
      Caption = 'Please help us to keep the development of these components free'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label12: TLabel
      Left = 5
      Top = 63
      Width = 125
      Height = 75
      Caption = 
        'If you like these components please simply click on each button ' +
        'below ... thanks for your support !'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel2: TPanel
      Left = 151
      Top = 8
      Width = 130
      Height = 134
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clWhite
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object PanelWebBrowser: TPanel
        Left = -5
        Top = -23
        Width = 133
        Height = 159
        BevelOuter = bvNone
        Color = clMedGray
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 0
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 184
    Top = 320
  end
end
