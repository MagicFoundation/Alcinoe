object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 562
  ClientWidth = 698
  Color = 15986666
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 18
    Top = 16
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
    Left = 18
    Top = 360
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
    Left = 75
    Top = 93
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
    Left = 51
    Top = 118
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
    Left = 63
    Top = 143
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
    Left = 32
    Top = 43
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
  object Label18: TLabel
    Left = 52
    Top = 69
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
  object Label30: TLabel
    Left = 34
    Top = 167
    Width = 72
    Height = 13
    Caption = 'Num_buffers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 18
    Top = 210
    Width = 76
    Height = 13
    Caption = 'Event Names'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ALButton1: TALButton
    Left = 18
    Top = 320
    Width = 121
    Height = 25
    Caption = 'Start Listenig'
    TabOrder = 8
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 145
    Top = 320
    Width = 225
    Height = 25
    Caption = 'Stop Listening'
    TabOrder = 9
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALComboBoxFirebirdapiVer: TALComboBox
    Left = 113
    Top = 13
    Width = 145
    Height = 21
    OnPaint = ALComboBoxPaint
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'FB102'
    Items.Strings = (
      'FB102'
      'FB103'
      'FB15'
      'FB20'
      'FB25')
  end
  object ALMemoResult: TALMemo
    Left = 18
    Top = 376
    Width = 353
    Height = 169
    TabStop = False
    OnPaint = ALMemoPaint
    OnPaintScrollBar = ALMemoPaintScrollBar
    TabOrder = 10
    DesignSize = (
      353
      169)
  end
  object ALEditFirebirdLogin: TALEdit
    Left = 113
    Top = 90
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
    Text = 'SYSDBA'
  end
  object ALEditFirebirdPassword: TALEdit
    Left = 113
    Top = 115
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
  end
  object ALEditFirebirdCharset: TALEdit
    Left = 113
    Top = 140
    Width = 72
    Height = 19
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEditPaint
    TabOrder = 5
    Text = 'NONE'
  end
  object ALEditFirebirdLib: TALEdit
    Left = 113
    Top = 40
    Width = 249
    Height = 19
    OnButtonClick = ALEditFirebirdLibButtonClick
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
    Text = 'FBClient.dll'
  end
  object ALEditFirebirdDatabase: TALEdit
    Left = 113
    Top = 66
    Width = 249
    Height = 19
    Cursor = crArrow
    OnButtonClick = ALEditFirebirdDatabaseButtonClick
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
  end
  object ALEditFireBirdNum_buffers: TALEdit
    Left = 113
    Top = 164
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
    TabOrder = 6
    Text = '0'
  end
  object ALMemoFireBirdEventName: TALMemo
    Left = 18
    Top = 229
    Width = 353
    Height = 79
    OnPaint = ALMemoPaint
    OnPaintScrollBar = ALMemoPaintScrollBar
    TabOrder = 7
    DesignSize = (
      353
      79)
  end
  object Panel2: TPanel
    Left = 389
    Top = 8
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 11
    object Label7: TLabel
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
    object Label8: TLabel
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
    object Panel3: TPanel
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
    Left = 354
    Top = 32
  end
  object OpenDialog2: TOpenDialog
    Left = 354
    Top = 64
  end
end
