object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'ALFBXdemo'
  ClientHeight = 806
  ClientWidth = 683
  Color = 15986666
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
  object Label2: TLabel
    Left = 68
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
    Left = 44
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
    Left = 56
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
    Left = 25
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
  object Label17: TLabel
    Left = 75
    Top = 285
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
    Left = 45
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
    Left = 27
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
  object Label31: TLabel
    Left = 11
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
  object Label1: TLabel
    Left = 75
    Top = 192
    Width = 25
    Height = 13
    Caption = 'TPB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 58
    Top = 386
    Width = 42
    Height = 13
    Caption = 'Params'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 32
    Top = 405
    Width = 68
    Height = 13
    Caption = '(1 item by row)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 379
    Top = 13
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
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
  object ALButtonFirebirdStartTransaction: TALButton
    Left = 379
    Top = 257
    Width = 161
    Height = 25
    Caption = 'Start Transaction'
    Enabled = False
    TabOrder = 2
    OnClick = ALButtonFirebirdStartTransactionClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALEditFirebirdLogin: TALEdit
    Left = 106
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
    OnPaint = ALEdit1Paint
    TabOrder = 3
    Text = 'SYSDBA'
  end
  object ALEditFirebirdPassword: TALEdit
    Left = 106
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
    OnPaint = ALEdit1Paint
    TabOrder = 4
  end
  object ALEditFirebirdCharset: TALEdit
    Left = 106
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
    OnPaint = ALEdit1Paint
    TabOrder = 5
    Text = 'NONE'
  end
  object ALEditFirebirdLib: TALEdit
    Left = 106
    Top = 40
    Width = 249
    Height = 19
    Cursor = crArrow
    OnButtonClick = ALEditFirebirdLibButtonClick
    btnVisible = True
    btnCaption = '...'
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = ALEdit1Paint
    TabOrder = 6
    Text = 'FBClient.dll'
  end
  object ALMemoFireBirdQuery: TALMemo
    Left = 106
    Top = 282
    Width = 249
    Height = 95
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    TabOrder = 7
    WordWrap = False
    DesignSize = (
      249
      95)
  end
  object ALEditFirebirdDatabase: TALEdit
    Left = 106
    Top = 66
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
    OnPaint = ALEdit1Paint
    TabOrder = 8
  end
  object ALButtonFirebirdCommit: TALButton
    Left = 379
    Top = 383
    Width = 161
    Height = 25
    Caption = 'Commit'
    Enabled = False
    TabOrder = 9
    OnClick = ALButtonFirebirdCommitClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonFirebirdSelect: TALButton
    Left = 379
    Top = 320
    Width = 161
    Height = 25
    Caption = 'Execute SELECT'
    Enabled = False
    TabOrder = 10
    OnClick = ALButtonFirebirdSelectClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonFirebirdRollBack: TALButton
    Left = 379
    Top = 414
    Width = 161
    Height = 25
    Caption = 'Rollback'
    Enabled = False
    TabOrder = 11
    OnClick = ALButtonFirebirdRollBackClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALEditFireBirdNum_buffers: TALEdit
    Left = 106
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
    OnPaint = ALEdit1Paint
    TabOrder = 12
    Text = '0'
  end
  object ALButtonFirebirdCreateDatabase: TALButton
    Left = 379
    Top = 195
    Width = 161
    Height = 25
    Caption = 'Create Database'
    TabOrder = 13
    OnClick = ALButtonFirebirdCreateDatabaseClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALComboBoxFirebirdapiVer: TALComboBox
    Left = 106
    Top = 13
    Width = 145
    Height = 21
    OnPaint = ComboBox_apiVerPaint
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 4
    TabOrder = 0
    Text = 'FB25'
    Items.Strings = (
      'FB102'
      'FB103'
      'FB15'
      'FB20'
      'FB25')
  end
  object ALMemoFirebirdTPB: TALMemo
    Left = 106
    Top = 189
    Width = 249
    Height = 85
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    Lines.Strings = (
      'isc_tpb_version3'
      'isc_tpb_write'
      'isc_tpb_read_committed'
      'isc_tpb_no_rec_version'
      'isc_tpb_nowait')
    TabOrder = 14
    WordWrap = False
    DesignSize = (
      249
      85)
  end
  object ALButtonFirebirdUpdate: TALButton
    Left = 379
    Top = 351
    Width = 161
    Height = 25
    Caption = 'Execute Update'
    Enabled = False
    TabOrder = 15
    OnClick = ALButtonFirebirdUpdateClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonFirebirdOpenConnection: TALButton
    Left = 379
    Top = 226
    Width = 161
    Height = 25
    Caption = 'Open Connection'
    TabOrder = 16
    OnClick = ALButtonFirebirdOpenConnectionClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonCloseConnection: TALButton
    Left = 379
    Top = 445
    Width = 161
    Height = 25
    Caption = 'Close Connection'
    Enabled = False
    TabOrder = 17
    OnClick = ALButtonCloseConnectionClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonFirebirdPrepare: TALButton
    Left = 379
    Top = 288
    Width = 161
    Height = 25
    Caption = 'Prepare'
    Enabled = False
    TabOrder = 18
    OnClick = ALButtonFirebirdPrepareClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object Panel3: TPanel
    Left = 0
    Top = 496
    Width = 683
    Height = 310
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = 15986666
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 19
    object Splitter1: TSplitter
      Left = 414
      Top = 0
      Width = 5
      Height = 310
      Align = alRight
      ExplicitLeft = 459
      ExplicitTop = 1
      ExplicitHeight = 304
    end
    object ALMemoFirebirdResult: TALMemo
      Left = 0
      Top = 0
      Width = 414
      Height = 310
      OnPaint = Memo_SQLPaint
      OnPaintScrollBar = Memo_SQLPaintScrollBar
      Align = alClient
      TabOrder = 0
      ExplicitTop = 1
      ExplicitWidth = 412
      ExplicitHeight = 308
      DesignSize = (
        414
        310)
    end
    object ALMemoFirebirdStats: TALMemo
      Left = 419
      Top = 0
      Width = 264
      Height = 310
      OnPaint = Memo_SQLPaint
      OnPaintScrollBar = Memo_SQLPaintScrollBar
      Align = alRight
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitLeft = 418
      ExplicitTop = 1
      ExplicitHeight = 308
      DesignSize = (
        264
        310)
    end
  end
  object ALMemoFireBirdParams: TALMemo
    Left = 106
    Top = 386
    Width = 249
    Height = 93
    OnPaint = Memo_SQLPaint
    OnPaintScrollBar = Memo_SQLPaintScrollBar
    TabOrder = 20
    WordWrap = False
    DesignSize = (
      249
      93)
  end
  object ALButtonFirebirdCommitRetaining: TALButton
    Left = 552
    Top = 383
    Width = 119
    Height = 25
    Caption = 'Commit Retaining'
    Enabled = False
    TabOrder = 21
    OnClick = ALButtonFirebirdCommitRetainingClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object ALButtonFirebirdRollBackRetaining: TALButton
    Left = 552
    Top = 414
    Width = 119
    Height = 25
    Caption = 'Rollback Retaining'
    Enabled = False
    TabOrder = 22
    OnClick = ALButtonFirebirdRollBackRetainingClick
    OnPaint = ALButtonFirebirdUpdatePaint
  end
  object OpenDialog1: TOpenDialog
    Left = 328
    Top = 8
  end
end
