object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'ALFBXdemo'
  ClientHeight = 843
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 68
    Top = 103
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
    Top = 132
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
    Top = 161
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
    Top = 45
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
    Top = 312
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
    Top = 74
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
    Top = 190
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
    Top = 219
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
    Top = 412
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
    Top = 431
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
  object ALButtonFirebirdStartTransaction: TButton
    Left = 379
    Top = 284
    Width = 161
    Height = 25
    Caption = 'Start Transaction'
    Enabled = False
    TabOrder = 1
    OnClick = ALButtonFirebirdStartTransactionClick
  end
  object ALEditFirebirdLogin: TEdit
    Left = 106
    Top = 100
    Width = 249
    Height = 21
    TabOrder = 2
    Text = 'SYSDBA'
  end
  object ALEditFirebirdPassword: TEdit
    Left = 106
    Top = 129
    Width = 249
    Height = 21
    TabOrder = 3
  end
  object ALEditFirebirdCharset: TEdit
    Left = 106
    Top = 158
    Width = 72
    Height = 21
    TabOrder = 4
    Text = 'NONE'
  end
  object ALEditFirebirdLib: TEdit
    Left = 106
    Top = 42
    Width = 224
    Height = 21
    TabOrder = 5
    Text = 'FBClient.dll'
  end
  object ALMemoFireBirdQuery: TMemo
    Left = 106
    Top = 309
    Width = 249
    Height = 95
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object ALEditFirebirdDatabase: TEdit
    Left = 106
    Top = 71
    Width = 224
    Height = 21
    TabOrder = 7
  end
  object ALButtonFirebirdCommit: TButton
    Left = 379
    Top = 410
    Width = 161
    Height = 25
    Caption = 'Commit'
    Enabled = False
    TabOrder = 8
    OnClick = ALButtonFirebirdCommitClick
  end
  object ALButtonFirebirdSelect: TButton
    Left = 379
    Top = 347
    Width = 161
    Height = 25
    Caption = 'Execute SELECT'
    Enabled = False
    TabOrder = 9
    OnClick = ALButtonFirebirdSelectClick
  end
  object ALButtonFirebirdRollBack: TButton
    Left = 379
    Top = 441
    Width = 161
    Height = 25
    Caption = 'Rollback'
    Enabled = False
    TabOrder = 10
    OnClick = ALButtonFirebirdRollBackClick
  end
  object ALEditFireBirdNum_buffers: TEdit
    Left = 106
    Top = 187
    Width = 46
    Height = 21
    TabOrder = 11
    Text = '0'
  end
  object ALButtonFirebirdCreateDatabase: TButton
    Left = 379
    Top = 222
    Width = 161
    Height = 25
    Caption = 'Create Database'
    TabOrder = 12
    OnClick = ALButtonFirebirdCreateDatabaseClick
  end
  object ALComboBoxFirebirdapiVer: TComboBox
    Left = 106
    Top = 13
    Width = 145
    Height = 21
    Style = csDropDownList
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
  object ALMemoFirebirdTPB: TMemo
    Left = 106
    Top = 216
    Width = 249
    Height = 85
    Lines.Strings = (
      'isc_tpb_version3'
      'isc_tpb_write'
      'isc_tpb_read_committed'
      'isc_tpb_no_rec_version'
      'isc_tpb_nowait')
    ScrollBars = ssVertical
    TabOrder = 13
    WordWrap = False
  end
  object ALButtonFirebirdUpdate: TButton
    Left = 379
    Top = 378
    Width = 161
    Height = 25
    Caption = 'Execute Update'
    Enabled = False
    TabOrder = 14
    OnClick = ALButtonFirebirdUpdateClick
  end
  object ALButtonFirebirdOpenConnection: TButton
    Left = 379
    Top = 253
    Width = 161
    Height = 25
    Caption = 'Open Connection'
    TabOrder = 15
    OnClick = ALButtonFirebirdOpenConnectionClick
  end
  object ALButtonCloseConnection: TButton
    Left = 379
    Top = 472
    Width = 161
    Height = 25
    Caption = 'Close Connection'
    Enabled = False
    TabOrder = 16
    OnClick = ALButtonCloseConnectionClick
  end
  object ALButtonFirebirdPrepare: TButton
    Left = 379
    Top = 315
    Width = 161
    Height = 25
    Caption = 'Prepare'
    Enabled = False
    TabOrder = 17
    OnClick = ALButtonFirebirdPrepareClick
  end
  object Panel3: TPanel
    Left = 0
    Top = 520
    Width = 683
    Height = 323
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Color = 15986666
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 18
    object Splitter1: TSplitter
      Left = 414
      Top = 0
      Width = 5
      Height = 323
      Align = alRight
      Color = clBtnFace
      ParentColor = False
      ExplicitLeft = 459
      ExplicitTop = 1
      ExplicitHeight = 304
    end
    object ALMemoFirebirdResult: TMemo
      Left = 0
      Top = 0
      Width = 414
      Height = 323
      Align = alClient
      TabOrder = 0
    end
    object ALMemoFirebirdStats: TMemo
      Left = 419
      Top = 0
      Width = 264
      Height = 323
      Align = alRight
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object ALMemoFireBirdParams: TMemo
    Left = 106
    Top = 412
    Width = 249
    Height = 93
    ScrollBars = ssBoth
    TabOrder = 19
    WordWrap = False
  end
  object ALButtonFirebirdCommitRetaining: TButton
    Left = 552
    Top = 410
    Width = 119
    Height = 25
    Caption = 'Commit Retaining'
    Enabled = False
    TabOrder = 20
    OnClick = ALButtonFirebirdCommitRetainingClick
  end
  object ALButtonFirebirdRollBackRetaining: TButton
    Left = 552
    Top = 441
    Width = 119
    Height = 25
    Caption = 'Rollback Retaining'
    Enabled = False
    TabOrder = 21
    OnClick = ALButtonFirebirdRollBackRetainingClick
  end
  object Button1: TButton
    Left = 330
    Top = 42
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 22
    OnClick = ALEditFirebirdLibButtonClick
  end
  object Button2: TButton
    Left = 330
    Top = 71
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 23
    OnClick = ALEditFirebirdLibButtonClick
  end
  object OpenDialog1: TOpenDialog
    Left = 592
    Top = 219
  end
end
