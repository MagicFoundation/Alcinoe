object CloneForm: TCloneForm
  Left = 529
  Top = 335
  ActiveControl = btStart
  Caption = 'Clone'
  ClientHeight = 382
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  DesignSize = (
    531
    382)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'Clone file'
  end
  object Log: TMemo
    Left = 8
    Top = 143
    Width = 513
    Height = 202
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btStart: TButton
    Left = 368
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Start'
    Default = True
    TabOrder = 1
    OnClick = btStartClick
    OnKeyPress = FormKeyPress
  end
  object btClose: TButton
    Left = 448
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = btCloseClick
    OnKeyPress = FormKeyPress
  end
  object cbSave: TCheckBox
    Left = 8
    Top = 355
    Width = 105
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save as default'
    TabOrder = 3
    OnKeyPress = FormKeyPress
  end
  object edCloneFile: TEdit
    Left = 8
    Top = 24
    Width = 457
    Height = 21
    TabOrder = 4
    OnChange = ConfigChange
    OnKeyPress = FormKeyPress
  end
  object btBrowse: TButton
    Left = 464
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btBrowseClick
    OnKeyPress = FormKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 49
    Width = 338
    Height = 88
    Caption = 'Options '
    TabOrder = 6
    object cbReplace: TCheckBox
      Left = 8
      Top = 16
      Width = 73
      Height = 17
      Caption = 'Replace'
      TabOrder = 0
      OnKeyPress = FormKeyPress
    end
    object cbMetadataOnly: TCheckBox
      Left = 8
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Metadata Only'
      TabOrder = 1
      OnKeyPress = FormKeyPress
    end
    object cbPageSize: TComboBox
      Left = 178
      Top = 35
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
    end
    object cbOverrideSourcePageSize: TCheckBox
      Left = 160
      Top = 16
      Width = 161
      Height = 17
      Caption = 'Override source page size'
      TabOrder = 3
      OnClick = cbOverrideSourcePageSizeClick
      OnKeyPress = FormKeyPress
    end
    object cbIgnoreConstraints: TCheckBox
      Left = 8
      Top = 62
      Width = 321
      Height = 17
      Caption = 'Do not restore indices and relational constraints'
      TabOrder = 4
      OnKeyPress = FormKeyPress
    end
  end
  object GroupBox2: TGroupBox
    Left = 352
    Top = 48
    Width = 171
    Height = 89
    Caption = 'Clone Options'
    TabOrder = 7
    object cbVerbose: TCheckBox
      Left = 11
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Verbose'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnKeyPress = FormKeyPress
    end
    object cbCloseWhenDone: TCheckBox
      Left = 11
      Top = 39
      Width = 113
      Height = 17
      Caption = 'Close after finished'
      TabOrder = 1
      OnKeyPress = FormKeyPress
    end
    object cbLocalHost: TCheckBox
      Left = 11
      Top = 63
      Width = 97
      Height = 17
      Caption = 'Localhost'
      TabOrder = 2
      OnKeyPress = FormKeyPress
    end
  end
  object Source: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 152
  end
  object Destination: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 184
  end
  object SaveDialog: TSaveDialog
    Filter = 'Backup file|*.gdb;*.fdb;*.ib'
    Left = 16
    Top = 216
  end
  object SrcTransaction: TUIBTransaction
    DataBase = Source
    Options = [tpConcurrency, tpWait, tpRead]
    Left = 48
    Top = 152
  end
  object DstTransaction: TUIBTransaction
    DataBase = Destination
    Options = [tpConcurrency, tpWait, tpWrite, tpNoAutoUndo]
    Left = 48
    Top = 184
  end
  object SrcQuery: TUIBQuery
    Transaction = SrcTransaction
    CachedFetch = False
    FetchBlobs = True
    Left = 80
    Top = 152
  end
end
