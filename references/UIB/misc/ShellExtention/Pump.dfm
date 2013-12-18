object PumpForm: TPumpForm
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
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 93
    Height = 13
    Caption = 'Pump data into file :'
  end
  object Log: TMemo
    Left = 8
    Top = 119
    Width = 513
    Height = 226
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
  object GroupBox2: TGroupBox
    Left = 279
    Top = 51
    Width = 244
    Height = 62
    Caption = 'Pump Options'
    TabOrder = 4
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
      Left = 131
      Top = 13
      Width = 97
      Height = 17
      Caption = 'Localhost'
      TabOrder = 2
      OnKeyPress = FormKeyPress
    end
  end
  object edPumpFile: TEdit
    Left = 8
    Top = 24
    Width = 457
    Height = 21
    TabOrder = 5
    OnChange = ConfigChange
    OnKeyPress = FormKeyPress
  end
  object Button1: TButton
    Left = 464
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 6
    OnClick = btBrowseClick
    OnKeyPress = FormKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 51
    Width = 265
    Height = 62
    Caption = 'Options'
    TabOrder = 7
    object cbEmptyTables: TCheckBox
      Left = 11
      Top = 16
      Width = 97
      Height = 17
      Caption = 'Empty tables'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnKeyPress = FormKeyPress
    end
  end
  object Source: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 160
  end
  object Destination: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 192
  end
  object SaveDialog: TSaveDialog
    Filter = 'Backup file|*.gdb;*.fdb;*.ib'
    Left = 16
    Top = 224
  end
  object SrcTransaction: TUIBTransaction
    DataBase = Source
    Options = [tpConcurrency, tpWait, tpRead]
    Left = 48
    Top = 160
  end
  object DstTransaction: TUIBTransaction
    DataBase = Destination
    Options = [tpConcurrency, tpWait, tpWrite, tpNoAutoUndo]
    Left = 48
    Top = 192
  end
  object SrcQuery: TUIBQuery
    Transaction = SrcTransaction
    CachedFetch = False
    FetchBlobs = True
    Left = 80
    Top = 160
  end
end
