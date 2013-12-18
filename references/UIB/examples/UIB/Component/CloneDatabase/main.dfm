object MainForm: TMainForm
  Left = 198
  Top = 107
  Caption = 'Clony&Pumpy - The Famous Firebird Databases Tool'
  ClientHeight = 516
  ClientWidth = 542
  Color = clBtnFace
  Constraints.MinHeight = 500
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    542
    516)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 400
    Top = 111
    Width = 134
    Height = 122
    Anchors = [akTop, akRight]
    Caption = 'Options'
    TabOrder = 1
    object cbVerbose: TCheckBox
      Left = 8
      Top = 43
      Width = 97
      Height = 17
      Caption = 'Verbose'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbCloseWhenDone: TCheckBox
      Left = 8
      Top = 66
      Width = 113
      Height = 17
      Caption = 'Close after finished'
      TabOrder = 1
    end
    object cbFailsafeDataPump: TCheckBox
      Left = 8
      Top = 20
      Width = 113
      Height = 17
      Hint = 'Commit after each record - slower'
      Caption = 'Secure data pump'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
  end
  object Log: TMemo
    Left = 8
    Top = 239
    Width = 526
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 8
    Width = 526
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Databases'
    TabOrder = 2
    DesignSize = (
      526
      97)
    object Label1: TLabel
      Left = 11
      Top = 76
      Width = 383
      Height = 13
      Caption = 
        '(Beware of database connection options like SQLDialect, Charset,' +
        ' Role Name ...)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clInactiveCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btDstDatabase: TButton
      Left = 443
      Top = 47
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Destination ...'
      TabOrder = 0
      OnClick = btDstDatabaseClick
    end
    object btSrcDatabase: TButton
      Left = 443
      Top = 16
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Source ...'
      TabOrder = 1
      OnClick = btSrcDatabaseClick
    end
    object edSrcDatabase: TEdit
      Left = 8
      Top = 18
      Width = 429
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = edSrcDatabaseChange
    end
    object edDstDatabase: TEdit
      Left = 8
      Top = 49
      Width = 429
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = edDstDatabaseChange
    end
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 111
    Width = 386
    Height = 122
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'Clone'
      DesignSize = (
        378
        94)
      object cbReplace: TCheckBox
        Left = 3
        Top = 3
        Width = 148
        Height = 17
        Caption = 'Replace dest. database'
        TabOrder = 0
      end
      object cbMetadataOnly: TCheckBox
        Left = 3
        Top = 26
        Width = 97
        Height = 17
        Caption = 'Metadata only'
        TabOrder = 1
      end
      object cbIgnoreConstraints: TCheckBox
        Left = 3
        Top = 49
        Width = 205
        Height = 17
        Caption = 'Skip indices and relational constraints'
        TabOrder = 2
      end
      object cbPageSize: TComboBox
        Left = 214
        Top = 22
        Width = 145
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
      end
      object cbOverrideSourcePageSize: TCheckBox
        Left = 196
        Top = 3
        Width = 179
        Height = 17
        Caption = 'Override src. database page size'
        TabOrder = 4
        OnClick = cbOverrideSourcePageSizeClick
      end
      object cbInternalNames: TCheckBox
        Left = 3
        Top = 72
        Width = 205
        Height = 17
        Caption = 'Force internal names'
        TabOrder = 5
      end
      object btStartClone: TButton
        Left = 300
        Top = 66
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Clone'
        TabOrder = 6
        OnClick = btStartCloneClick
      end
      object cbSkipGrants: TCheckBox
        Left = 214
        Top = 49
        Width = 161
        Height = 17
        Caption = 'Skip grants'
        TabOrder = 7
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Pump'
      ImageIndex = 1
      DesignSize = (
        378
        94)
      object cbEmptyTables: TCheckBox
        Left = 3
        Top = 3
        Width = 97
        Height = 17
        Caption = 'Empty tables'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object btStartPump: TButton
        Left = 300
        Top = 66
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Pump'
        TabOrder = 1
        OnClick = btStartPumpClick
      end
    end
  end
  object SrcDatabase: TUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA')
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 248
  end
  object SrcTransaction: TUIBTransaction
    DataBase = SrcDatabase
    Options = [tpConsistency, tpRead]
    Left = 16
    Top = 280
  end
  object SrcQuery: TUIBQuery
    Transaction = SrcTransaction
    DataBase = SrcDatabase
    CachedFetch = False
    FetchBlobs = True
    Left = 16
    Top = 312
  end
  object DstDatabase: TUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA')
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 48
    Top = 248
  end
  object DstTransaction: TUIBTransaction
    DataBase = DstDatabase
    Options = [tpNowait, tpWrite, tpReadCommitted, tpNoRecVersion, tpNoAutoUndo]
    Left = 48
    Top = 280
  end
  object XPManifest1: TXPManifest
    Left = 48
    Top = 344
  end
end
