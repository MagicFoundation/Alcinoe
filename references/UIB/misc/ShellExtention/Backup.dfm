object BackupForm: TBackupForm
  Left = 388
  Top = 316
  ActiveControl = btGO
  Caption = 'Backup database'
  ClientHeight = 393
  ClientWidth = 502
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
    502
    393)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Backup file'
  end
  object edBackupFile: TEdit
    Left = 8
    Top = 24
    Width = 457
    Height = 21
    TabOrder = 0
    OnChange = ConfigChange
    OnKeyPress = FormKeyPress
  end
  object btBrowse: TButton
    Left = 464
    Top = 24
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btBrowseClick
    OnKeyPress = FormKeyPress
  end
  object log: TMemo
    Left = 8
    Top = 184
    Width = 483
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
    OnKeyPress = FormKeyPress
  end
  object cbVerbose: TCheckBox
    Left = 288
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Verbose'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnKeyPress = FormKeyPress
  end
  object btGO: TButton
    Left = 338
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 1
    OnClick = btGOClick
    OnKeyPress = FormKeyPress
  end
  object btClose: TButton
    Left = 418
    Top = 360
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = btCloseClick
    OnKeyPress = FormKeyPress
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 56
    Width = 273
    Height = 89
    Caption = 'Options'
    TabOrder = 6
    object cbIgnoreChecksums: TCheckBox
      Left = 8
      Top = 16
      Width = 113
      Height = 17
      Caption = 'Ignore Checksums'
      TabOrder = 0
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbIgnoreLimbo: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Ignore Limbo'
      TabOrder = 1
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbMetadataOnly: TCheckBox
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'Metadata Only'
      TabOrder = 2
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbNoGarbageCollection: TCheckBox
      Left = 8
      Top = 64
      Width = 137
      Height = 17
      Caption = 'No Garbage Collection'
      TabOrder = 3
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbOldMetadataDesc: TCheckBox
      Left = 144
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Old Metadata Desc.'
      TabOrder = 4
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbNonTransportable: TCheckBox
      Left = 144
      Top = 32
      Width = 113
      Height = 17
      Caption = 'Non Transportable'
      TabOrder = 5
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbConvertExtTables: TCheckBox
      Left = 144
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Convert Ext. Tables'
      TabOrder = 6
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbExpand: TCheckBox
      Left = 144
      Top = 64
      Width = 97
      Height = 17
      Caption = 'Expand'
      TabOrder = 7
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
  end
  object cbSave: TCheckBox
    Left = 8
    Top = 359
    Width = 105
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save as default'
    TabOrder = 7
    OnKeyPress = FormKeyPress
  end
  object cbCloseWhenDone: TCheckBox
    Left = 288
    Top = 128
    Width = 113
    Height = 17
    Caption = 'Close after finished'
    TabOrder = 8
    OnKeyPress = FormKeyPress
  end
  object rgCompression: TRadioGroup
    Left = 8
    Top = 144
    Width = 337
    Height = 33
    Caption = 'Compression '
    Columns = 4
    ItemIndex = 0
    Items.Strings = (
      'none'
      '7-Zip'
      'WinRar'
      'WinAce')
    TabOrder = 9
  end
  object cbLocalHost: TCheckBox
    Left = 288
    Top = 88
    Width = 97
    Height = 17
    Caption = 'Localhost'
    TabOrder = 10
    OnKeyPress = FormKeyPress
  end
  object UIBBackup: TUIBBackup
    LibraryName = 'gds32.dll'
    OnVerbose = UIBBackupVerbose
    Verbose = True
    Left = 24
    Top = 264
  end
  object SaveDialog: TSaveDialog
    Filter = 'Backup file|*.gbk;*.fbk;*.ibk'
    Left = 56
    Top = 264
  end
end
