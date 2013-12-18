object RestoreForm: TRestoreForm
  Left = 419
  Top = 312
  ActiveControl = btGO
  Caption = 'Restore database'
  ClientHeight = 353
  ClientWidth = 505
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
    505
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Restore file'
  end
  object edRestoreFile: TEdit
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
    TabOrder = 1
    OnClick = btBrowseClick
    OnKeyPress = FormKeyPress
  end
  object log: TMemo
    Left = 8
    Top = 152
    Width = 486
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    OnKeyPress = FormKeyPress
  end
  object cbVerbose: TCheckBox
    Left = 352
    Top = 64
    Width = 97
    Height = 17
    Caption = 'Verbose'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnKeyPress = FormKeyPress
  end
  object btGO: TButton
    Left = 341
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 4
    OnClick = btGOClick
    OnKeyPress = FormKeyPress
  end
  object btClose: TButton
    Left = 421
    Top = 320
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
    Width = 337
    Height = 89
    Caption = 'Options'
    TabOrder = 6
    object Label5: TLabel
      Left = 150
      Top = 64
      Width = 43
      Height = 13
      Caption = 'Pagesize'
    end
    object cbDeactivateIndexes: TCheckBox
      Left = 8
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Deactivate Indexes'
      TabOrder = 0
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbNoShadow: TCheckBox
      Left = 8
      Top = 32
      Width = 97
      Height = 17
      Caption = 'No Shadow'
      TabOrder = 1
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbNoValidityCheck: TCheckBox
      Left = 8
      Top = 48
      Width = 97
      Height = 17
      Caption = 'No Validity Check'
      TabOrder = 2
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbOneRelationAtATime: TCheckBox
      Left = 8
      Top = 64
      Width = 129
      Height = 17
      Caption = 'One Relation at a time'
      TabOrder = 3
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbReplace: TCheckBox
      Left = 152
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Replace'
      TabOrder = 4
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbCreateNewDB: TCheckBox
      Left = 152
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Create New DB'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object cbUseAllSpace: TCheckBox
      Left = 152
      Top = 40
      Width = 97
      Height = 17
      Caption = 'Use All Space'
      TabOrder = 6
      OnClick = ConfigChange
      OnKeyPress = FormKeyPress
    end
    object edPageSize: TEdit
      Left = 200
      Top = 60
      Width = 81
      Height = 21
      TabOrder = 7
      Text = '4096'
      OnChange = ConfigChange
      OnKeyPress = FormKeyPress
    end
  end
  object cbSave: TCheckBox
    Left = 8
    Top = 319
    Width = 105
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Save as default'
    TabOrder = 7
    OnKeyPress = FormKeyPress
  end
  object cbCloseWhenDone: TCheckBox
    Left = 352
    Top = 120
    Width = 113
    Height = 17
    Caption = 'Close after finished'
    TabOrder = 8
    OnKeyPress = FormKeyPress
  end
  object cbLocalHost: TCheckBox
    Left = 351
    Top = 87
    Width = 97
    Height = 17
    Caption = 'Localhost'
    TabOrder = 9
    OnKeyPress = FormKeyPress
  end
  object SaveDialog: TSaveDialog
    Filter = 'Restore file|*.gdb;*.fdb;*.ib'
    Left = 56
    Top = 264
  end
  object UIBRestore: TUIBRestore
    LibraryName = 'gds32.dll'
    OnVerbose = UIBRestoreVerbose
    Verbose = True
    PageSize = 4096
    Left = 16
    Top = 264
  end
end
