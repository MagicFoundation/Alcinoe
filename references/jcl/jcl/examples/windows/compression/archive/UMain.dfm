object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 301
  ClientWidth = 771
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 81
    Width = 771
    Height = 204
    Align = alClient
    Columns = <
      item
        Caption = 'Local name'
        Width = 150
      end
      item
        Caption = 'Archive name'
        Width = 150
      end
      item
        Caption = 'Size'
        Width = 30
      end
      item
        Caption = 'Compressed'
        Width = 30
      end
      item
        Caption = 'Creation'
      end
      item
        Caption = 'Last access'
      end
      item
        Caption = 'Last write'
      end
      item
        Caption = 'Comment'
        Width = 30
      end
      item
        Caption = 'OS'
        Width = 20
      end
      item
        Caption = 'FS'
        Width = 20
      end
      item
        Caption = 'User'
        Width = 20
      end
      item
        Caption = 'Group'
        Width = 20
      end
      item
        Caption = 'CRC'
      end>
    MultiSelect = True
    OwnerData = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 285
    Width = 771
    Height = 16
    Align = alBottom
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 771
    Height = 81
    ActivePage = TabSheetReadOnly
    Align = alTop
    TabOrder = 2
    object TabSheetReadOnly: TTabSheet
      Caption = 'Read-only'
      object ButtonOpen: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionOpenRO
        TabOrder = 0
      end
      object ButtonExtractSelected: TButton
        Left = 97
        Top = 16
        Width = 96
        Height = 25
        Action = ActionExtractSelectedRO
        TabOrder = 1
      end
      object ButtonExtractAll: TButton
        Left = 199
        Top = 16
        Width = 75
        Height = 25
        Action = ActionExtractAllRO
        TabOrder = 2
      end
      object ButtonROProperties: TButton
        Left = 280
        Top = 16
        Width = 75
        Height = 25
        Action = ActionProperties
        TabOrder = 3
      end
      object ButtonDescend: TButton
        Left = 361
        Top = 16
        Width = 75
        Height = 25
        Action = ActionDescendRO
        TabOrder = 4
      end
      object ButtonLevelUp: TButton
        Left = 442
        Top = 16
        Width = 75
        Height = 25
        Action = ActionLevelUpRO
        TabOrder = 5
      end
    end
    object TabSheetWriteOnly: TTabSheet
      Caption = 'Write-only'
      ImageIndex = 1
      object ButtonNew: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionNewWO
        TabOrder = 0
      end
      object ButtonAddFile: TButton
        Left = 97
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddFile
        TabOrder = 1
      end
      object ButtonAddDirectory: TButton
        Left = 178
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddDirectory
        TabOrder = 2
      end
      object ButtonSave: TButton
        Left = 259
        Top = 16
        Width = 75
        Height = 25
        Action = ActionSave
        TabOrder = 3
      end
      object ButtonPropertiesWO: TButton
        Left = 340
        Top = 16
        Width = 75
        Height = 25
        Action = ActionProperties
        TabOrder = 4
      end
    end
    object TabSheetReadWrite: TTabSheet
      Caption = 'Read and write'
      ImageIndex = 2
      object ButtonNewRW: TButton
        Left = 16
        Top = 16
        Width = 75
        Height = 25
        Action = ActionNewRW
        TabOrder = 0
      end
      object ButtonOpenRW: TButton
        Left = 97
        Top = 16
        Width = 75
        Height = 25
        Action = ActionOpenRW
        TabOrder = 1
      end
      object ButtonDeleteRW: TButton
        Left = 178
        Top = 16
        Width = 75
        Height = 25
        Action = ActionDeleteRW
        TabOrder = 2
      end
      object ButtonAddFileRW: TButton
        Left = 259
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddFile
        TabOrder = 3
      end
      object ButtonAddDirectoryRW: TButton
        Left = 340
        Top = 16
        Width = 75
        Height = 25
        Action = ActionAddDirectory
        TabOrder = 4
      end
      object ButtonExtractSelectedRW: TButton
        Left = 421
        Top = 16
        Width = 92
        Height = 25
        Action = ActionExtractSelectedRO
        TabOrder = 5
      end
      object ButtonExtractAllRW: TButton
        Left = 519
        Top = 16
        Width = 75
        Height = 25
        Action = ActionExtractAllRO
        TabOrder = 6
      end
      object ButtonSaveRW: TButton
        Left = 600
        Top = 16
        Width = 75
        Height = 25
        Action = ActionSave
        TabOrder = 7
      end
      object ButtonPropertiesRW: TButton
        Left = 681
        Top = 16
        Width = 75
        Height = 25
        Action = ActionProperties
        TabOrder = 8
      end
    end
  end
  object ActionList1: TActionList
    Left = 64
    Top = 152
    object ActionOpenRO: TAction
      Category = 'ReadOnly'
      Caption = '&Open'
      OnExecute = ActionOpenROExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionExtractSelectedRO: TAction
      Category = 'ReadOnly'
      Caption = '&Extract selected'
      OnExecute = ActionExtractSelectedROExecute
      OnUpdate = ActionExtractSelectedROUpdate
    end
    object ActionExtractAllRO: TAction
      Category = 'ReadOnly'
      Caption = 'Extract &all'
      OnExecute = ActionExtractAllROExecute
      OnUpdate = ActionExtractAllROUpdate
    end
    object ActionNewWO: TAction
      Category = 'WriteOnly'
      Caption = '&New'
      OnExecute = ActionNewWOExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionAddFile: TAction
      Category = 'Write'
      Caption = 'Add &file'
      OnExecute = ActionAddFileExecute
      OnUpdate = ActionAddFileUpdate
    end
    object ActionAddDirectory: TAction
      Category = 'Write'
      Caption = 'Add &directory'
      OnExecute = ActionAddDirectoryExecute
      OnUpdate = ActionAddDirectoryUpdate
    end
    object ActionSave: TAction
      Category = 'Write'
      Caption = '&Save'
      OnExecute = ActionSaveExecute
      OnUpdate = ActionSaveUpdate
    end
    object ActionDeleteRW: TAction
      Category = 'ReadWrite'
      Caption = '&Delete'
      OnExecute = ActionDeleteRWExecute
      OnUpdate = ActionDeleteRWUpdate
    end
    object ActionNewRW: TAction
      Category = 'ReadWrite'
      Caption = '&New'
      OnExecute = ActionNewRWExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionOpenRW: TAction
      Category = 'ReadWrite'
      Caption = '&Open'
      OnExecute = ActionOpenRWExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionProperties: TAction
      Category = 'ReadWrite'
      Caption = '&Properties'
      OnExecute = ActionPropertiesExecute
      OnUpdate = ActionPropertiesUpdate
    end
    object ActionDescendRO: TAction
      Category = 'ReadOnly'
      Caption = 'Descend'
      OnExecute = ActionDescendROExecute
      OnUpdate = ActionDescendROUpdate
    end
    object ActionLevelUpRO: TAction
      Category = 'ReadOnly'
      Caption = 'Level up'
      OnExecute = ActionLevelUpROExecute
      OnUpdate = ActionLevelUpROUpdate
    end
  end
  object OpenDialogArchiveRO: TOpenDialog
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open an archive for extraction'
    Left = 104
    Top = 152
  end
  object SaveDialogArchiveWO: TSaveDialog
    DefaultExt = '*.zip'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Create a new archive'
    Left = 144
    Top = 152
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 184
    Top = 152
  end
  object OpenDialogArchiveRW: TOpenDialog
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open an archive for modification'
    Left = 104
    Top = 184
  end
  object SaveDialogArchiveRW: TSaveDialog
    DefaultExt = '*.zip'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofCreatePrompt, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Create a new archive'
    Left = 144
    Top = 184
  end
end
