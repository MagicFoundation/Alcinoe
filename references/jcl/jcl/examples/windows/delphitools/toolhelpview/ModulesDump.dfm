inherited ModulesDumpForm: TModulesDumpForm
  Left = 235
  Top = 159
  Width = 469
  Height = 336
  ActiveControl = ModulesListView
  Caption = 'Modules list'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited CoolBar: TCoolBar
    Width = 461
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 457
      end>
    inherited ToolBar: TToolBar
      Width = 444
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = Refresh1
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton2'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object ToolButton3: TToolButton
        Left = 31
        Top = 0
        Action = Copy1
      end
      object ToolButton4: TToolButton
        Left = 54
        Top = 0
        Action = SaveToFile1
      end
      object ToolButton10: TToolButton
        Left = 77
        Top = 0
        Action = Find1
      end
      object ToolButton5: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ToolButton6: TToolButton
        Left = 108
        Top = 0
        Action = SelectAll1
      end
      object ToolButton7: TToolButton
        Left = 131
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 139
        Top = 0
        Action = FileProp1
      end
      object ToolButton9: TToolButton
        Left = 162
        Top = 0
        Action = DumpPe1
      end
    end
  end
  object StatusBar: TStatusBar [1]
    Left = 0
    Top = 290
    Width = 461
    Height = 19
    Panels = <
      item
        Width = 90
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ModulesListView: TListView [2]
    Left = 0
    Top = 26
    Width = 461
    Height = 264
    Align = alClient
    Columns = <
      item
        Caption = 'Module'
        Width = 80
      end
      item
        Alignment = taRightJustify
        Caption = 'Usage'
      end
      item
        Alignment = taRightJustify
        Caption = 'Relocated'
        Width = 70
      end
      item
        Caption = 'Filename'
        Width = 300
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    SmallImages = GlobalModule.ToolbarImagesList
    TabOrder = 2
    ViewStyle = vsReport
    OnColumnClick = ModulesListViewColumnClick
    OnCompare = ModulesListViewCompare
    OnInfoTip = ModulesListViewInfoTip
  end
  inherited ActionList: TActionList
    inherited Refresh1: TAction
      OnExecute = Refresh1Execute
    end
    object FileProp1: TAction
      Caption = 'Properties'
      Hint = 'File properties'
      ImageIndex = 4
      ShortCut = 32781
      OnExecute = FileProp1Execute
      OnUpdate = FileProp1Update
    end
    object DumpPe1: TAction
      Caption = 'Dump PE'
      Hint = 'Dump PE'
      ImageIndex = 22
      ShortCut = 16452
      OnExecute = DumpPe1Execute
      OnUpdate = DumpPe1Update
    end
  end
  inherited PopupMenu: TPopupMenu
    object Refresh2: TMenuItem
      Action = Refresh1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Copy2: TMenuItem
      Action = Copy1
    end
    object Selectall2: TMenuItem
      Action = SaveToFile1
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Selectall3: TMenuItem
      Action = SelectAll1
    end
    object DumpPE2: TMenuItem
      Action = DumpPe1
    end
    object Properties1: TMenuItem
      Action = FileProp1
    end
  end
end
