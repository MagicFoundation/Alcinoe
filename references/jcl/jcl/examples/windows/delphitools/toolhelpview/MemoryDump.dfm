inherited MemoryDumpForm: TMemoryDumpForm
  Left = 206
  Top = 116
  Width = 654
  Height = 423
  Caption = 'MemoryDumpForm'
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter [0]
    Left = 0
    Top = 191
    Width = 646
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object Splitter2: TSplitter [1]
    Left = 105
    Top = 26
    Width = 3
    Height = 165
    Cursor = crHSplit
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  inherited CoolBar: TCoolBar
    Width = 646
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 642
      end>
    inherited ToolBar: TToolBar
      Width = 629
      object ToolButton5: TToolButton
        Left = 0
        Top = 0
        Action = Refresh1
      end
      object ToolButton6: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object ToolButton1: TToolButton
        Left = 31
        Top = 0
        Action = Copy1
      end
      object ToolButton2: TToolButton
        Left = 54
        Top = 0
        Action = SaveToFile1
      end
      object ToolButton10: TToolButton
        Left = 77
        Top = 0
        Action = Find1
      end
      object ToolButton9: TToolButton
        Left = 100
        Top = 0
        Action = SaveData1
      end
      object ToolButton3: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 131
        Top = 0
        Action = SelectAll1
      end
      object ToolButton7: TToolButton
        Left = 154
        Top = 0
        Width = 8
        Caption = 'ToolButton7'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 162
        Top = 0
        Action = ViewAsText1
        Style = tbsCheck
      end
    end
  end
  object StatusBar: TStatusBar [3]
    Left = 0
    Top = 377
    Width = 646
    Height = 19
    Panels = <
      item
        Width = 65
      end
      item
        Width = 130
      end
      item
        Width = 130
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object PagesListView: TListView [4]
    Left = 108
    Top = 26
    Width = 538
    Height = 165
    Align = alClient
    AllocBy = 64
    Columns = <
      item
        Caption = 'Base'
        Width = 80
      end
      item
        Caption = 'Protect'
        Width = 75
      end
      item
        Caption = 'Allocation'
        Width = 65
      end
      item
        Caption = 'Alloc.protect'
        Width = 75
      end
      item
        Alignment = taRightJustify
        Caption = 'Region size'
        Width = 90
      end
      item
        Caption = 'State'
        Width = 60
      end
      item
        Caption = 'ModuleName'
        Width = 100
      end
      item
        Caption = 'Type'
      end>
    ColumnClick = False
    HideSelection = False
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu
    SmallImages = GlobalModule.ToolbarImagesList
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawItem = PagesListViewCustomDrawItem
    OnData = PagesListViewData
    OnSelectItem = PagesListViewSelectItem
  end
  object DumpListView: TListView [5]
    Left = 0
    Top = 194
    Width = 646
    Height = 183
    Align = alBottom
    Columns = <
      item
        Caption = 'Address'
        Width = 80
      end
      item
        Caption = 'Data'
        Width = 350
      end
      item
        Caption = 'ASCII'
        Width = 130
      end>
    ColumnClick = False
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    PopupMenu = PopupMenu
    TabOrder = 2
    ViewStyle = vsReport
    OnData = DumpListViewData
  end
  object MemoryTreeView: TTreeView [6]
    Left = 0
    Top = 26
    Width = 105
    Height = 165
    Align = alLeft
    Images = GlobalModule.ToolbarImagesList
    Indent = 19
    ReadOnly = True
    TabOrder = 4
    OnChange = MemoryTreeViewChange
    OnGetSelectedIndex = MemoryTreeViewGetSelectedIndex
  end
  inherited ActionList: TActionList
    inherited Refresh1: TAction
      OnExecute = Refresh1Execute
    end
    object ViewAsText1: TAction
      Caption = 'View as text'
      Hint = 'View as text'
      ImageIndex = 23
      ShortCut = 16468
      OnExecute = ViewAsText1Execute
    end
    object SaveData1: TAction
      Caption = 'Save data'
      Hint = 'Save region data'
      ImageIndex = 25
      ShortCut = 16452
      OnExecute = SaveData1Execute
      OnUpdate = SaveData1Update
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
    object Save1: TMenuItem
      Action = SaveToFile1
    end
    object Savedata2: TMenuItem
      Action = SaveData1
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Selectall2: TMenuItem
      Action = SelectAll1
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Viewastext2: TMenuItem
      Action = ViewAsText1
    end
  end
  object SaveDataDialog: TSaveDialog
    DefaultExt = 'bin'
    Filter = 'Binary files (*.bin)|*.bin|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 104
    Top = 224
  end
end
