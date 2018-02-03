inherited HeapDumpForm: THeapDumpForm
  Left = 239
  Top = 152
  Width = 482
  Height = 380
  Caption = 'HeapDumpForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter [0]
    Left = 0
    Top = 281
    Width = 474
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object StatusBar: TStatusBar [1]
    Left = 0
    Top = 334
    Width = 474
    Height = 19
    Panels = <
      item
        Width = 90
      end
      item
        Width = 90
      end
      item
        Width = 90
      end
      item
        Width = 90
      end>
    SimplePanel = False
    OnResize = StatusBarResize
  end
  object Panel1: TPanel [2]
    Left = 0
    Top = 26
    Width = 474
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 137
      Top = 0
      Width = 3
      Height = 255
      Cursor = crHSplit
      AutoSnap = False
      ResizeStyle = rsUpdate
    end
    object HeapListView: TListView
      Tag = 1
      Left = 0
      Top = 0
      Width = 137
      Height = 255
      Align = alLeft
      AllocBy = 16
      Columns = <
        item
          Caption = 'HID'
          Width = 70
        end
        item
          Caption = 'Flags'
          Width = 60
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = HeapListViewColumnClick
      OnCompare = HeapListViewCompare
      OnSelectItem = HeapListViewSelectItem
    end
    object HeapEntryListView: TListView
      Left = 140
      Top = 0
      Width = 334
      Height = 255
      Align = alClient
      AllocBy = 128
      Columns = <
        item
          Caption = 'Handle'
          Width = 70
        end
        item
          Caption = 'Start Adress'
          Width = 70
        end
        item
          Alignment = taRightJustify
          Caption = 'BlockSize'
          Width = 70
        end
        item
          Caption = 'End Adress'
          Width = 70
        end
        item
          Caption = 'Flags'
          Width = 65
        end
        item
          Alignment = taRightJustify
          Caption = 'Lock Count'
          Width = 70
        end>
      ColumnClick = False
      HideSelection = False
      MultiSelect = True
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu
      TabOrder = 1
      ViewStyle = vsReport
      OnData = HeapEntryListViewData
      OnSelectItem = HeapEntryListViewSelectItem
    end
  end
  object HeapEntryMemo: TMemo [3]
    Left = 0
    Top = 284
    Width = 474
    Height = 50
    Align = alBottom
    PopupMenu = PopupMenu
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  inherited CoolBar: TCoolBar
    Width = 474
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 470
      end>
    inherited ToolBar: TToolBar
      Width = 457
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
      object ToolButton7: TToolButton
        Left = 31
        Top = 0
        Action = Copy1
      end
      object ToolButton8: TToolButton
        Left = 54
        Top = 0
        Action = SaveToFile1
      end
      object ToolButton3: TToolButton
        Left = 77
        Top = 0
        Action = Find1
      end
      object ToolButton1: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton1'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ToolButton2: TToolButton
        Left = 108
        Top = 0
        Action = SelectAll1
      end
    end
  end
  inherited ActionList: TActionList
    Top = 296
    inherited Refresh1: TAction
      OnExecute = Refresh1Execute
    end
  end
  inherited PopupMenu: TPopupMenu
    Top = 296
    object Refresh2: TMenuItem
      Caption = 'Refresh'
      Hint = 'Refresh HeapList'
      ImageIndex = 2
      ShortCut = 116
      OnClick = Refresh1Execute
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Copy2: TMenuItem
      Caption = 'Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 9
      ShortCut = 16451
    end
    object Save1: TMenuItem
      Caption = 'Save'
      Hint = 'Save to text file'
      ImageIndex = 3
      ShortCut = 16467
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Selectall2: TMenuItem
      Action = SelectAll1
    end
  end
end
