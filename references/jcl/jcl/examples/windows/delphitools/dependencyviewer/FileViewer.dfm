object FileViewerChild: TFileViewerChild
  Left = 205
  Top = 131
  ActiveControl = DependencyTreeView
  AutoScroll = False
  Caption = 'FileViewerChild'
  ClientHeight = 354
  ClientWidth = 576
  Color = clBtnFace
  Constraints.MinHeight = 100
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  Position = poDefault
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 134
    Top = 0
    Width = 3
    Height = 277
    Cursor = crHSplit
    ResizeStyle = rsUpdate
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 277
    Width = 576
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
  end
  object DependencyTreeView: TTreeView
    Left = 0
    Top = 0
    Width = 134
    Height = 277
    Align = alLeft
    ChangeDelay = 50
    HideSelection = False
    HotTrack = True
    Images = MainForm.ViewImageList
    Indent = 19
    ReadOnly = True
    ShowRoot = False
    StateImages = MainForm.ViewImageList
    TabOrder = 0
    OnChange = DependencyTreeViewChange
    OnDeletion = DependencyTreeViewDeletion
  end
  object ListViewsPanel: TPanel
    Left = 137
    Top = 0
    Width = 439
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 150
      Width = 439
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ResizeStyle = rsUpdate
    end
    object ImportListView: TListView
      Left = 0
      Top = 153
      Width = 439
      Height = 124
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 250
        end
        item
          Alignment = taRightJustify
          Caption = 'Ordinal'
          Width = 60
        end
        item
          Alignment = taRightJustify
          Caption = 'Hint'
        end
        item
          Caption = 'Module'
        end>
      HideSelection = False
      MultiSelect = True
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      SmallImages = MainForm.ViewImageList
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = ImportListViewColumnClick
      OnData = ImportListViewData
      OnDblClick = ExportListViewDblClick
    end
    object ExportListView: TListView
      Left = 0
      Top = 0
      Width = 439
      Height = 150
      Align = alTop
      Columns = <
        item
          Caption = 'Name'
          Width = 250
        end
        item
          Alignment = taRightJustify
          Caption = 'Ordinal'
        end
        item
          Alignment = taRightJustify
          Caption = 'Hint'
        end
        item
          Caption = 'Address'
          Width = 70
        end>
      HideSelection = False
      MultiSelect = True
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      SmallImages = MainForm.ViewImageList
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = ExportListViewColumnClick
      OnData = ExportListViewData
      OnDblClick = ExportListViewDblClick
    end
  end
  object ModulesListView: TListView
    Left = 0
    Top = 280
    Width = 576
    Height = 74
    Align = alBottom
    Columns = <
      item
        Caption = 'Module'
        Width = 100
      end
      item
        Caption = 'Date and time'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'Size'
        Width = 70
      end
      item
        Caption = 'Subsystem'
        Width = 65
      end
      item
        Caption = 'Base address'
        Width = 80
      end
      item
        Caption = 'File version'
        Width = 80
      end
      item
        Caption = 'Product version'
        Width = 90
      end
      item
        Caption = 'Img ver.'
      end
      item
        Caption = 'Linker'
      end
      item
        Caption = 'OS'
      end
      item
        Caption = 'Subsys ver.'
      end
      item
        Caption = 'Description'
        Width = 250
      end>
    ColumnClick = False
    GridLines = True
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    SmallImages = MainForm.ViewImageList
    TabOrder = 2
    ViewStyle = vsReport
    OnDblClick = ModulesListViewDblClick
    OnInfoTip = ModulesListViewInfoTip
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ToolbarImagesList
    Left = 8
    Top = 312
    object Copy1: TMenuItem
      Action = MainForm.Copy1
    end
    object Save1: TMenuItem
      Action = MainForm.Save1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Selectall1: TMenuItem
      Action = MainForm.SelectAll1
    end
    object DumpPEfile1: TMenuItem
      Action = MainForm.DumpPe1
    end
    object Win32helpkeyword1: TMenuItem
      Action = MainForm.Win32Help1
    end
  end
end
