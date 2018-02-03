object PeDumpChild: TPeDumpChild
  Left = 195
  Top = 152
  AutoScroll = False
  Caption = 'PeDumpChild'
  ClientHeight = 347
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 250
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
    Left = 121
    Top = 0
    Width = 3
    Height = 347
    Cursor = crHSplit
    ResizeStyle = rsUpdate
  end
  object SectionTreeView: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 347
    Align = alLeft
    HideSelection = False
    HotTrack = True
    Images = MainForm.IconImageList
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = SectionTreeViewChange
    OnDblClick = SectionTreeViewDblClick
    OnExpanding = SectionTreeViewExpanding
  end
  object PageControl1: TPageControl
    Left = 124
    Top = 0
    Width = 468
    Height = 347
    ActivePage = ItemsTab
    Align = alClient
    TabOrder = 1
    TabStop = False
    object ItemsTab: TTabSheet
      Caption = 'ItemsTab'
      object ItemsListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Item'
            Width = 200
          end
          item
            Caption = 'Value'
            Width = 100
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = ItemsListViewData
      end
    end
    object DirectoryTab: TTabSheet
      Caption = 'DirectoryTab'
      ImageIndex = 1
      object DirectoryListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Directory'
            Width = 120
          end
          item
            Caption = 'RVA'
            Width = 80
          end
          item
            Alignment = taRightJustify
            Caption = 'Size'
            Width = 80
          end
          item
            Alignment = taRightJustify
            Caption = 'Percent of file'
            Width = 80
          end
          item
            Caption = 'Section'
            Width = 70
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = DirectoryListViewCustomDrawItem
        OnData = DirectoryListViewData
      end
    end
    object ImportTab: TTabSheet
      Caption = 'ImportTab'
      ImageIndex = 2
      object ImportListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 300
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 230
          end
          item
            Caption = 'Ordinal'
            Width = 60
          end
          item
            Caption = 'Hint'
          end
          item
            Caption = 'Module'
            Width = 90
          end>
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ImportListViewColumnClick
        OnData = ImportListViewData
        OnDblClick = ImportListViewDblClick
      end
      object ImportStatusBar: TStatusBar
        Left = 0
        Top = 300
        Width = 460
        Height = 19
        Panels = <
          item
            Width = 90
          end
          item
            Width = 90
          end
          item
            Width = 50
          end>
        SimplePanel = False
      end
    end
    object ExportTab: TTabSheet
      Caption = 'ExportTab'
      ImageIndex = 3
      object ExportListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 300
        Align = alClient
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
          end
          item
            Caption = 'Forwarded'
            Width = 100
          end
          item
            Caption = 'Section'
          end>
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = ExportListViewColumnClick
        OnData = ExportListViewData
        OnDblClick = ImportListViewDblClick
      end
      object ExportStatusBar: TStatusBar
        Left = 0
        Top = 300
        Width = 460
        Height = 19
        Panels = <
          item
            Width = 90
          end
          item
            Width = 90
          end
          item
            Width = 100
          end
          item
            Width = 50
          end>
        SimplePanel = False
      end
    end
    object ResourceTab: TTabSheet
      Caption = 'ResourceTab'
      ImageIndex = 4
      object ResourceListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Names'
            Width = 200
          end
          item
            Caption = 'Offset'
            Width = 100
          end
          item
            Caption = 'Size'
          end
          item
            Caption = 'Languages'
            Width = 70
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = ResourceListViewData
      end
    end
    object SectionTab: TTabSheet
      Caption = 'SectionTab'
      ImageIndex = 5
      object SectionListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Section'
            Width = 70
          end
          item
            Caption = 'VirtSize'
            Width = 70
          end
          item
            Caption = 'RVA'
            Width = 70
          end
          item
            Caption = 'PhysSize'
            Width = 70
          end
          item
            Caption = 'PhysOfs'
            Width = 70
          end
          item
            Caption = 'Flags'
            Width = 70
          end
          item
            Caption = 'Info'
          end
          item
            Alignment = taRightJustify
            Caption = 'Percent of file'
            Width = 79
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = SectionListViewData
      end
    end
    object ResourceDirTab: TTabSheet
      Caption = 'ResourceDirTab'
      ImageIndex = 6
      object ResourceDirListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Type'
            Width = 200
          end
          item
            Caption = 'Count'
            Width = 100
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = ResourceDirListViewData
      end
    end
    object RelocTab: TTabSheet
      Caption = 'RelocTab'
      ImageIndex = 7
      object RelocListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 300
        Align = alClient
        Columns = <
          item
            Caption = 'Address'
            Width = 200
          end
          item
            Caption = 'Type'
            Width = 100
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = RelocListViewData
      end
      object RelocStatusBar: TStatusBar
        Left = 0
        Top = 300
        Width = 460
        Height = 19
        Panels = <
          item
            Width = 100
          end
          item
            Width = 50
          end>
        SimplePanel = False
      end
    end
    object DebugTab: TTabSheet
      Caption = 'DebugTab'
      ImageIndex = 8
      object DebugListView: TListView
        Left = 0
        Top = 0
        Width = 460
        Height = 319
        Align = alClient
        Columns = <
          item
            Caption = 'Type'
            Width = 100
          end
          item
            Caption = 'Size'
            Width = 70
          end
          item
            Caption = 'RVA'
            Width = 70
          end
          item
            Caption = 'FilePtr'
            Width = 70
          end
          item
            Caption = 'Version'
          end>
        ColumnClick = False
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = DebugListViewData
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ToolbarImagesList
    Left = 16
    Top = 312
    object Copytoclipboard1: TMenuItem
      Action = MainForm.Copy1
    end
    object Selectall1: TMenuItem
      Action = MainForm.SelectAll1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Openlibrary1: TMenuItem
      Action = MainForm.OpenLibrary1
    end
    object FindinWin32APIhelp1: TMenuItem
      Action = MainForm.InvokeHelp1
      Default = True
    end
  end
end
