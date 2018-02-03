object PeResViewChild: TPeResViewChild
  Left = 380
  Top = 203
  AutoScroll = False
  Caption = 'PeResViewChild'
  ClientHeight = 407
  ClientWidth = 597
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
    Height = 407
    Cursor = crHSplit
    ResizeStyle = rsUpdate
  end
  object ResourceTreeView: TTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 407
    Align = alLeft
    HideSelection = False
    Images = MainForm.IconImageList
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = ResourceTreeViewChange
    OnExpanding = ResourceTreeViewExpanding
  end
  object PageControl1: TPageControl
    Left = 124
    Top = 0
    Width = 473
    Height = 407
    ActivePage = DirTab
    Align = alClient
    TabOrder = 1
    TabStop = False
    object DirTab: TTabSheet
      Caption = 'DirTab'
      object DirListView: TListView
        Left = 0
        Top = 0
        Width = 465
        Height = 379
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Offset'
            Width = 70
          end
          item
            Caption = 'Size'
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
        OnData = DirListViewData
      end
    end
    object HexDumpTab: TTabSheet
      Caption = 'HexDumpTab'
      ImageIndex = 1
      object HexDumpListView: TListView
        Left = 0
        Top = 0
        Width = 465
        Height = 379
        Align = alClient
        Columns = <
          item
            Caption = 'Offset'
            Width = 70
          end
          item
            Caption = 'Data'
            Width = 250
          end
          item
            Caption = 'ASCII'
            Width = 70
          end>
        ColumnClick = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        GridLines = True
        HideSelection = False
        HotTrackStyles = []
        MultiSelect = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnData = HexDumpListViewData
      end
    end
    object StringsTab: TTabSheet
      Caption = 'StringsTab'
      ImageIndex = 2
      object Splitter2: TSplitter
        Left = 0
        Top = 341
        Width = 465
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ResizeStyle = rsUpdate
      end
      object StringsListView: TListView
        Left = 0
        Top = 0
        Width = 465
        Height = 341
        Align = alClient
        Columns = <
          item
            Caption = 'ID'
            Width = 70
          end
          item
            Caption = 'Text'
            Width = 300
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
        OnData = StringsListViewData
        OnSelectItem = StringsListViewSelectItem
      end
      object DetailedStringMemo: TMemo
        Left = 0
        Top = 344
        Width = 465
        Height = 35
        Align = alBottom
        Color = clBtnFace
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object GraphDirTab: TTabSheet
      Caption = 'GraphDirTab'
      ImageIndex = 3
      object GraphDrawGrid: TDrawGrid
        Left = 0
        Top = 0
        Width = 465
        Height = 379
        Align = alClient
        ColCount = 2
        DefaultDrawing = False
        FixedCols = 0
        Options = [goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        TabOrder = 0
        OnDrawCell = GraphDrawGridDrawCell
        ColWidths = (
          147
          277)
      end
    end
    object TextTab: TTabSheet
      Caption = 'TextTab'
      ImageIndex = 4
      object TextRichEdit: TRichEdit
        Left = 0
        Top = 0
        Width = 465
        Height = 379
        Align = alClient
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        HideScrollBars = False
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object AviTab: TTabSheet
      Caption = 'AviTab'
      ImageIndex = 5
      PopupMenu = AviPopupMenu
      object Bevel2: TBevel
        Left = 0
        Top = 26
        Width = 465
        Height = 334
        Align = alClient
      end
      object Animate1: TAnimate
        Left = 8
        Top = 40
        Width = 100
        Height = 80
        Active = False
        Color = clBtnFace
        ParentColor = False
        OnOpen = Animate1Open
        OnClose = Animate1Close
        OnStop = Animate1Stop
      end
      object AviToolBar: TToolBar
        Left = 0
        Top = 0
        Width = 465
        Height = 26
        AutoSize = True
        ButtonWidth = 51
        Caption = 'AviToolBar'
        EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
        Flat = True
        Images = MainForm.ToolbarImagesList
        List = True
        ShowCaptions = True
        TabOrder = 1
        object ToolButton1: TToolButton
          Left = 0
          Top = 0
          Action = AviPlay1
        end
        object ToolButton2: TToolButton
          Left = 51
          Top = 0
          Action = AviStop1
        end
        object ToolButton3: TToolButton
          Left = 102
          Top = 0
          Action = AviBkColor1
        end
      end
      object AviStatusBar: TStatusBar
        Left = 0
        Top = 360
        Width = 465
        Height = 19
        Panels = <
          item
            Width = 150
          end>
        SimplePanel = False
      end
    end
    object HTMLTab: TTabSheet
      Caption = 'HTMLTab'
      ImageIndex = 6
    end
    object GraphTab: TTabSheet
      Caption = 'GraphTab'
      ImageIndex = 7
      object GraphImage: TImage
        Left = 0
        Top = 0
        Width = 465
        Height = 360
        Align = alClient
        AutoSize = True
        Center = True
      end
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 465
        Height = 360
        Align = alClient
      end
      object GraphStatusBar: TStatusBar
        Left = 0
        Top = 360
        Width = 465
        Height = 19
        Panels = <
          item
            Width = 50
          end>
        SimplePanel = False
      end
    end
    object DialogTab: TTabSheet
      Caption = 'DialogTab'
      ImageIndex = 8
      object Bevel3: TBevel
        Left = 0
        Top = 0
        Width = 465
        Height = 379
        Align = alClient
      end
      object DialogTestBtn: TButton
        Left = 8
        Top = 8
        Width = 75
        Height = 25
        Caption = '&Show dialog'
        TabOrder = 0
        OnClick = DialogTestBtnClick
      end
    end
  end
  object ActionList1: TActionList
    Images = MainForm.ToolbarImagesList
    Left = 136
    Top = 352
    object AviPlay1: TAction
      Caption = 'Play'
      ImageIndex = 16
      OnExecute = AviPlay1Execute
    end
    object AviStop1: TAction
      Caption = 'Stop'
      Enabled = False
      ImageIndex = 17
      OnExecute = AviStop1Execute
    end
    object AviBkColor1: TAction
      Caption = 'Color'
      ImageIndex = 18
      OnExecute = AviBkColor1Execute
    end
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Options = [cdPreventFullOpen, cdSolidColor]
    Left = 168
    Top = 352
  end
  object AviPopupMenu: TPopupMenu
    Images = MainForm.ToolbarImagesList
    Left = 200
    Top = 352
    object Play1: TMenuItem
      Action = AviPlay1
    end
    object Stop1: TMenuItem
      Action = AviStop1
    end
    object Color1: TMenuItem
      Action = AviBkColor1
    end
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 232
    Top = 352
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ToolbarImagesList
    Left = 264
    Top = 352
    object Copytoclipboard1: TMenuItem
      Action = MainForm.Copy1
    end
    object Savetofile1: TMenuItem
      Action = MainForm.Save1
    end
    object Selectall1: TMenuItem
      Action = MainForm.SelectAll1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Viewdetails1: TMenuItem
      Action = MainForm.ViewResDetails1
    end
    object Viewashex1: TMenuItem
      Action = MainForm.ViewResHex1
    end
  end
end
