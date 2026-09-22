object MainForm: TMainForm
  Left = 377
  Top = 296
  Caption = 'Alcinoe CodeProfiler'
  ClientHeight = 985
  ClientWidth = 1264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 23
  object MainPageControl: TcxPageControl
    Left = 0
    Top = 0
    Width = 1264
    Height = 953
    Align = alClient
    TabOrder = 0
    Properties.ActivePage = InstrumentationTabSheet
    Properties.CustomButtons.Buttons = <>
    ClientRectBottom = 948
    ClientRectLeft = 5
    ClientRectRight = 1259
    ClientRectTop = 37
    object InstrumentationTabSheet: TcxTabSheet
      Caption = 'Source Code Instrumentation'
      ImageIndex = 0
      OnResize = InstrumentationTabSheetResize
      object InstructionPanel: TdxPanel
        Left = 0
        Top = 0
        Width = 1254
        Height = 353
        Align = alTop
        Color = 16448250
        TabOrder = 0
        object cxLabel1: TcxLabel
          AlignWithMargins = True
          Left = 3
          Top = 8
          Margins.Top = 8
          Align = alTop
          Caption = 
            'Alcinoe Code Profiler adds markers to functions to measure execu' +
            'tion time across Windows, macOS, iOS, and Android.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 0
          Transparent = True
          Width = 1246
        end
        object cxLabel2: TcxLabel
          AlignWithMargins = True
          Left = 3
          Top = 41
          Align = alTop
          Caption = 'Steps:'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 1
          Transparent = True
          Width = 1246
        end
        object cxLabel3: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 107
          Margins.Left = 16
          Align = alTop
          Caption = 
            '2. Click the "Insert Markers" button below to add profiler marke' +
            'rs to your code.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 2
          Transparent = True
          Width = 1233
        end
        object cxLabel4: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 140
          Margins.Left = 16
          Align = alTop
          Caption = '3. Recompile and run the application.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 3
          Transparent = True
          Width = 1233
        end
        object cxLabel5: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 173
          Margins.Left = 16
          Align = alTop
          Caption = 
            '4. If you are using Android or iOS, send the app to the backgrou' +
            'nd and bring it back to the foreground to generate the performan' +
            'ce file. On Windows and macOS, the performance file will be gene' +
            'rated when you close the app.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 4
          Transparent = True
          Width = 1233
        end
        object LastInstructionLabel: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 308
          Margins.Left = 16
          Margins.Bottom = 16
          Align = alTop
          Caption = 
            '6. Go to the Performance Analysis tab, click the Load Data butto' +
            'n, and run the analysis.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 5
          Transparent = True
          ExplicitTop = 285
          Width = 1233
        end
        object cxLabel13: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 74
          Margins.Left = 16
          Align = alTop
          Caption = 
            '1. If you run the program on a remote device (such as Android or' +
            ' iOS), specify the server IP and port for the listening process.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 6
          Transparent = True
          Width = 1233
        end
        object cxLabel14: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 229
          Margins.Left = 16
          Align = alTop
          Caption = 
            '5. If you specified the server IP and port in step 1, the perfor' +
            'mance file will be received automatically. Otherwise, if you are' +
            ' running the program on a remote device, download the data from ' +
            'the user'#39's documents folder and place it in the CodeProfiler dat' +
            'a folder. Note: On Windows, the performance file is directly sto' +
            'red in the CodeProfiler data folder if the app is running locall' +
            'y; otherwise, it is saved in the user'#39's document folder.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 7
          Transparent = True
          Width = 1233
        end
      end
      object dxPanel2: TdxPanel
        AlignWithMargins = True
        Left = 0
        Top = 361
        Width = 1254
        Height = 542
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alClient
        TabOrder = 1
        ExplicitTop = 432
        ExplicitHeight = 471
        object SourcesPathMemo: TcxMemo
          AlignWithMargins = True
          Left = 8
          Top = 362
          Margins.Left = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alClient
          TabOrder = 0
          ExplicitTop = 299
          ExplicitHeight = 118
          Height = 126
          Width = 1236
        end
        object cxLabel9: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 332
          Margins.Left = 8
          Margins.Top = 0
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'Enter source code paths for profiler markers, listing one folder' +
            ' or filename per line. Prefix a name with '#39'!'#39' to ignore the file'
          Properties.WordWrap = True
          TabOrder = 1
          ExplicitTop = 269
          Width = 1236
        end
        object dxPanel3: TdxPanel
          Left = 0
          Top = 148
          Width = 1252
          Height = 31
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 2
          object HttpServerPortEdit: TcxMaskEdit
            AlignWithMargins = True
            Left = 504
            Top = 0
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alLeft
            Properties.MaskKind = emkRegExpr
            Properties.EditMask = '[0-9]+'
            Properties.OnChange = HttpServerPortEditPropertiesChange
            TabOrder = 0
            Width = 106
          end
          object cxLabel11: TcxLabel
            Left = 461
            Top = 0
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alLeft
            Caption = 'Port'
            Properties.WordWrap = True
            TabOrder = 1
            Width = 35
          end
          object HttpServerNameEdit: TcxMaskEdit
            AlignWithMargins = True
            Left = 95
            Top = 0
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alLeft
            Properties.OnChange = HttpServerNameEditPropertiesChange
            TabOrder = 2
            Width = 358
          end
          object cxLabel12: TcxLabel
            AlignWithMargins = True
            Left = 8
            Top = 0
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alLeft
            Caption = 'Server IP'
            Properties.WordWrap = True
            TabOrder = 3
            Width = 71
          end
        end
        object cxLabel15: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 8
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            'Path to Alcinoe.CodeProfiler.inc, the include file where the opt' +
            'ions below are stored'
          Properties.WordWrap = True
          TabOrder = 6
          Width = 1236
        end
        object dxPanel5: TdxPanel
          Left = 0
          Top = 43
          Width = 1252
          Height = 31
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 7
          object BrowseCodeProfilerIncFilenameBtn: TcxButton
            AlignWithMargins = True
            Left = 1204
            Top = 0
            Width = 40
            Height = 31
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alRight
            Caption = '...'
            TabOrder = 0
            OnClick = BrowseCodeProfilerIncFilenameBtnClick
          end
          object CodeProfilerIncFilenameEdit: TcxTextEdit
            AlignWithMargins = True
            Left = 8
            Top = 0
            Margins.Left = 8
            Margins.Top = 0
            Margins.Right = 8
            Margins.Bottom = 0
            Align = alClient
            Properties.OnChange = CodeProfilerIncFilenameEditPropertiesChange
            TabOrder = 1
            Width = 1180
          end
        end
        object dxPanel6: TdxPanel
          Left = 0
          Top = 74
          Width = 1252
          Height = 31
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 8
          ExplicitLeft = 16
          ExplicitTop = 59
          object CodeProfilerEnabledCheckBox: TcxCheckBox
            AlignWithMargins = True
            Left = 8
            Top = 3
            Margins.Left = 8
            Align = alLeft
            Caption = 'Start profiling as soon as the application starts'
            Properties.OnChange = CodeProfilerEnabledCheckBoxPropertiesChange
            TabOrder = 0
            ExplicitLeft = -1
            ExplicitTop = 19
          end
        end
        object cxLabel10: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 113
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            '(Optional) Specify the IP address and port of this computer to a' +
            'utomatically receive the performance file. Not required for loca' +
            'l execution.'
          Properties.WordWrap = True
          TabOrder = 3
          Width = 1236
        end
        object dxPanel1: TdxPanel
          Left = 0
          Top = 488
          Width = 1252
          Height = 52
          Align = alBottom
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 4
          ExplicitTop = 417
          object InsertProfilerMarkersBtn: TcxButton
            Left = 8
            Top = 12
            Width = 150
            Height = 31
            Caption = 'Insert Markers'
            TabOrder = 0
            OnClick = InsertProfilerMarkersBtnClick
          end
          object RemoveProfilerMarkersBtn: TcxButton
            Left = 170
            Top = 12
            Width = 156
            Height = 31
            Caption = 'Remove Markers'
            TabOrder = 1
            OnClick = RemoveProfilerMarkersBtnClick
          end
        end
        object dxPanel4: TdxPanel
          AlignWithMargins = True
          Left = 3
          Top = 217
          Width = 1246
          Height = 39
          Margins.Bottom = 0
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 5
          ExplicitTop = 182
          object DoNotGroupRadioButton: TcxRadioButton
            AlignWithMargins = True
            Left = 8
            Top = 3
            Margins.Left = 8
            Align = alLeft
            Caption = 'Do not group (Huge memory usage!)'
            TabOrder = 0
            OnClick = HistoryGroupModeRadioButtonClick
            AutoSize = True
          end
          object GroupCallsByProcIDRadioButton: TcxRadioButton
            AlignWithMargins = True
            Left = 347
            Top = 3
            Margins.Left = 32
            Align = alLeft
            Caption = 'Group by procedure ID'
            TabOrder = 1
            OnClick = HistoryGroupModeRadioButtonClick
            AutoSize = True
          end
          object GroupCallsByCallStackRadioButton: TcxRadioButton
            AlignWithMargins = True
            Left = 579
            Top = 3
            Margins.Left = 32
            Align = alLeft
            Caption = 'Group by call stack (recommended)'
            Checked = True
            TabOrder = 2
            TabStop = True
            OnClick = HistoryGroupModeRadioButtonClick
            AutoSize = True
          end
        end
        object dxPanel7: TdxPanel
          AlignWithMargins = True
          Left = 3
          Top = 256
          Width = 1246
          Height = 39
          Margins.Top = 0
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 9
          ExplicitTop = 227
          object IgnoreThreadIDCheckBox: TcxCheckBox
            AlignWithMargins = True
            Left = 8
            Top = 3
            Margins.Left = 8
            Align = alLeft
            Caption = 
              'Ignore thread ID (This option is only available with Group by pr' +
              'ocedure ID)'
            Properties.OnChange = IgnoreThreadIDCheckBoxPropertiesChange
            Style.TransparentBorder = False
            TabOrder = 0
          end
        end
        object cxLabel7: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 302
          Margins.Left = 8
          Margins.Top = 4
          Margins.Right = 8
          Align = alTop
          Caption = 'Source Code Paths'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 10
          ExplicitTop = 349
          Width = 1236
        end
        object cxLabel16: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 187
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Call Grouping (Help is available in Alcinoe.CodeProfiler.inc)'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 11
          ExplicitTop = 195
          Width = 1236
        end
      end
    end
    object PerformanceAnalysisTabSheet: TcxTabSheet
      Caption = 'Performance Analysis'
      ImageIndex = 1
      object Panelfilter: TPanel
        Left = 0
        Top = 0
        Width = 1254
        Height = 89
        Margins.Left = 8
        Align = alTop
        BevelOuter = bvNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnResize = PanelfilterResize
        object Label1: TLabel
          Left = 289
          Top = 11
          Width = 168
          Height = 23
          Caption = 'Start Timestamp (Min)'
        end
        object Label2: TLabel
          Left = 657
          Top = 11
          Width = 171
          Height = 23
          Caption = 'Start Timestamp (Max)'
        end
        object ProcNameFilterEdit: TcxTextEdit
          Left = 288
          Top = 45
          TabOrder = 0
          TextHint = 
            'Search for procedure names. Accepts multiple entries separated b' +
            'y '#39';'#39
          Width = 724
        end
        object ApplyFilterBtn: TcxButton
          Left = 143
          Top = 45
          Width = 124
          Height = 31
          Caption = 'Filter'
          TabOrder = 1
          OnClick = ApplyFilterBtnClick
        end
        object LoadDataBtn: TcxButton
          Left = 8
          Top = 8
          Width = 124
          Height = 31
          Margins.Right = 8
          Caption = 'Load Data'
          TabOrder = 2
          OnClick = LoadDataBtnClick
        end
        object StartTimeStampMinEdit: TcxMaskEdit
          Left = 463
          Top = 8
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '([0-5][0-9]):([0-5][0-9]):([0-9]{3})\.([0-9]{1,4})'
          TabOrder = 3
          TextHint = 'mm:ss:zzz.zzzz'
          Width = 177
        end
        object StartTimeStampMaxEdit: TcxMaskEdit
          Left = 835
          Top = 8
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '([0-5][0-9]):([0-5][0-9]):([0-9]{3})\.([0-9]{1,4})'
          TabOrder = 4
          TextHint = 'mm:ss:zzz.zzzz'
          Width = 177
        end
        object ClearDataBtn: TcxButton
          Left = 8
          Top = 45
          Width = 124
          Height = 31
          Margins.Right = 8
          Caption = 'Clear Data'
          TabOrder = 5
          OnClick = ClearDataBtnClick
        end
        object ExportToCsvBtn: TcxButton
          Left = 143
          Top = 8
          Width = 124
          Height = 31
          Margins.Right = 8
          Caption = 'Export to CSV'
          TabOrder = 6
          OnClick = ExportToCsvBtnClick
        end
      end
      object TreeListProcMetrics: TcxTreeList
        Left = 0
        Top = 89
        Width = 1254
        Height = 145
        Align = alTop
        Bands = <
          item
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI Light'
        Font.Style = []
        OptionsBehavior.CopyCaptionsToClipboard = False
        OptionsData.Editing = False
        OptionsView.ColumnAutoWidth = True
        ParentFont = False
        ScrollbarAnnotations.CustomAnnotations = <>
        Styles.Background = cxStyleTreeListProcMetricsBackground
        TabOrder = 1
        OnDblClick = TreeListProcMetricsDblClick
        object TreeListProcMetricsColumnExecutionID: TcxTreeListColumn
          Caption.Text = '_ExecutionID'
          DataBinding.ValueType = 'Integer'
          Options.Moving = False
          Width = 120
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          SortOrder = soDescending
          SortIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListProcMetricsColumnProcName: TcxTreeListColumn
          Caption.Text = 'Name'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 453
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListProcMetricsColumnThreadID: TcxTreeListColumn
          Caption.Text = 'Thread ID'
          DataBinding.ValueType = 'LargeInt'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 140
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListProcMetricsColumnStartTimeStamp: TcxTreeListColumn
          Caption.Text = 'Start Timestamp (mm:ss:zzz)'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 250
          Position.ColIndex = 3
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
          OnGetDisplayText = TreeListProcMetricsColumnStartTimeStampGetDisplayText
        end
        object TreeListProcMetricsColumnCallCount: TcxTreeListColumn
          Caption.Text = 'Call Count'
          DataBinding.ValueType = 'LargeInt'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 120
          Position.ColIndex = 4
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object TreeListProcMetricsColumnTimeTaken: TcxTreeListColumn
          Caption.Text = 'TimeTaken'
          DataBinding.ValueType = 'Float'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 150
          Position.ColIndex = 5
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
      object GridProcMetrics: TcxGrid
        Left = 0
        Top = 241
        Width = 1254
        Height = 670
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI Light'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        object GridTableViewProcMetrics: TcxGridTableView
          OnCellDblClick = GridTableViewProcMetricsCellDblClick
          DataController.Summary.DefaultGroupSummaryItems = <
            item
              Format = '0.##;-0.##'
              Kind = skCount
              Column = GridTableViewProcMetricsColumnProcName
              Sorted = True
            end>
          DataController.Summary.FooterSummaryItems = <
            item
              Format = '0.######;-0.######'
              Kind = skSum
              Column = GridTableViewProcMetricsColumnTimeTaken
            end
            item
              Kind = skCount
              Column = GridTableViewProcMetricsColumnProcName
            end>
          DateTimeHandling.Grouping = dtgByDate
          OptionsBehavior.CellHints = True
          OptionsBehavior.CopyCaptionsToClipboard = False
          OptionsCustomize.ColumnsQuickCustomization = True
          OptionsData.Deleting = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.MultiSelect = True
          OptionsSelection.HideSelection = True
          OptionsView.CellEndEllipsis = True
          OptionsView.ColumnAutoWidth = True
          OptionsView.Footer = True
          OptionsView.GroupByBox = False
          OptionsView.HeaderEndEllipsis = True
          object GridTableViewProcMetricsColumnExecutionID: TcxGridColumn
            Caption = '_ExecutionID'
            DataBinding.ValueType = 'Integer'
            Options.Moving = False
            Width = 120
          end
          object GridTableViewProcMetricsColumnProcName: TcxGridColumn
            Caption = 'Name'
            Width = 575
          end
          object GridTableViewProcMetricsColumnThreadID: TcxGridColumn
            Caption = 'Thread ID'
            DataBinding.ValueType = 'LargeInt'
            Width = 140
          end
          object GridTableViewProcMetricsColumnStartTimestamp: TcxGridColumn
            Caption = 'Start Timestamp (mm:ss:zzz)'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = GridTableViewProcMetricsColumnStartTimestampGetDisplayText
            Options.Filtering = False
            Options.AutoWidthSizable = False
            Options.SortByDisplayText = isbtOff
            SortIndex = 0
            SortOrder = soAscending
            Width = 250
          end
          object GridTableViewProcMetricsColumnCallCount: TcxGridColumn
            Caption = 'Call Count'
            DataBinding.ValueType = 'LargeInt'
            Width = 120
          end
          object GridTableViewProcMetricsColumnTimeTaken: TcxGridColumn
            Caption = 'Time Taken'
            DataBinding.ValueType = 'Float'
            Options.Filtering = False
            Options.AutoWidthSizable = False
            Width = 150
          end
        end
        object GridLevelProcMetrics: TcxGridLevel
          GridView = GridTableViewProcMetrics
        end
      end
      object cxSplitter1: TcxSplitter
        Left = 0
        Top = 234
        Width = 1254
        Height = 7
        AlignSplitter = salTop
      end
    end
  end
  object MainStatusBar: TdxStatusBar
    Left = 0
    Top = 953
    Width = 1264
    Height = 32
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 400
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
  end
  object dxSkinController: TdxSkinController
    NativeStyle = False
    SkinName = 'Foggy'
    Left = 808
    Top = 152
  end
  object cxStyleRepository: TcxStyleRepository
    Left = 920
    Top = 152
    PixelsPerInch = 96
    object cxStyleTreeListProcMetricsBackground: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
  end
  object IdHTTPServer: TIdHTTPServer
    Bindings = <>
    OnConnect = IdHTTPServerConnect
    OnException = IdHTTPServerException
    OnListenException = IdHTTPServerListenException
    OnCommandGet = IdHTTPServerCommandGet
    Left = 693
    Top = 150
  end
end
