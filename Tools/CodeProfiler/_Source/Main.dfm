object MainForm: TMainForm
  Left = 377
  Top = 296
  Caption = 'Alcinoe CodeProfiler'
  ClientHeight = 900
  ClientWidth = 1080
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
    Width = 1080
    Height = 868
    Align = alClient
    TabOrder = 0
    Properties.ActivePage = InstrumentationTabSheet
    Properties.CustomButtons.Buttons = <>
    ExplicitHeight = 768
    ClientRectBottom = 863
    ClientRectLeft = 5
    ClientRectRight = 1075
    ClientRectTop = 37
    object InstrumentationTabSheet: TcxTabSheet
      Caption = 'Source Code Instrumentation'
      ImageIndex = 0
      OnResize = InstrumentationTabSheetResize
      ExplicitHeight = 726
      object InstructionPanel: TdxPanel
        Left = 0
        Top = 0
        Width = 1070
        Height = 424
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
          Width = 1062
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
          Width = 1062
        end
        object cxLabel3: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 107
          Margins.Left = 16
          Align = alTop
          Caption = '2. Add Alcinoe profiler markers to your code.'
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
          ExplicitTop = 74
          Width = 1049
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
          ExplicitTop = 107
          Width = 1049
        end
        object cxLabel5: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 173
          Margins.Left = 16
          Align = alTop
          Caption = 
            '4. If you are using Android or iOS, send the app to the backgrou' +
            'nd and then bring it back to the foreground to generate the perf' +
            'ormance file.'
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
          Width = 1049
        end
        object cxLabel6: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 285
          Margins.Left = 16
          Align = alTop
          Caption = '6. Perform the performance analysis.'
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
          Width = 1049
        end
        object cxLabel8: TcxLabel
          AlignWithMargins = True
          Left = 3
          Top = 318
          Align = alTop
          Caption = 'Note:'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = [fsBold]
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 6
          Transparent = True
          ExplicitTop = 206
          Width = 1062
        end
        object LastInstructionLabel: TcxLabel
          AlignWithMargins = True
          Left = 3
          Top = 351
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            'On Windows, the performance file is stored in the CodeProfiler d' +
            'ata folder if the app is running locally; otherwise, it is saved' +
            ' in the user'#39's document folder. On macOS, iOS, and Android, it i' +
            's always stored in the user'#39's document folder.'
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
          ExplicitTop = 239
          Width = 1062
        end
        object cxLabel13: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 74
          Margins.Left = 16
          Align = alTop
          Caption = '1. Specify the server IP and port for the listening process.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 8
          Transparent = True
          Width = 1049
        end
        object cxLabel14: TcxLabel
          AlignWithMargins = True
          Left = 16
          Top = 229
          Margins.Left = 16
          Align = alTop
          Caption = 
            '5. If you specified the server IP and port in step 1, the perfor' +
            'mance file will be received automatically. After receiving it, s' +
            'imply reload the data; otherwise, download the data from the use' +
            'r'#39's document folder and place it in the CodeProfiler data folder' +
            '.'
          ParentFont = False
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -17
          Style.Font.Name = 'Segoe UI'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          Properties.WordWrap = True
          TabOrder = 9
          Transparent = True
          Width = 1049
        end
      end
      object dxPanel2: TdxPanel
        AlignWithMargins = True
        Left = 0
        Top = 432
        Width = 1070
        Height = 386
        Margins.Left = 0
        Margins.Top = 8
        Margins.Right = 0
        Margins.Bottom = 8
        Align = alClient
        TabOrder = 1
        ExplicitTop = 393
        ExplicitHeight = 325
        object SourcesPathMemo: TcxMemo
          AlignWithMargins = True
          Left = 8
          Top = 164
          Margins.Left = 8
          Margins.Right = 8
          Margins.Bottom = 12
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 147
          Height = 208
          Width = 1052
        end
        object cxLabel9: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 134
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Caption = 
            'Enter source code paths for profiler markers, listing one folder' +
            ' or filename per line. Prefix a name with '#39'!'#39' to ignore the file'
          Properties.WordWrap = True
          TabOrder = 1
          Width = 1052
        end
        object dxPanel3: TdxPanel
          Left = 0
          Top = 95
          Width = 1068
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
        object cxLabel10: TcxLabel
          AlignWithMargins = True
          Left = 8
          Top = 60
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alTop
          Caption = 
            'Specify the IP address and port to automatically receive the per' +
            'formance file, then update the markers in your code.'
          Properties.WordWrap = True
          TabOrder = 3
          Width = 1052
        end
        object dxPanel1: TdxPanel
          Left = 0
          Top = 0
          Width = 1068
          Height = 52
          Align = alTop
          Frame.Borders = []
          LookAndFeel.NativeStyle = False
          LookAndFeel.SkinName = 'Foggy'
          TabOrder = 4
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
      end
    end
    object PerformanceAnalysisTabSheet: TcxTabSheet
      Caption = 'Performance Analysis'
      ImageIndex = 1
      ExplicitHeight = 726
      object Panelfilter: TPanel
        Left = 0
        Top = 0
        Width = 1070
        Height = 89
        Margins.Left = 8
        Align = alTop
        BevelOuter = bvNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        OnResize = PanelfilterResize
        object Label1: TLabel
          Left = 152
          Top = 11
          Width = 168
          Height = 23
          Caption = 'Start Timestamp (Min)'
        end
        object Label2: TLabel
          Left = 520
          Top = 11
          Width = 171
          Height = 23
          Caption = 'Start Timestamp (Max)'
        end
        object ProcNameFilterEdit: TcxTextEdit
          Left = 152
          Top = 45
          TabOrder = 0
          TextHint = 
            'Search for procedure names. Accepts multiple entries separated b' +
            'y '#39';'#39
          Width = 913
        end
        object ApplyFilterBtn: TcxButton
          Left = 8
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
          Left = 326
          Top = 8
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '([0-5][0-9]):([0-5][0-9]):([0-9]{3})\.([0-9]{1,4})'
          TabOrder = 3
          TextHint = 'mm:ss:zzz.zzzz'
          Width = 177
        end
        object StartTimeStampMaxEdit: TcxMaskEdit
          Left = 698
          Top = 8
          Properties.MaskKind = emkRegExpr
          Properties.EditMask = '([0-5][0-9]):([0-5][0-9]):([0-9]{3})\.([0-9]{1,4})'
          TabOrder = 4
          TextHint = 'mm:ss:zzz.zzzz'
          Width = 177
        end
      end
      object TreeListProcMetrics: TcxTreeList
        Left = 0
        Top = 89
        Width = 1070
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
        Navigator.Buttons.CustomButtons = <>
        OptionsBehavior.CopyCaptionsToClipboard = False
        OptionsData.Editing = False
        OptionsView.ColumnAutoWidth = True
        ParentFont = False
        ScrollbarAnnotations.CustomAnnotations = <>
        Styles.Background = cxStyleTreeListProcMetricsBackground
        TabOrder = 1
        OnDblClick = TreeListProcMetricsDblClick
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
        object TreeListProcMetricsColumnTimeTaken: TcxTreeListColumn
          Caption.Text = 'TimeTaken'
          DataBinding.ValueType = 'Float'
          Options.Filtering = False
          Options.Moving = False
          Options.Sorting = False
          Width = 150
          Position.ColIndex = 4
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
      end
      object GridProcMetrics: TcxGrid
        Left = 0
        Top = 241
        Width = 1070
        Height = 585
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -17
        Font.Name = 'Segoe UI Light'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        ExplicitHeight = 485
        object GridTableViewProcMetrics: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          ScrollbarAnnotations.CustomAnnotations = <>
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
          DataController.Summary.SummaryGroups = <>
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
        Width = 1070
        Height = 7
        AlignSplitter = salTop
      end
    end
  end
  object MainStatusBar: TdxStatusBar
    Left = 0
    Top = 868
    Width = 1080
    Height = 32
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 400
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    ExplicitTop = 768
  end
  object dxSkinController: TdxSkinController
    NativeStyle = False
    SkinName = 'Foggy'
    Left = 768
    Top = 120
  end
  object cxStyleRepository: TcxStyleRepository
    Left = 888
    Top = 120
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
    Left = 661
    Top = 118
  end
end
