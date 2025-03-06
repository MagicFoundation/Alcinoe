object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'ALDatabaseBenchmark'
  ClientHeight = 763
  ClientWidth = 1003
  Color = 15986666
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClick = FormClick
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 744
    Width = 1003
    Height = 19
    Color = 15525605
    Panels = <
      item
        Width = 150
      end
      item
        Width = 600
      end
      item
        Width = 200
      end>
  end
  object Panel3: TPanel
    Left = 0
    Top = 329
    Width = 1003
    Height = 415
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    object Splitter4: TSplitter
      Left = 705
      Top = 1
      Height = 413
      Align = alRight
      ExplicitLeft = 624
      ExplicitTop = -3
      ExplicitHeight = 469
    end
    object Panel5: TPanel
      Left = 708
      Top = 1
      Width = 294
      Height = 413
      Align = alRight
      Caption = 'Panel2'
      TabOrder = 0
      object ALMemoResult: TcxMemo
        Left = 1
        Top = 1
        Align = alClient
        ParentFont = False
        Properties.ReadOnly = True
        Properties.ScrollBars = ssVertical
        Style.Color = clBtnFace
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 0
        Height = 411
        Width = 292
      end
    end
    object PanelStats: TPanel
      Left = 1
      Top = 1
      Width = 704
      Height = 413
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      object GridThread: TcxGrid
        Left = 1
        Top = 1
        Width = 702
        Height = 411
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object TableViewThread: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          Navigator.Buttons.First.Visible = False
          Navigator.Buttons.PriorPage.Visible = False
          Navigator.Buttons.Prior.Visible = False
          Navigator.Buttons.Next.Visible = False
          Navigator.Buttons.NextPage.Visible = False
          Navigator.Buttons.Last.Visible = False
          Navigator.Buttons.Edit.Visible = True
          Navigator.Buttons.Refresh.Visible = False
          Navigator.Buttons.SaveBookmark.Visible = False
          Navigator.Buttons.GotoBookmark.Visible = False
          Navigator.Buttons.Filter.Visible = False
          FilterBox.CustomizeDialog = False
          ScrollbarAnnotations.CustomAnnotations = <>
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          Filtering.MRUItemsList = False
          Filtering.ColumnMRUItemsList = False
          OptionsBehavior.FocusCellOnTab = True
          OptionsBehavior.FocusFirstCellOnNewRecord = True
          OptionsBehavior.GoToNextCellOnEnter = True
          OptionsBehavior.FocusCellOnCycle = True
          OptionsBehavior.ImmediateEditor = False
          OptionsCustomize.ColumnFiltering = False
          OptionsCustomize.ColumnGrouping = False
          OptionsCustomize.ColumnHidingOnGrouping = False
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.HideSelection = True
          OptionsView.CellEndEllipsis = True
          OptionsView.ColumnAutoWidth = True
          OptionsView.GroupByBox = False
          OptionsView.HeaderEndEllipsis = True
          Styles.Background = cxStyle1
          object TableViewThreadNumber: TcxGridColumn
            Caption = 'Thread #'
            Width = 61
          end
          object TableViewThreadCount: TcxGridColumn
            Caption = 'Count'
            DataBinding.ValueType = 'Integer'
            Width = 46
          end
          object TableViewThreadAveragePrepareTimeTaken: TcxGridColumn
            Caption = 'Average Prepare (ms) / rec'
            DataBinding.ValueType = 'Float'
            HeaderAlignmentHorz = taCenter
            Width = 155
          end
          object TableViewThreadAverageExecuteTimeTaken: TcxGridColumn
            Caption = 'Average Execute (ms) / rec'
            DataBinding.ValueType = 'Float'
            HeaderAlignmentHorz = taCenter
            Width = 151
          end
          object TableViewThreadAverageCommitTimeTaken: TcxGridColumn
            Caption = 'Average Commit (ms) / rec'
            DataBinding.ValueType = 'Float'
            HeaderAlignmentHorz = taCenter
            Width = 160
          end
          object TableViewThreadErrorMsg: TcxGridColumn
            Caption = 'Error Msg'
            HeaderAlignmentHorz = taCenter
            Width = 131
          end
        end
        object levelThread: TcxGridLevel
          GridView = TableViewThread
        end
      end
    end
  end
  object PageControl1: TcxPageControl
    Left = 0
    Top = 0
    Width = 1003
    Height = 329
    Align = alTop
    Color = clBtnFace
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    Properties.ActivePage = MySQL
    Properties.CustomButtons.Buttons = <>
    ClientRectBottom = 324
    ClientRectLeft = 5
    ClientRectRight = 998
    ClientRectTop = 27
    object MySQL: TcxTabSheet
      Caption = 'MySQL'
      object Label3: TcxLabel
        Left = 45
        Top = 74
        Caption = 'Host Name'
        ParentFont = False
        TabOrder = 15
        Transparent = True
      end
      object Label6: TcxLabel
        Left = 72
        Top = 148
        Caption = 'Login'
        ParentFont = False
        TabOrder = 16
        Transparent = True
      end
      object Label7: TcxLabel
        Left = 51
        Top = 173
        Caption = 'Password'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object Label8: TcxLabel
        Left = 37
        Top = 99
        Caption = 'Port Number'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
      object Label10: TcxLabel
        Left = 59
        Top = 198
        Caption = 'Charset'
        ParentFont = False
        TabOrder = 19
        Transparent = True
      end
      object Label11: TcxLabel
        Left = 43
        Top = 49
        Caption = 'LibMySql dll'
        ParentFont = False
        TabOrder = 20
        Transparent = True
      end
      object Label9: TcxLabel
        Left = 377
        Top = 22
        Caption = 'SQL'
        ParentFont = False
        TabOrder = 21
        Transparent = True
      end
      object Label12: TcxLabel
        Left = 51
        Top = 124
        Caption = 'Database'
        ParentFont = False
        TabOrder = 22
        Transparent = True
      end
      object Label32: TcxLabel
        Left = 839
        Top = 182
        Caption = 'Nb Loop:'
        ParentFont = False
        TabOrder = 23
        Transparent = True
      end
      object Label33: TcxLabel
        Left = 812
        Top = 207
        Caption = 'Commit every:'
        ParentFont = False
        TabOrder = 24
        Transparent = True
      end
      object Label34: TcxLabel
        Left = 828
        Top = 157
        Caption = 'Nb Thread:'
        ParentFont = False
        TabOrder = 25
        Transparent = True
      end
      object Label40: TcxLabel
        Left = 18
        Top = 23
        Caption = 'LibMySql Version'
        ParentFont = False
        TabOrder = 26
        Transparent = True
      end
      object ALEditMySqlHost: TcxTextEdit
        Left = 106
        Top = 71
        TabOrder = 1
        Text = 'localhost'
        Width = 218
      end
      object ALEditMySqlLogin: TcxTextEdit
        Left = 106
        Top = 145
        TabOrder = 4
        Text = 'root'
        Width = 218
      end
      object ALEditMySqlPassword: TcxTextEdit
        Left = 106
        Top = 170
        TabOrder = 5
        Width = 218
      end
      object ALEditMySqlPort: TcxTextEdit
        Left = 106
        Top = 96
        TabOrder = 2
        Text = '3306'
        Width = 88
      end
      object ALEditMySqlCharset: TcxTextEdit
        Left = 106
        Top = 195
        TabOrder = 6
        Text = 'utf8'
        Width = 218
      end
      object ALEditMysqlLib: TcxButtonEdit
        Left = 106
        Top = 46
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = ALEditButtonFindFileClick
        TabOrder = 0
        Text = 'libmysql.dll'
        Width = 218
      end
      object ALMemoMySqlQuery: TcxMemo
        Left = 406
        Top = 21
        Lines.Strings = (
          'Select '
          '  FieldA'
          'From'
          '  TableSample'
          'Where'
          '  fieldB = '#39'<#randomString maxlength=50>'#39' or'
          '  fieldC= <#randomNumber>')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 7
        Height = 258
        Width = 338
      end
      object ALButtonMySQLSelect: TcxButton
        Left = 803
        Top = 22
        Width = 161
        Height = 25
        Caption = 'SELECT'
        TabOrder = 8
        OnClick = ALButtonMySqlSelectClick
      end
      object ALEditMySqlDatabaseName: TcxTextEdit
        Left = 106
        Top = 121
        TabOrder = 3
        Width = 218
      end
      object ALEditMySqlNBLoop: TcxTextEdit
        Left = 890
        Top = 179
        TabOrder = 13
        Text = '1000000'
        Width = 74
      end
      object ALEditMySqlNbLoopBeforeCommit: TcxTextEdit
        Left = 890
        Top = 204
        TabOrder = 14
        Text = '1'
        Width = 74
      end
      object ALEditMySqlNBThread: TcxTextEdit
        Left = 890
        Top = 154
        TabOrder = 12
        Text = '1'
        Width = 74
      end
      object ALButtonMysqlUpdate: TcxButton
        Left = 803
        Top = 53
        Width = 161
        Height = 25
        Caption = 'UPDATE'
        TabOrder = 9
        OnClick = ALButtonMysqlUpdateClick
      end
      object ALButtonMySqlLoopUpdate: TcxButton
        Left = 803
        Top = 115
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE'
        TabOrder = 11
        OnClick = ALButtonMySqlLoopUpdateClick
      end
      object ALButtonMysqlLoopSelect: TcxButton
        Left = 803
        Top = 84
        Width = 161
        Height = 25
        Caption = 'Loop SELECT'
        TabOrder = 10
        OnClick = ALButtonMysqlLoopSelectClick
      end
      object ALComboBoxMySqlApiVer: TcxComboBox
        Left = 106
        Top = 21
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'MYSQL50'
          'MYSQL55')
        TabOrder = 27
        Text = 'MYSQL50'
        Width = 121
      end
    end
    object SQLLite3: TcxTabSheet
      Caption = 'SQLLite3'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label24: TcxLabel
        Left = 17
        Top = 21
        Caption = 'Sqlite3.dll'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object Label25: TcxLabel
        Left = 378
        Top = 21
        Caption = 'SQL'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
      object Label19: TcxLabel
        Left = 17
        Top = 46
        Caption = 'Database'
        ParentFont = False
        TabOrder = 19
        Transparent = True
      end
      object Label20: TcxLabel
        Left = 831
        Top = 173
        Caption = 'Nb Loop:'
        ParentFont = False
        TabOrder = 20
        Transparent = True
      end
      object Label21: TcxLabel
        Left = 11
        Top = 235
        Caption = 'cache_size'
        ParentFont = False
        TabOrder = 21
        Transparent = True
      end
      object Label22: TcxLabel
        Left = 15
        Top = 260
        Caption = 'page_size'
        ParentFont = False
        TabOrder = 22
        Transparent = True
      end
      object Label23: TcxLabel
        Left = 804
        Top = 198
        Caption = 'Commit every:'
        ParentFont = False
        TabOrder = 23
        Transparent = True
      end
      object Label28: TcxLabel
        Left = 819
        Top = 148
        Caption = 'Nb Thread:'
        ParentFont = False
        TabOrder = 24
        Transparent = True
      end
      object ALEditSqlite3Lib: TcxButtonEdit
        Left = 73
        Top = 18
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = ALEditButtonFindFileClick
        TabOrder = 0
        Text = 'Sqlite3.dll'
        Width = 249
      end
      object ALMemoSqlite3Query: TcxMemo
        Left = 409
        Top = 18
        Lines.Strings = (
          'Select '
          '  FieldA'
          'From'
          '  TableSample'
          'Where'
          '  fieldB = '#39'<#randomString maxlength=50>'#39' or'
          '  fieldC= <#randomNumber>')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 9
        Height = 258
        Width = 338
      end
      object ALButtonSqlLite3Select: TcxButton
        Left = 803
        Top = 19
        Width = 161
        Height = 25
        Caption = 'SELECT'
        TabOrder = 10
        OnClick = ALButtonSqlLite3SelectClick
      end
      object ALEditSqlite3Database: TcxButtonEdit
        Left = 73
        Top = 43
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = ALEditButtonFindFileClick
        TabOrder = 1
        Width = 249
      end
      object ALButtonSqlite3LoopSelect: TcxButton
        Left = 803
        Top = 83
        Width = 161
        Height = 25
        Caption = 'Loop SELECT'
        TabOrder = 12
        OnClick = ALButtonSqlite3LoopSelectClick
      end
      object ALButtonSqlite3Update: TcxButton
        Left = 803
        Top = 52
        Width = 161
        Height = 25
        Caption = 'UPDATE'
        TabOrder = 11
        OnClick = ALButtonSqlite3UpdateClick
      end
      object ALButtonSqlite3LoopUpdate: TcxButton
        Left = 803
        Top = 114
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE'
        TabOrder = 13
        OnClick = ALButtonSqlite3LoopUpdateClick
      end
      object ALEditSQLite3NBLoop: TcxTextEdit
        Left = 890
        Top = 170
        TabOrder = 15
        Text = '1000000'
        Width = 74
      end
      object RadioGroupSqlite3Journal_Mode: TcxRadioGroup
        Left = 9
        Top = 79
        Caption = 'journal_mode'
        ParentFont = False
        Properties.Items = <
          item
            Caption = 'DELETE'
          end
          item
            Caption = 'TRUNCATE'
          end
          item
            Caption = 'PERSIST'
          end
          item
            Caption = 'MEMORY'
          end
          item
            Caption = 'WAL'
          end
          item
            Caption = 'OFF'
          end>
        ItemIndex = 0
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 2
        Height = 148
        Width = 104
      end
      object RadioGroupSQLite3Temp_Store: TcxRadioGroup
        Left = 119
        Top = 78
        Caption = 'temp_store'
        Properties.Items = <
          item
            Caption = 'DEFAULT'
          end
          item
            Caption = 'FILE'
          end
          item
            Caption = 'MEMORY'
          end>
        ItemIndex = 0
        TabOrder = 3
        Height = 89
        Width = 104
      end
      object RadioGroupSqlite3Synhcronous: TcxRadioGroup
        Left = 229
        Top = 78
        Caption = 'synchronous '
        Properties.Items = <
          item
            Caption = 'OFF'
          end
          item
            Caption = 'NORMAL'
          end
          item
            Caption = 'FULL'
          end>
        ItemIndex = 2
        TabOrder = 4
        Height = 89
        Width = 104
      end
      object ALEditSqlite3Cache_Size: TcxTextEdit
        Left = 73
        Top = 232
        TabOrder = 5
        Text = '2000'
        Width = 43
      end
      object ALEditSqlite3Page_Size: TcxTextEdit
        Left = 73
        Top = 257
        TabOrder = 6
        Text = '1024'
        Width = 46
      end
      object ALEditSQLite3NbLoopBeforeCommit: TcxTextEdit
        Left = 890
        Top = 195
        TabOrder = 16
        Text = '1'
        Width = 74
      end
      object ALEditSqlite3NBThread: TcxTextEdit
        Left = 890
        Top = 145
        TabOrder = 14
        Text = '1'
        Width = 74
      end
      object ALCheckBoxSqlite3SharedCache: TcxCheckBox
        Left = 141
        Top = 235
        Caption = 'shared cache'
        ParentFont = False
        TabOrder = 7
        Transparent = True
      end
      object ALCheckBoxSqlite3ReadUncommited: TcxCheckBox
        Left = 141
        Top = 257
        Caption = 'read uncommitted'
        ParentFont = False
        TabOrder = 8
        Transparent = True
      end
    end
    object Sphinx: TcxTabSheet
      Caption = 'Sphinx'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label36: TcxLabel
        Left = 38
        Top = 49
        Caption = 'LibMysql dll'
        ParentFont = False
        TabOrder = 11
        Transparent = True
      end
      object Label37: TcxLabel
        Left = 39
        Top = 75
        Caption = 'Host Name'
        ParentFont = False
        TabOrder = 12
        Transparent = True
      end
      object Label38: TcxLabel
        Left = 31
        Top = 100
        Caption = 'Port Number'
        ParentFont = False
        TabOrder = 13
        Transparent = True
      end
      object Label43: TcxLabel
        Left = 372
        Top = 21
        Caption = 'SQL'
        ParentFont = False
        TabOrder = 14
        Transparent = True
      end
      object Label44: TcxLabel
        Left = 819
        Top = 156
        Caption = 'Nb Thread:'
        ParentFont = False
        TabOrder = 15
        Transparent = True
      end
      object Label45: TcxLabel
        Left = 831
        Top = 181
        Caption = 'Nb Loop:'
        ParentFont = False
        TabOrder = 16
        Transparent = True
      end
      object Label46: TcxLabel
        Left = 804
        Top = 206
        Caption = 'Commit every:'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object Label39: TcxLabel
        Left = 12
        Top = 24
        Caption = 'LibMySql Version'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
      object ALEditSphinxLib: TcxButtonEdit
        Left = 104
        Top = 47
        Properties.Buttons = <
          item
            Default = True
            Kind = bkEllipsis
          end>
        Properties.OnButtonClick = ALEditButtonFindFileClick
        TabOrder = 0
        Text = 'libmysql.dll'
        Width = 221
      end
      object ALEditSphinxHost: TcxTextEdit
        Left = 104
        Top = 72
        TabOrder = 1
        Text = 'localhost'
        Width = 221
      end
      object ALEditSphinxPort: TcxTextEdit
        Left = 104
        Top = 97
        TabOrder = 2
        Text = '9306'
        Width = 88
      end
      object ALMemoSphinxQuery: TcxMemo
        Left = 403
        Top = 18
        Lines.Strings = (
          'Select '
          '  FieldA'
          'From'
          '  TableSample'
          'Where'
          '  fieldC= <#randomNumber>')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 3
        Height = 258
        Width = 338
      end
      object ALButtonSphinxSelect: TcxButton
        Left = 803
        Top = 21
        Width = 161
        Height = 25
        Caption = 'SELECT'
        TabOrder = 4
        OnClick = ALButtonSphinxSelectClick
      end
      object ALButtonSphinxUpdate: TcxButton
        Left = 804
        Top = 52
        Width = 161
        Height = 25
        Caption = 'UPDATE'
        TabOrder = 5
        OnClick = ALButtonSphinxUpdateClick
      end
      object ALButtonSphinxLoopSelect: TcxButton
        Left = 803
        Top = 83
        Width = 161
        Height = 25
        Caption = 'Loop SELECT'
        TabOrder = 6
        OnClick = ALButtonSphinxLoopSelectClick
      end
      object ALButtonSphinxLoopUpdate: TcxButton
        Left = 803
        Top = 114
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE'
        TabOrder = 7
        OnClick = ALButtonSphinxLoopUpdateClick
      end
      object ALEditSphinxNBThread: TcxTextEdit
        Left = 890
        Top = 153
        TabOrder = 8
        Text = '1'
        Width = 74
      end
      object ALEditSphinxNBLoop: TcxTextEdit
        Left = 890
        Top = 178
        TabOrder = 9
        Text = '1000000'
        Width = 74
      end
      object ALEditSphinxNbLoopBeforeCommit: TcxTextEdit
        Left = 891
        Top = 205
        TabOrder = 10
        Text = '1'
        Width = 74
      end
      object ALComboBoxSphinxApiVer: TcxComboBox
        Left = 104
        Top = 22
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'MYSQL50'
          'MYSQL55')
        TabOrder = 19
        Text = 'MYSQL50'
        Width = 121
      end
    end
    object MemCached: TcxTabSheet
      Caption = 'MemCached'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label41: TcxLabel
        Left = 25
        Top = 21
        Caption = 'Host Name'
        ParentFont = False
        TabOrder = 14
        Transparent = True
      end
      object Label42: TcxLabel
        Left = 17
        Top = 46
        Caption = 'Port Number'
        ParentFont = False
        TabOrder = 15
        Transparent = True
      end
      object Label47: TcxLabel
        Left = 365
        Top = 97
        Caption = 'DATA'
        ParentFont = False
        TabOrder = 16
        Transparent = True
      end
      object Label48: TcxLabel
        Left = 819
        Top = 244
        Caption = 'Nb Thread:'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object Label49: TcxLabel
        Left = 831
        Top = 269
        Caption = 'Nb Loop:'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
      object ALEditMemCachedHost: TcxTextEdit
        Left = 94
        Top = 18
        TabOrder = 0
        Text = 'localhost'
        Width = 221
      end
      object ALEditMemCachedPort: TcxTextEdit
        Left = 94
        Top = 43
        TabOrder = 1
        Text = '11211'
        Width = 88
      end
      object ALMemoMemCachedData: TcxMemo
        Left = 405
        Top = 96
        Lines.Strings = (
          '<#randomString maxlength=100000>')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 2
        Height = 180
        Width = 338
      end
      object ALButtonMemcachedGet: TcxButton
        Left = 803
        Top = 18
        Width = 161
        Height = 25
        Caption = 'GET'
        TabOrder = 3
        OnClick = ALButtonMemcachedGetClick
      end
      object ALButtonMemcachedSet: TcxButton
        Left = 804
        Top = 49
        Width = 161
        Height = 25
        Caption = 'SET'
        TabOrder = 4
        OnClick = ALButtonMemcachedSetClick
      end
      object ALButtonMemcachedLoopGet: TcxButton
        Left = 805
        Top = 112
        Width = 161
        Height = 25
        Caption = 'Loop GET'
        TabOrder = 5
        OnClick = ALButtonMemcachedLoopGetClick
      end
      object ALButtonMemcachedLoopSet: TcxButton
        Left = 805
        Top = 143
        Width = 161
        Height = 25
        Caption = 'Loop SET'
        TabOrder = 6
        OnClick = ALButtonMemcachedLoopSetClick
      end
      object ALEditMemcachedNBThread: TcxTextEdit
        Left = 891
        Top = 239
        TabOrder = 7
        Text = '1'
        Width = 74
      end
      object ALEditMemCachedNBLoop: TcxTextEdit
        Left = 890
        Top = 266
        TabOrder = 8
        Text = '1000000'
        Width = 74
      end
      object ALButtonMemcachedStats: TcxButton
        Left = 17
        Top = 97
        Width = 88
        Height = 25
        Caption = 'STATS'
        TabOrder = 9
        OnClick = ALButtonMemcachedStatsClick
      end
      object ALButtonMemcachedStatsSettings: TcxButton
        Left = 17
        Top = 128
        Width = 88
        Height = 25
        Caption = 'STATS Settings'
        TabOrder = 10
        OnClick = ALButtonMemcachedStatsSettingsClick
      end
      object ALButtonMemcachedStatsItems: TcxButton
        Left = 17
        Top = 159
        Width = 88
        Height = 25
        Caption = 'STATS items'
        TabOrder = 11
        OnClick = ALButtonMemcachedStatsItemsClick
      end
      object ALButtonMemcachedStatsSizes: TcxButton
        Left = 17
        Top = 221
        Width = 88
        Height = 25
        Caption = 'STATS sizes'
        TabOrder = 12
        OnClick = ALButtonMemcachedStatsSizesClick
      end
      object ALButtonMemcachedStatsSlabs: TcxButton
        Left = 17
        Top = 190
        Width = 88
        Height = 25
        Caption = 'STATS slabs'
        TabOrder = 13
        OnClick = ALButtonMemcachedStatsSlabsClick
      end
      object ALButtonMemcachedFLush_ALL: TcxButton
        Left = 131
        Top = 94
        Width = 88
        Height = 25
        Caption = 'FLUSH_ALL'
        TabOrder = 19
        OnClick = ALButtonMemcachedFLush_ALLClick
      end
      object ALButtonMemcachedVersion: TcxButton
        Left = 131
        Top = 128
        Width = 88
        Height = 25
        Caption = 'VERSION'
        TabOrder = 20
        OnClick = ALButtonMemcachedVersionClick
      end
      object ALEditMemCachedKey: TcxTextEdit
        Left = 405
        Top = 18
        TabOrder = 21
        Text = '<#randomString maxlength=250>'
        Width = 338
      end
      object ALlabel123123: TcxLabel
        Left = 374
        Top = 19
        Caption = 'Key'
        ParentFont = False
        TabOrder = 22
        Transparent = True
      end
      object ALLabel43234: TcxLabel
        Left = 367
        Top = 46
        Caption = 'Flags'
        ParentFont = False
        TabOrder = 23
        Transparent = True
      end
      object ALEditMemCachedFlags: TcxTextEdit
        Left = 402
        Top = 45
        TabOrder = 24
        Text = '<#randomnumber>'
        Width = 215
      end
      object cxLabel5: TcxLabel
        Left = 344
        Top = 70
        Caption = 'Expiration'
        ParentFont = False
        TabOrder = 25
        Transparent = True
      end
      object ALEditMemCachedExpTime: TcxTextEdit
        Left = 403
        Top = 69
        TabOrder = 26
        Text = '<#randomnumber>'
        Width = 214
      end
      object ALButtonMemcachedDelete: TcxButton
        Left = 805
        Top = 80
        Width = 161
        Height = 25
        Caption = 'DELETE'
        TabOrder = 27
        OnClick = ALButtonMemcachedDeleteClick
      end
      object ALButtonMemcachedLoopIncr: TcxButton
        Left = 805
        Top = 174
        Width = 161
        Height = 25
        Caption = 'Loop INCR'
        TabOrder = 28
        OnClick = ALButtonMemcachedLoopIncrClick
      end
      object ALButtonMemcachedLoopDecr: TcxButton
        Left = 804
        Top = 205
        Width = 161
        Height = 25
        Caption = 'Loop DECR'
        TabOrder = 29
        OnClick = ALButtonMemcachedLoopDecrClick
      end
    end
    object MongoDB: TcxTabSheet
      Caption = 'MongoDB'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cxLabel3: TcxLabel
        Left = 25
        Top = 21
        Caption = 'Host Name'
        ParentFont = False
        TabOrder = 16
        Transparent = True
      end
      object ALEditMongoDBHost: TcxTextEdit
        Left = 87
        Top = 18
        TabOrder = 0
        Text = 'localhost'
        Width = 221
      end
      object cxLabel4: TcxLabel
        Left = 17
        Top = 46
        Caption = 'Port Number'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object ALEditMongoDBPort: TcxTextEdit
        Left = 87
        Top = 43
        TabOrder = 1
        Text = '27017'
        Width = 88
      end
      object cxLabel9: TcxLabel
        Left = 341
        Top = 47
        Caption = 'For Select: Query'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
      object MemoMongoDBQuery: TcxMemo
        Left = 435
        Top = 46
        Lines.Strings = (
          '{FieldC: {'#39'$gt'#39': <#randomnumber>}}')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 3
        Height = 86
        Width = 326
      end
      object ALButtonMongoDBSelect: TcxButton
        Left = 802
        Top = 4
        Width = 161
        Height = 25
        Caption = 'SELECT'
        TabOrder = 7
        OnClick = ALButtonMongoDBSelectClick
      end
      object ALButtonMongoDBINSERT: TcxButton
        Left = 803
        Top = 35
        Width = 161
        Height = 25
        Caption = 'INSERT'
        TabOrder = 8
        OnClick = ALButtonMongoDBINSERTClick
      end
      object ALButtonMongoDBUpdate: TcxButton
        Left = 804
        Top = 66
        Width = 161
        Height = 25
        Caption = 'UPDATE'
        TabOrder = 9
        OnClick = ALButtonMongoDBUpdateClick
      end
      object ALButtonMongoDBDelete: TcxButton
        Left = 804
        Top = 98
        Width = 161
        Height = 25
        Caption = 'DELETE'
        TabOrder = 10
        OnClick = ALButtonMongoDBDeleteClick
      end
      object ALButtonMongoDBLOOPSELECT: TcxButton
        Left = 804
        Top = 129
        Width = 161
        Height = 25
        Caption = 'Loop SELECT'
        TabOrder = 11
        OnClick = ALButtonMongoDBLOOPSELECTClick
      end
      object ALButtonMongoDBLOOPINSERT: TcxButton
        Left = 804
        Top = 160
        Width = 161
        Height = 25
        Caption = 'Loop INSERT'
        TabOrder = 12
        OnClick = ALButtonMongoDBLOOPINSERTClick
      end
      object ALButtonMongoDBLOOPUPDATE: TcxButton
        Left = 803
        Top = 191
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE'
        TabOrder = 13
        OnClick = ALButtonMongoDBLOOPUPDATEClick
      end
      object ALEditMongoDBNBThread: TcxTextEdit
        Left = 891
        Top = 253
        TabOrder = 14
        Text = '1'
        Width = 74
      end
      object cxLabel10: TcxLabel
        Left = 819
        Top = 254
        Caption = 'Nb Thread:'
        ParentFont = False
        TabOrder = 19
        Transparent = True
      end
      object cxLabel11: TcxLabel
        Left = 831
        Top = 280
        Caption = 'Nb Loop:'
        ParentFont = False
        TabOrder = 20
        Transparent = True
      end
      object ALEditMongoDBNBLoop: TcxTextEdit
        Left = 890
        Top = 277
        TabOrder = 15
        Text = '1000000'
        Width = 74
      end
      object cxLabel6: TcxLabel
        Left = 273
        Top = 140
        Caption = 'For select: ReturnFieldsSelector'
        ParentFont = False
        TabOrder = 21
        Transparent = True
      end
      object MemoMongoDBSelector: TcxMemo
        Left = 435
        Top = 139
        Lines.Strings = (
          '{'#39'FieldA'#39': 1}')
        ParentFont = False
        Properties.ScrollBars = ssVertical
        TabOrder = 4
        Height = 103
        Width = 326
      end
      object cxLabel7: TcxLabel
        Left = 337
        Top = 21
        Caption = 'FullCollectionName'
        ParentFont = False
        TabOrder = 22
        Transparent = True
      end
      object EditMongoDBFullCollectionName: TcxTextEdit
        Left = 435
        Top = 18
        TabOrder = 2
        Text = 'test.TableSample'
        Width = 326
      end
      object cxLabel8: TcxLabel
        Left = 433
        Top = 249
        Caption = 'Skip'
        ParentFont = False
        TabOrder = 23
        Transparent = True
      end
      object EditMongoDBSkip: TcxTextEdit
        Left = 462
        Top = 248
        TabOrder = 5
        Text = '0'
        Width = 88
      end
      object cxLabel12: TcxLabel
        Left = 572
        Top = 249
        Caption = 'First'
        ParentFont = False
        TabOrder = 24
        Transparent = True
      end
      object EditMongoDBFirst: TcxTextEdit
        Left = 603
        Top = 248
        TabOrder = 6
        Text = '200'
        Width = 88
      end
      object cxLabel13: TcxLabel
        Left = 327
        Top = 78
        Caption = 'For update: Selector'
        ParentFont = False
        TabOrder = 25
        Transparent = True
      end
      object cxLabel14: TcxLabel
        Left = 331
        Top = 156
        Caption = 'For update: Update'
        ParentFont = False
        TabOrder = 26
        Transparent = True
      end
      object cxLabel15: TcxLabel
        Left = 320
        Top = 62
        Caption = 'For insert: Documents'
        ParentFont = False
        TabOrder = 27
        Transparent = True
      end
      object cxLabel16: TcxLabel
        Left = 331
        Top = 95
        Caption = 'For delete: Selector'
        ParentFont = False
        TabOrder = 28
        Transparent = True
      end
      object CheckGroupMongoDBSelectFlags: TcxCheckGroup
        Left = 17
        Top = 87
        Caption = 'SELECT flags'
        Properties.Items = <
          item
            Caption = 'SalveOK'
          end
          item
            Caption = 'Partial'
          end>
        TabOrder = 29
        Height = 67
        Width = 93
      end
      object CheckGroupMongoDBINSERTFlags: TcxCheckGroup
        Left = 129
        Top = 87
        Caption = 'INSERT flags'
        Properties.Items = <
          item
            Caption = 'ContinueOnError'
          end>
        TabOrder = 30
        Height = 47
        Width = 112
      end
      object CheckGroupMongoDBUpdateFlags: TcxCheckGroup
        Left = 17
        Top = 174
        Caption = 'UPDATE flags'
        ParentFont = False
        Properties.Items = <
          item
            Caption = 'Upsert'
          end
          item
            Caption = 'MultiUpdate'
          end>
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'MS Sans Serif'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 31
        Height = 67
        Width = 93
      end
      object CheckGroupMongoDBDeleteFlags: TcxCheckGroup
        Left = 129
        Top = 172
        Caption = 'DELETE flags'
        Properties.Items = <
          item
            Caption = 'SingleRemove'
          end>
        TabOrder = 32
        Height = 46
        Width = 112
      end
      object ALButtonMongoDBLOOPDELETE: TcxButton
        Left = 802
        Top = 222
        Width = 161
        Height = 25
        Caption = 'Loop DELETE'
        TabOrder = 33
        OnClick = ALButtonMongoDBLOOPDELETEClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 144
    Top = 656
  end
  object dxSkinController1: TdxSkinController
    NativeStyle = False
    SkinName = 'Foggy'
    Left = 56
    Top = 648
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 328
    Top = 560
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
  end
end
