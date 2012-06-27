object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'ALSQLBenchmark'
  ClientHeight = 764
  ClientWidth = 1007
  Color = 15986666
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 745
    Width = 1007
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 700
      end>
  end
  object Panel3: TPanel
    Left = 0
    Top = 329
    Width = 1007
    Height = 416
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 2
    object Splitter4: TSplitter
      Left = 709
      Top = 1
      Height = 414
      Align = alRight
      ExplicitLeft = 624
      ExplicitTop = -3
      ExplicitHeight = 469
    end
    object Panel5: TPanel
      Left = 712
      Top = 1
      Width = 294
      Height = 414
      Align = alRight
      Caption = 'Panel2'
      TabOrder = 0
      object ALMemoResult: TALMemo
        Left = 1
        Top = 1
        Width = 292
        Height = 259
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        WordWrap = False
        DesignSize = (
          292
          259)
      end
      object Panel2: TPanel
        Left = 1
        Top = 260
        Width = 292
        Height = 153
        Align = alBottom
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clSilver
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 1
        object Label5: TLabel
          Left = 5
          Top = 8
          Width = 132
          Height = 45
          Caption = 'Please help us to keep the development of these components free'
          Font.Charset = ANSI_CHARSET
          Font.Color = clMaroon
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object Label35: TLabel
          Left = 5
          Top = 63
          Width = 125
          Height = 75
          Caption = 
            'If you like these components please simply click on each button ' +
            'below ... thanks for your support !'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Panel4: TPanel
          Left = 151
          Top = 8
          Width = 130
          Height = 134
          BevelOuter = bvNone
          BorderStyle = bsSingle
          Color = clWhite
          Ctl3D = False
          ParentBackground = False
          ParentCtl3D = False
          TabOrder = 0
          object PanelWebBrowser: TPanel
            Left = -5
            Top = -23
            Width = 133
            Height = 159
            BevelOuter = bvNone
            Color = clMedGray
            Ctl3D = False
            ParentBackground = False
            ParentCtl3D = False
            TabOrder = 0
          end
        end
      end
    end
    object PanelStats: TPanel
      Left = 1
      Top = 1
      Width = 708
      Height = 414
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      object GridThread: TcxGrid
        Left = 1
        Top = 1
        Width = 706
        Height = 412
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object TableViewThread: TcxGridTableView
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
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <>
          DataController.Summary.SummaryGroups = <>
          Filtering.MRUItemsList = False
          Filtering.ColumnMRUItemsList = False
          OptionsBehavior.FocusCellOnTab = True
          OptionsBehavior.FocusFirstCellOnNewRecord = True
          OptionsBehavior.GoToNextCellOnEnter = True
          OptionsBehavior.ImmediateEditor = False
          OptionsBehavior.FocusCellOnCycle = True
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1007
    Height = 329
    ActivePage = Firebird
    Align = alTop
    TabOrder = 0
    object Firebird: TTabSheet
      Caption = 'Firebird'
      ImageIndex = 2
      object Label2: TLabel
        Left = 68
        Top = 101
        Width = 32
        Height = 13
        Caption = 'Login'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label4: TLabel
        Left = 44
        Top = 126
        Width = 55
        Height = 13
        Caption = 'Password'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label15: TLabel
        Left = 56
        Top = 151
        Width = 44
        Height = 13
        Caption = 'Charset'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label16: TLabel
        Left = 25
        Top = 51
        Width = 75
        Height = 13
        Caption = 'FBClient DLL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label17: TLabel
        Left = 401
        Top = 27
        Width = 25
        Height = 13
        Caption = 'SQL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label18: TLabel
        Left = 45
        Top = 77
        Width = 55
        Height = 13
        Caption = 'Database'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label13: TLabel
        Left = 836
        Top = 210
        Width = 53
        Height = 13
        Caption = 'Nb Loop:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label26: TLabel
        Left = 808
        Top = 235
        Width = 80
        Height = 13
        Caption = 'Commit every:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label29: TLabel
        Left = 823
        Top = 185
        Width = 65
        Height = 13
        Caption = 'Nb Thread:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label30: TLabel
        Left = 27
        Top = 175
        Width = 72
        Height = 13
        Caption = 'Num_buffers'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label31: TLabel
        Left = 11
        Top = 27
        Width = 89
        Height = 13
        Caption = 'Firebird Version'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 384
        Top = 154
        Width = 42
        Height = 13
        Caption = 'Params'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label14: TLabel
        Left = 380
        Top = 173
        Width = 46
        Height = 13
        Caption = '(1 by row)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label27: TLabel
        Left = 75
        Top = 202
        Width = 25
        Height = 13
        Caption = 'TPB'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ALButtonFirebirdSelect: TALButton
        Left = 807
        Top = 58
        Width = 161
        Height = 25
        Caption = 'Execute SELECT via FireBird'
        TabOrder = 11
        OnClick = ALButtonFirebirdSelectClick
        OnPaint = ALButtonPaint
      end
      object ALEditFirebirdLogin: TALEdit
        Left = 106
        Top = 101
        Width = 249
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 3
        Text = 'SYSDBA'
      end
      object ALEditFirebirdPassword: TALEdit
        Left = 106
        Top = 126
        Width = 249
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 4
      end
      object ALEditFirebirdCharset: TALEdit
        Left = 106
        Top = 151
        Width = 72
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 5
        Text = 'NONE'
      end
      object ALEditFirebirdLib: TALEdit
        Left = 106
        Top = 51
        Width = 249
        Height = 19
        Cursor = crArrow
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 1
        Text = 'FBClient.dll'
      end
      object ALMemoFireBirdQuery: TALMemo
        Left = 432
        Top = 24
        Width = 338
        Height = 121
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '***********************************'
          'Exemple step 1 create the database: '
          '***********************************'
          ''
          'CREATE DATABASE '#39'c:\test.fdb'#39' '
          '              USER '#39'sysdba'#39' '
          '              PASSWORD '#39'masterkey'#39' '
          '              PAGE_SIZE 4096 '
          '              DEFAULT CHARACTER SET ISO8859_1;'
          ''
          ''
          '********************************'
          'Exemple step 2 create the Table: '
          '********************************'
          ''
          'CREATE TABLE HASH('
          '  ID INTEGER NOT NULL,  '
          '  x1_y1 SMALLINT NOT NULL,'
          '  x1_y2 SMALLINT NOT NULL,'
          '  x1_y3 SMALLINT NOT NULL,'
          '  x1_y4 SMALLINT NOT NULL,'
          '  x1_y5 SMALLINT NOT NULL,'
          ' PRIMARY KEY (ID)  '
          ');'
          'CREATE INDEX HASH_X1_Y1_IDX ON HASH (X1_Y1);'
          'CREATE INDEX HASH_X1_Y2_IDX ON HASH (X1_Y2);'
          'CREATE INDEX HASH_X1_Y3_IDX ON HASH (X1_Y3);'
          'CREATE INDEX HASH_X1_Y4_IDX ON HASH (X1_Y4);'
          'CREATE INDEX HASH_X1_Y5_IDX ON HASH (X1_Y5);'
          'CREATE GENERATOR HASH_GEN;'
          ''
          ''
          '**********************************'
          'Exemple step 3 create the trigger: '
          'Note: we must create the trigger alone'
          'because their is the char ; inside the'
          'trigger and the set term is not yet '
          'implemented.'
          '**********************************'
          ''
          'CREATE OR ALTER TRIGGER OnBeforeInsertHash FOR HASH'
          'BEFORE INSERT POSITION 0'
          'AS BEGIN'
          ''
          '  NEW.ID = GEN_ID(HASH_GEN, 1);'
          ''
          'END;'
          ''
          ''
          '********************************'
          'Exemple step 4 add rows in loop:'
          '********************************'
          ''
          'SQL Text:'
          'INSERT INTO HASH('
          '  x1_y1,'
          '  x1_y2,'
          '  x1_y3,'
          '  x1_y4,'
          '  x1_y5'
          ') '
          'VALUES (?,?,?,?,?);'
          ''
          'Params Text: '
          '<#randomnumber min="0" max="255">'
          '<#randomnumber min="0" max="255">'
          '<#randomnumber min="0" max="255">'
          '<#randomnumber min="0" max="255">'
          '<#randomnumber min="0" max="255">'
          ''
          ''
          '***********************************'
          'Exemple step 5 select rows in loop:'
          '***********************************'
          ''
          'Select '
          '  ID '
          'from '
          '  HASH '
          'where '
          '  x1_y1 >= <#randomnumber min="0" max="255" index="1"> and '
          '  x1_y1 <= <#randomnumber min="0" max="255" index="1"> + 20 and '
          '  x1_y2 >= <#randomnumber min="0" max="255" index="2"> and '
          '  x1_y2 <= <#randomnumber min="0" max="255" index="2"> + 20 and '
          '  x1_y3 >= <#randomnumber min="0" max="255" index="3"> and '
          '  x1_y3 <= <#randomnumber min="0" max="255" index="3"> + 20 and '
          '  x1_y4 >= <#randomnumber min="0" max="255" index="4"> and '
          '  x1_y4 <= <#randomnumber min="0" max="255" index="4"> + 20 and '
          '  x1_y5 >= <#randomnumber min="0" max="255" index="5"> and '
          '  x1_y5 <= <#randomnumber min="0" max="255" index="5"> + 20;')
        ParentFont = False
        TabOrder = 8
        WordWrap = False
        DesignSize = (
          338
          121)
      end
      object ALEditFirebirdDatabase: TALEdit
        Left = 106
        Top = 77
        Width = 249
        Height = 19
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 2
      end
      object ALButtonFirebirdLoopSelect: TALButton
        Left = 807
        Top = 120
        Width = 161
        Height = 25
        Caption = 'Loop SELECT via FireBird'
        TabOrder = 13
        OnClick = ALButtonFirebirdLoopSelectClick
        OnPaint = ALButtonPaint
      end
      object ALButtonFirebirdUpdate: TALButton
        Left = 807
        Top = 89
        Width = 161
        Height = 25
        Caption = 'Execute UPDATE via FireBird'
        TabOrder = 12
        OnClick = ALButtonFirebirdUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALButtonFirebirdLoopUpdate: TALButton
        Left = 807
        Top = 151
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE via FireBird'
        TabOrder = 14
        OnClick = ALButtonFirebirdLoopUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALEditFirebirdNBLoop: TALEdit
        Left = 894
        Top = 207
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 16
        Text = '1000000'
      end
      object ALEditFirebirdNbLoopBeforeCommit: TALEdit
        Left = 894
        Top = 232
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 17
        Text = '1'
      end
      object ALEditFirebirdNBThread: TALEdit
        Left = 894
        Top = 182
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 15
        Text = '1'
      end
      object ALEditFireBirdNum_buffers: TALEdit
        Left = 106
        Top = 175
        Width = 46
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 6
        Text = '0'
      end
      object ALButtonFirebirdCreateDatabase: TALButton
        Left = 807
        Top = 27
        Width = 161
        Height = 25
        Caption = 'Create Database via FireBird'
        TabOrder = 10
        OnClick = ALButtonFirebirdCreateDatabaseClick
        OnPaint = ALButtonPaint
      end
      object ALComboBoxFirebirdapiVer: TALComboBox
        Left = 106
        Top = 24
        Width = 145
        Height = 21
        OnPaint = ALComboBoxPaint
        Style = csDropDownList
        ItemIndex = 4
        TabOrder = 0
        Text = 'FB25'
        Items.Strings = (
          'FB102'
          'FB103'
          'FB15'
          'FB20'
          'FB25')
      end
      object ALMemoFireBirdParams: TALMemo
        Left = 432
        Top = 151
        Width = 338
        Height = 134
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 9
        WordWrap = False
        DesignSize = (
          338
          134)
      end
      object ALMemoFirebirdTPB: TALMemo
        Left = 106
        Top = 200
        Width = 249
        Height = 85
        Lines.Strings = (
          'isc_tpb_version3'
          'isc_tpb_write'
          'isc_tpb_read_committed'
          'isc_tpb_no_rec_version'
          'isc_tpb_nowait')
        TabOrder = 7
        WordWrap = False
        DesignSize = (
          249
          85)
      end
    end
    object MySQL: TTabSheet
      Caption = 'MySQL'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label3: TLabel
        Left = 50
        Top = 78
        Width = 63
        Height = 13
        Caption = 'Host Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TLabel
        Left = 81
        Top = 152
        Width = 32
        Height = 13
        Caption = 'Login'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label7: TLabel
        Left = 57
        Top = 177
        Width = 55
        Height = 13
        Caption = 'Password'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label8: TLabel
        Left = 42
        Top = 103
        Width = 71
        Height = 13
        Caption = 'Port Number'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label10: TLabel
        Left = 69
        Top = 202
        Width = 44
        Height = 13
        Caption = 'Charset'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label11: TLabel
        Left = 44
        Top = 53
        Width = 69
        Height = 13
        Caption = 'LibMySql dll'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 377
        Top = 26
        Width = 25
        Height = 13
        Caption = 'SQL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label12: TLabel
        Left = 58
        Top = 128
        Width = 55
        Height = 13
        Caption = 'Database'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label32: TLabel
        Left = 831
        Top = 186
        Width = 53
        Height = 13
        Caption = 'Nb Loop:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label33: TLabel
        Left = 804
        Top = 211
        Width = 80
        Height = 13
        Caption = 'Commit every:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label34: TLabel
        Left = 819
        Top = 161
        Width = 65
        Height = 13
        Caption = 'Nb Thread:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label40: TLabel
        Left = 15
        Top = 27
        Width = 98
        Height = 13
        Caption = 'LibMySql Version'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ALEditMySqlHost: TALEdit
        Left = 119
        Top = 75
        Width = 218
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 1
        Text = 'localhost'
      end
      object ALEditMySqlLogin: TALEdit
        Left = 119
        Top = 149
        Width = 218
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 4
        Text = 'root'
      end
      object ALEditMySqlPassword: TALEdit
        Left = 119
        Top = 174
        Width = 218
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 5
      end
      object ALEditMySqlPort: TALEdit
        Left = 119
        Top = 100
        Width = 88
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 2
        Text = '3306'
      end
      object ALEditMySqlCharset: TALEdit
        Left = 119
        Top = 199
        Width = 218
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 6
        Text = 'utf8'
      end
      object ALEditMysqlLib: TALEdit
        Left = 119
        Top = 50
        Width = 218
        Height = 19
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 0
        Text = 'libmysql.dll'
      end
      object ALMemoMySqlQuery: TALMemo
        Left = 408
        Top = 23
        Width = 338
        Height = 258
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '***********************************'
          'Exemple step 1 create the database: '
          '***********************************'
          ''
          'CREATE DATABASE TEST;'
          ''
          ''
          '********************************'
          'Exemple step 2 create the Table: '
          '********************************'
          ''
          'CREATE TABLE HASH('
          '  ID INT NOT NULL AUTO_INCREMENT PRIMARY KEY,'
          '  x1_y1 SMALLINT NOT NULL,'
          '  x1_y2 SMALLINT NOT NULL,'
          '  x1_y3 SMALLINT NOT NULL,'
          '  x1_y4 SMALLINT NOT NULL,'
          '  x1_y5 SMALLINT NOT NULL'
          ');'
          'CREATE INDEX HASH_X1_Y1_IDX ON HASH (X1_Y1);'
          'CREATE INDEX HASH_X1_Y2_IDX ON HASH (X1_Y2);'
          'CREATE INDEX HASH_X1_Y3_IDX ON HASH (X1_Y3);'
          'CREATE INDEX HASH_X1_Y4_IDX ON HASH (X1_Y4);'
          'CREATE INDEX HASH_X1_Y5_IDX ON HASH (X1_Y5);'
          ''
          ''
          '********************************'
          'Exemple step 3 add rows in loop:'
          '********************************'
          ''
          'INSERT INTO HASH('
          '  x1_y1,'
          '  x1_y2,'
          '  x1_y3,'
          '  x1_y4,'
          '  x1_y5'
          ') '
          'VALUES ('
          '  <#randomnumber min="0" max="255">,'
          '  <#randomnumber min="0" max="255">,'
          '  <#randomnumber min="0" max="255">,'
          '  <#randomnumber min="0" max="255">,'
          '  <#randomnumber min="0" max="255">'
          ');'
          ''
          ''
          '***********************************'
          'Exemple step 4 select rows in loop:'
          '***********************************'
          ''
          'Select '
          '  ID '
          'from '
          '  HASH '
          'where '
          '  x1_y1 >= <#randomnumber min="0" max="255" index="1"> and '
          '  x1_y1 <= <#randomnumber min="0" max="255" index="1"> + 20 and '
          '  x1_y2 >= <#randomnumber min="0" max="255" index="2"> and '
          '  x1_y2 <= <#randomnumber min="0" max="255" index="2"> + 20 and '
          '  x1_y3 >= <#randomnumber min="0" max="255" index="3"> and '
          '  x1_y3 <= <#randomnumber min="0" max="255" index="3"> + 20 and '
          '  x1_y4 >= <#randomnumber min="0" max="255" index="4"> and '
          '  x1_y4 <= <#randomnumber min="0" max="255" index="4"> + 20 and '
          '  x1_y5 >= <#randomnumber min="0" max="255" index="5"> and '
          '  x1_y5 <= <#randomnumber min="0" max="255" index="5"> + 20;')
        ParentFont = False
        TabOrder = 7
        WordWrap = False
        DesignSize = (
          338
          258)
      end
      object ALButtonMySQLSelect: TALButton
        Left = 803
        Top = 26
        Width = 161
        Height = 25
        Caption = 'Execute SELECT via MySql'
        TabOrder = 8
        OnClick = ALButtonMySqlSelectClick
        OnPaint = ALButtonPaint
      end
      object ALEditMySqlDatabaseName: TALEdit
        Left = 119
        Top = 125
        Width = 218
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 3
      end
      object ALEditMySqlNBLoop: TALEdit
        Left = 890
        Top = 183
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 13
        Text = '1000000'
      end
      object ALEditMySqlNbLoopBeforeCommit: TALEdit
        Left = 890
        Top = 208
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 14
        Text = '1'
      end
      object ALEditMySqlNBThread: TALEdit
        Left = 890
        Top = 158
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 12
        Text = '1'
      end
      object ALButtonMysqlUpdate: TALButton
        Left = 803
        Top = 57
        Width = 161
        Height = 25
        Caption = 'Execute UPDATE via MySql'
        TabOrder = 9
        OnClick = ALButtonMysqlUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALButtonMySqlLoopUpdate: TALButton
        Left = 803
        Top = 119
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE via MySql'
        TabOrder = 11
        OnClick = ALButtonMySqlLoopUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALButtonMysqlLoopSelect: TALButton
        Left = 803
        Top = 88
        Width = 161
        Height = 25
        Caption = 'Loop SELECT via MySql'
        TabOrder = 10
        OnClick = ALButtonMysqlLoopSelectClick
        OnPaint = ALButtonPaint
      end
      object ALComboBoxMySqlApiVer: TALComboBox
        Left = 119
        Top = 23
        Width = 145
        Height = 21
        OnPaint = ALComboBoxPaint
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 15
        Text = 'MYSQL55'
        Items.Strings = (
          'MYSQL50'
          'MYSQL55')
      end
    end
    object SQLLite3: TTabSheet
      Caption = 'SQLLite3'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label24: TLabel
        Left = 16
        Top = 26
        Width = 57
        Height = 13
        Caption = 'Sqlite3.dll'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label25: TLabel
        Left = 380
        Top = 26
        Width = 25
        Height = 13
        Caption = 'SQL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label19: TLabel
        Left = 23
        Top = 51
        Width = 55
        Height = 13
        Caption = 'Database'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label20: TLabel
        Left = 831
        Top = 178
        Width = 53
        Height = 13
        Caption = 'Nb Loop:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label21: TLabel
        Left = 91
        Top = 187
        Width = 65
        Height = 13
        Caption = 'cache_size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label22: TLabel
        Left = 223
        Top = 187
        Width = 58
        Height = 13
        Caption = 'page_size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label23: TLabel
        Left = 804
        Top = 203
        Width = 80
        Height = 13
        Caption = 'Commit every:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label28: TLabel
        Left = 819
        Top = 153
        Width = 65
        Height = 13
        Caption = 'Nb Thread:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ALEditSqlite3Lib: TALEdit
        Left = 84
        Top = 23
        Width = 249
        Height = 19
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 0
        Text = 'Sqlite3.dll'
      end
      object ALMemoSqlite3Query: TALMemo
        Left = 411
        Top = 23
        Width = 338
        Height = 258
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '*********************************** '
          'Exemple step 1 create the database:'
          '***********************************'
          ''
          'The database will be created automatiquelly '
          'on the First Create Table Statement '
          ''
          ''
          '********************************'
          'Exemple step 2 create the Table: '
          '********************************'
          ''
          'CREATE VIRTUAL TABLE HASH USING rtree('
          '  ID INTEGER PRIMARY KEY ASC,'
          '  x1_y1_min UNSIGNED SMALLINT, x1_y1_max UNSIGNED SMALLINT,'
          '  x1_y2_min UNSIGNED SMALLINT, x1_y2_max UNSIGNED SMALLINT,'
          '  x1_y3_min UNSIGNED SMALLINT, x1_y3_max UNSIGNED SMALLINT,'
          '  x1_y4_min UNSIGNED SMALLINT, x1_y4_max UNSIGNED SMALLINT,'
          '  x1_y5_min UNSIGNED SMALLINT, x1_y5_max UNSIGNED SMALLINT'
          ');'
          ''
          ''
          '********************************'
          'Exemple step 3 add rows in loop:'
          '********************************'
          ''
          'INSERT INTO HASH('
          '  ID,'
          '  x1_y1_min, x1_y1_max,'
          '  x1_y2_min, x1_y2_max,'
          '  x1_y3_min, x1_y3_max,'
          '  x1_y4_min, x1_y4_max,'
          '  x1_y5_min, x1_y5_max) '
          'VALUES ('
          '  null,'
          '  <#randomnumber min="0" max="255" index="1">,'
          '  <#randomnumber min="0" max="255" index="1"> + 20,'
          '  <#randomnumber min="0" max="255" index="2">,'
          '  <#randomnumber min="0" max="255" index="2"> + 20,'
          '  <#randomnumber min="0" max="255" index="3">,'
          '  <#randomnumber min="0" max="255" index="3"> + 20,'
          '  <#randomnumber min="0" max="255" index="4">,'
          '  <#randomnumber min="0" max="255" index="4"> + 20,'
          '  <#randomnumber min="0" max="255" index="5">,'
          '  <#randomnumber min="0" max="255" index="5"> + 20'
          ');'
          ''
          ''
          '***********************************'
          'Exemple step 4 select rows in loop:'
          'Note: take in account that when the '
          'File system start put in cache the '
          'database file, the speed can change'
          'a lot... Do some select in loop for '
          '10-20 min to see the difference'
          '***********************************'
          ''
          'Select '
          '  H1.ID '
          'from '
          '  HASH H1'
          'where '
          '  x1_y1_min >= <#randomnumber min="0" max="255" index="1"> and '
          '  x1_y1_max <= <#randomnumber min="0" max="255" index="1"> and '
          '  x1_y2_min >= <#randomnumber min="0" max="255" index="2"> and '
          '  x1_y2_max <= <#randomnumber min="0" max="255" index="2"> and '
          '  x1_y3_min >= <#randomnumber min="0" max="255" index="3"> and '
          '  x1_y3_max <= <#randomnumber min="0" max="255" index="3"> and '
          '  x1_y4_min >= <#randomnumber min="0" max="255" index="4"> and '
          '  x1_y4_max <= <#randomnumber min="0" max="255" index="4"> and '
          '  x1_y5_min >= <#randomnumber min="0" max="255" index="5"> and '
          '  x1_y5_max <= <#randomnumber min="0" max="255" index="5">;')
        ParentFont = False
        TabOrder = 9
        WordWrap = False
        DesignSize = (
          338
          258)
      end
      object ALButtonSqlLite3Select: TALButton
        Left = 803
        Top = 24
        Width = 161
        Height = 25
        Caption = 'Execute SELECT via SqlLite3'
        TabOrder = 10
        OnClick = ALButtonSqlLite3SelectClick
        OnPaint = ALButtonPaint
      end
      object ALEditSqlite3Database: TALEdit
        Left = 84
        Top = 48
        Width = 249
        Height = 19
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 1
      end
      object ALButtonSqlite3LoopSelect: TALButton
        Left = 803
        Top = 88
        Width = 161
        Height = 25
        Caption = 'Loop SELECTs via SqlLite3'
        TabOrder = 12
        OnClick = ALButtonSqlite3LoopSelectClick
        OnPaint = ALButtonPaint
      end
      object ALButtonSqlite3Update: TALButton
        Left = 803
        Top = 57
        Width = 161
        Height = 25
        Caption = 'Execute UPDATE via SqlLite3'
        TabOrder = 11
        OnClick = ALButtonSqlite3UpdateClick
        OnPaint = ALButtonPaint
      end
      object ALButtonSqlite3LoopUpdate: TALButton
        Left = 803
        Top = 119
        Width = 161
        Height = 25
        Caption = 'Loop UPDATEs via SqlLite3'
        TabOrder = 13
        OnClick = ALButtonSqlite3LoopUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALEditSQLite3NBLoop: TALEdit
        Left = 890
        Top = 175
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 15
        Text = '1000000'
      end
      object RadioGroupSqlite3Journal_Mode: TRadioGroup
        Left = 9
        Top = 73
        Width = 104
        Height = 105
        Caption = 'journal_mode'
        ItemIndex = 0
        Items.Strings = (
          'DELETE'
          'TRUNCATE '
          'PERSIST '
          'MEMORY '
          'WAL '
          'OFF')
        TabOrder = 2
      end
      object RadioGroupSQLite3Temp_Store: TRadioGroup
        Left = 119
        Top = 73
        Width = 104
        Height = 105
        Caption = 'temp_store'
        ItemIndex = 0
        Items.Strings = (
          'DEFAULT '
          'FILE '
          'MEMORY')
        TabOrder = 3
      end
      object RadioGroupSqlite3Synhcronous: TRadioGroup
        Left = 229
        Top = 73
        Width = 104
        Height = 105
        Caption = 'synchronous '
        ItemIndex = 2
        Items.Strings = (
          'OFF '
          'NORMAL '
          'FULL')
        TabOrder = 4
      end
      object ALEditSqlite3Cache_Size: TALEdit
        Left = 162
        Top = 184
        Width = 43
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 5
        Text = '2000'
      end
      object ALEditSqlite3Page_Size: TALEdit
        Left = 287
        Top = 184
        Width = 46
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 6
        Text = '1024'
      end
      object ALEditSQLite3NbLoopBeforeCommit: TALEdit
        Left = 890
        Top = 200
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 16
        Text = '1'
      end
      object ALEditSqlite3NBThread: TALEdit
        Left = 890
        Top = 150
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 14
        Text = '1'
      end
      object ALCheckBoxSqlite3SharedCache: TALCheckBox
        Left = 91
        Top = 207
        Width = 97
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Caption = 'shared cache'
        TabOrder = 7
        OnPaint = ALCheckBoxSqlite3SharedCachePaint
      end
      object ALCheckBoxSqlite3ReadUncommited: TALCheckBox
        Left = 208
        Top = 207
        Width = 124
        Height = 19
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        Caption = 'read uncommitted'
        TabOrder = 8
        OnPaint = ALCheckBoxSqlite3SharedCachePaint
      end
    end
    object Sphinx: TTabSheet
      Caption = 'Sphinx'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Label36: TLabel
        Left = 38
        Top = 54
        Width = 67
        Height = 13
        Caption = 'LibMysql dll'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label37: TLabel
        Left = 42
        Top = 80
        Width = 63
        Height = 13
        Caption = 'Host Name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label38: TLabel
        Left = 34
        Top = 105
        Width = 71
        Height = 13
        Caption = 'Port Number'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label43: TLabel
        Left = 377
        Top = 26
        Width = 25
        Height = 13
        Caption = 'SQL'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label44: TLabel
        Left = 819
        Top = 161
        Width = 65
        Height = 13
        Caption = 'Nb Thread:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label45: TLabel
        Left = 831
        Top = 186
        Width = 53
        Height = 13
        Caption = 'Nb Loop:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label46: TLabel
        Left = 804
        Top = 211
        Width = 80
        Height = 13
        Caption = 'Commit every:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label39: TLabel
        Left = 7
        Top = 29
        Width = 98
        Height = 13
        Caption = 'LibMySql Version'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ALEditSphinxLib: TALEdit
        Left = 111
        Top = 52
        Width = 221
        Height = 19
        OnButtonClick = ALEditButtonFindFileClick
        btnVisible = True
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 0
        Text = 'libmysql.dll'
      end
      object ALEditSphinxHost: TALEdit
        Left = 111
        Top = 77
        Width = 221
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 1
        Text = 'localhost'
      end
      object ALEditSphinxPort: TALEdit
        Left = 111
        Top = 102
        Width = 88
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 2
        Text = '9306'
      end
      object ALMemoSphinxQuery: TALMemo
        Left = 408
        Top = 23
        Width = 338
        Height = 258
        OnPaint = ALMemoPaint
        OnPaintScrollBar = ALMemoPaintScrollBar
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '************************************'
          'Exemple step 1 init the sphinx.conf: '
          '************************************'
          ''
          'index HASH'
          '{'
          '  type               = rt'
          '  path               = c:\sphinxdata\HASH'
          '  rt_mem_limit       = 32M'
          '  rt_field           = x1_y1'
          '  rt_field           = x1_y2'
          '  rt_field           = x1_y3'
          '  rt_field           = x1_y4'
          '  rt_field           = x1_y5'
          '  charset_type       = utf-8'
          '}'
          ''
          '********************************'
          'Exemple step 2 add rows in loop:'
          '********************************'
          ''
          'INSERT INTO HASH('
          '  ID,'
          '  x1_y1,'
          '  x1_y2,'
          '  x1_y3,'
          '  x1_y4,'
          '  x1_y5'
          ') '
          'VALUES ('
          '  <#incnumber>,'
          '  '#39'<#randomnumber min=0 max=235 index=1>'
          '   <#randomnumber min=0 max=235 index=1 incby=1>'
          '   <#randomnumber min=0 max=235 index=1 incby=2>'
          '   <#randomnumber min=0 max=235 index=1 incby=3>'
          '   <#randomnumber min=0 max=235 index=1 incby=4>'
          '   <#randomnumber min=0 max=235 index=1 incby=5>'
          '   <#randomnumber min=0 max=235 index=1 incby=6>'
          '   <#randomnumber min=0 max=235 index=1 incby=7>'
          '   <#randomnumber min=0 max=235 index=1 incby=8>'
          '   <#randomnumber min=0 max=235 index=1 incby=9>'
          '   <#randomnumber min=0 max=235 index=1 incby=10>'
          '   <#randomnumber min=0 max=235 index=1 incby=11>'
          '   <#randomnumber min=0 max=235 index=1 incby=12>'
          '   <#randomnumber min=0 max=235 index=1 incby=13>'
          '   <#randomnumber min=0 max=235 index=1 incby=14>'
          '   <#randomnumber min=0 max=235 index=1 incby=15>'
          '   <#randomnumber min=0 max=235 index=1 incby=16>'
          '   <#randomnumber min=0 max=235 index=1 incby=17>'
          '   <#randomnumber min=0 max=235 index=1 incby=18>'
          '   <#randomnumber min=0 max=235 index=1 incby=19>'
          '   <#randomnumber min=0 max=235 index=1 incby=20>'#39','
          ''
          '  '#39'<#randomnumber min=0 max=235 index=2>'
          '   <#randomnumber min=0 max=235 index=2 incby=1>'
          '   <#randomnumber min=0 max=235 index=2 incby=2>'
          '   <#randomnumber min=0 max=235 index=2 incby=3>'
          '   <#randomnumber min=0 max=235 index=2 incby=4>'
          '   <#randomnumber min=0 max=235 index=2 incby=5>'
          '   <#randomnumber min=0 max=235 index=2 incby=6>'
          '   <#randomnumber min=0 max=235 index=2 incby=7>'
          '   <#randomnumber min=0 max=235 index=2 incby=8>'
          '   <#randomnumber min=0 max=235 index=2 incby=9>'
          '   <#randomnumber min=0 max=235 index=2 incby=10>'
          '   <#randomnumber min=0 max=235 index=2 incby=11>'
          '   <#randomnumber min=0 max=235 index=2 incby=12>'
          '   <#randomnumber min=0 max=235 index=2 incby=13>'
          '   <#randomnumber min=0 max=235 index=2 incby=14>'
          '   <#randomnumber min=0 max=235 index=2 incby=15>'
          '   <#randomnumber min=0 max=235 index=2 incby=16>'
          '   <#randomnumber min=0 max=235 index=2 incby=17>'
          '   <#randomnumber min=0 max=235 index=2 incby=18>'
          '   <#randomnumber min=0 max=235 index=2 incby=19>'
          '   <#randomnumber min=0 max=235 index=2 incby=20>'#39','
          '   '
          '  '#39'<#randomnumber min=0 max=235 index=3>'
          '   <#randomnumber min=0 max=235 index=3 incby=1>'
          '   <#randomnumber min=0 max=235 index=3 incby=2>'
          '   <#randomnumber min=0 max=235 index=3 incby=3>'
          '   <#randomnumber min=0 max=235 index=3 incby=4>'
          '   <#randomnumber min=0 max=235 index=3 incby=5>'
          '   <#randomnumber min=0 max=235 index=3 incby=6>'
          '   <#randomnumber min=0 max=235 index=3 incby=7>'
          '   <#randomnumber min=0 max=235 index=3 incby=8>'
          '   <#randomnumber min=0 max=235 index=3 incby=9>'
          '   <#randomnumber min=0 max=235 index=3 incby=10>'
          '   <#randomnumber min=0 max=235 index=3 incby=11>'
          '   <#randomnumber min=0 max=235 index=3 incby=12>'
          '   <#randomnumber min=0 max=235 index=3 incby=13>'
          '   <#randomnumber min=0 max=235 index=3 incby=14>'
          '   <#randomnumber min=0 max=235 index=3 incby=15>'
          '   <#randomnumber min=0 max=235 index=3 incby=16>'
          '   <#randomnumber min=0 max=235 index=3 incby=17>'
          '   <#randomnumber min=0 max=235 index=3 incby=18>'
          '   <#randomnumber min=0 max=235 index=3 incby=19>'
          '   <#randomnumber min=0 max=235 index=3 incby=20>'#39','
          '   '
          '  '#39'<#randomnumber min=0 max=235 index=4>'
          '   <#randomnumber min=0 max=235 index=4 incby=1>'
          '   <#randomnumber min=0 max=235 index=4 incby=2>'
          '   <#randomnumber min=0 max=235 index=4 incby=3>'
          '   <#randomnumber min=0 max=235 index=4 incby=4>'
          '   <#randomnumber min=0 max=235 index=4 incby=5>'
          '   <#randomnumber min=0 max=235 index=4 incby=6>'
          '   <#randomnumber min=0 max=235 index=4 incby=7>'
          '   <#randomnumber min=0 max=235 index=4 incby=8>'
          '   <#randomnumber min=0 max=235 index=4 incby=9>'
          '   <#randomnumber min=0 max=235 index=4 incby=10>'
          '   <#randomnumber min=0 max=235 index=4 incby=11>'
          '   <#randomnumber min=0 max=235 index=4 incby=12>'
          '   <#randomnumber min=0 max=235 index=4 incby=13>'
          '   <#randomnumber min=0 max=235 index=4 incby=14>'
          '   <#randomnumber min=0 max=235 index=4 incby=15>'
          '   <#randomnumber min=0 max=235 index=4 incby=16>'
          '   <#randomnumber min=0 max=235 index=4 incby=17>'
          '   <#randomnumber min=0 max=235 index=4 incby=18>'
          '   <#randomnumber min=0 max=235 index=4 incby=19>'
          '   <#randomnumber min=0 max=235 index=4 incby=20>'#39','
          '   '
          '  '#39'<#randomnumber min=0 max=235 index=5>'
          '   <#randomnumber min=0 max=235 index=5 incby=1>'
          '   <#randomnumber min=0 max=235 index=5 incby=2>'
          '   <#randomnumber min=0 max=235 index=5 incby=3>'
          '   <#randomnumber min=0 max=235 index=5 incby=4>'
          '   <#randomnumber min=0 max=235 index=5 incby=5>'
          '   <#randomnumber min=0 max=235 index=5 incby=6>'
          '   <#randomnumber min=0 max=235 index=5 incby=7>'
          '   <#randomnumber min=0 max=235 index=5 incby=8>'
          '   <#randomnumber min=0 max=235 index=5 incby=9>'
          '   <#randomnumber min=0 max=235 index=5 incby=10>'
          '   <#randomnumber min=0 max=235 index=5 incby=11>'
          '   <#randomnumber min=0 max=235 index=5 incby=12>'
          '   <#randomnumber min=0 max=235 index=5 incby=13>'
          '   <#randomnumber min=0 max=235 index=5 incby=14>'
          '   <#randomnumber min=0 max=235 index=5 incby=15>'
          '   <#randomnumber min=0 max=235 index=5 incby=16>'
          '   <#randomnumber min=0 max=235 index=5 incby=17>'
          '   <#randomnumber min=0 max=235 index=5 incby=18>'
          '   <#randomnumber min=0 max=235 index=5 incby=19>'
          '   <#randomnumber min=0 max=235 index=5 incby=20>'#39
          '      '
          ');'
          ''
          '***********************************'
          'Exemple step 3 select rows in loop:'
          '***********************************'
          ''
          'Select '
          '  ID '
          'from '
          '  HASH '
          'where '
          '  match('#39'@x1_y1 <#randomnumber min="0" max="255"> '
          '         @x1_y2 <#randomnumber min="0" max="255">'
          '         @x1_y3 <#randomnumber min="0" max="255">'
          '         @x1_y4 <#randomnumber min="0" max="255">'
          '         @x1_y5 <#randomnumber min="0" max="255">'#39');')
        ParentFont = False
        TabOrder = 3
        WordWrap = False
        DesignSize = (
          338
          258)
      end
      object ALButtonSphinxSelect: TALButton
        Left = 803
        Top = 26
        Width = 161
        Height = 25
        Caption = 'Execute SELECT via Sphinx'
        TabOrder = 4
        OnClick = ALButtonSphinxSelectClick
        OnPaint = ALButtonPaint
      end
      object ALButtonSphinxUpdate: TALButton
        Left = 804
        Top = 57
        Width = 161
        Height = 25
        Caption = 'Execute UPDATE via Sphinx'
        TabOrder = 5
        OnClick = ALButtonSphinxUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALButtonSphinxLoopSelect: TALButton
        Left = 803
        Top = 88
        Width = 161
        Height = 25
        Caption = 'Loop SELECT via Sphinx'
        TabOrder = 6
        OnClick = ALButtonSphinxLoopSelectClick
        OnPaint = ALButtonPaint
      end
      object ALButtonSphinxLoopUpdate: TALButton
        Left = 803
        Top = 119
        Width = 161
        Height = 25
        Caption = 'Loop UPDATE via Sphinx'
        TabOrder = 7
        OnClick = ALButtonSphinxLoopUpdateClick
        OnPaint = ALButtonPaint
      end
      object ALEditSphinxNBThread: TALEdit
        Left = 890
        Top = 158
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 8
        Text = '1'
      end
      object ALEditSphinxNBLoop: TALEdit
        Left = 890
        Top = 183
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 9
        Text = '1000000'
      end
      object ALEditSphinxNbLoopBeforeCommit: TALEdit
        Left = 890
        Top = 208
        Width = 74
        Height = 19
        btnCaption = '...'
        GlyphIndex = 0
        btnFont.Charset = DEFAULT_CHARSET
        btnFont.Color = clWindowText
        btnFont.Height = -11
        btnFont.Name = 'MS Sans Serif'
        btnFont.Style = []
        OnPaint = ALEditPaint
        TabOrder = 10
        Text = '1'
      end
      object ALComboBoxSphinxApiVer: TALComboBox
        Left = 111
        Top = 25
        Width = 145
        Height = 21
        OnPaint = ALComboBoxPaint
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 11
        Text = 'MYSQL50'
        Items.Strings = (
          'MYSQL50'
          'MYSQL55')
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 944
    Top = 8
  end
end
