object MainLogView: TMainLogView
  Left = 359
  Top = 232
  Width = 860
  Height = 639
  Caption = ' Synopse LogView %s -'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 0
    Top = 549
    Width = 852
    Height = 4
    Cursor = crVSplit
    Align = alBottom
  end
  object Splitter3: TSplitter
    Left = 805
    Top = 0
    Width = 4
    Height = 549
    Visible = False
  end
  object Splitter1: TSplitter
    Left = 649
    Top = 0
    Width = 4
    Height = 549
    Visible = False
  end
  object Splitter4: TSplitter
    Left = 801
    Top = 0
    Width = 4
    Height = 549
    Visible = False
  end
  object PanelLeft: TPanel
    Left = 225
    Top = 0
    Width = 150
    Height = 549
    Align = alLeft
    Constraints.MinWidth = 150
    TabOrder = 0
    OnResize = PanelLeftResize
    DesignSize = (
      150
      549)
    object ImageLogo: TImage
      Left = 8
      Top = 501
      Width = 137
      Height = 32
      Anchors = [akLeft, akRight, akBottom]
      Center = True
      Picture.Data = {
        07544269746D617076090000424D760900000000000076000000280000008900
        0000200000000100040000000000000900000000000000000000100000000000
        00000504180004A6FC00444446000704C400918FB000D1D1DF001D26E0005454
        5600726FA000046EFC00B2B0CD0031323300FCFEFC005A53D1003731CF004446
        FC00CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCC555A5555CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCC54444CCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCA77778777785CCCCCCCCCCCCC5444ACCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCC533338C
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC58274A555555A877ACCCCCCCCCCCD3333
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCC
        CCCCCCCCCC63333CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCA774A472BBBB74AA82
        4CCCCCCCCCCD3333CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC000
        0000CCCCCCCCCCCCCCCCCCCCCCA3333ACCCCCCCCCCCCCCCCCCCCCCCCCCCCC428
        4700000000000244775CCCCCCCCD3333CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCC3333ECCCCCCCCCCCCCCCC
        CCCCCCCCCCCC4278B0000000000000078775CCCCCCCD3336CCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCC54E3333EACCCCCCCCC83333
        CCCCCCCC888D5CCCC488DACCCCC4277000000000000000002727CCCCCCCD3336
        CD63E4CCCCCCCAF333368CCCCCCCCC5863333FACCCCCC0000000CCCC83333333
        3334CCCCCCCE3333DCCCCCCC3333ACCCC3333FCCCCA277000000000000000000
        0B728CCCCCCD333333333365CCC53333333333FCCCCC5E3333333333ACCCC000
        0000CCCCD333333333334CCCCC5333333CCCCCCC3333ACCCC6333DCCCC748000
        000033333333000000287ACCCCCD333333333333CCC533333333333ECCC53333
        33333333ACCCC0000000CCCCD3EACCCC83333CCCCCD3333334CCCCCC3333ACCC
        C6333DCCC48AB0000033333333333000000847CCCCCD33335AAD3333DCC53345
        CCCA33335CC333338A5C5463ACCCC0000000CCCC4ACCCCCC43333CCCCC333333
        36CCCCCC3333ACCCC6333DCCC7AA000003333333333333000002A75CCCCD3336
        CCCCD33335C58CCCCCC533334CA3333ACCCCCCC85CCCC0000000CCCCCCC5A8E3
        33333CCCCA3333E3335CCCCC3333ACCCC6333DCC5BA700003333333333333330
        0000587CCCCD3333CCCC53333ACCCCCA8F333333AC8333FCCCCCCCCCCCCCC000
        0000CCCCCCD333333333FCCCC63338A333DCCCCC3333ACCCC6333DCC42400000
        33333333333333330000442CCCCD3333CCCCC33334CCC43333333333CCD3333F
        EEEEEEE68CCCC0000000CCCCC3333333333DCCCC533335C3333CCCCC3333ACCC
        C6333DCC227000033333333333333333000028B5CCCD3333CCCCC33338CC8333
        33333335CCD3333333333333FCCCC0000000CCCC53333333E45CCCCC83336CCE
        3334CCCC3333ACCCC3333DCCB77000033333333333333333000008B5CCCD3333
        CCCCC33334CC33333336D5CCCCD3333333333333DCCCC0000000CCCC43333ACC
        CCCCCCCC33334CCA3333CCCC3333ACCCC3333DCC07B000033333336193333333
        000007BACCCD3336CCCCA3333ACC33338CCCCCC5CCA333DCCCCC63334CCCC000
        0000CCCCA3333CCCCC538CCA3333CCCC33335CCC333338AAF33334CC07B00003
        3333336193333333000007BACCCD3333ACC5333335CC3333ACCCC5D3CCC33335
        CCCC33335CCCC0000000CCCCC333333633334CC3333ECCCCD3336CCC33333333
        333335CC077000033333633333333333000007BACCCD333333333333DCCCD333
        33633333CCCA3333F4D3333FCCCCC0000000CCCCC533333333338C53333ACCCC
        A3333CCC3333F3333333FCCCB7700B03333F6333336663330000B8B5CCCD3333
        63333333CCCCCE3333333333CCCC43333333333CCCCCC0000000CCCCCCC46333
        3368CC4366ECCCCCCE6634CCE6635CD33338CCCC828004003333333333336333
        00B077BCCCC8366DC83333DCCCCCCCAE333333FACCCCCCD3333338CCCCCCC000
        0000CCCCCCCCCC555CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC55CCCCCC5B4B0A20
        DD3333EFF333336B0040872CCCCCCCCCCCC55CCCCCCCCCCCCC55CCCCCCCCCCCC
        5555CCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCC7440BB00333333333333372088B42ACCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCA4AB00000333EEEEE3330000B0847CCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC4440000000384A48300000
        007445CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC000
        0000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC574800
        000B0000000BB0000244ACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCC474400085555555554B007487CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCC72A4B0028A5C5A470008A825CCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC77AA8000000000007AA
        8B5CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC000
        0000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCA7
        4A5A8BB0BB7455478CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCC5474A55555555487ACCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCA44AAAAA4445CCCCCCCCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC555555CCCCC
        CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC000
        0000}
      Transparent = True
      OnClick = ImageLogoClick
    end
    object lblServerRoot: TLabel
      Left = 16
      Top = 48
      Width = 62
      Height = 13
      Caption = 'Server Root:'
    end
    object lblServerPort: TLabel
      Left = 16
      Top = 90
      Width = 59
      Height = 13
      Caption = 'Server Port:'
    end
    object BtnBrowse: TButton
      Left = 16
      Top = 8
      Width = 107
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Select File'
      TabOrder = 0
      OnClick = BtnBrowseClick
    end
    object EventsList: TCheckListBox
      Left = 16
      Top = 72
      Width = 118
      Height = 105
      Hint = 'Selection of Events - right click for shortcuts'
      OnClickCheck = EventsListClickCheck
      ItemHeight = 13
      ParentShowHint = False
      PopupMenu = FilterMenu
      ShowHint = True
      Style = lbOwnerDrawFixed
      TabOrder = 3
      OnDblClick = EventsListDblClick
      OnDrawItem = EventsListDrawItem
    end
    object EditSearch: TEdit
      Left = 16
      Top = 40
      Width = 85
      Height = 21
      Hint = 'Search (Ctrl+F, F3 for next) '
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnChange = BtnSearchNextClick
    end
    object BtnSearchNext: TButton
      Left = 105
      Top = 38
      Width = 20
      Height = 23
      Hint = 'Search Next (F3)'
      Anchors = [akTop, akRight]
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BtnSearchNextClick
    end
    object BtnStats: TButton
      Left = 16
      Top = 304
      Width = 57
      Height = 25
      Caption = 'Stats'
      TabOrder = 6
      OnClick = BtnStatsClick
    end
    object BtnMapSearch: TButton
      Left = 80
      Top = 304
      Width = 59
      Height = 25
      Hint = 'Search for an address in a .map file'
      Caption = '.map'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = BtnMapSearchClick
    end
    object MergedProfile: TCheckBox
      Left = 22
      Top = 283
      Width = 121
      Height = 17
      Caption = 'Merge method calls'
      TabOrder = 5
      OnClick = MergedProfileClick
    end
    object ProfileGroup: TRadioGroup
      Left = 16
      Top = 192
      Width = 118
      Height = 89
      Caption = ' Methods profiler '
      ParentShowHint = False
      ShowHint = False
      TabOrder = 4
      OnClick = ProfileGroupClick
    end
    object ThreadGroup: TGroupBox
      Left = 16
      Top = 336
      Width = 118
      Height = 97
      Caption = ' Threads '
      TabOrder = 8
      object BtnThreadNext: TButton
        Left = 8
        Top = 13
        Width = 33
        Height = 25
        Hint = 'Goto Next Thread'
        Caption = 'Next'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = BtnThreadNextClick
      end
      object BtnThreadShow: TButton
        Left = 8
        Top = 39
        Width = 97
        Height = 25
        Caption = 'View threads'
        TabOrder = 1
        OnClick = BtnThreadShowClick
      end
      object btnThread0: TButton
        Left = 8
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Select No Thread'
        Caption = '0'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = BtnThreadClick
      end
      object btnThread1: TButton
        Left = 40
        Top = 66
        Width = 25
        Height = 25
        Hint = 'Select Only This Thread'
        Caption = '1'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = BtnThreadClick
      end
      object btnThreadAll: TButton
        Left = 72
        Top = 66
        Width = 33
        Height = 25
        Hint = 'Select All Threads'
        Caption = 'All'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = BtnThreadClick
      end
      object btnThreadDown: TButton
        Left = 56
        Top = 13
        Width = 25
        Height = 25
        Hint = 'Goto Next  Row'
        Caption = 'v'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        OnClick = btnThreadDownClick
      end
      object btnThreadUp: TButton
        Left = 80
        Top = 13
        Width = 25
        Height = 25
        Hint = 'Goto Previous Row'
        Caption = '^'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        OnClick = btnThreadUpClick
      end
    end
    object BtnSearchPrevious: TButton
      Left = 127
      Top = 38
      Width = 20
      Height = 23
      Hint = 'Search Previous (Shift F3)'
      Anchors = [akTop, akRight]
      Caption = '^'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
      OnClick = BtnSearchNextClick
    end
    object btnServerLaunch: TButton
      Left = 16
      Top = 132
      Width = 107
      Height = 25
      Hint = 'Lauch a HTTP server  for remote logging'
      Anchors = [akTop, akRight]
      Caption = 'Server Launch'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      OnClick = btnServerLaunchClick
    end
    object edtServerRoot: TEdit
      Left = 16
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 11
      Text = 'LogService'
    end
    object edtServerPort: TEdit
      Left = 16
      Top = 106
      Width = 121
      Height = 21
      TabOrder = 12
      Text = '8091'
    end
    object btnListClear: TButton
      Left = 16
      Top = 160
      Width = 105
      Height = 25
      Caption = 'Clear List'
      TabOrder = 13
      Visible = False
      OnClick = btnListClearClick
    end
    object btnListSave: TButton
      Left = 16
      Top = 192
      Width = 105
      Height = 25
      Caption = 'Save List'
      TabOrder = 14
      Visible = False
      OnClick = btnListSaveClick
    end
    object lstDays: TListBox
      Left = 16
      Top = 440
      Width = 121
      Height = 57
      ItemHeight = 13
      TabOrder = 15
      OnDblClick = lstDaysDblClick
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 553
    Width = 852
    Height = 52
    Align = alBottom
    TabOrder = 3
    OnResize = PanelBottomResize
  end
  object List: TDrawGrid
    Left = 809
    Top = 0
    Width = 43
    Height = 549
    Align = alClient
    ColCount = 3
    DefaultColWidth = 100
    DefaultRowHeight = 14
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect, goThumbTracking]
    PopupMenu = ListMenu
    TabOrder = 2
    Visible = False
    OnClick = ListClick
    OnDblClick = ListDblClick
    OnDrawCell = ListDrawCell
  end
  object ProfileList: TDrawGrid
    Left = 375
    Top = 0
    Width = 274
    Height = 549
    Align = alLeft
    ColCount = 2
    DefaultColWidth = 100
    DefaultRowHeight = 14
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goThumbTracking]
    TabOrder = 1
    Visible = False
    OnClick = ProfileListClick
    OnDrawCell = ProfileListDrawCell
  end
  object PanelThread: TPanel
    Left = 653
    Top = 0
    Width = 148
    Height = 549
    Align = alLeft
    TabOrder = 4
    Visible = False
    object ThreadListBox: TCheckListBox
      Left = 1
      Top = 1
      Width = 146
      Height = 507
      OnClickCheck = ThreadListBoxClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ThreadListBoxClick
      OnDblClick = ThreadListBoxDblClick
    end
    object pnlThreadBottom: TPanel
      Left = 1
      Top = 508
      Width = 146
      Height = 40
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        146
        40)
      object lblThreadName: TLabel
        Left = 3
        Top = 3
        Width = 141
        Height = 33
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
  end
  object PanelBrowse: TPanel
    Left = 0
    Top = 0
    Width = 225
    Height = 549
    Align = alLeft
    Constraints.MinWidth = 80
    TabOrder = 5
    Visible = False
    DesignSize = (
      225
      549)
    object Drive: TDriveComboBox
      Left = 8
      Top = 8
      Width = 210
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      DirList = Directory
      TabOrder = 0
    end
    object Directory: TDirectoryListBox
      Left = 8
      Top = 33
      Width = 210
      Height = 220
      Anchors = [akLeft, akTop, akRight]
      FileList = Files
      ItemHeight = 16
      TabOrder = 1
    end
    object Files: TFileListBox
      Left = 8
      Top = 264
      Width = 210
      Height = 268
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      Mask = '*.log;*.synlz;*.txt'
      TabOrder = 2
      OnClick = FilesClick
    end
  end
  object FilterMenu: TPopupMenu
    Left = 320
    Top = 136
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.log'
    Filter = 'Log|*.log;*.txt;*.synlz'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 344
    Top = 72
  end
  object ListMenu: TPopupMenu
    Left = 816
    Top = 120
    object ListMenuCopy: TMenuItem
      Caption = '&Copy'
      OnClick = ListMenuCopyClick
    end
  end
  object tmrRefresh: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrRefreshTimer
    Left = 345
    Top = 336
  end
  object dlgSaveList: TSaveDialog
    DefaultExt = '.log'
    Filter = 'log|*.log|txt|*.txt|synlz|*.synlz'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 345
    Top = 200
  end
end
