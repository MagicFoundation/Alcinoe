object LogFrame: TLogFrame
  Left = 0
  Top = 0
  Width = 516
  Height = 367
  TabOrder = 0
  object spl2: TSplitter
    Left = 0
    Top = 275
    Width = 516
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 275
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      145
      275)
    object lblExistingLogKB: TLabel
      Left = 12
      Top = 34
      Width = 56
      Height = 13
      Caption = 'Existing KB:'
    end
    object edtSearch: TEdit
      Left = 5
      Top = 8
      Width = 98
      Height = 21
      Hint = 'Search (Ctrl+F, F3 for next) '
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Visible = False
      OnChange = btnSearchNextClick
    end
    object chklstEvents: TCheckListBox
      Left = 8
      Top = 56
      Width = 129
      Height = 105
      OnClickCheck = chklstEventsClickCheck
      ItemHeight = 13
      PopupMenu = pmFilter
      Style = lbOwnerDrawFixed
      TabOrder = 3
      OnDblClick = chklstEventsDblClick
      OnDrawItem = chklstEventsDrawItem
    end
    object btnStartLog: TButton
      Left = 16
      Top = 6
      Width = 113
      Height = 25
      Caption = 'Start Logging'
      TabOrder = 4
      OnClick = btnStartLogClick
    end
    object edtExistingLogKB: TEdit
      Left = 72
      Top = 32
      Width = 57
      Height = 21
      Hint = 'How many KB of log text should be transmitted at Start'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Text = '512'
    end
    object btnStopLog: TButton
      Left = 16
      Top = 168
      Width = 113
      Height = 25
      Caption = 'Stop Logging'
      TabOrder = 6
      Visible = False
      OnClick = btnStopLogClick
    end
    object BtnSearchNext: TButton
      Left = 103
      Top = 6
      Width = 20
      Height = 23
      Hint = 'Search Next (F3)'
      Anchors = [akTop, akRight]
      Caption = '?'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Visible = False
      OnClick = btnSearchNextClick
    end
    object BtnSearchPrevious: TButton
      Left = 123
      Top = 6
      Width = 20
      Height = 23
      Hint = 'Search Previous (Shift F3)'
      Anchors = [akTop, akRight]
      Caption = '^'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Visible = False
      OnClick = btnSearchNextClick
    end
  end
  object pnlRight: TPanel
    Left = 145
    Top = 0
    Width = 371
    Height = 275
    Align = alClient
    TabOrder = 1
    object spl1: TSplitter
      Left = 1
      Top = 1
      Height = 273
    end
    object drwgrdEvents: TDrawGrid
      Left = 4
      Top = 1
      Width = 366
      Height = 273
      Align = alClient
      ColCount = 3
      DefaultColWidth = 100
      DefaultRowHeight = 14
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowSelect, goThumbTracking]
      TabOrder = 0
      Visible = False
      OnClick = drwgrdEventsClick
      OnDblClick = drwgrdEventsDblClick
      OnDrawCell = drwgrdEventsDrawCell
    end
  end
  object mmoBottom: TMemo
    Left = 0
    Top = 278
    Width = 516
    Height = 89
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object pmFilter: TPopupMenu
    Left = 96
    Top = 112
  end
  object tmrRefresh: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrRefreshTimer
    Left = 153
    Top = 32
  end
end
