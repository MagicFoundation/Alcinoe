object MainForm: TMainForm
  Left = 199
  Top = 112
  ClientWidth = 632
  ClientHeight = 571
  Caption = 'JclLocales demo'
  Color = clBtnFace
  Constraints.MinHeight = 570
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 246
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Day names'
  end
  object Label2: TLabel
    Left = 8
    Top = 374
    Width = 64
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Month names'
  end
  object Label3: TLabel
    Left = 141
    Top = 246
    Width = 103
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Date and time formats'
  end
  object Bevel1: TBevel
    Left = 312
    Top = 200
    Width = 17
    Height = 363
    Anchors = [akLeft, akBottom]
    Shape = bsLeftLine
  end
  object Label4: TLabel
    Left = 320
    Top = 206
    Width = 81
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Keyboard layouts'
  end
  object Label5: TLabel
    Left = 320
    Top = 363
    Width = 126
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Available keyboard layouts'
  end
  object Label6: TLabel
    Left = 141
    Top = 504
    Width = 47
    Height = 13
    Caption = 'Calendars'
  end
  object LocalesListView: TListView
    Left = 0
    Top = 0
    Width = 632
    Height = 195
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Country'
        Width = 120
      end
      item
        Caption = 'LCID'
      end
      item
        Caption = 'LangName'
        Width = 130
      end
      item
        Caption = 'Lng'
        Width = 40
      end
      item
        Caption = 'CP'
      end
      item
        Caption = '$ Local'
      end
      item
        Caption = '$ Intl.'
      end
      item
        Caption = 'Code'
      end>
    ColumnClick = False
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawSubItem = LocalesListViewCustomDrawSubItem
    OnSelectItem = LocalesListViewSelectItem
  end
  object LocalesRadioGroup: TRadioGroup
    Left = 9
    Top = 206
    Width = 296
    Height = 37
    Anchors = [akLeft, akBottom]
    Caption = '&Locales'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '&Supported'
      '&Installed')
    TabOrder = 1
    OnClick = LocalesRadioGroupClick
  end
  object DayNamesListBox: TListBox
    Left = 8
    Top = 262
    Width = 121
    Height = 105
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 2
  end
  object MonthNamesListBox: TListBox
    Left = 8
    Top = 389
    Width = 121
    Height = 174
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 3
  end
  object FormatsListBox: TListBox
    Left = 141
    Top = 262
    Width = 163
    Height = 235
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 4
  end
  object KeyblayoutsListBox: TListBox
    Left = 320
    Top = 222
    Width = 305
    Height = 121
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
    OnClick = KeyblayoutsListBoxClick
    OnDblClick = ActivateBtnClick
  end
  object ActivateBtn: TButton
    Left = 512
    Top = 202
    Width = 57
    Height = 19
    Anchors = [akRight, akBottom]
    Caption = 'Activate'
    Enabled = False
    TabOrder = 7
    OnClick = ActivateBtnClick
  end
  object AvailableLayoutsListView: TListView
    Left = 320
    Top = 379
    Width = 305
    Height = 185
    Anchors = [akLeft, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 140
      end
      item
        Caption = 'Identifier'
        Width = 70
      end
      item
        Caption = 'ID'
        Width = 40
      end
      item
        Caption = 'File'
        Width = 90
      end>
    ColumnClick = False
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 9
    ViewStyle = vsReport
    OnChange = AvailableLayoutsListViewChange
    OnCustomDrawItem = AvailableLayoutsListViewCustomDrawItem
  end
  object LoadBtn: TButton
    Left = 576
    Top = 355
    Width = 49
    Height = 19
    Anchors = [akRight, akBottom]
    Caption = 'Load'
    Enabled = False
    TabOrder = 10
    OnClick = LoadBtnClick
  end
  object UnloadBtn: TButton
    Left = 576
    Top = 202
    Width = 49
    Height = 19
    Anchors = [akRight, akBottom]
    Caption = 'Unload'
    Enabled = False
    TabOrder = 11
    OnClick = UnloadBtnClick
  end
  object PrevBtn: TButton
    Left = 407
    Top = 202
    Width = 42
    Height = 19
    Anchors = [akRight, akBottom]
    Caption = 'Prev'
    TabOrder = 6
    OnClick = PrevBtnClick
  end
  object NextBtn: TButton
    Left = 455
    Top = 202
    Width = 42
    Height = 19
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    TabOrder = 8
    OnClick = NextBtnClick
  end
  object CalendarsListBox: TListBox
    Left = 141
    Top = 520
    Width = 163
    Height = 41
    Style = lbOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 12
  end
end
