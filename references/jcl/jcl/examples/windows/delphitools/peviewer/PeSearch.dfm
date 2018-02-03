object PeSearchChild: TPeSearchChild
  Left = 259
  Top = 176
  AutoScroll = False
  Caption = 'Search function'
  ClientHeight = 265
  ClientWidth = 397
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ProcessLabel: TLabel
    Left = 8
    Top = 80
    Width = 64
    Height = 13
    Caption = 'ProcessLabel'
  end
  object Bevel1: TBevel
    Left = 0
    Top = 56
    Width = 398
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object CountLabel: TLabel
    Left = 334
    Top = 81
    Width = 54
    Height = 13
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'CountLabel'
  end
  object Label1: TLabel
    Left = 6
    Top = 12
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = FuncNameEdit
  end
  object Label2: TLabel
    Left = 6
    Top = 44
    Width = 25
    Height = 13
    Caption = '&Path:'
    FocusControl = PathEdit
  end
  object FuncNameEdit: TEdit
    Left = 40
    Top = 8
    Width = 155
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    CharCase = ecUpperCase
    TabOrder = 0
    OnChange = FuncNameEditChange
  end
  object ResultListView: TListView
    Left = 0
    Top = 104
    Width = 397
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 90
      end
      item
        Caption = 'Filename'
        Width = 300
      end>
    ColumnClick = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ResultListViewDblClick
  end
  object StartBtn: TButton
    Left = 318
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 2
    OnClick = StartBtnClick
  end
  object StopBtn: TButton
    Left = 318
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = StopBtnClick
  end
  object PathEdit: TEdit
    Left = 40
    Top = 40
    Width = 251
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    CharCase = ecUpperCase
    TabOrder = 4
    OnChange = FuncNameEditChange
  end
  object SelectDirBtn: TButton
    Left = 295
    Top = 40
    Width = 13
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 5
    OnClick = SelectDirBtnClick
  end
  object ExportCheckBox: TCheckBox
    Left = 203
    Top = 8
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '&Export'
    TabOrder = 6
    OnClick = FuncNameEditChange
  end
  object ImportCheckBox: TCheckBox
    Left = 260
    Top = 8
    Width = 49
    Height = 17
    Anchors = [akTop, akRight]
    Caption = '&Import'
    TabOrder = 7
    OnClick = FuncNameEditChange
  end
end
