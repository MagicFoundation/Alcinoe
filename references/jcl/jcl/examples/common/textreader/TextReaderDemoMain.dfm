object MainForm: TMainForm
  Left = 275
  Top = 163
  ClientWidth = 763
  ClientHeight = 605
  Caption = 'TJclMappedTextReader class demo'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
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
  object ReadLnLabel: TLabel
    Left = 200
    Top = 16
    Width = 64
    Height = 13
    Caption = 'ReadLnLabel'
  end
  object TextListView: TListView
    Left = 0
    Top = 40
    Width = 763
    Height = 555
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Text'
        Width = 750
      end>
    ColumnClick = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    HotTrackStyles = []
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 0
    ViewStyle = vsReport
    OnData = TextListViewData
  end
  object OpenBtn: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open file'
    TabOrder = 1
    OnClick = OpenBtnClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 595
    Width = 763
    Height = 19
    Panels = <
      item
        Width = 210
      end
      item
        Width = 210
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ReadLnBtn: TButton
    Left = 112
    Top = 8
    Width = 75
    Height = 25
    Caption = 'ReadLn test'
    TabOrder = 3
    OnClick = ReadLnBtnClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 552
  end
end
