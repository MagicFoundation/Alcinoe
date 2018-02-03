object Form1: TForm1
  Left = 192
  Top = 107
  Width = 783
  Height = 540
  Caption = 'JclMapi messages reading example'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
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
  object Splitter1: TSplitter
    Left = 0
    Top = 177
    Width = 775
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
  end
  object HeadersListView: TListView
    Left = 0
    Top = 0
    Width = 775
    Height = 177
    Align = alTop
    Columns = <
      item
        Caption = 'From'
        Width = 180
      end
      item
        Caption = 'Subject'
        Width = 300
      end
      item
        Caption = 'Received'
        Width = 130
      end
      item
        Caption = 'MsgID'
        Width = 60
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = HeadersListViewCustomDrawItem
    OnSelectItem = HeadersListViewSelectItem
  end
  object PreviewRichEdit: TRichEdit
    Left = 0
    Top = 180
    Width = 775
    Height = 292
    Align = alClient
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'PreviewRichEdit')
    ParentFont = False
    PlainText = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object AttachmentsListBox: TListBox
    Left = 0
    Top = 472
    Width = 775
    Height = 41
    Align = alBottom
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 2
    Visible = False
  end
end
