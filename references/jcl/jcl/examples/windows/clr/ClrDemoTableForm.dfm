object frmTable: TfrmTable
  Left = 384
  Top = 245
  BorderStyle = bsDialog
  Caption = 'Table Stream'
  ClientHeight = 453
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblVer: TLabel
    Left = 8
    Top = 8
    Width = 119
    Height = 13
    Caption = 'Table Schemata Version:'
  end
  object edtVer: TEdit
    Left = 136
    Top = 4
    Width = 57
    Height = 21
    Color = clInactiveBorder
    ReadOnly = True
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 278
    Top = 424
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object lstTables: TListView
    Left = 8
    Top = 32
    Width = 617
    Height = 193
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Alignment = taCenter
        Caption = 'Rows'
        Width = 40
      end
      item
        Alignment = taCenter
        Caption = 'Offset'
        Width = 76
      end
      item
        Caption = 'Size'
        Width = 40
      end
      item
        Caption = 'Type'
        Width = 200
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnSelectItem = lstTablesSelectItem
  end
  object memDump: TMemo
    Left = 8
    Top = 232
    Width = 617
    Height = 185
    Color = clInactiveBorder
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Fixedsys'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnDumpIL: TButton
    Left = 200
    Top = 4
    Width = 75
    Height = 25
    Caption = 'Dump IL'
    TabOrder = 4
    OnClick = btnDumpILClick
  end
end
