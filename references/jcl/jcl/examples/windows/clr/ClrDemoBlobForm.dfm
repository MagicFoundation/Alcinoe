object frmBlobs: TfrmBlobs
  Left = 414
  Top = 406
  BorderStyle = bsDialog
  Caption = 'Blob Stream'
  ClientHeight = 273
  ClientWidth = 392
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
  object btnOK: TBitBtn
    Left = 159
    Top = 240
    Width = 75
    Height = 25
    TabOrder = 0
    Kind = bkOK
  end
  object lstBlobs: TListView
    Left = 8
    Top = 8
    Width = 377
    Height = 105
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Alignment = taCenter
        Caption = 'Offset'
        Width = 80
      end
      item
        Caption = 'Size'
        Width = 64
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = lstBlobsData
    OnSelectItem = lstBlobsSelectItem
  end
  object memDump: TMemo
    Left = 8
    Top = 120
    Width = 377
    Height = 113
    Color = clInactiveBorder
    Font.Charset = GB2312_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Fixedsys'
    Font.Style = []
    ImeName = #32043#20809#25340#38899#36755#20837#27861'2.2'#29256
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
