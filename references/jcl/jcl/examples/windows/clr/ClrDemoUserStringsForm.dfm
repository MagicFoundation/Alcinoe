object frmUserStrings: TfrmUserStrings
  Left = 299
  Top = 296
  BorderStyle = bsDialog
  Caption = 'User String Stream'
  ClientHeight = 273
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lstStrings: TListView
    Left = 8
    Top = 8
    Width = 377
    Height = 225
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Offset'
        Width = 64
      end
      item
        Caption = 'String'
        Width = 315
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lstStringsData
  end
  object btnOK: TBitBtn
    Left = 158
    Top = 240
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
