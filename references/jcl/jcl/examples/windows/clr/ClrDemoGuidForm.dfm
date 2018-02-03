object frmGuid: TfrmGuid
  Left = 339
  Top = 225
  BorderStyle = bsDialog
  Caption = 'Guid Stream'
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
  object lstGuids: TListView
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
        Caption = 'GUID'
        Width = 320
      end>
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = lstGuidsData
  end
end
