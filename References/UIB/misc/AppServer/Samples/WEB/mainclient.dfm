object MainForm: TMainForm
  Left = 430
  Top = 374
  Width = 434
  Height = 320
  Caption = 'client test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    426
    293)
  PixelsPerInch = 96
  TextHeight = 13
  object getdata: TButton
    Left = 0
    Top = 0
    Width = 75
    Height = 25
    Caption = 'getdata'
    TabOrder = 0
    OnClick = getdataClick
  end
  object Grid: TStringGrid
    Left = 0
    Top = 31
    Width = 425
    Height = 258
    Anchors = [akLeft, akTop, akRight, akBottom]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 1
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object table: TEdit
    Left = 80
    Top = 0
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'employee'
  end
end
