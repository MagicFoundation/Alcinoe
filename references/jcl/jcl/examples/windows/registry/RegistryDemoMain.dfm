object Form1: TForm1
  Left = 211
  Top = 136
  ClientWidth = 641
  ClientHeight = 448
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 225
    Top = 0
    Width = 3
    Height = 447
    Cursor = crHSplit
  end
  object tvKeys: TTreeView
    Left = 0
    Top = 0
    Width = 225
    Height = 447
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnChange = tvKeysChange
    OnExpanding = tvKeysExpanding
  end
  object lvValues: TListView
    Left = 228
    Top = 0
    Width = 413
    Height = 447
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Caption = 'Value'
        Width = 200
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
end
