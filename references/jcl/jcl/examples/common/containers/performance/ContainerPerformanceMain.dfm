object MainForm: TMainForm
  Left = 402
  Top = 120
  Caption = 'Container Performance'
  ClientHeight = 294
  ClientWidth = 569
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListPerformanceGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 569
    Height = 173
    Align = alClient
    DefaultColWidth = 100
    RowCount = 6
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
  end
  object HashPerformanceGrid: TStringGrid
    Left = 0
    Top = 173
    Width = 569
    Height = 121
    Align = alBottom
    ColCount = 6
    DefaultColWidth = 90
    RowCount = 4
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 216
    object FileMenu: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object TestMenu: TMenuItem
      Caption = '&Test'
      object mnList: TMenuItem
        Caption = 'TList'
        OnClick = mnListClick
      end
      object mnJclArrayList: TMenuItem
        Caption = 'TJclArrayList'
        OnClick = mnJclArrayListClick
      end
      object mnJclLinkedList: TMenuItem
        Caption = 'TJclLinkedList'
        OnClick = mnJclLinkedListClick
      end
      object mnJclVector: TMenuItem
        Caption = 'TJclVector'
        OnClick = mnJclVectorClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnBucketList: TMenuItem
        Caption = 'TBucketList'
        OnClick = mnBucketListClick
      end
      object mnJclHashMap: TMenuItem
        Caption = 'TJclHashMap'
        OnClick = mnJclHashMapClick
      end
      object mnHashedStringList: TMenuItem
        Caption = 'THashedStringList'
        OnClick = mnHashedStringListClick
      end
      object mnJclAnsiStrAnsiStrHashMap: TMenuItem
        Caption = 'TJclAnsiStrAnsiStrHashMap'
        OnClick = mnJclAnsiStrAnsiStrHashMapClick
      end
      object mnJclWideStrWideStrHashMap: TMenuItem
        Caption = 'TJclWideStrWideStrHashMap'
        OnClick = mnJclWideStrWideStrHashMapClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnAllTest: TMenuItem
        Caption = 'All'
        OnClick = mnAllTestClick
      end
    end
  end
end
