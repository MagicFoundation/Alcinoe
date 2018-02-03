object frmMemory: TfrmMemory
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lbMemoryAddr: TLabel
      Left = 52
      Top = 3
      Width = 69
      Height = 13
      Caption = 'lbMemoryAddr'
    end
    object Label6: TLabel
      Left = 3
      Top = 3
      Width = 43
      Height = 13
      Caption = 'Address:'
    end
  end
  object sgMemory: TStringGrid
    Left = 0
    Top = 17
    Width = 320
    Height = 223
    Align = alClient
    ColCount = 32
    DefaultColWidth = 18
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 16
    FixedRows = 0
    TabOrder = 1
  end
end
