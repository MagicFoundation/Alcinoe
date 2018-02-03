object frmLeakGroup: TfrmLeakGroup
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 58
      Height = 13
      Caption = 'Leak Count:'
    end
    object Label2: TLabel
      Left = 3
      Top = 19
      Width = 48
      Height = 13
      Caption = 'Leak Size:'
    end
    object lbLeakCount: TLabel
      Left = 67
      Top = 3
      Width = 59
      Height = 13
      Caption = 'lbLeakCount'
    end
    object lbLeakSize: TLabel
      Left = 67
      Top = 19
      Width = 49
      Height = 13
      Caption = 'lbLeakSize'
    end
  end
end
