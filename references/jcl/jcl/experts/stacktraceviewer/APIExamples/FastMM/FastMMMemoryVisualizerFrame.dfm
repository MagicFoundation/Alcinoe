object frmMemoryVisualizer: TfrmMemoryVisualizer
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object memString: TMemo
    Left = 0
    Top = 33
    Width = 320
    Height = 207
    Align = alClient
    TabOrder = 0
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label4: TLabel
      Left = 131
      Top = 19
      Width = 37
      Height = 13
      Caption = 'Length:'
    end
    object Label1: TLabel
      Left = 4
      Top = 3
      Width = 53
      Height = 13
      Caption = 'CodePage:'
    end
    object Label2: TLabel
      Left = 4
      Top = 19
      Width = 45
      Height = 13
      Caption = 'ElemSize:'
    end
    object lbCodePage: TLabel
      Left = 65
      Top = 3
      Width = 57
      Height = 13
      Caption = 'lbCodePage'
    end
    object Label3: TLabel
      Left = 131
      Top = 3
      Width = 50
      Height = 13
      Caption = 'RefCount:'
    end
    object lbRefCount: TLabel
      Left = 186
      Top = 3
      Width = 54
      Height = 13
      Caption = 'lbRefCount'
    end
    object lbLength: TLabel
      Left = 186
      Top = 19
      Width = 41
      Height = 13
      Caption = 'lbLength'
    end
    object lbElemSize: TLabel
      Left = 65
      Top = 19
      Width = 49
      Height = 13
      Caption = 'lbElemSize'
    end
  end
end
