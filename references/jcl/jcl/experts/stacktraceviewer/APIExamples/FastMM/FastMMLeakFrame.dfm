object frmLeak: TfrmLeak
  Left = 0
  Top = 0
  Width = 495
  Height = 240
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 495
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 55
      Height = 13
      Caption = 'Timestamp:'
    end
    object Label2: TLabel
      Left = 163
      Top = 3
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label3: TLabel
      Left = 163
      Top = 19
      Width = 38
      Height = 13
      Caption = 'Thread:'
    end
    object lbTimestamp: TLabel
      Left = 64
      Top = 3
      Width = 59
      Height = 13
      Caption = 'lbTimestamp'
    end
    object lbSize: TLabel
      Left = 224
      Top = 3
      Width = 27
      Height = 13
      Caption = 'lbSize'
    end
    object lbThread: TLabel
      Left = 224
      Top = 19
      Width = 42
      Height = 13
      Caption = 'lbThread'
    end
    object Label4: TLabel
      Left = 320
      Top = 3
      Width = 29
      Height = 13
      Caption = 'Class:'
    end
    object Label5: TLabel
      Left = 320
      Top = 19
      Width = 89
      Height = 13
      Caption = 'Allocation number:'
    end
    object lbClass: TLabel
      Left = 415
      Top = 3
      Width = 33
      Height = 13
      Caption = 'lbClass'
    end
    object lbAllocationNumber: TLabel
      Left = 415
      Top = 19
      Width = 91
      Height = 13
      Caption = 'lbAllocationNumber'
    end
  end
  object pg: TPageControl
    Left = 0
    Top = 33
    Width = 495
    Height = 207
    ActivePage = tsStack
    Align = alClient
    TabOrder = 1
    object tsStack: TTabSheet
      Caption = 'Stack'
      ImageIndex = 1
    end
    object tsMemory: TTabSheet
      Caption = 'Memory dump'
    end
    object tsMemoryVisualized: TTabSheet
      Caption = 'Memory visualized'
      ImageIndex = 2
    end
  end
end
