object frmFreedObject: TfrmFreedObject
  Left = 0
  Top = 0
  Width = 415
  Height = 240
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 415
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 95
      Height = 13
      Caption = 'Freed Object Class:'
    end
    object Label2: TLabel
      Left = 3
      Top = 19
      Width = 89
      Height = 13
      Caption = 'Allocation number:'
    end
    object Label3: TLabel
      Left = 227
      Top = 3
      Width = 73
      Height = 13
      Caption = 'Virtual Method:'
    end
    object Label4: TLabel
      Left = 227
      Top = 19
      Width = 115
      Height = 13
      Caption = 'Virtual Method Address:'
    end
    object lbVM: TLabel
      Left = 348
      Top = 3
      Width = 22
      Height = 13
      Caption = 'lbVM'
    end
    object lbVMAddr: TLabel
      Left = 348
      Top = 19
      Width = 45
      Height = 13
      Caption = 'lbVMAddr'
    end
    object lbFreedObjectClass: TLabel
      Left = 104
      Top = 3
      Width = 27
      Height = 13
      Caption = 'lbSize'
    end
    object lbAllocationNumber: TLabel
      Left = 104
      Top = 19
      Width = 91
      Height = 13
      Caption = 'lbAllocationNumber'
    end
  end
  object pg: TPageControl
    Left = 0
    Top = 41
    Width = 415
    Height = 199
    ActivePage = tsStack1
    Align = alClient
    TabOrder = 1
    object tsStack1: TTabSheet
      Caption = 'Stack (allocated by)'
    end
    object tsStack2: TTabSheet
      Caption = 'Stack (freed by)'
      ImageIndex = 1
    end
    object tsStack3: TTabSheet
      Caption = 'Stack (current)'
      ImageIndex = 2
    end
    object tsMemory: TTabSheet
      Caption = 'Memory Dump'
      ImageIndex = 3
    end
  end
end
