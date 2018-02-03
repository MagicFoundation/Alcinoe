inherited frmStackView: TfrmStackView
  Width = 364
  Height = 358
  Caption = 'Stack Traces'
  KeyPreview = True
  PopupMenu = nil
  ShowHint = False
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter1: TSplitter
    Width = 356
    Constraints.MinHeight = 3
  end
  inherited ToolBar1: TToolBar
    Width = 356
    ParentShowHint = False
    ShowHint = False
    object ToolButton1: TToolButton
      Left = 4
      Top = 0
      Action = MainFrame.acLoadStack
    end
    object ToolButton2: TToolButton
      Left = 27
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton3: TToolButton
      Left = 35
      Top = 0
      Action = MainFrame.acJumpToCodeLine
    end
    object ToolButton4: TToolButton
      Left = 58
      Top = 0
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 66
      Top = 0
      Action = MainFrame.acOptions
    end
    object ToolButton6: TToolButton
      Left = 89
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton7: TToolButton
      Left = 97
      Top = 0
      Action = MainFrame.acUpdateLocalInfo
    end
  end
  inline MainFrame: TfrmMain [2]
    Left = 0
    Top = 33
    Width = 356
    Height = 298
    Align = alClient
    TabOrder = 1
    inherited Splitter2: TSplitter
      Height = 298
    end
    inherited tv: TTreeView
      Height = 298
    end
  end
  inherited DockActionList: TActionList [3]
    Top = 216
  end
  inherited ToolbarPopupMenu: TPopupMenu [4]
  end
  inherited ToolActionList: TActionList [5]
  end
end
