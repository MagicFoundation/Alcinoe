object frmMain: TfrmMain
  Left = 0
  Top = 0
  Width = 372
  Height = 320
  TabOrder = 0
  OnResize = FrameResize
  object Splitter2: TSplitter
    Left = 145
    Top = 0
    Height = 301
  end
  object tv: TTreeView
    Left = 0
    Top = 0
    Width = 145
    Height = 301
    Align = alLeft
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = tvChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 301
    Width = 372
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object PB: TProgressBar
    Left = 19
    Top = 300
    Width = 41
    Height = 17
    TabOrder = 2
    Visible = False
  end
  object ActionList1: TActionList
    Left = 56
    Top = 216
    object acJumpToCodeLine: TAction
      Caption = 'RsJumpToCodeLine'
      Hint = 'Jump to the code line of the selected stack line'
      OnExecute = acJumpToCodeLineExecute
      OnUpdate = acJumpToCodeLineUpdate
    end
    object acLoadStack: TAction
      Caption = 'RsLoadStack'
      Hint = 'Load Stack from file'
      OnExecute = acLoadStackExecute
    end
    object acOptions: TAction
      Caption = 'RsOptions'
      OnExecute = acOptionsExecute
    end
    object acUpdateLocalInfo: TAction
      Caption = 'RsUpdateLocalInfo'
      OnExecute = acUpdateLocalInfoExecute
      OnUpdate = acUpdateLocalInfoUpdate
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 56
    Top = 264
  end
end
