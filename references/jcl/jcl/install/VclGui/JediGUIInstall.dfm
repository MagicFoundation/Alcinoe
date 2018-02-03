object InstallFrame: TInstallFrame
  Left = 0
  Top = 0
  Width = 791
  Height = 421
  HorzScrollBar.Range = 398
  TabOrder = 0
  TabStop = True
  object Splitter: TSplitter
    Left = 426
    Top = 0
    Width = 5
    Height = 421
    Align = alRight
    MinSize = 150
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
  end
  object InfoPanel: TPanel
    Left = 431
    Top = 0
    Width = 360
    Height = 421
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object LabelInstallationLog: TLabel
      Left = 6
      Top = 5
      Width = 100
      Height = 13
      Caption = 'RsGUIInstallationLog'
    end
    object InfoDisplay: TRichEdit
      Left = 6
      Top = 24
      Width = 347
      Height = 298
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clInfoBk
      Font.Charset = ANSI_CHARSET
      Font.Color = clInfoText
      Font.Height = -11
      Font.Name = 'Lucida Console'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentFont = False
      PlainText = True
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object OptionsGroupBox: TGroupBox
      Left = 6
      Top = 328
      Width = 348
      Height = 86
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'RsGUIAdvancedOptions'
      TabOrder = 1
    end
    object ProgressBar: TProgressBar
      Left = 96
      Top = 5
      Width = 257
      Height = 13
      TabOrder = 2
      Visible = False
    end
  end
  object ComponentsTreePanel: TPanel
    Left = 0
    Top = 0
    Width = 426
    Height = 421
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object LabelSelectComponents: TLabel
      Left = 8
      Top = 5
      Width = 119
      Height = 13
      Caption = 'RsGUISelectComponents'
    end
    object TreeView: TTreeView
      Left = 8
      Top = 24
      Width = 413
      Height = 390
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 19
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 0
      ToolTips = False
      OnCustomDrawItem = TreeViewCustomDrawItem
      OnKeyPress = TreeViewKeyPress
      OnMouseDown = TreeViewMouseDown
    end
  end
end
