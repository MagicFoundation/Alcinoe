object JclOtaOptionsForm: TJclOtaOptionsForm
  Left = 0
  Top = 0
  Caption = 'RsOtaConfigurationCaption'
  ClientHeight = 486
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterSep: TSplitter
    Left = 185
    Top = 0
    Height = 448
  end
  object PanelName: TPanel
    Left = 0
    Top = 448
    Width = 554
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object LabelHomePage: TLabel
      Left = 8
      Top = 8
      Width = 75
      Height = 13
      Cursor = crHandPoint
      Caption = 'RsHomePage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsUnderline]
      ParentFont = False
      OnClick = LabelHomePageClick
    end
    object ButtonOk: TButton
      Left = 391
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'RsCaptionOk'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 472
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'RsCaptionCancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PanelTree: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 448
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object TreeViewCategories: TTreeView
      Left = 8
      Top = 8
      Width = 171
      Height = 436
      Anchors = [akLeft, akTop, akRight, akBottom]
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      TabOrder = 0
      OnChange = TreeViewCategoriesChange
      OnChanging = TreeViewCategoriesChanging
    end
  end
  object PanelOptions: TPanel
    Left = 188
    Top = 0
    Width = 366
    Height = 448
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object LabelSelectPage: TLabel
      Left = 152
      Top = 184
      Width = 65
      Height = 13
      Caption = 'RsSelectPage'
      FocusControl = TreeViewCategories
    end
  end
end
