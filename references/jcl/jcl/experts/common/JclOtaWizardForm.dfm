object JclWizardForm: TJclWizardForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 423
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 376
    Width = 607
    Height = 17
    Shape = bsTopLine
  end
  object LabelProgression: TLabel
    Left = 8
    Top = 56
    Width = 101
    Height = 13
    Caption = 'RsWizardProgression'
  end
  object ButtonCancel: TButton
    Left = 540
    Top = 388
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'RsCancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonFinish: TButton
    Left = 459
    Top = 388
    Width = 75
    Height = 25
    Action = ActionFinish
    Default = True
    TabOrder = 1
  end
  object ButtonNext: TButton
    Left = 378
    Top = 388
    Width = 75
    Height = 25
    Action = ActionNext
    TabOrder = 0
  end
  object ButtonPrevious: TButton
    Left = 297
    Top = 388
    Width = 75
    Height = 25
    Action = ActionPrevious
    TabOrder = 5
  end
  object PanelTitle: TPanel
    Left = 0
    Top = 0
    Width = 625
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 3
    object ImageJcl: TImage
      Left = 8
      Top = 8
      Width = 33
      Height = 33
    end
    object LabelJcl: TLabel
      Left = 56
      Top = 10
      Width = 156
      Height = 23
      Caption = 'RsAboutDialogTitle'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
  end
  object PanelPages: TPanel
    Left = 2
    Top = 72
    Width = 623
    Height = 298
    BevelOuter = bvNone
    TabOrder = 4
  end
  object ActionListButtons: TActionList
    Left = 240
    Top = 384
    object ActionFinish: TAction
      Caption = 'RsFinish'
      OnExecute = ActionFinishExecute
      OnUpdate = ActionFinishUpdate
    end
    object ActionPrevious: TAction
      Caption = 'RsPrevious'
      OnExecute = ActionPreviousExecute
      OnUpdate = ActionPreviousUpdate
    end
    object ActionNext: TAction
      Caption = 'RsNext'
      OnExecute = ActionNextExecute
      OnUpdate = ActionNextUpdate
    end
  end
end
