object ViewForm: TViewForm
  Left = 288
  Top = 168
  ClientWidth = 340
  ClientHeight = 284
  BorderStyle = bsSizeToolWin
  Caption = 'ViewForm'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 340
    Height = 26
    AutoSize = True
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 22
        Width = 336
      end>
    PopupMenu = ToolBarPopupMenu
    object ToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 323
      Height = 22
      AutoSize = True
      Caption = 'ToolBar'
      EdgeBorders = []
      Flat = True
      Images = GlobalModule.ToolbarImagesList
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Images = GlobalModule.ToolbarImagesList
    Left = 8
    Top = 224
    object TextLabels1: TAction
      Caption = 'Text labels'
      OnExecute = TextLabels1Execute
    end
    object Copy1: TAction
      Caption = 'Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 9
      ShortCut = 16451
      OnExecute = Copy1Execute
      OnUpdate = Copy1Update
    end
    object SaveToFile1: TAction
      Caption = 'Save'
      Hint = 'Save to text file'
      ImageIndex = 3
      ShortCut = 16467
      OnExecute = SaveToFile1Execute
      OnUpdate = Copy1Update
    end
    object Refresh1: TAction
      Caption = 'Refresh'
      Hint = 'Refresh the list'
      ImageIndex = 2
      ShortCut = 116
    end
    object SelectAll1: TAction
      Caption = 'Select all'
      Hint = 'Select all listview items'
      ImageIndex = 17
      ShortCut = 16449
      OnExecute = SelectAll1Execute
      OnUpdate = SelectAll1Update
    end
    object Find1: TAction
      Caption = 'Find text'
      Hint = 'Find text'
      ImageIndex = 7
      ShortCut = 16454
      OnExecute = Find1Execute
      OnUpdate = Find1Update
    end
  end
  object PopupMenu: TPopupMenu
    Images = GlobalModule.ToolbarImagesList
    Left = 40
    Top = 224
  end
  object ToolBarPopupMenu: TPopupMenu
    Left = 72
    Top = 224
    object Textlabels2: TMenuItem
      Action = TextLabels1
    end
  end
end
