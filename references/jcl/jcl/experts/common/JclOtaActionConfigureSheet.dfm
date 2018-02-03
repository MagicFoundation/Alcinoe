object JclOtaActionConfigureFrame: TJclOtaActionConfigureFrame
  Left = 0
  Top = 0
  Width = 369
  Height = 375
  Anchors = [akLeft, akTop, akRight, akBottom]
  TabOrder = 0
  TabStop = True
  object LabelActions: TLabel
    Left = 16
    Top = 16
    Width = 47
    Height = 13
    Caption = 'RsActions'
    FocusControl = ListViewActions
  end
  object LabelShortcut: TLabel
    Left = 16
    Top = 333
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'RsShortcut'
    FocusControl = HotKeyShortcut
  end
  object ListViewActions: TListView
    Left = 16
    Top = 35
    Width = 337
    Height = 286
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'RsCaption'
        Width = 150
      end
      item
        Caption = 'RsShortcut'
        Width = 100
      end>
    HideSelection = False
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = ListViewActionsSelectItem
  end
  object HotKeyShortcut: THotKey
    Left = 80
    Top = 330
    Width = 121
    Height = 19
    Anchors = [akLeft, akBottom]
    HotKey = 0
    InvalidKeys = [hcNone]
    Modifiers = []
    TabOrder = 1
    OnExit = HotKeyShortcutExit
  end
  object ButtonRestore: TButton
    Left = 208
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'RsRestore'
    TabOrder = 2
    OnClick = ButtonRestoreClick
  end
end
