object JclVersionCtrlOptionsFrame: TJclVersionCtrlOptionsFrame
  Left = 0
  Top = 0
  Width = 389
  Height = 409
  Anchors = [akLeft, akTop, akRight, akBottom]
  TabOrder = 0
  TabStop = True
  object LabelIcons: TLabel
    Left = 16
    Top = 106
    Width = 39
    Height = 13
    Caption = 'RsIcons'
    FocusControl = ComboBoxIcons
  end
  object LabelMenuOrganization: TLabel
    Left = 16
    Top = 130
    Width = 99
    Height = 13
    Caption = 'RsMenuOrganization'
    FocusControl = TreeViewMenu
  end
  object CheckBoxHideActions: TCheckBox
    Left = 16
    Top = 31
    Width = 185
    Height = 17
    Caption = 'RsHideUnsupportedActions'
    TabOrder = 1
  end
  object ComboBoxIcons: TComboBox
    Left = 72
    Top = 103
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Items.Strings = (
      'RsNoIcon'
      'RsJCLIcons')
  end
  object TreeViewMenu: TTreeView
    Left = 16
    Top = 149
    Width = 260
    Height = 245
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideSelection = False
    Indent = 19
    RightClickSelect = True
    RowSelect = True
    ShowRoot = False
    TabOrder = 5
    OnEdited = TreeViewMenuEdited
    OnEditing = TreeViewMenuEditing
  end
  object CheckBoxDisableActions: TCheckBox
    Left = 16
    Top = 8
    Width = 201
    Height = 17
    Caption = 'RsDisableActions'
    TabOrder = 0
  end
  object ButtonNewSeparator: TButton
    Left = 282
    Top = 180
    Width = 87
    Height = 25
    Action = ActionNewSeparator
    Anchors = [akTop, akRight]
    TabOrder = 7
  end
  object ButtonDelete: TButton
    Left = 282
    Top = 258
    Width = 87
    Height = 25
    Action = ActionDeleteItem
    Anchors = [akTop, akRight]
    TabOrder = 9
  end
  object ButtonRename: TButton
    Left = 282
    Top = 289
    Width = 87
    Height = 25
    Action = ActionRenameItem
    Anchors = [akTop, akRight]
    TabOrder = 10
  end
  object ButtonMoveUp: TButton
    Left = 282
    Top = 336
    Width = 87
    Height = 25
    Action = ActionMoveItemUp
    Anchors = [akTop, akRight]
    TabOrder = 11
  end
  object ButtonMoveDown: TButton
    Left = 282
    Top = 367
    Width = 87
    Height = 25
    Action = ActionMoveItemDown
    Anchors = [akTop, akRight]
    TabOrder = 12
  end
  object CheckBoxSaveConfirmation: TCheckBox
    Left = 16
    Top = 54
    Width = 201
    Height = 17
    Caption = 'RsSaveConfirmation'
    TabOrder = 2
  end
  object ButtonNewAction: TButton
    Left = 282
    Top = 211
    Width = 87
    Height = 25
    Action = ActionNewAction
    Anchors = [akTop, akRight]
    TabOrder = 8
  end
  object ButtonNewSubMenu: TButton
    Left = 282
    Top = 149
    Width = 87
    Height = 25
    Action = ActionNewSubMenu
    Anchors = [akTop, akRight]
    TabOrder = 6
  end
  object CheckBoxActOnTopSandbox: TCheckBox
    Left = 16
    Top = 77
    Width = 201
    Height = 17
    Caption = 'RsActOnTopSandbox'
    TabOrder = 3
  end
  object ActionListVersionCtrl: TActionList
    Left = 256
    Top = 64
    object ActionNewSubMenu: TAction
      Caption = 'RsNewSubMenu'
      OnExecute = ActionNewSubMenuExecute
      OnUpdate = ActionNewSubMenuUpdate
    end
    object ActionNewSeparator: TAction
      Caption = 'RsNewSeparator'
      OnExecute = ActionNewSeparatorExecute
      OnUpdate = ActionNewSeparatorUpdate
    end
    object ActionNewAction: TAction
      Caption = 'RsNewAction'
      OnExecute = ActionNewActionExecute
      OnUpdate = ActionNewActionUpdate
    end
    object ActionDeleteItem: TAction
      Caption = 'RsDeleteItem'
      OnExecute = ActionDeleteItemExecute
      OnUpdate = ActionDeleteItemUpdate
    end
    object ActionRenameItem: TAction
      Caption = 'RsRenameItem'
      OnExecute = ActionRenameItemExecute
      OnUpdate = ActionRenameItemUpdate
    end
    object ActionMoveItemUp: TAction
      Caption = 'RsMoveItemUp'
      OnExecute = ActionMoveItemUpExecute
      OnUpdate = ActionMoveItemUpUpdate
    end
    object ActionMoveItemDown: TAction
      Caption = 'RsMoveItemDown'
      OnExecute = ActionMoveItemDownExecute
      OnUpdate = ActionMoveItemDownUpdate
    end
  end
  object PopupMenuActions: TPopupMenu
    Left = 296
    Top = 64
  end
end
