object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 423
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 410
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options:'
    TabOrder = 0
    object CheckBoxBinaryTree: TCheckBox
      Left = 16
      Top = 24
      Width = 161
      Height = 17
      Action = ActionBinaryTree
      TabOrder = 0
    end
    object CheckBoxGeneralPurposeTree: TCheckBox
      Left = 16
      Top = 48
      Width = 161
      Height = 17
      Action = ActionGeneralPurposeTree
      State = cbChecked
      TabOrder = 1
    end
    object CheckBoxCaseSensitive: TCheckBox
      Left = 16
      Top = 96
      Width = 97
      Height = 17
      Action = ActionCaseSensitive
      TabOrder = 2
    end
    object CheckBoxAllowDefault: TCheckBox
      Left = 199
      Top = 24
      Width = 178
      Height = 17
      Action = ActionAllowDefault
      TabOrder = 3
    end
    object CheckBoxAllowDuplicates: TCheckBox
      Left = 200
      Top = 48
      Width = 177
      Height = 17
      Action = ActionAllowDuplicates
      TabOrder = 4
    end
    object CheckBoxIgnoreDuplicates: TCheckBox
      Left = 216
      Top = 72
      Width = 161
      Height = 17
      Action = ActionIgnoreDuplicates
      TabOrder = 5
    end
    object CheckBoxRemoveSingle: TCheckBox
      Left = 200
      Top = 96
      Width = 177
      Height = 17
      Action = ActionRemoveSingle
      TabOrder = 6
    end
  end
  object TreeViewResults: TTreeView
    Left = 8
    Top = 143
    Width = 193
    Height = 272
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 1
  end
  object GroupBoxActions: TGroupBox
    Left = 207
    Top = 143
    Width = 211
    Height = 272
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Actions:'
    TabOrder = 2
    object ButtonGenerateRandom: TButton
      Left = 16
      Top = 24
      Width = 177
      Height = 25
      Action = ActionGenerateRandom
      TabOrder = 0
    end
    object ButtonRemoveSelected: TButton
      Left = 16
      Top = 56
      Width = 177
      Height = 25
      Action = ActionRemoveSelected
      TabOrder = 1
    end
    object EditNewItem: TEdit
      Left = 16
      Top = 168
      Width = 177
      Height = 21
      TabOrder = 2
      Text = 'New item'
    end
    object ButtonAddNew: TButton
      Left = 16
      Top = 200
      Width = 177
      Height = 25
      Action = ActionAddNew
      TabOrder = 3
    end
    object ButtonAddNewChild: TButton
      Left = 16
      Top = 231
      Width = 177
      Height = 25
      Action = ActionAddNewChild
      TabOrder = 4
    end
    object Button1: TButton
      Left = 16
      Top = 87
      Width = 177
      Height = 25
      Action = ActionPack
      TabOrder = 5
    end
    object Button2: TButton
      Left = 16
      Top = 120
      Width = 177
      Height = 25
      Action = ActionTestTree
      TabOrder = 6
    end
  end
  object ActionListMain: TActionList
    Left = 160
    Top = 112
    object ActionAllowDuplicates: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Allow duplicates'
      OnExecute = ActionDuplicatesExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionIgnoreDuplicates: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Ignore duplicates'
      OnExecute = ActionDuplicatesExecute
      OnUpdate = ActionIgnoreDuplicatesUpdate
    end
    object ActionAllowDefault: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Allow defaults (empty strings)'
      OnExecute = ActionAllowDefaultExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionRemoveSingle: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Remove single element'
      OnExecute = ActionRemoveSingleExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionCaseSensitive: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Case sensitive'
      OnExecute = ActionCaseSensitiveExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionGenerateRandom: TAction
      Category = 'Tree actions'
      Caption = 'Generate random tree'
      OnExecute = ActionGenerateRandomExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionAddNew: TAction
      Category = 'Tree actions'
      Caption = 'Add new'
      OnExecute = ActionAddNewExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionRemoveSelected: TAction
      Category = 'Tree actions'
      Caption = 'Remove selected'
      OnExecute = ActionRemoveSelectedExecute
      OnUpdate = ActionRemoveSelectedUpdate
    end
    object ActionAddNewChild: TAction
      Category = 'Tree actions'
      Caption = 'Add new child'
      OnExecute = ActionAddNewChildExecute
      OnUpdate = ActionAddNewChildUpdate
    end
    object ActionBinaryTree: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'Binary tree'
      OnExecute = ActionBinaryTreeExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionGeneralPurposeTree: TAction
      Category = 'Tree options'
      AutoCheck = True
      Caption = 'General purpose tree'
      Checked = True
      OnExecute = ActionGeneralPurposeTreeExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionPack: TAction
      Category = 'Tree actions'
      Caption = 'Pack'
      OnExecute = ActionPackExecute
      OnUpdate = ActionAlwaysEnabled
    end
    object ActionTestTree: TAction
      Category = 'Tree actions'
      Caption = 'Test tree'
      OnExecute = ActionTestTreeExecute
      OnUpdate = ActionAlwaysEnabled
    end
  end
end
