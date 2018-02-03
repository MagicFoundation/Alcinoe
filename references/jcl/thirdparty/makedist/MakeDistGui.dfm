object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Make distribution settings'
  ClientHeight = 533
  ClientWidth = 678
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterTasks: TSplitter
    Left = 161
    Top = 65
    Height = 365
    Beveled = True
  end
  object SplitterActions: TSplitter
    Left = 501
    Top = 65
    Height = 365
    Align = alRight
    Beveled = True
  end
  object SplitterMessages: TSplitter
    Left = 0
    Top = 430
    Width = 678
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object PanelFile: TPanel
    Left = 0
    Top = 0
    Width = 678
    Height = 65
    Align = alTop
    TabOrder = 4
    object ButtonNewDistribution: TButton
      Left = 16
      Top = 16
      Width = 121
      Height = 25
      Caption = '&New distribution'
      TabOrder = 0
      OnClick = ButtonNewDistributionClick
    end
    object ButtonOpenDistribution: TButton
      Left = 152
      Top = 16
      Width = 121
      Height = 25
      Caption = '&Open distribution...'
      TabOrder = 1
      OnClick = ButtonOpenDistributionClick
    end
    object ButtonSaveDistribution: TButton
      Left = 288
      Top = 16
      Width = 121
      Height = 25
      Caption = '&Save distribution'
      TabOrder = 2
      OnClick = ButtonSaveDistributionClick
    end
    object ButtonSaveDistributionAs: TButton
      Left = 424
      Top = 16
      Width = 121
      Height = 25
      Caption = 'Sa&ve distribution as...'
      TabOrder = 3
      OnClick = ButtonSaveDistributionAsClick
    end
  end
  object PanelTasks: TPanel
    Left = 0
    Top = 65
    Width = 161
    Height = 365
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object LabelTaskList: TLabel
      Left = 2
      Top = 8
      Width = 115
      Height = 13
      Caption = 'List of configured tasks:'
    end
    object CheckListBoxTasks: TCheckListBox
      Left = 2
      Top = 27
      Width = 153
      Height = 180
      OnClickCheck = CheckListBoxTasksClickCheck
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = CheckListBoxTasksClick
    end
    object ButtonNewTask: TButton
      Left = 16
      Top = 213
      Width = 129
      Height = 25
      Anchors = [akBottom]
      Caption = 'N&ew task'
      TabOrder = 1
      OnClick = ButtonNewTaskClick
    end
    object ButtonDeleteTask: TButton
      Left = 16
      Top = 275
      Width = 129
      Height = 25
      Anchors = [akBottom]
      Caption = 'De&lete task'
      TabOrder = 2
      OnClick = ButtonDeleteTaskClick
    end
    object ButtonExecuteSelectedTask: TButton
      Left = 16
      Top = 306
      Width = 129
      Height = 25
      Anchors = [akBottom]
      Caption = 'Exe&cute selected task'
      TabOrder = 3
      OnClick = ButtonExecuteSelectedTaskClick
    end
    object ButtonExecuteCheckedTasks: TButton
      Left = 16
      Top = 337
      Width = 129
      Height = 25
      Anchors = [akBottom]
      Caption = 'E&xecute checked tasks'
      Default = True
      TabOrder = 4
      OnClick = ButtonExecuteCheckedTasksClick
    end
    object ButtonRenameTask: TButton
      Left = 16
      Top = 244
      Width = 129
      Height = 25
      Anchors = [akBottom]
      Caption = '&Rename task'
      TabOrder = 5
      OnClick = ButtonRenameTaskClick
    end
  end
  object PanelActions: TPanel
    Left = 504
    Top = 65
    Width = 174
    Height = 365
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object LabelActionList: TLabel
      Left = 3
      Top = 8
      Width = 115
      Height = 13
      Caption = 'List of available actions:'
    end
    object ListBoxActionList: TListBox
      Left = 3
      Top = 27
      Width = 166
      Height = 304
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnDrawItem = ListBoxActionListDrawItem
    end
    object ButtonAddActionToTask: TButton
      Left = 16
      Top = 337
      Width = 145
      Height = 25
      Anchors = [akBottom]
      Caption = '&Add action to task'
      TabOrder = 1
      OnClick = ButtonAddActionToTaskClick
    end
  end
  object PanelMiddle: TPanel
    Left = 164
    Top = 65
    Width = 337
    Height = 365
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object PanelConfiguration: TPanel
      Left = 0
      Top = 288
      Width = 337
      Height = 77
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
    object PanelTaskActions: TPanel
      Left = 0
      Top = 0
      Width = 337
      Height = 288
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object LabelTaskActionList: TLabel
        Left = 8
        Top = 8
        Width = 130
        Height = 13
        Caption = 'List of actions for this task:'
      end
      object ListBoxTaskActions: TListBox
        Left = 6
        Top = 27
        Width = 243
        Height = 255
        Style = lbOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = ListBoxTaskActionsClick
        OnDrawItem = ListBoxTaskActionsDrawItem
      end
      object ButtonMoveUp: TButton
        Left = 256
        Top = 27
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move &up'
        TabOrder = 1
        OnClick = ButtonMoveUpClick
      end
      object ButtonMoveDown: TButton
        Left = 256
        Top = 56
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Move &down'
        TabOrder = 2
        OnClick = ButtonMoveDownClick
      end
      object ButtonDeleteAction: TButton
        Left = 256
        Top = 88
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = ButtonDeleteActionClick
      end
    end
  end
  object PanelMessages: TPanel
    Left = 0
    Top = 433
    Width = 678
    Height = 100
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object MemoMessages: TMemo
      Left = 0
      Top = 0
      Width = 678
      Height = 100
      Align = alClient
      Lines.Strings = (
        'Messages')
      ReadOnly = True
      TabOrder = 0
    end
  end
  object OpenDialogConfiguration: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'Configuration file (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open configuration...'
    Left = 200
    Top = 128
  end
  object SaveDialogConfiguration: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'Configuration file (*.xml)|*.xml|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Save configuration as...'
    Left = 232
    Top = 128
  end
end
