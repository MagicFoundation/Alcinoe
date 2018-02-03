object frmMain: TfrmMain
  Left = 271
  Top = 251
  ClientWidth = 640
  ClientHeight = 426
  Caption = 'NT Service Control Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lstSvc: TListView
    Left = 0
    Top = 0
    Width = 632
    Height = 415
    Align = alClient
    Columns = <
      item
        Caption = 'Service Name'
        Width = 80
      end
      item
        Caption = 'Display Name'
        Width = 300
      end
      item
        Caption = 'State'
        Width = 64
      end
      item
        Caption = 'Start Type'
        Width = 80
      end
      item
        Caption = 'Err Ctrl Type'
        Width = 80
      end
      item
        Caption = 'Exit Code'
        Width = 60
      end
      item
        AutoSize = True
        Caption = 'Description'
        WidthType = (
          -12)
      end
      item
        AutoSize = True
        Caption = 'File Name'
        WidthType = (
          -12)
      end
      item
        AutoSize = True
        Caption = 'Group'
        WidthType = (
          -12)
      end>
    GridLines = True
    HideSelection = False
    HotTrack = True
    HotTrackStyles = [htHandPoint, htUnderlineHot]
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = mnuPopup
    ShowHint = True
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = lstSvcColumnClick
    OnData = lstSvcData
  end
  object barStatus: TStatusBar
    Left = 0
    Top = 415
    Width = 632
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object lstActions: TActionList
    Left = 24
    Top = 40
    object actViewRefresh: TAction
      Category = 'View'
      Caption = '&Refresh'
      Hint = 'Refresh all'
      ShortCut = 116
      OnExecute = actViewRefreshExecute
    end
    object actFileConnect: TAction
      Category = 'File'
      Caption = '&Connect...'
      Hint = 'Connect to computer'
      ShortCut = 16462
      OnExecute = actFileConnectExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit the program'
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = 'About'
      Hint = 'About the program'
      ShortCut = 112
      OnExecute = actHelpAboutExecute
    end
    object actControlStart: TAction
      Category = 'Control'
      Caption = '&Start'
      Hint = 'Start Service'
      ShortCut = 16466
      OnExecute = actControlStartExecute
      OnUpdate = actControlStartUpdate
    end
    object actControlStop: TAction
      Category = 'Control'
      Caption = 'St&op'
      Hint = 'Stop Service'
      ShortCut = 16467
      OnExecute = actControlStopExecute
      OnUpdate = actControlStopUpdate
    end
    object actControlPause: TAction
      Category = 'Control'
      Caption = '&Pause'
      Hint = 'Pause Service'
      ShortCut = 16464
      OnExecute = actControlPauseExecute
      OnUpdate = actControlPauseUpdate
    end
    object actControlContinue: TAction
      Category = 'Control'
      Caption = '&Continue'
      Hint = 'Continue Service'
      ShortCut = 16468
      OnExecute = actControlContinueExecute
      OnUpdate = actControlContinueUpdate
    end
    object actViewDependent: TAction
      Category = 'View'
      Caption = '&Dependent'
      Hint = 'View the service dependent'
      ShortCut = 16452
      OnExecute = actViewDependentExecute
      OnUpdate = actViewDependentUpdate
    end
    object actViewGroups: TAction
      Category = 'View'
      Caption = 'Groups'
      Hint = 'View the service groups'
      ShortCut = 16455
      OnExecute = actViewGroupsExecute
      OnUpdate = actViewGroupsUpdate
    end
    object actControlDelete: TAction
      Category = 'Control'
      Caption = '&Delete'
      Hint = 'Delete Service'
      ShortCut = 16430
      OnExecute = actControlDeleteExecute
      OnUpdate = ActionItemSelected
    end
  end
  object mnuPopup: TPopupMenu
    Left = 136
    Top = 40
    object popControlStart: TMenuItem
      Action = actControlStart
    end
    object popControlStop: TMenuItem
      Action = actControlStop
    end
    object popControlPause: TMenuItem
      Action = actControlPause
    end
    object popControlContinue: TMenuItem
      Action = actControlContinue
    end
    object popLine0: TMenuItem
      Caption = '-'
    end
    object popControlDelete: TMenuItem
      Action = actControlDelete
    end
    object popLine1: TMenuItem
      Caption = '-'
    end
    object popViewDependent: TMenuItem
      Action = actViewDependent
    end
    object popViewGroups: TMenuItem
      Action = actViewGroups
    end
    object popLine2: TMenuItem
      Caption = '-'
    end
    object popViewRefresh: TMenuItem
      Action = actViewRefresh
    end
  end
  object mnuMain: TMainMenu
    Left = 80
    Top = 40
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuFileConnect: TMenuItem
        Action = actFileConnect
      end
      object mnuFileLine1: TMenuItem
        Caption = '-'
      end
      object mnuFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      object mnuViewDependent: TMenuItem
        Action = actViewDependent
      end
      object mnuViewGroups: TMenuItem
        Action = actViewGroups
      end
      object mnuViewLine1: TMenuItem
        Caption = '-'
      end
      object mnuViewRefreshStatus: TMenuItem
        Action = actViewRefresh
      end
    end
    object mnuControl: TMenuItem
      Caption = '&Control'
      object mnuControlStart: TMenuItem
        Action = actControlStart
      end
      object mnuControlStop: TMenuItem
        Action = actControlStop
      end
      object mnuControlPause: TMenuItem
        Action = actControlPause
      end
      object mnuControlContinue: TMenuItem
        Action = actControlContinue
      end
      object mnuControlLine1: TMenuItem
        Caption = '-'
      end
      object mnuControlDelete: TMenuItem
        Action = actControlDelete
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuHelpAbout: TMenuItem
        Action = actHelpAbout
      end
    end
  end
end
