object frmMain: TfrmMain
  Left = 288
  Top = 174
  Width = 696
  Height = 480
  Caption = 'Microsoft Task Schedule Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterV: TSplitter
    Left = 0
    Top = 209
    Width = 688
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object barStatus: TStatusBar
    Left = 0
    Top = 415
    Width = 688
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object lstTasks: TListView
    Left = 0
    Top = 0
    Width = 688
    Height = 209
    Align = alTop
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Alignment = taCenter
        Caption = 'Last Run Time'
        Width = 120
      end
      item
        Alignment = taCenter
        Caption = 'Next Run Time'
        Width = 120
      end
      item
        AutoSize = True
        Caption = 'Comment'
      end>
    FlatScrollBars = True
    GridLines = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = mnuPopup
    TabOrder = 1
    ViewStyle = vsReport
    OnSelectItem = lstTasksSelectItem
  end
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 212
    Width = 688
    Height = 203
    Align = alClient
    TabOrder = 2
    OnDocumentComplete = WebBrowserDocumentComplete
    ControlData = {
      4C0000001B470000FB1400000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object mnuMain: TMainMenu
    Images = DM.lstImage
    Left = 24
    Top = 40
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuFileExit: TMenuItem
        Action = DM.actFileExit
      end
    end
    object mnuTask: TMenuItem
      Caption = '&Task'
      object mnuTaskAdd: TMenuItem
        Action = DM.actTaskAdd
      end
      object mnuTaskDelete: TMenuItem
        Action = DM.actTaskDelete
      end
      object mnuTaskLine0: TMenuItem
        Caption = '-'
      end
      object mnuTaskRun: TMenuItem
        Action = DM.actTaskRun
      end
      object mnuTaskStop: TMenuItem
        Action = DM.actTaskStop
      end
      object mnuTaskLine1: TMenuItem
        Caption = '-'
      end
      object mnuTaskRefresh: TMenuItem
        Action = DM.actTaskRefresh
      end
      object mnuTaskLine2: TMenuItem
        Caption = '-'
      end
      object mnuTaskProp: TMenuItem
        Action = DM.actTaskProp
      end
    end
  end
  object mnuPopup: TPopupMenu
    Images = DM.lstImage
    Left = 80
    Top = 40
    object popTaskAdd: TMenuItem
      Action = DM.actTaskAdd
    end
    object popTaskDelete: TMenuItem
      Action = DM.actTaskDelete
    end
    object popLine0: TMenuItem
      Caption = '-'
    end
    object popTaskRun: TMenuItem
      Action = DM.actTaskRun
    end
    object popTaskStop: TMenuItem
      Action = DM.actTaskStop
    end
    object popLine1: TMenuItem
      Caption = '-'
    end
    object popTaskRefresh: TMenuItem
      Action = DM.actTaskRefresh
    end
    object popLine2: TMenuItem
      Caption = '-'
    end
    object popTaskProp: TMenuItem
      Action = DM.actTaskProp
    end
  end
  object ppTaskInfo: TPageProducer
    HTMLDoc.Strings = (
      '<HTML>'
      '<HEAD>'
      '<script language="javascript">'
      '<!--'
      'function click()'
      '{'
      '  window.event.returnValue=false;'
      '}'
      'document.oncontextmenu=click;'
      '-->'
      '</script>'
      '</HEAD>'
      '<BODY scroll="auto">'
      '<CENTER>'
      '<TABLE width="100%">'
      '  <TR><TH>Name</TH><TH>Value</TH></TR>'
      '  <TR><TD>TaskName</TD><TD><#TaskName></TD></TR>'
      '  <TR><TD>AccountName</TD><TD><#AccountName></TD></TR>'
      ''
      '  <TR><TD>Comment</TD><TD><#Comment></TD></TR>'
      '  <TR><TD>Creator</TD><TD><#Creator></TD></TR>'
      ''
      '  <TR><TD>ErrorRetryCount</TD><TD><#ErrorRetryCount></TD></TR>'
      
        '  <TR><TD>ErrorRetryInterval</TD><TD><#ErrorRetryInterval></TD><' +
        '/TR>'
      ''
      '  <TR><TD>ExitCode</TD><TD><#ExitCode></TD></TR>'
      ''
      '  <TR><TD>OwnerData</TD><TD><#Data></TD></TR>'
      ''
      '  <TR><TD>IdleMinutes</TD><TD><#IdleMinutes></TD></TR>'
      '  <TR><TD>DeadlineMinutes</TD><TD><#DeadlineMinutes></TD></TR>'
      ''
      
        '  <TR><TD>MostRecentRunTime</TD><TD><#MostRecentRunTime></TD></T' +
        'R>'
      '  <TR><TD>NextRunTime</TD><TD><#NextRunTime></TD></TR>'
      ''
      '  <TR><TD>Status</TD><TD><#Status></TD></TR>'
      '  <TR><TD>Flags</TD><TD><#Flags></TD></TR>'
      ''
      '  <TR><TD>ApplicationName</TD><TD><#ApplicationName></TD></TR>'
      '  <TR><TD>WorkingDirectory</TD><TD><#WorkingDirectory></TD></TR>'
      '  <TR><TD>MaxRunTime</TD><TD><#MaxRunTime></TD></TR>'
      '  <TR><TD>Parameters</TD><TD><#Parameters></TD></TR>'
      '  <TR><TD>Priority</TD><TD><#Priority></TD></TR>'
      '  <TR><TD>TaskFlags</TD><TD><#TaskFlags></TD></TR>'
      '  <TR><TD>Triggers</TD><TD><#Triggers></TD></TR>'
      '</TABLE>'
      '</CENTER>'
      '</BODY>'
      '</HTML>')
    OnHTMLTag = ppTaskInfoHTMLTag
    Left = 144
    Top = 40
  end
end
