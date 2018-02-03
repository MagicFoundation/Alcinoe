object MainForm: TMainForm
  Left = 382
  Top = 187
  ClientWidth = 476
  ClientHeight = 433
  Caption = 'JCL SysInfo demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pageSysInfo: TPageControl
    Left = 4
    Top = 4
    Width = 468
    Height = 389
    ActivePage = tabSystemFolders
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabSystemFolders: TTabSheet
      Caption = 'System Folders'
      object Label1: TLabel
        Left = 12
        Top = 16
        Width = 68
        Height = 13
        Caption = 'Common Files:'
      end
      object Label2: TLabel
        Left = 12
        Top = 40
        Width = 69
        Height = 13
        Caption = 'Current Folder:'
      end
      object Label3: TLabel
        Left = 12
        Top = 64
        Width = 66
        Height = 13
        Caption = 'Program Files:'
      end
      object Label4: TLabel
        Left = 12
        Top = 88
        Width = 79
        Height = 13
        Caption = 'Windows Folder:'
      end
      object Label5: TLabel
        Left = 12
        Top = 112
        Width = 69
        Height = 13
        Caption = 'System Folder:'
      end
      object Label6: TLabel
        Left = 12
        Top = 136
        Width = 62
        Height = 13
        Caption = 'Temp Folder:'
      end
      object Label20: TLabel
        Left = 12
        Top = 160
        Width = 61
        Height = 13
        Caption = 'Fonts Folder:'
      end
      object Label26: TLabel
        Left = 12
        Top = 184
        Width = 105
        Height = 13
        Caption = 'Internet Cache Folder:'
      end
      object Label27: TLabel
        Left = 12
        Top = 208
        Width = 73
        Height = 13
        Caption = 'Cookies Folder:'
      end
      object Label28: TLabel
        Left = 12
        Top = 232
        Width = 67
        Height = 13
        Caption = 'History Folder:'
      end
      object edtCommonFiles: TEdit
        Left = 124
        Top = 12
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object edtCurrentFolder: TEdit
        Left = 124
        Top = 36
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object edtProgramFiles: TEdit
        Left = 124
        Top = 60
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 2
      end
      object edtWindowsFolder: TEdit
        Left = 124
        Top = 84
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
      object edtSystemFolder: TEdit
        Left = 124
        Top = 108
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 4
      end
      object edtTempFolder: TEdit
        Left = 124
        Top = 132
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 5
      end
      object edtFontsFolder: TEdit
        Left = 124
        Top = 156
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 6
      end
      object edtInternetCacheFolder: TEdit
        Left = 124
        Top = 180
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 7
      end
      object edtCookiesFolder: TEdit
        Left = 124
        Top = 204
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 8
      end
      object edtHistoryFolder: TEdit
        Left = 124
        Top = 228
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 9
      end
    end
    object tabCommonDirectories: TTabSheet
      Caption = 'Common Directories'
      ImageIndex = 1
      object Label30: TLabel
        Left = 12
        Top = 16
        Width = 127
        Height = 13
        Caption = 'Common Startmenu Folder:'
      end
      object Label22: TLabel
        Left = 12
        Top = 88
        Width = 123
        Height = 13
        Caption = 'Common Programs Folder:'
      end
      object Label23: TLabel
        Left = 12
        Top = 112
        Width = 132
        Height = 13
        Caption = 'Common Desktop Directory:'
      end
      object Label11: TLabel
        Left = 12
        Top = 40
        Width = 122
        Height = 13
        Caption = 'Common Favorites Folder:'
      end
      object Label15: TLabel
        Left = 12
        Top = 64
        Width = 113
        Height = 13
        Caption = 'Common Startup Folder:'
      end
      object edtCommonStartmenuFolder: TEdit
        Left = 152
        Top = 12
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object edtCommonProgramsFolder: TEdit
        Left = 152
        Top = 84
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object edtCommonDesktopDirectory: TEdit
        Left = 152
        Top = 108
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 2
      end
      object edtCommonFavoritesFolder: TEdit
        Left = 152
        Top = 36
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
      object edtCommonStartupFolder: TEdit
        Left = 152
        Top = 60
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 4
      end
    end
    object tabCurrentUser: TTabSheet
      Caption = 'Current User Profile'
      ImageIndex = 2
      object Label7: TLabel
        Left = 12
        Top = 16
        Width = 75
        Height = 13
        Caption = 'Desktop Folder:'
      end
      object Label9: TLabel
        Left = 12
        Top = 40
        Width = 79
        Height = 13
        Caption = 'Programs Folder:'
      end
      object Label12: TLabel
        Left = 12
        Top = 64
        Width = 76
        Height = 13
        Caption = 'Personal Folder:'
      end
      object Label13: TLabel
        Left = 12
        Top = 88
        Width = 78
        Height = 13
        Caption = 'Favorites Folder:'
      end
      object Label14: TLabel
        Left = 12
        Top = 112
        Width = 69
        Height = 13
        Caption = 'Startup Folder:'
      end
      object Label8: TLabel
        Left = 12
        Top = 136
        Width = 62
        Height = 13
        Caption = 'Recent Files:'
      end
      object Label16: TLabel
        Left = 12
        Top = 160
        Width = 73
        Height = 13
        Caption = 'SendTo Folder:'
      end
      object Label17: TLabel
        Left = 12
        Top = 184
        Width = 86
        Height = 13
        Caption = 'Start menu Folder:'
      end
      object Label24: TLabel
        Left = 12
        Top = 208
        Width = 113
        Height = 13
        Caption = 'Application Data Folder:'
      end
      object Label25: TLabel
        Left = 12
        Top = 232
        Width = 80
        Height = 13
        Caption = 'Printhood Folder:'
      end
      object Label10: TLabel
        Left = 12
        Top = 256
        Width = 88
        Height = 13
        Caption = 'Desktop Directory:'
      end
      object Label18: TLabel
        Left = 12
        Top = 280
        Width = 76
        Height = 13
        Caption = 'Nethood Folder:'
      end
      object Label21: TLabel
        Left = 12
        Top = 304
        Width = 84
        Height = 13
        Caption = 'Templates Folder:'
      end
      object edtDesktopFolder: TEdit
        Left = 132
        Top = 12
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object edtProgramsFolder: TEdit
        Left = 132
        Top = 36
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 1
      end
      object edtPersonalFolder: TEdit
        Left = 132
        Top = 60
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 2
      end
      object edtFavoritesFolder: TEdit
        Left = 132
        Top = 84
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
      object edtStartupFolder: TEdit
        Left = 132
        Top = 108
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 4
      end
      object edtRecentFilesFolder: TEdit
        Left = 132
        Top = 132
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 5
      end
      object edtSendToFolder: TEdit
        Left = 132
        Top = 156
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 6
      end
      object edtStartMenuFolder: TEdit
        Left = 132
        Top = 180
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 7
      end
      object edtAppdataFolder: TEdit
        Left = 132
        Top = 204
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 8
      end
      object edtPrintHoodFolder: TEdit
        Left = 132
        Top = 228
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 9
      end
      object edtDesktopDirectory: TEdit
        Left = 132
        Top = 252
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 10
      end
      object edtNethoodFolder: TEdit
        Left = 132
        Top = 276
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 11
      end
      object edtTemplatesFolder: TEdit
        Left = 132
        Top = 300
        Width = 277
        Height = 21
        ReadOnly = True
        TabOrder = 12
      end
    end
    object tabAPM: TTabSheet
      Caption = 'APM'
      ImageIndex = 3
      object Label19: TLabel
        Left = 16
        Top = 16
        Width = 108
        Height = 13
        Caption = 'Battery Life Time (sec):'
      end
      object Label29: TLabel
        Left = 16
        Top = 40
        Width = 127
        Height = 13
        Caption = 'Battery Full Life Time (sec):'
      end
      object Label31: TLabel
        Left = 16
        Top = 64
        Width = 96
        Height = 13
        Caption = 'Battery Life Percent:'
      end
      object Label32: TLabel
        Left = 16
        Top = 88
        Width = 92
        Height = 13
        Caption = 'Battery Line Status:'
      end
      object Label33: TLabel
        Left = 16
        Top = 112
        Width = 59
        Height = 13
        Caption = 'Battery Flag:'
      end
      object lblAPMPlatforms: TLabel
        Left = 16
        Top = 148
        Width = 289
        Height = 13
        Caption = 'APM is only available on Windows 95 / 98 / Me / 2000/ XP !'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object edtBatteryLifeTime: TEdit
        Left = 152
        Top = 12
        Width = 61
        Height = 21
        Color = clInactiveCaptionText
        ReadOnly = True
        TabOrder = 0
      end
      object edtBatteryFullLifeTime: TEdit
        Left = 152
        Top = 36
        Width = 61
        Height = 21
        Color = clInactiveCaptionText
        ReadOnly = True
        TabOrder = 1
      end
      object edtBatteryLineStatus: TEdit
        Left = 152
        Top = 84
        Width = 161
        Height = 21
        Color = clInactiveCaptionText
        ReadOnly = True
        TabOrder = 2
      end
      object edtBatteryFlag: TEdit
        Left = 152
        Top = 108
        Width = 161
        Height = 21
        Color = clInactiveCaptionText
        ReadOnly = True
        TabOrder = 3
      end
      object pgrsBatteryLife: TProgressBar
        Left = 152
        Top = 62
        Width = 273
        Height = 16
        Min = 0
        Max = 100
        TabOrder = 4
      end
    end
    object tabMemory: TTabSheet
      Caption = 'Memory'
      ImageIndex = 4
      object Label34: TLabel
        Left = 4
        Top = 16
        Width = 120
        Height = 13
        Caption = 'Max. application address:'
      end
      object Label35: TLabel
        Left = 4
        Top = 40
        Width = 117
        Height = 13
        Caption = 'Min. application address:'
      end
      object Label36: TLabel
        Left = 4
        Top = 64
        Width = 67
        Height = 13
        Caption = 'Memory Load:'
      end
      object Label37: TLabel
        Left = 4
        Top = 88
        Width = 67
        Height = 13
        Caption = 'Swap file size:'
      end
      object Label38: TLabel
        Left = 4
        Top = 112
        Width = 78
        Height = 13
        Caption = 'Swap file usage:'
      end
      object Label39: TLabel
        Left = 4
        Top = 136
        Width = 107
        Height = 13
        Caption = 'Total physical memory:'
      end
      object Label40: TLabel
        Left = 4
        Top = 160
        Width = 109
        Height = 13
        Caption = 'Avail. physical memory:'
      end
      object Label41: TLabel
        Left = 4
        Top = 184
        Width = 97
        Height = 13
        Caption = 'Total virtual memory:'
      end
      object Label42: TLabel
        Left = 4
        Top = 208
        Width = 99
        Height = 13
        Caption = 'Avail. virtual memory:'
      end
      object Label43: TLabel
        Left = 4
        Top = 232
        Width = 109
        Height = 13
        Caption = 'Total page file memory:'
      end
      object Label44: TLabel
        Left = 4
        Top = 256
        Width = 111
        Height = 13
        Caption = 'Avail. page file memory:'
      end
      object Bevel1: TBevel
        Left = 0
        Top = 280
        Width = 457
        Height = 9
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Label60: TLabel
        Left = 4
        Top = 288
        Width = 183
        Height = 13
        Caption = 'Windows 95/98/Me system resources:'
      end
      object LabelSysResources: TLabel
        Left = 4
        Top = 312
        Width = 94
        Height = 13
        Caption = 'LabelSysResources'
      end
      object edtMaxAppAddress: TEdit
        Left = 136
        Top = 12
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtMinAppAddress: TEdit
        Left = 136
        Top = 36
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object pgrsMemLoad: TProgressBar
        Left = 136
        Top = 62
        Width = 150
        Height = 16
        Min = 0
        Max = 100
        TabOrder = 2
      end
      object edtSwapFileSize: TEdit
        Left = 136
        Top = 84
        Width = 121
        Height = 21
        TabOrder = 3
      end
      object pgrsSwapFileUsage: TProgressBar
        Left = 136
        Top = 110
        Width = 150
        Height = 16
        Min = 0
        Max = 100
        TabOrder = 4
      end
      object edtPhysicalTotal: TEdit
        Left = 136
        Top = 132
        Width = 121
        Height = 21
        TabOrder = 5
      end
      object edtPhysicalFree: TEdit
        Left = 136
        Top = 156
        Width = 121
        Height = 21
        TabOrder = 6
      end
      object edtVirtualTotal: TEdit
        Left = 136
        Top = 180
        Width = 121
        Height = 21
        TabOrder = 7
      end
      object edtVirtualFree: TEdit
        Left = 136
        Top = 204
        Width = 121
        Height = 21
        TabOrder = 8
      end
      object edtPageFileTotal: TEdit
        Left = 136
        Top = 228
        Width = 121
        Height = 21
        TabOrder = 9
      end
      object edtPageFileFree: TEdit
        Left = 136
        Top = 252
        Width = 121
        Height = 21
        TabOrder = 10
      end
    end
    object tabKeyboard: TTabSheet
      Caption = 'Keyboard'
      ImageIndex = 5
      object Label45: TLabel
        Left = 4
        Top = 16
        Width = 78
        Height = 13
        Caption = 'Num Lock state:'
      end
      object Label46: TLabel
        Left = 4
        Top = 40
        Width = 80
        Height = 13
        Caption = 'Caps Lock state:'
      end
      object Label47: TLabel
        Left = 4
        Top = 64
        Width = 82
        Height = 13
        Caption = 'Scroll Lock state:'
      end
      object edtNumLockState: TEdit
        Left = 97
        Top = 12
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object edtCapsLockState: TEdit
        Left = 97
        Top = 36
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object edtScrollLockState: TEdit
        Left = 97
        Top = 60
        Width = 121
        Height = 21
        TabOrder = 2
      end
    end
    object tabIdentification: TTabSheet
      Caption = 'Identification'
      ImageIndex = 6
      object grpBIOS: TGroupBox
        Left = 8
        Top = 8
        Width = 449
        Height = 117
        Caption = ' BIOS '
        TabOrder = 0
        object Label48: TLabel
          Left = 18
          Top = 18
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object Label49: TLabel
          Left = 18
          Top = 42
          Width = 47
          Height = 13
          Caption = 'Copyright:'
        end
        object Label50: TLabel
          Left = 18
          Top = 66
          Width = 69
          Height = 13
          Caption = 'Extended Info:'
        end
        object Label51: TLabel
          Left = 18
          Top = 90
          Width = 26
          Height = 13
          Caption = 'Date:'
        end
        object edtBIOSName: TEdit
          Left = 93
          Top = 14
          Width = 200
          Height = 21
          TabOrder = 0
        end
        object edtBIOSCopyright: TEdit
          Left = 93
          Top = 38
          Width = 200
          Height = 21
          TabOrder = 1
        end
        object edtBIOSExtendedInfo: TEdit
          Left = 93
          Top = 62
          Width = 121
          Height = 21
          TabOrder = 2
        end
        object edtBIOSDate: TEdit
          Left = 93
          Top = 86
          Width = 96
          Height = 21
          TabOrder = 3
        end
      end
      object grpNetwork: TGroupBox
        Left = 8
        Top = 132
        Width = 449
        Height = 117
        Caption = ' Network '
        TabOrder = 1
        object Label52: TLabel
          Left = 18
          Top = 18
          Width = 54
          Height = 13
          Caption = 'IP Address:'
        end
        object Label53: TLabel
          Left = 18
          Top = 42
          Width = 67
          Height = 13
          Caption = 'MAC Address:'
        end
        object Label54: TLabel
          Left = 18
          Top = 90
          Width = 39
          Height = 13
          Caption = 'Domain:'
        end
        object edtIPAddress: TEdit
          Left = 93
          Top = 14
          Width = 200
          Height = 21
          TabOrder = 0
        end
        object lbMACAddresses: TListBox
          Left = 92
          Top = 40
          Width = 201
          Height = 42
          ItemHeight = 13
          TabOrder = 1
        end
        object edtDomain: TEdit
          Left = 93
          Top = 86
          Width = 200
          Height = 21
          TabOrder = 2
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 256
        Width = 449
        Height = 97
        Caption = ' User '
        TabOrder = 2
        object Label56: TLabel
          Left = 18
          Top = 18
          Width = 31
          Height = 13
          Caption = 'Name:'
        end
        object Label57: TLabel
          Left = 18
          Top = 42
          Width = 73
          Height = 13
          Caption = 'Reg. Company:'
        end
        object Label58: TLabel
          Left = 18
          Top = 66
          Width = 60
          Height = 13
          Caption = 'Reg. Owner:'
        end
        object edtUserName: TEdit
          Left = 93
          Top = 14
          Width = 200
          Height = 21
          TabOrder = 0
        end
        object edtRegisteredCompany: TEdit
          Left = 93
          Top = 38
          Width = 200
          Height = 21
          TabOrder = 1
        end
        object edtRegisteredOwner: TEdit
          Left = 93
          Top = 62
          Width = 200
          Height = 21
          TabOrder = 2
        end
      end
    end
    object tabProcesses: TTabSheet
      Caption = 'Processes'
      ImageIndex = 7
      object Label55: TLabel
        Left = 8
        Top = 8
        Width = 95
        Height = 13
        Caption = 'Running Processes:'
      end
      object Label59: TLabel
        Left = 8
        Top = 208
        Width = 75
        Height = 13
        Caption = 'Running Tasks:'
      end
      object lbProcesses: TListBox
        Left = 8
        Top = 24
        Width = 445
        Height = 169
        ItemHeight = 13
        TabOrder = 0
      end
      object TasksListBox: TListBox
        Left = 8
        Top = 224
        Width = 445
        Height = 121
        ItemHeight = 13
        TabOrder = 1
      end
    end
  end
  object btnUpdate: TButton
    Left = 396
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Update'
    TabOrder = 1
    OnClick = btnUpdateClick
  end
  object btnOk: TButton
    Left = 316
    Top = 400
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
  end
end
