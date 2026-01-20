object Form1: TForm1
  Left = 485
  Top = 214
  Caption = 'SMTP Test'
  ClientHeight = 695
  ClientWidth = 857
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object MsgMemo: TMemo
    Left = 0
    Top = 241
    Width = 857
    Height = 120
    Hint = 'Enter the message text in this memo'
    Align = alTop
    EditMargins.Left = 6
    EditMargins.Right = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
    ExplicitWidth = 800
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 456
    Width = 857
    Height = 239
    Hint = 'This memo shows info messages'
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Color = clMenu
    Ctl3D = False
    EditMargins.Left = 6
    EditMargins.Right = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ParentShowHint = False
    ReadOnly = True
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 2
    ExplicitWidth = 800
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 857
    Height = 241
    Align = alTop
    TabOrder = 3
    ExplicitWidth = 800
    object Label1: TLabel
      Left = 23
      Top = 19
      Width = 69
      Height = 16
      Caption = 'SMTP Host'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 61
      Top = 49
      Width = 31
      Height = 16
      Caption = 'From'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 361
      Top = 49
      Width = 17
      Height = 16
      Caption = 'To'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Subject: TLabel
      Left = 47
      Top = 105
      Width = 45
      Height = 16
      Caption = 'Subject'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 354
      Top = 19
      Width = 24
      Height = 16
      Caption = 'Port'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 12
      Top = 220
      Width = 83
      Height = 16
      Caption = 'Message text:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 29
      Top = 136
      Width = 63
      Height = 16
      Caption = 'Username'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 347
      Top = 136
      Width = 31
      Height = 16
      Caption = 'Pass'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 9
      Top = 165
      Width = 83
      Height = 16
      Caption = 'Authentication'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 76
      Top = 78
      Width = 16
      Height = 16
      Caption = 'Cc'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 355
      Top = 78
      Width = 23
      Height = 16
      Caption = 'Bcc'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 311
      Top = 105
      Width = 67
      Height = 16
      Caption = 'Importance'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object HostEdit: TEdit
      Left = 100
      Top = 16
      Width = 197
      Height = 24
      Hint = 'Mail server hostname or IP address'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'smtp.gmail.com'
    end
    object FromEdit: TEdit
      Left = 100
      Top = 45
      Width = 197
      Height = 24
      Hint = 'Author'#39's EMail'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Text = 'alsmtpclientdemo@gmail.com'
    end
    object ToEdit: TEdit
      Left = 387
      Top = 45
      Width = 197
      Height = 24
      Hint = 'Destinators, delimited by semicolons'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
    object SubjectEdit: TEdit
      Left = 100
      Top = 103
      Width = 197
      Height = 24
      Hint = 'Message subject'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      Text = 'Test'
    end
    object PortEdit: TEdit
      Left = 387
      Top = 16
      Width = 197
      Height = 24
      Hint = 'Mail server port (should be smtp)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '465'
    end
    object ConnectPlainButton: TButton
      Left = 614
      Top = 24
      Width = 115
      Height = 20
      Hint = 'Connect to the mail server'
      Caption = 'Connect (Plain)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 12
      OnClick = ConnectPlainButtonClick
    end
    object HeloButton: TButton
      Left = 614
      Top = 74
      Width = 115
      Height = 20
      Hint = 'Send the signon message'
      Caption = 'Helo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 13
      OnClick = HeloButtonClick
    end
    object MailFromButton: TButton
      Left = 735
      Top = 49
      Width = 96
      Height = 20
      Hint = 'Send the mail originator'
      Caption = 'MailFrom'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 16
      OnClick = MailFromButtonClick
    end
    object RcptToButton: TButton
      Left = 735
      Top = 74
      Width = 96
      Height = 20
      Hint = 'Send the mail recipents'
      Caption = 'RcptTo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 17
      OnClick = RcptToButtonClick
    end
    object DataButton: TButton
      Left = 735
      Top = 99
      Width = 96
      Height = 20
      Hint = 'Send mail text and attached files'
      Caption = 'Data'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 18
      OnClick = DataButtonClick
    end
    object QuitButton: TButton
      Left = 735
      Top = 124
      Width = 96
      Height = 20
      Hint = 'Quit mail server'
      Caption = 'Quit'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 19
      OnClick = QuitButtonClick
    end
    object UsernameEdit: TEdit
      Left = 100
      Top = 132
      Width = 197
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      Text = 'alsmtpclientdemo@gmail.com'
    end
    object PasswordEdit: TEdit
      Left = 387
      Top = 132
      Width = 197
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 9
      Text = 'bnjn fovp etjq biwz'
    end
    object AuthComboBox: TComboBox
      Left = 100
      Top = 161
      Width = 197
      Height = 24
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 10
      Text = 'Plain'
      Items.Strings = (
        'Plain'
        'Login')
    end
    object EhloButton: TButton
      Left = 614
      Top = 99
      Width = 115
      Height = 20
      Caption = 'Ehlo'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 14
      OnClick = EhloButtonClick
    end
    object AuthButton: TButton
      Left = 735
      Top = 24
      Width = 96
      Height = 20
      Caption = 'Auth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 15
      OnClick = AuthButtonClick
    end
    object CcEdit: TEdit
      Left = 100
      Top = 74
      Width = 197
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
    object BccEdit: TEdit
      Left = 387
      Top = 74
      Width = 197
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
    end
    object PriorityComboBox: TComboBox
      Left = 387
      Top = 103
      Width = 197
      Height = 24
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemIndex = 1
      ParentFont = False
      TabOrder = 7
      Text = 'normal'
      Items.Strings = (
        'low'
        'normal'
        'high')
    end
    object ConfirmCheckBox: TCheckBox
      Left = 387
      Top = 165
      Width = 102
      Height = 17
      Caption = 'Confirm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
    object ConnectTslButton: TButton
      Left = 614
      Top = 49
      Width = 115
      Height = 20
      Hint = 'Connect to the mail server'
      Caption = 'Connect (TSL)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 20
      OnClick = ConnectTslButtonClick
    end
    object StartTLSButton: TButton
      Left = 614
      Top = 124
      Width = 115
      Height = 20
      Caption = 'StartTLS'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 21
      OnClick = StartTLSButtonClick
    end
  end
  object AttachPanel: TPanel
    Left = 0
    Top = 361
    Width = 857
    Height = 23
    Align = alTop
    TabOrder = 4
    ExplicitWidth = 800
    object Label6: TLabel
      Left = 16
      Top = 2
      Width = 83
      Height = 16
      Caption = 'Attached files:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object FileAttachMemo: TMemo
    Left = 0
    Top = 384
    Width = 857
    Height = 49
    Hint = 'Enter the attached file path, one per line'
    Align = alTop
    EditMargins.Left = 6
    EditMargins.Right = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 1
    ExplicitWidth = 800
  end
  object InfoPanel: TPanel
    Left = 0
    Top = 433
    Width = 857
    Height = 23
    Align = alTop
    TabOrder = 5
    ExplicitWidth = 800
    object Label7: TLabel
      Left = 16
      Top = 2
      Width = 91
      Height = 16
      Caption = 'Info messages:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
end
