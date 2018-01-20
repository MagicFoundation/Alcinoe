object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SRV.mORMot REST test'
  ClientHeight = 661
  ClientWidth = 1084
  Color = clBtnFace
  Constraints.MinHeight = 325
  Constraints.MinWidth = 1100
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1084
    661)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPortCap: TLabel
    Left = 867
    Top = 28
    Width = 24
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Port:'
    ExplicitLeft = 866
  end
  object LabelAuthenticationMode: TLabel
    Left = 37
    Top = 38
    Width = 74
    Height = 13
    Caption = 'Authentication:'
  end
  object LabelProtocol: TLabel
    Left = 68
    Top = 11
    Width = 43
    Height = 13
    Caption = 'Protocol:'
  end
  object LabelHTTPSnote: TLabel
    Left = 511
    Top = 11
    Width = 235
    Height = 13
    Caption = 'Don'#39't forget enable https support (check exe dir)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 136
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
    StyleElements = [seClient, seBorder]
  end
  object EditPort: TEdit
    Left = 897
    Top = 25
    Width = 60
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 0
    Text = '777'
  end
  object ButtonStartStop: TButton
    Left = 969
    Top = 19
    Width = 105
    Height = 33
    Anchors = [akTop, akRight]
    Caption = 'Start server'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = ButtonStartStopClick
  end
  object MemoLog: TMemo
    Left = 8
    Top = 255
    Width = 1066
    Height = 370
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ButtonCLS: TButton
    Left = 10
    Top = 631
    Width = 38
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'CLS'
    TabOrder = 3
    OnClick = ButtonCLSClick
  end
  object CheckBoxAutoScroll: TCheckBox
    Left = 57
    Top = 633
    Width = 69
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'auto scroll'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ComboBoxAuthentication: TComboBox
    Left = 117
    Top = 35
    Width = 388
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 5
    Text = 'Default'
    OnChange = ComboBoxAuthenticationChange
    Items.Strings = (
      'No authentication'
      'Default'
      'None'
      'HttpBasic'
      'SSPI')
  end
  object ButtonShowAuthorizationInfo: TButton
    Left = 511
    Top = 36
    Width = 42
    Height = 19
    Caption = 'Info'
    TabOrder = 6
    OnClick = ButtonShowAuthorizationInfoClick
  end
  object CheckBoxDisableLog: TCheckBox
    Left = 133
    Top = 633
    Width = 203
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'disable log (for max performance test)'
    TabOrder = 7
    OnClick = CheckBoxDisableLogClick
  end
  object ComboBoxProtocol: TComboBox
    Left = 117
    Top = 8
    Width = 388
    Height = 21
    Style = csDropDownList
    DropDownCount = 10
    ItemIndex = 7
    TabOrder = 8
    Text = ' '#8250' WebSocket ( bidirectional, binary + AES-CFB 256)'
    OnChange = ComboBoxProtocolChange
    Items.Strings = (
      'HTTP              ( socket )'
      
        ' '#8250' HTTP           ( fast http.sys // require admin rights else s' +
        'ocket )'
      ' '#8250#8250' HTTPS        ( fast http.sys + SSL // require admin rights )'
      
        ' '#8250' HTTP           ( fast http.sys + AES-CFB 256 // require admin' +
        ' rights )'
      'HTTP              ( web socket )'
      ' '#8250' WebSocket ( bidirectional, JSON )'
      ' '#8250' WebSocket ( bidirectional, binary )'
      ' '#8250' WebSocket ( bidirectional, binary + AES-CFB 256)'
      'Named pipe')
  end
  object GroupBoxMethodGroupConfiguration: TGroupBox
    Left = 164
    Top = 62
    Width = 520
    Height = 187
    Caption = 'Method / Group configuration'
    TabOrder = 9
    object ListViewMethodGroups: TListView
      Left = 10
      Top = 16
      Width = 503
      Height = 133
      Columns = <
        item
          Caption = ' Method'
          Width = 150
        end
        item
          Caption = ' Allow group by name'
          Width = 160
        end
        item
          Caption = ' Deny group by name'
          Width = 160
        end>
      Groups = <
        item
          Header = 'IRestMethods'
          GroupID = 0
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          Subtitle = 'InstanceImplementation = sicSingle'
          TitleImage = -1
        end>
      HideSelection = False
      Items.ItemData = {
        05860200000600000000000000FFFFFFFFFFFFFFFF0200000000000000000000
        000A480065006C006C006F0057006F0072006C00640014550073006500720073
        002C00410064006D0069006E006900730074007200610074006F007200730068
        0674240B53006F006D0065006F006E00650045006C007300650068FF73240000
        0000FFFFFFFFFFFFFFFF02000000000000000000000003530075006D00145500
        73006500720073002C00410064006D0069006E00690073007400720061007400
        6F0072007300686F7424008054742400000000FFFFFFFFFFFFFFFF0200000000
        000000000000000F47006500740043007500730074006F006D00520065006300
        6F00720064000E410064006D0069006E006900730074007200610074006F0072
        0073005013742400985C742400000000FFFFFFFFFFFFFFFF0200000000000000
        0000000010530065006E00640043007500730074006F006D005200650063006F
        00720064000E410064006D0069006E006900730074007200610074006F007200
        7300588F7424004041742400000000FFFFFFFFFFFFFFFF020000000000000000
        00000019530065006E0064004D0075006C007400690070006C00650043007500
        730074006F006D005200650063006F007200640073000E410064006D0069006E
        006900730074007200610074006F00720073003812742400E887742400000000
        FFFFFFFFFFFFFFFF020000000000000000000000154700650074004D00650074
        0068006F00640043007500730074006F006D0052006500730075006C00740014
        550073006500720073002C00410064006D0069006E0069007300740072006100
        74006F0072007300B08774240010387424FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF}
      GroupView = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ListViewMethodGroupsClick
      OnSelectItem = ListViewMethodGroupsSelectItem
    end
    object ButtonSaveRoleConfiguration: TButton
      Left = 463
      Top = 155
      Width = 50
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = ButtonSaveRoleConfigurationClick
    end
    object EditAllowGroupNames: TEdit
      Left = 136
      Top = 157
      Width = 160
      Height = 21
      TabOrder = 2
      TextHint = 'Allow group names  (sep by ",")'
    end
    object EditDenyAllowGroupNames: TEdit
      Left = 298
      Top = 157
      Width = 159
      Height = 21
      TabOrder = 3
      TextHint = 'Deny group names (sep by ",")'
    end
  end
  object RadioGroupAuthorizationPolicy: TRadioGroup
    Left = 8
    Top = 62
    Width = 150
    Height = 73
    Caption = 'Authorization policy'
    ItemIndex = 2
    Items.Strings = (
      'Allow all'
      'Deny all'
      'Follow groups settings')
    TabOrder = 10
    OnClick = RadioGroupAuthorizationPolicyClick
  end
  object GroupBoxUsers: TGroupBox
    Left = 690
    Top = 62
    Width = 383
    Height = 187
    Caption = 'Users'
    TabOrder = 11
    object ListViewUsers: TListView
      Left = 9
      Top = 16
      Width = 365
      Height = 133
      Columns = <
        item
          Caption = ' User'
          Width = 110
        end
        item
          Caption = ' Password'
          Width = 110
        end
        item
          Caption = ' Group'
          Width = 110
        end>
      HideSelection = False
      Items.ItemData = {
        05960000000200000000000000FFFFFFFFFFFFFFFF02000000FFFFFFFF000000
        0006470065006F0072006700650003310032003300B8EB761B0E410064006D00
        69006E006900730074007200610074006F007200730058EE761B00000000FFFF
        FFFFFFFFFFFF02000000FFFFFFFF000000000441006C00650078000333003200
        310090EE761B0555007300650072007300C8EE761BFFFFFFFFFFFFFFFF}
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ListViewUsersClick
      OnSelectItem = ListViewUsersSelectItem
    end
    object EditUserGroup: TEdit
      Left = 223
      Top = 157
      Width = 83
      Height = 21
      TabOrder = 1
      TextHint = 'Group'
    end
    object ButtonSaveUsers: TButton
      Left = 312
      Top = 155
      Width = 62
      Height = 25
      Caption = 'Save/Add'
      TabOrder = 2
      OnClick = ButtonSaveUsersClick
    end
    object EditUserName: TEdit
      Left = 53
      Top = 157
      Width = 83
      Height = 21
      TabOrder = 3
      TextHint = 'User'
    end
    object ButtonDeleteUser: TButton
      Left = 9
      Top = 155
      Width = 38
      Height = 25
      Caption = 'DeL'
      TabOrder = 4
      OnClick = ButtonDeleteUserClick
    end
    object EditUserPassword: TEdit
      Left = 138
      Top = 157
      Width = 83
      Height = 21
      TabOrder = 5
      TextHint = 'Password'
    end
  end
  object TimerRefreshLogMemo: TTimer
    OnTimer = TimerRefreshLogMemoTimer
    Left = 56
    Top = 264
  end
end
