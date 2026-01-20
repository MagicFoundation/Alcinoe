object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ALPOP3Client Demo'
  ClientHeight = 408
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 347
    Height = 226
    Align = alTop
    Caption = 'Actions'
    TabOrder = 0
    ExplicitWidth = 343
    object Label1: TLabel
      Left = 16
      Top = 80
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label2: TLabel
      Left = 141
      Top = 80
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Label3: TLabel
      Left = 16
      Top = 24
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object Label4: TLabel
      Left = 165
      Top = 24
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object Label5: TLabel
      Left = 13
      Top = 173
      Width = 95
      Height = 13
      Caption = 'Number of message'
    end
    object UsernameEdit: TEdit
      Left = 13
      Top = 99
      Width = 116
      Height = 21
      TabOrder = 0
      Text = 'alcinoe.alcinoe@yahoo.com'
    end
    object PasswordEdit: TEdit
      Left = 138
      Top = 99
      Width = 114
      Height = 21
      TabOrder = 1
      Text = 'alcinoe2012'
    end
    object LoginButton: TButton
      Left = 258
      Top = 97
      Width = 63
      Height = 25
      Caption = 'Login'
      TabOrder = 2
      OnClick = LoginButtonClick
    end
    object HostEdit: TEdit
      Left = 13
      Top = 43
      Width = 140
      Height = 21
      TabOrder = 3
      Text = 'pop.mail.yahoo.com'
    end
    object PortEdit: TEdit
      Left = 159
      Top = 43
      Width = 42
      Height = 21
      TabOrder = 4
      Text = '110'
    end
    object ConnectButton: TButton
      Left = 207
      Top = 41
      Width = 114
      Height = 25
      Caption = 'Connect'
      TabOrder = 5
      OnClick = ConnectButtonClick
    end
    object StatButton: TButton
      Left = 13
      Top = 142
      Width = 51
      Height = 25
      Caption = 'STAT'
      TabOrder = 6
      OnClick = StatButtonClick
    end
    object ListButton: TButton
      Left = 70
      Top = 142
      Width = 51
      Height = 25
      Caption = 'LIST'
      TabOrder = 7
      OnClick = ListButtonClick
    end
    object QuitButton: TButton
      Left = 184
      Top = 142
      Width = 51
      Height = 25
      Caption = 'QUIT'
      TabOrder = 8
      OnClick = QuitButtonClick
    end
    object RsetButton: TButton
      Left = 241
      Top = 142
      Width = 52
      Height = 25
      Caption = 'RSET'
      TabOrder = 9
      OnClick = RsetButtonClick
    end
    object NumberEdit: TEdit
      Left = 13
      Top = 192
      Width = 121
      Height = 21
      TabOrder = 10
    end
    object RetrButton: TButton
      Left = 140
      Top = 190
      Width = 45
      Height = 25
      Caption = 'RETR'
      TabOrder = 11
      OnClick = RetrButtonClick
    end
    object DeleButton: TButton
      Left = 191
      Top = 190
      Width = 45
      Height = 25
      Caption = 'DELE'
      TabOrder = 12
      OnClick = DeleButtonClick
    end
    object UIDLButton: TButton
      Left = 127
      Top = 142
      Width = 51
      Height = 25
      Caption = 'UIDL'
      TabOrder = 13
      OnClick = UIDLButtonClick
    end
  end
  object MemoDebug: TMemo
    Left = 0
    Top = 226
    Width = 347
    Height = 182
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 343
    ExplicitHeight = 181
  end
end
