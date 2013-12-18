object UIBDatabaseEditForm: TUIBDatabaseEditForm
  Left = 442
  Top = 340
  AutoSize = True
  BorderStyle = bsToolWindow
  Caption = 'UIB Database Editor'
  ClientHeight = 341
  ClientWidth = 344
  Color = clBtnFace
  Constraints.MinHeight = 330
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object UserNameLbl: TLabel
    Left = 8
    Top = 152
    Width = 53
    Height = 13
    Caption = '&User Name'
  end
  object PasswordLbl: TLabel
    Left = 8
    Top = 192
    Width = 46
    Height = 13
    Caption = 'Pass&word'
  end
  object SQLRoleLbl: TLabel
    Left = 8
    Top = 232
    Width = 46
    Height = 13
    Caption = 'SQL &Role'
  end
  object CharacterSetLbl: TLabel
    Left = 8
    Top = 272
    Width = 65
    Height = 13
    Caption = '&Character Set'
  end
  object Connection: TGroupBox
    Left = 0
    Top = 0
    Width = 343
    Height = 145
    Caption = 'Connection'
    TabOrder = 0
    object ModeLbl: TLabel
      Left = 8
      Top = 16
      Width = 27
      Height = 13
      Caption = '&Mode'
    end
    object ServerLbl: TLabel
      Left = 128
      Top = 16
      Width = 31
      Height = 13
      Caption = '&Server'
    end
    object PortLbl: TLabel
      Left = 270
      Top = 16
      Width = 19
      Height = 13
      Caption = '&Port'
    end
    object DatabaseLbl: TLabel
      Left = 8
      Top = 56
      Width = 75
      Height = 13
      Caption = '&Database name'
    end
    object LibraryNameLbl: TLabel
      Left = 8
      Top = 96
      Width = 60
      Height = 13
      Caption = '&Library name'
    end
    object Mode: TComboBox
      Left = 8
      Top = 32
      Width = 113
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ModeChange
      Items.Strings = (
        '[Local]'
        'TCP/IP'
        'NamedPipe'
        'IPX/SPX')
    end
    object ServerName: TEdit
      Left = 128
      Top = 32
      Width = 135
      Height = 21
      TabOrder = 1
    end
    object DatabaseName: TEdit
      Left = 8
      Top = 72
      Width = 295
      Height = 21
      TabOrder = 3
    end
    object PortName: TEdit
      Left = 270
      Top = 32
      Width = 57
      Height = 21
      TabOrder = 2
    end
    object Browse: TButton
      Left = 303
      Top = 72
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 4
      OnClick = BrowseClick
    end
    object LibraryName: TEdit
      Left = 8
      Top = 112
      Width = 295
      Height = 21
      TabOrder = 5
    end
    object BrowseLib: TButton
      Left = 303
      Top = 112
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 6
      OnClick = BrowseLibClick
    end
  end
  object OkBtn: TButton
    Left = 8
    Top = 316
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 6
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 262
    Top = 316
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object Parametters: TMemo
    Left = 144
    Top = 152
    Width = 199
    Height = 157
    TabOrder = 5
    OnExit = ParamettersExit
  end
  object UserName: TEdit
    Left = 8
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 1
    OnChange = UserNameChange
    OnExit = UserNameChange
  end
  object Password: TEdit
    Left = 8
    Top = 208
    Width = 121
    Height = 21
    TabOrder = 2
    OnChange = PasswordChange
    OnExit = PasswordChange
  end
  object SQLRole: TEdit
    Left = 8
    Top = 248
    Width = 121
    Height = 21
    TabOrder = 3
    OnChange = SQLRoleChange
    OnExit = SQLRoleChange
  end
  object CharacterSet: TComboBox
    Left = 8
    Top = 288
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 0
    TabOrder = 4
    OnChange = CharacterSetChange
    OnExit = CharacterSetChange
  end
  object Test: TButton
    Left = 136
    Top = 316
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 7
    OnClick = TestClick
  end
  object OpenDialog: TOpenDialog
    Left = 152
    Top = 160
  end
end
