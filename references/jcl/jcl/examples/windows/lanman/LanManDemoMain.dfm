object Form1: TForm1
  Left = 339
  Top = 230
  ClientWidth = 716
  ClientHeight = 390
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 205
    Caption = 'User'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 54
      Height = 13
      Caption = 'User name:'
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 49
      Height = 13
      Caption = 'Password:'
    end
    object Label3: TLabel
      Left = 16
      Top = 96
      Width = 44
      Height = 13
      Caption = 'Comment'
    end
    object Label4: TLabel
      Left = 16
      Top = 144
      Width = 30
      Height = 13
      Caption = 'Script:'
    end
    object Label5: TLabel
      Left = 16
      Top = 168
      Width = 34
      Height = 13
      Caption = 'Server:'
    end
    object Label6: TLabel
      Left = 16
      Top = 120
      Width = 44
      Height = 13
      Caption = 'HomeDir:'
    end
    object Label11: TLabel
      Left = 16
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Full Name:'
    end
    object edtUserName: TEdit
      Left = 80
      Top = 20
      Width = 165
      Height = 21
      TabOrder = 0
    end
    object edtPassword: TEdit
      Left = 80
      Top = 68
      Width = 165
      Height = 21
      TabOrder = 1
    end
    object edtComment: TEdit
      Left = 80
      Top = 92
      Width = 165
      Height = 21
      TabOrder = 2
    end
    object edtScript: TEdit
      Left = 80
      Top = 140
      Width = 165
      Height = 21
      TabOrder = 3
    end
    object edtServer: TEdit
      Left = 80
      Top = 164
      Width = 165
      Height = 21
      TabOrder = 4
    end
    object edtHomedir: TEdit
      Left = 80
      Top = 116
      Width = 165
      Height = 21
      TabOrder = 5
    end
    object btnAddUser: TButton
      Left = 262
      Top = 20
      Width = 75
      Height = 25
      Caption = '&Add'
      TabOrder = 6
      OnClick = btnAddUserClick
    end
    object btnDeleteUser: TButton
      Left = 262
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 7
      OnClick = btnDeleteUserClick
    end
    object edtFullName: TEdit
      Left = 80
      Top = 44
      Width = 165
      Height = 21
      TabOrder = 8
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 220
    Width = 257
    Height = 161
    Caption = 'Group Information'
    TabOrder = 1
    object Label7: TLabel
      Left = 16
      Top = 24
      Width = 21
      Height = 13
      Caption = 'SID:'
    end
    object Label8: TLabel
      Left = 16
      Top = 48
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object edtSIDName: TEdit
      Left = 80
      Top = 44
      Width = 165
      Height = 21
      TabOrder = 0
    end
    object cboSID: TComboBox
      Left = 80
      Top = 20
      Width = 165
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnChange = cboSIDChange
      Items.Strings = (
        'DOMAIN_ALIAS_RID_ADMINS'
        'DOMAIN_ALIAS_RID_USERS'
        'DOMAIN_ALIAS_RID_GUESTS'
        'DOMAIN_ALIAS_RID_POWER_USERS'
        'DOMAIN_ALIAS_RID_BACKUP_OPS'
        'DOMAIN_ALIAS_RID_REPLICATOR'
        'SECURITY_WORLD_RID')
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 72
      Width = 229
      Height = 73
      Caption = 'System'
      TabOrder = 2
      object rbLocal: TRadioButton
        Left = 12
        Top = 20
        Width = 113
        Height = 17
        Caption = 'Local'
        TabOrder = 0
      end
      object rbRemote: TRadioButton
        Left = 12
        Top = 42
        Width = 17
        Height = 17
        TabOrder = 1
      end
      object edtSystemName: TEdit
        Left = 32
        Top = 40
        Width = 181
        Height = 21
        TabOrder = 2
      end
    end
  end
  object GroupBox4: TGroupBox
    Left = 360
    Top = 8
    Width = 349
    Height = 89
    Caption = 'Group'
    TabOrder = 2
    object Label9: TLabel
      Left = 16
      Top = 24
      Width = 61
      Height = 13
      Caption = 'Group name:'
    end
    object Label10: TLabel
      Left = 16
      Top = 52
      Width = 44
      Height = 13
      Caption = 'Comment'
    end
    object edtGroupName: TEdit
      Left = 84
      Top = 20
      Width = 165
      Height = 21
      TabOrder = 0
    end
    object btnAddGroup: TButton
      Left = 262
      Top = 20
      Width = 75
      Height = 25
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddGroupClick
    end
    object btnDeleteGroup: TButton
      Left = 262
      Top = 52
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteGroupClick
    end
    object edtGroupComment: TEdit
      Left = 84
      Top = 48
      Width = 165
      Height = 21
      TabOrder = 3
    end
  end
end
