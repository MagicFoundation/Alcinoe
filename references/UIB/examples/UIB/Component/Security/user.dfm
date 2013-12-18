object UserForm: TUserForm
  Left = 461
  Top = 225
  Width = 282
  Height = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 14
    Width = 53
    Height = 13
    Caption = 'User Name'
  end
  object Label2: TLabel
    Left = 10
    Top = 39
    Width = 50
    Height = 13
    Caption = 'First Name'
  end
  object Label3: TLabel
    Left = 10
    Top = 65
    Width = 62
    Height = 13
    Caption = 'Middle Name'
  end
  object Label4: TLabel
    Left = 10
    Top = 91
    Width = 51
    Height = 13
    Caption = 'Last Name'
  end
  object Label5: TLabel
    Left = 10
    Top = 116
    Width = 36
    Height = 13
    Caption = 'User ID'
  end
  object Label6: TLabel
    Left = 10
    Top = 142
    Width = 43
    Height = 13
    Caption = 'Group ID'
  end
  object Label7: TLabel
    Left = 10
    Top = 168
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label8: TLabel
    Left = 10
    Top = 194
    Width = 35
    Height = 13
    Caption = 'Confirm'
  end
  object UserName: TEdit
    Left = 78
    Top = 10
    Width = 187
    Height = 21
    TabOrder = 0
  end
  object FirstName: TEdit
    Left = 78
    Top = 35
    Width = 187
    Height = 21
    TabOrder = 1
  end
  object MiddleName: TEdit
    Left = 78
    Top = 61
    Width = 187
    Height = 21
    TabOrder = 2
  end
  object LastName: TEdit
    Left = 78
    Top = 87
    Width = 187
    Height = 21
    TabOrder = 3
  end
  object UserID: TEdit
    Left = 78
    Top = 112
    Width = 187
    Height = 21
    TabOrder = 4
  end
  object GroupID: TEdit
    Left = 78
    Top = 138
    Width = 187
    Height = 21
    TabOrder = 5
  end
  object Password: TEdit
    Left = 78
    Top = 164
    Width = 187
    Height = 21
    PasswordChar = '*'
    TabOrder = 6
  end
  object Confirm: TEdit
    Left = 78
    Top = 190
    Width = 187
    Height = 21
    PasswordChar = '*'
    TabOrder = 7
  end
  object Ok: TButton
    Left = 110
    Top = 222
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object Cancel: TButton
    Left = 190
    Top = 222
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
end
