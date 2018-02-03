object Form1: TForm1
  Left = 386
  Top = 230
  ClientWidth = 390
  ClientHeight = 344
  Caption = 'CreateProcAsUser Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Domain:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 136
    Top = 8
    Width = 61
    Height = 13
    Caption = 'Username:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 264
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Password:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 52
    Width = 83
    Height = 13
    Caption = 'Command line:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 8
    Top = 104
    Width = 75
    Height = 13
    Caption = 'Environment:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object edtDomain: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object edtUserName: TEdit
    Left = 136
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object edtPassword: TEdit
    Left = 264
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object edtCommandLine: TEdit
    Left = 8
    Top = 68
    Width = 377
    Height = 21
    TabOrder = 3
  end
  object btnCreateProcAsUser: TButton
    Left = 8
    Top = 312
    Width = 129
    Height = 25
    Caption = 'Create Process As User'
    Default = True
    TabOrder = 4
    OnClick = btnCreateProcAsUserClick
  end
  object btnCreateProcAsUserEx: TButton
    Left = 148
    Top = 312
    Width = 145
    Height = 25
    Caption = 'Create Process As User Ex'
    TabOrder = 5
    OnClick = btnCreateProcAsUserExClick
  end
  object lbEnvironment: TListBox
    Left = 8
    Top = 120
    Width = 193
    Height = 141
    ItemHeight = 13
    TabOrder = 6
  end
  object edtEnvString: TEdit
    Left = 216
    Top = 120
    Width = 165
    Height = 21
    TabOrder = 7
  end
  object btnAddEnvString: TButton
    Left = 216
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 8
    OnClick = btnAddEnvStringClick
  end
  object btnRemoveEnvString: TButton
    Left = 216
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 9
    OnClick = btnRemoveEnvStringClick
  end
  object btnClearEnvStrings: TButton
    Left = 216
    Top = 204
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 10
    OnClick = btnClearEnvStringsClick
  end
  object chkEnvAdditional: TCheckBox
    Left = 216
    Top = 236
    Width = 97
    Height = 17
    Caption = 'Additional'
    TabOrder = 11
  end
  object chkEnvCurrentUser: TCheckBox
    Left = 216
    Top = 256
    Width = 97
    Height = 17
    Caption = 'Current User'
    TabOrder = 12
  end
  object chkEnvLocalMachine: TCheckBox
    Left = 216
    Top = 276
    Width = 97
    Height = 17
    Caption = 'Local Machine'
    TabOrder = 13
  end
end
