object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 567
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 18
    Top = 16
    Width = 89
    Height = 13
    Caption = 'Firebird Version'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 18
    Top = 370
    Width = 37
    Height = 13
    Caption = 'Result'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 75
    Top = 103
    Width = 32
    Height = 13
    Caption = 'Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 51
    Top = 132
    Width = 55
    Height = 13
    Caption = 'Password'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label15: TLabel
    Left = 63
    Top = 161
    Width = 44
    Height = 13
    Caption = 'Charset'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label16: TLabel
    Left = 32
    Top = 45
    Width = 75
    Height = 13
    Caption = 'FBClient DLL'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label18: TLabel
    Left = 52
    Top = 76
    Width = 55
    Height = 13
    Caption = 'Database'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label30: TLabel
    Left = 34
    Top = 190
    Width = 72
    Height = 13
    Caption = 'Num_buffers'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 18
    Top = 222
    Width = 76
    Height = 13
    Caption = 'Event Names'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ALButton1: TButton
    Left = 18
    Top = 332
    Width = 121
    Height = 25
    Caption = 'Start Listenig'
    TabOrder = 8
    OnClick = ALButton1Click
  end
  object ALButton2: TButton
    Left = 145
    Top = 332
    Width = 225
    Height = 25
    Caption = 'Stop Listening'
    TabOrder = 9
    OnClick = ALButton2Click
  end
  object ALComboBoxFirebirdapiVer: TComboBox
    Left = 113
    Top = 13
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'FB102'
    Items.Strings = (
      'FB102'
      'FB103'
      'FB15'
      'FB20'
      'FB25')
  end
  object ALMemoResult: TMemo
    Left = 18
    Top = 386
    Width = 353
    Height = 169
    TabStop = False
    TabOrder = 10
  end
  object ALEditFirebirdLogin: TEdit
    Left = 113
    Top = 100
    Width = 249
    Height = 21
    TabOrder = 3
    Text = 'SYSDBA'
  end
  object ALEditFirebirdPassword: TEdit
    Left = 113
    Top = 129
    Width = 249
    Height = 21
    TabOrder = 4
  end
  object ALEditFirebirdCharset: TEdit
    Left = 113
    Top = 158
    Width = 72
    Height = 21
    TabOrder = 5
    Text = 'NONE'
  end
  object ALEditFirebirdLib: TEdit
    Left = 113
    Top = 42
    Width = 222
    Height = 21
    TabOrder = 1
    Text = 'FBClient.dll'
  end
  object ALEditFirebirdDatabase: TEdit
    Left = 113
    Top = 71
    Width = 222
    Height = 21
    Cursor = crArrow
    TabOrder = 2
  end
  object ALEditFireBirdNum_buffers: TEdit
    Left = 113
    Top = 187
    Width = 46
    Height = 21
    TabOrder = 6
    Text = '0'
  end
  object ALMemoFireBirdEventName: TMemo
    Left = 18
    Top = 241
    Width = 353
    Height = 79
    TabOrder = 7
  end
  object Button2: TButton
    Left = 335
    Top = 71
    Width = 27
    Height = 21
    Caption = '...'
    TabOrder = 11
    OnClick = ALEditFirebirdDatabaseButtonClick
  end
  object Button1: TButton
    Left = 335
    Top = 42
    Width = 27
    Height = 21
    Caption = '...'
    TabOrder = 12
    OnClick = ALEditFirebirdLibButtonClick
  end
  object OpenDialog1: TOpenDialog
    Left = 250
    Top = 208
  end
  object OpenDialog2: TOpenDialog
    Left = 250
    Top = 152
  end
end
