object HTTPServerForm: THTTPServerForm
  Left = 148
  Top = 129
  BorderStyle = bsDialog
  Caption = ' SynDBExplorer HTTP Server'
  ClientHeight = 165
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbledtPort: TLabeledEdit
    Left = 40
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'HTTP Port'
    TabOrder = 0
    Text = '8092'
    OnKeyPress = lbledtPortKeyPress
  end
  object lbledtDatabase: TLabeledEdit
    Left = 176
    Top = 32
    Width = 121
    Height = 21
    EditLabel.Width = 75
    EditLabel.Height = 13
    EditLabel.Caption = 'Database name'
    TabOrder = 1
    Text = 'syndbremote'
    OnKeyPress = lbledtDatabaseKeyPress
  end
  object lbledtUser: TLabeledEdit
    Left = 40
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'User Name'
    TabOrder = 2
    Text = 'synopse'
    OnKeyPress = lbledtDatabaseKeyPress
  end
  object lbledtPassword: TLabeledEdit
    Left = 176
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    TabOrder = 3
    Text = 'synopse'
    OnKeyPress = lbledtDatabaseKeyPress
  end
  object btnConnect: TButton
    Left = 72
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 4
    OnClick = btnConnectClick
  end
  object btnDisconnect: TButton
    Left = 160
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 5
    OnClick = btnDisconnectClick
  end
end
