object MainForm: TMainForm
  Left = 229
  Top = 236
  BorderStyle = bsDialog
  Caption = ' Remote logger'
  ClientHeight = 285
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpEvent: TGroupBox
    Left = 8
    Top = 136
    Width = 313
    Height = 129
    Caption = ' Single Event  '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Visible = False
    object cbbEvent: TComboBox
      Left = 16
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      DropDownCount = 50
      ItemHeight = 13
      TabOrder = 0
    end
    object edtText: TEdit
      Left = 16
      Top = 56
      Width = 281
      Height = 21
      TabOrder = 1
      Text = 'Message'
    end
    object btnEventSend: TButton
      Left = 16
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Send'
      TabOrder = 2
      OnClick = btnEventSendClick
    end
    object btnDisconnect: TButton
      Left = 222
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = btnDisconnectClick
    end
  end
  object grpConnection: TGroupBox
    Left = 8
    Top = 16
    Width = 321
    Height = 105
    Caption = ' Connection To the LogView Server'
    TabOrder = 1
    object lblServer: TLabel
      Left = 8
      Top = 27
      Width = 78
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server Address:'
    end
    object lblPort: TLabel
      Left = 27
      Top = 51
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'Server Port:'
    end
    object lblInfoConnect: TLabel
      Left = 16
      Top = 80
      Width = 269
      Height = 13
      Caption = 'Please ensure that the LogView tool is running as server'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object edtServer: TEdit
      Left = 88
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 88
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '8091'
    end
    object btnConnect: TButton
      Left = 222
      Top = 40
      Width = 75
      Height = 33
      Caption = 'Connect'
      TabOrder = 0
      OnClick = btnConnectClick
    end
  end
end
