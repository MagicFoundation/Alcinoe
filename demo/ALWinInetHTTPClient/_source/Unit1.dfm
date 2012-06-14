object Form1: TForm1
  Left = 377
  Top = 296
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TALWinInetHttpClient test'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MainStatusBar: TStatusBar
    Left = 0
    Top = 581
    Width = 800
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 250
      end
      item
        Width = 50
      end>
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 800
    Height = 581
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'Main'
      object GroupBox9: TGroupBox
        Left = 8
        Top = 8
        Width = 777
        Height = 193
        Caption = 'REQUEST'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label4: TLabel
          Left = 8
          Top = 56
          Width = 100
          Height = 13
          Caption = 'Post Data Strings'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label6: TLabel
          Left = 8
          Top = 72
          Width = 289
          Height = 13
          Caption = 'Format "name=value" if not URL Encode Post Data checked'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label7: TLabel
          Left = 392
          Top = 72
          Width = 123
          Height = 13
          Caption = 'Format "Name=FileName"'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 392
          Top = 56
          Width = 87
          Height = 13
          Caption = 'Post Data Files'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label1: TLabel
          Left = 8
          Top = 27
          Width = 26
          Height = 13
          Caption = 'URL'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object editURL: TEdit
          Left = 40
          Top = 24
          Width = 305
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          Text = 'http://www.wikipedia.org'
        end
        object MemoPostDataStrings: TMemo
          Left = 8
          Top = 88
          Width = 375
          Height = 73
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
          WordWrap = False
        end
        object MemoPostDataFiles: TMemo
          Left = 392
          Top = 88
          Width = 375
          Height = 93
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 2
          WordWrap = False
        end
        object ButtonPost: TButton
          Left = 407
          Top = 21
          Width = 50
          Height = 25
          Caption = 'Post'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = ButtonPostClick
        end
        object ButtonGet: TButton
          Left = 351
          Top = 21
          Width = 50
          Height = 25
          Caption = 'Get'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = ButtonGetClick
        end
        object CheckBoxHttpEncodePostData: TCheckBox
          Left = 11
          Top = 167
          Width = 150
          Height = 17
          Caption = 'HTTP Encode Post Data'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
        end
        object ButtonHead: TButton
          Left = 462
          Top = 21
          Width = 50
          Height = 25
          Caption = 'Head'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          OnClick = ButtonHeadClick
        end
        object CheckBoxUrlEncodePostData: TCheckBox
          Left = 169
          Top = 167
          Width = 138
          Height = 17
          Caption = 'URL Encode Post Data'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
        end
        object ButtonTrace: TButton
          Left = 518
          Top = 21
          Width = 50
          Height = 25
          Caption = 'Trace'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          OnClick = ButtonTraceClick
        end
      end
      object GroupBox10: TGroupBox
        Left = 8
        Top = 208
        Width = 777
        Height = 338
        Caption = 'RESPONSE'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 21
          Width = 38
          Height = 13
          Caption = 'Header:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 304
          Top = 21
          Width = 40
          Height = 13
          Caption = 'Content:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object MemoResponseRawHeader: TMemo
          Left = 8
          Top = 37
          Width = 289
          Height = 290
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
        end
        object MemoContentBody: TMemo
          Left = 304
          Top = 37
          Width = 465
          Height = 290
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
          WordWrap = False
        end
        object ButtonOpenInExplorer: TButton
          Left = 656
          Top = 12
          Width = 113
          Height = 21
          Caption = 'Open in Explorer'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = ButtonOpenInExplorerClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Configuration'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TGroupBox
        Left = 7
        Top = 11
        Width = 289
        Height = 70
        Caption = 'Authentication'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label18: TLabel
          Left = 16
          Top = 21
          Width = 50
          Height = 13
          Caption = 'UserName'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label19: TLabel
          Left = 20
          Top = 46
          Width = 46
          Height = 13
          Caption = 'Password'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object EditUserName: TEdit
          Left = 73
          Top = 18
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          OnClick = OnCfgEditCHange
        end
        object EditPassword: TEdit
          Left = 73
          Top = 42
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 1
          OnClick = OnCfgEditCHange
        end
      end
      object GroupBox4: TGroupBox
        Left = 7
        Top = 91
        Width = 289
        Height = 94
        Caption = 'Timeout'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label14: TLabel
          Left = 41
          Top = 45
          Width = 25
          Height = 13
          Caption = 'Send'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 26
          Top = 21
          Width = 40
          Height = 13
          Caption = 'Connect'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label20: TLabel
          Left = 26
          Top = 69
          Width = 40
          Height = 13
          Caption = 'Receive'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object EditSendTimeout: TEdit
          Left = 73
          Top = 42
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          Text = '0'
          OnClick = OnCfgEditCHange
        end
        object EditReceiveTimeout: TEdit
          Left = 73
          Top = 66
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 1
          Text = '0'
          OnClick = OnCfgEditCHange
        end
        object EditConnectTimeout: TEdit
          Left = 73
          Top = 18
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 2
          Text = '0'
          OnClick = OnCfgEditCHange
        end
      end
      object GroupBox6: TGroupBox
        Left = 7
        Top = 195
        Width = 289
        Height = 54
        Caption = 'Protocole version'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        object RadioButtonProtocolVersion1_0: TRadioButton
          Left = 32
          Top = 21
          Width = 73
          Height = 17
          Caption = 'HTTP/1.0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object RadioButtonProtocolVersion1_1: TRadioButton
          Left = 128
          Top = 21
          Width = 81
          Height = 17
          Caption = 'HTTP/1.1'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
      end
      object GroupBox7: TGroupBox
        Left = 304
        Top = 11
        Width = 161
        Height = 534
        Caption = 'Internet options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object CheckBoxInternetOption_From_Cache: TCheckBox
          Left = 8
          Top = 24
          Width = 150
          Height = 17
          Caption = 'From_Cache'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Offline: TCheckBox
          Left = 8
          Top = 48
          Width = 150
          Height = 17
          Caption = 'Offline'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Keep_connection: TCheckBox
          Left = 8
          Top = 216
          Width = 150
          Height = 17
          Caption = 'Keep_connection'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 2
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_No_auto_redirect: TCheckBox
          Left = 8
          Top = 288
          Width = 150
          Height = 17
          Caption = 'No_auto_redirect'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Ignore_redirect_to_https: TCheckBox
          Left = 8
          Top = 192
          Width = 150
          Height = 17
          Caption = 'Ignore_redirect_to_https'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_No_auth: TCheckBox
          Left = 8
          Top = 264
          Width = 150
          Height = 17
          Caption = 'No_auth'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Ignore_cert_date_invalid: TCheckBox
          Left = 8
          Top = 144
          Width = 150
          Height = 17
          Caption = 'Ignore_cert_date_invalid'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Need_file: TCheckBox
          Left = 8
          Top = 240
          Width = 150
          Height = 17
          Caption = 'Need_file'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 7
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Ignore_redirect_to_http: TCheckBox
          Left = 8
          Top = 168
          Width = 150
          Height = 17
          Caption = 'Ignore_redirect_to_http'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Hyperlink: TCheckBox
          Left = 8
          Top = 96
          Width = 150
          Height = 17
          Caption = 'Hyperlink'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Ignore_cert_cn_invalid: TCheckBox
          Left = 8
          Top = 120
          Width = 150
          Height = 17
          Caption = 'Ignore_cert_cn_invalid'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 10
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Cache_if_net_fail: TCheckBox
          Left = 8
          Top = 72
          Width = 150
          Height = 17
          Caption = 'Cache_if_net_fail'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 11
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_No_cache_write: TCheckBox
          Left = 8
          Top = 312
          Width = 150
          Height = 17
          Caption = 'No_cache_write'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Resynchronize: TCheckBox
          Left = 8
          Top = 432
          Width = 150
          Height = 17
          Caption = 'Resynchronize'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 13
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_No_cookies: TCheckBox
          Left = 8
          Top = 336
          Width = 150
          Height = 17
          Caption = 'No_cookies'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 14
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Pragma_nocache: TCheckBox
          Left = 8
          Top = 384
          Width = 150
          Height = 17
          Caption = 'Pragma_nocache'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 15
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Reload: TCheckBox
          Left = 8
          Top = 408
          Width = 150
          Height = 17
          Caption = 'Reload'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 16
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_No_ui: TCheckBox
          Left = 8
          Top = 360
          Width = 150
          Height = 17
          Caption = 'No_ui'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 17
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object CheckBoxInternetOption_Secure: TCheckBox
          Left = 8
          Top = 456
          Width = 150
          Height = 17
          Caption = 'Secure'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 18
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 475
        Width = 289
        Height = 70
        Caption = 'Access Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        object RadioButtonAccessType_Direct: TRadioButton
          Left = 16
          Top = 48
          Width = 65
          Height = 17
          Caption = 'Direct'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object RadioButtonAccessType_Preconfig: TRadioButton
          Left = 16
          Top = 24
          Width = 73
          Height = 17
          Caption = 'Preconfig'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object RadioButtonAccessType_Preconfig_with_no_autoproxy: TRadioButton
          Left = 104
          Top = 24
          Width = 163
          Height = 17
          Caption = 'Preconfig_with_no_autoproxy'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
        object RadioButtonAccessType_Proxy: TRadioButton
          Left = 104
          Top = 48
          Width = 113
          Height = 17
          Caption = 'Proxy'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = OnCfgEditCHange
          OnEnter = OnCfgEditCHange
          OnKeyPress = OnCfgEditKeyPress
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 323
        Width = 289
        Height = 145
        Caption = 'Proxy'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        object Label15: TLabel
          Left = 47
          Top = 45
          Width = 19
          Height = 13
          Caption = 'Port'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label12: TLabel
          Left = 35
          Top = 21
          Width = 31
          Height = 13
          Caption = 'Server'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label11: TLabel
          Left = 16
          Top = 69
          Width = 50
          Height = 13
          Caption = 'UserName'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label16: TLabel
          Left = 20
          Top = 94
          Width = 46
          Height = 13
          Caption = 'Password'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label13: TLabel
          Left = 32
          Top = 117
          Width = 34
          Height = 13
          Caption = 'Bypass'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object EdProxyPort: TEdit
          Left = 73
          Top = 42
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          Text = '80'
          OnClick = OnCfgEditCHange
        end
        object EdProxyUserName: TEdit
          Left = 73
          Top = 66
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 1
          OnClick = OnCfgEditCHange
        end
        object EdProxyServer: TEdit
          Left = 73
          Top = 18
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 2
          OnClick = OnCfgEditCHange
        end
        object EdProxyPassword: TEdit
          Left = 73
          Top = 90
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 3
          OnClick = OnCfgEditCHange
        end
        object EdProxyBypass: TEdit
          Left = 73
          Top = 114
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 4
          Text = '<local>'
          OnClick = OnCfgEditCHange
        end
      end
      object GroupBox5: TGroupBox
        Left = 8
        Top = 259
        Width = 289
        Height = 54
        Caption = 'Buffer Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        object Label24: TLabel
          Left = 27
          Top = 21
          Width = 34
          Height = 13
          Caption = 'Upload'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object EditBufferUploadSize: TEdit
          Left = 73
          Top = 18
          Width = 200
          Height = 19
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          TabOrder = 0
          Text = '32768'
          OnClick = OnCfgEditCHange
        end
      end
      object GroupBox8: TGroupBox
        Left = 472
        Top = 11
        Width = 313
        Height = 364
        Caption = 'Request Raw Header '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
        object Label8: TLabel
          Left = 9
          Top = 22
          Width = 176
          Height = 13
          Caption = 'Format "Name: values" on each lines'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object MemoRequestRawHeader: TMemo
          Left = 9
          Top = 38
          Width = 294
          Height = 312
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
          OnClick = OnCfgEditCHange
        end
      end
      object Panel1: TPanel
        Left = 472
        Top = 384
        Width = 313
        Height = 161
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clSilver
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 8
        object Label9: TLabel
          Left = 7
          Top = 19
          Width = 152
          Height = 45
          Caption = 'Please help us to keep the development of these components free'
          Font.Charset = ANSI_CHARSET
          Font.Color = clMaroon
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object Label10: TLabel
          Left = 7
          Top = 74
          Width = 156
          Height = 60
          Caption = 
            'If you like these components please simply click on each button ' +
            'below ... thanks for your support !'
          Font.Charset = ANSI_CHARSET
          Font.Color = clBlack
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Panel2: TPanel
          Left = 173
          Top = 12
          Width = 130
          Height = 134
          BevelOuter = bvNone
          BorderStyle = bsSingle
          Color = clWhite
          Ctl3D = False
          ParentBackground = False
          ParentCtl3D = False
          TabOrder = 0
          object PanelWebBrowser: TPanel
            Left = -5
            Top = -23
            Width = 133
            Height = 159
            BevelOuter = bvNone
            Color = clMedGray
            Ctl3D = False
            ParentBackground = False
            ParentCtl3D = False
            TabOrder = 0
          end
        end
      end
    end
  end
end
