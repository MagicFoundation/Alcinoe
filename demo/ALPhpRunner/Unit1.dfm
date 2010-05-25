object Form1: TForm1
  Left = 412
  Top = 351
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TALPHPRunner'
  ClientHeight = 748
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object MainStatusBar: TStatusBar
    Left = 0
    Top = 729
    Width = 800
    Height = 19
    Panels = <
      item
        Width = 300
      end>
    ExplicitTop = 677
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 800
    Height = 729
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 677
    object TabSheet1: TTabSheet
      Caption = 'Main'
      ExplicitHeight = 649
      object GroupBox9: TGroupBox
        Left = 0
        Top = 134
        Width = 792
        Height = 192
        Align = alTop
        Caption = 'PARAMS'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object Label4: TLabel
          Left = 8
          Top = 85
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
          Top = 101
          Width = 103
          Height = 13
          Caption = 'Format "name=value"'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label1: TLabel
          Left = 8
          Top = 27
          Width = 88
          Height = 13
          Caption = 'Script Filename'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 370
          Top = 85
          Width = 94
          Height = 13
          Caption = 'Server Variables'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 370
          Top = 101
          Width = 103
          Height = 13
          Caption = 'Format "name=value"'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object editScriptFileName: TEdit
          Left = 104
          Top = 24
          Width = 241
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
          Text = 'test.php'
        end
        object MemoPostDataStrings: TMemo
          Left = 8
          Top = 117
          Width = 337
          Height = 65
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
        object ButtonInitAndPost: TButton
          Left = 432
          Top = 17
          Width = 75
          Height = 25
          Caption = 'Init && Post'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnClick = ButtonInitAndPostClick
        end
        object ButtonInitAndGet: TButton
          Left = 351
          Top = 17
          Width = 75
          Height = 25
          Caption = 'Init && Get'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          OnClick = ButtonInitAndGetClick
        end
        object MemoServerVariables: TMemo
          Left = 370
          Top = 116
          Width = 398
          Height = 66
          BevelInner = bvNone
          BevelOuter = bvNone
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'URL=/test.php'
            'REQUEST_METHOD=GET'
            'QUERY_STRING='
            'HTTP_COOKIE=')
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 4
          WordWrap = False
        end
        object RadioButtonPHPCGIRunnerEngineKind: TRadioButton
          Left = 529
          Top = 17
          Width = 48
          Height = 17
          Caption = 'CGI'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object RadioButtonPHPNamedPipeFastCGIRunnerEngineKind: TRadioButton
          Left = 599
          Top = 17
          Width = 138
          Height = 17
          Caption = 'NamedPipe FastCGI'
          TabOrder = 6
        end
        object RadioButtonPHPSocketFastCGIRunnerEngineKind: TRadioButton
          Left = 599
          Top = 40
          Width = 138
          Height = 17
          Caption = 'Socket FastCGI'
          TabOrder = 7
        end
        object RadioButtonPHPISAPIRunnerEngineKind: TRadioButton
          Left = 529
          Top = 40
          Width = 64
          Height = 17
          Caption = 'ISAPI'
          TabOrder = 8
        end
        object ButtonExecute: TButton
          Left = 351
          Top = 48
          Width = 75
          Height = 25
          Caption = 'Custom'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
          OnClick = ButtonExecuteClick
        end
      end
      object GroupBox10: TGroupBox
        Left = 0
        Top = 336
        Width = 792
        Height = 227
        Align = alTop
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
          Height = 180
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
          Height = 180
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
      object GroupBox1: TGroupBox
        Left = 0
        Top = 10
        Width = 792
        Height = 114
        Align = alTop
        Caption = 'INTERPRETER'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 777
        object Label11: TLabel
          Left = 8
          Top = 27
          Width = 105
          Height = 13
          Caption = 'PHP-CGI.exe Path'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 8
          Top = 52
          Width = 102
          Height = 13
          Caption = 'php5isapi.dll Path'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label9: TLabel
          Left = 8
          Top = 77
          Width = 73
          Height = 13
          Caption = 'FastCgi Host'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 317
          Top = 77
          Width = 70
          Height = 13
          Caption = 'FastCgi Port'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label12: TLabel
          Left = 119
          Top = 96
          Width = 142
          Height = 13
          Caption = 'php-cgi.exe -b 127.0.0.1:9000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object EditPhpCGIPath: TEdit
          Left = 119
          Top = 25
          Width = 384
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
          Text = 'c:\program files\php\php-cgi.exe'
        end
        object Panel2: TPanel
          Left = 551
          Top = 28
          Width = 200
          Height = 61
          BevelOuter = bvNone
          BorderStyle = bsSingle
          Ctl3D = False
          ParentColor = True
          ParentCtl3D = False
          TabOrder = 1
          object Label14: TLabel
            Left = 5
            Top = 0
            Width = 189
            Height = 52
            Caption = 
              'Please add in your website a link to http://www.arkadia.com or s' +
              'end me an email to svanderclock@arkadia.com if you like this com' +
              'ponent!'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clTeal
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
        end
        object EditPhpIsapiDllPath: TEdit
          Left = 119
          Top = 50
          Width = 384
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
          Text = 'c:\program files\php\php5isapi.dll'
        end
        object EditPhpFastCgiHost: TEdit
          Left = 119
          Top = 75
          Width = 194
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
          Text = '127.0.0.1'
        end
        object EditPhpFastCgiPort: TEdit
          Left = 393
          Top = 75
          Width = 110
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
          Text = '9000'
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 573
        Width = 792
        Height = 123
        Align = alTop
        Caption = 'BENCH'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object Label13: TLabel
          Left = 8
          Top = 91
          Width = 92
          Height = 13
          Caption = 'Cycles / Thread'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label15: TLabel
          Left = 8
          Top = 64
          Width = 41
          Height = 13
          Caption = 'Thread'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ButtonBench: TButton
          Left = 8
          Top = 25
          Width = 148
          Height = 25
          Caption = 'Bench'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnClick = ButtonBenchClick
        end
        object EditThreadCount: TEdit
          Left = 106
          Top = 64
          Width = 50
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
          Text = '12'
        end
        object EditCycleCount: TEdit
          Left = 106
          Top = 89
          Width = 50
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
          Text = '1000'
        end
        object MemoBenchResult: TMemo
          Left = 176
          Top = 14
          Width = 592
          Height = 99
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
          TabOrder = 3
          WordWrap = False
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 326
        Width = 792
        Height = 10
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        ExplicitLeft = -3
        ExplicitTop = 115
      end
      object Panel3: TPanel
        Left = 0
        Top = 124
        Width = 792
        Height = 10
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 5
        ExplicitLeft = 3
        ExplicitTop = 71
      end
      object Panel4: TPanel
        Left = 0
        Top = 563
        Width = 792
        Height = 10
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 6
        ExplicitLeft = -3
        ExplicitTop = 541
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 792
        Height = 10
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 7
        ExplicitLeft = -3
      end
    end
  end
end
