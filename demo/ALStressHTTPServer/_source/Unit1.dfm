object Form1: TForm1
  Left = 377
  Top = 296
  Caption = 'TALStressHttpServer'
  ClientHeight = 582
  ClientWidth = 1008
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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1008
    Height = 582
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Application'
      DesignSize = (
        1000
        554)
      object Label4: TLabel
        Left = 11
        Top = 8
        Width = 56
        Height = 13
        Caption = 'List of Url'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label1: TLabel
        Left = 11
        Top = 145
        Width = 82
        Height = 13
        Caption = 'Number of thread'
      end
      object Label2: TLabel
        Left = 11
        Top = 171
        Width = 137
        Height = 13
        Caption = 'Max Http Request by Thread'
      end
      object Label3: TLabel
        Left = 472
        Top = 145
        Width = 117
        Height = 13
        Caption = 'Delay between each call'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 662
        Top = 145
        Width = 13
        Height = 13
        Caption = 'ms'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ButtonStart: TButton
        Left = 368
        Top = 141
        Width = 75
        Height = 25
        Caption = 'Start'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = ButtonStartClick
      end
      object MemoLstUrl: TMemo
        Left = 3
        Top = 29
        Width = 977
        Height = 106
        Anchors = [akLeft, akTop, akRight]
        Ctl3D = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Lines.Strings = (
          'http://www.pap.fr')
        ParentCtl3D = False
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
        WordWrap = False
      end
      object GridThread: TcxGrid
        Left = 0
        Top = 198
        Width = 1000
        Height = 337
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        ExplicitTop = 197
        object TableViewThread: TcxGridTableView
          NavigatorButtons.ConfirmDelete = False
          NavigatorButtons.First.Visible = False
          NavigatorButtons.PriorPage.Visible = False
          NavigatorButtons.Prior.Visible = False
          NavigatorButtons.Next.Visible = False
          NavigatorButtons.NextPage.Visible = False
          NavigatorButtons.Last.Visible = False
          NavigatorButtons.Edit.Visible = True
          NavigatorButtons.Refresh.Visible = False
          NavigatorButtons.SaveBookmark.Visible = False
          NavigatorButtons.GotoBookmark.Visible = False
          NavigatorButtons.Filter.Visible = False
          FilterBox.CustomizeDialog = False
          DataController.Summary.DefaultGroupSummaryItems = <>
          DataController.Summary.FooterSummaryItems = <
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems0GetText
              Column = TableViewThreadBytesReceived
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems1GetText
              Column = TableViewThreadDownloadSpeed
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems2GetText
              Column = TableViewThreadDNS
              DisplayText = 'Average'
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems3GetText
              Column = TableViewThreadReceive
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems4GetText
              Column = TableViewThreadSend
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems5GetText
              Column = TableViewThreadWait
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItems6GetText
              Column = TableViewThreadConnect
            end>
          DataController.Summary.SummaryGroups = <>
          Filtering.MRUItemsList = False
          Filtering.ColumnMRUItemsList = False
          OptionsBehavior.FocusCellOnTab = True
          OptionsBehavior.FocusFirstCellOnNewRecord = True
          OptionsBehavior.GoToNextCellOnEnter = True
          OptionsBehavior.ImmediateEditor = False
          OptionsBehavior.FocusCellOnCycle = True
          OptionsCustomize.ColumnFiltering = False
          OptionsCustomize.ColumnGrouping = False
          OptionsCustomize.ColumnHidingOnGrouping = False
          OptionsData.Deleting = False
          OptionsData.DeletingConfirmation = False
          OptionsData.Editing = False
          OptionsData.Inserting = False
          OptionsSelection.HideSelection = True
          OptionsView.CellEndEllipsis = True
          OptionsView.ColumnAutoWidth = True
          OptionsView.Footer = True
          OptionsView.GroupByBox = False
          OptionsView.HeaderEndEllipsis = True
          object TableViewThreadNumber: TcxGridColumn
            Caption = 'Thread #'
            Width = 74
          end
          object TableViewThreadRequestCount: TcxGridColumn
            Caption = 'Count'
            DataBinding.ValueType = 'Integer'
            Width = 56
          end
          object TableViewThreadUrl: TcxGridColumn
            Caption = 'Url'
            HeaderAlignmentHorz = taCenter
            Width = 202
          end
          object TableViewThreadHttpStatus: TcxGridColumn
            Caption = 'Http Status'
            HeaderAlignmentHorz = taCenter
            Width = 116
          end
          object TableViewThreadDNS: TcxGridColumn
            Caption = 'DNS'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 70
          end
          object TableViewThreadConnect: TcxGridColumn
            Caption = 'Connect'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 70
          end
          object TableViewThreadSend: TcxGridColumn
            Caption = 'Send'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 70
          end
          object TableViewThreadWait: TcxGridColumn
            Caption = 'Wait'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 70
          end
          object TableViewThreadReceive: TcxGridColumn
            Caption = 'Receive'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 70
          end
          object TableViewThreadBytesReceived: TcxGridColumn
            Caption = 'Bytes Received'
            DataBinding.ValueType = 'Integer'
            Width = 90
          end
          object TableViewThreadDownloadSpeed: TcxGridColumn
            Caption = 'Download speed (KB/s)'
            DataBinding.ValueType = 'Integer'
            OnGetDisplayText = TableViewThreadDownloadSpeedGetDisplayText
            Width = 130
          end
        end
        object levelThread: TcxGridLevel
          GridView = TableViewThread
        end
      end
      object CheckBoxDoLikeSpider: TCheckBox
        Left = 252
        Top = 142
        Width = 97
        Height = 17
        Caption = 'Do like a Spider '
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object CheckBoxStopOnError: TCheckBox
        Left = 252
        Top = 165
        Width = 97
        Height = 17
        Caption = 'Stop on Error'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 535
        Width = 1000
        Height = 19
        Panels = <
          item
            Width = 150
          end
          item
            Width = 50
          end>
      end
      object EditMaxHttpRequest: TSpinEdit
        Left = 156
        Top = 169
        Width = 78
        Height = 22
        Ctl3D = False
        MaxValue = 0
        MinValue = 0
        ParentCtl3D = False
        TabOrder = 6
        Value = 100
      end
      object EditNbThread: TSpinEdit
        Left = 156
        Top = 141
        Width = 78
        Height = 22
        Ctl3D = False
        MaxValue = 0
        MinValue = 0
        ParentCtl3D = False
        TabOrder = 7
        Value = 10
      end
      object EditSendDelayBetweenEachSend: TEdit
        Left = 597
        Top = 141
        Width = 60
        Height = 21
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        Text = '0'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Configuration'
      ImageIndex = 1
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
        end
      end
      object GroupBox7: TGroupBox
        Left = 304
        Top = 419
        Width = 691
        Height = 126
        Caption = 'Internet options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object CheckBoxInternetOption_BYPASS_PROXY_CACHE: TCheckBox
          Left = 8
          Top = 24
          Width = 150
          Height = 17
          Caption = 'BYPASS_PROXY_CACHE'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object CheckBoxInternetOption_ESCAPE_DISABLE: TCheckBox
          Left = 8
          Top = 48
          Width = 150
          Height = 17
          Caption = 'ESCAPE_DISABLE'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object CheckBoxInternetOption_REFRESH: TCheckBox
          Left = 200
          Top = 48
          Width = 73
          Height = 17
          Caption = 'REFRESH'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 2
        end
        object CheckBoxInternetOption_SECURE: TCheckBox
          Left = 200
          Top = 72
          Width = 73
          Height = 17
          Caption = 'SECURE'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object CheckBoxInternetOption_ESCAPE_PERCENT: TCheckBox
          Left = 8
          Top = 96
          Width = 129
          Height = 17
          Caption = 'ESCAPE_PERCENT'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object CheckBoxInternetOption_NULL_CODEPAGE: TCheckBox
          Left = 200
          Top = 24
          Width = 121
          Height = 17
          Caption = 'NULL_CODEPAGE'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 5
        end
        object CheckBoxInternetOption_ESCAPE_DISABLE_QUERY: TCheckBox
          Left = 8
          Top = 72
          Width = 161
          Height = 17
          Caption = 'ESCAPE_DISABLE_QUERY'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 6
        end
        object CheckBoxInternetOption_KEEP_CONNECTION: TCheckBox
          Left = 336
          Top = 24
          Width = 129
          Height = 17
          Caption = 'KEEP_CONNECTION'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          State = cbChecked
          TabOrder = 7
        end
        object CheckBoxInternetOption_NO_COOKIES: TCheckBox
          Left = 200
          Top = 96
          Width = 97
          Height = 17
          Caption = 'NO_COOKIES'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
        end
        object CheckBoxInternetOption_NO_AUTO_REDIRECT: TCheckBox
          Left = 336
          Top = 48
          Width = 137
          Height = 17
          Caption = 'NO_AUTO_REDIRECT'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 9
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 474
        Width = 289
        Height = 71
        Caption = 'Access Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        object RadioButtonAccessType_NAMED_PROXY: TRadioButton
          Left = 16
          Top = 48
          Width = 105
          Height = 17
          Caption = 'NAMED_PROXY'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object RadioButtonAccessType_NO_PROXY: TRadioButton
          Left = 16
          Top = 24
          Width = 81
          Height = 17
          Caption = 'NO_PROXY'
          Checked = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
        end
        object RadioButtonAccessType_DEFAULT_PROXY: TRadioButton
          Left = 144
          Top = 24
          Width = 121
          Height = 17
          Caption = 'DEFAULT_PROXY'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
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
        end
      end
      object GroupBox8: TGroupBox
        Left = 304
        Top = 11
        Width = 385
        Height = 398
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
          Width = 360
          Height = 350
          Ctl3D = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'accept-language: en-us'
            'accept: */*'
            'user-agent Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0)')
          ParentCtl3D = False
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          WordWrap = False
        end
      end
      object Panel2: TPanel
        Left = 698
        Top = 16
        Width = 292
        Height = 392
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clSilver
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 8
        object Label7: TLabel
          Left = 5
          Top = 8
          Width = 132
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
        object Label6: TLabel
          Left = 5
          Top = 63
          Width = 125
          Height = 75
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
        object Panel3: TPanel
          Left = 151
          Top = 8
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
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
    end
  end
end
