object Form1: TForm1
  Left = 377
  Top = 296
  Caption = 'TALStressHttpServer'
  ClientHeight = 615
  ClientWidth = 1008
  Color = clBtnFace
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
  object PageControl1: TcxPageControl
    Left = 0
    Top = 0
    Width = 1008
    Height = 615
    Align = alClient
    TabOrder = 0
    Properties.ActivePage = TabSheet1
    Properties.CustomButtons.Buttons = <>
    ClientRectBottom = 610
    ClientRectLeft = 5
    ClientRectRight = 1003
    ClientRectTop = 27
    object TabSheet1: TcxTabSheet
      Caption = 'Application'
      Color = 15066860
      ParentColor = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        998
        583)
      object Label4: TcxLabel
        Left = 11
        Top = 8
        Caption = 'List of Url'
        ParentFont = False
        Transparent = True
      end
      object Label1: TcxLabel
        Left = 11
        Top = 167
        Caption = 'Number of thread'
        Transparent = True
      end
      object Label2: TcxLabel
        Left = 11
        Top = 193
        Caption = 'Max Http Request by Thread'
        Transparent = True
      end
      object Label3: TcxLabel
        Left = 503
        Top = 166
        Caption = 'Delay between each call'
        ParentFont = False
        Transparent = True
      end
      object Label5: TcxLabel
        Left = 693
        Top = 167
        Caption = 'ms'
        ParentFont = False
        Transparent = True
      end
      object MemoLstUrl: TcxMemo
        Left = 0
        Top = 29
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'http://www.pap.fr')
        ParentFont = False
        TabOrder = 0
        Height = 124
        Width = 998
      end
      object GridThread: TcxGrid
        Left = 0
        Top = 226
        Width = 998
        Height = 338
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object TableViewThread: TcxGridTableView
          Navigator.Buttons.CustomButtons = <>
          Navigator.Buttons.First.Visible = False
          Navigator.Buttons.PriorPage.Visible = False
          Navigator.Buttons.Prior.Visible = False
          Navigator.Buttons.Next.Visible = False
          Navigator.Buttons.NextPage.Visible = False
          Navigator.Buttons.Last.Visible = False
          Navigator.Buttons.Edit.Visible = True
          Navigator.Buttons.Refresh.Visible = False
          Navigator.Buttons.SaveBookmark.Visible = False
          Navigator.Buttons.GotoBookmark.Visible = False
          Navigator.Buttons.Filter.Visible = False
          FilterBox.CustomizeDialog = False
          ScrollbarAnnotations.CustomAnnotations = <>
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
          OptionsBehavior.FocusCellOnCycle = True
          OptionsBehavior.ImmediateEditor = False
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
          Styles.Background = cxStyle1
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
      object CheckBoxDoLikeSpider: TcxCheckBox
        Left = 252
        Top = 164
        Caption = 'Do like a Spider '
        State = cbsChecked
        TabOrder = 2
        Transparent = True
      end
      object CheckBoxStopOnError: TcxCheckBox
        Left = 252
        Top = 187
        Caption = 'Stop on Error'
        State = cbsChecked
        TabOrder = 3
        Transparent = True
      end
      object StatusBar1: TStatusBar
        Left = 0
        Top = 564
        Width = 998
        Height = 19
        Color = 15525605
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Panels = <
          item
            Width = 150
          end
          item
            Width = 50
          end>
        UseSystemFont = False
      end
      object EditMaxHttpRequest: TcxSpinEdit
        Left = 156
        Top = 191
        TabOrder = 5
        Value = 100
        Width = 78
      end
      object EditNbThread: TcxSpinEdit
        Left = 156
        Top = 163
        TabOrder = 6
        Value = 10
        Width = 78
      end
      object EditSendDelayBetweenEachSend: TcxTextEdit
        Left = 628
        Top = 163
        ParentFont = False
        TabOrder = 7
        Text = '0'
        Width = 60
      end
      object ButtonStart: TcxButton
        Left = 392
        Top = 164
        Width = 75
        Height = 25
        Caption = 'Start'
        TabOrder = 8
        OnClick = ButtonStartClick
      end
    end
    object TabSheet2: TcxTabSheet
      Caption = 'Configuration'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox3: TcxGroupBox
        Left = 7
        Top = 11
        Caption = 'Authentication'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 0
        Height = 78
        Width = 289
        object Label18: TcxLabel
          Left = 16
          Top = 21
          Caption = 'UserName'
          ParentFont = False
          Transparent = True
        end
        object Label19: TcxLabel
          Left = 20
          Top = 46
          Caption = 'Password'
          ParentFont = False
          Transparent = True
        end
        object EditUserName: TcxTextEdit
          Left = 73
          Top = 18
          ParentFont = False
          TabOrder = 0
          Width = 200
        end
        object EditPassword: TcxTextEdit
          Left = 73
          Top = 42
          ParentFont = False
          TabOrder = 1
          Width = 200
        end
      end
      object GroupBox4: TcxGroupBox
        Left = 7
        Top = 97
        Caption = 'Timeout'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 1
        Height = 102
        Width = 289
        object Label14: TcxLabel
          Left = 41
          Top = 45
          Caption = 'Send'
          ParentFont = False
          Transparent = True
        end
        object Label17: TcxLabel
          Left = 26
          Top = 21
          Caption = 'Connect'
          ParentFont = False
          Transparent = True
        end
        object Label20: TcxLabel
          Left = 26
          Top = 69
          Caption = 'Receive'
          ParentFont = False
          Transparent = True
        end
        object EditSendTimeout: TcxTextEdit
          Left = 73
          Top = 42
          ParentFont = False
          TabOrder = 0
          Text = '0'
          Width = 200
        end
        object EditReceiveTimeout: TcxTextEdit
          Left = 73
          Top = 66
          ParentFont = False
          TabOrder = 1
          Text = '0'
          Width = 200
        end
        object EditConnectTimeout: TcxTextEdit
          Left = 73
          Top = 18
          ParentFont = False
          TabOrder = 2
          Text = '0'
          Width = 200
        end
      end
      object GroupBox6: TcxGroupBox
        Left = 7
        Top = 208
        Caption = 'Protocole version'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 2
        Height = 54
        Width = 289
        object RadioButtonProtocolVersion1_0: TcxRadioButton
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
          Transparent = True
        end
        object RadioButtonProtocolVersion1_1: TcxRadioButton
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
          Transparent = True
        end
      end
      object GroupBox7: TcxGroupBox
        Left = 303
        Top = 453
        Caption = 'Internet options'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 3
        Height = 134
        Width = 691
        object CheckBoxInternetOption_BYPASS_PROXY_CACHE: TcxCheckBox
          Left = 8
          Top = 24
          Caption = 'BYPASS_PROXY_CACHE'
          ParentFont = False
          TabOrder = 0
          Transparent = True
        end
        object CheckBoxInternetOption_ESCAPE_DISABLE: TcxCheckBox
          Left = 8
          Top = 48
          Caption = 'ESCAPE_DISABLE'
          ParentFont = False
          TabOrder = 1
          Transparent = True
        end
        object CheckBoxInternetOption_REFRESH: TcxCheckBox
          Left = 200
          Top = 48
          Caption = 'REFRESH'
          ParentFont = False
          State = cbsChecked
          TabOrder = 2
          Transparent = True
        end
        object CheckBoxInternetOption_SECURE: TcxCheckBox
          Left = 200
          Top = 72
          Caption = 'SECURE'
          ParentFont = False
          TabOrder = 3
          Transparent = True
        end
        object CheckBoxInternetOption_ESCAPE_PERCENT: TcxCheckBox
          Left = 8
          Top = 96
          Caption = 'ESCAPE_PERCENT'
          ParentFont = False
          TabOrder = 4
          Transparent = True
        end
        object CheckBoxInternetOption_NULL_CODEPAGE: TcxCheckBox
          Left = 200
          Top = 24
          Caption = 'NULL_CODEPAGE'
          ParentFont = False
          TabOrder = 5
          Transparent = True
        end
        object CheckBoxInternetOption_ESCAPE_DISABLE_QUERY: TcxCheckBox
          Left = 8
          Top = 72
          Caption = 'ESCAPE_DISABLE_QUERY'
          ParentFont = False
          TabOrder = 6
          Transparent = True
        end
        object CheckBoxInternetOption_KEEP_CONNECTION: TcxCheckBox
          Left = 336
          Top = 24
          Caption = 'KEEP_CONNECTION'
          ParentFont = False
          State = cbsChecked
          TabOrder = 7
          Transparent = True
        end
        object CheckBoxInternetOption_NO_COOKIES: TcxCheckBox
          Left = 200
          Top = 96
          Caption = 'NO_COOKIES'
          ParentFont = False
          TabOrder = 8
          Transparent = True
        end
        object CheckBoxInternetOption_NO_AUTO_REDIRECT: TcxCheckBox
          Left = 336
          Top = 48
          Caption = 'NO_AUTO_REDIRECT'
          ParentFont = False
          TabOrder = 9
          Transparent = True
        end
      end
      object GroupBox2: TcxGroupBox
        Left = 8
        Top = 495
        Caption = 'Access Type'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 4
        Height = 79
        Width = 290
        object RadioButtonAccessType_NAMED_PROXY: TcxRadioButton
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
          Transparent = True
        end
        object RadioButtonAccessType_NO_PROXY: TcxRadioButton
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
          Transparent = True
        end
        object RadioButtonAccessType_DEFAULT_PROXY: TcxRadioButton
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
          Transparent = True
        end
      end
      object GroupBox1: TcxGroupBox
        Left = 8
        Top = 335
        Caption = 'Proxy'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 5
        Height = 148
        Width = 289
        object Label15: TcxLabel
          Left = 47
          Top = 45
          Caption = 'Port'
          ParentFont = False
          Transparent = True
        end
        object Label12: TcxLabel
          Left = 35
          Top = 21
          Caption = 'Server'
          ParentFont = False
          Transparent = True
        end
        object Label11: TcxLabel
          Left = 16
          Top = 69
          Caption = 'UserName'
          ParentFont = False
          Transparent = True
        end
        object Label16: TcxLabel
          Left = 20
          Top = 94
          Caption = 'Password'
          ParentFont = False
          Transparent = True
        end
        object Label13: TcxLabel
          Left = 32
          Top = 117
          Caption = 'Bypass'
          ParentFont = False
          Transparent = True
        end
        object EdProxyPort: TcxTextEdit
          Left = 73
          Top = 42
          ParentFont = False
          TabOrder = 0
          Text = '80'
          Width = 200
        end
        object EdProxyUserName: TcxTextEdit
          Left = 73
          Top = 66
          ParentFont = False
          TabOrder = 1
          Width = 200
        end
        object EdProxyServer: TcxTextEdit
          Left = 73
          Top = 18
          ParentFont = False
          TabOrder = 2
          Width = 200
        end
        object EdProxyPassword: TcxTextEdit
          Left = 73
          Top = 90
          ParentFont = False
          TabOrder = 3
          Width = 200
        end
        object EdProxyBypass: TcxTextEdit
          Left = 73
          Top = 114
          ParentFont = False
          TabOrder = 4
          Text = '<local>'
          Width = 200
        end
      end
      object GroupBox5: TcxGroupBox
        Left = 8
        Top = 272
        Caption = 'Buffer Size'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 6
        Height = 54
        Width = 289
        object Label24: TcxLabel
          Left = 27
          Top = 21
          Caption = 'Upload'
          ParentFont = False
          Transparent = True
        end
        object EditBufferUploadSize: TcxTextEdit
          Left = 73
          Top = 18
          ParentFont = False
          TabOrder = 0
          Text = '32768'
          Width = 200
        end
      end
      object GroupBox8: TcxGroupBox
        Left = 303
        Top = 11
        Caption = 'Request Raw Header '
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 7
        Height = 436
        Width = 658
        object Label8: TcxLabel
          Left = 9
          Top = 22
          Caption = 'Format "Name: values" on each lines'
          ParentFont = False
          Transparent = True
        end
        object MemoRequestRawHeader: TcxMemo
          Left = 9
          Top = 38
          Lines.Strings = (
            'accept-language: en-us'
            'accept: */*'
            'user-agent Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0)')
          ParentFont = False
          Properties.WordWrap = False
          TabOrder = 0
          Height = 385
          Width = 646
        end
      end
    end
  end
  object dxSkinController1: TdxSkinController
    NativeStyle = False
    SkinName = 'Foggy'
    Left = 496
    Top = 360
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 136
    Top = 160
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
  end
end
