object Form1: TForm1
  Left = 377
  Top = 296
  Caption = 'TALStressHttpServer'
  ClientHeight = 597
  ClientWidth = 1184
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object PageControl1: TcxPageControl
    Left = 0
    Top = 0
    Width = 1184
    Height = 597
    Align = alClient
    TabOrder = 0
    Properties.ActivePage = TabSheet1
    Properties.CustomButtons.Buttons = <>
    ClientRectBottom = 592
    ClientRectLeft = 5
    ClientRectRight = 1179
    ClientRectTop = 29
    object TabSheet1: TcxTabSheet
      Caption = 'Application'
      Color = 15066860
      ParentColor = False
      DesignSize = (
        1174
        563)
      object Label4: TcxLabel
        Left = 11
        Top = 8
        Caption = 'Urls'
        ParentFont = False
        TabOrder = 3
        Transparent = True
      end
      object Label1: TcxLabel
        Left = 11
        Top = 163
        Caption = 'Number of thread'
        TabOrder = 8
        Transparent = True
      end
      object Label2: TcxLabel
        Left = 11
        Top = 192
        Caption = 'Max Http Request by Thread'
        TabOrder = 9
        Transparent = True
      end
      object Label3: TcxLabel
        Left = 274
        Top = 191
        Caption = 'Delay between each call'
        ParentFont = False
        TabOrder = 10
        Transparent = True
      end
      object Label5: TcxLabel
        Left = 478
        Top = 190
        Caption = 'ms'
        ParentFont = False
        TabOrder = 11
        Transparent = True
      end
      object MemoLstUrl: TcxMemo
        Left = 0
        Top = 33
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'http://localhost:23456/hello')
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -13
        Style.Font.Name = 'Courier New'
        Style.Font.Style = []
        Style.IsFontAssigned = True
        TabOrder = 0
        Height = 88
        Width = 1174
      end
      object GridThread: TcxGrid
        Left = 0
        Top = 225
        Width = 1174
        Height = 318
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
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
              Kind = skSum
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsBytesGetText
              Column = TableViewThreadBytesReceived
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsSpeedGetText
              Column = TableViewThreadDownloadSpeed
            end
            item
              Format = '0.#### ms;-0.#### ms'
              Kind = skAverage
              Column = TableViewThreadDNS
              DisplayText = 'Average'
            end
            item
              Format = '0.#### ms;-0.#### ms'
              Kind = skAverage
              Column = TableViewThreadReceive
            end
            item
              Format = '0.#### ms;-0.#### ms'
              Kind = skAverage
              Column = TableViewThreadSend
            end
            item
              Format = '0.#### ms;-0.#### ms'
              Kind = skAverage
              Column = TableViewThreadWait
            end
            item
              Format = '0.#### ms;-0.#### ms'
              Kind = skAverage
              Column = TableViewThreadConnect
            end
            item
              Format = '0;-0'
              Kind = skSum
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsBytesGetText
              Column = TableViewThreadBytesSent
            end
            item
              Format = '0;-0'
              Kind = skAverage
              OnGetText = TableViewThreadTcxGridDataControllerTcxDataSummaryFooterSummaryItemsSpeedGetText
              Column = TableViewThreadUploadSpeed
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
          object TableViewThreadNumber: TcxGridColumn
            Caption = 'Thread #'
            Width = 75
          end
          object TableViewThreadRequestCount: TcxGridColumn
            Caption = 'Count'
            DataBinding.ValueType = 'Integer'
            Width = 57
          end
          object TableViewThreadUrl: TcxGridColumn
            Caption = 'Url'
            HeaderAlignmentHorz = taCenter
            Width = 171
          end
          object TableViewThreadHttpStatus: TcxGridColumn
            Caption = 'Http Status'
            DataBinding.ValueType = 'Integer'
            HeaderAlignmentHorz = taCenter
            Width = 90
          end
          object TableViewThreadDNS: TcxGridColumn
            Caption = 'DNS'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 90
          end
          object TableViewThreadConnect: TcxGridColumn
            Caption = 'Connect'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 90
          end
          object TableViewThreadSend: TcxGridColumn
            Caption = 'Send'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 90
          end
          object TableViewThreadWait: TcxGridColumn
            Caption = 'Wait'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 90
          end
          object TableViewThreadReceive: TcxGridColumn
            Caption = 'Receive'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadTimeTakenGetDisplayText
            Width = 90
          end
          object TableViewThreadBytesSent: TcxGridColumn
            Caption = 'Bytes Sent'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadBytesGetDisplayText
            Width = 100
          end
          object TableViewThreadBytesReceived: TcxGridColumn
            Caption = 'Bytes Received'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadBytesGetDisplayText
            Width = 100
          end
          object TableViewThreadUploadSpeed: TcxGridColumn
            Caption = 'Upload speed'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadSpeedGetDisplayText
            Width = 120
          end
          object TableViewThreadDownloadSpeed: TcxGridColumn
            Caption = 'Download speed'
            DataBinding.ValueType = 'Float'
            OnGetDisplayText = TableViewThreadSpeedGetDisplayText
            Width = 120
          end
        end
        object levelThread: TcxGridLevel
          GridView = TableViewThread
        end
      end
      object CheckBoxStopOnError: TcxCheckBox
        Left = 667
        Top = 190
        Caption = 'Stop on Error'
        State = cbsChecked
        TabOrder = 2
        Transparent = True
      end
      object MainStatusBar: TStatusBar
        Left = 0
        Top = 543
        Width = 1174
        Height = 20
        Color = 15526118
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
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
        Left = 175
        Top = 190
        TabOrder = 4
        Value = 100
        Width = 78
      end
      object EditNbThread: TcxSpinEdit
        Left = 175
        Top = 159
        TabOrder = 5
        Value = 10
        Width = 78
      end
      object EditSendDelayBetweenEachSend: TcxTextEdit
        Left = 412
        Top = 188
        ParentFont = False
        TabOrder = 6
        Text = '0'
        Width = 60
      end
      object ButtonStart: TcxButton
        Left = 553
        Top = 188
        Width = 75
        Height = 25
        Caption = 'Start'
        TabOrder = 7
        OnClick = ButtonStartClick
      end
      object EditFileToUpload: TcxTextEdit
        Left = 175
        Top = 128
        ParentFont = False
        TabOrder = 13
        Width = 548
      end
      object cxLabel1: TcxLabel
        Left = 11
        Top = 133
        Caption = 'File to upload'
        TabOrder = 14
        Transparent = True
      end
      object ButtonSelectFileToUpload: TcxButton
        Left = 723
        Top = 129
        Width = 34
        Height = 21
        Caption = '...'
        TabOrder = 15
        OnClick = ButtonSelectFileToUploadClick
      end
      object ComboBoxSimulateSlowClient: TcxComboBox
        Left = 602
        Top = 158
        Properties.Items.Strings = (
          '32'
          '64'
          '128'
          '256'
          '512'
          '1024')
        TabOrder = 16
        Width = 121
      end
      object cxLabel2: TcxLabel
        Left = 729
        Top = 159
        Caption = 'KB/s'
        ParentFont = False
        TabOrder = 17
        Transparent = True
      end
      object cxLabel3: TcxLabel
        Left = 274
        Top = 159
        Caption = 'Simulate SLOW CLIENT (throttle upload and limit download)'
        ParentFont = False
        TabOrder = 18
        Transparent = True
      end
    end
    object TabSheet2: TcxTabSheet
      Caption = 'Configuration'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 27
      ExplicitWidth = 1170
      ExplicitHeight = 565
      DesignSize = (
        1174
        563)
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
          TabOrder = 2
          Transparent = True
        end
        object Label19: TcxLabel
          Left = 20
          Top = 46
          Caption = 'Password'
          ParentFont = False
          TabOrder = 3
          Transparent = True
        end
        object EditUserName: TcxTextEdit
          Left = 80
          Top = 18
          ParentFont = False
          TabOrder = 0
          Width = 192
        end
        object EditPassword: TcxTextEdit
          Left = 80
          Top = 42
          ParentFont = False
          TabOrder = 1
          Width = 192
        end
      end
      object GroupBox4: TcxGroupBox
        Left = 7
        Top = 98
        Caption = 'Timeout'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 1
        Height = 106
        Width = 289
        object Label14: TcxLabel
          Left = 41
          Top = 45
          Caption = 'Send'
          ParentFont = False
          TabOrder = 3
          Transparent = True
        end
        object Label17: TcxLabel
          Left = 26
          Top = 21
          Caption = 'Connect'
          ParentFont = False
          TabOrder = 4
          Transparent = True
        end
        object Label20: TcxLabel
          Left = 26
          Top = 69
          Caption = 'Receive'
          ParentFont = False
          TabOrder = 5
          Transparent = True
        end
        object EditSendTimeout: TcxTextEdit
          Left = 80
          Top = 42
          ParentFont = False
          TabOrder = 0
          Text = '0'
          Width = 192
        end
        object EditReceiveTimeout: TcxTextEdit
          Left = 80
          Top = 66
          ParentFont = False
          TabOrder = 1
          Text = '0'
          Width = 192
        end
        object EditConnectTimeout: TcxTextEdit
          Left = 80
          Top = 18
          ParentFont = False
          TabOrder = 2
          Text = '0'
          Width = 192
        end
      end
      object GroupBox6: TcxGroupBox
        Left = 7
        Top = 213
        Caption = 'Protocole version'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 2
        Height = 78
        Width = 289
        object RadioButtonProtocolVersion1_0: TcxRadioButton
          Left = 32
          Top = 21
          Width = 73
          Height = 17
          Caption = 'HTTP/1.0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
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
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
          Transparent = True
        end
        object RadioButtonProtocolVersion2: TcxRadioButton
          Left = 34
          Top = 47
          Width = 81
          Height = 17
          Caption = 'HTTP/2'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Transparent = True
        end
        object RadioButtonProtocolVersion3: TcxRadioButton
          Left = 128
          Top = 47
          Width = 81
          Height = 17
          Caption = 'HTTP/3'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
          Transparent = True
        end
      end
      object GroupBox1: TcxGroupBox
        Left = 7
        Top = 300
        Caption = 'Proxy'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 3
        Height = 154
        Width = 289
        object Label15: TcxLabel
          Left = 47
          Top = 45
          Caption = 'Port'
          ParentFont = False
          TabOrder = 5
          Transparent = True
        end
        object Label12: TcxLabel
          Left = 35
          Top = 21
          Caption = 'Server'
          ParentFont = False
          TabOrder = 6
          Transparent = True
        end
        object Label11: TcxLabel
          Left = 16
          Top = 69
          Caption = 'UserName'
          ParentFont = False
          TabOrder = 7
          Transparent = True
        end
        object Label16: TcxLabel
          Left = 20
          Top = 94
          Caption = 'Password'
          ParentFont = False
          TabOrder = 8
          Transparent = True
        end
        object Label13: TcxLabel
          Left = 32
          Top = 117
          Caption = 'Bypass'
          ParentFont = False
          TabOrder = 9
          Transparent = True
        end
        object EdProxyPort: TcxTextEdit
          Left = 80
          Top = 42
          ParentFont = False
          TabOrder = 0
          Text = '80'
          Width = 192
        end
        object EdProxyUserName: TcxTextEdit
          Left = 80
          Top = 66
          ParentFont = False
          TabOrder = 1
          Width = 192
        end
        object EdProxyServer: TcxTextEdit
          Left = 80
          Top = 18
          ParentFont = False
          TabOrder = 2
          Width = 192
        end
        object EdProxyPassword: TcxTextEdit
          Left = 80
          Top = 90
          ParentFont = False
          TabOrder = 3
          Width = 192
        end
        object EdProxyBypass: TcxTextEdit
          Left = 80
          Top = 114
          ParentFont = False
          TabOrder = 4
          Text = '<local>'
          Width = 192
        end
      end
      object GroupBox2: TcxGroupBox
        Left = 7
        Top = 463
        Caption = 'Access Type'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 4
        Height = 77
        Width = 289
        object RadioButtonAccessType_NAMED_PROXY: TcxRadioButton
          Left = 16
          Top = 48
          Width = 105
          Height = 17
          Caption = 'NAMED_PROXY'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
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
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TabStop = True
          Transparent = True
        end
        object RadioButtonAccessType_DEFAULT_PROXY: TcxRadioButton
          Left = 150
          Top = 24
          Width = 121
          Height = 17
          Caption = 'DEFAULT_PROXY'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          Transparent = True
        end
      end
      object GroupBox7: TcxGroupBox
        Left = 304
        Top = 454
        Anchors = [akLeft, akBottom]
        Caption = 'Options'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 5
        Height = 86
        Width = 481
        object CheckBoxHttpOption_REFRESH: TcxCheckBox
          Left = 195
          Top = 24
          Caption = 'REFRESH'
          ParentFont = False
          State = cbsChecked
          Style.TransparentBorder = False
          TabOrder = 0
          Transparent = True
        end
        object CheckBoxHttpOption_KEEP_CONNECTION: TcxCheckBox
          Left = 9
          Top = 24
          Caption = 'KEEP_CONNECTION'
          ParentFont = False
          State = cbsChecked
          Style.TransparentBorder = False
          TabOrder = 1
          Transparent = True
        end
        object CheckBoxHttpOption_NO_COOKIES: TcxCheckBox
          Left = 9
          Top = 51
          Caption = 'NO_COOKIES'
          ParentFont = False
          State = cbsChecked
          Style.TransparentBorder = False
          TabOrder = 2
          Transparent = True
        end
        object CheckBoxHttpOption_NO_AUTO_REDIRECT: TcxCheckBox
          Left = 331
          Top = 24
          Caption = 'NO_AUTO_REDIRECT'
          ParentFont = False
          State = cbsChecked
          Style.TransparentBorder = False
          TabOrder = 3
          Transparent = True
        end
        object CheckBoxHttpOption_DECOMPRESSION: TcxCheckBox
          Left = 195
          Top = 51
          Caption = 'DECOMPRESSION'
          ParentFont = False
          State = cbsChecked
          Style.TransparentBorder = False
          TabOrder = 4
          Transparent = True
        end
      end
      object GroupBox8: TcxGroupBox
        Left = 304
        Top = 11
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Request Raw Header '
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        TabOrder = 6
        DesignSize = (
          869
          428)
        Height = 434
        Width = 869
        object Label8: TcxLabel
          Left = 9
          Top = 21
          Caption = 'Format "Name: values" on each lines'
          ParentFont = False
          TabOrder = 1
          Transparent = True
        end
        object MemoRequestRawHeader: TcxMemo
          Left = 9
          Top = 38
          Anchors = [akLeft, akTop, akRight, akBottom]
          Lines.Strings = (
            'Accept: */*'
            'User-Agent: ALWinHttpClient/1.0')
          ParentFont = False
          Properties.ScrollBars = ssBoth
          Style.Font.Charset = DEFAULT_CHARSET
          Style.Font.Color = clWindowText
          Style.Font.Height = -13
          Style.Font.Name = 'Courier New'
          Style.Font.Style = []
          Style.IsFontAssigned = True
          TabOrder = 0
          Height = 355
          Width = 847
        end
      end
    end
  end
  object dxSkinController1: TdxSkinController
    NativeStyle = False
    SkinName = 'Foggy'
    Left = 192
    Top = 456
  end
  object UpdateGuiTimer: TTimer
    Enabled = False
    OnTimer = UpdateGuiTimerTimer
    Left = 69
    Top = 379
  end
  object MainFileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    OnFileOkClick = MainFileOpenDialogFileOkClick
    Left = 261
    Top = 363
  end
end
