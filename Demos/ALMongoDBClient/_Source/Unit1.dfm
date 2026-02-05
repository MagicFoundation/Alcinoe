object Form1: TForm1
  Left = 485
  Top = 214
  Caption = 'ALMongoDBClientDemo'
  ClientHeight = 848
  ClientWidth = 784
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 105
    Width = 784
    Height = 743
    ActivePage = TabSheet1
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Find'
      object ToolsPanel: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 393
        Align = alTop
        TabOrder = 0
        object Label2: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label4: TLabel
          Left = 23
          Top = 63
          Width = 29
          Height = 16
          Caption = 'Filter'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 23
          Top = 183
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object FindDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object FindCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object FindFilterMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 82
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "city": { "$in": ["AGAWAM", "CHESTER", "BLANDFORD"] } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object FindOptsMemo: TMemo
          Left = 23
          Top = 205
          Width = 730
          Height = 130
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{'
            '  "sort": { "_id": 1 },'
            '  "skip": 10,'
            '  "limit": 3,'
            '  "projection": { "_id": 1, "city": 1, "state": 1, "pop": 1 }'
            '}')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 3
        end
        object FindButton: TButton
          Left = 23
          Top = 349
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'Find'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = FindButtonClick
        end
      end
      object InfoPanel: TPanel
        Left = 0
        Top = 393
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 1
        object Label7: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object FindResultMemo: TMemo
        Left = 0
        Top = 417
        Width = 776
        Height = 295
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'FindAndModify'
      ImageIndex = 2
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 497
        Align = alTop
        TabOrder = 0
        object Label14: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label15: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label16: TLabel
          Left = 23
          Top = 63
          Width = 36
          Height = 16
          Caption = 'Query'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 23
          Top = 151
          Width = 24
          Height = 16
          Caption = 'Sort'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label19: TLabel
          Left = 23
          Top = 239
          Width = 45
          Height = 16
          Caption = 'Update'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label20: TLabel
          Left = 23
          Top = 327
          Width = 37
          Height = 16
          Caption = 'Fields'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object FindAndModifyDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object FindAndModifyCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object FindAndModifyQueryMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "state": "MA", "pop": { "$gte": 20000 } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object FindAndModifyButton: TButton
          Left = 23
          Top = 452
          Width = 115
          Height = 26
          Hint = 'Connect to the mail server'
          Caption = 'FindAndModify'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = FindAndModifyButtonClick
        end
        object FindAndModifySortMemo: TMemo
          Left = 23
          Top = 173
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "pop": -1 }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 4
        end
        object FindAndModifyUpdateMemo: TMemo
          Left = 23
          Top = 261
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "$inc": { "pop": 100 } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 5
        end
        object FindAndModifyFieldsMemo: TMemo
          Left = 23
          Top = 349
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "_id": 0, "city": 1, "state": 1 }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 6
        end
        object FindAndModifyRemoveCheckBox: TCheckBox
          Left = 23
          Top = 419
          Width = 97
          Height = 17
          Caption = 'Remove'
          TabOrder = 7
        end
        object FindAndModifyUpsertCheckBox: TCheckBox
          Left = 135
          Top = 419
          Width = 97
          Height = 17
          Caption = 'Upsert'
          TabOrder = 8
        end
        object FindAndModifyNewCheckBox: TCheckBox
          Left = 254
          Top = 419
          Width = 97
          Height = 17
          Caption = 'New'
          TabOrder = 9
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 497
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 1
        object Label18: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object FindAndModifyResultMemo: TMemo
        Left = 0
        Top = 521
        Width = 776
        Height = 191
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Insert'
      ImageIndex = 4
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 401
        Align = alTop
        TabOrder = 0
        object Label29: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label30: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label31: TLabel
          Left = 23
          Top = 63
          Width = 76
          Height = 16
          Caption = 'Document(s)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label32: TLabel
          Left = 23
          Top = 223
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object InsertDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object InsertCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object InsertDocumentsMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 124
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{'
            '  "city": "AGAWAM",'
            '  "loc": [ -72.622739, 42.070206 ],'
            '  "pop": 15338,'
            '  "state": "MA"'
            '}')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object InsertOptsMemo: TMemo
          Left = 23
          Top = 245
          Width = 730
          Height = 97
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{'
            '  "validate": false,'
            '  "comment": "import zipcodes sample"'
            '}')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 3
        end
        object InsertOneButton: TButton
          Left = 23
          Top = 357
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'InsertOne'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = InsertOneButtonClick
        end
        object InsertManyButton: TButton
          Left = 153
          Top = 357
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'InsertMany'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = InsertManyButtonClick
        end
      end
      object Panel8: TPanel
        Left = 0
        Top = 401
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 1
        object Label33: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object InsertResultMemo: TMemo
        Left = 0
        Top = 425
        Width = 776
        Height = 287
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Update'
      ImageIndex = 3
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 385
        Align = alTop
        TabOrder = 0
        object Label35: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label36: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label37: TLabel
          Left = 23
          Top = 63
          Width = 50
          Height = 16
          Caption = 'Selector'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label38: TLabel
          Left = 23
          Top = 151
          Width = 45
          Height = 16
          Caption = 'Update'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label39: TLabel
          Left = 23
          Top = 239
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object UpdateDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object UpdateCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object UpdateSelectorMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "state": "MA", "pop": { "$lt": 2000 } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object UpdateOneButton: TButton
          Left = 23
          Top = 340
          Width = 115
          Height = 26
          Hint = 'Connect to the mail server'
          Caption = 'UpdateOne'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = UpdateOneButtonClick
        end
        object UpdateUpdateMemo: TMemo
          Left = 23
          Top = 173
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "$set": { "city": "AGAWAM (MA)" }, "$inc": { "pop": 10 } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 4
        end
        object UpdateOptsMemo: TMemo
          Left = 23
          Top = 261
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "upsert": true }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 5
        end
        object UpdateManyButton: TButton
          Left = 159
          Top = 340
          Width = 115
          Height = 26
          Hint = 'Connect to the mail server'
          Caption = 'UpdateMany'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = UpdateManyButtonClick
        end
      end
      object Panel10: TPanel
        Left = 0
        Top = 385
        Width = 776
        Height = 25
        Align = alTop
        TabOrder = 1
        object Label46: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object UpdateResultMemo: TMemo
        Left = 0
        Top = 410
        Width = 776
        Height = 302
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Replace'
      ImageIndex = 5
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 449
        Align = alTop
        TabOrder = 0
        object Label22: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label23: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label24: TLabel
          Left = 23
          Top = 63
          Width = 50
          Height = 16
          Caption = 'Selector'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label25: TLabel
          Left = 23
          Top = 151
          Width = 81
          Height = 16
          Caption = 'Replacement'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label26: TLabel
          Left = 23
          Top = 311
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ReplaceDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object ReplaceCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object ReplaceSelectorMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "_id": "01001" }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object ReplaceOneButton: TButton
          Left = 23
          Top = 404
          Width = 115
          Height = 26
          Hint = 'Connect to the mail server'
          Caption = 'ReplaceOne'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = ReplaceOneButtonClick
        end
        object ReplaceReplacementMemo: TMemo
          Left = 23
          Top = 173
          Width = 730
          Height = 121
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{'
            '  "_id": "01001",'
            '  "city": "AGAWAM",'
            '  "loc": [ -72.622739, 42.070206 ],'
            '  "pop": 16000,'
            '  "state": "MA"'
            '}')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 4
        end
        object ReplaceOptsMemo: TMemo
          Left = 23
          Top = 333
          Width = 730
          Height = 57
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "upsert": false }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 5
        end
      end
      object Panel9: TPanel
        Left = 0
        Top = 449
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 1
        object Label34: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object ReplaceResultMemo: TMemo
        Left = 0
        Top = 473
        Width = 776
        Height = 239
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Delete'
      ImageIndex = 6
      object Panel11: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 393
        Align = alTop
        TabOrder = 0
        object Label41: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label42: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label43: TLabel
          Left = 23
          Top = 63
          Width = 50
          Height = 16
          Caption = 'Selector'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label44: TLabel
          Left = 23
          Top = 183
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object DeleteDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object DeleteCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object DeleteSelectorMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 82
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "city": "holyoke", "state": "MA" }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object DeleteOptsMemo: TMemo
          Left = 23
          Top = 205
          Width = 730
          Height = 130
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '{ "collation": { "locale": "en", "strength": 2 } }')
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 3
        end
        object DeleteOneButton: TButton
          Left = 23
          Top = 349
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'DeleteOne'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = DeleteOneButtonClick
        end
        object DeleteManyButton: TButton
          Left = 173
          Top = 349
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'DeleteMany'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = DeleteManyButtonClick
        end
      end
      object Panel12: TPanel
        Left = 0
        Top = 393
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 1
        object Label45: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
      object DeleteResultMemo: TMemo
        Left = 0
        Top = 417
        Width = 776
        Height = 295
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'ChangeStream'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 776
        Height = 401
        Align = alTop
        TabOrder = 0
        object Label8: TLabel
          Left = 23
          Top = 25
          Width = 60
          Height = 16
          Caption = 'Database'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 375
          Top = 25
          Width = 59
          Height = 16
          Caption = 'Collection'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label10: TLabel
          Left = 23
          Top = 63
          Width = 49
          Height = 16
          Caption = 'Pipeline'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label11: TLabel
          Left = 23
          Top = 183
          Width = 28
          Height = 16
          Caption = 'Opts'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object ChangeStreamDatabaseNameEdit: TEdit
          Left = 89
          Top = 22
          Width = 264
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'test'
        end
        object ChangeStreamCollectionNameEdit: TEdit
          Left = 440
          Top = 22
          Width = 313
          Height = 24
          Hint = 'Mail server hostname or IP address'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'zipcodes'
        end
        object ChangeStreamPipelineMemo: TMemo
          Left = 23
          Top = 85
          Width = 730
          Height = 82
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 2
        end
        object ChangeStreamOptsMemo: TMemo
          Left = 23
          Top = 205
          Width = 730
          Height = 130
          Hint = 'Enter the message text in this memo'
          EditMargins.Left = 6
          EditMargins.Right = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ScrollBars = ssVertical
          ShowHint = True
          TabOrder = 3
        end
        object ChangeStreamStartButton: TButton
          Left = 23
          Top = 357
          Width = 115
          Height = 25
          Hint = 'Connect to the mail server'
          Caption = 'Start'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = ChangeStreamStartButtonClick
        end
      end
      object ChangeStreamResultMemo: TMemo
        Left = 0
        Top = 425
        Width = 776
        Height = 287
        Hint = 'This memo shows info messages'
        Align = alClient
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clMenu
        Ctl3D = False
        EditMargins.Left = 6
        EditMargins.Right = 6
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = 16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentCtl3D = False
        ParentFont = False
        ParentShowHint = False
        ReadOnly = True
        ScrollBars = ssVertical
        ShowHint = True
        TabOrder = 1
      end
      object Panel2: TPanel
        Left = 0
        Top = 401
        Width = 776
        Height = 24
        Align = alTop
        TabOrder = 2
        object Label12: TLabel
          Left = 16
          Top = 2
          Width = 91
          Height = 16
          Caption = 'Info messages:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = 16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
      end
    end
  end
  object Panel13: TPanel
    Left = 0
    Top = 0
    Width = 784
    Height = 105
    Align = alTop
    TabOrder = 1
    object Label47: TLabel
      Left = 23
      Top = 19
      Width = 17
      Height = 16
      Caption = 'Uri'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object UriEdit: TEdit
      Left = 56
      Top = 16
      Width = 697
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'mongodb://127.0.0.1:27017/?appname=ALMongoDBClientDemo'
      OnChange = UriEditChange
    end
    object StartTransactionButton: TButton
      Left = 56
      Top = 54
      Width = 115
      Height = 25
      Hint = 'Connect to the mail server'
      Caption = 'StartTransaction'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = StartTransactionButtonClick
    end
    object CommitTransactionButton: TButton
      Left = 192
      Top = 54
      Width = 151
      Height = 25
      Hint = 'Connect to the mail server'
      Caption = 'CommitTransaction'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = CommitTransactionButtonClick
    end
    object AbortTransactionButton: TButton
      Left = 365
      Top = 54
      Width = 151
      Height = 25
      Hint = 'Connect to the mail server'
      Caption = 'AbortTransaction'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = 16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = AbortTransactionButtonClick
    end
  end
end
