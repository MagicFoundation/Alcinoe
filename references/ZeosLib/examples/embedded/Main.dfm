object fmMain: TfmMain
  Left = 205
  Top = 193
  ActiveControl = btOpen
  Caption = 'ZoesDBO Demo - Using Embedded Server'
  ClientHeight = 429
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 13
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid: TDBGrid
    Left = 0
    Top = 124
    Width = 614
    Height = 286
    Align = alClient
    DataSource = DataSource
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = 13
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Pitch = fpVariable
    TitleFont.Style = []
  end
  object ToolBar: TPanel
    Left = 0
    Top = 0
    Width = 614
    Height = 28
    Align = alTop
    TabOrder = 2
    object btOpen: TButton
      Left = 0
      Top = 3
      Width = 75
      Height = 24
      Caption = 'Open'
      TabOrder = 0
      OnClick = btOpenClick
    end
    object btClose: TButton
      Left = 75
      Top = 3
      Width = 75
      Height = 24
      Caption = 'Close'
      TabOrder = 1
      OnClick = btCloseClick
    end
    object btExecute: TButton
      Left = 150
      Top = 3
      Width = 75
      Height = 24
      Caption = 'Execute'
      TabOrder = 2
      OnClick = btExecuteClick
    end
  end
  object meSQL: TMemo
    Left = 0
    Top = 28
    Width = 614
    Height = 72
    Align = alTop
    Lines.Strings = (
      'SELECT * FROM people;')
    ScrollBars = ssVertical
    TabOrder = 0
    OnExit = meSQLExit
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 410
    Width = 614
    Height = 19
    Panels = <
      item
        Text = 'State'
        Width = 200
      end>
    ParentFont = True
    UseSystemFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 100
    Width = 614
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object DBNavigator1: TDBNavigator
      Left = 0
      Top = 0
      Width = 250
      Height = 24
      DataSource = DataSource
      TabOrder = 0
    end
    object btApplyUpdates: TButton
      Left = 272
      Top = 0
      Width = 96
      Height = 24
      Caption = 'ApplyUpdates'
      TabOrder = 1
      OnClick = btApplyUpdatesClick
    end
    object btCancelUpdates: TButton
      Left = 368
      Top = 0
      Width = 96
      Height = 24
      Caption = 'CancelUpdates'
      TabOrder = 2
      OnClick = btCancelUpdatesClick
    end
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 472
    Top = 40
  end
  object EmbeddedConnection: TZConnection
    Protocol = 'mysqld-4.1'
    Database = 'zeoslib'
    Properties.Strings = (
      'compress=yes'
      'dbless=no'
      'useresult=no'
      'timeout=30'
      'ServerArgument1=--basedir=./'
      'ServerArgument2=--datadir=./data'
      'ServerArgument3=--character-sets-dir=./share/charsets'
      'ServerArgument4=--language=./share/english'
      'ServerArgument5=--skip-innodb'
      'ServerArgument6=--key_buffer_size=32M')
    Left = 232
    Top = 176
  end
  object ClientDataSet: TZQuery
    Connection = EmbeddedConnection
    CachedUpdates = True
    Params = <>
    Options = [doCalcDefaults, doSmartOpen]
    Left = 264
    Top = 176
  end
end
