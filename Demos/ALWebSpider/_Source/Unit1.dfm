object Form1: TForm1
  Left = 473
  Top = 288
  Caption = 'ALWebSpider'
  ClientHeight = 370
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 53
    Top = 19
    Width = 76
    Height = 13
    Caption = 'URL to Crawl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 44
    Top = 43
    Width = 85
    Height = 13
    Caption = 'Save Directory'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 36
    Top = 115
    Width = 93
    Height = 13
    Caption = 'Max Deep Level'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 15
    Top = 67
    Width = 114
    Height = 13
    Caption = 'Include links (mask)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 12
    Top = 91
    Width = 117
    Height = 13
    Caption = 'Exclude links (mask)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ButtonStop: TButton
    Left = 216
    Top = 229
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 12
    OnClick = ButtonStopClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 332
    Width = 518
    Height = 19
    Panels = <
      item
        Width = 150
      end
      item
        Width = 100
      end>
    ExplicitTop = 333
    ExplicitWidth = 522
  end
  object editURL2Crawl: TEdit
    Left = 136
    Top = 16
    Width = 362
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
    Text = 'http://www.pap.fr'
  end
  object ButtonStart: TButton
    Left = 136
    Top = 229
    Width = 75
    Height = 25
    Caption = 'Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    OnClick = ButtonStartClick
  end
  object EditSaveDirectory: TEdit
    Left = 136
    Top = 40
    Width = 345
    Height = 19
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    TabOrder = 8
  end
  object EditMaxDeepLevel: TEdit
    Left = 136
    Top = 112
    Width = 65
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
    Text = '-1'
  end
  object UpDownMaxDeepLevel: TUpDown
    Left = 201
    Top = 112
    Width = 15
    Height = 19
    Associate = EditMaxDeepLevel
    Min = -1
    Max = 10000
    Position = -1
    TabOrder = 3
  end
  object CheckBoxDownloadImage: TCheckBox
    Left = 136
    Top = 144
    Width = 105
    Height = 17
    Caption = 'Download image'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBoxUpdateHref: TCheckBox
    Left = 136
    Top = 168
    Width = 169
    Height = 17
    Caption = 'Update Href to local FileName'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object CheckBoxStayInStartSite: TCheckBox
    Left = 136
    Top = 192
    Width = 105
    Height = 17
    Caption = 'Stay in Start Site'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object StatusBar2: TStatusBar
    Left = 0
    Top = 351
    Width = 518
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 352
    ExplicitWidth = 522
  end
  object BtnChooseSaveDirectory: TButton
    Left = 481
    Top = 40
    Width = 17
    Height = 19
    Caption = '...'
    TabOrder = 1
    OnClick = BtnChooseSaveDirectoryClick
  end
  object MemoErrorMsg: TMemo
    Left = 0
    Top = 264
    Width = 518
    Height = 68
    Align = alBottom
    Ctl3D = True
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 9
    WordWrap = False
    ExplicitTop = 265
    ExplicitWidth = 522
  end
  object EditIncludeLink: TEdit
    Left = 136
    Top = 64
    Width = 362
    Height = 19
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 13
  end
  object EditExcludeLink: TEdit
    Left = 136
    Top = 88
    Width = 362
    Height = 19
    Ctl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    TabOrder = 14
  end
end
