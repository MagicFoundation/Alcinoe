object FormCompile: TFormCompile
  Left = 348
  Top = 311
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'RsGUICompiling'
  ClientHeight = 165
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PanelClient: TPanel
    Left = 8
    Top = 5
    Width = 345
    Height = 124
    TabOrder = 1
    object BevelProject: TBevel
      Left = 10
      Top = 6
      Width = 324
      Height = 19
    end
    object BevelStatus: TBevel
      Left = 10
      Top = 30
      Width = 324
      Height = 19
    end
    object BevelCurrentLine: TBevel
      Left = 10
      Top = 54
      Width = 160
      Height = 19
    end
    object BevelHints: TBevel
      Left = 10
      Top = 78
      Width = 105
      Height = 19
    end
    object LblProject: TLabel
      Left = 64
      Top = 9
      Width = 264
      Height = 13
      AutoSize = False
      Transparent = True
    end
    object LblStatusCaption: TLabel
      Left = 14
      Top = 33
      Width = 58
      Height = 13
      Caption = 'RsGUIDone'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object BevelTotalLines: TBevel
      Left = 174
      Top = 54
      Width = 160
      Height = 19
    end
    object LblCurrentLineCaption: TLabel
      Left = 14
      Top = 57
      Width = 86
      Height = 13
      Caption = 'RsGUICurrentLine'
    end
    object LblCurrentLine: TLabel
      Left = 158
      Top = 57
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblTotalLinesCaption: TLabel
      Left = 178
      Top = 57
      Width = 81
      Height = 13
      Caption = 'RsGUITotalLines'
    end
    object LblTotalLines: TLabel
      Left = 322
      Top = 57
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object BevelWarnings: TBevel
      Left = 120
      Top = 78
      Width = 105
      Height = 19
    end
    object BevelErrors: TBevel
      Left = 230
      Top = 78
      Width = 104
      Height = 19
    end
    object LblHintsCaption: TLabel
      Left = 14
      Top = 81
      Width = 56
      Height = 13
      Caption = 'RsGUIHints'
    end
    object LblHints: TLabel
      Left = 104
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblWarningsCaption: TLabel
      Left = 124
      Top = 81
      Width = 77
      Height = 13
      Caption = 'RsGUIWarnings'
    end
    object LblWarnings: TLabel
      Left = 213
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblErrorsCaption: TLabel
      Left = 234
      Top = 81
      Width = 59
      Height = 13
      Caption = 'RsGUIErrors'
    end
    object LblErrors: TLabel
      Left = 322
      Top = 81
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LblProjectCaption: TLabel
      Left = 14
      Top = 9
      Width = 65
      Height = 13
      Caption = 'RsGUIProject'
    end
    object LblStatus: TLabel
      Left = 110
      Top = 33
      Width = 218
      Height = 13
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object LblErrorReason: TLabel
      Left = 8
      Top = 104
      Width = 73
      Height = 13
      Caption = 'LblErrorReason'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Visible = False
    end
  end
  object BtnOk: TButton
    Left = 144
    Top = 134
    Width = 75
    Height = 25
    Caption = 'RsGUIOk'
    Default = True
    Enabled = False
    TabOrder = 0
    OnClick = BtnOkClick
  end
end
