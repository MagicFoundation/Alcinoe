object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DelphiAST Parser Demo'
  ClientHeight = 436
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 291
    Width = 666
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 41
    ExplicitWidth = 206
  end
  object OutputMemo: TMemo
    Left = 0
    Top = 41
    Width = 666
    Height = 250
    Align = alTop
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitTop = 33
    ExplicitHeight = 168
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 417
    Width = 666
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 370
  end
  object CheckBox1: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 397
    Width = 660
    Height = 17
    Align = alBottom
    Caption = 
      'Use string interning for less memory consumption (has a minor im' +
      'pact on speed)'
    TabOrder = 2
    ExplicitTop = 350
  end
  object CommentsBox: TListBox
    Left = 0
    Top = 335
    Width = 666
    Height = 59
    Align = alClient
    ItemHeight = 13
    TabOrder = 3
    ExplicitTop = 288
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 666
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitLeft = 88
    ExplicitTop = 8
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 16
      Top = 14
      Width = 63
      Height = 13
      Caption = 'Syntax Tree:'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 294
    Width = 666
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    ExplicitLeft = 184
    ExplicitTop = 272
    ExplicitWidth = 185
    object Label2: TLabel
      Left = 16
      Top = 14
      Width = 86
      Height = 13
      Caption = 'List of Comments:'
    end
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 96
    object OpenDelphiUnit1: TMenuItem
      Caption = 'Open Delphi Unit...'
      OnClick = OpenDelphiUnit1Click
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Unit|*.pas|Delphi Package|*.dpk|Delphi Project|*.dpr'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 96
  end
end
