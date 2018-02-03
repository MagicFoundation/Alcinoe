object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'MsBuild condition evaluator'
  ClientHeight = 536
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelProperties: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    Caption = '&Properties:'
    FocusControl = MemoProperties
  end
  object LabelConditions: TLabel
    Left = 8
    Top = 151
    Width = 54
    Height = 13
    Caption = '&Conditions:'
    FocusControl = MemoConditions
  end
  object Label1: TLabel
    Left = 8
    Top = 341
    Width = 39
    Height = 13
    Caption = '&Results:'
    FocusControl = MemoResults
  end
  object MemoProperties: TMemo
    Left = 8
    Top = 27
    Width = 451
    Height = 118
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'ZERO=0'
      'HUNDRED=100')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MemoConditions: TMemo
    Left = 8
    Top = 170
    Width = 451
    Height = 134
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      #39'$(HUNDRED)'#39' == 99'
      #39'$(HUNDRED)'#39' == '#39'0x$(zero)64'#39
      #39'$(HUNDRED)'#39' != 99'
      #39'$(HUNDRED)'#39' != 100'
      #39'$(HUNDRED)'#39' < 100'
      #39'$(HUNDRED)'#39' < 101'
      #39'$(HUNDRED)'#39' > 100'
      #39'$(HUNDRED)'#39' > 99'
      '(99 < '#39'$(HUNDRED)'#39') and ('#39'$(HUNDRED)'#39' < 101)'
      '(99 < '#39'$(HUNDRED)'#39') and ('#39'$(HUNDRED)'#39' < 100)'
      '(100 < '#39'$(HUNDRED)'#39') and ('#39'$(HUNDRED)'#39' < 101)'
      'Exists(toto)'
      'HasTrailingSlash(toto)'
      '!HasTrailingSlash(toto)'
      'HasTrailingSlash('#39'toto\'#39')')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object MemoResults: TMemo
    Left = 8
    Top = 360
    Width = 451
    Height = 168
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ButtonEval: TButton
    Left = 8
    Top = 310
    Width = 75
    Height = 25
    Caption = '&Eval'
    TabOrder = 3
    OnClick = ButtonEvalClick
  end
end
