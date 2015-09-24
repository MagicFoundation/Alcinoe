object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 775
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object ALButton1: TButton
    Left = 16
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStringReplace (AnsiString)'
    TabOrder = 0
    OnClick = ALButton1Click
  end
  object ALButton2: TButton
    Left = 266
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (Unicode)'
    TabOrder = 1
    OnClick = ALButton2Click
  end
  object ALButton3: TButton
    Left = 16
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPosEx (AnsiString)'
    TabOrder = 2
    OnClick = ALButton3Click
  end
  object ALButton4: TButton
    Left = 266
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (Unicode)'
    TabOrder = 3
    OnClick = ALButton4Click
  end
  object ALButton5: TButton
    Left = 16
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPos (AnsiString)'
    TabOrder = 4
    OnClick = ALButton5Click
  end
  object ALButton6: TButton
    Left = 266
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (Unicode)'
    TabOrder = 5
    OnClick = ALButton6Click
  end
  object ALButton7: TButton
    Left = 16
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCompareText (AnsiString)'
    TabOrder = 6
    OnClick = ALButton7Click
  end
  object ALButton8: TButton
    Left = 266
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (Unicode)'
    TabOrder = 7
    OnClick = ALButton8Click
  end
  object ALButton9: TButton
    Left = 16
    Top = 144
    Width = 230
    Height = 25
    Caption = 'Benchmark ALUpperCase (AnsiString)'
    TabOrder = 8
    OnClick = ALButton9Click
  end
  object ALButton10: TButton
    Left = 266
    Top = 145
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (Unicode)'
    TabOrder = 9
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 16
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark ALLowerCase (AnsiString)'
    TabOrder = 10
    OnClick = ALButton11Click
  end
  object ALButton12: TButton
    Left = 266
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (Unicode)'
    TabOrder = 11
    OnClick = ALButton12Click
  end
  object ALButton13: TButton
    Left = 16
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCopyStr (AnsiString)'
    TabOrder = 12
    OnClick = ALButton13Click
  end
  object ALButton14: TButton
    Left = 266
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (Unicode)'
    TabOrder = 13
    OnClick = ALButton14Click
  end
  object Panel2: TPanel
    Left = 16
    Top = 569
    Width = 345
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 14
    object cxLabel1: TcxLabel
      Left = 12
      Top = 12
      Hint = ''
      Caption = 'Please help us to keep the development of these components free'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 242
    end
    object cxLabel2: TcxLabel
      Left = 12
      Top = 55
      Hint = ''
      Caption = 'If you like these components please go to:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 240
    end
    object cxWwwArkadiaComLabel: TcxLabel
      Left = 12
      Top = 71
      Cursor = crHandPoint
      Hint = ''
      Caption = 'http://www.arkadia.com'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clRed
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.TextColor = clMaroon
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      OnClick = cxWwwArkadiaComLabelClick
      Width = 160
    end
    object cxLabel18: TcxLabel
      Left = 12
      Top = 88
      Hint = ''
      Caption = 'and click on the Facebook/Google+ like button'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 261
    end
    object cxLabel17: TcxLabel
      Left = 12
      Top = 120
      Hint = ''
      Caption = 'Thanks for your support !'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 144
    end
  end
  object ALButton15: TButton
    Left = 16
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark ALIntToStr (AnsiString)'
    TabOrder = 15
    OnClick = ALButton15Click
  end
  object ALButton16: TButton
    Left = 266
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi IntToStr (Unicode)'
    TabOrder = 16
    OnClick = ALButton16Click
  end
  object ALButton17: TButton
    Left = 16
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt (AnsiString)'
    TabOrder = 17
    OnClick = ALButton17Click
  end
  object ALButton18: TButton
    Left = 266
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt (Unicode)'
    TabOrder = 18
    OnClick = ALButton18Click
  end
  object ALButton19: TButton
    Left = 16
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt64 (AnsiString)'
    TabOrder = 19
    OnClick = ALButton19Click
  end
  object ALButton20: TButton
    Left = 266
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt64 (Unicode)'
    TabOrder = 20
    OnClick = ALButton20Click
  end
  object ALButton21: TButton
    Left = 16
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark ALDateToStr (AnsiString)'
    TabOrder = 21
    OnClick = ALButton21Click
  end
  object ALButton22: TButton
    Left = 266
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi DateToStr (Unicode)'
    TabOrder = 22
    OnClick = ALButton22Click
  end
  object ALButton23: TButton
    Left = 16
    Top = 496
    Width = 230
    Height = 25
    Caption = 'Benchmark AnsiString Memory Usage'
    TabOrder = 23
    OnClick = ALButton23Click
  end
  object ALButton24: TButton
    Left = 266
    Top = 496
    Width = 230
    Height = 25
    Caption = 'Benchmark UnicodeString Memory Usage'
    TabOrder = 24
    OnClick = ALButton24Click
  end
  object ALButton25: TButton
    Left = 16
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFloatToStr (AnsiString)'
    TabOrder = 25
    OnClick = ALButton25Click
  end
  object ALButton26: TButton
    Left = 266
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi FloatToStr (Unicode)'
    TabOrder = 26
    OnClick = ALButton26Click
  end
  object ALButton27: TButton
    Left = 16
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToDateTime (AnsiString)'
    TabOrder = 27
    OnClick = ALButton27Click
  end
  object ALButton28: TButton
    Left = 266
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToDateTime (Unicode)'
    TabOrder = 28
    OnClick = ALButton28Click
  end
  object ALButton29: TButton
    Left = 516
    Top = 16
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (AnsiString)'
    TabOrder = 29
    OnClick = ALButton29Click
  end
  object ALButton30: TButton
    Left = 516
    Top = 48
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (AnsiString)'
    TabOrder = 30
    OnClick = ALButton30Click
  end
  object ALButton31: TButton
    Left = 516
    Top = 80
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (AnsiString)'
    TabOrder = 31
    OnClick = ALButton31Click
  end
  object ALButton32: TButton
    Left = 516
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (AnsiString)'
    TabOrder = 32
    OnClick = ALButton32Click
  end
  object ALButton33: TButton
    Left = 516
    Top = 145
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (AnsiString)'
    TabOrder = 33
    OnClick = ALButton33Click
  end
  object ALButton34: TButton
    Left = 516
    Top = 176
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (AnsiString)'
    TabOrder = 34
    OnClick = ALButton34Click
  end
  object ALButton35: TButton
    Left = 516
    Top = 208
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (AnsiString)'
    TabOrder = 35
    OnClick = ALButton35Click
  end
  object ALButton36: TButton
    Left = 516
    Top = 240
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi IntToStr (AnsiString)'
    TabOrder = 36
    OnClick = ALButton36Click
  end
  object ALButton37: TButton
    Left = 516
    Top = 272
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt (AnsiString)'
    TabOrder = 37
    OnClick = ALButton37Click
  end
  object ALButton38: TButton
    Left = 516
    Top = 304
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt64 (AnsiString)'
    TabOrder = 38
    OnClick = ALButton38Click
  end
  object ALButton39: TButton
    Left = 516
    Top = 336
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi DateToStr (AnsiString)'
    TabOrder = 39
    OnClick = ALButton39Click
  end
  object ALButton41: TButton
    Left = 516
    Top = 400
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi FloatToStr (AnsiString)'
    TabOrder = 40
    OnClick = ALButton41Click
  end
  object ALButton42: TButton
    Left = 516
    Top = 368
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToDateTime (AnsiString)'
    TabOrder = 41
    OnClick = ALButton42Click
  end
  object ALButton40: TButton
    Left = 16
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFormat (AnsiString)'
    TabOrder = 42
    OnClick = ALButton40Click
  end
  object ALButton43: TButton
    Left = 266
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Format (Unicode)'
    TabOrder = 43
    OnClick = ALButton43Click
  end
  object ALButton44: TButton
    Left = 516
    Top = 432
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Format (AnsiString)'
    TabOrder = 44
    OnClick = ALButton44Click
  end
  object ALButton45: TButton
    Left = 16
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToFloat (AnsiString)'
    TabOrder = 45
    OnClick = ALButton45Click
  end
  object ALButton46: TButton
    Left = 266
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToFloat (Unicode)'
    TabOrder = 46
    OnClick = ALButton46Click
  end
  object ALButton47: TButton
    Left = 516
    Top = 464
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToFloat (AnsiString)'
    TabOrder = 47
    OnClick = ALButton47Click
  end
  object Button1: TButton
    Left = 16
    Top = 527
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStringReplace'
    TabOrder = 48
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 266
    Top = 527
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFastTagReplace'
    TabOrder = 49
    OnClick = Button2Click
  end
end
