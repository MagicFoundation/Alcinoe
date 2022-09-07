object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 702
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
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStringReplace (AnsiString)'
    TabOrder = 0
    OnClick = ALButton1Click
  end
  object ALButton2: TButton
    Left = 266
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (Unicode)'
    TabOrder = 1
    OnClick = ALButton2Click
  end
  object ALButton3: TButton
    Left = 16
    Top = 144
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPosEx (AnsiString)'
    TabOrder = 2
    OnClick = ALButton3Click
  end
  object ALButton4: TButton
    Left = 266
    Top = 144
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (Unicode)'
    TabOrder = 3
    OnClick = ALButton4Click
  end
  object ALButton5: TButton
    Left = 16
    Top = 210
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPos (AnsiString)'
    TabOrder = 4
    OnClick = ALButton5Click
  end
  object ALButton6: TButton
    Left = 266
    Top = 210
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (Unicode)'
    TabOrder = 5
    OnClick = ALButton6Click
  end
  object ALButton7: TButton
    Left = 16
    Top = 242
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCompareText (AnsiString)'
    TabOrder = 6
    OnClick = ALButton7Click
  end
  object ALButton8: TButton
    Left = 266
    Top = 242
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (Unicode)'
    TabOrder = 7
    OnClick = ALButton8Click
  end
  object ALButton9: TButton
    Left = 16
    Top = 274
    Width = 230
    Height = 25
    Caption = 'Benchmark ALUpperCase (AnsiString)'
    TabOrder = 8
    OnClick = ALButton9Click
  end
  object ALButton10: TButton
    Left = 266
    Top = 275
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (Unicode)'
    TabOrder = 9
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 16
    Top = 306
    Width = 230
    Height = 25
    Caption = 'Benchmark ALLowerCase (AnsiString)'
    TabOrder = 10
    OnClick = ALButton11Click
  end
  object ALButton12: TButton
    Left = 266
    Top = 306
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (Unicode)'
    TabOrder = 11
    OnClick = ALButton12Click
  end
  object ALButton13: TButton
    Left = 16
    Top = 338
    Width = 230
    Height = 25
    Caption = 'Benchmark ALCopyStr (AnsiString)'
    TabOrder = 12
    OnClick = ALButton13Click
  end
  object ALButton14: TButton
    Left = 266
    Top = 338
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (Unicode)'
    TabOrder = 13
    OnClick = ALButton14Click
  end
  object ALButton15: TButton
    Left = 16
    Top = 370
    Width = 230
    Height = 25
    Caption = 'Benchmark ALIntToStr (AnsiString)'
    TabOrder = 14
    OnClick = ALButton15Click
  end
  object ALButton16: TButton
    Left = 266
    Top = 370
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi IntToStr (Unicode)'
    TabOrder = 15
    OnClick = ALButton16Click
  end
  object ALButton17: TButton
    Left = 16
    Top = 402
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt (AnsiString)'
    TabOrder = 16
    OnClick = ALButton17Click
  end
  object ALButton18: TButton
    Left = 266
    Top = 402
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt (Unicode)'
    TabOrder = 17
    OnClick = ALButton18Click
  end
  object ALButton19: TButton
    Left = 16
    Top = 434
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToInt64 (AnsiString)'
    TabOrder = 18
    OnClick = ALButton19Click
  end
  object ALButton20: TButton
    Left = 266
    Top = 434
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToInt64 (Unicode)'
    TabOrder = 19
    OnClick = ALButton20Click
  end
  object ALButton21: TButton
    Left = 16
    Top = 466
    Width = 230
    Height = 25
    Caption = 'Benchmark ALDateToStr (AnsiString)'
    TabOrder = 20
    OnClick = ALButton21Click
  end
  object ALButton22: TButton
    Left = 266
    Top = 466
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi DateToStr (Unicode)'
    TabOrder = 21
    OnClick = ALButton22Click
  end
  object ALButton23: TButton
    Left = 516
    Top = 625
    Width = 230
    Height = 25
    Caption = 'Benchmark AnsiString Memory Usage'
    TabOrder = 22
    OnClick = ALButton23Click
  end
  object ALButton24: TButton
    Left = 266
    Top = 626
    Width = 230
    Height = 25
    Caption = 'Benchmark UnicodeString Memory Usage'
    TabOrder = 23
    OnClick = ALButton24Click
  end
  object ALButton25: TButton
    Left = 16
    Top = 530
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFloatToStr (AnsiString)'
    TabOrder = 24
    OnClick = ALButton25Click
  end
  object ALButton26: TButton
    Left = 266
    Top = 530
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi FloatToStr (Unicode)'
    TabOrder = 25
    OnClick = ALButton26Click
  end
  object ALButton27: TButton
    Left = 16
    Top = 498
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToDateTime (AnsiString)'
    TabOrder = 26
    OnClick = ALButton27Click
  end
  object ALButton28: TButton
    Left = 266
    Top = 498
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToDateTime (Unicode)'
    TabOrder = 27
    OnClick = ALButton28Click
  end
  object ALButton29: TButton
    Left = 516
    Top = 112
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StringReplace (AnsiString)'
    TabOrder = 28
    OnClick = ALButton29Click
  end
  object ALButton30: TButton
    Left = 516
    Top = 144
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi PosEx (AnsiString)'
    TabOrder = 29
    OnClick = ALButton30Click
  end
  object ALButton31: TButton
    Left = 516
    Top = 210
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Pos (AnsiString)'
    TabOrder = 30
    OnClick = ALButton31Click
  end
  object ALButton32: TButton
    Left = 516
    Top = 242
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi CompareText (AnsiString)'
    TabOrder = 31
    OnClick = ALButton32Click
  end
  object ALButton33: TButton
    Left = 516
    Top = 275
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi UpperCase (AnsiString)'
    TabOrder = 32
    OnClick = ALButton33Click
  end
  object ALButton34: TButton
    Left = 516
    Top = 306
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi LowerCase (AnsiString)'
    TabOrder = 33
    OnClick = ALButton34Click
  end
  object ALButton35: TButton
    Left = 516
    Top = 338
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Copy (AnsiString)'
    TabOrder = 34
    OnClick = ALButton35Click
  end
  object ALButton40: TButton
    Left = 16
    Top = 562
    Width = 230
    Height = 25
    Caption = 'Benchmark ALFormat (AnsiString)'
    TabOrder = 35
    OnClick = ALButton40Click
  end
  object ALButton43: TButton
    Left = 266
    Top = 562
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Format (Unicode)'
    TabOrder = 36
    OnClick = ALButton43Click
  end
  object ALButton45: TButton
    Left = 16
    Top = 594
    Width = 230
    Height = 25
    Caption = 'Benchmark ALStrToFloat (AnsiString)'
    TabOrder = 37
    OnClick = ALButton45Click
  end
  object ALButton46: TButton
    Left = 266
    Top = 594
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi StrToFloat (Unicode)'
    TabOrder = 38
    OnClick = ALButton46Click
  end
  object Button1: TButton
    Left = 16
    Top = 657
    Width = 230
    Height = 25
    Caption = 'Benchmark ALBase64Encode (AnsiString)'
    TabOrder = 39
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 266
    Top = 657
    Width = 230
    Height = 25
    Caption = 'Benchmark Delphi Base64Encode (Unicode)'
    TabOrder = 40
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 177
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPosExIgnoreCase (AnsiString)'
    TabOrder = 41
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 266
    Top = 177
    Width = 230
    Height = 25
    Caption = 'Benchmark ALPosExIgnoreCaseU (Unicode)'
    TabOrder = 42
    OnClick = Button4Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 763
    Height = 97
    Align = alTop
    Color = clPurple
    ParentBackground = False
    TabOrder = 43
    object Label1: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 745
      Height = 80
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 
        'You must run this demo in "Release" to see accurate benchmark. P' +
        'urpose of ALString is not to compete in speed with RTL functions' +
        ' but to offer 8 bit (ansiString) alternative version of the 16 b' +
        'it (UnicodeString) version of all tiny string functions. Speed i' +
        's very fast in Win32 thanks to the ASM customization made by the' +
        ' fastcode project'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 737
    end
  end
end
