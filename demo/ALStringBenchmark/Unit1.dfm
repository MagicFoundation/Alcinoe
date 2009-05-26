object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 322
  ClientWidth = 368
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 80
    Top = 248
    Width = 200
    Height = 70
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 0
    object Label5: TLabel
      Left = 5
      Top = 8
      Width = 189
      Height = 52
      Caption = 
        'Please add in your website a link to http://www.arkadia.com or s' +
        'end me an email to svanderclock@arkadia.com if you like this com' +
        'ponent!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object ALButton1: TALButton
    Left = 16
    Top = 16
    Width = 153
    Height = 25
    Caption = 'Benchmark AlStringReplace'
    TabOrder = 1
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 184
    Top = 16
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi StringReplace'
    TabOrder = 2
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALButton3: TALButton
    Left = 16
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Benchmark AlPosEx'
    TabOrder = 3
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object ALButton4: TALButton
    Left = 184
    Top = 48
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi PosEx'
    TabOrder = 4
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
  object ALButton5: TALButton
    Left = 16
    Top = 80
    Width = 153
    Height = 25
    Caption = 'Benchmark ALPos'
    TabOrder = 5
    OnClick = ALButton5Click
    OnPaint = ALButtonPaint
  end
  object ALButton6: TALButton
    Left = 184
    Top = 80
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi Pos'
    TabOrder = 6
    OnClick = ALButton6Click
    OnPaint = ALButtonPaint
  end
  object ALButton7: TALButton
    Left = 16
    Top = 112
    Width = 153
    Height = 25
    Caption = 'Benchmark ALCompareText'
    TabOrder = 7
    OnClick = ALButton7Click
    OnPaint = ALButtonPaint
  end
  object ALButton8: TALButton
    Left = 184
    Top = 112
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi CompareText'
    TabOrder = 8
    OnClick = ALButton8Click
    OnPaint = ALButtonPaint
  end
  object ALButton9: TALButton
    Left = 16
    Top = 144
    Width = 153
    Height = 25
    Caption = 'Benchmark ALUpperCase'
    TabOrder = 9
    OnClick = ALButton9Click
    OnPaint = ALButtonPaint
  end
  object ALButton10: TALButton
    Left = 184
    Top = 144
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi UpperCase'
    TabOrder = 10
    OnClick = ALButton10Click
    OnPaint = ALButtonPaint
  end
  object ALButton11: TALButton
    Left = 16
    Top = 176
    Width = 153
    Height = 25
    Caption = 'Benchmark ALLowerCase'
    TabOrder = 11
    OnClick = ALButton11Click
    OnPaint = ALButtonPaint
  end
  object ALButton12: TALButton
    Left = 184
    Top = 176
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi LowerCase'
    TabOrder = 12
    OnClick = ALButton12Click
    OnPaint = ALButtonPaint
  end
  object ALButton13: TALButton
    Left = 16
    Top = 208
    Width = 153
    Height = 25
    Caption = 'Benchmark ALCopyStr'
    TabOrder = 13
    OnClick = ALButton13Click
    OnPaint = ALButtonPaint
  end
  object ALButton14: TALButton
    Left = 184
    Top = 208
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi Copy'
    TabOrder = 14
    OnClick = ALButton14Click
    OnPaint = ALButtonPaint
  end
end
