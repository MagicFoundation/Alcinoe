object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 172
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
    Top = 88
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
    Caption = 'Benchmark ALStringList'
    TabOrder = 1
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 183
    Top = 17
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi StringList'
    TabOrder = 2
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALButton3: TALButton
    Left = 16
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Benchmark ALAVLStringList'
    TabOrder = 3
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object ALButton4: TALButton
    Left = 184
    Top = 48
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi StringList'
    TabOrder = 4
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
end
