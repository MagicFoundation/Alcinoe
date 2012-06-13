object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 421
  ClientWidth = 379
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ALButton1: TALButton
    Left = 16
    Top = 16
    Width = 153
    Height = 25
    Caption = 'Benchmark AlStringReplace'
    TabOrder = 0
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 184
    Top = 16
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi StringReplace'
    TabOrder = 1
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALButton3: TALButton
    Left = 16
    Top = 48
    Width = 153
    Height = 25
    Caption = 'Benchmark AlPosEx'
    TabOrder = 2
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object ALButton4: TALButton
    Left = 184
    Top = 48
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi PosEx'
    TabOrder = 3
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
  object ALButton5: TALButton
    Left = 16
    Top = 80
    Width = 153
    Height = 25
    Caption = 'Benchmark ALPos'
    TabOrder = 4
    OnClick = ALButton5Click
    OnPaint = ALButtonPaint
  end
  object ALButton6: TALButton
    Left = 184
    Top = 80
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi Pos'
    TabOrder = 5
    OnClick = ALButton6Click
    OnPaint = ALButtonPaint
  end
  object ALButton7: TALButton
    Left = 16
    Top = 112
    Width = 153
    Height = 25
    Caption = 'Benchmark ALCompareText'
    TabOrder = 6
    OnClick = ALButton7Click
    OnPaint = ALButtonPaint
  end
  object ALButton8: TALButton
    Left = 184
    Top = 112
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi CompareText'
    TabOrder = 7
    OnClick = ALButton8Click
    OnPaint = ALButtonPaint
  end
  object ALButton9: TALButton
    Left = 16
    Top = 144
    Width = 153
    Height = 25
    Caption = 'Benchmark ALUpperCase'
    TabOrder = 8
    OnClick = ALButton9Click
    OnPaint = ALButtonPaint
  end
  object ALButton10: TALButton
    Left = 184
    Top = 144
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi UpperCase'
    TabOrder = 9
    OnClick = ALButton10Click
    OnPaint = ALButtonPaint
  end
  object ALButton11: TALButton
    Left = 16
    Top = 176
    Width = 153
    Height = 25
    Caption = 'Benchmark ALLowerCase'
    TabOrder = 10
    OnClick = ALButton11Click
    OnPaint = ALButtonPaint
  end
  object ALButton12: TALButton
    Left = 184
    Top = 176
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi LowerCase'
    TabOrder = 11
    OnClick = ALButton12Click
    OnPaint = ALButtonPaint
  end
  object ALButton13: TALButton
    Left = 16
    Top = 208
    Width = 153
    Height = 25
    Caption = 'Benchmark ALCopyStr'
    TabOrder = 12
    OnClick = ALButton13Click
    OnPaint = ALButtonPaint
  end
  object ALButton14: TALButton
    Left = 184
    Top = 208
    Width = 177
    Height = 25
    Caption = 'Benchmark Delphi Copy'
    TabOrder = 13
    OnClick = ALButton14Click
    OnPaint = ALButtonPaint
  end
  object Panel2: TPanel
    Left = 16
    Top = 248
    Width = 344
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 14
    object Label7: TLabel
      Left = 10
      Top = 14
      Width = 152
      Height = 45
      Caption = 'Please help us to keep the development of these components free'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label8: TLabel
      Left = 10
      Top = 69
      Width = 168
      Height = 60
      Caption = 
        'If you like these components please simply click on each button ' +
        'below ... thanks for your support !'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel3: TPanel
      Left = 201
      Top = 8
      Width = 130
      Height = 134
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clWhite
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object PanelWebBrowser: TPanel
        Left = -5
        Top = -23
        Width = 133
        Height = 159
        BevelOuter = bvNone
        Color = clMedGray
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 0
      end
    end
  end
end
