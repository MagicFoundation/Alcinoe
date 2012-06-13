object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 258
  ClientWidth = 493
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
    Width = 225
    Height = 25
    Caption = 'Benchmark TALStringList (10 000 items)'
    TabOrder = 0
    OnClick = ALButton1Click
    OnPaint = ALButtonPaint
  end
  object ALButton2: TALButton
    Left = 247
    Top = 16
    Width = 226
    Height = 25
    Caption = 'Benchmark Delphi TStringList (10 000 items)'
    TabOrder = 1
    OnClick = ALButton2Click
    OnPaint = ALButtonPaint
  end
  object ALButton3: TALButton
    Left = 16
    Top = 48
    Width = 225
    Height = 25
    Caption = 'Benchmark TALAVLStringList (100 000 items)'
    TabOrder = 2
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object ALButton4: TALButton
    Left = 247
    Top = 47
    Width = 226
    Height = 25
    Caption = 'Benchmark Delphi TStringList (100 000 items)'
    TabOrder = 3
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
  object Panel2: TPanel
    Left = 72
    Top = 88
    Width = 344
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 4
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
