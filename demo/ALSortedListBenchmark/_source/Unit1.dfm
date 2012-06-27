object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 331
  ClientWidth = 500
  Color = 15986666
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
    Top = 17
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringKeyAVLBinaryTree'
    TabOrder = 0
    OnClick = ALButton3Click
    OnPaint = ALButtonPaint
  end
  object Panel2: TPanel
    Left = 80
    Top = 160
    Width = 344
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 1
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
  object ALButton2: TALButton
    Left = 258
    Top = 17
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 2
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
  object ALButton5: TALButton
    Left = 16
    Top = 49
    Width = 220
    Height = 25
    Caption = 'Benchmark TALInt64AVLList'
    TabOrder = 3
    OnClick = ALButton5Click
    OnPaint = ALButtonPaint
  end
  object ALButton6: TALButton
    Left = 258
    Top = 49
    Width = 220
    Height = 25
    Caption = 'Benchmark TALInt64List'
    TabOrder = 4
    OnClick = ALButton6Click
    OnPaint = ALButtonPaint
  end
  object ALButton10: TALButton
    Left = 16
    Top = 80
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringList'
    TabOrder = 5
    OnClick = ALButton10Click
    OnPaint = ALButtonPaint
  end
  object ALButton11: TALButton
    Left = 258
    Top = 80
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 6
    OnClick = ALButton11Click
    OnPaint = ALButtonPaint
  end
  object ALButton33: TALButton
    Left = 16
    Top = 112
    Width = 220
    Height = 25
    Caption = 'Benchmark TALAVLStringList'
    TabOrder = 7
    OnClick = ALButton33Click
    OnPaint = ALButtonPaint
  end
  object ALButton34: TALButton
    Left = 258
    Top = 111
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 8
    OnClick = ALButton4Click
    OnPaint = ALButtonPaint
  end
end
