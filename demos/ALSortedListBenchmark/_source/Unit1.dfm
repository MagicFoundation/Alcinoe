object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 331
  ClientWidth = 500
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
    Top = 17
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringKeyAVLBinaryTree'
    TabOrder = 0
    OnClick = ALButton3Click
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
    object cxLabel1: TcxLabel
      Left = 12
      Top = 12
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
  object ALButton2: TButton
    Left = 258
    Top = 17
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 2
    OnClick = ALButton4Click
  end
  object ALButton5: TButton
    Left = 16
    Top = 49
    Width = 220
    Height = 25
    Caption = 'Benchmark TALInt64AVLList'
    TabOrder = 3
    OnClick = ALButton5Click
  end
  object ALButton6: TButton
    Left = 258
    Top = 49
    Width = 220
    Height = 25
    Caption = 'Benchmark TALInt64List'
    TabOrder = 4
    OnClick = ALButton6Click
  end
  object ALButton10: TButton
    Left = 16
    Top = 80
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringList'
    TabOrder = 5
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 258
    Top = 80
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 6
    OnClick = ALButton11Click
  end
  object ALButton33: TButton
    Left = 16
    Top = 112
    Width = 220
    Height = 25
    Caption = 'Benchmark TALAVLStringList'
    TabOrder = 7
    OnClick = ALButton33Click
  end
  object ALButton34: TButton
    Left = 258
    Top = 111
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList'
    TabOrder = 8
    OnClick = ALButton4Click
  end
end
