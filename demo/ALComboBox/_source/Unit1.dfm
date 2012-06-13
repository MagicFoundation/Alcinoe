object Form1: TForm1
  Left = 478
  Top = 420
  Caption = 'Form1'
  ClientHeight = 242
  ClientWidth = 348
  Color = 14805482
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 81
    Height = 14
    Caption = 'TALComboBox'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 16
    Width = 67
    Height = 14
    Caption = 'TComboBox'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ComboBox1: TComboBox
    Left = 184
    Top = 32
    Width = 145
    Height = 22
    ItemHeight = 14
    TabOrder = 0
    Text = 'ComboBox1'
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6')
  end
  object ALComboBox1: TALComboBox
    Left = 16
    Top = 32
    Width = 145
    Height = 22
    OnPaint = ALComboBox1Paint
    ItemHeight = 14
    TabOrder = 1
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6')
  end
  object Panel2: TPanel
    Left = 27
    Top = 75
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 2
    object Label7: TLabel
      Left = 5
      Top = 8
      Width = 132
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
      Left = 5
      Top = 63
      Width = 125
      Height = 75
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
      Left = 151
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
