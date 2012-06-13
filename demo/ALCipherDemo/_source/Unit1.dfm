object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 508
  ClientWidth = 610
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 169
    Width = 22
    Height = 13
    Caption = 'Key'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 24
    Top = 225
    Width = 104
    Height = 13
    Caption = 'Unencrypted Data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 296
    Top = 225
    Width = 187
    Height = 13
    Caption = 'Crypted Data (BASE64 Encoded)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ALButton1: TALButton
    Left = 24
    Top = 8
    Width = 124
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 0
    OnClick = ALButton1Click
    OnPaint = ALButton1Paint
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 489
    Width = 610
    Height = 19
    Panels = <
      item
        Width = 160
      end
      item
        Width = 160
      end
      item
        Width = 50
      end>
    ExplicitTop = 427
    ExplicitWidth = 603
  end
  object ALButton3: TALButton
    Left = 24
    Top = 39
    Width = 124
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 2
    OnClick = ALButton3Click
    OnPaint = ALButton1Paint
  end
  object ALButton4: TALButton
    Left = 24
    Top = 70
    Width = 124
    Height = 25
    Caption = 'Bench Blowfish (EBC)'
    TabOrder = 3
    OnClick = ALButton4Click
    OnPaint = ALButton1Paint
  end
  object EditKey: TALEdit
    Left = 24
    Top = 188
    Width = 266
    Height = 19
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = EditKeyPaint
    TabOrder = 4
  end
  object ALMemoDecryptedData: TALMemo
    Left = 24
    Top = 244
    Width = 266
    Height = 229
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 5
    DesignSize = (
      266
      229)
  end
  object ALMemoCryptedData: TALMemo
    Left = 296
    Top = 244
    Width = 290
    Height = 229
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 6
    DesignSize = (
      290
      229)
  end
  object ALButton2: TALButton
    Left = 296
    Top = 183
    Width = 57
    Height = 25
    Caption = 'Crypt (BF)'
    TabOrder = 7
    OnClick = ALButton2Click
    OnPaint = ALButton1Paint
  end
  object ALButton5: TALButton
    Left = 359
    Top = 183
    Width = 74
    Height = 25
    Caption = 'DeCrypt (BF)'
    TabOrder = 8
    OnClick = ALButton5Click
    OnPaint = ALButton1Paint
  end
  object ALButton6: TALButton
    Left = 439
    Top = 183
    Width = 66
    Height = 25
    Caption = 'Crypt (AES)'
    TabOrder = 9
    OnClick = ALButton6Click
    OnPaint = ALButton1Paint
  end
  object ALButton7: TALButton
    Left = 511
    Top = 183
    Width = 75
    Height = 25
    Caption = 'DeCrypt (AES)'
    TabOrder = 10
    OnClick = ALButton7Click
    OnPaint = ALButton1Paint
  end
  object ALButton8: TALButton
    Left = 164
    Top = 8
    Width = 116
    Height = 25
    Caption = 'Check SHA1 Hash'
    TabOrder = 11
    OnClick = ALButton8Click
    OnPaint = ALButton1Paint
  end
  object ALButton9: TALButton
    Left = 164
    Top = 39
    Width = 116
    Height = 25
    Caption = 'Check MD5 Hash'
    TabOrder = 12
    OnClick = ALButton9Click
    OnPaint = ALButton1Paint
  end
  object Panel2: TPanel
    Left = 296
    Top = 8
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 13
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
