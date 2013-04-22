object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 610
  Color = 15986666
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
    Top = 222
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
    Top = 345
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
    Top = 345
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
    Top = 542
    Width = 610
    Height = 19
    Panels = <
      item
        Width = 140
      end
      item
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object ALButton3: TALButton
    Left = 24
    Top = 39
    Width = 124
    Height = 25
    Caption = 'Bench AES (CBC)'
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
    Top = 241
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
    Top = 364
    Width = 266
    Height = 165
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 5
    DesignSize = (
      266
      165)
  end
  object ALMemoCryptedData: TALMemo
    Left = 296
    Top = 364
    Width = 290
    Height = 165
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 6
    DesignSize = (
      290
      165)
  end
  object ALButton2: TALButton
    Left = 296
    Top = 177
    Width = 97
    Height = 25
    Caption = 'Crypt (BF)'
    TabOrder = 7
    OnClick = ALButton2Click
    OnPaint = ALButton1Paint
  end
  object ALButton5: TALButton
    Left = 399
    Top = 177
    Width = 115
    Height = 25
    Caption = 'DeCrypt (BF)'
    TabOrder = 8
    OnClick = ALButton5Click
    OnPaint = ALButton1Paint
  end
  object ALButton6: TALButton
    Left = 296
    Top = 208
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - EBC)'
    TabOrder = 9
    OnClick = ALButton6Click
    OnPaint = ALButton1Paint
  end
  object ALButton7: TALButton
    Left = 399
    Top = 208
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - EBC)'
    TabOrder = 10
    OnClick = ALButton7Click
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
    TabOrder = 11
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
  object ALButton10: TALButton
    Left = 159
    Top = 8
    Width = 124
    Height = 25
    Caption = 'Bench MD5'
    TabOrder = 12
    OnClick = ALButton10Click
    OnPaint = ALButton1Paint
  end
  object ALButton11: TALButton
    Left = 159
    Top = 39
    Width = 124
    Height = 25
    Caption = 'Bench SHA1'
    TabOrder = 13
    OnClick = ALButton11Click
    OnPaint = ALButton1Paint
  end
  object ALButton8: TALButton
    Left = 296
    Top = 239
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - CBC)'
    TabOrder = 14
    OnClick = ALButton8Click
    OnPaint = ALButton1Paint
  end
  object ALButton9: TALButton
    Left = 399
    Top = 239
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - CBC)'
    TabOrder = 15
    OnClick = ALButton9Click
    OnPaint = ALButton1Paint
  end
  object ALButton12: TALButton
    Left = 296
    Top = 270
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (SHA1)'
    TabOrder = 16
    OnClick = ALButton12Click
    OnPaint = ALButton1Paint
  end
  object ALButton13: TALButton
    Left = 296
    Top = 301
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (MD5)'
    TabOrder = 17
    OnClick = ALButton13Click
    OnPaint = ALButton1Paint
  end
end
