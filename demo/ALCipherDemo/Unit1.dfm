object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 446
  ClientWidth = 603
  Color = 14805482
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 117
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
    Top = 173
    Width = 89
    Height = 13
    Caption = 'deCrypted Data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 296
    Top = 173
    Width = 173
    Height = 13
    Caption = 'Crypt Data (BASE64 Encoded)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 386
    Top = 8
    Width = 200
    Height = 70
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 0
    object Label5: TLabel
      Left = 8
      Top = 9
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
    Left = 24
    Top = 8
    Width = 129
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 1
    OnClick = ALButton1Click
    OnPaint = ALButton1Paint
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 427
    Width = 603
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
    ExplicitTop = 444
    ExplicitWidth = 488
  end
  object ALButton3: TALButton
    Left = 24
    Top = 39
    Width = 129
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 3
    OnClick = ALButton3Click
    OnPaint = ALButton1Paint
  end
  object ALButton4: TALButton
    Left = 24
    Top = 70
    Width = 129
    Height = 25
    Caption = 'Bench Blowfish (EBC)'
    TabOrder = 4
    OnClick = ALButton4Click
    OnPaint = ALButton1Paint
  end
  object EditKey: TALEdit
    Left = 24
    Top = 136
    Width = 266
    Height = 20
    GlyphIndex = 0
    btnFont.Charset = DEFAULT_CHARSET
    btnFont.Color = clWindowText
    btnFont.Height = -11
    btnFont.Name = 'MS Sans Serif'
    btnFont.Style = []
    OnPaint = EditKeyPaint
    TabOrder = 5
  end
  object ALMemoDecryptedData: TALMemo
    Left = 24
    Top = 192
    Width = 266
    Height = 229
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 6
    DesignSize = (
      266
      229)
  end
  object ALMemoCryptedData: TALMemo
    Left = 296
    Top = 192
    Width = 290
    Height = 229
    OnPaint = ALMemoDecryptedDataPaint
    OnPaintScrollBar = ALMemoDecryptedDataPaintScrollBar
    TabOrder = 7
    DesignSize = (
      290
      229)
  end
  object ALButton2: TALButton
    Left = 296
    Top = 131
    Width = 57
    Height = 25
    Caption = 'Crypt (BF)'
    TabOrder = 8
    OnClick = ALButton2Click
    OnPaint = ALButton1Paint
  end
  object ALButton5: TALButton
    Left = 359
    Top = 131
    Width = 74
    Height = 25
    Caption = 'DeCrypt (BF)'
    TabOrder = 9
    OnClick = ALButton5Click
    OnPaint = ALButton1Paint
  end
  object ALButton6: TALButton
    Left = 439
    Top = 131
    Width = 66
    Height = 25
    Caption = 'Crypt (AES)'
    TabOrder = 10
    OnClick = ALButton6Click
    OnPaint = ALButton1Paint
  end
  object ALButton7: TALButton
    Left = 511
    Top = 131
    Width = 75
    Height = 25
    Caption = 'DeCrypt (AES)'
    TabOrder = 11
    OnClick = ALButton7Click
    OnPaint = ALButton1Paint
  end
  object ALButton8: TALButton
    Left = 209
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Check SHA1 Hash'
    TabOrder = 12
    OnClick = ALButton8Click
    OnPaint = ALButton1Paint
  end
  object ALButton9: TALButton
    Left = 209
    Top = 39
    Width = 121
    Height = 25
    Caption = 'Check MD5 Hash'
    TabOrder = 13
    OnClick = ALButton9Click
    OnPaint = ALButton1Paint
  end
end
