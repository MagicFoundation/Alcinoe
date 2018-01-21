object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 760
  ClientWidth = 1034
  Color = clBtnFace
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
    Top = 262
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
    Top = 321
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
    Left = 384
    Top = 321
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
  object Label4: TLabel
    Left = 24
    Top = 517
    Width = 54
    Height = 13
    Caption = 'Collisions'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PaintBox1: TPaintBox
    Left = 0
    Top = 240
    Width = 1034
    Height = 501
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Visible = False
    ExplicitTop = 234
  end
  object ALButton1: TButton
    Left = 24
    Top = 16
    Width = 140
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 0
    OnClick = ALButton1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 741
    Width = 1034
    Height = 19
    Panels = <
      item
        Width = 250
      end
      item
        Width = 250
      end
      item
        Width = 140
      end
      item
        Width = 100
      end
      item
        Width = 100
      end>
  end
  object ALButton3: TButton
    Left = 24
    Top = 47
    Width = 140
    Height = 25
    Caption = 'Bench AES (CBC)'
    TabOrder = 2
    OnClick = ALButton3Click
  end
  object ALButton4: TButton
    Left = 24
    Top = 78
    Width = 140
    Height = 25
    Caption = 'Bench Blowfish (EBC)'
    TabOrder = 3
    OnClick = ALButton4Click
  end
  object EditKey: TEdit
    Left = 24
    Top = 281
    Width = 345
    Height = 21
    TabOrder = 4
  end
  object ALMemoDecryptedData: TMemo
    Left = 24
    Top = 340
    Width = 345
    Height = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object ALMemoCryptedData: TMemo
    Left = 384
    Top = 340
    Width = 631
    Height = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object ALButton2: TButton
    Left = 797
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Crypt (BF)'
    TabOrder = 7
    OnClick = ALButton2Click
  end
  object ALButton5: TButton
    Left = 900
    Top = 16
    Width = 115
    Height = 25
    Caption = 'DeCrypt (BF)'
    TabOrder = 8
    OnClick = ALButton5Click
  end
  object ALButton6: TButton
    Left = 797
    Top = 47
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - EBC)'
    TabOrder = 9
    OnClick = ALButton6Click
  end
  object ALButton7: TButton
    Left = 900
    Top = 47
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - EBC)'
    TabOrder = 10
    OnClick = ALButton7Click
  end
  object ALButton10: TButton
    Left = 490
    Top = 47
    Width = 136
    Height = 25
    Caption = 'Bench ALMD5'
    TabOrder = 11
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 490
    Top = 109
    Width = 136
    Height = 25
    Caption = 'Bench ALSHA1'
    TabOrder = 12
    OnClick = ALButton11Click
  end
  object ALButton8: TButton
    Left = 797
    Top = 78
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - CBC)'
    TabOrder = 13
    OnClick = ALButton8Click
  end
  object ALButton9: TButton
    Left = 900
    Top = 78
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - CBC)'
    TabOrder = 14
    OnClick = ALButton9Click
  end
  object ALButton12: TButton
    Left = 797
    Top = 109
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (SHA1)'
    TabOrder = 15
    OnClick = ALButton12Click
  end
  object ALButton13: TButton
    Left = 797
    Top = 140
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (MD5)'
    TabOrder = 16
    OnClick = ALButton13Click
  end
  object Button1: TButton
    Left = 490
    Top = 140
    Width = 134
    Height = 25
    Caption = 'Bench CRC32 (Zlib)'
    TabOrder = 17
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 490
    Top = 171
    Width = 134
    Height = 25
    Caption = 'Bench ALCRC32'
    TabOrder = 18
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 640
    Top = 140
    Width = 136
    Height = 25
    Caption = 'Bench BobJenkinsHash'
    TabOrder = 19
    OnClick = Button3Click
  end
  object ALMemoCollisions: TMemo
    Left = 24
    Top = 536
    Width = 991
    Height = 189
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 20
    OnChange = ALMemoCollisionsChange
  end
  object Button4: TButton
    Left = 640
    Top = 109
    Width = 136
    Height = 25
    Caption = 'Bench FNV-1a (int64)'
    TabOrder = 21
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 640
    Top = 78
    Width = 136
    Height = 25
    Caption = 'Bench FNV-1a (int32)'
    TabOrder = 22
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 640
    Top = 16
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom32'
    TabOrder = 23
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 640
    Top = 47
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom64'
    TabOrder = 24
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 176
    Top = 78
    Width = 225
    Height = 25
    Caption = 'Bench BCrypt (12)'
    TabOrder = 25
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 176
    Top = 109
    Width = 225
    Height = 25
    Caption = 'Generate BCrypt (12) Hash'
    TabOrder = 26
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 176
    Top = 47
    Width = 225
    Height = 25
    Caption = 'Bench ALBase64Encode (ansiString)'
    TabOrder = 27
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 24
    Top = 109
    Width = 140
    Height = 25
    Caption = 'Bench HexEncode'
    TabOrder = 28
    OnClick = Button11Click
  end
  object Button13: TButton
    Left = 490
    Top = 16
    Width = 136
    Height = 25
    Caption = 'Bench MD5'
    TabOrder = 29
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 490
    Top = 78
    Width = 136
    Height = 25
    Caption = 'Bench SHA1'
    TabOrder = 30
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 176
    Top = 16
    Width = 225
    Height = 25
    Caption = 'Bench ALBase64EncodeU (UnicodeString)'
    TabOrder = 31
    OnClick = Button15Click
  end
end
