object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 790
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
    Top = 285
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
    Top = 344
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
    Top = 344
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
    Top = 540
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
    Top = 264
    Width = 1034
    Height = 507
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Visible = False
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
    Top = 771
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
    Top = 304
    Width = 345
    Height = 21
    TabOrder = 4
  end
  object ALMemoDecryptedData: TMemo
    Left = 24
    Top = 363
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
    Top = 363
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
    Left = 413
    Top = 47
    Width = 225
    Height = 25
    Caption = 'Bench ALMD5 (ansiString)'
    TabOrder = 11
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 413
    Top = 109
    Width = 225
    Height = 25
    Caption = 'Bench ALSHA1 (ansiString)'
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
    Left = 24
    Top = 140
    Width = 140
    Height = 25
    Caption = 'Bench CRC32 (Zlib)'
    TabOrder = 17
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 171
    Width = 140
    Height = 25
    Caption = 'Bench ALCRC32'
    TabOrder = 18
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 651
    Top = 140
    Width = 136
    Height = 25
    Caption = 'Bench BobJenkinsHash'
    TabOrder = 19
    OnClick = Button3Click
  end
  object ALMemoCollisions: TMemo
    Left = 24
    Top = 559
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
    Left = 651
    Top = 109
    Width = 136
    Height = 25
    Caption = 'Bench ALFNV-1a (int64)'
    TabOrder = 21
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 651
    Top = 78
    Width = 136
    Height = 25
    Caption = 'Bench ALFNV-1a (int32)'
    TabOrder = 22
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 651
    Top = 16
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom32'
    TabOrder = 23
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 651
    Top = 47
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom64'
    TabOrder = 24
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 176
    Top = 140
    Width = 226
    Height = 25
    Caption = 'Bench BCrypt(12) (ansiString)'
    TabOrder = 25
    OnClick = Button8Click
  end
  object Button10: TButton
    Left = 176
    Top = 16
    Width = 225
    Height = 25
    Caption = 'Test/Bench ALBase64Encode (ansiString)'
    TabOrder = 26
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 176
    Top = 78
    Width = 225
    Height = 25
    Caption = 'Bench HexEncode (ansiString)'
    TabOrder = 27
    OnClick = Button11Click
  end
  object Button13: TButton
    Left = 413
    Top = 16
    Width = 225
    Height = 25
    Caption = 'Bench ALMD5 (Unicode)'
    TabOrder = 28
    OnClick = Button13Click
  end
  object Button15: TButton
    Left = 176
    Top = 47
    Width = 225
    Height = 25
    Caption = 'Test/Bench ALBase64EncodeU (UNICODE)'
    TabOrder = 29
    OnClick = Button15Click
  end
  object Button12: TButton
    Left = 414
    Top = 171
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 256 (ansiString)'
    TabOrder = 30
    OnClick = Button12Click
  end
  object Button16: TButton
    Left = 414
    Top = 202
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 512 (ansiString)'
    TabOrder = 31
    OnClick = Button16Click
  end
  object Button14: TButton
    Left = 414
    Top = 78
    Width = 225
    Height = 25
    Caption = 'Bench ALSHA1 (Unicode)'
    TabOrder = 32
    OnClick = Button14Click
  end
  object Button17: TButton
    Left = 414
    Top = 140
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 256 (Unicode)'
    TabOrder = 33
    OnClick = Button17Click
  end
  object Button18: TButton
    Left = 176
    Top = 109
    Width = 225
    Height = 25
    Caption = 'Bench HexEncode (UNICODE)'
    TabOrder = 34
    OnClick = Button18Click
  end
  object Button9: TButton
    Left = 24
    Top = 202
    Width = 140
    Height = 25
    Caption = 'Bench ALCRC64'
    TabOrder = 35
    OnClick = Button9Click
  end
  object Button19: TButton
    Left = 24
    Top = 109
    Width = 140
    Height = 25
    Caption = 'Test ALCRC32'
    TabOrder = 36
    OnClick = Button19Click
  end
end
