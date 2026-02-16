object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 815
  ClientWidth = 768
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 305
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
    Top = 364
    Width = 28
    Height = 13
    Caption = 'Data'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 384
    Top = 364
    Width = 37
    Height = 13
    Caption = 'Result'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 24
    Top = 560
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
    Top = 280
    Width = 768
    Height = 516
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Visible = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 796
    Width = 768
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
  object EditKey: TEdit
    Left = 24
    Top = 325
    Width = 345
    Height = 21
    TabOrder = 1
  end
  object ALMemoDecryptedData: TMemo
    Left = 24
    Top = 383
    Width = 345
    Height = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ALMemoCryptedData: TMemo
    Left = 384
    Top = 383
    Width = 353
    Height = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ALButton10: TButton
    Left = 24
    Top = 78
    Width = 225
    Height = 25
    Caption = 'Bench ALMD5 (ansiString)'
    TabOrder = 4
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 24
    Top = 140
    Width = 225
    Height = 25
    Caption = 'Bench ALSHA1 (ansiString)'
    TabOrder = 5
    OnClick = ALButton11Click
  end
  object ALButton12: TButton
    Left = 415
    Top = 47
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (SHA1)'
    TabOrder = 6
    OnClick = ALButton12Click
  end
  object ALButton13: TButton
    Left = 415
    Top = 16
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (MD5)'
    TabOrder = 7
    OnClick = ALButton13Click
  end
  object Button1: TButton
    Left = 25
    Top = 16
    Width = 224
    Height = 25
    Caption = 'Bench CRC32 (Zlib)'
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 265
    Top = 140
    Width = 136
    Height = 25
    Caption = 'Bench BobJenkinsHash'
    TabOrder = 9
    OnClick = Button3Click
  end
  object ALMemoCollisions: TMemo
    Left = 24
    Top = 579
    Width = 713
    Height = 189
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object Button4: TButton
    Left = 265
    Top = 109
    Width = 136
    Height = 25
    Caption = 'Bench ALFNV-1a (int64)'
    TabOrder = 11
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 265
    Top = 78
    Width = 136
    Height = 25
    Caption = 'Bench ALFNV-1a (int32)'
    TabOrder = 12
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 265
    Top = 16
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom32'
    TabOrder = 13
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 265
    Top = 47
    Width = 136
    Height = 25
    Caption = 'Bench ALRandom64'
    TabOrder = 14
    OnClick = Button7Click
  end
  object Button13: TButton
    Left = 24
    Top = 47
    Width = 225
    Height = 25
    Caption = 'Bench ALMD5 (Unicode)'
    TabOrder = 15
    OnClick = Button13Click
  end
  object Button12: TButton
    Left = 25
    Top = 202
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 256 (ansiString)'
    TabOrder = 16
    OnClick = Button12Click
  end
  object Button16: TButton
    Left = 25
    Top = 233
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 512 (ansiString)'
    TabOrder = 17
    OnClick = Button16Click
  end
  object Button14: TButton
    Left = 25
    Top = 109
    Width = 225
    Height = 25
    Caption = 'Bench ALSHA1 (Unicode)'
    TabOrder = 18
    OnClick = Button14Click
  end
  object Button17: TButton
    Left = 25
    Top = 171
    Width = 224
    Height = 25
    Caption = 'Bench ALSHA2 256 (Unicode)'
    TabOrder = 19
    OnClick = Button17Click
  end
  object Button2: TButton
    Left = 415
    Top = 78
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (SHA2 - 256)'
    TabOrder = 20
    OnClick = Button2Click
  end
end
