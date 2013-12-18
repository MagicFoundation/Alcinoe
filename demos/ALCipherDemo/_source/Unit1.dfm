object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 561
  ClientWidth = 610
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
    Top = 286
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
  object ALButton1: TButton
    Left = 24
    Top = 8
    Width = 124
    Height = 25
    Caption = 'Bench AES (EBC)'
    TabOrder = 0
    OnClick = ALButton1Click
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
  object ALButton3: TButton
    Left = 24
    Top = 39
    Width = 124
    Height = 25
    Caption = 'Bench AES (CBC)'
    TabOrder = 2
    OnClick = ALButton3Click
  end
  object ALButton4: TButton
    Left = 24
    Top = 70
    Width = 124
    Height = 25
    Caption = 'Bench Blowfish (EBC)'
    TabOrder = 3
    OnClick = ALButton4Click
  end
  object EditKey: TEdit
    Left = 24
    Top = 305
    Width = 266
    Height = 21
    TabOrder = 4
  end
  object ALMemoDecryptedData: TMemo
    Left = 24
    Top = 364
    Width = 266
    Height = 165
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object ALMemoCryptedData: TMemo
    Left = 296
    Top = 364
    Width = 290
    Height = 165
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object ALButton2: TButton
    Left = 330
    Top = 177
    Width = 97
    Height = 25
    Caption = 'Crypt (BF)'
    TabOrder = 7
    OnClick = ALButton2Click
  end
  object ALButton5: TButton
    Left = 433
    Top = 177
    Width = 115
    Height = 25
    Caption = 'DeCrypt (BF)'
    TabOrder = 8
    OnClick = ALButton5Click
  end
  object ALButton6: TButton
    Left = 330
    Top = 208
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - EBC)'
    TabOrder = 9
    OnClick = ALButton6Click
  end
  object ALButton7: TButton
    Left = 433
    Top = 208
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - EBC)'
    TabOrder = 10
    OnClick = ALButton7Click
  end
  object ALButton10: TButton
    Left = 159
    Top = 8
    Width = 124
    Height = 25
    Caption = 'Bench MD5'
    TabOrder = 11
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 159
    Top = 39
    Width = 124
    Height = 25
    Caption = 'Bench SHA1'
    TabOrder = 12
    OnClick = ALButton11Click
  end
  object ALButton8: TButton
    Left = 330
    Top = 239
    Width = 97
    Height = 25
    Caption = 'Crypt (AES - CBC)'
    TabOrder = 13
    OnClick = ALButton8Click
  end
  object ALButton9: TButton
    Left = 433
    Top = 239
    Width = 115
    Height = 25
    Caption = 'DeCrypt (AES - CBC)'
    TabOrder = 14
    OnClick = ALButton9Click
  end
  object ALButton12: TButton
    Left = 330
    Top = 270
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (SHA1)'
    TabOrder = 15
    OnClick = ALButton12Click
  end
  object ALButton13: TButton
    Left = 330
    Top = 301
    Width = 218
    Height = 25
    Caption = 'Generate HMAC (MD5)'
    TabOrder = 16
    OnClick = ALButton13Click
  end
  object Panel7: TPanel
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
    TabOrder = 17
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
    object Label15: TcxLabel
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
    object cxLabel4: TcxLabel
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
    object cxLabel6: TcxLabel
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
  end
end
