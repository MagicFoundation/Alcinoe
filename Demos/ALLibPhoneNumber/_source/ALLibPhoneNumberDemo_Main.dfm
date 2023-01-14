object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ALLibPhoneNumberDemo'
  ClientHeight = 327
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    344
    327)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 8
    Width = 316
    Height = 26
    Caption = 
      'NOTE: to launch this demo be sure that you placed DLLs from the ' +
      'folder "/Libraries/dll/libphonenumber/" in one of your PATH locations '
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 4
    Top = 53
    Width = 333
    Height = 92
    Caption = 'Phone Number to Int64'
    TabOrder = 0
    object Label2: TLabel
      Left = 8
      Top = 29
      Width = 164
      Height = 13
      Caption = 'Type phone number in a free form'
    end
    object Label3: TLabel
      Left = 178
      Top = 12
      Width = 68
      Height = 26
      Caption = 'Country code (like FR)'
      WordWrap = True
    end
    object Edit1: TEdit
      Left = 8
      Top = 44
      Width = 164
      Height = 21
      TabOrder = 0
    end
    object Button1: TButton
      Left = 264
      Top = 42
      Width = 66
      Height = 25
      Caption = 'Convert'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Edit3: TEdit
      Left = 178
      Top = 44
      Width = 80
      Height = 21
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 264
    Width = 333
    Height = 61
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Result'
    TabOrder = 1
    object Edit2: TEdit
      Left = 8
      Top = 19
      Width = 313
      Height = 21
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 151
    Width = 333
    Height = 98
    Caption = 'Int64 to International phone number format'
    TabOrder = 2
    object Label4: TLabel
      Left = 8
      Top = 27
      Width = 101
      Height = 13
      Caption = 'Type an Int64 phone'
    end
    object Edit4: TEdit
      Left = 8
      Top = 43
      Width = 164
      Height = 21
      TabOrder = 0
    end
    object Button2: TButton
      Left = 178
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Convert'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 254
      Top = 41
      Width = 76
      Height = 25
      Caption = 'Define a type'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
