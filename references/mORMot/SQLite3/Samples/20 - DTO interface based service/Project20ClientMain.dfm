object Form1: TForm1
  Left = 334
  Top = 330
  Width = 703
  Height = 525
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    687
    487)
  PixelsPerInch = 96
  TextHeight = 16
  object lblA: TLabel
    Left = 72
    Top = 50
    Width = 21
    Height = 16
    Caption = 'ID='
  end
  object lbl1: TLabel
    Left = 403
    Top = 42
    Width = 87
    Height = 16
    Alignment = taRightJustify
    Caption = 'Numer of calls:'
  end
  object lblTiming: TLabel
    Left = 392
    Top = 80
    Width = 281
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
  end
  object lbl2: TLabel
    Left = 411
    Top = 20
    Width = 79
    Height = 16
    Alignment = taRightJustify
    Caption = 'Server name:'
  end
  object edtID: TEdit
    Left = 96
    Top = 48
    Width = 153
    Height = 24
    TabOrder = 0
    Text = '1234'
  end
  object btnCall: TButton
    Left = 96
    Top = 80
    Width = 97
    Height = 25
    Caption = 'Call Server'
    TabOrder = 1
    OnClick = btnCallClick
  end
  object btnCancel: TButton
    Left = 256
    Top = 80
    Width = 97
    Height = 25
    Caption = 'Quit'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object mmoResult: TMemo
    Left = 8
    Top = 120
    Width = 670
    Height = 358
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    ScrollBars = ssVertical
    TabOrder = 3
    WordWrap = False
  end
  object edtNumberOfCalls: TEdit
    Left = 496
    Top = 40
    Width = 121
    Height = 24
    TabOrder = 4
    Text = '1'
  end
  object edtServerName: TEdit
    Left = 496
    Top = 16
    Width = 121
    Height = 24
    TabOrder = 5
    Text = 'localhost'
  end
end
