object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 279
  ClientWidth = 320
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
    Left = 16
    Top = 16
    Width = 53
    Height = 13
    Caption = 'AlListBox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 16
    Width = 50
    Height = 13
    Caption = 'TListBox'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ALListBox1: TALListBox
    Left = 16
    Top = 32
    Width = 121
    Height = 145
    OnPaint = ALListBox1Paint
    OnPaintScrollBar = ALListBox1PaintScrollBar
    Style = lbOwnerDrawVariable
    ItemHeight = 13
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14'
      '15'
      '16'
      '17'
      '18'
      '19'
      '20')
    TabOrder = 1
    DesignSize = (
      121
      145)
  end
  object ListBox2: TListBox
    Left = 184
    Top = 32
    Width = 121
    Height = 145
    Style = lbOwnerDrawVariable
    IntegralHeight = True
    ItemHeight = 16
    Items.Strings = (
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14'
      '15'
      '16'
      '17'
      '18'
      '19'
      '20')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 60
    Top = 200
    Width = 200
    Height = 70
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Ctl3D = False
    ParentColor = True
    ParentCtl3D = False
    TabOrder = 2
    object Label5: TLabel
      Left = 5
      Top = 8
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
end
