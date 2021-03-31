object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 274
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 255
    Width = 518
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
    ExplicitTop = 771
    ExplicitWidth = 1034
  end
  object Button11: TButton
    Left = 24
    Top = 119
    Width = 230
    Height = 25
    Caption = 'Bench ALGetEnumName (AnsiString)'
    TabOrder = 4
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 274
    Top = 119
    Width = 230
    Height = 25
    Caption = 'Bench GetEnumName (String)'
    TabOrder = 5
    OnClick = Button12Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 518
    Height = 41
    Align = alTop
    Color = clPurple
    ParentBackground = False
    TabOrder = 13
    object Label1: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 500
      Height = 20
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 'You must run this demo in "Release" to see accurate benchmark. '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 464
    end
  end
  object Button1: TButton
    Left = 24
    Top = 150
    Width = 230
    Height = 25
    Caption = 'Bench ALTryGetEnumValue (AnsiString)'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 274
    Top = 150
    Width = 230
    Height = 25
    Caption = 'Bench GetEnumValue (String)'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 181
    Width = 230
    Height = 25
    Caption = 'Bench ALSetToString (AnsiString)'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 274
    Top = 181
    Width = 230
    Height = 25
    Caption = 'Bench SetToString (String)'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 212
    Width = 230
    Height = 25
    Caption = 'Bench ALTryStringToSet (AnsiString)'
    TabOrder = 10
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 274
    Top = 212
    Width = 230
    Height = 25
    Caption = 'Bench StringToSet (String)'
    TabOrder = 11
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 24
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetFields (AnsiString)'
    TabOrder = 0
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 274
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetFields (String)'
    TabOrder = 1
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 24
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetField (AnsiString)'
    TabOrder = 2
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 274
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetField (String)'
    TabOrder = 3
    OnClick = Button10Click
  end
end
