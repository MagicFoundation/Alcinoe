object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 129
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClick = FormClick
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 41
    Align = alTop
    Color = clPurple
    ParentBackground = False
    TabOrder = 4
    object Label1: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 511
      Height = 23
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
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
      ExplicitHeight = 20
    end
  end
  object ButtonBenchTALRttiTypeGetFields: TButton
    Left = 24
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetFields'
    TabOrder = 0
    OnClick = ButtonBenchTALRttiTypeGetFieldsClick
  end
  object ButtonBenchTRttiTypeGetFields: TButton
    Left = 280
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetFields'
    TabOrder = 1
    OnClick = ButtonBenchTRttiTypeGetFieldsClick
  end
  object ButtonBenchTALRttiTypeGetField: TButton
    Left = 24
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetField'
    TabOrder = 2
    OnClick = ButtonBenchTALRttiTypeGetFieldClick
  end
  object ButtonBenchTRttiTypeGetField: TButton
    Left = 280
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetField'
    TabOrder = 3
    OnClick = ButtonBenchTRttiTypeGetFieldClick
  end
end
