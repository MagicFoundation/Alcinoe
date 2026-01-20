object Form1: TForm1
  Left = 485
  Top = 214
  Caption = 'SMTP Test'
  ClientHeight = 684
  ClientWidth = 530
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object GoButton: TButton
    AlignWithMargins = True
    Left = 16
    Top = 16
    Width = 498
    Height = 25
    Margins.Left = 16
    Margins.Top = 16
    Margins.Right = 16
    Margins.Bottom = 16
    Align = alTop
    Caption = 'Go (www.howsmyssl.com)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = GoButtonClick
    ExplicitLeft = 11
    ExplicitTop = 10
    ExplicitWidth = 557
  end
  object ResultMemo: TMemo
    Left = 0
    Top = 57
    Width = 530
    Height = 627
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    EditMargins.Left = 6
    EditMargins.Right = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Lines.Strings = (
      
        'This demo tests the TALTlsClient implementation on Windows using' +
        ' the native '
      'SChannel API.')
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
