object MainForm: TMainForm
  Left = 342
  Top = 197
  ClientWidth = 606
  ClientHeight = 497
  Caption = 'Tracking unhandled exceptions in VCL application'
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
    Left = 0
    Top = 64
    Width = 64
    Height = 13
    Caption = '&Exception log'
    FocusControl = ExceptionLogMemo
  end
  object ExceptionLogMemo: TMemo
    Left = 0
    Top = 80
    Width = 606
    Height = 416
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Error1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Error2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Error3'
    TabOrder = 3
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 472
    Top = 8
    Width = 73
    Height = 49
    ItemHeight = 13
    TabOrder = 4
    Visible = False
  end
  object Button4: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Error4'
    TabOrder = 5
    OnClick = Button4Click
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    Left = 8
    Top = 440
  end
  object ActionList1: TActionList
    Left = 440
    Top = 8
  end
end
