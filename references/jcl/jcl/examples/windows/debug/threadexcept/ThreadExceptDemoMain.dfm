object MainForm: TMainForm
  Left = 286
  Top = 169
  Caption = 
    'Exception tracking in threads and IDE Thread Status window exten' +
    'sion demo'
  ClientHeight = 557
  ClientWidth = 715
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Thread name'
    FocusControl = ThreadNameEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 248
    Width = 55
    Height = 13
    Caption = 'Exceprtions'
  end
  object MessageRichEdit: TRichEdit
    Left = 8
    Top = 264
    Width = 697
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ReadOnly = True
    TabOrder = 0
    WordWrap = False
  end
  object ThreadsRichEdit: TRichEdit
    Left = 168
    Top = 8
    Width = 537
    Height = 249
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    PlainText = True
    ReadOnly = True
    TabOrder = 1
    WordWrap = False
  end
  object CreateThreadBtn: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Create Thread'
    TabOrder = 2
    OnClick = CreateThreadBtnClick
  end
  object ThreadNameEdit: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object ListThreadsBtn: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'List Threads'
    TabOrder = 4
    OnClick = ListThreadsBtnClick
  end
end
