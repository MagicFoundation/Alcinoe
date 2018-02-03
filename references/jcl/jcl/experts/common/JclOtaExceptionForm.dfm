object JclExpertExceptionForm: TJclExpertExceptionForm
  Left = 157
  Top = 183
  BorderIcons = [biSystemMenu]
  Caption = 'RsReportFormCaption'
  ClientHeight = 423
  ClientWidth = 551
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LabelURL: TLabel
    Left = 8
    Top = 135
    Width = 4
    Height = 16
    Cursor = crHandPoint
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
    OnClick = LabelURLClick
  end
  object MemoDetails: TMemo
    Left = 8
    Top = 8
    Width = 535
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object MemoCallStack: TMemo
    Left = 8
    Top = 168
    Width = 535
    Height = 222
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object ButtonClose: TButton
    Left = 470
    Top = 396
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'RsReportClose'
    ModalResult = 1
    TabOrder = 2
  end
end
