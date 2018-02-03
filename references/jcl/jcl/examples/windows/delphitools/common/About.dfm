object AboutBox: TAboutBox
  Left = 306
  Top = 208
  BorderStyle = bsDialog
  Caption = 'About ...'
  ClientHeight = 164
  ClientWidth = 258
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object IconPaintBox: TPaintBox
    Left = 8
    Top = 8
    Width = 32
    Height = 32
    OnPaint = IconPaintBoxPaint
  end
  object Bevel1: TBevel
    Left = 56
    Top = 121
    Width = 193
    Height = 14
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object ProductNameLabel: TLabel
    Left = 56
    Top = 16
    Width = 108
    Height = 13
    Caption = 'ProductNameLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object VersionLabel: TLabel
    Left = 56
    Top = 40
    Width = 61
    Height = 13
    Caption = 'VersionLabel'
  end
  object CompanyLabel: TLabel
    Left = 56
    Top = 64
    Width = 70
    Height = 13
    Caption = 'CompanyLabel'
  end
  object OkBtn: TButton
    Left = 174
    Top = 133
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
