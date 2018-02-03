object Form1: TForm1
  Left = 193
  Top = 103
  ClientWidth = 450
  ClientHeight = 330
  Caption = 'TJclPeMapImgHooks demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object HookBtn: TButton
    Left = 8
    Top = 16
    Width = 89
    Height = 25
    Caption = 'Hook'
    TabOrder = 0
    OnClick = HookBtnClick
  end
  object UnhookBtn: TButton
    Left = 8
    Top = 48
    Width = 89
    Height = 25
    Caption = 'Unhook'
    TabOrder = 1
    OnClick = UnhookBtnClick
  end
  object BeepBtn: TButton
    Left = 8
    Top = 104
    Width = 89
    Height = 25
    Caption = 'MessageBeep'
    TabOrder = 2
    OnClick = BeepBtnClick
  end
  object Memo1: TMemo
    Left = 132
    Top = 0
    Width = 318
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
end
