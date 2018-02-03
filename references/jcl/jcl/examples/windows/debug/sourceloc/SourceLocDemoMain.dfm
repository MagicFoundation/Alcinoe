object Form1: TForm1
  Left = 192
  Top = 107
  ClientWidth = 638
  ClientHeight = 485
  Caption = 'JclDebug Source location example'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 600
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
    Left = 16
    Top = 40
    Width = 29
    Height = 13
    Caption = 'Level:'
    FocusControl = LevelSpinEdit
  end
  object Label2: TLabel
    Left = 136
    Top = 40
    Width = 67
    Height = 13
    Caption = 'Address (hex):'
    FocusControl = AddrEdit
  end
  object Memo1: TMemo
    Left = 0
    Top = 88
    Width = 638
    Height = 396
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
  object CallerBtn: TButton
    Left = 16
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Caller()'
    TabOrder = 1
    OnClick = CallerBtnClick
  end
  object LevelSpinEdit: TSpinEdit
    Left = 16
    Top = 56
    Width = 89
    Height = 22
    MaxValue = 20
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object AddrBtn: TButton
    Left = 136
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Address lookup'
    TabOrder = 3
    OnClick = AddrBtnClick
  end
  object AddrEdit: TEdit
    Left = 136
    Top = 56
    Width = 89
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 4
    Text = 'ADDREDIT'
  end
  object StackBtn: TButton
    Left = 256
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Stack dump'
    TabOrder = 5
    OnClick = StackBtnClick
  end
  object TraceLocBtn: TButton
    Left = 368
    Top = 8
    Width = 89
    Height = 25
    Caption = 'TraceLoc("text")'
    TabOrder = 6
    OnClick = TraceLocBtnClick
  end
  object ProcBtn: TButton
    Left = 480
    Top = 8
    Width = 81
    Height = 25
    Caption = '__PROC__'
    TabOrder = 8
    OnClick = ProcBtnClick
  end
  object ModuleBtn: TButton
    Left = 480
    Top = 48
    Width = 81
    Height = 25
    Caption = '__MODULE__'
    TabOrder = 9
    OnClick = ModuleBtnClick
  end
  object RawCheckBox: TCheckBox
    Left = 256
    Top = 40
    Width = 89
    Height = 17
    Caption = 'Raw'
    TabOrder = 7
  end
end
