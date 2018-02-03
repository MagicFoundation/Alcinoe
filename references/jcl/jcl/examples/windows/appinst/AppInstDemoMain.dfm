object Form1: TForm1
  Left = 204
  Top = 125
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JclAppInst demo'
  ClientHeight = 365
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 256
    Top = 8
    Width = 65
    Height = 97
    Alignment = taCenter
    AutoSize = False
    Caption = '0'
    Font.Charset = EASTEUROPE_CHARSET
    Font.Color = clWindowText
    Font.Height = -96
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object InstancesListView: TListView
    Left = 0
    Top = 0
    Width = 241
    Height = 177
    Columns = <
      item
        Caption = 'Number'
      end
      item
        Alignment = taRightJustify
        Caption = 'ProcessID'
        Width = 70
      end
      item
        Alignment = taRightJustify
        Caption = 'Application HWND'
        Width = 110
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = SwitchBtnClick
  end
  object SwitchBtn: TButton
    Left = 248
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Switch to'
    TabOrder = 1
    OnClick = SwitchBtnClick
  end
  object MsgBtn: TButton
    Left = 248
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Message'
    TabOrder = 2
    OnClick = MsgBtnClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 184
    Width = 241
    Height = 177
    Lines.Strings = (
      'Enter a text')
    TabOrder = 3
    OnChange = Memo1Change
  end
  object SendBtn: TButton
    Left = 248
    Top = 336
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = SendBtnClick
  end
  object AutoUpdateCheckBox: TCheckBox
    Left = 248
    Top = 304
    Width = 97
    Height = 17
    Caption = 'Auto update'
    TabOrder = 5
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    CustomColors.Strings = (
      'ColorA=FFFFFFFF'
      'ColorB=FFFFFFFF'
      'ColorC=FFFFFFFF'
      'ColorD=FFFFFFFF'
      'ColorE=FFFFFFFF'
      'ColorF=FFFFFFFF'
      'ColorG=FFFFFFFF'
      'ColorH=FFFFFFFF'
      'ColorI=FFFFFFFF'
      'ColorJ=FFFFFFFF'
      'ColorK=FFFFFFFF'
      'ColorL=FFFFFFFF'
      'ColorM=FFFFFFFF'
      'ColorN=FFFFFFFF'
      'ColorO=FFFFFFFF'
      'ColorP=FFFFFFFF')
    Options = [cdPreventFullOpen]
    Left = 8
    Top = 144
  end
end
