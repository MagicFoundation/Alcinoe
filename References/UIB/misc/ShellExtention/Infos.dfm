object InfosForm: TInfosForm
  Left = 404
  Top = 291
  Width = 396
  Height = 347
  Caption = 'Database Infos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    388
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 208
    Width = 63
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Actives users'
  end
  object ValueList: TValueListEditor
    Left = 8
    Top = 8
    Width = 377
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goAlwaysShowEditor, goThumbTracking]
    Strings.Strings = (
      '')
    TabOrder = 0
    TitleCaptions.Strings = (
      'Key'
      'Value')
    OnKeyPress = FormKeyPress
    ColWidths = (
      126
      245)
  end
  object UserList: TListBox
    Left = 8
    Top = 224
    Width = 377
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnKeyPress = FormKeyPress
  end
  object Database: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE')
    LibraryName = 'gds32.dll'
    OnInfoUserNames = DatabaseInfoUserNames
    Left = 8
    Top = 8
  end
  object Config: TUIBConfig
    LibraryName = 'gds32.dll'
    Left = 40
    Top = 8
  end
end
