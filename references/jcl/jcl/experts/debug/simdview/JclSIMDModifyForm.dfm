object JclSIMDModifyFrm: TJclSIMDModifyFrm
  Left = 806
  Top = 175
  BorderStyle = bsDialog
  Caption = 'JclSIMDModifyFrm'
  ClientHeight = 388
  ClientWidth = 936
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDisplay: TLabel
    Left = 8
    Top = 16
    Width = 47
    Height = 13
    Caption = 'RsDisplay'
    Layout = tlCenter
  end
  object LabelFormat: TLabel
    Left = 240
    Top = 16
    Width = 45
    Height = 13
    Caption = 'RsFormat'
    Layout = tlCenter
  end
  object LabelBlank: TLabel
    Left = 480
    Top = 16
    Width = 65
    Height = 13
    Caption = 'RsKeepBlank'
  end
  object ComboBoxDisplay: TComboBox
    Left = 56
    Top = 13
    Width = 137
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBoxDisplayChange
  end
  object ComboBoxFormat: TComboBox
    Left = 288
    Top = 13
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = ComboBoxFormatChange
    Items.Strings = (
      'RsBinary'
      'RsSignedDecimal'
      'RsUnsignedDecimal'
      'RsHexadecimal')
  end
  object PanelModify: TPanel
    Left = 8
    Top = 40
    Width = 920
    Height = 265
    BevelInner = bvLowered
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 789
    Top = 355
    Width = 139
    Height = 25
    Caption = 'RsOk'
    Default = True
    TabOrder = 3
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 789
    Top = 324
    Width = 139
    Height = 25
    Cancel = True
    Caption = 'RsCancel'
    ModalResult = 2
    TabOrder = 4
  end
  object MemoTip: TMemo
    Left = 8
    Top = 311
    Width = 761
    Height = 65
    BorderStyle = bsNone
    Lines.Strings = (
      'RsSIMDModificationDescription')
    ParentColor = True
    TabOrder = 5
  end
end
