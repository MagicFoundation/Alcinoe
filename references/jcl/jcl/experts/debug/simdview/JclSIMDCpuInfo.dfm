object JclFormCpuInfo: TJclFormCpuInfo
  Left = 468
  Top = 438
  BorderStyle = bsDialog
  Caption = 'RsCpuInfoTitle'
  ClientHeight = 312
  ClientWidth = 322
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelName: TLabel
    Left = 8
    Top = 11
    Width = 39
    Height = 13
    Caption = 'RsName'
  end
  object LabelVendor: TLabel
    Left = 8
    Top = 43
    Width = 46
    Height = 13
    Caption = 'RsVendor'
  end
  object LabelFrequency: TLabel
    Left = 161
    Top = 43
    Width = 63
    Height = 13
    Caption = 'RsFrequency'
  end
  object EditName: TEdit
    Left = 64
    Top = 8
    Width = 249
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 0
  end
  object EditVendor: TEdit
    Left = 64
    Top = 40
    Width = 81
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 1
  end
  object EditFrequency: TEdit
    Left = 232
    Top = 40
    Width = 81
    Height = 21
    Enabled = False
    ParentColor = True
    TabOrder = 2
  end
  object CheckBoxMMX: TCheckBox
    Left = 8
    Top = 72
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsMMX'
    Enabled = False
    TabOrder = 3
  end
  object CheckBoxExMMX: TCheckBox
    Left = 8
    Top = 95
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsMMXExt'
    Enabled = False
    TabOrder = 4
  end
  object CheckBox3DNow: TCheckBox
    Left = 8
    Top = 118
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Rs3DNow'
    Enabled = False
    TabOrder = 5
  end
  object CheckBoxEx3DNow: TCheckBox
    Left = 8
    Top = 141
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Rs3DNowExt'
    Enabled = False
    TabOrder = 6
  end
  object CheckBox64Bits: TCheckBox
    Left = 8
    Top = 164
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsLong'
    Enabled = False
    TabOrder = 7
  end
  object CheckBoxSSE1: TCheckBox
    Left = 161
    Top = 72
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE1'
    Enabled = False
    TabOrder = 8
  end
  object CheckBoxSSE2: TCheckBox
    Left = 161
    Top = 95
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE2'
    Enabled = False
    TabOrder = 9
  end
  object CheckBoxSSE3: TCheckBox
    Left = 161
    Top = 118
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE3'
    Enabled = False
    TabOrder = 10
  end
  object ButtonClose: TButton
    Left = 120
    Top = 279
    Width = 83
    Height = 25
    Caption = 'RsClose'
    ModalResult = 2
    TabOrder = 15
  end
  object CheckBoxSSSE3: TCheckBox
    Left = 161
    Top = 141
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE3Ext'
    Enabled = False
    TabOrder = 11
  end
  object CheckBoxSSE41: TCheckBox
    Left = 161
    Top = 164
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE41'
    Enabled = False
    TabOrder = 12
  end
  object CheckBoxSSE5: TCheckBox
    Left = 161
    Top = 233
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE5'
    Enabled = False
    TabOrder = 14
  end
  object CheckBoxSSE42: TCheckBox
    Left = 161
    Top = 187
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE42'
    Enabled = False
    TabOrder = 13
  end
  object CheckBoxAVX: TCheckBox
    Left = 161
    Top = 256
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsAVX'
    Enabled = False
    TabOrder = 16
  end
  object CheckBoxEnabledFPU: TCheckBox
    Left = 8
    Top = 187
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsEnabledFPU'
    Enabled = False
    TabOrder = 17
  end
  object CheckBoxEnabledSSE: TCheckBox
    Left = 8
    Top = 210
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsEnabledSSE'
    Enabled = False
    TabOrder = 18
  end
  object CheckBoxEnabledAVX: TCheckBox
    Left = 8
    Top = 233
    Width = 137
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsEnabledAVX'
    Enabled = False
    TabOrder = 19
  end
  object CheckBoxSSE4A: TCheckBox
    Left = 161
    Top = 210
    Width = 153
    Height = 17
    Alignment = taLeftJustify
    Caption = 'RsSSE4A'
    Enabled = False
    TabOrder = 20
  end
end
