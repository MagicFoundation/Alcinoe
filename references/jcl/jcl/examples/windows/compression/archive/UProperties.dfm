object FormArchiveSettings: TFormArchiveSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Archive settings'
  ClientHeight = 371
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxGeneralSettings: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 97
    Caption = 'General settings:'
    TabOrder = 0
    object LabelPassword: TLabel
      Left = 16
      Top = 27
      Width = 50
      Height = 13
      Caption = '&Password:'
      FocusControl = EditPassword
    end
    object LabelNumberOfThreads: TLabel
      Left = 16
      Top = 62
      Width = 94
      Height = 13
      Caption = 'Number of &threads:'
      FocusControl = EditNumberOfThreads
    end
    object EditPassword: TEdit
      Left = 88
      Top = 24
      Width = 169
      Height = 21
      TabOrder = 0
      OnExit = EditPasswordExit
    end
    object EditNumberOfThreads: TEdit
      Left = 128
      Top = 59
      Width = 129
      Height = 21
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TabOrder = 1
      Text = '1'
      OnExit = EditNumberOfThreadsExit
    end
  end
  object GroupBoxCompressionProperties: TGroupBox
    Left = 8
    Top = 111
    Width = 273
    Height = 226
    Caption = 'Compression properties:'
    TabOrder = 1
    object LabelCompressionLevel: TLabel
      Left = 16
      Top = 27
      Width = 176
      Height = 13
      Caption = 'Compression &level (from %d to %d):'
      FocusControl = EditCompressionLevel
    end
    object LabelCompressionMethod: TLabel
      Left = 16
      Top = 58
      Width = 104
      Height = 13
      Caption = '&Compression method:'
      FocusControl = ComboBoxCompressionMethod
    end
    object LabelEncryptionMethod: TLabel
      Left = 16
      Top = 90
      Width = 94
      Height = 13
      Caption = '&Encryption method:'
      FocusControl = ComboBoxEncryptionMethod
    end
    object LabelDictionarySize: TLabel
      Left = 16
      Top = 123
      Width = 73
      Height = 13
      Caption = '&Dictionary size:'
      FocusControl = EditDictionarySize
    end
    object LabelNumberOfPasses: TLabel
      Left = 16
      Top = 158
      Width = 90
      Height = 13
      Caption = '&Number of passes:'
      FocusControl = EditNumberOfPasses
    end
    object LabelSolidBlockSize: TLabel
      Left = 16
      Top = 192
      Width = 74
      Height = 13
      Caption = '&Solid block size:'
      FocusControl = EditSolidBlockSize
    end
    object EditCompressionLevel: TEdit
      Left = 216
      Top = 24
      Width = 41
      Height = 21
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TabOrder = 0
      Text = '6'
      OnExit = EditCompressionLevelExit
    end
    object ComboBoxCompressionMethod: TComboBox
      Left = 136
      Top = 55
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnExit = ComboBoxCompressionMethodExit
    end
    object ComboBoxEncryptionMethod: TComboBox
      Left = 136
      Top = 88
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = ComboBoxEncryptionMethodChange
    end
    object EditDictionarySize: TEdit
      Left = 136
      Top = 122
      Width = 121
      Height = 21
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TabOrder = 3
      Text = '0'
      OnExit = EditDictionarySizeExit
    end
    object EditNumberOfPasses: TEdit
      Left = 136
      Top = 156
      Width = 121
      Height = 21
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TabOrder = 4
      Text = '0'
      OnExit = EditNumberOfPassesExit
    end
    object EditSolidBlockSize: TEdit
      Left = 136
      Top = 190
      Width = 121
      Height = 21
      BiDiMode = bdRightToLeft
      ParentBiDiMode = False
      TabOrder = 5
      Text = '0'
      OnExit = EditSolidBlockSizeExit
    end
  end
  object GroupBox1: TGroupBox
    Left = 296
    Top = 8
    Width = 185
    Height = 218
    Caption = 'Content:'
    TabOrder = 2
    object CheckBoxRemoveSfxBlock: TCheckBox
      Left = 16
      Top = 24
      Width = 153
      Height = 17
      Caption = '&Remove Sfx block'
      TabOrder = 0
      OnExit = CheckBoxRemoveSfxBlockExit
    end
    object CheckBoxCompressHeader: TCheckBox
      Left = 16
      Top = 48
      Width = 153
      Height = 17
      Caption = 'Compress &header'
      TabOrder = 1
      OnExit = CheckBoxCompressHeaderExit
    end
    object CheckBoxCompressHeaderFull: TCheckBox
      Left = 32
      Top = 72
      Width = 137
      Height = 17
      Caption = 'Compress header &full'
      TabOrder = 2
      OnExit = CheckBoxCompressHeaderFullExit
    end
    object CheckBoxEncryptHeader: TCheckBox
      Left = 16
      Top = 96
      Width = 153
      Height = 17
      Caption = 'Encr&ypt header'
      TabOrder = 3
      OnExit = CheckBoxEncryptHeaderExit
    end
    object CheckBoxSaveCreationDateTime: TCheckBox
      Left = 16
      Top = 120
      Width = 153
      Height = 17
      Caption = 'Save cr&eation date-time'
      TabOrder = 4
      OnExit = CheckBoxSaveCreationDateTimeExit
    end
    object CheckBoxSaveLastAccessDateTime: TCheckBox
      Left = 16
      Top = 144
      Width = 153
      Height = 17
      Caption = 'Save last &access date-time'
      TabOrder = 5
      OnExit = CheckBoxSaveLastAccessDateTimeExit
    end
    object CheckBoxSaveLastSaveDateTime: TCheckBox
      Left = 16
      Top = 167
      Width = 153
      Height = 17
      Caption = 'Save last &write date-time'
      TabOrder = 6
      OnExit = CheckBoxSaveLastSaveDateTimeExit
    end
    object CheckBoxSolidExtension: TCheckBox
      Left = 16
      Top = 190
      Width = 153
      Height = 17
      Caption = 'Solid archive by e&xtension'
      TabOrder = 7
      OnExit = CheckBoxSolidExtensionExit
    end
  end
  object ButtonClose: TButton
    Left = 328
    Top = 257
    Width = 121
    Height = 25
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
