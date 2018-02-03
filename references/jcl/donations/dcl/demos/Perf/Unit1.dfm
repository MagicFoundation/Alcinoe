object frmPerf: TfrmPerf
  Left = 259
  Top = 244
  Width = 579
  Height = 391
  HorzScrollBar.Range = 561
  VertScrollBar.Range = 353
  ActiveControl = btnTList
  AutoScroll = False
  Caption = 'Performances'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object gbxCompareList: TGroupBox
    Left = 8
    Top = 8
    Width = 553
    Height = 185
    Caption = 'Compare List'
    TabOrder = 0
    object lblAdd: TLabel
      Left = 16
      Top = 56
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblNext: TLabel
      Left = 16
      Top = 80
      Width = 22
      Height = 13
      Caption = 'Next'
    end
    object lblRandom: TLabel
      Left = 16
      Top = 104
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblAdd10: TLabel
      Left = 16
      Top = 128
      Width = 41
      Height = 13
      Caption = 'AddAt10'
    end
    object lblClear: TLabel
      Left = 16
      Top = 152
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblArrayAdd: TLabel
      Left = 152
      Top = 56
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblArrayNext: TLabel
      Left = 152
      Top = 80
      Width = 22
      Height = 13
      Caption = 'Next'
    end
    object lblArrayRandom: TLabel
      Left = 152
      Top = 104
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblArrayAdd10: TLabel
      Left = 152
      Top = 128
      Width = 41
      Height = 13
      Caption = 'AddAt10'
    end
    object lblArrayClear: TLabel
      Left = 152
      Top = 152
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblLinkedAdd: TLabel
      Left = 296
      Top = 56
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblLinkedNext: TLabel
      Left = 296
      Top = 80
      Width = 22
      Height = 13
      Caption = 'Next'
    end
    object lblLinkedRandom: TLabel
      Left = 296
      Top = 104
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblLinkedAdd10: TLabel
      Left = 296
      Top = 128
      Width = 41
      Height = 13
      Caption = 'AddAt10'
    end
    object lblLinkedClear: TLabel
      Left = 296
      Top = 152
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblSpeedAdd: TLabel
      Left = 440
      Top = 56
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblSpeedNext: TLabel
      Left = 440
      Top = 81
      Width = 22
      Height = 13
      Caption = 'Next'
    end
    object lblSpeedRandom: TLabel
      Left = 440
      Top = 105
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblSpeedAdd10: TLabel
      Left = 440
      Top = 128
      Width = 41
      Height = 13
      Caption = 'AddAt10'
    end
    object lblSpeedClear: TLabel
      Left = 440
      Top = 153
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object btnTList: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'TList'
      TabOrder = 0
      OnClick = btnTListClick
    end
    object btnTArrayList: TButton
      Left = 152
      Top = 24
      Width = 75
      Height = 25
      Caption = 'TArrayList'
      TabOrder = 1
      OnClick = btnTArrayListClick
    end
    object btnTLinkedList: TButton
      Left = 296
      Top = 24
      Width = 75
      Height = 25
      Caption = 'TLinkedList'
      TabOrder = 2
      OnClick = btnTLinkedListClick
    end
    object btnTSpeedList: TButton
      Left = 440
      Top = 24
      Width = 75
      Height = 25
      Caption = 'TVector'
      TabOrder = 3
      OnClick = btnTSpeedListClick
    end
  end
  object gbxCompareHash: TGroupBox
    Left = 8
    Top = 200
    Width = 553
    Height = 153
    Caption = 'Compare Hash'
    TabOrder = 1
    object lblBucketAdd: TLabel
      Left = 40
      Top = 68
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblBucketRandom: TLabel
      Left = 40
      Top = 92
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblBucketClear: TLabel
      Left = 40
      Top = 116
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblHashAdd: TLabel
      Left = 168
      Top = 68
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblHashRandom: TLabel
      Left = 168
      Top = 92
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblHashClear: TLabel
      Left = 168
      Top = 116
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblHashedStringAdd: TLabel
      Left = 296
      Top = 64
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblHashedStringRandom: TLabel
      Left = 296
      Top = 92
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblHashedStringClear: TLabel
      Left = 296
      Top = 116
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object lblStrStrHashAdd: TLabel
      Left = 432
      Top = 68
      Width = 19
      Height = 13
      Caption = 'Add'
    end
    object lblStrStrHashRandom: TLabel
      Left = 432
      Top = 92
      Width = 40
      Height = 13
      Caption = 'Random'
    end
    object lblStrStrHashClear: TLabel
      Left = 432
      Top = 116
      Width = 24
      Height = 13
      Caption = 'Clear'
    end
    object btnBucketList: TButton
      Left = 40
      Top = 24
      Width = 75
      Height = 25
      Caption = 'TBucketList'
      TabOrder = 0
      OnClick = btnBucketListClick
    end
    object btnTHashMap: TButton
      Left = 168
      Top = 24
      Width = 75
      Height = 25
      Caption = 'THashMap'
      TabOrder = 1
      OnClick = btnTHashMapClick
    end
    object btnHashedString: TButton
      Left = 296
      Top = 24
      Width = 97
      Height = 25
      Caption = 'THasedStringList'
      TabOrder = 2
      OnClick = btnHashedStringClick
    end
    object Button2: TButton
      Left = 432
      Top = 24
      Width = 91
      Height = 25
      Caption = 'TStrStrHashMap'
      TabOrder = 3
      OnClick = Button2Click
    end
  end
end
