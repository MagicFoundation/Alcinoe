object FrmServidor: TFrmServidor
  Left = 93
  Top = 85
  Width = 801
  Height = 598
  Caption = 'mORMot + MongoDB'
  Color = clCaptionText
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 536
    Width = 793
    Height = 35
    Align = alBottom
    Alignment = taCenter
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -29
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object bmp1: TImage
    Left = 6
    Top = 8
    Width = 781
    Height = 526
    Stretch = True
  end
  object title: TLabel
    Left = 228
    Top = 11
    Width = 503
    Height = 52
    Caption = 'mORMot WINE CELLAR'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -43
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object txtItems: TLabel
    Left = 207
    Top = 456
    Width = 3
    Height = 13
    Visible = False
  end
  object Label2: TLabel
    Left = 452
    Top = 306
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object status: TLabel
    Left = 23
    Top = 62
    Width = 3
    Height = 13
  end
  object bmp2: TImage
    Left = 151
    Top = 25
    Width = 32
    Height = 32
    AutoSize = True
    OnClick = btnSearchClick
  end
  object L1: TLabel
    Left = 461
    Top = 323
    Width = 23
    Height = 13
    Caption = 'NEW'
  end
  object Label3: TLabel
    Left = 510
    Top = 323
    Width = 25
    Height = 13
    Caption = 'SAVE'
  end
  object Label4: TLabel
    Left = 556
    Top = 321
    Width = 36
    Height = 13
    Caption = 'DELETE'
  end
  object Label5: TLabel
    Left = 609
    Top = 322
    Width = 25
    Height = 13
    Caption = 'PREV'
  end
  object Label6: TLabel
    Left = 706
    Top = 322
    Width = 25
    Height = 13
    Caption = 'NEXT'
  end
  object Panel1: TPanel
    Left = 580
    Top = 81
    Width = 169
    Height = 155
    BevelInner = bvSpace
    BevelOuter = bvLowered
    TabOrder = 14
    object photo: TImage
      Left = 2
      Top = 2
      Width = 165
      Height = 151
      Align = alTop
      Stretch = True
      OnClick = photoClick
    end
  end
  object txtID: TLabeledEdit
    Left = 196
    Top = 46
    Width = 183
    Height = 24
    EditLabel.Width = 11
    EditLabel.Height = 13
    EditLabel.Caption = 'ID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    Visible = False
  end
  object txtname: TLabeledEdit
    Left = 196
    Top = 86
    Width = 266
    Height = 22
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
  end
  object txtgrapes: TLabeledEdit
    Left = 196
    Top = 124
    Width = 132
    Height = 22
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Grapes'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object txtcountry: TLabeledEdit
    Left = 330
    Top = 124
    Width = 114
    Height = 22
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Country'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object txtregion: TLabeledEdit
    Left = 450
    Top = 125
    Width = 106
    Height = 22
    EditLabel.Width = 33
    EditLabel.Height = 13
    EditLabel.Caption = 'Region'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object txtyear: TLabeledEdit
    Left = 468
    Top = 85
    Width = 87
    Height = 22
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Year'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object txtphoto: TLabeledEdit
    Left = 580
    Top = 251
    Width = 169
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'Photo'
    TabOrder = 9
    Visible = False
  end
  object txtdescription: TMemo
    Left = 196
    Top = 164
    Width = 361
    Height = 62
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
  end
  object txtitem1: TEdit
    Left = 364
    Top = 338
    Width = 85
    Height = 21
    TabOrder = 10
  end
  object txtitem2: TEdit
    Left = 453
    Top = 338
    Width = 121
    Height = 21
    TabOrder = 11
  end
  object txtitem3: TEdit
    Left = 581
    Top = 338
    Width = 121
    Height = 21
    TabOrder = 12
  end
  object StringGrid1: TStringGrid
    Left = 194
    Top = 229
    Width = 252
    Height = 77
    BorderStyle = bsNone
    Color = clCaptionText
    ColCount = 1
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Options = []
    TabOrder = 13
  end
  object Memo1: TMemo
    Left = 200
    Top = 336
    Width = 543
    Height = 108
    TabOrder = 1
  end
  object txtsearch: TEdit
    Left = 19
    Top = 25
    Width = 131
    Height = 33
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 448
    Top = 278
    Width = 296
    Height = 45
    Align = alCustom
    ButtonHeight = 43
    ButtonWidth = 30
    EdgeBorders = []
    EdgeInner = esNone
    EdgeOuter = esNone
    Flat = True
    Indent = 1
    TabOrder = 15
    object btnClear: TBitBtn
      Left = 1
      Top = 0
      Width = 50
      Height = 43
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnSave: TBitBtn
      Left = 51
      Top = 0
      Width = 50
      Height = 43
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnDelete: TBitBtn
      Left = 101
      Top = 0
      Width = 50
      Height = 43
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnPrev: TBitBtn
      Left = 151
      Top = 0
      Width = 50
      Height = 43
      TabOrder = 3
      OnClick = btnPrevClick
      Layout = blGlyphTop
    end
    object pag: TLabel
      Left = 201
      Top = 0
      Width = 44
      Height = 43
      Alignment = taCenter
      AutoSize = False
      Caption = '    '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btnNext: TBitBtn
      Left = 245
      Top = 0
      Width = 50
      Height = 43
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = btnNextClick
      Margin = 0
    end
  end
  object btnItems: TBitBtn
    Left = 420
    Top = 232
    Width = 25
    Height = 21
    TabOrder = 16
    OnClick = btnItemsClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    FileName = 'warley.jpg'
    Left = 685
    Top = 228
  end
end
