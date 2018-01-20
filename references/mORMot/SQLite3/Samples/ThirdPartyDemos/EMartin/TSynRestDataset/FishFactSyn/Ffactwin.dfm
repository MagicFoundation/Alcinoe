object Form1: TForm1
  Left = 341
  Top = 83
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'FISH FACTS'
  ClientHeight = 584
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 6
    Top = 8
    Width = 299
    Height = 249
    Hint = 'Scroll grid below to see other fish'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object DBLabel1: TDBText
      Left = 4
      Top = 220
      Width = 249
      Height = 24
      DataField = 'Common_Name'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -19
      Font.Name = 'MS Serif'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object img: TImage
      Left = 8
      Top = 8
      Width = 281
      Height = 201
    end
    object btnUpload: TButton
      Left = 212
      Top = 216
      Width = 75
      Height = 25
      Caption = 'Upload'
      Enabled = False
      TabOrder = 0
      OnClick = btnUploadClick
    end
  end
  object Panel2: TPanel
    Left = 310
    Top = 8
    Width = 225
    Height = 22
    TabOrder = 1
    object Label1: TLabel
      Left = 7
      Top = 4
      Width = 56
      Height = 13
      Caption = 'About the'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object DBLabel2: TDBText
      Left = 67
      Top = 4
      Width = 56
      Height = 13
      AutoSize = True
      DataField = 'Common_Name'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object Panel3: TPanel
    Left = 312
    Top = 32
    Width = 223
    Height = 187
    BevelOuter = bvLowered
    TabOrder = 2
    object DBMemo1: TDBMemo
      Left = 3
      Top = 2
      Width = 217
      Height = 183
      BorderStyle = bsNone
      Color = clSilver
      Ctl3D = False
      DataField = 'Notes'
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 260
    Width = 542
    Height = 324
    Align = alBottom
    BevelInner = bvRaised
    BorderStyle = bsSingle
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object DBGrid1: TDBGrid
      Left = 2
      Top = 12
      Width = 534
      Height = 281
      Hint = 'Scroll up/down to see other fish!'
      Align = alBottom
      DataSource = DataSource1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clBlack
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object dbnvgr1: TDBNavigator
      Left = 2
      Top = 293
      Width = 534
      Height = 25
      DataSource = DataSource1
      Align = alBottom
      TabOrder = 1
      OnClick = dbnvgr1Click
    end
  end
  object DataSource1: TDataSource
    Left = 19
    Top = 193
  end
  object dlgOpenPic1: TOpenPictureDialog
    Filter = 
      'All (*.png;*.jpg;*.jpeg;*.gif;*.cur;*.pcx;*.ani;*.jpg;*.jpeg;*.b' +
      'mp;*.ico;*.emf;*.wmf)|*.png;*.jpg;*.jpeg;*.gif;*.cur;*.pcx;*.ani' +
      ';*.jpg;*.jpeg;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.' +
      'jpg|JPEG Image File (*.jpeg)|*.jpeg|CompuServe GIF Image (*.gif)' +
      '|*.gif|Cursor files (*.cur)|*.cur|PCX Image (*.pcx)|*.pcx|ANI Im' +
      'age (*.ani)|*.ani|JPEG Image File (*.jpg)|*.jpg|JPEG Image File ' +
      '(*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|Enhanc' +
      'ed Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|PNG Image Fil' +
      'e (*.png)|*.png'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Fish Image'
    Left = 174
    Top = 224
  end
end
