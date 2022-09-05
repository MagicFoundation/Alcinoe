object frmMain: TfrmMain
  Left = 527
  Top = 278
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'TAlHintBaloon demo application'
  ClientHeight = 629
  ClientWidth = 457
  Color = clBtnFace
  DockSite = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClick = FormClick
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblInfo: TLabel
    Left = 8
    Top = 8
    Width = 268
    Height = 39
    Caption = 
      'Hi! I am a test application for TAlHintBaloon component.'#13#10'With m' +
      'e you can show beatuful balloon hints like'#13#10'Windows XP (TM) does' +
      '!'
  end
  object LblAuthor: TLabel
    Left = 288
    Top = 8
    Width = 37
    Height = 13
    Caption = 'Author:'
  end
  object LblAuthorName: TLabel
    Left = 336
    Top = 8
    Width = 111
    Height = 13
    Cursor = crHandPoint
    Caption = 'Stephane Vander Clock'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LblAuthorNameClick
  end
  object LblDemo: TLabel
    Left = 288
    Top = 24
    Width = 147
    Height = 13
    Caption = 'This demo and some additions:'
  end
  object LblMe: TLabel
    Left = 288
    Top = 40
    Width = 36
    Height = 13
    Cursor = crHandPoint
    Caption = 'Quadr0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LblMeClick
  end
  object BtnExit: TButton
    Left = 152
    Top = 416
    Width = 153
    Height = 23
    Caption = '&OK, I got it!'
    Default = True
    TabOrder = 0
    OnClick = BtnExitClick
  end
  object GbStandart: TGroupBox
    Left = 8
    Top = 56
    Width = 441
    Height = 57
    Caption = 'Standart hint'
    TabOrder = 1
    object BtnError: TButton
      Left = 8
      Top = 24
      Width = 137
      Height = 23
      Caption = 'I am a frightening error!!!'
      TabOrder = 0
      OnClick = BtnErrorClick
    end
    object BtnWarn: TButton
      Left = 152
      Top = 24
      Width = 137
      Height = 23
      Caption = 'I am warning you!'
      TabOrder = 1
      OnClick = BtnWarnClick
    end
    object BtnInfo: TButton
      Left = 296
      Top = 24
      Width = 137
      Height = 23
      Caption = 'Information'
      TabOrder = 2
      OnClick = BtnInfoClick
    end
  end
  object GbPicture: TGroupBox
    Left = 8
    Top = 120
    Width = 441
    Height = 57
    Caption = 'Picture hint'
    TabOrder = 2
    object BtnPict1: TButton
      Left = 8
      Top = 24
      Width = 137
      Height = 23
      Caption = 'Show me picture hint'
      TabOrder = 0
      OnClick = BtnPict1Click
    end
    object BtnPict2: TButton
      Left = 152
      Top = 24
      Width = 137
      Height = 23
      Caption = 'One more picture hint'
      TabOrder = 1
      OnClick = BtnPict2Click
    end
    object BtnPict3: TButton
      Left = 296
      Top = 24
      Width = 137
      Height = 23
      Caption = 'My picture hint...'
      TabOrder = 2
      OnClick = BtnPict3Click
    end
  end
  object RadioButton1: TRadioButton
    Left = 120
    Top = 200
    Width = 113
    Height = 17
    Caption = 'Top left'
    TabOrder = 3
  end
  object RadioButton2: TRadioButton
    Left = 120
    Top = 216
    Width = 113
    Height = 17
    Caption = 'Bottom left'
    TabOrder = 4
  end
  object RadioButton5: TRadioButton
    Left = 264
    Top = 200
    Width = 113
    Height = 17
    Caption = 'Top right'
    TabOrder = 5
  end
  object RadioButton6: TRadioButton
    Left = 264
    Top = 216
    Width = 113
    Height = 17
    Caption = 'Bottom right'
    TabOrder = 6
  end
  object GbArrowPos: TRadioGroup
    Left = 8
    Top = 184
    Width = 441
    Height = 57
    Caption = 'Change my arrow position'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Top left'
      'Top right'
      'Bottom left'
      'Bottom right')
    TabOrder = 7
  end
  object GbAnimType: TRadioGroup
    Left = 8
    Top = 248
    Width = 441
    Height = 97
    Caption = 'Change my animation type'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Blend'
      'Center'
      'Slide left to right'
      'Slide right to left'
      'Slide top to bottom'
      'Slide bottom to top'
      'Wipe left to right'
      'Wipe right to left'
      'Wipe top to bottom'
      'Wipe bottom to top')
    TabOrder = 8
    OnClick = GbAnimTypeClick
  end
  object GbProps: TGroupBox
    Left = 8
    Top = 352
    Width = 441
    Height = 57
    Caption = 'Properities'
    TabOrder = 9
    object Label2: TLabel
      Left = 8
      Top = 28
      Width = 82
      Height = 13
      Caption = 'animation speed:'
    end
    object Label7: TLabel
      Left = 232
      Top = 28
      Width = 48
      Height = 13
      Caption = 'duratuon:'
    end
    object EdAnimSpeed: TEdit
      Left = 96
      Top = 24
      Width = 129
      Height = 21
      TabOrder = 0
      Text = '300'
      OnChange = EdAnimSpeedChange
    end
    object EdDuration: TEdit
      Left = 288
      Top = 24
      Width = 145
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = EdAnimSpeedChange
    end
  end
  object Panel2: TPanel
    Left = 85
    Top = 456
    Width = 292
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 10
    object Label1: TLabel
      Left = 5
      Top = 8
      Width = 132
      Height = 45
      Caption = 'Please help us to keep the development of these components free'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object Label8: TLabel
      Left = 5
      Top = 63
      Width = 125
      Height = 75
      Caption = 
        'If you like these components please simply click on each button ' +
        'below ... thanks for your support !'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
    object Panel3: TPanel
      Left = 151
      Top = 8
      Width = 130
      Height = 134
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Color = clWhite
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object PanelWebBrowser: TPanel
        Left = -5
        Top = -23
        Width = 133
        Height = 159
        BevelOuter = bvNone
        Color = clMedGray
        Ctl3D = False
        ParentBackground = False
        ParentCtl3D = False
        TabOrder = 0
      end
    end
  end
  object XPManifest: TXPManifest
    Left = 72
    Top = 208
  end
  object Hint: TALHintBalloonControl
    Left = 8
    Top = 208
  end
  object DlgOpen: TOpenDialog
    Left = 40
    Top = 208
  end
end
