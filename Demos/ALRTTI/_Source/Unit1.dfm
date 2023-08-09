object Form1: TForm1
  Left = 442
  Top = 108
  Caption = 'Form1'
  ClientHeight = 769
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnClick = FormClick
  TextHeight = 13
  object ButtonCreateObjectWithAutoInit: TButton
    Left = 24
    Top = 733
    Width = 230
    Height = 25
    Caption = 'Bench Create Object AutoInit'
    TabOrder = 4
    OnClick = ButtonCreateObjectWithAutoInitClick
  end
  object ButtonCreateObjectClassicalWay: TButton
    Left = 280
    Top = 733
    Width = 230
    Height = 25
    Caption = 'Bench Create Object Classical Way'
    TabOrder = 5
    OnClick = ButtonCreateObjectClassicalWayClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 529
    Height = 41
    Align = alTop
    Color = clPurple
    ParentBackground = False
    TabOrder = 6
    ExplicitWidth = 525
    object Label1: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 511
      Height = 23
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Caption = 'You must run this demo in "Release" to see accurate benchmark. '
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 464
      ExplicitHeight = 20
    end
  end
  object ButtonBenchTALRttiTypeGetFields: TButton
    Left = 24
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetFields'
    TabOrder = 0
    OnClick = ButtonBenchTALRttiTypeGetFieldsClick
  end
  object ButtonBenchTRttiTypeGetFields: TButton
    Left = 280
    Top = 57
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetFields'
    TabOrder = 1
    OnClick = ButtonBenchTRttiTypeGetFieldsClick
  end
  object ButtonBenchTALRttiTypeGetField: TButton
    Left = 24
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TALRttiType.GetField'
    TabOrder = 2
    OnClick = ButtonBenchTALRttiTypeGetFieldClick
  end
  object ButtonBenchTRttiTypeGetField: TButton
    Left = 280
    Top = 88
    Width = 230
    Height = 25
    Caption = 'Bench TRttiType.GetField'
    TabOrder = 3
    OnClick = ButtonBenchTRttiTypeGetFieldClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 224
    Width = 486
    Height = 497
    Lines.Strings = (
      '  TAutoInitObject = class(TObject)'
      '  public'
      '    [TALInit('#39#233#39')]'
      '    CharValue: Char;'
      '    [TALInit('#39'e'#39')]'
      '    AnsiCharValue: ansiChar;'
      '    [TALInit('#39'abcdefg'#39')]'
      '    StringValue: String;'
      '    [TALInit('#39'abcdefg'#39')]'
      '    AnsiStringValue: ansiString;'
      '    [TALInit('#39'64'#39')]'
      '    Int64Value: Int64;'
      '    [TALInit('#39'32'#39')]'
      '    Int32Value: Int32;'
      '    [TALInit('#39'3.14159265359'#39')]'
      '    SingleValue: Single;'
      '    [TALInit('#39'3.14159265359'#39')]'
      '    DoubleValue: Double;'
      '    [TALInit('#39'now'#39')]'
      '    DateTimeValue: TDateTime;'
      '    [TALInit('#39'true'#39')]'
      '    BooleanValue: Boolean;'
      '    [TALInit('#39'alRight'#39')]'
      '    AlignValue: TAlign;'
      '    [TALInit('#39'[alTop,alRight,alClient]'#39')]'
      '    AlignValues: TAlignSet;'
      
        '    [TALInit('#39'QuoteChar:A;NameValueSeparator:B;Options:[soStrict' +
        'Delimiter, soWriteBOM]'#39')]'
      '    StringList: TStringList;'
      '    [TALInit('#39'Owner.FormClick'#39')]'
      '    Onclick: TnotifyEvent;'
      '    [TALInit('#39'left:50;right:75'#39')]'
      '    Rect: TRect;'
      '    [TALInit('#39#39')]'
      '    ChildObject: TChildObject;'
      '    Owner: TForm1;'#11
      '  End;')
    TabOrder = 7
  end
  object Panel2: TPanel
    Left = 24
    Top = 122
    Width = 486
    Height = 96
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 8
    object Label2: TLabel
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 468
      Height = 80
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Caption = 
        'AutoInit will automatically instantiate the following object and' +
        ' initialize its fields based on their attributes. This is in con' +
        'trast to the traditional approach where members are manually ini' +
        'tialized in the constructor.'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      WordWrap = True
      ExplicitWidth = 467
    end
  end
end
