object Form1: TForm1
  Left = 438
  Top = 209
  Caption = 'Form1'
  ClientHeight = 697
  ClientWidth = 806
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 414
    Top = 28
    Width = 82
    Height = 13
    Caption = 'Sax mode events'
  end
  object ButtonLoadXmlWithALXmlDocument: TButton
    Left = 16
    Top = 16
    Width = 377
    Height = 25
    Caption = 'Load Json Document From the Content Below'
    TabOrder = 0
    OnClick = ButtonLoadXmlWithALXmlDocumentClick
  end
  object MemoLoadJsonDocument: TMemo
    Left = 16
    Top = 47
    Width = 377
    Height = 170
    Lines.Strings = (
      
        '{"_id":1.32,"name":{"first":"John","last":"Backus"},"birth":new ' +
        'Date('#39'2013-09-10T17:20:25.178Z'#39'),"contribs":["Fortran","ALGOL","' +
        'Backus-Naur Form","FP"],"awards":[{"award":"National Medal of Sc' +
        'ience","year":1975,"by":"National Science Foundation"},{"award":' +
        '"Turing Award","year":1977,"by":"ACM"}],"spouse":"","address":{}' +
        ',"phones":[]}')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object ButtonCreateDynamicallyJsonDocument: TButton
    Left = 16
    Top = 245
    Width = 377
    Height = 25
    Caption = 'Create Dynamically Json Document'
    TabOrder = 2
    OnClick = ButtonCreateDynamicallyJsonDocumentClick
  end
  object MemoCreateDynamicallyJsonDocument: TMemo
    Left = 16
    Top = 277
    Width = 377
    Height = 170
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object ButtonSaveToBson: TButton
    Left = 16
    Top = 474
    Width = 377
    Height = 25
    Caption = 'Save Dynamically created JSON To BSON'
    TabOrder = 4
    OnClick = ButtonSaveToBsonClick
  end
  object Panel1: TPanel
    Left = 468
    Top = 265
    Width = 292
    Height = 161
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 5
    object cxLabel1: TcxLabel
      Left = 12
      Top = 17
      Caption = 'Please help us to keep the development of these components free'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 242
    end
    object cxLabel2: TcxLabel
      Left = 12
      Top = 60
      Caption = 'If you like these components please go to:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 240
    end
    object cxWwwArkadiaComLabel: TcxLabel
      Left = 12
      Top = 76
      Cursor = crHandPoint
      Caption = 'http://www.arkadia.com'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clRed
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.TextColor = clMaroon
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      OnClick = cxWwwArkadiaComLabelClick
      Width = 160
    end
    object cxLabel18: TcxLabel
      Left = 12
      Top = 93
      Caption = 'and click on the Facebook/Google+ like button'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 261
    end
    object cxLabel17: TcxLabel
      Left = 12
      Top = 125
      Caption = 'Thanks for your support !'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 144
    end
  end
  object MemoLoadJsonDocumentSAXMODEResult: TMemo
    Left = 414
    Top = 47
    Width = 378
    Height = 170
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object MemoBSON1: TMemo
    Left = 16
    Top = 507
    Width = 377
    Height = 170
    ScrollBars = ssBoth
    TabOrder = 7
  end
  object MemoBSON2: TMemo
    Left = 414
    Top = 507
    Width = 378
    Height = 170
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object Button1: TButton
    Left = 414
    Top = 474
    Width = 378
    Height = 25
    Caption = 'Load JSON Document from BSON Content Below'
    TabOrder = 9
    OnClick = Button1Click
  end
end
