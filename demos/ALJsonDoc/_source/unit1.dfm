object Form1: TForm1
  Left = 438
  Top = 209
  Caption = 'Form1'
  ClientHeight = 674
  ClientWidth = 816
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
    Height = 186
    Lines.Strings = (
      
        '{_id : 1.32,"name": {"first": "John","last": "Backus"},"birth": ' +
        'new Date('#39'2013-09-10T17:20:25.178Z'#39'), "contribs":["Fortran","ALG' +
        'OL","Backus-Naur Form","FP"],"awards":[{"award":"National Medal ' +
        'of Science","year":1975,"by":"National Science Foundation"},{"aw' +
        'ard":"Turing Award","year":1977,"by":"ACM"}],"spouse":"","addres' +
        's":{},"phones":[]}')
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
  object ButtonSaveToBson: TButton
    Left = 16
    Top = 276
    Width = 377
    Height = 25
    Caption = 'Create Dynamically Json Document and save To BSON'
    TabOrder = 3
    OnClick = ButtonSaveToBsonClick
  end
  object MemoLoadJsonDocumentSAXMODEResult: TMemo
    Left = 414
    Top = 47
    Width = 378
    Height = 370
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object MemoCreateDynamicallyJsonDocument: TMemo
    Left = 16
    Top = 473
    Width = 377
    Height = 183
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object MemoBSON: TMemo
    Left = 414
    Top = 473
    Width = 378
    Height = 183
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object Button1: TButton
    Left = 414
    Top = 434
    Width = 378
    Height = 25
    Caption = 'Load JSON Document from BSON Content Below'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 16
    Top = 307
    Width = 377
    Height = 25
    Caption = 'Load Json Document from file using TALJsonDoc'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button2: TButton
    Left = 16
    Top = 370
    Width = 377
    Height = 25
    Caption = 'Load Json Document from file using SuperObject'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 16
    Top = 403
    Width = 377
    Height = 25
    Caption = 'Load Json Document from file using DBXJson'
    TabOrder = 10
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 16
    Top = 434
    Width = 377
    Height = 25
    Caption = 'Load Json Document from file using dwsJSON'
    TabOrder = 11
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 16
    Top = 339
    Width = 377
    Height = 25
    Caption = 'Load Bson Document from file using TALJsonDoc'
    TabOrder = 12
    OnClick = Button6Click
  end
  object MainOpenDialog: TOpenDialog
    Left = 552
    Top = 8
  end
end
