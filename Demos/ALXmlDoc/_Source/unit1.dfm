object Form1: TForm1
  Left = 438
  Top = 209
  Caption = 'Form1'
  ClientHeight = 563
  ClientWidth = 599
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object ButtonLoadXmlWithALXmlDocument: TButton
    Left = 16
    Top = 16
    Width = 265
    Height = 25
    Caption = 'Load Xml File using TALXmlDocument'
    TabOrder = 0
    OnClick = ButtonLoadXmlWithALXmlDocumentClick
  end
  object ButtonLoadXmlWithXmlDocument: TButton
    Left = 296
    Top = 16
    Width = 292
    Height = 25
    Caption = 'Load Xml File using TXmlDocument'
    TabOrder = 1
    OnClick = ButtonLoadXmlWithXmlDocumentClick
  end
  object MemoLoadXmlWithALXmlDocument: TMemo
    Left = 16
    Top = 48
    Width = 265
    Height = 129
    TabOrder = 2
  end
  object MemoLoadXmlWithXmlDocument: TMemo
    Left = 296
    Top = 48
    Width = 292
    Height = 129
    TabOrder = 3
  end
  object ButtonGenerate100000NodeWithALXmlDocument: TButton
    Left = 16
    Top = 200
    Width = 265
    Height = 25
    Caption = 'Generate 100 000 nodes using TALXmlDocument'
    TabOrder = 4
    OnClick = ButtonGenerate100000NodeWithALXmlDocumentClick
  end
  object MemoGenerate100000NodeWithALXmlDocument: TMemo
    Left = 16
    Top = 232
    Width = 265
    Height = 129
    TabOrder = 5
  end
  object MemoGenerate100000NodeWithXmlDocument: TMemo
    Left = 296
    Top = 232
    Width = 292
    Height = 129
    TabOrder = 6
  end
  object ButtonGenerate100000NodeWithXmlDocument: TButton
    Left = 296
    Top = 200
    Width = 292
    Height = 25
    Caption = 'Generate 100 000 nodes using TXmlDocument'
    TabOrder = 7
    OnClick = ButtonGenerate100000NodeWithXmlDocumentClick
  end
  object ButtonParseXMLWithALXmlDocumentInSaxMode: TButton
    Left = 16
    Top = 384
    Width = 265
    Height = 25
    Caption = 'Parse Xml using TALXmlDocument in SAX mode'
    TabOrder = 8
    OnClick = ButtonParseXMLWithALXmlDocumentInSaxModeClick
  end
  object MemoParseXmlWithALXmlDocumentInSaxMode: TMemo
    Left = 16
    Top = 416
    Width = 265
    Height = 129
    TabOrder = 9
  end
  object MainOpenDialog: TOpenDialog
    DefaultExt = '*.xml'
    Filter = 'Xml|*.xml'
    Left = 432
    Top = 384
  end
end
