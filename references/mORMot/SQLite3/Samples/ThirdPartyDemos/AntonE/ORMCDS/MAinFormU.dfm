object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'mORMot into Nested TClientdatasets 1.1'
  ClientHeight = 287
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 70
    Width = 473
    Height = 187
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'This will demo ORMCDS routines that convert mORMot TSQLRecord cl' +
      'asses into nested TClientdatasets for easy UI handlng.'#13#10'It will ' +
      'also apply delta updates, i.e. only update fields in records tha' +
      't have changed in UI, giving field-level isolation when multiple' +
      ' users edit the same record in UI.'#13#10#13#10'Dynamic arrays of records ' +
      'are created as nested CDS as well as linked TSQLRecord tables.'#13#10 +
      #13#10'Simple reconcilliation is done to handle deleted, inserted rec' +
      'ords, array re-ordering, etc.'#13#10#13#10'First verion with many untested' +
      ' scenarios... Use at own risk!'
    WordWrap = True
  end
  object BtnSample1: TButton
    Left = 88
    Top = 8
    Width = 305
    Height = 25
    Caption = 'Sample 1 - Static TClientdataset+Fields'
    TabOrder = 0
    OnClick = BtnSample1Click
  end
  object BtnSample2: TButton
    Left = 88
    Top = 39
    Width = 305
    Height = 25
    Caption = 'Sample 2 - Dynamic TClientdataset+Fields'
    TabOrder = 1
    OnClick = BtnSample2Click
  end
end
