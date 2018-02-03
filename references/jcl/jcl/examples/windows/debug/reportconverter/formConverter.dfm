object frmConverter: TfrmConverter
  Left = 227
  Top = 119
  ClientWidth = 454
  ClientHeight = 80
  Caption = 'Exception Report Converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnConvert: TButton
    Left = 368
    Top = 9
    Width = 75
    Height = 25
    Caption = '&Convert'
    TabOrder = 2
    OnClick = btnConvertClick
  end
  object txtReportFile: TEdit
    Left = 8
    Top = 11
    Width = 313
    Height = 21
    TabOrder = 0
    Text = 'txtReportFile'
  end
  object txtMapFile: TEdit
    Left = 8
    Top = 51
    Width = 313
    Height = 21
    TabOrder = 1
    Text = 'txtMapFile'
  end
  object btnReportFile: TButton
    Left = 328
    Top = 9
    Width = 22
    Height = 25
    Caption = '...'
    TabOrder = 3
    OnClick = btnReportFileClick
  end
  object btnMapFile: TButton
    Left = 328
    Top = 49
    Width = 22
    Height = 25
    Caption = '...'
    TabOrder = 4
    OnClick = btnMapFileClick
  end
  object dlgOpen: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 392
    Top = 51
  end
end
