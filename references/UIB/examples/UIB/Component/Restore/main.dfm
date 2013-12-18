object MainForm: TMainForm
  Left = 202
  Top = 188
  Width = 583
  Height = 284
  HorzScrollBar.Range = 54
  VertScrollBar.Range = 181
  ActiveControl = Log
  AutoScroll = False
  Caption = 'Restore Database'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Log: TMemo
    Left = 0
    Top = 40
    Width = 575
    Height = 217
    Align = alBottom
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Go: TButton
    Left = 0
    Top = 6
    Width = 65
    Height = 27
    Caption = 'Restore'
    TabOrder = 1
    OnClick = GoClick
  end
  object Restore: TUIBRestore
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    BackupFiles.Strings = (
      'd:\employee.gbk')
    Database = 'd:\employee.db'
    OnVerbose = RestoreVerbose
    Verbose = True
    Options = [roReplace]
    Left = 72
    Top = 8
  end
end
