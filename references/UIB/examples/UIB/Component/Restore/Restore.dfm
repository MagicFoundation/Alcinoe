object MainForm: TMainForm
  Left = 198
  Top = 168
  Width = 583
  Height = 284
  Caption = 'Backup Database'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Backup'
    TabOrder = 1
    OnClick = GoClick
  end
  object Backup: TUIBBackup
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    BackupFiles.Strings = (
      'd:\backup.gbk')
    Database = 'D:\employee.db'
    OnVerbose = BackupVerbose
    Left = 8
    Top = 48
  end
end
