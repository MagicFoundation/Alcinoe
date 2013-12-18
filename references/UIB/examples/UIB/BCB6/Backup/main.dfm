object MainForm: TMainForm
  Left = 192
  Top = 107
  Width = 383
  Height = 333
  Caption = 'Backup Sample'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 8
    Top = 40
    Width = 321
    Height = 249
    TabOrder = 0
  end
  object Go: TButton
    Left = 8
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
      'd:\employee.gbk')
    Database = 'D:\EMPLOYEE.DB'
    OnVerbose = BackupVerbose
    Left = 88
    Top = 8
  end
end
