object MainForm: TMainForm
  Left = 192
  Top = 107
  Width = 553
  Height = 186
  Caption = 'Strored Proc'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Go: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 0
    OnClick = GoClick
  end
  object Memo: TMemo
    Left = 8
    Top = 40
    Width = 489
    Height = 113
    TabOrder = 1
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'password=masterkey'
      'user_name=SYSDBA')
    DatabaseName = 'D:\EMPLOYEE.DB'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 96
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 128
  end
  object StoredProc: TUIBQuery
    Transaction = Transaction
    Left = 160
  end
end
