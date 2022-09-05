object MainForm: TMainForm
  Left = 198
  Top = 183
  Width = 586
  Height = 354
  Caption = 'Cursor Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Update: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 0
    OnClick = UpdateClick
  end
  object Log: TMemo
    Left = 16
    Top = 96
    Width = 521
    Height = 217
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
    Left = 8
    Top = 16
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 40
    Top = 16
  end
  object SelectQuery: TUIBQuery
    SQL.Strings = (
      'SELECT proj_id, dept_no, projected_budget '
      'FROM proj_dept_budget WHERE fiscal_year = 1994'
      'FOR UPDATE OF projected_budget')
    Transaction = Transaction
    CachedFetch = False
    Left = 72
    Top = 16
  end
  object UpdateQuery: TUIBQuery
    SQL.Strings = (
      '')
    Transaction = Transaction
    OnError = etmStayIn
    CachedFetch = False
    Left = 104
    Top = 16
  end
end
