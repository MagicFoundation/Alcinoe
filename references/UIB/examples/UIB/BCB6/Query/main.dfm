object MainForm: TMainForm
  Left = 195
  Top = 130
  Width = 233
  Height = 184
  Caption = 'Query sample'
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
    Left = 0
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Read Query'
    TabOrder = 0
    OnClick = GoClick
  end
  object Memo: TMemo
    Left = 0
    Top = 32
    Width = 209
    Height = 105
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
    Left = 80
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 112
  end
  object Query: TUIBQuery
    SQL.Strings = (
      
        'SELECT FIRST_NAME, LAST_NAME, SALARY FROM EMPLOYEE WHERE DEPT_NO' +
        ' = ?')
    Transaction = Transaction
    CachedFetch = False
    Left = 144
  end
end
