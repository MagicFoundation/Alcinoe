object Form1: TForm1
  Left = 218
  Top = 207
  Width = 255
  Height = 220
  HorzScrollBar.Range = 64
  VertScrollBar.Range = 134
  ActiveControl = Button1
  AutoScroll = False
  Caption = 'Simple Query'
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
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 73
    Height = 25
    Caption = 'ReadQuery'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 8
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo: TMemo
    Left = 0
    Top = 48
    Width = 247
    Height = 145
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'user_name=SYSDBA'
      'password=masterkey')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 104
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 136
  end
  object Query: TUIBQuery
    Transaction = Transaction
    CachedFetch = False
    SQL.Strings = (
      
        'SELECT FIRST_NAME, LAST_NAME, SALARY FROM EMPLOYEE WHERE DEPT_NO' +
        ' = :dept')
    Left = 168
  end
end
