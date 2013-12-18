object Form1: TForm1
  Left = 210
  Top = 167
  Width = 493
  Height = 178
  HorzScrollBar.Range = 417
  VertScrollBar.Range = 116
  ActiveControl = Button1
  AutoScroll = False
  Caption = 'Read/Write Blob String'
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
    Caption = 'Read project'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo: TMemo
    Left = 0
    Top = 31
    Width = 441
    Height = 93
    TabOrder = 1
  end
  object Button2: TButton
    Left = 80
    Top = 0
    Width = 97
    Height = 25
    Caption = 'Update Description'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Description: TEdit
    Left = 184
    Top = 0
    Width = 261
    Height = 21
    TabOrder = 3
    Text = 'Description'
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA'
      'lc_ctype=NONE'
      'sql_dialect=3')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 80
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 48
    Top = 80
  end
  object Query: TUIBQuery
    Transaction = Transaction
    SQL.Strings = (
      
        'SELECT proj_name, proj_desc, product FROM project  WHERE PROJ_ID' +
        ' = '#39'DGPII'#39';')
    Left = 80
    Top = 80
  end
  object UpdateQuery: TUIBQuery
    Transaction = Transaction
    SQL.Strings = (
      
        'UPDATE project SET proj_desc = :description WHERE proj_id = '#39'DGP' +
        'II'#39)
    Left = 112
    Top = 80
  end
end
