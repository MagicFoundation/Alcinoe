object MainForm: TMainForm
  Left = 223
  Top = 224
  Width = 334
  Height = 108
  HorzScrollBar.Range = 147
  VertScrollBar.Range = 30
  ActiveControl = btExecute
  AutoScroll = False
  Caption = 'Quick Script'
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
  object btExecute: TButton
    Left = 124
    Top = 12
    Width = 69
    Height = 29
    Caption = 'Execute'
    TabOrder = 0
    OnClick = btExecuteClick
  end
  object Query: TUIBQuery
    Transaction = Transaction
    QuickScript = True
    Left = 88
    Top = 16
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'password=masterkey'
      'user_name=SYSDBA')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    Left = 24
    Top = 16
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 56
    Top = 16
  end
end
