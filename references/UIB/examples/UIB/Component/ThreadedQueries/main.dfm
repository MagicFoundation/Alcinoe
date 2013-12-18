object Form1: TForm1
  Left = 209
  Top = 186
  Width = 193
  Height = 89
  HorzScrollBar.Range = 112
  VertScrollBar.Range = 24
  ActiveControl = Button1
  AutoScroll = False
  Caption = 'Threaded Queries'
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
    Left = 74
    Top = 6
    Width = 79
    Height = 27
    Caption = 'Run Queries'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'sql_dialect=3'
      'lc_ctype=NONE'
      'user_name=SYSDBA'
      'password=masterkey'
      '')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 8
    Top = 8
  end
end
