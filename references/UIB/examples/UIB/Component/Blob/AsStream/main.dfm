object MainForm: TMainForm
  Left = 253
  Top = 286
  Width = 305
  Height = 280
  HorzScrollBar.Range = 232
  VertScrollBar.Range = 197
  ActiveControl = LoadImage
  AutoScroll = False
  Caption = 'Read/Write Blob Stream'
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
  object Image: TImage
    Left = 58
    Top = 40
    Width = 215
    Height = 185
    Stretch = True
  end
  object LoadImage: TButton
    Left = 56
    Top = 6
    Width = 97
    Height = 27
    Caption = 'Load BMP Image'
    TabOrder = 0
    OnClick = LoadImageClick
  end
  object SaveImage: TButton
    Left = 156
    Top = 6
    Width = 101
    Height = 27
    Caption = 'Save BMP Image'
    TabOrder = 1
    OnClick = SaveImageClick
  end
  object DataBase: TUIBDataBase
    Params.Strings = (
      'password=masterkey'
      'user_name=SYSDBA'
      'sql_dialect=3'
      'lc_ctype=NONE')
    DatabaseName = 'd:\employee.db'
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    Left = 16
    Top = 8
  end
  object Transaction: TUIBTransaction
    DataBase = DataBase
    Left = 16
    Top = 40
  end
  object Query: TUIBQuery
    Transaction = Transaction
    Left = 16
    Top = 72
  end
  object OpenDialog: TOpenDialog
    FilterIndex = 0
    Left = 16
    Top = 104
  end
end
