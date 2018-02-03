object Form1: TForm1
  Left = 209
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JclPeImage PeXXX functions example'
  ClientHeight = 506
  ClientWidth = 561
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameLabel: TLabel
    Left = 96
    Top = 8
    Width = 70
    Height = 13
    Caption = 'FileNameLabel'
  end
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 91
    Height = 13
    Caption = '&Exported functions:'
    FocusControl = ExportsListBox
  end
  object Label2: TLabel
    Left = 8
    Top = 184
    Width = 87
    Height = 13
    Caption = 'I&mported functions'
    FocusControl = ImportsListBox
  end
  object Label3: TLabel
    Left = 368
    Top = 184
    Width = 79
    Height = 13
    Caption = 'Imported &libraries'
    FocusControl = ImportedLibsListBox
  end
  object Label4: TLabel
    Left = 8
    Top = 352
    Width = 28
    Height = 13
    Caption = '&Forms'
    FocusControl = FormsListBox
  end
  object Label5: TLabel
    Left = 216
    Top = 352
    Width = 37
    Height = 13
    Caption = '&Bitmaps'
    FocusControl = BitmapResListBox
  end
  object Label6: TLabel
    Left = 352
    Top = 352
    Width = 26
    Height = 13
    Caption = '&Icons'
    FocusControl = IconsListBox
  end
  object Label7: TLabel
    Left = 456
    Top = 352
    Width = 35
    Height = 13
    Caption = '&Cursors'
    FocusControl = CursorsListBox
  end
  object ExportsListBox: TListBox
    Left = 8
    Top = 56
    Width = 545
    Height = 121
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
  end
  object ImportsListBox: TListBox
    Left = 8
    Top = 200
    Width = 345
    Height = 145
    ItemHeight = 13
    Sorted = True
    TabOrder = 2
  end
  object ImportedLibsListBox: TListBox
    Left = 368
    Top = 200
    Width = 185
    Height = 145
    ItemHeight = 13
    Sorted = True
    TabOrder = 3
  end
  object BitmapResListBox: TListBox
    Tag = 200
    Left = 216
    Top = 368
    Width = 129
    Height = 129
    ItemHeight = 13
    Sorted = True
    TabOrder = 5
  end
  object OpenBtn: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = OpenBtnClick
  end
  object IconsListBox: TListBox
    Tag = 200
    Left = 352
    Top = 368
    Width = 97
    Height = 129
    ItemHeight = 13
    Sorted = True
    TabOrder = 6
  end
  object FormsListBox: TListBox
    Tag = 350
    Left = 8
    Top = 368
    Width = 201
    Height = 129
    ItemHeight = 13
    Sorted = True
    TabOrder = 4
  end
  object CursorsListBox: TListBox
    Tag = 200
    Left = 456
    Top = 368
    Width = 97
    Height = 129
    ItemHeight = 13
    Sorted = True
    TabOrder = 7
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Executable files (*.exe;*.dll;*.bpl;*.ocx)|*.exe;*.dll;*.bpl;*.o' +
      'cx'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 176
    Top = 8
  end
end
