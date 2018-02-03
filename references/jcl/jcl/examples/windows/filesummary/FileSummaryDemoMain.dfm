object FormMain: TFormMain
  Left = 0
  Top = 0
  Width = 440
  Height = 552
  Caption = ';'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DriveComboBox1: TDriveComboBox
    Left = 8
    Top = 8
    Width = 193
    Height = 19
    Anchors = [akLeft, akTop, akRight]
    DirList = DirectoryListBox1
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 8
    Top = 248
    Width = 416
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Select a file to have its properties')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object FileListBox1: TFileListBox
    Left = 207
    Top = 8
    Width = 217
    Height = 234
    Anchors = [akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = FileListBox1Change
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 33
    Width = 193
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    FileList = FileListBox1
    ItemHeight = 16
    TabOrder = 3
  end
end
