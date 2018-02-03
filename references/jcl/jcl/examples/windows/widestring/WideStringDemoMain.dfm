object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TWideStringList Example (JclUnicode)'
  ClientHeight = 544
  ClientWidth = 791
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 20
  object FileListView: TListView
    Left = 0
    Top = 177
    Width = 791
    Height = 342
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 200
      end
      item
        Caption = 'Location'
        Width = 400
      end
      item
        Caption = 'Encoding'
        Width = 150
      end>
    RowSelect = True
    PopupMenu = FilePopupMenu
    TabOrder = 0
    ViewStyle = vsReport
    OnColumnClick = FileListViewColumnClick
    OnDblClick = OpenwithNotepad1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 791
    Height = 177
    Align = alTop
    TabOrder = 1
    object Label3: TLabel
      Left = 216
      Top = 58
      Width = 106
      Height = 20
      Caption = 'Filter encoding :'
    end
    object Label2: TLabel
      Left = 8
      Top = 58
      Width = 68
      Height = 20
      Caption = 'File mask :'
    end
    object Label1: TLabel
      Left = 8
      Top = 4
      Width = 102
      Height = 20
      Caption = 'Root directory :'
    end
    object FilterEncodingComboBox: TComboBox
      Left = 216
      Top = 79
      Width = 201
      Height = 28
      ItemHeight = 20
      TabOrder = 0
      Items.Strings = (
        ''
        'ANSI'
        'Unicode'
        'Unicode big endian'
        'UTF-8')
    end
    object IncludeSubDirectoriesCheckBox: TCheckBox
      Left = 448
      Top = 84
      Width = 177
      Height = 17
      Caption = 'Include sub directories'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object ConvertButton: TButton
      Left = 216
      Top = 120
      Width = 201
      Height = 41
      Caption = 'Convert UTF-8 to ANSI'
      Enabled = False
      TabOrder = 2
      OnClick = ConvertButtonClick
    end
    object SearchButton: TButton
      Left = 8
      Top = 120
      Width = 202
      Height = 41
      Caption = 'Search'
      TabOrder = 3
      OnClick = SearchButtonClick
    end
    object FileMaskEdit: TEdit
      Left = 8
      Top = 79
      Width = 202
      Height = 28
      TabOrder = 4
      Text = '*.pas;*.dfm;*.xfm;*.dpr;*.dpk*'
    end
    object RootDirectoryEdit: TEdit
      Left = 8
      Top = 24
      Width = 769
      Height = 28
      TabOrder = 5
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 519
    Width = 791
    Height = 25
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    Panels = <>
    SimplePanel = True
    UseSystemFont = False
  end
  object FilePopupMenu: TPopupMenu
    Left = 712
    Top = 96
    object OpenwithNotepad1: TMenuItem
      Caption = 'Open with Notepad'
      Default = True
      OnClick = OpenwithNotepad1Click
    end
  end
end
