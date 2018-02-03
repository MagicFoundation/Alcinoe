object FileSearchForm: TFileSearchForm
  Left = 258
  Top = 301
  Width = 855
  Height = 509
  HorzScrollBar.Range = 378
  VertScrollBar.Range = 252
  ActiveControl = StartBtn
  Caption = 'File Search Demo (TJclFileEnumerator)'
  Color = clBtnFace
  Constraints.MinHeight = 279
  Constraints.MinWidth = 647
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = 12
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 461
    Width = 847
    Height = 21
    Panels = <
      item
        Alignment = taRightJustify
        Width = 100
      end
      item
        Alignment = taRightJustify
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object FileList: TListView
    Left = 0
    Top = 181
    Width = 847
    Height = 280
    Align = alClient
    Columns = <
      item
        Caption = 'File'
        Width = 360
      end
      item
        Alignment = taRightJustify
        AutoSize = True
        Caption = 'Size'
      end
      item
        Alignment = taCenter
        AutoSize = True
        Caption = 'Time'
      end
      item
        Caption = 'Attr.'
        Width = 60
      end>
    ReadOnly = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 847
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 14
      Top = 14
      Width = 16
      Height = 13
      Caption = 'List'
    end
    object Label2: TLabel
      Left = 216
      Top = 14
      Width = 29
      Height = 13
      Caption = 'files in'
    end
    object RootDirInput: TEdit
      Left = 256
      Top = 10
      Width = 248
      Height = 21
      TabOrder = 1
    end
    object StartBtn: TButton
      Left = 524
      Top = 10
      Width = 61
      Height = 25
      Caption = 'Start'
      TabOrder = 2
      OnClick = StartBtnClick
    end
    object StopBtn: TButton
      Left = 596
      Top = 10
      Width = 61
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 3
      OnClick = StopBtnClick
    end
    object DetailsBtn: TButton
      Left = 668
      Top = 10
      Width = 77
      Height = 25
      Caption = 'More >>'
      TabOrder = 4
      OnClick = DetailsBtnClick
    end
    object FileMaskInput: TEdit
      Left = 40
      Top = 10
      Width = 169
      Height = 21
      TabOrder = 0
      Text = '*'
    end
    object SaveBtn: TButton
      Left = 760
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 5
      OnClick = SaveBtnClick
    end
  end
  object DetailsPanel: TPanel
    Left = 0
    Top = 49
    Width = 847
    Height = 132
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object GroupBox1: TGroupBox
      Left = 256
      Top = 0
      Width = 249
      Height = 121
      Caption = 'File attributes'
      TabOrder = 0
      object cbReadOnly: TCheckBox
        Tag = 1
        Left = 16
        Top = 16
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Read only'
        State = cbGrayed
        TabOrder = 0
        OnClick = cbFileAttributeClick
      end
      object cbHidden: TCheckBox
        Tag = 2
        Left = 16
        Top = 40
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Hidden'
        TabOrder = 1
        OnClick = cbFileAttributeClick
      end
      object cbSystem: TCheckBox
        Tag = 4
        Left = 16
        Top = 64
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'System'
        TabOrder = 2
        OnClick = cbFileAttributeClick
      end
      object cbDirectory: TCheckBox
        Tag = 16
        Left = 16
        Top = 88
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Directory'
        TabOrder = 3
        OnClick = cbFileAttributeClick
      end
      object cbSymLink: TCheckBox
        Tag = 64
        Left = 136
        Top = 16
        Width = 101
        Height = 21
        AllowGrayed = True
        Caption = 'Symbolic link'
        State = cbGrayed
        TabOrder = 4
        OnClick = cbFileAttributeClick
      end
      object cbNormal: TCheckBox
        Tag = 128
        Left = 136
        Top = 88
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Normal'
        State = cbGrayed
        TabOrder = 7
        OnClick = cbFileAttributeClick
      end
      object cbArchive: TCheckBox
        Tag = 32
        Left = 136
        Top = 16
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Archive'
        State = cbGrayed
        TabOrder = 5
        OnClick = cbFileAttributeClick
      end
      object cbVolumeID: TCheckBox
        Tag = 8
        Left = 136
        Top = 40
        Width = 89
        Height = 21
        AllowGrayed = True
        Caption = 'Volume ID'
        TabOrder = 6
        OnClick = cbFileAttributeClick
      end
    end
    object cbLastChangeAfter: TCheckBox
      Left = 524
      Top = 12
      Width = 131
      Height = 30
      Caption = 'Last change after'
      TabOrder = 1
    end
    object edLastChangeAfter: TEdit
      Left = 656
      Top = 16
      Width = 113
      Height = 21
      MaxLength = 10
      TabOrder = 2
    end
    object cbLastChangeBefore: TCheckBox
      Left = 524
      Top = 36
      Width = 131
      Height = 30
      Caption = 'Last change before'
      TabOrder = 3
    end
    object edLastChangeBefore: TEdit
      Left = 656
      Top = 40
      Width = 113
      Height = 21
      MaxLength = 10
      TabOrder = 4
    end
    object cbFileSizeMax: TCheckBox
      Left = 524
      Top = 60
      Width = 131
      Height = 30
      Caption = 'Maximum size'
      TabOrder = 5
    end
    object edFileSizeMax: TEdit
      Left = 656
      Top = 64
      Width = 113
      Height = 21
      TabOrder = 6
      Text = '$7FFFFFFFFFFFFFFF'
    end
    object cbFileSizeMin: TCheckBox
      Left = 524
      Top = 84
      Width = 131
      Height = 30
      Caption = 'Minimum size'
      TabOrder = 7
    end
    object edFileSizeMin: TEdit
      Left = 656
      Top = 88
      Width = 113
      Height = 21
      TabOrder = 8
      Text = '0'
    end
    object IncludeSubDirectories: TCheckBox
      Left = 40
      Top = 18
      Width = 157
      Height = 17
      Caption = 'Include sub directories'
      Checked = True
      State = cbChecked
      TabOrder = 9
      OnClick = UpdateIncludeHiddenSubDirs
    end
    object IncludeHiddenSubDirs: TCheckBox
      Left = 40
      Top = 42
      Width = 201
      Height = 17
      Caption = 'Include hidden sub directories'
      TabOrder = 10
      OnClick = IncludeHiddenSubDirsClick
    end
    object cbDisplayLiveUpdate: TCheckBox
      Left = 40
      Top = 90
      Width = 189
      Height = 17
      Caption = '&Display live update'
      Checked = True
      State = cbChecked
      TabOrder = 12
    end
    object cbCaseInsensitiveSearch: TCheckBox
      Left = 40
      Top = 66
      Width = 177
      Height = 17
      Caption = 'Case insensitive search'
      TabOrder = 11
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Left = 216
    Top = 96
  end
end
