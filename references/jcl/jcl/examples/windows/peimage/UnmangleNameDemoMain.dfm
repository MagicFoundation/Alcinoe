object Form1: TForm1
  Left = 203
  Top = 123
  AutoScroll = False
  Caption = 'PeUnmangleName example'
  ClientHeight = 483
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PeFileLabel: TLabel
    Left = 104
    Top = 435
    Width = 72
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Borland PE file:'
  end
  object PackageLabel: TLabel
    Left = 224
    Top = 435
    Width = 46
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Package:'
  end
  object FilenameLabel: TLabel
    Left = 104
    Top = 452
    Width = 47
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'FileName:'
  end
  object PackageDescrLabel: TLabel
    Left = 104
    Top = 469
    Width = 103
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Package description: '
  end
  object PackageVerLabel: TLabel
    Left = 312
    Top = 435
    Width = 125
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Package compiler version:'
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 719
    Height = 433
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Export name'
        Width = 250
      end
      item
        Caption = 'Description'
        Width = 90
      end
      item
        Caption = 'RTTI TypeKind'
        Width = 85
      end
      item
        Caption = 'RTTI Name'
        Width = 120
      end
      item
        Caption = 'Info'
        Width = 200
      end>
    ColumnClick = False
    GridLines = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = ListView1Data
  end
  object OpenBtn: TButton
    Left = 6
    Top = 442
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Open'
    TabOrder = 1
    OnClick = OpenBtnClick
  end
  object OpenDialog1: TOpenDialog
    Filter = 'BPL|*.bpl|BPL, DLL|*.bpl;*.dll'
    Left = 8
    Top = 400
  end
end
