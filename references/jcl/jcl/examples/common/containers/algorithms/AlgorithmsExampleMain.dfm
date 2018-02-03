object MainForm: TMainForm
  Left = 280
  Top = 180
  ClientWidth = 384
  ClientHeight = 304
  ActiveControl = PageControl1
  Caption = 'Algos'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 384
    Height = 303
    ActivePage = tbsApply
    Align = alClient
    TabOrder = 0
    object tbsApply: TTabSheet
      Caption = 'Apply'
      object btnApplyGenerate: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnApplyGenerateClick
      end
      object btnApply: TButton
        Left = 152
        Top = 96
        Width = 75
        Height = 25
        Caption = 'Apply'
        TabOrder = 1
        OnClick = btnApplyClick
      end
      object lbxApply: TListBox
        Left = 16
        Top = 48
        Width = 121
        Height = 137
        ItemHeight = 13
        TabOrder = 2
      end
      object edtApply: TEdit
        Left = 240
        Top = 96
        Width = 121
        Height = 21
        TabOrder = 3
        Text = '4'
      end
    end
    object tbsFind: TTabSheet
      Caption = 'Find'
      ImageIndex = 1
      object lblFound: TLabel
        Left = 152
        Top = 136
        Width = 3
        Height = 13
      end
      object btnFindGenerate: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnFindGenerateClick
      end
      object btnFind: TButton
        Left = 152
        Top = 96
        Width = 75
        Height = 25
        Caption = 'Find'
        TabOrder = 1
        OnClick = btnFindClick
      end
      object lbxFind: TListBox
        Left = 16
        Top = 48
        Width = 121
        Height = 137
        ItemHeight = 13
        TabOrder = 2
      end
      object edtFind: TEdit
        Left = 240
        Top = 96
        Width = 121
        Height = 21
        TabOrder = 3
      end
    end
    object tbsCountObject: TTabSheet
      Caption = 'CountObject'
      ImageIndex = 2
      object lblCount: TLabel
        Left = 152
        Top = 136
        Width = 6
        Height = 13
        Caption = '0'
      end
      object btnCountGenerate: TButton
        Left = 152
        Top = 48
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnCountGenerateClick
      end
      object btnCount: TButton
        Left = 152
        Top = 96
        Width = 75
        Height = 25
        Caption = 'Count'
        TabOrder = 1
        OnClick = btnCountClick
      end
      object lbxCount: TListBox
        Left = 16
        Top = 48
        Width = 121
        Height = 137
        ItemHeight = 13
        TabOrder = 2
      end
      object edtCount: TEdit
        Left = 240
        Top = 96
        Width = 121
        Height = 21
        TabOrder = 3
      end
    end
    object tbsCopy: TTabSheet
      Caption = 'Copy'
      ImageIndex = 3
      object btnCopyGenerate: TButton
        Left = 136
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnCopyGenerateClick
      end
      object btnCopy: TButton
        Left = 136
        Top = 120
        Width = 75
        Height = 25
        Caption = 'Copy'
        TabOrder = 1
        OnClick = btnCopyClick
      end
      object lbxCopySrc: TListBox
        Left = 40
        Top = 56
        Width = 73
        Height = 153
        ItemHeight = 13
        TabOrder = 2
      end
      object lbxCopyDes: TListBox
        Left = 232
        Top = 56
        Width = 81
        Height = 153
        ItemHeight = 13
        TabOrder = 3
      end
    end
    object tbsReverse: TTabSheet
      Caption = 'Reverse'
      ImageIndex = 4
      object btnReverseGenerate: TButton
        Left = 176
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnReverseGenerateClick
      end
      object btnReverse: TButton
        Left = 176
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Reverse'
        TabOrder = 1
        OnClick = btnReverseClick
      end
      object lbxReverse: TListBox
        Left = 32
        Top = 56
        Width = 121
        Height = 153
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object tbsSort: TTabSheet
      Caption = 'Sort'
      ImageIndex = 5
      object btnSortGenerate: TButton
        Left = 176
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Generate'
        TabOrder = 0
        OnClick = btnSortGenerateClick
      end
      object btnSort: TButton
        Left = 176
        Top = 104
        Width = 75
        Height = 25
        Caption = 'Sort'
        TabOrder = 1
        OnClick = btnSortClick
      end
      object lbxSort: TListBox
        Left = 32
        Top = 56
        Width = 121
        Height = 153
        ItemHeight = 13
        TabOrder = 2
      end
    end
  end
end
