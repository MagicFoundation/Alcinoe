inherited frmMetadata: TfrmMetadata
  Width = 374
  Height = 276
  object pnlVer: TPanel
    Left = 0
    Top = 0
    Width = 374
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblVer: TLabel
      Left = 8
      Top = 8
      Width = 38
      Height = 13
      Caption = 'Version:'
    end
    object lblVerStr: TLabel
      Left = 160
      Top = 8
      Width = 68
      Height = 13
      Caption = 'Version String:'
    end
    object edtVer: TEdit
      Left = 56
      Top = 4
      Width = 89
      Height = 21
      Color = clInactiveBorder
      ReadOnly = True
      TabOrder = 0
    end
    object edtVerStr: TEdit
      Left = 240
      Top = 4
      Width = 89
      Height = 21
      Color = clInactiveBorder
      ReadOnly = True
      TabOrder = 1
    end
  end
  object lstStream: TListView
    Left = 0
    Top = 32
    Width = 374
    Height = 244
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Offset'
      end
      item
        AutoSize = True
        Caption = 'Size'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
end
