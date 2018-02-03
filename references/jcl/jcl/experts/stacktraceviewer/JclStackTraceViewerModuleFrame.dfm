object frmModule: TfrmModule
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object lv: TListView
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    Columns = <
      item
        Caption = 'RsStartAddr'
      end
      item
        Caption = 'RsEndAddr'
      end
      item
        Caption = 'RsSystemModule'
      end
      item
        Caption = 'RsFileName'
      end
      item
        Caption = 'RsBinFileVersion'
      end
      item
        Caption = 'RsFileVersion'
      end
      item
        Caption = 'RsFileDescription'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
