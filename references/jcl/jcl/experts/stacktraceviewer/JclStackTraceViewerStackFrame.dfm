object frmStack: TfrmStack
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
        Caption = 'RsModuleName'
      end
      item
        Caption = 'RsSourceUnitName'
      end
      item
        Caption = 'RsProcedureName'
      end
      item
        Caption = 'RsSourceName'
      end
      item
        Alignment = taRightJustify
        Caption = 'RsLineNumber'
      end
      item
        Alignment = taRightJustify
        Caption = 'RsLineNumberOffsetFromProcedureStart'
      end
      item
        Caption = 'RsRevision'
      end
      item
        Caption = 'RsProjectFile'
      end
      item
        Alignment = taRightJustify
        Caption = 'RsTranslatedLineNumber'
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvChange
    OnDblClick = lvDblClick
  end
end
