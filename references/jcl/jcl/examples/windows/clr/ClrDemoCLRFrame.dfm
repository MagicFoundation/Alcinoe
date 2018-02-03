inherited frmCLR: TfrmCLR
  Width = 422
  Height = 325
  object PC: TPageControl
    Left = 0
    Top = 89
    Width = 422
    Height = 236
    ActivePage = tsStrongNameSign
    Align = alClient
    TabOrder = 0
    object tsStrongNameSign: TTabSheet
      Caption = 'Strong Name Signature'
      object memStrongNameSign: TMemo
        Left = 0
        Top = 0
        Width = 414
        Height = 208
        TabStop = False
        Align = alClient
        Color = clInactiveBorder
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Fixedsys'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tsResources: TTabSheet
      Caption = 'Resources'
      ImageIndex = 1
      object memResources: TMemo
        Left = 0
        Top = 105
        Width = 414
        Height = 103
        TabStop = False
        Align = alClient
        Color = clInactiveBorder
        Font.Charset = GB2312_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Fixedsys'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object lstResources: TListView
        Left = 0
        Top = 0
        Width = 414
        Height = 105
        Align = alTop
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Alignment = taCenter
            Caption = 'Offset'
            Width = 80
          end
          item
            Alignment = taCenter
            Caption = 'RVA'
            Width = 80
          end
          item
            Caption = 'Size'
            Width = 64
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
        OnSelectItem = lstResourcesSelectItem
      end
    end
    object tsVTableFixup: TTabSheet
      Caption = 'VTable Fixups'
      ImageIndex = 2
      object lstVTableFixups: TListView
        Left = 0
        Top = 0
        Width = 414
        Height = 208
        Align = alClient
        Columns = <
          item
            Caption = 'Index'
            Width = 40
          end
          item
            Alignment = taCenter
            Caption = 'Offset'
            Width = 80
          end
          item
            Caption = 'Count'
            Width = 40
          end
          item
            Caption = 'Flags'
            Width = 200
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 422
    Height = 89
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblVer: TLabel
      Left = 8
      Top = 16
      Width = 108
      Height = 13
      Caption = 'Required CLR Version:'
    end
    object lblEntryPointToken: TLabel
      Left = 8
      Top = 56
      Width = 88
      Height = 13
      Caption = 'Entry Point Token:'
    end
    object edtVer: TEdit
      Left = 120
      Top = 12
      Width = 121
      Height = 21
      Color = clInactiveBorder
      ReadOnly = True
      TabOrder = 0
    end
    object boxFlags: TGroupBox
      Left = 256
      Top = 0
      Width = 145
      Height = 81
      Caption = 'Image Runtime Flags'
      TabOrder = 1
      object lstFlags: TCheckListBox
        Left = 8
        Top = 16
        Width = 129
        Height = 57
        TabStop = False
        Color = clInactiveBorder
        ItemHeight = 13
        Items.Strings = (
          'IL Only'
          '32bit Required'
          'Strong Name Signed'
          'Track Debug Data')
        TabOrder = 0
      end
    end
    object edtEntryPointToken: TEdit
      Left = 120
      Top = 52
      Width = 121
      Height = 21
      Color = clInactiveBorder
      ReadOnly = True
      TabOrder = 2
    end
  end
end
