object JclSIMDViewFrm: TJclSIMDViewFrm
  Left = 67
  Top = 78
  ClientHeight = 278
  ClientWidth = 429
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
  object Splitter: TSplitter
    Left = 371
    Top = 0
    Height = 278
    Align = alRight
  end
  object ListBoxRegs: TListBox
    Left = 0
    Top = 0
    Width = 371
    Height = 278
    Style = lbOwnerDrawFixed
    Align = alClient
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = PopupMenuRegs
    TabOrder = 0
    OnDrawItem = ListBoxRegsDrawItem
    OnMouseDown = ListBoxesMouseDown
  end
  object ListBoxMXCSR: TListBox
    Left = 374
    Top = 0
    Width = 55
    Height = 278
    Style = lbOwnerDrawFixed
    Align = alRight
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    PopupMenu = PopupMenuMXCSR
    TabOrder = 1
    OnDrawItem = ListBoxMXCSRDrawItem
    OnMouseDown = ListBoxesMouseDown
    OnMouseMove = ListBoxMXCSRMouseMove
  end
  object PopupMenuRegs: TPopupMenu
    Left = 64
    Top = 48
    object MenuItemDisplay: TMenuItem
      Caption = 'RsDisplay'
      object MenuItemBytes: TMenuItem
        Caption = '&Bytes'
        ShortCut = 16437
        OnClick = MenuItemDisplayClick
      end
      object MenuItemWords: TMenuItem
        Caption = '&Words'
        ShortCut = 16438
        OnClick = MenuItemDisplayClick
      end
      object MenuItemDWords: TMenuItem
        Caption = '&Double Words'
        ShortCut = 16439
        OnClick = MenuItemDisplayClick
      end
      object MenuItemQWords: TMenuItem
        Caption = '&Quads Words'
        ShortCut = 16440
        OnClick = MenuItemDisplayClick
      end
      object MenuItemSeparator1: TMenuItem
        Caption = '-'
      end
      object MenuItemSingles: TMenuItem
        Caption = '&Singles'
        ShortCut = 16441
        OnClick = MenuItemDisplayClick
      end
      object MenuItemDoubles: TMenuItem
        Caption = '&Doubles'
        ShortCut = 16432
        OnClick = MenuItemDisplayClick
      end
    end
    object MenuItemFormat: TMenuItem
      Caption = 'RsFormat'
      object MenuItemBinary: TMenuItem
        Caption = 'RsBinary'
        ShortCut = 16433
        OnClick = MenuItemFormatClick
      end
      object MenuItemSigned: TMenuItem
        Caption = 'RsSignedDecimal'
        ShortCut = 16434
        OnClick = MenuItemFormatClick
      end
      object MenuItemUnsigned: TMenuItem
        Caption = 'RsUnsignedDecimal'
        ShortCut = 16435
        OnClick = MenuItemFormatClick
      end
      object MenuItemHexa: TMenuItem
        Caption = 'RsHexadecimal'
        ShortCut = 16436
        OnClick = MenuItemFormatClick
      end
    end
    object MenuItemModify: TMenuItem
      Action = ActionModify
    end
    object MenuItemEmptyMM: TMenuItem
      Action = ActionEmpty
    end
    object MenuItemEmptyAll: TMenuItem
      Action = ActionEmptyAll
    end
    object MenuItemYMMEnabled: TMenuItem
      Action = ActionYMMEnabled
    end
    object MenuItemSeparator2: TMenuItem
      Caption = '-'
    end
    object MenuItemStayOnTop: TMenuItem
      Action = ActionStayOnTop
    end
    object MenuItemCpuInfo: TMenuItem
      Caption = 'RsCPUInfo'
      OnClick = MenuItemCpuInfoClick
    end
  end
  object PopupMenuMXCSR: TPopupMenu
    Left = 384
    Top = 48
    object MenuItemComplement: TMenuItem
      Action = ActionComplement
    end
  end
  object ActionListOptions: TActionList
    Left = 120
    Top = 48
    object ActionStayOnTop: TAction
      Caption = 'RsStayOnTop'
      OnExecute = ActionStayOnTopExecute
      OnUpdate = ActionStayOnTopUpdate
    end
    object ActionModify: TAction
      Caption = 'RsModify'
      OnExecute = ActionModifyExecute
      OnUpdate = ActionModifyUpdate
    end
    object ActionComplement: TAction
      Caption = 'RsComplementBit'
      ShortCut = 16468
      OnExecute = ActionComplementExecute
      OnUpdate = ActionComplementUpdate
    end
    object ActionEmpty: TAction
      Caption = 'RsEmptyMM'
      OnExecute = ActionEmptyExecute
      OnUpdate = ActionEmptyUpdate
    end
    object ActionEmptyAll: TAction
      Caption = 'RsEmptyAllMM'
      OnExecute = ActionEmptyAllExecute
      OnUpdate = ActionEmptyAllUpdate
    end
    object ActionYMMEnabled: TAction
      Caption = 'RsViewYMM'
      OnExecute = ActionYMMEnabledExecute
      OnUpdate = ActionYMMEnabledUpdate
    end
  end
end
