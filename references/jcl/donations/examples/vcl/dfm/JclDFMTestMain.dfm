object fmJclDFMTest: TfmJclDFMTest
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'JEDI DFMTest'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 688
    Height = 453
    ActivePage = tsSkipAndReplaceList
    Align = alClient
    TabIndex = 2
    TabOrder = 0
    object tsTV: TTabSheet
      Caption = 'TreeView'
      object tvDFMTree: TTreeView
        Left = 0
        Top = 49
        Width = 680
        Height = 376
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 2
          Top = 2
          Width = 437
          Height = 13
          Caption = 
            'This demo loads the choosen .dfm file into a TJclDFMComponent an' +
            'd show'#39's it in a treeview.'
        end
        object btnDFM2Tree: TButton
          Left = 2
          Top = 18
          Width = 75
          Height = 25
          Caption = 'DFM 2 Tree'
          TabOrder = 0
          OnClick = btnDFM2TreeClick
        end
      end
    end
    object tsComponents: TTabSheet
      Caption = 'Components'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 49
        Width = 680
        Height = 376
        Align = alClient
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 2
          Top = 2
          Width = 309
          Height = 13
          Caption = 
            'This demo list'#39's all component types used in the choosen .dfm fi' +
            'le.'
        end
        object btnDFMGetComps: TButton
          Left = 2
          Top = 18
          Width = 113
          Height = 25
          Caption = 'Components 2 Memo'
          TabOrder = 0
          OnClick = btnDFMGetCompsClick
        end
      end
    end
    object tsSkipAndReplaceList: TTabSheet
      Caption = 'Remove&&Replace'
      ImageIndex = 2
      object memSkipProperties: TMemo
        Left = 0
        Top = 73
        Width = 680
        Height = 238
        Align = alClient
        Lines.Strings = (
          '*.DesignSize'
          'TPageControl.TabIndex'
          'TJvPageControl.TabIndex'
          'TImage.Proportional'
          'TJvComboBox.AutoDropDown'
          'TComboBox.AutoDropDown'
          'TMenuItem.AutoCheck'
          'TAction.AutoCheck'
          'TAction.GroupIndex')
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label3: TLabel
          Left = 2
          Top = 2
          Width = 635
          Height = 13
          Caption = 
            'This demo remove'#39's all unwanted properties and replace'#39's all def' +
            'ined property values in the choosen .dfm file and save'#39's it to a' +
            ' new file.'
        end
        object btnCleanDFM: TButton
          Left = 2
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Clean DFM'
          TabOrder = 0
          OnClick = btnCleanDFMClick
        end
      end
      object memPropertyReplaceList: TMemo
        Left = 0
        Top = 336
        Width = 680
        Height = 89
        Align = alBottom
        Lines.Strings = (
          'clHotLight=clBlue')
        TabOrder = 2
      end
      object Panel1: TPanel
        Left = 0
        Top = 311
        Width = 680
        Height = 25
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'property values to replace'
        TabOrder = 3
      end
      object Panel11: TPanel
        Left = 0
        Top = 49
        Width = 680
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        Caption = 'properties to remove'
        TabOrder = 4
      end
    end
    object tsImage: TTabSheet
      Caption = 'Images'
      ImageIndex = 3
      OnResize = tsImageResize
      object Image: TImage
        Left = 0
        Top = 49
        Width = 680
        Height = 376
        Align = alClient
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          Left = 2
          Top = 2
          Width = 452
          Height = 13
          Caption = 
            'This demo extracts all imagelist'#39's from the choosen .dfm file an' +
            'd show'#39's them in the image below.'
        end
        object btnExtractImageLists: TButton
          Left = 2
          Top = 18
          Width = 105
          Height = 25
          Caption = 'Extract ImageLists'
          TabOrder = 0
          OnClick = btnExtractImageListsClick
        end
      end
    end
    object tsLayout: TTabSheet
      Caption = 'Layout'
      ImageIndex = 4
      object Panel9: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label5: TLabel
          Left = 2
          Top = 2
          Width = 515
          Height = 13
          Caption = 
            'This demo applies an alternative layout to the panel below using' +
            ' the layout defined in LoadLayout.partial_dfm.'
        end
        object btnLoadLayout: TButton
          Left = 2
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Load Layout'
          TabOrder = 0
          OnClick = btnLoadLayoutClick
        end
      end
      object pnlLayout: TPanel
        Left = 0
        Top = 49
        Width = 680
        Height = 376
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlLayout'
        TabOrder = 1
        object Panel2: TPanel
          Left = 16
          Top = 72
          Width = 649
          Height = 289
          Caption = 'Panel2'
          TabOrder = 0
          object Panel3: TPanel
            Left = 1
            Top = 176
            Width = 647
            Height = 112
            Align = alBottom
            Caption = 'Panel3 - child of Panel2'
            TabOrder = 0
            object Panel4: TPanel
              Left = 1
              Top = 70
              Width = 645
              Height = 41
              Align = alBottom
              Caption = 'Panel4 - child of Panel3'
              TabOrder = 0
            end
          end
        end
      end
    end
    object tsTVItems: TTabSheet
      Caption = 'TreeView Items'
      ImageIndex = 5
      object tvItems: TTreeView
        Left = 0
        Top = 49
        Width = 680
        Height = 376
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object Panel10: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label6: TLabel
          Left = 2
          Top = 2
          Width = 603
          Height = 13
          Caption = 
            'This demo extracts the extract the items from the first TTreeVie' +
            'w found choosen .dfm file and show'#39's them in the treeview below.'
        end
      end
      object btnLoadTreeViewItems: TButton
        Left = 2
        Top = 18
        Width = 113
        Height = 25
        Caption = 'Load TreeView Items'
        TabOrder = 2
        OnClick = btnLoadTreeViewItemsClick
      end
    end
    object tsTranslate: TTabSheet
      Caption = '"Translate"'
      ImageIndex = 6
      object Panel12: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label7: TLabel
          Left = 2
          Top = 2
          Width = 475
          Height = 13
          Caption = 
            'This demo reverse'#39's all Caption and Hint properties in the choos' +
            'en .dfm file and save'#39's it to a new file.'
        end
        object btnTranslate: TButton
          Left = 2
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Translate'
          TabOrder = 0
          OnClick = btnTranslateClick
        end
      end
    end
  end
  object OpenDialog: TOpenDialog
    Filter = '*.dfm|*.dfm'
    Left = 552
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 584
    Top = 8
  end
end
