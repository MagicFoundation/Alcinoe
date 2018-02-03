object frmMain: TfrmMain
  Left = 274
  Top = 241
  Width = 696
  Height = 480
  Caption = 'Microsoft .Net Framework CLR Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object barStatus: TStatusBar
    Left = 0
    Top = 415
    Width = 688
    Height = 19
    Panels = <>
  end
  object PC: TPageControl
    Left = 0
    Top = 0
    Width = 688
    Height = 415
    ActivePage = tsCLR
    Align = alClient
    TabOrder = 1
    OnChange = PCChange
    object tsCLR: TTabSheet
      Caption = 'CLR'
      inline frmCLR: TfrmCLR
        Left = 0
        Top = 0
        Width = 680
        Height = 387
        Align = alClient
        TabOrder = 0
        inherited PC: TPageControl
          Width = 680
          Height = 298
          inherited tsStrongNameSign: TTabSheet
            inherited memStrongNameSign: TMemo
              Width = 672
              Height = 270
            end
          end
        end
        inherited pnlTop: TPanel
          Width = 680
        end
      end
    end
    object tsMetadata: TTabSheet
      Caption = 'Metadata'
      ImageIndex = 1
      inline frmMetadata: TfrmMetadata
        Left = 0
        Top = 0
        Width = 680
        Height = 387
        Align = alClient
        TabOrder = 0
        inherited pnlVer: TPanel
          Width = 680
        end
        inherited lstStream: TListView
          Width = 680
          Height = 355
          PopupMenu = popMetadataStream
          OnDblClick = frmMetadatalstStreamDblClick
        end
      end
    end
  end
  object lstActions: TActionList
    Left = 24
    Top = 144
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open'
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About'
      ShortCut = 112
      OnExecute = actHelpAboutExecute
    end
    object actViewStreamData: TAction
      Category = 'View'
      Caption = 'Stream &Data'
      OnExecute = actViewStreamDataExecute
      OnUpdate = actViewStreamDataUpdate
    end
    object actFileDump: TAction
      Category = 'File'
      Caption = 'Dump IL...'
      ShortCut = 16452
      OnExecute = actFileDumpExecute
      OnUpdate = actFileDumpUpdate
    end
  end
  object mnuMain: TMainMenu
    Left = 88
    Top = 144
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuFileOpen: TMenuItem
        Action = actFileOpen
      end
      object mnuFileDump: TMenuItem
        Action = actFileDump
      end
      object mnuFileLine0: TMenuItem
        Caption = '-'
      end
      object mnuFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      object mnuViewStreamData: TMenuItem
        Action = actViewStreamData
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuFileAbout: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executable Files (*.exe;*.dll)|*.exe;*.dll|All Files (*.*)|*.*'
    Title = 'Select a file to browse'
    Left = 152
    Top = 144
  end
  object popMetadataStream: TPopupMenu
    Left = 236
    Top = 144
    object popViewStreamData: TMenuItem
      Action = actViewStreamData
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = '.il'
    Filter = 
      'IL Source Files (*.il)|*.il|Text Files (*.txt)|*.txt|All Files (' +
      '*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Dump Metadata to IL Source File'
    Left = 320
    Top = 144
  end
end
