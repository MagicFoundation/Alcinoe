object MainForm: TMainForm
  Left = 313
  Top = 238
  ClientWidth = 669
  ClientHeight = 541
  Caption = 'Multimedia example'
  Color = clBtnFace
  Constraints.MinHeight = 515
  Constraints.MinWidth = 562
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 669
    Height = 540
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'CD audio'
      object Label2: TLabel
        Left = 8
        Top = 80
        Width = 48
        Height = 13
        Caption = '&Track info'
        FocusControl = AudioInfoMemo
      end
      object OpenDriveBtn: TButton
        Left = 208
        Top = 48
        Width = 75
        Height = 25
        Caption = '&Open drive'
        TabOrder = 0
        OnClick = OpenDriveBtnClick
      end
      object CloseDriveBtn: TButton
        Left = 288
        Top = 48
        Width = 75
        Height = 25
        Caption = '&Close drive'
        TabOrder = 1
        OnClick = CloseDriveBtnClick
      end
      object MediaPresentBtn: TButton
        Left = 384
        Top = 48
        Width = 75
        Height = 25
        Caption = '&Media ?'
        TabOrder = 2
        OnClick = MediaPresentBtnClick
      end
      object AudioInfoBtn: TButton
        Left = 464
        Top = 48
        Width = 75
        Height = 25
        Caption = 'CD &Audio info'
        TabOrder = 3
        OnClick = AudioInfoBtnClick
      end
      object AudioInfoMemo: TMemo
        Left = 8
        Top = 96
        Width = 647
        Height = 407
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 185
        Height = 65
        Caption = 'Drive Select'
        TabOrder = 5
        object Label1: TLabel
          Left = 8
          Top = 16
          Width = 28
          Height = 13
          Caption = 'Drive:'
          FocusControl = DriveComboBox
        end
        object DriveComboBox: TComboBox
          Left = 8
          Top = 32
          Width = 81
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object DefaultDriveCheckBox: TCheckBox
          Left = 96
          Top = 32
          Width = 81
          Height = 17
          Caption = '&Default drive'
          TabOrder = 1
          OnClick = DefaultDriveCheckBoxClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Audio mixer'
      ImageIndex = 1
      object Label3: TLabel
        Left = 8
        Top = 8
        Width = 46
        Height = 13
        Caption = '&Mixer tree'
        FocusControl = MixerTreeView
      end
      object Label4: TLabel
        Left = 272
        Top = 8
        Width = 32
        Height = 13
        Caption = '&Details'
        FocusControl = MixerDetailListView
      end
      object MixerTreeView: TTreeView
        Left = 8
        Top = 24
        Width = 257
        Height = 481
        Anchors = [akLeft, akTop, akBottom]
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnChange = MixerTreeViewChange
        OnCustomDrawItem = MixerTreeViewCustomDrawItem
      end
      object MixerDetailListView: TListView
        Left = 272
        Top = 24
        Width = 377
        Height = 409
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Item'
            Width = 100
          end
          item
            Caption = 'Value'
            Width = 270
          end>
        ColumnClick = False
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object GroupBox2: TGroupBox
        Left = 272
        Top = 440
        Width = 378
        Height = 65
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 2
        object SpeakersMuteCheckBox: TCheckBox
          Left = 8
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Speakers Mute'
          TabOrder = 0
          OnClick = SpeakersMuteCheckBoxClick
        end
        object SaveMixerBtn: TButton
          Left = 120
          Top = 20
          Width = 75
          Height = 25
          Caption = 'Save to File...'
          TabOrder = 1
          OnClick = SaveMixerBtnClick
        end
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 20
    Top = 488
  end
end
