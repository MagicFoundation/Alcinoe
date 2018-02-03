object PeGenDefChild: TPeGenDefChild
  Left = 278
  Top = 149
  ClientWidth = 401
  ClientHeight = 312
  Caption = 'Pascal unit generator'
  Color = clBtnFace
  Constraints.MinHeight = 230
  Constraints.MinWidth = 270
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 401
    Height = 311
    ActivePage = TabSheet1
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = '&Options'
      object FunctionsListView: TListView
        Left = 0
        Top = 96
        Width = 393
        Height = 184
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Original name'
            Width = 140
          end
          item
            Caption = 'Function name'
            Width = 140
          end
          item
            Caption = 'Address'
            Width = 70
          end>
        ColumnClick = False
        HotTrackStyles = []
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        SmallImages = MainForm.IconImageList
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = FunctionsListViewCustomDrawItem
        OnData = FunctionsListViewData
      end
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 392
        Height = 81
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Code generation options'
        TabOrder = 1
        object Label1: TLabel
          Left = 8
          Top = 20
          Width = 107
          Height = 13
          Caption = '&Library constant name:'
          FocusControl = LibConstNameEdit
        end
        object LibConstNameEdit: TEdit
          Left = 132
          Top = 16
          Width = 116
          Height = 21
          MaxLength = 32
          TabOrder = 0
        end
        object WrapSpinEdit: TSpinEdit
          Left = 132
          Top = 47
          Width = 57
          Height = 22
          Enabled = False
          MaxLength = 3
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 80
        end
        object WrapCheckBox: TCheckBox
          Left = 8
          Top = 48
          Width = 118
          Height = 17
          Caption = '&Wrap text at column:'
          TabOrder = 2
          OnClick = WrapCheckBoxClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = '&Unit source'
      ImageIndex = 1
      object UnitRichEdit: TRichEdit
        Left = 0
        Top = 0
        Width = 393
        Height = 280
        Align = alClient
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        HideScrollBars = False
        ParentFont = False
        PlainText = True
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'pas'
    Filter = 'Pascal unit (*.pas)|*.pas|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 12
    Top = 195
  end
end
