object StretchDemoForm: TStretchDemoForm
  Left = 255
  Top = 208
  ClientWidth = 780
  ClientHeight = 583
  VertScrollBar.Range = 19
  ActiveControl = PageControl
  AutoScroll = False
  Caption = 'JCL Picture Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 12
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 780
    Height = 544
    ActivePage = OriginalPage
    Align = alClient
    TabOrder = 0
    OnChanging = PageControlChanging
    object OriginalPage: TTabSheet
      Caption = 'Original'
      object ScrollBox: TScrollBox
        Left = 0
        Top = 0
        Width = 772
        Height = 516
        HorzScrollBar.Tracking = True
        VertScrollBar.Tracking = True
        Align = alClient
        Color = clGray
        ParentColor = False
        TabOrder = 0
        object OriginalImage: TImage
          Left = 0
          Top = 0
          Width = 768
          Height = 512
          AutoSize = True
        end
      end
    end
    object StretchedPage: TTabSheet
      Caption = 'Resized'
      ImageIndex = 1
      OnResize = StretchedPageResize
      OnShow = StretchedPageShow
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 772
        Height = 516
        Align = alClient
      end
      object StretchedImage: TImage
        Left = 1
        Top = 1
        Width = 770
        Height = 513
        Anchors = [akLeft, akTop, akRight, akBottom]
      end
    end
    object FilesPage: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      object FileListView: TListView
        Left = 0
        Top = 0
        Width = 772
        Height = 516
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'File'
            MaxWidth = 800
            MinWidth = 300
          end
          item
            Caption = 'Size'
          end>
        HideSelection = False
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = FileListViewClick
        OnKeyDown = FileListViewKeyDown
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 544
    Width = 780
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 120
      end>
    SimplePanel = False
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'All (*.jpg;*.jpeg;*.bmp)|*.jpg;*.jpeg;*.bmp|JPEG Image File (*.j' +
      'pg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp'
    FilterIndex = 0
    Title = 'Open'
    Left = 240
    Top = 68
  end
  object MainMenu: TMainMenu
    Left = 208
    Top = 68
    object Fil1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = OpenFile
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitItem: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitApp
      end
    end
    object Filter1: TMenuItem
      Caption = '&Resampling Filter'
      object Box1: TMenuItem
        Caption = 'Bo&x'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Triangle1: TMenuItem
        Tag = 1
        Caption = '&Triangle'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Hermite1: TMenuItem
        Tag = 2
        Caption = '&Hermite'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Bell1: TMenuItem
        Tag = 3
        Caption = '&Bell'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Spline1: TMenuItem
        Tag = 4
        Caption = '&Spline'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Lanczos31: TMenuItem
        Tag = 5
        Caption = '&Lanczos 3'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
      object Mitchell1: TMenuItem
        Tag = 6
        Caption = '&Mitchell'
        GroupIndex = 1
        RadioItem = True
        OnClick = SelectFilter
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object PreserveAspectRatio1: TMenuItem
        Caption = 'Preserve Aspect Ratio'
        Checked = True
        OnClick = PreserveAspectRatio1Click
      end
    end
    object PrevItem: TMenuItem
      Caption = ' &<< '
      Hint = 'Previous file in directory'
      OnClick = PrevFile
    end
    object NextItem: TMenuItem
      Caption = ' &>> '
      Hint = 'Next file in directory'
      OnClick = NextFile
    end
  end
end
