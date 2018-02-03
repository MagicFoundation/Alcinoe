object MainForm: TMainForm
  Left = 285
  Top = 165
  ClientWidth = 692
  ClientHeight = 494
  Caption = 'JclMapi (TJclEmail class) example'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 458
    Width = 89
    Height = 33
    Anchors = [akLeft, akBottom]
  end
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 16
    Height = 13
    Caption = '&To:'
    FocusControl = ToNameEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 39
    Height = 13
    Caption = '&Subject:'
    FocusControl = SubjectEdit
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 24
    Height = 13
    Caption = '&Body'
    FocusControl = BodyEdit
  end
  object ClientLabel: TLabel
    Left = 12
    Top = 459
    Width = 83
    Height = 30
    Alignment = taCenter
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'ClientLabel'
    Transparent = True
    Layout = tlCenter
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 60
    Width = 57
    Height = 13
    Caption = 'Attachment:'
  end
  object Label5: TLabel
    Left = 48
    Top = 12
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object Label6: TLabel
    Left = 203
    Top = 12
    Width = 38
    Height = 13
    Caption = 'Address'
  end
  object AttachmentPaintBox: TPaintBox
    Left = 68
    Top = 60
    Width = 513
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    OnPaint = AttachmentPaintBoxPaint
  end
  object ClientTypeGroupBox: TGroupBox
    Left = 8
    Top = 371
    Width = 89
    Height = 84
    Anchors = [akLeft, akBottom]
    Caption = '&Client connect'
    TabOrder = 4
    object AutomaticRadioBtn: TRadioButton
      Left = 8
      Top = 16
      Width = 70
      Height = 17
      Caption = '&Automatic'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = AutomaticRadioBtnClick
    end
    object MapiRadioBtn: TRadioButton
      Left = 8
      Top = 40
      Width = 70
      Height = 17
      Caption = '&MAPI'
      TabOrder = 1
      OnClick = AutomaticRadioBtnClick
    end
    object DirectRadioBtn: TRadioButton
      Left = 8
      Top = 64
      Width = 70
      Height = 17
      Caption = '&Direct'
      TabOrder = 2
      OnClick = AutomaticRadioBtnClick
    end
  end
  object ClientsListView: TListView
    Left = 104
    Top = 374
    Width = 446
    Height = 114
    Anchors = [akLeft, akRight, akBottom]
    Columns = <
      item
        Caption = 'KeyValue'
        Width = 80
      end
      item
        Caption = 'Client'
        Width = 80
      end
      item
        Caption = 'Path'
        Width = 240
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnCustomDrawItem = ClientsListViewCustomDrawItem
    OnSelectItem = ClientsListViewSelectItem
  end
  object ToNameEdit: TEdit
    Left = 80
    Top = 8
    Width = 113
    Height = 21
    TabOrder = 0
  end
  object SubjectEdit: TEdit
    Left = 48
    Top = 32
    Width = 533
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object BodyEdit: TRichEdit
    Left = 8
    Top = 96
    Width = 680
    Height = 271
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideScrollBars = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object SendBtn: TButton
    Left = 603
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 6
    OnClick = SendBtnClick
  end
  object AttachmentBtn: TButton
    Left = 605
    Top = 58
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Attachment'
    TabOrder = 7
    OnClick = AttachmentBtnClick
  end
  object ToAddressEdit: TEdit
    Left = 247
    Top = 8
    Width = 269
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object DialogCheckBox: TCheckBox
    Left = 603
    Top = 35
    Width = 81
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Show &dialog'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object ProfilesListView: TListView
    Left = 556
    Top = 374
    Width = 132
    Height = 114
    Anchors = [akRight, akBottom]
    Columns = <
      item
        Caption = 'Profile name'
        Width = 125
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 9
    ViewStyle = vsReport
    OnCustomDrawItem = ProfilesListViewCustomDrawItem
    OnSelectItem = ClientsListViewSelectItem
  end
  object HtmlCheckBox: TCheckBox
    Left = 48
    Top = 79
    Width = 97
    Height = 17
    Caption = 'HTML message'
    TabOrder = 10
  end
  object SaveBtn: TButton
    Left = 522
    Top = 4
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Sa&ve'
    TabOrder = 11
    OnClick = SaveBtnClick
  end
  object OpenDialog1: TOpenDialog
    Title = 'Select attachment'
    Left = 472
    Top = 104
  end
end
