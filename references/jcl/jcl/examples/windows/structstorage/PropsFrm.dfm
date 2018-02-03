object frmProps: TfrmProps
  Left = 798
  Top = 376
  ClientWidth = 331
  ClientHeight = 349
  BorderIcons = [biSystemMenu]
  Caption = 'Properties'
  Color = clBtnFace
  Constraints.MaxHeight = 385
  Constraints.MaxWidth = 600
  Constraints.MinHeight = 385
  Constraints.MinWidth = 290
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 5
    Top = 4
    Width = 321
    Height = 309
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    Tabs.Strings = (
      'General')
    TabIndex = 0
    object Label1: TLabel
      Left = 12
      Top = 32
      Width = 35
      Height = 13
      Caption = 'Name:'
      FocusControl = edName
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 12
      Top = 103
      Width = 26
      Height = 13
      Caption = 'Size:'
      FocusControl = edSize
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 12
      Top = 139
      Width = 48
      Height = 13
      Caption = 'Created:'
      FocusControl = edCreated
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 12
      Top = 175
      Width = 51
      Height = 13
      Caption = 'Modified:'
      FocusControl = edModified
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 12
      Top = 211
      Width = 69
      Height = 13
      Caption = 'Last Access:'
      FocusControl = edAccessed
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label6: TLabel
      Left = 12
      Top = 67
      Width = 31
      Height = 13
      Caption = 'Type:'
      FocusControl = edType
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 12
      Top = 247
      Width = 36
      Height = 13
      Caption = 'CLSID:'
      FocusControl = edCLSID
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edName: TEdit
      Left = 26
      Top = 47
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object edSize: TEdit
      Left = 26
      Top = 118
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
    object edCreated: TEdit
      Left = 26
      Top = 154
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 3
    end
    object edModified: TEdit
      Left = 26
      Top = 190
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 4
    end
    object edAccessed: TEdit
      Left = 26
      Top = 226
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 5
    end
    object edType: TEdit
      Left = 26
      Top = 82
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object edCLSID: TEdit
      Left = 26
      Top = 262
      Width = 279
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      ParentColor = True
      ReadOnly = True
      TabOrder = 6
    end
  end
  object btnClose: TButton
    Left = 239
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    Default = True
    ModalResult = 7
    TabOrder = 1
  end
end
