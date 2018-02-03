object frmDependent: TfrmDependent
  Left = 356
  Top = 295
  BorderStyle = bsDialog
  Caption = 'Dependent Services'
  ClientHeight = 239
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object boxDependOn: TGroupBox
    Left = 0
    Top = 0
    Width = 201
    Height = 201
    Caption = 'Depend On'
    TabOrder = 0
    object treeDependOn: TTreeView
      Left = 8
      Top = 16
      Width = 185
      Height = 177
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnDblClick = treeDependDblClick
    end
  end
  object boxDependBy: TGroupBox
    Left = 208
    Top = 1
    Width = 201
    Height = 200
    Caption = 'Depend By'
    TabOrder = 1
    object treeDependBy: TTreeView
      Left = 8
      Top = 16
      Width = 185
      Height = 177
      Indent = 19
      ReadOnly = True
      ShowLines = False
      ShowRoot = False
      TabOrder = 0
      OnDblClick = treeDependDblClick
    end
  end
  object btnOK: TBitBtn
    Left = 168
    Top = 211
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkOK
  end
end
