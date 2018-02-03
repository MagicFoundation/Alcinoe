object frmServiceGroups: TfrmServiceGroups
  Left = 410
  Top = 278
  BorderStyle = bsDialog
  Caption = 'Service Groups'
  ClientHeight = 344
  ClientWidth = 216
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object treeServices: TTreeView
    Left = 8
    Top = 8
    Width = 201
    Height = 297
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnDblClick = treeServicesDblClick
  end
  object btnOK: TBitBtn
    Left = 72
    Top = 312
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
