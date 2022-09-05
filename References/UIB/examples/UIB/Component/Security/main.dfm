object MainForm: TMainForm
  Left = 202
  Top = 207
  Width = 583
  Height = 284
  HorzScrollBar.Range = 54
  VertScrollBar.Range = 181
  AutoScroll = False
  Caption = 'User Security Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  DesignSize = (
    575
    250)
  PixelsPerInch = 96
  TextHeight = 13
  object Add: TButton
    Left = 6
    Top = 6
    Width = 73
    Height = 27
    Caption = 'Add'
    TabOrder = 0
    OnClick = AddClick
  end
  object Grid: TStringGrid
    Left = 6
    Top = 38
    Width = 565
    Height = 207
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultColWidth = 90
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    TabOrder = 1
  end
  object Edit: TButton
    Left = 82
    Top = 6
    Width = 73
    Height = 27
    Caption = 'Edit'
    TabOrder = 2
    OnClick = EditClick
  end
  object Delete: TButton
    Left = 158
    Top = 6
    Width = 73
    Height = 27
    Caption = 'Delete'
    TabOrder = 3
    OnClick = DeleteClick
  end
  object List: TButton
    Left = 234
    Top = 6
    Width = 73
    Height = 27
    Caption = 'List Users'
    TabOrder = 4
    OnClick = ListClick
  end
  object Security: TUIBSecurity
    UserName = 'SYSDBA'
    PassWord = 'masterkey'
    LibraryName = 'gds32.dll'
    UserID = 0
    GroupID = 0
    Left = 310
    Top = 6
  end
end
