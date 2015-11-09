object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 710
  ClientWidth = 989
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 223
    Top = 57
    Width = 148
    Height = 13
    Caption = 'Number of items in the list'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel2: TPanel
    Left = 655
    Top = 20
    Width = 313
    Height = 153
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    object cxLabel1: TcxLabel
      Left = 12
      Top = 12
      Hint = ''
      Caption = 'Please help us to keep the development of these components free'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 242
    end
    object cxLabel2: TcxLabel
      Left = 12
      Top = 55
      Hint = ''
      Caption = 'If you like these components please go to:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 240
    end
    object cxWwwArkadiaComLabel: TcxLabel
      Left = 12
      Top = 71
      Cursor = crHandPoint
      Hint = ''
      Caption = 'http://www.arkadia.com'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clRed
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = [fsBold]
      Style.TextColor = clMaroon
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      OnClick = cxWwwArkadiaComLabelClick
      Width = 160
    end
    object cxLabel18: TcxLabel
      Left = 12
      Top = 88
      Hint = ''
      Caption = 'and click on the Facebook/Google+ like button'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 261
    end
    object cxLabel17: TcxLabel
      Left = 12
      Top = 120
      Hint = ''
      Caption = 'Thanks for your support !'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -12
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Properties.WordWrap = True
      Transparent = True
      Width = 144
    end
  end
  object Chart1: TChart
    Left = 0
    Top = 208
    Width = 989
    Height = 502
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.LabelsBehind = True
    BottomAxis.Title.Font.Style = [fsBold]
    BottomAxis.TitleSize = 1
    LeftAxis.Title.Caption = 'time taken'
    LeftAxis.Title.Font.Style = [fsBold]
    LeftAxis.TitleSize = 1
    View3D = False
    View3DOptions.Orthogonal = False
    Align = alBottom
    Color = clWhite
    TabOrder = 1
    PrintMargins = (
      15
      24
      15
      24)
    ColorPaletteIndex = 7
    object Series1: TBarSeries
      LegendTitle = 'TALAVLStringList'
      Marks.Angle = 90
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8618883
      Shadow.Color = 8618883
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series2: TBarSeries
      LegendTitle = 'TALHashedStringList'
      Marks.Angle = 90
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8684676
      Shadow.Color = 8684676
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series3: TBarSeries
      LegendTitle = 'TALStringList'
      Marks.Angle = 90
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8750469
      Shadow.Color = 8750469
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series4: TBarSeries
      LegendTitle = 'TStringList'
      Marks.Angle = 90
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8750469
      Shadow.Color = 8750469
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series5: TBarSeries
      LegendTitle = 'TALIntegerList'
      Marks.Angle = 65
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8684676
      Shadow.Color = 8684676
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series6: TBarSeries
      LegendTitle = 'TALNvStringList'
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Emboss.Color = 8487297
      Shadow.Color = 8487297
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
  end
  object Button4: TButton
    Left = 24
    Top = 164
    Width = 220
    Height = 25
    Caption = 'Run benchmark'
    TabOrder = 2
    OnClick = Button4Click
  end
  object SpinEditNbItems: TSpinEdit
    Left = 223
    Top = 76
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 1000
  end
  object CheckBoxALAVLStringList: TCheckBox
    Left = 24
    Top = 20
    Width = 145
    Height = 17
    Caption = 'TALAVLStringList'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBoxALHashedStringList: TCheckBox
    Left = 24
    Top = 43
    Width = 137
    Height = 17
    Caption = 'TALHashedStringList'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object CheckBoxALStringList: TCheckBox
    Left = 24
    Top = 66
    Width = 97
    Height = 17
    Caption = 'TALStringList'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object CheckBoxStringList: TCheckBox
    Left = 24
    Top = 88
    Width = 97
    Height = 17
    Caption = 'TStringList'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object CheckBoxALIntegerList: TCheckBox
    Left = 24
    Top = 111
    Width = 97
    Height = 17
    Caption = 'TALintegerList'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object CheckBoxALNvStringList: TCheckBox
    Left = 24
    Top = 134
    Width = 97
    Height = 17
    Caption = 'TALNVStringList'
    Checked = True
    State = cbChecked
    TabOrder = 9
  end
end
