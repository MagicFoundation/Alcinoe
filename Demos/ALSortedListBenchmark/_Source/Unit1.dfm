object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 617
  ClientWidth = 989
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label2: TLabel
    Left = 775
    Top = 17
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
  object Chart1: TChart
    Left = 0
    Top = 115
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
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      15
      24
      15
      24)
    ColorPaletteIndex = 7
    object Series1: TBarSeries
      HoverElement = []
      Legend.Text = 'TALAVLStringList'
      LegendTitle = 'TALAVLStringList'
      Marks.Visible = False
      Marks.Angle = 90
      Emboss.Color = 8618883
      Shadow.Color = 8618883
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series2: TBarSeries
      HoverElement = []
      Legend.Text = 'TALHashedStringList'
      LegendTitle = 'TALHashedStringList'
      Marks.Visible = False
      Marks.Angle = 90
      Emboss.Color = 8684676
      Shadow.Color = 8684676
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series3: TBarSeries
      HoverElement = []
      Legend.Text = 'TALStringList'
      LegendTitle = 'TALStringList'
      Marks.Visible = False
      Marks.Angle = 90
      Emboss.Color = 8750469
      Shadow.Color = 8750469
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series4: TBarSeries
      HoverElement = []
      Legend.Text = 'TStringList'
      LegendTitle = 'TStringList'
      Marks.Visible = False
      Marks.Angle = 90
      Emboss.Color = 8750469
      Shadow.Color = 8750469
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series5: TBarSeries
      HoverElement = []
      Legend.Text = 'TALIntegerList'
      LegendTitle = 'TALIntegerList'
      Marks.Visible = False
      Marks.Angle = 65
      Emboss.Color = 8684676
      Shadow.Color = 8684676
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object Series6: TBarSeries
      HoverElement = []
      Legend.Text = 'TALNvStringList'
      LegendTitle = 'TALNvStringList'
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
    Top = 60
    Width = 220
    Height = 25
    Caption = 'Run benchmark'
    TabOrder = 1
    OnClick = Button4Click
  end
  object SpinEditNbItems: TSpinEdit
    Left = 775
    Top = 36
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
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
    TabOrder = 3
  end
  object CheckBoxALHashedStringList: TCheckBox
    Left = 160
    Top = 20
    Width = 137
    Height = 17
    Caption = 'TALHashedStringList'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBoxALStringList: TCheckBox
    Left = 303
    Top = 20
    Width = 97
    Height = 17
    Caption = 'TALStringList'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object CheckBoxStringList: TCheckBox
    Left = 416
    Top = 20
    Width = 97
    Height = 17
    Caption = 'TStringList'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object CheckBoxALIntegerList: TCheckBox
    Left = 504
    Top = 20
    Width = 97
    Height = 17
    Caption = 'TALintegerList'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object CheckBoxALNvStringList: TCheckBox
    Left = 616
    Top = 20
    Width = 97
    Height = 17
    Caption = 'TALNVStringList'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
end
