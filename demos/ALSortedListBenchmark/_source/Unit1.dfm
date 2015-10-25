object Form1: TForm1
  Left = 445
  Top = 202
  Caption = 'Form1'
  ClientHeight = 738
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 239
    Top = 76
    Width = 215
    Height = 13
    Caption = 'bench the operation aLst.Values[xxx];'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 585
    Width = 711
    Height = 153
    Align = alBottom
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clSilver
    Ctl3D = False
    ParentBackground = False
    ParentCtl3D = False
    TabOrder = 0
    ExplicitWidth = 702
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
  object ALButton10: TButton
    Left = 234
    Top = 8
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringList (sorted)'
    TabOrder = 1
    OnClick = ALButton10Click
  end
  object ALButton11: TButton
    Left = 234
    Top = 39
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList (sorted)'
    TabOrder = 2
    OnClick = ALButton11Click
  end
  object ALButton33: TButton
    Left = 8
    Top = 39
    Width = 220
    Height = 25
    Caption = 'Benchmark TALAVLStringList'
    TabOrder = 3
    OnClick = ALButton33Click
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 220
    Height = 25
    Caption = 'Benchmark TALHashedStringList'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Chart1: TChart
    Left = 0
    Top = 104
    Width = 711
    Height = 481
    Legend.Visible = False
    BottomAxis.Title.Caption = 'NB items in the list'
    BottomAxis.Title.Font.Style = [fsBold]
    BottomAxis.TitleSize = 1
    LeftAxis.Title.Caption = 'time taken'
    LeftAxis.Title.Font.Style = [fsBold]
    LeftAxis.TitleSize = 1
    View3D = False
    View3DOptions.Orthogonal = False
    Align = alBottom
    Color = clWhite
    TabOrder = 5
    PrintMargins = (
      15
      24
      15
      24)
    ColorPaletteIndex = 7
    object Series1: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.Brush.Gradient.EndColor = 6724095
      Pointer.Gradient.EndColor = 6724095
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series2: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      Data = {0000000000}
    end
    object Series3: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series4: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series5: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object Series6: TLineSeries
      Marks.Arrow.Visible = True
      Marks.Callout.Brush.Color = clBlack
      Marks.Callout.Arrow.Visible = True
      Marks.ShapeStyle = fosRoundRectangle
      Marks.Visible = False
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      Pointer.Visible = False
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
  end
  object Button1: TButton
    Left = 460
    Top = 8
    Width = 220
    Height = 25
    Caption = 'Benchmark TALStringList (fullscan)'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 460
    Top = 39
    Width = 220
    Height = 25
    Caption = 'Benchmark TStringList (fulscan)'
    TabOrder = 7
    OnClick = Button3Click
  end
end
