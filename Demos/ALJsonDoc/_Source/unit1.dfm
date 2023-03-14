object Form1: TForm1
  Left = 438
  Top = 209
  Caption = 'Form1'
  ClientHeight = 728
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1024
    Height = 728
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 1028
    ExplicitHeight = 729
    object TabSheet1: TTabSheet
      Caption = 'Main'
      object Label1: TLabel
        Left = 510
        Top = 21
        Width = 82
        Height = 13
        Caption = 'Sax mode events'
      end
      object ButtonLoadXmlWithALXmlDocument: TButton
        Left = 16
        Top = 632
        Width = 473
        Height = 25
        Caption = 'Load Json Document From the Previous Content'
        TabOrder = 0
        OnClick = ButtonLoadXmlWithALXmlDocumentClick
      end
      object MemoJson: TMemo
        Left = 16
        Top = 18
        Width = 473
        Height = 599
        Lines.Strings = (
          '{'
          '_id : 1.32,'
          '"name": {"first": "John","last": "Backus"},'
          '"birth": new Date('#39'2013-09-10T17:20:25.178Z'#39'), '
          '"contribs":["Fortran","ALGOL","Backus-Naur Form","FP"],'
          
            '"awards":[{"award":"National Medal of Science","year":1975,"by":' +
            '"National Science Foundation"},{"award":"Turing Award","year":19' +
            '77,"by":"ACM"}],'
          '"spouse":"",'
          '"address":{},'
          '"phones":[],'
          '"regex": /<TAG\b[^>]*>(.*?)<TAG>/im,'
          '"binary": BinData(0, "JliB6gIMRuSphAD2KmhzgQ=="),'
          '"ObjectId": ObjectId ( "507f1f77bcf86cd799439011" ),'
          
            '"Javascript":function showMilitaryTime() {if (document.theForm.s' +
            'howMilitary[0].checked) {return true;}return false;}'
          '}')
        ScrollBars = ssBoth
        TabOrder = 1
      end
      object ButtonCreateDynamicallyJsonDocument: TButton
        Left = 16
        Top = 663
        Width = 473
        Height = 25
        Caption = 'Create Dynamically Json Document'
        TabOrder = 2
        OnClick = ButtonCreateDynamicallyJsonDocumentClick
      end
      object MemoSaxModeEvents: TMemo
        Left = 510
        Top = 40
        Width = 499
        Height = 418
        ScrollBars = ssBoth
        TabOrder = 3
      end
      object MemoBSON: TMemo
        Left = 510
        Top = 505
        Width = 499
        Height = 183
        ScrollBars = ssBoth
        TabOrder = 4
      end
      object Button1: TButton
        Left = 510
        Top = 470
        Width = 499
        Height = 25
        Caption = 'Load JSON Document from BSON Content Below'
        TabOrder = 5
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Benchmark'
      ImageIndex = 1
      object Label2: TLabel
        Left = 287
        Top = 57
        Width = 133
        Height = 13
        Caption = 'Number of nodes (Find)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Chart1: TChart
        Left = 0
        Top = 104
        Width = 1020
        Height = 597
        Legend.Alignment = laTop
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
          23
          15
          23)
        ColorPaletteIndex = 7
        object Series1: TBarSeries
          HoverElement = []
          Legend.Text = 'TALJsonDoc (AnsiString)'
          LegendTitle = 'TALJsonDoc (AnsiString)'
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
          Legend.Text = 'TALJsonDocU (Unicode)'
          LegendTitle = 'TALJsonDocU (Unicode)'
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
          Legend.Text = 'System.Json'
          LegendTitle = 'System.Json'
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
          Legend.Text = 'dwsJSON'
          LegendTitle = 'dwsJSON'
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
          Legend.Text = 'SuperObject'
          LegendTitle = 'SuperObject'
          Marks.Visible = False
          Marks.Angle = 65
          Emboss.Color = 8684676
          Shadow.Color = 8684676
          XValues.Name = 'X'
          XValues.Order = loAscending
          YValues.Name = 'Bar'
          YValues.Order = loNone
        end
      end
      object BtnRunBenchmark: TButton
        Left = 24
        Top = 73
        Width = 220
        Height = 25
        Caption = 'Run benchmark'
        TabOrder = 1
        OnClick = BtnRunBenchmarkClick
      end
      object SpinEditNbItems: TSpinEdit
        Left = 287
        Top = 76
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 100
      end
      object CheckBoxTALJsonDocJSON: TCheckBox
        Left = 24
        Top = 20
        Width = 145
        Height = 17
        Caption = 'TALJsonDoc (Ansistring)'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object CheckBoxTALJsonDocUJSON: TCheckBox
        Left = 184
        Top = 20
        Width = 153
        Height = 17
        Caption = 'TALJsonDocU (Unicode)'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object CheckBoxSuperObject: TCheckBox
        Left = 583
        Top = 20
        Width = 97
        Height = 17
        Caption = 'SuperObject'
        TabOrder = 5
      end
      object CheckBoxSystemJSON: TCheckBox
        Left = 360
        Top = 20
        Width = 97
        Height = 17
        Caption = 'System.JSON'
        Checked = True
        State = cbChecked
        TabOrder = 6
      end
      object CheckBoxDwsJSON: TCheckBox
        Left = 480
        Top = 20
        Width = 97
        Height = 17
        Caption = ' dwsJSON'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
    end
  end
  object MainOpenDialog: TOpenDialog
    Left = 552
    Top = 8
  end
end
