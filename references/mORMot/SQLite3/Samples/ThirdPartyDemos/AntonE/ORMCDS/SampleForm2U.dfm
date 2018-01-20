object SampleForm2: TSampleForm2
  Left = 0
  Top = 0
  Caption = 'Dynamic CDS+Fields'
  ClientHeight = 404
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button2Click
  end
  object BtnApply: TButton
    Left = 253
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 1
    OnClick = BtnApplyClick
  end
  object JvDBUltimGrid1: TJvDBUltimGrid
    Left = 8
    Top = 39
    Width = 320
    Height = 120
    DataSource = DSPerson
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
    Columns = <
      item
        Expanded = False
        FieldName = 'Name'
        Width = 150
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Int'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Gender'
        Visible = True
      end>
  end
  object JvDBUltimGrid3: TJvDBUltimGrid
    Left = 8
    Top = 278
    Width = 320
    Height = 120
    DataSource = DSChildren
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
    Columns = <
      item
        Expanded = False
        FieldName = 'ChildName'
        Width = 187
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ChildGender'
        Width = 74
        Visible = True
      end>
  end
  object JvDBUltimGrid2: TJvDBUltimGrid
    Left = 8
    Top = 159
    Width = 320
    Height = 120
    DataSource = DSPhones
    TabOrder = 4
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
    EditControls = <>
    RowsHeight = 17
    TitleRowHeight = 17
    Columns = <
      item
        Expanded = False
        FieldName = 'Number'
        Width = 98
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PType_ptWork'
        Title.Caption = 'Work'
        Width = 37
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PType_ptHome'
        Title.Caption = 'Home'
        Width = 37
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PType_ptFax'
        Title.Caption = 'Fax'
        Width = 37
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PType_ptSMS'
        Title.Caption = 'SMS'
        Visible = True
      end>
  end
  object DSPerson: TDataSource
    Left = 168
    Top = 112
  end
  object DSPhones: TDataSource
    Left = 232
    Top = 112
  end
  object DSChildren: TDataSource
    Left = 296
    Top = 112
  end
end
