object SampleForm1: TSampleForm1
  Left = 0
  Top = 0
  Caption = 'Static CDS+Fields'
  ClientHeight = 404
  ClientWidth = 630
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
  object Label1: TLabel
    Left = 334
    Top = 8
    Width = 288
    Height = 388
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Core work is done by creating a TORMCDSinfo object for every nes' +
      'ted TClientdataset.'#13#10'This is then linked to the TORMCDSinfo(TDat' +
      'asetField.Tag) as well as the nested-child TClientdataset.Tag.'#13#10 +
      'These TORMCDSinfo objects are referenced by the routines to esta' +
      'blish relations and supply type info.'#13#10#13#10'When TClientdatasets ex' +
      'ist,  just run ORM_LinkCDS '
    WordWrap = True
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 0
    OnClick = Button2Click
  end
  object JvDBUltimGrid1: TJvDBUltimGrid
    Left = 8
    Top = 39
    Width = 320
    Height = 120
    DataSource = DSPerson
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnGetCellParams = JvDBUltimGrid1GetCellParams
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
  object JvDBUltimGrid2: TJvDBUltimGrid
    Left = 8
    Top = 165
    Width = 320
    Height = 120
    DataSource = DSPhones
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    OnGetCellParams = JvDBUltimGrid1GetCellParams
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
    OnGetCellParams = JvDBUltimGrid1GetCellParams
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
  object BtnApply: TButton
    Left = 253
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 4
    OnClick = BtnApplyClick
  end
  object DSPerson: TDataSource
    DataSet = cdsPerson
    Left = 168
    Top = 112
  end
  object DSPhones: TDataSource
    DataSet = cdsPhones
    Left = 232
    Top = 112
  end
  object DSChildren: TDataSource
    DataSet = cdsChildren
    Left = 296
    Top = 112
  end
  object cdsChildren: TClientDataSet
    Aggregates = <>
    DataSetField = cdsPersonChildren
    Params = <>
    Left = 296
    Top = 56
    object cdsChildrenID: TLargeintField
      FieldName = 'ID'
    end
    object cdsChildrenParent: TLargeintField
      FieldName = 'Parent'
    end
    object cdsChildrenChildName: TStringField
      FieldName = 'ChildName'
      Size = 50
    end
    object cdsChildrenChildGender: TIntegerField
      FieldName = 'ChildGender'
    end
  end
  object cdsPhones: TClientDataSet
    Aggregates = <>
    DataSetField = cdsPersonPhones
    Params = <>
    Left = 232
    Top = 56
    object cdsPhonesNumber: TStringField
      FieldName = 'Number'
      Size = 30
    end
    object cdsPhonesPType_ptWork: TBooleanField
      FieldName = 'PType_ptWork'
    end
    object cdsPhonesPType_ptHome: TBooleanField
      FieldName = 'PType_ptHome'
    end
    object cdsPhonesPType_ptFax: TBooleanField
      FieldName = 'PType_ptFax'
    end
    object cdsPhonesPType_ptSMS: TBooleanField
      FieldName = 'PType_ptSMS'
    end
  end
  object cdsPerson: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 168
    Top = 56
    object cdsPersonID: TLargeintField
      FieldName = 'ID'
    end
    object cdsPersonName: TStringField
      FieldName = 'Name'
      Size = 50
    end
    object cdsPersonInt: TIntegerField
      FieldName = 'Int'
    end
    object cdsPersonPhones: TDataSetField
      FieldName = 'Phones'
    end
    object cdsPersonGender: TIntegerField
      FieldName = 'Gender'
    end
    object cdsPersonChildren: TDataSetField
      FieldName = 'Children'
    end
  end
end
