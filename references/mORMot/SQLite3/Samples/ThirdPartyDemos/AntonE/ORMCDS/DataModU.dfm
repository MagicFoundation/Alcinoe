object DataMod: TDataMod
  OldCreateOrder = False
  Height = 150
  Width = 215
  object cdsPerson: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 32
    Top = 16
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
  object DSPerson: TDataSource
    DataSet = cdsPerson
    Left = 32
    Top = 72
  end
end
