unit VariantRecordFieldAttributes;

interface

type
  TVariantRecord = record
    case byte of
      1:(
        Value: Double;
        [Example]
        ValueWithAttribute: Integer;
        );
  end;

implementation

end.