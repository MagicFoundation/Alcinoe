unit alignedrecords;

interface

type
  TMyRecord = record
    Value: Integer;
    Align: string;
    class operator Initialize (out Dest: TMyRecord);
    class operator Finalize(var Dest: TMyRecord);
  end align 8;

implementation
	
class operator TMyRecord.Initialize (out Dest: TMyRecord);
begin
  Dest.Value := 10;
  Log('created' + IntToHex (Integer(Pointer(@Dest))));
end;
 
class operator TMyRecord.Finalize(var Dest: TMyRecord);
begin
  Log('destroyed' + IntToHex (Integer(Pointer(@Dest))));
end;

end.
