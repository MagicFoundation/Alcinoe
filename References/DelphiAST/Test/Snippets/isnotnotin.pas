unit isnotnotin;

interface

procedure Test;

implementation

procedure Test;
var
  a: array of Integer;
  b: TObject;
begin
  if 1 not in a and b is not TButton then
    Exit;
end;

end.