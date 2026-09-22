unit ternaryop;

interface

procedure Test;

implementation

procedure Test;
var
  a,b: Integer;
begin
  a := if True then 1 else 2;
  b := if a > 2 then 5 else 6 * 18;
end;

end.
