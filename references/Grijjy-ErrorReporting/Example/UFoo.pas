unit UFoo;

interface

uses
  System.SysUtils;

type
  TFoo = class
  public
    function ToString: String; override;
  end;

implementation

{ TFoo }

function TFoo.ToString: String;
var
  P: PInteger;
  I: Integer;
begin
  { Create an Access Violation by accessing an invalid address. }
  P := Pointer(10);
  I := P^;
  P^ := I + 1;
  Result := IntToStr(I);
end;

end.
