unit finalizationinitializationexports;

interface

type
  TFoo = class(TObject)
    function A : Integer;
    constructor Create;
  end;

  TBar = record
    procedure B;
  end;

  IFooBar = interface
    ['{BED74FE6-570B-40F8-ABF0-5E23C8EE8E7E}']
    procedure C;
  end;

  procedure Hello;

const
  A = 1;

implementation

procedure Hello;
begin

end;

{ TFoo }

function TFoo.A : Integer;
begin

end;

constructor TFoo.Create;
begin

end;

exports
  Hello;

initialization
  Hello;

finalization
  Hello;

end.
