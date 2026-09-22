unit whitespacearoundifdefcondition;

interface

implementation

{$DEFINE CPUX86}

procedure Foo;
{$IFDEF  CPUX86 }
begin
end;
{$ENDIF}

procedure Foo2;
{$IF 	defined(CPUX86)  }
begin
end;
{$ENDIF}

initialization

end.
