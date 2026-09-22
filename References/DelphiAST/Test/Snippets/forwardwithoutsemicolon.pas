unit forwardwithoutsemicolon;

interface

procedure proc1(); forward // NO TRAILING SEMICOLON
procedure proc2();

implementation

procedure proc1(); 
begin

end;

procedure proc2();
begin 

end;

end.