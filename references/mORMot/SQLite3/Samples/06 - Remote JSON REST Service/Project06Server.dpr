program Project06Server;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SynCommons,
  mORMot,
  SysUtils;

type
  // TSQLRestServerFullMemory kind of server is light and enough for our purpose
  TServiceServer = class(TSQLRestServerFullMemory)
  published
    procedure Sum(Ctxt: TSQLRestServerURIContext);
  end;


{ TServiceServer }

procedure TServiceServer.Sum(Ctxt: TSQLRestServerURIContext);
// begin // the following would be faster to write, a bit slower to execute:
//   Ctxt.Results([Ctxt['a']+Ctxt['b']]);
// end;
var a,b: double;
begin
  if UrlDecodeNeedParameters(Ctxt.Parameters,'A,B') then begin
    while Ctxt.Parameters<>nil do begin
      UrlDecodeDouble(Ctxt.Parameters,'A=',a);
      UrlDecodeDouble(Ctxt.Parameters,'B=',b,@Ctxt.Parameters);
    end;
    Ctxt.Results([a+b]);
  end else
    Ctxt.Error('Missing Parameter');
end;

var
  aModel: TSQLModel;
begin
  aModel := TSQLModel.Create([],'service');
  try
    with TServiceServer.Create(aModel) do
    try
      if ExportServerNamedPipe('RestService') then
        writeln('Background server is running.'#10) else
        writeln('Error launching the server'#10);
      write('Press [Enter] to close the server.');
      readln;
    finally
      Free;
    end;
  finally
    aModel.Free;
  end;
end.
