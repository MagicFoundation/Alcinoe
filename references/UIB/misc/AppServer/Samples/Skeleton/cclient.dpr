program cclient;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestLib_Client in 'TestLib_Client.pas',
  PDGUtils in '..\..\src\PDGUtils.pas',
  PDGZlib in '..\..\src\PDGZlib.pas';

var
  obj: TMyObject2Client;
begin
  obj := TMyObject2Client.Create('localhost', 33000);
  try
    writeln(obj.GetString);
  finally
    obj.Free;
  end;
  Readln;
end.
