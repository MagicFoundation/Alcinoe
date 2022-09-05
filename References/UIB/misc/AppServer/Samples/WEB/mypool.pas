unit mypool;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses PDGDB, PDGUIB, SuperObject;

var
  pool: IPDGConnectionPool;

implementation
uses SysUtils, PDGSocketStub, PDGUtils;

procedure init;
var
  obj: ISuperObject;
begin
  obj := TSuperObject.ParseFile(ExtractFilePath(ParamStr(0)) + 'appserver.json', false);
  pool := TPDGUIBConnectionPool.Create(obj['database'], 0);
end;

initialization
 init;

finalization
  while TPDGThread.ThreadCount > 0 do sleep(100);
  pool := nil;

end.
