/// receive SQLite3 results from JSON/SQL HTTP server
program JSONSQLClient;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCrtSock,
  SynCommons;

function Client(const SQL: RawUTF8): RawUTF8;
var Http: THttpClientSocket;
    URI: AnsiString;
begin
  if ParamCount<>0 then
    URI := AnsiString(ParamStr(1)) else
    URI := 'localhost';
  Http := OpenHttp(URI,'888');
  if Http<>nil then
  try
    Http.Post('root',SQL,TEXT_CONTENT_TYPE);
    result := Http.Content;
  finally
    Http.Free;
  end else
    result := '';
end;

begin
  writeln(Client('select * from People where LastName=''Schubert'''));
  readln;
end.
