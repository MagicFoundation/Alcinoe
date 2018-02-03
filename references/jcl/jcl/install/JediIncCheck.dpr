{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
program JediIncCheck;

uses
  SysUtils;

var
  JediIncFileName: string;

function Main: Boolean;
var
  Line: string;
  F: TextFile;
  CompIfdefVersion: string;
  S: ShortString;
  Ps: Integer;
begin
  Str(CompilerVersion:1:1, S);
  {$IFDEF UNICODE}
  CompIfdefVersion := UTF8ToString(S);
  {$ELSE}
  CompIfdefVersion := S;
  {$ENDIF UNICODE}
  Ps := Pos('.', CompIfdefVersion);
  if Ps > 0 then
    Delete(CompIfdefVersion, Ps, 1)
  else
    CompIfdefVersion := CompIfdefVersion + '0';

  CompIfdefVersion := '{$IFDEF VER' + CompIfdefVersion + '}';

  Result := False;

  {$IOCHECKS OFF}
  AssignFile(F, JediIncFileName);
  Reset(F);
  if IOResult <> 0 then // file doesn't exit or can't be opened
    Exit;
  try
    while not Eof(F) do
    begin
      ReadLn(F, Line);
      if IOResult <> 0 then
        Exit;

      if Pos(CompIfdefVersion, Line) > 0 then // found a match for the compiler's version
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    CloseFile(F);
    IOResult;
  end;
  {$IOCHECKS ON}
end;

begin
  JediIncFileName := ExtractFileDir(ParamStr(0)) + '\..\source\include\jedi\jedi.inc';
  if Main then
    ExitCode := 0
  else
    ExitCode := 1;
end.
