program core_res;

uses
  Classes,
  SysUtils,
  SynCommons;

procedure removeCR(const Script: SynUnicode);
var
  c: PWideChar;
  i: Integer;
begin
  c := pointer(script);
  for I := 1 to Length(script) do begin
    if (c^ = #13) then
        c^ := ' ';
    Inc(c);
  end;
end;

Function FindCmdLineSwitchVal(const Switch: string; out Value: string): Boolean;
{$IFDEF FPC}
var
  I, L: Integer;
  S, T: String;
begin
  Result := False;
  S := Switch;
  Value := '';
  S := UpperCase(S);
  I := ParamCount;
  while (Not Result) and (I>0) do begin
    L := Length(Paramstr(I));
    if (L>0) and (ParamStr(I)[1] in SwitchChars) then begin
      T := Copy(ParamStr(I),2,L-1);
      T := UpperCase(T);
      Result := S=T;
      if Result and (I <> ParamCount) then
        Value := ParamStr(I+1)
    end;
    Dec(i);
  end;
end;
{$ELSE}
begin
   Result := FindCmdLineSwitch(Switch, value);
end;
{$ENDIF}

var
  L, R: TStringList;
  fIn, fOut: TFileName;

function transform(const L: TStringList; wrap: boolean): boolean;
var
  i: integer;
  s: SynUnicode;
  fn, folder: TFileName;
  fsOut: TFileStream;
begin
  for i := 0 to L.Count - 1 do begin
    fn := L[i];
    s := AnyTextFileToSynUnicode(fIn + fn, true);
    if s = '' then begin
      writeln('File not found', fIn + fn);
      exit(false);
    end;
    removeCR(s);
    if wrap then
      s := '(function (exports, require, module, __filename, __dirname) { ' + s + #10'});';
    folder := ExtractFileDir(fOut + fn);
    if not ForceDirectories(folder) then begin
      writeln('Can''t create output folder ', folder);
      exit;
    end;
    try
      fsOut := TFileStream.Create(fOut + fn, fmCreate);
      fsOut.Write(s[1], length(s)*2);
      fsOut.Free;
    except
      on E:Exception do begin
        writeln('File ', fOut + fn, ' could not be created because: ', E.Message);
        raise;
      end;
    end;
    R.Add(fn + #9'RCDATA "' + fn + '"');
  end;
  Result := true;
end;

begin
  ExitCode := -1;
  if not FindCmdLineSwitchVal('i', fIn) or
     not FindCmdLineSwitchVal('o', fOut) then
  begin
    writeln('Create a rc with files from modules_cjs.txt & modules_es6.txt');
    writeln('Files is converted to the Unicode as expected by SpiderMonkey');
    writeln('Usage: ');
    writeln(#9'core_res -i path/to/in/folder -o path/to/out/folder');
    exit;
  end;
  if not ForceDirectories(fOut) then begin
    writeln('Can''t create output folder ', fOut);
    exit;
  end;
  L := TStringList.Create;
  R := TStringList.Create;
  try
    L.LoadFromFile('./modules_cjs.txt');
    if not transform(L, true) then
      exit;
    L.LoadFromFile('./modules_es6.txt');
    if not transform(L, false) then
      exit;
    R.SaveToFile(fOut + 'core_res.rc');
  finally
    L.Free; R.Free;
  end;
  ExitCode := 0;
end.

