program UnitNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ioutils,
  System.types,
  ALFiles,
  ALString;

var
  LProjectDirectory: ansiString;
  LCreateBackup: Boolean;
  LFiles: TStringDynArray;
  LsourceFile: AnsiString;
  i,j: integer;

begin

  try

    //Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    LProjectDirectory := ansiString(paramstr(1));
    LCreateBackup := not ALSameText(ALTrim(ansiString(paramstr(2))), 'false');

    LFiles := TDirectory.GetFiles(string(LProjectDirectory), '*.pas', TSearchOption.soAllDirectories);
    for I := Low(LFiles) to High(LFiles) do begin
      LsourceFile := ALGetStringFromFile(ansiString(LFiles[i]));
      if ALpos(#0,LsourceFile) > 0 then begin
        Writeln('Skipped '+LFiles[i]);
        continue; // skip unicode file
      end;
      for j := 0 to 31 do begin
        if J = 9 then continue; // tab
        if j = 10 then continue; // New line
        if j = 13 then continue; // carriage return
        if ALpos(ansiString(Chr(j)),LsourceFile) > 0 then raise Exception.CreateFmt('%s contain a bad character (%d)', [LFiles[i], j]);
      end;
      LsourceFile := ALStringReplace(LsourceFile,#13#10,#1,[rfReplaceALL]);
      LsourceFile := ALStringReplace(LsourceFile,#13,#1,[rfReplaceALL]);
      LsourceFile := ALStringReplace(LsourceFile,#10,#1,[rfReplaceALL]);
      LsourceFile := ALStringReplace(LsourceFile,#1,#13#10,[rfReplaceALL]);
      while ALpos(' '#13#10,LsourceFile) > 0 do
        LsourceFile := ALStringReplace(LsourceFile,' '#13#10,#13#10,[rfReplaceALL]);
      if LCreateBackup then begin
        if ALFileExists(ansiString(LFiles[i]) + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [ansiString(LFiles[i]) + '.bak']);
        if not ALrenameFile(ansiString(LFiles[i]), ansiString(LFiles[i]) + '.bak') then raiseLastOsError;
      end;
      ALSaveStringToFile(LsourceFile,ansiString(LFiles[i]));
    end;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
