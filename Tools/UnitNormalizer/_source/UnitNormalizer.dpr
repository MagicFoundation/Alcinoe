program UnitNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ioutils,
  System.types,
  ALStringList,
  ALFiles,
  ALString,
  ALCommon;

begin

  try

    //Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    //init LRootDirectory / LCreateBackup
    var LRootDirectory: String;
    var LCreateBackup: Boolean;
    var LParamLst := TALStringListU.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      LRootDirectory := ALTrimU(LParamLst.Values['-Dir']);
      LCreateBackup := not ALSameTextU(ALTrimU(LParamLst.Values['-CreateBackup']), 'false');
    finally
      ALFreeAndNil(LParamLst);
    end;
    if LRootDirectory = '' then begin
      LRootDirectory := ALTrimU(paramstr(1));
      LCreateBackup := not ALSameTextU(ALTrimU(paramstr(2)), 'false');
    end;
    if LRootDirectory = '' then raise Exception.Create('Usage: UnitNormalizer.exe -Dir="<Directory>" -CreateBackup=<true/false>');

    //loop on all *.pas in LRootDirectory
    var LFiles := TDirectory.GetFiles(string(LRootDirectory), '*.pas', TSearchOption.soAllDirectories);
    for var I := Low(LFiles) to High(LFiles) do begin

      {$REGION 'Init LSourceStr'}
      var LSourceStr := ALGetStringFromFile(ansiString(LFiles[i]));
      {$ENDREGION}

      {$REGION 'skip unicode file'}
      if ALpos(#0,LSourceStr) > 0 then begin
        Writeln('Skipped '+LFiles[i]);
        continue;
      end;
      {$ENDREGION}

      {$REGION 'check that the source file do not contain bad characters'}
      for var j := 0 to 31 do begin
        if J = 9 then continue; // tab
        if j = 10 then continue; // New line
        if j = 13 then continue; // carriage return
        if ALpos(ansiString(Chr(j)),LSourceStr) > 0 then raise Exception.CreateFmt('%s contain a bad character (%d)', [LFiles[i], j]);
      end;
      {$ENDREGION}

      {$REGION 'replace #13 by #13#10'}
      LSourceStr := ALStringReplace(LSourceStr,#13#10,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplace(LSourceStr,#13,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplace(LSourceStr,#10,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplace(LSourceStr,#1,#13#10,[rfReplaceALL]);
      {$ENDREGION}

      {$REGION 'replace <space>#13 by #13#10'}
      while ALpos(' '#13#10,LSourceStr) > 0 do
        LSourceStr := ALStringReplace(LSourceStr,' '#13#10,#13#10,[rfReplaceALL]);
      {$ENDREGION}

      {$REGION 'Save the file'}
      if LCreateBackup then begin
        if ALFileExists(ansiString(LFiles[i]) + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [ansiString(LFiles[i]) + '.bak']);
        if not ALrenameFile(ansiString(LFiles[i]), ansiString(LFiles[i]) + '.bak') then raiseLastOsError;
      end;
      ALSaveStringToFile(LSourceStr,ansiString(LFiles[i]));
      Writeln('Updated '+ LFiles[i]);
      {$ENDREGION}

    end;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
