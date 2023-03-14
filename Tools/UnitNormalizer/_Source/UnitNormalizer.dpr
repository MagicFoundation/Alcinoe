program UnitNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ioutils,
  System.types,
  System.AnsiStrings,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.Common;

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
    var LParamLst := TALStringListW.Create;
    try
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      LRootDirectory := ALTrim(LParamLst.Values['-Dir']);
      LCreateBackup := not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false');
    finally
      ALFreeAndNil(LParamLst);
    end;
    if LRootDirectory = '' then begin
      LRootDirectory := ALTrim(paramstr(1));
      LCreateBackup := not ALSameTextW(ALTrim(paramstr(2)), 'false');
    end;
    if LRootDirectory = '' then raise Exception.Create('Usage: UnitNormalizer.exe -Dir="<Directory>" -CreateBackup=<true/false>');

    //loop on all *.pas in LRootDirectory
    var LFiles := TDirectory.GetFiles(string(LRootDirectory), '*.pas', TSearchOption.soAllDirectories);
    for var I := Low(LFiles) to High(LFiles) do begin

      {$REGION 'Init LSourceStr'}
      var LSourceStr := ALGetStringFromFile(ansiString(LFiles[i]));
      var LOriginalSourceStr := LSourceStr;
      {$ENDREGION}

      {$REGION 'skip unicode file'}
      if ALPosA(#0,LSourceStr) > 0 then begin
        Writeln('Skipped '+LFiles[i]);
        continue;
      end;
      {$ENDREGION}

      {$REGION 'check that the source file do not contain bad characters'}
      for var j := 0 to 31 do begin
        if J = 9 then continue; // tab
        if j = 10 then continue; // New line
        if j = 13 then continue; // carriage return
        if ALPosA(ansiString(Chr(j)),LSourceStr) > 0 then raise Exception.CreateFmt('%s contain a bad character (%d)', [LFiles[i], j]);
      end;
      {$ENDREGION}

      {$REGION 'replace #13 by #13#10'}
      LSourceStr := ALStringReplaceA(LSourceStr,#13#10,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplaceA(LSourceStr,#13,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplaceA(LSourceStr,#10,#1,[rfReplaceALL]);
      LSourceStr := ALStringReplaceA(LSourceStr,#1,#13#10,[rfReplaceALL]);
      {$ENDREGION}

      {$REGION 'replace <space>#13 by #13#10'}
      while ALPosA(' '#13#10,LSourceStr) > 0 do
        LSourceStr := ALStringReplaceA(LSourceStr,' '#13#10,#13#10,[rfReplaceALL]);
      {$ENDREGION}

      {$REGION 'Save the file'}
      if LOriginalSourceStr <> LSourceStr then begin
        if LCreateBackup then begin
          if Tfile.Exists(LFiles[i] + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [ansiString(LFiles[i]) + '.bak']);
          Tfile.Move(LFiles[i], LFiles[i] + '.bak');
        end;
        ALSaveStringToFile(LSourceStr,ansiString(LFiles[i]));
        Writeln('Updated '+ LFiles[i]);
      end;
      {$ENDREGION}

    end;

  except
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;

end.
