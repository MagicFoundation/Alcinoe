program PlatformApiInterfaceDiff;

{$APPTYPE CONSOLE}

{$R *.res}

{$I ..\..\..\Source\Alcinoe.inc}

uses
  System.SysUtils,
  System.IOUtils,
  system.AnsiStrings,
  system.Math,
  Alcinoe.FileUtils,
  Alcinoe.StringUtils,
  Alcinoe.Common,
  Alcinoe.StringList;

{************************************************************************************************************************************************}
procedure ComparePlatformApiSrc(var AUnitName: AnsiString; const AClassName, AOldSrc, ANewSrc: AnsiString; const AProjectfiles: TALNVStringListA);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Function NormalizeSrc(const ASrc: AnsiString): AnsiString;
  begin
    Result := ASrc;

    var P1 := alposA('(*', Result);
    while P1 > 0 do begin
      var P2 := alposA('*)', Result, P1);
      IF P2 <= 0 then raise Exception.Create('Error 5B3874CD-9637-4F51-A03A-DBFC9EC91C31');
      inc(P2, 2);
      delete(Result,P1,P2-P1);
      P1 := alposA('(*', Result);
    end;

    // To remove missing ) like in
    // function writeabilityHandler: void (^)(NSFileHandle ; cdecl;
    var LLst := TalStringListA.Create;
    try
      LLst.Text := result;
      For var I := LLst.Count - 1 downto 0 do
        if (alposA('//', ALTrim(LLst[i])) = 1) then LLst.Delete(i)
        else begin
          P1 := ALPosA('//', LLst[i]);
          if P1 > 0 then
            LLst[i] := AlTrim(ALCopyStr(LLst[i],1,P1-1));
        end;
      Result := LLst.Text;
    finally
      ALFreeAndNil(LLst);
    end;

    P1 := alposA('{$IF defined', Result); // {$IF defined(IOS)}
    while P1 > 0 do begin
      var P2 := alposA('{$', Result, P1+1); // {$ELSE // {$ENDIF
      IF P2 <= 0 then raise Exception.Create('Error 81BCD2FD-08E6-43FF-A9EB-36AEF3935200');
      delete(Result,P1,P2-P1);
      P1 := alposA('{$IF defined', Result);
    end;

    P1 := alposA('{', Result);
    while P1 > 0 do begin
      var P2 := alposA('}', Result, P1);
      IF P2 <= 0 then
        raise Exception.Create('Error E539070E-C4F0-41A3-934E-3F487F5073A4');
      inc(P2);
      delete(Result,P1,P2-P1);
      P1 := alposA('{', Result);
    end;

    P1 := alposA('(', Result);
    while P1 > 0 do begin
      inc(p1);
      while (P1 <= length(Result)) and (Result[P1] <> ')') do begin
        if (Result[P1] in [#13, #10]) then Result[P1] := ' ';
        inc(p1);
      end;
      P1 := alposA('(', Result, P1);
    end;

    if alposA(#0, Result) > 0 then raise Exception.Create('Error 693BFA2A-B389-4649-82EF-D61D06B92794');
    Result := ALStringReplaceA(Result, #13, #0, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #10, #0, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #0#0, #13#10, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #0, #13#10, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, ' or'#13#10, ' or ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #9, ' ', [rfReplaceALL]);
    While ALPosA('  ', Result) > 0 do
      Result := ALStringReplaceA(Result, '  ', ' ', [rfReplaceALL]);
    While ALPosA(#13#10' ', Result) > 0 do
      Result := ALStringReplaceA(Result, #13#10' ', #13#10, [rfReplaceALL]);
    While ALPosA(' '#13#10, Result) > 0 do
      Result := ALStringReplaceA(Result, ' '#13#10, #13#10, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10'external ', ' external ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10'cdecl;', ' cdecl;', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10': ', ': ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10'(', '(', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10']', ']', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, #13#10'of ', ' of ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, '; overload', ';', [rfReplaceALL]);
    While ALPosA(';;', Result) > 0 do
      Result := ALStringReplaceA(Result, ';;', ';', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, '>)'#13#10'end;', '>) end;', [rfReplaceALL]);

    P1 := alposA(#13#10'interface'#13#10, Result);
    IF P1 > 0 then delete(result, 1, P1 + length(#13#10'interface'#13#10) - 1);

    P1 := alposA(#13#10'uses'#13#10, Result);
    IF P1 > 0 then begin
      var P2 := alposA(';', Result, P1);
      IF P2 <= 0 then raise Exception.Create('Error 5B3874CD-9637-4F51-A03A-DBFC9EC91C31');
      delete(Result,P1,P2-P1+1);
    end
    else begin
      P1 := alposA(#13#10'uses ', Result);
      IF P1 > 0 then begin
        var P2 := alposA(';', Result, P1);
        IF P2 <= 0 then raise Exception.Create('Error 5B3874CD-9637-4F51-A03A-DBFC9EC91C31');
        delete(Result,P1,P2-P1+1);
      end;
    end;

    P1 := alposA(#13#10'implementation'#13#10, Result);
    IF P1 > 0 then delete(result, P1, maxint);

    LLst := TalStringListA.Create;
    try
      LLst.Text := result;
      For var I := LLst.Count - 1 downto 0 do
        if ALposA(' = interface;'#13#10, LLst[i]+#13#10) > 0 then
          LLst.Delete(i);
      Result := LLst.Text;
    finally
      ALFreeAndNil(LLst);
    end;

    LLst := TalStringListA.Create;
    try
      LLst.Text := result;
      For var I := LLst.Count - 1 downto 0 do begin
        var LLine := LLst[i];
        P1 := ALPosA('; cdecl;', LLst[i]);
        if P1 > 0 then delete(LLine, P1+1, Maxint)
        else begin
          P1 := ALPosA('; cdecl ', LLst[i]);
          if P1 > 0 then delete(LLine, P1+1, Maxint);
        end;
        LLst[i] := LLine;
      end;
      Result := LLst.Text;
    finally
      ALFreeAndNil(LLst);
    end;

    LLst := TalStringListA.Create;
    try
      LLst.Text := result;
      For var I := LLst.Count - 1 downto 0 do
        if (ALTrim(LLst[i]) = '') or
           (alposA('//', ALTrim(LLst[i])) = 1) or
           (alposA('[''', ALTrim(LLst[i])) = 1) or
           (alposA('>) end;'#13#10, ALTrim(LLst[i])+ #13#10) > 0) or
           (alposA('[MethodName(', ALTrim(LLst[i])) = 1) or
           (alsameTextA(LLst[i], 'const')) or
           (alsameTextA(LLst[i], 'type')) then LLst.Delete(i);
      Result := LLst.Text;
    finally
      ALFreeAndNil(LLst);
    end;

    LLst := TalStringListA.Create;
    try
      LLst.Text := result;
      For var I := LLst.Count - 1 downto 0 do
        LLst[i] := ALTrim(LLst[i]);
      Result := ALTrim(LLst.Text);
    finally
      ALFreeAndNil(LLst);
    end;

    Result := ALStringReplaceA(Result, ' ;'#13#10, ';'#13#10, [rfReplaceALL]);
    Result := ALStringReplaceA(Result, ' ; ', '; ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, ' )', ')', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, ' : ', ': ', [rfReplaceALL]);
    Result := ALStringReplaceA(Result, '='#13#10, '= ', [rfReplaceALL]);

    Result := #13#10+Result+#13#10;
    if (AClassName <> '') then begin
      P1 := ALPosIgnoreCaseA(#13#10'case Integer of'#13#10, Result);
      if P1 > 0 then
        delete(Result, P1, Maxint);
    end;

    Result := ALTrim(Result);

    {$IF defined(debug)}
    //if alposA('UIViewAnimationOptionLayoutSubviews',Result) > 0  then
    //  ALSaveStringTofile(Result, ALGetModulePathW + 'DEBUG_NormalizeSrc.pas');
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SameParams(const AOldLine, ANewLine: AnsiString): boolean;
  begin
    var P1 := ALPosA('(', AOldLine);
    var P2 := ALPosA('(', ANewLine);
    if (P1 <= 0) and (P2 <= 0) then exit(ALSameTextA(AOldLine, ANewLine))
    else if (P1 > 0) and (P2 > 0) then begin
      inc(P1);
      Inc(P2);
      var P3 := ALPosA(')', AOldLine);
      if P3 <= P1 then raise Exception.Create('Error D865E334-44DD-4C9D-A36E-E0D8CD33B89F');
      var P4 := ALPosA(')', ANewLine);
      if P4 <= P2 then raise Exception.Create('Error D865E334-44DD-4C9D-A36E-E0D8CD33B89F');
      Var LOldFunctionParamsLst := TalStringListA.Create;
      Var LNewFunctionParamsLst := TalStringListA.Create;
      try
        LOldFunctionParamsLst.NameValueSeparator := ':';
        LNewFunctionParamsLst.NameValueSeparator := ':';
        LOldFunctionParamsLst.LineBreak := ';';
        LNewFunctionParamsLst.LineBreak := ';';
        LOldFunctionParamsLst.Text := ALTrim(ALCopyStr(AOldLine, P1, P3-P1));
        LNewFunctionParamsLst.Text := ALTrim(ALCopyStr(ANewLine, P2, P4-P2));
        if LOldFunctionParamsLst.Count <> LNewFunctionParamsLst.Count then exit(False);
        for var I := 0 to LOldFunctionParamsLst.Count - 1 do
          if AlTrim(ALLowerCase(LOldFunctionParamsLst.ValueFromIndex[i])) <> AlTrim(Allowercase(LNewFunctionParamsLst.ValueFromIndex[i])) then
            Exit(False);
        var LOldLine := AOldLine;
        var LNewLine := ANewLine;
        delete(LOldLine, P1, P3-P1);
        delete(LNewLine, P2, P4-P2);
        Result := ALSameTextA(LOldLine, LNewLine);
      finally
        ALFreeAndNil(LOldFunctionParamsLst);
        ALFreeAndNil(LNewFunctionParamsLst);
      end;
    end
    else
      Result := False;
  end;

begin
  var LOldSrcLst := TALStringListA.Create;
  var LNewSrcLst := TALStringListA.Create;
  try

    Try

      LOldSrcLst.Text := NormalizeSrc(AOldSrc);
      LNewSrcLst.Text := NormalizeSrc(ANewSrc);
      var I: integer := -1;
      While I < LOldSrcLst.Count - 1 do begin
        inc(i);
        var LOldLine := LOldSrcLst[i];
        If LOldLine = '' then continue;

        If ((alposA(' = interface(', LOldLine) > 0) and (alposA(';', LOldLine) <= 0)) or
           ((alposA(' = record', LOldLine) > 0) and (alposA(';', LOldLine) <= 0)) then begin
          LOldSrcLst.NameValueSeparator := '=';
          LNewSrcLst.NameValueSeparator := '=';

          if AClassName <> '' then
            raise Exception.Create('Error C6021C63-503D-4642-9ACE-F97430EA67B1');
          var LCLassName := ALTrim(LOldSrcLst.Names[I]);

          var LtmpNewSrc: AnsiString := '';
          var J := LNewSrcLst.IndexOfName(LOldSrcLst.Names[I]);
          If (J >= 0) and (alposA(';', LNewSrcLst[J]) <= 0) then begin
            inc(j);
            if J >= LNewSrcLst.Count - 1 then raise Exception.Create('Error 589A03DB-B534-4481-916D-5C64BBA62150');
            var K := J;
            while LNewSrcLst[k] <> 'end;' do begin
              LtmpNewSrc := LtmpNewSrc + #13#10 + LNewSrcLst[k];
              inc(K);
              if K > LNewSrcLst.Count - 1 then
                raise Exception.Create('Error B2E85BE3-323D-46E0-9017-E0467B153A14');
            end;
          end;

          var LtmpOldSrc: AnsiString := '';
          inc(I);
          if I >= LOldSrcLst.Count - 1 then raise Exception.Create('Error 60CCF0C0-4396-4592-88D8-0E12E7CBDE1F');
          while LOldSrcLst[I] <> 'end;' do begin
            LtmpOldSrc := LtmpOldSrc + #13#10 + LOldSrcLst[I];
            inc(I);
            if I > LOldSrcLst.Count - 1 then
              raise Exception.Create('Error B1FBA781-B7D8-4717-93B2-801883D17E50');
          end;

          ComparePlatformApiSrc(AUnitName, LCLassName, LTmpOldSrc, LTmpNewSrc, AProjectfiles);
          continue;
        end;

        {$IF defined(DEBUG)}
        //if ALPosIgnoreCaseA('numberWithInt', LOldLine) > 0 then begin
        //  writeln;
        //end;
        {$ENDIF}

        if LNewSrcLst.IndexOf(LOldLine) >= 0 then continue;

        var LName: AnsiString := '';
        var LNewLine: AnsiString := '';
        var P1 := AlposA('=',LOldLine);
        var P2 := AlposA('(',LOldLine);
        var P3 := AlposA(':',LOldLine);


        if (P1 > 0) and (P1 = minIntValue([P1, if P2 <= 0 then Maxint else P2, if P3 <= 0 then Maxint else P3])) then begin // ABNewPersonViewController = interface(UIViewController)
          LOldSrcLst.NameValueSeparator := '=';
          LNewSrcLst.NameValueSeparator := '=';
          LName := LOldSrcLst.Names[I];
          var J := LNewSrcLst.IndexOfName(LName);
          If J >= 0 then LNewLine := LNewSrcLst[J];
        end
        else if (P2 > 0) and (P2 = minIntValue([P2, if P1 <= 0 then Maxint else P1, if P3 <= 0 then Maxint else P3])) then begin // procedure setAddressBook(addressBook: ABAddressBookRef); cdecl;
          LOldSrcLst.NameValueSeparator := '(';
          LNewSrcLst.NameValueSeparator := '(';
          LName := LOldSrcLst.Names[I];
          for var J := 0 to LNewSrcLst.Count - 1 do begin
            if ALSameTextA(LNewSrcLst.Names[J], LName) then begin
              LNewLine := LNewSrcLst[J];
              if (SameParams(LOldLine, LNewLine)) then break;
            end;
          end;
        end
        else if (P3 > 0) and (P3 = minIntValue([P3, if P2 <= 0 then Maxint else P2, if P1 <= 0 then Maxint else P1])) then begin // function parentGroup: ABRecordRef; cdecl;
          LOldSrcLst.NameValueSeparator := ':';
          LNewSrcLst.NameValueSeparator := ':';
          LName := LOldSrcLst.Names[I];
          var J := LNewSrcLst.IndexOfName(LName);
          If J >= 0 then LNewLine := LNewSrcLst[J];
        end
        else if AlposA(';',LOldLine) > 0 then begin // procedure beginInterruption; cdecl;
          LOldSrcLst.NameValueSeparator := ';';
          LNewSrcLst.NameValueSeparator := ';';
          LName := LOldSrcLst.Names[I];
          var J := LNewSrcLst.IndexOfName(LName);
          If J >= 0 then LNewLine := LNewSrcLst[J];
        end
        else
          raise Exception.Create('Error FC4E6E40-BBDD-4AB4-87A3-91BBD516DEA0 - ' + String(LOldLine));

        if (AlposA('(',LOldLine) > 0) and
           (SameParams(LOldLine, LNewLine)) then Continue;

        LName := ALLowerCase(LName);
        LName := ALStringReplaceA(LName, 'procedure ', '', [RFIgnoreCase]);
        LName := ALStringReplaceA(LName, 'function ', '', [RFIgnoreCase]);
        LName := ALStringReplaceA(LName, ' ', '', [RFIgnoreCase]);
        var LMatchedProjectFiles: AnsiString := '';
        for var J := 0 to AProjectFiles.Count - 1 do
          If alposA(LName, AProjectFiles.ValueFromIndex[J]) > 0 then
            LMatchedProjectFiles := LMatchedProjectFiles + ALIfThenA(LMatchedProjectFiles <> '', ', ') + AProjectFiles.names[J];
        if (AProjectfiles.Count > 0) and (LMatchedProjectFiles = '') then
          continue;

        if AUnitName <> '' then begin
          var Lstr := AUnitName;
          FillChar(Lstr[low(Lstr)], length(Lstr), ord('-'));
          ALWriteln(Lstr, TALConsoleColor.ccAqua);
          ALWriteln(AUnitName, TALConsoleColor.ccAqua);
          ALWriteln(Lstr, TALConsoleColor.ccAqua);
          Writeln;
          AUnitName := '';
        end;

        if (LOldLine <> '') and (LOldLine[high(LOldLine)] = ';')  then
          delete(LOldLine, high(LOldLine), 1);
        if (LNewLine <> '') and (LNewLine[high(LNewLine)] = ';')  then
          delete(LNewLine, high(LNewLine), 1);

        {$IF defined(debug)}
        //if LOldLine= 'procedure searchBarTextDidChange(searchBar: UISearchBar; textDidChange: NSString)' then begin
        //  Writeln;
        //end;
        {$ENDIF}

        if LNewLine = '' then LNewLine := '(deleted)';
        if ACLassName <> '' then ALWriteln('Classname: ' + ACLassName, TALConsoleColor.ccDarkYellow);
        Writeln('Old: ' + LOldLine);
        Writeln('New: ' + LNewLine);
        if LMatchedProjectFiles <> '' then
          ALWriteln('Found in: ' + LMatchedProjectFiles, TALConsoleColor.ccGrey);
        Writeln;

      end;

    Except
      On E: Exception do begin
        //ALSaveStringTofile(LOldSrcLst.Text, ALGetModulePathW + 'ERROR_old.pas');
        //ALSaveStringTofile(LNewSrcLst.Text, ALGetModulePathW + 'ERROR_new.pas');
        //Halt(1);
        Raise;
      end;
    End;

  finally
    ALFreeAndNil(LOldSrcLst);
    ALFreeAndNil(LNewSrcLst);
  end;
end;

{****************************************************************************************************************}
procedure ComparePlatformApiFile(const AOldFilename, ANewFilename: String; const AProjectfiles: TALNVStringListA);
begin
  var LUnitName := ALExtractFileName(AnsiString(AOldFilename));
  ComparePlatformApiSrc(LUnitName, ''{AClassName}, ALGetStringFromFile(AOldFilename), ALGetStringFromFile(ANewFilename), AProjectfiles);
end;

{***********************************************************************************************}
procedure ComparePlatformApiFiles(const AOldDirectory, ANewDirectory, AProjectDirectory: String);
begin
  var LProjectFiles := TALNVStringListA.Create;
  try

    if not TDirectory.Exists(AOldDirectory) then
      raise Exception.CreateFmt('OLD directory does not exist: %s', [AOldDirectory]);

    if not TDirectory.Exists(ANewDirectory) then
      raise Exception.CreateFmt('NEW directory does not exist: %s', [ANewDirectory]);

    if AProjectDirectory <> '' then begin
      var LProjectFilesArr := TDirectory.GetFiles(AProjectDirectory, '*.pas', TSearchOption.soAllDirectories);
      for var LProjectFile in LProjectFilesArr do
        LProjectFiles.AddNameValue(ansiString(ALExtractFileName(LProjectFile)), ALLowerCase(ALGetStringFromFile(LProjectFile)));
      LProjectFilesArr := TDirectory.GetFiles(AProjectDirectory, '*.dpr', TSearchOption.soAllDirectories);
      for var LProjectFile in LProjectFilesArr do
        LProjectFiles.AddNameValue(ansiString(ALExtractFileName(LProjectFile)), ALLowerCase(ALGetStringFromFile(LProjectFile)));
    end;

    {$IF defined(DEBUG)}
    //ComparePlatformApiFile(AOldDirectory + 'iOSapi.UIKit.pas', ANewDirectory + 'iOSapi.UIKit.pas', LProjectfiles);
    //Exit;
    {$ENDIF}
    var LOldFiles := TDirectory.GetFiles(AOldDirectory, '*.pas', TSearchOption.soTopDirectoryOnly);
    if Length(LOldFiles) = 0 then begin
      Writeln(Format('No files matching "%s" in %s', ['*.pas', AOldDirectory]));
      Exit;
    end;
    for var LOldFile in LOldFiles do begin
      var LBaseName := TPath.GetFileName(LOldFile);
      var LNewFile  := TPath.Combine(ANewDirectory, LBaseName);
      if TFile.Exists(LNewFile) then ComparePlatformApiFile(LOldFile, LNewFile, LProjectfiles)
      else Writeln(Format('Skipped (missing in NEW): %s', [LBaseName]));
    end;

  finally
    ALFreeAndNil(LProjectFiles);
  end;
end;

begin
  Try

    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    Writeln('This app will compare the *.pas interface units of a NEW Delphi version');
    Writeln('with a PREVIOUS one and report breaking changes (e.g., Pointer <> typed object)');
    Writeln;

    Writeln('Select target platform: [I] iOSApi  [M] Macapi');
    Writeln('Default: I');
    Write('>');
    var LPlatformApi: String;
    Readln(LPlatformApi);

    Writeln;

    {$IFNDEF ALCompilerVersionSupported130}
      {$MESSAGE WARN 'Update the paths below and adjust the IFDEF'}
      {$MESSAGE WARN 'Also run PlatformApiInterfaceDiff to detect changes in iOSApi/Macapi and adjust the IFDEF'}
    {$ENDIF}

    Writeln('Please enter the directory of the OLD ' + ALIfthenA(ALSameTextW(LPlatformApi, 'M'), 'Macapi', 'iOSApi') +'*.pas');
    if ALSameTextW(LPlatformApi, 'M') then Writeln('Default: c:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl\osx\')
    else Writeln('Default: c:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl\ios\');
    Write('>');
    var LOldPlatformApiDir: String;
    Readln(LOldPlatformApiDir);
    if LOldPlatformApiDir = '' then begin
      if ALSameTextW(LPlatformApi, 'M') then LOldPlatformApiDir := 'c:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl\osx\'
      else LOldPlatformApiDir := 'c:\Program Files (x86)\Embarcadero\Studio\23.0\source\rtl\ios\';
    end;
    Writeln;

    Writeln('Please enter the directory of the NEW ' + ALIfthenA(ALSameTextW(LPlatformApi, 'M'), 'Macapi', 'iOSApi') +'*.pas');
    if ALSameTextW(LPlatformApi, 'M') then Writeln('Default: c:\Program Files (x86)\Embarcadero\Studio\37.0\source\rtl\osx\')
    else Writeln('Default: c:\Program Files (x86)\Embarcadero\Studio\37.0\source\rtl\ios\');
    Write('>');
    var LNewPlatformApiDir: String;
    Readln(LNewPlatformApiDir);
    if LNewPlatformApiDir = '' then begin
      if ALSameTextW(LPlatformApi, 'M') then LNewPlatformApiDir := 'c:\Program Files (x86)\Embarcadero\Studio\37.0\source\rtl\osx\'
      else LNewPlatformApiDir := 'c:\Program Files (x86)\Embarcadero\Studio\37.0\source\rtl\ios\';
    end;
    Writeln;

    Writeln('Do you want to detect breaking changes in the');
    Writeln('Alcinoe source code located in ..\..\Source (y/n)?');
    Writeln('Default: n');
    Write('>');
    var LProjectDir: String;
    Readln(LProjectDir);
    If ALSameTextW(LProjectDir, 'Y') then LProjectDir := ALGetModulePathW + '..\..\Source'
    else LProjectDir := '';
    Writeln;

    If LProjectDir = '' then begin
      Writeln('To detect units in your project impacted by a breaking change,');
      Writeln('please enter your PROJECT SOURCE directory (leave empty to skip)');
      Write('>');
      Readln(LProjectDir);
      Writeln;
    end;

    ComparePlatformApiFiles(AlTrim(LOldPlatformApiDir), ALTrim(LNewPlatformApiDir), ALTrim(LProjectDir));

    Writeln('');
    Writeln('Finished');
    Writeln('Press <Enter> key to quit');
    Readln;

  except
    on E: Exception do begin
      ALWriteln(E.ClassName+': '+E.Message, TALConsoleColor.ccRed);
      Writeln;
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;
end.