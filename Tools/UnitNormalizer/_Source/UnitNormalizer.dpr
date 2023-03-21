program UnitNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ioutils,
  System.types,
  System.AnsiStrings,
  System.Math,
  System.Character,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{*******************************************************************}
//we need this function for debuging because their is a bug in delphi
//that make we can not debug inlined var when they are inside the
//begin ... end of the dpr
procedure Kickoff;
begin

  try

    //Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    {$REGION 'Init local vars'}
    var LNoInteraction: Boolean;
    {$ENDREGION}

    {$REGION 'create local objects'}
    var LParamLst := TALStringListW.Create;
    var LFilesToIgnore := TALStringListW.Create;
    {$ENDREGION}

    try

      {$REGION 'Init LParamLst'}
      for var I := 1 to ParamCount do
        LParamLst.Add(ParamStr(i));
      {$ENDREGION}

      {$REGION 'Init LNoInteraction'}
      LNoInteraction := AlStrToBool(ALTrim(LParamLst.Values['-NoInteraction']));
      {$ENDREGION}

      {$REGION 'Init LRootDirectory'}
      var LRootDirectory := ALTrim(LParamLst.Values['-Dir']);
      if LRootDirectory = '' then raise Exception.Create('Dir param is mandatory');
      {$ENDREGION}

      {$REGION 'Init LCreateBackup'}
      var LCreateBackup := not ALSameTextW(ALTrim(LParamLst.Values['-CreateBackup']), 'false');
      {$ENDREGION}

      {$REGION 'Init LFilesToIgnore'}
      LFilesToIgnore.LineBreak := ';';
      LFilesToIgnore.CaseSensitive := False;
      LFilesToIgnore.Sorted := true;
      LFilesToIgnore.Text := LParamLst.Values['-FilesToIgnore'];
      {$ENDREGION}

      //loop on all *.pas in LRootDirectory
      var LPasFiles := TDirectory.GetFiles(string(LRootDirectory), '*.pas', TSearchOption.soAllDirectories);
      var LDprFiles := TDirectory.GetFiles(string(LRootDirectory), '*.dpr', TSearchOption.soAllDirectories);
      var LFiles := LPasFiles;
      setlength(LFiles, length(LPasFiles) + length(LDprFiles));
      for var I := Low(LDprFiles) to High(LDprFiles) do
        LFiles[length(LPasFiles)+I] := LDprFiles[I];

      for var Lfile in LFiles do begin

        {$REGION 'skip if Lfile in LFilesToIgnore'}
        if (LFilesToIgnore.IndexOf(Lfile) >= 0) or
           (LFilesToIgnore.IndexOf(AlExtractFileName(Lfile)) >= 0) then begin
          Writeln('Skipped '+LFile+ ' (FilesToIgnore)');
          continue;
        end;
        {$ENDREGION}

        {$REGION 'Init LSourceStr'}
        var LSourceStr := ALGetStringFromFile(LFile);
        var LOriginalSourceStr := LSourceStr;
        {$ENDREGION}

        {$REGION 'skip unicode file'}
        if ALPosA(#0,LSourceStr) > 0 then begin
          Writeln('Skipped '+LFile+ ' (Unicode file)');
          continue;
        end;
        {$ENDREGION}

        {$REGION 'check that the source file do not contain bad characters'}
        for var j := 0 to 31 do begin
          if J = 9 then continue; // tab
          if j = 10 then continue; // New line
          if j = 13 then continue; // carriage return
          if ALPosA(ansiString(Chr(j)),LSourceStr) > 0 then raise Exception.CreateFmt('%s contain a bad character (%d)', [LFile, j]);
        end;
        {$ENDREGION}

        {$REGION 'replace #13 by #13#10'}
        LSourceStr := ALStringReplaceA(LSourceStr,#13#10,#1,[rfReplaceALL]);
        LSourceStr := ALStringReplaceA(LSourceStr,#13,#1,[rfReplaceALL]);
        LSourceStr := ALStringReplaceA(LSourceStr,#10,#1,[rfReplaceALL]);
        LSourceStr := ALStringReplaceA(LSourceStr,#1,#13#10,[rfReplaceALL]);
        {$ENDREGION}

        {$REGION 'replace #9 by <space><space> and <space>#13 by #13#10'}
        var LLst := TalStringListA.Create;
        try
          LLst.Text := LSourceStr;
          for var I := 0 to LLst.Count - 1 do begin
            var LLine := LLst[i];
            LLine := Trimright(LLine);
            Var J := Low(LLine);
            While J <= High(LLine) do begin
              if LLine[j] = #9 then begin
                LLine[J] := ' ';
                Insert(' ', LLine,J);
                inc(j);
              end;
              if LLine[j] <> ' ' then break;
              inc(J);
            end;
            LLst[i] := LLine;
          end;
          LSourceStr := ALTrimRight(LLst.Text) + #13#10;
        finally
          ALFreeAndNil(LLst);
        end;
        {$ENDREGION}

        {$REGION 'Normalize function parameters'}
        //procedure ALWinExecA(const aUserName: ANSIString;
        //                     const aPassword: ANSIString;
        //                     const aCommandLine: ANSIString;
        //                     const aCurrentDirectory: AnsiString;
        //                     const aLogonFlags: dword = 0);
        // =>
        //procedure ALWinExecA(
        //            const aUserName: ANSIString;
        //            const aPassword: ANSIString;
        //            const aCommandLine: ANSIString;
        //            const aCurrentDirectory: AnsiString;
        //            const aLogonFlags: dword = 0);
        var LInDoubleSlashComment: boolean := False; //
        var LInCurlyBracketComment: boolean := False; {}
        var LInRoundBracketComment: boolean := False; (* *)
        var LInSimpleQuote: boolean := False; // '
        var LInRoundBracket: integer := 0;
        var LRoundBracketStartAt: integer := 0;
        Var LSourceStrIdx: integer := 0;
        while LSourceStrIdx <= High(LSourceStr) do begin

          //init LPrevChar/LCurrChar
          inc(LSourceStrIdx);
          var LPrevChar: ansiChar := #0;
          if (LSourceStrIdx > low(LSourceStr)) then LPrevChar := LSourceStr[LSourceStrIdx-1];
          var LCurrChar := LSourceStr[LSourceStrIdx];
          var LNextChar: ansiChar := #0;
          if (LSourceStrIdx < high(LSourceStr)) then LNextChar := LSourceStr[LSourceStrIdx+1];

          //#13,#10
          if LCurrChar in [#13,#10] then begin
            LInDoubleSlashComment := False;
            LInSimpleQuote := False;
            continue;
          end;

          ////
          if (LPrevChar = '/') and (LCurrChar = '/') and
             (not LInDoubleSlashComment) and
             (not LInCurlyBracketComment) and
             (not LInRoundBracketComment) and
             (not LInSimpleQuote) then begin
            LInDoubleSlashComment := true;
            continue;
          end;

          //{
          if (LCurrChar = '{') and
             (not LInDoubleSlashComment) and
             (not LInCurlyBracketComment) and
             (not LInRoundBracketComment) and
             (not LInSimpleQuote) then begin
            LInCurlyBracketComment := true;
            continue;
          end;

          //}
          if (LCurrChar = '}') and
             (LInCurlyBracketComment) then begin
            LInCurlyBracketComment := False;
            continue;
          end;

          //(*
          if (LcurrChar = '(') and (LNextChar = '*') and
             (not LInDoubleSlashComment) and
             (not LInCurlyBracketComment) and
             (not LInRoundBracketComment) and
             (not LInSimpleQuote) then begin
            LInRoundBracketComment := true;
            continue;
          end;

          //*)
          if (LPrevChar = '*') and (LCurrChar = ')') and
             (LInRoundBracketComment) then begin
            LInRoundBracketComment := false;
            continue;
          end;

          //'
          if (LCurrChar = '''') and
             (not LInDoubleSlashComment) and
             (not LInCurlyBracketComment) and
             (not LInRoundBracketComment) then begin
            LInSimpleQuote := not LInSimpleQuote;
            continue;
          end;

          //we are in comment/quote
          if (LInDoubleSlashComment) or
             (LInCurlyBracketComment) or
             (LInRoundBracketComment) or
             (LInSimpleQuote) then continue;

          //(
          if (LcurrChar = '(') then begin
            if LInRoundBracket = 0 then LRoundBracketStartAt := LSourceStrIdx;
            inc(LInRoundBracket);
            continue;
          end;

          //)
          if (LcurrChar = ')') and (LInRoundBracket > 0) then begin
            dec(LInRoundBracket);
            if LInRoundBracket = 0 then begin
              var P2 := LSourceStrIdx;
              var P1 := LRoundBracketStartAt-1;
              var LInLessAndGreaterThanSign: integer := 0;
              while P1 > 0 do begin
                if (char(LSourceStr[P1]).IsLetterOrDigit) or
                   (LSourceStr[P1] = '_') or
                   ((LSourceStr[P1] = '.') and (P1 < LRoundBracketStartAt-1)) or
                   ((LSourceStr[P1] = '(') and (P1 < LRoundBracketStartAt-1)) or
                   ((LSourceStr[P1] = ')') and (P1 < LRoundBracketStartAt-1)) or
                   ((LSourceStr[P1] = '[') and (P1 < LRoundBracketStartAt-1)) or
                   ((LSourceStr[P1] = ']') and (P1 < LRoundBracketStartAt-1)) then dec(P1)
                else if (LSourceStr[P1] = '>') and (P1 < LRoundBracketStartAt-1) then begin
                  inc(LInLessAndGreaterThanSign);
                  dec(P1);
                end
                else if (LSourceStr[P1] = '<') and (P1 < LRoundBracketStartAt-1) then begin
                  dec(LInLessAndGreaterThanSign);
                  dec(P1);
                end
                else if (LSourceStr[P1] in [',' ,' ', #9]) and (LInLessAndGreaterThanSign > 0) then dec(P1)
                else break;
              end;
              inc(P1);
              while (P1 < LRoundBracketStartAt) and (LSourceStr[P1] in ['.', '(', ')', '[', ']', '>', '<']) do
                inc(P1);
              var P0 := P1;
              while (P0 > 0) and (LSourceStr[P0] <> #10) do dec(P0);
              inc(P0);
              if (P1 <> LRoundBracketStartAt) then begin
                var LStr := AlcopyStr(LSourceStr, P0, P2-P0+1);
                var LOriginalStr := Lstr;
                var LCRPos := AlposA(#13, LStr);
                //
                //<P0                              <P1                <LRoundBracketStartAt      <LCRPos
                //____if_(result=0)_then_Result_:=_List.CompareStrings(List.FList[Index1].FValue,
                //___________________________________^LTargetIndent____List.FList[Index2].FValue)
                //                                                                              ^P2
                if (LCRPos > 0) and
                   (AlPosIgnoreCaseA('[TWin_MongoDBFilerFieldAttributes(', LStr) <= 0) and
                   (AlposIgnoreCaseA('TShaderManager.RegisterShaderFromData', LStr) <= 0) and
                   (AlposIgnoreCaseA('TFilterRec.Create', LStr) <= 0) then begin
                  //
                  //get the LMinLineIndent that we will use as reference:
                  //
                  //ParseOPREPLYMessage(SendCmd(aSocketDescriptor,
                  //____________________________BuildOPGETMOREMessage(0, //const requestID: integer;
                  //__________________________________________________0, //const responseTo: integer;
                  //__________________________________________________fullCollectionName,
                  //__________________________________________________numberToReturn,
                  //__________________________________________________cursorID),
                  //____________________________True), // aGetResponse
                  //____________________LRequestID,
                  //____________________LResponseTo,
                  //____________________responseFlags);
                  //
                  //======>
                  //
                  //LMinLineIndent = ____________________
                  //
                  var LMinLineIndent: integer := MaxInt;
                  LLst := TALStringListA.Create;
                  Try
                    LLst.Text := LStr;
                    for var J := 1 to LLst.Count - 1 do begin
                      var LLine := LLst[J];
                      if ALTrimLeft(LLine) = '' then continue;
                      LMinLineIndent := min(LMinLineIndent, length(LLine) - length(ALTrimLeft(LLine)));
                    end;
                  Finally
                    ALFreeAndNil(LLst);
                  End;
                  //---
                  //break the very first line in 2 line:
                  //
                  //ParseOPREPLYMessage(SendCmd(aSocketDescriptor,
                  //____________________________BuildOPGETMOREMessage(0, //const requestID: integer;
                  //__________________________________________________0, //const responseTo: integer;
                  //__________________________________________________fullCollectionName,
                  //__________________________________________________numberToReturn,
                  //__________________________________________________cursorID),
                  //____________________________True), // aGetResponse
                  //____________________LRequestID,
                  //____________________LResponseTo,
                  //____________________responseFlags);
                  //
                  //======>
                  //
                  //ParseOPREPLYMessage(
                  //____________________SendCmd(aSocketDescriptor,
                  //____________________________BuildOPGETMOREMessage(0, //const requestID: integer;
                  //__________________________________________________0, //const responseTo: integer;
                  //__________________________________________________fullCollectionName,
                  //__________________________________________________numberToReturn,
                  //__________________________________________________cursorID),
                  //____________________________True), // aGetResponse
                  //____________________LRequestID,
                  //____________________LResponseTo,
                  //____________________responseFlags);
                  //
                  if (LCRPos > LRoundBracketStartAt - P0 + 2) and
                     ((AlPosIgnoreCaseA('TThread.Queue(nil,', LStr) <= 0) or (AlPosIgnoreCaseA('TThread.Queue(nil,', LStr) <> LRoundBracketStartAt - P0 + 1 - length('TThread.Queue'))) and
                     ((AlPosIgnoreCaseA('TThread.Synchronize(nil,', LStr) <= 0) or (AlPosIgnoreCaseA('TThread.Synchronize(nil,', LStr) <> LRoundBracketStartAt - P0 + 1 - length('TThread.Synchronize'))) and
                     ((AlPosIgnoreCaseA('TThread.ForceQueue(nil,', LStr) <= 0) or (AlPosIgnoreCaseA('TThread.ForceQueue(nil,', LStr) <> LRoundBracketStartAt - P0 + 1 - length('TThread.ForceQueue'))) and
                     ((AlPosIgnoreCaseA('MaxValue([', LStr) <= 0) or (AlPosIgnoreCaseA('MaxValue([', LStr) <> LRoundBracketStartAt - P0 + 1 - length('MaxValue'))) and
                     ((AlPosIgnoreCaseA('MinValue([', LStr) <= 0) or (AlPosIgnoreCaseA('MinValue([', LStr) <> LRoundBracketStartAt - P0 + 1 - length('MinValue'))) then begin
                     var LIndentStr: AnsiString := '';
                     for var J := 1 to LMinLineIndent do
                       LIndentStr := LIndentStr + ' ';
                     Insert(#13#10 + LIndentStr, LStr, LRoundBracketStartAt - P0 + 2);
                  end;
                  //---
                  //add/remove indentation
                  //
                  //ParseOPREPLYMessage(
                  //____________________SendCmd(aSocketDescriptor,
                  //____________________________BuildOPGETMOREMessage(0, //const requestID: integer;
                  //__________________________________________________0, //const responseTo: integer;
                  //__________________________________________________fullCollectionName,
                  //__________________________________________________numberToReturn,
                  //__________________________________________________cursorID),
                  //____________________________True), // aGetResponse
                  //____________________LRequestID,
                  //____________________LResponseTo,
                  //____________________responseFlags);
                  //
                  //======>
                  //
                  //ParseOPREPLYMessage(
                  //__SendCmd(aSocketDescriptor,
                  //__________BuildOPGETMOREMessage(0, //const requestID: integer;
                  //______________0, //const responseTo: integer;
                  //______________fullCollectionName,
                  //______________numberToReturn,
                  //______________cursorID),
                  //__________True), // aGetResponse
                  //__LRequestID,
                  //__LResponseTo,
                  //__responseFlags);
                  //
                  LLst := TALStringListA.Create;
                  Try
                    LLst.Text := LStr;
                    if LLst.Count <= 1 then raise Exception.Create('Error 8BF76840-31E0-4883-B752-3E75A7114CE4');
                    var LLine1 := LLst[1];
                    var LTargetIndent := P1-P0+2;
                    for var J := 1 to LLst.Count - 1 do begin
                      var LLine := LLst[J];
                      if ALTrim(LLine) = '' then continue;
                      Var LLineIndent := length(LLine) - length(ALTrimLeft(LLine));
                      var LDeltaIndent := LTargetIndent - LMinLineIndent;
                      while LDeltaIndent < 0 do begin
                        if LLine = '' then raise Exception.Create('Error E1A289AC-13F2-4FE1-AB10-0DC9091636A5');
                        if LLine[1] <> ' ' then break;
                        delete(LLine, 1, 1);
                        inc(LDeltaIndent);
                      end;
                      while LDeltaIndent > 0 do begin
                        LLine := ' ' + LLine;
                        dec(LDeltaIndent);
                      end;
                      LLst[J] := ALTrimRight(LLine);
                    end;
                    Lstr := ALTrimRight(LLst.Text);
                  Finally
                    ALFreeAndNil(LLst);
                  End;
                  if LOriginalStr <> Lstr then begin
                    delete(LSourceStr, P0, P2-P0+1);
                    insert(Lstr, LSourceStr, P0);
                    LSourceStrIdx := LRoundBracketStartAt;
                  end
                  else LSourceStrIdx := LRoundBracketStartAt;
                end;
              end
              else LSourceStrIdx := LRoundBracketStartAt;
            end;
            continue;
          end;

        end;
        {$ENDREGION}

        {$REGION 'Add *** on the top of procedure/function/contructor/destructor'}
        Var LSourceLst := TALStringListA.create;
        Try
          LSourceLst.text := LSourceStr;
          Var LAfterImplementation := False;
          var i := 1;
          While i <= LSourceLst.Count - 1 do begin
            Var LSourceLine := LSourceLst[i];
            if ALSameTextA(ALTrim(LSourceLine),'implementation') then LAfterImplementation := True;
            if (ALPosIgnoreCaseA('Procedure ', LSourceLine) = 1) or
               (ALPosIgnoreCaseA('Function ', LSourceLine) = 1) or
               (ALPosIgnoreCaseA('Class Procedure ', LSourceLine) = 1) or
               (ALPosIgnoreCaseA('Class Function ', LSourceLine) = 1) or
               (ALPosIgnoreCaseA('Constructor ', LSourceLine) = 1) or
               (ALPosIgnoreCaseA('Destructor ', LSourceLine) = 1) then begin
              Var LPrevSourceLine := LSourceLst[I - 1];
              if ((LPrevSourceLine = '') and (LAfterImplementation)) or
                 (ALPosA('{*', LPrevSourceLine) = 1) or
                 (ALPosA('{~', ALTrim(LPrevSourceLine)) = 1) or
                 (ALPosA('{-', ALTrim(LPrevSourceLine)) = 1) then begin
                var LNewPrevSourceLine: AnsiString;
                setlength(LNewPrevSourceLine, length(LSourceLine));
                FillChar(LNewPrevSourceLine[low(LNewPrevSourceLine)], length(LNewPrevSourceLine), Ord('*'));
                LNewPrevSourceLine[low(LNewPrevSourceLine)] := '{';
                LNewPrevSourceLine[high(LNewPrevSourceLine)] := '}';
                if LPrevSourceLine = '' then begin
                  LSourceLst.Insert(i-1,'');
                  inc(i);
                end;
                LSourceLst[i-1] := LNewPrevSourceLine;
              end;
            end
            else if (ALPosIgnoreCaseA('Procedure ', ALTrim(LSourceLine)) = 1) or
                    (ALPosIgnoreCaseA('Function ', ALTrim(LSourceLine)) = 1) then begin
              Var LPrevSourceLine := LSourceLst[I - 1];
              if ((LPrevSourceLine = '') and (LAfterImplementation)) or
                 (ALPosA('{*', ALTrim(LPrevSourceLine)) = 1) or
                 (ALPosA('{~', ALTrim(LPrevSourceLine)) = 1) or
                 (ALPosA('{-', ALTrim(LPrevSourceLine)) = 1) then begin
                var LNewPrevSourceLine: AnsiString;
                setlength(LNewPrevSourceLine, length(ALTrim(LSourceLine)));
                FillChar(LNewPrevSourceLine[low(LNewPrevSourceLine)], length(LNewPrevSourceLine), Ord('~'));
                LNewPrevSourceLine[low(LNewPrevSourceLine)] := '{';
                LNewPrevSourceLine[high(LNewPrevSourceLine)] := '}';
                while length(LNewPrevSourceLine) < length(LSourceLine) do
                  LNewPrevSourceLine := ' ' + LNewPrevSourceLine;
                if LPrevSourceLine = '' then begin
                  LSourceLst.Insert(i-1,'');
                  inc(i);
                end;
                LSourceLst[i-1] := LNewPrevSourceLine;
              end;
            end;
            inc(i);
          end;
          LSourceStr := ALTrim(LSourceLst.text) + #13#10;
        Finally
          ALFreeAndNil(LSourceLst);
        End;
        {$ENDREGION}

        {$REGION 'Save the file'}
        if LOriginalSourceStr <> LSourceStr then begin
          if LCreateBackup then begin
            if Tfile.Exists(LFile + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [ansiString(LFile) + '.bak']);
            Tfile.Move(LFile, LFile + '.bak');
          end;
          ALSaveStringToFile(LSourceStr,LFile);
          Writeln('Updated '+ LFile);
        end;
        {$ENDREGION}

      end;

    finally

      {$REGION 'Free local objects'}
      ALFreeAndNil(LParamLst);
      ALFreeandNil(LFilesToIgnore);
      {$ENDREGION}

    end;

    if not LNoInteraction then begin
      Writeln('');
      Writeln('Finished');
      Writeln('Press <Enter> key to quit');
      Readln;
    end;

  except
    on E: Exception do begin
      ALWriteln(E.ClassName+': '+E.Message, TALConsoleColor.ccRed);
      Writeln('');
      Writeln('Usage:');
      Writeln('  UnitNormalizer.exe');
      Writeln('    -Dir=The directory where all source files are located.');
      Writeln('    -FilesToIgnore=The list of filenames to ignore. Separate filename with '';''.');
      Writeln('    -CreateBackup=true or false.');
      Writeln('    -NoInteraction=Non-interactive mode.');
      Writeln('');
      Writeln('Example:');
      Writeln('  UnitNormalizer.exe^');
      Writeln('    -Dir="c:\MyProject\"^');
      Writeln('    -CreateBackup=false');
      Writeln('');
      Writeln('');
      Writeln('UnitNormalizer failed!');
      Writeln('Press <Enter> key to quit');
      Readln;
      halt(1);
    end;
  end;

end;


begin
  kickoff;
end.
