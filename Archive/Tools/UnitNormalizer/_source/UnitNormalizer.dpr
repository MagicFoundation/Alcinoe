program UnitNormalizer;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.ioutils,
  System.types,
  Alcinoe.StringList,
  Alcinoe.Files,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{*************************************************************}
Function _FormatFunctionCall(Const aSourceStr: String): String;
Begin

  //LNumberOfChars := aPaint.breakText(
  //                    LLine {text},
  //                    true {measureForwards},
  //                    LMaxWidth - LLineIndent, {maxWidth}
  //                    LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
  //                                                     // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
  //                                                     // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
  //                                                     //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
  //                                                     //   it's return for exemple 9 when height = 11

  var LSourceLst := TALStringListA.create;
  Try
    LSourceLst.text := LSourceStr;
    Var LUnclosedParenthese := 0;
    Var LAfterImplementation := False;
    var LinCurlyBrackets: Boolean := False; // {
    var LinAsteriskBracket: Boolean := False; // (*
    var LBeginLine: Integer := -1;
    var LEndLine: Integer := -1;
    var J := 1;
    While J <= LSourceLst.Count - 1 do begin
      Var LSourceLine := LSourceLst[J];
      if ALSameTextA(LSourceLine,'implementation') then LAfterImplementation := True;
      if not LAfterImplementation then begin
        inc(J);
        continue;
      end;
      var LInQuote: Boolean := False; // '
      var LInDoubleSlash: Boolean := False; // //
      var K: integer := Low(LSourceLine);
      While K < High(LSourceLine) do begin
        //-----
        var LCurrChar := LSourceLine[k];
        var LPrevChar := LSourceLine[k-1];
        inc(k);
        //-----
        if LInDoubleSlash then continue;
        //-----
        if (LinCurlyBrackets) and (LCurrChar <> '}') then continue;
        if (LinCurlyBrackets) and (LCurrChar = '}') then LinCurlyBrackets := False;
        //-----
        if (LinAsteriskBracket) and (LCurrChar <> ')') then continue;
        if (LinAsteriskBracket) and (LCurrChar = ')') and (LPrevChar = '*') then LinAsteriskBracket := false;
        //-----
        if (LCurrChar = '/') and (LPrevChar= '/') then begin
          LInDoubleSlash := true;
          continue;
        end;
        //-----
        if (LCurrChar = '{') then begin
          LinCurlyBrackets := true;
          continue;
        end;
        //-----
        if (LCurrChar = '*') and (LPrevChar= '(') then begin
          LinAsteriskBracket := true;
          inc(k);
          continue;
        end;
        //-----
        if LCurrChar = '''' then LInQuote := not LInQuote;
        if LInQuote then continue;
        //-----
        if LCurrChar = '(' then begin
          if LUnclosedParenthese = 0 then LBeginLine := K - 1;
          inc(LUnclosedParenthese);
          continue;
        end;
        if LCurrChar = ')' then begin
          if LUnclosedParenthese = 0 then raise Exception.Create('Error 0C208CA1-87C3-4972-83AF-61AC039927AD');
          dec(LUnclosedParenthese);
          if LUnclosedParenthese = 0 then LEndLine := K - 1;
          continue;
        end;
        //-----
      end;

      inc(j);
    end;
    LSourceStr := ALTrim(LSourceLst.text) + #13#10;
  Finally
    ALFreeAndNil(LSourceLst);
  End;
End;


begin

  try

    //Init project params
    {$IFDEF DEBUG}
    ReportMemoryleaksOnSHutdown := True;
    {$ENDIF}
    SetMultiByteConversionCodePage(CP_UTF8);

    var LProjectDirectory := ansiString(paramstr(1));
    var LCreateBackup := not ALSameTextA(ALTrim(ansiString(paramstr(2))), 'false');

    var LFiles := TDirectory.GetFiles(string(LProjectDirectory), '*.pas', TSearchOption.soAllDirectories);
    for var I := Low(LFiles) to High(LFiles) do begin

      {$REGION 'Init LSourceStr'}
      var LSourceStr := ALGetStringFromFile(ansiString(LFiles[i]));
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

      {$REGION 'Add *** on the top of procedure/function/contructor/destructor'}
      Var LSourceLst := TALStringListA.create;
      Try
        LSourceLst.text := LSourceStr;
        Var LAfterImplementation := False;
        var J := 1;
        While J <= LSourceLst.Count - 1 do begin
          Var LSourceLine := LSourceLst[J];
          if ALSameTextA(LSourceLine,'implementation') then LAfterImplementation := True;
          if not LAfterImplementation then begin
            inc(j);
            continue;
          end;
          if (ALPosIgnoreCaseA('Procedure ', LSourceLine) = 1) or
             (ALPosIgnoreCaseA('Function ', LSourceLine) = 1) or
             (ALPosIgnoreCaseA('Class Procedure ', LSourceLine) = 1) or
             (ALPosIgnoreCaseA('Class Function ', LSourceLine) = 1) or
             (ALPosIgnoreCaseA('Constructor ', LSourceLine) = 1) or
             (ALPosIgnoreCaseA('Destructor ', LSourceLine) = 1) then begin
            Var LPreviousSourceLine := LSourceLst[J - 1];
            if (LPreviousSourceLine = '') or
               (ALPosA('{*', LSourceLine) = 1) then begin
              var LNewPreviousSourceLine: AnsiString;
              setlength(LNewPreviousSourceLine, length(LSourceLine));
              FillChar(LNewPreviousSourceLine[low(LNewPreviousSourceLine)], length(LNewPreviousSourceLine), Ord('*'));
              LNewPreviousSourceLine[low(LNewPreviousSourceLine)] := '{';
              LNewPreviousSourceLine[high(LNewPreviousSourceLine)] := '}';
              if LPreviousSourceLine = '' then begin
                LSourceLst.Insert(J-1,'');
                inc(j);
              end;
              LSourceLst[J-1] := LNewPreviousSourceLine;
            end;
          end;
          inc(j);
        end;
        LSourceStr := ALTrim(LSourceLst.text) + #13#10;
      Finally
        ALFreeAndNil(LSourceLst);
      End;
      {$ENDREGION}

      {$REGION 'Add ~~~ on the top of procedure/function declared inside a procedure or function'}
      LSourceLst := TALStringListA.create;
      Try
        LSourceLst.text := LSourceStr;
        Var LAfterImplementation := False;
        var J := 1;
        While J <= LSourceLst.Count - 1 do begin
          Var LSourceLine := LSourceLst[J];
          if ALSameTextA(LSourceLine,'implementation') then LAfterImplementation := True;
          if not LAfterImplementation then begin
            inc(J);
            continue;
          end;
          if (ALPosIgnoreCaseA('Procedure ', LSourceLine) <> 1) and
             (ALPosIgnoreCaseA('Function ', LSourceLine) <> 1) and
             ((ALPosIgnoreCaseA('Procedure ', ALTrim(LSourceLine)) = 1) or
              (ALPosIgnoreCaseA('Function ', ALTrim(LSourceLine)) = 1)) then begin
            Var LPreviousSourceLine := LSourceLst[J - 1];
            if (LPreviousSourceLine = '') or
               (ALPosA('{~', ALTrim(LSourceLine)) = 1) then begin
              var LNewPreviousSourceLine: AnsiString;
              setlength(LNewPreviousSourceLine, length(LSourceLine));
              FillChar(LNewPreviousSourceLine[low(LNewPreviousSourceLine)], length(LNewPreviousSourceLine), Ord('~'));
              Var LLn := 0;
              for var K := low(LSourceLine) to High(LSourceLine) do
                if LSourceLine[k] = ' ' then inc(LLn)
                else break;
              FillChar(LNewPreviousSourceLine[low(LNewPreviousSourceLine)], LLn, Ord(' '));
              LNewPreviousSourceLine[LLn+1] := '{';
              LNewPreviousSourceLine[high(LNewPreviousSourceLine)] := '}';
              if LPreviousSourceLine = '' then begin
                LSourceLst.Insert(J-1,'');
                inc(j);
              end;
              LSourceLst[J-1] := LNewPreviousSourceLine;
            end;
          end;
          inc(j);
        end;
        LSourceStr := ALTrim(LSourceLst.text) + #13#10;
      Finally
        ALFreeAndNil(LSourceLst);
      End;
      {$ENDREGION}

      {$REGION 'format procedure/function call'}

      {$ENDREGION}

      {$REGION 'Save the file'}
      if LCreateBackup then begin
        if ALFileExists(ansiString(LFiles[i]) + '.bak') then raise Exception.CreateFmt('The backup file (%s) already exists!', [ansiString(LFiles[i]) + '.bak']);
        if not ALRenameFileA(ansiString(LFiles[i]), ansiString(LFiles[i]) + '.bak') then raiseLastOsError;
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
