program ALStringToAnsiString;

{$APPTYPE CONSOLE}

uses
  Windows,
  Masks,
  SysUtils,
  AlFiles,
  ALStringList,
  ALString;

{************************************************************}
function ALStringToAnsiString_UpdateFile(aBodyStr: AnsiString;
                                         aOldVarType,
                                         aNewVarType: AnsiString): ansiString;

var aLowerCaseBodyStr : ansiString;
    i, j: integer;

const aRestrictedsymbols = ['a'..'z', '0'..'9', '_'];

begin

  // set initial values
  aOldVarType := ALLowerCase(aOldVarType);
  aLowerCaseBodyStr := ALLowerCase(aBodyStr);
  Result := '';
  i := 1;

  //loop still the end of the file
  while i <= Length(aBodyStr) do begin

    if aLowerCaseBodyStr[i] = '''' then begin
      j := ALPosEx('''', aLowerCaseBodyStr, i + 1);
      if j <= 0 then raise Exception.Create('Wrong pas file');
      inc(j, 1);
      Result := Result + alcopyStr(aBodyStr, I, j-i);
      i := j;
      Continue;
    end
    else if ALPosEx('//', aLowerCaseBodyStr, i) = i then begin
      j := ALPosEx(#13#10, aLowerCaseBodyStr, i + 1);
      if j <= 0 then raise Exception.Create('Wrong pas file');
      inc(j, 2);
      Result := Result + alcopyStr(aBodyStr, I, j-i);
      i := j;
      Continue;
    end
    else if aLowerCaseBodyStr[i] = '{' then begin
      j := ALPosEx('}', aLowerCaseBodyStr, i + 1);
      if j <= 0 then raise Exception.Create('Wrong pas file');
      inc(j, 1);
      Result := Result + alcopyStr(aBodyStr, I, j-i);
      i := j;
      Continue;
    end
    else if ALPosEx('(*', aLowerCaseBodyStr, i) = i then begin
      j := ALPosEx('*)', aLowerCaseBodyStr, i + 1);
      if j <= 0 then raise Exception.Create('Wrong pas file');
      inc(j, 2);
      Result := Result + alcopyStr(aBodyStr, I, j-i);
      i := j;
      Continue;
    end
    else if (i > 1) and
            (i <= length(aLowerCaseBodyStr) - Length(aOldVarType)) and
            (aLowerCaseBodyStr[i] = aOldVarType[1]) and
            (aLowerCaseBodyStr[i+1] = aOldVarType[2]) and
            (aLowerCaseBodyStr[i+2] = aOldVarType[3]) and // "char" is the min for aOldVarType and contain only 4 char
            (ALPosEx(aOldVarType, aLowerCaseBodyStr, i) = i) and
            (not (aLowerCaseBodyStr[i - 1] in aRestrictedsymbols)) and
            (not (aLowerCaseBodyStr[i + Length(aOldVarType)] in aRestrictedsymbols)) then begin
      Result := Result + aNewVarType;
      i := i + Length(aOldVarType);
      Continue;
    end;

    Result := Result + aBodyStr[i];
    Inc(i);

  end;

end;

{***************************************************************}
Procedure ALStringToAnsiString_UpdateFiles(Directory: ansiString;
                                           SubDirectory: Boolean;
                                           BackupDir: AnsiString;
                                           FileNameMask: ansiString);

var aFileStr: AnsiString;
    aSR: TSearchRec;

begin
  Directory := ALIncludeTrailingPathDelimiter(Directory);
  BackupDir := ALIncludeTrailingPathDelimiter(BackupDir);
  if FindFirst(String(Directory) + '*', faAnyFile	, aSR) = 0 then begin
    Try
      repeat
        If (aSR.Name <> '.') and (aSR.Name <> '..') Then Begin
          If ((aSR.Attr and faDirectory) <> 0) then begin
            If SubDirectory then begin
              createDir(String(BackupDir) + aSR.Name);
              ALStringToAnsiString_UpdateFiles(Directory + AnsiString(aSR.Name),
                                               True,
                                               BackupDir + AnsiString(aSR.Name),
                                               fileNameMask);
            end;
          end
          else If ((FileNameMask = '*') or
                   ALMatchesMask(AnsiString(aSR.Name), FileNameMask)) then begin

            Writeln(Directory + AnsiString(aSR.Name));
            aFileStr := AlGetStringFromfile(Directory + AnsiString(aSR.Name));
            AlSaveStringTofile(aFileStr, BackupDir + AnsiString(aSR.Name));

            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'String', 'AnsiString');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Char', 'AnsiChar');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'PChar', 'PAnsiChar');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TStrings', 'TALStrings');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TStringList', 'TALStringList');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TStringStream', 'TALStringStream');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'CHR', 'AnsiChar');

            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'AnsiDequotedStr', 'ALDequotedStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'AnsiQuotedStr', 'ALQuotedStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'CompareStr', 'ALCompareStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'CompareText', 'ALCompareText');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Copy', 'ALCopyStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'CurrToStr', 'ALCurrToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'DateTimeToStr', 'ALDateTimeToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'DateToStr', 'ALDateToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractFileDir', 'ALExtractFileDir');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractFileDrive', 'ALExtractFileDrive');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractFileExt', 'ALExtractFileExt');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractFileName', 'ALExtractFileName');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractFilePath', 'ALExtractFilePath');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExtractQuotedStr', 'ALExtractQuotedStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'FloatToStr', 'ALFloatToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Format', 'ALFormat');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'GetLocaleFormatSettings', 'ALGetLocaleFormatSettings');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'IfThen', 'ALIfThen');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'IntToHex', 'ALIntToHex');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'IntToStr', 'ALIntToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'LastDelimiter', 'ALLastDelimiter');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'LowerCase', 'ALLowerCase');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Move', 'ALMove');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Pos', 'ALPos');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'PosEx', 'ALPosEx');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'QuotedStr', 'ALQuotedStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'SameStr', 'ALSameStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'SameText', 'ALSameText');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StringReplace', 'ALStringReplace');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToCurr', 'ALStrToCurr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToCurrDef', 'ALStrToCurrDef');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToDate', 'ALStrToDate');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToDateTime', 'ALStrToDateTime');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToFloat', 'ALStrToFloat');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToFloatDef', 'ALStrToFloatDef');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToInt', 'ALStrToInt');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToInt64', 'ALStrToInt64');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToIntDef', 'ALStrToIntDef');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToInt64Def', 'ALStrToInt64Def');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'StrToTime', 'ALStrToTime');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TimeToStr', 'ALTimeToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'Trim', 'ALTrim');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TrimLeft', 'ALTrimLeft');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TrimRight', 'ALTrimRight');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToCurr', 'ALTryStrToCurr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToDate', 'ALTryStrToDate');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToDateTime', 'ALTryStrToDateTime');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToFloat', 'ALTryStrToFloat');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToInt', 'ALTryStrToInt');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToInt64', 'ALTryStrToInt64');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TryStrToTime', 'ALTryStrToTime');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'UIntToStr', 'ALUIntToStr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'UpperCase', 'ALUpperCase');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'FormatDateTime', 'ALFormatDateTime');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'IsPathDelimiter', 'ALIsPathDelimiter');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'IncludeTrailingPathDelimiter', 'ALIncludeTrailingPathDelimiter');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ExcludeTrailingPathDelimiter', 'ALExcludeTrailingPathDelimiter');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'ALMakeGoodEndPath', 'ALIncludeTrailingPathDelimiter');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'MatchesMask', 'ALMatchesMask');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TFormatSettings', 'TALFormatSettings');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'FormatFloat', 'ALFormatFloat');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'FormatCurr', 'ALFormatCurr');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'TInifile', 'TALInifile');            
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'FileExists', 'ALFileExists');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'DirectoryExists', 'ALDirectoryExists');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'CreateDir', 'ALCreateDir');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'RemoveDir', 'ALRemoveDir');
                                   
            AlSaveStringTofile(aFileStr, Directory + AnsiString(aSR.Name));

          end;
        end;
      until FindNext(aSR) <> 0;
    finally
      FindClose(aSR);
    end;
  end

end;

{**********************************************************************************}
Function ALStringToAnsiString_ExtractParamValue(aParamName: ansiString): ansiString;
Var i: Integer;
    ACurrParamStr: ansiString;
Begin
  result := '';
  AParamName := AlLowerCase(AParamName) + ':';
  For i := 1 To paramCount do begin
    ACurrParamStr := AnsiString(paramstr(i));
    If AlPos(aParamName,AlLowerCase(ACurrParamStr)) = 1 then begin
      result := AlStringReplace(AlCopyStr(ACurrParamStr,
                                          AlPos(':', ACurrParamStr) + 1,
                                          maxint),
                                '"',
                                '',
                                [RfReplaceAll]);
      exit;
    end;
  end;
end;

var aDirectory: ansiString;
    aSubDirectory: boolean;
    aBackupDir: ansiString;
    aFileNameMask: ansiString;

begin

  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

  try

    aDirectory := ALStringToAnsiString_ExtractParamValue('Directory');
    aSubDirectory := ALStringToAnsiString_ExtractParamValue('SubDirectory') = '1';
    aFileNameMask := ALStringToAnsiString_ExtractParamValue('FileNameMask');
    aBackupDir := AlGetModulePath + 'backup_'+ALFormatDateTime('yyyy.mm.dd.hh.nn.ss', now, AlDefaultFormatSettings) +'\';
    createdir(String(aBackupDir));

    if (aDirectory = '') or
       (aFileNameMask = '') then raise Exception.Create('Wrong command line, it''s must look like ALStringToAnsiString.exe directory:c:\sample\ SubDirectory:1 FileNameMask:*.pas');
    ALStringToAnsiString_UpdateFiles(aDirectory,
                                     aSubDirectory,
                                     aBackupDir,
                                     aFileNameMask);
  except
    on E: Exception do
      Writeln(E.Message);
  end;
  
end.
