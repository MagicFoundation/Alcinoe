program ALStringToAnsiString;

{$APPTYPE CONSOLE}

uses
  Masks,
  SysUtils,
  AlFcnFile,
  ALStringList,
  ALFcnString;

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
            (aLowerCaseBodyStr[i+2] = aOldVarType[3]) and
            (aLowerCaseBodyStr[i+3] = aOldVarType[4]) and  // "char" is the min for aOldVarType and contain only 4 char             
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
  Directory := ALMakeGoodEndPath(Directory);
  BackupDir := ALMakeGoodEndPath(BackupDir);
  if FindFirst(Directory + '*', faAnyFile	, aSR) = 0 then begin
    Try
      repeat
        If (aSR.Name <> '.') and (aSR.Name <> '..') Then Begin
          If ((aSR.Attr and faDirectory) <> 0) then begin
            If SubDirectory then begin
              createDir(BackupDir + aSR.Name);
              ALStringToAnsiString_UpdateFiles(Directory + aSR.Name,
                                                  True,
                                                  BackupDir + aSR.Name,
                                                  fileNameMask);
            end;
          end
          else If ((FileNameMask = '*') or
                   MatchesMask(aSR.Name, FileNameMask)) then begin

            Writeln(Directory + aSR.Name);
            aFileStr := AlGetStringFromfile(Directory + aSR.Name);
            AlSaveStringTofile(aFileStr, BackupDir + aSR.Name);

            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'string', 'AnsiString');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'char',   'AnsiChar');
            aFileStr := ALStringToAnsiString_UpdateFile(aFileStr, 'PChar',  'PAnsiChar');

            AlSaveStringTofile(aFileStr, Directory + aSR.Name);

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
    ACurrParamStr := paramstr(i);
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

  try

    aDirectory := ALStringToAnsiString_ExtractParamValue('Directory');
    aSubDirectory := ALStringToAnsiString_ExtractParamValue('SubDirectory') = '1';
    aFileNameMask := ALStringToAnsiString_ExtractParamValue('FileNameMask');
    aBackupDir := AlGetModulePath + 'backup_'+FormatDateTime('yyyy.mm.dd.hh.nn.ss', now) +'\';
    createdir(aBackupDir);

    if (aDirectory = '') or
       (aFileNameMask = '')  then raise Exception.Create('Wrong command line, it''s must look like ALStringToAnsiString.exe directory:c:\sample\ SubDirectory:1 FileNameMask:*.pas');
    ALStringToAnsiString_UpdateFiles(aDirectory,
                                        aSubDirectory,
                                        aBackupDir,
                                        aFileNameMask);
  except
    on E: Exception do
      Writeln(E.Message);
  end;
  
end.
