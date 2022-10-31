unit ALFiles;

interface

{$I Alcinoe.inc}

uses
  ALCommon;

{$IFNDEF ALHideAnsiString}
Function  AlEmptyDirectory(Directory: ansiString;
                           SubDirectory: Boolean;
                           const IgnoreFiles: Array of AnsiString;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: ansiString = '*';
                           Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlEmptyDirectory(const Directory: ansiString;
                           SubDirectory: Boolean;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: ansiString = '*';
                           Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlCopyDirectory(SrcDirectory,
                          DestDirectory: ansiString;
                          SubDirectory: Boolean;
                          Const FileNameMask: ansiString = '*';
                          Const FailIfExists: Boolean = True;
                          Const SkipIfExists: Boolean = False): Boolean;
function  ALGetModuleName: ansistring;
function  ALGetModuleFileNameWithoutExtension: ansistring;
function  ALGetModulePath: ansiString;
Function  AlGetFileSize(const AFileName: ansistring): int64;
Function  AlGetFileVersion(const AFileName: ansistring): ansiString;
function  ALGetFileCreationDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime;
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime);
Procedure ALSetFileLastWriteDateTime(Const aFileName: Ansistring; Const aLastWriteDateTime: TDateTime);
Procedure ALSetFileLastAccessDateTime(Const aFileName: Ansistring; Const aLastAccessDateTime: TDateTime);
function  ALIsDirectoryEmpty(const directory: ansiString): boolean;
function  ALFileExists(const Path: ansiString): boolean;
function  ALDirectoryExists(const Directory: Ansistring): Boolean;
function  ALCreateDir(const Dir: Ansistring): Boolean;
function  ALRemoveDir(const Dir: Ansistring): Boolean;
function  ALDeleteFile(const FileName: Ansistring): Boolean;
function  ALRenameFile(const OldName, NewName: ansistring): Boolean;
{$ENDIF !ALHideAnsiString}

Function  AlEmptyDirectoryU(Directory: String;
                            SubDirectory: Boolean;
                            const IgnoreFiles: Array of String;
                            Const RemoveEmptySubDirectory: Boolean = True;
                            Const FileNameMask: String = '*';
                            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlEmptyDirectoryU(const Directory: String;
                            SubDirectory: Boolean;
                            Const RemoveEmptySubDirectory: Boolean = True;
                            Const FileNameMask: String = '*';
                            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
function  ALGetFileSizeU(const FileName : string): Int64;

{$IF defined(MSWINDOWS)}
Function  AlCopyDirectoryU(SrcDirectory,
                           DestDirectory: String;
                           SubDirectory: Boolean;
                           Const FileNameMask: String = '*';
                           Const FailIfExists: Boolean = True;
                           Const SkipIfExists: Boolean = False): Boolean;
function  ALGetModuleNameU: String;
function  ALGetModulePathU: String;
function  ALGetFileCreationDateTimeU(const aFileName: String): TDateTime;
function  ALGetFileLastWriteDateTimeU(const aFileName: String): TDateTime;
function  ALGetFileLastAccessDateTimeU(const aFileName: String): TDateTime;
Procedure ALSetFileCreationDateTimeU(Const aFileName: String; Const aCreationDateTime: TDateTime);
Procedure ALSetFileLastWriteDateTimeU(Const aFileName: String; Const aLastWriteDateTime: TDateTime);
Procedure ALSetFileLastAccessDateTimeU(Const aFileName: String; Const aLastAccessDateTime: TDateTime);
function  ALIsDirectoryEmptyU(const directory: String): boolean;
function  ALFileExistsU(const Path: String): boolean;
function  ALDirectoryExistsU(const Directory: string): Boolean;
function  ALCreateDirU(const Dir: string): Boolean;
function  ALRemoveDirU(const Dir: string): Boolean;
function  ALDeleteFileU(const FileName: string): Boolean;
function  ALRenameFileU(const OldName, NewName: string): Boolean;
{$ENDIF MSWINDOWS}


implementation

uses
  System.Classes,
  System.sysutils,
  System.Masks,
  {$IFNDEF ALHideAnsiString}
  System.AnsiStrings,
  Winapi.Windows,
  Winapi.ShLwApi,
  {$ELSE}
  Posix.Unistd,
  {$ENDIF}
  ALString,
  ALStringList;

{$IFNDEF ALHideAnsiString}

{***********************************************}
Function  AlEmptyDirectory(Directory: ansiString;
                           SubDirectory: Boolean;
                           const IgnoreFiles: Array of AnsiString;
                           const RemoveEmptySubDirectory: Boolean = True;
                           const FileNameMask: ansiString = '*';
                           const MinFileAge: TdateTime = ALNullDate): Boolean;
var LSR: TSearchRec;
    LIgnoreFilesLst: TalStringList;
    I: integer;
begin
  if (Directory = '') or
     (Directory = '.') or
     (Directory = '..') then raise EALException.CreateFmt('Wrong directory ("%s")', [Directory]);

  Result := True;
  Directory := ALIncludeTrailingPathDelimiter(Directory);
  LIgnoreFilesLst := TalStringList.Create;
  try
    for I := 0 to length(IgnoreFiles) - 1 do LIgnoreFilesLst.Add(ALExcludeTrailingPathDelimiter(IgnoreFiles[I]));
    LIgnoreFilesLst.Duplicates := DupIgnore;
    LIgnoreFilesLst.Sorted := True;
    if System.sysutils.FindFirst(string(Directory) + '*', faAnyFile	, LSR) = 0 then begin
      Try
        repeat
          If (LSR.Name <> '.') and
             (LSR.Name <> '..') and
             (LIgnoreFilesLst.IndexOf(Directory + ansistring(LSR.Name)) < 0) Then Begin
            If ((LSR.Attr and faDirectory) <> 0) then begin
              If SubDirectory then begin
                Result := AlEmptyDirectory(Directory + ansistring(LSR.Name),
                                           True,
                                           IgnoreFiles,
                                           RemoveEmptySubDirectory,
                                           fileNameMask,
                                           MinFileAge);
                If result and RemoveEmptySubDirectory then RemoveDir(string(Directory) + LSR.Name);
              end;
            end
            else If ((FileNameMask = '*') or
                     ALMatchesMask(AnsiString(LSR.Name), FileNameMask))
                    and
                    ((MinFileAge=ALNullDate) or
                     (LSR.TimeStamp < MinFileAge))
            then Result := System.sysutils.Deletefile(string(Directory) + LSR.Name);
          end;
        until (not result) or (FindNext(LSR) <> 0);
      finally
        System.sysutils.FindClose(LSR);
      end;
    end;
  finally
    LIgnoreFilesLst.Free;
  end;
end;

{****************************************************}
Function AlEmptyDirectory(const Directory: ansiString;
                          SubDirectory: Boolean;
                          Const RemoveEmptySubDirectory: Boolean = True;
                          Const FileNameMask: ansiString = '*';
                          Const MinFileAge: TdateTime = ALNullDate): Boolean;
begin
  result := AlEmptyDirectory(Directory,
                             SubDirectory,
                             [],
                             RemoveEmptySubDirectory,
                             FileNameMask,
                             MinFileAge);
end;

{************************************}
Function AlCopyDirectory(SrcDirectory,
                         DestDirectory: ansiString;
                         SubDirectory: Boolean;
                         Const FileNameMask: ansiString = '*';
                         Const FailIfExists: Boolean = True;
                         Const SkipIfExists: Boolean = False): Boolean;
var sr: TSearchRec;
begin
  Result := True;
  SrcDirectory := ALIncludeTrailingPathDelimiter(SrcDirectory);
  DestDirectory := ALIncludeTrailingPathDelimiter(DestDirectory);
  If not DirectoryExists(string(DestDirectory)) and (not Createdir(String(DestDirectory))) then begin
    result := False;
    exit;
  end;

  if System.sysutils.FindFirst(String(SrcDirectory) + '*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then Result := AlCopyDirectory(SrcDirectory + ansiString(sr.Name),
                                                           DestDirectory + ansiString(sr.Name),
                                                           SubDirectory,
                                                           FileNameMask,
                                                           FailIfExists,
                                                           SkipIfExists);
          end
          else If (FileNameMask = '*') or
                  (ALMatchesMask(AnsiString(sr.Name), FileNameMask)) then begin
            if SkipIfExists then begin
              if not ALFileExists(DestDirectory + AnsiString(sr.Name)) then
                Result := CopyfileA(PAnsiChar(SrcDirectory + AnsiString(sr.Name)),
                                    PAnsiChar(DestDirectory + AnsiString(sr.Name)),
                                    true{FailIfExists});
            end
            else begin
              result := CopyfileA(PAnsiChar(SrcDirectory + AnsiString(sr.Name)),
                                  PAnsiChar(DestDirectory + AnsiString(sr.Name)),
                                  FailIfExists);
            end;
          end;
        end;
      until (not result) or (FindNext(sr) <> 0);
    finally
      System.sysutils.FindClose(sr);
    end;
  end
end;

{**********************************************************}
Function  AlGetFileSize(const AFileName: ansistring): int64;
var
  Handle: THandle;
  FindData: TWin32FindDataA;
begin
  Handle := FindFirstFileA(PAnsiChar(AFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Winapi.Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(Result).Lo := FindData.nFileSizeLow;
      Int64Rec(Result).Hi := FindData.nFileSizeHigh;
      Exit;
    end;
  end;
  Result := -1;
end;

{*****************************************************************}
Function AlGetFileVersion(const AFileName: ansiString): ansiString;
var
  FileName: ansiString;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := '';
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSizeA(PAnsiChar(FileName), Wnd);
  if InfoSize <> 0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfoA(PAnsiChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := ALIntToStr(HiWord(FI.dwFileVersionMS)) +'.'+ ALIntToStr(LoWord(FI.dwFileVersionMS)) +'.'+ ALIntToStr(HiWord(FI.dwFileVersionLS)) +'.'+ ALIntToStr(LoWord(FI.dwFileVersionLS));
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{*******************************************************}
function ALGetModuleFileNameWithoutExtension: ansiString;
Var Ln: Integer;
begin
  result := ALExtractFileName(ALGetModuleName);
  ln := Length(ALExtractFileExt(Result));
  if Ln > 0 then delete(Result,length(Result)-ln+1,ln);
end;

{***********************************}
function ALGetModuleName: ansiString;
var ModName: array[0..MAX_PATH] of AnsiChar;
begin
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameA(HInstance, ModName, SizeOf(ModName)));
  If ALpos('\\?\',result) = 1 then delete(Result,1,4);
end;

{***********************************}
function ALGetModulePath: ansiString;
begin
  Result:=ALExtractFilePath(ALGetModuleName);
  If (length(result) > 0) and (result[length(result)] <> '\') then result := result + '\';
end;

{**************************************************************************}
function  ALGetFileCreationDateTime(const aFileName: Ansistring): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindDataA;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileA(PAnsiChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftCreationTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{***************************************************************************}
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindDataA;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileA(PAnsiChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftLastWriteTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{****************************************************************************}
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindDataA;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileA(PAnsiChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftLastAccessTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{***************************************************************************************************}
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(String(aFileName), fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aCreationDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, @LFiletime, nil, nil)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{*****************************************************************************************************}
Procedure ALSetFileLastWriteDateTime(Const aFileName: Ansistring; Const aLastWriteDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(String(aFileName), fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aLastWriteDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, nil, nil, @LFiletime)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{*******************************************************************************************************}
Procedure ALSetFileLastAccessDateTime(Const aFileName: Ansistring; Const aLastAccessDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(String(aFileName), fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aLastAccessDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, nil, @LFiletime, nil)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{****************************************************************}
function ALIsDirectoryEmpty(const directory: ansiString): boolean;
begin
  Result := PathIsDirectoryEmptyA(PansiChar(directory));
end;

{******************************************************}
function  ALFileExists(const Path: ansiString): boolean;
begin
  result := PathFileExistsA(PansiChar(Path)) and (not PathIsDirectoryA(PansiChar(Path)));
end;

{****************************************************************}
function  ALDirectoryExists(const Directory: Ansistring): Boolean;
begin
  result := PathFileExistsA(PansiChar(Directory)) and (PathIsDirectoryA(PansiChar(Directory)));
end;

{****************************************************}
function  ALCreateDir(const Dir: Ansistring): Boolean;
begin
  Result := CreateDirectoryA(PAnsiChar(Dir), nil);
end;

{****************************************************}
function  ALRemoveDir(const Dir: Ansistring): Boolean;
begin
  Result := RemoveDirectoryA(PansiChar(Dir));
end;

{**********************************************************}
function  ALDeleteFile(const FileName: Ansistring): Boolean;
begin
  Result := DeleteFileA(PAnsiChar(FileName));
end;

{******************************************************************}
function  ALRenameFile(const OldName, NewName: ansistring): Boolean;
begin
  Result := MoveFileA(PansiChar(OldName), PansiChar(NewName));
end;

{$ENDIF !ALHideAnsiString}

{********************************************}
Function  AlEmptyDirectoryU(Directory: String;
                            SubDirectory: Boolean;
                            const IgnoreFiles: Array of String;
                            const RemoveEmptySubDirectory: Boolean = True;
                            const FileNameMask: String = '*';
                            const MinFileAge: TdateTime = ALNullDate): Boolean;
var LSR: TSearchRec;
    LIgnoreFilesLst: TalStringListU;
    I: integer;
begin
  if (Directory = '') or
     (Directory = '.') or
     (Directory = '..') then raise EALExceptionU.CreateFmt('Wrong directory ("%s")', [Directory]);

  Result := True;
  Directory := ALIncludeTrailingPathDelimiterU(Directory);
  LIgnoreFilesLst := TalStringListU.Create;
  try
    for I := 0 to length(IgnoreFiles) - 1 do LIgnoreFilesLst.Add(ALExcludeTrailingPathDelimiterU(IgnoreFiles[I]));
    LIgnoreFilesLst.Duplicates := DupIgnore;
    LIgnoreFilesLst.Sorted := True;
    if System.sysutils.FindFirst(Directory + '*', faAnyFile	, LSR) = 0 then begin
      Try
        repeat
          If (LSR.Name <> '.') and
             (LSR.Name <> '..') and
             (LIgnoreFilesLst.IndexOf(Directory + LSR.Name) < 0) Then Begin
            If ((LSR.Attr and faDirectory) <> 0) then begin
              If SubDirectory then begin
                Result := AlEmptyDirectoryU(Directory + LSR.Name,
                                            True,
                                            IgnoreFiles,
                                            RemoveEmptySubDirectory,
                                            fileNameMask,
                                            MinFileAge);
                If result and RemoveEmptySubDirectory then RemoveDir(Directory + LSR.Name);
              end;
            end
            else If ((FileNameMask = '*') or
                     MatchesMask(LSR.Name, FileNameMask))
                    and
                    ((MinFileAge=ALNullDate) or
                     (LSR.TimeStamp < MinFileAge))
            then Result := System.sysutils.Deletefile(Directory + LSR.Name);
          end;
        until (not result) or (FindNext(LSR) <> 0);
      finally
        System.sysutils.FindClose(LSR);
      end;
    end;
  finally
    LIgnoreFilesLst.Free;
  end;
end;

{*************************************************}
Function AlEmptyDirectoryU(const Directory: String;
                           SubDirectory: Boolean;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: String = '*';
                           Const MinFileAge: TdateTime = 0): Boolean;
begin
  result := AlEmptyDirectoryU(Directory,
                              SubDirectory,
                              [],
                              RemoveEmptySubDirectory,
                              FileNameMask,
                              MinFileAge);
end;

{*******************************************************}
function ALGetFileSizeU(const FileName : string) : Int64;
var LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    result := LFileStream.Size;
  finally
    alFreeAndNil(LFileStream);
  end;
end;


{$IF defined(MSWINDOWS)}

{*************************************}
Function AlCopyDirectoryU(SrcDirectory,
                          DestDirectory: String;
                          SubDirectory: Boolean;
                          Const FileNameMask: String = '*';
                          Const FailIfExists: Boolean = True;
                          Const SkipIfExists: Boolean = False): Boolean;
var sr: TSearchRec;
begin
  Result := True;
  SrcDirectory := ALIncludeTrailingPathDelimiterU(SrcDirectory);
  DestDirectory := ALIncludeTrailingPathDelimiterU(DestDirectory);
  If not DirectoryExists(DestDirectory) and (not Createdir(DestDirectory)) then begin
    result := False;
    exit;
  end;

  if System.sysutils.FindFirst(SrcDirectory + '*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then Result := AlCopyDirectoryU(SrcDirectory + sr.Name,
                                                            DestDirectory + sr.Name,
                                                            SubDirectory,
                                                            FileNameMask,
                                                            FailIfExists,
                                                            SkipIfExists);
          end
          else If (FileNameMask = '*') or
                  (ALMatchesMaskU(sr.Name, FileNameMask)) then begin
            if SkipIfExists then begin
              if not ALFileExistsU(DestDirectory + sr.Name) then
                Result := CopyfileW(PChar(SrcDirectory + sr.Name),
                                    PChar(DestDirectory + sr.Name),
                                    True{FailIfExists});
            end
            else begin
              result := CopyfileW(PChar(SrcDirectory + sr.Name),
                                  PChar(DestDirectory + sr.Name),
                                  FailIfExists);
            end;
          end;
        end;
      until (not result) or (FindNext(sr) <> 0);
    finally
      System.sysutils.FindClose(sr);
    end;
  end
end;

{********************************}
function ALGetModuleNameU: String;
var ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameW(HInstance, ModName, SizeOf(ModName)));
  If ALposU('\\?\',result) = 1 then delete(Result,1,4);
end;

{********************************}
function ALGetModulePathU: String;
begin
  Result:=ALExtractFilePathU(ALGetModuleNameU);
  If (length(result) > 0) and (result[length(result)] <> '\') then result := result + '\';
end;

{***********************************************************************}
function  ALGetFileCreationDateTimeU(const aFileName: String): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindData;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileW(PChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftCreationTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{************************************************************************}
function  ALGetFileLastWriteDateTimeU(const aFileName: String): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindData;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileW(PChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftLastWriteTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{*************************************************************************}
function  ALGetFileLastAccessDateTimeU(const aFileName: String): TDateTime;
var LHandle: THandle;
    LFindData: TWin32FindData;
    LLocalFileTime: TFileTime;
begin
  LHandle := FindFirstFileW(PChar(aFileName), LFindData);
  if (LHandle = INVALID_HANDLE_VALUE) or
     (not Winapi.Windows.FindClose(LHandle)) or
     (not FileTimeToLocalFileTime(LFindData.ftLastAccessTime, LLocalFileTime)) then raiselastOsError;
  Result := ALFileTimeToDateTime(LLocalFileTime);
end;

{************************************************************************************************}
Procedure ALSetFileCreationDateTimeU(Const aFileName: String; Const aCreationDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(aFileName, fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aCreationDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, @LFiletime, nil, nil)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{**************************************************************************************************}
Procedure ALSetFileLastWriteDateTimeU(Const aFileName: String; Const aLastWriteDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(aFileName, fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aLastWriteDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, nil, nil, @LFiletime)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{****************************************************************************************************}
Procedure ALSetFileLastAccessDateTimeU(Const aFileName: String; Const aLastAccessDateTime: TDateTime);
Var LHandle: Thandle;
    LSystemTime: TsystemTime;
    LFiletime: TfileTime;
Begin
  LHandle := System.sysUtils.fileOpen(aFileName, fmOpenWrite or fmShareDenyNone);
  if LHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aLastAccessDateTime, LSystemTime);
    if (not SystemTimeToFileTime(LSystemTime, LFiletime)) or
       (not LocalFileTimeToFileTime(LFiletime, LFiletime)) or
       (not setFileTime(LHandle, nil, @LFiletime, nil)) then raiselastOsError;
  finally
    fileClose(LHandle);
  end;
End;

{*************************************************************}
function ALIsDirectoryEmptyU(const directory: String): boolean;
begin
  Result := PathIsDirectoryEmptyW(PChar(directory));
end;

{***************************************************}
function  ALFileExistsU(const Path: String): boolean;
begin
  result := PathFileExistsW(PChar(Path)) and (not PathIsDirectoryW(PChar(Path)));
end;

{*************************************************************}
function  ALDirectoryExistsU(const Directory: string): Boolean;
begin
  result := PathFileExistsW(PChar(Directory)) and (PathIsDirectoryW(PChar(Directory)));
end;

{*************************************************}
function  ALCreateDirU(const Dir: string): Boolean;
begin
  Result := CreateDirectoryW(PChar(Dir), nil);
end;

{*************************************************}
function  ALRemoveDirU(const Dir: string): Boolean;
begin
  Result := RemoveDirectoryW(PChar(Dir));
end;

{*******************************************************}
function  ALDeleteFileU(const FileName: String): Boolean;
begin
  Result := DeleteFileW(PChar(FileName));
end;

{***************************************************************}
function  ALRenameFileU(const OldName, NewName: String): Boolean;
begin
  Result := MoveFileW(PChar(OldName), PChar(NewName));
end;

{$ENDIF MSWINDOWS}

end.
