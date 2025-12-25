unit Alcinoe.Files;

interface

{$I Alcinoe.inc}

uses
  Alcinoe.Common;

Function  AlEmptyDirectoryA(
            Directory: ansiString;
            SubDirectory: Boolean;
            const IgnoreFiles: Array of AnsiString;
            Const RemoveEmptySubDirectory: Boolean = True;
            Const FileNameMask: ansiString = '*';
            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlEmptyDirectoryW(
            Directory: String;
            SubDirectory: Boolean;
            const IgnoreFiles: Array of String;
            Const RemoveEmptySubDirectory: Boolean = True;
            Const FileNameMask: String = '*';
            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlEmptyDirectoryA(
            const Directory: ansiString;
            SubDirectory: Boolean;
            Const RemoveEmptySubDirectory: Boolean = True;
            Const FileNameMask: ansiString = '*';
            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;
Function  AlEmptyDirectoryW(
            const Directory: String;
            SubDirectory: Boolean;
            Const RemoveEmptySubDirectory: Boolean = True;
            Const FileNameMask: String = '*';
            Const MinFileAge: TdateTime = ALNullDate): Boolean; overload;

function ALGetAppDataPathW: String;
function ALGetTempPathW: String;
function ALGetTempFilenameW(Const AExt: String = '.tmp'): String;
function ALGetCachePathW: String;

{$IF defined(MSWINDOWS)}
Function  AlCopyDirectoryA(
            SrcDirectory,
            DestDirectory: ansiString;
            SubDirectory: Boolean;
            Const FileNameMask: ansiString = '*';
            Const FailIfExists: Boolean = True;
            Const SkipIfExists: Boolean = False): Boolean;
Function  AlCopyDirectoryW(
            SrcDirectory,
            DestDirectory: String;
            SubDirectory: Boolean;
            Const FileNameMask: String = '*';
            Const FailIfExists: Boolean = True;
            Const SkipIfExists: Boolean = False): Boolean;
function  ALGetModuleNameA(const AWithoutExtension: boolean = False): ansistring;
function  ALGetModuleNameW(const AWithoutExtension: boolean = False): String;
function  ALGetModulePathA: ansiString;
function  ALGetModulePathW: String;
Function  AlGetFileSize(const AFileName: ansistring): int64; overload; deprecated 'Use Tfile.GetSize Instead';
function  ALGetFileSize(const FileName : string): Int64; overload; deprecated 'Use Tfile.GetSize Instead';
Function  AlGetFileVersion(const AFileName: ansistring): ansiString; overload;
Function  AlGetFileVersion(const AFileName: string): String; overload;
function  ALGetFileCreationDateTime(const aFileName: Ansistring): TDateTime; overload; deprecated 'Use Tfile.GetCreationTime/Tfile.GetCreationTimeUtc Instead';
function  ALGetFileCreationDateTime(const aFileName: String): TDateTime; overload; deprecated 'Use Tfile.GetCreationTime/Tfile.GetCreationTimeUtc Instead';
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime; overload; deprecated 'Use Tfile.GetLastWriteTime/Tfile.GetLastWriteTimeUtc Instead';
function  ALGetFileLastWriteDateTime(const aFileName: String): TDateTime; overload; deprecated 'Use Tfile.GetLastWriteTime/Tfile.GetLastWriteTimeUtc Instead';
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime; overload; deprecated 'Use Tfile.GetLastAccessTime/Tfile.GetLastAccessTimeUtc Instead';
function  ALGetFileLastAccessDateTime(const aFileName: String): TDateTime; overload; deprecated 'Use Tfile.GetLastAccessTime/Tfile.GetLastAccessTimeUtc Instead';
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime); overload; deprecated 'Use Tfile.SetCreationTime/Tfile.SetCreationTimeUtc Instead';
Procedure ALSetFileCreationDateTime(Const aFileName: String; Const aCreationDateTime: TDateTime); overload; deprecated 'Use Tfile.SetCreationTime/Tfile.SetCreationTimeUtc Instead';
Procedure ALSetFileLastWriteDateTime(Const aFileName: Ansistring; Const aLastWriteDateTime: TDateTime); overload; deprecated 'Use Tfile.SetLastWriteTime/Tfile.SetLastWriteTimeUtc Instead';
Procedure ALSetFileLastWriteDateTime(Const aFileName: String; Const aLastWriteDateTime: TDateTime); overload; deprecated 'Use Tfile.SetLastWriteTime/Tfile.SetLastWriteTimeUtc Instead';
Procedure ALSetFileLastAccessDateTime(Const aFileName: Ansistring; Const aLastAccessDateTime: TDateTime); overload; deprecated 'Use Tfile.LastAccessTime/Tfile.LastAccessTimeUtc Instead';
Procedure ALSetFileLastAccessDateTime(Const aFileName: String; Const aLastAccessDateTime: TDateTime); overload; deprecated 'Use Tfile.LastAccessTime/Tfile.LastAccessTimeUtc Instead';
function  ALIsDirectoryEmpty(const directory: ansiString): boolean; overload; deprecated 'Use Tdirectory.IsEmpty Instead';
function  ALIsDirectoryEmpty(const directory: String): boolean; overload; deprecated 'Use Tdirectory.IsEmpty Instead';
function  ALFileExists(const Path: ansiString): boolean; overload; deprecated 'Use Tfile.Exists Instead';
function  ALFileExists(const Path: String): boolean; overload; deprecated 'Use Tfile.Exists Instead';
function  ALDirectoryExists(const Directory: Ansistring): Boolean; overload; deprecated 'Use Tdirectory.Exists Instead';
function  ALDirectoryExists(const Directory: string): Boolean; overload; deprecated 'Use Tdirectory.Exists Instead';
function  ALCreateDir(const Dir: Ansistring): Boolean; overload; deprecated 'Use Tdirectory.CreateDirectory Instead';
function  ALCreateDir(const Dir: string): Boolean; overload; deprecated 'Use Tdirectory.CreateDirectory Instead';
function  ALRemoveDir(const Dir: Ansistring): Boolean; overload; deprecated 'Use Tdirectory.Delete Instead';
function  ALRemoveDir(const Dir: string): Boolean; overload; deprecated 'Use Tdirectory.Delete Instead';
function  ALDeleteFile(const FileName: Ansistring): Boolean; overload; deprecated 'Use Tfile.Delete Instead';
function  ALDeleteFile(const FileName: string): Boolean; overload; deprecated 'Use Tfile.Delete Instead';
function  ALRenameFileA(const OldName, NewName: ansistring): Boolean; deprecated 'Use Tfile.Move Instead';
function  ALRenameFileW(const OldName, NewName: string): Boolean; deprecated 'Use Tfile.Move Instead';
{$ENDIF}

implementation

uses
  System.Classes,
  System.sysutils,
  System.IOUtils,
  System.Masks,
  {$IF defined(MSWINDOWS)}
  System.AnsiStrings,
  Winapi.Windows,
  Winapi.ShLwApi,
  {$ELSE}
  Posix.Unistd,
  {$ENDIF}
  {$IF defined(Android)}
  Androidapi.IOUtils,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.Helpers,
  Macapi.Foundation,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  iOSapi.Foundation,
  {$ENDIF}
  Alcinoe.StringUtils,
  Alcinoe.StringList;

{**************************}
Function  AlEmptyDirectoryA(
            Directory: ansiString;
            SubDirectory: Boolean;
            const IgnoreFiles: Array of AnsiString;
            const RemoveEmptySubDirectory: Boolean = True;
            const FileNameMask: ansiString = '*';
            const MinFileAge: TdateTime = ALNullDate): Boolean;
var LIgnoreFilesW: Array of String;
begin
  setlength(LIgnoreFilesW, length(IgnoreFiles));
  for var I := Low(IgnoreFiles) to High(IgnoreFiles) do
    LIgnoreFilesW[i] := String(IgnoreFiles[i]);
  result := AlEmptyDirectoryW(
              String(Directory),
              SubDirectory,
              LIgnoreFilesW,
              RemoveEmptySubDirectory,
              String(FileNameMask),
              MinFileAge);
end;

{**************************}
Function  AlEmptyDirectoryW(
            Directory: String;
            SubDirectory: Boolean;
            const IgnoreFiles: Array of String;
            const RemoveEmptySubDirectory: Boolean = True;
            const FileNameMask: String = '*';
            const MinFileAge: TdateTime = ALNullDate): Boolean;
var LSR: TSearchRec;
    LIgnoreFilesLst: TALStringListW;
    I: integer;
begin
  if (Directory = '') or
     (Directory = '.') or
     (Directory = '..') then raise EALException.CreateFmt('Wrong directory ("%s")', [Directory]);

  Result := True;
  Directory := ALIncludeTrailingPathDelimiterW(Directory);
  LIgnoreFilesLst := TALStringListW.Create;
  try
    for I := 0 to length(IgnoreFiles) - 1 do LIgnoreFilesLst.Add(ALExcludeTrailingPathDelimiterW(IgnoreFiles[I]));
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
                Result := AlEmptyDirectoryW(
                            Directory + LSR.Name,
                            True,
                            IgnoreFiles,
                            RemoveEmptySubDirectory,
                            fileNameMask,
                            MinFileAge);
                If result and RemoveEmptySubDirectory then
                  result := RemoveDir(Directory + LSR.Name);
              end;
            end
            else If ((FileNameMask = '*') or
                     ALMatchesMaskW(LSR.Name, FileNameMask))
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

{*************************}
Function AlEmptyDirectoryA(
           const Directory: ansiString;
           SubDirectory: Boolean;
           Const RemoveEmptySubDirectory: Boolean = True;
           Const FileNameMask: ansiString = '*';
           Const MinFileAge: TdateTime = ALNullDate): Boolean;
begin
  result := AlEmptyDirectoryA(
              Directory,
              SubDirectory,
              [],
              RemoveEmptySubDirectory,
              FileNameMask,
              MinFileAge);
end;

{*************************}
Function AlEmptyDirectoryW(
           const Directory: String;
           SubDirectory: Boolean;
           Const RemoveEmptySubDirectory: Boolean = True;
           Const FileNameMask: String = '*';
           Const MinFileAge: TdateTime = 0): Boolean;
begin
  result := AlEmptyDirectoryW(
              Directory,
              SubDirectory,
              [],
              RemoveEmptySubDirectory,
              FileNameMask,
              MinFileAge);
end;

{*********************************}
function ALGetAppDataPathW: String;
begin

  {$IF defined(MSWindows)}
  // Windows XP:             C:\Documents and Settings\<username>\Application Data\<AppName>\
  // Windows Vista or later: C:\Users\<username>\AppData\Roaming\<AppName>\
  Result := TPath.Combine(System.IOUtils.TPath.GetHomePath, ALGetModuleNameW(True{AWithoutExtension}));
  {$ELSEIF defined(ALAppleOS)}
  // OS X:          /Users/<username>/Library/Application Support/
  // iOS Device:    /var/mobile/Containers/Data/Application/<application ID>/Library/Application Support/
  var LNSFile := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4412 was corrected and adjust the IFDEF'}
  {$ENDIF}
  var LErrorPtr: Pointer := nil;
  var LURL := LNSFile.URLForDirectory(
                NSApplicationSupportDirectory, // directory: NSSearchPathDirectory;
                NSUserDomainMask, // inDomain: NSSearchPathDomainMask;
                nil, // appropriateForURL: NSURL;
                true, // create: Boolean;
                @LErrorPtr); // error: PPointer
  if LErrorPtr <> nil then
    Raise Exception.Create(NSStrToStr(TNSError.Wrap(LErrorPtr).localizedDescription));
  if (LURL <> nil) then Result := UTF8ToString(LURL.path.UTF8String)
  else Raise Exception.Create('Error CBDF8B31-4AE2-4805-AD52-3678D3450A6A');
  {$ELSEIF defined(ANDROID)}
  // Android: /data/data/<application ID>/files
  Result := GetFilesDir;
  {$ELSE}
  raise Exception.Create('ALGetAppDataPathW is not supported on this platform.');
  {$ENDIF}

  If (length(result) > 0) and (result[length(result)] <> TPath.DirectorySeparatorChar) then result := result + TPath.DirectorySeparatorChar;
  If not TDirectory.Exists(result) then TDirectory.CreateDirectory(Result);

end;

{******************************}
function ALGetTempPathW: String;
begin

  {$IF defined(MSWindows)}
  // Windows XP:             C:\Documents and Settings\<User name>\Local Settings\Temp\
  // Windows Vista or later: C:\Users\<User name>\AppData\Local\Temp\
  Result := System.IOUtils.TPath.GetTempPath;
  {$ELSEIF defined(ALAppleOS)}
  // OS X:          /var/folders/<random folder name>/
  // iOS Device:    /private/var/mobile/Containers/Data/Application/<application ID>/tmp/
  Result := NSStrToStr(TNSString.Wrap(NSTemporaryDirectory));
  {$ELSEIF defined(ANDROID)}
  // Android: /data/data/<application ID>/cache/tmp/
  Result := TPath.Combine(Androidapi.IOUtils.GetCacheDir, 'tmp/');
  {$ELSE}
  raise Exception.Create('ALGetAppDataPathW is not supported on this platform.');
  {$ENDIF}

  If (length(result) > 0) and (result[length(result)] <> TPath.DirectorySeparatorChar) then result := result + TPath.DirectorySeparatorChar;
  If not TDirectory.Exists(result) then TDirectory.CreateDirectory(Result);

end;

{***************************************************************}
function ALGetTempFilenameW(Const AExt: String = '.tmp'): String;
begin
  Result := TPath.Combine(ALGetTempPathW, ALLowercase(ALNewGUIDStringW(True{WithoutBracket}, False{WithoutHyphen}))+AExt)
end;

{*******************************}
function ALGetCachePathW: String;
begin

  {$IF defined(MSWindows)}
  // Windows XP:             C:\Documents and Settings\<username>\Local Settings\Application Data\<AppName>\
  // Windows Vista or later: C:\Users\<username>\AppData\Local\<AppName>\
  Result := TPath.Combine(System.IOUtils.TPath.GetCachePath, ALGetModuleNameW(True{AWithoutExtension}));
  {$ELSEIF defined(ALAppleOS)}
  // OS X:          /Users/<username>/Library/Caches/
  // iOS Device:    /var/mobile/Containers/Data/Application/<application ID>/Library/Caches/
  var LNSFile := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  {$IFNDEF ALCompilerVersionSupported130}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-4412 was corrected and adjust the IFDEF'}
  {$ENDIF}
  var LErrorPtr: Pointer := nil;
  var LURL := LNSFile.URLForDirectory(
                NSCachesDirectory, // directory: NSSearchPathDirectory;
                NSUserDomainMask, // inDomain: NSSearchPathDomainMask;
                nil, // appropriateForURL: NSURL;
                true, // create: Boolean;
                @LErrorPtr); // error: PPointer
  if LErrorPtr <> nil then
    Raise Exception.Create(NSStrToStr(TNSError.Wrap(LErrorPtr).localizedDescription));
  if (LURL <> nil) then Result := UTF8ToString(LURL.path.UTF8String)
  else Raise Exception.Create('Error 557E87B9-4887-47F4-83F0-E65939AE4704');
  {$ELSEIF defined(ANDROID)}
  // Android: /data/data/<application ID>/cache/
  Result := Androidapi.IOUtils.GetCacheDir;
  {$ELSE}
  raise Exception.Create('ALGetAppDataPathW is not supported on this platform.');
  {$ENDIF}

  If (length(result) > 0) and (result[length(result)] <> TPath.DirectorySeparatorChar) then result := result + TPath.DirectorySeparatorChar;
  If not TDirectory.Exists(result) then TDirectory.CreateDirectory(Result);

end;

{**********************}
{$IF defined(MSWINDOWS)}
Function AlCopyDirectoryA(
           SrcDirectory,
           DestDirectory: ansiString;
           SubDirectory: Boolean;
           Const FileNameMask: ansiString = '*';
           Const FailIfExists: Boolean = True;
           Const SkipIfExists: Boolean = False): Boolean;
begin
  result := AlCopyDirectoryW(
              String(SrcDirectory),
              String(DestDirectory),
              SubDirectory,
              String(FileNameMask),
              FailIfExists,
              SkipIfExists);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Function AlCopyDirectoryW(
           SrcDirectory,
           DestDirectory: String;
           SubDirectory: Boolean;
           Const FileNameMask: String = '*';
           Const FailIfExists: Boolean = True;
           Const SkipIfExists: Boolean = False): Boolean;
var sr: TSearchRec;
begin
  Result := True;
  SrcDirectory := ALIncludeTrailingPathDelimiterW(SrcDirectory);
  DestDirectory := ALIncludeTrailingPathDelimiterW(DestDirectory);
  If not DirectoryExists(DestDirectory) and (not Createdir(DestDirectory)) then begin
    result := False;
    exit;
  end;

  if System.sysutils.FindFirst(SrcDirectory + '*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then Result := AlCopyDirectoryW(
                                             SrcDirectory + sr.Name,
                                             DestDirectory + sr.Name,
                                             SubDirectory,
                                             FileNameMask,
                                             FailIfExists,
                                             SkipIfExists);
          end
          else If (FileNameMask = '*') or
                  (ALMatchesMaskW(sr.Name, FileNameMask)) then begin
            if SkipIfExists then begin
              if not Tfile.Exists(DestDirectory + sr.Name) then
                Result := CopyfileW(
                            PChar(SrcDirectory + sr.Name),
                            PChar(DestDirectory + sr.Name),
                            True{FailIfExists});
            end
            else begin
              result := CopyfileW(
                          PChar(SrcDirectory + sr.Name),
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGetModuleNameA(const AWithoutExtension: boolean = False): ansiString;
begin
  var ModName: array[0..MAX_PATH] of AnsiChar;
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameA(HInstance, ModName, SizeOf(ModName)));
  result := ALExtractFileName(result);
  if AWithoutExtension then begin
    Var ln := Length(ALExtractFileExt(Result));
    if Ln > 0 then delete(Result,length(Result)-ln+1,ln);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGetModuleNameW(const AWithoutExtension: boolean = False): String;
begin
  var ModName: array[0..MAX_PATH] of Char;
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameW(HInstance, ModName, SizeOf(ModName)));
  result := ALExtractFileName(result);
  if AWithoutExtension then begin
    Var ln := Length(ALExtractFileExt(Result));
    if Ln > 0 then delete(Result,length(Result)-ln+1,ln);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGetModulePathA: ansiString;
begin
  var ModName: array[0..MAX_PATH] of AnsiChar;
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameA(HInstance, ModName, SizeOf(ModName)));
  If ALPosA('\\?\',result) = 1 then delete(Result,1,4);
  Result:=ALExtractFilePath(Result);
  If (length(result) > 0) and (result[length(result)] <> AnsiChar(TPath.DirectorySeparatorChar)) then result := result + AnsiChar(TPath.DirectorySeparatorChar);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALGetModulePathW: String;
begin
  var ModName: array[0..MAX_PATH] of Char;
  SetString(Result, ModName, Winapi.Windows.GetModuleFileNameW(HInstance, ModName, SizeOf(ModName)));
  If ALPosW('\\?\',result) = 1 then delete(Result,1,4);
  Result:=ALExtractFilePath(Result);
  If (length(result) > 0) and (result[length(result)] <> TPath.DirectorySeparatorChar) then result := result + TPath.DirectorySeparatorChar;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{******************************************************}
function ALGetFileSize(const FileName : string) : Int64;
begin
  Result := Tfile.GetSize(FileName);
end;

{**********************}
{$IF defined(MSWINDOWS)}
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
          Result := ALIntToStrA(HiWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrA(LoWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrA(HiWord(FI.dwFileVersionLS)) +'.'+ ALIntToStrA(LoWord(FI.dwFileVersionLS));
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Function  AlGetFileVersion(const AFileName: string): String;
var
  FileName: String;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := '';
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSizeW(PChar(FileName), Wnd);
  if InfoSize <> 0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfoW(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := ALIntToStrW(HiWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrW(LoWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrW(HiWord(FI.dwFileVersionLS)) +'.'+ ALIntToStrW(LoWord(FI.dwFileVersionLS));
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALGetFileCreationDateTime(const aFileName: String): TDateTime;
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALGetFileLastWriteDateTime(const aFileName: String): TDateTime;
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALGetFileLastAccessDateTime(const aFileName: String): TDateTime;
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Procedure ALSetFileCreationDateTime(Const aFileName: String; Const aCreationDateTime: TDateTime);
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Procedure ALSetFileLastWriteDateTime(Const aFileName: String; Const aLastWriteDateTime: TDateTime);
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Procedure ALSetFileLastAccessDateTime(Const aFileName: String; Const aLastAccessDateTime: TDateTime);
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
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALIsDirectoryEmpty(const directory: ansiString): boolean;
begin
  Result := PathIsDirectoryEmptyA(PansiChar(directory));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function ALIsDirectoryEmpty(const directory: String): boolean;
begin
  Result := PathIsDirectoryEmptyW(PChar(directory));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALFileExists(const Path: ansiString): boolean;
begin
  result := PathFileExistsA(PansiChar(Path)) and (not PathIsDirectoryA(PansiChar(Path)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALFileExists(const Path: String): boolean;
begin
  result := PathFileExistsW(PChar(Path)) and (not PathIsDirectoryW(PChar(Path)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALDirectoryExists(const Directory: Ansistring): Boolean;
begin
  result := PathFileExistsA(PansiChar(Directory)) and (PathIsDirectoryA(PansiChar(Directory)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALDirectoryExists(const Directory: string): Boolean;
begin
  result := PathFileExistsW(PChar(Directory)) and (PathIsDirectoryW(PChar(Directory)));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALCreateDir(const Dir: Ansistring): Boolean;
begin
  Result := CreateDirectoryA(PAnsiChar(Dir), nil);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALCreateDir(const Dir: string): Boolean;
begin
  Result := CreateDirectoryW(PChar(Dir), nil);
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALRemoveDir(const Dir: Ansistring): Boolean;
begin
  Result := RemoveDirectoryA(PansiChar(Dir));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALRemoveDir(const Dir: string): Boolean;
begin
  Result := RemoveDirectoryW(PChar(Dir));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALDeleteFile(const FileName: Ansistring): Boolean;
begin
  Result := DeleteFileA(PAnsiChar(FileName));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALDeleteFile(const FileName: String): Boolean;
begin
  Result := DeleteFileW(PChar(FileName));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALRenameFileA(const OldName, NewName: ansistring): Boolean;
begin
  Result := MoveFileA(PansiChar(OldName), PansiChar(NewName));
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
function  ALRenameFileW(const OldName, NewName: String): Boolean;
begin
  Result := MoveFileW(PChar(OldName), PChar(NewName));
end;
{$ENDIF}

initialization
  {$IF defined(DEBUG)}
  //ALLog('Alcinoe.Files','initialization');
  //ALLog('ALGetAppDataPathW', ALGetAppDataPathW);
  //ALLog('ALGetTempPathW', ALGetTempPathW);
  //ALLog('ALGetCachePathW', ALGetCachePathW);
  {$ENDIF}

end.