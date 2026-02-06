unit Alcinoe.FileUtils;

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
Function  AlGetFileVersionA(const AFileName: string): ansiString; overload;
Function  AlGetFileVersionW(const AFileName: string): String; overload;
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
Function AlGetFileVersionA(const AFileName: string): ansiString;
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
          Result := ALIntToStrA(HiWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrA(LoWord(FI.dwFileVersionMS)) +'.'+ ALIntToStrA(HiWord(FI.dwFileVersionLS)) +'.'+ ALIntToStrA(LoWord(FI.dwFileVersionLS));
    finally
      FreeMem(VerBuf);
    end;
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(MSWINDOWS)}
Function  AlGetFileVersionW(const AFileName: string): String;
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

initialization
  {$IF defined(DEBUG)}
  //ALLog('Alcinoe.FileUtils','initialization');
  //ALLog('ALGetAppDataPathW', ALGetAppDataPathW);
  //ALLog('ALGetTempPathW', ALGetTempPathW);
  //ALLog('ALGetCachePathW', ALGetCachePathW);
  {$ENDIF}

end.