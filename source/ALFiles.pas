{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code              
Author(s):    Stéphane Vander Clock (skype/email: svanderclock@yahoo.fr)

product:      Alcinoe File Management Routines
Version:      4.01

Description:  Alcinoe file Management Routines

Legal issues: Copyright (C) 1999-2013 by Arkadia Software Engineering

              This software is provided 'as-is', without any express
              or implied warranty.  In no event will the author be
              held liable for any  damages arising from the use of
              this software.

              Permission is granted to anyone to use this software
              for any purpose, including commercial applications,
              and to alter it and redistribute it freely, subject
              to the following restrictions:

              1. The origin of this software must not be
                 misrepresented, you must not claim that you wrote
                 the original software. If you use this software in
                 a product, an acknowledgment in the product
                 documentation would be appreciated but is not
                 required.

              2. Altered source versions must be plainly marked as
                 such, and must not be misrepresented as being the
                 original software.

              3. This notice may not be removed or altered from any
                 source distribution.

              4. You must register this software by sending a picture
                 postcard to the author. Use a nice stamp and mention
                 your name, street address, EMail address and any
                 comment you like to say.

Know bug :

History :     15/04/2008: Add AlIsEmptyDirectory Function
              15/06/2012: Add XE2 support
              01/12/2012: Add ALFileExists function

Note :        I know that most of the function here will be more efficient
              to be in unicode, but i leave it in AnsiString for the compatiblity
              with the Alcinoe Framework that is in AnsiString (UTF8)

Link :

**************************************************************}
unit ALFiles;

interface

{$IF CompilerVersion >= 25} {Delphi XE4}
  {$LEGACYIFEND ON} // http://docwiki.embarcadero.com/RADStudio/XE4/en/Legacy_IFEND_(Delphi)
{$IFEND}

Function  AlEmptyDirectory(Directory: ansiString;
                           SubDirectory: Boolean;
                           const IgnoreFiles: Array of AnsiString;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: ansiString = '*';
                           Const MinFileAge: TdateTime = 0): Boolean; overload;
Function  AlEmptyDirectory(const Directory: ansiString;
                           SubDirectory: Boolean;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: ansiString = '*';
                           Const MinFileAge: TdateTime = 0): Boolean; overload;
Function  AlCopyDirectory(SrcDirectory,
                          DestDirectory: ansiString;
                          SubDirectory: Boolean;
                          Const FileNameMask: ansiString = '*';
                          Const FailIfExists: Boolean = True): Boolean;
function  ALGetModuleName: ansistring;
function  ALGetModuleFileNameWithoutExtension: ansistring;
function  ALGetModulePath: ansiString;
Function  AlGetFileSize(const AFileName: ansistring): int64;
Function  AlGetFileVersion(const AFileName: ansistring): ansiString;
function  ALGetFileCreationDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime;
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime);
function  ALIsDirectoryEmpty(const directory: ansiString): boolean;
function  ALFileExists(const Path: ansiString): boolean;
function  ALDirectoryExists(const Directory: Ansistring): Boolean;
function  ALCreateDir(const Dir: Ansistring): Boolean;
function  ALRemoveDir(const Dir: Ansistring): Boolean;
function  ALDeleteFile(const FileName: Ansistring): Boolean;
function  ALRenameFile(const OldName, NewName: ansistring): Boolean;

implementation

uses {$IF CompilerVersion >= 23} {Delphi XE2}
     Winapi.Windows,
     System.Classes,
     Winapi.ShLwApi,
     System.Masks,
     System.sysutils,
     {$ELSE}
     Windows,
     Classes,
     ShLwApi,
     Masks,
     sysutils,
     {$IFEND}
     ALStringList,
     ALString;

{***********************************************}
Function  AlEmptyDirectory(Directory: ansiString;
                           SubDirectory: Boolean;
                           const IgnoreFiles: Array of AnsiString;
                           const RemoveEmptySubDirectory: Boolean = True;
                           const FileNameMask: ansiString = '*';
                           const MinFileAge: TdateTime = 0): Boolean;
var sr: TSearchRec;
    aIgnoreFilesLst: TalStringList;
    i: integer;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  if (Directory = '') or
     (Directory = '.') or
     (Directory = '..') then raise EALException.CreateFmt('Wrong directory ("%s")', [Directory]);

  Result := True;
  Directory := ALIncludeTrailingPathDelimiter(Directory);
  aIgnoreFilesLst := TalStringList.Create;
  try
    for I := 0 to length(IgnoreFiles) - 1 do aIgnoreFilesLst.Add(ALExcludeTrailingPathDelimiter(IgnoreFiles[i]));
    aIgnoreFilesLst.Duplicates := DupIgnore;
    aIgnoreFilesLst.Sorted := True;
    if FindFirst(string(Directory) + '*', faAnyFile	, sr) = 0 then begin
      Try
        repeat
          If (sr.Name <> '.') and
             (sr.Name <> '..') and
             (aIgnoreFilesLst.IndexOf(Directory + ansistring(sr.Name)) < 0) Then Begin
            If ((sr.Attr and faDirectory) <> 0) then begin
              If SubDirectory then begin
                Result := AlEmptyDirectory(Directory + ansistring(sr.Name),
                                           True,
                                           IgnoreFiles,
                                           RemoveEmptySubDirectory,
                                           fileNameMask,
                                           MinFileAge);
                If result and RemoveEmptySubDirectory then RemoveDir(string(Directory) + sr.Name);
              end;
            end
            else If ((FileNameMask = '*') or
                     ALMatchesMask(AnsiString(sr.Name), FileNameMask))
                    and
                    ((MinFileAge<=0) or
                     (FileDateToDateTime(sr.Time) < MinFileAge))
            then Result := Deletefile(string(Directory) + sr.Name);
          end;
        until (not result) or (FindNext(sr) <> 0);
      finally
        FindClose(sr);
      end;
    end;
  finally
    aIgnoreFilesLst.Free;
  end;
  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_PLATFORM ON}
end;

{****************************************************}
Function AlEmptyDirectory(const Directory: ansiString;
                          SubDirectory: Boolean;
                          Const RemoveEmptySubDirectory: Boolean = True;
                          Const FileNameMask: ansiString = '*';
                          Const MinFileAge: TdateTime = 0): Boolean;
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
                         Const FailIfExists: Boolean = True): Boolean;
var sr: TSearchRec;
begin
  Result := True;
  SrcDirectory := ALIncludeTrailingPathDelimiter(SrcDirectory);
  DestDirectory := ALIncludeTrailingPathDelimiter(DestDirectory);
  If not DirectoryExists(string(DestDirectory)) and (not Createdir(String(DestDirectory))) then begin
    result := False;
    exit;
  end;

  if FindFirst(String(SrcDirectory) + '*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then Result := AlCopyDirectory(SrcDirectory + ansiString(sr.Name),
                                                           DestDirectory + ansiString(sr.Name),
                                                           SubDirectory,
                                                           FileNameMask,
                                                           FailIfExists);
          end
          else If (FileNameMask = '*') or
                  (ALMatchesMask(AnsiString(sr.Name), FileNameMask)) then begin
            result := CopyfileA(PAnsiChar(SrcDirectory + AnsiString(sr.Name)),
                                PAnsiChar(DestDirectory + AnsiString(sr.Name)),
                                FailIfExists);
          end;
        end;
      until (not result) or (FindNext(sr) <> 0);
    finally
      FindClose(sr);
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
    {$IF CompilerVersion >= 23}{Delphi XE2}Winapi.{$IFEND}Windows.FindClose(Handle);
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
  SetString(Result, ModName, {$IF CompilerVersion >= 23}{Delphi XE2}Winapi.{$IFEND}Windows.GetModuleFileNameA(HInstance, ModName, SizeOf(ModName)));
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
var aHandle: THandle;
    aFindData: TWin32FindDataA;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFileA(PAnsiChar(aFileName), aFindData);
  if (aHandle = INVALID_HANDLE_VALUE) or
     (not {$IF CompilerVersion >= 23}{Delphi XE2}Winapi.{$IFEND}Windows.FindClose(aHandle)) or
     (not FileTimeToLocalFileTime(aFindData.ftCreationTime, aLocalFileTime)) or
     (not FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo)) then raiselastOsError;
  Result := filedatetodatetime(aFileDate);
end;

{***************************************************************************}
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindDataA;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFileA(PAnsiChar(aFileName), aFindData);
  if (aHandle = INVALID_HANDLE_VALUE) or
     (not {$IF CompilerVersion >= 23}{Delphi XE2}Winapi.{$IFEND}Windows.FindClose(aHandle)) or
     (not FileTimeToLocalFileTime(aFindData.ftLastWriteTime, aLocalFileTime)) or
     (not FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo)) then raiselastOsError;
  Result := filedatetodatetime(aFileDate);
end;

{****************************************************************************}
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindDataA;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFileA(PAnsiChar(aFileName), aFindData);
  if (aHandle = INVALID_HANDLE_VALUE) or
     (not {$IF CompilerVersion >= 23}{Delphi XE2}Winapi.{$IFEND}Windows.FindClose(aHandle)) or
     (not FileTimeToLocalFileTime(aFindData.ftLastAccessTime, aLocalFileTime)) or
     (not FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo)) then raiselastOsError;
  Result := filedatetodatetime(aFileDate);
end;

{***************************************************************************************************}
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime);
Var ahandle: Thandle;
    aSystemTime: TsystemTime;
    afiletime: TfileTime;
Begin
  aHandle := {$IF CompilerVersion >= 23}{Delphi XE2}System.{$IFEND}sysUtils.fileOpen(String(aFileName), fmOpenWrite or fmShareDenyNone);
  if aHandle = INVALID_HANDLE_VALUE then raiseLastOsError;
  Try
    dateTimeToSystemTime(aCreationDateTime, aSystemTime);
    if (not SystemTimeToFileTime(aSystemTime, aFileTime)) or
       (not LocalFileTimeToFileTime(aFileTime, aFileTime)) or
       (not setFileTime(aHandle, @aFileTime, nil, nil)) then raiselastOsError;
  finally
    fileClose(aHandle);
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

end.
