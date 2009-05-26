{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe File Management Routines
Version:      3.50

Description:  Alcinoe file Management Routines

Legal issues: Copyright (C) 1999-2009 by Arkadia Software Engineering

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

Link :

Please send all your feedback to svanderclock@arkadia.com
**************************************************************}
unit ALFcnFile;

interface

uses Windows,
     sysutils;

Function  AlEmptyDirectory(Directory: String; SubDirectory: Boolean; Const RemoveEmptySubDirectory: Boolean = True; Const FileNameMask: String = '*.*'; Const MinFileAge: TdateTime = 0): Boolean;
Function  AlCopyDirectory(SrcDirectory, DestDirectory: String; SubDirectory: Boolean; Const FileNameMask: String = '*.*'; Const ErraseIfExist: Boolean = False): Boolean;
function  ALGetModuleName: string;
function  ALGetModulePath: String;
Function  AlGetFileVersion(const AFileName: string): String;
Function  ALMakeGoodEndPath(Rep : string):string;
function  ALGetFileCreationDateTime(const aFileName: string): TDateTime;
function  ALGetFileLastWriteDateTime(const aFileName: string): TDateTime;
function  ALGetFileLastAccessDateTime(const aFileName: string): TDateTime;
Procedure ALSetFileCreationDateTime(Const aFileName: string; Const aCreationDateTime: TDateTime);
function  ALIsDirectoryEmpty(const directory : string) : boolean;

implementation

uses Masks,
     DateUtils,
     alFcnString;

{******************************************}
Function AlEmptyDirectory(Directory: String;
                          SubDirectory: Boolean;
                          Const RemoveEmptySubDirectory: Boolean = True;
                          Const FileNameMask: String = '*.*';
                          Const MinFileAge: TdateTime = 0): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
begin
  Result := True;
  Directory := ALMakeGoodEndPath(Directory);
  if FindFirst(Directory + '*.*', faAnyFile	, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then begin
              AlEmptyDirectory(Directory + sr.Name, True, RemoveEmptySubDirectory, fileNameMask, MinFileAge);
              If RemoveEmptySubDirectory then begin
                Abool := RemoveDir(Directory + sr.Name);
                If result and (fileNameMask = '*.*') then Result := Abool;
              end;
            end;
          end
          else If (
                   (FileNameMask = '*.*') or
                   MatchesMask(sr.Name, FileNameMask)
                  )
                  and
                  (
                   (MinFileAge<=0) or
                   (FileDateToDateTime(sr.Time) < MinFileAge)
                  )
          then begin
            abool := Deletefile(Directory + sr.Name);
            If result then result := aBool;
          end;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end
end;

{************************************}
Function AlCopyDirectory(SrcDirectory,
                         DestDirectory: String;
                         SubDirectory: Boolean;
                         Const FileNameMask: String = '*.*';
                         Const ErraseIfExist: Boolean = False): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
begin
  Result := True;
  SrcDirectory := ALMakeGoodEndPath(SrcDirectory);
  DestDirectory := ALMakeGoodEndPath(DestDirectory);
  If not DirectoryExists(DestDirectory) and (not Createdir(DestDirectory)) then begin
    result := False;
    exit;
  end;

  if FindFirst(SrcDirectory + '*.*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory and
               (
                not AlCopyDirectory(
                                    SrcDirectory + sr.Name,
                                    DestDirectory + sr.Name,
                                    SubDirectory,
                                    FileNameMask,
                                    ErraseIfExist
                                   )
               )
            then Result := False;
          end
          else If (
                   (FileNameMask = '*.*') or
                   MatchesMask(sr.Name, FileNameMask)
                  )
          then begin
            If (not fileExists(DestDirectory + sr.Name)) or ErraseIfExist
              then abool := Copyfile(
                                     Pchar(SrcDirectory + sr.Name),
                                     Pchar(DestDirectory + sr.Name),
                                     not ErraseIfExist
                                    )
              else aBool := True;
            If result then result := aBool;
          end;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end
end;

{*********************************************************}
Function AlGetFileVersion(const AFileName: string): String;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := '';
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := inttostr(HiWord(FI.dwFileVersionMS)) +'.'+ inttostr(LoWord(FI.dwFileVersionMS)) +'.'+ inttostr(HiWord(FI.dwFileVersionLS)) +'.'+ inttostr(LoWord(FI.dwFileVersionLS));
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

{**********************************************}
Function ALMakeGoodEndPath(Rep : string):string;
begin
  result :=  IncludeTrailingPathDelimiter(Rep);
end;

{*******************************}
function ALGetModuleName: string;
var ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Windows.GetModuleFileName(HInstance, ModName, SizeOf(ModName)));
  If ALpos('\\?\',result) = 1 then delete(Result,1,4);
end;

{*******************************}
function ALGetModulePath: String;
begin
  Result:=ExtractFilePath(ALGetModuleName);
  If (length(result) > 0) and (result[length(result)] <> '\') then result := result + '\';
end;

{**********************************************************************}
function  ALGetFileCreationDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftCreationTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then Begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

{***********************************************************************}
function  ALGetFileLastWriteDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftLastWriteTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

{************************************************************************}
function  ALGetFileLastAccessDateTime(const aFileName: string): TDateTime;
var aHandle: THandle;
    aFindData: TWin32FindData;
    aLocalFileTime: TFileTime;
    aFileDate: Integer;
begin
  aHandle := FindFirstFile(PChar(aFileName), aFindData);
  if aHandle <> INVALID_HANDLE_VALUE then begin
    Windows.FindClose(aHandle);
    if (aFindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then begin
      FileTimeToLocalFileTime(aFindData.ftLastAccessTime, aLocalFileTime);
      if FileTimeToDosDateTime(aLocalFileTime, LongRec(aFileDate).Hi, LongRec(aFileDate).Lo) then begin
        Result := filedatetodatetime(aFileDate);
        Exit;
      end;
    end;
  end;
  Result := -0.5; //invalid DateTime
end;

{***********************************************************************************************}
Procedure ALSetFileCreationDateTime(Const aFileName: string; Const aCreationDateTime: TDateTime);
Var ahandle: integer;
    aSystemTime: TsystemTime;
    afiletime: TfileTime;
Begin
  aHandle := sysUtils.fileOpen(aFileName, fmOpenWrite or fmShareDenyNone);
  if aHandle < 0 then exit;
  Try
    dateTimeToSystemTime(aCreationDateTime, aSystemTime);
    SystemTimeToFileTime(aSystemTime, aFileTime);
    LocalFileTimeToFileTime(aFileTime, aFileTime);
    setFileTime(aHandle, @aFileTime, nil, nil);
  finally
    fileClose(aHandle);
  end;
End;

{**************************************************************}
function ALIsDirectoryEmpty(const directory : string) : boolean;
var SR: TSearchRec;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(Directory) + '*', faAnyFile, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          Result := False;
          break;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end
end;

end.
