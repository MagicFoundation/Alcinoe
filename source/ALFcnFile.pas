{*************************************************************
www:          http://sourceforge.net/projects/alcinoe/              
svn:          https://alcinoe.svn.sourceforge.net/svnroot/alcinoe              
Author(s):    Stéphane Vander Clock (svanderclock@arkadia.com)
Sponsor(s):   Arkadia SA (http://www.arkadia.com)

product:      Alcinoe File Management Routines
Version:      4.00

Description:  Alcinoe file Management Routines

Legal issues: Copyright (C) 1999-2012 by Arkadia Software Engineering

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

Note :        I know that most of the function here will be more efficient
              to be in unicode, but i leave it in AnsiString for the compatiblity
              with the Alcinoe Framework that is in AnsiString (UTF8)

Link :

* Please send all your feedback to svanderclock@arkadia.com
* If you have downloaded this source from a website different from 
  sourceforge.net, please get the last version on http://sourceforge.net/projects/alcinoe/
* Please, help us to keep the development of these components free by 
  promoting the sponsor on http://www.arkadia.com/html/alcinoe_like.html
**************************************************************}
unit ALFcnFile;

interface

Function  AlEmptyDirectory(Directory: ansiString;
                           SubDirectory: Boolean;
                           Const RemoveEmptySubDirectory: Boolean = True;
                           Const FileNameMask: ansiString = '*';
                           Const MinFileAge: TdateTime = 0): Boolean;
Function  AlCopyDirectory(SrcDirectory,
                          DestDirectory: ansiString;
                          SubDirectory: Boolean;
                          Const FileNameMask: ansiString = '*';
                          Const ErraseIfExist: Boolean = False): Boolean;
function  ALGetModuleName: ansistring;
function  ALGetModuleFileNameWithoutExtension: ansistring;
function  ALGetModulePath: ansiString;
Function  AlGetFileSize(const AFileName: ansistring): int64;
Function  AlGetFileVersion(const AFileName: ansistring): ansiString;
function  ALGetFileCreationDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastWriteDateTime(const aFileName: Ansistring): TDateTime;
function  ALGetFileLastAccessDateTime(const aFileName: Ansistring): TDateTime;
Procedure ALSetFileCreationDateTime(Const aFileName: Ansistring; Const aCreationDateTime: TDateTime);
function  ALIsDirectoryEmpty(const directory: ansiString) : boolean;

implementation

uses Windows,
     Masks,
     sysutils,
     alFcnString;

{**********************************************}
Function AlEmptyDirectory(Directory: ansiString;
                          SubDirectory: Boolean;
                          Const RemoveEmptySubDirectory: Boolean = True;
                          Const FileNameMask: ansiString = '*';
                          Const MinFileAge: TdateTime = 0): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  Result := True;
  Directory := ALIncludeTrailingPathDelimiter(Directory);
  if FindFirst(string(Directory) + '*', faAnyFile	, sr) = 0 then begin
    Try
      repeat
        If (sr.Name <> '.') and (sr.Name <> '..') Then Begin
          If ((sr.Attr and faDirectory) <> 0) then begin
            If SubDirectory then begin
              Result := AlEmptyDirectory(Directory + ansistring(sr.Name),
                                         True,
                                         RemoveEmptySubDirectory,
                                         fileNameMask,
                                         MinFileAge) and Result;
              If RemoveEmptySubDirectory then begin
                Abool := RemoveDir(string(Directory) + sr.Name);
                If result and (fileNameMask = '*') then Result := Abool;
              end;
            end;
          end
          else If ((FileNameMask = '*') or
                   MatchesMask(sr.Name, string(FileNameMask)))
                  and
                  ((MinFileAge<=0) or
                   (FileDateToDateTime(sr.Time) < MinFileAge))
          then begin
            Result := Deletefile(string(Directory) + sr.Name) and result;
          end;
        end;
      until FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_PLATFORM ON}
end;

{************************************}
Function AlCopyDirectory(SrcDirectory,
                         DestDirectory: ansiString;
                         SubDirectory: Boolean;
                         Const FileNameMask: ansiString = '*';
                         Const ErraseIfExist: Boolean = False): Boolean;
var sr: TSearchRec;
    aBool: Boolean;
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
            If SubDirectory and
               (not AlCopyDirectory(SrcDirectory + ansiString(sr.Name),
                                    DestDirectory + ansiString(sr.Name),
                                    SubDirectory,
                                    FileNameMask,
                                    ErraseIfExist))
            then Result := False;
          end
          else If ((FileNameMask = '*') or
                   MatchesMask(sr.Name, string(FileNameMask))) then begin
            If (not fileExists(String(DestDirectory) + sr.Name)) or ErraseIfExist then abool := Copyfile(PChar(String(SrcDirectory) + sr.Name),
                                                                                                         PChar(String(DestDirectory) + sr.Name),
                                                                                                         not ErraseIfExist)
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

{**********************************************************}
Function  AlGetFileSize(const AFileName: ansistring): int64;
var
  Handle: THandle;
  FindData: TWin32FindDataA;
begin
  Handle := FindFirstFileA(PAnsiChar(AFileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
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
  SetString(Result, ModName, Windows.GetModuleFileNameA(HInstance, ModName, SizeOf(ModName)));
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
     (not Windows.FindClose(aHandle)) or
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
     (not Windows.FindClose(aHandle)) or
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
     (not Windows.FindClose(aHandle)) or
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
  aHandle := sysUtils.fileOpen(String(aFileName), fmOpenWrite or fmShareDenyNone);
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

{*****************************************************************}
function ALIsDirectoryEmpty(const directory: ansiString) : boolean;
var SR: TSearchRec;
begin
  Result := True;
  if FindFirst(IncludeTrailingPathDelimiter(String(Directory)) + '*', faAnyFile, sr) = 0 then begin
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
