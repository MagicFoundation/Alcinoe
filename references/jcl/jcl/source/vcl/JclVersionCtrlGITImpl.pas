{ ************************************************************************************************** }
{ }
{ Project JEDI Code Library (JCL) }
{ }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the }
{ License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF }
{ ANY KIND, either express or implied. See the License for the specific language governing rights }
{ and limitations under the License. }
{ }
{ The Original Code is JclVersionCtrlGITImpl.pas }
{ }
{ The Initial Developer of the Original Code is Florent Ouchet. }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. }
{ Portions created by Elahn Ientile are Copyright (C) of Elahn Ientile. }
{ }
{ }
{ ************************************************************************************************** }
{ }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{ }
{ ************************************************************************************************** }

unit JclVersionCtrlGITImpl;

{$I jcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, Winapi.Windows, System.Classes, Vcl.Graphics,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Windows, Classes, Graphics,
  {$ENDIF ~HAS_UNITSCOPE}
  JclVersionControl;

type
  TJclVersionControlGIT = class(TJclVersionControlPlugin)
  private
    FTortoiseGITProc: string;
  protected
    function GetEnabled: Boolean; override;
    function GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes; override;
    function GetGitBaseDir(const FileName: TFileName): string;
    function GetName: string; override;
    function GetSupportedActionTypes: TJclVersionControlActionTypes; override;
    function IsGitSupportedDir(const FileDir: string): Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ExecuteAction(const FileName: TFileName; const Action: TJclVersionControlActionType): Boolean; override;
    function GetSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean; override;
  end;

{$IFDEF UNITVERSIONING}

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$'; Date: '$Date$';
    LogPath: 'JCL\source\vcl'; Extra: ''; Data: nil);
{$ENDIF UNITVERSIONING}

implementation

uses
  JclVclResources,
  JclFileUtils, JclSysInfo, JclSysUtils, JclRegistry, JclStrings;

const
  JclVersionCtrlRegKeyName = 'SOFTWARE\TortoiseGIT';
  JclVersionCtrlRegValueName = 'ProcPath';
  JclVersionCtrlGITAddVerb = 'add';
  JclVersionCtrlGITBlameVerb = 'blame';
  // JclVersionCtrlGITBranchVerb = 'copy';
  JclVersionCtrlGITCheckOutVerb = 'checkout';
  JclVersionCtrlGITCommitVerb = 'commit';
  JclVersionCtrlGITDiffVerb = 'diff';
  // JclVersionCtrlGITGraphVerb = 'revisiongraph';
  JclVersionCtrlGITLogVerb = 'log';
  // JclVersionCtrlGITLockVerb = 'lock';
  JclVersionCtrlGITMergeVerb = 'merge';
  JclVersionCtrlGITRenameVerb = 'rename';
  JclVersionCtrlGITRepoBrowserVerb = 'repobrowser';
  JclVersionCtrlGITRevertVerb = 'revert';
  JclVersionCtrlGITStatusVerb = 'repostatus';
  // JclVersionCtrlGITTagVerb = 'copy';
  JclVersionCtrlGITUpdateVerb = 'sync';
  // JclVersionCtrlGITUpdateToParam = '/rev';
  // JclVersionCtrlGITUnlockVerb = 'unlock';
  JclVersionCtrlGITDirectory1 = '.git\';
  JclVersionCtrlGITIndexFile = 'index';
  JclVersionCtrlGITIgnoreFile = '.gitignore';
  JclVersionCtrlGITLinkFile = '.git';

  JclVersionCtrlGITDirectories: array [0 .. 0] of string = (JclVersionCtrlGITDirectory1);

  // === TJclVersionControlGIT ==================================================

constructor TJclVersionControlGIT.Create;
var
  SaveAcc: TJclRegWOW64Access;
begin
  inherited Create;

  if IsWindows64 then
  begin
    // on 64 bit machines look in the 64bit section of registy for tortoise GIT (64bit) registry stuff
    SaveAcc := RegGetWOW64AccessMode;
    try
      RegSetWOW64AccessMode(ra64Key);
      FTortoiseGITProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
    finally
      RegSetWOW64AccessMode(SaveAcc);
    end;
    if FTortoiseGITProc = '' then
      // when the 64bit Version is not found try to find the 32bit version
      FTortoiseGITProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
  end
  else
    FTortoiseGITProc := RegReadStringDef(HKLM, JclVersionCtrlRegKeyName, JclVersionCtrlRegValueName, '');
end;

destructor TJclVersionControlGIT.Destroy;
begin
  inherited Destroy;
end;

function TJclVersionControlGIT.ExecuteAction(const FileName: TFileName;
  const Action: TJclVersionControlActionType): Boolean;

  function CallTortoiseGITProc(const ActionName: string; const Param: string = ''): Boolean;
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CurrentDir, CommandLine: string;
  begin
    ResetMemory(StartupInfo, SizeOf(TStartupInfo));
    ResetMemory(ProcessInfo, SizeOf(TProcessInformation));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOW;

    if FileName = '' then
      raise EJclVersionControlError.Create(RsEEmptyFileName);
    if not Enabled then
      raise EJclVersionControlError.Create(RsENoTortoiseGIT);

    if FileName[Length(FileName)] = DirDelimiter then
      CurrentDir := FileName
    else
      CurrentDir := ExtractFilePath(FileName);
    CommandLine := Format('%s /command:%s /path:"%s" %s /notempfile', [FTortoiseGITProc, ActionName, FileName, Param]);

    Result := CreateProcess(nil, PChar(CommandLine), nil, nil, False, 0, nil, PChar(CurrentDir), StartupInfo,
      ProcessInfo);

    if Result then
    begin
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  end;

begin
  case Action of
    vcaAdd, vcaAddSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITAddVerb);
    vcaBlame:
      Result := CallTortoiseGITProc(JclVersionCtrlGITBlameVerb);
    // vcaBranch,
    // vcaBranchSandbox:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITBranchVerb);
    vcaCheckOutSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITCheckOutVerb);
    vcaCommit, vcaCommitSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITCommitVerb);
    vcaDiff:
      Result := CallTortoiseGITProc(JclVersionCtrlGITDiffVerb);
    // vcaGraph:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITGraphVerb);
    vcaLog, vcaLogSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITLogVerb);
    // vcaLock,
    // vcaLockSandbox:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITLockVerb);
    vcaMerge, vcaMergeSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITMergeVerb);
    vcaRename:
      Result := CallTortoiseGITProc(JclVersionCtrlGITRenameVerb);
    vcaRepoBrowser:
      Result := CallTortoiseGITProc(JclVersionCtrlGITRepoBrowserVerb);
    vcaRevert, vcaRevertSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITRevertVerb);
    vcaStatus, vcaStatusSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITStatusVerb);
    // vcaTag,
    // vcaTagSandBox:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITTagVerb);
    vcaUpdate, vcaUpdateSandbox:
      Result := CallTortoiseGITProc(JclVersionCtrlGITUpdateVerb);
    // vcaUpdateTo,
    // vcaUpdateSandboxTo:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITUpdateVerb, JclVersionCtrlGITUpdateToParam);
    // vcaUnlock,
    // vcaUnlockSandbox:
    // Result := CallTortoiseGITProc(JclVersionCtrlGITUnlockVerb);
  else
    Result := inherited ExecuteAction(FileName, Action);
  end;
end;

function TJclVersionControlGIT.GetEnabled: Boolean;
begin
  Result := FTortoiseGITProc <> '';
end;

function TJclVersionControlGIT.GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes;
begin
  Result := inherited GetFileActions(FileName);

  if Enabled then
  begin
    if IsGitSupportedDir(ExtractFilePath(FileName)) then
      Result := GetSupportedActionTypes;
  end;
end;

function TJclVersionControlGIT.GetGitBaseDir(const FileName: TFileName): string;
var
  DirectoryName: String;
  IndexFileName: Integer;
  IndexDir: Integer;
begin
  Result := '';
  if Enabled then
    for IndexFileName := Length(FileName) downto 1 do
      if FileName[IndexFileName] = DirDelimiter then
      begin
        DirectoryName := Copy(FileName, 1, IndexFileName);
        for IndexDir := Low(JclVersionCtrlGITDirectories) to High(JclVersionCtrlGITDirectories) do
          if FileExists(DirectoryName + JclVersionCtrlGITDirectories[IndexDir] + JclVersionCtrlGITIndexFile) then
          begin
            Result := DirectoryName;
            Exit;
          end;

        //Account for submodules and multiple worktree's
        if FileExists(DirectoryName + JclVersionCtrlGITLinkFile) then
        begin
          Result := DirectoryName;
          Exit;
        end;
      end;
end;

function TJclVersionControlGIT.GetName: string;
begin
  Result := LoadResString(@RsVersionCtrlGITName);
end;

function TJclVersionControlGIT.GetSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean;
var
  DirectoryName: string;
  List: TStringList;
  i: Integer;
  Found: Boolean;
  IndexDir: Integer;
begin
  Result := True;
  Found := False;

  SdBxNames.BeginUpdate;
  try
    SdBxNames.Clear;

    if Enabled then
    begin
      List := TStringList.Create;
      try
        StrIToStrings(ExtractFilePath(FileName), DirDelimiter, List, True);
        for i := List.Count downto 1 do
        begin
          DirectoryName := StringsToStr(List, DirDelimiter, i)+DirDelimiter;
          SdBxNames.Add(DirectoryName);
          for IndexDir := Low(JclVersionCtrlGITDirectories) to High(JclVersionCtrlGITDirectories) do
            if DirectoryExists(DirectoryName + JclVersionCtrlGITDirectories[IndexDir]) then
            begin
              // When the first directory is found stop searching
              Found := True;
              break;
            end;

          if FileExists(DirectoryName + DirDelimiter + JclVersionCtrlGITLinkFile) then
          begin
            // When the first .git file is found stop searching
            Found := True;
            Break;
          end;

        end;
        if not Found then // if no direcory is found delete the list
          SdBxNames.Clear;
      finally
        List.Free;
      end;
    end;
  finally
    SdBxNames.EndUpdate;
  end;

  if SdBxNames.Count = 0 then
    Result := inherited GetSandboxNames(FileName, SdBxNames);
end;

function TJclVersionControlGIT.GetSupportedActionTypes: TJclVersionControlActionTypes;
begin
  Result := inherited GetSupportedActionTypes;
  if Enabled then
    Result := Result + [vcaAdd, { vcaAddSandbox, } vcaBlame, { vcaBranch,
      vcaBranchSandbox, vcaCheckOutSandbox, } vcaCommit,
    { vcaCommitSandbox, } vcaDiff,
    { vcaGraph, } vcaLog, { vcaLogSandbox, vcaLock, vcaLockSandbox, } vcaMerge,
    { vcaMergeSandbox, } vcaRename, vcaRepoBrowser, vcaRevert,
    { vcaRevertSandbox, }
    vcaStatus, { vcaStatusSandbox, vcaTag, vcaTagSandBox, } vcaUpdate { ,
      vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox } ];
end;

function TJclVersionControlGIT.IsGitSupportedDir(const FileDir: string): Boolean;
var
  BaseDir: String;
  IgnoreList: TStringList;
  IgnoreDir : String;
  i: Integer;
begin
  Result := False;
  if Enabled then
  begin
    BaseDir := GetGitBaseDir(FileDir);
    if (BaseDir <> '') then
    begin
      if FileExists(BaseDir + JclVersionCtrlGITIgnoreFile) then
      begin
        IgnoreList := TStringList.Create;
        try
          IgnoreList.LoadFromFile(BaseDir + JclVersionCtrlGITIgnoreFile);
          for i := 0 to IgnoreList.Count - 1 do
            if (IgnoreList[i] <> '') then
            begin
              IgnoreDir := PathAppend(BaseDir, StrReplaceChar(IgnoreList[i], '/', DirDelimiter));
              if PathIsEqualOrChild(FileDir, IgnoreDir) then
                Exit;
            end;
        finally
          IgnoreList.Free;
        end;
      end;
      Result := True;
    end;
  end;
end;

initialization

{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}
RegisterVersionControlPluginClass(TJclVersionControlGIT);

finalization

UnregisterVersionControlPluginClass(TJclVersionControlGIT);
{$IFDEF UNITVERSIONING}
UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
