{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclVersionCtrlCVSImpl.pas                                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclVersionCtrlCVSImpl;

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
  TJclVersionControlCVS = class (TJclVersionControlPlugin)
  private
    FTortoiseCVSAct: string;
  protected
    function GetSupportedActionTypes: TJclVersionControlActionTypes; override;
    function GetFileActions(const FileName: TFileName): TJclVersionControlActionTypes; override;
    function GetSandboxActions(const SdBxName: TFileName): TJclVersionControlActionTypes; override;
    function GetEnabled: Boolean; override;
    function GetName: string; override;
  public
    constructor Create; override;
    function GetSandboxNames(const FileName: TFileName; SdBxNames: TStrings): Boolean; override;
    function ExecuteAction(const FileName: TFileName;
      const Action: TJclVersionControlActionType): Boolean; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\vcl';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclVclResources,
  JclFileUtils, JclSysUtils, JclRegistry, JclStrings;

const
//  JclVersionCtrlCVSTrtseShlDLL = 'TrtseShl.dll';
  JclVersionCtrlCVSRegKeyName = 'SOFTWARE\TortoiseCVS';
  JclVersionCtrlCVSRegValueName = 'RootDir';
  JclVersionCtrlCVSTortoiseAct = 'TortoiseAct.exe';
  JclVersionCtrlCVSDirectory = 'CVS\';
  JclVersionCtrlCVSEntriesFile = 'Entries';

  JclVersionControlCVSAddVerb = 'CVSAdd';
  JclVersionControlCVSAddRecurseVerb = 'CVSAddRecursive';
  JclVersionControlCVSAnnotateVerb = 'CVSAnnotate';
  JclVersionControlCVSBranchVerb = 'CVSBranch';
  JclVersionControlCVSCheckOutVerb = 'CVSCheckOut';
  JclVersionControlCVSCommitVerb = 'CVSCommitDialog';
  JclVersionControlCVSDiffVerb = 'CVSDiff';
  JclVersionControlCVSGraphVerb = 'CVSRevisionGraph';
  JclVersionControlCVSLogVerb = 'CVSLog';
  JclVersionControlCVSEditVerb = 'CVSEdit';
  JclVersionControlCVSListEditorsVerb = 'CVSListEditors';
  JclVersionControlCVSTagVerb = 'CVSTag';
  JclVersionControlCVSUpdateVerb = 'CVSUpdate';
  JclVersionControlCVSUpdateDialogVerb = 'CVSUpdateDialog';
  JclVersionControlCVSUnEditVerb = 'CVSUnedit';

//=== TJclVersionControlCVS ==================================================

constructor TJclVersionControlCVS.Create;
begin
  inherited Create;
  FTortoiseCVSAct := RegReadStringDef(HKLM, JclVersionCtrlCVSRegKeyName,
    JclVersionCtrlCVSRegValueName, '');

  if FTortoiseCVSAct <> '' then
    FTortoiseCVSAct := PathAddSeparator(FTortoiseCVSAct) + JclVersionCtrlCVSTortoiseAct;
end;

function TJclVersionControlCVS.ExecuteAction(const FileName: TFileName;
  const Action: TJclVersionControlActionType): Boolean;

  function CallTortoiseCVSAct(const ActionName: string): Boolean;
  var
    StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    CurrentDir, CommandLine: TFileName;
  begin
    ResetMemory(StartupInfo, SizeOf(TStartupInfo));
    ResetMemory(ProcessInfo, SizeOf(TProcessInformation));
    startupInfo.cb := SizeOf(TStartupInfo);
    startupInfo.dwFlags := STARTF_USESHOWWINDOW;
    startupInfo.wShowWindow := SW_SHOW;

    if FileName = '' then
      raise EJclVersionControlError.Create(RsEEmptyFileName);
    if not Enabled then
      raise EJclVersionControlError.Create(RsENoTortoiseCVS);

    if FileName[Length(FileName)] = DirDelimiter then
      CurrentDir := FileName
    else
      CurrentDir := ExtractFilePath(FileName);
      
    CommandLine := Format('%s %s -l "%s"', [FTortoiseCVSAct, ActionName, PathRemoveSeparator(FileName)]);

    Result := CreateProcess(nil, PChar(CommandLine), nil,
      nil, False, 0, nil, PChar(CurrentDir), StartupInfo, ProcessInfo);

    if Result then
    begin
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
    Result := False;
  end;
begin
  case Action of
    vcaAdd:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAddVerb);
    vcaAddSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAddRecurseVerb);
    vcaBlame:
      Result := CallTortoiseCVSAct(JclVersionControlCVSAnnotateVerb);
    vcaBranch,
    vcaBranchSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSBranchVerb);
    vcaCheckOutSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSCheckOutVerb);
    vcaCommit,
    vcaCommitSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSCommitVerb);
    vcaDiff:
      Result := CallTortoiseCVSAct(JclVersionControlCVSDiffVerb);
    vcaGraph:
      Result := CallTortoiseCVSAct(JclVersionControlCVSGraphVerb);
    vcaLog,
    vcaLogSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSLogVerb);
    vcaLock,
    vcaLockSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSEditVerb);
    vcaStatus,
    vcaStatusSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSListEditorsVerb);
    vcaTag,
    vcaTagSandBox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSTagVerb);
    vcaUpdate,
    vcaUpdateSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUpdateVerb);
    vcaUpdateTo,
    vcaUpdateSandboxTo:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUpdateDialogVerb);
    vcaUnlock,
    vcaUnlockSandbox:
      Result := CallTortoiseCVSAct(JclVersionControlCVSUnEditVerb);
    else
      Result := inherited ExecuteAction(FileName, Action);
  end;
end;

function TJclVersionControlCVS.GetEnabled: Boolean;
begin
  Result := FTortoiseCVSAct <> '';
end;

function TJclVersionControlCVS.GetFileActions(
  const FileName: TFileName): TJclVersionControlActionTypes;
var
  CvsDirectory, EntriesFileName: TFileName;
  Entries: TStrings;
  Index: Integer;
  FileNameLine: string;
  Added: Boolean;
begin
  Result := inherited GetFileActions(FileName);

  CvsDirectory := PathAddSeparator(ExtractFilePath(FileName)) + JclVersionCtrlCVSDirectory;
  FileNameLine := Format('/%s/', [ExtractFileName(AnsiUpperCase(FileName))]);

  if DirectoryExists(CvsDirectory) and Enabled then
  begin
    Entries := TStringList.Create;
    try
      EntriesFileName := CvsDirectory + JclVersionCtrlCVSEntriesFile;

      if FileExists(EntriesFileName) then
      begin
        Entries.LoadFromFile(EntriesFileName);
        Added := False;
        for Index := 0 to Entries.Count - 1 do
          if Pos(FileNameLine, StrUpper(Entries.Strings[Index])) = 1 then
        begin
          Added := True;
          Break;
        end;

        if Added then
        // TODO: check modifications
          Result := Result + [vcaBlame, vcaBranch, vcaCommit, vcaDiff, vcaGraph,
            vcaLog, vcaLock, vcaStatus, vcaTag, vcaUpdate, vcaUpdateTo, vcaUnlock]
        else
          Result := Result + [vcaAdd];
      end;
    finally
      Entries.Free;
    end;
  end;
end;

function TJclVersionControlCVS.GetSupportedActionTypes:
    TJclVersionControlActionTypes;
begin
  Result := inherited GetSupportedActionTypes;
  if Enabled then
    Result := Result + [vcaAdd, vcaAddSandbox, vcaBlame, vcaBranch,
    vcaBranchSandbox, vcaCheckOutSandbox, vcaCommit, vcaCommitSandbox,
    vcaDiff, vcaGraph, vcaLog, vcaLogSandbox, vcaLock, vcaLockSandbox,
    vcaStatus, vcaStatusSandbox, vcaTag, vcaTagSandBox, vcaUpdate,
    vcaUpdateSandbox, vcaUpdateTo, vcaUpdateSandboxTo, vcaUnlock, vcaUnlockSandbox];
end;

function TJclVersionControlCVS.GetName: string;
begin
  Result := LoadResString(@RsVersionCtrlCVSName);
end;

function TJclVersionControlCVS.GetSandboxActions(
  const SdBxName: TFileName): TJclVersionControlActionTypes;
var
  CvsDirectory: TFileName;
begin
  Result := inherited GetSandboxActions(SdBxName);

  CvsDirectory := sdBxName + JclVersionCtrlCvsDirectory;

  if Enabled then
  begin
    if DirectoryExists(CvsDirectory) then
      Result := Result + [vcaAddSandbox, vcaBranchSandbox, vcaCommitSandbox,
        vcaLogSandbox, vcaLockSandbox, vcaStatusSandbox, vcaTagSandBox,
        vcaUpdateSandbox, vcaUpdateSandboxTo, vcaUnlockSandbox]
    else
      Result := Result + [vcaCheckOutSandbox];
  end;
end;

function TJclVersionControlCVS.GetSandboxNames(const FileName: TFileName;
  SdBxNames: TStrings): Boolean;
var
  DirectoryName: TFileName;
  Index: Integer;
begin
  Result := True;

  SdBxNames.BeginUpdate;
  try
    SdBxNames.Clear;

    if Enabled then
      for Index := Length(FileName) downto 1 do
        if FileName[Index] = DirDelimiter then
    begin
      DirectoryName := Copy(FileName, 1, Index);
      if DirectoryExists(DirectoryName + JclVersionCtrlCVSDirectory) then
        SdBxNames.Add(DirectoryName);
    end;

    if SdBxNames.Count = 0 then
      Result := inherited GetSandboxNames(FileName, SdBxNames);
  finally
    SdBxNames.EndUpdate;
  end;
end;


initialization

  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterVersionControlPluginClass(TJclVersionControlCVS);

finalization

  UnregisterVersionControlPluginClass(TJclVersionControlCVS);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
