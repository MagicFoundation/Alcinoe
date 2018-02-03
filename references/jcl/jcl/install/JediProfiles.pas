{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediProfiles.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by Florent Ouchet }
{ are Copyright (C) of Florent Ouchet. All Rights Reserved.                                        }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Core unit to manipulate multiple users' profiles at install time                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JediProfiles;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils,
  Classes,
  JediInstall;

type
  {$IFDEF MSWINDOWS}
  TJediProfile = record
    UserName: string;
    SID: string;
    LocalProfile: string;
    UserKey: HKEY;
    CloseKey: Boolean;
    UnloadKey: Boolean;
  end;
  {$ENDIF MSWINDOWS}

  TJediProfilesManager = class(TInterfacedObject, IJediProfilesManager)
  private
    FMultipleProfileMode: Boolean;
    {$IFDEF MSWINDOWS}
    FProfiles: array of TJediProfile;
    procedure LoadProfiles;
    {$ENDIF MSWINDOWS}
  public
    destructor Destroy; override;
    { IJediProfileManager }
    function CheckPrerequisites: Boolean;
    function GetMultipleProfileMode: Boolean;
    function GetProfileKey(Index: Integer): LongWord; // HKEY is Windows specific
    function GetProfileCount: Integer;
    function GetProfileName(Index: Integer): string;
    procedure SetMultipleProfileMode(Value: Boolean);
    property MultipleProfileMode: Boolean read GetMultipleProfileMode write SetMultipleProfileMode;
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  JclAnsiStrings,
  JclFileUtils,
  JclRegistry,
  JclSecurity,
  JclShell,
  JclSysInfo,
  JclWin32;

const
  RegProfileListKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList';
{$ENDIF MSWINDOWS}

//=== { TJediProfileManager } ================================================

destructor TJediProfilesManager.Destroy;
{$IFDEF MSWINDOWS}
var
  Index: Integer;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  for Index := Low(FProfiles) to High(FProfiles) do
  begin
    if FProfiles[Index].CloseKey then
    begin
      Windows.RegFlushKey(FProfiles[Index].UserKey);
      Windows.RegCloseKey(FProfiles[Index].UserKey);
    end;

    if FProfiles[Index].UnloadKey then
      Windows.RegUnLoadKey(HKUS, PChar(FProfiles[Index].SID));
  end;
  SetLength(FProfiles, 0);
  {$ENDIF MSWINDOWS}
  inherited Destroy;
end;

function TJediProfilesManager.CheckPrerequisites: Boolean;
{$IFDEF MSWINDOWS}
var
  InstallGUI: IJediInstallGUI;
  Fork: Boolean;
  Parameters: string;
  Index: Integer;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  FMultipleProfileMode := FMultipleProfileMode and IsWinNT;
  Result := not FMultipleProfileMode;
  if not Result then
  begin
    Result := IsElevated;
    if not Result then
    begin
      // attempt to fork as an administrator
      InstallGUI := InstallCore.InstallGUI;
      if Assigned(InstallGUI) then
        Fork := InstallGUI.Dialog('Installation requires administrator privilege, do you want to run installer with' +
                                  ' administrator rights?', dtConfirmation, [drYes, drNo]) = drYes
      else
        Fork := True;
      if Fork then
      begin
        Parameters := '';
        for Index := 1 to ParamCount do
          Parameters := Parameters + AnsiQuotedStr(ParamStr(Index), AnsiDoubleQuote);
        ShellExecEx(ParamStr(0), Parameters, 'runas');
        Result := False;
      end
      else
      begin
        // single profile installation for current user
        FMultipleProfileMode := False;
        Result := True;
      end;
    end;
  end;
  if FMultipleProfileMode and Result then
    LoadProfiles;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  // don't know how to enumerate profiles on Linux
  Result := not FMultipleProfileMode;
  FMultipleProfileMode := False;
  {$ENDIF UNIX}
end;

function TJediProfilesManager.GetMultipleProfileMode: Boolean;
begin
  Result := FMultipleProfileMode;
end;

function TJediProfilesManager.GetProfileCount: Integer;
begin
  {$IFDEF MSWINDOWS}
  if FMultipleProfileMode then
    Result := Length(FProfiles)
  else
  {$ENDIF MSWINDOWS}
    Result := 0;
end;

function TJediProfilesManager.GetProfileKey(Index: Integer): LongWord;
{$IFDEF MSWINDOWS}
var
  NtUserFileName: string;
  Key: HKEY;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  if FMultipleProfileMode then
  begin
    if FProfiles[Index].UserKey = 0 then
    begin
      if AnsiSameText(FProfiles[Index].UserName, GetLocalUserName) then
        FProfiles[Index].UserKey := HKCU
      else
      begin
        NtUserFileName := PathAddSeparator(FProfiles[Index].LocalProfile) + 'NTUSER.DAT';
        ExpandEnvironmentVar(NtUserFileName);
        if not RegKeyExists(HKUS, '\' + FProfiles[Index].SID) then
        begin
          EnableProcessPrivilege(True, SE_RESTORE_NAME);
          EnableProcessPrivilege(True, SE_BACKUP_NAME);
          if RegLoadKey(HKUS, PChar(FProfiles[Index].SID), PChar(NtUserFileName)) = ERROR_SUCCESS then
            FProfiles[Index].UnloadKey := True
          else
            {$IFDEF COMPILER5}
            RaiseLastWin32Error;
            {$ELSE ~COMPILER5}
            RaiseLastOSError;
            {$ENDIF ~COMPILER5}
        end;
        if RegOpenKey(HKUS, PChar(FProfiles[Index].SID), Key) = ERROR_SUCCESS then
          FProfiles[Index].CloseKey := True
        else
          raise EJclSecurityError.CreateFmt('Unable to load profile for user "%s"', [FProfiles[Index].UserName]);
        FProfiles[Index].UserKey := Key;
      end;
    end;
    Result := FProfiles[Index].UserKey;
  end
  else
    Result := HKCU;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := 0;
  {$ENDIF LINUX}
end;

function TJediProfilesManager.GetProfileName(Index: Integer): string;
begin
  {$IFDEF MSWINDOWS}
  if FMultipleProfileMode then
    Result := FProfiles[Index].UserName
  else
  {$ENDIF MSWINDOWS}
    Result := '';
end;

{$IFDEF MSWINDOWS}
procedure TJediProfilesManager.LoadProfiles;
var
  Index: Integer;
  SID: PSID;
  DataSize: Cardinal;
  Name, Domain: WideString;
  KeyName, SIDStr, ProfileDir: string;
  RegProfiles: TStrings;
begin
  if FMultipleProfileMode then
  begin
    RegProfiles := TStringList.Create;
    try
      GetMem(SID, SECURITY_MAX_SID_SIZE);
      try
        if RegGetKeyNames(HKLM, RegProfileListKey, RegProfiles) then
          for Index := 0 to RegProfiles.Count - 1 do
        begin
          KeyName := RegProfileListKey + '\' + RegProfiles.Strings[Index];
          if RegReadBinaryEx(HKLM, KeyName, 'Sid', SID^, SECURITY_MAX_SID_SIZE, DataSize, False)
            and RegReadStringEx(HKLM, KeyName, 'ProfileImagePath', ProfileDir, False) then
          begin
            try
              SIDStr := SIDToString(SID);
              LookupAccountBySid(SID, Name, Domain);
              if SameText(Domain, GetLocalComputerName) then
              begin
                SetLength(FProfiles, Length(FProfiles) + 1);
                FProfiles[High(FProfiles)].UserName := Name;
                FProfiles[High(FProfiles)].SID := SIDStr;
                FProfiles[High(FProfiles)].LocalProfile := ProfileDir;
                FProfiles[High(FProfiles)].UserKey := 0;
                FProfiles[High(FProfiles)].CloseKey := False;
                FProfiles[High(FProfiles)].UnloadKey := False;
              end;
            except
              // trap deleted accounts
            end;
          end;
        end;
      finally
        FreeMem(SID);
      end;
    finally
      RegProfiles.Free;
    end;
  end;
end;
{$ENDIF MSWINDOWS}

procedure TJediProfilesManager.SetMultipleProfileMode(Value: Boolean);
begin
  FMultipleProfileMode := Value;
end;

end.
