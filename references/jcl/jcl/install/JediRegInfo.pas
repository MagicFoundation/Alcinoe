{**************************************************************************************************}
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License")  }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/MPL-1.1.html                                               }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either expressed or implied. See the License for the specific language governing       }
{ rights and limitations under the License.                                                        }
{                                                                                                  }
{ The Original Code is: JediInfo.pas, released on 2006-02-26.                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Andreas Hausladen                                  }
{ (Andreas dott Hausladen att gmx dott de)                                                         }
{ Portions created by Andreas Hausladen are Copyright (C) 2006 Andreas Hausladen.                  }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL / JVCL                }
{ home page, located at https://github.com/project-jedi/jcl / https://github.com/project-jedi/jvcl }
{                                                                                                  }
{ Known Issues:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit JediRegInfo;

{$I jedi\jedi.inc}

interface

uses
  Windows, SysUtils, Classes;

type
  TJediInformation = record
    Version: string; // example: '1.98'
    DcpDir: string;  // example: 'C:\Program Files\Borland\Delphi7\Projects\BPL', the JVCL Installer resolves macros
    BplDir: string;  // example: 'C:\Program Files\Borland\Delphi7\Projects\BPL', the JVCL Installer resolves macros
    RootDir: string; // example: 'C:\Program Files\Borland\Delphi7', the JVCL Installer resolves macros
  end;

{ InstallJediInformation() writes the "Version", "DcpDir", "BplDir" and "RootDir"
  values into the registry key IdeRegKey\Jedi\ProjectName. Returns True if the
  values could be written. }
function InstallJediRegInformation(const IdeRegKey, ProjectName, PlatformName, Version, DcpDir,
  BplDir, RootDir: string; RootKey: HKEY = HKEY_CURRENT_USER): Boolean;

{ RemoveJediInformation() deletes the registry key IdeRegKey\Jedi\ProjectName.
  If there is no further subkeys to IdeRegKey\Jedi and no values in this key,
  the whole Jedi-key is deleted. }
procedure RemoveJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string;
  RootKey: HKEY = HKEY_CURRENT_USER);

{ ReadJediInformation() reads the JEDI Information from the registry. Returns
  False if Version='' or DcpDir='' or BplDir='' or RootDir=''. }
function ReadJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string; out Version,
  DcpDir, BplDir, RootDir: string; RootKey: HKEY = HKEY_CURRENT_USER): Boolean; overload;

{ ReadJediInformation() reads the JEDI Information from the registry. }
function ReadJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string
  ; RootKey: HKEY = HKEY_CURRENT_USER): TJediInformation; overload;

{ ParseVersionNumber() converts a version number 'major.minor.release.build' to
  cardinal like the JclBase JclVersion constant. If the VersionStr is invalid
  the function returns 0. }
function ParseVersionNumber(const VersionStr: string): Cardinal;

implementation

uses
  Registry;

function InstallJediRegInformation(const IdeRegKey, ProjectName, PlatformName, Version, DcpDir,
  BplDir, RootDir: string; RootKey: HKEY): Boolean;
var
  Reg: TRegistry;
  PlatformKeyStr: string;
begin
  Result := False;
  if (Version <> '') and (DcpDir <> '') and (BplDir <> '') and (RootDir <> '') then
  begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      if Reg.OpenKey(IdeRegKey + '\Jedi', True) then // do not localize
        Reg.CloseKey;
      if PlatformName <> '' then
      begin
        if Reg.OpenKey(IdeRegKey + '\Jedi\' + ProjectName, True) then // do not localize
          Reg.CloseKey;
        PlatformKeyStr := '\' + PlatformName;
      end
      else
        PlatformKeyStr := '';
      if Reg.OpenKey(IdeRegKey + '\Jedi\' + ProjectName + PlatformKeyStr, True) then // do not localize
      begin
        Reg.WriteString('Version', Version); // do not localize
        Reg.WriteString('DcpDir', ExcludeTrailingPathDelimiter(DcpDir)); // do not localize
        Reg.WriteString('BplDir', ExcludeTrailingPathDelimiter(BplDir)); // do not localize
        Reg.WriteString('RootDir', ExcludeTrailingPathDelimiter(RootDir)); // do not localize
        Result := True;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure RemoveJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string; RootKey: HKEY);
var
  Reg: TRegistry;
  Names: TStringList;
  JediKeyName, ProjectKeyName, ProjectPlatformKeyName: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
// (outchy) do not delete target settings
//    Reg.DeleteKey(IdeRegKey + '\Jedi\' + ProjectName); // do not localize

    JediKeyName := IdeRegKey + '\Jedi';                  // do not localize
    ProjectKeyName := JediKeyName + '\' + ProjectName;   // do not localize
    ProjectPlatformKeyName := ProjectKeyName;
    if PlatformName <> '' then
      ProjectPlatformKeyName := ProjectPlatformKeyName + '\' + PlatformName;

    if Reg.OpenKey(ProjectPlatformKeyName, False) then
    begin
      Reg.DeleteValue('Version'); // do not localize
      Reg.DeleteValue('DcpDir'); // do not localize
      Reg.DeleteValue('BplDir'); // do not localize
      Reg.DeleteValue('RootDir'); // do not localize

      Names := TStringList.Create;
      try
        Reg.GetKeyNames(Names);
        if Names.Count = 0 then
        begin
          Reg.GetValueNames(Names);
          if Names.Count = 0 then
          begin
            Reg.CloseKey;
            Reg.DeleteKey(ProjectPlatformKeyName); // do not localize
          end;
        end;
      finally
        Names.Free;
      end;
    end;

    if ProjectPlatformKeyName <> ProjectKeyName then
    begin
      if Reg.OpenKey(ProjectKeyName, False) then // do not localize
      begin
        Names := TStringList.Create;
        try
          Reg.GetKeyNames(Names);
          if Names.Count = 0 then
          begin
            Reg.GetValueNames(Names);
            if Names.Count = 0 then
            begin
              Reg.CloseKey;
              Reg.DeleteKey(ProjectKeyName); // do not localize
            end;
          end;
        finally
          Names.Free;
        end;
      end;
    end;

    if Reg.OpenKey(JediKeyName, False) then // do not localize
    begin
      Names := TStringList.Create;
      try
        Reg.GetKeyNames(Names);
        if Names.Count = 0 then
        begin
          Reg.GetValueNames(Names);
          if Names.Count = 0 then
          begin
            Reg.CloseKey;
            Reg.DeleteKey(JediKeyName); // do not localize
          end;
        end;
      finally
        Names.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function ReadJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string; out Version,
  DcpDir, BplDir, RootDir: string; RootKey: HKEY): Boolean; overload;
var
  Reg: TRegistry;
  PlatformKeyStr: string;
begin
  Version := '';
  DcpDir := '';
  BplDir := '';
  RootDir := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if PlatformName <> '' then
      PlatformKeyStr := '\' + PlatformName
    else
      PlatformKeyStr := '';
    if Reg.OpenKeyReadOnly(IdeRegKey + '\Jedi\' + ProjectName + PlatformKeyStr) then // do not localize
    begin
      if Reg.ValueExists('Version') then // do not localize
        Version := Reg.ReadString('Version'); // do not localize
      if Reg.ValueExists('DcpDir') then // do not localize
        DcpDir := ExcludeTrailingPathDelimiter(Reg.ReadString('DcpDir')); // do not localize
      if Reg.ValueExists('BplDir') then // do not localize
        BplDir := ExcludeTrailingPathDelimiter(Reg.ReadString('BplDir')); // do not localize
      if Reg.ValueExists('RootDir') then // do not localize
        RootDir := ExcludeTrailingPathDelimiter(Reg.ReadString('RootDir')); // do not localize
    end;
  finally
    Reg.Free;
  end;
  Result := (Version <> '') and (DcpDir <> '') and (BplDir <> '') and (RootDir <> '');
end;

function ReadJediRegInformation(const IdeRegKey, ProjectName, PlatformName: string; RootKey: HKEY): TJediInformation;
begin
  ReadJediRegInformation(IdeRegKey, ProjectName, PlatformName, Result.Version, Result.DcpDir,
    Result.BplDir, Result.RootDir, RootKey);
end;

function ParseVersionNumber(const VersionStr: string): Cardinal;
const
  Shifts: array[0..3] of Integer = (24, 16, 15, 0);
var
  S: string;
  ps: Integer;
  Count: Integer;
begin
  S := VersionStr;
  Result := 0;
  if S <> '' then
  begin
    Result := 0;
    try
      Count := 0;
      ps := Pos('.', S);
      while (ps > 0) and (Count < High(Shifts)) do
      begin
        Result := Result or (Cardinal(StrToInt(Copy(S, 1, ps - 1))) shl Shifts[Count]);
        S := Copy(S, ps + 1, MaxInt);
        ps := Pos('.', S);
        Inc(Count);
      end;
      Result := Result or (Cardinal(StrToInt(Copy(S, 1, MaxInt))) shl Shifts[Count]);
    except
      Result := 0;
    end;
  end;
end;

end.
