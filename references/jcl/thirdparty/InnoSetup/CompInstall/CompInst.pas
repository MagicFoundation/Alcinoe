unit CompInst;

interface

uses
  Windows, SysUtils, Classes, JclIDEUtils;

function Installations: TJclBorRADToolInstallations;

function compinst_init: Integer; stdcall;

function compinst_isDelphiInstalled(Version: Integer): Integer; stdcall;
function compinst_isBCBInstalled(Version: Integer): Integer; stdcall;
function compinst_isBDSInstalled(IDEVersion: Integer): Integer; stdcall;

function compinst_installDelphiDesignPackage(Version: Integer; const BplFilename, Description: PAnsiChar): Integer; stdcall;
function compinst_installBCBDesignPackage(Version: Integer; const BplFilename, Description: PAnsiChar): Integer; stdcall;
function compinst_uninstallDelphiDesignPackage(Version: Integer; const BplFilename: PAnsiChar): Integer; stdcall;
function compinst_uninstallBCBDesignPackage(Version: Integer; const BplFilename: PAnsiChar): Integer; stdcall;
function compinst_uninstallDelphiDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PAnsiChar): Integer; stdcall;
function compinst_uninstallBCBDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PAnsiChar): Integer; stdcall;

function compinst_installDelphiExpert(Version: Integer; const Filename, Description: PAnsiChar): Integer; stdcall;
function compinst_installBCBExpert(Version: Integer; const Filename, Description: PAnsiChar): Integer; stdcall;
function compinst_uninstallDelphiExpert(Version: Integer; const Filename: PAnsiChar): Integer; stdcall;
function compinst_uninstallBCBExpert(Version: Integer; const Filename: PAnsiChar): Integer; stdcall;
function compinst_uninstallDelphiExpertsPrefixed(Version: Integer; FilenamePrefix: PAnsiChar): Integer; stdcall;
function compinst_uninstallBCBExpertsPrefixed(Version: Integer; FilenamePrefix: PAnsiChar): Integer; stdcall;

function compinst_addDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
function compinst_addBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
function compinst_removeDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
function compinst_removeBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;

implementation

uses
  JclPeImage, StrUtils;

var
  GlobalInstallations: TJclBorRADToolInstallations;

procedure HandleException;
begin
  if (ExceptObject <> nil) and (ExceptObject is Exception) then
    MessageBox(0, PChar('Exception ' + Exception(ExceptObject).ClassName + ': ' + Exception(ExceptObject).Message), 'CompInstall.dll', MB_OK or MB_ICONERROR);
end;

function GetPackageDescription(const BplFilename: string): string;
var
  hLib: THandle;
  Info: TJclPePackageInfo;
begin
  Result := '';
  hLib := LoadLibraryEx(PChar(BplFilename), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hLib <> 0 then
  begin
    Info := TJclPePackageInfo.Create(hLib);
    try
      Result := Trim(Info.Description);
    finally
      Info.Free;
    end;
    FreeLibrary(hLib);
  end;
  if Result = '' then
    Result := ChangeFileExt(ExtractFileName(BplFilename), '');
end;

procedure SplitPaths(List: TStrings; const Paths: string);
var
  I, StartI: Integer;
  S: string;
begin
  StartI := 1;
  for I := 1 to Length(Paths) do
  begin
    if Paths[I] = PathSep then
    begin
      S := Trim(Copy(Paths, StartI, I - StartI));
      if S <> '' then
        List.Add(S);
      StartI := I + 1;
    end;
  end;
  S := Trim(Copy(Paths, StartI, MaxInt));
  if S <> '' then
    List.Add(S);
end;

function Installations: TJclBorRADToolInstallations;
begin
  if GlobalInstallations = nil then
    GlobalInstallations := TJclBorRADToolInstallations.Create;
  Result := GlobalInstallations;
end;

function compinst_init: Integer; stdcall;
var
  I: Integer;
  Inst: TJclBorRADToolInstallation;
  VStr: string;
  ConfigDataLocation: string;
  VersionNum: Integer;
begin
  Result := 0;
  try
    for I := 0 to Installations.Count - 1 do
    begin
      Inst := Installations[I];

      ConfigDataLocation := Inst.ConfigDataLocation;
      if (ConfigDataLocation <> '') and (ConfigDataLocation[1] = PathDelim) then
        ConfigDataLocation := Copy(ConfigDataLocation, 2, MaxInt); // there is no such thing as an absolute "\Software" registry key

      VersionNum := 0;
      case Inst.RadToolKind of
        brDelphi:
          begin
            VersionNum := Inst.PackageVersionNumber;
            VStr := IntToStr(VersionNum);
            SetEnvironmentVariable(PChar('DELPHI' + VStr), PChar(Inst.RootDir));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'BPL'), PChar(Inst.BPLOutputPath[bpWin32]));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'DCP'), PChar(Inst.DCPOutputPath[bpWin32]));
            SetEnvironmentVariable(PChar('DELPHI' + VStr + 'RegKey'), PChar(ConfigDataLocation));
          end;
        brCppBuilder:
          begin
            VersionNum := Inst.PackageVersionNumber;
            VStr := IntToStr(VersionNum);
            SetEnvironmentVariable(PChar('BCB' + VStr), PChar(Inst.RootDir));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'BPL'), PChar(Inst.BPLOutputPath[bpWin32]));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'DCP'), PChar(Inst.DCPOutputPath[bpWin32]));
            SetEnvironmentVariable(PChar('BCB' + VStr + 'RegKey'), PChar(ConfigDataLocation));
          end;
        brBorlandDevStudio:
          begin
            VersionNum := Inst.PackageVersionNumber;
            VStr := IntToStr(VersionNum);
            if bpDelphi32 in Inst.Personalities then
            begin
              SetEnvironmentVariable(PChar('DELPHI' + VStr), PChar(Inst.RootDir));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'BPL'), PChar(Inst.BPLOutputPath[bpWin32]));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'DCP'), PChar(Inst.DCPOutputPath[bpWin32]));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'RegKey'), PChar(ConfigDataLocation));
            end;
            if bpBCBuilder32 in Inst.Personalities then
            begin
              SetEnvironmentVariable(PChar('BCB' + VStr), PChar(Inst.RootDir));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'BPL'), PChar(Inst.BPLOutputPath[bpWin32]));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'DCP'), PChar(Inst.DCPOutputPath[bpWin32]));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'RegKey'), PChar(ConfigDataLocation));
            end;
            if bpDelphi64 in Inst.Personalities then
            begin
              SetEnvironmentVariable(PChar('DELPHI' + VStr), PChar(Inst.RootDir));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'BPL_x64'), PChar(Inst.BPLOutputPath[bpWin64]));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'DCP_x64'), PChar(Inst.DCPOutputPath[bpWin64]));
              SetEnvironmentVariable(PChar('DELPHI' + VStr + 'RegKey'), PChar(ConfigDataLocation));
            end;
            if bpBCBuilder64 in Inst.Personalities then
            begin
              SetEnvironmentVariable(PChar('BCB' + VStr), PChar(Inst.RootDir));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'BPL_x64'), PChar(Inst.BPLOutputPath[bpWin64]));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'DCP_x64'), PChar(Inst.DCPOutputPath[bpWin64]));
              SetEnvironmentVariable(PChar('BCB' + VStr + 'RegKey'), PChar(ConfigDataLocation));
            end;
            SetEnvironmentVariable(PChar('BDSCOMMONDIR' + VStr), PChar(Inst.EnvironmentVariables.Values['BDSCOMMONDIR']));
          end;
      end;
      if Result < VersionNum then
        Result := VersionNum;
    end;
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_IsDelphiInstalled(Version: Integer): Integer; stdcall;
begin
  try
    Result := Ord(Installations.DelphiVersionInstalled[Version]);
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_IsBCBInstalled(Version: Integer): Integer; stdcall;
begin
  try
    Result := Ord(Installations.BCBVersionInstalled[Version]);
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_IsBDSInstalled(IDEVersion: Integer): Integer; stdcall;
begin
  try
    Result := Ord(Installations.BDSVersionInstalled[IDEVersion]);
  except
    HandleException;
    Result := 0;
  end;
end;

{ Design Packages }

function InstallDesignPackage(Inst: TJclBorRADToolInstallation; const BplFilename, Description: string): Integer;
var
  Filename, Descr: string;
  MatchFound: TFilenameCaseMatch;
begin
  Result := 0;
  if Inst <> nil then
  begin
    try
      Descr := Description;
      if Descr = '' then
        Descr := GetPackageDescription(BplFilename);
      Filename := ExpandFileNameCase(BplFilename, MatchFound); // correct file name
      if Inst.RegisterPackage(Filename, Descr) then
        Result := 1;
    except
      HandleException;
    end;
  end;
end;

function UninstallDesignPackage(Inst: TJclBorRADToolInstallation; const BplFilename: string): Integer;
begin
  Result := 0;
  if Inst <> nil then
    if Inst.UnregisterPackage(BplFilename) then
      Result := 1;
end;

function UninstallDesignPackagesPrefixed(Inst: TJclBorRADToolInstallation; const BplFilenamePrefix: string): Integer;
var
  I: Integer;
  Filename: string;
begin
  Result := 0;
  if Inst <> nil then
  begin
    for I := Inst.IdePackages.Count - 1 downto 0 do
    begin
      FileName := Inst.IdePackages.PackageFileNames[I];
      if AnsiStartsText(BplFilenamePrefix, ExtractFileName(FileName)) then
      begin
        UninstallDesignPackage(Inst, Filename);
        Inc(Result);
      end;
    end;
  end;
end;

{ Experts }

function InstallExpert(Inst: TJclBorRADToolInstallation; const Filename, Description: string): Integer;
var
  MatchFound: TFilenameCaseMatch;
begin
  Result := 0;
  if Inst <> nil then
    if Inst.RegisterExpert(ExpandFileNameCase(Filename, MatchFound), Description) then
      Result := 1;
end;

function UninstallExpert(Inst: TJclBorRADToolInstallation; const Filename: string): Integer;
begin
  Result := 0;
  if Inst <> nil then
    if Inst.UnregisterExpert(Filename) then
      Result := 1;
end;

function UninstallExpertsPrefixed(Inst: TJclBorRADToolInstallation; const FilenamePrefix: string): Integer;
var
  I: Integer;
  Filename: string;
begin
  Result := 0;
  if Inst <> nil then
  begin
    for I := Inst.IdePackages.ExpertCount - 1 downto 0 do
    begin
      FileName := Inst.IdePackages.ExpertFileNames[I];
      if AnsiStartsText(FilenamePrefix, ExtractFileName(FileName)) then
      begin
        UninstallExpert(Inst, Filename);
        Inc(Result);
      end;
    end;
  end;
end;

{ Search Paths }

function ReplacePlatform(const Paths: string; BDSPlatform: TJclBDSPlatform): string;
begin
  case BDSPlatform of
    bpWin32:
      Result := StringReplace(Paths, '$(Platform)', 'Win32', [rfReplaceAll, rfIgnoreCase]);
    bpWin64:
      Result := StringReplace(Paths, '$(Platform)', 'Win64', [rfReplaceAll, rfIgnoreCase]);
//    bpOSX32:
//      Result := StringReplace(Paths, '$(Platform)', 'OSX32', [rfReplaceAll, rfIgnoreCase]);
  else
    Result := Paths;
  end;
end;

function ChangeSearchPaths(Inst: TJclBorRADToolInstallation; Installing: Boolean;
  const SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string): Integer;

  procedure AddPaths(BDSPlatform: TJclBDSPlatform);
  begin
    Inst.AddToLibrarySearchPath(ReplacePlatform(SearchPaths, BDSPlatform), BDSPlatform);
    Inst.AddToDebugDCUPath(ReplacePlatform(DebugPaths, BDSPlatform), BDSPlatform);
    Inst.AddToLibraryBrowsingPath(ReplacePlatform(BrowsePaths, BDSPlatform), BDSPlatform);
    if Inst is TJclBDSInstallation then
    begin
      if ((BDSPlatform = bpWin32) and (bpBCBuilder32 in Inst.Personalities)) or
         ((BDSPlatform = bpWin64) and (bpBCBuilder64 in Inst.Personalities)) then
      begin
        TJclBDSInstallation(Inst).AddToCppLibraryPath(ReplacePlatform(SearchPaths, BDSPlatform), BDSPlatform); // for .lib and .bpi
        TJclBDSInstallation(Inst).AddToCppIncludePath(ReplacePlatform(IncludePaths, BDSPlatform), BDSPlatform);
        TJclBDSInstallation(Inst).AddToCppBrowsingPath(ReplacePlatform(BrowsePaths, BDSPlatform), BDSPlatform);
      end;
    end;
  end;

  procedure RemovePaths(BDSPlatform: TJclBDSPlatform);
  begin
    Inst.RemoveFromLibrarySearchPath(ReplacePlatform(SearchPaths, BDSPlatform), BDSPlatform);
    Inst.RemoveFromDebugDCUPath(ReplacePlatform(DebugPaths, BDSPlatform), BDSPlatform);
    Inst.RemoveFromLibraryBrowsingPath(ReplacePlatform(BrowsePaths, BDSPlatform), BDSPlatform);
    if Inst is TJclBDSInstallation then
    begin
      if ((BDSPlatform = bpWin32) and (bpBCBuilder32 in Inst.Personalities)) or
         ((BDSPlatform = bpWin64) and (bpBCBuilder64 in Inst.Personalities)) then
      begin
        TJclBDSInstallation(Inst).RemoveFromCppLibraryPath(ReplacePlatform(SearchPaths, BDSPlatform), BDSPlatform); // for .lib and .bpi
        TJclBDSInstallation(Inst).RemoveFromCppIncludePath(ReplacePlatform(IncludePaths, BDSPlatform), BDSPlatform);
        TJclBDSInstallation(Inst).RemoveFromCppBrowsingPath(ReplacePlatform(BrowsePaths, BDSPlatform), BDSPlatform);
      end;
    end;
  end;

begin
  Result := 0;
  if Inst <> nil then
  begin
    if Installing then
    begin
      AddPaths(bpWin32);
      if [bpDelphi64, bpBCBuilder64] * Inst.Personalities <> [] then
        AddPaths(bpWin64);
    end
    else
    begin
      RemovePaths(bpWin32);
      if [bpDelphi64, bpBCBuilder64] * Inst.Personalities <> [] then
        RemovePaths(bpWin64);
    end;
    Result := 1;
  end;
end;

{ Design Packages }

function compinst_installDelphiDesignPackage(Version: Integer; const BplFilename, Description: PAnsiChar): Integer; stdcall;
begin
  try
    Result := InstallDesignPackage(Installations.DelphiInstallationFromVersion[Version], string(BplFilename), string(Description));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_installBCBDesignPackage(Version: Integer; const BplFilename, Description: PAnsiChar): Integer; stdcall;
begin
  try
    Result := InstallDesignPackage(Installations.BCBInstallationFromVersion[Version], string(BplFilename), string(Description));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallDelphiDesignPackage(Version: Integer; const BplFilename: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallDesignPackage(Installations.DelphiInstallationFromVersion[Version], string(BplFilename));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallBCBDesignPackage(Version: Integer; const BplFilename: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallDesignPackage(Installations.BCBInstallationFromVersion[Version], string(BplFilename));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallDelphiDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallDesignPackagesPrefixed(Installations.DelphiInstallationFromVersion[Version], string(BplFilenamePrefix));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallBCBDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallDesignPackagesPrefixed(Installations.BCBInstallationFromVersion[Version], string(BplFilenamePrefix));
  except
    HandleException;
    Result := 0;
  end;
end;

{ Experts }

function compinst_installDelphiExpert(Version: Integer; const Filename, Description: PAnsiChar): Integer; stdcall;
begin
  try
    Result := InstallExpert(Installations.DelphiInstallationFromVersion[Version], string(Filename), string(Description));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_installBCBExpert(Version: Integer; const Filename, Description: PAnsiChar): Integer; stdcall;
begin
  try
    Result := InstallExpert(Installations.BCBInstallationFromVersion[Version], string(Filename), string(Description));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallDelphiExpert(Version: Integer; const Filename: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallExpert(Installations.DelphiInstallationFromVersion[Version], string(Filename));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallBCBExpert(Version: Integer; const Filename: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallExpert(Installations.BCBInstallationFromVersion[Version], string(Filename));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallDelphiExpertsPrefixed(Version: Integer; FilenamePrefix: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallExpertsPrefixed(Installations.DelphiInstallationFromVersion[Version], string(FilenamePrefix));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_uninstallBCBExpertsPrefixed(Version: Integer; FilenamePrefix: PAnsiChar): Integer; stdcall;
begin
  try
    Result := UninstallExpertsPrefixed(Installations.BCBInstallationFromVersion[Version], string(FilenamePrefix));
  except
    HandleException;
    Result := 0;
  end;
end;

{ Search Paths }

function compinst_addDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
begin
  try
    Result := ChangeSearchPaths(Installations.DelphiInstallationFromVersion[Version], True, string(SearchPaths), string(DebugPaths), string(BrowsePaths), string(IncludePaths));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_addBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
begin
  try
    Result := ChangeSearchPaths(Installations.BCBInstallationFromVersion[Version], True, string(SearchPaths), string(DebugPaths), string(BrowsePaths), string(IncludePaths));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_removeDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
begin
  try
    Result := ChangeSearchPaths(Installations.DelphiInstallationFromVersion[Version], False, string(SearchPaths), string(DebugPaths), string(BrowsePaths), string(IncludePaths));
  except
    HandleException;
    Result := 0;
  end;
end;

function compinst_removeBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PAnsiChar): Integer; stdcall;
begin
  try
    Result := ChangeSearchPaths(Installations.BCBInstallationFromVersion[Version], False, string(SearchPaths), string(DebugPaths), string(BrowsePaths), string(IncludePaths));
  except
    HandleException;
    Result := 0;
  end;
end;

initialization

finalization
  Installations.Free;

end.
