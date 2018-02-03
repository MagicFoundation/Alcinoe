[Files]
Source: CompInstall.dll; DestDir: {app}; Flags: solidbreak

[Code]
type
  TIdeKind = (ikUnknown, ikDelphi, ikBCB);
var
  LastInstalledIDEVersionNumber: Integer;

function MapDesignPackage(IdeKind: TIdeKind; Version: Integer; const PackageName: string): string;
  forward; // user defined mapping function that maps the component name @PackageName to a full qualified DesignPackage file name
function MapExpert(IdeKind: TIdeKind; Version: Integer; const ExpertName: string): string;
  forward; // user defined mapping function that maps the component name @PackageName to a full qualified IDE expert file name
procedure GetSearchPaths(IdeKind: TIdeKind; Version: Integer; var SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string);
  forward; // user defined function that returns a semicolon separated list of paths that the installer should add to the IDE paths
procedure UserRegisterComponents(Components: TStrings);
  forward; // user defined function that does additional component registration. In Components[] is the list of selected wizard components
procedure UserUnregisterComponents(Components: TStrings);
  forward; // user defined function that does additional component unregistration. In Components[] is the list of installed wizard components

{----------------------------------------------------------}

function GetDelphiDir(Version: string): string;
begin
  Result := GetEnv('DELPHI' + Version);
  if Result = '' then
    Result := ExpandConstant('{app}') + '\bpl\d' + Version;
end;

function GetBCBBplDir(Version: string): string;
begin
  Result := GetEnv('BCB' + Version + 'BPL');
  if Result = '' then
    Result := ExpandConstant('{app}') + '\bpl\c' + Version;
end;

function GetDelphiBplDir(Version: string): string;
begin
  Result := GetEnv('DELPHI' + Version + 'BPL');
  if Result = '' then
    Result := ExpandConstant('{app}') + '\bpl\d' + Version;
end;

function GetBCBDir(Version: string): string;
begin
  Result := GetEnv('BCB' + Version);
  if Result = '' then
    Result := ExpandConstant('{app}') + '\bpl\d' + Version;
end;

function GetDelphiRegKey(Version: string): string;
begin
  Result := GetEnv('DELPHI' + Version + 'RegKey');
end;

function GetBCBRegKey(Version: string): string;
begin
  Result := GetEnv('BCB' + Version + 'RegKey');
end;

function GetHPPDir(Version: string): string;
begin
  Result := GetEnv('BDSCOMMONDIR' + Version);
  if Result = '' then
    Result := ExpandConstant('{app}') + '\HPP\d' + Version
  else
    Result := Result + '\HPP';
end;

{----------------------------------------------------------}

function StartsText(const SubStr, S: string): Boolean;
begin
  Result := (Length(S) >= Length(SubStr)) and (CompareText(Copy(S, 1, Length(SubStr)), SubStr) = 0);
end;

function EndsText(const SubStr, S: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(SubStr);
  Result := (Length(S) >= Len) and (CompareText(Copy(S, Length(S) - Len + 1, Len), SubStr) = 0);
end;

procedure GetSelectedList(List: TStrings; Prefix: string; Components: TStrings);
var
  I, Len: Integer;
begin
  Prefix := Prefix + '\';
  Len := Length(Prefix);
  for I := 0 to Components.Count - 1 do
    if StartsText(Prefix, Components[I]) then
      List.Add(Copy(Components[I], Len + 1, Length(Components[I])));
end;

procedure ExtractIdeInfo(const IdeName: string; var Kind: TIdeKind; var Version: Integer);
begin
  Kind := ikUnknown;
  Version := 0;
  if StartsText('Delphi', IdeName) then
  begin
    Kind := ikDelphi;
    Version := StrToInt(Copy(IdeName, 7, Length(IdeName)));
  end
  else if StartsText('BCB', IdeName) then
  begin
    Kind := ikBCB;
    Version := StrToInt(Copy(IdeName, 4, Length(IdeName)));
  end;
end;

// -------------------
function compinst_init(): Integer;
  external 'compinst_init@files:CompInstall.dll stdcall';
function compinst_initUninstall(): Integer;
  external 'compinst_init@{app}\CompInstall.dll stdcall uninstallonly';

// install only
function compinst_isDelphiInstalled(Version: Integer): Integer;
  external 'compinst_isDelphiInstalled@files:CompInstall.dll stdcall';
function compinst_isBCBInstalled(Version: Integer): Integer;
  external 'compinst_isBCBInstalled@files:CompInstall.dll stdcall';
function compinst_isBDSInstalled(IDEVersion: Integer): Integer;
  external 'compinst_isBDSInstalled@files:CompInstall.dll stdcall';

  // design package
function compinst_installDelphiDesignPackage(Version: Integer; BplFilename, Description: PChar): Integer;
  external 'compinst_installDelphiDesignPackage@files:CompInstall.dll stdcall';
function compinst_installBCBDesignPackage(Version: Integer; BplFilename, Description: PChar): Integer;
  external 'compinst_installBCBDesignPackage@files:CompInstall.dll stdcall';

  // expert
function compinst_installDelphiExpert(Version: Integer; Filename, Description: PChar): Integer;
  external 'compinst_installDelphiExpert@files:CompInstall.dll stdcall';
function compinst_installBCBExpert(Version: Integer; Filename, Description: PChar): Integer;
  external 'compinst_installBCBExpert@files:CompInstall.dll stdcall';

  // search path
function compinst_addDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PChar): Integer;
  external 'compinst_addDelphiSearchPaths@files:CompInstall.dll stdcall';
function compinst_addBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PChar): Integer;
  external 'compinst_addBCBSearchPaths@files:CompInstall.dll stdcall';

// uninstall only
  // design package
function compinst_uninstallDelphiDesignPackage(Version: Integer; BplFilename: PChar): Integer;
  external 'compinst_uninstallDelphiDesignPackage@{app}\CompInstall.dll stdcall uninstallonly';
function compinst_uninstallBCBDesignPackage(Version: Integer; BplFilename: PChar): Integer;
  external 'compinst_uninstallBCBDesignPackage@{app}\CompInstall.dll stdcall uninstallonly';

function compinst_uninstallDelphiDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer;
  external 'compinst_uninstallDelphiDesignPackagesPrefixed@{app}\CompInstall.dll stdcall uninstallonly';
function compinst_uninstallBCBDesignPackagesPrefixed(Version: Integer; BplFilenamePrefix: PChar): Integer;
  external 'compinst_uninstallBCBDesignPackagesPrefixed@{app}\CompInstall.dll stdcall uninstallonly';

  // expert
function compinst_uninstallDelphiExpert(Version: Integer; Filename: PChar): Integer;
  external 'compinst_uninstallDelphiExpert@{app}\CompInstall.dll stdcall uninstallonly';
function compinst_uninstallBCBExpert(Version: Integer; Filename: PChar): Integer;
  external 'compinst_uninstallBCBExpert@{app}\CompInstall.dll stdcall uninstallonly';

function compinst_uninstallDelphiExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer;
  external 'compinst_uninstallDelphiExpertsPrefixed@{app}\CompInstall.dll stdcall uninstallonly';
function compinst_uninstallBCBExpertsPrefixed(Version: Integer; FilenamePrefix: PChar): Integer;
  external 'compinst_uninstallBCBExpertsPrefixed@{app}\CompInstall.dll stdcall uninstallonly';

  // search path
function compinst_removeDelphiSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PChar): Integer;
  external 'compinst_removeDelphiSearchPaths@{app}\CompInstall.dll stdcall uninstallonly';
function compinst_removeBCBSearchPaths(Version: Integer; SearchPaths, DebugPaths, BrowsePaths, IncludePaths: PChar): Integer;
  external 'compinst_removeBCBSearchPaths@{app}\CompInstall.dll stdcall uninstallonly';

function IsDelphiInstalled(Version: Integer): Boolean;
begin
  Result := compinst_isDelphiInstalled(Version) <> 0;
end;

function IsBCBInstalled(Version: Integer): Boolean;
begin
  Result := compinst_isBCBInstalled(Version) <> 0;
end;

function IsStudioInstalled(Version: Integer): Boolean;
begin
  Result := compinst_isBDSInstalled(Version) <> 0;
end;

function IsSourceInstall: Boolean;
var
  List: TStrings;
  I: Integer;
begin
  Result := False;
  List := TStringList.Create;
  try
    List.CommaText := WizardSelectedComponents(False);
    for I := 0 to List.Count - 1 do
      if StartsText('IDE\', List[I]) then
        Exit;
  finally
    List.Free;
  end;
  Result := True;
end;

// Design Packages
procedure InstallDesignPackage(Kind: TIdeKind; Version: Integer; const BplFilename: string);
begin
  if not FileExists(BplFilename) then
    Exit;
  Log('Register designtime package: ' + BplFilename);
  case Kind of
    ikDelphi:
      if compinst_installDelphiDesignPackage(Version, PChar(BplFilename), '') = 0 then
        MsgBox('Failed to install designtime package ' + ExtractFileName(BplFilename), mbError, MB_OK);
    ikBCB:
      if compinst_installBCBDesignPackage(Version, PChar(BplFilename), '') = 0 then
        MsgBox('Failed to install designtime package ' + ExtractFileName(BplFilename), mbError, MB_OK);
  end;
end;

function UninstallDesignPackage(Kind: TIdeKind; Version: Integer; const BplFilename: string): Boolean;
begin
  Log('Unregister designtime package: ' + BplFilename);
  Result := False;
  case Kind of
    ikDelphi:
      Result := compinst_uninstallDelphiDesignPackage(Version, PChar(BplFilename)) <> 0;
    ikBCB:
      Result := compinst_uninstallBCBDesignPackage(Version, PChar(BplFilename)) <> 0;
  end;
end;

function UninstallDesignPackagesPrefixed(Kind: TIdeKind; Version: Integer; const BplFilenamePrefix: string): Boolean;
begin
  Log('Unregister designtime packages that have the prefix: ' + BplFilenamePrefix);
  Result := False;
  case Kind of
    ikDelphi:
      Result := compinst_uninstallDelphiDesignPackagesPrefixed(Version, PChar(BplFilenamePrefix)) <> 0;
    ikBCB:
      Result := compinst_uninstallBCBDesignPackagesPrefixed(Version, PChar(BplFilenamePrefix)) <> 0;
  end;
end;

// Experts
procedure InstallExpert(Kind: TIdeKind; Version: Integer; const Filename, Description: string);
begin
  if not FileExists(Filename) then
    Exit;
  Log('Register IDE expert: ' + Filename);
  case Kind of
    ikDelphi:
      if compinst_installDelphiExpert(Version, PChar(Filename), Description) = 0 then
        MsgBox('Failed to install IDE expert ' + ExtractFileName(Filename), mbError, MB_OK);
    ikBCB:
      if compinst_installBCBExpert(Version, PChar(Filename), Description) = 0 then
        MsgBox('Failed to install IDE expert ' + ExtractFileName(Filename), mbError, MB_OK);
  end;
end;

function UninstallExpert(Kind: TIdeKind; Version: Integer; const Filename: string): Boolean;
begin
  Log('Unregister IDE expert: ' + Filename);
  Result := False;
  case Kind of
    ikDelphi:
      Result := compinst_uninstallDelphiExpert(Version, PChar(Filename)) <> 0;
    ikBCB:
      Result := compinst_uninstallBCBExpert(Version, PChar(Filename)) <> 0;
  end;
end;

procedure InstallExpertEx(Kind: TIdeKind; Version: Integer; const Filename, Description: string);
begin
  if not FileExists(Filename) then
    Exit;
  if EndsText('.bpl', Filename) then
    InstallDesignPackage(Kind, Version, Filename)
  else
    InstallExpert(Kind, Version, Filename, Description);
end;

function UninstallExpertEx(Kind: TIdeKind; Version: Integer; const Filename: string): Boolean;
begin
  if EndsText('.bpl', Filename) then
    Result := UninstallDesignPackage(Kind, Version, Filename)
  else
    Result := UninstallExpert(Kind, Version, Filename);
end;

function UninstallExpertsPrefixed(Kind: TIdeKind; Version: Integer; const FilenamePrefix: string): Boolean;
begin
  Log('Unregister IDE experts that have the prefix: ' + FilenamePrefix);
  Result := False;
  case Kind of
    ikDelphi:
      Result := compinst_uninstallDelphiExpertsPrefixed(Version, PChar(FilenamePrefix)) <> 0;
    ikBCB:
      Result := compinst_uninstallBCBExpertsPrefixed(Version, PChar(FilenamePrefix)) <> 0;
  end;
end;

// Search Paths
procedure ChangeIdeSearchPaths(Kind: TIdeKind; Version: Integer; Installing: Boolean);
var
  SearchPaths, DebugPaths, BrowsePaths, IncludePaths: string;
  Value: Integer;
begin
  GetSearchPaths(Kind, Version, SearchPaths, DebugPaths, BrowsePaths, IncludePaths);
  if (SearchPaths = '') and (DebugPaths = '') and (BrowsePaths = '') then
    Exit;

  if Installing then
  begin
    Log('Adding search paths: ' + SearchPaths);
    Log('Adding debug paths: ' + DebugPaths);
    Log('Adding browsing paths: ' + BrowsePaths);
    if IncludePaths <> '' then
      Log('Adding include paths: ' + IncludePaths);
  end
  else
  begin
    Log('Removing search paths: ' + SearchPaths);
    Log('Removing debug paths: ' + DebugPaths);
    Log('Removing browsing paths: ' + BrowsePaths);
    if IncludePaths <> '' then
      Log('Removing include paths: ' + IncludePaths);
  end;

  case Kind of
    ikDelphi:
      if Installing then
        Value := compinst_addDelphiSearchPaths(Version, PChar(SearchPaths), PChar(DebugPaths), PChar(BrowsePaths), PChar(IncludePaths))
      else
        Value := compinst_removeDelphiSearchPaths(Version, PChar(SearchPaths), PChar(DebugPaths), PChar(BrowsePaths), PChar(IncludePaths));
    ikBCB:
      if Installing then
        Value := compinst_addBCBSearchPaths(Version, PChar(SearchPaths), PChar(DebugPaths), PChar(BrowsePaths), PChar(IncludePaths))
      else
        Value := compinst_removeBCBSearchPaths(Version, PChar(SearchPaths), PChar(DebugPaths), PChar(BrowsePaths), PChar(IncludePaths));
  end;
end;

procedure ChangeComponentRegistration(Installing: Boolean; Components: TStrings);
var
  IdeList, PackageList, ExpertList, IdeExpertList: TStrings;
  IdeIndex, Index: Integer;
  DesignPackageName, ExpertName, Name: string;
  IdeKind: TIdeKind;
  Version: Integer;
  ComponentsFilename: string;
begin
  // Register Packages into the IDEs
  IdeList := nil;
  PackageList := nil;
  ExpertList := nil;
  IdeExpertList := nil;
  try
    IdeList := TStringList.Create;
    PackageList := TStringList.Create;
    ExpertList := TStringList.Create;
    IdeExpertList := TStringList.Create;

    ComponentsFilename := ExpandConstant('{app}\uninscmp.dat');

    if Installing then
      Components.CommaText := WizardSelectedComponents(False)
    else
      if FileExists(ComponentsFilename) then
        Components.LoadFromFile(ComponentsFilename);

    GetSelectedList(IdeList, 'IDE', Components);
    GetSelectedList(PackageList, 'Packages', Components);
    GetSelectedList(ExpertList, 'Experts', Components);
    GetSelectedList(IdeExpertList, 'IdeExperts', Components);

    // install per IDE
    for IdeIndex := 0 to IdeList.Count - 1 do
    begin
      ExtractIdeInfo(IdeList[IdeIndex], IdeKind, Version);

      // install per Package
      for Index := 0 to PackageList.Count - 1 do
      begin
        Name := ExtractFileName(PackageList[Index]);
        if Trim(Name) <> '' then
        begin
          DesignPackageName := MapDesignPackage(IdeKind, Version, Name);
          if Trim(DesignPackageName) <> '' then
          begin
            if Installing then
              InstallDesignPackage(IdeKind, Version, DesignPackageName)
            else
              UninstallDesignPackage(IdeKind, Version, DesignPackageName);
          end;
        end;
      end;

      // install per Expert
      for Index := 0 to ExpertList.Count - 1 do
      begin
        Name := ExtractFileName(ExpertList[Index]);
        if Trim(Name) <> '' then
        begin
          ExpertName := MapExpert(IdeKind, Version, Name);
          if Trim(ExpertName) <> '' then
          begin
            if Installing then
              InstallExpertEx(IdeKind, Version, ExpertName, Name)
            else
              UninstallExpertEx(IdeKind, Version, ExpertName);
          end;
        end;
      end;

      // install per IdeExpert
      for Index := 0 to IdeExpertList.Count - 1 do
      begin
        Name := ExtractFileName(IdeExpertList[Index]);
        if Trim(Name) <> '' then
        begin
          ExpertName := MapExpert(IdeKind, Version, Name);
          if Trim(ExpertName) <> '' then
          begin
            if Installing then
              InstallExpert(IdeKind, Version, ExpertName, Name)
            else
              UninstallExpert(IdeKind, Version, ExpertName);
          end;
        end;
      end;

      ChangeIdeSearchPaths(IdeKind, Version, Installing);
    end;

    if Installing then
      Components.SaveToFile(ComponentsFilename)
    else
      DeleteFile(ComponentsFilename);
  finally
    IdeList.Free;
    PackageList.Free;
    ExpertList.Free;
    IdeExpertList.Free;
  end;
end;

function InitComponentInstaller(): Boolean;
var
  Version: Integer;
begin
  LastInstalledIDEVersionNumber := compinst_init;  // sets the "DELPHIx[|BPL|DCP|RegKey]" and "BCBx[|BPL|DCP|RegKey] environment variables

  // Check if there is any Delphi IDE installed
  Result := False;
  for Version := 6 to LastInstalledIDEVersionNumber do
    if IsDelphiInstalled(Version) then
      Result := True;

  if not Result then
    MsgBox('No supported Delphi IDE is installed. Installation aborted.', mbError, MB_OK);
end;

function InitComponentUninstaller(): Boolean;
begin
  LastInstalledIDEVersionNumber := compinst_initUninstall;  // sets the "DELPHIx[|BPL|DCP|RegKey]" and "BCBx[|BPL|DCP|RegKey] environment variables
  Result := True;
end;

procedure RegisterComponents();
var
  Components: TStrings;
begin
  Components := TStringList.Create;
  try
    ChangeComponentRegistration(True, Components);
    UserRegisterComponents(Components);
  finally
    Components.Free;
  end;
end;

procedure UnregisterComponents();
var
  Components: TStrings;
begin
  Components := TStringList.Create;
  try
    ChangeComponentRegistration(False, Components);
    UserUnregisterComponents(Components);
  finally
    Components.Free;
    UnloadDLL(ExpandConstant('{app}\CompInstall.dll')); // make the file deletable
  end;
end;
