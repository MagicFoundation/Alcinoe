{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}
program dcc32ex;

{$APPTYPE CONSOLE}

{$IF CompilerVersion >= 15.0}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$IFEND}

uses
  Windows;

var
  DxgettextDir: string;
  ExtraUnitDirs: string;
  UseSearchPaths: Boolean;
  Verbose: Boolean;
  PreserveConfig: Boolean;
  RequireJcl: Boolean;
  RequireJvcl: Boolean;
  UseJclSource: Boolean;
  UseJvclSource: Boolean;
  RequireJclVersion: string;
  RequireJvclVersion: string;
  RuntimePackageRtl: Boolean;
  RuntimePackageVcl: Boolean;

type
  TTargetType = (ttNone, ttDelphi, ttBCB, ttBDS);
  TLibraryType = (ltJCL, ltJVCL);

const
  ttFirst = ttDelphi;

type
  TTarget = record
    Typ: TTargetType;
    Version: Integer;
    IDEVersion: Integer;
    Name: string;
    RootDir: string;
    LibDirs: string;
    SearchPaths: string;
    KeyName: string;
    Id: string; // ["d"|"c"]<version>
    InstalledJcl: Boolean;
    JclVersion: string;
    InstalledJvcl: Boolean;
    JvclVersion: string;
  end;

{ Helper functions because no SysUtils unit is used. }
{******************************************************************************}
function ExtractFileDir(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, 1, ps - 1);
end;
{******************************************************************************}
function ExtractFileName(const S: string): string;
var
  ps: Integer;
begin
  ps := Length(S);
  while (ps > 1) and (S[ps] <> '\') do
    Dec(ps);
  Result := Copy(S, ps + 1, MaxInt);
end;
{******************************************************************************}
function ChangeFileExt(const Filename, NewExt: string): string;
var
  ps: Integer;
begin
  ps := Length(Filename);
  while (ps > 1) and (Filename[ps] <> '.') do
    Dec(ps);
  if ps > 0 then
    Result := Copy(Filename, 1, ps - 1) + NewExt
  else
    Result := Filename + NewExt;
end;
{******************************************************************************}
function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  if (S <> '') and (S[Length(S)] = '\') then
    Result := Copy(S, 1, Length(S) - 1)
  else
    Result := S;
end;
{******************************************************************************}
function StrLen(P: PChar): Integer;
begin
  Result := 0;
  while P[Result] <> #0 do
    Inc(Result);
end;
{******************************************************************************}
function StrToInt(const S: string): Integer;
var
  Error: Integer;
begin
  Val(S, Result, Error);
  if Error <> 0 then
    Result := 0;
end;
{******************************************************************************}
function IntToStr(Value: Integer): string;
var
  S: ShortString;
begin
  Str(Value, S);
  Result := string(S);
end;
{******************************************************************************}
function SameText(const S1, S2: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(S1);
  if len = Length(S2) then
  begin
    for i := 1 to len do
      if UpCase(S1[i]) <> UpCase(S2[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function StartsText(const SubStr, S: string): Boolean;
var
  i, len: Integer;
begin
  Result := False;
  len := Length(SubStr);
  if len <= Length(S) then
  begin
    for i := 1 to len do
      if UpCase(SubStr[i]) <> UpCase(S[i]) then
        Exit;
    Result := True;
  end;
end;
{******************************************************************************}
function GetEnvironmentVariable(const Name: string): string;
begin
  SetLength(Result, 8 * 1024);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result)));
end;
{******************************************************************************}
function ExpandDirMacros(const Path, RootDir: string): string;
var
  i: Integer;
  Start, Len: Integer;
  NewS, S: string;
begin
  Result := Path;
  Len := Length(Result);
  i := 1;
  while i <= Len do
  begin
    if (Result[i] = '$') and (i < Len - 1) and (Result[i + 1] = '(') then
    begin
      Start := i;
      while (i <= Len) and (Result[i] <> ')') do
        Inc(i);
      if i <= Len then
      begin
        S := Copy(Result, Start + 2, i - Start - 2);

        if SameText(S, 'BDS') or SameText(S, 'BCB') or SameText(S, 'DELPHI') then
          NewS := ExtractFileDir(RootDir)
        else
          NewS := GetEnvironmentVariable(S);

        Delete(Result, Start, i - Start + 1);
        Insert(NewS, Result, Start);
        Dec(i, Length(S) + 3);
        Inc(i, Length(NewS));
        Len := Length(Result);
      end;
    end;
    Inc(i);
  end;
end;
{******************************************************************************}
function FileExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY = 0);
end;
{******************************************************************************}
function DirectoryExists(const Filename: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Filename));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;
{******************************************************************************}
function FileSearch(const Name, DirList: string): string;
var
  I, P, L: Integer;
  C: Char;
begin
  Result := Name;
  if Result <> '' then
  begin
    P := 1;
    L := Length(DirList);
    while True do
    begin
      if FileExists(Result) then
        Exit;
      while (P <= L) and (DirList[P] = ';') do
        Inc(P);
      if P > L then Break;
      I := P;
      while (P <= L) and (DirList[P] <> ';') do
        Inc(P);
      Result := Copy(DirList, I, P - I);
      C := #0;
      if Result <> '' then
        C := Result[Length(Result)];
      if (C <> ':') and (C <> '\') then
        Result := Result + '\';
      Result := Result + Name;
    end;
    Result := '';
  end;
end;
{******************************************************************************}
function GetSysErrorMessage(ErrorCode: Cardinal): string;
var
  Buffer: array[0..4096] of Char;
  Len: Integer;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrorCode, 0, Buffer,
    Length(Buffer), nil);
  while (Len > 0) and ((Buffer[Len - 1] <= ' ') or (Buffer[Len - 1] = '.')) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;
{******************************************************************************}
procedure FailedToStart(const Cmd: string; ErrorCode: Cardinal);
begin
  WriteLn(ErrOutput, 'Failed to start: ', Cmd);
  if ErrorCode <> ERROR_SUCCESS then
    WriteLn(ErrOutput, '  System error (', ErrorCode, '): ', GetSysErrorMessage(ErrorCode));
end;
{******************************************************************************}
function Execute(Cmd: string; const StartDir: string; HideOutput: Boolean): Integer;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  StartupInfo.cb := SizeOf(StartupInfo);
  GetStartupInfo(StartupInfo);
  if HideOutput then
  begin
    StartupInfo.hStdOutput := 0;
    StartupInfo.hStdError := 0;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES;
  end;
  UniqueString(Cmd);
  if CreateProcess(nil, PChar(Cmd), nil, nil, True, 0, nil,
    Pointer(StartDir), StartupInfo, ProcessInfo) then
  begin
    CloseHandle(ProcessInfo.hThread);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    Result := -1;
end;
{******************************************************************************}
function GetTempDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(Length(Result), PChar(Result)));
  Result := ExcludeTrailingPathDelimiter(Result);
  if Result = '' then
    Result := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'));
  if Result = '' then
    Result := '.';
end;
{******************************************************************************}
function RegReadStr(Reg: HKEY; const Name: string): string;
var
  Len: Longint;
  Buf: array[0..MAX_PATH] of Char;
begin
  Len := MAX_PATH * SizeOf(Char);
  case RegQueryValueEx(Reg, PChar(Name), nil, nil, PByte(@Buf[0]), @Len) of
    ERROR_SUCCESS:
      SetString(Result, Buf, Len div SizeOf(Char) - 1); // Len contains the #0, Len containts the byte size
    ERROR_MORE_DATA:
      begin
        SetLength(Result, Len div SizeOf(Char) - 1);
        if RegQueryValueEx(Reg, PChar(Name), nil, nil, PByte(Result), @Len) = ERROR_SUCCESS then
          SetLength(Result, Len div SizeOf(Char) - 1) // Len contains the #0, Len containts the byte size
        else
          Result := '';
      end;
  else
    Result := '';
  end;
end;
{******************************************************************************}
procedure FindDxgettext(Version: Integer);
var
  reg: HKEY;
  i: Integer;
  S: string;
begin
 // dxgettext detection
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'bplfile\Shell\Extract strings\Command', 0, KEY_QUERY_VALUE or KEY_READ, reg) <> ERROR_SUCCESS then
    Exit;
  S := RegReadStr(reg, '');
  SetLength(S, StrLen(PChar(S)));
  RegCloseKey(reg);

  if S <> '' then
  begin
    if S[1] = '"' then
    begin
      Delete(S, 1, 1);
      i := 1;
      while (i <= Length(S)) and (S[i] <> '"') do
        Inc(i);
      SetLength(S, i - 1);
    end;
    S := ExtractFileDir(S);
    DxgettextDir := S;
    if not FileExists(DxgettextDir + '\msgfmt.exe') then
      DxgettextDir := ''
    else
    begin
      if ExtraUnitDirs <> '' then
        ExtraUnitDirs := ExtraUnitDirs + ';' + S
      else
        ExtraUnitDirs := S;
    end;
  end;
end;
{******************************************************************************}
{ GetJediVersionFromCode returns the exact version number of the JCL or JVCL by
  compiling a simple test application that writes the exact version number to a
  file.
  This makes it possible to install the JVCL if the JCL was installed by hand. }
function GetJediVersionFromCode(const Target: TTarget; LibraryType: TLibraryType): string;
var
  f: TextFile;
  TestFilename, VersionOutputFilename: string;
  Status: Integer;
  CmdLine: string;
  LastError: Cardinal;
begin
  Result := '';

  // Get the JCL/JVCL Version number from the JCL/JVCL itself by compiling and
  // starting a test application.
  TestFilename := GetTempDir + '\JediVersionCheck.dpr';
  VersionOutputFilename := ChangeFileExt(TestFilename, '.output');
  DeleteFile(PChar(VersionOutputFilename));
  AssignFile(f, Testfilename);
  {$I-}
  Rewrite(f);
  WriteLn(f, 'program JediVersionCheck;');
  if LibraryType = ltJCL then
    WriteLn(f, 'uses JclBase;')
  else
    WriteLn(f, 'uses JVCLVer;');
  WriteLn(f, 'var f: TextFile;');
  WriteLn(f, 'begin');
  WriteLn(f, '  ExitCode := 0;');
  WriteLn(f, '  AssignFile(f, ''' + VersionOutputFilename + ''');');
  WriteLn(f, '  {$I-}');
  WriteLn(f, '  Rewrite(f);');
  if LibraryType = ltJCL then
    WriteLn(f, '  WriteLn(f, JclVersionMajor, ''.'', JclVersionMinor, ''.'', JclVersionRelease, ''.'', JclVersionBuild);')
  else
    WriteLn(f, '  WriteLn(f, JvclVersionMajor, ''.'', JvclVersionMinor, ''.'', JvclVersionRelease, ''.'', JvclVersionBuild);');
  WriteLn(f, '  CloseFile(f);');
  WriteLn(f, '  if IOResult <> 0 then');
  WriteLn(f, '    ExitCode := 1;');
  WriteLn(f, '  {$I+}');
  WriteLn(f, 'end.');
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(ErrOutput, 'Failed to write file ', TestFilename);
    DeleteFile(PChar(TestFilename));
  end
  else
  begin
    // compile <TestFilename>.dpr
    CmdLine := '"' + Target.RootDir + '\bin\dcc32.exe" ' +
               '-Q -E. -N. -U"' + Target.LibDirs + '" ' + ExtractFileName(TestFilename);
    Status := Execute(CmdLine, ExtractFileDir(TestFilename), {HideOutput:}True);
    LastError := GetLastError;
    DeleteFile(PChar(TestFilename));
    if LibraryType = ltJCL then
      DeleteFile(PChar(ExtractFileDir(TestFilename) + '\JclBase.dcu'))
    else
    begin
      DeleteFile(PChar(ExtractFileDir(TestFilename) + '\JVCLVer.dcu'));
      DeleteFile(PChar(ExtractFileDir(TestFilename) + '\JclUnitVersioning.dcu'));
    end;
    if Status <> 0 then
    begin
      if Status = -1 then
        FailedToStart(CmdLine, LastError)
      else
        ;//WriteLn(ErrOutput, 'Compilation of "', TestFilename, '" failed.');
      Exit;
    end;
    // start <TextFilename>.exe
    Status := Execute('"' + ChangeFileExt(TestFilename, '.exe') + '"',
                      ExtractFileDir(TestFilename), False);
    DeleteFile(PChar(ChangeFileExt(TestFilename, '.exe')));
    if Status <> 0 then
    begin
      if Status = -1 then
        WriteLn(ErrOutput, '"' + ChangeFileExt(TestFilename, '.exe') + '"');
    end
    else
    begin
      AssignFile(f, VersionOutputFilename);
      {$I-}
      Reset(f);
      ReadLn(f, Result);
      CloseFile(f);
      {$I+}
      if IOResult <> 0 then
        Result := '';
    end;
    DeleteFile(PChar(VersionOutputFilename));
  end;
end;
{******************************************************************************}
{ ReadTargetInfo }
function ReadTargetInfo(Typ: TTargetType; IDEVersion: Integer): TTarget;
var
  Reg: HKEY;
  IDEVersionStr: string;
  JediLibDirs, Dir, DcpDir, RootDir, Filename: string;
  IsSourceFile: Boolean;
begin
  Result.Typ := ttNone;
  Result.Version := 0;
  Result.IDEVersion := 0;
  Result.RootDir := '';
  Result.KeyName := '';
  Result.Name := '';
  Result.Id := '';
  Result.InstalledJcl := False;
  Result.JclVersion := '';
  Result.InstalledJvcl := False;
  Result.JvclVersion := '';
  Result.SearchPaths := '';
  Result.LibDirs := '';

  IDEVersionStr := IntToStr(IDEVersion);
  case Typ of
    ttDelphi:
      begin
        Result.KeyName := 'Software\Borland\Delphi\' + IDEVersionStr + '.0';
        Result.Id := 'd';
      end;
    ttBCB:
      begin
        Result.KeyName := 'Software\Borland\C++Builder\' + IDEVersionStr + '.0';
         Result.Id := 'c';
      end;
    ttBDS:
      begin
        if IDEVersion < 6 then
          Result.KeyName := 'Software\Borland\BDS\' + IDEVersionStr + '.0'
        else
        if IDEVersion < 8 then
          Result.KeyName := 'Software\Codegear\BDS\' + IDEVersionStr + '.0'
        else
          Result.KeyName := 'Software\Embarcadero\BDS\' + IDEVersionStr + '.0';
        Result.Id := 'd';
      end;
  end;

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(Result.KeyName), 0,
                  KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
  begin
    Result.RootDir := ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'RootDir'));
    RegCloseKey(Reg);
    if Result.RootDir = '' then
      Exit;
    Result.Version := IDEVersion;
    case Typ of 
      ttBDS:
        begin
          if IDEVersion <= 2 then // C#Builder 1 and Delphi 8 can't build the installer
          begin
            Result.Typ := ttNone;
            Result.Version := 0;
            Result.IDEVersion := 0;
            Result.RootDir := '';
            Result.KeyName := '';
            Result.Id := '';
            Exit;
          end;
          if IDEVersion < 7 then
            Inc(Result.Version, 6)  // 3.0 => 9
          else if IDEVersion < 14 then
            Inc(Result.Version, 7) // 7.0 => 14
          else
            Inc(Result.Version, 6) // 14.0 => 20  // there is no 13.0
        end;
      ttDelphi,
      ttBCB:
        begin
          if IDEVersion < 6 then   // Delphi 5 and under are not supported
          begin
            Result.Typ := ttNone;
            Result.Version := 0;
            Result.IDEVersion := 0;
            Result.RootDir := '';
            Result.KeyName := '';
            Result.Id := '';
            Exit;
          end;
        end;
    end;
    Result.Typ := Typ;
    Result.IDEVersion := IDEVersion;
    Result.Id := Result.Id + IntToStr(Result.Version);

    Result.Name := 'Delphi ' + IntToStr(Result.Version);
    case Result.Typ of
      ttDelphi:
        Result.Name := 'Delphi ' + IntToStr(Result.Version);
      ttBCB:
        Result.Name := 'C++Builder ' + IntToStr(Result.Version);
      ttBDS:
        case Result.IDEVersion of
          1: Result.Name := 'C#Builder';
          2: Result.Name := 'Delphi 8';
          3: Result.Name := 'Delphi 2005';
          4: Result.Name := 'Borland Developer Studio 2006';
          5: Result.Name := 'CodeGear Delphi 2007 for Win32';
          6: Result.Name := 'CodeGear RAD Studio 2009';
          7: Result.Name := 'Embarcadero RAD Studio 2010';
          8: Result.Name := 'Embarcadero RAD Studio XE';
         17: Result.Name := 'Embarcadero RAD Studio 10 Seattle';
       else
         if Result.IDEVersion > 17 then
           Result.Name := 'Embarcadero RAD Studio 10.' + IntToStr(Result.IDEVersion - 17) // just a guess
         else if Result.IDEVersion > 13 then
           Result.Name := 'Embarcadero RAD Studio XE' + IntToStr(2 + (Result.IDEVersion - 10))
         else
           Result.Name := 'Embarcadero RAD Studio XE' + IntToStr(2 + (Result.IDEVersion - 9));
       end;
    end;

    if (Result.Typ = ttBDS) and (Result.IDEVersion >= 8) then
      Result.LibDirs := Result.RootDir + '\lib\win32\release'
    else
    begin
      Result.LibDirs := Result.RootDir + '\Lib';
      if DirectoryExists(Result.RootDir + '\Lib\Obj') then
        Result.LibDirs := Result.LibDirs + ';' + Result.RootDir + '\Lib\Obj';
    end;


    { Read IDE search paths }
    if RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Library'), 0,
                    KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS then
    begin
      Result.SearchPaths := ExpandDirMacros(ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'Search Path')), Result.RootDir);
      RegCloseKey(Reg);
      if UseSearchPaths then
        Result.LibDirs := Result.LibDirs + ';' + Result.SearchPaths;
    end;

    { Read JCL information }
    JediLibDirs := '';
    if RequireJcl and
       (RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Jedi\JCL'), 0,
                      KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS) then
    begin
      if Result.Version >= 16 then
        RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Jedi\JCL\Win32'), 0, KEY_QUERY_VALUE or KEY_READ, Reg);

      DcpDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'DcpDir')), Result.RootDir));
      RootDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'RootDir')), Result.RootDir));
      Result.JclVersion := RegReadStr(Reg, 'Version');
      RegCloseKey(Reg);
      Dir := RootDir + '\lib\' + Result.Id;
      if not UseJclSource and
         FileExists(Dir + '\JclBase.dcu') then
      begin
        if not SameText(Dir, DcpDir) then
          JediLibDirs := JediLibDirs + ';' + Dir + ';' + DcpDir
        else
          JediLibDirs := JediLibDirs + ';' + Dir;
        JediLibDirs := JediLibDirs + ';' + RootDir + '\source;' + RootDir + '\source\include';
        Result.InstalledJcl := True;
      end
      else if FileExists(RootDir + '\source\common\JclBase.pas') then
      begin
        JediLibDirs := ';' + RootDir + '\source;' + RootDir + '\source\include;' + RootDir + '\source\common;' + RootDir + '\source\vcl;' +
                       RootDir + '\source\windows' + JediLibDirs; // JediLibDirs has leading ';'
        Result.InstalledJcl := True;
      end;
    end
    else if RequireJcl then
    begin
      { Registry key Jedi\JCL doesn't exist, maybe it was installed by hand.
        Try to find the JCL in the IDE's search paths. }
      if UseSearchPaths then
      begin
        IsSourceFile := True;
        Filename := FileSearch('JclBase.pas', Result.LibDirs);
        if Filename = '' then
        begin
          IsSourceFile := False;
          Filename := FileSearch('JclBase.dcu', Result.LibDirs);
        end;
        if Filename <> '' then
        begin
          Result.JclVersion := GetJediVersionFromCode(Result, ltJCL);
          if IsSourceFile then
          begin
            { Default directory is: jcl\source\common\JclBase.pas }
            RootDir := ExtractFileDir(ExtractFileDir(ExtractFileDir(Filename)));
            Dir := RootDir + '\lib\' + Result.Id;
          end
          else
          begin
            { Default directory is: jcl\lib\dxx\JclBase.dcu }
            Dir := ExtractFileDir(Filename);
            RootDir := ExtractFileDir(ExtractFileDir(Dir));
            if not FileExists(RootDir + '\source\common\JclBase.pas') then
              RootDir := '';
          end;

          if RootDir <> '' then
          begin
            JediLibDirs := JediLibDirs + ';' + Dir;
            JediLibDirs := JediLibDirs + ';' + RootDir + '\source;' + RootDir + '\source\include';
            Result.InstalledJcl := True;
          end
          else
          if not UseJclSource then
            Result.InstalledJcl := True;
        end;
      end;
    end;

    { Read JVCL information }
    if RequireJvcl and
       (RegOpenKeyEx(HKEY_CURRENT_USER, PChar(Result.KeyName + '\Jedi\JVCL'), 0,
                     KEY_QUERY_VALUE or KEY_READ, Reg) = ERROR_SUCCESS) then
    begin
      DcpDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'DcpDir')), Result.RootDir));
      RootDir := ExcludeTrailingPathDelimiter(ExpandDirMacros(ExcludeTrailingPathDelimiter(RegReadStr(Reg, 'RootDir')), Result.RootDir));
      Result.JvclVersion := RegReadStr(Reg, 'Version');
      RegCloseKey(Reg);
      Dir := RootDir + '\lib\' + Result.Id;
      if not UseJvclSource and FileExists(Dir + '\JVCLVer.dcu') then
      begin
        if not SameText(Dir, DcpDir) then
          JediLibDirs := JediLibDirs + ';' + Dir + ';' + DcpDir
        else
          JediLibDirs := JediLibDirs + ';' + Dir;
        JediLibDirs := JediLibDirs + ';' + RootDir + '\common;' + RootDir + '\Resources';
        Result.InstalledJvcl := True;
      end
      else if FileExists(RootDir + '\run\JVCLVer.pas') then
      begin
        JediLibDirs := ';' + RootDir + '\run;' + RootDir + '\common;' + RootDir + '\Resources' +
                       JediLibDirs; // JediLibDirs has leading ';'
        Result.InstalledJvcl := True;
      end;
    end
    else if RequireJvcl then
    begin
      { Registry key Jedi\JVCL doesn't exist, maybe it was installed by hand.
        Try to find the JVCL in the IDE's search paths. }
      if UseSearchPaths then
      begin
        IsSourceFile := True;
        Filename := FileSearch('JVCLVer.pas', Result.LibDirs);
        if Filename = '' then
        begin
          IsSourceFile := False;
          Filename := FileSearch('JVCLVer.dcu', Result.LibDirs);
        end;
        if Filename <> '' then
        begin
          Result.JvclVersion := GetJediVersionFromCode(Result, ltJVCL);
          if IsSourceFile then
          begin
            { Default directory is: jvcl3\run\JVCLVer.pas }
            RootDir := ExtractFileDir(ExtractFileDir(Filename));
            Dir := RootDir + '\lib\' + Result.Id;
          end
          else
          begin
            { Default directory is: jvcl3\lib\dxx\JVCLVer.dcu }
            Dir := ExtractFileDir(Filename);
            RootDir := ExtractFileDir(ExtractFileDir(Dir));
            if not FileExists(RootDir + '\run\JVCLVer.pas') then
              RootDir := '';
          end;

          if RootDir <> '' then
          begin
            JediLibDirs := JediLibDirs + ';' + Dir;
            JediLibDirs := JediLibDirs + ';' + RootDir + '\common;' + RootDir + '\Resources';
            Result.InstalledJvcl := True;
          end
          else
          if not UseJclSource then
            Result.InstalledJvcl := True;
        end;
      end;
    end;

    if JediLibDirs <> '' then
      //Result.LibDirs := Result.LibDirs + JediLibDirs; // leading ';' is already in JediLibDirs
      { Prefer our own files over outdated copies in other component libraries }
      Result.LibDirs := Copy(JediLibDirs, 2, MaxInt) + ';' + Result.LibDirs;
  end
  else
  begin
    Result.KeyName := '';
    Exit;
  end;
end;
{******************************************************************************}
{ TestDelphi6Update2 tests the Delphi 6 installation for the Update 2 which is
  required by JCL and JVCL. This is achieved by compiling and starting a simple
  test application that tests for the Update 2 Graphics.pas symbol clHotLight. }
procedure TestDelphi6Update2(const Target: TTarget);
var
  f: TextFile;
  TestFilename: string;
  Status: Integer;
  CmdLine: string;
  LastError: Cardinal;
begin
  // Test for Delphi 6 Update 2
  TestFilename := GetTempDir + '\delphi6compiletest.dpr';
  AssignFile(f, Testfilename);
  {$I-}
  Rewrite(f);
  WriteLn(f, 'program delphi6compiletest;');
  WriteLn(f, 'uses Windows, Graphics;');
  WriteLn(f, 'begin');
  WriteLn(f, '  ExitCode := ');
  WriteLn(f, '  {$IF declared(clHotLight)}');
  WriteLn(f, '  0;');
  WriteLn(f, '  {$ELSE}');
  WriteLn(f, '  1;');
  WriteLn(f, '  {$IFEND}');
  WriteLn(f, 'end.');
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn(ErrOutput, 'Failed to write file ', TestFilename);
    DeleteFile(PChar(TestFilename));
  end
  else
  begin
    // compile <TestFilename>.dpr
    CmdLine := '"' + Target.RootDir + '\bin\dcc32.exe" ' +
               '-Q -E. -N. -U"' + Target.LibDirs + '" ' + ExtractFileName(TestFilename);
    Status := Execute(CmdLine, ExtractFileDir(TestFilename), True);
    LastError := GetLastError;
    DeleteFile(PChar(TestFilename));
    if Status <> 0 then
    begin
      if Status = -1 then
        FailedToStart(CmdLine, LastError)
      else
        ;//WriteLn(ErrOutput, 'Compilation of "', TestFilename, '" failed.');
      Halt(1);
    end;

    // start <TextFilename>.exe
    CmdLine := '"' + ChangeFileExt(TestFilename, '.exe') + '"';
    Status := Execute(CmdLine, ExtractFileDir(TestFilename), False);
    LastError := GetLastError;
    DeleteFile(PChar(ChangeFileExt(TestFilename, '.exe')));
    if Status <> 0 then
    begin
      if Status = -1 then
        FailedToStart(CmdLine, LastError)
      else
      begin
        WriteLn(ErrOutput, 'Delphi 6 Update 2 is not installed.');
        MessageBox(0, 'Delphi 6 Update 2 is not installed.', 'dcc32ex.exe', MB_ICONERROR or MB_OK);
      end;
      Halt(1);
    end;
  end;
end;
{******************************************************************************}
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
{******************************************************************************}
function IsVersionCompatible(const RequiredVersion, Version: string): Boolean;
var
  ReqVer, Ver: Cardinal;
begin
  Result := False;
  if RequiredVersion = '' then
    Result := True
  else
  if Version <> '' then
  begin
    ReqVer := ParseVersionNumber(RequiredVersion);
    Ver := ParseVersionNumber(Version);
    Result := ReqVer < Ver;
  end;
end;
{******************************************************************************}
procedure CheckTargets(const PreferedTyp: TTargetType; const PreferedVersion: Integer; var NewestTarget: TTarget; ShowErrors: Boolean);
var
  PreferedTarget: TTarget;
  IDEVersion: Integer;
  Typ: TTargetType;
  Target: TTarget;
  InvalidFound: Boolean;
  DependencyCheckFailed: Boolean;
  ErrMsg: string;
begin
  PreferedTarget.Typ := ttNone;

  DependencyCheckFailed := False;
  InvalidFound := False;
  for Typ := ttFirst to High(TTargetType) do
  begin
    for IDEVersion := 1 to 40 do
    begin
      Target := ReadTargetInfo(Typ, IDEVersion);
      if (Target.Typ <> ttNone) and (Target.Version >= 5) then
      begin
        // is the target valid
        if FileExists(Target.RootDir + '\bin\dcc32.exe') and
           (FileExists(Target.RootDir + '\lib\System.dcu') or FileExists(Target.RootDir + '\lib\obj\System.dcu') or
            FileExists(Target.RootDir + '\lib\win32\release\System.dcu')) then
        begin
          if (not RequireJcl or (Target.InstalledJcl and IsVersionCompatible(RequireJclVersion, Target.JclVersion))) and
             (not RequireJvcl or (Target.InstalledJvcl and IsVersionCompatible(RequireJvclVersion, Target.JvclVersion))) then
          begin
            if (NewestTarget.Typ = ttNone) or (NewestTarget.Version < Target.Version) then
              NewestTarget := Target;

            if (Target.Typ = PreferedTyp) and (Target.Version = PreferedVersion) then
              PreferedTarget := Target;
          end
          else
          begin
            if ShowErrors then
            begin
              WriteLn('Missing dependencies for ', Target.Name);

              if RequireJcl and not Target.InstalledJcl then
                WriteLn(' - JCL  is required but not installed. (https://github.com/project-jedi/jcl)')
              else if RequireJcl and Target.InstalledJcl and
                      not IsVersionCompatible(RequireJclVersion, Target.JclVersion) then
                WriteLn(' - JCL  version ', Target.JclVersion, ' is too old. Version ', RequireJclVersion, ' is required.');

              if RequireJvcl and not Target.InstalledJvcl then
                WriteLn(' - JVCL is required but not installed. (https://github.com/project-jedi/jcl)')
              else if RequireJvcl and Target.InstalledJvcl and
                      not IsVersionCompatible(RequireJvclVersion, Target.JvclVersion) then
                WriteLn(' - JVCL version ', Target.JvclVersion, ' is too old. Version ', RequireJvclVersion, ' is required.');
              WriteLn;
            end;
            DependencyCheckFailed := True;
            InvalidFound := True;
          end;
        end
        else
        begin
          if ShowErrors then
          begin
            WriteLn(Target.Name, ' is no valid installation');
            if not DirectoryExists(Target.RootDir) then
              WriteLn(' - RootDir registry entry is not valid')
            else
            begin
              if not FileExists(Target.RootDir + '\bin\dcc32.exe') then
                WriteLn(' - dcc32.exe missing (Evaluation version and TurboExplorer are not supported) ');
              if not (FileExists(Target.RootDir + '\lib\System.dcu') or FileExists(Target.RootDir + '\lib\obj\System.dcu') or
                FileExists(Target.RootDir + '\lib\win32\release\System.dcu')) then
                WriteLn(' - System.dcu missing');
            end;
            WriteLn;
          end;
          InvalidFound := True;
        end;
      end;
    end;
  end;

  if PreferedTarget.Typ <> ttNone then
    NewestTarget := PreferedTarget;

  if ShowErrors and (NewestTarget.Typ = ttNone) then
  begin
    if InvalidFound then
    begin
      if DependencyCheckFailed then
        ErrMsg := 'No Delphi/BCB/BDS/RAD-Studio versions was found that has the required' + sLineBreak +
                  'dependencies installed. Please install the dependencies first.'
      else
        ErrMsg := 'No valid Delphi/BCB/BDS version found. Are your registry settings correct?';
    end
    else
      ErrMsg := 'No Delphi/BCB/BDS version installed.';
    WriteLn;
    WriteLn(ErrOutput, ErrMsg);
    MessageBox(0, PChar(ErrMsg), 'dcc32ex.exe', MB_ICONERROR or MB_OK);
  end;
end;
{******************************************************************************}
function SkipOption(CmdLine: PChar): PChar;
begin
  Result := CmdLine;
  if Result <> nil then
  begin
    if Result[0] = '"' then
    begin
      Inc(Result);
      while (Result[0] <> #0) and (Result[0] <> '"') do
        Inc(Result);
      if Result[0] = '"' then
        Inc(Result);
    end
    else
    begin
      while (Result[0] <> #0) and (Result[0] <> ' ') and (Result[0] <> #9) do
      begin
        if Result[0] = '"' then // embedded quotes:  -U"C:\Program Files\Borland\Delphi7\Lib"
        begin
          Inc(Result);
          while (Result[0] <> #0) and (Result[0] <> '"') do
            Inc(Result);
          if Result[0] = '"' then
            Inc(Result);
        end;
        Inc(Result);
      end;
      if (Result[0] = ' ') or (Result[0] = #9) then
        Inc(Result);
    end;

    // skip whitespaces
    while Result[0] = ' ' do
      Inc(Result);

    if Result[0] = #0 then
      Result := nil;
  end;
end;
{******************************************************************************}
function ParseParams(CmdLine: PChar): PChar;
var
  S: string;
  i: Integer;
begin
  Result := CmdLine;
  while CmdLine <> nil do
  begin
    CmdLine := SkipOption(Result);
    if CmdLine = nil then
      S := Result
    else
      SetString(S, Result, CmdLine - Result);

    // delete right spaces
    i := Length(S);
    while (i > 0) and (S[i] = ' ') do
      Dec(i);
    if i <> Length(S) then
      S := Copy(S, 1, i);

    if (S <> '') and (S[1] = '"') then
      S := Copy(S, 2, Length(S) - 2);
    if StartsText('--delphi-version=', S) then
      SetEnvironmentVariable('DELPHIVERSION', PChar(Copy(S, 18, MaxInt)))
    else
    if SameText(S, '--verbose') then
      Verbose := True
    else
    if SameText(S, '--preserve-config') then
       PreserveConfig := True
    else
    if SameText(S, '--use-search-paths') then
      UseSearchPaths := True
    else
    if SameText(S, '--requires-jcl') then
      RequireJcl := True
    else
    if SameText(S, '--requires-jvcl') then
      RequireJvcl := True
    else
    if StartsText('--requires-jcl=', S) then
    begin
      RequireJcl := True;
      RequireJclVersion := Copy(S, 16, MaxInt);
    end
    else
    if SameText('--requires-jvcl=', S) then
    begin
      RequireJvcl := True;
      RequireJvclVersion := Copy(S, 16, MaxInt);
    end
    else
    if SameText('--use-jcl-source', S) then
      UseJclSource := True
    else
    if SameText('--use-jvcl-source', S) then
      UseJvclSource := True
    else
    if SameText('--runtime-package-rtl', S) then
      RuntimePackageRtl := True
    else
    if SameText('--runtime-package-vcl', S) then
      RuntimePackageVcl := True
    else
      Break;
    Result := CmdLine;
  end;
end;

{******************************************************************************}
{******************************************************************************}
var
  NewestTarget: TTarget;
  f: TextFile;
  Status: Integer;
  Dcc32Cfg, CurDir, ExtraOpts: string;
  CmdLine: PChar;
  DelphiVersion: string;
  PreferedTyp: TTargetType;
  PreferedVersion: Integer;
  Err: Integer;
  Target: TTarget;
  Dcc32CmdLine: string;
begin
  CmdLine := GetCommandLine;
  CmdLine := SkipOption(CmdLine); // skip executable name
  CmdLine := ParseParams(CmdLine);

  PreferedTyp := ttNone;
  PreferedVersion := 0;
  DelphiVersion := GetEnvironmentVariable('DELPHIVERSION');
  if DelphiVersion <> '' then
  begin
    Val(Copy(DelphiVersion, 2, MaxInt), PreferedVersion, Err);
    if (Err = 0) and (PreferedVersion >= 5) then
    begin
      if (DelphiVersion[1] = 'D') or (DelphiVersion[1] = 'd') then
        PreferedTyp := ttDelphi;
      if (DelphiVersion[1] = 'C') or (DelphiVersion[1] = 'c') then
      begin
        if PreferedVersion <> 7 then
          PreferedTyp := ttBCB;
      end;
      if PreferedVersion > 7 then
        PreferedTyp := ttBDS;
    end;
  end;

  NewestTarget.Typ := ttNone;
  CheckTargets(PreferedTyp, PreferedVersion, NewestTarget, Verbose);

  if NewestTarget.Typ = ttNone then
  begin
    if not Verbose then
    begin
      { Show detection errors and warnings }
      NewestTarget.Typ := ttNone;
      CheckTargets(PreferedTyp, PreferedVersion, NewestTarget, True);
    end;
    Halt(1);
  end;


  Target := NewestTarget;
  WriteLn('Using ', Target.Name);
  if Target.Version = 6 then
    TestDelphi6Update2(Target);

  ExtraOpts := '';
  // dxgettext
  FindDxgettext(Target.Version);
  if ExtraUnitDirs <> '' then
  begin
    Target.LibDirs := Target.LibDirs + ';' + ExtraUnitDirs;
    ExtraOpts := ExtraOpts + '-DUSE_DXGETTEXT ';
  end;

  // start dcc32.exe
  GetDir(0, CurDir);
  CurDir := ExcludeTrailingPathDelimiter(CurDir);
  Dcc32Cfg := CurDir + '\dcc32.cfg';
  SetFileAttributes(PChar(Dcc32Cfg), FILE_ATTRIBUTE_NORMAL);
  AssignFile(f, Dcc32Cfg);
  {$I-}
  Rewrite(f);
  WriteLn(f, '-U"' + Target.LibDirs + '"');
  WriteLn(f, '-I"' + Target.LibDirs + '"');
  WriteLn(f, '-R"' + Target.LibDirs + '"');
  WriteLn(f, '-O"' + Target.LibDirs + '"');
  if RuntimePackageRtl then
    WriteLn(f, '-LUrtl');
  if RuntimePackageVcl then
    WriteLn(f, '-LUvcl');
  if Target.Version >= 16 then
    WriteLn(f, '-nsSystem;System.Win;WinAPI;Vcl;Vcl.Imaging');
  CloseFile(f);
  {$I+}
  if IOResult <> 0 then
  begin
    //WriteLn(ErrOutput, 'Failed to write file ', Dcc32Cfg);
    ExtraOpts := ExtraOpts + '-U"' + Target.LibDirs + '" -I"' + Target.LibDirs + '" -R"' + Target.LibDirs + '" -O"' + Target.LibDirs + '" ';
    if RuntimePackageRtl then
      ExtraOpts := ExtraOpts + '-LUrtl ';
    if RuntimePackageVcl then
      ExtraOpts := ExtraOpts + '-LUvcl ';
    DeleteFile(PChar(Dcc32Cfg));
    Dcc32Cfg := '';
  end;

  Dcc32CmdLine := '"' + Target.RootDir + '\bin\dcc32.exe" ' + ExtraOpts + CmdLine;
  if Verbose then
  begin
    WriteLn('Using search path: ', Target.LibDirs);
    if PreserveConfig then
    begin
      WriteLn('===============================================================================');
      WriteLn(Dcc32CmdLine);
      WriteLn('===============================================================================');
    end;
  end;
  WriteLn;

  if PreserveConfig then
  begin
    AssignFile(f, CurDir + '\dcc32_command.cmd');
    {$I-}
    Rewrite(f);
    WriteLn(f, Dcc32CmdLine);
    CloseFile(f);
    {$I+}
    IOResult; // ignore all errors
  end;

  Status := Execute(Dcc32CmdLine, CurDir, False);
  if (Dcc32Cfg <> '') and not PreserveConfig then
    DeleteFile(PChar(Dcc32Cfg));

  if ParamCount = 0 then
  begin
    WriteLn;
    WriteLn('Additional options (must be specified before any dcc32 parameter):');
    WriteLn('  --delphi-version=d25   Prefer this version, overrides environment variable');
    WriteLn('  --verbose              Show warnings and errors during the compiler detection');
    WriteLn('  --use-search-paths     Use the IDE''s search paths');
    WriteLn('  --preserve-config      Keep the dcc32.cfg file and create a dcc32_command.cmd');
    WriteLn('  --requires-jcl         Requires an installed JCL');
    WriteLn('  --requires-jvcl        Requires an installed JVCL');
    WriteLn('  --use-jcl-source       Use the source code instead of the DCUs for the JCL');
    WriteLn('  --use-jvcl-source      Use the source code instead of the DCUs for the JVCL');
    WriteLn('  --runtime-package-rtl  Link the executable against the rtl package');
    WriteLn('  --runtime-package-vcl  Link the executable against the vcl package');
    WriteLn;
    WriteLn('Environment variables:');
    WriteLn('  DELPHIVERSION = d25    Prefer this Delphi/BCB/BDS version');
    WriteLn('                         (d6, d7, c6, d9, d10, d11, d12, d14, ..., d25, ...)');
  end;

  ExitCode := Status;
  {if DebugHook <> 0 then
    ReadLn;}
end.