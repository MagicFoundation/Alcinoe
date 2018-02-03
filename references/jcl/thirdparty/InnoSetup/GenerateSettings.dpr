program GenerateSettings;

uses
  {$IF CompilerVersion >= 23.0} // XE2+
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$IFEND}
  JclBase;

var
  f: TextFile;
  Version: Integer;
begin
  Version := Trunc(CompilerVersion) - 7;

  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Settings.iss');
  Rewrite(f);
  WriteLn(f, '#define Include_Delphi', Version);
  WriteLn(f, '#define JclVersionStr "', JclVersionMajor, '.', JclVersionMinor, '.', JclVersionRelease, '.', JclVersionBuild, '"');
  CloseFile(f);
end.