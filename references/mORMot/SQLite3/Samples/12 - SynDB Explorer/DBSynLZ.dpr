/// command-line tool for compressing/uncompressing .dbsynlz files
program DBSynLZ;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SynCommons,
  SysUtils;

{$R SynDBExplorer.res}

const
  /// the "magic" number used to identify .dbsynlz compressed files, as
  // created by TSQLDataBase.BackupSynLZ() or if SynLZCompress parameter is TRUE
  // for the TSQLDataBase.BackupBackground() method
  // - note that the SynDBExplorer tool is able to recognize such files, and
  // open them directly - or use this DBSynLZ.dpr command-line sample tool
  SQLITE3_MAGIC = $ABA5A5AB;

function IsSQLite3File(const FileName: TFileName): boolean;
var F: integer;
    Header: array[0..15] of AnsiChar;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F<0 then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      (Header='SQLite format 3');
    FileClose(F);
  end;
end;

procedure Process(const FN: TFileName);
var f, dest: TFileName;
    timer: TPrecisionTimer;
begin
  timer.Start;
  f := ExtractFileName(FN);
  if ParamCount = 2 then
    dest := ParamStr(2);
  if FileIsSynLZ(FN, SQLITE3_MAGIC) then begin
    if dest = '' then
      dest := ChangeFileExt(FN, '.db');
    write('Decompressing ', f, #13#10' into ', ExtractFileName(dest), '...');
    if FileExists(dest) then
      writeln(' FAILED: already existing')
    else if FileUnSynLZ(FN, dest, SQLITE3_MAGIC) then
      writeln(' OK in ', timer.Stop)
    else
      writeln(' FAILED');
  end else
  if IsSQlite3File(FN) then begin
    if dest = '' then
      dest := ChangeFileExt(FN, '.dbsynlz');
    write('Compressing ', f, #13#10' into ', ExtractFileName(dest), '...');
    if FileExists(dest) then
      writeln(' FAILED: already existing')
    else if FileSynLZ(FN, dest, SQLITE3_MAGIC) then
      writeln(' OK in ', timer.Stop)
    else
      writeln(' FAILED');
  end else
    writeln(f, ' is not a Sqlite3 compressed/uncompressed file');
end;

begin
  if ParamCount < 1 then  begin
    TextColor(ccLightGreen);
    writeln(#13#10' SQLite3 database files compression/decompression tool via SynLZ');
    writeln(StringOfChar('-',65));
    TextColor(ccGreen);
    writeln('  Using SynLZ compression from Synopse mORMot ', SYNOPSE_FRAMEWORK_VERSION);
    TextColor(ccLightGray);
    writeln(#13#10'  DBSynLZ filename.dbsynlz [outfilename.db]'#13#10 +
      'or'#13#10'  DBSynLZ filename.db [outfilename.dbsynlz]');
  end
  else
    Process(ParamStr(1));
end.
 
