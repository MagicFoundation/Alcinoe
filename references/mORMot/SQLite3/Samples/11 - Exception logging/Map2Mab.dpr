/// sample program to create .mab files from existing .map files
// - if some .map file name is specified (you can use wild chars), will
// process all those .map files, then create the corresponding .mab files
// - if some .exe/.dll file name is specified (you can use wild chars), will
// process all matching .exe/.dll files with an associated .map file, and will
// create the .mab files, then embedd the .mab content to the .exe/.dll
// - if no file name is specified, will process '*.map' into '*.mab'
// - you can make map2mapb.exe file small if you define LVCL as conditional in
// the Project options and set the ..\lib\LVCL directories as expected
program Map2Mab;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons,
  SynLog;

procedure Process(const FileName: TFileName);
var SR: TSearchRec;
    Path, FN: TFileName;
    Ext: integer;
    AllOk: boolean;
begin
  AllOk := True;
  Ext := GetFileNameExtIndex(FileName,'map,exe,dll,ocx,bpl');
  if (Ext>=0) and (FindFirst(FileName,faAnyFile,SR)=0) then
  try
    Path := ExtractFilePath(FileName);
    repeat
      FN := Path+SR.Name;
      if (SR.Name[1]<>'.') and (faDirectory and SR.Attr=0) then
      try
        with TSynMapFile.Create(FN,true) do // true = .map -> .mab
        try
          if not HasDebugInfo then begin
            WriteLn('Error: no Debug Info found on ',FN);
            AllOk := False;            
          end else if (Ext>0) then // has debug info and is not a map
            SaveToExe(FN);
        finally
          Free;
        end;
      except
        on E: Exception do begin // ignore any problem here: just print it and process next file
          WriteLn('Error: ', E.ClassName,' ',E.Message);
          AllOk := False;
        end;
      end;
    until FindNext(SR)<>0;
  finally
    FindClose(SR);
  end else begin
    WriteLn('Error: cant find any file to process matching: ', FileName);
    ExitCode := 2;
  end;
  if not AllOk then
    ExitCode := 1;
end;

begin
  if paramCount>0 then
    Process(paramstr(1)) else
    Process('*.map');
end.