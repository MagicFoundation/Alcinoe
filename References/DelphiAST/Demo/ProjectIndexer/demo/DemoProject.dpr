program DemoProject;

{$APPTYPE CONSOLE}

{$R *.res}

// Search path: sub2

uses
  System.SysUtils,
  Unit1 in 'sub1\Unit1.pas',
  Unit2;
//  UnitA;

begin
  try
    Writeln(Unit1Folder);
    Writeln(Unit1.unitfolder);
    Writeln(Unit1FolderIndirect);
    Writeln(Unit2.unitfolder);
//    Writeln(UnitA.ID);
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
