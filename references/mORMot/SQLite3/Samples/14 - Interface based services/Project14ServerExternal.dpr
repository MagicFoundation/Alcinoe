program Project14ServerExternal;

{ this sample will create the main SQLite3 DB as in-memory, but will define all
  tables as external, in the same .db file than Project14Server
  -> just to demonstrate VirtualTableExternalRegisterAll() function and
  reproduce the https://synopse.info/forum/viewtopic.php?id=1008 issue }

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  mORMot,
  mORMotSQLite3,
  SynCommons, SynLog,
  SynDB,
  SynDBSQLite3, SynSQLite3, SynSQLite3Static,
  mORMotDB,
  Project14Interface;

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

var
  aModel: TSQLModel;
  aProps: TSQLDBSQLite3ConnectionProperties;
begin
  aProps := TSQLDBSQLite3ConnectionProperties.Create(
    StringToUtf8(ChangeFileExt(ExeVersion.ProgramFileName,'.db')),'','','');
  try
    aModel := TSQLModel.Create([TSQLAuthGroup,TSQLAuthUser],ROOT_NAME);
    VirtualTableExternalRegisterAll(aModel,aProps);
    try
      with TSQLRestServerDB.Create(aModel,SQLITE_MEMORY_DATABASE_NAME,true) do
      try
        CreateMissingTables; // we need AuthGroup and AuthUser tables
        ServiceDefine(TServiceCalculator,[ICalculator],sicShared);
        if ExportServerNamedPipe(APPLICATION_NAME) then
          writeln('Background server is running.'#10) else
          writeln('Error launching the server'#10);
        write('Press [Enter] to close the server.');
        readln;
      finally
        Free;
      end;
    finally
      aModel.Free;
    end;
  finally
    aProps.Free;
  end;
end.
