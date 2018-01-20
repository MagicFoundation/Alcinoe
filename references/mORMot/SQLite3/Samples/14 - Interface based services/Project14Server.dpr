program Project14Server;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons, SynLog, mORMot,
  mORMotSQLite3, SynSQLite3Static,
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
begin
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    with TSQLRestServerDB.Create(aModel,ChangeFileExt(ExeVersion.ProgramFileName,'.db'),true) do
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
end.
