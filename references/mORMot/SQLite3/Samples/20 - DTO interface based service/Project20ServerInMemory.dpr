/// this server will use TSQLRestServerFullMemory kind of in-memory server
program Project20ServerInMemory;

{$APPTYPE CONSOLE}

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotHttpServer,
  Project20Interface;

type
  TAirportService = class(TInterfacedObject, IAirportService)
  public
    procedure GetAirportDefinition(const AirPortID: integer; out Definition: TDTOAirportDefinition);
  end;


{ TAirportService }

procedure TAirportService.GetAirportDefinition(const AirPortID: integer;
  out Definition: TDTOAirportDefinition);
begin
  // create an object from static data
  // (real application may use database and complex code to retrieve the values)
  with Definition.Airport.Add do begin
    Location := 'LAX';
    Terminal := TRawUTF8DynArrayFrom(['terminalA', 'terminalB', 'terminalC']);
    Gate := TRawUTF8DynArrayFrom(['gate1', 'gate2', 'gate3', 'gate4', 'gate5']);
    BHS := 'Siemens';
    DCS := 'Altiea';
  end;
  with Definition.Airline.Add do begin
    CX := TRawUTF8DynArrayFrom(['B777', 'B737', 'A380', 'A320']);
    QR := TRawUTF8DynArrayFrom(['A319', 'A380', 'B787']);
    ET := '380';
    SQ := 'A320';
  end;
  Definition.GroundHandler := TRawUTF8DynArrayFrom(['Swissport','SATS','Wings','TollData']);
end;

var
  aModel: TSQLModel;
  aDB: TSQLRestServer;
  aServer: TSQLHttpServer;
begin
  // set the logs level to only important events (reduce .log size)
  TSQLLog.Family.Level := LOG_STACKTRACE+[sllInfo,sllServer];
  // initialize the ORM data model
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    // create a fast in-memory ORM server
    aDB := TSQLRestServerFullMemory.Create(aModel,'test.json',false,false);
    try
      // register our TAirportServer implementation
      aDB.ServiceRegister(TAirportService,[TypeInfo(IAirportService)],sicShared);
      // launch the HTTP server
      aServer := TSQLHttpServer.Create(PORT_NAME,[aDB],'+',useHttpApiRegisteringURI);
      try
        aServer.AccessControlAllowOrigin := '*'; // allow cross-site AJAX queries
        writeln('Background server is running'#10);
        write('Press [Enter] to close the server.');
        ConsoleWaitForEnterKey;
      finally
        aServer.Free;
      end;
    finally
      aDB.Free;
    end;
  finally
    aModel.Free;
  end;
end.
