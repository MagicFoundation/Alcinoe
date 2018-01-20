/// this server will demonstrate how to publish code generation wrappers
program Project14ServerHttpWrapper;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotHttpServer,
  mORMotWrappers,
  Project14Interface in '..\14 - Interface based services\Project14Interface.pas';

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
  aServer: TSQLRestServer;
  aHTTPServer: TSQLHttpServer;
begin
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    //EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  // create a Data Model
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    // initialize a TObjectList-based database engine
    aServer := TSQLRestServerFullMemory.Create(aModel,'test.json',false,true);
    try
      // add the http://localhost:888/root/wrapper code generation web page
      AddToServerWrapperMethod(aServer,
        ['..\..\..\CrossPlatform\templates','..\..\..\..\CrossPlatform\templates']);
      // register our ICalculator service on the server side
      aServer.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
      // launch the HTTP server
      aHTTPServer := TSQLHttpServer.Create(PORT_NAME,[aServer],'+',useHttpApiRegisteringURI);
      try
        aHTTPServer.AccessControlAllowOrigin := '*'; // for AJAX requests to work
        writeln(#10'Background server is running.'#10);
        writeln('Cross-Platform wrappers are available at localhost:',
          PORT_NAME,'/',ROOT_NAME,'/wrapper'#10);
        writeln(#10'Press [Enter] to close the server.'#10);
        readln;
      finally
        aHTTPServer.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
