/// minimal REST client for a list of Persons from RESTserver.exe
program RESTclient;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SynCommons,          // framework core
  mORMot,              // RESTful server & ORM
  mORMotHttpClient,    // HTTP client to a mORMot RESTful server
  RESTModel;           // data model unit, shared between server and client

var aModel: TSQLModel;
    aClient: TSQLHttpClient;
    aPerson: TPerson;
    aID: integer;
begin
  aModel := DataModel;
  try
    aClient := TSQLHttpClientWinHTTP.Create('localhost',SERVER_PORT,aModel);
    try
      writeln('Add a new TPerson');
      aPerson := TPerson.Create;
      try
        Randomize;
        aPerson.Name := 'Name'+Int32ToUtf8(Random(10000));
        aID := aClient.Add(aPerson,true);
      finally
        aPerson.Free;
      end;
      writeln('Added TPerson.ID=',aID);
      aPerson := TPerson.Create(aClient,aID);
      try
        writeln('Name read for ID=',aPerson.ID,' from DB = "',aPerson.Name,'"');
      finally
        aPerson.Free;
      end;
    finally
      aClient.Free;
    end;
    write(#10'Press [Enter] to quit');
    readln;
  finally
    aModel.Free;
  end;
end.
