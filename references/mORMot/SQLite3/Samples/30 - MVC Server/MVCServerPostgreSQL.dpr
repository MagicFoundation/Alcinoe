/// MVC sample web application, publishing a simple BLOG and PostgreSQL as DB
program MVCServerPostgreSQL;

{$APPTYPE CONSOLE}


// please select one of the two! :)
{$define USEZEOSPOSTGRESQL}
{.$define USEFIREDACPOSTGRESQL}

// direct ZDBC/FireDAC driver needs only libpq.dll and libintl.dll e.g. from
// http://www.enterprisedb.com/products-services-training/pgbindownload

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  SynCrtSock,
  SynCommons,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotMVC,
  SynDB,
  mORMotDB,
  {$ifdef USEZEOSPOSTGRESQL}
  SynDBZeos, // use at least R3435 testing-7.2 - see synopse.info/forum
  {$endif}
  {$ifdef USEFIREDACPOSTGRESQL}
  SynDBFireDAC,
  {$ifdef ISDELPHIXE5} FireDAC.Phys.PG, {$else} uADPhysPG, {$endif}
  {$endif}
  MVCModel,
  MVCViewModel,
  SysUtils;

var aModel: TSQLModel;
    aExternalDB: TSQLDBConnectionPropertiesThreadSafe;
    aServer: TSQLRestServerDB;
    aApplication: TBlogApplication;
    aHTTPServer: TSQLHttpServer;
begin
  aModel := CreateModel;
  try
    {$ifdef USEZEOSPOSTGRESQL}
    aExternalDB := TSQLDBZEOSConnectionProperties.Create(
      TSQLDBZEOSConnectionProperties.URI(dPostgreSQL,'localhost:5433'),
    {$endif}
    {$ifdef USEFIREDACPOSTGRESQL}
    aExternalDB := TSQLDBFireDACConnectionProperties.Create(
      'PG?Server=localhost;Port=5433',
    {$endif}
      'postgres','postgres','postgresPassword');
    try
      aExternalDB.ThreadingMode := tmMainConnection; // force SINGLE connection
      VirtualTableExternalRegisterAll(aModel,aExternalDB,[regMapAutoKeywordFields]);
      aServer := TSQLRestServerDB.Create(aModel,SQLITE_MEMORY_DATABASE_NAME);
      try // PostgreSQL uses one fork per connection -> better only two threads
        aServer.AcquireExecutionMode[execORMGet] := amBackgroundThread;
        aServer.AcquireExecutionMode[execORMWrite] := amBackgroundThread;
        aServer.CreateMissingTables;
        aApplication := TBlogApplication.Create;
        try
          aApplication.Start(aServer);
          aHTTPServer := TSQLHttpServer.Create('8092',aServer,'+',useHttpApiRegisteringURI);
          try
            aHTTPServer.RootRedirectToURI('blog/default'); // redirect localhost:8092
            writeln('"MVC Blog Server" launched on port 8092 using ',aHttpServer.HttpServer.ClassName);
            writeln(#10'You can check http://localhost:8092/blog/mvc-info for information');
            writeln('or point to http://localhost:8092 to access the web app.');
            writeln(#10'Press [Enter] to close the server.'#10);
            readln;
          finally
            aHTTPServer.Free;
          end;
        finally
          aApplication.Free;
        end;
      finally
        aServer.Free;
      end;
    finally
      aExternalDB.Free;
    end;
  finally
    aModel.Free;
  end;
end.
