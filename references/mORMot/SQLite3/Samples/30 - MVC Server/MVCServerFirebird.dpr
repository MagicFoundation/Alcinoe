/// MVC sample web application, publishing a simple BLOG and PostgreSQL as DB
program MVCServerFirebird;

{$APPTYPE CONSOLE}


// please select one of the two! :)
{$define USEZEOSFIREBIRD}
{.$define USEFIREDACFIREBIRD}

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  SynCrtSock,
  SynCommons,
  SynLog,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotMVC,
  SynDB,
  mORMotDB,
  {$ifdef USEZEOSFIREBIRD}
  SynDBZeos,  
  {$endif}
  {$ifdef USEFIREDACFIREBIRD}
  SynDBFireDAC,
  {$ifdef ISDELPHIXE5} FireDAC.Phys.IB, {$else} uADPhysIB, {$endif}
  {$endif}
  MVCModel,
  MVCViewModel,
  SysUtils;

var aModel: TSQLModel;
    {$ifdef USEFIREDACFIREBIRD}
    aURI: RawUTF8;
    aDriver: TADPhysIBDriverLink;
    {$endif}
    aExternalDB: TSQLDBConnectionPropertiesThreadSafe;
    aServer: TSQLRestServerDB;
    aApplication: TBlogApplication;
    aHTTPServer: TSQLHttpServer;
begin
  with TSQLLog.Family do
    Level := LOG_VERBOSE;
  aModel := CreateModel;
  try
    {$ifdef USEZEOSFIREBIRD}
    aExternalDB := TSQLDBZEOSConnectionProperties.Create(
      TSQLDBZEOSConnectionProperties.URI(dFIREBIRD,'',
      '..\15 - External DB performance\Firebird\fbembed.dll'),
      'MVCServerFirebird.fdb','sysdba','masterkey');
    aExternalDB.ThreadingMode := tmMainConnection; // as expected for FB embedded
    {$endif}
    {$ifdef USEFIREDACFIREBIRD}
    aDriver := TADPhysIBDriverLink.Create(nil);
    aDriver.VendorLib := '..\15 - External DB performance\Firebird\fbembed.dll';
    aURI := FIREDAC_PROVIDER[dFirebird];
    if not FileExists('MVCServerFirebird.fdb') then
      aURI := aURI+'?CreateDatabase=Yes';
    aExternalDB := TSQLDBFireDACConnectionProperties.Create(
      aURI,'MVCServerFirebird.fdb','sysdba','masterkey');
    {$endif}
    try
      VirtualTableExternalRegisterAll(aModel,aExternalDB,[regMapAutoKeywordFields]);
      aServer := TSQLRestServerDB.Create(aModel,SQLITE_MEMORY_DATABASE_NAME);
      try
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
  {$ifdef USEFIREDACFIREBIRD}
  aDriver.Free;
  {$endif}
end.
