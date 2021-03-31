/// MVC sample web application, publishing a simple BLOG
program MVCServer;

{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$ifdef CPUARM}
      //if GUI, then uncomment
      //{$linklib GLESv2}
    {$endif}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif MSWINDOWS}

{$I Synopse.inc} // define HASINLINE WITHLOG ONLYUSEHTTPSOCKET

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  SysUtils,
  SynCrtSock,
  SynCommons,
  SynTable,
  SynLog,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotMVC,
  MVCModel in 'MVCModel.pas',
  MVCViewModel in 'MVCViewModel.pas';

var aModel: TSQLModel;
    aServer: TSQLRestServerDB;
    aApplication: TBlogApplication;
    aHTTPServer: TSQLHttpServer;
begin
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    PerThreadLog := ptIdentifiedInOnFile;
    RotateFileCount := 10;
    RotateFileSizeKB := 20 shl 10;
    FileExistsAction := acAppend; // as expected by rotation
  end;
  aModel := CreateModel;
  try
    aServer := TSQLRestServerDB.Create(aModel,ChangeFileExt(ExeVersion.ProgramFileName,'.db'));
    try
      aServer.DB.Synchronous := smNormal;
      aServer.DB.LockingMode := lmExclusive;
      aServer.Options := aServer.Options+[rsoNoTableURI];
      aServer.CreateMissingTables;
      aApplication := TBlogApplication.Create;
      try
        aApplication.Start(aServer);
        aServer.ServiceMethodRegisterPublishedMethods('', aApplication);
        aHTTPServer := TSQLHttpServer.Create('8092',aServer
          {$ifndef ONLYUSEHTTPSOCKET},'+',useHttpApiRegisteringURI{$endif});
        try
          aHTTPServer.RootRedirectToURI('blog/default'); // redirect / to blog/default
          aServer.RootRedirectGet := 'blog/default';  // redirect blog to blog/default
          writeln('"MVC Blog Server" launched on port 8092 using ',aHttpServer.HttpServer.ClassName);
          writeln(#10'You can check http://localhost:8092/blog/mvc-info for information');
          writeln('or point to http://localhost:8092 to access the web app.');
          writeln(#10'Press [Enter] to close the server.'#10);
          readln;
          writeln('HTTP server shutdown...');
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
    aModel.Free;
  end;
end.
