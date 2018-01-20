/// MVC sample web application, publishing a simple BLOG
program MVCServerMongoDB;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  SynCrtSock,
  SynCommons,
  SynLog,
  mORMot,
  SynMongoDB,
  mORMotMongoDB,
  mORMotHttpServer,
  mORMotMVC,
  MVCModel,
  MVCViewModel,
  SysUtils;

var aModel: TSQLModel;
    aServer: TSQLRestServer;
    aMongoClient: TMongoClient;
    aApplication: TBlogApplication;
    aHTTPServer: TSQLHttpServer;
begin
  aModel := CreateModel;
  try
    aServer := TSQLRestServer.Create(aModel);
    try
      aServer.LogFamily.Level := LOG_VERBOSE;
      aMongoClient := TMongoClient.Create('localhost');
      try
        StaticMongoDBRegisterAll(aServer,aMongoClient.Open('blog'));
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
        aMongoClient.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
