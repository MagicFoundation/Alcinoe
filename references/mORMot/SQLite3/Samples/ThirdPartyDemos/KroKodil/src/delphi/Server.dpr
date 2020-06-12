{
   Synopse mORMot framework

   Sample 04 - HTTP Client-Server
     purpose of this sample is to show how to serve files in
     addition to RESTful Client/Server of a SQLite3 database

   This sample will serve as REST the data as defined in SampleData,
   and serve 'www' sub-folder content within localhost:8080/static

   It is IMHO preferred and less-error prone to define a method-based service,
   then let the method return the file using Ctxt.ReturnFile() method.
   
   See also https://synopse.info/forum/viewtopic.php?id=1896

   Version 1.18
  - added Project04ServerStatic.dpr program

}

program Server;

uses
  FastMM4,
  Forms,
  ServerForm in 'ServerForm.pas' {Form1},
  SampleData in 'SampleData.pas',
  CalcInterface in 'CalcInterface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
