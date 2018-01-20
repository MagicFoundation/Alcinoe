{
   Synopse mORMot framework

   Sample 01 - In Memory ORM
     purpose of this sample is to show the basic ORM usage of the framework:

   - a TRecord class is defined in Unit1.pas
   - a static server (i.e. in-memory database) is initialized (see
     TSQLRestStorage.Create below);
     it will store the data in a JSON file in the disk and won't require
     the SQLite3 database engine
   - the purpose of the form in Unit1.pas is to add a record to the
     database; the Time field is filled with the current date and time
   - the 'Find a previous message' button show how to perform a basic query
   - on application quit, the Database.Destroy will update the JSON file
   - since the framework use UTF-8 encoding, we use some basic functions for
     fast conversion to/from the User Interface; in real applications,
     you should better use our SQLite3i18n unit and the corresponding
     TLanguageFile.StringToUTF8() and TLanguageFile.UTF8ToString() methods

  Version 1.0 - January 24, 2010
    - Initial Release

  Version 1.1 - April 14, 2011
    - use TSQLRestStorageInMemory instead of abstract TSQLRestStorage

}

program Project01;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  mORMot,
  Unit1 {Form1},
  SampleData in 'SampleData.pas';

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.Caption := ' Sample 01 - In Memory ORM';
  Form1.Database := TSQLRestStorageInMemory.Create(TSQLSampleRecord,nil,
    ChangeFileExt(paramstr(0),'.db'));
  Application.Run;
end.

