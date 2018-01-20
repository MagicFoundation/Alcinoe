{
   Synopse mORMot framework

   Sample 19 - HTTP Server for FishFacts ExtJS queries

   Version 1.18
   - download and unzip the FishFacts SQLite3 database if not available
   - added button to open the Browser on the AJAX application page  
}

program Project19Server;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  Unit2 in 'Unit2.pas' {Form1};

{$ifndef FPC}
{$R *.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
