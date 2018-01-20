{
   Synopse mORMot framework

   Sample 18 - HTTP Server for ExtJS queries

}

program Project18Server;

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
