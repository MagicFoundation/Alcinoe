program TestSQLite3Pages;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
