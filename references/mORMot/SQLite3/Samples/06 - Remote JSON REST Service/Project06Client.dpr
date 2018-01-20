program Project06Client;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  Project06ClientMain in 'Project06ClientMain.pas' {Form1};

{$ifndef FPC}
{$R *.res}
{$endif FPC}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
