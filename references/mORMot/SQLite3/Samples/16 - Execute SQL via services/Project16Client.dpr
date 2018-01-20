program Project16Client;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  Project16ClientMain in 'Project16ClientMain.pas' {MainForm},
  Project16Interface in 'Project16Interface.pas';

{$ifndef FPC}
{$R *.res}
{$endif}


begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
