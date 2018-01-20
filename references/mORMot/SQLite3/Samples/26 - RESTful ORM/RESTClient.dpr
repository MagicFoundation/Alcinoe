program RESTClient;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  RestClientMain in 'RestClientMain.pas' {MainForm};

{$ifndef FPC}
{$R *.res}
{$endif FPC}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
