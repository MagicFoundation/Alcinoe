/// this client will stress a remote TSQLRestServerDB over HTTP
program Project21HttpClient;

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  ECCProcess in '..\33 - ECC\ECCProcess.pas',
  Project21HttpClientMain in 'Project21HttpClientMain.pas' {MainForm};

{$ifndef FPC}
{$R *.res}
{$R Vista.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
