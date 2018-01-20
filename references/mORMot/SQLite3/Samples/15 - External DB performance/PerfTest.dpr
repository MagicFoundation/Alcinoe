program PerfTest;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  // SynFastWideString, // still works with fast WideString, and slightly faster
  Forms,
  {$ifdef FPC}
  Interfaces,
  {$endif}
  PerfMain in 'PerfMain.pas' {MainForm};

{$ifndef FPC}
{$R *.res}
{$endif}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
