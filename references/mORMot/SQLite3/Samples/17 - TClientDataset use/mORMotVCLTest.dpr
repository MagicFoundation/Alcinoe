program mORMotVCLTest;

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  // SynFastWideString,    // no real performance impact
  Forms,
  mORMotVCLUnit in 'mORMotVCLUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
