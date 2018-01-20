program FishFactSyn;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  Ffactwin in 'Ffactwin.pas' {Form1},
  SynRestVCL in '..\SynRestVCL.pas',
  SynRestMidasVCL in '..\SynRestMidasVCL.pas',
  SampleData in '..\SampleData.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
