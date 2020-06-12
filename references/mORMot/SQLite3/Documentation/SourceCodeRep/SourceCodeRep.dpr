program SourceCodeRep;

{$ifndef MSWINDOWS}
  {$AppType console}
{$endif}

{$I ../../../Synopse.inc}

uses
  {$I ../../../SynDprUses.inc} // includes FastMM4
  {$ifdef FPC}
  Interfaces, // set appropriate LCL CreateWidgetset()
  {$endif FPC}
  Forms,
  SourceCodeRepMain in 'SourceCodeRepMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
