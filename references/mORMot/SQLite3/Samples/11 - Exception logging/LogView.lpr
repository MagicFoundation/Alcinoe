program LogView;

{$mode objfpc}{$H+}

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Interfaces, // this includes the LCL widgetset
  Forms,
  LogViewMain in 'LogViewMain.pas' {MainLogView};

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainLogView, MainLogView);
  Application.Run;
end.
