program CloneDB;

uses
{$IFDEF USE_FASTMM4}
  FastMM4,
{$ENDIF}
  Forms,
  exceptionform in 'exceptionform.pas' {ExceptionDialog},
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clony&Pumpy - The Famous Firebird Database Tool';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
