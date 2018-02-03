program Algos;

uses
{$IFDEF WIN32}
	Forms,
{$ENDIF}
{$IFDEF LINUX}
  QForms,
{$ENDIF}
  Unit1 in 'Unit1.pas' {frmAlgos};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAlgos, frmAlgos);
  Application.Run;
end.
