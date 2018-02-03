program Hash;

uses
{$IFDEF WIN32}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
	QForms,
{$ENDIF}
  Unit1 in 'Unit1.pas' {frmHash};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmHash, frmHash);
  Application.Run;
end.
