program List;

uses
{$IFDEF WIN32}
  Forms,
{$ENDIF}
{$IFDEF LINUX}
	QForms,
{$ENDIF}
  Unit1 in 'Unit1.pas' {frmList},
  MyObjectList in 'MyObjectList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmList, frmList);
  Application.Run;
end.
