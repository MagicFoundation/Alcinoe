program Embedded;

uses
{$IFDEF LINUX}
  QForms,
{$ELSE}
  Forms,
{$ENDIF}
  Main in 'Main.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
