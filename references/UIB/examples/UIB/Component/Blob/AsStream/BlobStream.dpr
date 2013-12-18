program BlobStream;

uses
{$IFDEF LINUX}QForms,{$ELSE}Forms,{$ENDIF}
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
