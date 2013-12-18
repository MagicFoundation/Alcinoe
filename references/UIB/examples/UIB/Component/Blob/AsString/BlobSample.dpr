program BlobSample;

uses
{$IFDEF LINUX}QForms,{$ELSE}Forms,{$ENDIF}
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
