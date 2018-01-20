program CSV2ORM;

uses
  MidasLib,
  Vcl.Forms,
  MAinFormU in 'MAinFormU.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
