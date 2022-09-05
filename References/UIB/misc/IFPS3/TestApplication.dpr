program TestApplication;

uses
  Forms,
  fMain in 'fMain.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Test Application';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
