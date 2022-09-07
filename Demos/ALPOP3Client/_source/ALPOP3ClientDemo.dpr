program ALPOP3ClientDemo;

uses
  Forms,
  ALPOP3ClientDemo_main in 'ALPOP3ClientDemo_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
