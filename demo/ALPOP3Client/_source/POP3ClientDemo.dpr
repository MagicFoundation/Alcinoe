program POP3ClientDemo;

uses
  Forms,
  POP3ClientDemo_main in 'POP3ClientDemo_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
