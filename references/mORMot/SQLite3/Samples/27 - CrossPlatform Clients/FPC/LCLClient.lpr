program LCLClient;

{$MODE Delphi}

uses
  Forms, Interfaces,
  LCLMain in 'LCLMain.pas' {Form1};

{.$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
