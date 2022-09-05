program client;

uses
  Forms,
  main in 'main.pas' {Form1},
  TestLib_Client in 'TestLib_Client.pas',
  TestLib_Intf in 'TestLib_Intf.pas',
  kbUIBLoader in '..\..\src\kbUIBLoader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
