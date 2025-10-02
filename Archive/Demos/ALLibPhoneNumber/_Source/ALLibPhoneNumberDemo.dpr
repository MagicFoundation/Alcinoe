program ALLibPhoneNumberDemo;

uses
  Vcl.Forms,
  ALLibPhoneNumberDemo_Main in 'ALLibPhoneNumberDemo_Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
