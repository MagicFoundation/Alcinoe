program ALFacebookLogin;

uses
  System.StartUpCopy,
  FMX.Forms,
  Alcinoe.FMX.Facebook.Core,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  ALInitFacebookSDKAtStartup := false;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
