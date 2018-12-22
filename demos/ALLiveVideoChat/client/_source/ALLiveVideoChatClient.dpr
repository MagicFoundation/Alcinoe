program ALLiveVideoChatClient;



uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
