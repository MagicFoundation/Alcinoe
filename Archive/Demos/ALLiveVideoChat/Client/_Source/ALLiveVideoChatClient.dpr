program ALLiveVideoChatClient;



uses
  System.StartUpCopy,
  FMX.Forms,
  fmx.Types,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  //GlobalUseMetal := true;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait];
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
