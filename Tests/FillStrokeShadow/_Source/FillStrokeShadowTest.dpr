program FillStrokeShadowTest;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FillStrokeShadowTestMain in 'FillStrokeShadowTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
