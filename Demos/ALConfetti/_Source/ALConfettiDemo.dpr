program ALConfettiDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  Main in 'Main.pas' {TMainForm};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TTMainForm, TMainForm);
  Application.Run;
end.
