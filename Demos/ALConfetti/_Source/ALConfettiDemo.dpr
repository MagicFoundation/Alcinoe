program ALConfettiDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {TMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTMainForm, TMainForm);
  Application.Run;
end.
