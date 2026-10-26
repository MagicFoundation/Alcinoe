program ALFmxMapLibreDemo;

{$R 'AlcinoeResources.res' '..\..\..\Source\Resources\AlcinoeResources.rc'}
{$R 'MapLibreDemoResources.res' 'Resources\MapLibreDemoResources.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
