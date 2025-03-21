program ALFmxDynamicListBoxDemo;

{$R 'Resources.res' 'Resources\Resources.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  Main in 'Main.pas' {MainForm};

{$R *.res}

begin
  {$IF defined(SKIA)}
  GlobalUseSkia := True;
  GlobalUseVulkan := False;
  {$ELSE}
  GlobalUseSkia := FALSE;
  GlobalUseVulkan := False;
  {$ENDIF}
  //GlobalUseGPUCanvas := True;
  //GlobalUseMetal := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
