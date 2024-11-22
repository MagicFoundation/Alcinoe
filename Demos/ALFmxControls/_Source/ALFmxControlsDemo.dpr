program ALFmxControlsDemo;

{$R 'Resources.res' 'Resources\Resources.rc'}

{$I Alcinoe.inc}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  {$IF defined(ALAppleOS)}
  FMX.Context.Metal,
  {$ENDIF}
  Main in 'Main.pas' {MainForm},
  ScrollBoxDemo in 'ScrollBoxDemo.pas' {ScrollBoxDemoForm};

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
  {$IF defined(ALAppleOS)}
  if TCustomContextMetal.IsMetalSupported then
    GlobalUseMetal := True;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
