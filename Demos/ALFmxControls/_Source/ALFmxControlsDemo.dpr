program ALFmxControlsDemo;

{$R 'Resources.res' 'Resources\Resources.rc'}

{$I Alcinoe.inc}

uses
  System.StartUpCopy,
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF}
  {$IF defined(ALAppleOS)}
  FMX.Context.Metal,
  {$ENDIF}
  FMX.Forms,
  FMX.Types,
  Main in 'Main.pas' {MainForm},
  ScrollBoxDemo in 'ScrollBoxDemo.pas' {ScrollBoxDemoForm};

{$R *.res}

begin
  {$IF defined(SKIA)}
  GlobalUseSkia := True;
  {$IF defined(ALUseVulkan)}
  GlobalUseVulkan := True;
  {$ELSE}
  GlobalUseVulkan := False;
  {$ENDIF}
  {$ENDIF}
  {$IF defined(ALUseMetal)}
  if TCustomContextMetal.IsMetalSupported then
    GlobalUseMetal := True
  else
    GlobalUseMetal := False;
  {$ELSE}
  GlobalUseMetal := False;
  {$ENDIF}
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
