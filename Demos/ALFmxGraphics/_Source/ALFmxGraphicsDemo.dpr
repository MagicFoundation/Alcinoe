program ALFmxGraphicsDemo;

{$R 'Resources.res' 'Resources\Resources.rc'}

{$I Alcinoe.inc}

uses
  System.StartUpCopy,
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF}
  {$IF defined(ALAppleOS) and defined(ALUseMetal)}
  FMX.Context.Metal,
  {$ENDIF}
  FMX.Forms,
  FMX.Types,
  Main in 'Main.pas' {MainForm};

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
  {$IF defined(ALAppleOS) and defined(ALUseMetal)}
  if TCustomContextMetal.IsMetalSupported then
    GlobalUseMetal := True
  else
    GlobalUseMetal := False;
  {$ELSE}
  GlobalUseMetal := False;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.