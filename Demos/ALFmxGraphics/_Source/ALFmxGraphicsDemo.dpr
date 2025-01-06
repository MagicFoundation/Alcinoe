program ALFmxGraphicsDemo;



{$R 'Resources.res' 'Resources\Resources.rc'}

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IF defined(SKIA)}
  FMX.Skia,
  {$ENDIF}
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
  {$IF defined(ALUseMetal)}
  GlobalUseMetal := True;
  {$ELSE}
  GlobalUseMetal := False;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
