unit UnitDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ALFmxLayouts,
  System.Diagnostics, fmx.layouts, ALFmxObjects, FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TDemoForm = class(TForm)
    ALText1: TALText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
  private
    fDebugFpsStarted: Boolean;
    fDebugFpsCount: integer;
    fDebugFpsStopWatch: TstopWatch;
    fDebugFpsRenderTimeStopWatch: TstopWatch;
    fDebugFpsGraph: String;
    fDebugAverageFpsCount: integer;
    fDebugAverageFps: double;
  public
    [weak] fALAniCalculations: TALScrollBoxAniCalculations;
    [weak] fAniCalculations: TScrollCalculations;
    [weak] fvertScrollBox: Tcontrol;
  end;

var
  DemoForm: TDemoForm;

implementation

uses alCommon,
     aLString;

{$R *.fmx}

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := TcloseAction.caFree;
end;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  fDebugFpsStarted := false;
  fDebugFpsCount := 0;
  //fDebugFpsStopWatch
  //fDebugFpsRenderTimeStopWatch
  fDebugAverageFpsCount := 0;
  fDebugAverageFps := 0;
end;

procedure TDemoForm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin

  if fALAniCalculations <> nil then begin
    fDebugFpsRenderTimeStopWatch.stop;
    if (fDebugFpsStarted) and
       (not fALAniCalculations.down) and
       (abs(fALAniCalculations.CurrentVelocity.y) > 500) and
       (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds > 18) and
       (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds < 300) then begin
      ALLog('FramePaint', 'Slow frame detected - '  + ALFormatFloatU('0.00', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) +
                          ' - Velocity: ' + ALFormatFloatU('0', fALAniCalculations.CurrentVelocity.y, alDefaultFormatSettingsU), TalLogType.warn);
    end;
    if (abs(fALAniCalculations.CurrentVelocity.y) > 500) then begin
      if not fDebugFpsStarted then begin
        fDebugFpsGraph := ' |';
        fDebugFpsCount := 0;
        fDebugFpsStopWatch := TstopWatch.StartNew;
        fDebugFpsStarted := true;
      end
      else begin
        fDebugFpsGraph := fDebugFpsGraph + ALFormatFloatU('0', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) + '|';
        inc(fDebugFpsCount);
      end;
    end
    else if fDebugFpsStarted then begin
      fDebugFpsStopWatch.stop;
      fDebugFpsGraph := fDebugFpsGraph + ALFormatFloatU('0', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) + '|';
      inc(fDebugFpsCount);
      if fDebugFpsStopWatch.Elapsed.totalMilliseconds > 0 then begin
        fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
        inc(fDebugAverageFpsCount);
        ALLog('FramePaint.fps', ALFormatFloatU('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, alDefaultFormatSettingsU) + ' fps' +
                                ' - average: ' + ALFormatFloatU('0.##', fDebugAverageFps, alDefaultFormatSettingsU) + ' fps' +
                                fDebugFpsGraph , TalLogType.verbose);
        ALText1.Text := ALFormatFloatU('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, alDefaultFormatSettingsU) + ' fps' +
                        ' (average: ' + ALFormatFloatU('0.##', fDebugAverageFps, alDefaultFormatSettingsU) + ' fps)';
      end;
      fDebugFpsStarted := False;
    end;
    fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew;
  end
  else if fAniCalculations <> nil then begin
    fDebugFpsRenderTimeStopWatch.stop;
    if (fDebugFpsStarted) and
       (not fAniCalculations.down) and
       (abs(fAniCalculations.CurrentVelocity.y) > 500) and
       (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds > 18) and
       (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds < 300) then begin
      ALLog('FramePaint', 'Slow frame detected - '  + ALFormatFloatU('0.00', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) +
                          ' - Velocity: ' + ALFormatFloatU('0', fAniCalculations.CurrentVelocity.y, alDefaultFormatSettingsU), TalLogType.warn);
    end;
    if (abs(fAniCalculations.CurrentVelocity.y) > 500) then begin
      if not fDebugFpsStarted then begin
        fDebugFpsGraph := ' |';
        fDebugFpsCount := 0;
        fDebugFpsStopWatch := TstopWatch.StartNew;
        fDebugFpsStarted := true;
      end
      else begin
        fDebugFpsGraph := fDebugFpsGraph + ALFormatFloatU('0', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) + '|';
        inc(fDebugFpsCount);
      end;
    end
    else if fDebugFpsStarted then begin
      fDebugFpsStopWatch.stop;
      fDebugFpsGraph := fDebugFpsGraph + ALFormatFloatU('0', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, alDefaultFormatSettingsU) + '|';
      inc(fDebugFpsCount);
      if fDebugFpsStopWatch.Elapsed.totalMilliseconds > 0 then begin
        fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
        inc(fDebugAverageFpsCount);
        ALLog('FramePaint.fps', ALFormatFloatU('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, alDefaultFormatSettingsU) + ' fps' +
                                ' - average: ' + ALFormatFloatU('0.##', fDebugAverageFps, alDefaultFormatSettingsU) + ' fps' +
                                fDebugFpsGraph , TalLogType.verbose);
        ALText1.Text := ALFormatFloatU('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, alDefaultFormatSettingsU) + ' fps' +
                        ' (average: ' + ALFormatFloatU('0.##', fDebugAverageFps, alDefaultFormatSettingsU) + ' fps)';
      end;
      fDebugFpsStarted := False;
    end;
    fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew;
  end;

end;

end.
