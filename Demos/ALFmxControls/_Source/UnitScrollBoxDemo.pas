unit UnitScrollBoxDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.Layouts,
  System.Diagnostics, fmx.layouts, Alcinoe.FMX.Objects, FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TScrollBoxDemoForm = class(TForm)
    ALText1: TALText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormCreate(Sender: TObject);
  private
    fDebugFpsStarted: Boolean;
    fDebugFpsCount: integer;
    fDebugFpsStopWatch: TstopWatch;
    fDebugFpsRenderTimeStopWatch: TstopWatch;
    fDebugAverageFpsCount: integer;
    fDebugAverageFps: double;
  public
    fScrollEngine: TALScrollBoxScrollEngine;
    fAniCalculations: TScrollCalculations;
    fvertScrollBox: Tcontrol;
  end;

var
  ScrollBoxDemoForm: TScrollBoxDemoForm;

implementation

uses
  Alcinoe.Common,
  Alcinoe.StringUtils;

{$R *.fmx}

{********************************************************************************}
procedure TScrollBoxDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := TcloseAction.caFree;
end;

{*******************************************************}
procedure TScrollBoxDemoForm.FormCreate(Sender: TObject);
begin
  fDebugFpsStarted := false;
  fDebugFpsCount := 0;
  fDebugFpsStopWatch := TstopWatch.StartNew; // to avoid warning fDebugFpsStopWatch no used in windows
  fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew; // to avoid warning fDebugFpsRenderTimeStopWatch no used in windows
  fDebugAverageFpsCount := 0;
  fDebugAverageFps := 0;
end;

{*************************************}
procedure TScrollBoxDemoForm.FormPaint(
            Sender: TObject;
            Canvas: TCanvas;
            const ARect: TRectF);
begin

  {$IF defined(ANDROID) or defined(IOS)}

  // In order to compute the frame rate, we need
  // to constantly refresh the form
  TThread.ForceQueue(nil,
    procedure
    begin
      invalidate;
    end);

  fDebugFpsRenderTimeStopWatch.stop;
  //--
  //if (fDebugFpsStarted) and
  //   (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds > 18) then begin
  //  ALLog(
  //    'FramePaint',
  //    'Slow frame detected | '  + ALFormatFloatW('0.00', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, ALDefaultFormatSettingsW),
  //    TalLogType.warn);
  //end;
  //--
  if not fDebugFpsStarted then begin
    fDebugFpsStarted := true;
    fDebugFpsCount := 0;
    fDebugFpsStopWatch := TstopWatch.StartNew;
  end
  else begin
    inc(fDebugFpsCount);
    if fDebugFpsCount >= 300 then begin
      fDebugFpsStopWatch.stop;
      fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
      inc(fDebugAverageFpsCount);
      ALLog(
        'FramePaint.fps',
        ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, ALDefaultFormatSettingsW) + ' fps' + ' | ' +
        'average: ' + ALFormatFloatW('0.##', fDebugAverageFps, ALDefaultFormatSettingsW) + ' fps',
        TalLogType.verbose);
      ALText1.Text := ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, ALDefaultFormatSettingsW) + ' fps' +
                      ' (average: ' + ALFormatFloatW('0.##', fDebugAverageFps, ALDefaultFormatSettingsW) + ' fps)';
      fDebugFpsCount := 0;
      fDebugFpsStopWatch := TstopWatch.StartNew;
    end;
  end;
  //--
  fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew;

  {$ENDIF}

end;

end.
