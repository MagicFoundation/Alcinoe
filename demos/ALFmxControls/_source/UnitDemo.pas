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
    Timer1: TTimer;
    ALText2: TALText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fStopWatch: TstopWatch;
    fCount: integer;
    fStarted: boolean;
  public
    { Public declarations }
    [weak] fALAniCalculations: TALScrollBoxAniCalculations;
    [weak] fAniCalculations: TScrollCalculations;
    [weak] fvertScrollBox: Tcontrol;
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.fmx}

procedure TDemoForm.Button1Click(Sender: TObject);
begin
  Timer1.enabled := not Timer1.enabled;
  if Timer1.enabled then begin
    ALText2.Text := 'Stop calculating the max possible FPS';
    ALText1.Text := 'Calculating';
    fStopWatch := TstopWatch.StartNew;
    fCount := 0;
    fvertScrollBox.Visible := False;
  end
  else begin
    ALText2.Text := 'Calculate the max possible FPS';
    ALText1.Text := 'FPS Analyzing start when velocity > 2000';
    fvertScrollBox.Visible := true;
  end;
end;

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := TcloseAction.caFree;
end;

procedure TDemoForm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin

  if Timer1.enabled then begin
    inc(fCount);
    if (fcount mod 100 = 0) then begin
      fStopWatch.stop;
      if fStopWatch.Elapsed.TotalMilliseconds > 0 then
        ALText1.Text := FormatFloat('0.##', (fCount / fStopWatch.Elapsed.TotalMilliseconds) * 1000) + ' fps';
      fcount := 0;
      fStopWatch := TstopWatch.StartNew;
    end;
  end

  else begin
    if fALAniCalculations <> nil then begin
      if (fALAniCalculations.CurrentVelocity.y > 2000) or
         (fALAniCalculations.CurrentVelocity.y < -2000) then begin
        if not fStarted then begin
          ALText1.Text := 'Analyzing';
          fCount := 0;
          fStopWatch := TstopWatch.StartNew;
        end
        else inc(fcount);
        fStarted := true;
      end
      else if fstarted then begin
        inc(fcount);
        fStopWatch.stop;
        if (fStopWatch.Elapsed.TotalMilliseconds > 0) and (fcount > 10) then
          ALText1.Text := FormatFloat('0.##', (fCount / fStopWatch.Elapsed.TotalMilliseconds) * 1000) + ' fps'
        else
          ALText1.Text := 'Not enough data';
        fstarted := False;
      end;
    end
    else begin
      if (fAniCalculations.CurrentVelocity.y > 2000) or
         (fAniCalculations.CurrentVelocity.y < -2000) then begin
        if not fStarted then begin
          ALText1.Text := 'Analyzing';
          fCount := 0;
          fStopWatch := TstopWatch.StartNew;
        end
        else inc(fcount);
        fStarted := true;
      end
      else if fstarted then begin
        inc(fcount);
        fStopWatch.stop;
        if (fStopWatch.Elapsed.TotalMilliseconds > 0) and (fcount > 10) then
          ALText1.Text := FormatFloat('0.##', (fCount / fStopWatch.Elapsed.TotalMilliseconds) * 1000) + ' fps'
        else
          ALText1.Text := 'Not enough data';
        fstarted := False;
      end;
    end;
  end;

end;

procedure TDemoForm.Timer1Timer(Sender: TObject);
begin
  invalidate;
end;

end.
