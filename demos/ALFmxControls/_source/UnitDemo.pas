unit UnitDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ALFmxLayouts,
  System.Diagnostics, fmx.layouts, ALFmxObjects;

type
  TDemoForm = class(TForm)
    ALText1: TALText;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    { Private declarations }
    fStopWatch: TstopWatch;
    fCount: integer;
    fStarted: boolean;
  public
    { Public declarations }
    [weak] fALAniCalculations: TALScrollBoxAniCalculations;
    [weak] fAniCalculations: TScrollCalculations;
  end;

var
  DemoForm: TDemoForm;

implementation

{$R *.fmx}

procedure TDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action := TcloseAction.caFree;
end;

procedure TDemoForm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
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
      ALText1.Text := FormatFloat('#.##', (fCount / fStopWatch.Elapsed.Milliseconds) * 1000) + ' fps';
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
      ALText1.Text := FormatFloat('#.##', (fCount / fStopWatch.Elapsed.Milliseconds) * 1000) + ' fps';
      fstarted := False;
    end;
  end;
end;

end.
