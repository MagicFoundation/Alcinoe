unit ClipLineDemoMain;

{$I jcl.inc}

interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Graphics, ExtCtrls, Forms, JclGraphUtils,
  JclBase;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    R: TRect;
    P: TPointArray;
    FPenColor: TColor;
    FPenColorLight: TColor;
    procedure InitLines;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  R.Left := 100;
  R.Top := 100;
  R.Right := 300;
  R.Bottom := 300;
  SetLength(P, 50);
  InitLines;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(R);
  Canvas.Pen.Color := FPenColorLight;
  Canvas.PolyLine(P);
  Canvas.Pen.Color := FPenColor;
  DrawPolyLine(Canvas, P, R);
end;

procedure TForm1.InitLines;
var
  i: Integer;
  H, S, L: Single;
begin
  for i := 0 to Length(P)-1 do
  begin
    P[i].X := Random(Width);
    P[i].Y := Random(Height);
  end;
  H := Random;
  S := Random;
  L := 0.4 * Random;

  FPenColor := WinColor(HSLToRGB(H, S, L));
  FPenColorLight := WinColor(HSLToRGB(H, S, 1 - 0.2 * (1 - L)));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  InitLines;
  Refresh;
end;

end.

