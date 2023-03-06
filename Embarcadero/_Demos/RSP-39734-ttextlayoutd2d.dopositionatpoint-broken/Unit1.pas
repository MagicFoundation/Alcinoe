unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Text1: TText;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  system.Math.Vectors,
  fmx.TextLayout,
  fmx.Canvas.D2D;


procedure TForm1.Button1Click(Sender: TObject);
var LLayout: TTextLayout;
    LMeasuredWidth: single;
begin
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    LLayout.Font.Family := 'Segoe UI Semilight';
    LLayout.Font.Style := [TFontStyle.fsBold];
    LLayout.Font.Size := 15;
    LLayout.MaxSize := Tpointf.Create(20, 65535);
    LLayout.Trimming := TTextTrimming.Character;
    LLayout.VerticalAlign := TTextAlign.Leading;
    LLayout.HorizontalAlign := TTextAlign.Leading;
    LLayout.WordWrap := false;
    LLayout.Text := '2personnes';
    LLayout.EndUpdate;
    LMeasuredWidth := LLayout.TextWidth;
    showmessage('MeasuredWidth: ' + floattostr(LMeasuredWidth));
    showmessage('PositionAtPoint(MeasuredWidth): ' + inttostr(LLayout.PositionAtPoint(TpointF.Create(LMeasuredWidth - Tepsilon.Position,0))));
    showmessage('As you can see in the TText on the main form it is not possible to write the text "2 personnes" (11 chars) in 1 line');
    showmessage('Please note also that the result is zero based when we are waiting 1 based');
  finally
    LLayout.Free;
  end;
end;

end.
