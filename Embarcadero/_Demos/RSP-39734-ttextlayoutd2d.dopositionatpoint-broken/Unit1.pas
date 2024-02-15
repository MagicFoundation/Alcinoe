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
    showmessage('PositionAtPoint(x:'+floattostr(LMeasuredWidth - Tepsilon.Position) + '| y:0): ' + inttostr(LLayout.PositionAtPoint(TpointF.Create(LMeasuredWidth - Tepsilon.Position,0))));
    showmessage('PositionAtPoint must return 1 (zero based)');
    showmessage('The design of PositionAtPoint is not optimal, as it appears to be specifically ' +
      'tailored to work with TEdit and its text selection mechanism. In its current ' +
      'implementation, PositionAtPoint modifies the returned text position by incrementing ' +
      'it by one when the point is at the trailing edge of a character. This approach ' +
      'might be suitable for the specific behavior expected in TEdit, but it could be limiting ' +
      'or inappropriate for other use cases where a different text interaction model is ' +
      'required. This design choice in PositionAtPoint makes it less flexible and potentially ' +
      'less useful in a broader range of applications beyond its intended integration ' +
      'with TEdit');
  finally
    LLayout.Free;
  end;
end;

end.
