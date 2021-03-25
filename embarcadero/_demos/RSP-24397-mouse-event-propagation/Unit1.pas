unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type

  TMyRectangle = Class(TRectangle)
  protected
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
  end;

  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Memo3: TMemo;
    Memo4: TMemo;
    Text1: TText;
    Text2: TText;
    Rectangle3: TRectangle;
    Text3: TText;
    Text4: TText;
    Text5: TText;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

var ChildrenMouseMoveCount: integer;
    CMGestureCount: integer;


procedure TForm1.FormCreate(Sender: TObject);
begin
ChildrenMouseMoveCount := 0;
CMGestureCount := 0;

  var LParentRectangle := TMyRectangle.Create(self);
  LParentRectangle.Parent := Self;
  LParentRectangle.Align := TalignLayout.Client;
  LParentRectangle.Touch.DefaultInteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Pan];
  LParentRectangle.Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.Pan];


  var LChildRectangle := Trectangle.Create(LParentRectangle);
  LChildRectangle.Parent := LParentRectangle;
  LChildRectangle.Align := TalignLayout.Client;
  LChildRectangle.Fill.Kind := TbrushKind.Solid;
  LChildRectangle.Fill.Color := TAlphaColorrec.yellow;

end;

{ TMyRectangle }

procedure TMyRectangle.ChildrenMouseMove(const AObject: TControl;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  inc(ChildrenMouseMoveCount);
  Form1.Memo4.Lines.Add(formatFloat('00.000',x) + 'x' + formatFloat('00.000',y));
  Form1.Text3.text := Inttostr(ChildrenMouseMoveCount);
end;

procedure TMyRectangle.CMGesture(var EventInfo: TGestureEventInfo);
begin
  inherited;
  inc(CMGestureCount);
  Form1.Memo3.Lines.Add(formatFloat('00.000',EventInfo.Location.X) + 'x' + formatFloat('00.000',EventInfo.Location.y));
  Form1.Text4.text := Inttostr(CMGestureCount);
end;

end.
