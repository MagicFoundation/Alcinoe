unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.StdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Layouts, Alcinoe.FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.FMX.FilterEffects, system.messaging,
  Alcinoe.FMX.Objects, FMX.ListBox, FMX.ani, Alcinoe.FMX.Ani, System.Generics.Collections,
  FMX.TabControl, Alcinoe.FMX.Controls;

type
  TForm1 = class(TForm)
    CircleBullet: TALCircle;
    TextAnimationType: TALText;
    ComboBoxAnimationType: TComboBox;
    ButtonStart: TButton;
    ALFloatPropertyAnimation: TALFloatPropertyAnimation;
    LayoutInterpolation: TALLayout;
    LayoutStart: TALLayout;
    LayoutBullet: TALLayout;
    TextInterpolation: TALText;
    ComboBoxInterpolation: TComboBox;
    PaintBoxChart: TPaintBox;
    LayoutGraph: TALLayout;
    ALText0_0: TALText;
    ALText1_0: TALText;
    ALText1_4: TALText;
    ALTextmin1_0: TALText;
    ALTextmin1_4: TALText;
    CheckBoxClearGraph: TCheckBox;
    LayoutAnimationType: TALLayout;
    MainTabControl: TTabControl;
    TabItemInterpolation: TTabItem;
    TabItemSpringForce: TTabItem;
    CircleSpringForce: TALCircle;
    ALSpringForcePropertyAnimationY: TALSpringForcePropertyAnimation;
    ALSpringForcePropertyAnimationX: TALSpringForcePropertyAnimation;
    LayoutDampingRatio: TALLayout;
    TextDampingRatio: TALText;
    ComboBoxDampingRatio: TComboBox;
    LayoutStiffness: TALLayout;
    TextStiffness: TALText;
    ComboBoxStiffness: TComboBox;
    LayoutSpringForce: TALLayout;
    TextDragMe: TALText;
    procedure ButtonStartClick(Sender: TObject);
    procedure ALFloatPropertyAnimationProcess(Sender: TObject);
    procedure ALFloatPropertyAnimationFirstFrame(Sender: TObject);
    procedure PaintBoxChartPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LayoutResized(Sender: TObject);
    function ALFloatPropertyAnimationCustomInterpolation(Sender: TObject): Single;
    procedure CheckBoxClearGraphChange(Sender: TObject);
    procedure LayoutSpringForceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseLeave(Sender: TObject);
  private
    FPoints: Array[TALInterpolationType] of Tlist<Tpointf>;
    FStartTime: int64;
    FIsDragging: Boolean;
    FOffset: TPointF;  // This will store the offset
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  system.Math,
  System.TypInfo,
  Alcinoe.Common;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  for var I := Low(FPoints) to High(FPoints) do
    FPoints[i] := Tlist<Tpointf>.create;
  LayoutResized(nil);
end;

{********************************************}
procedure TForm1.FormDestroy(Sender: TObject);
begin
  for var I := Low(FPoints) to High(FPoints) do
    AlFreeAndNil(FPoints[i]);
end;

{***********************************************************************************}
function TForm1.ALFloatPropertyAnimationCustomInterpolation(Sender: TObject): Single;
begin
  With TALFloatPropertyAnimation(Sender) do
    result := ALInterpolateViscousFluid(CurrentTime / duration);
end;

{*******************************************************************}
procedure TForm1.ALFloatPropertyAnimationFirstFrame(Sender: TObject);
begin
  FPoints[ALFloatPropertyAnimation.Interpolation].Add(
    TpointF.Create(
      ALElapsedTimeMillisAsInt64 - FStartTime,
      ALFloatPropertyAnimation.CurrentValue));
  invalidate;
end;

{****************************************************************}
procedure TForm1.ALFloatPropertyAnimationProcess(Sender: TObject);
begin
  FPoints[ALFloatPropertyAnimation.Interpolation].Add(
    TpointF.Create(
      ALElapsedTimeMillisAsInt64 - FStartTime,
      ALFloatPropertyAnimation.CurrentValue));
  invalidate;
end;

{**********************************************}
procedure TForm1.LayoutResized(Sender: TObject);
begin
  LayoutBullet.Margins.Bottom := LayoutGraph.Height / 2;
  ALText0_0.Position.X := (paintboxChart.Position.X) - ALText0_0.Width - 3;
  ALText1_0.Position.X := (paintboxChart.Position.X) - ALText1_0.Width - 3;
  ALText1_4.Position.X := (paintboxChart.Position.X) - ALText1_4.Width - 3;
  ALTextmin1_0.Position.X := (paintboxChart.Position.X) - ALTextmin1_0.Width - 3;
  ALTextmin1_4.Position.X := (paintboxChart.Position.X) - ALTextmin1_4.Width - 3;
  ALText0_0.Position.Y := (LayoutGraph.Height / 2) - (ALText0_0.Height / 2);
  ALText1_0.Position.Y := ((LayoutGraph.Height / 2) / 100 * (100 - (100/1.40))) - (ALText1_4.Height / 2);
  ALText1_4.Position.Y := (0) - (ALText1_4.Height / 2);
  ALTextmin1_0.Position.Y := (LayoutGraph.Height) - ((LayoutGraph.Height / 2) / 100 * (100 - (100/1.40))) - (ALText1_4.Height / 2);
  ALTextmin1_4.Position.Y := LayoutGraph.Height - (ALText1_4.Height / 2);
  LayoutInterpolation.Height := max(ComboBoxInterpolation.Height, TextInterpolation.Height);
  TextInterpolation.Position.Y := (LayoutInterpolation.Height - TextInterpolation.Height) / 2;
  ComboBoxInterpolation.Position.Y := (LayoutInterpolation.Height - ComboBoxInterpolation.Height) / 2;
  ComboBoxInterpolation.Position.X := textInterpolation.position.X + textInterpolation.Width + 15;
  LayoutAnimationType.Height := max(ComboBoxAnimationType.Height, TextAnimationType.Height);
  TextAnimationType.Position.Y := (LayoutAnimationType.Height - TextAnimationType.Height) / 2;
  ComboBoxAnimationType.Position.Y := (LayoutAnimationType.Height - ComboBoxAnimationType.Height) / 2;
  ComboBoxAnimationType.Position.X := textAnimationType.position.X + textAnimationType.Width + 15;
  LayoutStart.Height := max(ButtonStart.Height, CheckBoxClearGraph.Height);
  ButtonStart.Position.Y := (LayoutStart.Height - ButtonStart.Height) / 2;
  CheckBoxClearGraph.Position.Y := (LayoutStart.Height - CheckBoxClearGraph.Height) / 2;
  CheckBoxClearGraph.Position.X := ButtonStart.position.X + ButtonStart.Width + 25;
  CircleSpringForce.Position.X := (CircleSpringForce.ParentControl.Width / 2) - (CircleSpringForce.width / 2);
  CircleSpringForce.Position.Y := (CircleSpringForce.ParentControl.Height / 2) - (CircleSpringForce.Height / 2);
  LayoutDampingRatio.Height := max(ComboBoxDampingRatio.Height, TextDampingRatio.Height);
  TextDampingRatio.Position.Y := (LayoutDampingRatio.Height - TextDampingRatio.Height) / 2;
  ComboBoxDampingRatio.Position.Y := (LayoutDampingRatio.Height - ComboBoxDampingRatio.Height) / 2;
  ComboBoxDampingRatio.Position.X := textDampingRatio.position.X + textDampingRatio.Width + 15;
  LayoutStiffness.Height := max(ComboBoxStiffness.Height, TextStiffness.Height);
  TextStiffness.Position.Y := (LayoutStiffness.Height - TextStiffness.Height) / 2;
  ComboBoxStiffness.Position.Y := (LayoutStiffness.Height - ComboBoxStiffness.Height) / 2;
  ComboBoxStiffness.Position.X := textStiffness.position.X + textStiffness.Width + 15;
  CircleSpringForce.Width := TextDragMe.Width + 10;
end;

{*************************************************}
procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  FStartTime := ALElapsedTimeMillisAsInt64;
  ALFloatPropertyAnimation.Duration := 3;
  ALFloatPropertyAnimation.StartValue := -CircleBullet.height / 2;
  ALFloatPropertyAnimation.StopValue := (LayoutBullet.height / 1.4) - (CircleBullet.height / 2);
  ALFloatPropertyAnimation.Interpolation := TALInterpolationType(GetEnumValue(TypeInfo(TALInterpolationType), ComboBoxInterpolation.Items[ComboBoxInterpolation.ItemIndex]));
  ALFloatPropertyAnimation.AnimationType := TAnimationType(GetEnumValue(TypeInfo(TAnimationType), ComboBoxAnimationType.Items[ComboBoxAnimationType.ItemIndex]));
  FPoints[ALFloatPropertyAnimation.Interpolation].Clear;
  ALFloatPropertyAnimation.Start;
end;

{*********************************************************}
procedure TForm1.CheckBoxClearGraphChange(Sender: TObject);
begin
  invalidate;
end;

{********************************************************************}
procedure TForm1.PaintBoxChartPaint(Sender: TObject; Canvas: TCanvas);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _drawGraph(const APoints: Tlist<Tpointf>; const AColor: TAlphaColor);
  begin
    if APoints.Count <= 1 then exit;

    var xScale := PaintBoxChart.Width / (ALFloatPropertyAnimation.Duration * 1000);
    var yScale := (PaintBoxChart.Height / 2 / 1.4) / (ALFloatPropertyAnimation.StopValue - ALFloatPropertyAnimation.StartValue);

    Canvas.Stroke.Color := AColor;
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    for var i := 1 to APoints.Count - 1 do
      Canvas.DrawLine(
        TpointF.create(
          APoints[i-1].x * xScale,
          (PaintBoxChart.Height / 2) - ((APoints[i-1].y + (CircleBullet.height / 2)) * yScale)),
        TpointF.create(
          APoints[i].x * xScale,
          (PaintBoxChart.Height / 2) - ((APoints[i].y + (CircleBullet.height / 2)) * yScale)),
        1);
  end;

begin
  Canvas.Fill.Color := TAlphaColors.Lavender;
  Canvas.FillRect(
    RectF(0, 0, PaintBoxChart.Width, PaintBoxChart.Height),
    0, 0, [], 1);
  Canvas.Stroke.Color := TAlphaColors.Lightgrey;
  Canvas.Stroke.Thickness := 1;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.DrawRect(
    RectF(0, 0, PaintBoxChart.Width, PaintBoxChart.Height),
    0, 0, [], 1);

  for var LInterpolationType := Low(FPoints) to High(FPoints) do begin
    Var Lcolor: TAlphaColor;
    case LInterpolationType of
      TALInterpolationType.Linear: Lcolor := TAlphaColorRec.Darkblue;
      TALInterpolationType.Quadratic: Lcolor := TAlphaColorRec.Darkcyan;
      TALInterpolationType.Cubic: Lcolor := TAlphaColorRec.Darkgoldenrod;
      TALInterpolationType.Quartic: Lcolor := TAlphaColorRec.Darkviolet;
      TALInterpolationType.Quintic: Lcolor := TAlphaColorRec.Darkgreen;
      TALInterpolationType.Sinusoidal: Lcolor := TAlphaColorRec.Darkslateblue;
      TALInterpolationType.Exponential: Lcolor := TAlphaColorRec.Brown;
      TALInterpolationType.Circular: Lcolor := TAlphaColorRec.Darkmagenta;
      TALInterpolationType.Elastic: Lcolor := TAlphaColorRec.Darkolivegreen;
      TALInterpolationType.Back: Lcolor := TAlphaColorRec.Green;
      TALInterpolationType.Bounce: Lcolor := TAlphaColorRec.Darkorchid;
      TALInterpolationType.Custom: Lcolor := TAlphaColorRec.Darkred;
      else Lcolor := TAlphaColorRec.Black;
    end;
    if (CheckBoxClearGraph.IsChecked) and
       (LInterpolationType <> ALFloatPropertyAnimation.Interpolation) then continue;
    _drawGraph(FPoints[LInterpolationType], Lcolor);
  end;
end;

{*******************************************************************************************************************}
procedure TForm1.LayoutSpringForceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LPoint := TpointF.create(X, Y);
  if CircleSpringForce.BoundsRect.Contains(TpointF.Create(X, Y)) then begin
    FIsDragging := True;
    ALSpringForcePropertyAnimationX.Enabled := False;
    ALSpringForcePropertyAnimationY.Enabled := False;
    FOffset.X := Lpoint.X - CircleSpringForce.Position.X;
    FOffset.Y := Lpoint.Y - CircleSpringForce.Position.Y;
  end;
end;

{************************************************************}
procedure TForm1.LayoutSpringForceMouseLeave(Sender: TObject);
begin
  LayoutSpringForceMouseUp(self, TMouseButton.mbLeft, [], 0, 0);
end;

{*********************************************************************************************}
procedure TForm1.LayoutSpringForceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if FIsDragging then begin
    CircleSpringForce.Position.X := X - FOffset.X;
    CircleSpringForce.Position.Y := Y - FOffset.Y;
  end;
end;

{*****************************************************************************************************************}
procedure TForm1.LayoutSpringForceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FIsDragging then begin
    FIsDragging := False;

    if ComboBoxStiffness.Items[ComboBoxStiffness.ItemIndex] = 'HIGH' then ALSpringForcePropertyAnimationX.Stiffness := TALSpringForce.STIFFNESS_HIGH
    else if ComboBoxStiffness.Items[ComboBoxStiffness.ItemIndex] = 'MEDIUM' then ALSpringForcePropertyAnimationX.Stiffness := TALSpringForce.STIFFNESS_MEDIUM
    else if ComboBoxStiffness.Items[ComboBoxStiffness.ItemIndex] = 'LOW' then ALSpringForcePropertyAnimationX.Stiffness := TALSpringForce.STIFFNESS_LOW
    else if ComboBoxStiffness.Items[ComboBoxStiffness.ItemIndex] = 'VERY_LOW' then ALSpringForcePropertyAnimationX.Stiffness := TALSpringForce.STIFFNESS_VERY_LOW
    else raise Exception.Create('Error 748614FD-01A9-48D3-86F9-F7C19D773AC3');


    if ComboBoxDampingRatio.Items[ComboBoxDampingRatio.ItemIndex] = 'HIGH_BOUNCY' then ALSpringForcePropertyAnimationX.DampingRatio := TALSpringForce.DAMPING_RATIO_HIGH_BOUNCY
    else if ComboBoxDampingRatio.Items[ComboBoxDampingRatio.ItemIndex] = 'MEDIUM_BOUNCY' then ALSpringForcePropertyAnimationX.DampingRatio := TALSpringForce.DAMPING_RATIO_MEDIUM_BOUNCY
    else if ComboBoxDampingRatio.Items[ComboBoxDampingRatio.ItemIndex] = 'LOW_BOUNCY' then ALSpringForcePropertyAnimationX.DampingRatio := TALSpringForce.DAMPING_RATIO_LOW_BOUNCY
    else if ComboBoxDampingRatio.Items[ComboBoxDampingRatio.ItemIndex] = 'NO_BOUNCY' then ALSpringForcePropertyAnimationX.DampingRatio := TALSpringForce.DAMPING_RATIO_NO_BOUNCY
    else raise Exception.Create('Error 38445E0C-5076-4B3E-AA7A-BD58286774CD');

    ALSpringForcePropertyAnimationX.StartValue := CircleSpringForce.Position.X;
    ALSpringForcePropertyAnimationX.StopValue := (CircleSpringForce.ParentControl.width / 2) - (CircleSpringForce.width / 2);
    ALSpringForcePropertyAnimationX.InitialVelocity := 0;
    ALSpringForcePropertyAnimationX.Start;

    ALSpringForcePropertyAnimationY.Stiffness := ALSpringForcePropertyAnimationX.Stiffness;
    ALSpringForcePropertyAnimationY.DampingRatio := ALSpringForcePropertyAnimationX.DampingRatio;
    ALSpringForcePropertyAnimationY.StartValue := CircleSpringForce.Position.y;
    ALSpringForcePropertyAnimationY.StopValue := (CircleSpringForce.ParentControl.Height / 2) - (CircleSpringForce.Height / 2);
    ALSpringForcePropertyAnimationY.InitialVelocity := 0;
    ALSpringForcePropertyAnimationY.Start;
  end;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
