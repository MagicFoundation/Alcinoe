unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Alcinoe.FMX.StdCtrls,
  FMX.Effects, FMX.Filter.Effects, FMX.Objects, FMX.Layouts, Alcinoe.FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.FMX.FilterEffects, system.messaging,
  Alcinoe.FMX.Objects, FMX.ListBox, FMX.ani, Alcinoe.FMX.Ani, System.Generics.Collections,
  FMX.TabControl, Alcinoe.FMX.Controls, FMX.Edit, FMX.EditBox, FMX.NumberBox;

type
  TForm1 = class(TForm)
    CircleBullet: TALCircle;
    TextInterpolationMode: TALText;
    ComboBoxInterpolationMode: TComboBox;
    ButtonStart: TButton;
    ALFloatPropertyAnimation: TALFloatPropertyAnimation;
    LayoutInterpolationType: TALLayout;
    LayoutStart: TALLayout;
    LayoutBullet: TALLayout;
    TextInterpolationType: TALText;
    ComboBoxInterpolationType: TComboBox;
    PaintBoxChart: TPaintBox;
    LayoutGraph: TALLayout;
    ALText0_0: TALText;
    ALText1_0: TALText;
    ALText1_4: TALText;
    ALTextmin1_0: TALText;
    ALTextmin1_4: TALText;
    LayoutInterpolationMode: TALLayout;
    MainTabControl: TTabControl;
    TabItemInterpolationType: TTabItem;
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
    LayoutCustomInterpolation: TALLayout;
    TextCustomInterpolation: TALText;
    ComboBoxCustomInterpolation: TComboBox;
    ButtonClearGraph: TButton;
    LayoutBezier: TALLayout;
    TextBezier: TALText;
    NumberBoxBezierX1: TNumberBox;
    NumberBoxBezierY1: TNumberBox;
    NumberBoxBezierX2: TNumberBox;
    NumberBoxBezierY2: TNumberBox;
    LayoutOvershoot: TALLayout;
    TextOvershoot: TALText;
    NumberBoxOvershoot: TNumberBox;
    procedure ButtonStartClick(Sender: TObject);
    procedure ALFloatPropertyAnimationProcess(Sender: TObject);
    procedure ALFloatPropertyAnimationFirstFrame(Sender: TObject);
    procedure PaintBoxChartPaint(Sender: TObject; Canvas: TCanvas);
    procedure FormCreate(Sender: TObject);
    procedure LayoutResized(Sender: TObject);
    function ALFloatPropertyAnimationCustomInterpolation(Sender: TObject): Single;
    procedure ButtonClearGraphClick(Sender: TObject);
    procedure LayoutSpringForceMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure LayoutSpringForceMouseLeave(Sender: TObject);
    procedure ComboBoxInterpolationTypeChange(Sender: TObject);
    procedure ButtonStartTestClick(Sender: TObject);
  private
    FPoints: TArray<TPair<TAlphaColor, Tpointf>>;
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
  {$IF defined(Android)}
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  {$ENDIF}
  system.Math,
  System.TypInfo,
  Alcinoe.Common;

{*******************************************}
procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBoxInterpolationType.Items.Clear;
  for var I := Ord(Low(TALInterpolationType)) to Ord(High(TALInterpolationType)) do
    ComboBoxInterpolationType.Items.Add(GetEnumName(TypeInfo(TALInterpolationType), I));
  ComboBoxInterpolationType.ItemIndex := 0;
  LayoutResized(nil);
end;

{***********************************************************************************}
function TForm1.ALFloatPropertyAnimationCustomInterpolation(Sender: TObject): Single;
begin
  case ComboBoxCustomInterpolation.ItemIndex of
    //ViscousFluid
    0: begin
        With TALFloatPropertyAnimation(Sender) do
          result := ALInterpolateViscousFluid(CurrentTime / duration);
       end;
    //cubic-bezier(0.42, 0, 1, 1)
    1: begin
        var LCubicBezier := TALCubicBezier.Create(0.42, 0, 1, 1);
        try
          With TALFloatPropertyAnimation(Sender) do
            result := LCubicBezier.SolveForY(CurrentTime / duration);
        finally
          ALFreeAndNil(LCubicBezier);
        end;
       end;
    //cubic-bezier(0, 0, 0.58, 1)
    2: begin
        var LCubicBezier := TALCubicBezier.Create(0, 0, 0.58, 1);
        try
          With TALFloatPropertyAnimation(Sender) do
            result := LCubicBezier.SolveForY(CurrentTime / duration);
        finally
          ALFreeAndNil(LCubicBezier);
        end;
       end;
    //cubic-bezier(0.42, 0, 0.58, 1)
    3: begin
        var LCubicBezier := TALCubicBezier.Create(0.42, 0, 0.58, 1);
        try
          With TALFloatPropertyAnimation(Sender) do
            result := LCubicBezier.SolveForY(CurrentTime / duration);
        finally
          ALFreeAndNil(LCubicBezier);
        end;
       end;
    else
      Raise Exception.Create('Error 1DC3C7D7-6157-4550-BEB0-C738C11B44E2')
  end;
end;

{*******************************************************************}
procedure TForm1.ALFloatPropertyAnimationFirstFrame(Sender: TObject);
begin

end;

{****************************************************************}
procedure TForm1.ALFloatPropertyAnimationProcess(Sender: TObject);
begin
  var LPair: TPair<TAlphaColor, Tpointf>;
  Lpair.Key := Cardinal(TALFloatPropertyAnimation(Sender).Tag);
  Lpair.Value := TpointF.Create(
                   ALElapsedTimeMillisAsInt64 - FStartTime,
                   ALFloatPropertyAnimation.CurrentValue);
  SetLength(FPoints, length(FPoints) + 1);
  FPoints[high(FPoints)] := Lpair;
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
  LayoutInterpolationType.Height := max(ComboBoxInterpolationType.Height, TextInterpolationType.Height);
  TextInterpolationType.Position.Y := (LayoutInterpolationType.Height - TextInterpolationType.Height) / 2;
  ComboBoxInterpolationType.Position.Y := (LayoutInterpolationType.Height - ComboBoxInterpolationType.Height) / 2;
  ComboBoxInterpolationType.Position.X := textInterpolationType.position.X + textInterpolationType.Width + 15;
  LayoutCustomInterpolation.Height := max(ComboBoxCustomInterpolation.Height, TextCustomInterpolation.Height);
  TextCustomInterpolation.Position.Y := (LayoutCustomInterpolation.Height - TextCustomInterpolation.Height) / 2;
  ComboBoxCustomInterpolation.Position.Y := (LayoutCustomInterpolation.Height - ComboBoxCustomInterpolation.Height) / 2;
  ComboBoxCustomInterpolation.Position.X := textCustomInterpolation.position.X + textCustomInterpolation.Width + 15;
  LayoutInterpolationMode.Height := max(ComboBoxInterpolationMode.Height, TextInterpolationMode.Height);
  TextInterpolationMode.Position.Y := (LayoutInterpolationMode.Height - TextInterpolationMode.Height) / 2;
  ComboBoxInterpolationMode.Position.Y := (LayoutInterpolationMode.Height - ComboBoxInterpolationMode.Height) / 2;
  LayoutBezier.Height := max(NumberBoxBezierX1.Height, TextBezier.Height);
  TextBezier.Position.Y := (LayoutBezier.Height - TextBezier.Height) / 2;
  NumberBoxBezierX1.Position.Y := (LayoutBezier.Height - NumberBoxBezierX1.Height) / 2;
  NumberBoxBezierY1.Position.Y := (LayoutBezier.Height - NumberBoxBezierX1.Height) / 2;
  NumberBoxBezierX2.Position.Y := (LayoutBezier.Height - NumberBoxBezierX1.Height) / 2;
  NumberBoxBezierY2.Position.Y := (LayoutBezier.Height - NumberBoxBezierX1.Height) / 2;
  LayoutOvershoot.Height := max(NumberBoxOvershoot.Height, TextOvershoot.Height);
  TextOvershoot.Position.Y := (LayoutOvershoot.Height - TextOvershoot.Height) / 2;
  NumberBoxOvershoot.Position.Y := (LayoutOvershoot.Height - NumberBoxOvershoot.Height) / 2;
  ComboBoxInterpolationMode.Position.X := textInterpolationMode.position.X + textInterpolationMode.Width + 15;
  LayoutStart.Height := max(ButtonStart.Height, ButtonClearGraph.Height);
  ButtonStart.Position.Y := (LayoutStart.Height - ButtonStart.Height) / 2;
  ButtonClearGraph.Position.Y := (LayoutStart.Height - ButtonClearGraph.Height) / 2;
  ButtonClearGraph.Position.X := ButtonStart.position.X + ButtonStart.Width + 25;
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
  ALFloatPropertyAnimation.Duration := 2;
  ALFloatPropertyAnimation.StartValue := -CircleBullet.height / 2;
  ALFloatPropertyAnimation.StopValue := (LayoutBullet.height / 1.4) - (CircleBullet.height / 2);
  ALFloatPropertyAnimation.InterpolationType := TALInterpolationType(GetEnumValue(TypeInfo(TALInterpolationType), ComboBoxInterpolationType.Items[ComboBoxInterpolationType.ItemIndex]));
  ALFloatPropertyAnimation.InterpolationMode := TALInterpolationMode(GetEnumValue(TypeInfo(TALInterpolationMode), ComboBoxInterpolationMode.Items[ComboBoxInterpolationMode.ItemIndex]));
  ALFloatPropertyAnimation.InterpolationParams.BezierX1 := NumberBoxBezierX1.Value;
  ALFloatPropertyAnimation.InterpolationParams.BezierY1 := NumberBoxBezierY1.Value;
  ALFloatPropertyAnimation.InterpolationParams.BezierX2 := NumberBoxBezierX2.Value;
  ALFloatPropertyAnimation.InterpolationParams.BezierY2 := NumberBoxBezierY2.Value;
  ALFloatPropertyAnimation.InterpolationParams.Overshoot := NumberBoxOvershoot.Value;
  var LColor: TalphaColor;
  case 1 + random(60) of
    001: LColor := TalphaColors.Black; // TAlphaColor(Alpha or $000000);
    002: LColor := TalphaColors.Blue; // TAlphaColor(Alpha or $0000FF);
    003: LColor := TalphaColors.Blueviolet; // TAlphaColor(Alpha or $8A2BE2);
    004: LColor := TalphaColors.Brown; // TAlphaColor(Alpha or $A52A2A);
    005: LColor := TalphaColors.Darkblue; // TAlphaColor(Alpha or $00008B);
    006: LColor := TalphaColors.Darkcyan; // TAlphaColor(Alpha or $008B8B);
    007: LColor := TalphaColors.Darkgoldenrod; // TAlphaColor(Alpha or $B8860B);
    008: LColor := TalphaColors.Darkgreen; // TAlphaColor(Alpha or $006400);
    009: LColor := TalphaColors.Darkmagenta; // TAlphaColor(Alpha or $8B008B);
    010: LColor := TalphaColors.Darkolivegreen; // TAlphaColor(Alpha or $556B2F);
    011: LColor := TalphaColors.Darkorange; // TAlphaColor(Alpha or $FF8C00);
    012: LColor := TalphaColors.Darkorchid; // TAlphaColor(Alpha or $9932CC);
    013: LColor := TalphaColors.Darkred; // TAlphaColor(Alpha or $8B0000);
    014: LColor := TalphaColors.Darkslateblue; // TAlphaColor(Alpha or $483D8B);
    015: LColor := TalphaColors.Darkslategray; // TAlphaColor(Alpha or $2F4F4F);
    016: LColor := TalphaColors.Darkslategrey; // TAlphaColor(Alpha or $2F4F4F);
    017: LColor := TalphaColors.Darkviolet; // TAlphaColor(Alpha or $9400D3);
    018: LColor := TalphaColors.Deeppink; // TAlphaColor(Alpha or $FF1493);
    019: LColor := TalphaColors.Firebrick; // TAlphaColor(Alpha or $B22222);
    020: LColor := TalphaColors.Forestgreen; // TAlphaColor(Alpha or $228B22);
    021: LColor := TalphaColors.Fuchsia; // TAlphaColor(Alpha or $FF00FF);
    022: LColor := TalphaColors.Green; // TAlphaColor(Alpha or $008000);
    023: LColor := TalphaColors.Indigo; // TAlphaColor(Alpha or $4B0082);
    024: LColor := TalphaColors.Magenta; // TAlphaColor(Alpha or $FF00FF);
    025: LColor := TalphaColors.Maroon; // TAlphaColor(Alpha or $800000);
    026: LColor := TalphaColors.Mediumblue; // TAlphaColor(Alpha or $0000CD);
    027: LColor := TalphaColors.Mediumvioletred; // TAlphaColor(Alpha or $C71585);
    028: LColor := TalphaColors.Midnightblue; // TAlphaColor(Alpha or $191970);
    029: LColor := TalphaColors.Navy; // TAlphaColor(Alpha or $000080);
    030: LColor := TalphaColors.Olive; // TAlphaColor(Alpha or $808000);
    031: LColor := TalphaColors.Olivedrab; // TAlphaColor(Alpha or $6B8E23);
    032: LColor := TalphaColors.Orangered; // TAlphaColor(Alpha or $FF4500);
    033: LColor := TalphaColors.Purple; // TAlphaColor(Alpha or $800080);
    034: LColor := TalphaColors.Red; // TAlphaColor(Alpha or $FF0000);
    035: LColor := TalphaColors.Royalblue; // TAlphaColor(Alpha or $4169E1);
    036: LColor := TalphaColors.Saddlebrown; // TAlphaColor(Alpha or $8B4513);
    037: LColor := TalphaColors.Sienna; // TAlphaColor(Alpha or $A0522D);
    038: LColor := TalphaColors.Slateblue; // TAlphaColor(Alpha or $6A5ACD);
    039: LColor := TalphaColors.Slategray; // TAlphaColor(Alpha or $708090);
    040: LColor := TalphaColors.Slategrey; // TAlphaColor(Alpha or $708090);
    041: LColor := TalphaColors.Steelblue; // TAlphaColor(Alpha or $4682B4);
    042: LColor := TalphaColors.Teal; // TAlphaColor(Alpha or $008080);
    043: LColor := TalphaColors.Tomato; // TAlphaColor(Alpha or $FF6347);
    044: LColor := TalphaColors.Crimson; // TAlphaColor(Alpha or $DC143C);
    045: LColor := TalphaColors.Darkturquoise; // TAlphaColor(Alpha or $00CED1);
    046: LColor := TalphaColors.Deepskyblue; // TAlphaColor(Alpha or $00BFFF);
    047: LColor := TalphaColors.Dodgerblue; // TAlphaColor(Alpha or $1E90FF);
    048: LColor := TalphaColors.Mediumorchid; // TAlphaColor(Alpha or $BA55D3);
    049: LColor := TalphaColors.Mediumpurple; // TAlphaColor(Alpha or $9370DB);
    050: LColor := TalphaColors.Mediumseagreen; // TAlphaColor(Alpha or $3CB371);
    051: LColor := TalphaColors.Mediumslateblue; // TAlphaColor(Alpha or $7B68EE);
    052: LColor := TalphaColors.Mediumturquoise; // TAlphaColor(Alpha or $48D1CC);
    053: LColor := TalphaColors.Chocolate; // TAlphaColor(Alpha or $D2691E);
    054: LColor := TalphaColors.Coral; // TAlphaColor(Alpha or $FF7F50);
    055: LColor := TalphaColors.Darkkhaki; // TAlphaColor(Alpha or $BDB76B);
    056: LColor := TalphaColors.Darksalmon; // TAlphaColor(Alpha or $E9967A);
    057: LColor := TalphaColors.Hotpink; // TAlphaColor(Alpha or $FF69B4);
    058: LColor := TalphaColors.Indianred; // TAlphaColor(Alpha or $CD5C5C);
    059: LColor := TalphaColors.Orchid; // TAlphaColor(Alpha or $DA70D6);
    060: LColor := TalphaColors.Palevioletred; // TAlphaColor(Alpha or $DB7093);
    else
      LColor := TalphaColors.Black;
  end;
  ALFloatPropertyAnimation.Tag := LColor;
  ALFloatPropertyAnimation.Start;
end;

{******************************************************}
procedure TForm1.ButtonClearGraphClick(Sender: TObject);
begin
  Setlength(FPoints, 0);
  invalidate;
end;

{****************************************************************}
procedure TForm1.ComboBoxInterpolationTypeChange(Sender: TObject);
begin
  if ComboBoxInterpolationType.ItemIndex >= 0 then
    ComboBoxCustomInterpolation.Enabled := TALInterpolationType(GetEnumValue(TypeInfo(TALInterpolationType), ComboBoxInterpolationType.Items[ComboBoxInterpolationType.ItemIndex])) = TALInterpolationType.Custom;
end;

{********************************************************************}
procedure TForm1.PaintBoxChartPaint(Sender: TObject; Canvas: TCanvas);
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

  var xScale := PaintBoxChart.Width / (ALFloatPropertyAnimation.Duration * 1000);
  var yScale := (PaintBoxChart.Height / 2 / 1.4) / (ALFloatPropertyAnimation.StopValue - ALFloatPropertyAnimation.StartValue);
  for var I := Low(FPoints) to High(FPoints) do begin
    if (I = 0) or (FPoints[i-1].Key <> FPoints[i].Key) then begin
      Canvas.fill.Color := FPoints[i].Key;
      Canvas.fill.Kind := TBrushKind.Solid;
      Canvas.fillRect(
        TrectF.Create(
          TpointF.create(
            FPoints[i].Value.x * xScale,
            (PaintBoxChart.Height / 2) - ((FPoints[i].Value.y + (CircleBullet.height / 2)) * yScale)),
          1, 1),
        1);
    end
    else begin
      Canvas.Stroke.Color := FPoints[i].Key;
      Canvas.Stroke.Thickness := 1;
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.DrawLine(
        TpointF.create(
          FPoints[i-1].Value.x * xScale,
          (PaintBoxChart.Height / 2) - ((FPoints[i-1].Value.y + (CircleBullet.height / 2)) * yScale)),
        TpointF.create(
          FPoints[i].Value.x * xScale,
          (PaintBoxChart.Height / 2) - ((FPoints[i].Value.y + (CircleBullet.height / 2)) * yScale)),
        1);
    end;
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


////////////////////////
/// For Testing only ///
////////////////////////

{$IF defined(android)}
type
  [JavaSignature('android/view/animation/PathInterpolator')]
  JPathInterpolator = interface(JObject)
    ['{E4F2C0D1-8A3B-4F3E-9C56-5A1D9F8B7C02}']
    function getInterpolation(input: Single): Single; cdecl;
  end;
  JPathInterpolatorClass = interface(JObjectClass)
    ['{B7A1F631-09D3-4C5D-87F4-23E4C1A9E1AB}']
    { constructor overloads }
    function init(path: JPath): JPathInterpolator; cdecl; overload;
    function init(controlX1, controlY1, controlX2, controlY2: Single): JPathInterpolator; cdecl; overload;
  end;
  TJPathInterpolator = class(TJavaGenericImport<JPathInterpolatorClass, JPathInterpolator>) end;
{$ENDIF}

{*****************************************************}
procedure TForm1.ButtonStartTestClick(Sender: TObject);
begin
  Setlength(FPoints, 0);
  ALFloatPropertyAnimation.Duration := 2;
  ALFloatPropertyAnimation.StartValue := -CircleBullet.height / 2;
  ALFloatPropertyAnimation.StopValue := (LayoutBullet.height / 1.4) - (CircleBullet.height / 2);

  {$IF defined(android)}
  var Path := TJPath.JavaClass.init;
  Path.moveTo(0, 0);
  Path.cubicTo(0.05, 0, 0.133333, 0.06, 0.166666, 0.4);
  Path.cubicTo(0.208333, 0.82, 0.25, 1, 1, 1);
  var Interpolator := TJPathInterpolator.JavaClass.init(Path);
  {$ENDIF}

  const StepCount = 100;
  for var I: Integer := 0 to StepCount do
  begin
    var t: Single := I / StepCount;              // 0.00, 0.01, …, 1.00
    var y1: Single := ALInterpolateMaterialEmphasized(t, 1);
    var LPair: TPair<TAlphaColor, Tpointf>;
    Lpair.Key := TalphaColors.darkred;
    Lpair.Value := TpointF.Create(
                     t * 1000 * 3,
                     ALFloatPropertyAnimation.StartValue + (y1 * (ALFloatPropertyAnimation.StopValue - ALFloatPropertyAnimation.StartValue)));
    SetLength(FPoints, length(FPoints) + 1);
    FPoints[high(FPoints)] := Lpair;
    {$IF defined(android)}
    var y2: Single := Interpolator.getInterpolation(t);
    Lpair.Key := TalphaColors.DarkGreen;
    Lpair.Value := TpointF.Create(
                     t * 1000 * 3,
                     ALFloatPropertyAnimation.StartValue + (y2 * (ALFloatPropertyAnimation.StopValue - ALFloatPropertyAnimation.StartValue)));
    SetLength(FPoints, length(FPoints) + 1);
    FPoints[high(FPoints)] := Lpair;
    {$ENDIF}

  end;
  Invalidate;
end;

initialization
  {$IFDEF DEBUG}
  ReportMemoryleaksOnSHutdown := True;
  {$ENDIF}
  SetMultiByteConversionCodePage(CP_UTF8);

end.
