// Inspired from
// https://github.com/catdad/canvas-confetti

unit Alcinoe.FMX.Confetti;

interface

uses
  System.Math.Vectors,
  System.types,
  System.Classes,
  system.Generics.Collections,
  System.UITypes,
  FMX.types,
  FMX.controls,
  FMX.Graphics,
  Alcinoe.FMX.Ani;

Type

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALConfetti = Class(TFmxObject)
  public
    type
      TParticule = record
      public
        type
          TShape = (square, circle);
      public
        x: Single;
        y: Single;
        wobble: Single;
        wobbleSpeed: Single;
        velocity: Single;
        angle2D: Single;
        tiltAngle: Single;
        color: TalphaColor;
        shape: TShape;
        tick: Single;
        totalTicks: Single;
        decay: Single;
        drift: Single;
        random: Single;
        tiltSin: Single;
        tiltCos: Single;
        wobbleX: Single;
        wobbleY: Single;
        gravity: Single;
        ovalScalar: Single;
        scalar: Single;
      end;
  private
    FOriginalOnPaint: TOnPaintEvent;
    FOnFinish: TNotifyEvent;
    FParticules: TList<TParticule>;
    FTimer: {$IF defined(ANDROID)}TALChoreographerThread{$ELSEIF defined(IOS)}TALDisplayLinkThread{$ELSE}TTimer{$ENDIF};
    function updateParticule(const ACanvas: TCanvas; const ARect: TRectF; var AParticule: TParticule): boolean;
    function randomPhysics(
               const AX: Single;
               const AY: Single;
               const AAngle: Single;
               const ASpread: Single;
               const AStartVelocity: Single;
               const AColor: TalphaColor;
               const AShape: TParticule.Tshape;
               const ATicks: Single;
               const ADecay: Single;
               const AGravity: Single;
               const ADrift: Single;
               const AScalar: Single): TParticule;
    procedure onTimer(Sender: TObject);
    procedure onPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    function getParticuleCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function isRunning: Boolean;
    procedure Fire(
                const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
                const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
                const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
                const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
                const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
                const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
                const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
                const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
                const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
                const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
                const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
                const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
                const AScalar: Single = 1); // Scale factor for each confetti particle. Use decimals to make the confetti smaller. Go on, try teeny tiny confetti, they are adorable!
    Property ParticuleCount: integer read getParticuleCount;
  published
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  End;

procedure Register;

implementation

uses
  system.Math,
  FMX.Forms,
  Alcinoe.Common;

{*************************************************}
constructor TALConfetti.Create(AOwner: TComponent);
begin
  inherited;
  FOriginalOnPaint := nil;
  FOnFinish := nil;
  Fparticules := TList<TParticule>.create;
  {$IF defined(ANDROID)}
  FTimer := TALChoreographerThread.Create(nil);
  {$ELSEIF defined(IOS)}
  FTimer := TALDisplayLinkThread.Create(nil);
  {$ELSE}
  FTimer := TTimer.Create(nil);
  {$ENDIF}
  FTimer.Enabled := False;
  Ftimer.Interval := Trunc(1000 / TALAnimation.AniFrameRate / 10) * 10;
  if (Ftimer.Interval <= 0) then Ftimer.Interval := 1;
  FTimer.OnTimer := OnTimer;
end;

{*****************************}
destructor TALConfetti.Destroy;
begin
  AlFreeAndNil(FParticules);
  AlFreeAndNil(FTimer);
  inherited;
end;

{*********************************************************************************************************************}
function TALConfetti.updateParticule(const ACanvas: TCanvas; const ARect: TRectF; Var AParticule: TParticule): boolean;
begin
  AParticule.x := AParticule.x + (Cos(AParticule.angle2D) * AParticule.velocity + AParticule.drift);
  AParticule.y := AParticule.y + (Sin(AParticule.angle2D) * AParticule.velocity + AParticule.gravity);
  AParticule.wobble := AParticule.wobble + AParticule.wobbleSpeed;
  AParticule.velocity := AParticule.velocity * AParticule.decay;
  AParticule.tiltAngle := AParticule.tiltAngle + 0.1;
  AParticule.tiltSin := Sin(AParticule.tiltAngle);
  AParticule.tiltCos := Cos(AParticule.tiltAngle);
  AParticule.random := random + 2;
  AParticule.wobbleX := AParticule.x + ((10 * AParticule.scalar) * Cos(AParticule.wobble));
  AParticule.wobbleY := AParticule.y + ((10 * AParticule.scalar) * Sin(AParticule.wobble));

  var LProgress := AParticule.tick / AParticule.totalTicks;
  AParticule.tick := AParticule.tick + 1;

  var x1 := AParticule.x + (AParticule.random * AParticule.tiltCos);
  var y1 := AParticule.y + (AParticule.random * AParticule.tiltSin);
  var x2 := AParticule.wobbleX + (AParticule.random * AParticule.tiltCos);
  var y2 := AParticule.wobbleY + (AParticule.random * AParticule.tiltSin);

  var LAlphaColorF := TAlphaColorF.Create(AParticule.color);
  LAlphaColorF.A := (1 - LProgress);
  var LPrevCanvasFillColor := ACanvas.fill.Color;
  try
    ACanvas.fill.Color := LAlphaColorF.ToAlphaColor;
    if AParticule.shape = TParticule.TShape.circle then begin
      var LRadiusX := abs(x2 - x1) * AParticule.ovalScalar;
      var LRadiusY := abs(y2 - y1) * AParticule.ovalScalar;
      Var LDestRectF := TRectF.Create(
                          TpointF.Create(AParticule.x, AParticule.y),
                          LRadiusX * 2,
                          LRadiusY * 2);
      if ARect.IntersectsWith(LDestRectF) then begin
        var LSavedMatrix := aCanvas.Matrix;
        try
          var LMatrixRotationCenter: TpointF;
          LMatrixRotationCenter.X := AParticule.x + LRadiusX + aCanvas.Matrix.m31;
          LMatrixRotationCenter.Y := AParticule.y + LRadiusY + aCanvas.Matrix.m32;
          var aMatrix := aCanvas.Matrix;
          aMatrix := aMatrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
          aMatrix := aMatrix * TMatrix.CreateRotation(PI / 10 * AParticule.wobble);
          aMatrix := aMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
          aCanvas.SetMatrix(aMatrix);
          ACanvas.FillEllipse(
            LDestRectF, // const ARect: TRectF;
            1); // const AOpacity: Single);
        finally
          aCanvas.SetMatrix(LSavedMatrix);
        end;
      end;
    end
    else begin
      var LPolygon: TPolygon;
      setlength(LPolygon, 4);
      LPolygon[0] := TpointF.Create(AParticule.x, AParticule.y);
      LPolygon[1] := TpointF.Create(AParticule.wobbleX, y1);
      LPolygon[2] := TpointF.Create(x2, y2);
      LPolygon[3] := TpointF.Create(x1, AParticule.wobbleY);
      if ARect.IntersectsWith(TrectF.Union(LPolygon)) then
        ACanvas.FillPolygon(
          LPolygon, // const Points: TPolygon;
          1); // const AOpacity: Single);
    end;
  finally
    ACanvas.fill.Color := LPrevCanvasFillColor;
  end;
  result := AParticule.tick < AParticule.totalTicks;
end;

{*********************************}
function TALConfetti.randomPhysics(
           const AX: Single;
           const AY: Single;
           const AAngle: Single;
           const ASpread: Single;
           const AStartVelocity: Single;
           const AColor: TalphaColor;
           const AShape: TParticule.Tshape;
           const ATicks: Single;
           const ADecay: Single;
           const AGravity: Single;
           const ADrift: Single;
           const AScalar: Single): TParticule;
begin
  var LRadAngle := AAngle * (PI / 180);
  var LRadSpread := ASpread * (PI / 180);
  Result.x := Ax;
  Result.y := Ay;
  Result.wobble := Random * 10;
  Result.wobbleSpeed := min(0.11, random * 0.1 + 0.05);
  Result.velocity := (AstartVelocity * 0.5) + (random * AstartVelocity);
  Result.angle2D := -LRadAngle + ((0.5 * LRadSpread) - (random * LRadSpread));
  Result.tiltAngle := (random * (0.75 - 0.25) + 0.25) * PI;
  Result.color := Acolor;
  Result.shape := Ashape;
  Result.tick := 0;
  Result.totalTicks := Aticks;
  Result.decay := Adecay;
  Result.drift := Adrift;
  Result.random := random + 2;
  Result.tiltSin := 0;
  Result.tiltCos := 0;
  Result.wobbleX := 0;
  Result.wobbleY := 0;
  Result.gravity := Agravity * 3;
  Result.ovalScalar := 0.6;
  Result.scalar := Ascalar;
end;

{*********************************************}
procedure TALConfetti.onTimer(Sender: TObject);
begin
  if Parent is TCustomForm then TCustomForm(Parent).Invalidate
  else if Parent is Tcontrol then Tcontrol(Parent).Repaint;
end;

{***********************************************************************************}
procedure TALConfetti.onPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  if assigned(FOriginalOnPaint) then FOriginalOnPaint(Sender, Canvas, ARect);
  for var i := FParticules.Count - 1 downto 0 do begin
    var LParticule := FParticules[i];
    if not updateParticule(Canvas, ARect, LParticule) then FParticules.Delete(i)
    else FParticules[i] := LParticule;
  end;
  if FParticules.Count = 0 then begin
    onTimer(nil);
    Ftimer.Enabled := false;
    if Parent is TCustomForm then TCustomForm(Parent).OnPaint := FOriginalOnPaint
    else if Parent is Tcontrol then Tcontrol(Parent).OnPaint := FOriginalOnPaint;
    FOriginalOnPaint := nil;
    if assigned(fOnfinish) then FonFinish(self);
  end;
end;

{*************************}
procedure TALConfetti.Fire(
            const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
            const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
            const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
            const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
            const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
            const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
            const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
            const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
            const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
            const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
            const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
            const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
            const AScalar: Single = 1); // Scale factor for each confetti particle. Use decimals to make the confetti smaller. Go on, try teeny tiny confetti, they are adorable!
begin

  var LStartX: single;
  var LStartY: single;
  if Parent is TCustomForm then begin
    var LCustomForm := TCustomForm(Parent);
    if not isRunning then begin
      FOriginalOnPaint := LCustomForm.OnPaint;
      LCustomForm.OnPaint := onPaint;
    end;
    LStartX := LCustomForm.width * AOriginX;
    LStartY := LCustomForm.height * AOriginY;
  end
  else if Parent is Tcontrol then begin
    var LControl := Tcontrol(Parent);
    if not isRunning then begin
      FOriginalOnPaint := LControl.OnPaint;
      LControl.OnPaint := onPaint;
    end;
    LStartX := LControl.width * AOriginX;
    LStartY := LControl.height * AOriginY;
  end
  else Exit;

  var LShapes := AShapes;
  if length(LShapes) = 0 then begin
    setlength(LShapes, 2);
    LShapes[0] := TALConfetti.TParticule.TShape.square;
    LShapes[1] := TALConfetti.TParticule.TShape.circle;
  end;
  var LColors := AColors;
  if length(LColors) = 0 then begin
    setlength(LColors, 7);
    LColors[0] := $ff26ccff;
    LColors[1] := $ffa25afd;
    LColors[2] := $ffff5e7e;
    LColors[3] := $ff88ff5a;
    LColors[4] := $fffcff42;
    LColors[5] := $ffffa62d;
    LColors[6] := $ffff36ff;
  end;

  for Var I := 1 to AParticleCount do
    FParticules.Add(
      randomPhysics(
        LStartX, // const AX: Single;
        LStartY, // const AY: Single;
        AAngle, // const AAngle: Single;
        ASpread, // const ASpread: Single;
        AStartVelocity, // const AStartVelocity: Single;
        LColors[i mod length(LColors)], // const AColor: Single;
        LShapes[Random(length(LShapes))], // const AShape: TParticule.Tshape;
        ATicks, // const ATicks: Single;
        ADecay, // const ADecay: Single;
        AGravity, // const AGravity: Single;
        ADrift, // const ADrift: Single;
        AScalar)); // const AScalar: Single)

  FTimer.Enabled := true;

end;

{**********************************************}
function TALConfetti.getParticuleCount: integer;
begin
  result := Fparticules.Count;
end;

{**************************************}
function TALConfetti.isRunning: Boolean;
begin
  result := FTimer.Enabled;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALConfetti]);
end;

initialization
  RegisterFmxClasses([TALConfetti]);

end.
