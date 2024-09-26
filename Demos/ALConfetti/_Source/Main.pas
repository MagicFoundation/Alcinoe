unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, Alcinoe.FMX.Confetti, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, system.Diagnostics;

type
  TTMainForm = class(TForm)
    Confetti: TALConfetti;
    BtnFireSchoolPride: TButton;
    BtnFireSnow: TButton;
    BtnFireFireworks: TButton;
    BtnFireRealisticLook: TButton;
    FireworksTimer: TTimer;
    SnowTimer: TTimer;
    SchoolPrideTimer: TTimer;
    BtnRandomDirection: TButton;
    Label1: TLabel;
    StatusBar: TStatusBar;
    StatusBarLabel: TLabel;
    procedure BtnFireRealisticLookClick(Sender: TObject);
    procedure BtnFireFireworksClick(Sender: TObject);
    procedure FireworksTimerTimer(Sender: TObject);
    procedure BtnFireSnowClick(Sender: TObject);
    procedure SnowTimerTimer(Sender: TObject);
    procedure BtnFireSchoolPrideClick(Sender: TObject);
    procedure SchoolPrideTimerTimer(Sender: TObject);
    procedure BtnRandomDirectionClick(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ConfettiFinish(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fSkew: single;
    fDebugFpsStarted: Boolean;
    fDebugFpsCount: integer;
    fDebugFpsStopWatch: TstopWatch;
    procedure UpdateStatusBar;
  public
    { Public declarations }
  end;

var
  TMainForm: TTMainForm;

implementation

uses
  system.Math,
  Alcinoe.Common,
  Alcinoe.StringUtils;

{$R *.fmx}

{************************************************}
function _randomInRange(min, max: single): Single;
begin
  result := random * (max - min) + min;
end;

{**********************************************************}
procedure TTMainForm.BtnFireFireworksClick(Sender: TObject);
begin
  if FireworksTimer.Enabled then exit;
  BtnFireFireworks.Enabled := False;
  FireworksTimer.Tag := 0;
  FireworksTimer.Enabled := True;
end;

{********************************************************}
procedure TTMainForm.FireworksTimerTimer(Sender: TObject);
begin
  FireworksTimer.tag := FireworksTimer.tag + NativeInt(FireworksTimer.Interval);
  var LTimeLeft := (15 * 1000) - FireworksTimer.tag;
  if LTimeLeft <= 0 then begin
    FireworksTimer.Enabled := False;
    BtnFireFireworks.Enabled := True;
  end;
  var LParticleCount := round(50 * (LTimeLeft / (15 * 1000)));

  Confetti.Fire(
    _randomInRange(0.1, 0.3), // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    random - 0.2, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    LParticleCount, // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    360, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    30, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    60, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

  Confetti.Fire(
    _randomInRange(0.7, 0.9), // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    random - 0.2, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    LParticleCount, // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    360, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    30, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    60, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

end;

{***********************************************}
procedure TTMainForm.FormCreate(Sender: TObject);
begin
  fDebugFpsStarted := False;
end;

{************************************************************************************}
procedure TTMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  UpdateStatusBar;
end;

{**************************************************************}
procedure TTMainForm.BtnFireRealisticLookClick(Sender: TObject);
begin
  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.7, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    Round(0.25*200), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    26, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    55, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.7, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    Round(0.2*200), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    60, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.7, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    Round(0.35*200), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    100, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.91, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    0.8); // const AScalar: Single = 1)

  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.7, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    Round(0.1*200), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    120, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    25, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.92, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1.2); // const AScalar: Single = 1)

  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.7, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    Round(0.1*200), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    120, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

end;

{************************************************************}
procedure TTMainForm.BtnFireSchoolPrideClick(Sender: TObject);
begin
  if SchoolPrideTimer.Enabled then exit;
  BtnFireSchoolPride.Enabled := False;
  SchoolPrideTimer.Tag := 0;
  SchoolPrideTimer.Enabled := True;
end;

{**********************************************************}
procedure TTMainForm.SchoolPrideTimerTimer(Sender: TObject);
begin

  SchoolPrideTimer.tag := SchoolPrideTimer.tag + NativeInt(SchoolPrideTimer.Interval);
  var LTimeLeft := (15 * 1000) - SchoolPrideTimer.tag;
  Confetti.Fire(
    0, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.5, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    10, // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    60, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    55, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [$ffbb0000, $ffffffff], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

  Confetti.Fire(
    1, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.5, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    10, // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    120, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    55, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [$ffbb0000, $ffffffff], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)

  if LTimeLeft <= 0 then begin
    SchoolPrideTimer.Enabled := False;
    BtnFireSchoolPride.Enabled := True;
  end;

end;

{*****************************************************}
procedure TTMainForm.BtnFireSnowClick(Sender: TObject);
begin
  if SnowTimer.Enabled then exit;
  BtnFireSnow.Enabled := False;
  fSkew := 1;
  SnowTimer.Tag := 0;
  SnowTimer.Enabled := True;
end;

{************************************************************}
procedure TTMainForm.BtnRandomDirectionClick(Sender: TObject);
begin
  Confetti.Fire(
    0.5, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    0.6, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    round(_randomInRange(50, 100)), // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    _randomInRange(55, 125), // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    _randomInRange(50, 70), // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    45, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    200, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    1, // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    0, // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    1); // const AScalar: Single = 1)
end;

{***************************************************}
procedure TTMainForm.ConfettiFinish(Sender: TObject);
begin
  UpdateStatusBar;
  TThread.ForceQueue(nil,
    procedure
    begin
      invalidate;
    end);
end;

{***************************************************}
procedure TTMainForm.SnowTimerTimer(Sender: TObject);
begin

  SnowTimer.tag := SnowTimer.tag + NativeInt(SnowTimer.Interval);
  var LTimeLeft := (15 * 1000) - SnowTimer.tag;
  var Lticks := max(200, 500 * (LtimeLeft / (15 * 1000)));
  Fskew := max(0.8, Fskew - 0.001);

  Confetti.Fire(
    random, // const AOriginX: Single = 0.5; // The x position on the page where to start firing confetti from, with 0 being the left edge and 1 being the right edge.
    (random * FSkew) - 0.2, // const AOriginY: Single = 0.5; // The y position on the page where to start firing confetti from, with 0 being the top edge and 1 being the bottom edge.
    1, // const AParticleCount: Integer = 50; // The number of confetti to launch. More is always fun... but be cool, there's a lot of math involved.
    90, // const AAngle: Single = 90; // The angle in which to launch the confetti, in degrees. 90 is straight up.
    45, // const ASpread: Single = 45; // How far off center the confetti can go, in degrees. 45 means the confetti will launch at the defined angle plus or minus 22.5 degrees.
    0, // const AStartVelocity: Single = 45; // How fast the confetti will start going, in pixels.
    [$ffffffff], // const AColors: TArray<TalphaColor> = []; // An array of color strings, in the HEX format... you know, like #bada55.
    [TALConfetti.TParticule.TShape.circle], // const AShapes: TArray<TParticule.TShape> = []; // An array of shapes for the confetti. The possible values are square and circle. The default is to use both shapes in an even mix. You can even change the mix by providing a value such as ['circle', 'circle', 'square'] to use two third circles and one third squares.
    Lticks, // const ATicks: Single = 200; // How many times the confetti will move. This is abstract... but play with it if the confetti disappear too quickly for you.
    0.9, // const ADecay: Single = 0.9; // How quickly the confetti will lose speed. Keep this number between 0 and 1, otherwise the confetti will gain speed. Better yet, just never change it.
    _randomInRange(0.4, 0.6), // const AGravity: Single = 1; // How quickly the particles are pulled down. 1 is full gravity, 0.5 is half gravity, etc., but there are no limits. You can even make particles go up if you'd like.
    _randomInRange(-0.4, 0.4), // const ADrift: Single = 0; // How much to the side the confetti will drift. The default is 0, meaning that they will fall straight down. Use a negative number for left and positive number for right.
    _randomInRange(0.4, 1)); // const AScalar: Single = 1)

  if LTimeLeft <= 0 then begin
    SnowTimer.Enabled := False;
    BtnFireSnow.Enabled := True;
  end;

end;

{***********************************}
procedure TTMainForm.UpdateStatusBar;
begin
  var LAverageFPS: Double := 0;
  if Confetti.isRunning then begin
    if not fDebugFpsStarted then begin
      fDebugFpsCount := 0;
      fDebugFpsStopWatch := TstopWatch.StartNew;
      fDebugFpsStarted := true;
    end
    else begin
      inc(fDebugFpsCount);
      fDebugFpsStopWatch.stop;
      LAverageFPS := (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000;
      fDebugFpsStopWatch.start;
    end;
  end
  else if fDebugFpsStarted then begin
    fDebugFpsStopWatch.stop;
    inc(fDebugFpsCount);
    LAverageFPS := (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000;
    fDebugFpsStarted := False;
  end;
  if (not Confetti.isRunning) or ((fDebugFpsCount mod 40) = 0)  then begin
    Var LText := '';
    if not Confetti.isRunning then LText := 'Pause - '
    else LText := 'Running - ';
    LText := LText + ALIntToStrW(Confetti.ParticuleCount) + ' particules - ';
    LText := LText + ALFormatFloatW('0.##', LAverageFPS, ALDefaultFormatSettingsW) + ' fps';
    StatusBarLabel.Text := LText;
  end;
end;

end.
