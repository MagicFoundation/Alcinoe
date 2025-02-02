{*******************************************************************************

TALInterpolatedAnimation
------------------------

The TALAnimation component is a refined iteration of Delphi's foundational
TAnimation object, meticulously tailored for mobile platforms. By forgoing the
traditional Timer mechanism and instead adopting platform-specific technologies,
this component offers a vastly improved animation experience for mobile users.
On Android, animations are seamlessly integrated with the Choreographer,
ensuring they sync perfectly with the device's refresh rate. Meanwhile, on iOS,
the precision of DisplayLink is harnessed, leading to optimized and fluid
animation rendering. Beyond these foundational changes, one of the most notable
enhancements is the capability to support custom interpolation algorithms. This
offers developers the flexibility to design unique and intricate animation
patterns, moving beyond the traditional ease-in or ease-out sequences.

TALSpringForceAnimation
-----------------------

Inspired by Android's SpringForce, the TALSpringForceAnimation Component brings
the intrigue of physics-based animations to the Delphi platform. This component
captures the real-world dynamism of objects influenced by spring mechanics. The
resulting animations are ones that stretch, bounce, and settle, mirroring
real-world behaviors and offering a tangible sense of realism to users.
Developers have the added advantage of being able to adjust various physical
properties of the spring, such as its stiffness and damping ratio. This ensures
that a broad spectrum of animation behaviors can be realized, catering to the
specific nuances of different applications.

DEMO :
------

https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALAnimation

*******************************************************************************}
unit Alcinoe.FMX.Ani;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if FMX.Ani.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  System.UITypes,
  System.TypInfo,
  {$IFDEF IOS}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.QuartzCore,
  {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  FMX.Types,
  Alcinoe.Common;

var
  // https://developer.apple.com/documentation/quartzcore/optimizing_promotion_refresh_rates_for_iphone_13_pro_and_ipad_pro?language=objc
  // Important: Be selective when requesting the maximum frame rate. If your animation
  // requests 120Hz but can’t keep up, it may render poorly. But the same
  // animation may be able to maintain a steady cadence at a lower rate.
  // => That's pure bullsheet! How are we supposed to know if the hardware will
  // even support 120Hz?
  ALMinimumFramesPerSecond: Integer = 80;
  ALMaximumFramesPerSecond: Integer = 120;
  ALPreferredFramesPerSecond: Integer = 120;

type

  {~~~~~~~~~~~~~~~~~~~}
  TALAnimation = Class;

  {~~~~~~~~~~~~~~}
  {$IFDEF ANDROID}
  TALChoreographerThread = class(TObject)
  private type
    TChoreographerFrameCallback = class(TJavaLocal, JChoreographer_FrameCallback)
    private
      fChoreographerThread: TALChoreographerThread;
    public
      constructor Create(const AChoreographerThread: TALChoreographerThread);
      procedure doFrame(frameTimeNanos: Int64); cdecl;
    end;
  private
    FChoreographerFrameCallback: TChoreographerFrameCallback;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
  protected
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetInterval(Value: Cardinal); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}

  {~~~~~~~~~~}
  {$IFDEF IOS}
  TALDisplayLinkThread = class(TObject)
  private type
    IDisplayLinkListener = interface(NSObject)
    ['{810AD3F0-265C-4A73-9B96-74103268884A}']
      procedure displayLinkUpdated; cdecl;
    end;
    TDisplayLinkListener = class(TOCLocal)
    private
      fDisplayLinkThread: TALDisplayLinkThread;
    protected
      function GetObjectiveCClass: PTypeInfo; override;
    public
      constructor Create(const aDisplayLinkThread: TALDisplayLinkThread);
      procedure displayLinkUpdated; cdecl;
    end;
  private
    fDisplayLink: CADisplayLink;
    fDisplayLinkListener: TDisplayLinkListener;
  private
    FTimerEvent: TNotifyEvent;
    FInterval: Cardinal;
    FEnabled: Boolean;
  protected
    procedure SetEnabled(Value: Boolean); virtual;
    procedure SetInterval(Value: Cardinal); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    property Interval: Cardinal read FInterval write SetInterval;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnTimer: TNotifyEvent read FTimerEvent write FTimerEvent;
  end;
  {$ENDIF}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAniThread = class({$IF defined(ANDROID)}TALChoreographerThread{$ELSEIF defined(IOS)}TALDisplayLinkThread{$ELSE}TTimer{$ENDIF})
  private
    FAniList: TList<TALAnimation>;
    FTime: Double;
    procedure DoSyncTimer(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAnimation(const Ani: TALAnimation);
    procedure RemoveAnimation(const Ani: TALAnimation);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAnimation = class(TObject)
  public const
    DefaultAniFrameRate = 60;
  public class var
    // The AniFrameRate property is unnecessary on Android and iOS since
    // the animation is synchronized with the system's Choreographer (Android)
    // or DisplayLink (iOS)
    AniFrameRate: Integer;
  private class var
    FAniThread: TALAniThread;
  private
    FTag: int64;
    FTagObject: TObject;
    FTagFloat: Double;
    FDelay: Single;
    FDelayTimeLeft: Single;
    FTime: Double;
    FLoop: Boolean;
    FAutoReverse: Boolean;
    FDidAutoReverse: Boolean;
    FInverse: Boolean;
    FSavedInverse: Boolean;
    FPause: Boolean;
    FRunning: Boolean;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    class procedure Uninitialize;
  protected
    procedure ProcessTick(const ATime, ADeltaTime: Double); virtual; abstract;
    procedure FirstFrame; virtual;
    procedure ProcessAnimation; virtual; abstract;
    procedure DoProcess; virtual;
    procedure DoFinish; virtual;
    class property AniThread: TALAniThread read FAniThread;
  public
    constructor Create; Virtual;
    destructor Destroy; override;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    procedure StopAtCurrent; virtual; abstract;
    property Running: Boolean read FRunning;
    property Pause: Boolean read FPause write FPause;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Delay: Single read FDelay write FDelay;
    property Loop: Boolean read FLoop write FLoop;
    property AutoReverse: Boolean read FAutoReverse write FAutoReverse;
    property Inverse: Boolean read FInverse write FInverse;
    property CurrentTime: Double read FTime;
    property OnFirstFrame: TNotifyEvent read FOnFirstFrame write FOnFirstFrame;
    property OnProcess: TNotifyEvent read FOnProcess write FOnProcess;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property Tag: int64 read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: Double read FTagFloat write FTagFloat;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDisplayAnimation = class(TALAnimation)
  protected
    procedure ProcessTick(const ATime, ADeltaTime: Double); override;
    procedure ProcessAnimation; override;
  public
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDisplayTimer = class(TALDisplayAnimation)
  private
    FInterval: Single;
    FIntervalTimeLeft: Single;
  protected
    procedure ProcessTick(const ATime, ADeltaTime: Double); override;
  public
    constructor Create; override;
    procedure Start; override;
    property Interval: Single read FInterval write FInterval;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  TALInterpolationType = (
    Linear,
    Quadratic,
    Cubic,
    Quartic,
    Quintic,
    Sinusoidal,
    Exponential,
    Circular,
    Elastic,
    Back,
    Bounce,
    Custom);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomInterpolationEvent = function(Sender: TObject): Single of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInterpolatedAnimation = class(TALAnimation)
  private
    FOvershoot: Single;
    FDuration: Single;
    FOnCustomInterpolation: TALCustomInterpolationEvent;
    FInterpolation: TALInterpolationType;
    FAnimationType: TAnimationType;
  protected
    procedure ProcessTick(const ATime, ADeltaTime: Double); override;
    function GetNormalizedTime: Single; virtual;
    function DoCustomInterpolation: Single; virtual;
  public
    constructor Create; override;
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; override;
    property AnimationType: TAnimationType read FAnimationType write FAnimationType;
    property Duration: Single read FDuration write FDuration;
    property Interpolation: TALInterpolationType read FInterpolation write FInterpolation;
    property OnCustomInterpolation: TALCustomInterpolationEvent read FOnCustomInterpolation write FOnCustomInterpolation;
    property Overshoot: Single read fOvershoot write fOvershoot;
    // Given the current time, NormalizedTime returns a number in the range from
    // 0 through 1, indicating how far the controlled property value has changed
    // from the StartValue to the StopValue
    property NormalizedTime: Single read GetNormalizedTime;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // float-based interpolated animations
  TALFloatAnimation = class(TALInterpolatedAnimation)
  private
    FStartValue: Single;
    FStopValue: Single;
    fCurrentValue: Single;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: Single read FStartValue write FStartValue;
    property StopValue: Single read FStopValue write FStopValue;
    property CurrentValue: Single read fCurrentValue;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // color-based interpolated animations
  TALColorAnimation = class(TALInterpolatedAnimation)
  private
    FStartValue: TAlphaColor;
    FStopValue: TAlphaColor;
    fCurrentValue: TAlphaColor;
  protected
    procedure ProcessAnimation; override;
  public
    constructor Create; override;
    procedure Start; override;
    property StartValue: TAlphaColor read FStartValue write FStartValue;
    property StopValue: TAlphaColor read FStopValue write FStopValue;
    property CurrentValue: TAlphaColor read fCurrentValue;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomPropertyAnimation = class(TFmxObject)
  protected
    FEnabled: Boolean;
    FInstance: TObject;
    FRttiProperty: TRttiProperty;
    FPath: string;
    FPropertyName: string;
    function getEnabled: Boolean; virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
    procedure SetPropertyName(const AValue: string);
    function FindProperty: Boolean;
    procedure ParentChanged; override;
    procedure Loaded; override;
  public
    property PropertyName: string read FPropertyName write SetPropertyName;
    procedure Start; virtual; abstract;
    procedure Stop; virtual;
  published
    property Enabled: Boolean read getEnabled write SetEnabled default False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // float-based interpolated animations tied to properties
  [ComponentPlatforms($FFFF)]
  TALFloatPropertyAnimation = class(TALCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fFloatAnimation: TALFloatAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnCustomInterpolation: TALCustomInterpolationEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getInterpolation: TALInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function GetCurrentTime: Single;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: Single;
    function GetStopValue: Single;
    function GetCurrentValue: Single;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure setInterpolation(const Value: TALInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: Single);
    procedure setStopValue(const Value: Single);
  protected
    function getEnabled: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
    function doCustomInterpolation(Sender: TObject): Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
    property CurrentValue: Single read GetCurrentValue;
    property CurrentTime: Single read GetCurrentTime;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TALInterpolationType read getInterpolation write setInterpolation default TALInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnCustomInterpolation: TALCustomInterpolationEvent read FOnCustomInterpolation write FOnCustomInterpolation;
    property PropertyName;
    property StartValue: Single read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Single read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // color-based interpolated animations tied to properties
  [ComponentPlatforms($FFFF)]
  TALColorPropertyAnimation = class(TALCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    fColorAnimation: TALColorAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnCustomInterpolation: TALCustomInterpolationEvent;
    function getAnimationType: TAnimationType;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getDuration: Single;
    function getInterpolation: TALInterpolationType;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function GetCurrentTime: Single;
    function getOvershoot: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: TAlphaColor;
    function GetStopValue: TAlphaColor;
    function GetCurrentValue: TAlphaColor;
    function OvershootStored: Boolean;
    procedure setAnimationType(const Value: TAnimationType);
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setDuration(const Value: Single);
    procedure setInterpolation(const Value: TALInterpolationType);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setOvershoot(const Value: Single);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: TAlphaColor);
    procedure setStopValue(const Value: TAlphaColor);
  protected
    function getEnabled: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
    function doCustomInterpolation(Sender: TObject): Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
    property CurrentValue: TAlphaColor read GetCurrentValue;
    property CurrentTime: Single read GetCurrentTime;
  published
    property AnimationType: TAnimationType read getAnimationType write setAnimationType default TAnimationType.In;
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Delay: Single read getDelay write setDelay;
    property Duration: Single read getDuration write setDuration nodefault;
    property Interpolation: TALInterpolationType read getInterpolation write setInterpolation default TALInterpolationType.Linear;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnCustomInterpolation: TALCustomInterpolationEvent read FOnCustomInterpolation write FOnCustomInterpolation;
    property PropertyName;
    property StartValue: TAlphaColor read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TAlphaColor read GetStopValue write setStopValue stored True nodefault;
    property Overshoot: Single read getOvershoot write setOvershoot Stored OvershootStored nodefault;
  end;

//function ALInterpolateLinear(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single): Single; => function FMX.Ani.InterpolateLinear(t, B, C, D: Single): Single;
//function ALInterpolateSinusoidal(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateSine(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateQuintic(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateQuint(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateQuartic(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateQuart(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateQuadratic(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateQuad(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateExponential(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateExpo(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateElastic(AElapsedTime, AStartOfRange, ARangeSpan, ADuration, AAmplitude, AOscillationPeriod: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateElastic(t, B, C, D, A, P: Single; AType: TAnimationType): Single;
//function ALInterpolateCubic(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateCubic(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateCircular(AElapsedTime, AStartOfRange, ARangeSpan, ADuration: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateCirc(t, B, C, D: Single; AType: TAnimationType): Single;
//function ALInterpolateBack(AElapsedTime, AStartOfRange, ARangeSpan, ADuration, AOvershoot: Single; AType: TAnimationType): Single; => function FMX.Ani.InterpolateBack(t, B, C, D, S: Single; AType: TAnimationType): Single;
function ALInterpolateBounce(AElapsedTime, ADuration: Single; AType: TAnimationType): Single;
function ALInterpolateDecelerate(input: Single; const factor: Single = 1.0): Single;
function ALInterpolateViscousFluid(input: Single): Single;
function ALInterpolateColor(Start, Stop: TAlphaColor; T: Single): TAlphaColor;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Spring Force defines the characteristics of the spring being used in the animation.
  // By configuring the stiffness and damping ratio, callers can create a spring with the look and
  // feel suits their use case. Stiffness corresponds to the spring constant. The stiffer the spring
  // is, the harder it is to stretch it, the faster it undergoes dampening.
  // Spring damping ratio describes how oscillations in a system decay after a disturbance.
  // When damping ratio > 1* (i.e. over-damped), the object will quickly return to the rest position
  // without overshooting. If damping ratio equals to 1 (i.e. critically damped), the object will
  // return to equilibrium within the shortest amount of time. When damping ratio is less than 1
  // (i.e. under-damped), the mass tends to overshoot, and return, and overshoot again. Without any
  // damping (i.e. damping ratio := 0), the mass will oscillate forever.
  // Taken from Android SpringForce
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if android SpringForce.java was not updated and adjust the IFDEF'}
    //Compare <Alcinoe>\References\Android\SpringForce.java with https://android.googlesource.com/platform/frameworks/support/dynamicanimation/dynamicanimation/src/main/java/androidx/dynamicanimation/animation/SpringForce.java
  {$ENDIF}
  TALSpringForce = class(TObject)
  public
    type
      TMassState = record
        Value: Double;
        Velocity: Double;
      end;
  public
    // The minimum visible change in pixels that can be visible to users.
    const MIN_VISIBLE_CHANGE_PIXELS = 1.0;
    // The minimum visible change in degrees that can be visible to users.
    const MIN_VISIBLE_CHANGE_ROTATION_DEGREES = 1.0 / 10.0;
    // The minimum visible change in alpha that can be visible to users.
    const MIN_VISIBLE_CHANGE_ALPHA = 1.0 / 256.0;
    // The minimum visible change in scale that can be visible to users.
    const MIN_VISIBLE_CHANGE_SCALE = 1.0 / 500.0;
    // Multiplier to the min visible change value for value threshold
    const THRESHOLD_MULTIPLIER = 0.75;
    // Stiffness constant for extremely stiff spring.
    const STIFFNESS_HIGH = 10_000.0;
    // Stiffness constant for medium stiff spring. This is the default stiffness for spring force.
    const STIFFNESS_MEDIUM = 1500.0;
    // Stiffness constant for a spring with low stiffness.
    const STIFFNESS_LOW = 200.0;
    // Stiffness constant for a spring with very low stiffness.
    const STIFFNESS_VERY_LOW = 50.0;
    // Damping ratio for a very bouncy spring. Note for under-damped springs
    // (i.e. damping ratio < 1), the lower the damping ratio, the more bouncy the spring.
    const DAMPING_RATIO_HIGH_BOUNCY = 0.2;
    // Damping ratio for a medium bouncy spring. This is also the default damping ratio for spring
    // force. Note for under-damped springs (i.e. damping ratio < 1), the lower the damping ratio,
    // the more bouncy the spring.
    const DAMPING_RATIO_MEDIUM_BOUNCY = 0.5;
    // Damping ratio for a spring with low bounciness. Note for under-damped springs
    // (i.e. damping ratio < 1), the lower the damping ratio, the higher the bounciness.
    const DAMPING_RATIO_LOW_BOUNCY = 0.75;
    // Damping ratio for a spring with no bounciness. This damping ratio will create a critically
    // damped spring that returns to equilibrium within the shortest amount of time without
    // oscillating.
    const DAMPING_RATIO_NO_BOUNCY = 1.0;
  private
    // This multiplier is used to calculate the velocity threshold given a certain value threshold.
    // The idea is that if it takes >= 1 frame to move the value threshold amount, then the velocity
    // is a reasonable threshold.
    const VELOCITY_THRESHOLD_MULTIPLIER = 1000.0 / 16.0;
  public
    // Natural frequency
    FNaturalFreq: double;
    // Damping ratio.
    FDampingRatio: double;
  private
    // Indicates whether the spring has been initialized
    FInitialized: boolean;
    // Threshold for velocity and value to determine when it's reasonable to assume that the spring
    // is approximately at rest.
    FValueThreshold: double;
    FVelocityThreshold: double;
    // Intermediate values to simplify the spring function calculation per frame.
    FGammaPlus: double;
    FGammaMinus: double;
    FDampedFreq: double;
    // Final position of the spring. This must be set before the start of the animation.
    FFinalPosition: double;
  private
    procedure init;
  public
    constructor Create;
    function setStiffness(stiffness: Double): TALSpringForce;
    function getStiffness: Double;
    function setDampingRatio(dampingRatio: Double): TALSpringForce;
    function getDampingRatio: Double;
    function setFinalPosition(finalPosition: Double): TALSpringForce;
    function getFinalPosition: Double;
    function getAcceleration(lastDisplacement: Double; lastVelocity: Double): Double;
    function isAtEquilibrium(value: Double; velocity: Double): boolean;
    function updateValues(lastDisplacement: double; lastVelocity: double; timeElapsedMS: double): TMassState;
    function setValueThreshold(threshold: double): TALSpringForce;
    function getValueThreshold: Double;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALSpringForceAnimation is an animation that is driven by a TALSpringForce. The spring force defines
  // the spring's stiffness, damping ratio, as well as the rest position. Once the SpringAnimation is
  // started, on each frame the spring force will update the animation's value and velocity.
  // The animation will continue to run until the spring force reaches equilibrium. If the spring used
  // in the animation is undamped, the animation will never reach equilibrium. Instead, it will
  // oscillate forever.
  TALSpringForceAnimation = class(TALAnimation)
  private
    FSpringForce: TALSpringForce;
    FInitialVelocity: Single;
    FCurrentVelocity: Single;
    FStartValue: Single;
    fCurrentValue: Single;
    FDeltaTime: Double;
    procedure setStiffness(stiffness: Single);
    function getStiffness: Single;
    procedure setDampingRatio(dampingRatio: Single);
    function getDampingRatio: Single;
    procedure SetStopValue(const Value: Single);
    function GetStopValue: Single;
    procedure SetValueThreshold(const Value: Single);
    function GetValueThreshold: Single;
  protected
    procedure ProcessTick(const ATime, ADeltaTime: Double); override;
    procedure ProcessAnimation; override;
    property SpringForce: TALSpringForce read FSpringForce;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; override;
    property Stiffness: Single read getStiffness write setStiffness;
    property DampingRatio: Single read getDampingRatio write setDampingRatio;
    property ValueThreshold: Single read getValueThreshold write setValueThreshold;
    property InitialVelocity: Single read FInitialVelocity write FInitialVelocity;
    property CurrentVelocity: Single read FCurrentVelocity;
    property StartValue: Single read FStartValue write FStartValue;
    property StopValue: Single read GetStopValue write SetStopValue;
    property CurrentValue: Single read fCurrentValue;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALSpringForcePropertyAnimation = class(TALCustomPropertyAnimation)
  private
    FStartFromCurrent: Boolean;
    FSpringForceAnimation: TALSpringForceAnimation;
    FOnFirstFrame: TNotifyEvent;
    FOnProcess: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    FOnCustomInterpolation: TALCustomInterpolationEvent;
    function getAutoReverse: Boolean;
    function getDelay: Single;
    function getInverse: Boolean;
    function getLoop: Boolean;
    function GetCurrentTime: Single;
    function getPause: Boolean;
    function getRunning: Boolean;
    function GetStartValue: Single;
    function GetStopValue: Single;
    function GetCurrentValue: Single;
    function GetCurrentVelocity: Single;
    function getStiffness: Single;
    function getDampingRatio: Single;
    function GetInitialVelocity: Single;
    function GetValueThreshold: Single;
    procedure setAutoReverse(const Value: Boolean);
    procedure setDelay(const Value: Single);
    procedure setInverse(const Value: Boolean);
    procedure setLoop(const Value: Boolean);
    procedure setPause(const Value: Boolean);
    procedure SetStartValue(const Value: Single);
    procedure setStopValue(const Value: Single);
    procedure setStiffness(const Value: Single);
    procedure setDampingRatio(const Value: Single);
    procedure SetInitialVelocity(const Value: Single);
    procedure SetValueThreshold(const Value: Single);
    function StiffnessStored: Boolean;
    function DampingRatioStored: Boolean;
    function ValueThresholdStored: Boolean;
    function InitialVelocityStored: Boolean;
  protected
    function getEnabled: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    procedure doFirstFrame(Sender: TObject);
    procedure doProcess(Sender: TObject);
    procedure doFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure StopAtCurrent; virtual;
    property Running: Boolean read getRunning;
    property Pause: Boolean read getPause write setPause;
    property CurrentValue: Single read GetCurrentValue;
    property CurrentVelocity: Single read GetCurrentVelocity;
    property CurrentTime: Single read GetCurrentTime;
  published
    property AutoReverse: Boolean read getAutoReverse write setAutoReverse default False;
    property Delay: Single read getDelay write setDelay;
    property Inverse: Boolean read getInverse write setInverse default False;
    property Loop: Boolean read getLoop write setLoop default False;
    property OnFirstFrame: TNotifyEvent read fOnFirstFrame write fOnFirstFrame;
    property OnProcess: TNotifyEvent read fOnProcess write fOnProcess;
    property OnFinish: TNotifyEvent read fOnFinish write fOnFinish;
    property OnCustomInterpolation: TALCustomInterpolationEvent read FOnCustomInterpolation write FOnCustomInterpolation;
    property PropertyName;
    property StartValue: Single read GetStartValue write SetStartValue stored True nodefault;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: Single read GetStopValue write setStopValue stored True nodefault;
    property Stiffness: Single read getStiffness write setStiffness stored StiffnessStored nodefault;
    property DampingRatio: Single read getDampingRatio write setDampingRatio stored DampingRatioStored nodefault;
    property ValueThreshold: Single read getValueThreshold write setValueThreshold stored ValueThresholdStored nodefault;
    property InitialVelocity: Single read GetInitialVelocity write SetInitialVelocity stored InitialVelocityStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // Converted from Chromium's source code:
  // https://github.com/chromium/chrome/browser/resources/lens/overlay/cubic_bezier.ts
  // - JavaScript "ease-in" corresponds to: cubic-bezier(0.42, 0, 1, 1)
  // - JavaScript "ease-out" corresponds to: cubic-bezier(0, 0, 0.58, 1)
  // - JavaScript "ease-in-out" corresponds to: cubic-bezier(0.42, 0, 0.58, 1)
  TALCubicBezier = class
  private
    const
      BEZIER_EPSILON = 1e-7;
      CUBIC_BEZIER_SPLINE_SAMPLES = 11;
      MAX_NEWTON_METHOD_ITERATIONS = 4;
  private
    P1, P2: TALPointD;
    A, B, C: TALPointD;
    SplineSamples: array[0..CUBIC_BEZIER_SPLINE_SAMPLES - 1] of Double;
    procedure InitCoefficients(const P1, P2: TALPointD);
    procedure InitSpline;
    function SampleCurveX(T: Double): Double;
    function SampleCurveDerivativeX(T: Double): Double;
    function SampleCurveY(T: Double): Double;
    function ToFinite(N: Double): Double;
  public
    constructor Create(X1, Y1, X2, Y2: Double);
    // Determines the Y value of the cubic Bezier curve for a given X value.
    function SolveForY(X: Double): Double;
    // Finds the parameter T (a value between 0 and 1) for a given X value on the curve.
    function SolveCurveX(X: Double): Double;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.math,
  System.Math.Vectors,
  {$IFDEF IOS}
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  Alcinoe.iOSapi.QuartzCore,
  {$ENDIF}
  FMX.Ani,
  FMX.Utils;

{*****************************************************}
// Taken from android.view.animation.BounceInterpolator
function ALInterpolateBounce(AElapsedTime, ADuration: Single; AType: TAnimationType): Single;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _bounce(t: Single): Single;
  begin
    Result := t * t * 8.0;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _EaseOut(t, D: Single): Single;
  begin
    t := t / D;
    // _b(t) = t * t * 8
    // bs(t) = _b(t) for t < 0.3535
    // bs(t) = _b(t - 0.54719) + 0.7 for t < 0.7408
    // bs(t) = _b(t - 0.8526) + 0.9 for t < 0.9644
    // bs(t) = _b(t - 1.0435) + 0.95 for t <= 1.0
    // b(t) = bs(t * 1.1226)
    t := t * 1.1226;
    if (t < 0.3535) then result := _bounce(t)
    else if (t < 0.7408) then result := _bounce(t - 0.54719) + 0.7
    else if (t < 0.9644) then result := _bounce(t - 0.8526) + 0.9
    else result := _bounce(t - 1.0435) + 0.95;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _EaseIn(t, D: Single): Single;
  begin
    Result := 1 - _EaseOut(D - t, D);
  end;

begin
  case AType of

    TAnimationType.In:
      begin
        Result := _EaseIn(AElapsedTime, ADuration);
      end;

    TAnimationType.Out:
      begin
        Result := _EaseOut(AElapsedTime, ADuration);
      end;

    TAnimationType.InOut:
      begin
        if AElapsedTime < ADuration / 2 then
          Result := _EaseIn(AElapsedTime * 2, ADuration) * 0.5
        else
          Result := _EaseOut(AElapsedTime * 2 - ADuration, ADuration) * 0.5 + 0.5;
      end;

  else
    Result := 0;
  end;
end;

{*********************************************************************************************}
// @param factor Degree to which the animation should be eased. Setting factor to 1.0f produces
// an upside-down y=x^2 parabola. Increasing factor above 1.0f exaggerates the
// ease-out effect (i.e., it starts even faster and ends evens slower).
// Taken from android.view.animation.DecelerateInterpolator
function ALInterpolateDecelerate(input: Single; const factor: Single = 1.0): Single;
begin
  if SameValue(Factor, 1.0, TEpsilon.Vector) then
    //Exactly the same result as InterpolateQuartic
    result := {(float)} (1.0 - (1.0 - input) * (1.0 - input))
  else
    result := {(float)} (1.0 - power((1.0 - input), 2 * Factor));
end;

{***}
const
  // Controls the viscous fluid effect (how much of it).
  ALVISCOUS_FLUID_SCALE = 8.0;
var
  ALVISCOUS_FLUID_NORMALIZE: Single;
  ALVISCOUS_FLUID_OFFSET: Single;

{************************************************************}
// Taken from android.widget.Scroller.ViscousFluidInterpolator
function ALViscousFluid(x: Single): Single;
begin
  x := x * ALVISCOUS_FLUID_SCALE;
  if (x < 1.0) then begin
      x := x - (1.0 - {(Single)} Exp(-x));
  end
  else begin
      var start: Single := 0.36787944117;   // 1/e == exp(-1)
      x := 1.0 - {(Single)} Exp(1.0 - x);
      x := start + x * (1.0 - start);
  end;
  result := x;
end;

{************************************************************}
// Taken from android.widget.Scroller.ViscousFluidInterpolator
procedure ALInitViscousFluid;
begin
  // must be set to 1.0 (used in viscousFluid())
  ALVISCOUS_FLUID_NORMALIZE := 1.0 / ALViscousFluid(1.0);
  // account for very small Singleing-point error
  ALVISCOUS_FLUID_OFFSET := 1.0 - ALVISCOUS_FLUID_NORMALIZE * ALViscousFluid(1.0);
end;

{************************************************************}
// Taken from android.widget.Scroller.ViscousFluidInterpolator
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if android.widget.Scroller.ViscousFluidInterpolator was not updated and adjust the IFDEF'}
  //Compare <Alcinoe>\References\Android\Scroller.java with <SDKs>c:\SDKs\android\sources\android-33\android\widget\Scroller.java
{$ENDIF}
function ALInterpolateViscousFluid(input: Single): Single;
begin
  const interpolated: Single = ALVISCOUS_FLUID_NORMALIZE * ALViscousFluid(input);
  if (interpolated > 0) then result := interpolated + ALVISCOUS_FLUID_OFFSET
  else result := interpolated;
end;

{****************************************************************************}
function ALInterpolateColor(Start, Stop: TAlphaColor; T: Single): TAlphaColor;
begin
  // If start or stop is null, then perform the animation only on the alpha channel.
  if Start = TALphaColors.Null then begin
    TAlphaColorRec(Start).A := 0;
    TAlphaColorRec(Start).R := TAlphaColorRec(Stop).R;
    TAlphaColorRec(Start).G := TAlphaColorRec(Stop).G;
    TAlphaColorRec(Start).B := TAlphaColorRec(Stop).B;
  end;
  if Stop = TALphaColors.Null then begin
    TAlphaColorRec(Stop).A := 0;
    TAlphaColorRec(Stop).R := TAlphaColorRec(Start).R;
    TAlphaColorRec(Stop).G := TAlphaColorRec(Start).G;
    TAlphaColorRec(Stop).B := TAlphaColorRec(Start).B;
  end;
  TAlphaColorRec(Result).A := TAlphaColorRec(Start).A + Trunc((TAlphaColorRec(Stop).A - TAlphaColorRec(Start).A) * T);
  TAlphaColorRec(Result).R := TAlphaColorRec(Start).R + Trunc((TAlphaColorRec(Stop).R - TAlphaColorRec(Start).R) * T);
  TAlphaColorRec(Result).G := TAlphaColorRec(Start).G + Trunc((TAlphaColorRec(Stop).G - TAlphaColorRec(Start).G) * T);
  TAlphaColorRec(Result).B := TAlphaColorRec(Start).B + Trunc((TAlphaColorRec(Stop).B - TAlphaColorRec(Start).B) * T);
end;


{$IFDEF ANDROID}

{************************************************************************************************************************}
constructor TALChoreographerThread.TChoreographerFrameCallback.Create(const AChoreographerThread: TALChoreographerThread);
 begin
  inherited Create;
  fChoreographerThread := AChoreographerThread;
end;

{******************************************************************************************}
procedure TALChoreographerThread.TChoreographerFrameCallback.doFrame(frameTimeNanos: Int64);
begin

  {$IFDEF DEBUG}
  //ALLog('TALChoreographerThread.TChoreographerFrameCallback.doFrame');
  {$ENDIF}

  if assigned(fChoreographerThread.FTimerEvent) then
    fChoreographerThread.FTimerEvent(fChoreographerThread);

  if fChoreographerThread.Enabled then
    TJChoreographer.JavaClass.getInstance.postFrameCallback(self);

end;

{************************************************************}
constructor TALChoreographerThread.Create(AOwner: TComponent);
begin
  inherited create;
  fChoreographerFrameCallback := TChoreographerFrameCallback.create(self);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{****************************************}
destructor TALChoreographerThread.Destroy;
begin
  alFreeAndNil(fChoreographerFrameCallback);
  inherited;
end;

{**********************************************************}
procedure TALChoreographerThread.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then TJChoreographer.JavaClass.getInstance.postFrameCallback(fChoreographerFrameCallback)
    else TJChoreographer.JavaClass.getInstance.removeFrameCallback(fChoreographerFrameCallback);
  end;
end;

{************************************************************}
procedure TALChoreographerThread.SetInterval(Value: Cardinal);
begin
  FInterval := Value;
end;

{$ENDIF}

{$IFDEF IOS}

{***********************************************************************************************************}
constructor TALDisplayLinkThread.TDisplayLinkListener.Create(const aDisplayLinkThread: TALDisplayLinkThread);
begin
  inherited Create;
  fDisplayLinkThread := aDisplayLinkThread;
end;

{*********************************************************************}
procedure TALDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated;
begin

  {$IFDEF DEBUG}
  //ALLog('TALDisplayLinkThread.TDisplayLinkListener.displayLinkUpdated');
  {$ENDIF}

  if assigned(fDisplayLinkThread.FTimerEvent) then
    fDisplayLinkThread.FTimerEvent(fDisplayLinkThread);

end;

{*******************************************************************************}
function TALDisplayLinkThread.TDisplayLinkListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDisplayLinkListener);
end;

{**********************************************************}
constructor TALDisplayLinkThread.Create(AOwner: TComponent);
begin
  inherited create;
  fDisplayLinkListener := TDisplayLinkListener.Create(self);
  fDisplayLink := TCADisplayLink.Wrap(TCADisplayLink.OCClass.displayLinkWithTarget(fDisplayLinkListener.GetObjectID, sel_getUid('displayLinkUpdated')));
  fDisplayLink.retain;
  if GlobalUseMetal then begin
    // In OpenGL, the animation appears more jerky when using
    // a high frame rate
    if TOSVersion.Check(17) then begin
      var LFrameRateRange: CAFrameRateRange;
      LFrameRateRange.minimum := ALMinimumFramesPerSecond;
      LFrameRateRange.maximum := ALMaximumFramesPerSecond;
      LFrameRateRange.preferred := ALPreferredFramesPerSecond;
      TALCADisplayLink.Wrap(NSObjectToID(fDisplayLink)).setPreferredFrameRateRange(LFrameRateRange);
    end
    else
      TALCADisplayLink.Wrap(NSObjectToID(fDisplayLink)).setPreferredFramesPerSecond(ALPreferredFramesPerSecond);
  end;
  fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  fDisplayLink.setPaused(true);
  FTimerEvent := nil;
  Interval := 1;
  FEnabled := False;
end;

{**************************************}
destructor TALDisplayLinkThread.Destroy;
begin
  // Removes the display link from all run loop modes.
  // Removing the display link from all run loop modes causes it to be released by the run loop. The display link also releases the target.
  // invalidate is thread safe meaning that it can be called from a thread separate to the one in which the display link is running.
  fDisplayLink.invalidate;
  fDisplayLink.release;
  AlFreeAndNil(fDisplayLinkListener);
  inherited;
end;

{********************************************************}
procedure TALDisplayLinkThread.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then fDisplayLink.setPaused(False)
    else fDisplayLink.setPaused(True);
  end;
end;

{**********************************************************}
procedure TALDisplayLinkThread.SetInterval(Value: Cardinal);
begin
  FInterval := Value;
end;

{$ENDIF}

{******************************}
constructor TALAniThread.Create;
begin
  inherited Create(nil);
  TALAnimation.AniFrameRate := EnsureRange(TALAnimation.AniFrameRate, 5, 120);
  Interval := Trunc(1000 / TALAnimation.AniFrameRate / 10) * 10;
  if (Interval <= 0) then
    Interval := 1;

  OnTimer := DoSyncTimer;
  FAniList := TList<TALAnimation>.Create;
  FTime := ALElapsedTimeSecondsAsDouble;

  Enabled := False;
end;

{******************************}
destructor TALAniThread.Destroy;
begin
  ALFreeAndNil(FAniList);
  inherited Destroy;
end;

{***********************************************************}
procedure TALAniThread.AddAnimation(const Ani: TALAnimation);
begin
  if FAniList.IndexOf(Ani) < 0 then
    FAniList.Add(Ani);
  if not Enabled and (FAniList.Count > 0) then
    FTime := ALElapsedTimeSecondsAsDouble;
  Enabled := FAniList.Count > 0;
end;

{**************************************************************}
procedure TALAniThread.RemoveAnimation(const Ani: TALAnimation);
begin
  FAniList.Remove(Ani);
  Enabled := FAniList.Count > 0;
end;

{**************************************************}
procedure TALAniThread.DoSyncTimer(Sender: TObject);
begin
  {$IF not defined(ALDPK)}
  var NewTime := ALElapsedTimeSecondsAsDouble;
  var LDeltaTime := NewTime - FTime;
  FTime := NewTime;
  if LDeltaTime <= 0 then
    Exit;
  if FAniList.Count > 0 then begin
    var I := FAniList.Count - 1;
    while I >= 0 do begin
      var Ani := FAniList[I];
      Ani.ProcessTick(FTime, LDeltaTime);
      Dec(I);
      if I >= FAniList.Count then
        I := FAniList.Count - 1;
    end;
  end;
  {$ENDIF}
end;

{******************************}
constructor TALAnimation.Create;
begin
  inherited Create;
  FEnabled := False;
  fTag := 0;
  FTagObject := nil;
  FTagFloat := 0.0;
end;

{******************************}
destructor TALAnimation.Destroy;
begin
  if AniThread <> nil then
    AniThread.FAniList.Remove(Self);
  inherited Destroy;
end;

{******************************************************}
procedure TALAnimation.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      Start
    else
      Stop;
  end;
end;

{********************************}
procedure TALAnimation.FirstFrame;
begin
  // Do not call fOnFirstFrame if enabled is false
  if fEnabled and assigned(fOnFirstFrame) then
    fOnFirstFrame(Self);
end;

{*******************************}
procedure TALAnimation.DoProcess;
begin
  // Do not call FOnProcess if enabled is false
  if fEnabled and Assigned(FOnProcess) then
    FOnProcess(Self);
end;

{******************************}
procedure TALAnimation.DoFinish;
begin
  // Do not call FOnFinish if enabled is false
  if fEnabled and Assigned(FOnFinish) then
    FOnFinish(Self);
end;

{****************************************}
class procedure TALAnimation.Uninitialize;
begin
  ALFreeAndNil(FAniThread);
end;

{*************************************************************************}
procedure TALDisplayAnimation.ProcessTick(const ATime, ADeltaTime: Double);
begin
  if (not FRunning) or FPause then
    Exit;

  if (FDelay > 0) and (FDelayTimeLeft <> 0) then begin
    FDelayTimeLeft := FDelayTimeLeft - ADeltaTime;
    if FDelayTimeLeft <= 0 then begin
      FDelayTimeLeft := 0;
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;
    Exit;
  end;

  FTime := FTime + ADeltaTime;

  ProcessAnimation;
  DoProcess;

  if not FRunning then begin
    if AniThread <> nil then
      AniThread.RemoveAnimation(Self);
    DoFinish;
  end;
end;

{*********************************************}
procedure TALDisplayAnimation.ProcessAnimation;
begin
  // Nothing to do
end;

{**********************************}
procedure TALDisplayAnimation.Start;
begin
  if (FRunning) then
    Exit;
  FEnabled := True;
  FRunning := True;
  FTime := 0;
  FDelayTimeLeft := FDelay;
  if FDelay = 0 then begin
    FirstFrame;
    ProcessAnimation;
    DoProcess;
  end;

  if AniThread = nil then
    FAniThread := TALAniThread.Create;

  AniThread.AddAnimation(Self);
  if not AniThread.Enabled then
    Stop;
end;

{*********************************}
procedure TALDisplayAnimation.Stop;
begin
  StopAtCurrent;
end;

{******************************************}
procedure TALDisplayAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    AniThread.RemoveAnimation(Self);

  FRunning := False;
  DoFinish;
end;

{*********************************}
constructor TALDisplayTimer.Create;
begin
  inherited Create;
  FInterval := 0.05;
end;

{*********************************************************************}
procedure TALDisplayTimer.ProcessTick(const ATime, ADeltaTime: Double);
begin
  if (not FRunning) or FPause then
    Exit;

  if (FDelay > 0) and (FDelayTimeLeft <> 0) then begin
    FDelayTimeLeft := FDelayTimeLeft - ADeltaTime;
    if FDelayTimeLeft <= 0 then begin
      FDelayTimeLeft := 0;
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;
    Exit;
  end;

  FIntervalTimeLeft := FIntervalTimeLeft - ADeltaTime;
  if CompareValue(FIntervalTimeLeft, 0, 0.001) > 0 then exit;
  FIntervalTimeLeft := FInterval;

  FTime := FTime + ADeltaTime;

  ProcessAnimation;
  DoProcess;

  if not FRunning then begin
    if AniThread <> nil then
      AniThread.RemoveAnimation(Self);
    DoFinish;
  end;
end;

{******************************}
procedure TALDisplayTimer.Start;
begin
  if (FRunning) then
    Exit;
  FEnabled := True;
  FRunning := True;
  FTime := 0;
  FIntervalTimeLeft := FInterval;
  FDelayTimeLeft := FDelay;
  if FDelay = 0 then begin
    FirstFrame;
    ProcessAnimation;
    DoProcess;
  end;

  if AniThread = nil then
    FAniThread := TALAniThread.Create;

  AniThread.AddAnimation(Self);
  if not AniThread.Enabled then
    Stop;
end;

{******************************************}
constructor TALInterpolatedAnimation.Create;
begin
  inherited Create;
  FDuration := 0.2;
  FOvershoot := 0.0;
end;

{******************************************************************************}
procedure TALInterpolatedAnimation.ProcessTick(const ATime, ADeltaTime: Double);
begin
  if (not FRunning) or FPause then
    Exit;

  var LContinueRunning := FRunning;

  if (FDelay > 0) and (FDelayTimeLeft <> 0) then begin
    FDelayTimeLeft := FDelayTimeLeft - ADeltaTime;
    if FDelayTimeLeft <= 0 then begin
      FDelayTimeLeft := 0;
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;
    Exit;
  end;

  if FInverse then FTime := FTime - ADeltaTime
  else FTime := FTime + ADeltaTime;

  if FTime >= FDuration then begin
    FTime := FDuration;
    if FLoop then begin
      if FAutoReverse then begin
        FInverse := True;
        FTime := FDuration;
      end
      else FTime := 0;
    end
    else begin
      if FAutoReverse and (not FDidAutoReverse) then begin
        FDidAutoReverse := true;
        FInverse := True;
        FTime := FDuration;
      end
      else LContinueRunning := False;
    end;
  end
  else if FTime <= 0 then begin
    FTime := 0;
    if FLoop then begin
      if FAutoReverse then begin
        FInverse := False;
        FTime := 0;
      end
      else FTime := FDuration;
    end
    else begin
      if FAutoReverse and (not FDidAutoReverse) then begin
        FDidAutoReverse := True;
        FInverse := False;
        FTime := 0;
      end
      else LContinueRunning := False;
    end;
  end;

  ProcessAnimation;
  DoProcess;

  FRunning := FRunning and LContinueRunning;
  if not FRunning then begin
    FInverse := FSavedInverse;
    if AniThread <> nil then
      AniThread.RemoveAnimation(Self);
    DoFinish;
  end;
end;

{***************************************}
procedure TALInterpolatedAnimation.Start;
begin
  if (FRunning) then
    Exit;
  FEnabled := True;
  FRunning := True;
  FDidAutoReverse := False;
  FSavedInverse := FInverse;
  if FInverse then FTime := FDuration
  else FTime := 0;
  FDelayTimeLeft := FDelay;
  if FDelay = 0 then begin
    FirstFrame;
    ProcessAnimation;
    DoProcess;
  end;

  if AniThread = nil then
    FAniThread := TALAniThread.Create;

  AniThread.AddAnimation(Self);
  if not AniThread.Enabled then
    Stop;
end;

{**************************************}
procedure TALInterpolatedAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    AniThread.RemoveAnimation(Self);

  FInverse := FSavedInverse;
  if FInverse then FTime := 0
  else FTime := FDuration;
  ProcessAnimation;
  DoProcess;
  FRunning := False;
  DoFinish;
end;

{***********************************************}
procedure TALInterpolatedAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    AniThread.RemoveAnimation(Self);

  FInverse := FSavedInverse;
  FRunning := False;
  DoFinish;
end;

{**********************************************************}
function TALInterpolatedAnimation.GetNormalizedTime: Single;
begin
  Result := 0;
  if (FDuration > 0) and (FDelayTimeLeft <= 0) then
  begin
    case FInterpolation of
      TALInterpolationType.Linear:
        Result := InterpolateLinear(FTime, 0, 1, FDuration);
      TALInterpolationType.Quadratic:
        Result := InterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Cubic:
        Result := InterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Quartic:
        Result := InterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Quintic:
        Result := InterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Sinusoidal:
        Result := InterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Exponential:
        Result := InterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Circular:
        Result := InterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      TALInterpolationType.Elastic:
        Result := InterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      TALInterpolationType.Back:
        Result := InterpolateBack(FTime, 0, 1, FDuration, fOvershoot, FAnimationType);
      TALInterpolationType.Bounce:
        //the InterpolateBounce is a little buggy
        //Result := InterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
        Result := ALInterpolateBounce(FTime, FDuration, FAnimationType);
      TALInterpolationType.Custom:
        Result := DoCustomInterpolation;
    end;
  end;
end;

{**************************************************************}
function TALInterpolatedAnimation.DoCustomInterpolation: Single;
begin
  if Assigned(FOnCustomInterpolation) then
    result := FOnCustomInterpolation(Self)
  else
    result := 0;
end;

{***********************************}
constructor TALFloatAnimation.Create;
begin
  inherited;
  FStartValue := 0;
  FStopValue := 0;
  fCurrentValue := 0;
end;

{********************************}
procedure TALFloatAnimation.Start;
begin
  if (Running) then
    Exit;
  fCurrentValue := FStartValue;
  inherited Start;
end;

{*******************************************}
procedure TALFloatAnimation.ProcessAnimation;
begin
  fCurrentValue := FStartValue + (FStopValue - FStartValue) * NormalizedTime;
end;

{***********************************}
constructor TALColorAnimation.Create;
begin
  inherited;
  FStartValue := $FFFFFFFF;
  FStopValue := $FFFFFFFF;
  fCurrentValue := $FFFFFFFF;
end;

{********************************}
procedure TALColorAnimation.Start;
begin
  if (Running) then
    Exit;
  fCurrentValue := FStartValue;
  inherited Start;
end;

{*******************************************}
procedure TALColorAnimation.ProcessAnimation;
begin
  fCurrentValue := ALInterpolateColor(FStartValue, FStopValue, NormalizedTime);
end;

{********************************************************}
function TALCustomPropertyAnimation.FindProperty: Boolean;
var
  Persistent: string;
  Comp: TFmxObject;
  I: Integer;
  T: TRttiType;
  P: TRttiProperty;
  Properties: TList<TRttiProperty>;
begin
  Result := False;

  //This is to permit to put a TALxxxPropertyAnimation on a Form
  //but use it only via it's onprocess event
  if FPropertyName = '' then begin
    FInstance := nil;
    FRttiProperty := nil;
    FPath := '';
    exit(true);
  end;

  if (Parent <> nil) and (FPropertyName <> '') then
  begin
    if FInstance = nil then
    begin
      FInstance := Parent;
      FPath := FPropertyName;
      while FPath.Contains('.') do
      begin
        Persistent := GetToken(FPath, '.');
        T := SharedContext.GetType(FInstance.ClassInfo);
        if T <> nil then
        begin
          P := T.GetProperty(Persistent);
          if (P <> nil) and (P.PropertyType.IsInstance) then
            FInstance := P.GetValue(FInstance).AsObject
          else
          if Parent <> nil then
          begin
            for I := 0 to Parent.ChildrenCount - 1 do
              if CompareText(Parent.Children[I].Name, Persistent) = 0 then
              begin
                Comp := Parent.Children[I];
                T := SharedContext.GetType(Comp.ClassInfo);
                if T <> nil then
                begin
                  P := T.GetProperty(FPath);
                  if P <> nil then
                  begin
                    FInstance := Comp;
                    Break;
                  end;
                end;
              end;
          end;
        end;
      end;
      if FInstance <> nil then
      begin

        if not ClonePropertiesCache.TryGetValue(FInstance.ClassName, Properties) then
        begin
          Properties := TList<TRttiProperty>.Create;
          ClonePropertiesCache.Add(FInstance.ClassName, Properties);
        end;

        for P in Properties do
          if P.Name = FPath then
          begin
            FRttiProperty := P;
            Break;
          end;

        if FRttiProperty = nil then
        begin
          T := SharedContext.GetType(FInstance.ClassInfo);
          FRttiProperty := T.GetProperty(FPath);
          if FRttiProperty <> nil then
            Properties.Add(FRttiProperty);
        end;
        Result := FRttiProperty <> nil;
      end;
    end
    else
      Result := True;
  end;
end;

{*************************************************}
procedure TALCustomPropertyAnimation.ParentChanged;
begin
  inherited;
  FInstance := nil;
end;

{******************************************}
procedure TALCustomPropertyAnimation.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and FEnabled then
    Start;
end;

{*************************************************************************}
procedure TALCustomPropertyAnimation.SetPropertyName(const AValue: string);
begin
  if not SameText(AValue, PropertyName) then
    FInstance := nil;
  FPropertyName := AValue;
end;

{****************************************}
procedure TALCustomPropertyAnimation.Stop;
begin
  FInstance := nil;
end;

{***************************************************************}
constructor TALFloatPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fFloatAnimation := TALFloatAnimation.Create;
  fFloatAnimation.OnFirstFrame := DoFirstFrame;
  fFloatAnimation.OnProcess := DoProcess;
  fFloatAnimation.OnFinish := DoFinish;
  fFloatAnimation.OnCustomInterpolation := DoCustomInterpolation;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
  FOnCustomInterpolation := nil;
end;

{*******************************************}
destructor TALFloatPropertyAnimation.Destroy;
begin
  ALFreeAndNil(fFloatAnimation);
  inherited;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if (FInstance <> nil) and StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TALFloatPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, fFloatAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TALFloatPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{********************************************************************************}
function TALFloatPropertyAnimation.doCustomInterpolation(Sender: TObject): single;
begin
  if assigned(FOnCustomInterpolation) then
    result := FOnCustomInterpolation(self)
  else
    result := 0;
end;

{******************************************************************}
function TALFloatPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fFloatAnimation.AnimationType;
end;

{*********************************************************}
function TALFloatPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fFloatAnimation.AutoReverse;
end;

{**************************************************}
function TALFloatPropertyAnimation.getDelay: Single;
begin
  result := fFloatAnimation.Delay;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getDuration: Single;
begin
  result := fFloatAnimation.Duration;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getEnabled: Boolean;
begin
  result := FEnabled;
end;

{************************************************************************}
function TALFloatPropertyAnimation.getInterpolation: TALInterpolationType;
begin
  result := fFloatAnimation.Interpolation;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getInverse: Boolean;
begin
  result := fFloatAnimation.Inverse;
end;

{**************************************************}
function TALFloatPropertyAnimation.getLoop: Boolean;
begin
  result := fFloatAnimation.Loop;
end;

{********************************************************}
function TALFloatPropertyAnimation.getCurrentTime: Single;
begin
  result := fFloatAnimation.CurrentTime;
end;

{******************************************************}
function TALFloatPropertyAnimation.getOvershoot: Single;
begin
  result := fFloatAnimation.Overshoot;
end;

{***************************************************}
function TALFloatPropertyAnimation.getPause: Boolean;
begin
  result := fFloatAnimation.Pause;
end;

{*****************************************************}
function TALFloatPropertyAnimation.getRunning: Boolean;
begin
  result := fFloatAnimation.Running;
end;

{*******************************************************}
function TALFloatPropertyAnimation.GetStartValue: Single;
begin
  result := fFloatAnimation.StartValue;
end;

{******************************************************}
function TALFloatPropertyAnimation.GetStopValue: Single;
begin
  result := fFloatAnimation.StopValue;
end;

{*********************************************************}
function TALFloatPropertyAnimation.GetCurrentValue: Single;
begin
  result := fFloatAnimation.CurrentValue;
end;

{**********************************************************}
function TALFloatPropertyAnimation.OvershootStored: Boolean;
begin
  result := not sameValue(fFloatAnimation.Overshoot, 0);
end;

{********************************************************************************}
procedure TALFloatPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fFloatAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TALFloatPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fFloatAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.setDelay(const Value: Single);
begin
  fFloatAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.setDuration(const Value: Single);
begin
  fFloatAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if [csDesigning, csReading, csLoading] * ComponentState = [] then begin
    if (fFloatAnimation.Enabled <> Value) and
       ((not value) or (FindProperty)) then
      fFloatAnimation.Enabled := Value;
  end;
end;

{**************************************************************************************}
procedure TALFloatPropertyAnimation.setInterpolation(const Value: TALInterpolationType);
begin
  fFloatAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TALFloatPropertyAnimation.setInverse(const Value: Boolean);
begin
  fFloatAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TALFloatPropertyAnimation.setLoop(const Value: Boolean);
begin
  fFloatAnimation.Loop := Value;
end;

{********************************************************************}
procedure TALFloatPropertyAnimation.setOvershoot(const Value: Single);
begin
  fFloatAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TALFloatPropertyAnimation.setPause(const Value: Boolean);
begin
  fFloatAnimation.Pause := Value;
end;

{*********************************************************************}
procedure TALFloatPropertyAnimation.SetStartValue(const Value: Single);
begin
  fFloatAnimation.StartValue := Value;
end;

{********************************************************************}
procedure TALFloatPropertyAnimation.setStopValue(const Value: Single);
begin
  fFloatAnimation.StopValue := Value;
end;

{****************************************}
procedure TALFloatPropertyAnimation.Start;
begin
  if FindProperty then
    fFloatAnimation.Start;
end;

{***************************************}
procedure TALFloatPropertyAnimation.Stop;
begin
  fFloatAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TALFloatPropertyAnimation.StopAtCurrent;
begin
  fFloatAnimation.StopAtCurrent;
end;

{***************************************************************}
constructor TALColorPropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  fColorAnimation := TALColorAnimation.Create;
  fColorAnimation.OnFirstFrame := DoFirstFrame;
  fColorAnimation.OnProcess := DoProcess;
  fColorAnimation.OnFinish := DoFinish;
  fColorAnimation.OnCustomInterpolation := DoCustomInterpolation;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
  FOnCustomInterpolation := nil;
end;

{*******************************************}
destructor TALColorPropertyAnimation.Destroy;
begin
  ALFreeAndNil(fColorAnimation);
  inherited;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if (FInstance <> nil) and StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        StartValue := TAlphaColor(P.GetValue(FInstance).AsOrdinal);
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*************************************************************}
procedure TALColorPropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.IsOrdinal) then
        P.SetValue(FInstance, fColorAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{************************************************************}
procedure TALColorPropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{********************************************************************************}
function TALColorPropertyAnimation.doCustomInterpolation(Sender: TObject): Single;
begin
  if assigned(FOnCustomInterpolation) then
    result := FOnCustomInterpolation(self)
  else
    result := 0;
end;

{******************************************************************}
function TALColorPropertyAnimation.getAnimationType: TAnimationType;
begin
  result := fColorAnimation.AnimationType;
end;

{*********************************************************}
function TALColorPropertyAnimation.getAutoReverse: Boolean;
begin
  result := fColorAnimation.AutoReverse;
end;

{**************************************************}
function TALColorPropertyAnimation.getDelay: Single;
begin
  result := fColorAnimation.Delay;
end;

{*****************************************************}
function TALColorPropertyAnimation.getDuration: Single;
begin
  result := fColorAnimation.Duration;
end;

{*****************************************************}
function TALColorPropertyAnimation.getEnabled: Boolean;
begin
  result := FEnabled;
end;

{************************************************************************}
function TALColorPropertyAnimation.getInterpolation: TALInterpolationType;
begin
  result := fColorAnimation.Interpolation;
end;

{*****************************************************}
function TALColorPropertyAnimation.getInverse: Boolean;
begin
  result := fColorAnimation.Inverse;
end;

{**************************************************}
function TALColorPropertyAnimation.getLoop: Boolean;
begin
  result := fColorAnimation.Loop;
end;

{********************************************************}
function TALColorPropertyAnimation.getCurrentTime: Single;
begin
  result := fColorAnimation.CurrentTime;
end;

{******************************************************}
function TALColorPropertyAnimation.getOvershoot: Single;
begin
  result := fColorAnimation.Overshoot;
end;

{***************************************************}
function TALColorPropertyAnimation.getPause: Boolean;
begin
  result := fColorAnimation.Pause;
end;

{*****************************************************}
function TALColorPropertyAnimation.getRunning: Boolean;
begin
  result := fColorAnimation.Running;
end;

{************************************************************}
function TALColorPropertyAnimation.GetStartValue: TAlphaColor;
begin
  result := fColorAnimation.StartValue;
end;

{***********************************************************}
function TALColorPropertyAnimation.GetStopValue: TAlphaColor;
begin
  result := fColorAnimation.StopValue;
end;

{**************************************************************}
function TALColorPropertyAnimation.GetCurrentValue: TAlphaColor;
begin
  result := fColorAnimation.CurrentValue;
end;

{**********************************************************}
function TALColorPropertyAnimation.OvershootStored: Boolean;
begin
  result := not sameValue(fColorAnimation.Overshoot, 0);
end;

{********************************************************************************}
procedure TALColorPropertyAnimation.setAnimationType(const Value: TAnimationType);
begin
  fColorAnimation.AnimationType := Value;
end;

{***********************************************************************}
procedure TALColorPropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  fColorAnimation.AutoReverse := Value;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.setDelay(const Value: Single);
begin
  fColorAnimation.Delay := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.setDuration(const Value: Single);
begin
  fColorAnimation.Duration := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if [csDesigning, csReading, csLoading] * ComponentState = [] then begin
    if (fColorAnimation.Enabled <> Value) and
       ((not value) or (FindProperty)) then
      fColorAnimation.Enabled := Value;
  end;
end;

{**************************************************************************************}
procedure TALColorPropertyAnimation.setInterpolation(const Value: TALInterpolationType);
begin
  fColorAnimation.Interpolation := Value;
end;

{*******************************************************************}
procedure TALColorPropertyAnimation.setInverse(const Value: Boolean);
begin
  fColorAnimation.Inverse := Value;
end;

{****************************************************************}
procedure TALColorPropertyAnimation.setLoop(const Value: Boolean);
begin
  fColorAnimation.Loop := Value;
end;

{********************************************************************}
procedure TALColorPropertyAnimation.setOvershoot(const Value: Single);
begin
  fColorAnimation.Overshoot := Value;
end;

{*****************************************************************}
procedure TALColorPropertyAnimation.setPause(const Value: Boolean);
begin
  fColorAnimation.Pause := Value;
end;

{**************************************************************************}
procedure TALColorPropertyAnimation.SetStartValue(const Value: TAlphaColor);
begin
  fColorAnimation.StartValue := Value;
end;

{*************************************************************************}
procedure TALColorPropertyAnimation.setStopValue(const Value: TAlphaColor);
begin
  fColorAnimation.StopValue := Value;
end;

{****************************************}
procedure TALColorPropertyAnimation.Start;
begin
  if FindProperty then
    fColorAnimation.Start;
end;

{***************************************}
procedure TALColorPropertyAnimation.Stop;
begin
  fColorAnimation.Stop;
  inherited;
end;

{************************************************}
procedure TALColorPropertyAnimation.StopAtCurrent;
begin
  fColorAnimation.StopAtCurrent;
end;

{***************************************************}
// Creates a spring with a given final rest position.
// @param finalPosition final position of the spring when it reaches equilibrium
constructor TALSpringForce.Create;
begin
  FFinalPosition := 0.0;
  FNaturalFreq := sqrt(STIFFNESS_MEDIUM);
  FDampingRatio := DAMPING_RATIO_MEDIUM_BOUNCY;
  setValueThreshold(MIN_VISIBLE_CHANGE_PIXELS * THRESHOLD_MULTIPLIER);
  FInitialized := false;
end;

{*****************************************************************************************}
// Sets the stiffness of a spring. The more stiff a spring is, the more force it applies to
// the object attached when the spring is not at the final position. Default stiffness is
// STIFFNESS_MEDIUM.
// @param stiffness non-negative stiffness constant of a spring
// @return the spring force that the given stiffness is set on
// @throws IllegalArgumentException if the given spring stiffness is not positive
function TALSpringForce.setStiffness(stiffness: Double): TALSpringForce;
begin
  if (stiffness <= 0) then
    Raise Exception.create('Spring stiffness constant must be positive.');
  FNaturalFreq := sqrt(stiffness);
  // All the intermediate values need to be recalculated.
  FInitialized := false;
  Result := Self;
end;

{**********************************}
// Gets the stiffness of the spring.
// @return the stiffness of the spring
function TALSpringForce.getStiffness: Double;
begin
  Result := {(Single)} (FNaturalFreq * FNaturalFreq);
end;

{***************************************************************************************}
// Spring damping ratio describes how oscillations in a system decay after a disturbance.
// When damping ratio > 1 (over-damped), the object will quickly return to the rest position
// without overshooting. If damping ratio equals to 1 (i.e. critically damped), the object will
// return to equilibrium within the shortest amount of time. When damping ratio is less than 1
// (i.e. under-damped), the mass tends to overshoot, and return, and overshoot again. Without
// any damping (i.e. damping ratio := 0), the mass will oscillate forever.
// Default damping ratio is DAMPING_RATIO_MEDIUM_BOUNCY.
// @param dampingRatio damping ratio of the spring, it should be non-negative
// @return the spring force that the given damping ratio is set on
// @throws IllegalArgumentException if the param dampingRatio is negative.
function TALSpringForce.setDampingRatio(dampingRatio: Double): TALSpringForce;
begin
  if (dampingRatio < 0) then
    Raise Exception.create('Damping ratio must be non-negative');
  FDampingRatio := dampingRatio;
  // All the intermediate values need to be recalculated.
  FInitialized := false;
  Result := Self;
end;

{*****************************************}
// Returns the damping ratio of the spring.
// @return damping ratio of the spring
function TALSpringForce.getDampingRatio: Double;
begin
  Result := {(Single)} FDampingRatio;
end;

{**************************************}
// Sets the rest position of the spring.
// @param finalPosition rest position of the spring
// @return the spring force that the given final position is set on
function TALSpringForce.setFinalPosition(finalPosition: Double): TALSpringForce;
begin
  FFinalPosition := finalPosition;
  Result := Self;
end;

{*****************************************}
// Returns the rest position of the spring.
// @return rest position of the spring
function TALSpringForce.getFinalPosition: Double;
begin
  Result := {(Single)} FFinalPosition;
end;

{**********************************************************************************************}
function TALSpringForce.getAcceleration(lastDisplacement: Double; lastVelocity: Double): Double;
begin
  lastDisplacement := lastDisplacement - getFinalPosition;

  var k: double := FNaturalFreq * FNaturalFreq;
  var c: double := 2 * FNaturalFreq * FDampingRatio;

  Result := {(Single)} (-k * lastDisplacement - c * lastVelocity);
end;

{********************************************************************************}
function TALSpringForce.isAtEquilibrium(value: Double; velocity: Double): boolean;
begin
  if (Abs(velocity) < FVelocityThreshold) and
     (Abs(value - getFinalPosition) < FValueThreshold) then
    Result := true
  else
    Result := false;
end;

{******************************************************************************************}
// Initialize the string by doing the necessary pre-calculation as well as some sanity check
// on the setup.
// @throws IllegalStateException if the final position is not yet set by the time the spring
// animation has started
procedure TALSpringForce.init;
begin
  if (FInitialized) then
    exit;

  if (FDampingRatio > 1) then begin
    // Over damping
    FGammaPlus := -FDampingRatio * FNaturalFreq + FNaturalFreq * sqrt(FDampingRatio * FDampingRatio - 1);
    FGammaMinus := -FDampingRatio * FNaturalFreq - FNaturalFreq * sqrt(FDampingRatio * FDampingRatio - 1);
  end
  else if ((FDampingRatio >= 0) and (FDampingRatio < 1)) then begin
    // Under damping
    FDampedFreq := FNaturalFreq * sqrt(1 - FDampingRatio * FDampingRatio);
  end;

  FInitialized := true;
end;

{******************************************************************************}
// Internal only call for Spring to calculate the spring position/velocity using
// an analytical approach.
function TALSpringForce.updateValues(lastDisplacement: double; lastVelocity: double; timeElapsedMS: double): TMassState;
begin
  init;

  var deltaT: double := timeElapsedMS / 1000; // unit: seconds
  lastDisplacement := lastDisplacement - FFinalPosition;
  var displacement: double;
  var currentVelocity: double;
  if (FDampingRatio > 1) then begin
    // Overdamped
    var coeffA: double :=  lastDisplacement - (FGammaMinus * lastDisplacement - lastVelocity) / (FGammaMinus - FGammaPlus);
    var coeffB: double :=  (FGammaMinus * lastDisplacement - lastVelocity) / (FGammaMinus - FGammaPlus);
    displacement := coeffA * Exp(FGammaMinus * deltaT) + coeffB * Exp(FGammaPlus * deltaT);
    currentVelocity := coeffA * FGammaMinus * Exp(FGammaMinus * deltaT) + coeffB * FGammaPlus * Exp(FGammaPlus * deltaT);
  end
  else if (FDampingRatio = 1) then begin
    // Critically damped
    var coeffA: double := lastDisplacement;
    var coeffB: double := lastVelocity + FNaturalFreq * lastDisplacement;
    displacement := (coeffA + coeffB * deltaT) * Exp(-FNaturalFreq * deltaT);
    currentVelocity := (coeffA + coeffB * deltaT) * Exp(-FNaturalFreq * deltaT) * -FNaturalFreq + coeffB * Exp(-FNaturalFreq * deltaT);
  end
  else begin
    // Underdamped
    var cosCoeff: double := lastDisplacement;
    var sinCoeff: double := (1 / FDampedFreq) * (FDampingRatio * FNaturalFreq * lastDisplacement + lastVelocity);
    displacement := Exp(-FDampingRatio * FNaturalFreq * deltaT) * (cosCoeff * cos(FDampedFreq * deltaT) + sinCoeff * sin(FDampedFreq * deltaT));
    currentVelocity := displacement * -FNaturalFreq * FDampingRatio + Exp(-FDampingRatio * FNaturalFreq * deltaT) * (-FDampedFreq * cosCoeff * sin(FDampedFreq * deltaT) + FDampedFreq * sinCoeff * cos(FDampedFreq * deltaT));
  end;

  Result.Value := {(Single)} (displacement + FFinalPosition);
  Result.Velocity := {(Single)} currentVelocity;
end;

{******************************************************************************************}
// This threshold defines how close the animation value needs to be before the animation can
// finish. This default value is based on the property being animated, e.g. animations on alpha,
// scale, translation or rotation would have different thresholds. This value should be small
// enough to avoid visual glitch of "jumping to the end". But it shouldn't be so small that
// animations take seconds to finish.
// @param threshold the difference between the animation value and final spring position that
//                  is allowed to end the animation when velocity is very low
function TALSpringForce.setValueThreshold(threshold: double): TALSpringForce;
begin
  FValueThreshold := Abs(threshold);
  FVelocityThreshold := FValueThreshold * VELOCITY_THRESHOLD_MULTIPLIER;
  result := Self;
end;

{************************************************}
function TALSpringForce.getValueThreshold: Double;
begin
  result := FValueThreshold;
end;

{*****************************************}
constructor TALSpringForceAnimation.Create;
begin
  inherited Create;
  FSpringForce := TALSpringForce.Create;
  FInitialVelocity := 0.0;
  FCurrentVelocity := 0.0;
  FStartValue := 0.0;
  fCurrentValue := 0.0;
  FDeltaTime := 0.0;
end;

{*****************************************}
destructor TALSpringForceAnimation.Destroy;
begin
  ALFreeAndNil(FSpringForce);
  inherited Destroy;
end;

{*************************************************}
procedure TALSpringForceAnimation.ProcessAnimation;
begin
  var LMassState := FSpringForce.updateValues(FCurrentValue, FCurrentVelocity, FDeltaTime * 1000);
  FCurrentVelocity := LMassState.Velocity;
  FCurrentValue := LMassState.Value;
end;

{*****************************************************************************}
procedure TALSpringForceAnimation.ProcessTick(const ATime, ADeltaTime: Double);
begin
  if (not FRunning) or FPause then
    Exit;

  var LContinueRunning := FRunning;

  if (FDelay > 0) and (FDelayTimeLeft <> 0) then begin
    FDelayTimeLeft := FDelayTimeLeft - ADeltaTime;
    if FDelayTimeLeft <= 0 then begin
      FDelayTimeLeft := 0;
      FirstFrame;
      ProcessAnimation;
      DoProcess;
    end;
    Exit;
  end;

  FTime := FTime + ADeltaTime;
  FDeltaTime := ADeltaTime;

  ProcessAnimation;

  if FSpringForce.isAtEquilibrium(FCurrentValue,FCurrentVelocity) then begin
    if FLoop then begin
      if FAutoReverse then begin
        FInverse := not FInverse;
        var LSavedStartValue := FStartValue;
        FStartValue := GetStopValue;
        SetStopValue(LSavedStartValue);
      end;
      FTime := 0;
      FCurrentValue := FStartValue;
      FCurrentVelocity := FInitialVelocity;
    end
    else begin
      if FAutoReverse and (not FDidAutoReverse) then begin
        FDidAutoReverse := true;
        FInverse := not FInverse;
        var LSavedStartValue := FStartValue;
        FStartValue := GetStopValue;
        SetStopValue(LSavedStartValue);
        FTime := 0;
        FCurrentValue := FStartValue;
        FCurrentVelocity := FInitialVelocity;
      end
      else LContinueRunning := False;
    end;
  end;

  DoProcess;

  FRunning := FRunning and LContinueRunning;
  if not FRunning then begin
    if FInverse then begin
      var LSavedStartValue := FStartValue;
      FStartValue := GetStopValue;
      SetStopValue(LSavedStartValue);
    end;
    FInverse := FSavedInverse;
    if AniThread <> nil then
      AniThread.RemoveAnimation(Self);
    DoFinish;
  end;
end;

{**************************************}
procedure TALSpringForceAnimation.Start;
begin
  if (FRunning) then
    Exit;
  FEnabled := True;
  FRunning := True;
  FDidAutoReverse := False;
  FSavedInverse := FInverse;
  if FInverse then begin
    var LSavedStartValue := FStartValue;
    FStartValue := GetStopValue;
    SetStopValue(LSavedStartValue);
  end;
  FTime := 0;
  FCurrentValue := FStartValue;
  FCurrentVelocity := FInitialVelocity;
  FDelayTimeLeft := FDelay;
  if FDelay = 0 then begin
    FirstFrame;
    FDeltaTime := 0;
    ProcessAnimation;
    DoProcess;
  end;

  if AniThread = nil then
    FAniThread := TALAniThread.Create;

  AniThread.AddAnimation(Self);
  if not AniThread.Enabled then
    Stop;
end;

{*************************************}
procedure TALSpringForceAnimation.Stop;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    AniThread.RemoveAnimation(Self);

  if FInverse then begin
    var LSavedStartValue := FStartValue;
    FStartValue := GetStopValue;
    SetStopValue(LSavedStartValue);
  end;
  FInverse := FSavedInverse;
  FCurrentValue := FStartValue;
  FCurrentVelocity := 0;
  FDeltaTime := 0;
  ProcessAnimation;
  DoProcess;
  FRunning := False;
  DoFinish;
end;

{**********************************************}
procedure TALSpringForceAnimation.StopAtCurrent;
begin
  if not FRunning then
    Exit;

  if AniThread <> nil then
    AniThread.RemoveAnimation(Self);

  if FInverse then begin
    var LSavedStartValue := FStartValue;
    FStartValue := GetStopValue;
    SetStopValue(LSavedStartValue);
  end;
  FInverse := FSavedInverse;
  FCurrentVelocity := 0;
  FRunning := False;
  DoFinish;
end;

{****************************************************************}
procedure TALSpringForceAnimation.setStiffness(stiffness: Single);
begin
  FSpringForce.setStiffness(stiffness);
end;

{****************************************************}
function TALSpringForceAnimation.getStiffness: Single;
begin
  result := FSpringForce.getStiffness;
end;

{**********************************************************************}
procedure TALSpringForceAnimation.setDampingRatio(dampingRatio: Single);
begin
  FSpringForce.setDampingRatio(dampingRatio);
end;

{*******************************************************}
function TALSpringForceAnimation.getDampingRatio: Single;
begin
  result := FSpringForce.getDampingRatio;
end;

{******************************************************************}
procedure TALSpringForceAnimation.SetStopValue(const Value: Single);
begin
  FSpringForce.setFinalPosition(Value);
end;

{****************************************************}
function TALSpringForceAnimation.GetStopValue: Single;
begin
  result := FSpringForce.getFinalPosition;
end;

{***********************************************************************}
procedure TALSpringForceAnimation.SetValueThreshold(const Value: Single);
begin
  FSpringForce.SetValueThreshold(Value);
end;

{*********************************************************}
function TALSpringForceAnimation.GetValueThreshold: Single;
begin
  result := FSpringForce.GetValueThreshold;
end;

{*********************************************************************}
constructor TALSpringForcePropertyAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FStartFromCurrent := False;
  FSpringForceAnimation := TALSpringForceAnimation.Create;
  FSpringForceAnimation.OnFirstFrame := DoFirstFrame;
  FSpringForceAnimation.OnProcess := DoProcess;
  FSpringForceAnimation.OnFinish := DoFinish;
  FOnFirstFrame := nil;
  FOnProcess := nil;
  FOnFinish := nil;
  FOnCustomInterpolation := nil;
end;

{*************************************************}
destructor TALSpringForcePropertyAnimation.Destroy;
begin
  ALFreeAndNil(FSpringForceAnimation);
  inherited;
end;

{**********************************************************************}
procedure TALSpringForcePropertyAnimation.doFirstFrame(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if (FInstance <> nil) and StartFromCurrent then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        StartValue := P.GetValue(FInstance).AsExtended;
    end;
  end;
  if assigned(fOnFirstFrame) then
    fOnFirstFrame(self);
end;

{*******************************************************************}
procedure TALSpringForcePropertyAnimation.doProcess(Sender: TObject);
var
  T: TRttiType;
  P: TRttiProperty;
begin
  if FInstance <> nil then
  begin
    T := SharedContext.GetType(FInstance.ClassInfo);
    if T <> nil then
    begin
      P := T.GetProperty(FPath);
      if (P <> nil) and (P.PropertyType.TypeKind = tkFloat) then
        P.SetValue(FInstance, FSpringForceAnimation.CurrentValue);
    end;
  end;
  if assigned(fOnProcess) then
    fOnProcess(self);
end;

{******************************************************************}
procedure TALSpringForcePropertyAnimation.doFinish(Sender: TObject);
begin
  if assigned(fOnFinish) then
    fOnFinish(self);
end;

{***************************************************************}
function TALSpringForcePropertyAnimation.getAutoReverse: Boolean;
begin
  result := FSpringForceAnimation.AutoReverse;
end;

{********************************************************}
function TALSpringForcePropertyAnimation.getDelay: Single;
begin
  result := FSpringForceAnimation.Delay;
end;

{***********************************************************}
function TALSpringForcePropertyAnimation.getEnabled: Boolean;
begin
  result := FEnabled;
end;

{***********************************************************}
function TALSpringForcePropertyAnimation.getInverse: Boolean;
begin
  result := FSpringForceAnimation.Inverse;
end;

{********************************************************}
function TALSpringForcePropertyAnimation.getLoop: Boolean;
begin
  result := FSpringForceAnimation.Loop;
end;

{**************************************************************}
function TALSpringForcePropertyAnimation.getCurrentTime: Single;
begin
  result := FSpringForceAnimation.CurrentTime;
end;

{*********************************************************}
function TALSpringForcePropertyAnimation.getPause: Boolean;
begin
  result := FSpringForceAnimation.Pause;
end;

{***********************************************************}
function TALSpringForcePropertyAnimation.getRunning: Boolean;
begin
  result := FSpringForceAnimation.Running;
end;

{*************************************************************}
function TALSpringForcePropertyAnimation.GetStartValue: Single;
begin
  result := FSpringForceAnimation.StartValue;
end;

{************************************************************}
function TALSpringForcePropertyAnimation.GetStopValue: Single;
begin
  result := FSpringForceAnimation.StopValue;
end;

{***************************************************************}
function TALSpringForcePropertyAnimation.GetCurrentValue: Single;
begin
  result := FSpringForceAnimation.CurrentValue;
end;

{******************************************************************}
function TALSpringForcePropertyAnimation.GetCurrentVelocity: Single;
begin
  result := FSpringForceAnimation.CurrentVelocity;
end;

{************************************************************}
function TALSpringForcePropertyAnimation.getStiffness: Single;
begin
  result := FSpringForceAnimation.Stiffness;
end;

{***************************************************************}
function TALSpringForcePropertyAnimation.getDampingRatio: Single;
begin
  result := FSpringForceAnimation.DampingRatio;
end;

{******************************************************************}
function TALSpringForcePropertyAnimation.GetInitialVelocity: Single;
begin
  result := FSpringForceAnimation.InitialVelocity;
end;

{*****************************************************************}
function TALSpringForcePropertyAnimation.GetValueThreshold: Single;
begin
  result := FSpringForceAnimation.ValueThreshold;
end;

{*****************************************************************************}
procedure TALSpringForcePropertyAnimation.setAutoReverse(const Value: Boolean);
begin
  FSpringForceAnimation.AutoReverse := Value;
end;

{**********************************************************************}
procedure TALSpringForcePropertyAnimation.setDelay(const Value: Single);
begin
  FSpringForceAnimation.Delay := Value;
end;

{*************************************************************************}
procedure TALSpringForcePropertyAnimation.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if [csDesigning, csReading, csLoading] * ComponentState = [] then begin
    if (FSpringForceAnimation.Enabled <> Value) and
       ((not value) or (FindProperty)) then
      FSpringForceAnimation.Enabled := Value;
  end;
end;

{*************************************************************************}
procedure TALSpringForcePropertyAnimation.setInverse(const Value: Boolean);
begin
  FSpringForceAnimation.Inverse := Value;
end;

{**********************************************************************}
procedure TALSpringForcePropertyAnimation.setLoop(const Value: Boolean);
begin
  FSpringForceAnimation.Loop := Value;
end;

{***********************************************************************}
procedure TALSpringForcePropertyAnimation.setPause(const Value: Boolean);
begin
  FSpringForceAnimation.Pause := Value;
end;

{***************************************************************************}
procedure TALSpringForcePropertyAnimation.SetStartValue(const Value: Single);
begin
  FSpringForceAnimation.StartValue := Value;
end;

{**************************************************************************}
procedure TALSpringForcePropertyAnimation.setStopValue(const Value: Single);
begin
  FSpringForceAnimation.StopValue := Value;
end;

{**************************************************************************}
procedure TALSpringForcePropertyAnimation.setStiffness(const Value: Single);
begin
  FSpringForceAnimation.Stiffness := Value;
end;

{*****************************************************************************}
procedure TALSpringForcePropertyAnimation.setDampingRatio(const Value: Single);
begin
  FSpringForceAnimation.DampingRatio := Value;
end;

{********************************************************************************}
procedure TALSpringForcePropertyAnimation.SetInitialVelocity(const Value: Single);
begin
  FSpringForceAnimation.InitialVelocity := Value;
end;

{*******************************************************************************}
procedure TALSpringForcePropertyAnimation.SetValueThreshold(const Value: Single);
begin
  FSpringForceAnimation.ValueThreshold := Value;
end;

{****************************************************************}
function TALSpringForcePropertyAnimation.StiffnessStored: Boolean;
begin
  result := not SameValue(FSpringForceAnimation.Stiffness, TALSpringForce.STIFFNESS_MEDIUM);
end;

{*******************************************************************}
function TALSpringForcePropertyAnimation.DampingRatioStored: Boolean;
begin
  result := not SameValue(FSpringForceAnimation.DampingRatio, TALSpringForce.DAMPING_RATIO_MEDIUM_BOUNCY);
end;

{*********************************************************************}
function TALSpringForcePropertyAnimation.ValueThresholdStored: Boolean;
begin
  result := not SameValue(FSpringForceAnimation.ValueThreshold, TALSpringForce.MIN_VISIBLE_CHANGE_PIXELS * TALSpringForce.THRESHOLD_MULTIPLIER);
end;

{**********************************************************************}
function TALSpringForcePropertyAnimation.InitialVelocityStored: Boolean;
begin
  result := not SameValue(FSpringForceAnimation.InitialVelocity, 0.0);
end;

{**********************************************}
procedure TALSpringForcePropertyAnimation.Start;
begin
  if FindProperty then
    FSpringForceAnimation.Start;
end;

{*********************************************}
procedure TALSpringForcePropertyAnimation.Stop;
begin
  FSpringForceAnimation.Stop;
  inherited;
end;

{******************************************************}
procedure TALSpringForcePropertyAnimation.StopAtCurrent;
begin
  FSpringForceAnimation.StopAtCurrent;
end;

{********************************************************}
constructor TALCubicBezier.Create(X1, Y1, X2, Y2: Double);
begin
  P1 := TALPointD.Create(X1, Y1);
  P2 := TALPointD.Create(X2, Y2);
  InitCoefficients(P1, P2);
  InitSpline;
end;

{*****************************************************************}
procedure TALCubicBezier.InitCoefficients(const P1, P2: TALPointD);
begin
  // Calculate the polynomial coefficients, implicit first and last control
  // points are (0,0) and (1,1). First, for x.
  c.x := 3 * p1.x;
  b.x := 3 * (p2.x - p1.x) - c.x;
  a.x := 1 - c.x - b.x;

  // Now for y.
  c.y := toFinite(3 * p1.y);
  b.y := toFinite(3 * (p2.y - p1.y) - c.y);
  a.y := toFinite(1 - c.y - b.y);
end;

{**********************************}
procedure TALCubicBezier.InitSpline;
begin
  const deltaT = 1 / (CUBIC_BEZIER_SPLINE_SAMPLES - 1);
  for var i := 0 to CUBIC_BEZIER_SPLINE_SAMPLES - 1 do
    splineSamples[i] := sampleCurveX(i * deltaT);
end;

{******************************************************}
function TALCubicBezier.SampleCurveX(T: Double): Double;
begin
  // `ax t^3 + bx t^2 + cx t' expanded using Horner's rule.
  // The x values are in the range [0, 1]. So it isn't needed toFinite
  // clamping.
  // https://drafts.csswg.org/css-easing-1/#funcdef-cubic-bezier-easing-function-cubic-bezier
  result := ((a.x * t + b.x) * t + c.x) * t;
end;

{****************************************************************}
function TALCubicBezier.SampleCurveDerivativeX(T: Double): Double;
begin
  Result := (3 * a.x * t + 2 * b.x) * t + c.x;
end;

{******************************************************}
function TALCubicBezier.SampleCurveY(T: Double): Double;
begin
  Result := toFinite(((a.y * t + b.y) * t + c.y) * t);
end;

{**************************************************}
function TALCubicBezier.ToFinite(N: Double): Double;
begin
  if IsInfinite(N) then
    Result := IfThen(N > 0, MaxInt, -MaxInt)
  else
    Result := N;
end;

{*****************************************************}
function TALCubicBezier.SolveCurveX(X: Double): Double;
begin
  var t0: Double := NAN;
  var t1: Double := NAN;
  var x2: Double{ := NAN};
  var d2: Double;

  var t2: Double := x;

  // Linear interpolation of spline curve for initial guess.
  const deltaT = 1 / (CUBIC_BEZIER_SPLINE_SAMPLES - 1);
  for var i := 1 to CUBIC_BEZIER_SPLINE_SAMPLES - 1 do begin
    if (x <= splineSamples[i]) then begin
      t1 := deltaT * i;
      t0 := t1 - deltaT;
      t2 := t0 + (t1 - t0) * (x - splineSamples[i - 1]) / (splineSamples[i] - splineSamples[i - 1]);
      break;
    end;
  end;

  // Perform a few iterations of Newton's method -- normally very fast.
  // See https://en.wikipedia.org/wiki/Newton%27s_method.
  for var i := 0 to MAX_NEWTON_METHOD_ITERATIONS - 1 do begin
    x2 := sampleCurveX(t2) - x;
    if (abs(x2) < BEZIER_EPSILON) then
      exit(t2);
    d2 := sampleCurveDerivativeX(t2);
    if (abs(d2) < BEZIER_EPSILON) then
      break;
    t2 := t2 - x2 / d2;
  end;
  if ((not IsNan(x2)) and (abs(x2) < BEZIER_EPSILON)) then
    exit(t2);

  // Fall back to the bisection method for reliability.
  if ((not IsNan(t0)) and (not IsNan(t1))) then begin
    while (t0 < t1) do begin
      x2 := sampleCurveX(t2);
      if (abs(x2 - x) < BEZIER_EPSILON) then
        exit(t2);

      if (x > x2) then
        t0 := t2
      else
        t1 := t2;

      t2 := (t1 + t0) * 0.5;
    end;
  end;

  // Failed to solve.
  result := t2;
end;

{***************************************************}
function TALCubicBezier.SolveForY(X: Double): Double;
begin
  x := max(0, min(1, x));
  Result := sampleCurveY(solveCurveX(x));
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALFloatPropertyAnimation]);
  RegisterComponents('Alcinoe', [TALColorPropertyAnimation]);
  RegisterComponents('Alcinoe', [TALSpringForcePropertyAnimation]);
end;


{************}
initialization
  RegisterFmxClasses([TALFloatPropertyAnimation]);
  RegisterFmxClasses([TALColorPropertyAnimation]);
  RegisterFmxClasses([TALSpringForcePropertyAnimation]);
  TALAnimation.AniFrameRate := TALAnimation.DefaultAniFrameRate;
  ALInitViscousFluid;

{**********}
finalization
  TALAnimation.Uninitialize;

end.
