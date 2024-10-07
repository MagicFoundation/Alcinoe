{*******************************************************************************
TALOverScroller and TALVelocityTracker are essential components of the
TALScrollEngine, playing crucial roles in refining user interface interactions.
While TALOverScroller is designed to animate and manage actions like scrolling
and flinging, offering a decelerating animation as users scroll past a view's
edge to indicate the boundary, TALVelocityTracker measures the velocity of
touch events, helping to determine the speed and direction of user gestures.
Together within the TALScrollEngine, they elevate the overall user experience
by offering smooth animations and intuitive touch feedback.

Building a scrolling engine is a complex endeavor, requiring expertise in
various domains, including physics. Rather than starting from scratch and
potentially reinventing the wheel, we looked towards proven solutions. To this
end, we utilized the robust and well-tested code from Android's VelocityTracker
and OverScroller. By translating their Java and C++ codes into Delphi, we've
ensured that the resultant TALScrollEngine not only meets but exceeds the
standard for scrolling dynamics. Leveraging the reliability and efficiency of
Android's scrolling mechanisms, the TALScrollEngine offers Delphi developers a
top-notch scrolling experience, rooted in established and trusted technologies.

DEMO :
------

https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFmxControls
*******************************************************************************}
unit Alcinoe.FMX.ScrollEngine;

interface

{$I Alcinoe.inc}

uses
  System.Types,
  System.UITypes,
  System.Classes,
  System.Messaging,
  {$IFDEF IOS}
  System.TypInfo,
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

type

  {****************************************}
  TALScrollCapturedMessage = class(TMessage)
  private
    FCaptured: boolean;
  public
    constructor Create(const ACaptured: boolean);
    property Captured: boolean read FCaptured;
  end;

  {*********************************}
  TALVelocityTrackerStrategy = class;

  {********************************************************}
  // Calculates the velocity of pointer movements over time.
  // Taken from Android VelocityTracker
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if android VelocityTracker was not updated and adjust the IFDEF'}
    //Compare <Alcinoe>\References\Android\VelocityTracker.java with <SDKs>\android\sources\android-33\android\view\VelocityTracker.java
    //Compare <Alcinoe>\References\Android\VelocityTracker.cpp with https://android.googlesource.com/platform/frameworks/native/libs/input/VelocityTracker.cpp
    //Compare <Alcinoe>\References\Android\VelocityTracker.h with https://android.googlesource.com/platform/frameworks/native/libs/input/VelocityTracker.h
    //Compare <Alcinoe>\References\Android\android_view_VelocityTracker.cpp with https://android.googlesource.com/platform/frameworks/base/core/jni/android_view_VelocityTracker.cpp
  {$ENDIF}
  TALVelocityTracker = class(TObject)
  public
    type
      TStrategy = (
        // Velocity Tracker Strategy: Invalid.
        DEFAULT = -1,
        // Velocity Tracker Strategy: Impulse.
        // Physical model of pushing an object.  Quality: VERY GOOD.
        // Works with duplicate coordinates, unclean finger liftoff.
        IMPULSE = 0,
        // Velocity Tracker Strategy: LSQ1.
        // 1st order least squares.  Quality: POOR.
        // Frequently underfits the touch data especially when the finger accelerates
        // or changes direction.  Often underestimates velocity.  The direction
        // is overly influenced by historical touch points.
        LSQ1 = 1,
        // Velocity Tracker Strategy: LSQ2.
        // 2nd order least squares.  Quality: VERY GOOD.
        // Pretty much ideal, but can be confused by certain kinds of touch data,
        // particularly if the panel has a tendency to generate delayed,
        // duplicate or jittery touch coordinates when the finger is released.
        LSQ2 = 2,
        // Velocity Tracker Strategy: LSQ3.
        // 3rd order least squares.  Quality: UNUSABLE.
        // Frequently overfits the touch data yielding wildly divergent estimates
        // of the velocity when the finger is released.
        LSQ3 = 3,
        // Velocity Tracker Strategy: WLSQ2_DELTA.
        // 2nd order weighted least squares, delta weighting.  Quality: EXPERIMENTAL
        WLSQ2_DELTA = 4,
        // Velocity Tracker Strategy: WLSQ2_CENTRAL.
        // 2nd order weighted least squares, central weighting.  Quality: EXPERIMENTAL
        WLSQ2_CENTRAL = 5,
        // Velocity Tracker Strategy: WLSQ2_RECENT.
        // 2nd order weighted least squares, recent weighting.  Quality: EXPERIMENTAL
        WLSQ2_RECENT = 6,
        // Velocity Tracker Strategy: INT1.
        // 1st order integrating filter.  Quality: GOOD.
        // Not as good as 'lsq2' because it cannot estimate acceleration but it is
        // more tolerant of errors.  Like 'lsq1', this strategy tends to underestimate
        // the velocity of a fling but this strategy tends to respond to changes in
        // direction more quickly and accurately.
        INT1 = 7,
        // Velocity Tracker Strategy: INT2.
        // 2nd order integrating filter.  Quality: EXPERIMENTAL.
        // For comparison purposes only.  Unlike 'int1' this strategy can compensate
        // for acceleration but it typically overestimates the effect.
        INT2 = 8,
        // Velocity Tracker Strategy: Legacy.
        // Legacy velocity tracker algorithm.  Quality: POOR.
        // For comparison purposes only.  This algorithm is strongly influenced by
        // old data points, consistently underestimates velocity and takes a very long
        // time to adjust to changes in direction.
        LEGACY = 9);
  public
    type
      // An estimator for the movements of a pointer based on a polynomial model.
      // The last recorded position of the pointer is at time zero seconds.
      // Past estimated positions are at negative times and future estimated positions
      // are at positive times.
      // First coefficient is position (in pixels), second is velocity (in pixels per second),
      // third is acceleration (in pixels per second squared).
      TEstimator = record
      public
        const MAX_DEGREE = 4;
      public
        type
          TCoeffArray = array[0..MAX_DEGREE] of Single;
      private
        function estimate(time: Single; const c: TCoeffArray): Single;
      public
        // Estimator time base.
        time: Int64;
        // Polynomial coefficients describing motion in X and Y.
        xCoeff: TCoeffArray;
        yCoeff: TCoeffArray;
        // Polynomial degree (number of coefficients), or zero if no information is available.
        degree: Integer{UInt32};
        // Confidence (coefficient of determination), between 0 (no fit) and 1 (perfect fit).
        confidence: Single;
        procedure clear;
        function estimateX(time: Single): Single;
        function estimateY(time: Single): Single;
        function getXCoeff(index: Integer{UInt32}): Single;
        function getYCoeff(index: Integer{UInt32}): Single;
      end;
  private
    // The default velocity tracker strategy.
    // Although other strategies are available for testing and comparison purposes,
    // this is the strategy that applications will actually use.  Be very careful
    // when adjusting the default strategy because it can dramatically affect
    // (often in a bad way) the user experience.
    const DEFAULT_STRATEGY: TStrategy = TStrategy.LSQ2;
  private
    FLastEventTime: Int64;
    FStrategy: TALVelocityTrackerStrategy;
    function configureStrategy(const strategy: TStrategy): boolean;
    function createStrategy(const strategy: TStrategy): TALVelocityTrackerStrategy;
  public
    constructor Create(const strategy: TStrategy = TStrategy.DEFAULT); virtual;
    destructor Destroy; override;
    procedure clear;
    procedure addMovement(eventTime: Int64; const position: TPointF);
    function getVelocity(out outVx: Single; out outVy: Single): Boolean;
    function getEstimator(out outEstimator: TEstimator): Boolean;
  end;

  {****************************************************}
  // Implements a particular velocity tracker algorithm.
  // Taken from Android VelocityTracker
  TALVelocityTrackerStrategy = class(TObject)
  public
    procedure clear; virtual; abstract;
    procedure addMovement(eventTime: Int64; const position: TPointF); virtual; abstract;
    function getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean; virtual; abstract;
  end;

  {*********************************************************************}
  // Velocity tracker algorithm based on least-squares linear regression.
  // Taken from Android VelocityTracker
  TALLeastSquaresVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  public
    type
      TWeighting = (
        // No weights applied.  All data points are equally reliable.
        WEIGHTING_NONE,
        // Weight by time delta.  Data points clustered together are weighted less.
        WEIGHTING_DELTA,
        // Weight such that points within a certain horizon are weighed more than those
        // outside of that horizon.
        WEIGHTING_CENTRAL,
        // Weight such that points older than a certain amount are weighed less.
        WEIGHTING_RECENT);
  private
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    const HORIZON = 100 * 1000000; // 100 ms
    // Number of samples to keep.
    const HISTORY_SIZE = 20;
  private
    type
      TMovement = record
        eventTime: Int64;
        Position: TPointF;
      end;
  private
    FDegree: Integer{UInt32};
    FWeighting: TWeighting;
    FIndex: Integer{UInt32};
    FMovements: array[0..HISTORY_SIZE-1] of TMovement;
    function chooseWeight(index: Integer{UInt32}): Single;
    function vectorDot(a: system.PSingle; b: system.PSingle; m: Integer{UInt32}): Single;
    function vectorNorm(a: system.PSingle; m: Integer{UInt32}): Single;
    function solveLeastSquares(const x: TArray<Single>; const y: TArray<Single>; const w: TArray<Single>; n: Integer{UInt32}; out outB: TALVelocityTracker.TEstimator.TCoeffArray; out outDet: Single): boolean;
    function solveUnweightedLeastSquaresDeg2(const x: TArray<Single>; const y: TArray<Single>): TArray<Single>;
  public
    constructor Create(degree: Integer{UInt32}; const weighting: TWeighting = TWeighting.WEIGHTING_NONE); virtual;
    procedure clear; override;
    procedure addMovement(eventTime: Int64; const position: TPointF); override;
    function getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean; override;
  end;

  {****************************************************}
  // Velocity tracker algorithm that uses an IIR filter.
  // Taken from Android VelocityTracker
  TALIntegratingVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  private
    type
      // Current state estimate for a particular pointer.
      TState = record
        updateTime: Int64;
        degree: Integer{UInt32};
        xpos, xvel, xaccel: Single;
        ypos, yvel, yaccel: Single;
      end;
  private
    FDegree: Integer{UInt32};
    FState: TState;
    procedure initState(var state: TState; eventTime: Int64; xpos: Single; ypos: Single);
    procedure updateState(var state: TState; eventTime: Int64; xpos: Single; ypos: Single);
    procedure populateEstimator(const state: TState; out outEstimator: TALVelocityTracker.TEstimator);
  public
    constructor Create(degree: Integer{UInt32}); virtual;
    procedure clear; override;
    procedure addMovement(eventTime: Int64; const position: TPointF); override;
    function getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean; override;
  end;

  {*********************************************}
  // Velocity tracker strategy used prior to ICS.
  // Taken from Android VelocityTracker
  TALLegacyVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  private
    type
      TMovement = record
        eventTime: Int64;
        position: Tpointf;
      end;
  private
    // Oldest sample to consider when calculating the velocity.
    const HORIZON = 200 * 1000000; // 100 ms
    // Number of samples to keep.
    const HISTORY_SIZE = 20;
    // The minimum duration between samples when estimating velocity.
    const MIN_DURATION = 10 * 1000000; // 10 ms
  private
    FIndex: Integer{UInt32};
    FMovements: array[0..HISTORY_SIZE-1] of TMovement;
  public
    constructor Create; virtual;
    procedure clear; override;
    procedure addMovement(eventTime: Int64; const position: TPointF); override;
    function getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean; override;
  end;

  {***********************************}
  // Taken from Android VelocityTracker
  TALImpulseVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  private
    type
      TMovement = record
        eventTime: Int64;
        position: Tpointf;
      end;
  private
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    const HORIZON = 100 * 1000000; // 100 ms
    // Number of samples to keep.
    const HISTORY_SIZE = 20;
  private
    FIndex: Integer{NativeUInt};
    FMovements: Array[0..HISTORY_SIZE-1] of TMovement;
    function kineticEnergyToVelocity(work: Single): Single;
    function calculateImpulseVelocity(const t: TArray<Int64>; const x: TArray<Single>; count: Integer{NativeUInt}): Single;
  public
    constructor Create; virtual;
    procedure clear; override;
    procedure addMovement(eventTime: Int64; const position: TPointF); override;
    function getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean; override;
  end;

  {********************************}
  // Taken from Android OverScroller
  TALSplineOverScroller = class(TObject)
  public
    class var PixelsPerInch: Integer;
    class procedure InitPixelsPerInch;
  private
    // Initial position
    FStart: integer;
    // Current position
    FCurrentPosition: integer;
    // Final position
    FFinal: integer;
    // Initial velocity
    FVelocity: integer;
    // Current velocity
    FCurrVelocity: Single;
    // Constant current deceleration
    FDeceleration: Single;
    // Animation starting time, in system milliseconds
    FStartTime: int64;
    // Animation duration, in milliseconds
    FDuration: integer;
    // Duration to complete spline component of animation
    FSplineDuration: integer;
    // Distance to travel along spline animation
    FSplineDistance: integer;
    // Whether the animation is currently in progress
    FFinished: Boolean;
    // The allowed overshot distance before boundary is reached.
    FOver: integer;
    // Fling friction
    FFlingFriction: Single;
    // Current state of the animation.
    FState: integer;
  private
    // Constant gravity value, used in the deceleration phase.
    const GRAVITY = 2000.0;
  private
    // A context-specific coefficient adjusted to physical values.
    FPhysicalCoeff: Single;
  private
    class var DECELERATION_RATE: Single;
  private
    const INFLEXION = 0.35; // Tension lines cross at (INFLEXION, 1)
    const START_TENSION = 0.5;
    const END_TENSION = 1.0;
    const P1 = START_TENSION * INFLEXION;
    const P2 = 1.0 - END_TENSION * (1.0 - INFLEXION);
    const NB_SAMPLES = 100;
  private
    class var SPLINE_POSITION: array[0..NB_SAMPLES] of Single;
    class var SPLINE_TIME: array[0..NB_SAMPLES] of Single;
    class procedure InitSpline;
  private
    const SPLINE = 0;
    const CUBIC = 1;
    const BALLISTIC = 2;
  private
    class function getDeceleration(velocity: integer): Single;
  private
    procedure adjustDuration(start: integer; oldFinal: integer; newFinal: integer);
    procedure startSpringback(start: integer; &end: integer; velocity: integer);
    function getSplineDeceleration(velocity: integer): double;
    function getSplineFlingDistance(velocity: integer): double;
    function getSplineFlingDuration(velocity: integer): integer;
    procedure fitOnBounceCurve(start: integer; &end: integer; velocity: integer);
    procedure startBounceAfterEdge(start: integer; &end: integer; velocity: integer);
    procedure startAfterEdge(start: integer; min: integer; max: integer; velocity: integer);
    procedure onEdgeReached;
  public
    constructor Create; virtual;
    procedure setFriction(friction: Single);
    procedure updateScroll(q: Single);
    procedure startScroll(start: integer; distance: integer; duration: integer);
    procedure finish;
    procedure setStartPosition(position: integer);
    procedure setFinalPosition(position: integer);
    procedure extendDuration(extend: integer);
    function springback(start: integer; min: integer; max: integer): Boolean;
    procedure fling(start: integer; velocity: integer; min: integer; max: integer; over: integer);
    procedure notifyEdgeReached(start: integer; &end: integer; over: integer);
    function continueWhenFinished: Boolean;
    function update: Boolean;
  end;

  {*******************************************}
  // This class encapsulates scrolling with the
  // ability to overshoot the bounds of a scrolling operation.
  // Taken from Android OverScroller
  // https://www.sobyte.net/post/2022-02/android-over-scroller/
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if android OverScroller was not updated and adjust the IFDEF'}
    //Compare <Alcinoe>\References\Android\OverScroller.java with <SDKs>\android\sources\android-33\android\widget\OverScroller.java
  {$ENDIF}
  TALOverScroller = class(Tobject)
  private
    type
      TInterpolateFunc = function(input: Single): Single;
  private
    FMode: integer;
    FScrollerX: TALSplineOverScroller;
    FScrollerY: TALSplineOverScroller;
    FInterpolateFunc: TInterpolateFunc;
    FFlywheel: Boolean;
  private
    const DEFAULT_DURATION = 250;
    const SCROLL_MODE = 0;
    const FLING_MODE = 1;
  protected
    procedure setInterpolateFunc(interpolateFunc: TInterpolateFunc);
  public
    constructor Create(const interpolateFunc: TInterpolateFunc = nil; const flywheel: Boolean = true); virtual;
    destructor Destroy; override;
    procedure setFriction(friction: Single);
    function isFinished: Boolean;
    procedure forceFinished(finished: boolean);
    function getCurrX: integer;
    function getCurrY: integer;
    function getCurrVelocity: TPointF;
    function getStartX: integer;
    function getStartY: integer;
    function getFinalX: integer;
    function getFinalY: integer;
    function getDuration: integer;
    procedure extendDuration(extend: integer);
    procedure setCurrX(newX: integer);
    procedure setCurrY(newY: integer);
    procedure setStartX(newX: integer);
    procedure setStartY(newY: integer);
    procedure setFinalX(newX: integer);
    procedure setFinalY(newY: integer);
    function computeScrollOffset: Boolean;
    procedure startScroll(startX: integer; startY: integer; dx: integer; dy: integer; const duration: integer = DEFAULT_DURATION);
    function springBack(startX: integer; startY: integer; minX: integer; maxX: integer; minY: integer; maxY: integer): Boolean;
    procedure fling(startX: integer; startY: integer; velocityX: integer; velocityY: integer; minX: integer; maxX: integer; minY: integer; maxY: integer; const overX: integer = 0; const overY: integer = 0);
    procedure notifyHorizontalEdgeReached(startX: integer; finalX: integer; overX: integer);
    procedure notifyVerticalEdgeReached(startY: integer; finalY: integer; overY: integer);
    function isOverScrolled: Boolean;
    procedure abortAnimation;
    function timePassed: integer;
    function isScrollingInDirection(xvel: Single; yvel: Single): Boolean;
  end;

  {******************************}
  TALScrollEngine = class(TObject)

  {$IFDEF IOS}

  private type
    IDisplayLinkListener = interface(NSObject)
    ['{810AD3F0-265C-4A73-9B96-74103268884A}']
      procedure displayLinkUpdated; cdecl;
    end;

    TDisplayLinkListener = class(TOCLocal)
    private
      fScrollEngine: TALScrollEngine;
    protected
      function GetObjectiveCClass: PTypeInfo; override;
    public
      constructor Create(const aScrollEngine: TALScrollEngine);
      procedure displayLinkUpdated; cdecl;
    end;

  private
    fDisplayLink: CADisplayLink;
    fDisplayLinkListener: TDisplayLinkListener;

  {$ELSEIF defined(ANDROID)}

  private type
    TChoreographerFrameCallback = class(TJavaLocal, JChoreographer_FrameCallback)
    private
      fScrollEngine: TALScrollEngine;
    public
      constructor Create(const aScrollEngine: TALScrollEngine);
      procedure doFrame(frameTimeNanos: Int64); cdecl;
    end;

  private
    FChoreographerFrameCallback: TChoreographerFrameCallback;

  {$ELSE}

  private
    class var FPlatformTimer: IFMXTimerService;
    class procedure InitPlatformTimer; inline;
  private
    FTimerHandle: TFmxHandle;

  {$ENDIF}

  private
    FTimerActive: Boolean;
    FTimerInterval: Integer;
    FOverScroller: TALOverScroller;
    FVelocityTracker: TALVelocityTracker;
    FTouchSlop: Single;
    FOverflingDistance: Single;
    FDragResistanceFactor: Single;
    FMinEdgeSpringbackEnabled: Boolean;
    FMaxEdgeSpringbackEnabled: Boolean;
    FTouchTracking: TTouchTracking;
    FMinScrollLimit: TALPointD;
    FMaxScrollLimit: TALPointD;
    FOnStart: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FViewportPosition: TALPointD;
    FLastMotionPos: TPointF;
    FDown: Boolean;
    FDownPosition: TPointF;
    FUpPosition: TPointF;
    FUpVelocity: TPointF;
    FMoved: Boolean;
    FAutoShowing: Boolean;
    FOpacity: Single;
    FOpacityTransitionStarted: Boolean;
    FOpacityTransitionStartTime: int64;
    FTag: NativeInt;
    procedure StartTimer;
    procedure StopTimer(const AAbruptly: Boolean = False);
    {$IF (not defined(IOS)) and (not defined(ANDROID))}
    procedure TimerProc;
    {$ENDIF}
    procedure SetTimerInterval(const Value: Integer);
    procedure SetDown(const Value: Boolean);
    procedure SetAutoShowing(const Value: Boolean);
    function GetCurrentVelocity: TPointF;
    function GetIsVelocityLow: Boolean;
    procedure SetMinScrollLimit(const Value: TalPointD);
    procedure SetMaxScrollLimit(const Value: TalPointD);
    procedure SetViewportPosition(const Value: TALPointD; const EnforceLimits: Boolean; const SynchOverScroller: Boolean); overload;
  public
    procedure SetViewportPosition(const Value: TALPointD; const EnforceLimits: Boolean); overload;
    procedure SetViewportPosition(const Value: TALPointD); overload;
    procedure SetScrollLimits(const MinValue: TalPointD; const MaxValue: TalPointD; const EnforceLimits: Boolean = True);
  protected
    procedure DoStart; virtual;
    procedure DoChanged; virtual;
    procedure DoStop; virtual;
  public
    const
      // taken from android ViewConfiguration TOUCH_SLOP = 8;
      DefaultTouchSlop = 8;
      // Instead of the android ViewConfiguration OVERFLING_DISTANCE = 6;
      DefaultOverflingDistance = 65;
      // We don't use ALDefaultIntervalOfAni for Android/iOS since we
      // utilize DisplayLink and JChoreographer
      DefaultTimerInterval = 10; // 100 fps
      // The default multiplier that dampens a drag movement at boundaries.
      DefaultDragResistanceFactor = 0.4;
      // A constant value of 0.3, representing the default duration (in seconds)
      // for opacity transitions, such as fade-in and fade-out effects.
      DefaultOpacityTransitionDuration = 0.3;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Calculate: boolean;
    // Specifies the duration between offset recalculations. We don't use
    // TimerInterval for Android/iOS since we utilize DisplayLink and JChoreographer
    property TimerInterval: Integer read FTimerInterval write SetTimerInterval;
    property TimerActive: boolean read FTimerActive;
    procedure setFriction(friction: Single);
    procedure MouseDown(X, Y: Single); virtual;
    procedure MouseMove(X, Y: Single); virtual;
    procedure MouseUp(X, Y: Single); virtual;
    procedure MouseLeave; virtual;
    procedure MouseWheel(X, Y: Double); virtual;
    // Distance a touch can wander before we think the user is scrolling
    property TouchSlop: Single read FTouchSlop write FTouchSlop;
    // Max distance to overfling for edge effects
    property OverflingDistance: Single read FOverflingDistance write FOverflingDistance;
    // A multiplier applied during drag operations to simulate resistance. A value
    // of 1 implies normal drag behavior, while values below 1 introduce increased
    // resistance, making the drag feel heavier.
    property DragResistanceFactor: Single read FDragResistanceFactor write FDragResistanceFactor;
    // Halts the ongoing animation, freezing the scroller at its current position
    // without completing the remaining motion.
    procedure Stop(const AAbruptly: Boolean = False);
    property MinEdgeSpringbackEnabled: Boolean read FMinEdgeSpringbackEnabled write FMinEdgeSpringbackEnabled;
    property MaxEdgeSpringbackEnabled: Boolean read FMaxEdgeSpringbackEnabled write FMaxEdgeSpringbackEnabled;
    property TouchTracking: TTouchTracking read FTouchTracking write FTouchTracking;
    // Controls the opacity behavior typically used by elements like scrollbars.
    // When enabled, the opacity increases, making the element visible during
    // scrolling activity and fades out, hiding the element when the scrolling stops
    property AutoShowing: Boolean read FAutoShowing write SetAutoShowing;
    property Opacity: Single read FOpacity;
    // In virtual pixels per second.
    property CurrentVelocity: TPointF read GetCurrentVelocity;
    property IsVelocityLow: Boolean read GetIsVelocityLow;
    property ViewportPosition: TALPointD read FViewportPosition write SetViewportPosition;
    property MinScrollLimit: TALPointD read FMinScrollLimit write SetMinScrollLimit;
    property MaxScrollLimit: TALPointD read FMaxScrollLimit write SetMaxScrollLimit;
    property Down: Boolean read FDown write SetDown;
    property DownPosition: TPointF read fDownPosition;
    property UpPosition: TPointF read FUpPosition;
    property UpVelocity: TPointF read FUpVelocity;
    // The Moved flag is activated when the mouse is dragged beyond the
    // TouchSlop distance following a mousedown event.
    property Moved: Boolean read FMoved;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property Tag: NativeInt read FTag write FTag;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IFDEF IOS}
  Macapi.ObjCRuntime,
  {$ENDIF}
  FMX.Platform,
  Alcinoe.FMX.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Ani;

const

  // Threshold for determining that a pointer has stopped moving.
  // Some input devices do not send ACTION_MOVE events in the case where a pointer has
  // stopped.  We need to detect this case so that we can accurately predict the
  // velocity after the pointer starts moving again.
  ASSUME_POINTER_STOPPED_TIME = 40 * ALNanosPerMs;

{********************************************}
procedure TALVelocityTracker.TEstimator.clear;
begin
  time := 0;
  degree := 0;
  confidence := 0;
  for var i := 0 to MAX_DEGREE do begin
    xCoeff[i] := 0;
    yCoeff[i] := 0;
  end;
end;

{*******************************************************************************}
// Gets an estimate of the X position of the pointer at the specified time point.
// @param time The time point in seconds, 0 is the last recorded time.
// @return The estimated X coordinate.
function TALVelocityTracker.TEstimator.estimateX(time: Single): Single;
begin
  result := estimate(time, xCoeff);
end;

{*******************************************************************************}
// Gets an estimate of the Y position of the pointer at the specified time point.
// @param time The time point in seconds, 0 is the last recorded time.
// @return The estimated Y coordinate.
function TALVelocityTracker.TEstimator.estimateY(time: Single): Single;
begin
  result := estimate(time, yCoeff);
end;

{*************************************************}
// Gets the X coefficient with the specified index.
// @param index The index of the coefficient to return.
// @return The X coefficient, or 0 if the index is greater than the degree.
function TALVelocityTracker.TEstimator.getXCoeff(index: Integer{UInt32}): Single;
begin
  IF index <= degree then
    result := xCoeff[index]
  else
    result := 0;
end;

{*************************************************}
// Gets the Y coefficient with the specified index.
// @param index The index of the coefficient to return.
// @return The Y coefficient, or 0 if the index is greater than the degree.
function TALVelocityTracker.TEstimator.getYCoeff(index: Integer{UInt32}): Single;
begin
  If index <= degree then
    result := yCoeff[index]
  else
    result := 0;
end;

{******************************************************************************************}
function TALVelocityTracker.TEstimator.estimate(time: Single; const c: TCoeffArray): Single;
begin
  var a: Single := 0;
  var scale: Single := 1;
  for var i := 0 to degree do begin
    a := a + (c[i] * scale);
    scale := scale * time;
  end;
  Result := a;
end;

{*********************************************************}
// Creates a velocity tracker using the specified strategy.
// If strategy is not provided, uses the default strategy for the platform.
constructor TALVelocityTracker.Create(const strategy: TStrategy = TStrategy.DEFAULT);
begin
  inherited Create;
  FLastEventTime := 0;
  // Configure the strategy.
  if (not configureStrategy(strategy)) then
    raise Exception.Create('Unrecognized velocity tracker strategy');
end;

{************************************}
destructor TALVelocityTracker.Destroy;
begin
  ALFreeAndNil(FStrategy);
  inherited Destroy;
end;

{***********************************}
// Resets the velocity tracker state.
procedure TALVelocityTracker.clear;
begin
  FLastEventTime := 0;
  FStrategy.clear;
end;

{********************************************************************************}
function TALVelocityTracker.configureStrategy(const strategy: TStrategy): boolean;
begin
  if (strategy = TStrategy.DEFAULT) then
    FStrategy := createStrategy(DEFAULT_STRATEGY)
  else
    FStrategy := createStrategy(strategy);
  Result := FStrategy <> nil;
end;

{************************************************************************************************}
function TALVelocityTracker.createStrategy(const strategy: TStrategy): TALVelocityTrackerStrategy;
begin
  Case strategy of
    TStrategy.IMPULSE:
      Result := TALImpulseVelocityTrackerStrategy.create;

    TStrategy.LSQ1:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(1);

    TStrategy.LSQ2:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(2);

    TStrategy.LSQ3:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(3);

    TStrategy.WLSQ2_DELTA:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(2, TALLeastSquaresVelocityTrackerStrategy.TWeighting.WEIGHTING_DELTA);

    TStrategy.WLSQ2_CENTRAL:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(2, TALLeastSquaresVelocityTrackerStrategy.TWeighting.WEIGHTING_CENTRAL);

    TStrategy.WLSQ2_RECENT:
      Result := TALLeastSquaresVelocityTrackerStrategy.create(2, TALLeastSquaresVelocityTrackerStrategy.TWeighting.WEIGHTING_RECENT);

    TStrategy.INT1:
      Result := TALIntegratingVelocityTrackerStrategy.create(1);

    TStrategy.INT2:
      Result := TALIntegratingVelocityTrackerStrategy.create(2);

    TStrategy.LEGACY:
      Result := TALLegacyVelocityTrackerStrategy.create;

    else
      Result := nil;
  End;
end;

{**************************}
// Adds movement information
procedure TALVelocityTracker.addMovement(eventTime: Int64; const position: TPointF);
begin
  {$IF defined(Debug)}
  if eventTime = 0 then
    raise Exception.Create('The "eventTime" cannot be set to 0');
  {$ENDIF}

  if (FLastEventTime <> 0) and
     (eventTime >= FLastEventTime + ASSUME_POINTER_STOPPED_TIME) then
    // We have not received any movements for too long.  Assume that all pointers
    // have stopped.
    FStrategy.clear;

  FLastEventTime := eventTime;

  FStrategy.addMovement(eventTime, position);
end;

{************************************************}
// Gets the velocity in position units per second.
// Returns false and sets the velocity components to zero if there is
// insufficient movement information.
function TALVelocityTracker.getVelocity(out outVx: Single; out outVy: Single): Boolean;
begin
  var estimator: TEstimator;
  if (getEstimator(estimator) and (estimator.degree >= 1)) then begin
      outVx := estimator.xCoeff[1];
      outVy := estimator.yCoeff[1];
      exit(true);
  end;
  outVx := 0;
  outVy := 0;
  Result := false;
end;

{********************************************}
// Gets an estimator for the recent movements.
// Returns false and clears the estimator if there is no information available.
function TALVelocityTracker.getEstimator(out outEstimator: TEstimator): Boolean;
begin
  result := FStrategy.getEstimator(outEstimator);
end;

{******************************************************}
// Degree must be no greater than Estimator::MAX_DEGREE.
constructor TALLeastSquaresVelocityTrackerStrategy.Create(degree: Integer{UInt32}; const weighting: TWeighting = TWeighting.WEIGHTING_NONE);
begin
  inherited Create;
  FDegree := degree;
  FWeighting := weighting;
  clear;
end;

{*****************************************************}
procedure TALLeastSquaresVelocityTrackerStrategy.clear;
begin
  FIndex := 0;
  FMovements[0].eventTime := 0;
end;

{******************************************************************************************************}
procedure TALLeastSquaresVelocityTrackerStrategy.addMovement(eventTime: Int64; const position: TPointF);
begin
  if (FMovements[FIndex].eventTime <> eventTime) then begin
    // When ACTION_POINTER_DOWN happens, we will first receive ACTION_MOVE with the coordinates
    // of the existing pointers, and then ACTION_POINTER_DOWN with the coordinates that include
    // the new pointer. If the eventtimes for both events are identical, just update the data
    // for this time.
    // We only compare against the last value, as it is likely that addMovement is called
    // in chronological order as events occur.
    inc(FIndex);
  end;
  if (FIndex = HISTORY_SIZE) then
      FIndex := 0;

  FMovements[FIndex].eventTime := eventTime;
  FMovements[FIndex].Position := position;
end;

{*********************************************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean;
begin
  outEstimator.clear;

  // Iterate over movement samples in reverse time order and collect samples.
  var x: TArray<Single>;
  var y: TArray<Single>;
  var w: TArray<Single>;
  var time: TArray<Single>;

  setlength(x, HISTORY_SIZE);
  setlength(y, HISTORY_SIZE);
  setlength(w, HISTORY_SIZE);
  setlength(time, HISTORY_SIZE);

  var m: Integer{UInt32} := 0;

  var index := FIndex;
  var newestMovement := FMovements[FIndex];
  repeat
    var movement := FMovements[index];
    if (movement.eventTime = 0) then
      break;

    var age: Int64 := newestMovement.eventTime - movement.eventTime;
    if (age > HORIZON) then
      break;

    var position := movement.Position;
    inc(m);
    x[m-1] := position.x;
    y[m-1] := position.y;
    w[m-1] := chooseWeight(index);
    time[m-1] := -age * 0.000000001;
    index := IfThen(index = 0, HISTORY_SIZE, index) - 1;
  until (m >= HISTORY_SIZE);

  if (m = 0) then
    exit(false); // no data

  setlength(x, m);
  setlength(y, m);
  setlength(w, m);
  setlength(time, m);

  // Calculate a least squares polynomial fit.
  var degree := FDegree;
  if (degree > m - 1) then degree := m - 1;

  if ((degree = 2) and (FWeighting = TWeighting.WEIGHTING_NONE)) then begin
    // Optimize unweighted, quadratic polynomial fit
    var xCoeff := solveUnweightedLeastSquaresDeg2(time, x);
    var yCoeff := solveUnweightedLeastSquaresDeg2(time, y);
    if ((xCoeff <> nil) and (yCoeff <> nil)) then begin
      outEstimator.time := newestMovement.eventTime;
      outEstimator.degree := 2;
      outEstimator.confidence := 1;
      for var i := 0 to outEstimator.degree do begin
        outEstimator.xCoeff[i] := xCoeff[i];
        outEstimator.yCoeff[i] := yCoeff[i];
      end;
      exit(true);
    end;
  end
  else if (degree >= 1) then begin
    // General case for an Nth degree polynomial fit
    var xdet, ydet: Single;
    var n: Integer{UInt32} := degree + 1;
    if (solveLeastSquares(time, x, w, n, outEstimator.xCoeff, xdet) and
        solveLeastSquares(time, y, w, n, outEstimator.yCoeff, ydet)) then begin
      outEstimator.time := newestMovement.eventTime;
      outEstimator.degree := degree;
      outEstimator.confidence := xdet * ydet;
      exit(true);
    end;
  end;

  // No velocity data available for this pointer, but we do have its current position.
  outEstimator.xCoeff[0] := x[0];
  outEstimator.yCoeff[0] := y[0];
  outEstimator.time := newestMovement.eventTime;
  outEstimator.degree := 0;
  outEstimator.confidence := 1;
  Result := true;
end;

{**************************************************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.vectorDot(a: system.PSingle; b: system.PSingle; m: Integer{UInt32}): Single;
begin
  var r: Single := 0;
  for var i := 0 to m - 1 do begin
    r := r + (a^ * b^);
    inc(a);
    inc(b);
  end;
  result := r;
end;

{********************************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.vectorNorm(a: system.PSingle; m: Integer{UInt32}): Single;
begin
  var r: Single := 0;
  for var i := 0 to m - 1 do begin
    var t: Single := a^;
    r := r + (t * t);
    inc(a);
  end;
  result := sqrt(r);
end;

{********************************************************************************}
// Solves a linear least squares problem to obtain a N degree polynomial that fits
// the specified input data as nearly as possible.
//
// Returns true if a solution is found, false otherwise.
//
// The input consists of two vectors of data points X and Y with indices 0..m-1
// along with a weight vector W of the same size.
//
// The output is a vector B with indices 0..n that describes a polynomial
// that fits the data, such the sum of W[i] * W[i] * abs(Y[i] - (B[0] + B[1] X[i]
// + B[2] X[i]^2 ... B[n] X[i]^n)) for all i between 0 and m-1 is minimized.
//
// Accordingly, the weight vector W should be initialized by the caller with the
// reciprocal square root of the variance of the error in each input data point.
// In other words, an ideal choice for W would be W[i] = 1 / var(Y[i]) = 1 / stddev(Y[i]).
// The weights express the relative importance of each data point.  If the weights are
// all 1, then the data points are considered to be of equal importance when fitting
// the polynomial.  It is a good idea to choose weights that diminish the importance
// of data points that may have higher than usual error margins.
//
// Errors among data points are assumed to be independent.  W is represented here
// as a vector although in the literature it is typically taken to be a diagonal matrix.
//
// That is to say, the function that generated the input data can be approximated
// by y(x) ~= B[0] + B[1] x + B[2] x^2 + ... + B[n] x^n.
//
// The coefficient of determination (R^2) is also returned to describe the goodness
// of fit of the model for the given data.  It is a value between 0 and 1, where 1
// indicates perfect correspondence.
//
// This function first expands the X vector to a m by n matrix A such that
// A[i][0] = 1, A[i][1] = X[i], A[i][2] = X[i]^2, ..., A[i][n] = X[i]^n, then
// multiplies it by w[i]./
//
// Then it calculates the QR decomposition of A yielding an m by m orthonormal matrix Q
// and an m by n upper triangular matrix R.  Because R is upper triangular (lower
// part is all zeroes), we can simplify the decomposition into an m by n matrix
// Q1 and a n by n matrix R1 such that A = Q1 R1.
//
// Finally we solve the system of linear equations given by R1 B = (Qtranspose W Y)
// to find B.
//
// For efficiency, we lay out A and Q column-wise in memory because we frequently
// operate on the column vectors.  Conversely, we lay out R row-wise.
//
// http://en.wikipedia.org/wiki/Numerical_methods_for_linear_least_squares
// http://en.wikipedia.org/wiki/Gram-Schmidt
function TALLeastSquaresVelocityTrackerStrategy.solveLeastSquares(const x: TArray<Single>; const y: TArray<Single>; const w: TArray<Single>; n: Integer{UInt32}; out outB: TALVelocityTracker.TEstimator.TCoeffArray; out outDet: Single): boolean;
begin
  var m := length(x);
  If (m <> length(y)) or (m <> length(w)) then
    raise exception.Create('Mismatched vector sizes');

  // Expand the X vector to a matrix A, pre-multiplied by the weights.
  var a: TArray<TArray<Single>>;
  Setlength(a,n,m); // column-major order
  for var h := 0 to m - 1 do begin
    a[0][h] := w[h];
    for var i := 1 to n - 1 do begin
      a[i][h] := a[i - 1][h] * x[h];
    end;
  end;

  // Apply the Gram-Schmidt process to A to obtain its QR decomposition.
  var q: TArray<TArray<Single>>;
  var r: TArray<TArray<Single>>;
  Setlength(q,n,m); // orthonormal basis, column-major order
  Setlength(r,n,n); // upper triangular matrix, row-major order
  for var j := 0 to n - 1 do begin
    for var h := 0 to m - 1 do begin
      q[j][h] := a[j][h];
    end;
    for var i := 0 to j - 1 do begin
      var dot: Single := vectorDot(@q[j][0], @q[i][0], m);
      for var h := 0 to m - 1 do begin
        q[j][h] := q[j][h] - (dot * q[i][h]);
      end;
    end;

    var norm: Single := vectorNorm(@q[j][0], m);
    if (norm < 0.000001) then begin
      // vectors are linearly dependent or zero so no solution
      exit(false);
    end;

    var invNorm: Single := 1.0 / norm;
    for var h := 0 to m - 1 do begin
      q[j][h] := q[j][h] * invNorm;
    end;
    for var i := 0 to n - 1 do begin
      if i < j then
        r[j][i] := 0
      else
        r[j][i] := vectorDot(@q[j][0], @a[i][0], m);
    end;
  end;

  // Solve R B = Qt W Y to find B.  This is easy because R is upper triangular.
  // We just work from bottom-right to top-left calculating B's coefficients.
  var wy: TArray<Single>;
  SetLength(wy,m);
  for var h := 0 to m - 1 do begin
    wy[h] := y[h] * w[h];
  end;
  for var i := n - 1 downto 0 do begin
    outB[i] := vectorDot(@q[i][0], @wy[0], m);
    for var j := n - 1 downto i + 1 do begin
      outB[i] := outB[i] - (r[i][j] * outB[j]);
    end;
    outB[i] := outB[i] / r[i][i];
  end;

  // Calculate the coefficient of determination as 1 - (SSerr / SStot) where
  // SSerr is the residual sum of squares (variance of the error),
  // and SStot is the total sum of squares (variance of the data) where each
  // has been weighted.
  var ymean: Single := 0;
  for var h := 0 to m - 1 do begin
    ymean := ymean + y[h];
  end;
  ymean := ymean / m;

  var sserr: Single := 0;
  var sstot: Single := 0;
  for var h := 0 to m - 1 do begin
    var err: Single := y[h] - outB[0];
    var term: Single := 1;
    for var i := 1 to n - 1 do begin
      term := term * x[h];
      err := err - (term * outB[i]);
    end;
    sserr := sserr + (w[h] * w[h] * err * err);
    var &var: Single := y[h] - ymean;
    sstot := sstot + (w[h] * w[h] * &var * &var);
  end;
  if sstot > 0.000001 then
    outDet := 1.0 - (sserr / sstot)
  else
    outDet := 1;
  Result := true;
end;

{********************************************************************************************}
// Optimized unweighted second-order least squares fit. About 2x speed improvement compared to
// the default implementation
function TALLeastSquaresVelocityTrackerStrategy.solveUnweightedLeastSquaresDeg2(const x: TArray<Single>; const y: TArray<Single>): TArray<Single>;
begin
  var count := length(x);
  if count <> length(y) then
    raise Exception.create('Mismatching array sizes');
  // Solving y := a*x^2 + b*x + c
  var sxi: Single := 0;
  var sxiyi: Single := 0;
  var syi: Single := 0;
  var sxi2: Single := 0;
  var sxi3: Single := 0;
  var sxi2yi: Single := 0;
  var sxi4: Single := 0;

  for var i := 0 to count - 1 do begin
    var xi: Single := x[i];
    var yi: Single := y[i];
    var xi2: Single := xi*xi;
    var xi3: Single := xi2*xi;
    var xi4: Single := xi3*xi;
    var xiyi: Single := xi*yi;
    var xi2yi: Single := xi2*yi;

    sxi := sxi + xi;
    sxi2 := sxi2 + xi2;
    sxiyi := sxiyi + xiyi;
    sxi2yi := sxi2yi + xi2yi;
    syi := syi + yi;
    sxi3 := sxi3 + xi3;
    sxi4 := sxi4 + xi4;
  end;

  var Sxx: Single := sxi2 - sxi*sxi / count;
  var Sxy: Single := sxiyi - sxi*syi / count;
  var Sxx2: Single := sxi3 - sxi*sxi2 / count;
  var Sx2y: Single := sxi2yi - sxi2*syi / count;
  var Sx2x2: Single := sxi4 - sxi2*sxi2 / count;

  var denominator: Single := Sxx*Sx2x2 - Sxx2*Sxx2;
  if (denominator = 0) then
    exit(nil);

  // Compute a
  var numerator: Single := Sx2y*Sxx - Sxy*Sxx2;
  var a: Single := numerator / denominator;

  // Compute b
  numerator := Sxy*Sx2x2 - Sx2y*Sxx2;
  var b: Single := numerator / denominator;

  // Compute c
  var c: Single := syi/count - b * sxi/count - a * sxi2/count;

  Result := TArray<Single>.Create(c, b, a);
end;

{*******************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.chooseWeight(index: Integer{UInt32}): Single;
begin
  case FWeighting of
    TWeighting.WEIGHTING_DELTA: begin
      // Weight points based on how much time elapsed between them and the next
      // point so that points that "cover" a shorter time span are weighed less.
      //   delta  0ms: 0.5
      //   delta 10ms: 1.0
      if (index = FIndex) then exit(1.0);
      var nextIndex: Integer{UInt32} := (index + 1) mod HISTORY_SIZE;
      var deltaMillis: Single := (FMovements[nextIndex].eventTime- FMovements[index].eventTime) * 0.000001;
      if (deltaMillis < 0) then exit(0.5);
      if (deltaMillis < 10) then exit(0.5 + deltaMillis * 0.05);
      exit(1.0);
    end;

    TWeighting.WEIGHTING_CENTRAL: begin
      // Weight points based on their age, weighing very recent and very old points less.
      //   age  0ms: 0.5
      //   age 10ms: 1.0
      //   age 50ms: 1.0
      //   age 60ms: 0.5
      var ageMillis: Single := (FMovements[FIndex].eventTime - FMovements[index].eventTime) * 0.000001;
      if (ageMillis < 0) then exit(0.5);
      if (ageMillis < 10) then exit(0.5 + ageMillis * 0.05);
      if (ageMillis < 50) then exit(1.0);
      if (ageMillis < 60) then exit(0.5 + (60 - ageMillis) * 0.05);
      exit(0.5);
    end;

    TWeighting.WEIGHTING_RECENT: begin
      // Weight points based on their age, weighing older points less.
      //   age   0ms: 1.0
      //   age  50ms: 1.0
      //   age 100ms: 0.5
      var ageMillis: Single := (FMovements[FIndex].eventTime - FMovements[index].eventTime) * 0.000001;
      if (ageMillis < 50) then exit(1.0);
      if (ageMillis < 100) then exit(0.5 + (100 - ageMillis) * 0.01);
      exit(0.5);
    end;

    TWeighting.WEIGHTING_NONE: begin
      exit(1.0);
    end;

    else
      Raise exception.Create('Unknown Weighting Type');

  end;
end;

{***********************}
// Degree must be 1 or 2.
constructor TALIntegratingVelocityTrackerStrategy.Create(degree: Integer{UInt32});
begin
  inherited Create;
  FDegree := degree;
end;

{*************************************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.initState(var state: TState; eventTime: Int64; xpos: Single; ypos: Single);
begin
  state.updateTime := eventTime;
  state.degree := 0;

  state.xpos := xpos;
  state.xvel := 0;
  state.xaccel := 0;
  state.ypos := ypos;
  state.yvel := 0;
  state.yaccel := 0;
end;

{***************************************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.updateState(var state: TState; eventTime: Int64; xpos: Single; ypos: Single);
begin
  const MIN_TIME_DELTA: Int64 = 2 * ALNanosPerMs;
  const FILTER_TIME_CONSTANT: Single = 0.010; // 10 milliseconds

  if (eventTime <= state.updateTime + MIN_TIME_DELTA) then
    exit;

  var dt: Single := (eventTime - state.updateTime) * 0.000000001;
  state.updateTime := eventTime;

  var xvel: Single := (xpos - state.xpos) / dt;
  var yvel: Single := (ypos - state.ypos) / dt;
  if (state.degree = 0) then begin
    state.xvel := xvel;
    state.yvel := yvel;
    state.degree := 1;
  end
  else begin
    var alpha: Single := dt / (FILTER_TIME_CONSTANT + dt);
    if (FDegree = 1) then begin
      state.xvel := state.xvel + (xvel - state.xvel) * alpha;
      state.yvel := state.yvel + (yvel - state.yvel) * alpha;
    end
    else begin
        var xaccel: Single := (xvel - state.xvel) / dt;
        var yaccel: Single := (yvel - state.yvel) / dt;
        if (state.degree = 1) then begin
          state.xaccel := xaccel;
          state.yaccel := yaccel;
          state.degree := 2;
        end
        else begin
          state.xaccel := state.xaccel + (xaccel - state.xaccel) * alpha;
          state.yaccel := state.yaccel + (yaccel - state.yaccel) * alpha;
        end;
        state.xvel := state.xvel + (state.xaccel * dt) * alpha;
        state.yvel := state.yvel + (state.yaccel * dt) * alpha;
    end;
  end;
  state.xpos := xpos;
  state.ypos := ypos;
end;

{**************************************************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.populateEstimator(const state: TState; out outEstimator: TALVelocityTracker.TEstimator);
begin
  outEstimator.time := state.updateTime;
  outEstimator.confidence := 1.0;
  outEstimator.degree := state.degree;
  outEstimator.xCoeff[0] := state.xpos;
  outEstimator.xCoeff[1] := state.xvel;
  outEstimator.xCoeff[2] := state.xaccel / 2;
  outEstimator.yCoeff[0] := state.ypos;
  outEstimator.yCoeff[1] := state.yvel;
  outEstimator.yCoeff[2] := state.yaccel / 2;
end;

{****************************************************}
procedure TALIntegratingVelocityTrackerStrategy.clear;
begin
  FState.updateTime := 0;
end;

{*****************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.addMovement(eventTime: Int64; const position: TPointF);
begin
  if FState.updateTime <> 0 then
    updateState(Fstate, eventTime, position.x, position.y)
  else
    initState(Fstate, eventTime, position.x, position.y);
end;

{********************************************************************************************************************}
function TALIntegratingVelocityTrackerStrategy.getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean;
begin
  outEstimator.clear;

  if FState.updateTime <> 0 then begin
    populateEstimator(FState, outEstimator);
    exit(true);
  end;

  exit(false);
end;

{**************************************************}
constructor TALLegacyVelocityTrackerStrategy.Create;
begin
  inherited Create;
  clear;
end;

{***********************************************}
procedure TALLegacyVelocityTrackerStrategy.clear;
begin
  FIndex := 0;
  FMovements[0].eventTime := 0;
end;

{************************************************************************************************}
procedure TALLegacyVelocityTrackerStrategy.addMovement(eventTime: Int64; const position: TPointF);
begin
  inc(FIndex);
  if (FIndex = HISTORY_SIZE) then
    FIndex := 0;

  FMovements[FIndex].eventTime := eventTime;
  FMovements[FIndex].position := position;
end;

{***************************************************************************************************************}
function TALLegacyVelocityTrackerStrategy.getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean;
begin
  outEstimator.clear;

  var newestMovement := FMovements[FIndex];
  if (newestMovement.eventTime = 0) then
      exit(false); // no data

  // Find the oldest sample that contains the pointer and that is not older than HORIZON.
  var minTime: Int64 := newestMovement.eventTime - HORIZON;
  var oldestIndex: Integer{UInt32} := FIndex;
  var numTouches: Integer{UInt32} := 1;
  repeat
    var nextOldestIndex: Integer{UInt32} := IfThen(oldestIndex = 0, HISTORY_SIZE, oldestIndex) - 1;
    var nextOldestMovement := FMovements[nextOldestIndex];
    if (nextOldestMovement.eventTime = 0) or
       (nextOldestMovement.eventTime < minTime) then
      break;
    oldestIndex := nextOldestIndex;
    inc(numTouches);
  until (numTouches >= HISTORY_SIZE);

  // Calculate an exponentially weighted moving average of the velocity estimate
  // at different points in time measured relative to the oldest sample.
  // This is essentially an IIR filter.  Newer samples are weighted more heavily
  // than older samples.  Samples at equal time points are weighted more or less
  // equally.
  //
  // One tricky problem is that the sample data may be poorly conditioned.
  // Sometimes samples arrive very close together in time which can cause us to
  // overestimate the velocity at that time point.  Most samples might be measured
  // 16ms apart but some consecutive samples could be only 0.5sm apart because
  // the hardware or driver reports them irregularly or in bursts.
  var accumVx: Single := 0;
  var accumVy: Single := 0;
  var index: Integer{UInt32} := oldestIndex;
  var samplesUsed: Integer{UInt32} := 0;
  var oldestMovement := FMovements[oldestIndex];
  var oldestPosition := oldestMovement.Position;
  var lastDuration: Int64 := 0;

  while (numTouches > 1) do begin
    dec(numTouches);
    inc(index);
    if (index = HISTORY_SIZE) then
      index := 0;
    var movement := FMovements[index];
    var duration: Int64 := movement.eventTime - oldestMovement.eventTime;

    // If the duration between samples is small, we may significantly overestimate
    // the velocity.  Consequently, we impose a minimum duration constraint on the
    // samples that we include in the calculation.
    if (duration >= MIN_DURATION) then begin
      var position := movement.Position;
      var scale: Single := 1000000000.0 / duration; // one over time delta in seconds
      var vx: Single := (position.x - oldestPosition.x) * scale;
      var vy: Single := (position.y - oldestPosition.y) * scale;
      accumVx := (accumVx * lastDuration + vx * duration) / (duration + lastDuration);
      accumVy := (accumVy * lastDuration + vy * duration) / (duration + lastDuration);
      lastDuration := duration;
      samplesUsed := samplesUsed + 1;
    end;
  end;

  // Report velocity.
  var newestPosition := newestMovement.Position;
  outEstimator.time := newestMovement.eventTime;
  outEstimator.confidence := 1;
  outEstimator.xCoeff[0] := newestPosition.x;
  outEstimator.yCoeff[0] := newestPosition.y;
  if (samplesUsed > 0) then begin
    outEstimator.xCoeff[1] := accumVx;
    outEstimator.yCoeff[1] := accumVy;
    outEstimator.degree := 1;
  end
  else begin
    outEstimator.degree := 0;
  end;
  Result := true;
end;

{***************************************************}
constructor TALImpulseVelocityTrackerStrategy.Create;
begin
  inherited Create;
  clear;
end;

{************************************************}
procedure TALImpulseVelocityTrackerStrategy.clear;
begin
  FIndex := 0;
  FMovements[0].eventTime := 0;
end;

{*******************************************************************************}
// Calculate the total impulse provided to the screen and the resulting velocity.
//
// The touchscreen is modeled as a physical object.
// Initial condition is discussed below, but for now suppose that v(t=0) := 0
//
// The kinetic energy of the object at the release is E=0.5*m*v^2
// Then vfinal := sqrt(2E/m). The goal is to calculate E.
//
// The kinetic energy at the release is equal to the total work done on the object by the finger.
// The total work W is the sum of all dW along the path.
//
// dW := F*dx, where dx is the piece of path traveled.
// Force is change of momentum over time, F := dp/dt := m dv/dt.
// Then substituting:
// dW := m (dv/dt) * dx := m * v * dv
//
// Summing along the path, we get:
// W := sum(dW) := sum(m * v * dv) := m * sum(v * dv)
// Since the mass stays constant, the equation for final velocity is:
// vfinal := sqrt(2*sum(v * dv))
//
// Here,
// dv : change of velocity := (v[i+1]-v[i])
// dx : change of distance := (x[i+1]-x[i])
// dt : change of time := (t[i+1]-t[i])
// v : instantaneous velocity := dx/dt
//
// The final formula is:
// vfinal := sqrt(2) * sqrt(sum((v[i]-v[i-1])*|v[i]|)) for all i
// The absolute value is needed to properly account for the sign. If the velocity over a
// particular segment descreases, then this indicates braking, which means that negative
// work was done. So for two positive, but decreasing, velocities, this contribution would be
// negative and will cause a smaller final velocity.
//
// Initial condition
// There are two ways to deal with initial condition:
// 1) Assume that v(0) := 0, which would mean that the screen is initially at rest.
// This is not entirely accurate. We are only taking the past X ms of touch data, where X is
// currently equal to 100. However, a touch event that created a fling probably lasted for longer
// than that, which would mean that the user has already been interacting with the touchscreen
// and it has probably already been moving.
// 2) Assume that the touchscreen has already been moving at a certain velocity, calculate this
// initial velocity and the equivalent energy, and start with this initial energy.
// Consider an example where we have the following data, consisting of 3 points:
//                 time: t0, t1, t2
//                 x   : x0, x1, x2
//                 v   : 0 , v1, v2
// Here is what will happen in each of these scenarios:
// 1) By directly applying the formula above with the v(0) := 0 boundary condition, we will get
// vfinal := sqrt(2*(|v1|*(v1-v0) + |v2|*(v2-v1))). This can be simplified since v0=0
// vfinal := sqrt(2*(|v1|*v1 + |v2|*(v2-v1))) := sqrt(2*(v1^2 + |v2|*(v2 - v1)))
// since velocity is a real number
// 2) If we treat the screen as already moving, then it must already have an energy (per mass)
// equal to 1/2*v1^2. Then the initial energy should be 1/2*v1*2, and only the second segment
// will contribute to the total kinetic energy (since we can effectively consider that v0=v1).
// This will give the following expression for the final velocity:
// vfinal := sqrt(2*(1/2*v1^2 + |v2|*(v2-v1)))
// This analysis can be generalized to an arbitrary number of samples.
//
//
// Comparing the two equations above, we see that the only mathematical difference
// is the factor of 1/2 in front of the first velocity term.
// This boundary condition would allow for the "proper" calculation of the case when all of the
// samples are equally spaced in time and distance, which should suggest a constant velocity.
//
// Note that approach 2) is sensitive to the proper ordering of the data in time, since
// the boundary condition must be applied to the oldest sample to be accurate.
function TALImpulseVelocityTrackerStrategy.kineticEnergyToVelocity(work: Single): Single;
begin
  const sqrt2: Single = 1.41421356237;
  result := IfThen(work < 0, -1.0, 1.0) * sqrt(abs(work)) * sqrt2;
end;

{*******************************************************************************************************************************************************}
function TALImpulseVelocityTrackerStrategy.calculateImpulseVelocity(const t: TArray<Int64>; const x: TArray<Single>; count: Integer{NativeUInt}): Single;
begin
  // The input should be in reversed time order (most recent sample at index i=0)
  // t[i] is in nanoseconds, but due to FP arithmetic, convert to seconds inside this function
  const SECONDS_PER_NANO: Single = 1E-9;

  if (count < 2) then
    exit(0); // if 0 or 1 points, velocity is zero
  if (count = 2) then begin // if 2 points, basic linear calculation
    if (t[1] = t[0]) then
      exit(0);
    Exit((x[1] - x[0]) / (SECONDS_PER_NANO * (t[1] - t[0])));
  end;
  // Guaranteed to have at least 3 points here
  var work: Single := 0;
  for var i := count - 1 downto 1 do begin // start with the oldest sample and go forward in time
    if (t[i] = t[i-1]) then
      continue;
    var vprev: Single := kineticEnergyToVelocity(work); // v[i-1]
    var vcurr: Single := (x[i] - x[i-1]) / (SECONDS_PER_NANO * (t[i] - t[i-1])); // v[i]
    work := work + (vcurr - vprev) * abs(vcurr);
    if (i = count - 1) then
        work := work * 0.5; // initial condition, case 2) above
  end;
  Result := kineticEnergyToVelocity(work);
end;

{*************************************************************************************************}
procedure TALImpulseVelocityTrackerStrategy.addMovement(eventTime: Int64; const position: TPointF);
begin
  if (FMovements[FIndex].eventTime <> eventTime) then begin
    // When ACTION_POINTER_DOWN happens, we will first receive ACTION_MOVE with the coordinates
    // of the existing pointers, and then ACTION_POINTER_DOWN with the coordinates that include
    // the new pointer. If the eventtimes for both events are identical, just update the data
    // for this time.
    // We only compare against the last value, as it is likely that addMovement is called
    // in chronological order as events occur.
    inc(FIndex);
  end;
  if (FIndex = HISTORY_SIZE) then
    FIndex := 0;

  var movement := FMovements[FIndex];
  movement.eventTime := eventTime;
  movement.position := position;
end;

{****************************************************************************************************************}
function TALImpulseVelocityTrackerStrategy.getEstimator(out outEstimator: TALVelocityTracker.TEstimator): boolean;
begin
  outEstimator.clear;

  // Iterate over movement samples in reverse time order and collect samples.
  var x: Tarray<Single>;
  setlength(x, HISTORY_SIZE);
  var y: Tarray<Single>;
  setlength(y, HISTORY_SIZE);
  var time: Tarray<Int64>;
  setlength(time, HISTORY_SIZE);
  var m: Integer{NativeUInt} := 0; // number of points that will be used for fitting
  var index: Integer{NativeUInt} := FIndex;
  var newestMovement := FMovements[FIndex];
  repeat
    var movement := FMovements[index];
    if (movement.eventTime = 0) then
      break;

    var age: Int64 := newestMovement.eventTime - movement.eventTime;
    if (age > HORIZON) then
      break;

    var position := movement.Position;
    x[m] := position.x;
    y[m] := position.y;
    time[m] := movement.eventTime;
    index := IfThen(index = 0, HISTORY_SIZE, index) - 1;
    Inc(m);
  until (m >= HISTORY_SIZE);

  if (m = 0) then
    exit(false); // no data
  outEstimator.xCoeff[0] := 0;
  outEstimator.yCoeff[0] := 0;
  outEstimator.xCoeff[1] := calculateImpulseVelocity(time, x, m);
  outEstimator.yCoeff[1] := calculateImpulseVelocity(time, y, m);
  outEstimator.xCoeff[2] := 0;
  outEstimator.yCoeff[2] := 0;
  outEstimator.time := newestMovement.eventTime;
  outEstimator.degree := 2; // similar results to 2nd degree fit
  outEstimator.confidence := 1;
  Result := true;
end;

{***********************************************}
class procedure TALSplineOverScroller.InitSpline;
begin
  var x_min: Single := 0.0;
  var y_min: Single := 0.0;
  for var i := 0 to NB_SAMPLES - 1 do begin
    const alpha: Single = {(Single)} i / NB_SAMPLES;

    var x_max: Single := 1.0;
    var x, tx, coef: Single;
    while (true) do begin
      x := x_min + (x_max - x_min) / 2.0;
      coef := 3.0 * x * (1.0 - x);
      tx := coef * ((1.0 - x) * P1 + x * P2) + x * x * x;
      if (Abs(tx - alpha) < 1E-5) then break;
      if (tx > alpha) then x_max := x
      else x_min := x;
    end;
    SPLINE_POSITION[i] := coef * ((1.0 - x) * START_TENSION + x) + x * x * x;

    var y_max: Single := 1.0;
    var y, dy: Single;
    while (true) do begin
      y := y_min + (y_max - y_min) / 2.0;
      coef := 3.0 * y * (1.0 - y);
      dy := coef * ((1.0 - y) * START_TENSION + y) + y * y * y;
      if (Abs(dy - alpha) < 1E-5) then break;
      if (dy > alpha) then y_max := y
      else y_min := y;
    end;
    SPLINE_TIME[i] := coef * ((1.0 - y) * P1 + y * P2) + y * y * y;
  end;
  SPLINE_POSITION[NB_SAMPLES] := 1.0;
  SPLINE_TIME[NB_SAMPLES] := 1.0;
end;

{******************************************************}
class procedure TALSplineOverScroller.InitPixelsPerInch;
begin
  if PixelsPerInch = 0 then begin
    {$IF defined(DEBUG)}
    if TThread.Current.ThreadID <> MainThreadID then
      raise Exception.Create('InitPixelsPerInch must be called from the main thread');
    {$ENDIF}
    var FMXDeviceMetrics: IFMXDeviceMetricsService;
    if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService, IInterface(FMXDeviceMetrics)) then
      PixelsPerInch := FMXDeviceMetrics.GetDisplayMetrics.PixelsPerInch
    else
      PixelsPerInch := TDeviceDisplayMetrics.Default.PixelsPerInch;
  end;
end;

{***************************************}
constructor TALSplineOverScroller.Create;
begin
  inherited Create;
  FFlingFriction := 0.015; {ViewConfiguration.getScrollFriction}
  FState := SPLINE;
  FFinished := true;
  // const ppi: Single = context.getResources().getDisplayMetrics().density * 160.0;
  // ppi could also be calculated like this: ppi := screenscale * 160.0
  InitPixelsPerInch;
  FPhysicalCoeff := 9.80665 {SensorManager.GRAVITY_EARTH} // g (m/s^2)
                    * 39.37 // inch/meter
                    * PixelsPerInch
                    * 0.84; // look and feel tuning
  {$IFDEF DEBUG}
  ALLog(
    'TALSplineOverScroller.Create',
    'PixelsPerInch:' + ALFormatFloatW('0.##', PixelsPerInch, ALDefaultFormatSettingsW) + ' | ' +
    'PhysicalCoeff:' + ALFormatFloatW('0.##', FPhysicalCoeff, ALDefaultFormatSettingsW),
    TalLogType.verbose);
  {$ENDIF}
end;

{************************************************************}
procedure TALSplineOverScroller.setFriction(friction: Single);
begin
  FFlingFriction := friction;
end;

{******************************************************}
procedure TALSplineOverScroller.updateScroll(q: Single);
begin
  FCurrentPosition := FStart + Round(q * (FFinal - FStart));
end;

{*********************************************************}
// Get a signed deceleration that will reduce the velocity.
Class function TALSplineOverScroller.getDeceleration(velocity: integer): Single;
begin
  Result := IfThen(velocity > 0, -GRAVITY, GRAVITY);
end;

{************************************************************************************}
// Modifies FDuration to the duration it takes to get from start to newFinal using the
// spline interpolation. The previous duration was needed to get to oldFinal.
procedure TALSplineOverScroller.adjustDuration(start: integer; oldFinal: integer; newFinal: integer);
begin
  const oldDistance: integer = oldFinal - start;
  const newDistance: integer = newFinal - start;
  const x: Single = Abs({(Single)} newDistance / oldDistance);
  const index: integer = {(integer)} trunc(NB_SAMPLES * x);
  if (index < NB_SAMPLES) then begin
    const x_inf: Single = {(Single)} index / NB_SAMPLES;
    const x_sup: Single = {(Single)} (index + 1) / NB_SAMPLES;
    const t_inf: Single = SPLINE_TIME[index];
    const t_sup: Single = SPLINE_TIME[index + 1];
    const timeCoef: Single = t_inf + (x - x_inf) / (x_sup - x_inf) * (t_sup - t_inf);
    FDuration := trunc(FDuration * timeCoef);
  end;
end;

{************************************************************************************************}
procedure TALSplineOverScroller.startScroll(start: integer; distance: integer; duration: integer);
begin
  FFinished := false;

  FCurrentPosition := start;
  FStart := start;
  FFinal := start + distance;

  FStartTime := ALElapsedTimeMillisAsInt64;
  FDuration := duration;

  // Unused
  FDeceleration := 0.0;
  FVelocity := 0;
end;

{*************************************}
procedure TALSplineOverScroller.finish;
begin
  FCurrentPosition := FFinal;
  // Not reset since WebView relies on this value for fast fling.
  // TODO: restore when WebView uses the fast fling implemented in this class.
  // FCurrVelocity := 0.0;
  // As we do not use the WebView I restore FCurrVelocity to 0.0
  FCurrVelocity := 0.0;
  FFinished := true;
end;

{******************************************************************}
procedure TALSplineOverScroller.setStartPosition(position: integer);
begin
  FStart := position;
  FSplineDistance := FFinal - FStart;
  FFinished := false;
end;

{******************************************************************}
procedure TALSplineOverScroller.setFinalPosition(position: integer);
begin
  FFinal := position;
  FSplineDistance := FFinal - FStart;
  FFinished := false;
end;

{**************************************************************}
procedure TALSplineOverScroller.extendDuration(extend: integer);
begin
  const time: int64 = ALElapsedTimeMillisAsInt64;
  const elapsedTime: integer = {(Integer)} (time - FStartTime);
  FDuration := elapsedTime + extend;
  FSplineDuration := elapsedTime + extend;
  FFinished := false;
end;

{*********************************************************************************************}
function TALSplineOverScroller.springback(start: integer; min: integer; max: integer): Boolean;
begin
  FFinished := true;

  FCurrentPosition := start;
  FStart := start;
  FFinal := start;
  FVelocity := 0;

  FStartTime := ALElapsedTimeMillisAsInt64;
  FDuration := 0;

  if (start < min) then
    startSpringback(start, min, 0)
  else if (start > max) then
    startSpringback(start, max, 0);

  result := not FFinished;
end;

{************************************************************************************************}
procedure TALSplineOverScroller.startSpringback(start: integer; &end: integer; velocity: integer);
begin
  // FStartTime has been set
  FFinished := false;
  FState := CUBIC;
  FCurrentPosition := start;
  FStart := start;
  FFinal := &end;
  const delta: integer = start - &end;
  FDeceleration := getDeceleration(delta);
  // TODO take velocity into account
  FVelocity := -delta; // only sign is used
  FOver := Abs(delta);
  //https://stackoverflow.com/questions/77048191/android-overscroller-deceleration-behavior-making-it-similar-to-ios
  FDuration := {(Integer)} trunc(min(200, 1000.0 * sqrt(-2.0 * delta / FDeceleration)));
  //FDuration := {(Integer)} trunc(1000.0 * sqrt(-2.0 * delta / FDeceleration));
end;

{******************************************************************************************************************}
procedure TALSplineOverScroller.fling(start: integer; velocity: integer; min: integer; max: integer; over: integer);
begin
  FOver := over;
  FFinished := false;
  FCurrVelocity := velocity;
  FVelocity := velocity;
  FDuration := 0;
  FSplineDuration := 0;
  FStartTime := ALElapsedTimeMillisAsInt64;
  FCurrentPosition := start;
  FStart := start;

  if ((start > max) or (start < min)) then begin
    startAfterEdge(start, min, max, velocity);
    exit;
  end;

  FState := SPLINE;
  var totalDistance: double := 0.0;

  if (velocity <> 0) then begin
    FDuration := getSplineFlingDuration(velocity);
    FSplineDuration := FDuration;
    totalDistance := getSplineFlingDistance(velocity);
  end;

  FSplineDistance := {(Integer)} trunc(totalDistance * Sign(velocity));
  FFinal := start + FSplineDistance;

  // Clamp to a valid final position
  if (FFinal < min) then begin
    adjustDuration(FStart, FFinal, min);
    FFinal := min;
  end;

  if (FFinal > max) then begin
    adjustDuration(FStart, FFinal, max);
    FFinal := max;
  end;
end;

{******************************************************************************}
function TALSplineOverScroller.getSplineDeceleration(velocity: integer): double;
begin
  Result := ln(INFLEXION * Abs(velocity) / (FFlingFriction * FPhysicalCoeff));
end;

{*******************************************************************************}
function TALSplineOverScroller.getSplineFlingDistance(velocity: integer): double;
begin
  const l: double = getSplineDeceleration(velocity);
  const decelMinusOne: double = DECELERATION_RATE - 1.0;
  result := FFlingFriction * FPhysicalCoeff * Exp(DECELERATION_RATE / decelMinusOne * l);
end;

{************************************************}
// Returns the duration, expressed in milliseconds
function TALSplineOverScroller.getSplineFlingDuration(velocity: integer): integer;
begin
  const l: double = getSplineDeceleration(velocity);
  const decelMinusOne: double = DECELERATION_RATE - 1.0;
  result := {(Integer)} trunc(1000.0 * Exp(l / decelMinusOne));
end;

{*************************************************************************************************}
procedure TALSplineOverScroller.fitOnBounceCurve(start: integer; &end: integer; velocity: integer);
begin
  // Simulate a bounce that started from edge
  const durationToApex: Single = - velocity / FDeceleration;
  // The Single cast below is necessary to avoid integer overflow.
  const velocitySquared: Single = {(Single)} Single(velocity) * Single(velocity);
  const distanceToApex: Single = velocitySquared / 2.0 / Abs(FDeceleration);
  const distanceToEdge: Single = Abs(&end - start);
  const totalDuration: Single = {(Single)} Sqrt(2.0 * (distanceToApex + distanceToEdge) / Abs(FDeceleration));
  FStartTime := FStartTime - {(Integer)} trunc(1000.0 * (totalDuration - durationToApex));
  FCurrentPosition := &end;
  FStart := &end;
  FVelocity := {(Integer)} trunc(- FDeceleration * totalDuration);
end;

{*****************************************************************************************************}
procedure TALSplineOverScroller.startBounceAfterEdge(start: integer; &end: integer; velocity: integer);
begin
  FDeceleration := getDeceleration(IfThen(velocity = 0, start - &end, velocity));
  fitOnBounceCurve(start, &end, velocity);
  onEdgeReached;
end;

{************************************************************************************************************}
procedure TALSplineOverScroller.startAfterEdge(start: integer; min: integer; max: integer; velocity: integer);
begin
  if ((start > min) and (start < max)) then begin
    ALLog('TALSplineOverScroller', 'startAfterEdge called from a valid position', TALLogType.ERROR);
    FFinished := true;
    exit;
  end;
  const positive: Boolean = start > max;
  const edge: integer = IfThen(positive, max, min);
  const overDistance: integer = start - edge;
  // The Single cast below is necessary to avoid integer overflow.
  var keepIncreasing: Boolean := Single(overDistance) * Single(velocity) >= 0;
  if (keepIncreasing) then begin
    // Will result in a bounce or a to_boundary depending on velocity.
    startBounceAfterEdge(start, edge, velocity);
  end
  else begin
    const totalDistance: double = getSplineFlingDistance(velocity);
    if (totalDistance > Abs(overDistance)) then begin
      fling(start, velocity, IfThen(positive, min, start), IfThen(positive, start, max), FOver);
    end
    else begin
      startSpringback(start, edge, velocity);
    end;
  end;
end;

{**********************************************************************************************}
procedure TALSplineOverScroller.notifyEdgeReached(start: integer; &end: integer; over: integer);
begin
  // FState is used to detect successive notifications
  if (FState = SPLINE) then begin
    FOver := over;
    FStartTime := ALElapsedTimeMillisAsInt64;
    // We were in fling/scroll mode before: current velocity is such that distance to
    // edge is increasing. This ensures that startAfterEdge will not start a new fling.
    startAfterEdge(start, &end, &end, {(Integer)} trunc(FCurrVelocity));
  end;
end;

{********************************************}
procedure TALSplineOverScroller.onEdgeReached;
begin
  // FStart, FVelocity and FStartTime were adjusted to their values when edge was reached.
  // The Single cast below is necessary to avoid integer overflow.
  const velocitySquared: Single = {(Single)} Single(FVelocity) * Single(FVelocity);
  var distance: Single := velocitySquared / (2.0 * Abs(FDeceleration));
  const sign: Single = Sign(FVelocity);

  if (distance > FOver) then begin
    // Default deceleration is not sufficient to slow us down before boundary
    FDeceleration := - sign * velocitySquared / (2.0 * FOver);
    distance := FOver;
  end;

  FOver := {(Integer)} trunc(distance);
  FState := BALLISTIC;
  FFinal := FStart + {(Integer)} trunc(IfThen(FVelocity > 0, distance, -distance));
  FDuration := - {(Integer)} trunc(1000.0 * FVelocity / FDeceleration);
end;

{***********************************************************}
function TALSplineOverScroller.continueWhenFinished: Boolean;
begin
  case (FState) of
    SPLINE: begin
      // Duration from start to null velocity
      if (FDuration < FSplineDuration) then begin
        // If the animation was clamped, we reached the edge
        FCurrentPosition := FFinal;
        FStart := FFinal;
        // TODO Better compute speed when edge was reached
        FVelocity := {(Integer)} trunc(FCurrVelocity);
        FDeceleration := getDeceleration(FVelocity);
        FStartTime := FStartTime + FDuration;
        onEdgeReached;
      end
      else begin
        // Normal stop, no need to continue
        exit(false);
      end;
    end;
    BALLISTIC: begin
      FStartTime := FStartTime + FDuration;
      startSpringback(FFinal, FStart, 0);
    end;
    CUBIC:
      exit(false);
  end;

  update;
  Result := true;
end;

{*******************************************************************}
// Update the current position and velocity for current time. Returns
// true if update has been done and false if animation duration has been
// reached.
function TALSplineOverScroller.update: Boolean;
begin
  const time: int64 = ALElapsedTimeMillisAsInt64;
  const currentTime: int64 = time - FStartTime;

  if (currentTime = 0) then
    // Skip work but report that we're still going if we have a nonzero duration.
    exit(FDuration > 0);
  if (currentTime > FDuration) then
    exit(false);

  var distance: double := 0.0;
  case (FState) of
    SPLINE: begin
      const t: Single = {(Single)} currentTime / FSplineDuration;
      const index: integer = {(Integer)} trunc(NB_SAMPLES * t);
      var distanceCoef: Single := 1.0;
      var velocityCoef: Single := 0.0;
      if (index < NB_SAMPLES) then begin
        const t_inf: Single = {(Single)} index / NB_SAMPLES;
        const t_sup: Single = {(Single)} (index + 1) / NB_SAMPLES;
        const d_inf: Single = SPLINE_POSITION[index];
        const d_sup: Single = SPLINE_POSITION[index + 1];
        velocityCoef := (d_sup - d_inf) / (t_sup - t_inf);
        distanceCoef := d_inf + (t - t_inf) * velocityCoef;
      end;

      distance := distanceCoef * FSplineDistance;
      FCurrVelocity := velocityCoef * FSplineDistance / FSplineDuration * 1000.0;
    end;

    BALLISTIC: begin
      const t: Single = currentTime / 1000.0;
      FCurrVelocity := FVelocity + FDeceleration * t;
      distance := FVelocity * t + FDeceleration * t * t / 2.0;
    end;

    CUBIC: begin
      const t: Single = {(Single)} (currentTime) / FDuration;
      const t2: Single = t * t;
      const sign: Single = Sign(FVelocity);
      distance := sign * FOver * (3.0 * t2 - 2.0 * t * t2);
      FCurrVelocity := sign * FOver * 6.0 * (- t + t2);
    end;
  end;

  FCurrentPosition := FStart + {(Integer)} Round(distance);

  result := true;
end;

{*************************}
// Creates an OverScroller.
// @param interpolateFunc The scroll interpolator. If null, a default (viscous) interpolator will be used.
// @param flywheel If true, successive fling motions will keep on increasing scroll speed.
constructor TALOverScroller.Create(const interpolateFunc: TInterpolateFunc = nil; const flywheel: Boolean = true);
begin
  inherited Create;
  if not assigned(interpolateFunc) then FInterpolateFunc := ALInterpolateViscousFluid
  else FInterpolateFunc := interpolateFunc;
  FFlywheel := flywheel;
  FScrollerX := TALSplineOverScroller.Create;
  FScrollerY := TALSplineOverScroller.Create;
end;

{*********************************}
destructor TALOverScroller.Destroy;
begin
  inherited Destroy;
  ALFreeAndNil(FScrollerX);
  ALFreeAndNil(FScrollerY);
end;

{******************************************************************************}
procedure TALOverScroller.setInterpolateFunc(interpolateFunc: TInterpolateFunc);
begin
  if not assigned(interpolateFunc) then FInterpolateFunc := ALInterpolateViscousFluid
  else FInterpolateFunc := interpolateFunc;
end;

{******************************************}
// The amount of friction applied to flings.
// @param friction A scalar dimension-less value representing the coefficient of friction.
procedure TALOverScroller.setFriction(friction: Single);
begin
  FScrollerX.setFriction(friction);
  FScrollerY.setFriction(friction);
end;

{*****************************************************}
// Returns whether the scroller has finished scrolling.
// @return True if the scroller has finished scrolling, false otherwise.
function TALOverScroller.isFinished: Boolean;
begin
  Result := FScrollerX.FFinished and FScrollerY.FFinished;
end;

{************************************************************}
// Force the finished field to a particular value. Contrary to
// abortAnimation, forcing the animation to finished
// does NOT cause the scroller to move to the const x and y
// position.
// @param finished The new finished value.
procedure TALOverScroller.forceFinished(finished: boolean);
begin
  FScrollerX.FFinished := finished;
  FScrollerY.FFinished := finished;
end;

{********************************************}
// Returns the current X offset in the scroll.
// @return The new X offset as an absolute distance from the origin.
function TALOverScroller.getCurrX: integer;
begin
  result := FScrollerX.FCurrentPosition;
end;

{********************************************}
// Returns the current Y offset in the scroll.
// @return The new Y offset as an absolute distance from the origin.
function TALOverScroller.getCurrY: integer;
begin
  result := FScrollerY.FCurrentPosition;
end;

{*******************************************}
// Returns the value of the current velocity.
// @return The original velocity less the deceleration.
function TALOverScroller.getCurrVelocity: TPointF;
begin
  //result := {(Single)} system.Math.hypot(FScrollerX.FCurrVelocity, FScrollerY.FCurrVelocity);
  result := TPointF.Create(FScrollerX.FCurrVelocity, FScrollerY.FCurrVelocity);
  if FScrollerX.FState = FScrollerX.CUBIC then Result.X := -Result.X;
  if FScrollerY.FState = FScrollerY.CUBIC then Result.Y := -Result.Y;
end;

{******************************************}
// Returns the start X offset in the scroll.
// @return The start X offset as an absolute distance from the origin.
function TALOverScroller.getStartX: integer;
begin
  result := FScrollerX.FStart;
end;

{******************************************}
// Returns the start Y offset in the scroll.
// @return The start Y offset as an absolute distance from the origin.
function TALOverScroller.getStartY: integer;
begin
  result := FScrollerY.FStart;
end;

{*******************************************************************}
// Returns where the scroll will end. Valid only for "fling" scrolls.
// @return The const X offset as an absolute distance from the origin.
function TALOverScroller.getFinalX: integer;
begin
  result := FScrollerX.FFinal;
end;

{*******************************************************************}
// Returns where the scroll will end. Valid only for "fling" scrolls.
// @return The const Y offset as an absolute distance from the origin.
function TALOverScroller.getFinalY: integer;
begin
  result := FScrollerY.FFinal;
end;

{***************************************************************}
// Returns how int64 the scroll event will take, in milliseconds.
// @return The duration of the scroll in milliseconds.
function TALOverScroller.getDuration: integer;
begin
  result := System.Math.max(FScrollerX.FDuration, FScrollerY.FDuration);
end;

{***********************************************************************}
// Extend the scroll animation. This allows a running animation to scroll
// further and longer, when used with setFinalX(Integer) or setFinalY(Integer).
// @param extend Additional time to scroll in milliseconds.
// @see #setFinalX(Integer)
// @see #setFinalY(Integer)
procedure TALOverScroller.extendDuration(extend: integer);
begin
  FScrollerX.extendDuration(extend);
  FScrollerY.extendDuration(extend);
end;

{************************************************}
procedure TALOverScroller.setCurrX(newX: integer);
begin
  FScrollerX.FCurrentPosition := newX;
end;

{************************************************}
procedure TALOverScroller.setCurrY(newY: integer);
begin
  FScrollerY.FCurrentPosition := newY;
end;

{*************************************************}
procedure TALOverScroller.setStartX(newX: integer);
begin
  FScrollerX.setStartPosition(newX);
end;

{*************************************************}
procedure TALOverScroller.setStartY(newY: integer);
begin
  FScrollerY.setStartPosition(newY);
end;

{***********************************************}
// Sets the const position (X) for this scroller.
// @param newX The new X offset as an absolute distance from the origin.
// @see #extendDuration(Integer)
// @see #setFinalY(Integer)
procedure TALOverScroller.setFinalX(newX: integer);
begin
  FScrollerX.setFinalPosition(newX);
end;

{***********************************************}
// Sets the const position (Y) for this scroller.
// @param newY The new Y offset as an absolute distance from the origin.
// @see #extendDuration(Integer)
// @see #setFinalX(Integer)
procedure TALOverScroller.setFinalY(newY: integer);
begin
  FScrollerY.setFinalPosition(newY);
end;

{**************************************************************************}
// Call this when you want to know the new location. If it returns true, the
// animation is not yet finished.
function TALOverScroller.computeScrollOffset: Boolean;
begin
  if (isFinished) then
    exit(false);

  case (FMode) of

    SCROLL_MODE: begin
      var time: int64 := ALElapsedTimeMillisAsInt64;
      // Any scroller can be used for time, since they were started
      // together in scroll mode. We use X here.
      const elapsedTime: int64 = time - FScrollerX.FStartTime;

      const duration: integer = FScrollerX.FDuration;
      if (elapsedTime < duration) then begin
        const q: Single = FInterpolateFunc(elapsedTime / {(Single)} duration);
        FScrollerX.updateScroll(q);
        FScrollerY.updateScroll(q);
      end
      else begin
        abortAnimation;
      end;
    end;

    FLING_MODE: begin
      if (not FScrollerX.FFinished) then begin
        if (not FScrollerX.update) then begin
          if (not FScrollerX.continueWhenFinished) then begin
            FScrollerX.finish;
          end;
        end;
      end;

      if (not FScrollerY.FFinished) then begin
        if (not FScrollerY.update) then begin
          if (not FScrollerY.continueWhenFinished) then begin
            FScrollerY.finish;
          end;
        end;
      end;
    end;

  end;

  Result := true;
end;

{*************************************************************************}
// Start scrolling by providing a starting point and the distance to travel.
// @param startX Starting horizontal scroll offset in pixels. Positive numbers will scroll the content to the left.
// @param startY Starting vertical scroll offset in pixels. Positive numbers will scroll the content up.
// @param dx Horizontal distance to travel. Positive numbers will scroll the content to the left.
// @param dy Vertical distance to travel. Positive numbers will scroll the content up.
// @param duration Duration of the scroll in milliseconds.
procedure TALOverScroller.startScroll(startX: integer; startY: integer; dx: integer; dy: integer; const duration: integer = DEFAULT_DURATION);
begin
  FMode := SCROLL_MODE;
  FScrollerX.startScroll(startX, dx, duration);
  FScrollerY.startScroll(startY, dy, duration);
end;

{************************************************************************}
// Call this when you want to 'spring back' into a valid coordinate range.
// @param startX Starting X coordinate
// @param startY Starting Y coordinate
// @param minX Minimum valid X value
// @param maxX Maximum valid X value
// @param minY Minimum valid Y value
// @param maxY Minimum valid Y value
// @return true if a springback was initiated, false if startX and startY were already within the valid range.
function TALOverScroller.springBack(startX: integer; startY: integer; minX: integer; maxX: integer; minY: integer; maxY: integer): Boolean;
begin
  FMode := FLING_MODE;

  // Make sure both methods are called.
  const spingbackX: Boolean = FScrollerX.springback(startX, minX, maxX);
  const spingbackY: Boolean = FScrollerY.springback(startY, minY, maxY);
  Result := spingbackX or spingbackY;
end;

{*********************************************************************}
// Start scrolling based on a fling gesture. The distance traveled will
// depend on the initial velocity of the fling.
// @param startX Starting point of the scroll (X)
// @param startY Starting point of the scroll (Y)
// @param velocityX Initial velocity of the fling (X) measured in pixels per second.
// @param velocityY Initial velocity of the fling (Y) measured in pixels per second
// @param minX Minimum X value. The scroller will not scroll past this point unless overX > 0. If overfling is allowed, it will use minX as a springback boundary.
// @param maxX Maximum X value. The scroller will not scroll past this point unless overX > 0. If overfling is allowed, it will use maxX as a springback boundary.
// @param minY Minimum Y value. The scroller will not scroll past this point unless overY > 0. If overfling is allowed, it will use minY as a springback boundary.
// @param maxY Maximum Y value. The scroller will not scroll past this point unless overY > 0. If overfling is allowed, it will use maxY as a springback boundary.
// @param overX Overfling range. If > 0, horizontal overfling in either direction will be possible.
// @param overY Overfling range. If > 0, vertical overfling in either direction will be possible.
procedure TALOverScroller.fling(startX: integer; startY: integer; velocityX: integer; velocityY: integer; minX: integer; maxX: integer; minY: integer; maxY: integer; const overX: integer = 0; const overY: integer = 0);
begin
  // Continue a scroll or fling in progress
  if (FFlywheel and (not isFinished)) then begin
    var oldVelocityX: Single := FScrollerX.FCurrVelocity;
    var oldVelocityY: Single := FScrollerY.FCurrVelocity;
    if ((Sign(velocityX) = Sign(oldVelocityX)) and
        (Sign(velocityY) = Sign(oldVelocityY))) then begin
      velocityX := trunc(velocityX + oldVelocityX);
      velocityY := trunc(velocityY + oldVelocityY);
    end;
  end;

  FMode := FLING_MODE;
  FScrollerX.fling(startX, velocityX, minX, maxX, overX);
  FScrollerY.fling(startY, velocityY, minY, maxY, overY);
end;

{**************************************************************}
// Notify the scroller that we've reached a horizontal boundary.
// Normally the information to handle this will already be known
// when the animation is started, such as in a call to one of the
// fling functions. However there are cases where this cannot be known
// in advance. This function will transition the current motion and
// animate from startX to finalX as appropriate.
// @param startX Starting/current X position
// @param finalX Desired const X position
// @param overX Magnitude of overscroll allowed. This should be the maximum desired distance from finalX. Absolute value - must be positive.
procedure TALOverScroller.notifyHorizontalEdgeReached(startX: integer; finalX: integer; overX: integer);
begin
  FScrollerX.notifyEdgeReached(startX, finalX, overX);
end;

{************************************************************}
// Notify the scroller that we've reached a vertical boundary.
// Normally the information to handle this will already be known
// when the animation is started, such as in a call to one of the
// fling functions. However there are cases where this cannot be known
// in advance. This function will animate a parabolic motion from
// startY to finalY.
// @param startY Starting/current Y position
// @param finalY Desired const Y position
// @param overY Magnitude of overscroll allowed. This should be the maximum desired distance from finalY. Absolute value - must be positive.
procedure TALOverScroller.notifyVerticalEdgeReached(startY: integer; finalY: integer; overY: integer);
begin
  FScrollerY.notifyEdgeReached(startY, finalY, overY);
end;

{*********************************************************************************}
// Returns whether the current Scroller is currently returning to a valid position.
// Valid bounds were provided by the
// fling(int, int, int, int, int, int, int, int, int, int) method.
// One should check this value before calling
// startScroll(int, int, int, int) as the interpolation currently in progress
// to restore a valid position will then be stopped. The caller has to take into account
// the fact that the started scroll will start from an overscrolled position.
// @return true when the current position is overscrolled and in the process of interpolating back to a valid value.
function TALOverScroller.isOverScrolled: Boolean;
begin
  Result := (((not FScrollerX.FFinished) and
              (FScrollerX.FState <> TALSplineOverScroller.SPLINE)) or
             ((not FScrollerY.FFinished) and
              (FScrollerY.FState <> TALSplineOverScroller.SPLINE)));
end;

{*********************************************************}
// Stops the animation. Contrary to forceFinished(boolean),
// aborting the animating causes the scroller to move to the const x and y
// positions.
// @see #forceFinished(boolean)
procedure TALOverScroller.abortAnimation;
begin
  FScrollerX.finish;
  FScrollerY.finish;
end;

{***************************************************************}
// Returns the time elapsed since the beginning of the scrolling.
// @return The elapsed time in milliseconds.
function TALOverScroller.timePassed: integer;
begin
  const time: int64 = ALElapsedTimeMillisAsInt64;
  const startTime: int64 = System.Math.min(FScrollerX.FStartTime, FScrollerY.FStartTime);
  result := {(Integer)} (time - startTime);
end;

{***********************************************************************************}
function TALOverScroller.isScrollingInDirection(xvel: Single; yvel: Single): Boolean;
begin
  const dx: integer = FScrollerX.FFinal - FScrollerX.FStart;
  const dy: integer = FScrollerY.FFinal - FScrollerY.FStart;
  result := (not isFinished) and
            (Sign(xvel) = Sign(dx)) and
            (Sign(yvel) = Sign(dy));
end;

{********************************************************************}
constructor TALScrollCapturedMessage.Create(const ACaptured: boolean);
begin
  inherited Create;
  fCaptured := ACaptured;
end;

{$IFDEF IOS}

{********************************************************************************************}
constructor TALScrollEngine.TDisplayLinkListener.Create(const aScrollEngine: TALScrollEngine);
begin
  inherited Create;
  fScrollEngine := aScrollEngine;
end;

{****************************************************************}
procedure TALScrollEngine.TDisplayLinkListener.displayLinkUpdated;
begin
  {$IFDEF DEBUG}
  //ALLog('TALScrollEngine.TDisplayLinkListener.displayLinkUpdated', TalLogType.verbose);
  {$ENDIF}
  if not fScrollEngine.Calculate then
    fScrollEngine.StopTimer;
end;

{**************************************************************************}
function TALScrollEngine.TDisplayLinkListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IDisplayLinkListener);
end;

{$ENDIF}

{$IF defined(ANDROID)}

{***************************************************************************************************}
constructor TALScrollEngine.TChoreographerFrameCallback.Create(const aScrollEngine: TALScrollEngine);
 begin
  inherited Create;
  fScrollEngine := aScrollEngine;
end;

{***********************************************************************************}
procedure TALScrollEngine.TChoreographerFrameCallback.doFrame(frameTimeNanos: Int64);
begin
  {$IFDEF DEBUG}
  //ALLog('TALScrollEngine.TChoreographerFrameCallback.doFrame', TalLogType.verbose);
  {$ENDIF}
  if not fScrollEngine.Calculate then
    fScrollEngine.StopTimer
  else
    TJChoreographer.JavaClass.getInstance.postFrameCallback(self);
end;

{$ENDIF}

{*************}
//[MultiThread]
// We can create a TALScrollEngine from a background thread
// but we can use it only in the main thread. If we create the
// TALScrollEngine from a background thread we must call in the
// mainthread:
//   TALScrollEngine.InitPlatformTimer
//   TALSplineOverScroller.InitPixelsPerInch
constructor TALScrollEngine.Create;
begin
  inherited Create;
  {$IFDEF IOS}
  fDisplayLinkListener := nil;
  fDisplayLink := nil;
  {$ELSEIF defined(ANDROID)}
  fChoreographerFrameCallback := nil;
  {$ELSE}
  FTimerHandle := TFmxHandle(-1);
  {$ENDIF}
  FTimerActive := False;
  FTimerInterval := DefaultTimerInterval;
  FOverScroller := TALOverScroller.Create;
  FVelocityTracker := TALVelocityTracker.Create;
  FTouchSlop := DefaultTouchSlop;
  FOverflingDistance := DefaultOverflingDistance;
  FDragResistanceFactor := DefaultDragResistanceFactor;
  FMinEdgeSpringbackEnabled := True;
  FMaxEdgeSpringbackEnabled := True;
  FTouchTracking := [ttVertical, ttHorizontal];
  FMinScrollLimit := TALPointD.Zero;
  FMaxScrollLimit := TALPointD.Zero;
  FOnStart := nil;
  FOnChanged := nil;
  FOnStop := nil;
  FViewportPosition := TALPointD.Zero;
  FLastMotionPos := TPointF.Zero;
  FDown := false;
  FDownPosition := TPointF.Zero;
  FUpPosition := TPointF.Zero;
  FUpVelocity := TPointF.Zero;
  FMoved := false;
  FAutoShowing := False;
  FOpacity := 1;
  FOpacityTransitionStarted := False;
  FOpacityTransitionStartTime := 0;
  FTag := 0;
end;

{*********************************}
destructor TALScrollEngine.Destroy;
begin
  StopTimer;
  {$IFDEF IOS}
  if fDisplayLink <> nil then begin
    // Removes the display link from all run loop modes.
    // Removing the display link from all run loop modes causes it to be released by the run loop. The display link also releases the target.
    // invalidate is thread safe meaning that it can be called from a thread separate to the one in which the display link is running.
    fDisplayLink.invalidate;
    fDisplayLink.release;
  end;
  AlFreeAndNil(fDisplayLinkListener);
  {$ELSEIF defined(ANDROID)}
  alFreeAndNil(fChoreographerFrameCallback);
  {$ENDIF}
  ALFreeAndNil(FOverScroller);
  ALFreeAndNil(FVelocityTracker);
  inherited Destroy;
end;

{*************************************************}
{$IF (not Defined(IOS)) and (not defined(ANDROID))}
class Procedure TALScrollEngine.InitPlatformTimer;
begin
  if (FPlatformTimer = nil) then begin
    {$IF defined(DEBUG)}
    if TThread.Current.ThreadID <> MainThreadID then
      raise Exception.Create('InitPlatformTimer must be called from the main thread');
    {$ENDIF}
    if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FPlatformTimer) then
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;
{$ENDIF}

{******************************************}
// The amount of friction applied to flings.
// @param friction A scalar dimension-less value representing the coefficient of friction.
procedure TALScrollEngine.setFriction(friction: Single);
begin
  FoverScroller.setFriction(friction);
end;

{***********************************************************************************************************************************}
procedure TALScrollEngine.SetScrollLimits(const MinValue: TalPointD; const MaxValue: TalPointD; const EnforceLimits: Boolean = True);
begin
  Var LUpdated: Boolean := False;
  if FMinScrollLimit <> MinValue then begin
    FMinScrollLimit := MinValue;
    LUpdated := True;
  end;
  if FMaxScrollLimit <> MaxValue then begin
    FMaxScrollLimit := MaxValue;
    LUpdated := True;
  end;
  if LUpdated and EnforceLimits then
    SetViewPortPosition(FViewPortPosition);
end;

{******************************************************************}
procedure TALScrollEngine.SetMinScrollLimit(const Value: TalPointD);
begin
  SetScrollLimits(Value, FMaxScrollLimit);
end;

{******************************************************************}
procedure TALScrollEngine.SetMaxScrollLimit(const Value: TalPointD);
begin
  SetScrollLimits(FMinScrollLimit, Value);
end;

{******************************************************}
procedure TALScrollEngine.SetDown(const Value: Boolean);
begin
  if value then
    raise Exception.Create('"SetDown" can only accept "false" as its parameter value');
  if not Fdown then exit;
  FVelocityTracker.clear;
  MouseUp(FLastMotionPos.X, FLastMotionPos.y);
end;

{***************************************************************}
procedure TALScrollEngine.SetTimerInterval(const Value: Integer);
begin
  if FTimerInterval <> Value then begin
    if Value <= 0 then
      raise Exception.Create('Interval must be greater than 0');
    FTimerInterval := Value;
  end;
end;

{***************************************************}
function TALScrollEngine.GetCurrentVelocity: TPointF;
begin
  result := Foverscroller.getCurrVelocity / ALScreenScale;
end;

{*************************************************}
function TALScrollEngine.GetIsVelocityLow: Boolean;
begin
  var LCurrentVelocity := GetCurrentVelocity;
  // virtual pixels per second
  result := (abs(LCurrentVelocity.X) < 10) and
            (abs(LCurrentVelocity.Y) < 10);
end;

{***********************************}
procedure TALScrollEngine.StartTimer;
begin

  if FTimerActive then exit;
  FTimerActive := True;

  DoStart;

  {$IFDEF IOS}
  if fDisplayLinkListener = nil then
    fDisplayLinkListener := TDisplayLinkListener.Create(self);
  if fDisplayLink = nil then begin
    fDisplayLink := TCADisplayLink.Wrap(TCADisplayLink.OCClass.displayLinkWithTarget(fDisplayLinkListener.GetObjectID, sel_getUid('displayLinkUpdated')));
    fDisplayLink.retain;
    fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  end;
  fDisplayLink.setPaused(False);
  {$ELSEIF defined(ANDROID)}
  if fChoreographerFrameCallback = nil then
    fChoreographerFrameCallback := TChoreographerFrameCallback.create(self);
  TJChoreographer.JavaClass.getInstance.postFrameCallback(fChoreographerFrameCallback);
  {$ELSE}
  if FTimerHandle = TFmxHandle(-1) then begin
    InitPlatformTimer;
    FTimerHandle := FPlatformTimer.CreateTimer(FTimerInterval, TimerProc);
  end;
  {$ENDIF}

end;

{********************************************************************}
procedure TALScrollEngine.StopTimer(const AAbruptly: Boolean = False);
begin

  if not FTimerActive then exit;
  FTimerActive := False;

  if not AAbruptly then
    DoStop;

  {$IFDEF IOS}
  if fDisplayLink <> nil then
    fDisplayLink.setPaused(True);
  {$ELSEIF defined(ANDROID)}
  if fChoreographerFrameCallback <> nil then
    TJChoreographer.JavaClass.getInstance.removeFrameCallback(fChoreographerFrameCallback);
  {$ELSE}
  if FTimerHandle <> TFmxHandle(-1) then begin
    InitPlatformTimer;
    FPlatformTimer.DestroyTimer(FTimerHandle);
    FTimerHandle := TFmxHandle(-1);
  end;
  {$ENDIF}

end;

{********************************}
procedure TALScrollEngine.DoStart;
begin
  {$IFDEF DEBUG}
  ALLog('TALScrollEngine.DoStart', TalLogType.verbose);
  {$ENDIF}
  if Assigned(FOnStart) then
    FOnStart(self);
end;

{**********************************}
procedure TALScrollEngine.DoChanged;
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALScrollEngine.DoChanged',
  //  'ViewPortPosition:' + ALFormatFloatW('0.##', ViewPortPosition.x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', ViewPortPosition.y, ALDefaultFormatSettingsW),
  //TalLogType.verbose);
  {$ENDIF}
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

{*******************************}
procedure TALScrollEngine.DoStop;
begin
  {$IFDEF DEBUG}
  ALLog('TALScrollEngine.DoStop', TalLogType.verbose);
  {$ENDIF}
  if Assigned(FOnStop) then
    FOnStop(self);
end;

{***************************************************************************}
// Halts the ongoing animation, freezing the scroller at its current position
// without completing the remaining motion.
procedure TALScrollEngine.Stop(const AAbruptly: Boolean = False);
begin
  if AAbruptly then
    FDown := False;
  //--
  if FOverScroller.isFinished then begin
    if AAbruptly then
      StopTimer(true{AAbruptly});
    exit;
  end;
  //--
  if not AAbruptly then begin
    if not FOverScroller.isOverScrolled then
      FOverScroller.forceFinished(true{finished});
  end
  else begin
    FOverScroller.forceFinished(true{finished});
    StopTimer(true{AAbruptly});
  end;
end;

{*************************************************************}
procedure TALScrollEngine.SetAutoShowing(const Value: Boolean);
begin
  if FAutoShowing <> Value then begin
    FAutoShowing := Value;
    if AutoShowing then FOpacity := 0
    else FOpacity := 1;
  end;
end;

{************************************************************************************************************************************}
procedure TALScrollEngine.SetViewportPosition(const Value: TALPointD; const EnforceLimits: Boolean; const SynchOverScroller: Boolean);
begin

  // If FOverScroller is still active and is in an over-scrolled state,
  // we check if the final position lies within the ViewportPosition boundaries.
  // If it doesn't, we terminate the FOverScroller to allow the subsequent block
  // to initiate a new spring back action.
  if EnforceLimits and (not FOverScroller.isFinished) and FOverScroller.isOverScrolled then begin
    var LMinX: integer := trunc(FMinScrollLimit.x*ALScreenScale);
    var LMaxX: integer := trunc(FMaxScrollLimit.x*ALScreenScale);
    var LMinY: integer := trunc(FMinScrollLimit.y*ALScreenScale);
    var LMaxY: integer := trunc(FMaxScrollLimit.y*ALScreenScale);
    var LFinalX: integer := FOverScroller.getFinalX;
    var LFinalY: integer := FOverScroller.getFinalY;
    if SynchOverScroller then begin
      var LDelta := (Value - FViewportPosition) * ALScreenScale;
      LFinalX := Trunc(LFinalX + LDelta.X);
      LFinalY := Trunc(LFinalY + LDelta.Y);
    end;
    if (LFinalX < LMinX) or
       (LFinalX > LMaxX) or
       (LFinalY < LMinY) or
       (LFinalY > LMaxY) then FOverScroller.forceFinished(true);
  end;

  // If the FOverScroller has completed, we verify the ViewportPosition boundaries.
  // Should the new ViewportPosition exceed these boundaries, we initiate the springBack function.
  if FOverScroller.isFinished then begin
    if EnforceLimits then begin
      var LStartX: integer := trunc(Value.X*ALScreenScale);
      var LStartY: integer := trunc(Value.Y*ALScreenScale);
      var LMinX: integer := trunc(FMinScrollLimit.x*ALScreenScale);
      var LMaxX: integer := trunc(FMaxScrollLimit.x*ALScreenScale);
      var LMinY: integer := trunc(FMinScrollLimit.y*ALScreenScale);
      var LMaxY: integer := trunc(FMaxScrollLimit.y*ALScreenScale);
      if FOverScroller.springBack(
           LStartX, // startX: integer;
           LStartY, // startY: integer;
           LMinX, // minX: integer;
           LMaxX, // maxX: integer;
           LMinY, // minY: integer;
           LMaxY) then startTimer;
    end;
    if FViewportPosition <> Value then begin
      FViewportPosition := Value;
      DoChanged;
    end;
  end

  // If FOverScroller hasn't finished, we skip checking the ViewportPosition
  // limits. The upcoming calculation will call notifyVerticalEdgeReached or
  // notifyHorizontalEdgeReached to adjust the viewport to its boundary limits.
  else begin
    if FViewportPosition <> Value then begin
      if SynchOverScroller then begin
        var LDelta := (Value - FViewportPosition) * ALScreenScale;
        FOverScroller.setCurrX(Trunc(FOverScroller.getCurrX + LDelta.X));
        FOverScroller.setCurrY(Trunc(FOverScroller.getCurrY + LDelta.Y));
        FOverScroller.setStartX(Trunc(FOverScroller.getStartX + LDelta.X));
        FOverScroller.setStartY(Trunc(FOverScroller.getStartY + LDelta.Y));
        FOverScroller.setFinalX(Trunc(FOverScroller.getFinalX + LDelta.X));
        FOverScroller.setFinalY(Trunc(FOverScroller.getFinalY + LDelta.Y));
      end;
      FViewportPosition := Value;
      DoChanged
    end;
  end;

end;

{**************************************************************************************************}
procedure TALScrollEngine.SetViewportPosition(const Value: TALPointD; const EnforceLimits: Boolean);
begin
  SetViewportPosition(Value, EnforceLimits, true{SynchOverScroller});
end;

{********************************************************************}
procedure TALScrollEngine.SetViewportPosition(const Value: TALPointD);
begin
  SetViewportPosition(Value, true{EnforceLimits}, true{SynchOverScroller});
end;

{******************************************}
function TALScrollEngine.Calculate: boolean;
begin

  if not FDown then Result := FOverScroller.computeScrollOffset
  else Result := False;

  if Result and (not FOverScroller.isOverScrolled) then begin

    if ttVertical in FTouchTracking then begin
      var LCurrVelocityY := FOverScroller.getCurrVelocity.Y;
      if (LCurrVelocityY > 0) and
         (FOverScroller.getCurrY > FMaxScrollLimit.Y*ALScreenScale) then begin
        FOverScroller.notifyVerticalEdgeReached(
          FOverScroller.getCurrY, // startY: integer;
          trunc(FMaxScrollLimit.Y*ALScreenScale), // finalY: integer;
          trunc(FOverflingDistance*ALScreenScale));  // overY: integer;
        {$IFDEF DEBUG}
        ALLog(
          'TALScrollEngine.Calculate',
          'notifyVerticalEdgeReached',
          TalLogType.verbose);
        {$ENDIF}
      end
      else if (LCurrVelocityY < 0) and
              (FOverScroller.getCurrY < FMinScrollLimit.Y*ALScreenScale) then begin
        FOverScroller.notifyVerticalEdgeReached(
          FOverScroller.getCurrY, // startY: integer;
          trunc(FMinScrollLimit.Y*ALScreenScale), // finalY: integer;
          trunc(FOverflingDistance*ALScreenScale));  // overY: integer;
        {$IFDEF DEBUG}
        ALLog(
          'TALScrollEngine.Calculate',
          'notifyVerticalEdgeReached',
          TalLogType.verbose);
        {$ENDIF}
      end;
    end;

    if ttHorizontal in FTouchTracking then begin
      var LCurrVelocityX := FOverScroller.getCurrVelocity.X;
      if (LCurrVelocityX > 0) and
         (FOverScroller.getCurrX > FMaxScrollLimit.X*ALScreenScale) then begin
        FOverScroller.notifyHorizontalEdgeReached(
          FOverScroller.getCurrX, // startX: integer;
          trunc(FMaxScrollLimit.X*ALScreenScale), // finalX: integer;
          trunc(FOverflingDistance*ALScreenScale));  // overX: integer;
        {$IFDEF DEBUG}
        ALLog(
          'TALScrollEngine.Calculate',
          'notifyHorizontalEdgeReached',
          TalLogType.verbose);
        {$ENDIF}
      end
      else if (LCurrVelocityX < 0) and
              (FOverScroller.getCurrX < FMinScrollLimit.X*ALScreenScale) then begin
        FOverScroller.notifyHorizontalEdgeReached(
          FOverScroller.getCurrX, // startX: integer;
          trunc(FMinScrollLimit.X*ALScreenScale), // finalX: integer;
          trunc(FOverflingDistance*ALScreenScale));  // overX: integer;
        {$IFDEF DEBUG}
        ALLog(
          'TALScrollEngine.Calculate',
          'notifyHorizontalEdgeReached',
          TalLogType.verbose);
        {$ENDIF}
      end;
    end;

  end;

  Var LOpacityChanged := False;
  if AutoShowing then begin
    if Result or FDown then begin
      if CompareValue(opacity, 1, Tepsilon.Scale) < 0 then begin
        if not FOpacityTransitionStarted then begin
          FOpacityTransitionStarted := True;
          FOpacityTransitionStartTime := ALElapsedTimeMillisAsInt64;
        end;
        FOpacity := Min(1, (ALElapsedTimeMillisAsInt64 - FOpacityTransitionStartTime) / (DefaultOpacityTransitionDuration * ALMsPerSec));
        if CompareValue(opacity, 1, Tepsilon.Scale) >= 0 then
          FOpacityTransitionStarted := False;
        LOpacityChanged := True;
      end;
    end
    else begin
      if CompareValue(opacity, 0, Tepsilon.Scale) > 0 then begin
        if not FOpacityTransitionStarted then begin
          FOpacityTransitionStarted := True;
          FOpacityTransitionStartTime := ALElapsedTimeMillisAsInt64;
        end;
        FOpacity := max(0, 1 - ((ALElapsedTimeMillisAsInt64 - FOpacityTransitionStartTime) / (DefaultOpacityTransitionDuration * ALMsPerSec)));
        if CompareValue(opacity, 0, Tepsilon.Scale) <= 0 then
          FOpacityTransitionStarted := False;
        LOpacityChanged := True;
      end;
    end;
  end;

  if result then begin
    SetViewportPosition(
      TALPointD.Create(
        FOverScroller.getCurrX/ALScreenScale,
        FOverScroller.getCurrY/ALScreenScale),
      False{EnforceLimits},
      False{SynchOverScroller});
  end
  else if LOpacityChanged then begin
    Result := True;
    Dochanged;
  end
  else if Fdown then
    result := True;

end;

{*************************************************}
{$IF (not defined(IOS)) and (not defined(ANDROID))}
procedure TALScrollEngine.TimerProc;
begin
  if not calculate then
    StopTimer;
end;
{$ENDIF}

{************************************************}
procedure TALScrollEngine.MouseDown(X, Y: Single);
begin
  if FTouchTracking = [] then exit;
  FDown := True;
  FMoved := False;
  FLastMotionPos := TpointF.Create(X,Y);
  fDownPosition := FLastMotionPos;
  {$IFDEF DEBUG}
  //ALLog(
  //    'TALScrollEngine.MouseDown',
  //    'Position:' + ALFormatFloatW('0.##', FDownPosition.x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', FDownPosition.y, ALDefaultFormatSettingsW),
  //  TalLogType.verbose);
  {$ENDIF}
  FOverScroller.forceFinished(true{finished});
  FVelocityTracker.clear;
  FVelocityTracker.addMovement(ALElapsedTimeNano, FLastMotionPos*ALScreenScale);
  StartTimer;
end;

{************************************************}
procedure TALScrollEngine.MouseMove(X, Y: Single);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetDiff(Var xDiff, yDiff: Single): boolean;
  begin
    Result := True;
    if FTouchTracking = [ttVertical, ttHorizontal] then begin
      xDiff := FLastMotionPos.X - x;
      yDiff := FLastMotionPos.Y - y;
    end
    else if FTouchTracking = [ttVertical] then begin
      xDiff := 0.0;
      yDiff := FLastMotionPos.Y - y;
    end
    else if FTouchTracking = [ttHorizontal] then begin
      xDiff := FLastMotionPos.X - x;
      yDiff := 0.0;
    end
    else
      Result := False;
  end;

begin

  //
  // Todo: use the historical data
  // In android VelocityTracker.cpp they do like this :
  //
  // size_t historySize = event->getHistorySize();
  // for (size_t h = 0; h <= historySize; h++) {
  //     nsecs_t eventTime = event->getHistoricalEventTime(h);
  //     for (size_t i = 0; i < pointerCount; i++) {
  //         uint32_t index = pointerIndex[i];
  //         positions[index].x = event->getHistoricalX(i, h);
  //         positions[index].y = event->getHistoricalY(i, h);
  //     }
  //     addMovement(eventTime, idBits, positions);
  // }
  //
  // We can do the same in FMX.Platform.UI.Android by updating the function
  //   function TAndroidMotionManager.HandleMotionEvent(const AEvent: JMotionEvent): Boolean;
  // and storing temporarly the AEvent inside a global variable that we can
  // access from here
  //
  // The concept is the same in iOS with coalescedTouchesForTouch
  // https://developer.apple.com/documentation/uikit/uievent/1613808-coalescedtouchesfortouch?language=objc
  //

  if not fDown then exit;

  {$IFDEF DEBUG}
  //ALLog(
  //  'TALScrollEngine.MouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW),
  //  TalLogType.verbose);
  {$ENDIF}

  var xDiff: Single;
  var yDiff: Single;
  if not _GetDiff(xDiff, yDiff) then
    exit;

  if (not FMoved) then begin
    if (abs(yDiff) <= FTouchSlop) and (abs(xDiff) <= FTouchSlop) then exit;
    FLastMotionPos := TpointF.Create(X,Y);
    if not _GetDiff(xDiff, yDiff) then
      exit;
  end;

  FMoved := True;

  if (FViewportPosition.x < FMinScrollLimit.x) or
     (FViewportPosition.x > FMaxScrollLimit.x) then xDiff := xDiff * FDragResistanceFactor;
  if (FViewportPosition.y < FMinScrollLimit.y) or
     (FViewportPosition.y > FMaxScrollLimit.y) then yDiff := yDiff * FDragResistanceFactor;

  if FDragResistanceFactor = 0 then begin
         if FViewportPosition.x + xDiff < FMinScrollLimit.x then xDiff := FMinScrollLimit.x - FViewportPosition.x
    else if FViewportPosition.x + xDiff > FMaxScrollLimit.x then xDiff := FMaxScrollLimit.x - FViewportPosition.x;
         if FViewportPosition.y + YDiff < FMinScrollLimit.y then yDiff := FMinScrollLimit.y - FViewportPosition.y
    else if FViewportPosition.y + YDiff > FMaxScrollLimit.y then yDiff := FMaxScrollLimit.y - FViewportPosition.y;
  end;

  if sameValue(xDiff, 0, Tepsilon.Position) and
     sameValue(YDiff, 0, Tepsilon.Position) then exit;

  SetViewportPosition(
    TALPointD.Create(
      FViewportPosition.x + xDiff,
      FViewportPosition.Y + YDiff),
    False{EnforceLimits},
    False{SynchOverScroller});

  FLastMotionPos := TpointF.Create(X,Y);
  FVelocityTracker.addMovement(ALElapsedTimeNano, FLastMotionPos*ALScreenScale);

end;

{**********************************************}
procedure TALScrollEngine.MouseUp(X, Y: Single);
begin
  if not Fdown then exit;

  FDown := False;

  if (not SameValue(X, FLastMotionPos.X, Tepsilon.Position)) or
     (not SameValue(Y, FLastMotionPos.Y, Tepsilon.Position)) then
    MouseMove(X, Y);

  var LStartX: integer;
  var LStartY: integer;
  var LVelocityX: Single;
  var LVelocityY: Single;
  var LMinX: integer;
  var LMaxX: integer;
  var LMinY: integer;
  var LMaxY: integer;

  FvelocityTracker.getVelocity(LVelocityX, LVelocityY);

  if FTouchTracking = [ttVertical, ttHorizontal] then begin
    LStartX := trunc(FViewPortPosition.X*ALScreenScale);
    LStartY := trunc(FViewPortPosition.Y*ALScreenScale);
    LVelocityX := -LVelocityX;
    LVelocityY := -LVelocityY;
    LMinX := trunc(FMinScrollLimit.x*ALScreenScale);
    LMaxX := trunc(FMaxScrollLimit.x*ALScreenScale);
    LMinY := trunc(FMinScrollLimit.y*ALScreenScale);
    LMaxY := trunc(FMaxScrollLimit.y*ALScreenScale);
  end
  else if FTouchTracking = [ttVertical] then begin
    LStartX := 0;
    LStartY := trunc(FViewPortPosition.Y*ALScreenScale);
    LVelocityX := 0;
    LVelocityY := -LVelocityY;
    LMinX := 0;
    LMaxX := 0;
    LMinY := trunc(FMinScrollLimit.y*ALScreenScale);
    LMaxY := trunc(FMaxScrollLimit.y*ALScreenScale);
  end
  else if FTouchTracking = [ttHorizontal] then begin
    LStartX := trunc(FViewPortPosition.X*ALScreenScale);
    LStartY := 0;
    LVelocityX := -LVelocityX;
    LVelocityY := 0;
    LMinX := trunc(FMinScrollLimit.x*ALScreenScale);
    LMaxX := trunc(FMaxScrollLimit.x*ALScreenScale);
    LMinY := 0;
    LMaxY := 0;
  end
  else
    exit;

  FUpPosition := TPointF.Create(X, Y);
  FUpVelocity := TPointF.Create(LVelocityX/ALScreenScale, LVelocityY/ALScreenScale);

  {$IFDEF DEBUG}
  //ALLog(
  //  'TALScrollEngine.MouseUp',
  //  'Position:' + ALFormatFloatW('0.##', FUpPosition.x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', FUpPosition.y, ALDefaultFormatSettingsW) + ' | ' +
  //  'Velocity:' + ALFormatFloatW('0.##', FUpVelocity.x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', FUpVelocity.y, ALDefaultFormatSettingsW),
  //TalLogType.verbose);
  {$ENDIF}

  if (not FOverScroller.springBack(
            LStartX, // startX: integer;
            LStartY, // startY: integer;
            LMinX, // minX: integer;
            LMaxX, // maxX: integer;
            LMinY, // minY: integer;
            LMaxY)) then // maxY: integer): Boolean;
    FOverScroller.fling(
      LstartX, // startX: integer;
      LstartY, // startY: integer;
      trunc(LVelocityX), // velocityX: integer;
      trunc(LVelocityY), // velocityY: integer;
      ALIfThen(FMinEdgeSpringbackEnabled, -MaxInt, LMinX), // minX: integer;
      ALIfThen(FMaxEdgeSpringbackEnabled, MaxInt, LMaxX), // maxX: integer;
      ALIfThen(FMinEdgeSpringbackEnabled, -MaxInt, LMinY), // minY: integer;
      ALIfThen(FMaxEdgeSpringbackEnabled, MaxInt, LMaxY)); // maxY: integer

  Starttimer;
end;

{***********************************}
procedure TALScrollEngine.MouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog('TALScrollEngine.MouseLeave', TalLogType.verbose);
  {$ENDIF}
  MouseUp(FLastMotionPos.X, FLastMotionPos.y);
end;

{*************************************************}
procedure TALScrollEngine.MouseWheel(X, Y: Double);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALScrollEngine.MouseWheel',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW),
  //  TalLogType.verbose);
  {$ENDIF}

  if not FOverScroller.isFinished then begin
    FoverScroller.abortAnimation;
    x := x + ((FOverScroller.getCurrX / ALScreenScale) - FViewportPosition.x);
    Y := Y + ((FOverScroller.getCurrY / ALScreenScale) - FViewportPosition.y);
  end;
  if X > 0 then X := Min(X, FMaxScrollLimit.X - FViewportPosition.X)
  else X := Max(X, FMinScrollLimit.X - FViewportPosition.X);
  if Y > 0 then Y := Min(Y, FMaxScrollLimit.Y - FViewportPosition.Y)
  else Y := Max(Y, FMinScrollLimit.Y - FViewportPosition.Y);
  FOverScroller.startScroll(
    trunc(FViewportPosition.x * ALScreenScale), // startX: integer;
    trunc(FViewportPosition.Y * ALScreenScale), // startY: integer;
    trunc(X * ALScreenScale), // dx: integer;
    trunc(Y * ALScreenScale), // dy: integer;
    TALOverScroller.DEFAULT_DURATION); // const duration: integer = DEFAULT_DURATION);

  StartTimer;
end;

initialization
  TALSplineOverScroller.PixelsPerInch := 0;
  TALSplineOverScroller.DECELERATION_RATE := {(Single)} (ln(0.78) / ln(0.9));
  TALSplineOverScroller.InitSpline;
  {$IF (not defined(IOS)) and (not defined(ANDROID))}
  //Unfortunatly, under android (at least) we can not initialize
  //TALScrollEngine.FPlatformTimer in the initialization section
  //else the app crash at startup
  TALScrollEngine.FPlatformTimer := nil;
  {$ENDIF}

end.
