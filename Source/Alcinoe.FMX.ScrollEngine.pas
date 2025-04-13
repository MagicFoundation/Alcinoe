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
  System.Generics.Collections,
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

  {*********************************************}
  // IMPORTANT: Avoid using NativeUInt or UInt32.
  // For example, consider the following C++ code:
  //
  //   uint32_t m;
  //   for (size_t i = 0; i < m; i++) { ... }
  //
  // A direct translation to Delphi might be:
  //
  //   for i: NativeUInt := 0 to m - 1 do
  //
  // This is problematic because if m is 0 then (m - 1) underflows to 4294967296,
  // causing the loop to iterate unexpectedly. Instead, use a signed integer type
  // (or explicitly handle the m = 0 case) to avoid this issue.
  //
  // size_t = NativeUInt
  // ssize_t = NativeInt
  // int32_t = Int32;
  // uint32_t = UInt32
  // nsecs_t = Int64;
  // float = Single;
  // float* = System.PSingle;
  // bool = Boolean;

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
  // Based on Android's VelocityTracker
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if android VelocityTracker was not updated and adjust the IFDEF'}
    //Compare <Alcinoe>\References\Android\VelocityTracker.h with https://android.googlesource.com/platform/frameworks/native/+/refs/heads/main/include/input/VelocityTracker.h
    //Compare <Alcinoe>\References\Android\VelocityTracker.cpp with https://android.googlesource.com/platform/frameworks/native/+/refs/heads/main/libs/input/VelocityTracker.cpp
  {$ENDIF}
  TALVelocityTracker = class(TObject)
  public
    // Maximum pointer id value supported in a motion event.
    // Smallest pointer id is 0.
    // (This is limited by our use of BitSet32 to track pointer assignments.)
    const MAX_POINTER_ID = 31;
    // Axis constant: X axis.
    const AXIS_X = 0; // AMOTION_EVENT_AXIS_X = 0;
    // Axis constant: Y axis.
    const AXIS_Y = 1; // AMOTION_EVENT_AXIS_Y = 1;
    // Axis constant:  Generic scroll axis of a motion event.
    // - This is used for scroll axis motion events that can't be classified as strictly
    //   vertical or horizontal. The movement of a rotating scroller is an example of this.
    const AXIS_SCROLL = 2; // AMOTION_EVENT_AXIS_SCROLL = 26;
    // Note: This is not an "Axis constant". It does not represent any axis, nor should it be used
    // to represent any axis. It is a constant holding the value of the largest defined axis value,
    // to make some computations (like iterating through all possible axes) cleaner.
    // Please update the value accordingly if you add a new axis.
    const MAXIMUM_VALID_AXIS_VALUE = AXIS_SCROLL; // AMOTION_EVENT_MAXIMUM_VALID_AXIS_VALUE = AMOTION_EVENT_AXIS_GESTURE_SWIPE_FINGER_COUNT
  public
    const MAX_DEGREE{: size_t} = 4;
  public
    Type
      TStrategy = (
        // Use the default Velocity Tracker Strategy. Different axes may use different default
        // strategies.
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
        // 2nd order weighted least squares, central weighting.  Quality: EXPERIMENTALe
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
  private
    FLastEventTime: Int64;
    FCurrentPointerIdBits: TALBitSet32;
    FActivePointerId: Int32;
    // An override strategy passed in the constructor to be used for all axes.
    // This strategy will apply to all axes, unless the default strategy is specified here.
    // When default strategy is specified, then each axis will use a potentially different strategy
    // based on a hardcoded mapping.
    FOverrideStrategy: TStrategy;
    // Maps axes to their respective VelocityTrackerStrategy instances.
    // Note that, only axes that have had MotionEvents (and not all supported axes) will be here.
    FConfiguredStrategies: Array[0..MAXIMUM_VALID_AXIS_VALUE{axis}] of TALVelocityTrackerStrategy;
    procedure configureStrategy(axis: Int32);
    // Generates a VelocityTrackerStrategy instance for the given Strategy type.
    // The `deltaValues` parameter indicates whether or not the created strategy should treat motion
    // values as deltas (and not as absolute values). This the parameter is applicable only for
    // strategies that support differential axes.
    class function createStrategy(const strategy: TStrategy; deltaValues: Boolean): TALVelocityTrackerStrategy;
  public
    // Creates a velocity tracker using the specified strategy for each supported axis.
    // If strategy is not provided, uses the default strategy for the platform.
    // TODO(b/32830165): support axis-specific strategies.
    constructor Create(const strategy: TStrategy = TStrategy.DEFAULT); virtual;
    destructor Destroy; override;
    // Return true if the axis is supported for velocity tracking, false otherwise.
    class function isAxisSupported(axis: Int32): Boolean;
    // Resets the velocity tracker state.
    procedure clear;
    // Resets the velocity tracker state for a specific pointer.
    // Call this method when some pointers have changed and may be reusing
    // an id that was assigned to a different pointer earlier.
    procedure clearPointer(pointerId: Int32);
    // Adds movement information for a pointer for a specific axis
    procedure addMovement(eventTime: Int64; pointerId: Int32; axis: Int32; position: Single);
    // Returns the velocity of the specified pointer id and axis in position units per second.
    // Returns empty optional if there is insufficient movement information for the pointer, or if
    // the given axis is not supported for velocity tracking.
    function getVelocity(axis: Int32; pointerId: Int32): Single;
    // Gets the active pointer id, or -1 if none.
    function getActivePointerId: Int32; inline;
  end;

  {****************************************************}
  // Implements a particular velocity tracker algorithm.
  // Based on Android's VelocityTrackerStrategy
  TALVelocityTrackerStrategy = class(TObject)
  public
    procedure clear; virtual; abstract;
    procedure clearPointer(pointerId: Int32); virtual; abstract;
    procedure addMovement(eventTime: Int64; pointerId: Int32; position: Single); virtual; abstract;
    function getVelocity(pointerId: Int32): Single; virtual; abstract;
  end;

  {**************************************************************************************************}
  // A `VelocityTrackerStrategy` that accumulates added data points and processes the accumulated data
  // points when getting velocity.
  // Based on Android's AccumulatingVelocityTrackerStrategy
  TALAccumulatingVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  protected
    type
      TMovement = record
        eventTime: Int64;
        position: Single;
      end;
  protected
    // Number of samples to keep.
    // If different strategies would like to maintain different history size, we can make this a
    // protected const field.
    const HISTORY_SIZE: Int32{uint32_t} = 20;
  protected
    // Duration, in nanoseconds, since the latest movement where a movement may be considered for
    // velocity calculation.
    FHorizonNanos: Int64;
    // If true, data points outside of horizon (see `mHorizonNanos`) will be cleared after each
    // addition of a new movement.
    FMaintainHorizonDuringAdd: Boolean;
    FMovements: Array[0..TALVelocityTracker.MAX_POINTER_ID] of TALRingBuffer<TMovement>;
  public
    constructor Create(horizonNanos: Int64; maintainHorizonDuringAdd: Boolean); virtual;
    destructor Destroy; override;
    procedure addMovement(eventTime: Int64; pointerId: Int32; position: Single); override;
    procedure clear; override;
    procedure clearPointer(pointerId: Int32); override;
  end;

  {*********************************************************************}
  // Velocity tracker algorithm based on least-squares linear regression.
  // Based on Android's LeastSquaresVelocityTrackerStrategy
  TALLeastSquaresVelocityTrackerStrategy = class(TALAccumulatingVelocityTrackerStrategy)
  public
    type
      TWeighting = (
        // No weights applied.  All data points are equally reliable.
        NONE,
        // Weight by time delta.  Data points clustered together are weighted less.
        DELTA,
        // Weight such that points within a certain horizon are weighed more than those
        // outside of that horizon.
        CENTRAL,
        // Weight such that points older than a certain amount are weighed less.
        RECENT);
  private
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    const HORIZON: Int64 = 100 * 1000000; // 100 ms
  private
    FDegree: Int32{uint32_t};
    FWeighting: TWeighting;
    function chooseWeight(pointerId: Int32; index: Int32{uint32_t}): Single;
    // An optimized least-squares solver for degree 2 and no weight (i.e. `Weighting.NONE`).
    // The provided container of movements shall NOT be empty, and shall have the movements in
    // chronological order.
    function solveUnweightedLeastSquaresDeg2(const movements: TALRingBuffer<TALAccumulatingVelocityTrackerStrategy.TMovement>): Single;
  public
    // Degree must be no greater than VelocityTracker::MAX_DEGREE.
    constructor Create(degree: Int32{uint32_t}; weighting: TWeighting = TWeighting.NONE); reintroduce; virtual;
    function getVelocity(pointerId: Int32): Single; override;
  end;

  {****************************************************}
  // Velocity tracker algorithm that uses an IIR filter.
  // Based on Android's IntegratingVelocityTrackerStrategy
  TALIntegratingVelocityTrackerStrategy = class(TALVelocityTrackerStrategy)
  private
    type
      // Current state estimate for a particular pointer.
      TState = record
        updateTime: Int64;
        degree: Int32{uint32_t};
        pos, vel, accel: Single;
      end;
  private
    FDegree: Int32{uint32_t};
    FPointerIdBits: TALBitSet32;
    FPointerState: Array[0..TALVelocityTracker.MAX_POINTER_ID] of TState;
    procedure initState(var state: TState; eventTime: Int64; pos: Single);
    procedure updateState(var state: TState; eventTime: Int64; pos: Single);
  public
    // Degree must be 1 or 2.
    constructor Create(degree: Int32{uint32_t}); virtual;
    procedure clear; override;
    procedure clearPointer(pointerId: Int32); override;
    procedure addMovement(eventTime: Int64; pointerId: Int32; position: Single); override;
    function getVelocity(pointerId: Int32): Single; override;
  end;

  {*********************************************}
  // Velocity tracker strategy used prior to ICS.
  // Based on Android's LegacyVelocityTrackerStrategy
  TALLegacyVelocityTrackerStrategy = class(TALAccumulatingVelocityTrackerStrategy)
  private
    // Oldest sample to consider when calculating the velocity.
    const HORIZON: Int64 = 200 * 1000000; // 100 ms
    // The minimum duration between samples when estimating velocity.
    const MIN_DURATION: Int64 = 10 * 1000000; // 10 ms
  public
    constructor Create; reintroduce; virtual;
    function getVelocity(pointerId: Int32): Single; override;
  end;

  {**************************************************}
  // Based on Android's ImpulseVelocityTrackerStrategy
  TALImpulseVelocityTrackerStrategy = class(TALAccumulatingVelocityTrackerStrategy)
  private
    // Sample horizon.
    // We don't use too much history by default since we want to react to quick
    // changes in direction.
    const HORIZON: Int64 = 100 * 1000000; // 100 ms
  private
    // Whether or not the input movement values for the strategy come in the form of delta values.
    // If the input values are not deltas, the strategy needs to calculate deltas as part of its
    // velocity calculation.
    FDeltaValues: Boolean;
  public
    constructor Create(deltaValues: Boolean); reintroduce; virtual;
    function getVelocity(pointerId: Int32): Single; override;
  end;

  {****************************************}
  // Based on the Android OverScroller class
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
    function getFriction: Single;
    procedure setFriction(friction: Single);
    procedure updateScroll(q: Single; q2: Single);
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
  // Based on the Android OverScroller class
  // https://www.sobyte.net/post/2022-02/android-over-scroller/
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if android OverScroller was not updated and adjust the IFDEF'}
    //Compare <Alcinoe>\References\Android\OverScroller.java with <SDKs>\android\sources\android-XX\android\widget\OverScroller.java
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
    function getFriction: Single;
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
    function getSplineFlingDistance(velocity: integer): double;
  end;

  {**********************************}
  TALScrollEngine = class(TPersistent)

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

  private
    procedure doFrame(frameTimeNanos: Int64);

  {$ELSE}

  private
    class var FPlatformTimer: IFMXTimerService;
    class procedure InitPlatformTimer; inline;
  private
    FTimerHandle: TFmxHandle;

  {$ENDIF}

  private
    type
      TMovement = record
        eventTime: Int64;
        position: TPointF;
      end;
  public
    type
      TTouchMode = (Disabled, Enabled, Auto);
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
    FTouchMode: TTouchMode;
    FTouchTracking: TTouchTracking;
    FMinScrollLimit: TALPointD;
    FMaxScrollLimit: TALPointD;
    FOnStart: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FMovements: TList<TMovement>;
    FViewportPosition: TALPointD;
    FLastMotionPos: TPointF;
    FDown: Boolean;
    FDownPosition: TPointF;
    FUpPosition: TPointF;
    FUpVelocity: TPointF;
    FMoved: Boolean;
    FTag: NativeInt;
    procedure StartTimer;
    procedure StopTimer(const AAbruptly: Boolean = False);
    {$IF (not defined(IOS)) and (not defined(ANDROID))}
    procedure TimerProc;
    {$ENDIF}
    procedure SetTimerInterval(const Value: Integer);
    procedure SetDown(const Value: Boolean);
    function IsTouchSlopStored: Boolean;
    function IsOverflingDistanceStored: Boolean;
    function IsDragResistanceFactorStored: Boolean;
    function IsFrictionStored: Boolean;
    function GetFriction: Single;
    procedure SetFriction(const Value: Single);
    function GetCurrentVelocity: TPointF;
    function GetIsVelocityLow: Boolean;
    function GetTouchEnabled: Boolean;
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
    procedure DoMouseDown; virtual;
    procedure DoMouseMove; virtual;
    procedure DoMouseUp; virtual;
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
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Calculate: boolean;
    /// <summary>
    ///   Specifies the duration between offset recalculations. We don't use
    ///   TimerInterval for Android/iOS since we utilize DisplayLink and JChoreographer
    /// </summary>
    property TimerInterval: Integer read FTimerInterval write SetTimerInterval;
    property TimerActive: boolean read FTimerActive;
    procedure startScroll(startX: Double; startY: Double; dx: Double; dy: Double; const duration: integer = TALOverScroller.DEFAULT_DURATION);
    procedure MouseDown(X, Y: Single); overload;
    procedure MouseDown(const AHandle: TWindowHandle); overload;
    procedure MouseMove(X, Y: Single); overload;
    procedure MouseMove(const AHandle: TWindowHandle); overload;
    procedure MouseUp(X, Y: Single); overload;
    procedure MouseUp(const AHandle: TWindowHandle); overload;
    procedure MouseLeave;
    procedure MouseWheel(X, Y: Double); virtual;
    /// <summary>
    ///   Halts the ongoing animation, freezing the scroller at its current position
    ///   without completing the remaining motion.
    /// </summary>
    procedure Stop(const AAbruptly: Boolean = False);
    property TouchTracking: TTouchTracking read FTouchTracking write FTouchTracking;
    property TouchEnabled: Boolean read GetTouchEnabled;
    /// <summary>
    ///   In virtual pixels per second.
    /// </summary>
    property CurrentVelocity: TPointF read GetCurrentVelocity;
    property IsVelocityLow: Boolean read GetIsVelocityLow;
    property ViewportPosition: TALPointD read FViewportPosition write SetViewportPosition;
    property MinScrollLimit: TALPointD read FMinScrollLimit write SetMinScrollLimit;
    property MaxScrollLimit: TALPointD read FMaxScrollLimit write SetMaxScrollLimit;
    property Down: Boolean read FDown write SetDown;
    property DownPosition: TPointF read fDownPosition;
    property UpPosition: TPointF read FUpPosition;
    property UpVelocity: TPointF read FUpVelocity;
    /// <summary>
    ///   The Moved flag is activated when the mouse is dragged beyond the
    ///   TouchSlop distance following a mousedown event.
    /// </summary>
    property Moved: Boolean read FMoved;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property Tag: NativeInt read FTag write FTag;
  published
    property TouchMode: TTouchMode read FTouchMode write FTouchMode default TTouchMode.Auto;
    /// <summary>
    ///   Distance a touch can wander before we think the user is scrolling
    /// </summary>
    property TouchSlop: Single read FTouchSlop write FTouchSlop stored IsTouchSlopStored nodefault;
    /// <summary>
    ///   Max distance to overfling for edge effects
    /// </summary>
    property OverflingDistance: Single read FOverflingDistance write FOverflingDistance stored IsOverflingDistanceStored nodefault;
    /// <summary>
    ///   A multiplier applied during drag operations to simulate resistance. A value
    ///   of 1 implies normal drag behavior, while values below 1 introduce increased
    ///   resistance, making the drag feel heavier.
    /// </summary>
    property DragResistanceFactor: Single read FDragResistanceFactor write FDragResistanceFactor stored IsDragResistanceFactorStored nodefault;
    property MinEdgeSpringbackEnabled: Boolean read FMinEdgeSpringbackEnabled write FMinEdgeSpringbackEnabled default true;
    property MaxEdgeSpringbackEnabled: Boolean read FMaxEdgeSpringbackEnabled write FMaxEdgeSpringbackEnabled default true;
    /// <summary>
    ///   The amount of friction applied to flings.
    /// </summary>
    property Friction: Single read GetFriction write SetFriction stored IsFrictionStored nodefault;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IFDEF ANDROID}
  Androidapi.Input,
  FMX.Platform.Android,
  FMX.Platform.UI.Android,
  Alcinoe.Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.UIKit,
  Macapi.Helpers,
  Macapi.ObjCRuntime,
  FMX.Platform.iOS,
  Alcinoe.iOSapi.UIKit,
  Alcinoe.iOSapi.QuartzCore,
  {$ENDIF}
  FMX.Platform,
  Alcinoe.FMX.Common,
  Alcinoe.StringUtils,
  Alcinoe.FMX.Ani;

{$IF DEFINED(DEBUG)}

  // Log debug messages about velocity tracking.
  {.$DEFINE DEBUG_VELOCITY}

  // Log debug messages about the progress of the algorithm itself.
  {.$DEFINE DEBUG_STRATEGY}

  // Log debug messages about the 'impulse' strategy.
  {.$DEFINE DEBUG_IMPULSE}

{$ENDIF}

// All axes supported for velocity tracking, mapped to their default strategies.
// Although other strategies are available for testing and comparison purposes,
// the default strategy is the one that applications will actually use.  Be very careful
// when adjusting the default strategy because it can dramatically affect
// (often in a bad way) the user experience.
const DEFAULT_STRATEGY_BY_AXIS: array[0..TALVelocityTracker.MAXIMUM_VALID_AXIS_VALUE] of TALVelocityTracker.TStrategy = (
                                  TALVelocityTracker.TStrategy.LSQ2, // TALVelocityTracker.AXIS_X = 0
                                  TALVelocityTracker.TStrategy.LSQ2, // TALVelocityTracker.AXIS_Y = 1
                                  TALVelocityTracker.TStrategy.IMPULSE); // TALVelocityTracker.AXIS_SCROLL = 2

// Axes specifying location on a 2D plane (i.e. X and Y).
const PLANAR_AXES: set of Byte = [TALVelocityTracker.AXIS_X, TALVelocityTracker.AXIS_Y];

// Axes whose motion values are differential values (i.e. deltas).
const DIFFERENTIAL_AXES: set of Byte = [TALVelocityTracker.AXIS_SCROLL];

// Threshold for determining that a pointer has stopped moving.
// Some input devices do not send ACTION_MOVE events in the case where a pointer has
// stopped.  We need to detect this case so that we can accurately predict the
// velocity after the pointer starts moving again.
const ASSUME_POINTER_STOPPED_TIME = 40 * ALNanosPerMs;

{*********************************************************************}
function vectorDot(a: PSingle; b: PSingle; m: Int32{uint32_t}): Single;
begin
  var r: Single := 0;
  for var i{: size_t} := 0 to m - 1 do begin
    r := r + (a^ * b^);
    inc(a);
    inc(b);
  end;
  result := r;
end;

{**********************************************************}
function vectorNorm(a: PSingle; m: Int32{uint32_t}): Single;
begin
  var r: Single := 0;
  for var i{: size_t} := 0 to m - 1 do begin
    var t: Single := a^;
    r := r + (t * t);
    inc(a);
  end;
  result := sqrt(r);
end;

{********************************************************}
function vectorToString(const a: Array of Single): String;
begin
  var str: String := '';
  str := str + '[';
  for var i{: size_t} := 0 to length(a) - 1 do begin
    if i > 0 then
      str := str + ',';
    str := str + ' ' + ALFloatToStrW(a[i], ALDefaultFormatSettingsW);
  end;
  str := str + ' ]';
  Result := str;
end;

{**********************************************************************************}
function matrixToString(const a: TArray<TArray<Single>>; rowMajor: Boolean): String;
begin
  var str: string := '';
  str := str + '[';
  if length(a) > 0 then begin
    if RowMajor then begin
      for var i{: size_t} := 0 to length(a) - 1 do begin
        if i > 0 then
          str := str + ',';
        str := str + ' [';
        for var j{: size_t} := 0 to length(a[i]) - 1 do begin
          if j > 0 then
            str := str + ',';
          str := str + ALFloatToStrW(a[i][j], ALDefaultFormatSettingsW)
        end;
        str := str + ' ]';
      end;
    end
    else begin
      for var i{: size_t} := 0 to length(a[0]) - 1 do begin
        if i > 0 then
          str := str + ',';
        str := str + ' [';
        for var j{: size_t} := 0 to length(a) - 1 do begin
          if j > 0 then
            str := str + ',';
          str := str + ALFloatToStrW(a[j][i], ALDefaultFormatSettingsW)
        end;
        str := str + ' ]';
      end;
    end;
  end;
  str := str + ' ]';
  Result := str;
end;

{***********************************************************************************}
constructor TALVelocityTracker.Create(const strategy: TStrategy = TStrategy.DEFAULT);
begin
  inherited Create;
  FLastEventTime := 0;
  FCurrentPointerIdBits.Clear;
  FOverrideStrategy := strategy;
  FActivePointerId := -1;
  for var I := Low(FConfiguredStrategies) to High(FConfiguredStrategies) do
    FConfiguredStrategies[I] := nil;
end;

{************************************}
destructor TALVelocityTracker.Destroy;
begin
  for var I := Low(FConfiguredStrategies) to High(FConfiguredStrategies) do
    ALFreeAndNil(FConfiguredStrategies[I]);
  inherited;
end;

{**********************************************************************}
class function TALVelocityTracker.isAxisSupported(axis: Int32): Boolean;
begin
  result := axis in [low(DEFAULT_STRATEGY_BY_AXIS)..high(DEFAULT_STRATEGY_BY_AXIS)];
end;

{**********************************************************}
procedure TALVelocityTracker.configureStrategy(axis: Int32);
begin
  const isDifferentialAxis: Boolean = axis in DIFFERENTIAL_AXES;
  if (isDifferentialAxis or (FOverrideStrategy = TALVelocityTracker.TStrategy.DEFAULT)) then
    // Do not allow overrides of strategies for differential axes, for now.
    FConfiguredStrategies[axis] := createStrategy(DEFAULT_STRATEGY_BY_AXIS[axis], {deltaValues=}isDifferentialAxis)
  else
    FConfiguredStrategies[axis] := createStrategy(FOverrideStrategy, {deltaValues=}false);
end;

{****************************************************************************************************************************}
class function TALVelocityTracker.createStrategy(const strategy: TStrategy; deltaValues: Boolean): TALVelocityTrackerStrategy;
begin
  case strategy of

    TALVelocityTracker.TStrategy.IMPULSE: begin
      {$IF defined(DEBUG_STRATEGY)}
      ALLog('Alcinoe.FMX.ScrollEngine.TALVelocityTracker', 'Initializing impulse strategy');
      {$ENDIF}
      Result := TALImpulseVelocityTrackerStrategy.Create(deltaValues);
    end;

    TALVelocityTracker.TStrategy.LSQ1:
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(1);

    TALVelocityTracker.TStrategy.LSQ2: begin
      {$IF defined(DEBUG_STRATEGY) and (not defined(DEBUG_IMPULSE))}
      ALOG('TALVelocityTracker', 'Initializing lsq2 strategy');
      {$ENDIF}
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(2);
    end;

    TALVelocityTracker.TStrategy.LSQ3:
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(3);

    TALVelocityTracker.TStrategy.WLSQ2_DELTA:
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(2,TALLeastSquaresVelocityTrackerStrategy.TWeighting.DELTA);

    TALVelocityTracker.TStrategy.WLSQ2_CENTRAL:
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(2,TALLeastSquaresVelocityTrackerStrategy.TWeighting.CENTRAL);

    TALVelocityTracker.TStrategy.WLSQ2_RECENT:
      Result := TALLeastSquaresVelocityTrackerStrategy.Create(2,TALLeastSquaresVelocityTrackerStrategy.TWeighting.RECENT);

    TALVelocityTracker.TStrategy.INT1:
      Result := TALIntegratingVelocityTrackerStrategy.Create(1);

    TALVelocityTracker.TStrategy.INT2:
      Result := TALIntegratingVelocityTrackerStrategy.Create(2);

    TALVelocityTracker.TStrategy.LEGACY:
      Result := TALLegacyVelocityTrackerStrategy.Create();

    else
      raise Exception.Create('Invalid strategy');

  end;
end;

{*********************************}
procedure TALVelocityTracker.clear;
begin
  FCurrentPointerIdBits.clear;
  FActivePointerId := -1;
  for var I := Low(FConfiguredStrategies) to High(FConfiguredStrategies) do
    if FConfiguredStrategies[I] <> nil then
      FConfiguredStrategies[I].clear;
end;

{**********************************************************}
procedure TALVelocityTracker.clearPointer(pointerId: Int32);
begin
  FCurrentPointerIdBits.clearBit(pointerId);

  if ((FActivePointerId <> -1) and (FActivePointerId = pointerId)) then begin
    // The active pointer id is being removed. Mark it invalid and try to find a new one
    // from the remaining pointers.
    FActivePointerId := -1;
    if (not FCurrentPointerIdBits.isEmpty()) then
      FActivePointerId := FCurrentPointerIdBits.firstMarkedBit();
  end;

  for var I := Low(FConfiguredStrategies) to High(FConfiguredStrategies) do
    if FConfiguredStrategies[I] <> nil then
      FConfiguredStrategies[I].clearPointer(pointerId);
end;

{**********************************************************************************************************}
procedure TALVelocityTracker.addMovement(eventTime: Int64; pointerId: Int32; axis: Int32; position: Single);
begin
  if ((pointerId < 0) or (pointerId > MAX_POINTER_ID)) then
    Raise Exception.CreateFmt('Invalid pointer ID %d for axis %d', [pointerId, axis]);

  if (FCurrentPointerIdBits.hasBit(pointerId)) and
     (eventTime - FLastEventTime > ASSUME_POINTER_STOPPED_TIME) then begin
    {$IF defined(DEBUG_VELOCITY)}
    ALLOG('Alcinoe.FMX.ScrollEngine.TALVelocityTracker', 'VelocityTracker: stopped for %.2f ms, clearing state.', [(eventTime - FLastEventTime) / ALNanosPerMs]);
    {$ENDIF}

    // We have not received any movements for too long.  Assume that all pointers
    // have stopped.
    for var I := Low(FConfiguredStrategies) to High(FConfiguredStrategies) do
      if FConfiguredStrategies[I] <> nil then
        FConfiguredStrategies[I].clear;
  end;
  FLastEventTime := eventTime;

  FCurrentPointerIdBits.markBit(pointerId);
  if (FActivePointerId = -1) then
    // Let this be the new active pointer if no active pointer is currently set
    FActivePointerId := pointerId;

  if (FConfiguredStrategies[axis] = nil) then
    configureStrategy(axis);
  FConfiguredStrategies[axis].addMovement(eventTime, pointerId, position);

  {$IF defined(DEBUG_VELOCITY)}
  var OldDisableLog := ALDisableLog;
  ALDisableLog := true;
  var velocity: Single := getVelocity(axis, pointerId);
  ALDisableLog := OldDisableLog;
  ALLOG(
    'Alcinoe.FMX.ScrollEngine.TALVelocityTracker',
    'VelocityTracker: addMovement axis=%d, eventTime=%d, pointerId=%d, activePointerId=%d, position=%.2f, velocity=%.2f',
    [axis, eventTime, pointerId, FActivePointerId, position, velocity]);
  {$ENDIF}
end;

{*****************************************************************************}
function TALVelocityTracker.getVelocity(axis: Int32; pointerId: Int32): Single;
begin
  if FConfiguredStrategies[axis] <> nil then
    result := FConfiguredStrategies[axis].getVelocity(pointerId)
  else
    result := 0;
end;

{****************************************************}
function TALVelocityTracker.getActivePointerId: Int32;
begin
  Result := FActivePointerId;
end;

{****************************************************************************************************************}
constructor TALAccumulatingVelocityTrackerStrategy.Create(horizonNanos: Int64; maintainHorizonDuringAdd: Boolean);
begin
  inherited Create;
  FHorizonNanos := horizonNanos;
  FMaintainHorizonDuringAdd := maintainHorizonDuringAdd;
  for var I := Low(FMovements) to High(FMovements) do
    FMovements[I] := nil;
end;

{********************************************************}
destructor TALAccumulatingVelocityTrackerStrategy.Destroy;
begin
  for var I := Low(FMovements) to High(FMovements) do
    ALFreeAndNil(FMovements[I]);
  inherited;
end;

{*****************************************************}
procedure TALAccumulatingVelocityTrackerStrategy.clear;
begin
  for var I := Low(FMovements) to High(FMovements) do
    if FMovements[I] <> nil then
      FMovements[I].Clear;
end;

{******************************************************************************}
procedure TALAccumulatingVelocityTrackerStrategy.clearPointer(pointerId: Int32);
begin
  if FMovements[pointerId] <> nil then
    FMovements[pointerId].Clear;
end;

{*****************************************************************************************************************}
procedure TALAccumulatingVelocityTrackerStrategy.addMovement(eventTime: Int64; pointerId: Int32; position: Single);
begin
  If FMovements[pointerId] = nil then
    FMovements[pointerId] := TALRingBuffer<TMovement>.Create(HISTORY_SIZE);
  var movements := FMovements[pointerId];
  const size{: size_t} = movements.Count;

  if ((size <> 0) and (movements[size - 1].eventTime = eventTime)) then
    // When ACTION_POINTER_DOWN happens, we will first receive ACTION_MOVE with the coordinates
    // of the existing pointers, and then ACTION_POINTER_DOWN with the coordinates that include
    // the new pointer. If the eventtimes for both events are identical, just update the data
    // for this time (i.e. pop out the last element, and insert the updated movement).
    // We only compare against the last value, as it is likely that addMovement is called
    // in chronological order as events occur.
    movements.popBack();

  var LMovement: TMovement;
  LMovement.eventTime := eventTime;
  LMovement.position := position;
  movements.pushBack(LMovement);

  // Clear movements that do not fall within `mHorizonNanos` of the latest movement.
  // Note that, if in the future we decide to use more movements (i.e. increase HISTORY_SIZE),
  // we can consider making this step binary-search based, which will give us some improvement.
  if (FMaintainHorizonDuringAdd) then
    while (eventTime - movements[0].eventTime > FHorizonNanos) do
      movements.popFront();
end;

{**************************************************************************************************************************}
constructor TALLeastSquaresVelocityTrackerStrategy.Create(degree: Int32{uint32_t}; weighting: TWeighting = TWeighting.NONE);
begin
  inherited create(HORIZON{horizonNanos}, true{maintainHorizonDuringAdd});
  FDegree := degree;
  FWeighting := weighting;
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
function solveLeastSquares(
           const x: TArray<Single>;
           const y: TArray<Single>;
           const w: TArray<Single>;
           n: Int32{uint32_t}): Single;
begin
  const m{: size_t} = length(x);

  {$IF defined(DEBUG_STRATEGY)}
  ALLog(
    'Alcinoe.FMX.ScrollEngine.solveLeastSquares',
    'solveLeastSquares: m=%d, n=%d, x=%s, y=%s, w=%s',
    [m, n, vectorToString(x), vectorToString(y), vectorToString(w)]);
  {$ENDIF}

  if (m <> length(y)) or (m <> length(w)) then
    raise Exception.Create('Mismatched vector sizes');

  // Expand the X vector to a matrix A, pre-multiplied by the weights.
  var a: TArray<TArray<Single>>; // column-major order
  Setlength(a,n,m);
  for var h{: uint32_t} := 0 to m - 1 do begin
    a[0][h] := w[h];
    for var i{: uint32_t} := 1 to n - 1 do
      a[i][h] := a[i - 1][h] * x[h];
  end;

  {$IF defined(DEBUG_STRATEGY)}
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - a=%s', [matrixToString(a, {rowMajor=}false)]);
  {$ENDIF}

  // Apply the Gram-Schmidt process to A to obtain its QR decomposition.
  var q: TArray<TArray<Single>>; // orthonormal basis, column-major order
  var r: TArray<TArray<Single>>; // upper triangular matrix, row-major order
  Setlength(q,n,m);
  Setlength(r,n,n);
  for var j{: uint32_t} := 0 to n - 1 do begin
    for var h{: uint32_t} := 0 to m - 1 do
      q[j][h] := a[j][h];
    for var i{: uint32_t} := 0 to j - 1 do begin
      var dot: Single := vectorDot(@q[j][0], @q[i][0], m);
      for var h{: uint32_t} := 0 to m - 1 do
        q[j][h] := q[j][h] - (dot * q[i][h]);
    end;

    var norm: Single := vectorNorm(@q[j][0], m);
    if (norm < 0.000001) then begin
      // vectors are linearly dependent or zero so no solution
      {$IF defined(DEBUG_STRATEGY)}
      ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - no solution, norm=%.7f', [norm]);
      {$ENDIF}
      exit(0);
    end;

    var invNorm: Single := 1.0 / norm;
    for var h{: uint32_t} := 0 to m - 1 do
      q[j][h] := q[j][h] * invNorm;
    for var i{: uint32_t} := 0 to n - 1 do begin
      if i < j then
        r[j][i] := 0
      else
        r[j][i] := vectorDot(@q[j][0], @a[i][0], m);
    end;
  end;
  {$IF defined(DEBUG_STRATEGY)}
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - q=%s', [matrixToString(q, {rowMajor=}false)]);
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - r=%s', [matrixToString(r, {rowMajor=}true)]);

  // calculate QR, if we factored A correctly then QR should equal A
  var qr: TArray<TArray<Single>>;
  Setlength(qr,n,m);
  for var h{: uint32_t} := 0 to m - 1 do begin
    for var i{: uint32_t} := 0 to n - 1 do begin
      qr[i][h] := 0;
      for var j{: uint32_t} := 0 to n - 1 do
        qr[i][h] := qr[i][h] + (q[j][h] * r[j][i]);
    end;
  end;
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - qr=%s', [matrixToString(qr, {rowMajor=}false)]);
  {$ENDIF}

  // Solve R B = Qt W Y to find B.  This is easy because R is upper triangular.
  // We just work from bottom-right to top-left calculating B's coefficients.
  var wy: TArray<Single>;
  setlength(wy,m);
  for var h{: uint32_t} := 0 to m - 1 do
    wy[h] := y[h] * w[h];
  var outB: array[0..TALVelocityTracker.MAX_DEGREE] of Single;
  for var i{: uint32_t} := n - 1 downto 0 do begin
    outB[i] := vectorDot(@q[i][0], @wy[0], m);
    for var j{: uint32_t} := n - 1 downto i + 1 do
      outB[i] := outB[i] - (r[i][j] * outB[j]);
    outB[i] := outB[i] / r[i][i];
  end;

  {$IF defined(DEBUG_STRATEGY)}
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - b=%s', [vectorToString(outB)]);
  {$ENDIF}

  {$IF defined(DEBUG_STRATEGY)}
  // Calculate the coefficient of determination as 1 - (SSerr / SStot) where
  // SSerr is the residual sum of squares (variance of the error),
  // and SStot is the total sum of squares (variance of the data) where each
  // has been weighted.
  var ymean: Single := 0;
  for var h{: uint32_t} := 0 to m - 1 do
    ymean := ymean + y[h];
  ymean := ymean / m;

  var sserr: Single := 0;
  var sstot: Single := 0;
  for var h{: uint32_t} := 0 to m - 1 do begin
    var err: Single := y[h] - outB[0];
    var term: Single := 1;
    for var i{: uint32_t} := 1 to n - 1 do begin
      term := term * x[h];
      err := err - (term * outB[i]);
    end;
    sserr := sserr + (w[h] * w[h] * err * err);
    var &var: Single := y[h] - ymean;
    sstot := sstot + (w[h] * w[h] * &var * &var);
  end;
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - sserr=%.7f', [sserr]);
  ALLog('Alcinoe.FMX.ScrollEngine.solveLeastSquares', '  - sstot=%.7f', [sstot]);
  {$ENDIF}

  result := outB[1];
end;

{********************************************************************************************}
// Optimized unweighted second-order least squares fit. About 2x speed improvement compared to
// the default implementation
function TALLeastSquaresVelocityTrackerStrategy.solveUnweightedLeastSquaresDeg2(const movements: TALRingBuffer<TALAccumulatingVelocityTrackerStrategy.TMovement>): Single;
begin
  // Solving y = a*x^2 + b*x + c, where
  //      - "x" is age (i.e. duration since latest movement) of the movemnets
  //      - "y" is positions of the movements.
  var sxi: Single := 0;
  var sxiyi: Single := 0;
  var syi: Single := 0;
  var sxi2: Single := 0;
  var sxi3: Single := 0;
  var sxi2yi: Single := 0;
  var sxi4: Single := 0;

  const count{: size_t} = movements.count;
  const newestMovement: TMovement = movements[count - 1];
  for var i{: size_t} := 0 to count - 1 do begin
    const movement: TMovement = movements[i];
    var age: Int64 := newestMovement.eventTime - movement.eventTime;
    var xi: Single := -age * ALSecondsPerNano;
    var yi: Single := movement.position;

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
  if (denominator = 0) then begin
    ALLog(
      'Alcinoe.FMX.ScrollEngine.TALLeastSquaresVelocityTrackerStrategy.solveUnweightedLeastSquaresDeg2',
      'division by 0 when computing velocity, Sxx=%.7f, Sx2x2=%.7f, Sxx2=%.7f',
      [Sxx, Sx2x2, Sxx2],
      TALLogtype.WARN);
    exit(0);
  end;

  Result := (Sxy * Sx2x2 - Sx2y * Sxx2) / denominator;
end;

{************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.getVelocity(pointerId: Int32): Single;
begin
  if (FMovements[pointerId] = nil) then
    exit(0); // no data

  const movements = FMovements[pointerId];
  const size{: size_t} = movements.count;
  if (size = 0) then
    exit(0); // no data

  var degree: Int32{uint32_t} := FDegree;
  if (degree > size - 1) then
    degree := size - 1;

  if (degree <= 0) then
    exit(0);

  if ((degree = 2) and (FWeighting = TWeighting.NONE)) then
    // Optimize unweighted, quadratic polynomial fit
    exit(solveUnweightedLeastSquaresDeg2(movements));

  // Iterate over movement samples in reverse time order and collect samples.
  var positions: TArray<Single>;
  var w: TArray<Single>;
  var time: TArray<Single>;
  setlength(positions, size);
  setlength(w, size);
  setlength(time, size);

  const newestMovement: TMovement = movements[size - 1];
  for var i{: ssize_t} := size - 1 downto 0 do begin
    const movement: TMovement = movements[i];
    var age: Int64 := newestMovement.eventTime - movement.eventTime;
    positions[Size - 1 - i] := movement.position;
    w[Size - 1 - i] := chooseWeight(pointerId, i);
    time[Size - 1 - i] := -age * 0.000000001;
  end;

  // General case for an Nth degree polynomial fit
  Result := solveLeastSquares(time, positions, w, degree + 1);
end;

{*************************************************************************************************************}
function TALLeastSquaresVelocityTrackerStrategy.chooseWeight(pointerId: Int32; index: Int32{uint32_t}): Single;
begin
  const movements = FMovements[pointerId];
  const size{: size_t} = movements.count;
  case FWeighting of
    TWeighting.DELTA: begin
      // Weight points based on how much time elapsed between them and the next
      // point so that points that "cover" a shorter time span are weighed less.
      //   delta  0ms: 0.5
      //   delta 10ms: 1.0
      if (index = size - 1) then
        exit(1.0);
      var deltaMillis: Single := (movements[index + 1].eventTime - movements[index].eventTime) * 0.000001;
      if (deltaMillis < 0) then
        exit(0.5);
      if (deltaMillis < 10) then
        exit(0.5 + deltaMillis * 0.05);
      Result := 1.0;
    end;

    TWeighting.CENTRAL: begin
      // Weight points based on their age, weighing very recent and very old points less.
      //   age  0ms: 0.5
      //   age 10ms: 1.0
      //   age 50ms: 1.0
      //   age 60ms: 0.5
      var ageMillis: Single := (movements[size - 1].eventTime - movements[index].eventTime) * 0.000001;
      if (ageMillis < 0) then
        exit(0.5);
      if (ageMillis < 10) then
        exit(0.5 + ageMillis * 0.05);
      if (ageMillis < 50) then
        exit(1.0);
      if (ageMillis < 60) then
        exit(0.5 + (60 - ageMillis) * 0.05);
      Result := 0.5;
    end;

    TWeighting.RECENT: begin
      // Weight points based on their age, weighing older points less.
      //   age   0ms: 1.0
      //   age  50ms: 1.0
      //   age 100ms: 0.5
      var ageMillis: Single := (movements[size - 1].eventTime - movements[index].eventTime) * 0.000001;
      if (ageMillis < 50) then
        exit(1.0);
      if (ageMillis < 100) then
        exit(0.5 + (100 - ageMillis) * 0.01);
      Result := 0.5;
    end;

    TWeighting.NONE:
      Result := 1.0;

    else
      Raise Exception.Create('Invalid weighting strategy')
  end;
end;

{********************************************************************************}
constructor TALIntegratingVelocityTrackerStrategy.Create(degree: Int32{uint32_t});
begin
  Inherited Create;
  FDegree := degree;
end;

{****************************************************}
procedure TALIntegratingVelocityTrackerStrategy.clear;
begin
  FPointerIdBits.Clear;
end;

{*****************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.clearPointer(pointerId: Int32);
begin
  FPointerIdBits.clearBit(pointerId);
end;

{****************************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.addMovement(eventTime: Int64; pointerId: Int32; position: Single);
begin
  if (FPointerIdBits.hasBit(pointerId)) then
    updateState(FPointerState[pointerId], eventTime, position)
  else
    initState(FPointerState[pointerId], eventTime, position);

  FPointerIdBits.markBit(pointerId);
end;

{***********************************************************************************}
function TALIntegratingVelocityTrackerStrategy.getVelocity(pointerId: Int32): Single;
begin
  if (FPointerIdBits.hasBit(pointerId)) then
    exit(FPointerState[pointerId].vel);

  Result := 0;
end;

{**********************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.initState(var state: TState; eventTime: Int64; pos: Single);
begin
  state.updateTime := eventTime;
  state.degree := 0;

  state.pos := pos;
  state.accel := 0;
  state.vel := 0;
end;

{************************************************************************************************************}
procedure TALIntegratingVelocityTrackerStrategy.updateState(var state: TState; eventTime: Int64; pos: Single);
begin
  const MIN_TIME_DELTA: Int64 = 2 * ALNanosPerMs;
  const FILTER_TIME_CONSTANT: Single = 0.010; // 10 milliseconds

  if (eventTime <= state.updateTime + MIN_TIME_DELTA) then
    Exit;

  var dt: Single := (eventTime - state.updateTime) * 0.000000001;
  state.updateTime := eventTime;

  var vel: Single := (pos - state.pos) / dt;
  if (state.degree = 0) then begin
    state.vel := vel;
    state.degree := 1;
  end
  else begin
    var alpha: Single := dt / (FILTER_TIME_CONSTANT + dt);
    if (fDegree = 1) then
      state.vel := state.vel + ((vel - state.vel) * alpha)
    else begin
      var accel: Single := (vel - state.vel) / dt;
      if (state.degree = 1) then begin
        state.accel := accel;
        state.degree := 2;
      end
      else
        state.accel := state.accel + ((accel - state.accel) * alpha);
      state.vel := state.vel + ((state.accel * dt) * alpha);
    end;
  end;
  state.pos := pos;
end;

{**************************************************}
constructor TALLegacyVelocityTrackerStrategy.Create;
begin
  inherited Create(HORIZON{horizonNanos}, false{maintainHorizonDuringAdd});
end;

{******************************************************************************}
function TALLegacyVelocityTrackerStrategy.getVelocity(pointerId: Int32): Single;
begin
  if (FMovements[pointerId] = nil) then
    exit(0); // no data

  const movements = FMovements[pointerId];
  const size{: size_t} = movements.count;
  if (size = 0) then
    exit(0); // no data

  const newestMovement: TMovement = movements[size - 1];

  // Find the oldest sample that contains the pointer and that is not older than HORIZON.
  var minTime: Int64 := newestMovement.eventTime - HORIZON;
  var oldestIndex: Int32{uint32_t} := size - 1;
  for var i{: ssize_t} := size - 1 downto 0 do begin
    const nextOldestMovement: TMovement = movements[i];
    if (nextOldestMovement.eventTime < minTime) then
      break;
    oldestIndex := i;
  end;

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
  var accumV: Single := 0;
  var samplesUsed: Int32{uint32_t} := 0;
  const oldestMovement: TMovement = movements[oldestIndex];
  var oldestPosition: Single := oldestMovement.position;
  var lastDuration: Int64 := 0;

  for var i{: size_t} := oldestIndex to size - 1 do begin
    const movement: TMovement = movements[i];
    var duration: Int64 := movement.eventTime - oldestMovement.eventTime;

    // If the duration between samples is small, we may significantly overestimate
    // the velocity.  Consequently, we impose a minimum duration constraint on the
    // samples that we include in the calculation.
    if (duration >= MIN_DURATION) then begin
      var position: Single := movement.position;
      var scale: Single := 1000000000.0 / duration; // one over time delta in seconds
      var v: Single := (position - oldestPosition) * scale;
      accumV := (accumV * lastDuration + v * duration) / (duration + lastDuration);
      lastDuration := duration;
      samplesUsed := samplesUsed + 1;
    end;
  end;

  if (samplesUsed <> 0) then
    exit(accumV);
  Result := 0;
end;

{*************************************************************************}
constructor TALImpulseVelocityTrackerStrategy.Create(deltaValues: Boolean);
begin
  inherited Create(HORIZON{horizonNanos}, true{maintainHorizonDuringAdd});
  FDeltaValues := deltaValues;
end;

{*******************************************************************************}
// Calculate the total impulse provided to the screen and the resulting velocity.
//
// The touchscreen is modeled as a physical object.
// Initial condition is discussed below, but for now suppose that v(t=0) = 0
//
// The kinetic energy of the object at the release is E=0.5*m*v^2
// Then vfinal = sqrt(2E/m). The goal is to calculate E.
//
// The kinetic energy at the release is equal to the total work done on the object by the finger.
// The total work W is the sum of all dW along the path.
//
// dW = F*dx, where dx is the piece of path traveled.
// Force is change of momentum over time, F = dp/dt = m dv/dt.
// Then substituting:
// dW = m (dv/dt) * dx = m * v * dv
//
// Summing along the path, we get:
// W = sum(dW) = sum(m * v * dv) = m * sum(v * dv)
// Since the mass stays constant, the equation for final velocity is:
// vfinal = sqrt(2*sum(v * dv))
//
// Here,
// dv : change of velocity = (v[i+1]-v[i])
// dx : change of distance = (x[i+1]-x[i])
// dt : change of time = (t[i+1]-t[i])
// v : instantaneous velocity = dx/dt
//
// The final formula is:
// vfinal = sqrt(2) * sqrt(sum((v[i]-v[i-1])*|v[i]|)) for all i
// The absolute value is needed to properly account for the sign. If the velocity over a
// particular segment descreases, then this indicates braking, which means that negative
// work was done. So for two positive, but decreasing, velocities, this contribution would be
// negative and will cause a smaller final velocity.
//
// Initial condition
// There are two ways to deal with initial condition:
// 1) Assume that v(0) = 0, which would mean that the screen is initially at rest.
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
// 1) By directly applying the formula above with the v(0) = 0 boundary condition, we will get
// vfinal = sqrt(2*(|v1|*(v1-v0) + |v2|*(v2-v1))). This can be simplified since v0=0
// vfinal = sqrt(2*(|v1|*v1 + |v2|*(v2-v1))) = sqrt(2*(v1^2 + |v2|*(v2 - v1)))
// since velocity is a real number
// 2) If we treat the screen as already moving, then it must already have an energy (per mass)
// equal to 1/2*v1^2. Then the initial energy should be 1/2*v1*2, and only the second segment
// will contribute to the total kinetic energy (since we can effectively consider that v0=v1).
// This will give the following expression for the final velocity:
// vfinal = sqrt(2*(1/2*v1^2 + |v2|*(v2-v1)))
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
function kineticEnergyToVelocity(work: Single): Single;
begin
  const sqrt2: Single = 1.41421356237;
  Result := ALIfThen(work < 0, -1.0, 1.0) * sqrt(abs(work)) * sqrt2;
end;

{*******************************************************************************}
function TALImpulseVelocityTrackerStrategy.getVelocity(pointerId: Int32): Single;
begin
  if (FMovements[pointerId] = nil) then
    Exit(0); // no data

  const movements = FMovements[pointerId];
  const size{: size_t} = movements.count;
  if (size = 0) then
    Exit(0); // no data

  var work: Single := 0;
  for var i{: size_t} := 0 to size - 2 do begin
    const mvt: TMovement = movements[i];
    const nextMvt: TMovement = movements[i + 1];

    var vprev: Single := kineticEnergyToVelocity(work);
    var delta: Single := ALIfThen(FDeltaValues, nextMvt.position, nextMvt.position - mvt.position);
    var vcurr: Single := delta / (ALSecondsPerNano * (nextMvt.eventTime - mvt.eventTime));
    work := work + ((vcurr - vprev) * abs(vcurr));

    if (i = 0) then
      work := work * 0.5; // initial condition, case 2) above
  end;

  const velocity: Single = kineticEnergyToVelocity(work);
  {$IF defined(DEBUG_STRATEGY)}
  ALLog('Alcinoe.FMX.ScrollEngine.TALImpulseVelocityTrackerStrategy.getVelocity', 'velocity: %.1f', [velocity]);
  {$ENDIF}

  {$IF defined(DEBUG_IMPULSE)}
  // TODO(b/134179997): delete this block once the switch to 'impulse' is complete.
  // Calculate the lsq2 velocity for the same inputs to allow runtime comparisons.
  // X axis chosen arbitrarily for velocity comparisons.
  var lsq2 := TALVelocityTracker.Create(TALVelocityTracker.TStrategy.LSQ2);
  Try
    for var i{: size_t} := 0 to size - 1 do begin
      const mvt: TMovement = movements[i];
      lsq2.addMovement(mvt.eventTime, pointerId, TALVelocityTracker.AXIS_X, mvt.position);
    end;
    var v: Single := lsq2.getVelocity(TALVelocityTracker.AXIS_X, pointerId);
    if (v <> 0) then
      ALLog('Alcinoe.FMX.ScrollEngine.TALImpulseVelocityTrackerStrategy.getVelocity', 'lsq2 velocity: %.1f', [v])
    else
      ALLog('Alcinoe.FMX.ScrollEngine.TALImpulseVelocityTrackerStrategy.getVelocity', 'lsq2 velocity: could not compute velocity');
  finally
    ALFreeAndNil(lsq2);
  end;
  {$ENDIF}
  Result := velocity;
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
    'Alcinoe.FMX.ScrollEngine.TALSplineOverScroller.Create',
    'PixelsPerInch:' + ALFormatFloatW('0.##', PixelsPerInch, ALDefaultFormatSettingsW) + ' | ' +
    'PhysicalCoeff:' + ALFormatFloatW('0.##', FPhysicalCoeff, ALDefaultFormatSettingsW));
  {$ENDIF}
end;

{*************************************************}
function TALSplineOverScroller.getFriction: Single;
begin
  Result := FFlingFriction;
end;

{************************************************************}
procedure TALSplineOverScroller.setFriction(friction: Single);
begin
  FFlingFriction := friction;
end;

{******************************************************************}
procedure TALSplineOverScroller.updateScroll(q: Single; q2: Single);
begin
  var distance: integer := FFinal - FStart;
  FCurrentPosition := FStart + Round(q * distance);
  // q2 is 1ms before q1
  FCurrVelocity := 1000 * (q - q2) * distance;
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
    ALLog('Alcinoe.FMX.ScrollEngine.TALSplineOverScroller', 'startAfterEdge called from a valid position', TALLogType.ERROR);
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

{*******************************************}
function TALOverScroller.getFriction: Single;
begin
  Result := FScrollerX.getFriction;
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
        const q2: Single = FInterpolateFunc((elapsedTime - 1) / {(Single)} duration);
        FScrollerX.updateScroll(q, q2);
        FScrollerY.updateScroll(q, q2);
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

{**************************************************************************}
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

{*************************************************************************}
function TALOverScroller.getSplineFlingDistance(velocity: integer): double;
begin
  Result := FScrollerY.getSplineFlingDistance(velocity);
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
  //ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.TDisplayLinkListener.displayLinkUpdated');
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

{*******************************************************}
procedure TALScrollEngine.doFrame(frameTimeNanos: Int64);
begin
  {$IFDEF DEBUG}
  //ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.doFrame');
  {$ENDIF}
  if not Calculate then
    StopTimer
  else
    TChoreographer.Instance.PostAniFrameCallback(doFrame);
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
  ALInitScreenScale;
  {$IFDEF IOS}
  fDisplayLinkListener := nil;
  fDisplayLink := nil;
  {$ELSEIF not defined(ANDROID)}
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
  FTouchMode := TTouchMode.Auto;
  FTouchTracking := [ttVertical, ttHorizontal];
  FMinScrollLimit := TALPointD.Zero;
  FMaxScrollLimit := TALPointD.Zero;
  FOnStart := nil;
  FOnChanged := nil;
  FOnStop := nil;
  FMovements := TList<TMovement>.Create;
  FViewportPosition := TALPointD.Zero;
  FLastMotionPos := TPointF.Zero;
  FDown := false;
  FDownPosition := TPointF.Zero;
  FUpPosition := TPointF.Zero;
  FUpVelocity := TPointF.Zero;
  FMoved := false;
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
  {$ENDIF}
  ALFreeAndNil(FOverScroller);
  ALFreeAndNil(FVelocityTracker);
  ALFreeAndNil(FMovements);
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

{****************************************************}
procedure TALScrollEngine.Assign(Source: TPersistent);
begin
  if Source is TALScrollEngine then begin
    TouchMode := TALScrollEngine(Source).TouchMode;
    TouchSlop := TALScrollEngine(Source).TouchSlop;
    OverflingDistance := TALScrollEngine(Source).OverflingDistance;
    DragResistanceFactor := TALScrollEngine(Source).DragResistanceFactor;
    MinEdgeSpringbackEnabled := TALScrollEngine(Source).MinEdgeSpringbackEnabled;
    MaxEdgeSpringbackEnabled := TALScrollEngine(Source).MaxEdgeSpringbackEnabled;
    Friction := TALScrollEngine(Source).Friction;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**************************************************************************}
// Start scrolling by providing a starting point and the distance to travel.
// @param startX Starting horizontal scroll offset in pixels. Positive numbers will scroll the content to the left.
// @param startY Starting vertical scroll offset in pixels. Positive numbers will scroll the content up.
// @param dx Horizontal distance to travel. Positive numbers will scroll the content to the left.
// @param dy Vertical distance to travel. Positive numbers will scroll the content up.
// @param duration Duration of the scroll in milliseconds.
procedure TALScrollEngine.startScroll(startX: Double; startY: Double; dx: Double; dy: Double; const duration: integer = TALOverScroller.DEFAULT_DURATION);
begin
  if not FOverScroller.isFinished then
    FoverScroller.abortAnimation;

  FOverScroller.startScroll(
    trunc(startX * ALScreenScale), // startX: integer;
    trunc(startY * ALScreenScale), // startY: integer;
    trunc(dx * ALScreenScale), // dx: integer;
    trunc(dy * ALScreenScale), // dy: integer;
    duration); // const duration: integer = DEFAULT_DURATION);

  StartTimer;
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
  DoMouseUp;
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

{************************************************}
function TALScrollEngine.GetTouchEnabled: Boolean;
begin
  Result := FTouchTracking <> [];
  If result then
    case FTouchMode of
      TTouchMode.Disabled: Result := False;
      TTouchMode.Enabled: Result := True;
      TTouchMode.Auto: Result := ALGetHasTouchScreen;
      else Raise Exception.Create('Error 400A0C7F-EE78-464B-8FE9-BEA521225713')
    end;
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
    fDisplayLink.addToRunLoop(TNSRunLoop.Wrap(TNSRunLoop.OCClass.mainRunLoop), NSRunLoopCommonModes); // I don't really know with is the best, NSDefaultRunLoopMode or NSRunLoopCommonModes
  end;
  fDisplayLink.setPaused(False);
  {$ELSEIF defined(ANDROID)}
  {$IF defined(DEBUG)}
  if TThread.Current.ThreadID <> MainThreadID then
    raise Exception.Create('TALScrollEngine.StartTimer must only be called from the main thread.');
  {$ENDIF}
  TChoreographer.Instance.PostAniFrameCallback(DoFrame);
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
  {$IF defined(DEBUG)}
  if TThread.Current.ThreadID <> MainThreadID then
    raise Exception.Create('TALScrollEngine.StopTimer must only be called from the main thread.');
  {$ENDIF}
  TChoreographer.Instance.RemoveAniFrameCallback(DoFrame);
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
  //ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.DoStart');
  {$ENDIF}
  if Assigned(FOnStart) then
    FOnStart(self);
end;

{**********************************}
procedure TALScrollEngine.DoChanged;
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'Alcinoe.FMX.ScrollEngine.TALScrollEngine.DoChanged',
  //  'ViewPortPosition:' + ALFormatFloatW('0.##', ViewPortPosition.x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', ViewPortPosition.y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

{*******************************}
procedure TALScrollEngine.DoStop;
begin
  {$IFDEF DEBUG}
  //ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.DoStop');
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

{**************************************************}
function TALScrollEngine.IsTouchSlopStored: Boolean;
begin
  Result := Not sameValue(FTouchSlop, DefaultTouchSlop, TEpsilon.Position);
end;

{**********************************************************}
function TALScrollEngine.IsOverflingDistanceStored: Boolean;
begin
  Result := Not sameValue(FOverflingDistance, DefaultOverflingDistance, TEpsilon.Position);
end;

{*************************************************************}
function TALScrollEngine.IsDragResistanceFactorStored: Boolean;
begin
  Result := Not sameValue(FDragResistanceFactor, DefaultDragResistanceFactor, TEpsilon.Scale);
end;

{*************************************************}
function TALScrollEngine.IsFrictionStored: Boolean;
begin
  Result := Not sameValue(GetFriction, 0.015{ViewConfiguration.getScrollFriction}, TEpsilon.Scale);
end;

{*******************************************}
function TALScrollEngine.GetFriction: Single;
begin
  Result := FOverScroller.getFriction;
end;

{******************************************}
// The amount of friction applied to flings.
// @param friction A scalar dimension-less value representing the coefficient of friction.
procedure TALScrollEngine.SetFriction(const Value: Single);
begin
  FOverScroller.setFriction(Value);
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
          'Alcinoe.FMX.ScrollEngine.TALScrollEngine.Calculate',
          'notifyVerticalEdgeReached');
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
          'Alcinoe.FMX.ScrollEngine.TALScrollEngine.Calculate',
          'notifyVerticalEdgeReached');
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
          'Alcinoe.FMX.ScrollEngine.TALScrollEngine.Calculate',
          'notifyHorizontalEdgeReached');
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
          'Alcinoe.FMX.ScrollEngine.TALScrollEngine.Calculate',
          'notifyHorizontalEdgeReached');
        {$ENDIF}
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

{************************************}
procedure TALScrollEngine.DoMouseDown;
begin
  if not TouchEnabled then FMovements.Count := 0;
  if FMovements.Count = 0 then exit;
  var LNewestMovement := FMovements.Last;
  FDown := True;
  FMoved := False;
  FLastMotionPos := LNewestMovement.position;
  fDownPosition := FLastMotionPos;
  FOverScroller.forceFinished(true{finished});
  FVelocityTracker.clear;
  if ttHorizontal in FTouchTracking then
    FVelocityTracker.addMovement(LNewestMovement.eventTime{eventTime}, 0{pointerId}, TALVelocityTracker.AXIS_X{axis}, FLastMotionPos.x*ALScreenScale{position});
  if ttVertical in FTouchTracking then
    FVelocityTracker.addMovement(LNewestMovement.eventTime{eventTime}, 0{pointerId}, TALVelocityTracker.AXIS_Y{axis}, FLastMotionPos.y*ALScreenScale{position});
  FMovements.Count := 0;
  StartTimer;
end;

{************************************************}
procedure TALScrollEngine.MouseDown(X, Y: Single);
begin
  if not TouchEnabled then exit;
  var LMovement: TMovement;
  LMovement.eventTime := ALElapsedTimeNano;
  LMovement.position := TPointF.Create(X, Y);
  FMovements.Add(LMovement);
  DoMouseDown;
end;

{****************************************************************}
procedure TALScrollEngine.MouseDown(const AHandle: TWindowHandle);
begin
  if (AHandle = nil) or (not TouchEnabled) then exit;

  {$IF defined(ANDROID)}
  var LTmpCurrentMotionEvent := WindowHandleToPlatform(AHandle).CurrentMotionEvent;
  if LTmpCurrentMotionEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseDown', 'CurrentMotionEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentMotionEvent := TJALMotionEvent.wrap(LTmpCurrentMotionEvent);
  if (LCurrentMotionEvent.getPointerCount = 0) or
     (LCurrentMotionEvent.getActionMasked <> AMOTION_EVENT_ACTION_DOWN) then exit;
  //--
  var LMovement: TMovement;
  if TOSVersion.Check(14) {API level >= 34 (UPSIDE_DOWN_CAKE)} then
    LMovement.eventTime := LCurrentMotionEvent.getEventTimeNanos
  else
    LMovement.eventTime := LCurrentMotionEvent.getEventTime*ALNanosPerMs;
  LMovement.position := PlatformAndroid.WindowService.PixelToPoint(
                          TPointF.Create(
                            LCurrentMotionEvent.getX(0),
                            LCurrentMotionEvent.getY(0)));
  FMovements.Add(LMovement);
  {$ENDIF}

  {$IF defined(IOS)}
  var LTmpCurrentTouchEvent := WindowHandleToPlatform(AHandle).CurrentTouchEvent;
  if LTmpCurrentTouchEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseDown', 'CurrentTouchEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentTouchEvent := TALUIEvent.Wrap(NSObjectToID(LTmpCurrentTouchEvent));
  if (LCurrentTouchEvent.AllTouches = nil) or
     (LCurrentTouchEvent.AllTouches.count = 0) then exit;
  var LTouch := TUITouch.Wrap(LCurrentTouchEvent.alltouches.anyObject);
  if LTouch.phase <> UITouchPhaseBegan then exit;
  //--
  var LTouchPoint := LTouch.locationInView(WindowHandleToPlatform(AHandle).View);
  var LMovement: TMovement;
  LMovement.eventTime := Trunc(LTouch.timestamp*ALNanosPerSec);
  LMovement.position := TPointF.create(LTouchPoint.X, LTouchPoint.Y);
  FMovements.Add(LMovement);
  {$ENDIF}

  DoMouseDown;
end;

{************************************}
procedure TALScrollEngine.DoMouseMove;

var
  LNewestMovement: TMovement;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetDiff(Var xDiff, yDiff: Single): boolean;
  begin
    Result := True;
    if FTouchTracking = [ttVertical, ttHorizontal] then begin
      xDiff := FLastMotionPos.X - LNewestMovement.position.x;
      yDiff := FLastMotionPos.Y - LNewestMovement.position.y;
    end
    else if FTouchTracking = [ttVertical] then begin
      xDiff := 0.0;
      yDiff := FLastMotionPos.Y - LNewestMovement.position.y;
    end
    else if FTouchTracking = [ttHorizontal] then begin
      xDiff := FLastMotionPos.X - LNewestMovement.position.x;
      yDiff := 0.0;
    end
    else
      Result := False;
  end;

begin

  if not fDown then FMovements.Count := 0;
  if FMovements.Count = 0 then exit;
  LNewestMovement := FMovements.Last;

  var xDiff: Single;
  var yDiff: Single;
  if not _GetDiff(xDiff, yDiff) then
    exit;

  if (not FMoved) then begin
    if (abs(yDiff) <= FTouchSlop) and (abs(xDiff) <= FTouchSlop) then exit;
    FLastMotionPos := LNewestMovement.position;
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

  For var I := 0 to FMovements.Count - 1 do begin
    var LMovement := FMovements[I];
    FLastMotionPos := LMovement.position;
    if ttHorizontal in FTouchTracking then
      FVelocityTracker.addMovement(LMovement.eventTime{eventTime}, 0{pointerId}, TALVelocityTracker.AXIS_X{axis}, FLastMotionPos.x*ALScreenScale{position});
    if ttVertical in FTouchTracking then
      FVelocityTracker.addMovement(LMovement.eventTime{eventTime}, 0{pointerId}, TALVelocityTracker.AXIS_Y{axis}, FLastMotionPos.y*ALScreenScale{position});
  end;
  FMovements.Count := 0;

end;

{************************************************}
procedure TALScrollEngine.MouseMove(X, Y: Single);
begin
  if not fDown then exit;
  var LMovement: TMovement;
  LMovement.eventTime := ALElapsedTimeNano;
  LMovement.position := TPointF.Create(X, Y);
  FMovements.Add(LMovement);
  DoMouseMove;
end;

{****************************************************************}
procedure TALScrollEngine.MouseMove(const AHandle: TWindowHandle);
begin
  if (AHandle = nil) or (not fDown) then exit;

  {$IF defined(ANDROID)}
  var LTmpCurrentMotionEvent := WindowHandleToPlatform(AHandle).CurrentMotionEvent;
  if LTmpCurrentMotionEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseMove', 'CurrentMotionEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentMotionEvent := TJALMotionEvent.wrap(LTmpCurrentMotionEvent);
  if (LCurrentMotionEvent.getPointerCount = 0) or
     (LCurrentMotionEvent.getActionMasked <> AMOTION_EVENT_ACTION_MOVE) then exit;
  //--
  for var I := 0 to LCurrentMotionEvent.getHistorySize - 1 do begin
    var LMovement: TMovement;
    if TOSVersion.Check(14) {API level >= 34 (UPSIDE_DOWN_CAKE)} then
      LMovement.eventTime := LCurrentMotionEvent.getHistoricalEventTimeNanos(I)
    else
      LMovement.eventTime := LCurrentMotionEvent.getHistoricalEventTime(I)*ALNanosPerMs;
    LMovement.position := PlatformAndroid.WindowService.PixelToPoint(
                            TPointF.Create(
                              LCurrentMotionEvent.getHistoricalX(0, I),
                              LCurrentMotionEvent.getHistoricalY(0, I)));
    FMovements.Add(LMovement);
  end;
  //--
  var LMovement: TMovement;
  if TOSVersion.Check(14) {API level >= 34 (UPSIDE_DOWN_CAKE)} then
    LMovement.eventTime := LCurrentMotionEvent.getEventTimeNanos
  else
    LMovement.eventTime := LCurrentMotionEvent.getEventTime*ALNanosPerMs;
  LMovement.position := PlatformAndroid.WindowService.PixelToPoint(
                          TPointF.Create(
                            LCurrentMotionEvent.getX(0),
                            LCurrentMotionEvent.getY(0)));
  FMovements.Add(LMovement);
  {$ENDIF}

  {$IF defined(IOS)}
  var LTmpCurrentTouchEvent := WindowHandleToPlatform(AHandle).CurrentTouchEvent;
  if LTmpCurrentTouchEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseMove', 'CurrentTouchEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentTouchEvent := TALUIEvent.Wrap(NSObjectToID(LTmpCurrentTouchEvent));
  if (LCurrentTouchEvent.AllTouches = nil) or
     (LCurrentTouchEvent.AllTouches.count = 0) then exit;
  var LTouch := TUITouch.Wrap(LCurrentTouchEvent.alltouches.anyObject);
  if LTouch.phase <> UITouchPhaseMoved then exit;
  //--
  var LCoalescedTouches: NSArray := LCurrentTouchEvent.coalescedTouchesForTouch(LTouch);
  if (LCoalescedTouches <> nil) then begin
    for var I := 0 to LCoalescedTouches.count - 1 do begin
      var LCoalescedTouch := TUITouch.Wrap(LCoalescedTouches.objectAtIndex(I));
      if LCoalescedTouch <> nil then begin
        var LCoalescedTouchPoint := LCoalescedTouch.locationInView(WindowHandleToPlatform(AHandle).View);
        var LMovement: TMovement;
        LMovement.eventTime := Trunc(LCoalescedTouch.timestamp*ALNanosPerSec);
        LMovement.position := TPointF.create(LCoalescedTouchPoint.X, LCoalescedTouchPoint.Y);
        FMovements.Add(LMovement);
      end;
    end;
  end;
  {$ENDIF}

  DoMouseMove;

end;

{**********************************}
procedure TALScrollEngine.DoMouseUp;
begin
  var LUpPosition: TPointF;
  if FMovements.Count > 0 then begin
    var LNewestMovement := FMovements.Last;
    if (SameValue(LNewestMovement.position.X, FLastMotionPos.X, Tepsilon.Position)) and
       (SameValue(LNewestMovement.position.Y, FLastMotionPos.Y, Tepsilon.Position)) then
      FMovements.Count := FMovements.Count - 1;
    LUpPosition := LNewestMovement.position;
    if (LNewestMovement.eventTime - FvelocityTracker.FLastEventTime > ASSUME_POINTER_STOPPED_TIME) then begin
      {$IF defined(DEBUG_VELOCITY)}
      ALLOG('Alcinoe.FMX.ScrollEngine.TALVelocityTracker', 'VelocityTracker: stopped for %.2f ms, clearing state upon pointer liftoff.', [(LNewestMovement.eventTime - FvelocityTracker.FLastEventTime) / ALNanosPerMs]);
      {$ENDIF}
      // We have not received any movements for too long.  Assume that all pointers
      // have stopped.
      for var axis := Low(FvelocityTracker.FConfiguredStrategies) to High(FvelocityTracker.FConfiguredStrategies) do
        if (axis in PLANAR_AXES) and (FvelocityTracker.FConfiguredStrategies[axis] <> nil) then
          FvelocityTracker.FConfiguredStrategies[axis].clear;
    end;
  end
  else LUpPosition := FLastMotionPos;

  DoMouseMove;
  if not Fdown then exit;
  FDown := False;

  var LStartX: integer;
  var LStartY: integer;
  var LVelocityX: Single;
  var LVelocityY: Single;
  var LMinX: integer;
  var LMaxX: integer;
  var LMinY: integer;
  var LMaxY: integer;

  if FTouchTracking = [ttVertical, ttHorizontal] then begin
    LStartX := trunc(FViewPortPosition.X*ALScreenScale);
    LStartY := trunc(FViewPortPosition.Y*ALScreenScale);
    LVelocityX := -FvelocityTracker.getVelocity(TALVelocityTracker.AXIS_X{axis}, 0{pointerId});
    LVelocityY := -FvelocityTracker.getVelocity(TALVelocityTracker.AXIS_Y{axis}, 0{pointerId});
    LMinX := trunc(FMinScrollLimit.x*ALScreenScale);
    LMaxX := trunc(FMaxScrollLimit.x*ALScreenScale);
    LMinY := trunc(FMinScrollLimit.y*ALScreenScale);
    LMaxY := trunc(FMaxScrollLimit.y*ALScreenScale);
  end
  else if FTouchTracking = [ttVertical] then begin
    LStartX := 0;
    LStartY := trunc(FViewPortPosition.Y*ALScreenScale);
    LVelocityX := 0;
    LVelocityY := -FvelocityTracker.getVelocity(TALVelocityTracker.AXIS_Y{axis}, 0{pointerId});
    LMinX := 0;
    LMaxX := 0;
    LMinY := trunc(FMinScrollLimit.y*ALScreenScale);
    LMaxY := trunc(FMaxScrollLimit.y*ALScreenScale);
  end
  else if FTouchTracking = [ttHorizontal] then begin
    LStartX := trunc(FViewPortPosition.X*ALScreenScale);
    LStartY := 0;
    LVelocityX := -FvelocityTracker.getVelocity(TALVelocityTracker.AXIS_X{axis}, 0{pointerId});
    LVelocityY := 0;
    LMinX := trunc(FMinScrollLimit.x*ALScreenScale);
    LMaxX := trunc(FMaxScrollLimit.x*ALScreenScale);
    LMinY := 0;
    LMaxY := 0;
  end
  else
    exit;

  FUpPosition := LUpPosition;
  FUpVelocity := TPointF.Create(LVelocityX/ALScreenScale, LVelocityY/ALScreenScale);

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

{**********************************************}
procedure TALScrollEngine.MouseUp(X, Y: Single);
begin
  if not Fdown then exit;
  var LMovement: TMovement;
  LMovement.eventTime := ALElapsedTimeNano;
  LMovement.position := TPointF.Create(X, Y);
  FMovements.Add(LMovement);
  DoMouseUp;
end;

{**************************************************************}
procedure TALScrollEngine.MouseUp(const AHandle: TWindowHandle);
begin
  if (AHandle = nil) or (not Fdown) then exit;

  {$IF defined(ANDROID)}
  var LTmpCurrentMotionEvent := WindowHandleToPlatform(AHandle).CurrentMotionEvent;
  if LTmpCurrentMotionEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseUp', 'CurrentMotionEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentMotionEvent := TJALMotionEvent.wrap(LTmpCurrentMotionEvent);
  if (LCurrentMotionEvent.getPointerCount = 0) or
     (LCurrentMotionEvent.getActionMasked <> AMOTION_EVENT_ACTION_UP) then exit;
  //--
  var LMovement: TMovement;
  if TOSVersion.Check(14) {API level >= 34 (UPSIDE_DOWN_CAKE)} then
    LMovement.eventTime := LCurrentMotionEvent.getEventTimeNanos
  else
    LMovement.eventTime := LCurrentMotionEvent.getEventTime*ALNanosPerMs;
  LMovement.position := PlatformAndroid.WindowService.PixelToPoint(
                          TPointF.Create(
                            LCurrentMotionEvent.getX(0),
                            LCurrentMotionEvent.getY(0)));
  FMovements.Add(LMovement);
  {$ENDIF}
  {$IF defined(IOS)}
  var LTmpCurrentTouchEvent := WindowHandleToPlatform(AHandle).CurrentTouchEvent;
  if LTmpCurrentTouchEvent = nil then begin
    {$IF defined(DEBUG)}
    ALLog('Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseUp', 'CurrentTouchEvent is nil', TALLogType.ERROR);
    {$ENDIF}
    exit;
  end;
  var LCurrentTouchEvent := TALUIEvent.Wrap(NSObjectToID(LTmpCurrentTouchEvent));
  if (LCurrentTouchEvent.AllTouches = nil) or
     (LCurrentTouchEvent.AllTouches.count = 0) then exit;
  var LTouch := TUITouch.Wrap(LCurrentTouchEvent.alltouches.anyObject);
  if not LTouch.phase in [UITouchPhaseEnded, UITouchPhaseCancelled] then exit;
  //--
  var LTouchPoint := LTouch.locationInView(WindowHandleToPlatform(AHandle).View);
  var LMovement: TMovement;
  LMovement.eventTime := Trunc(LTouch.timestamp*ALNanosPerSec);
  LMovement.position := TPointF.create(LTouchPoint.X, LTouchPoint.Y);
  FMovements.Add(LMovement);
  {$ENDIF}

  DoMouseUp;
end;

{***********************************}
procedure TALScrollEngine.MouseLeave;
begin
  DoMouseUp;
end;

{*************************************************}
procedure TALScrollEngine.MouseWheel(X, Y: Double);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'Alcinoe.FMX.ScrollEngine.TALScrollEngine.MouseWheel',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
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
