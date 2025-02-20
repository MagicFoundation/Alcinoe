unit Alcinoe.Common;

interface

{$I Alcinoe.inc}

uses
  {$IFDEF IOS}
  iOSapi.Foundation,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  system.Classes,
  system.Generics.Collections,
  system.SyncObjs,
  system.sysutils,
  system.math,
  system.types,
  System.UITypes;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWorkerThreadRefProc = reference to procedure(var AContext: Tobject);
  TALWorkerThreadObjProc = procedure(var AContext: Tobject) of object;
  TALWorkerThreadGetPriorityFunc = function(const AContext: Tobject): Int64 of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWorkerThreadRequest = Class(Tobject)
  private
    FRefProc: TALWorkerThreadRefProc;
    FObjProc: TALWorkerThreadObjProc;
    FContext: Tobject;
    FPriority: Int64;
    FGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
    FSignal: TEvent;
    function GetPriority: Int64; inline;
  public
    constructor Create(
                  const AProc: TALWorkerThreadRefProc;
                  const AContext: Tobject;
                  const APriority: Int64;
                  const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                  const ASignal: TEvent); overload;
    constructor Create(
                  const AProc: TALWorkerThreadobjProc;
                  const AContext: Tobject;
                  const APriority: Int64;
                  const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                  const ASignal: TEvent); overload;
    destructor Destroy; override;
    property RefProc: TALWorkerThreadRefProc read FRefProc;
    property ObjProc: TALWorkerThreadObjProc read FObjProc;
    property Context: Tobject read FContext;
    property Priority: Int64 read GetPriority;
    property Signal: TEvent read FSignal;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWorkerThreadPool = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWorkerThread = class(TThread)
  private
    FWorkerThreadPool: TALWorkerThreadPool;
    FSignal: TEvent;
    FWaiting: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const aWorkerThreadPool: TALWorkerThreadPool);
    destructor Destroy; override;
    property Signal: TEvent read FSignal;
    property Waiting: Boolean read fWaiting write fWaiting;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALWorkerThreadPool = Class(Tobject)
  public
    type
      TPriorityDirection = (lessThan, GreaterThan);
  private
    fMaxThreadCount: integer;
    fPriorityStartingPoint: int64;
    fPriorityDirection: TPriorityDirection;
    fThreads: TObjectList<TALWorkerThread>;
    fRequests: TObjectList<TALWorkerThreadRequest>;
    function GetPriorityDirection: TPriorityDirection;
    function GetPriorityStartingPoint: int64;
    function GetPriorityStartingPointExt(const AContext: Tobject): Int64;
    procedure SetPriorityDirection(const Value: TPriorityDirection);
    procedure SetPriorityStartingPoint(const Value: int64);
  protected
    procedure EnqueueRequest(const ARequest: TALWorkerThreadRequest);
    function DequeueRequest: TALWorkerThreadRequest;
  public
    constructor Create(const AMaxThreadCount: integer);
    destructor Destroy; override;
    //TALWorkerThreadRefProc
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload; virtual;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const APriority: Int64;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                const AContext: Tobject; // Context will be free by the worker thread
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadRefProc;
                Const AAsync: Boolean = True); overload;
    //TALWorkerThreadObjProc
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload; virtual;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const APriority: Int64;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AContext: Tobject; // Context will be free by the worker thread
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const APriority: Int64;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                const AContext: Tobject; // Context will be free by the worker thread
                Const AAsync: Boolean = True); overload;
    procedure ExecuteProc(
                const AProc: TALWorkerThreadObjProc;
                Const AAsync: Boolean = True); overload;
    //When we Dequeue a Request, we look for the request with a priority
    //the most closest to PriorityStartingPoint in the PriorityDirection.
    //Exemple if we have in the queue the requests with those priorities: 25, 75, 100, 125, 150
    //and if PriorityStartingPoint = 110, PriorityDirection = lessThan then
    //dequeue will return the request with a priority of 100. in the opposite
    //way if PriorityStartingPoint = 110, PriorityDirection = greaterThan then
    //dequeue will return 125
    property PriorityStartingPoint: int64 read GetPriorityStartingPoint write SetPriorityStartingPoint;
    property PriorityDirection: TPriorityDirection read GetPriorityDirection write SetPriorityDirection;
  end;

{$IF CompilerVersion <= 25} // xe4
type
  THorzRectAlign = (Center, Left, Right);
  TVertRectAlign = (Center, Top, Bottom);
{$ENDIF}

type

  TALPointFHelper = record helper for TPointF
    function RoundTo(const ADigit: TRoundToEXRangeExtended): TPointF;
  end;

  TALRectFHelper = record helper for TRectF
    function RoundTo(const ADigit: TRoundToEXRangeExtended): TRectF;
  end;

  TALPointDType = array [0..1] of Double;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.Types.TPointf still having the same implementation and adjust the IFDEF'}
  {$ENDIF}
  PALPointD = ^TALPointD;
  TALPointD = record
    class function Create(const AX, AY: Double): TALPointD; overload; static; inline;
    class function Create(const APoint: TPoint): TALPointD; overload; static; inline;
    class function Create(const APoint: TpointF): TALPointD; overload; static; inline;

    class operator Add(const APoint1, APoint2: TALPointD): TALPointD;
    class operator Subtract(const APoint1, APoint2: TALPointD): TALPointD;
    class operator Equal(const APoint1, APoint2: TALPointD): Boolean;
    class operator NotEqual(const APoint1, APoint2: TALPointD): Boolean;
    class operator Implicit(const APoint: TPoint): TALPointD;
    class operator Negative(const APoint: TALPointD): TALPointD;
    class operator Multiply(const APoint1, APoint2: TALPointD): TALPointD;
    class operator Multiply(const APoint: TALPointD; const AFactor: Double): TALPointD;
    class operator Multiply(const AFactor: Double; const APoint: TALPointD): TALPointD;
    class operator Divide(const APoint: TALPointD; const AFactor: Double): TALPointD;

    class function PointInCircle(const Point, Center: TALPointD; const Radius: Double): Boolean; static; inline;
    function IsInCircle(const Center: TALPointD; const Radius: Double): Boolean;

    /// <summary> Zero point having values of (0, 0). </summary>
    class function Zero: TALPointD; inline; static;

    function Distance(const APoint: TALPointD): Double;
    // 3D cross-product with Z = 0
    function CrossProduct(const APoint: TALPointD): Double;
    function DotProduct(const APoint: TALPointD): Double; inline;

    procedure Offset(const APoint: TALPointD); overload; inline;
    procedure Offset(const APoint: TPointf); overload; inline;
    procedure Offset(const ADeltaX, ADeltaY: Double); overload; inline;
    procedure Offset(const APoint: TPoint); overload; inline;

    procedure SetLocation(const X, Y: Double); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TALPointD); overload; deprecated 'Use ":=" assignment instead';
    procedure SetLocation(const P: TPoint); overload; deprecated 'Use ":=" assignment instead';
    function Subtract(const Point: TALPointD): TALPointD; overload; deprecated 'Use TALPointD.Offset instead';
    function Subtract(const Point: TPoint): TALPointD; overload; deprecated 'Use TALPointD.Offset instead';
    function Add(const Point: TALPointD): TALPointD; overload; deprecated 'Use TALPointD.Offset instead';
    function Add(const Point: TPoint): TALPointD; overload; deprecated 'Use TALPointD.Offset instead';
    function Scale(const AFactor: Double): TALPointD; deprecated;
    function EqualsTo(const Point: TALPointD; const Epsilon: Double = 0): Boolean;

    function IsZero: Boolean;
    function Ceiling: TPoint;
    function Truncate: TPoint;
    function Round: TPoint;
    function RoundTo(const ADigit: TRoundToEXRangeExtended): TALPointD;
    function ReducePrecision: TPointf;
    /// <summary> Rounds the current point to the specified scale value
    /// <param name="AScale"> The scale of scene </param>
    /// <param name="APlaceBetweenPixels"> If <c>True</c> (by default) the resulting point moves to half scale </param>
    /// </summary>
    /// <returns> The current point after transformation </returns>
    function SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean = True): TALPointD;

    function Normalize: TALPointD;
    function Length: Double;
    function Rotate(const AAngle: Double): TALPointD;
    function Reflect(const APoint: TALPointD): TALPointD; inline;
    function MidPoint(const APoint: TALPointD): TALPointD; inline;
    function AngleCosine(const APoint: TALPointD): Double;
    function Angle(const APoint: TALPointD): Double;

    function Abs: Double;

    case Integer of
      0: (V: TALPointDType;);
      1: (X: Double;
          Y: Double;);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.Types.TSizef still having the same implementation and adjust the IFDEF'}
  {$ENDIF}
  PALSizeD = ^TALSizeD;
  TALSizeD = record
    cx: Double;
    cy: Double;
  public
    constructor Create(P: TALSizeD); overload;
    constructor Create(P: TSizeF); overload;
    constructor Create(const X, Y: Double); overload;
    // operator overloads
    class operator Equal(const Lhs, Rhs: TALSizeD): Boolean;
    class operator NotEqual(const Lhs, Rhs: TALSizeD): Boolean;
    class operator Add(const Lhs, Rhs: TALSizeD): TALSizeD;
    class operator Subtract(const Lhs, Rhs: TALSizeD): TALSizeD;

    class operator Implicit(const Size: TALSizeD): TALPointD;
    class operator Implicit(const Point: TALPointD): TALSizeD;
    class operator Implicit(const Size: TSize): TALSizeD;

    function Ceiling: TSize;
    function Truncate: TSize;
    function Round: TSize;
    function RoundTo(const ADigit: TRoundToEXRangeExtended): TALSizeD;
    function ReducePrecision: TSizeF;

    // metods
    function Add(const Point: TALSizeD): TALSizeD;
    function Subtract(const Point: TALSizeD): TALSizeD;
    function Distance(const P2: TALSizeD): Double;
    function IsZero: Boolean;
    /// <summary>Returns size with swapped width and height</summary>
    function SwapDimensions: TALSizeD;
    // properties
    property Width: Double read cx write cx;
    property Height: Double read cy write cy;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if System.Types.TRectf still having the same implementation and adjust the IFDEF'}
  {$ENDIF}
  PALRectD = ^TALRectD;
  TALRectD = record
  private
    function GetWidth: Double;
    procedure SetWidth(const Value: Double);
    function GetHeight: Double;
    procedure SetHeight(const Value: Double);
    function GetSize: TALSizeD;
    procedure SetSize(const Value: TALSizeD);
    function GetLocation: TALPointD;
  public
    constructor Create(const Origin: TALPointD); overload;                              // empty rect at given origin
    constructor Create(const Origin: TALPointD; const Width, Height: Double); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Double); overload;               // at Left, Top, Right, and Bottom
    constructor Create(const P1, P2: TALPointD; Normalize: Boolean = False); overload;  // with corners specified by p1 and p2
    constructor Create(const R: TALRectD; Normalize: Boolean = False); overload;
    constructor Create(const R: TRectF; Normalize: Boolean = False); overload;
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TALRectD): Boolean;
    class operator NotEqual(const Lhs, Rhs: TALRectD): Boolean;
    class operator Implicit(const Source: TRect): TALRectD;
    {$IFNDEF ALDPK} // << else i receive Unsupported language feature: 'operator explicit'
    class operator Explicit(const Source: TALRectD): TRect;
    {$ENDIF}

    // union of two rectangles
    class operator Add(const Lhs, Rhs: TALRectD): TALRectD;

    // intersection of two rectangles
    class operator Multiply(const Lhs, Rhs: TALRectD): TALRectD;

    class function Empty: TALRectD; inline; static;

    { This method is to be deprecated. It stretches current rectangle into designated area similarly to FitInto,
      but only when current rectangle is bigger than the area; otherwise, it only centers it. }
    function Fit(const BoundsRect: TALRectD): Double; // deprecated 'Please consider using FitInto instead.';

    { Stretches current rectangle into the designated area, preserving aspect ratio. Note that unlike Fit, when designated
      area is bigger than current rectangle, the last one will be stretched to fill designated area (while Fit would only
      center it). }
    function FitInto(const ADesignatedArea: TALRectD; out Ratio: Double): TALRectD; overload;
    function FitInto(const ADesignatedArea: TALRectD): TALRectD; overload;

    /// <summary> Places the rectangle at center of designated area without scaling </summary>
    function CenterAt(const ADesignatedArea: TALRectD): TALRectD;

    /// <summary> This method places the rectangle inside the designated area. If the rectangle is greater
    /// than the designated area then the source rectangle is scaled with aspect ratio.
    /// </summary>
    /// <param name="ADesignatedArea"> The place in which the current rectangle will be placed </param>
    /// <param name="AHorzAlign"> The horizontal arrangement, if the width of the rectangle is smaller than the width
    /// of the designated area. The <b>Center</b> by default </param>
    /// <param name="AVertAlign"> The vertical arrangement, if the height of the rectangle is smaller than the height
    /// of the designated area. The <b>Center</b> by default </param>
    /// <returns> The current rectangle after transformation </returns>
    function PlaceInto(
               const ADesignatedArea: TALRectD;
               const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
               const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TALRectD;

    /// <summary> Rounds the location and size of the current rectangle to the specified value
    /// <param name="AScale"> The scale of scene </param>
    /// <param name="APlaceBetweenPixels"> If <c>True</c> (by default) the resulting rectangle moves to half scale </param>
    /// </summary>
    /// <returns> The current rectangle after transformation </returns>
    function SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean = True): TALRectD;

    //makes sure TopLeft is above and to the left of BottomRight
    procedure NormalizeRect;

    //returns true if left = right or top = bottom
    function IsEmpty: Boolean;

    //returns true if the point is inside the rect
    function Contains(const Pt: TALPointD): Boolean; overload;
    function Contains(const Pt: TPointf): Boolean; overload;

    // returns true if the rect encloses R completely
    function Contains(const R: TALRectD): Boolean; overload;

    // returns true if any part of the rect covers R
    function IntersectsWith(const R: TALRectD): Boolean;

    // computes an intersection of R1 and R2
    class function Intersect(const R1: TALRectD; const R2: TALRectD): TALRectD; overload; static;

    // replaces current rectangle with its intersection with R
    procedure Intersect(const R: TALRectD); overload;

    // computes a union of R1 and R2
    class function Union(const R1: TALRectD; const R2: TALRectD): TALRectD; overload; static;

    // replaces current rectangle with its union with R
    procedure Union(const R: TALRectD); overload;

    // creates a minimal rectangle that contains all points from array Points
    class function Union(const Points: Array of TALPointD): TALRectD; overload; static;

    // offsets the rectangle origin relative to current position
    procedure Offset(const DX, DY: Double); overload;
    procedure Offset(const Point: TALPointD); overload;
    procedure Offset(const Point: TPointf); overload;

    // sets new origin
    procedure SetLocation(const X, Y: Double); overload;
    procedure SetLocation(const Point: TALPointD); overload;
    procedure SetLocation(const Point: TPointf); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Double); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Double); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TALPointD;

    function Ceiling: TRect;
    function Truncate: TRect;
    function Round: TRect;
    function RoundTo(const ADigit: TRoundToEXRangeExtended): TALRectD;
    function ReducePrecision: TRectF;

    function EqualsTo(const R: TALRectD; const Epsilon: Double = 0): Boolean;

    {
    function SplitRect(SplitType: TSplitRectType; Size: Integer): TRect; overload;
    function SplitRect(SplitType: TSplitRectType; Percent: Double): TRect; overload;
    }

    // changing the width is always relative to Left;
    property Width: Double read GetWidth write SetWidth;
    // changing the Height is always relative to Top
    property Height: Double read GetHeight write SetHeight;

    property Size: TALSizeD read GetSize write SetSize;

    property Location: TALPointD read GetLocation write SetLocation;

  case Integer of
    0: (Left, Top, Right, Bottom: Double);
    1: (TopLeft, BottomRight: TALPointD);
  end;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if functions below implemented in System.Types still having the same implementation and adjust the IFDEF'}
{$ENDIF}
function ALRectWidth(const Rect: TRect): Integer; inline; overload;
function ALRectWidth(const Rect: TRectF): Single; inline; overload;
function ALRectWidth(const Rect: TALRectD): Double; inline; overload;
function ALRectHeight(const Rect: TRect): Integer; inline; overload;
function ALRectHeight(const Rect: TRectF): Single; inline; overload;
function ALRectHeight(const Rect: TALRectD): Double; inline; overload;
function ALOffsetRect(var R: TRect; DX, DY: Integer): Boolean; inline; overload;
function ALOffsetRect(var R: TRectf; DX, DY: single): Boolean; inline; overload;
function ALOffsetRect(var R: TALRectD; DX, DY: double): Boolean; overload;
function ALRectCenter(var R: TRect; const Bounds: TRect): TRect; inline; overload;
function ALRectCenter(var R: TRectf; const Bounds: TRectf): TRectf; inline; overload;
function ALRectCenter(var R: TALRectD; const Bounds: TALRectD): TALRectD; overload;
function ALIntersectRect(out Rect: TALRectD; const R1, R2: TALRectD): Boolean;
function ALUnionRect(out Rect: TALRectD; const R1, R2: TALRectD): Boolean;
function ALScaleRect(const Rect: TRectF; const Ratio: Single): TRectF; inline; overload;
function ALScaleRect(const Rect: TALRectD; const Ratio: Double): TALRectD; inline; overload;

{**************************************************************************************************************************}
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectF): TRectF; overload;
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF; overload;
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center; const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF; overload;
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectF; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center; const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF; overload;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  EALException = class(Exception)
  public
    constructor Create(const Msg: AnsiString); overload;
    constructor Create(const Msg: string); overload;
    constructor CreateFmt(const Msg: ansistring; const Args: array of const); overload;
    constructor CreateFmt(const Msg: string; const Args: array of const); overload;
  end;

{~~}
Type
  TalLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);
  TALCustomLogMsgProc = procedure(Const Tag: String; Const msg: String; const &Type: TalLogType) of object;
  TALCustomLogExceptionProc = procedure(Const Tag: String; Const E: Exception; const &Type: TalLogType) of object;

var
  ALCustomLogMsgProc: TALCustomLogMsgProc = nil;
  ALCustomLogExceptionProc: TALCustomLogExceptionProc = nil;

procedure _ALLog(
            Const Tag: String;
            Const msg: String;
            Const &Type: TalLogType;
            Const ThreadID: TThreadID;
            Const CanPreserve: boolean);
procedure ALLog(Const Tag: String; Const msg: String; const &Type: TalLogType = TalLogType.VERBOSE); overload;
procedure ALLog(Const Tag: String; const &Type: TalLogType = TalLogType.VERBOSE); overload;
procedure ALLog(Const Tag: String; Const E: Exception; const &Type: TalLogType = TalLogType.ERROR); overload;
procedure ALLog(Const Tag: String; const TagArgs: array of const; Const msg: String; const msgArgs: array of const; const &Type: TalLogType = TalLogType.VERBOSE); overload;
procedure ALLog(Const Tag: String; const Args: array of const; const &Type: TalLogType = TalLogType.VERBOSE); overload;
procedure ALLog(Const Tag: String; const Args: array of const; Const E: Exception; const &Type: TalLogType = TalLogType.ERROR); overload;

var ALMaxLogHistory: integer = 0;
function ALGetLogHistory(const AIgnoreLastLogItemMsg: Boolean = False): String;

Var ALEnqueueLog: Boolean = False; // We can use this flag to enqueue the log when the device just started and when we didn't yet
procedure ALPrintLogQueue;         // pluged the device to the monitoring tool, so that we can print the log a little later

{~~}
type
  TALCustomDelayedFreeObjectProc = procedure(var aObject: Tobject) of object;

var
  ALCustomDelayedFreeObjectProc: TALCustomDelayedFreeObjectProc = nil;

{$IF CompilerVersion >= 34} // sydney
Procedure ALFreeAndNil(const [ref] Obj: TObject; const ADelayed: boolean = false); inline;
{$ELSE}
Procedure ALFreeAndNil(var Obj; const ADelayed: boolean = false); inline;
{$ENDIF}

{******************************************}
Function AlBoolToInt(Value:Boolean):Integer;
Function AlIntToBool(Value:integer):boolean;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
function ALTryRGBAHexToAlphaColor(const aHexValue: String; out AAlphaColor: TAlphaColor): Boolean;
function ALRGBAHexToAlphaColor(const aHexValue: String): TAlphaColor;
function ALTryARGBHexToAlphaColor(const aHexValue: String; out AAlphaColor: TAlphaColor): Boolean;
function ALARGBHexToAlphaColor(const aHexValue: String): TAlphaColor;
function ALCeil(const X: Single; const Epsilon: Single = 0): Integer; overload;
function ALCeil(const X: Double; const Epsilon: Double = 0): Integer; overload;
function ALCeil(const X: Extended; const Epsilon: Extended = 0): Integer; overload;
function ALFloor(const X: Single; const Epsilon: Single = 0): Integer; overload;
function ALFloor(const X: Double; const Epsilon: Double = 0): Integer; overload;
function ALFloor(const X: Extended; const Epsilon: Extended = 0): Integer; overload;

{**************************************************************************************************************}
function  ALIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64 = 0): UInt64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single = 0): Single; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended = 0): Extended; overload; inline;
function  ALIfThenA(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString; overload; inline;
function  ALIfThenW(AValue: Boolean; const ATrue: String; AFalse: String = ''): String; overload; inline;

const
  ALMsPerSec = 1000;
  ALNanosPerMs = 1000000;
  ALNanosPerSec = 1000000000;

{********************************}
function ALElapsedTimeNano: int64;
function ALElapsedTimeMillisAsDouble: Double;
function ALElapsedTimeMillisAsInt64: int64;
function ALElapsedTimeSecondsAsDouble: Double;
function ALElapsedTimeSecondsAsInt64: int64;

{$IFDEF MSWINDOWS}
{$IFNDEF ALCompilerVersionSupported122}
  {$MESSAGE WARN 'Check if EnumDynamicTimeZoneInformation/SystemTimeToTzSpecificLocalTimeEx/TzSpecificLocalTimeToSystemTimeEx are still not declared in Winapi.Windows and adjust the IFDEF'}
{$ENDIF}
{$WARNINGS OFF}
function EnumDynamicTimeZoneInformation(dwIndex: DWORD; lpTimeZoneInformation: PDynamicTimeZoneInformation): DWORD; stdcall; external advapi32 delayed;
function SystemTimeToTzSpecificLocalTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpUniversalTime, lpLocalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
function TzSpecificLocalTimeToSystemTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
{$WARNINGS ON}
Function ALGetDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
Function ALGetDynamicTimeZoneInformation(const aTimeZoneKeyName: String): TDynamicTimeZoneInformation;
function ALFileTimeToDateTime(const AFileTime: TFileTime): TDateTime;
{$ENDIF}
function AlLocalDateTimeToUTC(Const aLocalDateTime: TDateTime): TdateTime; overload;
function AlUTCDateTimeToLocal(Const aUTCDateTime: TDateTime): TdateTime; overload;
{$IFDEF MSWINDOWS}
Function AlLocalDateTimeToUTC(const aTimeZoneKeyName: String; aLocalDateTime: TdateTime): TdateTime; overload;
Function AlLocalDateTimeToUTC(const aTimeZoneInformation: TDynamicTimeZoneInformation; aLocalDateTime: TdateTime): TdateTime; overload;
Function AlUTCDateTimeToLocal(const aTimeZoneKeyName: String; aUTCDateTime: TdateTime): TdateTime; overload;
Function AlUTCDateTimeToLocal(const aTimeZoneInformation: TDynamicTimeZoneInformation; aUTCDateTime: TdateTime): TdateTime; overload;
{$ENDIF}
{$IFDEF IOS}
function ALNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
{$ENDIF}
function ALUTCNow: TDateTime;
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
Function ALInc(var x: integer; Count: integer): Integer;
procedure ALAssignError(Const ASource: TObject; const ADest: Tobject);
var ALMove: procedure (const Source; var Dest; Count: NativeInt);
{$IFDEF MSWINDOWS}
type
  TALConsoleColor = (
    ccRed,
    ccDarkRed,
    ccBlue,
    ccDarkBlue,
    ccGreen,
    ccDarkGreen,
    ccYellow,
    ccDarkYellow,
    ccAqua,
    ccDarkAqua,
    ccPurple,
    ccDarkPurple,
    ccGrey,
    ccBlack,
    ccWhite,
    ccDarkWhite);

function ALConsoleForegroundColorToCode(const AColor : TALConsoleColor): Word;
function ALConsoleBackgroundColorToCode(const AColor : TALConsoleColor): Word;
function ALConsoleCodeToForegroundColor(const ACode : Word): TALConsoleColor;
function ALConsoleCodeToBackgroundColor(const ACode : Word): TALConsoleColor;
procedure ALGetConsoleColors(out AForegroundColor: TALConsoleColor; out ABackgroundColor: TALConsoleColor);
procedure ALSetConsoleColors(const AForegroundColor: TALConsoleColor; const ABackgroundColor: TALConsoleColor);
Procedure ALWriteLN(const AStr: AnsiString); overload;
Procedure ALWriteLN(const AStr: AnsiString; const aForegroundColor: TALConsoleColor); overload;
Procedure ALWriteLN(const AStr: AnsiString; const aForegroundColor: TALConsoleColor; const aBackgroundColor: TALConsoleColor); overload;
Procedure ALWriteLN(const AStr: String); overload;
Procedure ALWriteLN(const AStr: String; const aForegroundColor: TALConsoleColor); overload;
Procedure ALWriteLN(const AStr: String; const aForegroundColor: TALConsoleColor; const aBackgroundColor: TALConsoleColor); overload;
{$ENDIF}


{~~~}
const

  ALMAXUInt64: UInt64 = 18446744073709551615;
  ALMAXInt64: Int64 = 9223372036854775807;
  ALMAXUInt: cardinal = 4294967295;
  ALMAXInt: int32 = 2147483647;
  ALNullDate = 0; // There are no TDateTime values from –1 through 0
                  // dt := -0.5;
                  // writeln(formatFloat('0.0', dt));                    => -0.5
                  // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000
                  //
                  // dt := encodedatetime(1899,12,30,12,00,00,000);
                  // writeln(formatFloat('0.0', dt));                    => 0.5
                  // writeln(DateTimeToStr(dt));                         => 1899/12/30 12:00:00.000
                  //
                  // also -0.5 have the advantage to be in the form
                  // m*2^e (-1*2^-1) with mean we don't need to use
                  // samevalue to compare
                  // https://stackoverflow.com/questions/41779801/single-double-and-precision
                  //
                  // but finally -0.5 have a big drawback, if you convert it to string and back
                  // to datetime then you will obtain 0.5 ! same if you convert it to unix and
                  // back to datetime :( so i decide that 0 if more suitable than -0.5
  ALNullLatLng = 999; // 999 because 0 is a valid latitude/longitude

implementation

uses
  system.Rtti,
  System.RTLConsts,
  {$IF defined(MSWindows)}
  Winapi.MMSystem,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Posix.Sched,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Alcinoe.AndroidApi.Common,
  Posix.Time,
  {$ENDIF}
  {$IF defined(IOS)}
  Posix.Sched,
  Macapi.Helpers,
  Macapi.Mach,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.Mach,
  {$ENDIF}
  system.DateUtils,
  System.UIConsts,
  System.Diagnostics,
  Alcinoe.StringUtils;

{****************************************}
constructor TALWorkerThreadRequest.Create(
              const AProc: TALWorkerThreadRefProc;
              const AContext: Tobject;
              const APriority: Int64;
              const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
              const ASignal: TEvent);
begin
  FRefProc := AProc;
  FContext := AContext;
  FPriority := APriority;
  FGetPriorityFunc := AGetPriorityFunc;
  FSignal := ASignal;
end;

{****************************************}
constructor TALWorkerThreadRequest.Create(
              const AProc: TALWorkerThreadObjProc;
              const AContext: Tobject;
              const APriority: Int64;
              const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
              const ASignal: TEvent);
begin
  FObjProc := AProc;
  FContext := AContext;
  FPriority := APriority;
  FGetPriorityFunc := AGetPriorityFunc;
  FSignal := ASignal;
end;

{****************************************}
destructor TALWorkerThreadRequest.Destroy;
begin
  ALFreeAndNil(FContext);
  inherited;
end;

{*************************************************}
function TALWorkerThreadRequest.GetPriority: Int64;
begin
  if Assigned(FGetPriorityFunc) then result := fGetPriorityFunc(FContext)
  else result := FPriority;
end;

{*******************************************************************************}
constructor TALWorkerThread.Create(const aWorkerThreadPool: TALWorkerThreadPool);
begin
  inherited Create(True);
  FSignal := TEvent.Create(nil{EventAttributes}, false{ManualReset}, false{InitialState}, ''{Name});
  FWaiting := False;
  FWorkerThreadPool := aWorkerThreadPool;
  {$IF defined(ANDROID) or defined(IOS)}
  Policy := SCHED_OTHER;
  priority := sched_get_priority_min(SCHED_OTHER);
  {$ENDIF}
end;

{*********************************}
destructor TALWorkerThread.Destroy;
begin
  Terminate;
  FSignal.setevent;
  WaitFor;
  ALfreeandNil(FSignal);
  inherited;
end;

{********************************}
procedure TALWorkerThread.Execute;
begin
  while not terminated do begin

    //Try to get a request in the queue
    var LWorkerThreadRequest := FWorkerThreadPool.DequeueRequest;
    if LWorkerThreadRequest <> nil then begin
      try
        try
          {$IF defined(ios)}
          //If you write a loop that creates many temporary objects.
          //You may use an autorelease pool block inside the loop to
          //dispose of those objects before the next iteration. Using an
          //autorelease pool block in the loop helps to reduce the maximum
          //memory footprint of the application.
          var LAutoReleasePool := TNSAutoreleasePool.Create;
          try
          {$ENDIF}
            {$IFDEF DEBUG}
            //ALLog(
            //  ClassName + '.Execute',
            //  'Request.Priority:' + ALIntToStrW(LWorkerThreadRequest.Priority) + ' | ' +
            //  'PriorityStartingPoint:' + ALIntToStrW(FWorkerThreadPool.PriorityStartingPoint) + ' | ' +
            //  'PriorityDirection:' + TRttiEnumerationType.GetName(FWorkerThreadPool.PriorityDirection));
            {$ENDIF}
            if assigned(LWorkerThreadRequest.ObjProc) then LWorkerThreadRequest.ObjProc(LWorkerThreadRequest.FContext)
            else LWorkerThreadRequest.RefProc(LWorkerThreadRequest.FContext);
          {$IF defined(ios)}
          finally
            LAutoReleasePool.release;
          end;
          {$ENDIF}
        finally
          if LWorkerThreadRequest.Signal <> nil then LWorkerThreadRequest.Signal.SetEvent;
          alFreeAndNil(LWorkerThreadRequest);
        end;
      except
        //hide the exception
      end;
    end

    // else wait the signal
    else begin
      FWaiting := True;
      fSignal.WaitFor(INFINITE);
      FWaiting := False;
    end;

  end;
end;

{*********************************************************************}
constructor TALWorkerThreadPool.Create(const AMaxThreadCount: integer);
begin
  FMaxThreadCount := AMaxThreadCount;
  fPriorityStartingPoint := 0;
  fPriorityDirection := TPriorityDirection.GreaterThan;
  fThreads:= TObjectList<TALWorkerThread>.create(true{aOwnObjects});
  fRequests:= TObjectList<TALWorkerThreadRequest>.create(true{aOwnObjects});
end;

{*************************************}
destructor TALWorkerThreadPool.Destroy;
begin
  ALFreeandNil(fThreads);
  ALFreeandNil(fRequests);
  inherited;
end;

{********************************************************************}
function TALWorkerThreadPool.GetPriorityDirection: TPriorityDirection;
begin
  //sizeof fPriorityDirection is 1 that mean read and write are ATOMIC
  //no need to use any Interlock mechanism
  result := fPriorityDirection;
end;

{**********************************************************************************}
procedure TALWorkerThreadPool.SetPriorityDirection(const Value: TPriorityDirection);
begin
  //sizeof fPriorityDirection is 1 that mean read and write are ATOMIC
  //no need to use any Interlock mechanism
  fPriorityDirection := Value;
end;

{***********************************************************}
function TALWorkerThreadPool.GetPriorityStartingPoint: int64;
begin
  result := AtomicCmpExchange(FPriorityStartingPoint{Target},0{NewValue},0{Comparand});
end;

{***************************************************************************************}
function TALWorkerThreadPool.GetPriorityStartingPointExt(const AContext: Tobject): Int64;
begin
  result := GetPriorityStartingPoint;
end;

{*************************************************************************}
procedure TALWorkerThreadPool.SetPriorityStartingPoint(const Value: int64);
begin
  AtomicExchange(FPriorityStartingPoint{Target},Value{Value});
end;

{***********************************************************************************}
procedure TALWorkerThreadPool.EnqueueRequest(const ARequest: TALWorkerThreadRequest);
begin

  //add the request in fRequests
  TMonitor.Enter(fRequests);
  try
    fRequests.Add(ARequest);
  finally
    TMonitor.Exit(fRequests);
  end;

  //-----
  TMonitor.Enter(fThreads);
  try

    //look if their is a WAITING thread available
    for var i := 0 to fThreads.Count - 1 do begin
      if fThreads[i].Waiting then begin
        fThreads[i].Waiting := False;
        fThreads[i].Signal.SetEvent;
        Exit;
      end;
    end;

    //if their no waiting thread available AND lower than FMaxThreadCount working thread: create a new one
    if (fThreads.Count < FMaxThreadCount) then begin
      var LWorkerThread := TALWorkerThread.Create(self);
      try
        fThreads.Add(LWorkerThread);
      except
        ALFreeandNil(LWorkerThread);
        raise;
      end;
      LWorkerThread.Start;
    end

    //else all the threads are busy just signal all of then (in case)
    else begin
      for var i := 0 to fThreads.Count - 1 do
        fThreads[i].Signal.SetEvent;
    end;

  finally
    TMonitor.Exit(fThreads);
  end;

end;

{******************************************************************}
function TALWorkerThreadPool.DequeueRequest: TALWorkerThreadRequest;
begin
  var LPriorityStartingPoint := GetPriorityStartingPoint;
  TMonitor.Enter(fRequests);
  try

    var LLesserThanFoundIndex := -1;
    Var LLesserThanFoundCompare := -ALMaxint64;
    var LGreaterThanFoundIndex := -1;
    Var LGreaterThanFoundCompare := ALMaxint64;

    for Var I := 0 to fRequests.Count - 1 do begin
      var LCompare := fRequests[i].Priority - LPriorityStartingPoint;
      if (LCompare > 0) then begin
        if (LCompare < LGreaterThanFoundCompare) then begin
          LGreaterThanFoundCompare := LCompare;
          LGreaterThanFoundIndex := i;
        end
      end
      else if (LCompare < 0) then begin
        if (LCompare > LLesserThanFoundCompare) then begin
          LLesserThanFoundCompare := LCompare;
          LLesserThanFoundIndex := i;
        end
      end
      else begin
        LGreaterThanFoundIndex := i;
        LLesserThanFoundIndex := i;
        break;
      end;
    end;

    if (PriorityDirection = TPriorityDirection.GreaterThan) then begin
      if LGreaterThanFoundIndex > -1 then result := fRequests.ExtractAt(LGreaterThanFoundIndex)
      else if LLesserThanFoundIndex > -1 then result := fRequests.ExtractAt(LLesserThanFoundIndex)
      else result := nil;
    end
    else begin
      if LLesserThanFoundIndex > -1 then result := fRequests.ExtractAt(LLesserThanFoundIndex)
      else if LGreaterThanFoundIndex > -1 then result := fRequests.ExtractAt(LGreaterThanFoundIndex)
      else result := nil;
    end;

  finally
    TMonitor.Exit(fRequests);
  end;
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  Var LSignal: TEvent := nil;
  if not AAsync then LSignal := TEvent.Create(nil{EventAttributes}, false{ManualReset}, false{InitialState}, ''{Name});
  try
    var LWorkerThreadRequest := TALWorkerThreadRequest.Create(
                                  AProc,
                                  AContext,
                                  APriority,
                                  AGetPriorityFunc,
                                  LSignal);
    try
      EnqueueRequest(LWorkerThreadRequest);
    except
      ALFreeAndNil(LWorkerThreadRequest);
      Raise;
    end;
    if assigned(LSignal) then LSignal.WaitFor(INFINITE);
  finally
    ALFreeAndNil(LSignal);
  end;
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, APriority, nil{AGetPriorityFunc}, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, 0{APriority}, AGetPriorityFunc, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const APriority: Int64;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, APriority, nil{AGetPriorityFunc}, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, 0{APriority}, AGetPriorityFunc, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            const AContext: Tobject; // Context will be free by the worker thread
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, 0{APriority}, GetPriorityStartingPointExt, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadRefProc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, 0{APriority}, GetPriorityStartingPointExt, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  Var LSignal: TEvent := nil;
  if not AAsync then LSignal := TEvent.Create(nil{EventAttributes}, false{ManualReset}, false{InitialState}, ''{Name});
  try
    var LWorkerThreadRequest := TALWorkerThreadRequest.Create(
                                  AProc,
                                  AContext,
                                  APriority,
                                  AGetPriorityFunc,
                                  LSignal);
    try
      EnqueueRequest(LWorkerThreadRequest);
    except
      ALFreeAndNil(LWorkerThreadRequest);
      Raise;
    end;
    if assigned(LSignal) then LSignal.WaitFor(INFINITE);
  finally
    ALFreeAndNil(LSignal);
  end;
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const APriority: Int64;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, APriority, nil{AGetPriorityFunc}, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, 0{APriority}, AGetPriorityFunc, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const APriority: Int64;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, APriority, nil{AGetPriorityFunc}, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, 0{APriority}, AGetPriorityFunc, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            const AContext: Tobject; // Context will be free by the worker thread
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, AContext, 0{APriority}, GetPriorityStartingPointExt, AAsync);
end;

{****************************************}
procedure TALWorkerThreadPool.ExecuteProc(
            const AProc: TALWorkerThreadObjProc;
            Const AAsync: Boolean = True);
begin
  ExecuteProc(AProc, nil{AContext}, 0{APriority}, GetPriorityStartingPointExt, AAsync);
end;

{***********************************************}
function ALRectWidth(const Rect: TRect): Integer;
begin
  Result := RectWidth(Rect);
end;

{***********************************************}
function ALRectWidth(const Rect: TRectF): Single;
begin
  Result := RectWidth(Rect);
end;

{*************************************************}
function ALRectWidth(const Rect: TALRectD): Double;
begin
  Result := Rect.Right - Rect.Left;
end;

{************************************************}
function ALRectHeight(const Rect: TRect): Integer;
begin
  Result := RectHeight(Rect);
end;

{************************************************}
function ALRectHeight(const Rect: TRectF): Single;
begin
  Result := RectHeight(Rect);
end;

{**************************************************}
function ALRectHeight(const Rect: TALRectD): Double;
begin
  Result := Rect.Bottom - Rect.Top;
end;

{**************************************************************}
function ALRectCenter(var R: TRect; const Bounds: TRect): TRect;
begin
  result := RectCenter(R, Bounds);
end;

{*****************************************************************}
function ALRectCenter(var R: TRectf; const Bounds: TRectf): TRectf;
begin
  result := RectCenter(R, Bounds);
end;

{***********************************************************************}
function ALRectCenter(var R: TALRectD; const Bounds: TALRectD): TALRectD;
begin
  ALOffsetRect(R, -R.Left, -R.Top);
  ALOffsetRect(R, (ALRectWidth(Bounds)/2 - ALRectWidth(R)/2), (ALRectHeight(Bounds)/2 - ALRectHeight(R)/2));
  ALOffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

{************************************************************}
function ALOffsetRect(var R: TRect; DX, DY: Integer): Boolean;
begin
  result := OffsetRect(R, DX, DY);
end;

{************************************************************}
function ALOffsetRect(var R: TRectf; DX, DY: single): Boolean;
begin
  result := system.types.OffsetRect(R, DX, DY);
end;

{**************************************************************}
function ALOffsetRect(var R: TALRectD; DX, DY: double): Boolean;
begin
  if @R <> nil then // Test to increase compatiblity with Windows
  begin
    R.Left := R.Left + DX;
    R.Right := R.Right + DX;
    R.Top := R.Top + DY;
    R.Bottom := R.Bottom + DY;
    Result := True;
  end
  else
    Result := False;
end;

{****************************************************************************}
function ALIntersectRect(out Rect: TALRectD; const R1, R2: TALRectD): Boolean;
var
  tmpRect: TALRectD;
begin
  tmpRect := R1;
  if R2.Left > R1.Left then tmpRect.Left := R2.Left;
  if R2.Top > R1.Top then tmpRect.Top := R2.Top;
  if R2.Right < R1.Right then tmpRect.Right := R2.Right;
  if R2.Bottom < R1.Bottom then tmpRect.Bottom := R2.Bottom;
  Result := not tmpRect.IsEmpty;
  if not Result then
  begin
    tmpRect.Top := 0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

{************************************************************************}
function ALUnionRect(out Rect: TALRectD; const R1, R2: TALRectD): Boolean;
var
  tmpRect: TALRectD;
begin
  tmpRect := R1;
  if not ((R2.Right < R2.Left) or (R2.Bottom < R2.Top)) then
  begin
    if R2.Left < R1.Left then tmpRect.Left := R2.Left;
    if R2.Top < R1.Top then tmpRect.Top := R2.Top;
    if R2.Right > R1.Right then tmpRect.Right := R2.Right;
    if R2.Bottom > R1.Bottom then tmpRect.Bottom := R2.Bottom;
  end;
  Result := not tmpRect.IsEmpty;
  if not Result then
  begin
    tmpRect.Top :=0.0;
    tmpRect.Bottom := 0.0;
    tmpRect.Left := 0.0;
    tmpRect.Right := 0.0;
  end;
  Rect := tmpRect;
end;

{********************************************************************}
function ALScaleRect(const Rect: TRectF; const Ratio: Single): TRectF;
begin
  Result := Rect;
  Result.Top := Result.Top * Ratio;
  Result.Bottom := Result.Bottom * Ratio;
  Result.Left := Result.Left * Ratio;
  Result.Right := Result.Right * Ratio;
end;

{************************************************************************}
function ALScaleRect(const Rect: TALRectD; const Ratio: Double): TALRectD;
begin
  Result := Rect;
  Result.Top := Result.Top * Ratio;
  Result.Bottom := Result.Bottom * Ratio;
  Result.Left := Result.Left * Ratio;
  Result.Right := Result.Right * Ratio;
end;

{***********************************************************************************************}
//Resizes the current rectangle, preserving the current rectangle proportions, to best fit in the
//bounds rectangle, and returns the scaled rectangle centered in bounds.
//FitInto implements the following functionality:
// * If any of the current rectangle dimensions is greater than the corresponding dimension of the bounds
//   rectangle, then FitInto scales down the current rectangle to fit into bounds. The scaled rectangle is centered in
//   the bounds rectangle at CenterAt and the obtained scaled and centered rectangle is returned.
// * If both width and height of the current rectangle dimensions is smaller than the corresponding dimensions of the
//   bounds rectangle, then FitInto stretches the current rectangle to best fit into bounds. The stretched
//   rectangle is centered in the bounds rectangle at CenterAt and the obtained stretched and centered rectangle is returned.
// * If any of the bounds dimensions is zero then FitInto returns the current rectangle and sets Ratio equals to 1.
//Ratio is the implemented scaling ratio.
//CenterAt is where we need to center the result in the bounds (ex: center the result on a face instead of the middle of the bounds)
//if center contain negative value then it's indicate percentage
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF;
begin

  if (Bounds.Width <= 0) or (Bounds.Height <= 0) then
  begin
    Ratio := 1;
    Exit(R);
  end;

  if (R.Width / Bounds.Width) > (R.Height / Bounds.Height) then
    Ratio := R.Width / Bounds.Width
  else
    Ratio := R.Height / Bounds.Height;

  if Ratio = 0 then
    Exit(R)
  else
  begin

    Result := TRectF.Create(0, 0, R.Width / Ratio, R.Height / Ratio);

    system.types.OffsetRect(Result, -Result.Left, -Result.Top);
    if (CenterAt.X < 0) or (CenterAt.y < 0) then system.types.OffsetRect(Result, max(0, (((Bounds.Width) / 100) * -CenterAt.x) - (Result.Width / 2)), max(0, (((Bounds.Height) / 100) * -CenterAt.Y) - (Result.height / 2)))
    else system.types.OffsetRect(Result, max(0, CenterAt.x - (Result.Width / 2)), max(0, CenterAt.y - (Result.height / 2)));
    system.types.OffsetRect(Result, -max(0, Result.Right - Bounds.Width), -max(0, Result.bottom - Bounds.height));
    system.types.OffsetRect(Result, Bounds.Left, Bounds.Top);

  end;

end;

{*********************************************************************************************}
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF;
var
  Ratio: Single;
begin
  Result := ALRectFitInto(R, Bounds, CenterAt, Ratio);
end;

{******************************************************************************************************************}
//this is the same as TRectf.fitInto but it is here for old delphi version (like xe4) with don't have it implemented
function ALRectFitInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single): TRectF;
begin
  if (Bounds.Width <= 0) or (Bounds.Height <= 0) then
  begin
    Ratio := 1;
    Exit(R);
  end;

  if (R.Width / Bounds.Width) > (R.Height / Bounds.Height) then
    Ratio := R.Width / Bounds.Width
  else
    Ratio := R.Height / Bounds.Height;

  if Ratio = 0 then
    Exit(R)
  else
  begin
    Result := TRectF.Create(0, 0, R.Width / Ratio, R.Height / Ratio);
    RectCenter(Result, Bounds);
  end;
end;

{******************************************************************************************************************}
//this is the same as TRectf.fitInto but it is here for old delphi version (like xe4) with don't have it implemented
function ALRectFitInto(const R: TRectf; const Bounds: TRectF): TRectF;
var
  Ratio: Single;
begin
  Result := ALRectFitInto(R, Bounds, Ratio);
end;

{**************************************************************************************************************}
//If any dimension of the current rectangle is greater than the corresponding dimension of the Bounds rectangle,
//then the current rectangle is scaled down to best fit the Bounds rectangle. The obtained rectangle is aligned in Bounds.
//PlaceInto implements the following behavior:
// * If the width or height of the current rectangle is greater than the corresponding dimension of Bounds.
//   Then PlaceInto scales down the current rectangle (preserving the current rectangle proportions – the ratio between the width
//   and height) to fit in the Bounds rectangle and centers the scaled rectangle in Bounds at CenterAt.
// * Otherwise, PlaceInto just center the current rectangle in the Bounds rectangle according to CenterAt
// * PlaceInto returns the current rectangle if any of the Bounds dimensions is zero.
function ALRectPlaceInto(
           const R: TRectf;
           const Bounds: TRectf;
           const CenterAt: TpointF; // << this is used only when we need to fit the result
           out Ratio: Single): TRectF;
begin

  if (R.Width > Bounds.Width) or (R.Height > Bounds.Height) then Result := ALRectFitInto(R, Bounds, CenterAt, Ratio)
  else begin

    Result := R;
    system.types.OffsetRect(Result, -Result.Left, -Result.Top);
    if (CenterAt.X < 0) or (CenterAt.y < 0) then system.types.OffsetRect(Result, max(0, (((Bounds.Width) / 100) * -CenterAt.x) - (Result.Width / 2)), max(0, (((Bounds.Height) / 100) * -CenterAt.Y) - (Result.height / 2)))
    else system.types.OffsetRect(Result, max(0, CenterAt.x - (Result.Width / 2)), max(0, CenterAt.y - (Result.height / 2)));
    system.types.OffsetRect(Result, -max(0, Result.Right - Bounds.Width), -max(0, Result.bottom - Bounds.height));
    system.types.OffsetRect(Result, Bounds.Left, Bounds.Top);

  end;

end;

{***********************************************************************************************}
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF;
var
  Ratio: Single;
begin
  Result := ALRectPlaceInto(R, Bounds, CenterAt, Ratio);
end;

{********************************************************************************************************************}
//this is the same as TRectf.PlaceInto but it is here for old delphi version (like xe4) with don't have it implemented
function ALRectPlaceInto(
           const R: TRectf;
           const Bounds: TRectF;
           out Ratio: Single;
           const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
           const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
var
  LLocation: TPointF;
begin
  Result := R;
  if (R.Width > Bounds.Width) or (R.Height > Bounds.Height) then
    Result := ALRectFitInto(Result, Bounds, Ratio)
 else
    Ratio := 1;
  case AHorzAlign of
    THorzRectAlign.Center: LLocation.X := (Bounds.Left + Bounds.Right - Result.Width) / 2;
    THorzRectAlign.Left: LLocation.X := Bounds.Left;
    THorzRectAlign.Right: LLocation.X := Bounds.Right - Result.Width;
  end;
  case AVertAlign of
    TVertRectAlign.Center: LLocation.Y := (Bounds.Top + Bounds.Bottom - Result.Height) / 2;
    TVertRectAlign.Top: LLocation.Y := Bounds.Top;
    TVertRectAlign.Bottom: LLocation.Y := Bounds.Bottom - Result.Height;
  end;
  Result.SetLocation(LLocation);
end;

{********************************************************************************************************************}
//this is the same as TRectf.PlaceInto but it is here for old delphi version (like xe4) with don't have it implemented
function ALRectPlaceInto(
           const R: TRectf;
           const Bounds: TRectF;
           const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
           const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
var
  LRatio: Single;
begin
  Result := ALRectPlaceInto(R, Bounds, LRatio, AHorzAlign, AVertAlign);
end;

{*******************************************************************************}
function TALPointFHelper.RoundTo(const ADigit: TRoundToEXRangeExtended): TPointF;
begin
  Result.X := System.math.RoundTo(X, ADigit);
  Result.Y := System.math.RoundTo(Y, ADigit);
end;

{*****************************************************************************}
function TALRectFHelper.RoundTo(const ADigit: TRoundToEXRangeExtended): TRectF;
begin
  Result.TopLeft := TopLeft.RoundTo(ADigit);
  Result.BottomRight := BottomRight.RoundTo(ADigit);
end;

{***************************************************************}
class function TALPointD.Create(const AX, AY: Double): TALPointD;
begin
  Result.X := AX;
  Result.Y := AY;
end;

{***************************************************************}
class function TALPointD.Create(const APoint: TPoint): TALPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{****************************************************************}
class function TALPointD.Create(const APoint: TPointF): TALPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{*************************************************************************}
class operator TALPointD.Add(const APoint1, APoint2: TALPointD): TALPointD;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
end;

{******************************************************************************}
class operator TALPointD.Subtract(const APoint1, APoint2: TALPointD): TALPointD;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
end;

{*************************************************************************}
class operator TALPointD.Equal(const APoint1, APoint2: TALPointD): Boolean;
begin
  Result := SameValue(APoint1.X, APoint2.X) and SameValue(APoint1.Y, APoint2.Y);
end;

{**********************************************************************************}
function TALPointD.EqualsTo(const Point: TALPointD; const Epsilon: Double): Boolean;
begin
  Result := SameValue(X, Point.X, Epsilon) and SameValue(Y, Point.Y, Epsilon);
end;

{****************************************************************************}
class operator TALPointD.NotEqual(const APoint1, APoint2: TALPointD): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{*****************************************************************}
class operator TALPointD.Implicit(const APoint: TPoint): TALPointD;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

{********************************************************************}
class operator TALPointD.Negative(const APoint: TALPointD): TALPointD;
begin
  Result.X := - APoint.X;
  Result.Y := - APoint.Y;
end;

{******************************************************************************}
class operator TALPointD.Multiply(const APoint1, APoint2: TALPointD): TALPointD;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
end;

{*******************************************************************************************}
class operator TALPointD.Multiply(const APoint: TALPointD; const AFactor: Double): TALPointD;
begin
  Result.X := APoint.X * AFactor;
  Result.Y := APoint.Y * AFactor;
end;

{*******************************************************************************************}
class operator TALPointD.Multiply(const AFactor: Double; const APoint: TALPointD): TALPointD;
begin
  Result.X := AFactor * APoint.X;
  Result.Y := AFactor * APoint.Y;
end;

{*****************************************************************************************}
class operator TALPointD.Divide(const APoint: TALPointD; const AFactor: Double): TALPointD;
var
  InvFactor: Double;
begin
  if AFactor <> 0 then
  begin
    InvFactor := 1 / AFactor;

    Result.X := APoint.X * InvFactor;
    Result.Y := APoint.Y * InvFactor;
  end else
    Result := APoint;
end;

{********************************************************}
function TALPointD.Add(const Point: TALPointD): TALPointD;
begin
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
end;

{*****************************************************}
function TALPointD.Add(const Point: TPoint): TALPointD;
begin
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
end;

{***********************************************************}
function TALPointD.Distance(const APoint: TALPointD): Double;
begin
  Result := (APoint - Self).Length;
end;

{***************************************************************}
function TALPointD.CrossProduct(const APoint: TALPointD): Double;
begin
  Result := Self.X * APoint.Y - Self.Y * APoint.X;
end;

{*************************************************************}
function TALPointD.DotProduct(const APoint: TALPointD): Double;
begin
  Result := (Self.X * APoint.X) + (Self.Y * APoint.Y);
end;

{**************************************************}
procedure TALPointD.Offset(const APoint: TALPointD);
begin
  Self := Self + APoint;
end;

{************************************************}
procedure TALPointD.Offset(const APoint: TPointf);
begin
  Self.Offset(TALPointD.Create(APoint));
end;

{*********************************************************}
procedure TALPointD.Offset(const ADeltaX, ADeltaY: Double);
begin
  Self.Offset(TALPointD.Create(ADeltaX, ADeltaY));
end;

{***********************************************}
procedure TALPointD.Offset(const APoint: TPoint);
begin
  Self.Offset(TALPointD.Create(APoint));
end;

{************************************************************************************}
function TALPointD.IsInCircle(const Center: TALPointD; const Radius: Double): Boolean;
var
  D: Double;
begin
  D := Distance(Center);
  Result := (D < Radius) or SameValue(D, Radius);
end;

{****************************************************************************************************}
class function TALPointD.PointInCircle(const Point, Center: TALPointD; const Radius: Double): Boolean;
begin
  Result := Point.IsInCircle(Center, Radius);
end;

{***************************************}
class function TALPointD.Zero: TALPointD;
begin
  Result.X := 0;
  Result.Y := 0;
end;

{*********************************}
function TALPointD.IsZero: Boolean;
begin
  Result := SameValue(X, 0.0) and SameValue(Y, 0.0);
end;

{*********************************}
function TALPointD.Ceiling: TPoint;
begin
  Result.X := Ceil(X);
  Result.Y := Ceil(Y);
end;

{**********************************}
function TALPointD.Truncate: TPoint;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
end;

{*******************************}
function TALPointD.Round: TPoint;
begin
  Result.X := System.Round(X);
  Result.Y := System.Round(Y);
end;

{***************************************************************************}
function TALPointD.RoundTo(const ADigit: TRoundToEXRangeExtended): TALPointD;
begin
  Result.X := System.Math.RoundTo(X, ADigit);
  Result.Y := System.Math.RoundTo(Y, ADigit);
end;

{******************************************}
function TALPointD.ReducePrecision: TPointf;
begin
  Result.X := Single(X);
  Result.Y := Single(Y);
end;

{**************************************************************************************************}
function TALPointD.SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean): TALPointD;
var
  LScale: Double;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.X := System.Round(Self.X * LScale) / LScale;
  Result.Y := System.Round(Self.Y * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    LScale := LScale / 2;
    Result.Offset(LScale, LScale);
  end;
end;

{*********************************************************}
function TALPointD.Scale(const AFactor: Double): TALPointD;
begin
  Result := Self * AFactor;
end;

{**************************************}
function TALPointD.Normalize: TALPointD;
var
  Len: Double;
begin
  Len := Sqrt(Sqr(X) + Sqr(Y));

  if (Len <> 0.0) then
  begin
    Result.X := X / Len;
    Result.Y := Y / Len;
  end
  else
    Result := Self;
end;

{********************************}
function TALPointD.Length: Double;
begin
  Result := Sqrt(Sqr(X) + Sqr(Y));
end;

{***********************************************}
procedure TALPointD.SetLocation(const P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

{**************************************************}
procedure TALPointD.SetLocation(const P: TALPointD);
begin
  Self := P;
end;

{**************************************************}
procedure TALPointD.SetLocation(const X, Y: Double);
begin
  Self.X := X;
  Self.Y := Y;
end;

{*************************************************************}
function TALPointD.Subtract(const Point: TALPointD): TALPointD;
begin
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
end;

{**********************************************************}
function TALPointD.Subtract(const Point: TPoint): TALPointD;
begin
  Result.X := Self.X - Point.X;
  Result.Y := Self.Y - Point.Y;
end;

{****************************************************************}
procedure SinCosDouble(const Theta: Double; var Sin, Cos: Double);
var
{$IF SizeOf(Extended) > SizeOf(Double)}
  S, C: Extended;
{$ELSE}
  S, C: Double;
{$ENDIF}
begin
  System.SineCosine(Theta, S, C);
  Sin := S;
  Cos := C;
end;

{*********************************************************}
function TALPointD.Rotate(const AAngle: Double): TALPointD;
var
  Sine, Cosine: Double;
begin
  SinCosDouble(AAngle, Sine, Cosine);
  Result.X := X * Cosine - Y * Sine;
  Result.Y := X * Sine + Y * Cosine;
end;

{*************************************************************}
function TALPointD.Reflect(const APoint: TALPointD): TALPointD;
begin
  Result := Self + APoint * (-2 * Self.DotProduct(APoint));
end;

{**************************************************************}
function TALPointD.MidPoint(const APoint: TALPointD): TALPointD;
begin
  Result.X := (Self.X + APoint.X) / 2;
  Result.Y := (Self.Y + APoint.Y) / 2;
end;

{********************************************************}
function TALPointD.Angle(const APoint: TALPointD): Double;
begin
  Result := Arctan2(Self.Y - APoint.Y, Self.X - APoint.X);
end;

{*****************************}
function TALPointD.Abs: Double;
begin
  Result := Sqrt(Sqr(self.X) + Sqr(self.Y));
end;

{**************************************************************}
function TALPointD.AngleCosine(const APoint: TALPointD): Double;
begin
  Result := Self.Length * APoint.Length;

  if system.Abs(Result) > Epsilon then
    Result := Self.DotProduct(APoint) / Result
  else
    Result := Self.DotProduct(APoint) / Epsilon;

  Result := Max(Min(Result, 1), -1);
end;

{*****************************************************************}
constructor TALRectD.Create(const R: TALRectD; Normalize: Boolean);
begin
  Self := R;
  if Normalize then NormalizeRect;
end;

{***************************************************************}
constructor TALRectD.Create(const R: TRectF; Normalize: Boolean);
begin
  Self.Left := R.Left; Self.Top := R.Top;
  Self.Right := R.Right; Self.Bottom := R.Bottom;
  if Normalize then NormalizeRect;
end;

{**************************************************************}
constructor TALRectD.Create(const R: TRect; Normalize: Boolean);
begin
  Self.Left := R.Left;
  Self.Top  := R.Top;
  Self.Right := R.Right;
  Self.Bottom := R.Bottom;
  if Normalize then NormalizeRect;
end;

{***************************************************}
constructor TALRectD.Create(const Origin: TALPointD);
begin
  TopLeft := Origin;
  BottomRight := Origin;
end;

{******************************************************************}
constructor TALRectD.Create(const Left, Top, Right, Bottom: Double);
begin
  Self.Left := Left; Self.Top := Top;
  Self.Right := Right; Self.Bottom := Bottom;
end;

{***********************************************************************}
constructor TALRectD.Create(const P1, P2: TALPointD; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then NormalizeRect;
end;

{********************************************************************************}
constructor TALRectD.Create(const Origin: TALPointD; const Width, Height: Double);
begin
  Self.TopLeft := Origin;
  Self.Width := Width;
  Self.Height := Height;
end;

{***************************************************************}
class operator TALRectD.Equal(const Lhs, Rhs: TALRectD): Boolean;
begin
  Result := (Lhs.TopLeft = Rhs.TopLeft) and
            (Lhs.BottomRight = Rhs.BottomRight);
end;

{****************************************************************************}
function TALRectD.EqualsTo(const R: TALRectD; const Epsilon: Double): Boolean;
begin
  Result := TopLeft.EqualsTo(R.TopLeft, Epsilon) and BottomRight.EqualsTo(R.BottomRight, Epsilon);
end;

{**************************************************************************************}
function TALRectD.FitInto(const ADesignatedArea: TALRectD; out Ratio: Double): TALRectD;
begin
  if (ADesignatedArea.Width <= 0) or (ADesignatedArea.Height <= 0) then
  begin
    Ratio := 1;
    Exit(Self);
  end;

  if (Self.Width / ADesignatedArea.Width) > (Self.Height / ADesignatedArea.Height) then
    Ratio := Self.Width / ADesignatedArea.Width
  else
    Ratio := Self.Height / ADesignatedArea.Height;

  if Ratio = 0 then
    Exit(Self)
  else
  begin
    Result := TALRectD.Create(0, 0, Self.Width / Ratio, Self.Height / Ratio);
    ALRectCenter(Result, ADesignatedArea);
  end;
end;

{*******************************************************************}
function TALRectD.FitInto(const ADesignatedArea: TALRectD): TALRectD;
var
  Ratio: Double;
begin
  Result := FitInto(ADesignatedArea, Ratio);
end;

{********************************************************************}
function TALRectD.CenterAt(const ADesignatedArea: TALRectD): TALRectD;
begin
  Result := Self;
  ALRectCenter(Result, ADesignatedArea);
end;

{**************************}
function TALRectD.PlaceInto(
           const ADesignatedArea: TALRectD;
           const AHorzAlign: THorzRectAlign;
           const AVertAlign: TVertRectAlign): TALRectD;
var
  LLocation: TALPointD;
begin
  Result := Self;
  if (Self.Width > ADesignatedArea.Width) or (Self.Height > ADesignatedArea.Height) then
    Result := Result.FitInto(ADesignatedArea);
 case AHorzAlign of
   THorzRectAlign.Center: LLocation.X := (ADesignatedArea.Left + ADesignatedArea.Right - Result.Width) / 2;
   THorzRectAlign.Left: LLocation.X := ADesignatedArea.Left;
   THorzRectAlign.Right: LLocation.X := ADesignatedArea.Right - Result.Width;
 end;
 case AVertAlign of
   TVertRectAlign.Center: LLocation.Y := (ADesignatedArea.Top + ADesignatedArea.Bottom - Result.Height) / 2;
   TVertRectAlign.Top: LLocation.Y := ADesignatedArea.Top;
   TVertRectAlign.Bottom: LLocation.Y := ADesignatedArea.Bottom - Result.Height;
 end;
 Result.SetLocation(LLocation);
end;

{************************************************************************************************}
function TALRectD.SnapToPixel(const AScale: Double; const APlaceBetweenPixels: Boolean): TALRectD;
var
  LScale, HalfPixel: Double;
begin
  if AScale <= 0 then
    LScale := 1
  else
    LScale := AScale;
  Result.Left := System.Trunc(Self.Left * LScale) / LScale;
  Result.Top := System.Trunc(Self.Top * LScale) / LScale;
  Result.Width := System.Round(Self.Width * LScale) / LScale;
  Result.Height := System.Round(Self.Height * LScale) / LScale;
  if APlaceBetweenPixels then
  begin
    HalfPixel := 1 / (2 * LScale);
    Result.Offset(HalfPixel, HalfPixel);
  end;
end;

{********************************************************}
function TALRectD.Fit(const BoundsRect: TALRectD): Double;
var
  Ratio: Double;
begin
  Result := 1;
  if (BoundsRect.Width <= 0) or (BoundsRect.Height <= 0) then
    Exit;

  if (Self.Width / BoundsRect.Width) > (Self.Height / BoundsRect.Height) then
    Ratio := Self.Width / BoundsRect.Width
  else
    Ratio := Self.Height / BoundsRect.Height;

  if Ratio < 1 then
    Self := TALRectD.Create(0, 0, Self.Width, Self.Height)
  else
    Self := TALRectD.Create(0, 0, Self.Width / Ratio, Self.Height / Ratio);

  Result := Ratio;
  ALRectCenter(Self, BoundsRect);
end;

{******************************************************************}
class operator TALRectD.NotEqual(const Lhs, Rhs: TALRectD): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

{**************************************************************}
class operator TALRectD.Implicit(const Source: TRect): TALRectD;
begin
  Result := TALRectD.Create(Source);
end;

{*************}
{$IFNDEF ALDPK}
class operator TALRectD.Explicit(const Source: TALRectD): TRect;
begin
  Result := Source.Round;
end;
{$ENDIF}

{**************************************************************}
class operator TALRectD.Add(const Lhs, Rhs: TALRectD): TALRectD;
begin
  Result := TALRectD.Union(Lhs, Rhs);
end;

{*******************************************************************}
class operator TALRectD.Multiply(const Lhs, Rhs: TALRectD): TALRectD;
begin
  Result := TALRectD.Intersect(Lhs, Rhs);
end;

{***************************************}
function TALRectD.CenterPoint: TALPointD;
begin
  Result.X := (Right - Left)/2.0 + Left;
  Result.Y := (Bottom - Top)/2.0 + Top;
end;

{*****************************************************}
function TALRectD.Contains(const R: TALRectD): Boolean;
begin
  Result := (Self.Left <= R.Left)
        and (Self.Right >= R.Right)
        and (Self.Top <= R.Top)
        and (Self.Bottom >= R.Bottom);
end;

{*******************************************************************}
function PtInRect(const Rect: TALRectD; const P: TALPointD): Boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X < Rect.Right) and (P.Y >= Rect.Top)
    and (P.Y < Rect.Bottom);
end;

{*******************************************************}
function TALRectD.Contains(const Pt: TALPointD): Boolean;
begin
  Result := (Pt.X >= Self.Left)
        and (Pt.X < Self.Right)
        and (Pt.Y >= Self.Top)
        and (Pt.Y < Self.Bottom);
end;

{*****************************************************}
function TALRectD.Contains(const Pt: TPointf): Boolean;
begin
  Result := (Pt.X >= Self.Left)
        and (Pt.X < Self.Right)
        and (Pt.Y >= Self.Top)
        and (Pt.Y < Self.Bottom);
end;

{**************************************}
class function TALRectD.Empty: TALRectD;
begin
  Result := TALRectD.Create(0,0,0,0);
end;

{**********************************}
function TALRectD.GetHeight: Double;
begin
  Result := Self.Bottom - Self.Top;
end;

{************************************************}
procedure TALRectD.SetHeight(const Value: Double);
begin
  Self.Bottom := Self.Top + Value;
end;

{*********************************}
function TALRectD.GetWidth: Double;
begin
  Result := Self.Right - Self.Left;
end;

{***********************************************}
procedure TALRectD.SetWidth(const Value: Double);
begin
  Self.Right := Self.Left + Value;
end;

{**********************************}
function TALRectD.GetSize: TALSizeD;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

{************************************************}
procedure TALRectD.SetSize(const Value: TALSizeD);
begin
  Width := Value.cx;
  Height := Value.cy;
end;

{***********************************************}
procedure TALRectD.Inflate(const DX, DY: Double);
begin
  TopLeft.Offset(-DX, -DY);
  BottomRight.Offset(DX, DY);
end;

{*******************************************************}
procedure TALRectD.Inflate(const DL, DT, DR, DB: Double);
begin
  TopLeft.Offset(-DL, -DT);
  BottomRight.Offset(DR, DB);
end;

{************************************************}
procedure TALRectD.Offset(const Point: TALPointD);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

{**********************************************}
procedure TALRectD.Offset(const Point: TPointf);
begin
  TopLeft.Offset(Point);
  BottomRight.Offset(Point);
end;

{**********************************************}
procedure TALRectD.Offset(const DX, DY: Double);
begin
  TopLeft.Offset(DX, DY);
  BottomRight.Offset(DX, DY);
end;

{***************************************}
function TALRectD.GetLocation: TALPointD;
begin
  Result := TopLeft;
end;

{*****************************************************}
procedure TALRectD.SetLocation(const Point: TALPointD);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

{***************************************************}
procedure TALRectD.SetLocation(const Point: TPointf);
begin
  Offset(Point.X - Left, Point.Y - Top);
end;

{*************************************************}
procedure TALRectD.SetLocation(const X, Y: Double);
begin
  Offset(X - Left, Y - Top);
end;

{***********************************************************}
function TALRectD.IntersectsWith(const R: TALRectD): Boolean;
begin
  Result := (Self.Left < R.Right)
        and (Self.Right > R.Left)
        and (Self.Top < R.Bottom)
        and (Self.Bottom > R.Top);
end;

{*********************************}
function TALRectD.IsEmpty: Boolean;
begin
  Result := (Right <= Left) or (Bottom <= Top);
end;

{*******************************}
procedure TALRectD.NormalizeRect;
var
  temp: Double;
begin
  if Top > Bottom then
  begin
    temp := Top;
    Top := Bottom;
    Bottom := temp;
  end;
  if Left > Right then
  begin
    temp := Left;
    Left := Right;
    Right := temp;
  end
end;

{*******************************}
function TALRectD.Ceiling: TRect;
begin
  Result.TopLeft := TopLeft.Ceiling;
  Result.BottomRight := BottomRight.Ceiling;
end;

{********************************}
function TALRectD.Truncate: TRect;
begin
  Result.TopLeft := TopLeft.Truncate;
  Result.BottomRight := BottomRight.Truncate;
end;

{*****************************}
function TALRectD.Round: TRect;
begin
  Result.TopLeft := TopLeft.Round;
  Result.BottomRight := BottomRight.Round;
end;

{*************************************************************************}
function TALRectD.RoundTo(const ADigit: TRoundToEXRangeExtended): TALRectD;
begin
  Result.TopLeft := TopLeft.RoundTo(ADigit);
  Result.BottomRight := BottomRight.RoundTo(ADigit);
end;

{****************************************}
function TALRectD.ReducePrecision: TRectF;
begin
  Result.TopLeft := TopLeft.ReducePrecision;
  Result.BottomRight := BottomRight.ReducePrecision;
end;

{******************************************************************}
class function TALRectD.Intersect(const R1, R2: TALRectD): TALRectD;
begin
  ALIntersectRect(Result, R1, R2);
end;

{**********************************************}
procedure TALRectD.Intersect(const R: TALRectD);
begin
  Self := Intersect(Self, R);
end;

{**************************************************************}
class function TALRectD.Union(const R1, R2: TALRectD): TALRectD;
begin
  ALUnionRect(Result, R1, R2);
end;

{******************************************}
procedure TALRectD.Union(const R: TALRectD);
begin
  Self := TALRectD.Union(Self, R);
end;

{************************************************************************}
class function TALRectD.Union(const Points: Array of TALPointD): TALRectD;
var
  I: Integer;
  TLCorner, BRCorner: TALPointD;
begin
  if Length(Points) > 0 then
  begin
    TLCorner := Points[Low(Points)];
    BRCorner := Points[Low(Points)];

    if Length(Points) > 1 then
    begin
      for I := Low(Points) + 1 to High(Points) do
      begin
        if Points[I].X < TLCorner.X then TLCorner.X := Points[I].X;
        if Points[I].X > BRCorner.X then BRCorner.X := Points[I].X;
        if Points[I].Y < TLCorner.Y then TLCorner.Y := Points[I].Y;
        if Points[I].Y > BRCorner.Y then BRCorner.Y := Points[I].Y;
      end;
    end;

    Result := TALRectD.Create(TLCorner, BRCorner);
  end
  else begin
    Result := TALRectD.Empty;
  end;
end;

{*****************************************************}
function TALSizeD.Add(const Point: TALSizeD): TALSizeD;
begin
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
end;

{**************************************************************}
class operator TALSizeD.Add(const Lhs, Rhs: TALSizeD): TALSizeD;
begin
  Result.cx := Lhs.cx + Rhs.cx;
  Result.cy := Lhs.cy + Rhs.cy;
end;

{**********************************************}
constructor TALSizeD.Create(const X, Y: Double);
begin
  cx := X;
  cy := Y;
end;

{***************************************}
constructor TALSizeD.Create(P: TALSizeD);
begin
  cx := P.cx;
  cy := P.cy;
end;

{*************************************}
constructor TALSizeD.Create(P: TSizeF);
begin
  cx := P.cx;
  cy := P.cy;
end;

{*****************************************************}
function TALSizeD.Distance(const P2: TALSizeD): Double;
begin
  Result := Sqrt(Sqr(Self.cx - P2.cx) + Sqr(Self.cy - P2.cy));
end;

{*****************************************************************}
class operator TALSizeD.Implicit(const Point: TALPointD): TALSizeD;
begin
  Result.cx := Point.X;
  Result.cy := Point.Y;
end;

{****************************************************************}
class operator TALSizeD.Implicit(const Size: TALSizeD): TALPointD;
begin
  Result.X := Size.cx;
  Result.Y := Size.cy;
end;

{********************************}
function TALSizeD.IsZero: Boolean;
begin
  Result := SameValue(cx, 0.0) and SameValue(cy, 0.0);
end;

{***************************************************************}
class operator TALSizeD.Equal(const Lhs, Rhs: TALSizeD): Boolean;
begin
  Result := SameValue(Lhs.cx, Rhs.cx) and SameValue(Lhs.cy, Rhs.cy);
end;

{******************************************************************}
class operator TALSizeD.NotEqual(const Lhs, Rhs: TALSizeD): Boolean;
begin
  Result := not (Lhs = Rhs);
end;

{**********************************************************}
function TALSizeD.Subtract(const Point: TALSizeD): TALSizeD;
begin
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
end;

{*****************************************}
function TALSizeD.SwapDimensions: TALSizeD;
begin
  Result := TALSizeD.Create(Height, Width);
end;

{*******************************************************************}
class operator TALSizeD.Subtract(const Lhs, Rhs: TALSizeD): TALSizeD;
begin
  Result.cx := Lhs.cx - Rhs.cx;
  Result.cy := Lhs.cy - Rhs.cy;
end;

{*******************************}
function TALSizeD.Ceiling: TSize;
begin
  Result.cx := Ceil(cx);
  Result.cy := Ceil(cy);
end;

{*****************************}
function TALSizeD.Round: TSize;
begin
  Result.cx := Trunc(cx + 0.5);
  Result.cy := Trunc(cy + 0.5);
end;

{*************************************************************************}
function TALSizeD.RoundTo(const ADigit: TRoundToEXRangeExtended): TALSizeD;
begin
  Result.cx := system.math.RoundTo(cx, ADigit);
  Result.cy := system.math.RoundTo(cy, ADigit);
end;

{****************************************}
function TALSizeD.ReducePrecision: TSizeF;
begin
  Result.cx := Single(cx);
  Result.cy := Single(cy);
end;

{********************************}
function TALSizeD.Truncate: TSize;
begin
  Result.cx := Trunc(cx);
  Result.cy := Trunc(cy);
end;

{************************************************************}
class operator TALSizeD.Implicit(const Size: TSize): TALSizeD;
begin
  Result.cx := Size.cx;
  Result.cy := Size.cy;
end;

{*****************************************************}
constructor EALException.Create(const Msg: AnsiString);
begin
  inherited create(String(Msg));
end;

{*************************************************}
constructor EALException.Create(const Msg: string);
begin
  inherited create(Msg);
end;

{************************************************************************************}
constructor EALException.CreateFmt(const Msg: ansistring; const Args: array of const);
begin
  inherited CreateFmt(String(Msg), Args);
end;

{********************************************************************************}
constructor EALException.CreateFmt(const Msg: string; const Args: array of const);
begin
  inherited CreateFmt(Msg, Args);
end;

type

  {******************}
  _TALLogItem = record
  private
    Tag: String;
    msg: String;
    &Type: TalLogType;
    ThreadID: TThreadID;
    TimeStamp: TDateTime;
  public
    class function Create(
                     const ATag: String;
                     const AMsg: String;
                     Const AType: TalLogType;
                     Const AThreadID: TThreadID;
                     const ATimeStamp: TDateTime): _TALLogItem; static; inline;
  end;

var
  _ALLogQueue: TList<_TALLogItem>;
  _ALLogHistory: TList<_TALLogItem>;
  _ALLogHistoryIndex: integer = -1;

{********************************}
class function _TALLogItem.Create(
                 const ATag: String;
                 const AMsg: String;
                 Const AType: TalLogType;
                 Const AThreadID: TThreadID;
                 const ATimeStamp: TDateTime): _TALLogItem;
begin
  Result.Tag := aTag;
  Result.Msg := aMsg;
  Result.&Type := aType;
  Result.ThreadID := aThreadID;
  Result.TimeStamp := ATimeStamp;
end;

{*****************************************************************************}
function ALGetLogHistory(const AIgnoreLastLogItemMsg: Boolean = False): String;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _LogItemToStr(const ALogItem: _TALLogItem; const aIgnoreLogItemMsg: Boolean): String;
  begin
    Result := ALFormatDateTimeW(
                'yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''',
                TTimeZone.Local.ToUniversalTime(ALogItem.TimeStamp),
                ALDefaultFormatSettingsW);
    case ALogItem.&Type of
      TalLogType.VERBOSE: Result := Result + ' [V]';
      TalLogType.DEBUG:   Result := Result + ' [D][V]';
      TalLogType.INFO:    Result := Result + ' [I][D][V]';
      TalLogType.WARN:    Result := Result + ' [W][I][D][V]';
      TalLogType.ERROR:   Result := Result + ' [E][W][I][D][V]';
      TalLogType.ASSERT:  Result := Result + ' [A][E][W][I][D][V]';
      else raise Exception.Create('Error 651AAEA2-4FF7-4621-A4DB-4BA299E238CE');
    end;
    if ALogItem.ThreadID <> MainThreadID then Result := Result + '['+ALIntToStrW(ALogItem.ThreadID)+']';
    if ALogItem.Tag <> '' then Result := Result + ' ' + ALogItem.Tag;
    if (not aIgnoreLogItemMsg) and (ALogItem.msg <> '') then begin
      var lMsg := ALStringReplaceW(ALogItem.msg, #13#10, #10, [rfreplaceALL]);
      lMsg := ALStringReplaceW(lMsg, #13, #10, [rfreplaceALL]);
      Result := ALTrim(ALStringReplaceW(#10+lMsg, #10, #13#10+Result+' | ', [RfReplaceALL]));
    end;
  end;

begin
  Result := '';
  Tmonitor.enter(_ALLogHistory);
  Try
    for var i := _ALLogHistoryIndex downto 0 do
      Result := Result + _LogItemToStr(_ALLogHistory[i], AIgnoreLastLogItemMsg and (_ALLogHistoryIndex=i)) + #13#10;
    for var i := _ALLogHistory.Count - 1 downto _ALLogHistoryIndex + 1 do
      Result := Result + _LogItemToStr(_ALLogHistory[i], false) + #13#10;
  Finally
    Tmonitor.exit(_ALLogHistory);
  End;
  Result := ALTrim(Result);
end;

{***************}
procedure _ALLog(
            Const Tag: String;
            Const msg: String;
            Const &Type: TalLogType;
            Const ThreadID: TThreadID;
            Const CanPreserve: boolean);
begin
  {$IF defined(ALCodeProfiler)}
  // Logging can consume time, so to ensure accurate performance statistics,
  // ignore all verbose logs.
  if &Type = TalLogType.VERBOSE then exit;
  {$ENDIF}
  if CanPreserve and (ALMaxLogHistory > 0) then begin
    var LLogItem := _TALLogItem.Create(
                      Tag, // const ATag: String;
                      Msg, // const AMsg: String;
                      &Type, // Const aType: TalLogType
                      ThreadID, // Const AThreadID: TThreadID;
                      Now); // const ATimeStamp: TDateTime
    Tmonitor.enter(_ALLogHistory);
    Try
      _ALLogHistoryIndex := (_ALLogHistoryIndex + 1) mod ALMaxLogHistory;
      if _ALLogHistoryIndex <= _ALLogHistory.Count - 1 then
        _ALLogHistory[_ALLogHistoryIndex] := LLogItem
      else
        _ALLogHistoryIndex := _ALLogHistory.Add(LLogItem);
    Finally
      Tmonitor.exit(_ALLogHistory);
    End;
  end;
  //--
  if CanPreserve and ALEnqueueLog then begin
    Tmonitor.Enter(_ALLogQueue);
    try
      _ALLogQueue.Add(
        _TALLogItem.Create(
          Tag, // const ATag: String;
          Msg, // const AMsg: String;
          &Type, // Const aType: TalLogType
          ThreadID, // Const AThreadID: TThreadID;
          Now)); // const ATimeStamp: TDateTime
    finally
      Tmonitor.Exit(_ALLogQueue);
    end;
    if &Type = TalLogType.ASSERT then ALPrintLogQueue
  end
  else begin
    var LMsg: String;
    {$IF defined(ALCodeProfiler)}
    if ALCodeProfilerIsrunning then begin
      var LMilliseconds: Double := (TStopwatch.GetTimeStamp - ALCodeProfilerAppStartTimeStamp) * ALCodeProfilerMillisecondsPerTick;
      var LMinutes: integer := Trunc(LMilliseconds) div (1000 * 60);
      LMilliseconds := LMilliseconds - (LMinutes * 1000 * 60);
      var LSeconds: integer := Trunc(LMilliseconds) div 1000;
      LMilliseconds := LMilliseconds - (LSeconds * 1000);
      var LMicroSec: integer := Round(Frac(LMilliseconds) * 100000);
      if Msg <> '' then
        LMsg := Msg + ALFormatW(' | StartTimeStamp: %.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LMicroSec])
      else
        LMsg := ALFormatW('StartTimeStamp: %.2d:%.2d:%.3d.%.5d', [{LHours,} LMinutes, LSeconds, Trunc(LMilliseconds), LMicroSec]);
    end;
    {$ELSE}
    LMsg := msg;
    {$ENDIF}
    {$IF defined(ANDROID)}
    if LMsg = '' then LMsg := '<empty>';
    if ThreadID <> MainThreadID then LMsg := '['+ALIntToStrW(ThreadID)+'] ' + LMsg;
    case &Type of
      TalLogType.VERBOSE: TJLog.JavaClass.v(StringToJString(Tag), StringToJString(LMsg));
      TalLogType.DEBUG: TJLog.JavaClass.d(StringToJString(Tag), StringToJString(LMsg));
      TalLogType.INFO: TJLog.JavaClass.i(StringToJString(Tag), StringToJString(LMsg));
      TalLogType.WARN: TJLog.JavaClass.w(StringToJString(Tag), StringToJString(LMsg));
      TalLogType.ERROR: TJLog.JavaClass.e(StringToJString(Tag), StringToJString(LMsg));
      TalLogType.ASSERT: TJLog.JavaClass.wtf(StringToJString(Tag), StringToJString(LMsg)); // << wtf for What a Terrible Failure but everyone know that it's for what the fuck !
    end;
    {$ELSEIF defined(IOS)}
    if LMsg <> '' then LMsg := Tag + ' | ' + LMsg
    else LMsg := Tag;
    var LThreadID: String;
    if ThreadID <> MainThreadID then LThreadID := '['+ALIntToStrW(ThreadID)+']'
    else LThreadID := '';
    //On iOS NSLog is limited to 1024 Bytes so if the
    //message is > 1024 bytes split it
    var P: integer := 1;
    while P <= length(LMsg) do begin
      var LMsgPart := ALCopyStr(LMsg, P, 950); // to stay safe
      inc(P, 950);
      case &Type of
        TalLogType.VERBOSE: NSLog(StringToID('[V]'+LThreadID+' ' + LMsgPart));
        TalLogType.DEBUG:   NSLog(StringToID('[D][V]'+LThreadID+' ' + LMsgPart));
        TalLogType.INFO:    NSLog(StringToID('[I][D][V]'+LThreadID+' ' + LMsgPart));
        TalLogType.WARN:    NSLog(StringToID('[W][I][D][V]'+LThreadID+' ' + LMsgPart));
        TalLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V]'+LThreadID+' ' + LMsgPart));
        TalLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V]'+LThreadID+' ' + LMsgPart));
      end;
    end;
    {$ELSEIF defined(MSWINDOWS)}
    if LMsg <> '' then LMsg := Tag + ' | ' + stringReplace(LMsg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
    else LMsg := Tag;
    case &Type of
      TalLogType.VERBOSE: OutputDebugString(pointer('[V] ' + LMsg + ' |'));
      TalLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + LMsg + ' |'));
      TalLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + LMsg + ' |'));
      TalLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + LMsg + ' |'));
      TalLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + LMsg + ' |'));
      TalLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + LMsg + ' |'));
    end;
    {$ENDIF}
  end;
end;

{**************************************************************************************************}
procedure ALLog(Const Tag: String; Const msg: String; const &Type: TalLogType = TalLogType.VERBOSE);
begin
  if assigned(ALCustomLogMsgProc) then
    ALCustomLogMsgProc(Tag, msg, &Type)
  else
    _ALLog(Tag, msg, &Type, TThread.Current.ThreadID, True{CanPreserve});
end;

{*******************************************************************************}
procedure ALLog(Const Tag: String; const &Type: TalLogType = TalLogType.VERBOSE);
begin
  if assigned(ALCustomLogMsgProc) then
    ALCustomLogMsgProc(Tag, '', &Type)
  else
    _ALLog(Tag, '', &Type, TThread.Current.ThreadID, True{CanPreserve});
end;

{*************************************************************************************************}
procedure ALLog(Const Tag: String; Const E: Exception; const &Type: TalLogType = TalLogType.ERROR);
begin
  if assigned(ALCustomLogExceptionProc) then
    ALCustomLogExceptionProc(Tag, E, &Type)
  else
    _ALLog(Tag, E.message, &Type, TThread.Current.ThreadID, True{CanPreserve});
end;

{****************************************************************************************************************************************************************}
procedure ALLog(Const Tag: String; const TagArgs: array of const; Const msg: String; const msgArgs: array of const; const &Type: TalLogType = TalLogType.VERBOSE);
begin
  ALLog(ALFormatW(Tag, TagArgs), ALFormatW(msg, msgArgs), &Type);
end;

{***********************************************************************************************************}
procedure ALLog(Const Tag: String; const Args: array of const; const &Type: TalLogType = TalLogType.VERBOSE);
begin
  ALLog(ALFormatW(Tag, Args), &Type);
end;

{*****************************************************************************************************************************}
procedure ALLog(Const Tag: String; const Args: array of const; Const E: Exception; const &Type: TalLogType = TalLogType.ERROR);
begin
  ALLog(ALFormatW(Tag, Args), E, &Type);
end;

{************************}
procedure ALPrintLogQueue;
begin
  Tmonitor.Enter(_ALLogQueue);
  try
    for var I := 0 to _ALLogQueue.Count - 1 do
      with _ALLogQueue[i] do
        _ALLog(Tag, Msg, &Type, ThreadID, False{CanPreserve});
    _ALLogQueue.Clear;
  finally
    Tmonitor.Exit(_ALLogQueue);
  end;
end;

{******************************************}
Function AlBoolToInt(Value:Boolean):Integer;
Begin
  If Value then result := 1
  else result := 0;
end;

{******************************************}
Function AlIntToBool(Value:integer):boolean;
begin
  result := Value <> 0;
end;

{***************************************************************}
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
Begin
  result := (LTotal - (LBorder*2) - LObject) div 2 + LBorder;
End;

{************************************************************************************************}
function ALTryRGBAHexToAlphaColor(const aHexValue: String; out AAlphaColor: TAlphaColor): Boolean;
begin
  var R, G, B, A: Integer;
  case Length(aHexValue) of
    6: // RRGGBB
      begin
        If not ALTryStrToInt('$' + AHexValue.Substring(0, 2), R) or (R < low(Byte)) or (R > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(2, 2), G) or (G < low(Byte)) or (G > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(4, 2), B) or (B < low(Byte)) or (B > high(Byte)) then exit(false);
        A := high(Byte);
      end;
    8: // RRGGBBAA
      begin
        If not ALTryStrToInt('$' + AHexValue.Substring(0, 2), R) or (R < low(Byte)) or (R > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(2, 2), G) or (G < low(Byte)) or (G > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(4, 2), B) or (B < low(Byte)) or (B > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(6, 2), A) or (A < low(Byte)) or (A > high(Byte)) then exit(false);
      end;
  else
    exit(False);
  end;
  AAlphaColor := MakeColor(R, G, B, A);
  Result := True;
end;

{*******************************************************************}
function ALRGBAHexToAlphaColor(const aHexValue: String): TAlphaColor;
begin
  if not ALTryRGBAHexToAlphaColor(aHexValue, Result) then
    raise Exception.Create('Invalid RGBA hex color format');
end;

{************************************************************************************************}
function ALTryARGBHexToAlphaColor(const aHexValue: String; out AAlphaColor: TAlphaColor): Boolean;
begin
  var R, G, B, A: Integer;
  case Length(aHexValue) of
    6: // RRGGBB
      begin
        A := high(Byte);
        If not ALTryStrToInt('$' + AHexValue.Substring(0, 2), R) or (R < low(Byte)) or (R > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(2, 2), G) or (G < low(Byte)) or (G > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(4, 2), B) or (B < low(Byte)) or (B > high(Byte)) then exit(false);
      end;
    8: // AARRGGBB
      begin
        If not ALTryStrToInt('$' + AHexValue.Substring(0, 2), A) or (A < low(Byte)) or (A > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(2, 2), R) or (R < low(Byte)) or (R > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(4, 2), G) or (G < low(Byte)) or (G > high(Byte)) then exit(false);
        If not ALTryStrToInt('$' + AHexValue.Substring(6, 2), B) or (B < low(Byte)) or (B > high(Byte)) then exit(false);
      end;
  else
    exit(False);
  end;
  AAlphaColor := MakeColor(R, G, B, A);
  Result := True;
end;

{*******************************************************************}
function ALARGBHexToAlphaColor(const aHexValue: String): TAlphaColor;
begin
  if not ALTryARGBHexToAlphaColor(aHexValue, Result) then
    raise Exception.Create('Invalid ARGB hex color format');
end;

{*******************************************************************}
function ALCeil(const X: Single; const Epsilon: Single = 0): Integer;
begin
  Result := ceil(X - Epsilon);
end;

{*******************************************************************}
function ALCeil(const X: Double; const Epsilon: Double = 0): Integer;
begin
  Result := ceil(X - Epsilon);
end;

{***********************************************************************}
function ALCeil(const X: Extended; const Epsilon: Extended = 0): Integer;
begin
  Result := ceil(X - Epsilon);
end;

{********************************************************************}
function ALFloor(const X: Single; const Epsilon: Single = 0): Integer;
begin
  Result := Floor(X + Epsilon);
end;

{********************************************************************}
function ALFloor(const X: Double; const Epsilon: Double = 0): Integer;
begin
  Result := Floor(X + Epsilon);
end;

{************************************************************************}
function ALFloor(const X: Extended; const Epsilon: Extended = 0): Integer;
begin
  Result := Floor(X + Epsilon);
end;

{************************************************************************************************}
function ALIfThenA(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{************************************************************************************}
function ALIfThenW(AValue: Boolean; const ATrue: String; AFalse: String = ''): String;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{*******************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{*************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64 = 0): UInt64;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single = 0): Single;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{****************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{**********************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended = 0): Extended;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$IF defined(MSWindows)}
var
  ALPerformanceFrequency: TLargeInteger;
{$ENDIF}

{********************************}
function ALElapsedTimeNano: int64;
begin
  {$IFDEF ANDROID}
  var res: timespec;
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result := Int64(ALNanosPerSec) * res.tv_sec + res.tv_nsec;
  {$ELSEIF defined(IOS)}
  Result := AbsoluteToNanoseconds(mach_absolute_time)
  {$ELSEIF defined(ALMacOS)}
  Result := AbsoluteToNanoseconds(mach_absolute_time)
  {$ELSEIF defined(MSWindows)}
  if ALPerformanceFrequency = -1 then begin
    if not QueryPerformanceFrequency(ALPerformanceFrequency) then
      ALPerformanceFrequency := 0;
  end;
  if ALPerformanceFrequency = 0 then Result := timeGetTime * ALNanosPerMs
  else begin
    var PerformanceCounter: Int64;
    QueryPerformanceCounter(PerformanceCounter);
    Result := trunc(PerformanceCounter / (ALPerformanceFrequency{counts per second} / ALNanosPerSec{nanos per second}));
  end;
  {$ELSE}
    Raise Exception.Create('The platform has not been implemented yet');
  {$ENDIF}
end;

{*******************************************}
function ALElapsedTimeMillisAsDouble: Double;
begin
  Result := ALElapsedTimeNano / ALNanosPerMs;
end;

{*****************************************}
function ALElapsedTimeMillisAsInt64: int64;
begin
  Result := trunc(ALElapsedTimeNano / ALNanosPerMs);
end;

{********************************************}
function ALElapsedTimeSecondsAsDouble: Double;
begin
  Result := ALElapsedTimeNano / ALNanosPerSec;
end;

{******************************************}
function ALElapsedTimeSecondsAsInt64: int64;
begin
  Result := trunc(ALElapsedTimeNano / ALNanosPerSec);
end;

{****************}
{$IFDEF MSWINDOWS}
Function ALGetDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
var LResult: Dword;
    LdynamicTimezone:TDynamicTimeZoneInformation;
    I: integer;
begin
  setlength(result, 200);
  for i := 0 to maxint do begin
    Lresult := EnumDynamicTimeZoneInformation(I, @LdynamicTimezone);
    if (Lresult = ERROR_SUCCESS) then begin
      if i >= length(result) then Setlength(Result, length(result) + 100);
      Result[i] := LdynamicTimezone;
    end
    else if (Lresult = ERROR_NO_MORE_ITEMS) then begin
      setlength(result, i);
      break
    end
    else raiseLastOsError
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function ALGetDynamicTimeZoneInformation(const aTimeZoneKeyName: String): TDynamicTimeZoneInformation;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    I: integer;
begin
  LDynamicTimeZoneInformations := ALGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do
    if ALSameTextW(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      result := LDynamicTimeZoneInformations[i];
      Exit;
    end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
function ALFileTimeToDateTime(Const AFileTime: TFileTime): TDateTime;
var LSystemTime: TSystemTime;
begin
  if not FileTimeToSystemTime(AFileTime, LSystemTime) then raiseLastOsError;
  result := SystemTimeToDateTime(LSystemTime);
end;
{$ENDIF MSWINDOWS}

{************************************************************************}
function AlLocalDateTimeToUTC(Const aLocalDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(aLocalDateTime);
end;

{****************}
{$IFDEF MSWINDOWS}
Function AlLocalDateTimeToUTC(const aTimeZoneKeyName: String; aLocalDateTime: TdateTime): TdateTime; overload;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    LSystemTime, LLocalTime: TSystemTime;
    I: integer;
begin
  LDynamicTimeZoneInformations := ALGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do begin
    if ALSameTextW(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      DecodeDateTime(
        aLocalDateTime,
        LLocalTime.wYear,
        LLocalTime.wMonth,
        LLocalTime.wDay,
        LLocalTime.wHour,
        LLocalTime.wMinute,
        LLocalTime.wSecond,
        LLocalTime.wMilliseconds);
      LLocalTime.wDayOfWeek := DayOfWeek(aLocalDateTime) - 1;
      if TzSpecificLocalTimeToSystemTimeEx(@LDynamicTimeZoneInformations[i], LLocalTime, LSystemTime) then begin
        Result := EncodeDateTime(
                    LSystemTime.wYear,
                    LSystemTime.wMonth,
                    LSystemTime.wDay,
                    LSystemTime.wHour,
                    LSystemTime.wMinute,
                    LSystemTime.wSecond,
                    LSystemTime.wMilliseconds);
        exit;
      end
      else
        RaiseLastOsError;
    end;
  end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function AlLocalDateTimeToUTC(const aTimeZoneInformation: TDynamicTimeZoneInformation; aLocalDateTime: TdateTime): TdateTime; overload;
var LSystemTime, LLocalTime: TSystemTime;
begin
  DecodeDateTime(
    aLocalDateTime,
    LLocalTime.wYear,
    LLocalTime.wMonth,
    LLocalTime.wDay,
    LLocalTime.wHour,
    LLocalTime.wMinute,
    LLocalTime.wSecond,
    LLocalTime.wMilliseconds);
  LLocalTime.wDayOfWeek := DayOfWeek(aLocalDateTime) - 1;
  if TzSpecificLocalTimeToSystemTimeEx(@aTimeZoneInformation, LLocalTime, LSystemTime) then
    Result := EncodeDateTime(
                LSystemTime.wYear,
                LSystemTime.wMonth,
                LSystemTime.wDay,
                LSystemTime.wHour,
                LSystemTime.wMinute,
                LSystemTime.wSecond,
                LSystemTime.wMilliseconds)
  else begin
    result := 0; // to hide a warning
    RaiseLastOsError;
  end;
end;
{$ENDIF}

{**********************************************************************}
function AlUTCDateTimeToLocal(Const aUTCDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToLocalTime(aUTCDateTime);
end;

{****************}
{$IFDEF MSWINDOWS}
Function AlUTCDateTimeToLocal(const aTimeZoneKeyName: String; aUTCDateTime: TdateTime): TdateTime; overload;
var LDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
    LSystemTime, LLocalTime: TSystemTime;
    I: integer;
begin
  LDynamicTimeZoneInformations := ALGetDynamicTimeZoneInformations;
  for I := low(LDynamicTimeZoneInformations) to High(LDynamicTimeZoneInformations) do begin
    if ALSameTextW(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      DecodeDateTime(
        aUTCDateTime,
        LSystemTime.wYear,
        LSystemTime.wMonth,
        LSystemTime.wDay,
        LSystemTime.wHour,
        LSystemTime.wMinute,
        LSystemTime.wSecond,
        LSystemTime.wMilliseconds);
      LLocalTime.wDayOfWeek := DayOfWeek(aUTCDateTime) - 1;
      if SystemTimeToTzSpecificLocalTime(@LDynamicTimeZoneInformations[i], LSystemTime, LLocalTime) then begin
        Result := EncodeDateTime(
                    LLocalTime.wYear,
                    LLocalTime.wMonth,
                    LLocalTime.wDay,
                    LLocalTime.wHour,
                    LLocalTime.wMinute,
                    LLocalTime.wSecond,
                    LLocalTime.wMilliseconds);
        exit;
      end
      else
        RaiseLastOsError;
    end;
  end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Function AlUTCDateTimeToLocal(const aTimeZoneInformation: TDynamicTimeZoneInformation; aUTCDateTime: TdateTime): TdateTime; overload;
var LSystemTime, LLocalTime: TSystemTime;
begin
  DecodeDateTime(
    aUTCDateTime,
    LSystemTime.wYear,
    LSystemTime.wMonth,
    LSystemTime.wDay,
    LSystemTime.wHour,
    LSystemTime.wMinute,
    LSystemTime.wSecond,
    LSystemTime.wMilliseconds);
  LLocalTime.wDayOfWeek := DayOfWeek(aUTCDateTime) - 1;
  if SystemTimeToTzSpecificLocalTime(@aTimeZoneInformation, LSystemTime, LLocalTime) then
    Result := EncodeDateTime(
                LLocalTime.wYear,
                LLocalTime.wMonth,
                LLocalTime.wDay,
                LLocalTime.wHour,
                LLocalTime.wMinute,
                LLocalTime.wSecond,
                LLocalTime.wMilliseconds)
  else begin
    result := 0; // to hide a warning
    RaiseLastOsError;
  end;
end;
{$ENDIF}

{**********}
{$IFDEF IOS}
function ALNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
begin
  if ADateTime <> nil then
    Result := ADateTime.TimeIntervalSince1970 / SecsPerDay + EncodeDate(1970, 1, 1)
  else
    Result := 0.0;
end;
{$ENDIF}

{*************************}
{The same like Now but used
 UTC-time not local time.}
function ALUTCNow: TDateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(NOW);
end;

{******************************************************}
Function ALInc(var x: integer; Count: integer): Integer;
begin
  inc(X, count);
  result := X;
end;

{********************************************************************}
procedure ALAssignError(Const ASource: TObject; const ADest: Tobject);
begin
  var LSourceName: string;
  if ASource <> nil then LSourceName := ASource.ClassName
  else LSourceName := 'nil';
  var LDestName: string;
  if ADest <> nil then LDestName := ADest.ClassName
  else LDestName := 'nil';
  raise EConvertError.CreateResFmt(@SAssignError, [LSourceName, LDestName]);
end;

{******************************************************}
{Accepts number of milliseconds in the parameter aValue,
 provides 1000 times more precise value of TDateTime}
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
begin
  Result := IncMilliSecond(UnixDateDelta, aValue);
end;

{*******************************************************}
{Returns UNIX-time as the count of milliseconds since the
 UNIX epoch. Can be very useful for the purposes of
 special precision.}
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
begin
  result := MilliSecondsBetween(UnixDateDelta, aValue);
  if aValue < UnixDateDelta then result := -result;
end;

{****************}
{$IFDEF MSWINDOWS}
function ALConsoleForegroundColorToCode(const AColor : TALConsoleColor): Word;
begin
  case AColor of
    TALConsoleColor.ccRed : Result := FOREGROUND_RED or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkRed : Result := FOREGROUND_RED;
    TALConsoleColor.ccBlue : Result := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkBlue : Result := FOREGROUND_BLUE;
    TALConsoleColor.ccGreen : Result := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkGreen : Result := FOREGROUND_GREEN;
    TALConsoleColor.ccYellow : Result := FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkYellow : Result := FOREGROUND_GREEN or FOREGROUND_RED;
    TALConsoleColor.ccAqua : Result := FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkAqua : Result := FOREGROUND_GREEN or FOREGROUND_BLUE;
    TALConsoleColor.ccPurple : Result := FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkPurple : Result := FOREGROUND_BLUE or FOREGROUND_RED;
    TALConsoleColor.ccGrey : Result := FOREGROUND_INTENSITY;
    TALConsoleColor.ccBlack : Result := 0;
    TALConsoleColor.ccWhite : Result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    TALConsoleColor.ccDarkWhite : Result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
    else raise Exception.Create('Unknown color');
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
function ALConsoleBackgroundColorToCode(const AColor : TALConsoleColor): Word;
begin
  case AColor of
    TALConsoleColor.ccRed : Result := BACKGROUND_RED or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkRed : Result := BACKGROUND_RED;
    TALConsoleColor.ccBlue : Result := BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkBlue : Result := BACKGROUND_BLUE;
    TALConsoleColor.ccGreen : Result := BACKGROUND_GREEN or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkGreen : Result := BACKGROUND_GREEN;
    TALConsoleColor.ccYellow : Result := BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkYellow : Result := BACKGROUND_GREEN or BACKGROUND_RED;
    TALConsoleColor.ccAqua : Result := BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkAqua : Result := BACKGROUND_GREEN or BACKGROUND_BLUE;
    TALConsoleColor.ccPurple : Result := BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkPurple : Result := BACKGROUND_BLUE or BACKGROUND_RED;
    TALConsoleColor.ccGrey : Result := BACKGROUND_INTENSITY;
    TALConsoleColor.ccBlack : Result := 0;
    TALConsoleColor.ccWhite : Result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    TALConsoleColor.ccDarkWhite : Result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED;
    else raise Exception.Create('Unknown color');
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
function ALConsoleCodeToForegroundColor(const ACode : Word): TALConsoleColor;
begin
  case ACode of
    FOREGROUND_RED or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccRed;
    FOREGROUND_RED: Result := TALConsoleColor.ccDarkRed;
    FOREGROUND_BLUE or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccBlue;
    FOREGROUND_BLUE: Result := TALConsoleColor.ccDarkBlue;
    FOREGROUND_GREEN or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccGreen;
    FOREGROUND_GREEN: Result := TALConsoleColor.ccDarkGreen;
    FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccYellow;
    FOREGROUND_GREEN or FOREGROUND_RED: Result := TALConsoleColor.ccDarkYellow;
    FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccAqua;
    FOREGROUND_GREEN or FOREGROUND_BLUE: Result := TALConsoleColor.ccDarkAqua;
    FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccPurple;
    FOREGROUND_BLUE or FOREGROUND_RED: Result := TALConsoleColor.ccDarkPurple;
    FOREGROUND_INTENSITY: Result := TALConsoleColor.ccGrey;
    0: Result := TALConsoleColor.ccBlack;
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY: Result := TALConsoleColor.ccWhite;
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED: Result := TALConsoleColor.ccDarkWhite;
    else raise Exception.Create('Unknown code');
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
function ALConsoleCodeToBackgroundColor(const ACode : Word): TALConsoleColor;
begin
  case ACode of
    BACKGROUND_RED or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccRed;
    BACKGROUND_RED: Result := TALConsoleColor.ccDarkRed;
    BACKGROUND_BLUE or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccBlue;
    BACKGROUND_BLUE: Result := TALConsoleColor.ccDarkBlue;
    BACKGROUND_GREEN or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccGreen;
    BACKGROUND_GREEN: Result := TALConsoleColor.ccDarkGreen;
    BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccYellow;
    BACKGROUND_GREEN or BACKGROUND_RED: Result := TALConsoleColor.ccDarkYellow;
    BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccAqua;
    BACKGROUND_GREEN or BACKGROUND_BLUE: Result := TALConsoleColor.ccDarkAqua;
    BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccPurple;
    BACKGROUND_BLUE or BACKGROUND_RED: Result := TALConsoleColor.ccDarkPurple;
    BACKGROUND_INTENSITY: Result := TALConsoleColor.ccGrey;
    0: Result := TALConsoleColor.ccBlack;
    BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY: Result := TALConsoleColor.ccWhite;
    BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED: Result := TALConsoleColor.ccDarkWhite;
    else raise Exception.Create('Unknown code');
  end;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
procedure ALGetConsoleColors(out AForegroundColor: TALConsoleColor; out ABackgroundColor: TALConsoleColor);
var LConsoleInfo: _CONSOLE_SCREEN_BUFFER_INFO;
    LStdOut: THandle;
begin
  LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if LStdOut = INVALID_HANDLE_VALUE then RaiseLastOsError;
  if GetConsoleScreenBufferInfo(LStdOut, LConsoleInfo) then begin
    AForegroundColor := ALConsoleCodeToForegroundColor(LConsoleInfo.wAttributes and (FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY));
    ABackgroundColor := ALConsoleCodeToBackgroundColor(LConsoleInfo.wAttributes and (BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY));
  end
  else RaiseLastOsError;
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
procedure ALSetConsoleColors(const AForegroundColor: TALConsoleColor; const ABackgroundColor: TALConsoleColor);
var LStdOut: THandle;
begin
  LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if LStdOut = INVALID_HANDLE_VALUE then RaiseLastOsError;
  SetConsoleTextAttribute(
    LStdOut,
    ALConsoleForegroundColorToCode(AForegroundColor) or ALConsoleBackgroundColorToCode(ABackgroundColor));
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: AnsiString);
begin
  System.Write(AStr);
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: AnsiString; const aForegroundColor: TALConsoleColor);
var LForegroundColor: TALConsoleColor;
    LBackgroundColor: TALConsoleColor;
begin
  ALGetConsoleColors(LForegroundColor, LBackgroundColor);
  ALSetConsoleColors(AForegroundColor, LBackgroundColor);
  System.Writeln(AStr);
  ALSetConsoleColors(LForegroundColor, LBackgroundColor);
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: AnsiString; const aForegroundColor: TALConsoleColor; const aBackgroundColor: TALConsoleColor);
var LForegroundColor: TALConsoleColor;
    LBackgroundColor: TALConsoleColor;
begin
  ALGetConsoleColors(LForegroundColor, LBackgroundColor);
  ALSetConsoleColors(AForegroundColor, ABackgroundColor);
  System.Writeln(AStr);
  ALSetConsoleColors(LForegroundColor, LBackgroundColor);
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: String);
begin
  System.Write(AStr);
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: String; const aForegroundColor: TALConsoleColor);
var LForegroundColor: TALConsoleColor;
    LBackgroundColor: TALConsoleColor;
begin
  ALGetConsoleColors(LForegroundColor, LBackgroundColor);
  ALSetConsoleColors(AForegroundColor, LBackgroundColor);
  System.Writeln(AStr);
  ALSetConsoleColors(LForegroundColor, LBackgroundColor);
end;
{$ENDIF}

{****************}
{$IFDEF MSWINDOWS}
Procedure ALWriteLN(const AStr: String; const aForegroundColor: TALConsoleColor; const aBackgroundColor: TALConsoleColor);
var LForegroundColor: TALConsoleColor;
    LBackgroundColor: TALConsoleColor;
begin
  ALGetConsoleColors(LForegroundColor, LBackgroundColor);
  ALSetConsoleColors(AForegroundColor, ABackgroundColor);
  System.Writeln(AStr);
  ALSetConsoleColors(LForegroundColor, LBackgroundColor);
end;
{$ENDIF}

{***********************************}
{$IF CompilerVersion >= 34} // sydney
Procedure ALFreeAndNil(const [ref] Obj: TObject; const ADelayed: boolean = false);
{$ELSE}
Procedure ALFreeAndNil(var Obj; const ADelayed: boolean = false);
{$ENDIF}
var Temp: TObject;
begin
  {$IF CompilerVersion >= 34} // sydney
  Temp := Obj;
  if Temp = nil then exit;
  TObject(Pointer(@Obj)^) := nil;
  {$ELSE}
  Temp := TObject(Obj);
  if Temp = nil then exit;
  TObject(Obj) := nil;
  {$ENDIF}
  if adelayed and assigned(ALCustomDelayedFreeObjectProc) then ALCustomDelayedFreeObjectProc(Temp)
  else begin
    {$IF defined(AUTOREFCOUNT)}
    //AUTOREFCOUNT was removed in 10.4 (sydney) so the
    //code below is for version < than 10.4
    if AtomicCmpExchange(temp.refcount{Target}, 0{NewValue}, 0{Compareand}) = 1 then begin // it's seam it's not an atomic operation (http://stackoverflow.com/questions/39987850/is-reading-writing-an-integer-4-bytes-atomic-on-ios-android-like-on-win32-win6)
      Temp.Free;
      Temp := nil;
    end
    else begin
      Temp.DisposeOf; // TComponent Free Notification mechanism notifies registered components that particular
                      // component instance is being freed. Notified components can handle that notification inside
                      // virtual Notification method and make sure that they clear all references they may hold on
                      // component being destroyed.
                      //
                      // Free Notification mechanism is being triggered in TComponent destructor and without DisposeOf
                      // and direct execution of destructor, two components could hold strong references to each
                      // other keeping themselves alive during whole application lifetime.
      Temp := nil;
    end;
    {$ELSE}
    Temp.Free;
    Temp := nil;
    {$ENDIF}
  end;
end;


initialization
  {$IF defined(MSWindows)}
  ALPerformanceFrequency := -1;
  {$ENDIF}
  _ALLogQueue := TList<_TALLogItem>.Create;
  _ALLogHistory := TList<_TALLogItem>.Create;
  ALMove := system.Move;


Finalization
  ALFreeAndNil(_ALLogHistory);
  ALFreeAndNil(_ALLogQueue);

end.
