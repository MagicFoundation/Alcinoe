unit ALCommon;

interface

uses
  {$IFDEF IOS}
  iOSapi.Foundation,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  system.sysutils,
  system.types;

{$I Alcinoe.inc}

{$IF CompilerVersion <= 25} // xe4
type
  {$SCOPEDENUMS ON}
  THorzRectAlign = (Center, Left, Right);
  TVertRectAlign = (Center, Top, Bottom);
  {$SCOPEDENUMS OFF}
{$IFEND}

type

  TALPointDType = array [0..1] of Double;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 34} // sydney
    {$MESSAGE WARN 'Check if System.Types.TPointf still having the same implementation and adjust the IFDEF'}
  {$IFEND}
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

    class function PointInCircle(const Point, Center: TALPointD; const Radius: Integer): Boolean; static; inline;
    /// <summary> Zero point having values of (0, 0). </summary>
    class function Zero: TALPointD; inline; static;

    function Distance(const APoint: TALPointD): Double;
    // 3D cross-product with Z = 0
    function CrossProduct(const APoint: TALPointD): Double;
    function DotProduct(const APoint: TALPointD): Double; inline;

    procedure Offset(const APoint: TALPointD); overload; inline;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 34} // sydney
    {$MESSAGE WARN 'Check if System.Types.TSizef still having the same implementation and adjust the IFDEF'}
  {$IFEND}
  PALSizeD = ^TALSizeD;
  TALSizeD = record
    cx: Double;
    cy: Double;
  public
    constructor Create(P: TALSizeD); overload;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IF CompilerVersion > 34} // sydney
    {$MESSAGE WARN 'Check if System.Types.TRectf still having the same implementation and adjust the IFDEF'}
  {$IFEND}
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
    function PlaceInto(const ADesignatedArea: TALRectD; const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
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

    // sets new origin
    procedure SetLocation(const X, Y: Double); overload;
    procedure SetLocation(const Point: TALPointD); overload;

    // inflate by DX and DY
    procedure Inflate(const DX, DY: Double); overload;

    // inflate in all directions
    procedure Inflate(const DL, DT, DR, DB: Double); overload;

    //returns the center point of the rectangle;
    function CenterPoint: TALPointD;

    function Ceiling: TRect;
    function Truncate: TRect;
    function Round: TRect;

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

{~~~~~~~~~~~~~~~~~~~~~~~~}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if functions below implemented in System.Types still having the same implementation and adjust the IFDEF'}
{$IFEND}
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

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF; out Ratio: Single): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectF; out Ratio: Single): TRectF; overload;
function ALRectFitInto(const R: TRectf; const Bounds: TRectF): TRectF; overload;
function ALRectPlaceInto(const R: TRectf;
                         const Bounds: TRectf;
                         const CenterAt: TpointF;
                         out Ratio: Single): TRectF; overload;
function ALRectPlaceInto(const R: TRectf; const Bounds: TRectf; const CenterAt: TpointF): TRectF; overload;
function ALRectPlaceInto(const R: TRectf;
                         const Bounds: TRectF;
                         const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
                         const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF; overload;

type

  {$IFNDEF ALHideAnsiString}
  EALException = class(Exception)
  public
    constructor Create(const Msg: AnsiString);
    constructor CreateFmt(const Msg: ansistring; const Args: array of const);
  end;
  {$ENDIF !ALHideAnsiString}
  EALExceptionU = class(Exception);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
var ALCallStackCustomLogsMaxCount: integer = 50;
procedure ALAddCallStackCustomLogU(Const aLog: String);
function ALGetCallStackCustomLogsU(Const aPrependTimeStamp: boolean = True; Const aPrependThreadID: boolean = True): String;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Type TalLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);
Var ALEnqueueLog: Boolean; // We can use this flag to enqueue the log when the device just started and when we didn't yet
procedure ALPrintLogQueue; // pluged the device to the monitoring tool, so that we can print the log a little later

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
type TALCustomDelayedFreeObjectProc = procedure(var aObject: Tobject) of object;
var ALCustomDelayedFreeObjectProc: TALCustomDelayedFreeObjectProc;
{$IF CompilerVersion >= 34} // sydney
Procedure ALFreeAndNil(const [ref] Obj: TObject; const ADelayed: boolean = false); inline;
{$ELSE}
Procedure ALFreeAndNil(var Obj; const ADelayed: boolean = false); inline;
{$ENDIF}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Function AlBoolToInt(Value:Boolean):Integer;
Function AlIntToBool(Value:integer):boolean;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function  ALIfThen(AValue: Boolean; const ATrue: Integer; const AFalse: Integer = 0): Integer; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Int64; const AFalse: Int64 = 0): Int64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: UInt64; const AFalse: UInt64 = 0): UInt64; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Single; const AFalse: Single = 0): Single; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Double; const AFalse: Double = 0): Double; overload; inline;
function  ALIfThen(AValue: Boolean; const ATrue: Extended; const AFalse: Extended = 0): Extended; overload; inline;
{$IFNDEF ALHideAnsiString}
function  ALIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString; overload; inline;
{$ENDIF !ALHideAnsiString}
function  ALIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String; overload; inline;

{$IFDEF MSWINDOWS}
{$IF CompilerVersion > 34} // sydney
  {$MESSAGE WARN 'Check if EnumDynamicTimeZoneInformation/SystemTimeToTzSpecificLocalTimeEx/TzSpecificLocalTimeToSystemTimeEx are still not declared in Winapi.Windows and adjust the IFDEF'}
{$ENDIF}
{$WARNINGS OFF}
function EnumDynamicTimeZoneInformation(dwIndex: DWORD; lpTimeZoneInformation: PDynamicTimeZoneInformation): DWORD; stdcall; external advapi32 delayed;
function SystemTimeToTzSpecificLocalTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpUniversalTime, lpLocalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
function TzSpecificLocalTimeToSystemTimeEx(lpTimeZoneInformation: PDynamicTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall; external Kernel32 delayed;
{$WARNINGS ON}
Function ALGetDynamicTimeZoneInformations: Tarray<TDynamicTimeZoneInformation>;
Function ALGetDynamicTimeZoneInformation(const aTimeZoneKeyName: String): TDynamicTimeZoneInformation;
{$ENDIF MSWINDOWS}
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
var ALMove: procedure (const Source; var Dest; Count: NativeInt);

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

implementation

uses
  system.Classes,
  system.math,
  system.generics.collections,
  {$IF defined(ANDROID)}
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  ALAndroidApi,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.Helpers,
  {$ENDIF}
  system.DateUtils,
  ALString;

type

  {******************************}
  _TALCallStackCustomLogU = record
    ThreadID: TThreadID;
    TimeStamp: TDateTime;
    log: String;
  end;

var
  _ALCallStackCustomLogsU: TList<_TALCallStackCustomLogU>;
  _ALCallStackCustomLogsUCurrentIndex: integer = -1;

type

  {***********************}
  _TALLogQueueItem = record
  private
    Tag: String;
    msg: String;
    _type: TalLogType;
  public
    class function Create(const ATag: String; const AMsg: String; Const aType: TalLogType): _TALLogQueueItem; static; inline;
  end;

var
  _ALLogQueue: TList<_TALLogQueueItem>;

{************************************************************************************************************************}
class function _TALLogQueueItem.Create(const ATag: String; const AMsg: String; Const aType: TalLogType): _TALLogQueueItem;
begin
  Result.Tag := aTag;
  Result.Msg := aMsg;
  Result._Type := aType;
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
  if not R2.IsEmpty then
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
function ALRectPlaceInto(const R: TRectf;
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
function ALRectPlaceInto(const R: TRectf;
                         const Bounds: TRectF;
                         const AHorzAlign: THorzRectAlign = THorzRectAlign.Center;
                         const AVertAlign: TVertRectAlign = TVertRectAlign.Center): TRectF;
var
  LLocation: TPointF;
begin
  Result := R;
  if (R.Width > Bounds.Width) or (R.Height > Bounds.Height) then
    Result := ALRectFitInto(Result, Bounds);
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

{*****************************************************************************************************}
class function TALPointD.PointInCircle(const Point, Center: TALPointD; const Radius: Integer): Boolean;
begin
  Result := Point.Distance(Center) <= Radius;
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

{********************************************************************************************}
function TALRectD.PlaceInto(const ADesignatedArea: TALRectD; const AHorzAlign: THorzRectAlign;
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

{$IFNDEF ALHideAnsiString}

{*****************************************************}
constructor EALException.Create(const Msg: AnsiString);
begin
  inherited create(String(Msg));
end;

{************************************************************************************}
constructor EALException.CreateFmt(const Msg: ansistring; const Args: array of const);
begin
  inherited CreateFmt(String(Msg), Args);
end;

{$ENDIF !ALHideAnsiString}

{*****************************************************}
procedure ALAddCallStackCustomLogU(Const aLog: String);
var LCallStackCustomLogU: _TALCallStackCustomLogU;
begin
  LCallStackCustomLogU.ThreadID := TThread.Current.ThreadID;
  LCallStackCustomLogU.TimeStamp := Now;
  LCallStackCustomLogU.log := aLog;
  Tmonitor.enter(_ALCallStackCustomLogsU);
  Try
    _ALCallStackCustomLogsUCurrentIndex := (_ALCallStackCustomLogsUCurrentIndex + 1) mod ALCallStackCustomLogsMaxCount;
    if _ALCallStackCustomLogsUCurrentIndex <= _ALCallStackCustomLogsU.Count - 1 then
      _ALCallStackCustomLogsU[_ALCallStackCustomLogsUCurrentIndex] := LCallStackCustomLogU
    else
      _ALCallStackCustomLogsUCurrentIndex := _ALCallStackCustomLogsU.Add(LCallStackCustomLogU);
  Finally
    Tmonitor.exit(_ALCallStackCustomLogsU);
  End;
end;

{**************************************************************************************************************************}
function ALGetCallStackCustomLogsU(Const aPrependTimeStamp: boolean = True; Const aPrependThreadID: boolean = True): String;
Var i: integer;
begin
  Result := '';
  Tmonitor.enter(_ALCallStackCustomLogsU);
  Try
    if aPrependTimeStamp and aPrependThreadID then begin
      for i := _ALCallStackCustomLogsUCurrentIndex downto 0 do
        Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_ALCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ' [' + ALIntToStrU(_ALCallStackCustomLogsU[i].ThreadID) + ']: ' + _ALCallStackCustomLogsU[i].log + #13#10;
      for i := _ALCallStackCustomLogsU.Count - 1 downto _ALCallStackCustomLogsUCurrentIndex + 1 do
        Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_ALCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ' [' + ALIntToStrU(_ALCallStackCustomLogsU[i].ThreadID) + ']: ' + _ALCallStackCustomLogsU[i].log + #13#10;
    end
    else if aPrependTimeStamp then begin
      for i := _ALCallStackCustomLogsUCurrentIndex downto 0 do
        Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_ALCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ': ' + _ALCallStackCustomLogsU[i].log + #13#10;
      for i := _ALCallStackCustomLogsU.Count - 1 downto _ALCallStackCustomLogsUCurrentIndex + 1 do
        Result := Result + ALFormatDateTimeU('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss''.''zzz''Z''', TTimeZone.Local.ToUniversalTime(_ALCallStackCustomLogsU[i].TimeStamp), ALDefaultFormatSettingsU) + ': ' + _ALCallStackCustomLogsU[i].log + #13#10;
    end
    else if aPrependThreadID then begin
      for i := _ALCallStackCustomLogsUCurrentIndex downto 0 do
        Result := Result + '[' + ALIntToStrU(_ALCallStackCustomLogsU[i].ThreadID) + ']: ' + _ALCallStackCustomLogsU[i].log + #13#10;
      for i := _ALCallStackCustomLogsU.Count - 1 downto _ALCallStackCustomLogsUCurrentIndex + 1 do
        Result := Result + '[' + ALIntToStrU(_ALCallStackCustomLogsU[i].ThreadID) + ']: ' + _ALCallStackCustomLogsU[i].log + #13#10;
    end
    else begin
      for i := _ALCallStackCustomLogsUCurrentIndex downto 0 do
        Result := Result + _ALCallStackCustomLogsU[i].log + #13#10;
      for i := _ALCallStackCustomLogsU.Count - 1 downto _ALCallStackCustomLogsUCurrentIndex + 1 do
        Result := Result + _ALCallStackCustomLogsU[i].log + #13#10;
    end;
  Finally
    Tmonitor.exit(_ALCallStackCustomLogsU);
  End;
  Result := ALTrimU(Result);
end;

{***********************************************************************************************}
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);
{$IF defined(IOS) or defined(MSWINDOWS)}
var LMsg: String;
{$ENDIF}
begin
  if ALEnqueueLog then begin
    Tmonitor.Enter(_ALLogQueue);
    try
      _ALLogQueue.Add(
        _TALLogQueueItem.Create(
          Tag, // const ATag: String;
          Msg, // const AMsg: String;
          _Type)); // Const aType: TalLogType
    finally
      Tmonitor.Exit(_ALLogQueue);
    end;
  end
  else begin
    {$IF defined(ANDROID)}
    case _type of
      TalLogType.VERBOSE: TJLog.JavaClass.v(StringToJString(Tag), StringToJString(msg));
      TalLogType.DEBUG: TJLog.JavaClass.d(StringToJString(Tag), StringToJString(msg));
      TalLogType.INFO: TJLog.JavaClass.i(StringToJString(Tag), StringToJString(msg));
      TalLogType.WARN: TJLog.JavaClass.w(StringToJString(Tag), StringToJString(msg));
      TalLogType.ERROR: TJLog.JavaClass.e(StringToJString(Tag), StringToJString(msg));
      TalLogType.ASSERT: TJLog.JavaClass.wtf(StringToJString(Tag), StringToJString(msg)); // << wtf for What a Terrible Failure but everyone know that it's for what the fuck !
    end;
    {$ELSEIF defined(IOS)}
    // https://forums.developer.apple.com/thread/4685
    //if _type <> TalLogType.VERBOSE  then begin // because log on ios slow down the app so skip verbosity
      if msg <> '' then LMsg := ' => ' + msg
      else LMsg := '';
      case _type of
        TalLogType.VERBOSE: NSLog(StringToID('[V] ' + Tag + LMsg));
        TalLogType.DEBUG:   NSLog(StringToID('[D][V] ' + Tag + LMsg));
        TalLogType.INFO:    NSLog(StringToID('[I][D][V] ' + Tag + LMsg));
        TalLogType.WARN:    NSLog(StringToID('[W][I][D][V] ' + Tag + LMsg));
        TalLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V] ' + Tag + LMsg));
        TalLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V] ' + Tag + LMsg));
      end;
    //end;
    {$ELSEIF defined(MSWINDOWS)}
    if _type <> TalLogType.VERBOSE  then begin // because log on windows slow down the app so skip verbosity
      if msg <> '' then LMsg := ' => ' + stringReplace(msg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
      else LMsg := '';
      case _type of
        TalLogType.VERBOSE: OutputDebugString(pointer('[V] ' + Tag + LMsg + ' |'));
        TalLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + Tag + LMsg + ' |'));
        TalLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + Tag + LMsg + ' |'));
        TalLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + Tag + LMsg + ' |'));
        TalLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + Tag + LMsg + ' |'));
        TalLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + Tag + LMsg + ' |'));
      end;
    end;
    {$IFEND}
  end;
end;

{************************}
procedure ALPrintLogQueue;
var LOldEnqueueLogValue: Boolean;
    i: integer;
begin
  LOldEnqueueLogValue := ALEnqueueLog;
  ALEnqueueLog := False;
  Tmonitor.Enter(_ALLogQueue);
  try
    for I := 0 to _ALLogQueue.Count - 1 do
      with _ALLogQueue[i] do
        ALLog(Tag, Msg, _type);
    _ALLogQueue.Clear;
  finally
    Tmonitor.Exit(_ALLogQueue);
    ALEnqueueLog := LOldEnqueueLogValue;
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

{$IFNDEF ALHideAnsiString}

{***********************************************************************************************}
function ALIfThen(AValue: Boolean; const ATrue: AnsiString; AFalse: AnsiString = ''): AnsiString;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

{$ENDIF !ALHideAnsiString}

{************************************************************************************}
function ALIfThenU(AValue: Boolean; const ATrue: String; AFalse: String = ''): String;
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
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
      result := LDynamicTimeZoneInformations[i];
      Exit;
    end;
  raise Exception.Create('Unknown TimeZoneKeyName');
end;
{$ENDIF}

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
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
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
    if ALSameTextU(LDynamicTimeZoneInformations[i].TimeZoneKeyName, aTimeZoneKeyName) then begin
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

{******************************************************}
{Accepts number of milliseconds in the parameter aValue,
 provides 1000 times more precise value of TDateTime}
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
begin
  Result := IncMilliSecond(UnixDateDelta, aValue);
end;

{********************************************************}
{Returns UNIX-time as the count of milliseconds since the
 UNIX epoch. Can be very useful for the purposes of
 special precision.}
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
begin
  result := MilliSecondsBetween(UnixDateDelta, aValue);
  if aValue < UnixDateDelta then result := -result;
end;

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
  ALCustomDelayedFreeObjectProc := nil;
  ALEnqueueLog := False;
  _ALLogQueue := TList<_TALLogQueueItem>.Create;
  _ALCallStackCustomLogsU := TList<_TALCallStackCustomLogU>.Create;
  ALMove := system.Move;


Finalization
  ALFreeAndNil(_ALCallStackCustomLogsU);
  ALFreeAndNil(_ALLogQueue);

end.
