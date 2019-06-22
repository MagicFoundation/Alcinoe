unit ALCommon;

interface

uses {$IFDEF IOS}
     iOSapi.Foundation,
     {$ENDIF}
     system.types;

{$IF CompilerVersion < 29} {Delphi XE8}
  {$IF defined(CPUX64)} // The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU64BITS} // The CPU is in a 64-bit environment, such as DCC64.EXE. *New* in XE8
  {$ENDIF}
  {$IF defined(CPUX86)} // 	The CPU supports the x86-64 instruction set, and is in a 64-bit environment. *New* in XE2/x64
    {$DEFINE CPU32BITS} // The CPU is in a 32-bit environment, such as DCC32.EXE. *New* in XE8
  {$ENDIF}
{$ENDIF}

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
  {$IF CompilerVersion > 33} // rio
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
  {$IF CompilerVersion > 33} // rio
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
  {$IF CompilerVersion > 33} // rio
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
    constructor Create(const Origin: TALPointD); overload;                               // empty rect at given origin
    constructor Create(const Origin: TALPointD; const Width, Height: Double); overload; // at TPoint of origin with width and height
    constructor Create(const Left, Top, Right, Bottom: Double); overload;              // at x, y with width and height
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
{$IF CompilerVersion > 33} // rio
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

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Type TalLogType = (VERBOSE, DEBUG, INFO, WARN, ERROR, ASSERT);
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
type TALCustomDelayedFreeObjectProc = procedure(var aObject: Tobject) of object;
var ALCustomDelayedFreeObjectProc: TALCustomDelayedFreeObjectProc;
{$IFDEF DEBUG}
var ALFreeAndNilRefCountWarn: boolean;
threadvar ALCurThreadFreeAndNilNORefCountWarn: boolean;
type TALFreeAndNilCanRefCountWarnProc = function(const aObject: Tobject): boolean of object;
var ALFreeAndNilCanRefCountWarnProc: TALFreeAndNilCanRefCountWarnProc;
{$ENDIF}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean = false); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean); overload; {$IFNDEF DEBUG}inline;{$ENDIF}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Function AlBoolToInt(Value:Boolean):Integer;
Function AlIntToBool(Value:integer):boolean;
Function ALMediumPos(LTotal, LBorder, LObject : integer):Integer;
function AlLocalDateTimeToUTCDateTime(Const aLocalDateTime: TDateTime): TdateTime;
function AlUTCDateTimeToLocalDateTime(Const aUTCDateTime: TDateTime): TdateTime;
{$IFDEF IOS}
function ALNSDateToUTCDateTime(const ADateTime: NSDate): TDateTime;
{$ENDIF}
function ALUTCNow: TDateTime;
function ALUnixMsToDateTime(const aValue: Int64): TDateTime;
function ALDateTimeToUnixMs(const aValue: TDateTime): Int64;
Function ALInc(var x: integer; Count: integer): Integer;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
const ALMAXUInt64: UInt64 = 18446744073709551615;
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

{$IFNDEF NEXTGEN}

type
  /// the potential features, retrieved from an Intel CPU
  // - see https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  TALIntelCpuFeature =
   ( { in EDX }
   cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA64, cfPBE,
   { in ECX }
   cfSSE3, cfCLMUL, cfDS64, cfMON, cfDSCPL, cfVMX, cfSMX, cfEST,
   cfTM2, cfSSSE3, cfCID, cfSDBG, cfFMA, cfCX16, cfXTPR, cfPDCM,
   cf_c16, cfPCID, cfDCA, cfSSE41, cfSSE42, cfX2A, cfMOVBE, cfPOPCNT,
   cfTSC2, cfAESNI, cfXS, cfOSXS, cfAVX, cfF16C, cfRAND, cfHYP,
   { extended features in EBX, ECX }
   cfFSGS, cf_b01, cfSGX, cfBMI1, cfHLE, cfAVX2, cf_b06, cfSMEP, cfBMI2,
   cfERMS, cfINVPCID, cfRTM, cfPQM, cf_b13, cfMPX, cfPQE, cfAVX512F,
   cfAVX512DQ, cfRDSEED, cfADX, cfSMAP, cfAVX512IFMA, cfPCOMMIT,
   cfCLFLUSH, cfCLWB, cfIPT, cfAVX512PF, cfAVX512ER, cfAVX512CD,
   cfSHA, cfAVX512BW, cfAVX512VL, cfPREFW1, cfAVX512VBMI);

  /// all features, as retrieved from an Intel CPU
  TALIntelCpuFeatures = set of TALIntelCpuFeature;

var
  /// the available CPU features, as recognized at program startup
  ALCpuFeatures: TALIntelCpuFeatures;

{$ENDIF}

implementation

uses system.Classes,
     system.math,
     {$IFDEF MSWINDOWS}
     Winapi.Windows,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.JavaTypes,
     Androidapi.Helpers,
     ALAndroidApi,
     {$ENDIF}
     {$IF defined(IOS)}
     Macapi.Helpers,
     {$ENDIF}
     system.sysutils,
     system.DateUtils;

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

{***********************************************************************************************}
procedure ALLog(Const Tag: String; Const msg: String; const _type: TalLogType = TalLogType.INFO);
{$IF defined(IOS) or defined(MSWINDOWS)}
var aMsg: String;
{$ENDIF}
begin
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
    if msg <> '' then aMsg := ' => ' + msg
    else aMsg := '';
    case _type of
      TalLogType.VERBOSE: NSLog(StringToID('[V] ' + Tag + aMsg));
      TalLogType.DEBUG:   NSLog(StringToID('[D][V] ' + Tag + aMsg));
      TalLogType.INFO:    NSLog(StringToID('[I][D][V] ' + Tag + aMsg));
      TalLogType.WARN:    NSLog(StringToID('[W][I][D][V] ' + Tag + aMsg));
      TalLogType.ERROR:   NSLog(StringToID('[E][W][I][D][V] ' + Tag + aMsg));
      TalLogType.ASSERT:  NSLog(StringToID('[A][E][W][I][D][V] ' + Tag + aMsg));
    end;
  //end;
  {$ELSEIF defined(MSWINDOWS)}
  if _type <> TalLogType.VERBOSE  then begin // because log on windows slow down the app so skip verbosity
    if msg <> '' then aMsg := ' => ' + stringReplace(msg, '%', '%%', [rfReplaceALL]) // https://quality.embarcadero.com/browse/RSP-15942
    else aMsg := '';
    case _type of
      TalLogType.VERBOSE: OutputDebugString(pointer('[V] ' + Tag + aMsg + ' |'));
      TalLogType.DEBUG:   OutputDebugString(pointer('[D][V] ' + Tag + aMsg + ' |'));
      TalLogType.INFO:    OutputDebugString(pointer('[I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.WARN:    OutputDebugString(pointer('[W][I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.ERROR:   OutputDebugString(pointer('[E][W][I][D][V] ' + Tag + aMsg + ' |'));
      TalLogType.ASSERT:  OutputDebugString(pointer('[A][E][W][I][D][V] ' + Tag + aMsg + ' |'));
    end;
  end;
  {$IFEND}
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

{********************************************************************************}
function AlLocalDateTimeToUTCDateTime(Const aLocalDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToUniversalTime(aLocalDateTime);
end;

{******************************************************************************}
function AlUTCDateTimeToLocalDateTime(Const aUTCDateTime: TDateTime): TdateTime;
begin
  result := TTimeZone.Local.ToLocalTime(aUTCDateTime);
end;

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

{*******************************************************}
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

{***************************************************************}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean = false);
var Temp: TObject;
begin
  Temp := TObject(Obj);
  if temp = nil then exit;
  TObject(Obj) := nil;
  if adelayed and assigned(ALCustomDelayedFreeObjectProc) then ALCustomDelayedFreeObjectProc(Temp)
  else begin
    {$IF defined(AUTOREFCOUNT)}
    if AtomicCmpExchange(temp.refcount{Target}, 0{NewValue}, 0{Compareand}) = 1 then begin // it's seam it's not an atomic operation (http://stackoverflow.com/questions/39987850/is-reading-writing-an-integer-4-bytes-atomic-on-ios-android-like-on-win32-win6)
      temp.Free;
      temp := nil;
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
      {$IF defined(DEBUG)}
      if ALFreeAndNilRefCountWarn and
        (not ALCurThreadFreeAndNilNORefCountWarn) and
        ((not assigned(ALFreeAndNilCanRefCountWarnProc)) or
         (ALFreeAndNilCanRefCountWarnProc(Temp))) then begin
        if (Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag}) <> 0 then
          ALLog('ALFreeAndNil', Temp.ClassName + ' | Refcount is not null (' + Inttostr((Temp.RefCount - 1) and (not $40000000{Temp.objDisposedFlag})) + ')', TalLogType.warn);
      end;
      {$ENDIF}
      temp := nil;
    end;
    {$ELSE}
    temp.Free;
    temp := nil;
    {$IFEND}
  end;
end;

{*************************************************************************************}
Procedure ALFreeAndNil(var Obj; const adelayed: boolean; const aRefCountWarn: Boolean);
{$IFDEF DEBUG}
var aOldCurThreadFreeAndNilNoRefCountWarn: boolean;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  aOldCurThreadFreeAndNilNoRefCountWarn := ALCurThreadFreeAndNilNORefCountWarn;
  ALCurThreadFreeAndNilNORefCountWarn := not aRefCountWarn;
  try
  {$ENDIF}

    ALFreeAndNil(Obj, adelayed);

  {$IFDEF DEBUG}
  finally
    ALCurThreadFreeAndNilNORefCountWarn := aOldCurThreadFreeAndNilNORefCountWarn;
  end;
  {$ENDIF}
end;


//
// Taken from https://github.com/synopse/mORMot.git
// https://synopse.info
// http://mormot.net
//

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if https://github.com/synopse/mORMot.git SynCommons.pas was not updated from references\mORMot\SynCommons.pas and adjust the IFDEF'}
{$ENDIF}

{$IFNDEF NEXTGEN}

{**}
type
 TRegisters = record
   eax,ebx,ecx,edx: cardinal;
 end;

{*************************************************************}
procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
{$IF defined(CPU64BITS)}
asm // ecx=param, rdx=Registers (Linux: edi,rsi)
        .NOFRAME
        mov     eax, ecx
        mov     r9, rdx
        mov     r10, rbx // preserve rbx
        xor     ebx, ebx
        xor     ecx, ecx
        xor     edx, edx
        cpuid
        mov     TRegisters(r9).&eax, eax
        mov     TRegisters(r9).&ebx, ebx
        mov     TRegisters(r9).&ecx, ecx
        mov     TRegisters(r9).&edx, edx
        mov     rbx, r10
end;
{$else}
asm
        push    esi
        push    edi
        mov     esi, edx
        mov     edi, eax
        pushfd
        pop     eax
        mov     edx, eax
        xor     eax, $200000
        push    eax
        popfd
        pushfd
        pop     eax
        xor     eax, edx
        jz      @nocpuid
        push    ebx
        mov     eax, edi
        xor     ecx, ecx
        cpuid
        mov     TRegisters(esi).&eax, eax
        mov     TRegisters(esi).&ebx, ebx
        mov     TRegisters(esi).&ecx, ecx
        mov     TRegisters(esi).&edx, edx
        pop     ebx
@nocpuid:
        pop     edi
        pop     esi
end;
{$ifend}

{*****************************}
procedure TestIntelCpuFeatures;
var regs: TRegisters;
begin
  {$R-} // this code require range check error OFF
  regs.edx := 0;
  regs.ecx := 0;
  GetCPUID(1,regs);
  PIntegerArray(@ALCpuFeatures)^[0] := regs.edx;
  PIntegerArray(@ALCpuFeatures)^[1] := regs.ecx;
  GetCPUID(7,regs);
  PIntegerArray(@ALCpuFeatures)^[2] := regs.ebx;
  PIntegerArray(@ALCpuFeatures)^[3] := regs.ecx;
  PByte(@PIntegerArray(@ALCpuFeatures)^[4])^ := regs.edx;
  {$R+} // enable back the {$R+}
end;

{$ENDIF}

initialization

  {$IFNDEF NEXTGEN}
  TestIntelCpuFeatures;
  {$ENDIF}

  ALCustomDelayedFreeObjectProc := nil;
  {$IFDEF DEBUG}
  ALFreeAndNilRefCountWarn := False;
  ALFreeAndNilCanRefCountWarnProc := nil;
  {$ENDIF}

end.
