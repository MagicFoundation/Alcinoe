{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclMath.pas.                                                                }
{                                                                                                  }
{ The Initial Developers of the Original Code are Clayton Collie, David Butler, ESB Consultancy,   }
{ Jean Debord, Marcel van Brakel and Michael Schnell.                                              }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Ernesto Benestante                                                                             }
{   Marcel van Brakel                                                                              }
{   Aleksei Koudinov                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Mark Vaughan                                                                                   }
{   Andreas Hausladen                                                                              }
{   unknown                                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various mathematics classes and routines. Includes prime numbers, rational numbers,              }
{ complex numbers, generic floating point routines, hyperbolic and transcendenatal routines,       }
{ NAN and INF support and more.                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMath;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes,
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes,
  {$ENDIF ~HAS_UNITSCOPE}
  JclBase;

{ Mathematical constants }

const
  Bernstein: Float = 0.2801694990238691330364364912307;  // Bernstein constant
  Cbrt2: Float     = 1.2599210498948731647672106072782;  // CubeRoot(2)
  Cbrt3: Float     = 1.4422495703074083823216383107801;  // CubeRoot(3)
  Cbrt10: Float    = 2.1544346900318837217592935665194;  // CubeRoot(10)
  Cbrt100: Float   = 4.6415888336127788924100763509194;  // CubeRoot(100)
  CbrtPi: Float    = 1.4645918875615232630201425272638;  // CubeRoot(PI)
  Catalan: Float   = 0.9159655941772190150546035149324;  // Catalan constant
  Pi: Float        = 3.1415926535897932384626433832795;  // PI
  PiOn2: Float     = 1.5707963267948966192313216916398;  // PI / 2
  PiOn3: Float     = 1.0471975511965977461542144610932;  // PI / 3
  PiOn4: Float     = 0.78539816339744830961566084581988; // PI / 4
  Sqrt2: Float     = 1.4142135623730950488016887242097;  // Sqrt(2)
  Sqrt3: Float     = 1.7320508075688772935274463415059;  // Sqrt(3)
  Sqrt5: Float     = 2.2360679774997896964091736687313;  // Sqrt(5)
  Sqrt10: Float    = 3.1622776601683793319988935444327;  // Sqrt(10)
  SqrtPi: Float    = 1.7724538509055160272981674833411;  // Sqrt(PI)
  Sqrt2Pi: Float   = 2.506628274631000502415765284811;   // Sqrt(2 * PI)
  TwoPi: Float     = 6.283185307179586476925286766559;   // 2 * PI
  ThreePi: Float   = 9.4247779607693797153879301498385;  // 3 * PI
  Ln2: Float       = 0.69314718055994530941723212145818; // Ln(2)
  Ln10: Float      = 2.3025850929940456840179914546844;  // Ln(10)
  LnPi: Float      = 1.1447298858494001741434273513531;  // Ln(PI)
  Log2: Float      = 0.30102999566398119521373889472449; // Log10(2)
  Log3: Float      = 0.47712125471966243729502790325512; // Log10(3)
  LogPi: Float     = 0.4971498726941338543512682882909;  // Log10(PI)
  LogE: Float      = 0.43429448190325182765112891891661; // Log10(E)
  E: Float         = 2.7182818284590452353602874713527;  // Natural constant
  hLn2Pi: Float    = 0.91893853320467274178032973640562; // Ln(2*PI)/2
  inv2Pi: Float    = 0.15915494309189533576888376337251436203445964574046; // 0.5 / Pi
  TwoToPower63: Float = 9223372036854775808.0;           // 2^63
  GoldenMean: Float   = 1.618033988749894848204586834365638;  // GoldenMean
  EulerMascheroni: Float = 0.5772156649015328606065120900824;  // Euler GAMMA

const
  MaxAngle: Float = 9223372036854775808.0; // 2^63 Rad

  {$IFDEF MATH_EXTENDED_PRECISION}
  MaxTanH: Float = 5678.2617031470719747459655389854; // Ln(2^16384)/2
  MaxFactorial   = 1754;
  MaxFloatingPoint: Float = 1.189731495357231765085759326628E+4932; // 2^16384
  MinFloatingPoint: Float = 3.3621031431120935062626778173218E-4932; // 2^(-16382)
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  MaxTanH: Float = 354.89135644669199842162284618659; // Ln(2^1024)/2
  MaxFactorial   = 170;
  MaxFloatingPoint: Float = 1.797693134862315907729305190789E+308; // 2^1024
  MinFloatingPoint: Float = 2.2250738585072013830902327173324E-308; // 2^(-1022)
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  MaxTanH: Float = 44.361419555836499802702855773323; // Ln(2^128)/2
  MaxFactorial   = 33;
  MaxFloatingPoint: Float = 3.4028236692093846346337460743177E+38; // 2^128
  MinFloatingPoint: Float = 1.1754943508222875079687365372222E-38; // 2^(-126)
  {$ENDIF MATH_SINGLE_PRECISION}

const
  PiExt = 3.1415926535897932384626433832795;
  RatioDegToRad : Extended = PiExt / 180.0;
  RatioRadToDeg : Extended = 180.0 / PiExt;
  RatioGradToRad : Extended = PiExt / 200.0;
  RatioRadToGrad : Extended = 200.0 / PiExt;
  RatioDegToGrad : Extended = 200.0 / 180.0;
  RatioGradToDeg : Extended = 180.0 / 200.0;

var
  PrecisionTolerance: Float = 0.0000001;
  EpsSingle: Single;
  EpsDouble: Double;
  {$IFDEF SUPPORTS_EXTENDED}
  EpsExtended: Extended;
  {$ENDIF SUPPORTS_EXTENDED}
  Epsilon: Float;
  ThreeEpsSingle: Single;
  ThreeEpsDouble: Double;
  {$IFDEF SUPPORTS_EXTENDED}
  ThreeEpsExtended: Extended;
  {$ENDIF SUPPORTS_EXTENDED}
  ThreeEpsilon: Float;

type
  TPrimalityTestMethod = (ptTrialDivision {$IFDEF CPU32}, ptRabinMiller{$ENDIF CPU32});

// swaps 2 bytes
procedure SwapOrd(var X, Y: Integer); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

// converts double to hex
function DoubleToHex(const D: Double): string; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
// converts hex to double
function HexToDouble(const Hex: string): Double;

// Converts degrees to radians.
{$IFDEF SUPPORTS_EXTENDED}
function DegToRad(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function DegToRad(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function DegToRad(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastDegToRad;

// Converts radians to degrees.
{$IFDEF SUPPORTS_EXTENDED}
function RadToDeg(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function RadToDeg(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function RadToDeg(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastRadToDeg;

// Converts grads to radians.
{$IFDEF SUPPORTS_EXTENDED}
function GradToRad(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function GradToRad(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function GradToRad(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastGradToRad;

// Converts radians to grads.
{$IFDEF SUPPORTS_EXTENDED}
function RadToGrad(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function RadToGrad(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function RadToGrad(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastRadToGrad;

// Converts degrees to grads.
{$IFDEF SUPPORTS_EXTENDED}
function DegToGrad(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function DegToGrad(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function DegToGrad(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastDegToGrad;

// Converts grads to degrees.
{$IFDEF SUPPORTS_EXTENDED}
function GradToDeg(const Value: Extended): Extended; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}
function GradToDeg(const Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function GradToDeg(const Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure FastGradToDeg;

{ Logarithmic }

function LogBase10(X: Float): Float;
function LogBase2(X: Float): Float;
function LogBaseN(Base, X: Float): Float;

{ Transcendental }

function ArcCos(X: Float): Float;
function ArcCot(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ArcCsc(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ArcSec(X: Float): Float;
function ArcSin(X: Float): Float;
function ArcTan(X: Float): Float;
function ArcTan2(Y, X: Float): Float;
function Cos(X: Float): Float; overload;
function Cot(X: Float): Float; overload;
function Coversine(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function Csc(X: Float): Float; overload;
function Exsecans(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function Haversine(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function Sec(X: Float): Float; overload;
function Sin(X: Float): Float; overload;
procedure SinCos(X: Single; out Sin, Cos: Single); overload;
procedure SinCos(X: Double; out Sin, Cos: Double); overload;
{$IFDEF SUPPORTS_EXTENDED}
procedure SinCos(X: Extended; out Sin, Cos: Extended); overload;
{$ENDIF SUPPORTS_EXTENDED}
function Tan(X: Float): Float; overload;
function Versine(X: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{ Hyperbolic }

function ArcCosH(X: Float): Float;
function ArcCotH(X: Float): Float;
function ArcCscH(X: Float): Float;
function ArcSecH(X: Float): Float;
function ArcSinH(X: Float): Float;
function ArcTanH(X: Float): Float;
function CosH(X: Float): Float; overload;
function CotH(X: Float): Float; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function CscH(X: Float): Float; overload;
function SecH(X: Float): Float; overload;
function SinH(X: Float): Float; overload; {IFDEF SUPPORTS_INLINE inline; ENDIF}
function TanH(X: Float): Float; overload;

{ Coordinate conversion }

function DegMinSecToFloat(const Degs, Mins, Secs: Float): Float; // obsolete (see JclUnitConv)
procedure FloatToDegMinSec(const X: Float; var Degs, Mins, Secs: Float); // obsolete (see JclUnitConv)

{ Exponential }

function Exp(const X: Float): Float; overload;
function Power(const Base, Exponent: Float): Float; overload;
function PowerInt(const X: Float; N: Integer): Float; overload;
function TenToY(const Y: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function TruncPower(const Base, Exponent: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function TwoToY(const Y: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{ Floating point numbers support routines }

function IsFloatZero(const X: Float): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function FloatsEqual(const X, Y: Float): Boolean;
function MaxFloat(const X, Y: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function MinFloat(const X, Y: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function ModFloat(const X, Y: Float): Float;
function RemainderFloat(const X, Y: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function SetPrecisionTolerance(NewTolerance: Float): Float; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure SwapFloats(var X, Y: Float); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
procedure CalcMachineEpsSingle;
procedure CalcMachineEpsDouble;
{$IFDEF SUPPORTS_EXTENDED}
procedure CalcMachineEpsExtended;
{$ENDIF SUPPORTS_EXTENDED}
procedure CalcMachineEps;
procedure SetPrecisionToleranceToEpsilon; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{ Miscellaneous }

function Ackermann(const A, B: Integer): Integer;
function Ceiling(const X: Float): Integer;
function CommercialRound(const X: Float): Int64;
function Factorial(const N: Integer): Float;
function Fibonacci(const N: Integer): Integer;
function Floor(const X: Float): Integer;
function GCD(X, Y: Cardinal): Cardinal;
function ISqrt(const I: Smallint): Smallint;
function LCM(const X, Y: Cardinal): Cardinal;
function NormalizeAngle(const Angle: Float): Float;
function Pythagoras(const X, Y: Float): Float;
function Sgn(const X: Float): Integer;
function Signe(const X, Y: Float): Float;

{ Ranges }
function EnsureRange(const AValue, AMin, AMax: Integer): Integer; overload;
function EnsureRange(const AValue, AMin, AMax: Int64): Int64; overload;
function EnsureRange(const AValue, AMin, AMax: Double): Double; overload;

{ Prime numbers }

function IsRelativePrime(const X, Y: Cardinal): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function IsPrimeTD(N: Cardinal): Boolean;
{$IFDEF CPU32}
function IsPrimeRM(N: Cardinal): Boolean;
{$ENDIF CPU32}
function IsPrimeFactor(const F, N: Cardinal): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function PrimeFactors(N: Cardinal): TDynCardinalArray;

var
  IsPrime: function(N: Cardinal): Boolean = IsPrimeTD;

procedure SetPrimalityTest(const Method: TPrimalityTestMethod);

{ Floating point value classification }

type
  TFloatingPointClass =
   (
    fpZero,     // zero
    fpNormal,   // normal finite <> 0
    fpDenormal, // denormalized finite
    fpInfinite, // infinite
    fpNaN,      // not a number
    fpInvalid,  // unsupported floating point format
    fpEmpty     // should not happen
   );

const
  Infinity    = 1/0;       // tricky
  {$EXTERNALSYM Infinity}
  NaN         = 0/0;       // tricky
  {$EXTERNALSYM NaN}
  NegInfinity = -Infinity;
  {$EXTERNALSYM NegInfinity}

{$IFDEF WIN64}
{$HPPEMIT 'static const double Infinity    =  1.0 / 0.0;'}
{$HPPEMIT 'static const double NaN         =  0.0 / 0.0;'}
{$HPPEMIT 'static const double NegInfinity = -1.0 / 0.0;'}
{$ELSE}
{$HPPEMIT 'static const Infinity    =  1.0 / 0.0;'}
{$HPPEMIT 'static const NaN         =  0.0 / 0.0;'}
{$HPPEMIT 'static const NegInfinity = -1.0 / 0.0;'}
{$ENDIF WIN64}

{$IFDEF CPU32}

function FloatingPointClass(const Value: Single): TFloatingPointClass; overload;
function FloatingPointClass(const Value: Double): TFloatingPointClass; overload;
{$IFDEF SUPPORTS_EXTENDED}
function FloatingPointClass(const Value: Extended): TFloatingPointClass; overload;
{$ENDIF SUPPORTS_EXTENDED}

{ NaN and INF support }

type
  TNaNTag = type Integer;

const
  LowValidNaNTag = -$3FFFFF;
  HighValidNaNTag = $3FFFFE;

function IsInfinite(const Value: Single): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function IsInfinite(const Value: Double): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$IFDEF SUPPORTS_EXTENDED}
function IsInfinite(const Value: Extended): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}

function IsNaN(const Value: Single): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function IsNaN(const Value: Double): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$IFDEF SUPPORTS_EXTENDED}
function IsNaN(const Value: Extended): Boolean; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
{$ENDIF SUPPORTS_EXTENDED}

function IsSpecialValue(const X: Float): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

procedure MakeQuietNaN(var X: Single; Tag: TNaNTag = 0); overload;
procedure MakeQuietNaN(var X: Double; Tag: TNaNTag = 0); overload;
{$IFDEF SUPPORTS_EXTENDED}
procedure MakeQuietNaN(var X: Extended; Tag: TNaNTag = 0); overload;
{$ENDIF SUPPORTS_EXTENDED}

procedure MakeSignalingNaN(var X: Single; Tag: TNaNTag = 0); overload;
procedure MakeSignalingNaN(var X: Double; Tag: TNaNTag = 0); overload;
{$IFDEF SUPPORTS_EXTENDED}
procedure MakeSignalingNaN(var X: Extended; Tag: TNaNTag = 0); overload;
{$ENDIF SUPPORTS_EXTENDED}

{ Mine*Buffer fills "Buffer" with consecutive tagged signaling NaNs.

  This allows for real number arrays which enforce initialization: any attempt
  to load an uninitialized array element into the FPU will raise an exception
  either of class EInvalidOp (Windows 9x/ME) or EJclNaNSignal (Windows NT).

  Under Windows NT it is thus possible to derive the violating array index from
  the EJclNaNSignal object's Tag property. }

procedure MineSingleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag = 0);
procedure MineDoubleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag = 0);

function MinedSingleArray(Length: Integer): TDynSingleArray;
function MinedDoubleArray(Length: Integer): TDynDoubleArray;

function GetNaNTag(const NaN: Single): TNaNTag; overload;
function GetNaNTag(const NaN: Double): TNaNTag; overload;
{$IFDEF SUPPORTS_EXTENDED}
function GetNaNTag(const NaN: Extended): TNaNTag; overload;
{$ENDIF SUPPORTS_EXTENDED}

{$ENDIF CPU32}

{ Set support }

type
  TJclASet = class(TObject)
  public
    function GetBit(const Idx: Integer): Boolean; virtual; abstract;
    procedure SetBit(const Idx: Integer; const Value: Boolean); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Invert; virtual; abstract;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; virtual; abstract;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); virtual; abstract;
  end;

type
  TJclFlatSet = class(TJclASet)
  private
    FBits: TBits;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetBit(const Idx: Integer): Boolean; override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
  end;

type
  {$IFNDEF FPC}
  TPointerArray = array [0..MaxLongint div 256] of Pointer;
  PPointerArray = ^TPointerArray;
  {$ENDIF ~FPC}
  TDelphiSet = set of Byte; // 256 elements
  PDelphiSet = ^TDelphiSet;

const
  EmptyDelphiSet: TDelphiSet = [];
  CompleteDelphiSet: TDelphiSet = [0..255];

type
  TJclSparseFlatSet = class(TJclASet)
  private
    FSetList: PPointerArray;
    FSetListEntries: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    function GetBit(const Idx: Integer): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
  end;

{ Rational numbers }

type
  TJclRational = class(TObject)
  private
    FT: Integer;
    FN: Integer;
    function GetAsString: string;
    procedure SetAsString(const S: string);
    function GetAsFloat: Float;
    procedure SetAsFloat(const R: Float);
  protected
    procedure Simplify;
  public
    constructor Create; overload;
    constructor Create(const R: Float); overload;
    constructor Create(const Numerator: Integer; const Denominator: Integer = 1); overload;

    property Numerator: Integer read FT;
    property Denominator: Integer read FN;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: Float read GetAsFloat write SetAsFloat;

    procedure Assign(const R: TJclRational); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Assign(const R: Float); overload;
    procedure Assign(const Numerator: Integer; const Denominator: Integer = 1); overload;

    procedure AssignZero; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure AssignOne; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function Duplicate: TJclRational; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    function IsEqual(const R: TJclRational): Boolean; reintroduce; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function IsEqual(const Numerator: Integer; const Denominator: Integer = 1) : Boolean; reintroduce; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function IsEqual(const R: Float): Boolean; reintroduce; overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    function IsZero: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function IsOne: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure Add(const R: TJclRational); overload;
    procedure Add(const V: Float); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Add(const V: Integer); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure Subtract(const R: TJclRational); overload;
    procedure Subtract(const V: Float); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Subtract(const V: Integer); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure Negate; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Abs;
    function Sgn: Integer;

    procedure Multiply(const R: TJclRational); overload;
    procedure Multiply(const V: Float); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Multiply(const V: Integer); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure Reciprocal;

    procedure Divide(const R: TJclRational); overload;
    procedure Divide(const V: Float); overload; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Divide(const V: Integer); overload;

    procedure Sqrt; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    procedure Sqr; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    procedure Power(const R: TJclRational); overload;
    procedure Power(const V: Integer); overload;
    procedure Power(const V: Float); overload;
  end;

type
  EJclMathError = class(EJclError);

  {$IFDEF CPU32}
  EJclNaNSignal = class(EJclMathError)
  private
    FTag: TNaNTag;
  public
    constructor Create(ATag: TNaNTag; Dummy: Boolean = False);
    property Tag: TNaNTag read FTag;
  end;
  {$ENDIF CPU32}

procedure DomainCheck(Err: Boolean);

{ Checksums }

function GetParity(Buffer: TDynByteArray; Len: Integer): Boolean; overload;
function GetParity(Buffer: PByte; Len: Integer): Boolean; overload;

{ CRC-16 }

type
  TCrc16Table = array [0..255] of Word;

var
  //  CRC16Polynom = $1021;
  Crc16DefaultTable: TCrc16Table = (
    $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0
   );
  Crc16DefaultStart: Cardinal = $FFFF;

const
  Crc16PolynomCCITT = $1021;
  Crc16PolynomIBM   = $8005;
  Crc16Bits = 16;
  Crc16Bytes = 2;
  Crc16HighBit = $8000;
  NotCrc16HighBit = $7FFF;

// for backward compatibility (default polynom = CCITT = $1021)
function Crc16_P(X: PJclByteArray; N: Integer; Crc: Word = 0): Word; overload;
function Crc16(const X: array of Byte; N: Integer; Crc: Word = 0): Word; overload;
function Crc16_A(const X: array of Byte; Crc: Word = 0): Word; overload;

function CheckCrc16_P(X: PJclByteArray; N: Integer; Crc: Word): Integer; overload;
function CheckCrc16(var X: array of Byte; N: Integer; Crc: Word): Integer; overload;
function CheckCrc16_A(var X: array of Byte; Crc: Word): Integer; overload;

// change the default polynom
procedure InitCrc16(Polynom, Start: Word); overload;

// arbitrary polynom
function Crc16_P(const Crc16Table: TCrc16Table; X: PJclByteArray; N: Integer; Crc: Word = 0): Word; overload;
function Crc16(const Crc16Table: TCrc16Table; const X: array of Byte; N: Integer; Crc: Word = 0): Word; overload;
function Crc16_A(const Crc16Table: TCrc16Table; const X: array of Byte; Crc: Word = 0): Word; overload;

function CheckCrc16_P(const Crc16Table: TCrc16Table; X: PJclByteArray; N: Integer; Crc: Word): Integer; overload;
function CheckCrc16(const Crc16Table: TCrc16Table; var X: array of Byte; N: Integer; Crc: Word): Integer; overload;
function CheckCrc16_A(const Crc16Table: TCrc16Table; var X: array of Byte; Crc: Word): Integer; overload;

// initialize a table
procedure InitCrc16(Polynom, Start: Word; out Crc16Table: TCrc16Table); overload;

{ CRC-32 }

type
  TCrc32Table = array [0..255] of Cardinal;

var
  //  CRC32Polynom = $04C11DB7;
  Crc32DefaultTable: TCrc32Table = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61, $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039, $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1, $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,
    $34867077, $30476DC0, $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE, $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6, $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637, $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF, $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7, $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8, $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0, $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668, $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
    );
  Crc32DefaultStart: Cardinal = $FFFFFFFF;

const
  Crc32PolynomIEEE       = $04C11DB7;
  Crc32PolynomCastagnoli = $1EDC6F41;
  Crc32Koopman           = $741B8CD7;
  Crc32Bits = 32;
  Crc32Bytes = 4;
  Crc32HighBit = $80000000;
  NotCrc32HighBit = $7FFFFFFF;

// for backward compatibility (default polynom = IEEE = $04C11DB7)
function Crc32_P(X: PJclByteArray; N: Integer; Crc: Cardinal = 0): Cardinal; overload;
function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal = 0): Cardinal; overload;
function Crc32_A(const X: array of Byte; Crc: Cardinal = 0): Cardinal; overload;

function CheckCrc32_P(X: PJclByteArray; N: Integer; Crc: Cardinal): Integer; overload;
function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer; overload;
function CheckCrc32_A(var X: array of Byte; Crc: Cardinal): Integer; overload;

// change the default polynom
procedure InitCrc32(Polynom, Start: Cardinal); overload;

// arbitrary polynom
function Crc32_P(const Crc32Table: TCrc32Table; X: PJclByteArray; N: Integer; Crc: Cardinal = 0): Cardinal; overload;
function Crc32(const Crc32Table: TCrc32Table; const X: array of Byte; N: Integer; Crc: Cardinal = 0): Cardinal; overload;
function Crc32_A(const Crc32Table: TCrc32Table; const X: array of Byte; Crc: Cardinal = 0): Cardinal; overload;

function CheckCrc32_P(const Crc32Table: TCrc32Table; X: PJclByteArray; N: Integer; Crc: Cardinal): Integer; overload;
function CheckCrc32(const Crc32Table: TCrc32Table; var X: array of Byte; N: Integer; Crc: Cardinal): Integer; overload;
function CheckCrc32_A(const Crc32Table: TCrc32Table; var X: array of Byte; Crc: Cardinal): Integer; overload;

// initialize a table
procedure InitCrc32(Polynom, Start: Cardinal; out Crc32Table: TCrc32Table); overload;

{ Complex numbers }

type
  TRectComplex = record
    Re: Float;
    Im: Float;
    {$IFDEF SUPPORTS_CLASS_OPERATORS}
    class operator Implicit(const Value: Float): TRectComplex;
    class operator Equal(const Z1, Z2: TRectComplex): Boolean;
    class operator NotEqual(const Z1, Z2: TRectComplex): Boolean;
    class operator Add(const Z1, Z2: TRectComplex): TRectComplex;
    class operator Subtract(const Z1, Z2: TRectComplex): TRectComplex;
    class operator Multiply(const Z1, Z2: TRectComplex): TRectComplex;
    class operator Divide(const Z1, Z2: TRectComplex): TRectComplex;
    class operator Negative(const Z: TRectComplex): TRectComplex;
    class operator Positive(const Z: TRectComplex): TRectComplex;
    function AsString: string;
    function Conjugate: TRectComplex;
    function IsZero: Boolean;
    function IsInfinite: Boolean;
    {$ENDIF SUPPORTS_CLASS_OPERATORS}
  end;

  TPolarComplex = record
    Radius: Float;
    Angle: Float;
    {$IFDEF SUPPORTS_CLASS_OPERATORS}
    class operator Implicit(const Value: Float): TPolarComplex;
    class operator Implicit(const Z: TPolarComplex): TRectComplex;
    class operator Implicit(const Z: TRectComplex): TPolarComplex;
    {$IFNDEF CPPBUILDER}
    // OK with Delphi, but will yield errors in .hpp files:
    class operator Explicit(const Z: TPolarComplex): TRectComplex;
    class operator Explicit(const Z: TRectComplex): TPolarComplex;
    {$ENDIF CPPBUILDER}
    class operator Equal(const Z1, Z2: TPolarComplex): Boolean;
    class operator NotEqual(const Z1, Z2: TPolarComplex): Boolean;
    class operator Add(const Z1, Z2: TPolarComplex): TRectComplex;
    class operator Subtract(const Z1, Z2: TPolarComplex): TRectComplex;
    class operator Multiply(const Z1, Z2: TPolarComplex): TPolarComplex;
    class operator Divide(const Z1, Z2: TPolarComplex): TPolarComplex;
    class operator Negative(const Z: TPolarComplex): TPolarComplex;
    class operator Positive(const Z: TPolarComplex): TPolarComplex;
    function AsString: string;
    function Conjugate: TPolarComplex;
    function IsZero: Boolean;
    function IsInfinite: Boolean;
    function Power(const Exponent: TRectComplex): TPolarComplex; overload;
    function Power(const Exponent: Float): TPolarComplex; overload;
    function Power(const Exponent: Integer): TPolarComplex; overload;
    // computes the kth nth root, k in 1..n
    function Root(const K, N: Cardinal): TPolarComplex;
    {$ENDIF SUPPORTS_CLASS_OPERATORS}
  end;

{$IFDEF DEBUG}
var
  // accumulated count of (computational costly) TRectComplex <-> TPolarComplex conversions
  ComplexTypeConversions: Cardinal;
{$ENDIF DEBUG}

// sets format string for ComplexToStr(TRectComplex) and TRectComplex.AsString
procedure SetRectComplexFormatStr(const S: string);
// sets format string for ComplexToStr(TPolarComplex) and TPolarComplex.AsString
procedure SetPolarComplexFormatStr(const S: string);

function ComplexToStr(const Z: TRectComplex): string; overload;
function ComplexToStr(const Z: TPolarComplex): string; overload;

function RectComplex(const Re: Float; const Im: Float = 0): TRectComplex; overload;
function RectComplex(const Z: TPolarComplex): TRectComplex; overload;
function PolarComplex(const Radius: Float; const Angle: Float = 0): TPolarComplex; overload;
function PolarComplex(const Z: TRectComplex): TPolarComplex; overload;

function Equal(const Z1, Z2: TRectComplex): Boolean; overload;
function Equal(const Z1, Z2: TPolarComplex): Boolean; overload;

function IsZero(const Z: TRectComplex): Boolean; overload;
function IsZero(const Z: TPolarComplex): Boolean; overload;
{$IFDEF CPU32}
function IsInfinite(const Z: TRectComplex): Boolean; overload;
function IsInfinite(const Z: TPolarComplex): Boolean; overload;
{$ENDIF CPU32}

function Norm(const Z: TRectComplex): Float; overload;
function Norm(const Z: TPolarComplex): Float; overload;
function AbsSqr(const Z: TRectComplex): Float; overload;
function AbsSqr(const Z: TPolarComplex): Float; overload;
function Conjugate(const Z: TRectComplex): TRectComplex; overload;
function Conjugate(const Z: TPolarComplex): TPolarComplex; overload;
function Inv(const Z: TRectComplex): TRectComplex; overload;
function Inv(const Z: TPolarComplex): TPolarComplex; overload;
function Neg(const Z: TRectComplex): TRectComplex; overload;
function Neg(const Z: TPolarComplex): TPolarComplex; overload;

function Sum(const Z1, Z2: TRectComplex): TRectComplex; overload;
function Sum(const Z: array of TRectComplex): TRectComplex; overload;
function Diff(const Z1, Z2: TRectComplex): TRectComplex;
function Product(const Z1, Z2: TRectComplex): TRectComplex; overload;
function Product(const Z1, Z2: TPolarComplex): TPolarComplex; overload;
function Product(const Z: array of TPolarComplex): TPolarComplex; overload;
function Quotient(const Z1, Z2: TRectComplex): TRectComplex; overload;
function Quotient(const Z1, Z2: TPolarComplex): TPolarComplex; overload;

function Ln(const Z: TPolarComplex): TRectComplex;
function Exp(const Z: TRectComplex): TPolarComplex; overload;
function Power(const Z: TPolarComplex; const Exponent: TRectComplex): TPolarComplex; overload;
function Power(const Z: TPolarComplex; const Exponent: Float): TPolarComplex; overload;
function PowerInt(const Z: TPolarComplex; const Exponent: Integer): TPolarComplex; overload;
// a complex number has n different nth roots in the complex plane
// Root() computes the kth nth root, k in 1..n
function Root(const Z: TPolarComplex; const K, N: Cardinal): TPolarComplex;

function Cos(const Z: TRectComplex): TRectComplex; overload;
function Sin(const Z: TRectComplex): TRectComplex; overload;
function Tan(const Z: TRectComplex): TRectComplex; overload;
function Cot(const Z: TRectComplex): TRectComplex; overload;
function Sec(const Z: TRectComplex): TRectComplex; overload;
function Csc(const Z: TRectComplex): TRectComplex; overload;

function CosH(const Z: TRectComplex): TRectComplex; overload;
function SinH(const Z: TRectComplex): TRectComplex; overload;
function TanH(const Z: TRectComplex): TRectComplex; overload;
function CotH(const Z: TRectComplex): TRectComplex; overload;
function SecH(const Z: TRectComplex): TRectComplex; overload;
function CscH(const Z: TRectComplex): TRectComplex; overload;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF DELPHI64_TEMPORARY}
  {$DEFINE USE_MATH_UNIT}
{$ENDIF DELPHI64_TEMPORARY}

uses
  {$IFDEF HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Winapi.Windows,
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  {$ELSE ~HAS_UNITSCOPE}
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  Windows,
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  {$ENDIF ~HAS_UNITSCOPE}
  {$IFDEF USE_MATH_UNIT}
  System.Math,
  {$ENDIF USE_MATH_UNIT}
  Jcl8087,
  JclResources,
  JclSynch;

// Note (rrossmair): Usage of the "assembler" directive seems to be an Free Pascal requirement
// (it's obsolete in Delphi since v. 2 I believe).

// Internal helper routines
// Linux: Get Global Offset Table (GOT) adress for Position Independent Code
// (PIC, used by shared objects)

{$IFDEF PIC}
function GetGOT: Pointer; export;
begin
  asm
        {$IFDEF CPU32}
        MOV Result, EBX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        XOR Result, RBX
        {$ENDIF CPU64}
  end;
end;
{$ENDIF PIC}

// to keep name space usage low
const
  JclMathSgn: function(const X: Float): Integer = Sgn;
  JclMathPower: function(const Base, Exponent: Float): Float = Power;

// to be independent from JclLogic

function Min(const X, Y: Integer): Integer;
begin
  if X < Y then
    Result := X
  else
    Result := Y;
end;

// to be independent from JCLLogic

procedure SwapOrd(var X, Y: Integer);
var
  Temp: Integer;
begin
  Temp := X;
  X := Y;
  Y := Temp;
end;

function DoubleToHex(const D: Double): string;
var
  Overlay: array [1..2] of Longint absolute D;
begin
  // Look at element 2 before element 1 because of "Little Endian" order.
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;

function HexToDouble(const Hex: string): Double;
var
  D: Double;
  Overlay: array [1..2] of Longint absolute D;
begin
  if Length(Hex) <> 16 then
    raise EJclMathError.CreateRes(@RsUnexpectedValue);
  Overlay[1] := StrToInt('$' + Copy(Hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(Hex, 1, 8));
  Result := D;
end;

// Converts degrees to radians.

{$IFDEF SUPPORTS_EXTENDED}
function DegToRad(const Value: Extended): Extended;
begin
  Result := Value * RatioDegToRad;
end;
{$ENDIF SUPPORTS_EXTENDED}

function DegToRad(const Value: Double): Double;
begin
  Result := Value * RatioDegToRad;
end;

function DegToRad(const Value: Single): Single;
begin
  Result := Value * RatioDegToRad;
end;

// Expects degrees in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 180
procedure FastDegToRad; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioDegToRad]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioDegToRad]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioDegToRad]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

// Converts radians to degrees.

{$IFDEF SUPPORTS_EXTENDED}
function RadToDeg(const Value: Extended): Extended;
begin
  Result := Value * RatioRadToDeg;
end;
{$ENDIF SUPPORTS_EXTENDED}

function RadToDeg(const Value: Double): Double;
begin
  Result := Value * RatioRadToDeg;
end;

function RadToDeg(const Value: Single): Single;
begin
  Result := Value * RatioRadToDeg;
end;

// Expects radians in ST(0), leaves degrees in ST(0)
// ST(0) := ST(0) * (180 / PI);
procedure FastRadToDeg; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioRadToDeg]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioRadToDeg]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioRadToDeg]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

// Converts grads to radians.

{$IFDEF SUPPORTS_EXTENDED}
function GradToRad(const Value: Extended): Extended;
begin
  Result := Value * RatioGradToRad;
end;
{$ENDIF SUPPORTS_EXTENDED}

function GradToRad(const Value: Double): Double;
begin
  Result := Value * RatioGradToRad;
end;

function GradToRad(const Value: Single): Single;
begin
  Result := Value * RatioGradToRad;
end;

// Expects grads in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 200
procedure FastGradToRad; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioGradToRad]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioGradToRad]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioGradToRad]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

// Converts radians to grads.

{$IFDEF SUPPORTS_EXTENDED}
function RadToGrad(const Value: Extended): Extended;
begin
  Result := Value * RatioRadToGrad;
end;
{$ENDIF SUPPORTS_EXTENDED}

function RadToGrad(const Value: Double): Double;
begin
  Result := Value * RatioRadToGrad;
end;

function RadToGrad(const Value: Single): Single;
begin
  Result := Value * RatioRadToGrad;
end;

// Expects radians in ST(0), leaves grads in ST(0)
// ST(0) := ST(0) * (200 / PI);
procedure FastRadToGrad; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioRadToGrad]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioRadToGrad]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioRadToGrad]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

// Converts degrees to grads.

{$IFDEF SUPPORTS_EXTENDED}
function DegToGrad(const Value: Extended): Extended;
begin
  Result := Value * RatioDegToGrad;
end;
{$ENDIF SUPPORTS_EXTENDED}

function DegToGrad(const Value: Double): Double;
begin
  Result := Value * RatioDegToGrad;
end;

function DegToGrad(const Value: Single): Single;
begin
  Result := Value * RatioDegToGrad;
end;

// Expects Degrees in ST(0), leaves grads in ST(0)
// ST(0) := ST(0) * (200 / 180);
procedure FastDegToGrad; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioDegToGrad]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioDegToGrad]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioDegToGrad]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

// Converts grads to degrees.

{$IFDEF SUPPORTS_EXTENDED}
function GradToDeg(const Value: Extended): Extended;
begin
  Result := Value * RatioGradToDeg;
end;
{$ENDIF SUPPORTS_EXTENDED}

function GradToDeg(const Value: Double): Double;
begin
  Result := Value * RatioGradToDeg;
end;

function GradToDeg(const Value: Single): Single;
begin
  Result := Value * RatioGradToDeg;
end;

// Expects grads in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 200
procedure FastGradToDeg; assembler;
asm
        {$IFDEF PIC}
        CALL    GetGOT
        {$IFDEF CPU32}
        FLD     [EAX][RatioGradToDeg]
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        FLD     [RAX][RatioGradToDeg]
        {$ENDIF CPU64}
        {$ELSE ~PIC}
        FLD     [RatioGradToDeg]
        {$ENDIF ~PIC}
        FMULP
        FWAIT
end;

procedure DomainCheck(Err: Boolean);
begin
  if Err then
    raise EJclMathError.CreateRes(@RsMathDomainError);
end;

//=== Logarithmic ============================================================

function LogBase10(X: Float): Float;
begin
  DomainCheck(X <= 0.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Log10(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLDLG2
          FLD     X
          FYL2X
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function LogBase2(X: Float): Float;
begin
  DomainCheck(X <= 0.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Log2(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD1
          FLD     X
          FYL2X
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function LogBaseN(Base, X: Float): Float;
begin
  DomainCheck((X <= 0.0) or (Base <= 0.0) or (Base = 1.0));
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.LogN(Base, X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD1
          FLD     X
          FYL2X
          FLD1
          FLD     Base
          FYL2X
          FDIVP   ST(1), ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

//=== Transcendental =========================================================

function ArcCos(X: Float): Float;
begin
  DomainCheck(Abs(X) > 1.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcCos(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST(0)
          FLD1
          FSUBRP  ST(1), ST(0)
          FSQRT
          FXCH
          FPATAN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function ArcCot(X: Float): Float;
begin
  DomainCheck(X = 0);
  Result := ArcTan(1 / X);
end;

function ArcCsc(X: Float): Float;
begin
  Result := ArcSec(X / Sqrt(X * X -1));
end;

function ArcSec(X: Float): Float;
begin
  DomainCheck((X > -1) and (X < 1));
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcSec(X);
  {$ELSE ~USE_MATH_UNIT}
  // FArcTan(Sqrt(X*X - 1));
  asm
          FLD1
          FLD     X
          FLD     ST(0)
          FMULP
          FSUBRP  ST(1), ST(0)
          FSQRT
          FLD1
          FPATAN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function ArcSin(X: Float): Float;
begin
  DomainCheck(Abs(X) > 1.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcSin(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST(0)
          FLD1
          FSUBRP  ST(1), ST(0)
          FSQRT
          FPATAN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

{$IFDEF CPU32}
function ArcTan(X: Float): Float; assembler;
asm
          FLD     X
          FLD1
          FPATAN
          FWAIT
end;
{$ENDIF CPU32}

{$IFDEF CPU64}
function ArcTan(X: Float): Float;
begin
  {$IFDEF USE_MATH_UNIT}
  System.Error(rePlatformNotImplemented);
  Result := NaN; 
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FLD1
          FPATAN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF CPU64}

{$IFDEF CPU32}
function ArcTan2(Y, X: Float): Float; assembler;
asm
          FLD     Y
          FLD     X
          FPATAN
          FWAIT
end;
{$ENDIF CPU32}

{$IFDEF CPU64}
function ArcTan2(Y, X: Float): Float;
begin
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcTan2(Y, X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     Y
          FLD     X
          FPATAN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF CPU64}

function Cos(X: Float): Float;
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  System.Error(rePlatformNotImplemented);
  Result := NaN;
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FCOS
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function Cot(X: Float): Float;
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Cot(X);
  {$ELSE ~USE_MATH_UNIT}
  { TODO : Cot = 1 / Tan -> Tan(X) <> 0.0 }
  asm
          FLD     X
          FPTAN
          FDIVRP  ST(1), ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function Coversine(X: Float): Float;
begin
  Result := 1 - JclMath.Sin(X);
end;

function Csc(X: Float): Float;
var
  Y: Float;
begin
  DomainCheck(Abs(X) > MaxAngle);

  Y := JclMath.Sin(X);
  DomainCheck(Y = 0.0);
  Result := 1.0 / Y;
end;

function Exsecans(X: Float): Float;
begin
  Result := JclMath.Sec(X) - 1;
end;

function Haversine(X: Float): Float;
begin
  Result := 0.5 * (1 - JclMath.Cos(X));
end;

function Sec(X: Float): Float;
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Sec(X);
  {$ELSE ~USE_MATH_UNIT}
  { TODO : Sec = 1 / Cos -> Cos(X) <> 0! }
  asm
          FLD     X
          FCOS
          FLD1
          FDIVRP  ST(1), ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function Sin(X: Float): Float;
begin
  {$IFNDEF MATH_EXT_SPECIALVALUES}
  DomainCheck(Abs(X) > MaxAngle);
  {$ENDIF ~MATH_EXT_SPECIALVALUES}
  {$IFDEF USE_MATH_UNIT}
  System.Error(rePlatformNotImplemented);
  Result := NaN;
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FSIN
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

{$IFDEF SUPPORTS_EXTENDED}
procedure SinCos(X: Extended; out Sin, Cos: Extended);
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  System.Math.SinCos(X, Sin, Cos);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          {$IFDEF CPU32}
          MOV     EDX, Cos
          MOV     EAX, Sin
          FSINCOS
          FSTP    Extended PTR [EDX]
          FSTP    Extended PTR [EAX]
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          MOV     RDX, Cos
          MOV     RAX, Sin
          FSINCOS
          FSTP    Extended PTR [RDX]
          FSTP    Extended PTR [RAX]
          {$ENDIF CPU64}
          FWAIT
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF SUPPORTS_EXTENDED}

procedure SinCos(X: Double; out Sin, Cos: Double);
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  System.Math.SinCos(X, Sin, Cos);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          {$IFDEF CPU32}
          MOV     EDX, Cos
          MOV     EAX, Sin
          FSINCOS
          FSTP    Double PTR [EDX]
          FSTP    Double PTR [EAX]
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          MOV     RDX, Cos
          MOV     RAX, Sin
          FSINCOS
          FSTP    Double PTR [RDX]
          FSTP    Double PTR [RAX]
          {$ENDIF CPU64}
          FWAIT
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

procedure SinCos(X: Single; out Sin, Cos: Single);
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  System.Math.SinCos(X, Sin, Cos);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          {$IFDEF CPU32}
          MOV     EDX, Cos
          MOV     EAX, Sin
          FSINCOS
          FSTP    Single PTR [EDX]
          FSTP    Single PTR [EAX]
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          MOV     RDX, Cos
          MOV     RAX, Sin
          FSINCOS
          FSTP    Single PTR [RDX]
          FSTP    Single PTR [RAX]
          {$ENDIF CPU64}
          FWAIT
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function Tan(X: Float): Float;
begin
  DomainCheck(Abs(X) > MaxAngle);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Tan(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLD     X
          FPTAN
          FSTP    ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function Versine(X: Float): Float;
begin
  Result := 1 - JclMath.Cos(X);
end;

//=== Hyperbolic =============================================================

function ArcCosH(X: Float): Float;
begin
  DomainCheck(X < 1.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcCosh(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST(0)
          FLD1
          FSUBP   ST(1), ST(0)
          FSQRT
          FADDP   ST(1), ST(0)
          FYL2X
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function ArcCotH(X: Float): Float;
begin
  DomainCheck(Abs(X) = 1.0);
  Result := 0.5 * System.Ln((X + 1.0) / (X - 1.0));
end;

function ArcCscH(X: Float): Float;
begin
  DomainCheck(X = 0);
  Result := System.Ln((Sgn(X) * Sqrt(Sqr(X) + 1.0) + 1.0) / X);
end;

function ArcSecH(X: Float): Float;
begin
  DomainCheck(Abs(X) > 1.0);
  Result := System.Ln((Sqrt(1.0 - Sqr(X)) + 1.0) / X);
end;

{$IFDEF CPU32}
function ArcSinH(X: Float): Float; assembler;
asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST(0)
          FLD1
          FADDP   ST(1), ST(0)
          FSQRT
          FADDP   ST(1), ST(0)
          FYL2X
end;
{$ENDIF CPU32}

{$IFDEF CPU64}
function ArcSinH(X: Float): Float;
begin
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcSinh(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST(0)
          FLD1
          FADDP   ST(1), ST(0)
          FSQRT
          FADDP   ST(1), ST(0)
          FYL2X
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF CPU64}

function ArcTanH(X: Float): Float;
begin
  DomainCheck(Abs(X) >= 1.0);
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.ArcTanh(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FLD1
          FADDP   ST(1), ST(0)
          FXCH
          FLD1
          FSUBRP  ST(1), ST(0)
          FDIVP   ST(1), ST(0)
          FSQRT
          FYL2X
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;

function CosH(X: Float): Float;
{$IFDEF PUREPASCAL}
begin
  Result := 0.5 * (Exp(X) + Exp(-X));
end;
{$ELSE ~PUREPASCAL}
const
  RoundDown: Word = $177F;
  OneHalf: Float = 0.5;
{$IFNDEF USE_MATH_UNIT}
var
  ControlWW: Word;
{$ENDIF ~USE_MATH_UNIT}
begin
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Cosh(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          {$IFDEF PIC}
          CALL    GetGOT
          {$ENDIF PIC}
          FLD     X    { TODO : Legal values for X? }
          FLDL2E
          FMULP   ST(1), ST(0)
          FSTCW   ControlWW
          {$IFDEF PIC}
          {$IFDEF CPU32}
          FLDCW   [EAX].RoundDown
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          FLDCW   [RAX].RoundDown
          {$ENDIF CPU64}
          {$ELSE ~PIC}
          FLDCW   RoundDown
          {$ENDIF ~PIC}
          FLD     ST(0)
          FRNDINT
          FLDCW   ControlWW
          FXCH
          FSUB    ST(0), ST(1)
          F2XM1
          FLD1
          FADDP   ST(1), ST(0)
          FSCALE
          FST     ST(1)
          FLD1
          FDIVRP  ST(1), ST(0)
          FADDP   ST(1), ST(0)
          {$IFDEF PIC}
          {$IFDEF CPU32}
          FLD     [EAX].OneHalf
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          FLD     [RAX].OneHalf
          {$ENDIF CPU64}
          {$ELSE ~PIC}
          FLD     OneHalf
          {$ENDIF ~PIC}
          FMULP   ST(1), ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF ~PUREPASCAL}

function CotH(X: Float): Float;
begin
  Result := 1 / JclMath.TanH(X);
end;

function CscH(X: Float): Float;
begin
  Result := JclMath.Exp(X) - JclMath.Exp(-X);
  DomainCheck(Result = 0.0);
  Result := 2.0 / Result;
end;

function SecH(X: Float): Float;
begin
  Result := JclMath.Exp(X) + JclMath.Exp(-X);
  DomainCheck(Result = 0.0);
  Result := 2.0 / Result;
end;

function SinH(X: Float): Float;
{$IFDEF PUREPASCAL}
begin
  Result := 0.5 * (JclMath.Exp(X) - JclMath.Exp(-X));
end;
{$ELSE ~PUREPASCAL}
const
  RoundDown: Word = $177F;
  OneHalf: Float = 0.5;
{$IFNDEF USE_MATH_UNIT}
var
  ControlWW: Word;
{$ENDIF ~USE_MATH_UNIT}
begin
  {$IFDEF USE_MATH_UNIT}
  Result := System.Math.Sinh(X);
  {$ELSE ~USE_MATH_UNIT}
  asm
          {$IFDEF PIC}
          CALL    GetGOT
          {$ENDIF PIC}
          FLD     X  { TODO : Legal values for X? }
          FLDL2E
          FMULP   ST(1), ST(0)
          FSTCW   ControlWW
          {$IFDEF PIC}
          {$IFDEF CPU32}
          FLDCW   [EAX].RoundDown
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          FLDCW   [RAX].RoundDown
          {$ENDIF CPU64}
          {$ELSE ~PIC}
          FLDCW   RoundDown
          {$ENDIF ~PIC}
          FLD     ST(0)
          FRNDINT
          FLDCW   ControlWW
          FXCH
          FSUB    ST(0), ST(1)
          F2XM1
          FLD1
          FADDP   ST(1), ST(0)
          FSCALE
          FST     ST(1)
          FLD1
          FDIVRP  ST(1), ST(0)
          FSUBP   ST(1), ST(0)
          {$IFDEF PIC}
          {$IFDEF CPU32}
          FLD     [EAX].OneHalf
          {$ENDIF CPU32}
          {$IFDEF CPU64}
          FLD     [RAX].OneHalf
          {$ENDIF CPU64}
          {$ELSE ~PIC}
          FLD     OneHalf
          {$ENDIF ~PIC}
          FMULP   ST(1), ST(0)
          FWAIT
          FSTP    Result
  end;
  {$ENDIF ~USE_MATH_UNIT}
end;
{$ENDIF ~PUREPASCAL}

function TanH(X: Float): Float;
begin
  if X > MaxTanH then
    Result := 1.0
  else
  begin
    if X < -MaxTanH then
      Result := -1.0
    else
    begin
      Result := JclMath.Exp(X);
      Result := Result * Result;
      Result := (Result - 1.0) / (Result + 1.0);
    end;
  end;
end;

//=== Coordinate conversion ==================================================

function DegMinSecToFloat(const Degs, Mins, Secs: Float): Float; // obsolete
begin
  Result := Degs + (Mins / 60.0) + (Secs / 3600.0);
end;

procedure FloatToDegMinSec(const X: Float; var Degs, Mins, Secs: Float); // obsolete
var
  Y: Float;
begin
  Degs := System.Int(X);
  Y := Frac(X) * 60;
  Mins := System.Int(Y);
  Secs := Frac(Y) * 60;
end;

//=== Exponential ============================================================

function Exp(const X: Float): Float;
begin
  {$IFDEF MATH_EXT_EXTREMEVALUES}
  {$IFDEF CPU32}
  if IsSpecialValue(X) then
  begin
    if IsNaN(X) or (X = Infinity) then
      Result := X
    else
      Result := 0;
    Exit;
  end;
  {$ENDIF CPU32}
  {$ENDIF MATH_EXT_EXTREMEVALUES}

  Result := System.Exp(X);
end;

function Power(const Base, Exponent: Float): Float;
var
  IsAnInteger, IsOdd: Boolean;
begin
  if (Exponent = 0.0) or (Base = 1.0) then
    Result := 1
  else
  if Base = 0.0 then
  begin
    if Exponent > 0.0 then
      Result := 0.0
    else
      {$IFDEF MATH_EXT_EXTREMEVALUES}
      Result := Infinity;
      {$ELSE ~MATH_EXT_EXTREMEVALUES}
      raise EJclMathError.CreateRes(@RsPowerInfinite);
      {$ENDIF ~MATH_EXT_EXTREMEVALUES}
  end
  else
  if Base > 0.0 then
    Result := JclMath.Exp(Exponent * System.Ln(Base))
  else
  begin
    IsAnInteger := (Frac(Exponent) = 0.0);
    if IsAnInteger then
    begin
      Result := JclMath.Exp(Exponent * System.Ln(Abs(Base)));
      IsOdd := Abs(Round(ModFloat(Exponent, 2))) = 1;
      if IsOdd then
        Result := -Result;
    end
    else
      raise EJclMathError.CreateRes(@RsPowerComplex);
  end;
end;

function PowerInt(const X: Float; N: Integer): Float;
var
  M: Integer;
  T: Float;
  Xc: Float;
begin
  if X = 0.0 then
  begin
    if N = 0 then
      Result := 1.0
    else
    if N > 0 then
      Result := 0.0
    else
      Result := MaxFloatingPoint;
    Exit;
  end;

  if N = 0 then
  begin
    Result := 1.0;
    Exit;
  end;

  // Legendre's algorithm for minimizing the number of multiplications
  T := 1.0;
  M := Abs(N);
  Xc := X;
  repeat
    if Odd(M) then
    begin
      Dec(M);
      T := T * Xc;
    end
    else
    begin
      M := M div 2;
      Xc := Sqr(Xc);
    end;
  until M = 0;

  if N > 0 then
    Result := T
  else
    Result := 1.0 / T;
end;

function TenToY(const Y: Float): Float;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := JclMath.Exp(Y * Ln10);
end;

function TruncPower(const Base, Exponent: Float): Float;
begin
  if Base > 0 then
    Result := JclMath.Power(Base, Exponent)
  else
    Result := 0;
end;

function TwoToY(const Y: Float): Float;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := JclMath.Exp(Y * Ln2);
end;

//=== Floating point support routines ========================================

function IsFloatZero(const X: Float): Boolean;
begin
  Result := Abs(X) < PrecisionTolerance;
end;

function FloatsEqual(const X, Y: Float): Boolean;
begin
  try
    if Y = 0 then
      // catch exact equality
      Result := (X = Y) or (Abs(1 - Y/X ) <= PrecisionTolerance)
    else
      // catch exact equality
      Result := (X = Y) or (Abs(1 - X/Y ) <= PrecisionTolerance);
  except
    Result := False;  // catch real rare overflow e.g.  1.0e3000/1.0e-3000
  end
end;

function MaxFloat(const X, Y: Float): Float;
begin
  if X < Y then
    Result := Y
  else
    Result := X;
end;

function MinFloat(const X, Y: Float): Float;
begin
  if X > Y then
    Result := Y
  else
    Result := X;
end;

function ModFloat(const X, Y: Float): Float;
var
  Z: Float;
begin
  Result := X / Y;
  Z := System.Int(Result);
  if Frac(Result) < 0.0 then
    Z := Z - 1.0;
  Result := X - Y * Z;
end;

function RemainderFloat(const X, Y: Float): Float;
begin
  Result := X - System.Int(X / Y) * Y;
end;

procedure SwapFloats(var X, Y: Float);
var
  T: Float;
begin
  T := X;
  X := Y;
  Y := T;
end;

procedure CalcMachineEpsSingle;
var
  One: Single;
  T: Single;
begin
  One := 1.0;
  EpsSingle := One;
  repeat
    EpsSingle := 0.5 * EpsSingle;
    T := One + EpsSingle;
  until One = T;
  EpsSingle := 2.0 * EpsSingle;
  ThreeEpsSingle := 3.0 * EpsSingle;
end;

procedure CalcMachineEpsDouble;
var
  One: Double;
  T: Double;
begin
  One := 1.0;
  EpsDouble := One;
  repeat
    EpsDouble := 0.5 * EpsDouble;
    T := One + EpsDouble;
  until One = T;
  EpsDouble := 2.0 * EpsDouble;
  ThreeEpsDouble := 3.0 * EpsDouble;
end;

{$IFDEF SUPPORTS_EXTENDED}
procedure CalcMachineEpsExtended;
var
  One: Extended;
  T: Extended;
begin
  One := 1.0;
  EpsExtended := One;
  repeat
    EpsExtended := 0.5 * EpsExtended;
    T := One + EpsExtended;
  until One = T;
  EpsExtended := 2.0 * EpsExtended;
  ThreeEpsExtended := 3.0 * EpsExtended;
end;
{$ENDIF SUPPORTS_EXTENDED}

procedure CalcMachineEps;
begin
  {$IFDEF MATH_EXTENDED_PRECISION}
  CalcMachineEpsExtended;
  Epsilon := EpsExtended;
  ThreeEpsilon := ThreeEpsExtended;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  CalcMachineEpsDouble;
  Epsilon := EpsDouble;
  ThreeEpsilon := ThreeEpsDouble;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  CalcMachineEpsSingle;
  Epsilon := EpsSingle;
  ThreeEpsilon := ThreeEpsSingle;
  {$ENDIF MATH_SINGLE_PRECISION}
end;

procedure SetPrecisionToleranceToEpsilon;
begin
  CalcMachineEps;
  PrecisionTolerance := Epsilon;
end;

function SetPrecisionTolerance(NewTolerance: Float): Float;
begin
  Result := PrecisionTolerance;
  PrecisionTolerance := NewTolerance;
end;

//=== Miscellaneous ==========================================================

function Ceiling(const X: Float): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

function CommercialRound(const X: Float): Int64;
begin
  Result := Trunc(X);
  if Frac(Abs(X)) >= 0.5 then
    Result := Result + Sgn(X);
end;

const
  PreCompFactsCount = 33; // all factorials that fit in a Single

  {$IFDEF MATH_SINGLE_PRECISION}
  PreCompFacts: array [0..PreCompFactsCount] of Float =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178289152.0,
    1307674279936.0,
    20922788478976.0,
    355687414628352.0,
    6.4023735304192E15,
    1.21645096004223E17,
    2.43290202316367E18,
    5.10909408371697E19,
    1.12400072480601E21,
    2.58520174445945E22,
    6.20448454699065E23,
    1.55112110792462E25,
    4.03291499589617E26,
    1.08888704151327E28,
    3.04888371623715E29,
    8.8417630793192E30,
    2.65252889961724E32,
    8.22283968552752E33,
    2.63130869936881E35,
    8.68331850984666E36
   );
  {$ENDIF MATH_SINGLE_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  PreCompFacts: array [0..PreCompFactsCount] of Float =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6.402373705728E15,
    1.21645100408832E17,
    2.43290200817664E18,
    5.10909421717094E19,
    1.12400072777761E21,
    2.5852016738885E22,
    6.20448401733239E23,
    1.5511210043331E25,
    4.03291461126606E26,
    1.08888694504184E28,
    3.04888344611714E29,
    8.8417619937397E30,
    2.65252859812191E32,
    8.22283865417792E33,
    2.63130836933694E35,
    8.68331761881189E36
   );
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_EXTENDED_PRECISION}
  PreCompFacts: array [0..PreCompFactsCount] of Float =
   (
    1.0,
    1.0,
    2.0,
    6.0,
    24.0,
    120.0,
    720.0,
    5040.0,
    40320.0,
    362880.0,
    3628800.0,
    39916800.0,
    479001600.0,
    6227020800.0,
    87178291200.0,
    1307674368000.0,
    20922789888000.0,
    355687428096000.0,
    6.402373705728E15,
    1.21645100408832E17,
    2.43290200817664E18,
    5.10909421717094E19,
    1.12400072777761E21,
    2.5852016738885E22,
    6.20448401733239E23,
    1.5511210043331E25,
    4.03291461126606E26,
    1.08888694504184E28,
    3.04888344611714E29,
    8.8417619937397E30,
    2.65252859812191E32,
    8.22283865417792E33,
    2.63130836933694E35,
    8.68331761881189E36
   );
  {$ENDIF MATH_EXTENDED_PRECISION}

function Factorial(const N: Integer): Float;
var
  I: Integer;
begin
  if (N < 0) or (N > MaxFactorial) then
    Result := 0.0
  else
  begin
    if N <= PreCompFactsCount then
      Result := PreCompFacts[N]
    else
    begin  { TODO : Change following by: Gamma(N + 1) }
      Result := PreCompFacts[PreCompFactsCount];
      for I := PreCompFactsCount + 1 to N do
        Result := Result * I;
    end;
  end;
end;

function Floor(const X: Float): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

function GCD(X, Y: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
begin
  Result := X;
  while Y <> 0 do
  begin
    X := Result;
    Result := Y;
    Y := X mod Y;
  end;
end;
{$ELSE ~PUREPASCAL}
assembler;
{ Euclid's algorithm }
asm
   // 32 --> EAX X
   //        EDX Y
   //    <-- EAX Result
   // 64 --> ECX X
   //        EDX Y
   //    <-- EAX Result
        {$IFDEF CPU64}
        MOV     EAX, ECX
        {$ENDIF CPU64}
        JMP     @01      // We start with EAX <- X, EDX <- Y, and check to see if Y=0
@00:
        MOV     ECX, EDX // ECX <- EDX prepare for division
        XOR     EDX, EDX // clear EDX for Division
        DIV     ECX      // EAX <- EDX:EAX div ECX, EDX <- EDX:EAX mod ECX
        MOV     EAX, ECX // EAX <- ECX, and repeat if EDX <> 0
@01:
        AND     EDX, EDX // test to see if EDX is zero, without changing EDX
        JNZ     @00      // when EDX is zero EAX has the Result
end;
{$ENDIF ~PUREPASCAL}

function ISqrt(const I: Smallint): Smallint;
{$IFDEF PUREPASCAL}
var
  b, d: Smallint;
begin
  Result := -1;
  d := -1;
  b := 0;
  repeat
    Inc(Result);
    Inc(d, 2);
    b := b + d;
  until b > I;
end;
{$ELSE ~PUREPASCAL}
assembler;
asm
  // 32 --> AX I
  //    <-- AX Result
  // 64 --> CX I
  //    <-- AX Result
        {$IFDEF CPU32}
        PUSH    EBX
        MOV     CX, AX  // load argument
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        PUSH    RBX
        {$ENDIF CPU64}

        MOV     AX, -1  // init Result
        CWD             // init odd numbers to -1
        XOR     BX, BX  // init perfect squares to 0
@LOOP:
        INC     AX      // increment Result
        INC     DX      // compute
        INC     DX      // next odd number
        ADD     BX, DX  // next perfect square
        CMP     BX, CX  // perfect square > argument ?
        JBE     @LOOP   // until square greater than argument

        {$IFDEF CPU32}
        POP     EBX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        POP     RBX
        {$ENDIF CPU64}
end;
{$ENDIF ~PUREPASCAL}

function LCM(const X, Y: Cardinal): Cardinal;
var
  E: Cardinal;
begin
  E := GCD(X, Y);
  if E > 0 then
    Result := (X div E) * Y
  else
    Result := 0;
end;

function NormalizeAngle(const Angle: Float): Float;
begin
  Result := Angle;
  if Result = 0 then
    Exit;

  {$IFDEF MATH_ANGLE_DEGREES}
  Result := DegToRad(Result);
  {$ENDIF MATH_ANGLE_DEGREES}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := GradToRad(Result);
  {$ENDIF MATH_ANGLE_GRADS}

  if (Result < -Pi) or (Result >= Pi) then
  begin
    Result := Frac(Result * Inv2Pi);
    if Result < 0 then
      Result := Result + 1.0
    else
      Result := Result - 1.0;
    Result := Result * TwoPi;
  end;

  {$IFDEF MATH_ANGLE_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF MATH_ANGLE_DEGREES}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF MATH_ANGLE_GRADS}
end;

function Pythagoras(const X, Y: Float): Float;
var
  AbsX, AbsY: Float;
begin
  AbsX := Abs(X);
  AbsY := Abs(Y);

  if AbsX > AbsY then
    Result := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
  else
  if AbsY = 0.0 then
    Result := 0.0
  else
    Result := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
end;

function Sgn(const X: Float): Integer;
begin
  if X > 0.0 then
    Result := 1
  else
  if X < 0.0 then
    Result := -1
  else
    Result := 0;
end;

function Signe(const X, Y: Float): Float;
begin
  if X > 0.0 then
  begin
    if Y > 0.0 then
      Result := X
    else
      Result := -X;
  end
  else
  begin
    if Y < 0.0 then
      Result := X
    else
      Result := -X;
  end;
end;

function Ackermann(const A, B: Integer): Integer;
begin
  if A = 0 then
  begin
    Result := B + 1;
    Exit;
  end;

  if B = 0 then
    Result := Ackermann(A - 1, 1)
  else
    Result := Ackermann(A - 1, Ackermann(A, B - 1));
end;

function Fibonacci(const N: Integer): Integer;
var
  I: Integer;
  P1, P2: Integer;
begin
  Assert(N >= 0);
  Result := 0;
  P1 := 1;
  P2 := 1;

  if (N = 1) or (N = 2) then
    Result := 1
  else
    for I := 3 to N do
    begin
      Result := P1 + P2;
      P1 := P2;
      P2 := Result;
    end;
end;

//=== { TJclFlatSet } ========================================================

constructor TJclFlatSet.Create;
begin
  inherited Create;
  FBits := TBits.Create;
end;

destructor TJclFlatSet.Destroy;
begin
  FBits.Free;
  FBits := nil;
  inherited Destroy;
end;

procedure TJclFlatSet.Clear;
begin
  FBits.Size := 0;
end;

procedure TJclFlatSet.Invert;
var
  I: Integer;
begin
  for I := 0 to FBits.Size - 1 do
    FBits[I] := not FBits[I];
end;

procedure TJclFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I: Integer;
begin
  for I := High downto Low do
    FBits[I] := Value;
end;

function TJclFlatSet.GetBit(const Idx: Integer): Boolean;
begin
  Result := FBits[Idx];
end;

function TJclFlatSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I: Integer;
begin
  if not Value and (High >= FBits.Size) then
  begin
    Result := False;
    Exit;
  end;
  for I := Low to Min(High, FBits.Size - 1) do
    if FBits[I] <> Value then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

procedure TJclFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
begin
  FBits[Idx] := Value;
end;

//== { TJclSparseFlatSet } ===================================================

destructor TJclSparseFlatSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclSparseFlatSet.Clear;
var
  F: Integer;
begin
  if FSetList <> nil then
  begin
    for F := 0 to FSetListEntries - 1 do
      if FSetList^[F] <> nil then
        Dispose(PDelphiSet(FSetList^[F]));
    FreeMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FSetList := nil;
    FSetListEntries := 0;
  end;
end;

procedure TJclSparseFlatSet.Invert;
var
  F: Integer;
begin
  for F := 0 to FSetListEntries - 1 do
    if FSetList^[F] <> nil then
      PDelphiSet(FSetList^[F])^ := CompleteDelphiSet - PDelphiSet(FSetList^[F])^;
end;

function TJclSparseFlatSet.GetBit(const Idx: Integer): Boolean;
var
  SetIdx: Integer;
begin
  SetIdx := Idx shr 8;
  Result := (SetIdx < FSetListEntries) and (FSetList^[SetIdx] <> nil) and
    (Byte(Idx and $FF) in PDelphiSet(FSetList^[SetIdx])^);
end;

procedure TJclSparseFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
var
  I, SetIdx: Integer;
  S: PDelphiSet;
begin
  SetIdx := Idx shr 8;
  if SetIdx >= FSetListEntries then
    if Value then
    begin
      I := FSetListEntries;
      FSetListEntries := SetIdx + 1;
      ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
      FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
    end
    else
      Exit;
  S := FSetList^[SetIdx];
  if S = nil then
    if Value then
    begin
      New(S);
      S^ := [];
      FSetList^[SetIdx] := S;
    end
    else
      Exit;
  Include(S^, Byte(Idx and $FF));
end;

procedure TJclSparseFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I, LowSet, HighSet: Integer;

  procedure SetValue(const S: TDelphiSet; const SetIdx: Integer);
  var
    D: PDelphiSet;
  begin
    D := FSetList^[SetIdx];
    if D = nil then
    begin
      if Value then
      begin
        New(D);
        D^ := S;
        FSetList^[SetIdx] := D;
      end;
    end
    else
    if Value then
      D^ := D^ + S
    else
      D^ := D^ - S;
  end;

begin
  LowSet := Low shr 8;
  HighSet := High shr 8;
  if HighSet >= FSetListEntries then
  begin
    I := FSetListEntries;
    FSetListEntries := HighSet + 1;
    ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
  end;
  if LowSet = HighSet then
    SetValue([Byte(Low and $FF)..Byte(High and $FF)], LowSet)
  else
  begin
    SetValue([Byte(Low and $FF)..$FF], LowSet);
    SetValue([0..Byte(High and $FF)], HighSet);
    for I := LowSet + 1 to HighSet - 1 do
      SetValue(CompleteDelphiSet, I);
  end;
end;

function TJclSparseFlatSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I: Integer;
begin
  if not Value and (High >= FSetListEntries) then
  begin
    Result := False;
    Exit;
  end;
  for I := Low to Min(High, FSetListEntries) do
    if GetBit(I) <> Value then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//=== Ranges =================================================================

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;
begin
  Result := AValue;
  Assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Int64): Int64;
begin
  Result := AValue;
  Assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Double): Double;
begin
  Result := AValue;
  Assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

//=== Prime numbers ==========================================================

const
  PrimeCacheLimit = 65537; // 4K lookup table. Note: Sqr(65537) > MaxLongint

var
  PrimeSet: TJclFlatSet = nil;

procedure InitPrimeSet;
var
  I, J, MaxI, MaxJ : Integer;
begin
  PrimeSet := TJclFlatSet.Create;
  PrimeSet.SetRange(1, PrimeCacheLimit div 2, True);
  PrimeSet.SetBit(0, False);               // 1 is no prime
  MaxI := Trunc(Sqrt(PrimeCacheLimit));
  I := 3;
  repeat
    if PrimeSet.GetBit(I div 2) then
    begin
      MaxJ := PrimeCacheLimit div I;
      J := 3;
      repeat
        PrimeSet.SetBit((I*J) div 2, False);
        Inc(J,2);
      until J > MaxJ;
    end;
    Inc(I, 2);
  until I > MaxI;
end;

function IsPrimeTD(N: Cardinal): Boolean;
{ Trial Division Algorithm }
var
  I, Max: Cardinal;
  R: Extended;
begin
  if N = 2 then
  begin
    Result := True;
    Exit;
  end;
  if (N and 1) = 0 then   //Zero or even
  begin
    Result := False;
    Exit;
  end;
  if PrimeSet = nil then // initialize look-up table
     InitPrimeSet;
  if N <= PrimeCacheLimit then // do look-up
    Result := PrimeSet.GetBit(N div 2)
  else
  begin // calculate
    R := N;
    Max := Round(Sqrt (R));
    if Max > PrimeCacheLimit then
    begin
      raise EJclMathError.CreateRes(@RsUnexpectedValue);
      Exit;
    end;
    I := 1;
    repeat
      Inc(I,2);
      if PrimeSet.GetBit(I div 2) then
        if N mod I = 0 then
        begin
          Result := False;
          Exit;
        end;
    until I >= Max;
    Result := True;
  end;
end;

{$IFDEF CPU32}
// OF: need a complete rewrite for CPU64
// OF: why is there less pop than push?

{ Rabin-Miller Strong Primality Test }

function IsPrimeRM(N: Cardinal): Boolean; assembler;
asm
        // 32 --> EAX N
        //    <-- AL  Result
        TEST  EAX,1            // Odd(N) ??
        JNZ   @@1
        CMP   EAX,2            // N == 2 ??
        SETE  AL
        RET
@@1:    CMP   EAX,73
        JBE   @@C
        PUSH  ESI
        PUSH  EDI
        PUSH  EBX
        PUSH  EBP
        PUSH  EAX              // save N as Param for @@5
        MOV   EBP, EAX
        DEC   EBP              // M == N -1, Exponent
        MOV   ECX,32           // calc remaining Bits of M and shift M'
        MOV   ESI,EBP
@@2:    DEC   ECX
        SHL   ESI,1
        JNC   @@2
        PUSH  ECX              // save Bits as Param for @@5
        PUSH  ESI              // save M' as Param for @@5
        CMP   EAX,08A8D7Fh     // N >= 9080191 ??
        JAE   @@3
// now if (N < 9080191) and SPP(31, N) and SPP(73, N) then N is prime
        MOV   EAX,31
        CALL  @@5
        JC    @@4
        MOV   EAX,73
        PUSH  OFFSET @@4
        JMP   @@5
// now if (N < 4759123141) and SPP(2, N) and SPP(7, N) and SPP(61, N) then N is prime
@@3:    MOV   EAX,2
        CALL  @@5
        JC    @@4
        MOV   EAX,7
        CALL  @@5
        JC    @@4
        MOV   EAX,61
        CALL  @@5
@@4:    SETNC AL
        ADD   ESP,4 * 3
        POP   EBP
        POP   EBX
        POP   EDI
        POP   ESI
        RET
// do a Strong Pseudo Prime Test
@@5:
        MOV   EBX,[ESP + 12]   // N on stack
        MOV   ECX,[ESP +  8]   // remaining Bits
        MOV   ESI,[ESP +  4]   // M'
        MOV   EDI,EAX          // T = b, temp. Base
@@6:    DEC   ECX
        MUL   EAX
        DIV   EBX
        MOV   EAX,EDX
        SHL   ESI,1
        JNC   @@7
        MUL   EDI
        DIV   EBX
        AND   ESI,ESI
        MOV   EAX,EDX
@@7:    JNZ   @@6
        CMP   EAX,1            // b^((N -1)(2^s)) mod N ==  1 mod N ??
        JE    @@A
@@8:    CMP   EAX,EBP          // b^((N -1)(2^s)) mod N == -1 mod N ??
        JE    @@A
        DEC   ECX              // second part to 2^s
        JNG   @@9
        MUL   EAX
        DIV   EBX
        CMP   EDX,1
        MOV   EAX,EDX
        JNE   @@8
@@9:    STC
@@A:    RET
@@B:    DB    3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73
@@C:    MOV   ECX,19
        MOV   EDX,OFFSET @@B
@@D:    CMP   AL,[EDX + ECX]
        JE    @@E
        DEC   ECX
        JNL   @@D
@@E:    SETE  AL
end;
{$ENDIF CPU32}

function PrimeFactors(N: Cardinal): TDynCardinalArray;
var
  I, L, Max: Cardinal;
  R: Extended;
begin
  SetLength(Result, 0);
  if N <= 1 then
    Exit
  else
  begin
    if PrimeSet = nil then
      InitPrimeSet;
    L := 0;
    R := N;
    R := Sqrt(R);
    Max := Round(R);         // only one factor can be > Sqrt (N)
    if N mod 2 = 0 then      // test even at first
    begin                    // 2 is a prime factor
      Inc(L);
      SetLength(Result, L);
      Result[L - 1] := 2;
      repeat
        N := N div 2;
        if N = 1 then        // no more factors
          Exit;
      until N mod 2 <> 0;
    end;
    I := 3;                  // test all odd factors
    repeat
      if (N mod I = 0) and IsPrime(I)  then
      begin                  // I is a prime factor
        Inc(L);
        SetLength(Result, L);
        Result[L - 1] := I;
        repeat
          N := N div I;
          if N = 1 then      // no more factors
            Exit;
        until N mod I <> 0;
      end;
      Inc(I, 2);
    until I > Max;
    Inc(L);                  // final factor (> Sqrt(N))
    SetLength(Result, L);
    Result[L - 1] := N;
  end;
end;

function IsPrimeFactor(const F, N: Cardinal): Boolean;
begin
  Result := (N mod F = 0) and IsPrime(F);
end;

function IsRelativePrime(const X, Y: Cardinal): Boolean;
begin
  Result := GCD(X, Y) = 1;
end;

procedure SetPrimalityTest(const Method: TPrimalityTestMethod);
begin
  case Method of
    ptTrialDivision:
      IsPrime := IsPrimeTD;
    {$IFDEF CPU32}
    ptRabinMiller:
      IsPrime := IsPrimeRM;
    {$ENDIF CPU32}
  end;
end;

{$IFDEF CPU32}

//=== Floating point value classification ====================================

type
  TC3C2C0 = 0..6;

const
  FPClasses: array [TC3C2C0] of TFloatingPointClass =
   (
    fpInvalid,
    fpNaN,
    fpNormal,
    fpInfinite,
    fpZero,
    fpEmpty,
    fpDenormal
   );

// _C3C2C0 returns the set of condition code flags C0, C2, and C3 of the FPU status word
// to indicate the class of value or number in register ST(0) as follows:
// C0 in Bit 0 of EAX
// C2 in Bit 1 of EAX
// C3 in Bit 2 of EAX

function _C3C2C0: TC3C2C0; assembler;
// In: ST(0) Value to examine
asm
        FXAM
        XOR     EDX, EDX
        FNSTSW  AX
        FFREE   ST(0)
        FINCSTP
        BT      EAX, 14 // C3
        RCL     EDX, 1
        BT      EAX, 10 // C2
        RCL     EDX, 1
        BT      EAX, 8  // C0
        RCL     EDX, 1
        MOV     EAX, EDX
end;

function C3C2C0(const Value: Single): TC3C2C0; overload; assembler;
asm
        FLD     Value
        CALL    _C3C2C0
end;

function C3C2C0(const Value: Double): TC3C2C0; overload; assembler;
asm
        FLD     Value
        CALL    _C3C2C0
end;

{$IFDEF SUPPORTS_EXTENDED}
function C3C2C0(const Value: Extended): TC3C2C0; overload; assembler;
asm
        FLD     Value
        CALL    _C3C2C0
end;
{$ENDIF SUPPORTS_EXTENDED}

function FloatingPointClass(const Value: Single): TFloatingPointClass; overload;
begin
  Result := FPClasses[C3C2C0(Value)];
end;

function FloatingPointClass(const Value: Double): TFloatingPointClass; overload;
begin
  Result := FPClasses[C3C2C0(Value)];
end;

{$IFDEF SUPPORTS_EXTENDED}
function FloatingPointClass(const Value: Extended): TFloatingPointClass; overload;
begin
  Result := FPClasses[C3C2C0(Value)];
end;
{$ENDIF SUPPORTS_EXTENDED}

//=== NaN and Infinity support ===============================================

function IsInfinite(const Value: Single): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;

function IsInfinite(const Value: Double): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;

{$IFDEF SUPPORTS_EXTENDED}
function IsInfinite(const Value: Extended): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpInfinite;
end;
{$ENDIF SUPPORTS_EXTENDED}

const
  sSignBit = 31;
  dSignBit = 63;
  xSignBit = 79;

type
  TSingleBits = set of 0..sSignBit;
  TDoubleBits = set of 0..dSignBit;
  TExtendedBits = set of 0..xSignBit;

  sFractionBits = 0..22; // Single type fraction bits
  dFractionBits = 0..51; // Double type fraction bits
  xFractionBits = 0..62; // Extended type fraction bits

  sExponentBits = 23..sSignBit-1;
  dExponentBits = 52..dSignBit-1;
  xExponentBits = 64..xSignBit-1;

  {$IFNDEF FPC}
  QWord = Int64;
  {$ENDIF ~FPC}

  PExtendedRec = ^TExtendedRec;
  TExtendedRec = packed record
    Significand: QWord;
    Exponent: Word;
  end;

const
  ZeroTag = $3FFFFF;
  InvalidTag = TNaNTag($80000000);
  NaNTagMask = $3FFFFF;

  sNaNQuietFlag = High(sFractionBits);
  dNaNQuietFlag = High(dFractionBits);
  xNaNQuietFlag = High(xFractionBits);

  dNaNTagShift = High(dFractionBits) - High(sFractionBits);
  xNaNTagShift = High(xFractionBits) - High(sFractionBits);

  sNaNBits = $7F800000;
  dNaNBits = $7FF0000000000000;

  sQuietNaNBits = sNaNBits or (1 shl sNaNQuietFlag);
  dQuietNaNBits = dNaNBits or (Int64(1) shl dNaNQuietFlag);

function IsNaN(const Value: Single): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;

function IsNaN(const Value: Double): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;

{$IFDEF SUPPORTS_EXTENDED}
function IsNaN(const Value: Extended): Boolean; overload;
begin
  Result := FloatingPointClass(Value) = fpNaN;
end;
{$ENDIF SUPPORTS_EXTENDED}

procedure CheckNaN(const Value: Single); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJclMathError.CreateRes(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;

procedure CheckNaN(const Value: Double); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJclMathError.CreateRes(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;

{$IFDEF SUPPORTS_EXTENDED}
procedure CheckNaN(const Value: Extended); overload;
var
  SaveExMask: T8087Exceptions;
begin
  SaveExMask := Mask8087Exceptions([emInvalidOp]);
  try
    if FloatingPointClass(Value) <> fpNaN then
      raise EJclMathError.CreateRes(@RsNoNaN);
  finally
    SetMasked8087Exceptions(SaveExMask);
  end;
end;
{$ENDIF SUPPORTS_EXTENDED}

function GetNaNTag(const NaN: Single): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := PLongint(@NaN)^ and NaNTagMask;
  if sSignBit in TSingleBits(NaN) then
    Result := -Temp
  else
  if Temp = ZeroTag then
    Result := 0
  else
    Result := Temp;
end;

function GetNaNTag(const NaN: Double): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := (PInt64(@NaN)^ shr dNaNTagShift) and NaNTagMask;
  {$IFDEF FPC}
  if Int64(NaN) < 0 then
  {$ELSE ~FPC}
  if dSignBit in TDoubleBits(NaN) then
  {$ENDIF ~FPC}
    Result := -Temp
  else
  if Temp = ZeroTag then
    Result := 0
  else
    Result := Temp;
end;

{$IFDEF SUPPORTS_EXTENDED}
function GetNaNTag(const NaN: Extended): TNaNTag;
var
  Temp: Integer;
begin
  CheckNaN(NaN);
  Temp := (PExtendedRec(@NaN)^.Significand shr xNaNTagShift) and NaNTagMask;
  {$IFDEF FPC}
  if (TExtendedRec(NaN).Exponent and $8000) <> 0 then
  {$ELSE ~FPC}
  if xSignBit in TExtendedBits(NaN) then
  {$ENDIF ~FPC}
    Result := -Temp
  else
  if Temp = ZeroTag then
    Result := 0
  else
    Result := Temp;
end;
{$ENDIF SUPPORTS_EXTENDED}

{$IFDEF MSWINDOWS}
// ExceptObjProc is not used in FPC
{$IFNDEF FPC}

type
  TRealType = (rtUndef, rtSingle, rtDouble, rtExtended);

  { ExceptionInformation record for FPU exceptions under WinNT,
    where documented? }
  PFPUExceptionInfo = ^TFPUExceptionInfo;
  TFPUExceptionInfo = packed record
    Unknown: array [0..7] of Longint;
    ControlWord: Word;
    Dummy1: Word;
    StatusWord: Word;
    Dummy2: Word;
    TagWord: Word;
    Dummy3: Word;
    InstructionPtr: Pointer;
    UnknownW: Word;
    OpCode: Word;  // Note: 5 most significant bits of first opcode byte
                   // (always 11011b) not stored in FPU opcode register
    OperandPtr: Pointer;
    UnknownL: Longint;
  end;

  TExceptObjProc = function(P: PExceptionRecord): Exception;

var
  PrevExceptObjProc: TExceptObjProc;
  ExceptObjProcInitialized: Integer = 0;

function GetExceptionObject(P: PExceptionRecord): Exception;
var
  Tag: TNaNTag;
  FPUExceptInfo: PFPUExceptionInfo;
  OPtr: Pointer;
  OType: TRealType;

  function GetOperandType(OpCode: Word): TRealType;
  var
    NNN: 0..7;
  begin
    Result := rtUndef;
    NNN := (Lo(OpCode) shr 3) and 7;   // NNN field of ModR/M byte
    if Lo(OpCode) <= $BF then
    case Hi(OpCode) of   // 3 least significant bits of first opcode byte
      0:
        Result := rtSingle;
      1:
        if NNN < 4 then
          Result := rtSingle;
      // Extended signaling NaNs don't cause exceptions on FLD/FST(P) ?!
      3:
        if NNN = 5 then
          Result := rtExtended;
      4:
        Result := rtDouble;
      5:
        if NNN = 0 then
          Result := rtDouble;
    end;
  end;

begin
  Tag := InvalidTag; // shut up compiler warning
  OType := rtUndef;
  if P^.ExceptionCode = STATUS_FLOAT_INVALID_OPERATION then
  begin
    FPUExceptInfo := @P^.ExceptionInformation;
    OPtr := FPUExceptInfo^.OperandPtr;
    OType := GetOperandType(FPUExceptInfo^.OpCode);
    case OType of
      rtSingle:
        Tag := GetNaNTag(PSingle(OPtr)^);
      rtDouble:
        Tag := GetNaNTag(PDouble(OPtr)^);
      rtExtended:
        Tag := GetNaNTag(PExtended(OPtr)^);
    end;
  end;

  if OType = rtUndef then
    Result := PrevExceptObjProc(P)
  else
    Result := EJclNaNSignal.Create(Tag);
end;

procedure InitExceptObjProc;
begin
  if LockedExchange(ExceptObjProcInitialized, 1) = 0 then
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      {$IFDEF RTL200_UP} // Delphi 2009+
      PrevExceptObjProc := InterlockedExchangePointer(ExceptObjProc, @GetExceptionObject);
      {$ELSE}
      PrevExceptObjProc := Pointer(InterlockedExchange(Integer(ExceptObjProc), Integer(@GetExceptionObject)));
      {$ENDIF RTL200_UP}
end;
{$ENDIF ~FPC}
{$ENDIF MSWINDOWS}

procedure CheckTag(Tag: TNaNTag);
begin
  if (Tag < LowValidNaNTag) or (Tag > HighValidNaNTag) then
    raise EJclMathError.CreateResFmt(@RsNaNTagError, [Tag]);
end;

procedure MakeQuietNaN(var X: Single; Tag: TNaNTag);
var
  Bits: LongWord;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag or sQuietNaNBits
  else
    Bits := Abs(Tag) or sQuietNaNBits;
  if Tag < 0 then
    Include(TSingleBits(Bits), sSignBit);
  PLongWord(@X)^ := Bits;
end;

procedure MakeQuietNaN(var X: Double; Tag: TNaNTag);
var
  Bits: Int64;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag
  else
    Bits := Abs(Tag);
  PInt64(@X)^ := (Bits shl dNaNTagShift) or dQuietNaNBits;
  if Tag < 0 then
    {$IFDEF FPC}
    QWord(X) := QWord(X) or (1 shl dSignBit);
    {$ELSE ~FPC}
    Include(TDoubleBits(X), dSignBit);
    {$ENDIF ~FPC}
end;

{$IFDEF SUPPORTS_EXTENDED}
procedure MakeQuietNaN(var X: Extended; Tag: TNaNTag);
const
  QuietNaNSignificand = $C000000000000000;
  QuietNaNExponent = $7FFF;
var
  Bits: Int64;
begin
  CheckTag(Tag);
  if Tag = 0 then
    Bits := ZeroTag
  else
    Bits := Abs(Tag);
  TExtendedRec(X).Significand := (Bits shl xNaNTagShift) or QuietNaNSignificand;
  TExtendedRec(X).Exponent := QuietNaNExponent;
  if Tag < 0 then
    {$IFDEF FPC}
    TExtendedRec(X).Exponent := TExtendedRec(X).Exponent or $8000;
    {$ELSE ~FPC}
    Include(TExtendedBits(X), xSignBit);
    {$ENDIF ~FPC}
end;
{$ENDIF SUPPORTS_EXTENDED}

procedure MakeSignalingNaN(var X: Single; Tag: TNaNTag);
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  InitExceptObjProc;
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  MakeQuietNaN(X, Tag);
  Exclude(TSingleBits(X), sNaNQuietFlag);
end;

procedure MakeSignalingNaN(var X: Double; Tag: TNaNTag);
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  InitExceptObjProc;
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  {$IFDEF FPC}
  MakeQuietNaN(X, Tag);
  QWord(X) := QWord(X) and not (1 shl dNaNQuietFlag);
  {$ELSE ~FPC}
  MakeQuietNaN(X, Tag);
  Exclude(TDoubleBits(X), dNaNQuietFlag);
  {$ENDIF ~FPC}
end;

{$IFDEF SUPPORTS_EXTENDED}
procedure MakeSignalingNaN(var X: Extended; Tag: TNaNTag);
begin
  {$IFDEF FPC}
  MakeQuietNaN(X, Tag);
  TExtendedRec(X).Significand := TExtendedRec(X).Significand and not (1 shl xNaNQuietFlag);
  {$ELSE ~FPC}
  {$IFDEF MSWINDOWS}
  InitExceptObjProc;
  {$ENDIF MSWINDOWS}
  MakeQuietNaN(X, Tag);
  Exclude(TExtendedBits(X), xNaNQuietFlag);
  {$ENDIF ~FPC}
end;
{$ENDIF SUPPORTS_EXTENDED}

procedure MineSingleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag);
var
  Tag, StopTag: TNaNTag;
  P: PLongint;
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  InitExceptObjProc;
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  StopTag := StartTag + Count - 1;
  CheckTag(StartTag);
  CheckTag(StopTag);
  P := @Buffer;
  for Tag := StartTag to StopTag do
  begin
    if Tag > 0 then
      P^ := sNaNBits or Tag
    else
    if Tag < 0 then
      P^ := sNaNBits or Longint($80000000) or -Tag
    else
      P^ := sNaNBits or ZeroTag;
    Inc(P);
  end;
end;

procedure MineDoubleBuffer(var Buffer; Count: Integer; StartTag: TNaNTag);
var
  Tag, StopTag: TNaNTag;
  P: PInt64;
begin
  {$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  InitExceptObjProc;
  {$ENDIF ~FPC}
  {$ENDIF MSWINDOWS}
  StopTag := StartTag + Count - 1;
  CheckTag(StartTag);
  CheckTag(StopTag);
  P := @Buffer;
  for Tag := StartTag to StopTag do
  begin
    if Tag > 0 then
      P^ := dNaNBits or (Int64(Tag) shl dNaNTagShift)
    else
    if Tag < 0 then
      P^ := dNaNBits or $8000000000000000 or (Int64(-Tag) shl dNaNTagShift)
    else
      P^ := dNaNBits or (Int64(ZeroTag) shl dNaNTagShift);
    Inc(P);
  end;
end;

function MinedSingleArray(Length: Integer): TDynSingleArray;
begin
  SetLength(Result, Length);
  MineSingleBuffer(Result[0], Length, 0);
end;

function MinedDoubleArray(Length: Integer): TDynDoubleArray;
begin
  SetLength(Result, Length);
  MineDoubleBuffer(Result[0], Length, 0);
end;

function IsSpecialValue(const X: Float): Boolean;
begin
  Result := IsNaN(X) or IsInfinite(X);
end;

//=== { EJclNaNSignal } ======================================================

constructor EJclNaNSignal.Create(ATag: TNaNTag; Dummy: Boolean);
begin
  FTag := ATag;
  CreateResFmt(@RsNaNSignal, [ATag]);
end;

{$ENDIF CPU32}

//=== { TJclRational } =======================================================

constructor TJclRational.Create(const Numerator: Integer; const Denominator: Integer);
begin
  inherited Create;
  Assign(Numerator, Denominator);
end;

constructor TJclRational.Create;
begin
  inherited Create;
  AssignZero;
end;

constructor TJclRational.Create(const R: Float);
begin
  inherited Create;
  Assign(R);
end;

procedure TJclRational.Simplify;
var
  I: Integer;
begin
  if FN < 0 then
  begin
    FT := -FT;
    FN := -FN;
  end;

  if (FT = 1) or (FN = 1) or (FT = 0) then
    Exit;

  I := GCD(System.Abs(FT), FN);
  FT := FT div I;
  FN := FN div I;
end;

procedure TJclRational.Assign(const Numerator: Integer; const Denominator: Integer);
begin
  if Denominator = 0 then
    raise EJclMathError.CreateRes(@RsInvalidRational);
  FT := Numerator;
  FN := Denominator;
  if FN <> 1 then
    Simplify;
end;

procedure TJclRational.Assign(const R: TJclRational);
begin
  FT := R.FT;
  FN := R.FN;
end;

procedure TJclRational.Assign(const R: Float);
var
  T: TJclRational;
  Z: Integer;

  function CalcFrac(const R: Float; const Level: Integer): TJclRational;
  var
    I: Float;
    Z: Integer;
  begin
    if IsFloatZero(R) or (Level = 12) then // 0 (if Level = 12 we get an approximation)
      Result := TJclRational.Create
    else
    if FloatsEqual(R, 1.0) then // 1
    begin
      Result := TJclRational.Create;
      Result.AssignOne;
    end
    else
    if IsFloatZero(Frac(R * 1E8)) then // terminating decimal (<8)
      Result := TJclRational.Create(Trunc(R * 1E8), 100000000)
    else
    begin // recursive process
      I := 1.0 / R;
      Result := CalcFrac(Frac(I), Level + 1);
      Z := Trunc(I);
      Result.Add(Z);
      Result.Reciprocal;
    end;
  end;

begin
  T := CalcFrac(Frac(R), 1);
  try
    Z := Trunc(R);
    T.Add(Z);
    Assign(T);
  finally
    T.Free;
  end;
end;

procedure TJclRational.AssignOne;
begin
  FT := 1;
  FN := 1;
end;

procedure TJclRational.AssignZero;
begin
  FT := 0;
  FN := 1;
end;

function TJclRational.IsEqual(const Numerator: Integer; const Denominator: Integer): Boolean;
var
  R: TJclRational;
begin
  R := TJclRational.Create(Numerator, Denominator);
  Result := IsEqual(R);
  R.Free;
end;

function TJclRational.IsEqual(const R: TJclRational): Boolean;
begin
  Result := (FT = R.FT) and (FN = R.FN);
end;

function TJclRational.IsEqual(const R: Float): Boolean;
begin
  Result := FloatsEqual(R, GetAsFloat);
end;

function TJclRational.IsOne: Boolean;
begin
  Result := (FT = 1) and (FN = 1);
end;

function TJclRational.IsZero: Boolean;
begin
  Result := FT = 0;
end;

function TJclRational.Duplicate: TJclRational;
begin
  Result := TJclRational.Create(FT, FN);
end;

procedure TJclRational.SetAsFloat(const R: Float);
begin
  Assign(R);
end;

procedure TJclRational.SetAsString(const S: string);
var
  F: Integer;
begin
  F := Pos('/', S);
  if F = 0 then
    Assign(StrToFloat(S))
  else
    Assign(StrToInt(Trim(Copy(S,1,F - 1))), StrToInt(Trim(Copy(S, F + 1,Length(s)))));
end;

function TJclRational.GetAsFloat: Float;
begin
  Result := FT / FN;
end;

function TJclRational.GetAsString: string;
begin
  Result := IntToStr(FT) + '/' + IntToStr(FN);
end;

procedure TJclRational.Add(const R: TJclRational);
begin
  FT := FT * R.FN + R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

procedure TJclRational.Add(const V: Integer);
begin
  Inc(FT, FN * V);
end;

procedure TJclRational.Add(const V: Float);
begin
  Assign(GetAsFloat + V);
end;

procedure TJclRational.Subtract(const V: Float);
begin
  Assign(GetAsFloat - V);
end;

procedure TJclRational.Subtract(const R: TJclRational);
begin
  FT := FT * R.FN - R.FT * FN;
  FN := FN * R.FN;
  Simplify;
end;

procedure TJclRational.Subtract(const V: Integer);
begin
  Dec(FT, FN * V);
end;

procedure TJclRational.Negate;
begin
  FT := -FT;
end;

procedure TJclRational.Abs;
begin
  FT := System.Abs(FT);
  FN := System.Abs(FN);
end;

function TJclRational.Sgn: Integer;
begin
  if FT = 0 then
    Result := 0
  else
  begin
    if JclMathSgn(FT) = JclMathSgn(FN) then
      Result := 1
    else
      Result := -1;
  end;
end;

procedure TJclRational.Divide(const V: Integer);
begin
  if V = 0 then
    raise EJclMathError.CreateRes(@RsDivByZero);

  FN := FN * V;
  Simplify;
end;

procedure TJclRational.Divide(const R: TJclRational);
begin
  if R.FT = 0 then
    raise EJclMathError.CreateRes(@RsRationalDivByZero);

  FT := FT * R.FN;
  FN := FN * R.FT;
  Simplify;
end;

procedure TJclRational.Divide(const V: Float);
begin
  Assign(GetAsFloat / V);
end;

procedure TJclRational.Reciprocal;
begin
  if FT = 0 then
    raise EJclMathError.CreateRes(@RsRationalDivByZero);

  SwapOrd(FT, FN);
end;

procedure TJclRational.Multiply(const R: TJclRational);
begin
  FT := FT * R.FT;
  FN := FN * R.FN;
  Simplify;
end;

procedure TJclRational.Multiply(const V: Integer);
begin
  FT := FT * V;
  Simplify;
end;

procedure TJclRational.Multiply(const V: Float);
begin
  Assign(GetAsFloat * V);
end;

procedure TJclRational.Power(const R: TJclRational);
begin
  Assign(JclMathPower(GetAsFloat, R.GetAsFloat));
end;

procedure TJclRational.Power(const V: Integer);
var
  T, N: Extended;
begin
  T := FT;
  N := FN;
  FT := Round(JclMathPower(T, V));
  FN := Round(JclMathPower(N, V));
end;

procedure TJclRational.Power(const V: Float);
begin
  Assign(JclMathPower(FT, V) / JclMathPower(FN, V));
end;

procedure TJclRational.Sqrt;
begin
  Assign(System.Sqrt(FT / FN));
end;

procedure TJclRational.Sqr;
begin
  FT := System.Sqr(FT);
  FN := System.Sqr(FN);
end;

//=== Checksums ==============================================================

// See also: CountBitsSet in JclLogic (bug fixing etc.) - similar algorithm!

function GetParity(Buffer: TDynByteArray; Len: Integer): Boolean;
const
  lu: packed array [0..15] of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
var
  b: Byte;
  BitsSet: Cardinal;
  Index: Cardinal;
begin
  BitsSet := 0;
  Index := 0;
  if Len > Length(Buffer) then
    Len := Length(Buffer);
  while Len > 0 do
  begin
    b := Buffer[Index];
    // lower Nibble
    Inc(BitsSet, lu[b and $0F]);
    // upper Nibble
    Inc(BitsSet, lu[b shr 4]);

    Dec(Len);
    Inc(Index);
  end;

  Result := (BitsSet mod 2) = 0;
end;

function GetParity(Buffer: PByte; Len: Integer): Boolean;
const
  lu: packed array [0..15] of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4);
var
  b: Byte;
  BitsSet: Cardinal;
begin
  BitsSet := 0;
  while Len > 0 do
  begin
    b := PByte(Buffer)^;
    // lower Nibble
    Inc(BitsSet, lu[b and $0F]);
    // upper Nibble
    Inc(BitsSet, lu[b shr 4]);

    Dec(Len);
    Inc(PByte(Buffer));
  end;

  Result := (BitsSet mod 2) = 0;
end;

// CRC 16

function Crc16Corr(const Crc16Table: TCrc16Table; Crc: Word; N: Integer): Integer;
var
  I: Integer;
//  CrcX : Cardinal;
begin
  // calculate Syndrome
//  CrcX := CrC;
  for I := 1 to Crc16Bytes do
    // a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Crc := Crc16Table[Crc shr (CRC16Bits - 8)] xor Word(Crc shl 8);
  I := -1;
  repeat
    Inc(I);
    if (Crc and 1) <> 0 then
      Crc := ((Crc xor Crc16Table[1]) shr 1) or Crc16HighBit
//      Crc16Table[1] = Crc16Polynom
    else
      Crc := (Crc shr 1) and NotCrc16HighBit;
  until (Crc = Crc16HighBit) or (I = (N + Crc16Bytes) * 8);
  if Crc <> Crc16HighBit then
    Result := -1000 // not correctable
  else
    // I = No. of single faulty bit
    // (high bit first,
    // starting from lowest with CRC bits)
    Result := I - Crc16Bits;
    // Result <  0 faulty CRC-bit
    // Result >= 0 No. of faulty data bit
end;

function Crc16_P(const Crc16Table: TCrc16Table; X: PJclByteArray; N: Integer; Crc: Word): Word;
var
  I: Integer;
begin
  Result := Crc16DefaultStart;
  for I := 0 to N - 1 do // The CRC Bytes are located at the end of the information
    // a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc16Table[Result shr (CRC16Bits - 8)] xor Word((Result shl 8)) xor X[I];
  for I := 0 to Crc16Bytes - 1 do
  begin
    // a 16 bit value shr 8 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc16Table[Result shr (CRC16Bits-8)] xor Word((Result shl 8)) xor (Crc shr (CRC16Bits-8));
    Crc := Word(Crc shl 8);
  end;
end;

function Crc16_P(X: PJclByteArray; N: Integer; Crc: Word): Word;
begin
  Result := Crc16_P(Crc16DefaultTable, X, N, Crc);
end;

function CheckCrc16_P(const Crc16Table: TCrc16Table; X: PJclByteArray; N: Integer; Crc: Word): Integer;
// checks and corrects a single bit in up to 2^15-16 Bit -> 2^12-2 = 4094 Byte
var
  I, J: Integer;
  C: Byte;
begin
  Crc := Crc16_P(Crc16Table, X, N, Crc);
  if Crc = 0 then
    Result := 0 // No CRC-error
  else
  begin
    J := Crc16Corr(Crc16Table, Crc, N);
    if J < -(Crc16Bytes * 8 + 1) then
      Result := -1 // non-correctable error (more than one wrong bit)
    else
    begin
      if J < 0 then
        Result := 1 // one faulty Bit in CRC itself
      else
      begin // Bit J is faulty
        I := J and 7; // I <= 7 (faulty Bit in Byte)
        C := 1 shl I; // C <= 128
        I := J shr 3; // I: Index of faulty Byte
        X[N - 1 - I] := X[N - 1 - I] xor C; // correct faulty bit
        Result := 1; // Correctable error
      end;
    end;
  end;
end;

function CheckCrc16_P(X: PJclByteArray; N: Integer; Crc: Word): Integer;
begin
  Result := CheckCrc16_P(Crc16DefaultTable, X, N, Crc);
end;

function Crc16(const Crc16Table: TCrc16Table; const X: array of Byte; N: Integer; Crc: Word): Word;
begin
  Result := Crc16_P(Crc16Table, @X, N, Crc);
end;

function Crc16(const X: array of Byte; N: Integer; Crc: Word): Word;
begin
  Result := Crc16_P(Crc16DefaultTable, @X, N, Crc);
end;

function CheckCrc16(const Crc16Table: TCrc16Table; var X: array of Byte; N: Integer; Crc: Word): Integer;
begin
  Result := CheckCRC16_P(Crc16Table, @X, N, CRC);
end;

function CheckCrc16(var X: array of Byte; N: Integer; Crc: Word): Integer;
begin
  Result := CheckCRC16_P(Crc16DefaultTable, @X, N, CRC);
end;

function Crc16_A(const Crc16Table: TCrc16Table; const X: array of Byte; Crc: Word): Word;
begin
  Result := Crc16_P(Crc16Table, @X, Length(X), Crc);
end;

function Crc16_A(const X: array of Byte; Crc: Word): Word;
begin
  Result := Crc16_P(Crc16DefaultTable, @X, Length(X), Crc);
end;

function CheckCrc16_A(const Crc16Table: TCrc16Table; var X: array of Byte; Crc: Word): Integer;
begin
  Result := CheckCrc16_P(Crc16Table, @X, Length(X), Crc);
end;

function CheckCrc16_A(var X: array of Byte; Crc: Word): Integer;
begin
  Result := CheckCrc16_P(Crc16DefaultTable, @X, Length(X), Crc);
end;

// The CRC Table can be generated like this:
// const Crc16Start0 = 0;  !!

function Crc16_Bitwise(const X: array of Byte; N: Integer; Crc: Word; Polynom: Word): Word;
const
  Crc16Start0 = 0;   //Generating the table
var
  I, J: Integer;
  Sr, SrHighBit: Word;
  B: Byte;
begin
   Sr := Crc16Start0;
   SrHighBit := 0;
   for I := 0 to N - 1 + Crc16Bytes do
   begin
      if I >= N then
      begin
         B := Crc shr (Crc16Bits - 8);
         Crc := Crc shl 8;
      end
      else
        B := X[I];
      for J := 1 to 8 do
      begin
        if SrHighBit <> 0 then
          Sr := Sr xor Polynom;
        SrHighBit := Sr and Crc16HighBit;
        Sr := (Word (Sr shl 1)) or ((B shr 7) and 1);
        B := Byte(B shl 1);
      end;
   end;
   if SrHighBit <> 0 then
      Sr := Sr xor Polynom;
   Result := Sr;
end;

procedure InitCrc16(Polynom, Start: Word; out Crc16Table: TCrc16Table);
var
  X: array [0..0] of Byte;
  I: Integer;
begin
   for I := 0 to 255 do
   begin
     X[0] := I;
     Crc16Table[I] := Crc16_Bitwise(X, 1, 0, Polynom); { only with crcstart=0 !!!!}
   end;
   Crc16DefaultStart := Start;
end;

procedure InitCrc16(Polynom, Start: Word);
begin
  InitCrc16(Polynom, Start, Crc16DefaultTable);
end;

// CRC 32

function Crc32Corr(const Crc32Table: TCrc32Table; Crc: Cardinal; N: Integer): Integer;
var
  I: Integer;
begin
  // calculate Syndrome
  for I := 1 to Crc32Bytes do
    Crc := Crc32Table[Crc shr (CRC32Bits - 8)] xor (Crc shl 8);
  I := -1;
  repeat
    Inc(I);
    if (Crc and 1) <> 0 then
      Crc := ((Crc xor Crc32Table[1]) shr 1) or Crc32HighBit
//      Crc32Table[1] = Crc32Polynom
    else
      Crc := (Crc shr 1) and NotCrc32HighBit;
  until (Crc = Crc32HighBit) or (I = (N + Crc32Bytes) * 8);
  if Crc <> Crc32HighBit then
    Result := -1000 // not correctable
  else
    // I = No. of single faulty bit
    // (high bit first,
    // starting from lowest with CRC bits)
    Result := I - Crc32Bits;
    // Result <  0 faulty CRC-bit
    // Result >= 0 No. of faulty data bit
end;

function Crc32_P(const Crc32Table: TCrc32Table; X: PJclByteArray; N: Integer; Crc: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := Crc32DefaultStart;
  for I := 0 to N - 1 do // The CRC Bytes are located at the end of the information
    // a 32 bit value shr 24 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc32Table[Result shr (CRC32Bits-8)] xor (Result shl 8) xor X[I];
  for I := 0 to Crc32Bytes - 1 do
  begin
    // a 32 bit value shr 24 is a Byte, explictit type conversion to Byte adds an ASM instruction
    Result := Crc32Table[Result shr (CRC32Bits-8)] xor (Result shl 8) xor (Crc shr (CRC32Bits-8));
    Crc := Crc shl 8;
  end;
end;

function Crc32_P(X: PJclByteArray; N: Integer; Crc: Cardinal): Cardinal;
begin
  Result := Crc32_P(Crc32DefaultTable, X, N, Crc);
end;

function CheckCrc32_P(const Crc32Table: TCrc32Table; X: PJclByteArray; N: Integer; Crc: Cardinal): Integer;
// checks and corrects a single bit in up to 2^31-32 Bit -> 2^28-4 = 268435452 Byte
var
  I, J: Integer;
  C: Byte;
begin
  Crc := Crc32_P(Crc32Table, X, N, Crc);
  if Crc = 0 then
    Result := 0 // No CRC-error
  else
  begin
    J := Crc32Corr(Crc32Table, Crc, N);
    if J < -(Crc32Bytes * 8 + 1) then
      Result := -1 // non-correctable error (more than one wrong bit)
    else
    begin
      if J < 0 then
        Result := 1 // one faulty Bit in CRC itself
      else
      begin // Bit J is faulty
        I := J and 7; // I <= 7 (faulty Bit in Byte)
        C := 1 shl I; // C <= 128
        I := J shr 3; // I: Index of faulty Byte
        X[N - 1 - I] := X[N - 1 - I] xor C; // correct faulty bit
        Result := 1; // Correctable error
      end;
    end;
  end;
end;

function CheckCrc32_P(X: PJclByteArray; N: Integer; Crc: Cardinal): Integer;
begin
  Result := CheckCrc32_P(Crc32DefaultTable, X, N, Crc);
end;

function Crc32(const Crc32Table: TCrc32Table; const X: array of Byte; N: Integer; Crc: Cardinal): Cardinal;
begin
  Result := Crc32_P(Crc32Table, @X, N, Crc);
end;

function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal): Cardinal;
begin
  Result := Crc32_P(Crc32DefaultTable, @X, N, Crc);
end;

function CheckCrc32(const Crc32Table: TCrc32Table; var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
begin
  Result := CheckCRC32_P(Crc32Table, @X, N, CRC);
end;

function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
begin
  Result := CheckCRC32_P(Crc32DefaultTable, @X, N, CRC);
end;

function Crc32_A(const Crc32Table: TCrc32Table; const X: array of Byte; Crc: Cardinal): Cardinal;
begin
  Result := Crc32_P(Crc32Table, @X, Length(X), Crc);
end;

function Crc32_A(const X: array of Byte; Crc: Cardinal): Cardinal;
begin
  Result := Crc32_P(Crc32DefaultTable, @X, Length(X), Crc);
end;

function CheckCrc32_A(const Crc32Table: TCrc32Table; var X: array of Byte; Crc: Cardinal): Integer;
begin
  Result := CheckCrc32_P(Crc32Table, @X, Length(X), Crc);
end;

function CheckCrc32_A(var X: array of Byte; Crc: Cardinal): Integer;
begin
  Result := CheckCrc32_P(Crc32DefaultTable, @X, Length(X), Crc);
end;

// The CRC Table can be generated like this:
// const Crc32Start0 = 0;  !!

function Crc32_Bitwise(const X: array of Byte; N: Integer; Crc: Cardinal; Polynom: Cardinal) : Cardinal;
const
  Crc32Start0 = 0;   //Generating the table
var
  I, J: Integer;
  Sr, SrHighBit: Cardinal;
  B: Byte;
begin
  Sr := Crc32Start0;
  SrHighBit := 0;
  for I := 0 to N - 1 + Crc32Bytes do
  begin
    if I >= N then
    begin
      B := Crc shr (Crc32Bits - 8);
      Crc := Crc shl 8;
    end
    else
       B := X[I];
    for J := 1 to 8 do
    begin
       if SrHighBit <> 0 then
         Sr := Sr xor Polynom;
       SrHighBit := Sr and Crc32HighBit;
       Sr := (Sr shl 1) or ((B shr 7) and 1);
       B := Byte(B shl 1);
    end
  end;

  if SrHighBit <> 0 then
    Sr := Sr xor Polynom;
  Result := Sr;
end;

procedure InitCrc32(Polynom, Start: Cardinal; out Crc32Table: TCrc32Table);
var
  X: array [0..0] of Byte;
  I: Integer;
begin
   for I := 0 to 255 do
   begin
     X[0] := I;
     Crc32Table[I] := Crc32_Bitwise(X, 1, 0, Polynom);
   end;
   Crc32DefaultStart := Start;
end;

procedure InitCrc32(Polynom, Start: Cardinal);
begin
  InitCrc32(Polynom, Start, Crc32DefaultTable);
end;

//=== complex numbers support ================================================

const
  RectOne: TRectComplex = (Re: 1.0; Im: 0.0);
  RectZero: TRectComplex = (Re: 0.0; Im: 0.0);

var
  RectComplexFormatStr: string = '%g + %gi';
  PolarComplexFormatStr: string = '%g e^%gi';

procedure SetRectComplexFormatStr(const S: string);
begin
  RectComplexFormatStr := S;
end;

procedure SetPolarComplexFormatStr(const S: string);
begin
  PolarComplexFormatStr := S;
end;

function ComplexToStr(const Z: TRectComplex): string;
begin
  Result := Format(RectComplexFormatStr, [Z.Re, Z.Im]);
end;

function ComplexToStr(const Z: TPolarComplex): string;
begin
  Result := Format(PolarComplexFormatStr, [Z.Radius, Z.Angle]);
end;

function RectComplex(const Re: Float; const Im: Float = 0): TRectComplex;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

function RectComplex(const Z: TPolarComplex): TRectComplex;
var
  ASin, ACos: Float;
begin
  {$IFDEF DEBUG}
  Inc(ComplexTypeConversions);
  {$ENDIF DEBUG}
  JclMath.SinCos(Z.Angle, ASin, ACos);
  Result.Re := Z.Radius * ACos;
  Result.Im := Z.Radius * ASin;
end;

function PolarComplex(const Radius: Float; const Angle: Float = 0): TPolarComplex;
begin
  if Radius < 0 then
  begin
    Result.Radius := -Radius;
    Result.Angle := Pi;
  end
  else
  begin
    Result.Radius := Radius;
    Result.Angle := 0.0;
  end;
end;

function PolarComplex(const Z: TRectComplex): TPolarComplex;
begin
  {$IFDEF DEBUG}
  Inc(ComplexTypeConversions);
  {$ENDIF DEBUG}
  Result.Radius := Norm(Z);
  Result.Angle := ArcTan2(Z.Im, Z.Re);
end;

function Equal(const Z1, Z2: TRectComplex): Boolean;
begin
  Result := (Z1.Re = Z2.Re) and (Z1.Im = Z2.Im);
end;

function Equal(const Z1, Z2: TPolarComplex): Boolean;
begin
  Result := (Z1.Radius = Z2.Radius)
    and ((Z1.Radius = 0) or IsFloatZero(NormalizeAngle(Z1.Angle - Z2.Angle)));
end;

function IsZero(const Z: TRectComplex): Boolean;
begin
  Result := IsFloatZero(Z.Re) and IsFloatZero(Z.Im);
end;

function IsZero(const Z: TPolarComplex): Boolean;
begin
  Result := IsFloatZero(Z.Radius);
end;

{$IFDEF CPU32}

function IsInfinite(const Z: TRectComplex): Boolean;
begin
  Result := IsInfinite(Z.Re) or IsInfinite(Z.Im);
end;

function IsInfinite(const Z: TPolarComplex): Boolean;
begin
  Result := IsInfinite(Z.Radius);
end;

{$ENDIF CPU32}

function Norm(const Z: TRectComplex): Float;
begin
  Result := Sqrt(Sqr(Z.Re) + Sqr(Z.Im));
end;

function Norm(const Z: TPolarComplex): Float;
begin
  Result := Z.Radius;
end;

function AbsSqr(const Z: TRectComplex): Float;
begin
  Result := Sqr(Z.Re) + Sqr(Z.Im);
end;

function AbsSqr(const Z: TPolarComplex): Float;
begin
  Result := Sqr(Z.Radius);
end;

function Conjugate(const Z: TRectComplex): TRectComplex;
begin
  Result.Re :=  Z.Re;
  Result.Im := -Z.Im;
end;

function Conjugate(const Z: TPolarComplex): TPolarComplex;
begin
  Result.Radius :=  Z.Radius;
  Result.Angle := -Z.Angle;
end;

function Inv(const Z: TRectComplex): TRectComplex;
var
  Denom: Float;
begin
  Denom := Sqr(Z.Re) + Sqr(Z.Im);
  Result.Re :=  Z.Re / Denom;
  Result.Im := -Z.Im / Denom;
end;

function Inv(const Z: TPolarComplex): TPolarComplex;
begin
  Result.Radius := 1 / Z.Radius;
  Result.Angle := - Z.Angle;
end;

function Neg(const Z: TRectComplex): TRectComplex;
begin
  Result.Re := -Z.Re;
  Result.Im := -Z.Im;
end;

function Neg(const Z: TPolarComplex): TPolarComplex;
begin
  Result.Radius := Z.Radius;
  Result.Angle := NormalizeAngle(Z.Angle + Pi);
end;

function Sum(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result.Re := Z1.Re + Z2.Re;
  Result.Im := Z1.Im + Z2.Im;
end;

function Sum(const Z: array of TRectComplex): TRectComplex;
var
  I: Integer;
begin
  Result := RectZero;
  for I := Low(Z) to High(Z) do
  begin
    Result.Re := Result.Re + Z[I].Re;
    Result.Im := Result.Im + Z[I].Im;
  end;
end;

function Diff(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result.Re := Z1.Re - Z2.Re;
  Result.Im := Z1.Im - Z2.Im;
end;

function Product(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result.Re := Z1.Re * Z2.Re - Z1.Im * Z2.Im;
  Result.Im := Z1.Re * Z2.Im + Z1.Im * Z2.Re;
end;

function Product(const Z1, Z2: TPolarComplex): TPolarComplex;
begin
  Result.Radius := Z1.Radius * Z2.Radius;
  Result.Angle := NormalizeAngle(Z1.Angle + Z2.Angle);
end;

function Product(const Z: array of TPolarComplex): TPolarComplex;
var
  I: Integer;
begin
  Result.Radius := 1.0;
  Result.Angle := 0;
  for I := Low(Z) to High(Z) do
  begin
    Result.Radius := Result.Radius * Z[I].Radius;
    Result.Angle := Result.Angle + Z[I].Angle;
  end;
  Result.Angle := NormalizeAngle(Result.Angle);
end;


function Quotient(const Z1, Z2: TRectComplex): TRectComplex;
var
  Denom: Float;
begin
  Denom := Sqr(Z2.Re) + Sqr(Z2.Im);
  Result.Re := (Z1.Re * Z2.Re + Z1.Im * Z2.Im) / Denom;
  Result.Im := (Z1.Im * Z2.Re - Z1.Re * Z2.Im) / Denom;
end;

function Quotient(const Z1, Z2: TPolarComplex): TPolarComplex;
begin
  Result.Radius := Z1.Radius / Z2.Radius;
  Result.Angle := NormalizeAngle(Z1.Angle - Z2.Angle);
end;

function Ln(const Z: TPolarComplex): TRectComplex;
begin
  Result.Re := System.Ln(Z.Radius);
  Result.Im := NormalizeAngle(Z.Angle);
end;

function Exp(const Z: TRectComplex): TPolarComplex;
begin
  Result.Radius := System.Exp(Z.Re);
  Result.Angle := Z.Im;
end;

function Power(const Z: TPolarComplex; const Exponent: Float): TPolarComplex;
begin
  Result.Radius := JclMath.Power(Z.Radius, Exponent);
  Result.Angle := NormalizeAngle(Exponent * Z.Angle);
end;

function Power(const Z: TPolarComplex; const Exponent: TRectComplex): TPolarComplex;
begin
  Result := Exp(Product(Exponent, Ln(Z)));
end;

function PowerInt(const Z: TPolarComplex; const Exponent: Integer): TPolarComplex;
begin
  Result.Radius := PowerInt(Z.Radius, Exponent);
  Result.Angle := NormalizeAngle(Exponent * Z.Angle);
end;

function Root(const Z: TPolarComplex; const K, N: Cardinal): TPolarComplex;
begin
  Result.Radius := JclMath.Power(Z.Radius, 1.0 / N);
  Result.Angle := NormalizeAngle((Z.Angle + K * TwoPi) / N);
end;

//=== complex trigonometric functions ========================================

function Cos(const Z: TRectComplex): TRectComplex;
var
  ACos, ASin: Float;
begin
  JclMath.SinCos(Z.Re, ASin, ACos);
  Result.Re :=  ACos * JclMath.CosH(Z.Im);
  Result.Im := -ASin * JclMath.SinH(Z.Im);
end;

function Sin(const Z: TRectComplex): TRectComplex;
var
  ACos, ASin: Float;
begin
  JclMath.SinCos(Z.Re, ASin, ACos);
  Result.Re := ASin * JclMath.CosH(Z.Im);
  Result.Im := ACos * JclMath.SinH(Z.Im);
end;

function Tan(const Z: TRectComplex): TRectComplex;
var
  Denom: Float;
  ACos, ASin: Float;
begin
  JclMath.SinCos(2.0 * Z.Re, ASin, ACos);
  Denom := ACos + JclMath.CosH(2.0 * Z.Im);
  Result.Re := ASin / Denom;
  Result.Im := JclMath.SinH(2.0 * Z.Im) / Denom;
end;

function Cot(const Z: TRectComplex): TRectComplex;
var
  Denom: Float;
  ACos, ASin: Float;
begin
  JclMath.SinCos(2.0 * Z.Re, ASin, ACos);
  Denom := JclMath.CosH(2.0 * Z.Im) - ACos;
  Result.Re := ASin / Denom;
  Result.Im := -JclMath.SinH(2.0 * Z.Im) / Denom;
end;

function Sec(const Z: TRectComplex): TRectComplex;
begin
  Result := Quotient(RectOne, Cos(Z));
end;

function Csc(const Z: TRectComplex): TRectComplex;
begin
  Result := Quotient(RectOne, Sin(Z));
end;

//=== complex hyperbolic functions ===========================================

function CosH(const Z: TRectComplex): TRectComplex;
var
  ACos, ASin: Float;
begin
  JclMath.SinCos(Z.Im, ASin, ACos);
  Result.Re := JclMath.CosH(Z.Re) * ACos;
  Result.Im := JclMath.SinH(Z.Re) * ASin;
end;

function SinH(const Z: TRectComplex): TRectComplex;
var
  ACos, ASin: Float;
begin
  JclMath.SinCos(Z.Im, ASin, ACos);
  Result.Re := JclMath.SinH(Z.Re) * ACos;
  Result.Im := JclMath.CosH(Z.Re) * ASin;
end;

function TanH(const Z: TRectComplex): TRectComplex;
var
  Denom: Float;
  ACos, ASin: Float;
begin
  JclMath.SinCos(2.0 * Z.Im, ASin, ACos);
  Denom := JclMath.CosH(2.0 * Z.Re) + ACos;
  Result.Re := JclMath.SinH(2.0 * Z.Re) / Denom;
  Result.Im := ASin / Denom;
end;

function CotH(const Z: TRectComplex): TRectComplex;
var
  Denom: Float;
  ACos, ASin: Float;
begin
  JclMath.SinCos(2.0 * Z.Im, ASin, ACos);
  Denom := JclMath.CosH(2.0 * Z.Re) - ACos;
  Result.Re := JclMath.SinH(2.0 * Z.Re) / Denom;
  Result.Im := -ASin / Denom;
end;

function SecH(const Z: TRectComplex): TRectComplex;
begin
  Result := Quotient(RectOne, CosH(Z));
end;

function CscH(const Z: TRectComplex): TRectComplex;
begin
  Result := Quotient(RectOne, SinH(Z));
end;

{$IFDEF SUPPORTS_CLASS_OPERATORS}

{ TRectComplex }

class operator TRectComplex.Implicit(const Value: Float): TRectComplex;
begin
  Result := RectComplex(Value);
end;

class operator TRectComplex.Equal(const Z1, Z2: TRectComplex): Boolean;
begin
  Result := Equal(Z1, Z2);
end;

class operator TRectComplex.NotEqual(const Z1, Z2: TRectComplex): Boolean;
begin
  Result := not Equal(Z1, Z2);
end;

class operator TRectComplex.Add(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result := Sum(Z1, Z2);
end;

class operator TRectComplex.Subtract(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result := Diff(Z1, Z2);
end;

class operator TRectComplex.Multiply(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result := Product(Z1, Z2);
end;

class operator TRectComplex.Divide(const Z1, Z2: TRectComplex): TRectComplex;
begin
  Result := Quotient(Z1, Z2);
end;

class operator TRectComplex.Negative(const Z: TRectComplex): TRectComplex;
begin
  Result := Neg(Z);
end;

class operator TRectComplex.Positive(const Z: TRectComplex): TRectComplex;
begin
  Result := Z;
end;

function TRectComplex.AsString: string;
begin
  Result := ComplexToStr(Self);
end;

function TRectComplex.Conjugate: TRectComplex;
begin
  Result := JclMath.Conjugate(Self);
end;

function TRectComplex.IsZero: Boolean;
begin
  Result := JclMath.IsZero(Self);
end;

function TRectComplex.IsInfinite: Boolean;
begin
  {$IFDEF DELPHI64_TEMPORARY}
  //IsInfinite is disabled for 64-bit, because BASM is not 64-bit compatible (see logmessage of public repo @3070)
  System.Error(rePlatformNotImplemented);
  Result := False;
  {$ELSE ~DELPHI64_TEMPORARY}
  Result := JclMath.IsInfinite(Self);
  {$ENDIF ~DELPHI64_TEMPORARY}
end;

{ TPolarComplex }

class operator TPolarComplex.Implicit(const Value: Float): TPolarComplex;
begin
  Result := PolarComplex(Value);
end;

class operator TPolarComplex.Implicit(const Z: TPolarComplex): TRectComplex;
begin
  Result := RectComplex(Z);
end;

class operator TPolarComplex.Implicit(const Z: TRectComplex): TPolarComplex;
begin
  Result := PolarComplex(Z);
end;

{$IFNDEF CPPBUILDER}
class operator TPolarComplex.Explicit(const Z: TPolarComplex): TRectComplex;
begin
  Result := RectComplex(Z);
end;

class operator TPolarComplex.Explicit(const Z: TRectComplex): TPolarComplex;
begin
  Result := PolarComplex(Z);
end;
{$ENDIF CPPBUILDER}

class operator TPolarComplex.Equal(const Z1, Z2: TPolarComplex): Boolean;
begin
  Result := Equal(Z1, Z2);
end;

class operator TPolarComplex.NotEqual(const Z1, Z2: TPolarComplex): Boolean;
begin
  Result := not Equal(Z1, Z2);
end;

class operator TPolarComplex.Add(const Z1, Z2: TPolarComplex): TRectComplex;
begin
  Result := Sum(Z1, Z2);
end;

class operator TPolarComplex.Subtract(const Z1, Z2: TPolarComplex): TRectComplex;
begin
  Result := Diff(Z1, Z2);
end;

class operator TPolarComplex.Multiply(const Z1, Z2: TPolarComplex): TPolarComplex;
begin
  Result := Product(Z1, Z2);
end;

class operator TPolarComplex.Divide(const Z1, Z2: TPolarComplex): TPolarComplex;
begin
  Result := Quotient(Z1, Z2);
end;

class operator TPolarComplex.Negative(const Z: TPolarComplex): TPolarComplex;
begin
  Result := Neg(Z);
end;

class operator TPolarComplex.Positive(const Z: TPolarComplex): TPolarComplex;
begin
  Result := Z;
end;

function TPolarComplex.AsString: string;
begin
  Result := ComplexToStr(Self);
end;

function TPolarComplex.Conjugate: TPolarComplex;
begin
  Result := JclMath.Conjugate(Self);
end;

function TPolarComplex.IsZero: Boolean;
begin
  Result := JclMath.IsZero(Self);
end;

function TPolarComplex.IsInfinite: Boolean;
begin
  {$IFDEF DELPHI64_TEMPORARY}
  //IsInfinite is disabled for 64-bit, because BASM is not 64-bit compatible (see logmessage of public repo @3070)
  System.Error(rePlatformNotImplemented);
  Result := False;
  {$ELSE ~DELPHI64_TEMPORARY}
  Result := JclMath.IsInfinite(Self);
  {$ENDIF ~DELPHI64_TEMPORARY}
end;

function TPolarComplex.Power(const Exponent: TRectComplex): TPolarComplex;
begin
  Result := JclMath.Power(Self, Exponent);
end;

function TPolarComplex.Power(const Exponent: Float): TPolarComplex;
begin
  Result := JclMath.Power(Self, Exponent);
end;

function TPolarComplex.Power(const Exponent: Integer): TPolarComplex;
begin
  Result := JclMath.PowerInt(Self, Exponent);
end;

function TPolarComplex.Root(const K, N: Cardinal): TPolarComplex;
begin
  Result := JclMath.Root(Self, K, N);
end;

{$ENDIF SUPPORTS_CLASS_OPERATORS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
