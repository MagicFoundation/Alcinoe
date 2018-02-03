{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test                                                                                  }
{                                                                                                  }
{ Last Update: 19-Jan-2002                                                                         }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclMath;

interface
uses
  TestFramework,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
{$IFDEF VCL}
  Dialogs,
{$ENDIF VCL}
  Classes,
  SysUtils,
  Math,
  JclMath;

{ TMathLogarithmicTest }

type
  TMathLogarithmicTest = class (TTestCase)
  published
    procedure _LogBase10;
    procedure _LogBase2;
    procedure _LogBaseN;
  end;

type
  TMathTranscendentalTest = class (TTestCase)
  published
    procedure _ArcCos;
    procedure _ArcCot;
    procedure _ArcCsc;
    procedure _ArcSec;
    procedure _ArcSin;
    procedure _ArcTan;
    procedure _ArcTan2;
    procedure _Cos;
    procedure _Cot;
    procedure _Csc;
    procedure _Sec;
    procedure _Sin;
    procedure _SinCos;
    procedure _Tan;
  end;

type
  TMathMiscTest = class (TTestCase)
  published
    procedure _Ackermann;
    procedure _Ceiling;
    procedure _Factorial;
    procedure _Fibonacci;
    procedure _Floor;
    procedure _GCD;
    procedure _ISqrt;
    procedure _LCM;
    procedure _NormalizeA;
    procedure _Pythagoras;
    procedure _Sgn;
    procedure _Signe;
  end;

type
  TMathRationalTest = class(TTestCase)
  private
    RN1: TJclRational;
    RN2: TJclRational;
    RN3: TJclRational;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure _Assign;
    procedure _Add;
    procedure _IsEqual;
    procedure _IsZero;
    procedure _IsOne;
    procedure _Subtract;
    procedure _Multiply;
    procedure _Divide;
    procedure _Power;
    procedure _AsFloat;
    procedure _AsString;
    procedure _Sqr;
    procedure _Sqrt;
  end;

type
  TMathExponentialTest = class(TTestCase)
  published
    procedure _Exp;
    procedure _Power;
    procedure _PowerInt;
    procedure _TenToY;
    procedure _TwoToY;
  end;

type
  TMathPrimeTest = class(TTestCase)
  published
    procedure _IsPrime;
    procedure _IsRelativePrime;
  end;

type
  TMathInfNanSupportTest = class(TTestCase)
  private
    s: single;
    d: Double;
    e: Extended;

  published
     procedure _IsInfinite;
     procedure _IsNaN;
     procedure _IsSpecialValue;
     procedure _MakeQuietNaN;
     procedure _GetNaNTag;
  end;

type
  TSetCrack = class(TJclASet);

type
  TMathASetTest = class(TTestCase)
  protected
    ASet: TJclASet;
    procedure TearDown; override;

  published
    procedure _Invert;
    procedure _SetGet;
    procedure _SetGetRange;
  end;

  TMathFlatSetTest = class(TMathASetTest)
  protected
    procedure SetUp; override;
  end;

implementation

//==================================================================================================
// Logarithmic
//==================================================================================================

procedure TMathLogarithmicTest._LogBase10;
var
  x: Extended;

begin
  x := 0.1;
  while x < 100 do
  begin
    CheckEquals(Log10(x), LogBase10(x), PrecisionTolerance);
    x := x + 0.5;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathLogarithmicTest._LogBase2;
var
  x: Extended;

begin
  x := 0.1;
  while x < 100 do
  begin
    CheckEquals(Math.Log2(x), LogBase2(x), PrecisionTolerance);
    x := x + 0.5;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathLogarithmicTest._LogBaseN;
var
  x: Extended;
  Base: Integer;

begin
  x := 0.1;
  for Base := 2 to 60 do
  while x < 100 do
  begin
    CheckEquals(Math.LogN(Base, x), LogBaseN(Base, x), PrecisionTolerance);
    x := x + 0.5;
  end;
end;

//==================================================================================================
// Transcendental
//==================================================================================================

procedure TMathTranscendentalTest._ArcCos;
var
  x: Extended;

begin
  x := -0.98;

  while x < 1 do
  begin
    CheckEquals(Math.ArcCos(X), JclMath.ArcCos(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcCot;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcCsc;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcSec;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcSin;
var
  x: Extended;

begin
  x := -0.98;

  while x < 1 do
  begin
    CheckEquals(Math.ArcSin(X), JclMath.ArcSin(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcTan;
var
  x: Extended;

begin
  x := -Pi;

  while x < Pi do
  begin
    if x <> 0 then
      CheckEquals(System.ArcTan(X), JclMath.ArcTan(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._ArcTan2;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Cos;
var
  x: Extended;

begin
  x := -Pi;

  while x <= Pi do
  begin
    CheckEquals(System.Cos(X), JclMath.Cos(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Cot;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Csc;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Sec;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Sin;
var
  x: Extended;

begin
  x := -Pi;

  while x <= Pi do
  begin
    CheckEquals(System.Sin(X), JclMath.Sin(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._SinCos;
var
  x, s, c: Extended;

begin
  x := -Pi;

  while x <= Pi do
  begin
    SinCos(x, s, c);

    CheckEquals(System.Sin(X), s, PrecisionTolerance);
    CheckEquals(System.Cos(X), c, PrecisionTolerance);
    x := x + 0.1;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathTranscendentalTest._Tan;
var
  x: Extended;

begin
  x := -Pi;

  while x <= Pi do
  begin
    CheckEquals(Math.Tan(X), JclMath.Tan(X), PrecisionTolerance);
    x := x + 0.1;
  end;
end;


//==================================================================================================
// Hyperbolic
//==================================================================================================


//==================================================================================================
// Miscellaneous
//==================================================================================================

procedure TMathMiscTest._Ackermann;
begin
  CheckEquals(1, Ackermann(0,0));
  CheckEquals(7, Ackermann(2,2));
  CheckEquals(5, Ackermann(3,0));
  CheckEquals(61, Ackermann(3,3));
  CheckEquals(125, Ackermann(3,4));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Ceiling;
var
  i: Integer;
  e: Extended;

begin
  RandSeed := 12321;
  for i := 1 to 2000 do
  begin
    e := random(100000) / (random(230000)+1);
    CheckEquals(Math.Ceil(e), JclMath.Ceiling(e), PrecisionTolerance);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Factorial;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Fibonacci;
begin
  CheckEquals(0, Fibonacci(0));
  CheckEquals(1, Fibonacci(1));
  CheckEquals(1, Fibonacci(2));
  CheckEquals(2, Fibonacci(3));
  CheckEquals(3, Fibonacci(4));
  CheckEquals(5, Fibonacci(5));
  CheckEquals(8, Fibonacci(6));
  CheckEquals(13, Fibonacci(7));
  CheckEquals(21, Fibonacci(8));
  CheckEquals(34, Fibonacci(9));
  CheckEquals(55, Fibonacci(10));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Floor;
var
  i: Integer;
  e: Extended;

begin
  RandSeed := 12321;
  for i := 1 to 2000 do
  begin
    e := random(100000) / random(230000);
    CheckEquals(Math.Floor(e), JclMath.Floor(e));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._GCD;
begin
  CheckEquals(1, GCD(2,5), PrecisionTolerance);
  CheckEquals(4, GCD(8,4), PrecisionTolerance);
  CheckEquals(3, GCD(801,48), PrecisionTolerance);
  CheckEquals(2 , GCD(80,98), PrecisionTolerance);
  CheckEquals(0 , GCD(0,0), PrecisionTolerance);
  CheckEquals(5 , GCD(100,5), PrecisionTolerance);
  CheckEquals(50 , GCD(100,50), PrecisionTolerance);
  CheckEquals(100, GCD(18700,700), PrecisionTolerance);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._ISqrt;
var
  i,v : Integer;

begin
  for i := 1 to 10000 do
  begin
    v := ISqrt(i);
    CheckEquals(integer(trunc(sqrt(i))),v);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._LCM;
begin
  CheckEquals(0, LCM(0,0), PrecisionTolerance);
  CheckEquals(300, LCM(100,150), PrecisionTolerance);
  CheckEquals(600, LCM(200,150), PrecisionTolerance);
  CheckEquals(400, LCM(400,50), PrecisionTolerance);
  CheckEquals(10, LCM(10,10), PrecisionTolerance);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._NormalizeA;
begin

end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Pythagoras;
var
  i: Integer;
  a,b,c : Extended;

begin
  RandSeed := 86543;

  for i := 1 to 10000 do
  begin
    a := random(100000) / (random(20000)+1);
    b := random(200000) / (random(24000)+1);
    c := sqrt(a*a + b*b);
    CheckEquals(c, Pythagoras(a,b), PrecisionTolerance);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Sgn;
var
  i: Integer;
  v: Integer;

begin
  RandSeed := 86543;

  for i := 1 to 10000 do
  begin
    v := random(MaxInt-1)+1;
    CheckEquals(1, Sgn(v));
    CheckEquals(-1, Sgn(-v));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathMiscTest._Signe;
begin
end;


//==================================================================================================
// Rational
//==================================================================================================

procedure TMathRationalTest._Assign;
var
  i, n, d: Integer;

begin
  RandSeed := 12345;
  for i := 1 to 1000 do
  begin
    n := random(100000)+1;
    d := 1;

    RN1.Assign(n, d);
    CheckEquals(n, RN1.Numerator);
    CheckEquals(d, RN1.Denominator);

    RN2.Assign(RN1);
    CheckEquals(n, RN2.Numerator);
    CheckEquals(d, RN2.Denominator);
  end;

  RN1.AssignOne;
  CheckEquals(1, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN1.AssignZero;
  CheckEquals(0, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Add;
begin
  RN2.AssignOne;
  RN1.Assign(5,5);
  RN1.Add(RN2);

  CheckEquals(2, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN2.Assign(6,9); RN1.Assign(3,7); RN2.Add(RN1);
  CheckEquals(23, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(-6,9); RN1.Assign(3,7); RN2.Add(RN1);
  CheckEquals(-5, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(-6,9); RN1.Assign(3,7); RN2.Add(RN1);
  CheckEquals(-5, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(-6,9); RN1.Assign(-3,7); RN2.Add(RN1);
  CheckEquals(-23, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(6,9); RN2.Add(3);
  CheckEquals(11, RN2.Numerator);
  CheckEquals(3, RN2.Denominator);

  RN2.Assign(2,2); RN2.Add(0.25);
  CheckEquals(5, RN2.Numerator);
  CheckEquals(4, RN2.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._IsEqual;
var
  i, n, d: Integer;

begin
  RandSeed := 12345;
  for i := 1 to 1000 do
  begin
    n := random(100000)+1;
    d := random(100000)+1;

    RN1.Assign(n, d);
    RN2.Assign(RN1);
    CheckEquals(True, RN2.IsEqual(RN1));
    CheckEquals(True, RN2.IsEqual(n, d));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._IsOne;
var
  i, n, d: Integer;

begin
  RandSeed := 12345;

  for i := 1 to 1000 do
  begin
    n := random(100000)+1;
    d := random(100000)+1;

    RN1.Assign(n, d);
    CheckEquals(False, RN1.IsOne);
  end;

  RN1.Assign(1);
  CheckEquals(True, RN1.IsOne);

  RN1.AssignOne;
  CheckEquals(True, RN1.IsOne);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._IsZero;
var
  i, n, d: Integer;

begin
  RandSeed := 12345;

  for i := 1 to 1000 do
  begin
    n := random(100000)+1;
    d := random(100000)+1;

    RN1.Assign(n, d);
    CheckEquals(False, RN1.IsZero);
  end;

  RN1.Assign(0);
  CheckEquals(True, RN1.IsZero);

  RN1.Assignzero;
  CheckEquals(True, RN1.IsZero);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Subtract;
begin
  RN2.AssignOne;
  RN1.Assign(5,5);
  RN1.Subtract(RN2);

  CheckEquals(0, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN2.Assign(6,9); RN1.Assign(3,7); RN2.Subtract(RN1);
  CheckEquals(5, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(-6,9); RN1.Assign(3,7); RN2.Subtract(RN1);
  CheckEquals(-23, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.Assign(18,9); RN1.Assign(3,7); RN2.Subtract(RN1);
  CheckEquals(11, RN2.Numerator);
  CheckEquals(7, RN2.Denominator);

  RN2.Assign(18,9); RN2.Subtract(19);
  CheckEquals(-17, RN2.Numerator);
  CheckEquals(1, RN2.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Multiply;
begin
  RN2.AssignOne; RN1.Assign(5,5); RN1.Multiply(RN2);
  CheckEquals(1, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN2.Assign(18,9); RN1.Assign(3,7); RN2.Multiply(RN1);
  CheckEquals(6, RN2.Numerator);
  CheckEquals(7, RN2.Denominator);

  RN2.Assign(1,9); RN1.Assign(3,7); RN2.Multiply(RN1);
  CheckEquals(1, RN2.Numerator);
  CheckEquals(21, RN2.Denominator);

  RN2.AssignZero; RN1.Assign(5,5); RN1.Multiply(RN2);
  CheckEquals(0, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Divide;
begin
  RN2.AssignOne; RN1.Assign(5,5); RN1.Divide(RN2);
  CheckEquals(1, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN2.Assign(18,9); RN1.Assign(3,7); RN2.Divide(RN1);
  CheckEquals(14, RN2.Numerator);
  CheckEquals(3, RN2.Denominator);

  RN2.Assign(1,9); RN1.Assign(3,7); RN2.Divide(RN1);
  CheckEquals(7, RN2.Numerator);
  CheckEquals(27, RN2.Denominator);

  RN1.Assign(5,5); RN1.Divide(2);
  CheckEquals(1, RN1.Numerator);
  CheckEquals(2, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Power;
begin
  RN2.Assign(18,9); RN1.Assign(18,9); RN2.Power(RN1);
  CheckEquals(4, RN2.Numerator);
  CheckEquals(1, RN2.Denominator);

  RN1.Assign(5,5); RN1.Power(2);
  CheckEquals(1, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN1.Assign(7,5); RN1.Power(2);
  CheckEquals(49, RN1.Numerator);
  CheckEquals(25, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._AsFloat;
var
  e: extended;
  i: Integer;

begin
  RandSeed := 123;
  for i := 1 to 2000 do
  begin
    e := random(10000) / (random(1000)+1);
    RN1.AsFloat := e; CheckEquals(e, RN1.AsFloat, 0.05);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._AsString;
var
  i, i1, i2: Integer;

begin
  RandSeed := 123;
  for i := 1 to 2000 do
  begin
    i1 := random(10000)+1;
    i2 := random(1000)+1;
    Rn1.AsString := inttostr(i1) + ' / ' + inttostr(i2);
    RN2.Assign(i1,i2);
    CheckEquals(True, RN2.IsEqual(RN1));
  end;

   Rn1.AsString := '6 / 2';;
   CheckEquals(3, RN1.Numerator);
   CheckEquals(1, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Sqr;
begin
  RN1.Assign(5,5); RN1.Sqr;
  CheckEquals(1, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN1.Assign(18,9); RN1.Sqr;
  CheckEquals(4, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN1.Assign(1,5); RN1.Sqr;
  CheckEquals(1, RN1.Numerator);
  CheckEquals(25, RN1.Denominator);

  RN1.Assign(3,5); RN1.Sqr;
  CheckEquals(9, RN1.Numerator);
  CheckEquals(25, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest._Sqrt;
begin
  RN1.Assign(0,1); RN1.Sqrt;
  CheckEquals(0, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);

  RN1.Assign(144,9); RN1.Sqrt;
  CheckEquals(4, RN1.Numerator);
  CheckEquals(1, RN1.Denominator);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest.SetUp;
begin
  RN1 := TJclRational.Create;
  RN2 := TJclRational.Create;
  RN3 := TJclRational.Create;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathRationalTest.TearDown;
begin
  FreeAndNil(RN1);
  FreeAndNil(RN2);
  FreeAndNil(RN3);
end;

//==================================================================================================
// Exponential
//==================================================================================================

procedure TMathExponentialTest._Exp;
var
  i: Integer;
  e: extended;

begin
  RandSeed := 73162;

  for i := 1 to 100 do
  begin
    e := Random(1000) / (Random(1000) + 1);
    CheckEquals(System.exp(e),JclMath.exp(e), PrecisionTolerance);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathExponentialTest._Power;
var
  Base, Exponent: extended;
  i: Integer;

begin
  RandSeed := 73162;

  for i := 1 to 100 do
  begin
    Base := Random(10);
    Exponent := Random(10);

    CheckEquals(Math.Power(Base, Exponent),JclMath.Power(Base, Exponent), PrecisionTolerance);
  end;

end;

//--------------------------------------------------------------------------------------------------

procedure TMathExponentialTest._PowerInt;
begin
   CheckEquals(1, PowerInt(0,0), PrecisionTolerance);
   CheckEquals(4, PowerInt(2,2), PrecisionTolerance);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathExponentialTest._TenToY;
begin
   CheckEquals(1,JclMath.TenToY(0), PrecisionTolerance);
   CheckEquals(10,JclMath.TenToY(1), PrecisionTolerance);
   CheckEquals(100,JclMath.TenToY(2), PrecisionTolerance);
   CheckEquals(1000,JclMath.TenToY(3), PrecisionTolerance);
   CheckEquals(0.1,JclMath.TenToY(-1), PrecisionTolerance);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathExponentialTest._TwoToY;
begin
   CheckEquals(1,JclMath.TwoToY(0), PrecisionTolerance);
   CheckEquals(2,JclMath.TwoToY(1), PrecisionTolerance);
   CheckEquals(4,JclMath.TwoToY(2), PrecisionTolerance);
   CheckEquals(8,JclMath.TwoToY(3), PrecisionTolerance);
end;

//==================================================================================================
// FlatSet
//==================================================================================================

procedure TMathASetTest._Invert;
begin
  TSetCrack(ASet).SetBit(1, True);
  TSetCrack(ASet).SetBit(2, False);
  TSetCrack(ASet).Invert;
  CheckEquals(False, TSetCrack(ASet).GetBit(1));
  CheckEquals(True, TSetCrack(ASet).GetBit(2));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathASetTest._SetGet;
var
  i,t : Integer;

begin
  for i := 0 to 1000 do
  begin
    TSetCrack(ASet).SetBit(i, True);
    CheckEquals(True, TSetCrack(ASet).GetBit(i));
    TSetCrack(ASet).SetBit(i, False);
    CheckEquals(False, TSetCrack(ASet).GetBit(i));
  end;

  for i := 0 to 20 do
  begin
    TSetCrack(ASet).SetBit(i, True);

    for t := 0 to i do
      CheckEquals(True, TSetCrack(ASet).GetBit(t));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathASetTest._SetGetRange;
var
  i: Integer;
  b: Boolean;

begin
  TSetCrack(ASet).SetRange(0, 100, True);

  for i := 0 to 100 do
   CheckEquals(True, TSetCrack(ASet).GetBit(i));

  B := TSetCrack(ASet).GetRange(0, 100, True);
  CheckEquals(True, B);

  TSetCrack(ASet).SetRange(50, 101, False);

  for i := 50 to 101 do
   CheckEquals(False, TSetCrack(ASet).GetBit(i));

  B := TSetCrack(ASet).GetRange(50, 101, False);
  CheckEquals(True, B);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathFlatSetTest.SetUp;
begin
  ASet := TJclFlatSet.Create;
  TSetCrack(ASet).SetBit(1, True);
end;

//--------------------------------------------------------------------------------------------------

procedure TMathASetTest.TearDown;
begin
  ASet.Free;
end;

//==================================================================================================
// Prime numbers
//==================================================================================================

procedure TMathPrimeTest._IsPrime;

  function GetFactor(N: Longint): Longint;  { from a usenet post - completely tested }
  var
    I,M,Act: Word;
  begin
    if N<=0 then
      RunError(215); { Arithmetic Overflow if zero or below }
    if Lo(N) and 1=0 then begin { can be divided by 2? }
      GetFactor:=2;
      Exit;
    end;
    if N mod 3=0 then begin { can be divided by 3? }
      GetFactor:=3;
      Exit;
    end;
    Act:=5; { next number to be tested }
    I:=2; { next increment of the test number }
    M:=Trunc(Sqrt(N));
    while (Act<=M) and (N mod Act>0) do begin { test for division }
      Inc(Act,I);
      I:=6-I; { alternate I between 2 and 4 }
    end;
    if Act > M then { factor found? }
      GetFactor := N { no }
    else
      GetFactor:=Act; { yes }
  end;

  function IsPrimeAlternative(N: Longint): Boolean;
  begin
    Result :=(N>1) and (GetFactor(N)=N);
  end;

var
  i: Integer;
  tm: TPrimalityTestMethod;

begin
  for tm := Low(TPrimalityTestMethod) to High(TPrimalityTestMethod) do
  begin
    SetPrimalityTest(TPrimalityTestMethod(tm));

    CheckEquals(False, IsPrime(0));
    CheckEquals(False, IsPrime(1));
    CheckEquals(True, IsPrime(2));

    for i := 1 to 4000 do
      CheckEquals(IsPrimeAlternative(i), IsPrime(i));

    for i := MaxInt - 4000 to MaxInt do
      CheckEquals(IsPrimeAlternative(i), IsPrime(i));

    for i := (MaxInt div 2) - 2000 to (MaxInt div 2) + 2000 do
      CheckEquals(IsPrimeAlternative(i), IsPrime(i));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TMathPrimeTest._IsRelativePrime;
begin
  CheckEquals(True, IsRelativePrime(1,4));
  CheckEquals(True, IsRelativePrime(3,4));
  CheckEquals(True, IsRelativePrime(13,19));
  CheckEquals(True, IsRelativePrime(17,99));
  CheckEquals(False, IsRelativePrime(0,4));
  CheckEquals(False, IsRelativePrime(2,4));
end;

//==================================================================================================
// NaN and Inf support
//==================================================================================================

procedure TMathInfNanSupportTest._IsInfinite;
begin
  s := Infinity;
  d := JclMath.Infinity;
  e := Infinity;
  CheckEquals(True, IsInfinite(s));
  CheckEquals(True, IsInfinite(d));
  CheckEquals(True, IsInfinite(e));

  s := 0;
  d := 0;
  e := 0;
  CheckEquals(False, IsInfinite(s));
  CheckEquals(False, IsInfinite(d));
  CheckEquals(False, IsInfinite(e));

  s := NaN;
  d := NaN;
  e := NaN;
  CheckEquals(False, IsInfinite(s));
  CheckEquals(False, IsInfinite(d));
  CheckEquals(False, IsInfinite(e));

  s := NegInfinity;
  d := NegInfinity;
  e := NegInfinity;
  CheckEquals(True, IsInfinite(s));
  CheckEquals(True, IsInfinite(d));
  CheckEquals(True, IsInfinite(e));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathInfNanSupportTest._IsNaN;
begin
  s := Infinity;
  d := JclMath.Infinity;
  e := Infinity;
  CheckEquals(False, JclMath.IsNan(s));
  CheckEquals(False, JclMath.IsNan(d));
  CheckEquals(False, JclMath.IsNan(e));

  s := 0;
  d := 0;
  e := 0;
  CheckEquals(False, JclMath.IsNan(s));
  CheckEquals(False, JclMath.IsNan(d));
  CheckEquals(False, JclMath.IsNan(e));

  s := NaN;
  d := NaN;
  e := NaN;
  CheckEquals(True, JclMath.IsNan(s));
  CheckEquals(True, JclMath.IsNan(d));
  CheckEquals(True, JclMath.IsNan(e));

  s := NegInfinity;
  d := NegInfinity;
  e := NegInfinity;
  CheckEquals(False, JclMath.IsNan(s));
  CheckEquals(False, JclMath.IsNan(d));
  CheckEquals(False, JclMath.IsNan(e));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathInfNanSupportTest._IsSpecialValue;
begin
  s := Infinity;
  d := JclMath.Infinity;
  e := Infinity;
  CheckEquals(True, IsSpecialValue(s));
  CheckEquals(True, IsSpecialValue(d));
  CheckEquals(True, IsSpecialValue(e));

  s := 0;
  d := 0;
  e := 0;
  CheckEquals(False, IsSpecialValue(s));
  CheckEquals(False, IsSpecialValue(d));
  CheckEquals(False, IsSpecialValue(e));

  s := NaN;
  d := NaN;
  e := NaN;
  CheckEquals(True, IsSpecialValue(s));
  CheckEquals(True, IsSpecialValue(d));
  CheckEquals(True, IsSpecialValue(e));

  s := NegInfinity;
  d := NegInfinity;
  e := NegInfinity;
  CheckEquals(True, IsSpecialValue(s));
  CheckEquals(True, IsSpecialValue(d));
  CheckEquals(True, IsSpecialValue(e));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathInfNanSupportTest._MakeQuietNaN;
begin
  s := NegInfinity;
  d := NegInfinity;
  e := NegInfinity;

  MakeQuietNaN(s, 0);
  MakeQuietNaN(d, 0);
  MakeQuietNaN(e, 0);

  CheckEquals(True, JclMath.IsNan(s));
  CheckEquals(True, JclMath.IsNan(d));
  CheckEquals(True, JclMath.IsNan(e));

  MakeQuietNaN(s, 1);
  MakeQuietNaN(d, 2);
  MakeQuietNaN(e, 3);
  CheckEquals(True, JclMath.IsNan(s));
  CheckEquals(True, JclMath.IsNan(d));
  CheckEquals(True, JclMath.IsNan(e));
end;

//--------------------------------------------------------------------------------------------------

procedure TMathInfNanSupportTest._GetNaNTag;
var
  i: Integer;

begin
  for i := 1 to 8000 do
  begin
    MakeQuietNaN(s, i);
    MakeQuietNaN(d, i);
    MakeQuietNaN(e, i);
    CheckEquals(True, JclMath.IsNan(s));
    CheckEquals(True, JclMath.IsNan(d));
    CheckEquals(True, JclMath.IsNan(e));

    CheckEquals(i, GetNaNTag(s));
    CheckEquals(i, GetNaNTag(d));
    CheckEquals(i, GetNaNTag(e));
  end;
end;

//--------------------------------------------------------------------------------------------------

initialization
  RegisterTest('JCLMath', TMathLogarithmicTest.Suite);
  RegisterTest('JCLMath', TMathTranscendentalTest.Suite);
  RegisterTest('JCLMath', TMathMiscTest.Suite);
  RegisterTest('JCLMath', TMathRationalTest.Suite);
  RegisterTest('JCLMath', TMathExponentialTest.Suite);
  RegisterTest('JCLMath', TMathFlatSetTest.Suite);
  RegisterTest('JCLMath', TMathPrimeTest.Suite);
  RegisterTest('JCLMath', TMathInfNanSupportTest.Suite);
end.
