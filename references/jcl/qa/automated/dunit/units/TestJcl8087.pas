{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      Jcl8087                                                                                  }
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

unit TestJcl8087;

interface
uses
  TestFramework,
  SysUtils,
  Jcl8087;

 { TJcl8087Test}

type
  TJcl8087Test = class (TTestCase)
  private
    FControlWord: Word;
    procedure SaveControlWord;
    procedure RestoreControlWord;

  published
    procedure Infinity;
    procedure Precision;
    procedure Rounding;
    procedure Exceptions;
  end;

implementation

//==================================================================================================
// TJcl8087Test
//==================================================================================================

procedure TJcl8087Test.SaveControlWord;
begin
  FControlWord := System.Get8087CW;
end;

//--------------------------------------------------------------------------------------------------

procedure TJcl8087Test.RestoreControlWord;
begin
  System.Set8087CW(FControlWord);
end;

//--------------------------------------------------------------------------------------------------

procedure TJcl8087Test.Infinity;
var
  i: integer;
  r: integer;

begin
  SaveControlWord;

  // Test icAffine
  Set8087Infinity(icAffine);
  Check(Get8087Infinity = icAffine);
  Check((System.Get8087CW and 4096) > 0);

  // Test icProjective
  Set8087Infinity(icProjective);
  Check(Get8087Infinity = icProjective);
  Check(Get8087Infinity = icProjective);
  Check((System.Get8087CW and 4096) = 0);

  RandSeed := 1234561;

  for i := 1 to 50000 do
  begin
    r := random(2);
    Set8087Infinity(T8087Infinity(r));
    Check(Get8087Infinity = T8087Infinity(r));
    Check(Get8087Infinity = T8087Infinity(r));

    if r = 0 then
     Check((System.Get8087CW and 4096) = 0)
    else
     Check((System.Get8087CW and 4096) > 0);
  end;

  RestoreControlWord;
end;

//--------------------------------------------------------------------------------------------------

procedure TJcl8087Test.Precision;
var
  i: Integer;
  r: Integer;

begin
  SaveControlWord;
  Set8087Precision(pcSingle);
  Set8087Precision(pcReserved);
  Set8087Precision(pcDouble);
  Set8087Precision(pcExtended);

  Check(Get8087Precision = pcExtended);
  Check((System.Get8087CW and 768) = 768);

  Set8087Precision(pcDouble);
  Check(Get8087Precision = pcDouble);
  Check((System.Get8087CW and 768) = 512);

  Set8087Precision(pcSingle);
  Check(Get8087Precision = pcSingle);
  Check((System.Get8087CW and 768) = 0);

  RandSeed := 1234561;

  for i := 1 to 50000 do
  begin
    r := random(4);
    Set8087Precision(T8087Precision(r));
    Check(Get8087Precision = T8087Precision(r));
    Check(Get8087Precision = T8087Precision(r));

    case r of
      0:  Check((System.Get8087CW and 768) = 0);
      1:  Check((System.Get8087CW and 768) = 256);
      2:  Check((System.Get8087CW and 768) = 512);
      3:  Check((System.Get8087CW and 768) = 768);
    end;
  end;

  RestoreControlWord;
end;

//--------------------------------------------------------------------------------------------------

procedure TJcl8087Test.Rounding;
var
  i: Integer;
  r: Integer;

begin
  SaveControlWord;

  RandSeed := 1234561;

  for i := 1 to 50000 do
  begin
    r := random(4);
    Set8087Rounding(T8087Rounding(r));

    Check(Get8087Rounding = T8087Rounding(r));

    case r of
      0: Check((System.Get8087CW and 3072) = 0);
      1: Check((System.Get8087CW and 3072) = 1024);
      2: Check((System.Get8087CW and 3072) = 2048);
      3: Check((System.Get8087CW and 3072) = 3072);
    end;
  end;

  RestoreControlWord;
end;
//--------------------------------------------------------------------------------------------------

procedure TJcl8087Test.Exceptions;
begin
  SaveControlWord;
  Mask8087Exceptions(All8087Exceptions);
  Check(GetMasked8087Exceptions = All8087Exceptions);

  UnMask8087Exceptions([emZeroDivide]);
  Check(GetMasked8087Exceptions = [emInvalidOp, emDenormalizedOperand, emOverflow, emUnderflow, emPrecision]);

  UnMask8087Exceptions([emInvalidOp]);
  Check(GetMasked8087Exceptions = [emDenormalizedOperand, emOverflow, emUnderflow, emPrecision]);

  UnMask8087Exceptions([emDenormalizedOperand]);
  Check(GetMasked8087Exceptions = [emOverflow, emUnderflow, emPrecision]);

  UnMask8087Exceptions([emOverflow]);
  Check(GetMasked8087Exceptions = [emUnderflow, emPrecision]);

  UnMask8087Exceptions([emUnderflow]);
  Check(GetMasked8087Exceptions = [emPrecision]);

  UnMask8087Exceptions([emPrecision]);
  Check(GetMasked8087Exceptions = []);

  Mask8087Exceptions([emOverflow]);
  Check(GetMasked8087Exceptions = [emOverflow]);
  Check(GetMasked8087Exceptions <> [emOverflow, emPrecision]);

  RestoreControlWord;
end;


initialization
  RegisterTest('JCL8087', TJcl8087Test.Suite);


end.









