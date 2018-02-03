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
{ The Original Code is JclValidation.pas                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is Ivo Bauer.                                         }
{ Portions created by Ivo Bauer are Copyright Ivo Bauer. All rights reserved.                      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains ISBN validation routines                                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclValidation;

{$I jcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

// ISBN: International Standard Book Number
function IsValidISBN(const ISBN: string): Boolean;

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

uses
  JclBase,
  JclSysUtils;
  
{ TODO -cDoc : Donator: Ivo Bauer }
function IsValidISBN(const ISBN: string): Boolean;
//
// References:
// ===========
// [1] http://isbn-international.org/en/userman/chapter4.html
//
type
  TISBNPart = (ipGroupID, ipPublisherID, ipTitleID, ipCheckDigit);
  TISBNPartSizes = array [TISBNPart] of Integer;
const
  ISBNSize = 13;

function CharIsISBNSpecialDigit(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  case C of
    'x', 'X':
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsISBNSeparator(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  case C of
    #32, '-':
      Result := True;
  else
    Result := False;
  end;
end;

function CharIsISBNCharacter(const C: Char): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  case C of
    '0'..'9',
    'x', 'X',
    #32, '-':
      Result := True;
  else
    Result := False;
  end;
end;

var
  CurPtr, EndPtr: Integer;
  Accumulator, Counter: Integer;
  Part: TISBNPart;
  PartSizes: TISBNPartSizes;

  function IsPartSizeValid(APart: TISBNPart): Boolean;
  const
    MaxPartSizes: TISBNPartSizes = (5, 7, 6, 1);
  begin
    Result := PartSizes[APart] <= MaxPartSizes[APart];
  end;

begin
  Result := False;
  // At first, check the overall string length.
  if Length(ISBN) <> ISBNSize then
    Exit;

  CurPtr := 1;
  EndPtr := ISBNSize - 1;
  Accumulator := 0;
  Counter := 10;
  Part := ipGroupID;
  ResetMemory(PartSizes[Low(PartSizes)], SizeOf(PartSizes));

  while CurPtr <= EndPtr do
  begin
    if CharIsISBNCharacter(ISBN[CurPtr]) then
    begin
      if CharIsISBNSeparator(ISBN[CurPtr]) then
      begin
        // Switch to the next ISBN part, but take care of two conditions:
        // 1. Do not let Part go beyond its upper bound (ipCheckDigit).
        // 2. Verify if the current ISBN part does not exceed its size limit.
        if (Part < High(Part)) and IsPartSizeValid(Part) then
          Inc(Part)
        else
          Exit;
      end
      else // CurPtr^ in [ISBNDigits, ISBNSpecialDigits]
      begin
        // Is it the last character of the string?
        if (CurPtr = EndPtr) then
        begin
          // Check the following conditions:
          // 1. Make sure current ISBN Part equals to ipCheckDigit.
          // 2. Verify if the check digit does not exceed its size limit.
          if (Part <> High(Part)) and not IsPartSizeValid(Part) then
            Exit;
        end
        else
          // Special check digit is allowed to occur only at the end of ISBN.
          if CharIsISBNSpecialDigit(ISBN[CurPtr]) then
            Exit;

        // Increment the size of the current ISBN part.
        Inc(PartSizes[Part]);

        // Increment the accumulator by current ISBN digit multiplied by a weight.
        // To get more detailed information, please refer to the web site [1].
        if (CurPtr = EndPtr) and CharIsISBNSpecialDigit(ISBN[CurPtr]) then
          Inc(Accumulator, 10 * Counter)
        else
          Inc(Accumulator, (Ord(ISBN[CurPtr]) - Ord('0')) * Counter);
        Dec(Counter);
      end;
      Inc(CurPtr);
    end
    else
      Exit;
  end;
  // Accumulator content must be divisible by 11 without a remainder.
  Result := (Accumulator mod 11) = 0;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
