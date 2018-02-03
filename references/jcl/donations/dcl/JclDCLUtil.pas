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
{ The Original Code is DCLUtil.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclDCLUtil;

interface

uses
  SysUtils,
  JclBase, JclDCL_Intf, JclStrings;

const
  DCLDefaultCapacity = 16;

type
  // Exceptions
  EDCLError = class(EJclError);
  EDCLOutOfBoundsError = class(EDCLError);
  EDCLNoSuchElementError = class(EDCLError);
  EDCLIllegalStateError = class(EDCLError);
  EDCLConcurrentModificationError = class(EDCLError);
  EDCLIllegalArgumentError = class(EDCLError);
  EDCLOperationNotSupportedError = class(EDCLError);

resourcestring
  RsEOutOfBounds = 'Out of bounds';
  //RsENoSuchElement = 'No such element';
  //RsEIllegalState = 'Illegal state';
  //RsEConcurrentModification = 'Concurrent modification';
  //RsEIllegalArgument = 'Illegal argument';
  RsEOperationNotSupported = 'Operation not supported';
  RsEValueNotFound = 'Value %s not found';
  RsENoCollection = 'Collection = nil';

procedure DCLAppendDelimited(Obj: IStrCollection; const AString, Separator: string);

implementation

procedure DCLAppendDelimited(Obj: IStrCollection; const AString, Separator: string);
var
  Item: string;
  SepLen: Integer;
  PString, PSep, PPos: PChar;
begin
  PString := PChar(AString);
  PSep := PChar(Separator);
  PPos := StrPos(PString, PSep);
  if PPos <> nil then
  begin
    SepLen := StrLen(PSep);
    repeat
      SetLength(Item, PPos - PString + 1);
      Move(PString^, Item[1], PPos - PString);
      Item[PPos - PString + 1] := #0;
      Obj.Add(Item);
      PString := PPos + SepLen;
      PPos := StrPos(PString, PSep);
    until PPos = nil;
    if StrLen(PString) > 0 then //ex. hello#world
      Obj.Add(PString);
  end
  else //There isnt a Separator in AString
    Obj.Add(AString);
end;

end.
