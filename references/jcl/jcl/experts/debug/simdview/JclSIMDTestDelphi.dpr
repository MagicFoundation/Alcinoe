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
{ The Original Code is: JvSIMDTest.dpr, released on 2004-10-11.                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{ [ouchet dott florent att laposte dott net]                                                       }
{ All Rights Free.                                                                                 }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
{ located at https://github.com/project-jedi/jcl                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

program JclSIMDTestDelphi;

{$APPTYPE CONSOLE}

{$I jedi.inc}

uses
  SysUtils,
  Windows,
  Dialogs;

var
   Values: array [0..3] of single;
   Index, ErrorCode: Integer;
   Number: String;
begin
  WriteLn('Streaming SIMD Extension of Intel Pentium and AMD Athlon processors');
  WriteLn('By Ouchet Florent <outchy_at_users.sourceforge.net>');
  WriteLn('Released 2004,10,12');
  WriteLn('All rights free');
  WriteLn;
  for Index:=Low(Values) to High(Values) do
    repeat
      Write('Enter the floating point value ',Index,' : ');
      ReadLn(Number);
      if (DecimalSeparator<>'.') then
        Number:=StringReplace(Number,DecimalSeparator,'.',[rfReplaceAll,rfIgnoreCase]);
      Val(Number,Values[Index],ErrorCode);
    until (ErrorCode=0);

  WriteLn;
  WriteLn('Check values :');
  for Index:=Low(Values) to High(Values) do
    WriteLn('Value ',Index,' is : ',Values[Index]:2:3);

  WriteLn;
  WriteLn('Starting computations : Values*2 ...');
  asm
  // breakpoint here
  // hit ctrl+alt+D or go to View/Debug window and open the last item
  // these instructions operate on 4-packed-single-precision floating point values
  // so you should view these registers has single values
    LEA      EAX,  Values
{$IFDEF COMPILER6_UP}
    movups   xmm0, [eax]       // moving Values into xmm0
    addps    xmm0, xmm0        // xmm0 :- xmm0 + xmm0
    movups   [eax], xmm0       // moving xmm0 into Values
{$ELSE}
    DB       0Fh, 10h, 00h     // movups xmm0, [eax]
    DB       0Fh, 58h, 0C0h    // addps xmm0, xmm0
    DB       0Fh, 11h, 00h     // movups [eax], xmm0
{$ENDIF}
  end;
  WriteLn('Computations ended');
  WriteLn;
  WriteLn('Now values are :');
  for Index:=Low(Values) to High(Values) do
    WriteLn('Value ',Index,' is : ',Values[Index]:2:3);
  WriteLn;
  WriteLn('Program terminated');
  ReadLn;

end.
