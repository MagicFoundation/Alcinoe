/// this unit will patch the System.pas RTL to use a custom NON OLE COMPATIBLE
// WideString type, NOT using the slow Windows API, but FastMM4 (without COW)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFastWideString;

interface

(*
    This file is part of Synopse Framework.

    Synopse Framework. Copyright (C) 2021 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse Framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2021
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  DISCLAIMER:

   Purpose of this unit is to patch the system.pas unit for older versions
   of Delphi, so that WideString memory allocation would use FastMM4 instead
   of the slow BSTR Windows API.

   It will speed up the WideString process a lot, especially when a lot of
   content is allocated, since FastMM4 is much more aggressive than Windows'
   global heap and the BSTR slow API. It could be more than 50 times faster,
   especially when releasing the used memory.

   The WideString implementation pattern does NOT feature Copy-On-Write, so is
   slower than the string=UnicodeString type as implemented since Delphi 2009.
   This is the reason why this unit won't do anything on Unicode versions of
   the compiler, since the new string type is to be preferred.

  HOW TO USE:

   Just add the unit at the TOP of your .dpr uses clause, just after FastMM4
   (if you use it, and you should!) i.e. before all other units used by your
   program. It should be initialized before any WideString is allocated.

   Then the patch will be applied at runtime. Nothing to recompile!

    program MyProgram;

    uses
      {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
      SynFastWideString,     // will use FastMM4 prior to Delphi 2009
      ...

  NOTE:

   Since we add a trailing 0 byte at the end of the buffer, we need the
   memory manager to let enough place for it: oldest Borland MM does not :(
   SO IF YOU WORK WITH A VERSION PRIOR TO DELPHI 2006, ADD FASTMM4 TO YOUR .DPR

  WARNING:

   ------------------------------------------------------------------
    USING THIS UNIT MAY BREAK COMPATIBILITY WITH OLE/COM LIBRARIES !
   ------------------------------------------------------------------
   You won't be able to retrieve and release WideString/BSTR variables from an
   OleDB / ADO database provider, or any COM object.
   Do not use this unit if you are calling such external call!

   In practice, if you only SEND some BSTR content to the provider (e.g. if
   you use our SynOleDB unit without stored procedure call, or if you use
   TWideString fields for most SynDBDataSet classes), it will work.
   You would have issues only if you *retrieve* a BSTR from the COM object,
   or expect the COM object to *change* the BSTR size, e.g. with a "var"
   WideString parameter or a COM method returning a WideString. In this case,
   you could use the WideStringFree() procedure to release such an instance.

   It is for educational purpose only, and/or if you are 100% sure that your
   code will stay self-contained, under Delphi 7 or Delphi 2007, and need use
   of WideString instead of string=AnsiString.

   -----------------------------------------------
    YOU HAVE BEEN WARNED - USE AT YOUR OWN RISK !   :)
   -----------------------------------------------

*)

/// this low-level helper can be used to free a WideString returned by COM/OLE
// - WideString instances created with this unit can be safely sent to any
// COM/OLE object, as soon as they are constant parameters, but not a "var"
// parameter or a callback function result
// - any WideString instance returned by a COM object should NOT be released
// by Delphi automatically, since the following would create a memory error:
// ! TrueBSTRWideStringVariable := '';
// - if you are using SynFastWideString, you should use this procedure to
// release true BSTR WideString instance, as such:
// ! type
// !  _Catalog = interface(IDispatch)
// !    // this method will be safe to use with our unit
// !    function Create(const ConnectString: WideString): OleVariant; safecall;
// !    // this method won't be safe, since it returns a true BSTR as WideString
// !    function GetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
// !                            ObjectTypeId: OleVariant): WideString; safecall;
// ! end;
// !...
// !function CheckCatalogOwner(const catalog: _Catalog): string;
// !var bstr: WideString;
// !begin
// !  try // force manual handling of this true BSTR instance lifetime
// !   bstr := catalog.GetObjectOwner('name',null,null);
// !   result := bstr; // conversion to string will work
// !  finally
// !    WideStringFree(bstr); // manual release, and set bstr := nil
// !  end;
// !end;
// - do a regular TrueBSTRWideStringVariable := '' since Delphi 2009, or
// call the low-level oleaut32.dll API for older versions, as expected by COM
procedure WideStringFree(var TrueBSTRWideStringVariable: WideString);


implementation

{$ifdef UNICODE}
  // since Delphi 2009, use string=UnicodeString type, which features CopyOnWrite
  // -> do not patch anything
  {$define NOOVERRIDE}
{$endif}
{$ifdef FPC}
  // our low-level Delphi Win32 specific hack won't work with FPC
  {$define NOOVERRIDE}
{$endif}

{$ifdef NOOVERRIDE}

procedure WideStringFree(var TrueBSTRWideStringVariable: WideString);
begin
  TrueBSTRWideStringVariable := ''; // regular handling via System.pas
end;

{$else}

uses
  Windows;

{$RANGECHECKS OFF}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}
{$DEBUGINFO OFF}

type // some types here since we do not want any dependency on any other units
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of byte;

// we need to patch the oleaut32.dll library calls as defined in System.pas
// -> retrieve CALL address from low-level funtions asm, then the API slot

function _SysAllocStringLen: pointer;
asm
  lea eax,System.@WStrFromPWCharLen+8
end;

function _SysReAllocStringLen: pointer;
asm
  lea eax,System.@WStrAsg+8
end;

function _SysFreeString: pointer;
asm
  lea eax,System.@WStrClr+8
end;

procedure PatchAPI(source,dest: pointer);
var RestoreProtection,Ignore: DWORD;
begin
  while PByte(source)^<>$e8 do  // search first CALL within the function code
    inc(PByte(source));
  inc(PByte(source));
  inc(PByte(source),PInteger(source)^+SizeOf(integer)); // go to the CALL stub
  if PWord(source)^<>$25ff then // expect "jmp dword ptr []" asm
    halt;
  inc(PWord(source));
  source := PPointer(source)^;  // get "dword ptr []" address of API redirection
  if VirtualProtect(source,SizeOf(source),PAGE_EXECUTE_READWRITE,RestoreProtection) then begin
    PPointer(source)^ := dest;  // replace oleaut32.dll API with our own function
    VirtualProtect(source,SizeOf(source),RestoreProtection,Ignore);
    FlushInstructionCache(GetCurrentProcess,source,SizeOf(source));
  end;
end;


// those are the 3 redirected API calls -> just use AnsiString for allocation

function SysAllocStringLen(P: PAnsiChar; Len: integer): pointer; stdcall;
begin
  result := nil;
  Len := Len*2;
  SetString(AnsiString(result),P,Len);
  PByteArray(result)[Len+1] := 0; // ensure finishes with a #0 WideChar
end;

function SysReAllocStringLen(var S: AnsiString; P: PAnsiChar; Len: integer): LongBool; stdcall;
begin
  Len := Len*2;
  SetString(S,P,Len);
  PByteArray(S)[Len+1] := 0; // ensure finishes with a #0 WideChar
  result := true;
end;

procedure SysFreeString(S: pointer); stdcall;
begin
  AnsiString(S) := '';
end;

procedure OleAut32SysFreeString(S: pointer); stdcall;
  external 'oleaut32.dll' name 'SysFreeString'

procedure WideStringFree(var TrueBSTRWideStringVariable: WideString);
begin
  if pointer(TrueBSTRWideStringVariable)<>nil then begin
    OleAut32SysFreeString(pointer(TrueBSTRWideStringVariable));
    pointer(TrueBSTRWideStringVariable) := nil;
  end;
end;

initialization
  PatchAPI(_SysAllocStringLen,@SysAllocStringLen);
  PatchAPI(_SysReAllocStringLen,@SysReAllocStringLen);
  PatchAPI(_SysFreeString,@SysFreeString);

{$endif NOOVERRIDE}

end.
