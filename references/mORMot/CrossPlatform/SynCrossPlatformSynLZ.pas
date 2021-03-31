/// SynLZ compression cross-platform unit
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformSynLZ;

interface

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2021 Arnaud Bouchez
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

  The Original Code is Synopse mORMot framework.

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


  Compatible with the main SynLZ.pas unit
  Should compile with Delphi for any platform (including NextGen for mobiles),
    with FPC 2.7 or Kylix - but not yet with SmartMobileStudio 2.1.1

}

/// get maximum possible (worse) compressed size for out_p
function SynLZcomplen(in_len: cardinal): cardinal;

/// get uncompressed size from lz-compressed buffer (to reserve memory, e.g.)
function SynLZdecomplen(in_p: pointer): cardinal;

/// 1st compression algorithm uses hashing with a 32bits control word
function SynLZcomp(src: pointer; size: cardinal; dst: pointer): cardinal;

/// 1st compression algorithm uses hashing with a 32bits control word
// - this is the fastest pure pascal implementation
function SynLZdecomp(src: pointer; size: cardinal; dst: pointer): cardinal;


implementation

function SynLZcomplen(in_len: cardinal): cardinal;
begin
  result := in_len+in_len shr 3+16; // worse case
end;

function SynLZdecomplen(in_p: pointer): cardinal;
begin
  result := PWord(in_p)^;
  inc(PWord(in_p));
  if result and $8000<>0 then
    result := (result and $7fff) or (cardinal(PWord(in_p)^) shl 15);
end;

type
{$ifdef FPC}
  PBytes = PAnsiChar;
{$else}
  PtrUInt = {$ifdef UNICODE} NativeUInt {$else} cardinal {$endif};
  TBytes = array[0..maxInt-1] of byte;
  PBytes = ^TBytes;
{$endif FPC}

function SynLZcomp(src: pointer; size: cardinal; dst: pointer): cardinal;
var dst_beg,          // initial dst value
    src_end,          // real last byte available in src
    src_endmatch,     // last byte to try for hashing
    o: PtrUInt;
    CWbit: cardinal;
    CWpoint: PCardinal;
    v, h, cached, t, tmax: PtrUInt;
    offset: array[0..4095] of PtrUInt;
    cache: array[0..4095] of cardinal; // 16KB+16KB=32KB on stack (48KB under Win64)
begin
  dst_beg := PtrUInt(dst);
  // 1. store in_len
  if size>=$8000 then begin // size in 32KB..2GB -> stored as integer
    PWord(dst)^ := $8000 or (size and $7fff);
    PWord(PtrUInt(dst)+2)^ := size shr 15;
    inc(PCardinal(dst));
  end else begin
    PWord(dst)^ := size ; // size<32768 -> stored as word
    if size=0 then begin
      result := 2;
      exit;
    end;
    inc(PWord(dst));
  end;
  // 2. compress
  src_end := PtrUInt(src)+size;
  src_endmatch := src_end-(6+5);
  CWbit := 1;
  CWpoint := pointer(dst);
  PCardinal(dst)^ := 0;
  inc(PByte(dst),sizeof(CWpoint^));
  fillchar(offset,sizeof(offset),0); // fast 16KB reset to 0
  // 1. main loop to search using hash[]
  if PtrUInt(src)<=src_endmatch then
  repeat
    v := PCardinal(src)^;
    h := ((v shr 12) xor v) and 4095;
    o := offset[h];
    offset[h] := PtrUInt(src);
    cached := v xor cache[h]; // o=nil if cache[h] is uninitialized
    cache[h] := v;
    if (cached and $00ffffff=0) and (o<>0) and (PtrUInt(src)-o>2) then begin
      CWpoint^ := CWpoint^ or CWbit;
      inc(PWord(src));
      inc(o,2);
      t := 1;
      tmax := src_end-PtrUInt(src)-1;
      if tmax>=(255+16) then
        tmax := (255+16);
      while (PBytes(o)[t]=PBytes(src)[t]) and (t<tmax) do
        inc(t);
      inc(PByte(src),t);
      h := h shl 4;
      // here we have always t>0
      if t<=15 then begin // mark 2 to 17 bytes -> size=1..15
        PWord(dst)^ := cardinal(t or h);
        inc(PWord(dst));
      end else begin // mark 18 to (255+16) bytes -> size=0, next byte=t
        dec(t,16);
        PWord(dst)^ := h; // size=0
        PByte(PtrUInt(dst)+2)^ := t;
        inc(PByte(dst),3);
      end;
    end else begin
      PByte(dst)^ := PByte(src)^;
      inc(PByte(src));
      inc(PByte(dst));
    end;
    inc(CWbit,CWBit);
    if CWbit=0 then begin
      CWpoint := pointer(dst);
      PCardinal(dst)^ := 0;
      inc(PCardinal(dst));
      inc(CWbit);
    end;
    if PtrUInt(src)<=src_endmatch then continue else break;
  until false;
  // 2. store remaining bytes
  if PtrUInt(src)<src_end then
  repeat
    PByte(dst)^ := PByte(src)^;
    inc(PByte(src));
    inc(PByte(dst));
    inc(CWbit,CWBit);
    if CWbit=0 then begin
      PCardinal(dst)^ := 0;
      inc(PCardinal(dst));
      inc(CWbit);
    end;
    if PtrUInt(src)<src_end then continue else break;
  until false;
  result := PtrUInt(dst)-dst_beg;
end;

function SynLZdecomp(src: pointer; size: cardinal; dst: pointer): cardinal;
var last_hashed, // initial src and dst value
    src_end: PtrUInt;
    CW, CWbit: cardinal;
    v, t, h, o: PtrUInt;
    i: integer;
    offset: array[0..4095] of PtrUInt; // 16KB hashing code
label nextCW;
begin
  src_end := PtrUInt(src)+size;
  // 1. retrieve out_len
  result := PWord(src)^;
  if result=0 then exit;
  inc(PWord(src));
  if result and $8000<>0 then begin
    result := (result and $7fff) or (cardinal(PWord(src)^) shl 15);
    inc(PWord(src));
  end;
  // 2. decompress
  last_hashed := PtrUInt(dst)-1;
nextCW:
  CW := PCardinal(src)^;
  inc(PCardinal(src));
  CWbit := 1;
  if PtrUInt(src)<src_end then
  repeat
    if CW and CWbit=0 then begin
      PByte(dst)^ := PByte(src)^;
      inc(PByte(src));
      inc(PByte(dst));
      if PtrUInt(src)>=src_end then break;
      if last_hashed<PtrUInt(dst)-3 then begin
        inc(last_hashed);
        v := PCardinal(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end else begin
      h := PWord(src)^;
      inc(PWord(src));
      t := (h and 15)+2;
      h := h shr 4;
      if t=2 then begin
        t := PByte(src)^+(16+2);
        inc(PByte(src));
      end;
      o := offset[h];
      if PtrUInt(dst)-o<t then
        for i := 0 to t-1 do // movechars is slower
          PBytes(dst)[i] := PBytes(o)[i] else
        if t<=8 then
          PInt64(dst)^ := PInt64(o)^ else
          move(pointer(o)^,pointer(dst)^,t);
      if PtrUInt(src)=src_end then break;
      while last_hashed<PtrUInt(dst) do begin
        inc(last_hashed);
        v := PCardinal(last_hashed)^;
        offset[((v shr 12) xor v) and 4095] := last_hashed;
      end;
      inc(PByte(dst),t);
      last_hashed := PtrUInt(dst)-1;
      CWbit := CWbit shl 1;
      if CWbit<>0 then
        continue else
        goto nextCW;
    end;
  until false;
end;

end.
